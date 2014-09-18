************************************************************************
* Program Name      : ZACO28U_SKF9
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.11.17.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K904241
* Addl Documentation:
* Description       : Create Input Working hours by Process


* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT ZACO28U_SKF9 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

** Table
TABLES : CSKS ,  CBPR , COSR . " , ZTCO_SKF_STATUS.

** For OBJECT KEY
DATA: BEGIN OF IT_CBPR OCCURS 0,
*        KOKRS TYPE CBPR-KOKRS,
        PRZNR TYPE CBPR-PRZNR,
        OBJNR TYPE CBPR-OBJNR,
        KOSTL LIKE CBPR-KOSTL,
      END OF IT_CBPR.

** Internal Table
DATA: BEGIN OF IT_COSR OCCURS 0,
        OBJNR TYPE COSR-OBJNR,
        GJAHR TYPE COSR-GJAHR,
        WRTTP TYPE COSR-WRTTP,
        VERSN TYPE COSR-VERSN,
        STAGR TYPE COSR-STAGR,
        MEINH TYPE COSR-MEINH,
        SME001 TYPE COSR-SME001,
        SME002 TYPE COSR-SME002,
        SME003 TYPE COSR-SME003,
        SME004 TYPE COSR-SME004,
        SME005 TYPE COSR-SME005,
        SME006 TYPE COSR-SME006,
        SME007 TYPE COSR-SME007,
        SME008 TYPE COSR-SME008,
        SME009 TYPE COSR-SME009,
        SME010 TYPE COSR-SME010,
        SME011 TYPE COSR-SME011,
        SME012 TYPE COSR-SME012,
        SME013 TYPE COSR-SME013,
        SME014 TYPE COSR-SME014,
        SME015 TYPE COSR-SME015,
        SME016 TYPE COSR-SME016,
      END OF IT_COSR.

DATA: BEGIN OF IT_TMP_COSR OCCURS 0,
        OBJNR TYPE COSR-OBJNR,
        PRZNR TYPE CBPR-PRZNR,
        GJAHR TYPE COSR-GJAHR,
        PERID  LIKE COEJL-PERBL,
        STAGR TYPE COSR-STAGR,
        MEINH TYPE COSR-MEINH,
        SMEXXX TYPE COSR-SME001,
      END OF IT_TMP_COSR.

* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0,
        GJAHR   LIKE COSL-GJAHR,
        PERID   LIKE COEJL-PERBL,
        BLDAT   LIKE COHEADER-BLDAT,          " doc date
        BUDAT   LIKE COHEADER-BUDAT,         " Posting date
        SENBUSPROC LIKE BAPIAAITM-SENBUSPROC, "Sender business process
        RECBUSPROC LIKE BAPIAAITM-RECBUSPROC, "Receiver business process
        TQTY    LIKE COSL-LST001,
      END OF IT_REPORT.



** For ALV
DATA : GV_REPID LIKE SY-REPID.
DATA : GV_STATUS       TYPE SLIS_FORMNAME VALUE 'PF_STATUS'.
DATA : GV_USER_COMMAND TYPE SLIS_FORMNAME VALUE 'USER_COMMAND'.
DATA : IT_SORT         TYPE SLIS_T_SORTINFO_ALV WITH HEADER LINE .
DATA : GV_COL_POS TYPE I.
DATA : IT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
       WA_FIELDCAT          LIKE LINE OF IT_FIELDCAT,
       IT_EVENTCAT          TYPE SLIS_T_EVENT,
       WA_EVENTCAT          LIKE LINE OF IT_EVENTCAT.
DATA : IT_EVENTS	          TYPE SLIS_T_EVENT,
       IT_EVENT_EXIT	    TYPE SLIS_T_EVENT_EXIT.

*** For BAPI
DATA : IT_VALUE LIKE STANDARD TABLE OF BAPI1114_VALUES
                             WITH HEADER LINE.
DATA : IT_NODE LIKE STANDARD TABLE OF BAPISET_HIER
                             WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : WA_DOC_HEADER LIKE BAPIDOCHDRP .
DATA : IT_DOC_ITEMS  LIKE STANDARD TABLE OF BAPIAAITM
                      WITH HEADER LINE.

DATA  W_DAY(30).
DATA  GV_P_VERSN(3).
*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
             P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
             P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY,
             P_TRUN(1).
SELECTION-SCREEN SKIP 1.


SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_PRZNR  FOR CBPR-PRZNR.
PARAMETERS:      P_PRZNR  LIKE KMAS_D-PRZNR_SET.
*                                     DEFAULT 'BP_HMMA'.
SELECTION-SCREEN END OF BLOCK BL3.
SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.

*  KB21NP- BUSINESS PROCESS ALLOCATION - Posting, version.
  GV_P_VERSN = '500'.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM CHK_INPUT_VALUE.

* Searching for Bus.Process group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_PRZNR.
  PERFORM READ_CEGRP_GROUP USING '0107'
                                 P_PRZNR.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read Factory Calendar Information
  PERFORM READ_CALENDAR_INFO.
* Read Bus.Process group - > Bus.Process
  PERFORM READ_CEGRP.
* Read Business Process Info.
  PERFORM READ_CBPR.
* Read _Quantity Information from COSR.
  PERFORM READ_COSR_QTY.
* Preparation of reporting data.
  PERFORM PRE_REPORT_DATA.
* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV LIST
  PERFORM CALL_ALV_LIST.


*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*       Search Help for Cost element Group / CCTr Group
*----------------------------------------------------------------------*
*  -->  p_class      Class Name
*  <--  P_SET_NAME   Result Group Name
*----------------------------------------------------------------------*
FORM READ_CEGRP_GROUP USING   P_CLASS
                               P_SET_NAME.

  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      CLASS                    = P_CLASS
*     CRUSER                   = '*'
      FIELD_NAME               = SPACE
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      SEARCHFLD_REQUIRED       = 'X'
*     SET                      = '*'
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      SET_NAME                 = P_SET_NAME
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      NO_SET_PICKED            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " READ_CEGRP_GROUP
*&---------------------------------------------------------------------*
*&      Form  READ_CEGRP
*&---------------------------------------------------------------------*
*       read Bus.Process
*----------------------------------------------------------------------*
FORM READ_CEGRP.

* Making an internal table for Bus.process group to select data
* Selected Group on screen
  CLEAR : IT_VALUE, IT_VALUE[].
  CLEAR : IT_NODE, IT_NODE[].
  CLEAR : IT_RETURN, IT_RETURN[].

* Set Validity Date (Start)
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO LV_DATUM.

  IF NOT P_PRZNR IS INITIAL.
    CALL FUNCTION 'BAPI_BUSPROCESSCOGRP_GETDETAIL'
      EXPORTING
        CONTROLLINGAREA       = P_KOKRS
        GROUPNAME             = P_PRZNR
*  IMPORTING
*    RETURN                =
      TABLES
        HIERARCHYNODES        = IT_NODE
        HIERARCHYVALUES       = IT_VALUE.
* Message
    PERFORM DIS_BAPI_MESSAGE.
  ENDIF.


** From Bus.process  on selection screen
** From Select-options.
  DATA : IT_L_CCTR LIKE STANDARD TABLE OF BAPI1036_BPLIST
                 WITH HEADER LINE.

  IF NOT S_PRZNR[] IS INITIAL.
    LOOP AT S_PRZNR.
      CLEAR : IT_L_CCTR, IT_L_CCTR[].

      CALL FUNCTION 'BAPI_PROCESS_GETLIST'
       EXPORTING
         CONTROLLINGAREA               = P_KOKRS
         BUSINESSPROCESS               = S_PRZNR-LOW
         BUSINESSPROCESSTO             = S_PRZNR-HIGH
         DATEFROM                      = LV_DATUM
*         DATETO                        =
*         MASTER_DATA_INACTIVE          =
        TABLES
          BUSINESSPROCESSLIST           = IT_L_CCTR
          RETURN                        = IT_RETURN      .

* Message
      PERFORM DIS_BAPI_MESSAGE.


* Appending  list
      LOOP AT IT_L_CCTR.
        MOVE IT_L_CCTR-CO_BUSPROC TO IT_VALUE-VALFROM.
        APPEND IT_VALUE.
      ENDLOOP.
*      APPEND LINES OF IT_L_CCTR  TO IT_VALUE.
      CLEAR IT_VALUE.
      CLEAR S_PRZNR.
    ENDLOOP.
  ENDIF.

* Sorting
*  SORT IT_VALUE BY VALFROM.
*  DELETE ADJACENT DUPLICATES FROM IT_VALUE.
*
** Check Bus.p
  IF IT_VALUE[] IS INITIAL .
    MESSAGE E018(ZMCO) WITH 'Business Process'.
  ENDIF.


ENDFORM.                    " READ_CEGRP
*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_BAPI_MESSAGE.
  IF NOT IT_RETURN[] IS INITIAL.
    LOOP AT   IT_RETURN.
      MESSAGE ID     IT_RETURN-ID
              TYPE   IT_RETURN-TYPE
              NUMBER IT_RETURN-NUMBER
              WITH   IT_RETURN-MESSAGE_V1
                     IT_RETURN-MESSAGE_V2
                     IT_RETURN-MESSAGE_V3
                     IT_RETURN-MESSAGE_V4.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  READ_CBPR
*&---------------------------------------------------------------------*
*       Read Business Process Info. (only, First Attribute : 'DP000' )
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CBPR.

* Set Validity Date (Start)
  DATA   LV_DATUM LIKE SY-DATUM.
  CLEAR  LV_DATUM.
  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO LV_DATUM.

  CLEAR : IT_CBPR, IT_CBPR[].
  CLEAR CBPR.
  SELECT PRZNR OBJNR KOSTL
         INTO CORRESPONDING FIELDS OF TABLE IT_CBPR
         FROM CBPR
          FOR ALL ENTRIES IN IT_VALUE
           WHERE KOKRS = P_KOKRS
             AND PRZNR = IT_VALUE-VALFROM
             AND DATBI >= LV_DATUM
             AND DATAB <= LV_DATUM       "Validity Date (Start)
             AND ATTR1 LIKE '%DP000%'.
  CLEAR  IT_CBPR.

*Sort
  SORT IT_CBPR BY OBJNR.

  DATA : LINE TYPE I.
  CLEAR : LINE.
  DESCRIBE TABLE IT_CBPR LINES LINE.
  IF LINE = 0.
    MESSAGE E000(ZMCO) WITH ' Entry not found '.
  ENDIF.


ENDFORM.                    " READ_CBPR
*&---------------------------------------------------------------------*
*&      Form  READ_COSR_QTY
*&---------------------------------------------------------------------*
*       Read Quantity data from COSR
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COSR_QTY.

  CLEAR : IT_COSR, IT_COSR[].
  CLEAR COSR.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_COSR
         FROM COSR
         FOR ALL ENTRIES IN IT_CBPR
        WHERE LEDNR = '00'
          AND OBJNR = IT_CBPR-OBJNR
          AND GJAHR = P_GJAHR
          AND WRTTP = '04'                 " Actual
          AND VERSN = P_VERSN
          AND STAGR = 'AS003'.
  CLEAR : IT_COSR.


* Local Data definition
  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_LST_NAM(30).

  CLEAR   IT_COSR.
  CLEAR : IT_TMP_COSR, IT_TMP_COSR[].


  LOOP AT IT_COSR.
* Key Part
    IT_TMP_COSR-GJAHR = P_GJAHR.
    IT_TMP_COSR-PERID = P_PERID.

    CLEAR IT_CBPR.
    READ TABLE    IT_CBPR
         WITH KEY OBJNR = IT_COSR-OBJNR.

    IT_TMP_COSR-PRZNR = IT_CBPR-PRZNR.
    IT_TMP_COSR-OBJNR = IT_COSR-OBJNR.

    IT_TMP_COSR-MEINH = IT_COSR-MEINH.

* Value Transferring
    CLEAR LV_LST_NAM.
    CONCATENATE 'IT_COSR-'  'SME'  P_PERID
           INTO LV_LST_NAM.
    ASSIGN (LV_LST_NAM) TO <FS1>.
    CLEAR IT_TMP_COSR-SMEXXX.
    IT_TMP_COSR-SMEXXX = <FS1>.

    check IT_TMP_COSR-SMEXXX <> 0.
*   Unit Conversion
*    IF IT_TMP_COSR-MEINH <> 'STD'.
*      PERFORM UNIT_CONV USING IT_TMP_COSR-MEINH
*                              IT_TMP_COSR-SMEXXX.
*    ENDIF.

* Collect
    COLLECT IT_TMP_COSR.

    CLEAR IT_TMP_COSR.
    CLEAR IT_COSR.
  ENDLOOP.
  CLEAR IT_TMP_COSR.


ENDFORM.                    " READ_COSR_QTY
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.

* Building Field Cat.
  PERFORM FIELDCAT_INIT .

* Sort
  SORT IT_REPORT BY GJAHR PERID SENBUSPROC.
  CLEAR IT_REPORT.


* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.


ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT.

  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

* Key
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'GJAHR'  'X'            SPACE    SPACE
*    SPACE        '4'      'Year'           SPACE    SPACE    SPACE.
*
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'PERID'  'X'            SPACE    SPACE
*    SPACE        '6'      'Period'           SPACE    SPACE    SPACE.



  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'BUDAT'  'X'          SPACE    SPACE
    SPACE '10'      'Post Date'    SPACE    SPACE SPACE.


** Value
  PERFORM BUILD_FIELDCAT USING
  'IT_REPORT' 'SENBUSPROC'  SPACE            SPACE    SPACE
  SPACE '12'      'Send.Process'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
  'IT_REPORT' 'RECBUSPROC'  SPACE            SPACE    SPACE
  SPACE '12'      'Rec. Process'    SPACE    SPACE SPACE.

*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
*    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'TQTY'  SPACE            SPACE    SPACE
   SPACE '15'      'Total Qty'    SPACE    SPACE SPACE.



ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1197   text
*      -->P_1198   text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1203   text
*      -->P_1204   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT USING    VALUE(P_0100)
                             VALUE(P_0101)
                             VALUE(P_0102)
                             VALUE(P_0103)
                             VALUE(P_0104)
                             VALUE(P_0105)
                             VALUE(P_0106)
                             VALUE(P_0107)
                             VALUE(P_0108)
                             VALUE(P_0109)
                             VALUE(P_0110).

  ADD 1 TO GV_COL_POS.
  WA_FIELDCAT-TABNAME     = P_0100.
  WA_FIELDCAT-FIELDNAME   = P_0101.
  WA_FIELDCAT-KEY         = P_0102.
  WA_FIELDCAT-DO_SUM      = P_0103.
  WA_FIELDCAT-CFIELDNAME  = P_0104.
  WA_FIELDCAT-CTABNAME    = P_0105.
  WA_FIELDCAT-OUTPUTLEN   = P_0106.
  WA_FIELDCAT-SELTEXT_L   = P_0107.
  WA_FIELDCAT-DATATYPE    = P_0108.
  WA_FIELDCAT-QFIELDNAME  = P_0109.
  WA_FIELDCAT-QTABNAME    = P_0110.
  WA_FIELDCAT-COL_POS     = GV_COL_POS.
  APPEND WA_FIELDCAT TO IT_FIELDCAT.
  CLEAR WA_FIELDCAT.

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       call ALV function
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_LIST.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
       I_CALLBACK_PROGRAM             = GV_REPID
       I_CALLBACK_PF_STATUS_SET       = GV_STATUS
       I_CALLBACK_USER_COMMAND        = GV_USER_COMMAND
*     I_STRUCTURE_NAME               =
*     IS_LAYOUT                      =
       IT_FIELDCAT                    = IT_FIELDCAT[]
*     IT_EXCLUDING                   =
*     IT_SPECIAL_GROUPS              =
*       IT_SORT                        = IT_SORT[]        "
*     IT_FILTER                      =
*     IS_SEL_HIDE                    =
*     I_DEFAULT                      = 'X'
       I_SAVE                         = 'A'
*     IS_VARIANT                     =
       IT_EVENTS                      = IT_EVENTS     "
       IT_EVENT_EXIT                  = IT_EVENT_EXIT   "
*     IS_PRINT                       =
*     IS_REPREP_ID                   =
*     I_SCREEN_START_COLUMN          = 0
*     I_SCREEN_START_LINE            = 0
*     I_SCREEN_END_COLUMN            = 0
*     I_SCREEN_END_LINE              = 0
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER        =
*     ES_EXIT_CAUSED_BY_USER         =
     TABLES
       T_OUTTAB                       = IT_REPORT
       EXCEPTIONS
       PROGRAM_ERROR                  = 1
       OTHERS                         = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_ALV_LIST

*-----------------------------------------------------------------------
*    FORM PF_STATUS
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST' EXCLUDING EXTAB .
ENDFORM.

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       For User_command - AT User Command                            *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                        SELFIELD TYPE SLIS_SELFIELD.
  CASE UCOMM.
* Important part !
    WHEN 'POST'.
      PERFORM POST_STD  USING UCOMM.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE : / 'Controlling Area/          : '
            , P_KOKRS .
  WRITE : / 'Fiscal Year/Period/Version : '
            , P_GJAHR, '/', P_PERID , '/', P_VERSN.
  WRITE : / 'Posting Version            : '
            , GV_P_VERSN.

* CCTR
  IF P_PRZNR NE SPACE.
    WRITE : / 'Business Process Grp.      : ', P_PRZNR.
  ENDIF.

  IF NOT S_PRZNR[] IS INITIAL.
    LOOP AT S_PRZNR.
      AT FIRST.
        WRITE : / 'Business Process           : '.
      ENDAT.
      WRITE : / '                            ', S_PRZNR-LOW, '~',
    S_PRZNR-HIGH.
    ENDLOOP.
  ENDIF.

  WRITE : / 'Test Run                     ', P_TRUN.
  SKIP 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POST_STD
*&---------------------------------------------------------------------*
*        The Preparation for posting
*----------------------------------------------------------------------*
*      -->P_UCOMM  text
*----------------------------------------------------------------------*
FORM POST_STD USING    P_UCOMM.

  DELETE IT_REPORT WHERE TQTY EQ '0'.

  DATA  : LINE  TYPE I.
  CLEAR : LINE.
  DESCRIBE TABLE IT_REPORT LINES LINE.

  IF LINE = 0.
    MESSAGE E000(ZMCO) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.


* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].

  DATA : LV_CONF_TEXT(50).
* TEXT
  CLEAR LV_CONF_TEXT.
  CONCATENATE SY-UNAME  SY-DATUM  SY-REPID
         INTO LV_CONF_TEXT
         SEPARATED BY '/'.

* Fill Header DATA _ kb21np
  CLEAR WA_DOC_HEADER.
  WA_DOC_HEADER-CO_AREA           = P_KOKRS.
  WA_DOC_HEADER-DOCDATE           = IT_REPORT-BLDAT.
  WA_DOC_HEADER-POSTGDATE         = IT_REPORT-BUDAT.
  WA_DOC_HEADER-VERSION           = GV_P_VERSN.     "P_VERSN.
  WA_DOC_HEADER-VARIANT           = 'SAP08'.
  WA_DOC_HEADER-DOC_HDR_TX        = LV_CONF_TEXT.
  WA_DOC_HEADER-USERNAME          = SY-UNAME.

* Fill Object List
  CLEAR : IT_DOC_ITEMS, IT_DOC_ITEMS[].

  LOOP AT IT_REPORT.

    IT_DOC_ITEMS-SENBUSPROC = IT_REPORT-SENBUSPROC.
    IT_DOC_ITEMS-RECBUSPROC = IT_REPORT-RECBUSPROC.
    IT_DOC_ITEMS-ACTVTY_QTY = IT_REPORT-TQTY.
    IT_DOC_ITEMS-ACTIVITYUN = 'STD'.
    APPEND IT_DOC_ITEMS.
  ENDLOOP.

* Call BAPI FM
  PERFORM CALL_POST_FM.     " KB21NP- BUSINESS PROCESS ALLOCATION

* Commit
  IF P_TRUN = 'X'.
    READ TABLE IT_RETURN  INDEX 1.
    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
  ELSE.

*----    status bar
*    ZTCO_SKF_STATUS-KOKRS = P_KOKRS.
*    ZTCO_SKF_STATUS-GJAHR = P_GJAHR.
*    ZTCO_SKF_STATUS-PERBL = P_PERID.
*    ZTCO_SKF_STATUS-VERSN = P_VERSN.
*    ZTCO_SKF_STATUS-REPID = SY-REPID.
*
*    CALL FUNCTION 'Z_FCO_CHK_STATUS_OF_PROGRESS'
*          EXPORTING
*             IM_ZTCO_SKF_STATUS = ZTCO_SKF_STATUS.
*------
    COMMIT WORK.
    READ TABLE IT_RETURN  INDEX 1.
    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
*    MESSAGE S009(ZMCO) WITH P_UCOMM.

*       Consolidation management for the SKF
        CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
          EXPORTING
            IM_PGMNO         =   SY-TCODE
            IM_KOKRS         =   P_KOKRS
            IM_GJAHR         =   P_GJAHR
            IM_PERBL         =   P_PERID
*           IM_PERBL_T       =
            IM_VERSN         =   P_VERSN
*           IM_KOSTL_F        =
*           IM_KOSTL_T        =
           IM_GNAME          =    P_PRZNR
           IM_PRZNR_F       =   S_PRZNR-LOW
           IM_PRZNR_T       =   S_PRZNR-HIGH.
*         IMPORTING
*           SUBRC            =

  ENDIF.

ENDFORM.                    " POST_STD_CCTR_AT_CE
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM
*&---------------------------------------------------------------------*
*       Call bapi function
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM.

  CALL FUNCTION 'BAPI_ACC_ACTIVITY_ALLOC_POST'
     EXPORTING
       DOC_HEADER            = WA_DOC_HEADER
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
     TABLES
       DOC_ITEMS             = IT_DOC_ITEMS
       RETURN                = IT_RETURN.
*   CRITERIA              =

* Check error
  CLEAR  IT_RETURN.
  LOOP AT IT_RETURN  WHERE TYPE CA 'AE'.
    MESSAGE ID     IT_RETURN-ID
            TYPE   IT_RETURN-TYPE
            NUMBER IT_RETURN-NUMBER
            WITH   IT_RETURN-MESSAGE_V1
                   IT_RETURN-MESSAGE_V2
                   IT_RETURN-MESSAGE_V3
                   IT_RETURN-MESSAGE_V4.
    CLEAR IT_RETURN.
  ENDLOOP.


ENDFORM.                    " CALL_POST_FM
*&---------------------------------------------------------------------*
*&      Form  CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*       check input value
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_INPUT_VALUE.

* Check Input Value (Period)
  IF P_PERID < 0 OR P_PERID > 12.
    MESSAGE E007(ZMCO) WITH P_PERID .
  ENDIF.

* Check Cost Center/Cost Center Group
  IF     S_PRZNR[] IS INITIAL
     AND P_PRZNR   IS INITIAL .
    MESSAGE E000(ZMCO) WITH
    'Please, Check Business process value or BP group.'.
  ELSEIF
         NOT S_PRZNR[] IS INITIAL
     AND NOT P_PRZNR   IS INITIAL .
    MESSAGE E000(ZMCO) WITH
    'Only one value is required (bs value or bs group)'.
  ENDIF.

* Check TEST-RUN  Flag
  IF P_TRUN NA 'X '.
    MESSAGE E008(ZMCO).
  ENDIF.


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*        The preparation for reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_DATA.

  SORT IT_TMP_COSR BY PRZNR GJAHR PERID .

  CLEAR : IT_REPORT, IT_REPORT[].
  LOOP AT IT_TMP_COSR.
** <  Posting , hard coding > **
* doc date / posting date   =  ' YEAR/MONTH/28 '
* receiver.business process =  ' DUMMY-PROC'
    CONCATENATE P_GJAHR P_PERID+1(2) '28'
                INTO IT_REPORT-BLDAT.
    CONCATENATE IT_TMP_COSR-GJAHR IT_TMP_COSR-PERID+1(2) '28'
                INTO IT_REPORT-BUDAT.
    IT_REPORT-SENBUSPROC = IT_TMP_COSR-PRZNR.
    IT_REPORT-RECBUSPROC = 'DUMMY-PROC'.

* Cal : Process_AS003 (total HR) * work day * 8.
    IT_REPORT-TQTY = IT_TMP_COSR-SMEXXX * W_DAY * 8.  " Total qty

    APPEND IT_REPORT.
  ENDLOOP.
  CLEAR IT_REPORT.


ENDFORM.                    " PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT  UNIT
*      -->P_QTY   Quantity
*----------------------------------------------------------------------*
FORM UNIT_CONV USING    P_UNIT
                        P_QTY.

  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
   EXPORTING
     INPUT                      = P_QTY
*    NO_TYPE_CHECK              = 'X'
     ROUND_SIGN                 = 'X'
     UNIT_IN                    = P_UNIT
     UNIT_OUT                   = 'STD'
   IMPORTING
*    ADD_CONST                  =
*    DECIMALS                   =
*    DENOMINATOR                =
*    NUMERATOR                  =
     OUTPUT                     = P_QTY
   EXCEPTIONS
     CONVERSION_NOT_FOUND       = 1
     DIVISION_BY_ZERO           = 2
     INPUT_INVALID              = 3
     OUTPUT_INVALID             = 4
     OVERFLOW                   = 5
     TYPE_INVALID               = 6
     UNITS_MISSING              = 7
     UNIT_IN_NOT_FOUND          = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  P_UNIT = 'STD'.

ENDFORM.                    " UNIT_CONV
*&---------------------------------------------------------------------*
*&      Form  READ_CALENDAR_INFO
*&---------------------------------------------------------------------*
*       Read Factory Calendar  infomation. ( id : HM )
*----------------------------------------------------------------------*
FORM READ_CALENDAR_INFO.

  DATA : BEGIN OF WA_OUT OCCURS 0,
           period LIKE cotplicval-period,
           value(30),
           value2(30),
           value3(30),                   "XZG Felder für Mengen
           value4(30),                   "XZG bei Primärkosten
         END OF WA_OUT.

  CLEAR : WA_OUT, WA_OUT[].
  CALL FUNCTION 'K_ABC_WORKDAYS_FOR_PERIODS_GET'
       EXPORTING
            KOKRS       = P_KOKRS
            GJAHR       = P_GJAHR
            CALID       = 'HM'  "'US'
            PERIOD_FROM = P_PERID
            PERIOD_CNT  = '1'
            CALL_PROG   = 'ZACO28U_SKF9'  "SY-REPID
       TABLES
            RTABLE_VAL  = WA_OUT.

  CLEAR  W_DAY.
  READ TABLE WA_OUT INDEX 1.
  IF SY-SUBRC = 0.
    MOVE WA_OUT-VALUE TO W_DAY.
  ELSE.
    MESSAGE S000(ZMCO) WITH ' Please check , Factory Calendar '.
  ENDIF.

ENDFORM.                    " READ_CALENDAR_INFO
