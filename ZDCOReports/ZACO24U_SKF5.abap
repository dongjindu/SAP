************************************************************************
* Program Name      : ZACO24U_SKF5
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.10.21.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K903252
* Addl Documentation:
* Description       : Create Machine hour for Process Driver


* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date        Developer    RequestNo    Description
* 10/19/2006  Manju        UD1K922643   Display detailed log
*
*
************************************************************************
* ZMM_IF_POPUP_TO_ERROR_MESSAGE

REPORT ZACO24U_SKF5 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.
include:  <icon>.

** Table
TABLES : CSKS , COSL.

** For OBJECT KEY
DATA : BEGIN OF WA_OBJ ,
        OBJNR  LIKE  COSS-OBJNR,
        KOSTL  LIKE  CSKS-KOSTL,
        LSTAR  LIKE  CSLA-LSTAR,
       END OF WA_OBJ.
DATA : IT_OBJ_CCTR_AT   LIKE STANDARD TABLE OF WA_OBJ
                        WITH HEADER LINE .


** Internal Table
DATA : BEGIN OF IT_COSL OCCURS 500.
        INCLUDE STRUCTURE ZSCO_COSL_KEY01.
        INCLUDE STRUCTURE ZSCO_COSL_LST01.
DATA : END OF   IT_COSL.

DATA: BEGIN OF IT_TMP_COSL OCCURS 0,
        GJAHR  LIKE COSS-GJAHR,
        PERID  LIKE COEJL-PERBL,
        OBJNR  LIKE COSS-OBJNR,
        KOSTL  LIKE CSKS-KOSTL,
        LSTAR  LIKE CSLA-LSTAR,
        CURQTY LIKE COSL-LST001,
        UNIT   LIKE COSL-MEINH,
      END OF IT_TMP_COSL.



* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0,
        GJAHR LIKE  COSL-GJAHR,
        PERID LIKE  COEJL-PERBL,
        KOSTL LIKE  CSKS-KOSTL,
        LSTAR LIKE  CSLA-LSTAR,

        BLDAT   LIKE COHEADER-BLDAT,          " doc date
        BUDAT  LIKE COHEADER-BUDAT,          " Posting date
        EPRZNR LIKE RK40C_KBXXN-EPRZNR,      " Receiver business process
        STAGR  LIKE RK40C_KBXXN-STAGR,       " SKF
        CURQTY LIKE COSL-LST001,             " Total qty
        UNIT   LIKE COSL-MEINH,

        SENBUSPROC LIKE BAPIAAITM-SENBUSPROC, "Sender business process
        RECBUSPROC LIKE BAPIAAITM-RECBUSPROC, "Receiver business process
        chkbox  type c,
        icon  type icon_d,
      END OF IT_REPORT.

* For DD data
DATA : GV_CI_TABNAME     TYPE DDOBJNAME .
DATA : IT_ET_FIELDLIST   LIKE TABLE OF RFVICP_DDIC_TABL_FIELDNAME
                         WITH HEADER LINE.

* for posting
DATA : BEGIN OF IT_POST OCCURS 0,
        GJAHR   LIKE  COSL-GJAHR,
        R_KOSTL LIKE  CSSL-KOSTL,
        R_LSTAR LIKE  CSSL-LSTAR,
        KOSTL   LIKE  CSKS-KOSTL,
        LSTAR   LIKE  CSLA-LSTAR.
        INCLUDE STRUCTURE ZSCO_COSP_AMT02.
DATA : END OF  IT_POST.

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
DATA : IT_COSTCENTERLIST LIKE STANDARD TABLE OF BAPI0012_CCLIST
                         WITH HEADER LINE.
DATA : IT_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : IT2_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.
DATA : IT3_RETURN         LIKE STANDARD TABLE OF BAPIRET2
                         WITH HEADER LINE.

*  (kb31np)
DATA : WA_DOC_HEADER LIKE BAPIDOCHDRP .
DATA : IT_DOC_ITEMS  LIKE STANDARD TABLE OF BAPISKFITM
                     WITH HEADER LINE.
*  (kb21np)
DATA : WA2_DOC_HEADER LIKE BAPIDOCHDRP .
DATA : IT2_DOC_ITEMS  LIKE STANDARD TABLE OF BAPIAAITM
                     WITH HEADER LINE.

DATA:  BEGIN OF IT_BELNR OCCURS 0,
         BELNR  LIKE COBK-BELNR,
       END OF   IT_BELNR.

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
*             P_CURRT LIKE PLNHDR-PLNCT DEFAULT 'C' OBLIGATORY,
             P_TRUN(1).
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL4 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_LSTAR LIKE CSLA-LSTAR            DEFAULT 'MCH_HR'
                                                         OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK BL4.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_KOSTL  FOR CSKS-KOSTL DEFAULT 'MXBXB1'.
PARAMETERS:      P_NCOAL LIKE GRPDYNP-NAME_COALL.
*                                     DEFAULT 'SEMIDIRECT'.
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

* Searching for Cost Center group
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_NCOAL.
  PERFORM READ_CEGRP_GROUP USING '0101'
                                 P_NCOAL.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Read Cost Center Group - > Cost Center
  PERFORM READ_CEGRP.
* Read OBJ Key Combination
  PERFORM SET_OBJ_KEY.
* Read Dynamic Fields Name
  PERFORM READ_FIELD_NAME_FROM_DD_COSL.
* Read Sender_Quantity Information from COSL
  PERFORM READ_COSL_QTY.
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
*       read cost center
*----------------------------------------------------------------------*
FORM READ_CEGRP.

* Making an internal table for CCtr to select data
* Selected Group on screen
  CLEAR : IT_COSTCENTERLIST, IT_COSTCENTERLIST[].
  CLEAR : IT_RETURN, IT_RETURN[].

* Set Validity Date (Start)
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO LV_DATUM.

* From CCtr Group
  IF NOT P_NCOAL IS INITIAL.
    CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
         EXPORTING
              CONTROLLINGAREA = P_KOKRS
              DATE_FROM       = LV_DATUM
              COSTCENTERGROUP = P_NCOAL
         TABLES
              COSTCENTERLIST  = IT_COSTCENTERLIST
              RETURN          = IT_RETURN.

* Message
    PERFORM DIS_BAPI_MESSAGE.
  ENDIF.

* From CCtrs on selection screen
* From Select-options.
  DATA : IT_L_CCTR LIKE STANDARD TABLE OF IT_COSTCENTERLIST
                 WITH HEADER LINE.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      CLEAR : IT_L_CCTR, IT_L_CCTR[].
      CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
           EXPORTING
                CONTROLLINGAREA = P_KOKRS
                DATE_FROM       = LV_DATUM
                COSTCENTER_FROM = S_KOSTL-LOW
                COSTCENTER_TO   = S_KOSTL-HIGH
           TABLES
                COSTCENTERLIST  = IT_L_CCTR
                RETURN          = IT_RETURN.
* Message
      PERFORM DIS_BAPI_MESSAGE.
* Appending CE list
      APPEND LINES OF IT_L_CCTR  TO IT_COSTCENTERLIST.
      CLEAR IT_COSTCENTERLIST.
      CLEAR S_KOSTL.
    ENDLOOP.
  ENDIF.

* Sorting
  SORT IT_COSTCENTERLIST BY CO_AREA COSTCENTER.
  DELETE ADJACENT DUPLICATES FROM IT_COSTCENTERLIST.

* Check CCtr
  IF IT_COSTCENTERLIST[] IS INITIAL .
    MESSAGE E018(ZMCO) WITH 'Cost Center'.
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
*&      Form  SET_OBJ_KEY
*&---------------------------------------------------------------------*
*       Read OBJ Key Combination
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_OBJ_KEY.

  CLEAR : IT_OBJ_CCTR_AT, IT_OBJ_CCTR_AT[].

  LOOP AT IT_COSTCENTERLIST.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              KOKRS = P_KOKRS
              KOSTL = IT_COSTCENTERLIST-COSTCENTER
              LSTAR = P_LSTAR
         IMPORTING
              OBJNR = IT_OBJ_CCTR_AT-OBJNR.
    IT_OBJ_CCTR_AT-KOSTL = IT_COSTCENTERLIST-COSTCENTER.
    IT_OBJ_CCTR_AT-LSTAR = P_LSTAR.
    APPEND IT_OBJ_CCTR_AT. CLEAR IT_OBJ_CCTR_AT.
    CLEAR IT_COSTCENTERLIST.
  ENDLOOP.
  CLEAR : IT_OBJ_CCTR_AT.


ENDFORM.                    " SET_OBJ_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_COSL_QTY
*&---------------------------------------------------------------------*
*       Read Quantity data from COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_COSL_QTY.

  CLEAR : IT_COSL, IT_COSL[].
  CLEAR COSL.
  SELECT (IT_ET_FIELDLIST)
         INTO CORRESPONDING FIELDS OF TABLE IT_COSL
         FROM COSL
          FOR ALL ENTRIES IN IT_OBJ_CCTR_AT
        WHERE LEDNR = '00'
          AND OBJNR = IT_OBJ_CCTR_AT-OBJNR
          AND GJAHR = P_GJAHR
          AND WRTTP = '04'             " Actual
          AND VERSN = P_VERSN.
  CLEAR : IT_COSL.


* Local Data definition
  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_LST_NAM(30).

  CLEAR   IT_COSL.
  CLEAR : IT_TMP_COSL, IT_TMP_COSL[].

  LOOP AT IT_COSL.
* Key Part
    IT_TMP_COSL-GJAHR = P_GJAHR.
    IT_TMP_COSL-PERID = P_PERID.

    CLEAR IT_OBJ_CCTR_AT.
    READ TABLE    IT_OBJ_CCTR_AT
         WITH KEY OBJNR = IT_COSL-OBJNR.
    IT_TMP_COSL-KOSTL = IT_OBJ_CCTR_AT-KOSTL.
    IT_TMP_COSL-LSTAR = IT_OBJ_CCTR_AT-LSTAR.
    IT_TMP_COSL-OBJNR = IT_OBJ_CCTR_AT-OBJNR.

    IT_TMP_COSL-UNIT = IT_COSL-MEINH.     " unit

* Value Transferring
    CLEAR LV_LST_NAM.
    CONCATENATE 'IT_COSL-'  'LST'  P_PERID
           INTO LV_LST_NAM.
    ASSIGN (LV_LST_NAM) TO <FS1>.
    CLEAR IT_TMP_COSL-CURQTY.
    IT_TMP_COSL-CURQTY = <FS1>.
* Collect
    COLLECT IT_TMP_COSL.

    CLEAR IT_TMP_COSL.
    CLEAR IT_COSL.
  ENDLOOP.
  CLEAR IT_TMP_COSL.


ENDFORM.                    " READ_COSL_QTY
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
  SORT IT_REPORT BY GJAHR PERID KOSTL LSTAR.
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
    'IT_REPORT' 'KOSTL'  'X'            SPACE    SPACE
    SPACE        '10'      'Sender'           SPACE    SPACE    SPACE
.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'LSTAR'  'X'            SPACE    SPACE
    SPACE        '6'      'Sen.AT'           SPACE    SPACE    SPACE
.



  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'BUDAT'  'X'            SPACE    SPACE
   SPACE '10'      'Post Date'    SPACE    SPACE SPACE
   .

** Value
  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'EPRZNR'  SPACE            SPACE    SPACE
   SPACE '12'      'Rec.Process'    SPACE    SPACE SPACE
   .

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'STAGR'  SPACE            SPACE    SPACE
   SPACE '6'      'SKF'    SPACE    SPACE SPACE .

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'CURQTY'  SPACE            SPACE    SPACE
   SPACE '15'      'Total Qty'    SPACE    SPACE SPACE .

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE .

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'SENBUSPROC'  SPACE            SPACE    SPACE
   SPACE '12'      'Send.Process'    SPACE    SPACE SPACE .

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'RECBUSPROC'  SPACE            SPACE    SPACE
   SPACE '12'      'Rec. Process'    SPACE    SPACE SPACE .

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'CURQTY'  SPACE            SPACE    SPACE
   SPACE '15'      'Total Qty'    SPACE    SPACE SPACE .

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'UNIT'  SPACE            SPACE    SPACE
    SPACE   '4'      'Unit'      'UNIT'   SPACE     SPACE .

  PERFORM BUILD_FIELDCAT USING                              "UD1K92264
    'IT_REPORT' 'ICON'  SPACE            SPACE    SPACE
    SPACE   '4'      'STS'      'STS'   SPACE     SPACE .


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
*  WA_FIELDCAT-checkbox    = P_0111.
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

  data : CS_layo type SLIS_LAYOUT_ALV.
  cs_layo-box_fieldname          = 'CHKBOX'.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
*     I_INTERFACE_CHECK              = ' '
*     I_BYPASSING_BUFFER             =
*     I_BUFFER_ACTIVE                = ' '
       I_CALLBACK_PROGRAM             = GV_REPID
       I_CALLBACK_PF_STATUS_SET       = GV_STATUS
       I_CALLBACK_USER_COMMAND        = GV_USER_COMMAND
*     I_STRUCTURE_NAME               =
      IS_LAYOUT                      = cs_layo
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
 if SY-UCOMM eq '&F03'.
   sy-lsind = 0.
*   LEAVE PROGRAM.
   leave to screen 0.
   exit.
 endif.
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
    WHEN 'LOG'.
      PERFORM DIS_LOG_FOR_POST_DOC.

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
  WRITE : / 'Business Process PostVersion:'
            , GV_P_VERSN.

* CCTR
  IF P_NCOAL NE SPACE.
    WRITE : / 'Cost Center Grp.           : ', P_NCOAL.
  ENDIF.

  IF NOT S_KOSTL[] IS INITIAL.
    LOOP AT S_KOSTL.
      AT FIRST.
        WRITE : / 'Cost Center                : '.
      ENDAT.
      WRITE : / '                            ', S_KOSTL-LOW, '~',
    S_KOSTL-HIGH.
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

  DELETE IT_REPORT WHERE CURQTY EQ '0'.

  DATA  : LINE  TYPE I.
  CLEAR : LINE.
  DESCRIBE TABLE IT_REPORT   LINES LINE.

  IF LINE = 0.
    MESSAGE E000(ZMCO) WITH
    ' Enter Value not equal to 0 '.
  ENDIF.


* Init. Message TAB
  CLEAR : IT_RETURN, IT_RETURN[].
  CLEAR : IT2_RETURN, IT2_RETURN[].

  DATA : LV_CONF_TEXT(50).
* TEXT
  CLEAR LV_CONF_TEXT.
  CONCATENATE SY-UNAME  SY-DATUM  SY-REPID
         INTO LV_CONF_TEXT
         SEPARATED BY '/'.

* Fill Header DATA _ kn31np
  CLEAR WA_DOC_HEADER.
  WA_DOC_HEADER-CO_AREA           = P_KOKRS.
  WA_DOC_HEADER-DOCDATE           = IT_REPORT-BLDAT.
  WA_DOC_HEADER-POSTGDATE         = IT_REPORT-BUDAT.
  WA_DOC_HEADER-VERSION           = P_VERSN.
  WA_DOC_HEADER-VARIANT           = 'SAP06'.
  WA_DOC_HEADER-DOC_HDR_TX        = LV_CONF_TEXT.
  WA_DOC_HEADER-USERNAME          = SY-UNAME.

* Fill Header DATA _ kb21np
  CLEAR WA2_DOC_HEADER.
  WA2_DOC_HEADER-CO_AREA           = P_KOKRS.
  WA2_DOC_HEADER-DOCDATE           = IT_REPORT-BLDAT.
  WA2_DOC_HEADER-POSTGDATE         = IT_REPORT-BUDAT.
  WA2_DOC_HEADER-VERSION           = GV_P_VERSN.    " P_VERSN.
  WA2_DOC_HEADER-VARIANT           = 'SAP08'.
  WA2_DOC_HEADER-DOC_HDR_TX        = LV_CONF_TEXT.
  WA2_DOC_HEADER-USERNAME          = SY-UNAME.

* Fill Object List
  CLEAR : IT_DOC_ITEMS, IT_DOC_ITEMS[].
  CLEAR : IT2_DOC_ITEMS, IT2_DOC_ITEMS[].

  LOOP AT IT_REPORT where CHKBOX eq 'X'.                    "UD1K922643

    IT_DOC_ITEMS-STATKEYFIG = 'AS007'.   "IT_REPORT-STAGR.
    IT_DOC_ITEMS-STAT_QTY   = IT_REPORT-CURQTY.
    IT_DOC_ITEMS-RECBUSPROC = IT_REPORT-EPRZNR.
    APPEND IT_DOC_ITEMS.

    IT2_DOC_ITEMS-SENBUSPROC = IT_REPORT-EPRZNR.
    IT2_DOC_ITEMS-RECBUSPROC = 'DUMMY-PROC'.
    IT2_DOC_ITEMS-ACTVTY_QTY = IT_REPORT-CURQTY.
    IT2_DOC_ITEMS-ACTIVITYUN = 'STD'.
    APPEND IT2_DOC_ITEMS.

  ENDLOOP.

* Call BAPI FM
  PERFORM CALL_POST_FM.     " KB31NP- SKF
  PERFORM CALL_POST_FM_2.   " KB21NP- BUSINESS PROCESS ALLOCATION


* Commit
  IF P_TRUN = 'X'.
*   READ TABLE IT_RETURN  INDEX 1.
*   MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
  ELSE.
    COMMIT WORK.
*    READ TABLE IT_RETURN  INDEX 1.
*    MESSAGE S000(ZMCO) WITH IT_RETURN-MESSAGE.
    MESSAGE S009(ZMCO) WITH P_UCOMM.


*       Consolidation management for the SKF
    CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
      EXPORTING
        IM_PGMNO         =   SY-TCODE
        IM_KOKRS         =   P_KOKRS
        IM_GJAHR         =   P_GJAHR
        IM_PERBL         =   P_PERID
*           IM_PERBL_T       =
        IM_VERSN         =   P_VERSN
       IM_KOSTL_F        =    S_KOSTL-LOW
       IM_KOSTL_T        =    S_KOSTL-HIGH
       IM_GNAME          =    P_NCOAL.
*           IM_PRZNR_F       =
*           IM_PRZNR_T       =
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

  CALL FUNCTION 'BAPI_ACC_STAT_KEY_FIG_POST'
    EXPORTING
      DOC_HEADER            = WA_DOC_HEADER
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
    TABLES
      DOC_ITEMS             = IT_DOC_ITEMS
      RETURN                = IT_RETURN.


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
  IF     S_KOSTL[] IS INITIAL
     AND P_NCOAL   IS INITIAL .
    MESSAGE E016(ZMCO).
  ELSEIF
         NOT S_KOSTL[] IS INITIAL
     AND NOT P_NCOAL   IS INITIAL .
    MESSAGE E017(ZMCO).
  ENDIF.

* Check TEST-RUN  Flag
  IF P_TRUN NA 'X '.
    MESSAGE E008(ZMCO).
  ENDIF.

* Check Activity type
  IF P_LSTAR NE 'MCH_HR'.
    MESSAGE E018(ZMCO) WITH P_LSTAR.
  ENDIF.

** Check Currency IND.
*  IF P_CURRT NA 'CTO'.
*    MESSAGE E000(ZMCO) WITH P_CURRT ' is not a posible value' .
*  ENDIF.

ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_NAME_FROM_DD_COSL.

  CLEAR : IT_ET_FIELDLIST, IT_ET_FIELDLIST[].

* read DD infor. COSL Key Part
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSL_KEY01'.

* read DD infor. COSL Value Part (Total Quantity)
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSL_LST01'.

ENDFORM.                    " READ_FIELD_NAME_FROM_DD_COSL
*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*        Read DD information
*----------------------------------------------------------------------*
*     -->IT_l_ET_FIELDLIST  Field-List Table
*      -->P_CI_TABNAME       DD name
*----------------------------------------------------------------------*
FORM READ_DD_INFO TABLES   IT_L_ET_FIELDLIST STRUCTURE IT_ET_FIELDLIST
                  USING    P_CI_TABNAME      LIKE GV_CI_TABNAME.
* Local DATA definition
  DATA : IT_L_FDLIST LIKE STANDARD TABLE OF IT_ET_FIELDLIST
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : IT_L_FDLIST,     IT_L_FDLIST[].
  CLEAR GV_CI_TABNAME.
  GV_CI_TABNAME = P_CI_TABNAME.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            IC_TABNAME   = GV_CI_TABNAME
       TABLES
            ET_FIELDLIST = IT_L_FDLIST
       EXCEPTIONS
            NOT_FOUND    = 1
            OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  APPEND LINES OF  IT_L_FDLIST       TO IT_L_ET_FIELDLIST.

ENDFORM.                    " READ_DD_INFO
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*        The preparation for reporting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_DATA.
** <  Posting , hard coding > **
* doc date / posting date   =  ' YEAR/MONTH/28 '
* SKF     =  'AS007'
* receiver.business process =  ' DUMMY-PROC'

  CLEAR : IT_REPORT, IT_REPORT[].
  CLEAR IT_TMP_COSL.
  LOOP AT IT_TMP_COSL.
*   Unit Conversion
    IF IT_TMP_COSL-UNIT <> 'STD'.
      PERFORM UNIT_CONV USING IT_TMP_COSL-UNIT
                              IT_TMP_COSL-CURQTY.
    ENDIF.
    MOVE-CORRESPONDING IT_TMP_COSL TO IT_REPORT.
    CONCATENATE IT_TMP_COSL-GJAHR IT_TMP_COSL-PERID+1(2) '28'
                INTO IT_REPORT-BLDAT.
    CONCATENATE IT_TMP_COSL-GJAHR IT_TMP_COSL-PERID+1(2) '28'
                INTO IT_REPORT-BUDAT.
    CONCATENATE 'P4201-' IT_TMP_COSL-KOSTL INTO IT_REPORT-EPRZNR.
    IT_REPORT-STAGR = 'AS007'.

    CONCATENATE 'P4201-' IT_TMP_COSL-KOSTL INTO IT_REPORT-SENBUSPROC.
    IT_REPORT-RECBUSPROC = 'DUMMY-PROC'.

    APPEND IT_REPORT.
  ENDLOOP.

ENDFORM.                    " PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_POST_FM_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_POST_FM_2.
  DATA: it_xmsg   LIKE TABLE OF ztismessage WITH HEADER LINE,
        flag .

  CALL FUNCTION 'BAPI_ACC_ACTIVITY_ALLOC_POST'
    EXPORTING
      DOC_HEADER            = WA2_DOC_HEADER
*   IGNORE_WARNINGS       = ' '
* IMPORTING
*   DOC_NO                =
    TABLES
      DOC_ITEMS             = IT2_DOC_ITEMS
      RETURN                = IT2_RETURN.
*   CRITERIA              =

  LOOP AT IT2_RETURN where type eq 'S'.
    IT_REPORT-iCON = icon_green_light.
    modify IT_REPORT transporting ICON where chkbox = 'X'.
  endloop.

* Check error
  CLEAR:IT2_RETURN,flag. clear it_XMSG[].
  LOOP AT IT2_RETURN  WHERE TYPE CA 'WAE'.
* Begin of changes - UD1K922643
*    MESSAGE ID     IT2_RETURN-ID
*            TYPE   IT2_RETURN-TYPE
*            NUMBER IT2_RETURN-NUMBER
*            WITH   IT2_RETURN-MESSAGE_V1
*                   IT2_RETURN-MESSAGE_V2
*                   IT2_RETURN-MESSAGE_V3
*                   IT2_RETURN-MESSAGE_V4.
*    CLEAR IT2_RETURN.

    move-corresponding IT2_RETURN to It_XMSG.
    It_XMSG-MSGID = IT2_return-ID.
    It_XMSG-MSGTY = IT2_return-TYPE.
    It_XMSG-MSGNO = IT2_return-NUMBER.
    It_XMSG-MSGV1 = IT2_return-MESSAGE_V1.
    It_XMSG-MSGV2 = IT2_return-MESSAGE_V2.
    It_XMSG-MSGV3 = IT2_return-MESSAGE_V3.
    It_XMSG-MSGV4 = IT2_return-MESSAGE_V4.
    It_XMSG-MSGTX = IT2_return-MESSAGE.
    append It_XMSG.
    if It_XMSG-MSGTY = 'E' OR
       It_XMSG-MSGTY = 'W' .
      flag = 'X'.
    endif.
  ENDLOOP.

  if FLAG = 'X' .
    CALL FUNCTION 'ZMM_IF_POPUP_TO_ERROR_MESSAGE'
     EXPORTING
*     XLOGNO            =
       XDOCNO_SHOW       = 'X'
     TABLES
       XMSG              =  It_XMSG.
  endif.

*  if flag eq 'X'.
  LOOP AT IT2_RETURN  WHERE TYPE CA 'AE'.
    MESSAGE ID     IT2_RETURN-ID
            TYPE   IT2_RETURN-TYPE
            NUMBER IT2_RETURN-NUMBER
            WITH   IT2_RETURN-MESSAGE_V1
                   IT2_RETURN-MESSAGE_V2
                   IT2_RETURN-MESSAGE_V3
                   IT2_RETURN-MESSAGE_V4.
    CLEAR IT2_RETURN.
  endloop.

*  endif.
   sy-lsind = 0.
  if sy-ucomm eq 'POST'.
    LOOP AT IT_REPORT where not ICON is initial.
      IT_REPORT-chkbox = ''.
      modify IT_REPORT transporting chkbox where not ICON is initial.
      exit.
    endloop.
    PERFORM CALL_ALV_LIST.
  endif.
* END of changes - UD1K922643


ENDFORM.                    " CALL_POST_FM_2
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
*&      Form  DIS_LOG_FOR_POST_DOC
*&---------------------------------------------------------------------*
*        Display posted documents
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_LOG_FOR_POST_DOC.

  CLEAR :IT3_RETURN[], IT3_RETURN.

  LOOP AT IT_RETURN WHERE TYPE  = 'S'
                        AND ID = 'BK'
                        AND NUMBER = '003'.
    MOVE-CORRESPONDING IT_RETURN TO IT3_RETURN.
    APPEND IT3_RETURN.
    CLEAR IT3_RETURN.
    CLEAR IT_RETURN.
  ENDLOOP.

  LOOP AT IT2_RETURN WHERE TYPE  = 'S'
                        AND ID = 'BK'
                        AND NUMBER = '003'.
    MOVE-CORRESPONDING IT2_RETURN TO IT3_RETURN.
    APPEND IT3_RETURN.
    CLEAR IT3_RETURN.
    CLEAR IT2_RETURN.
  ENDLOOP.


* Saving Document numbers which were posted successfully.
  CLEAR : IT_BELNR, IT_BELNR[].
  LOOP AT IT3_RETURN WHERE TYPE  = 'S'
                       AND ID = 'BK'
                       AND NUMBER = '003'.
    IT_BELNR-BELNR = IT3_RETURN-MESSAGE_V1.

    APPEND IT_BELNR.
    CLEAR  IT_BELNR.
    CLEAR IT3_RETURN.
  ENDLOOP.

* Check if at least one document was created.
  IF IT_BELNR[] IS INITIAL .
    MESSAGE E025.
  ENDIF.

* Read Informations from Document Header
  DATA : IT_L_COBK LIKE STANDARD TABLE OF COBK
                   WITH HEADER LINE .
  CLEAR : IT_L_COBK, IT_L_COBK[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_L_COBK
           FROM COBK
           FOR ALL ENTRIES IN IT_BELNR
          WHERE KOKRS = P_KOKRS
            AND BELNR = IT_BELNR-BELNR.

  IF IT_L_COBK[] IS INITIAL .
    MESSAGE E025.
  ENDIF.

  SORT IT_L_COBK BY BELNR.

  CALL FUNCTION 'STC1_POPUP_WITH_TABLE_CONTROL'
    EXPORTING
      HEADER                  = 'CO Document Information'
      TABNAME                 = 'COBK'
      DISPLAY_ONLY            = 'X'
      NO_BUTTON               = SPACE
*     X_START                 = 5
*     Y_START                 = 5
*     X_END                   = 80
*     Y_END                   = 25
    TABLES
      TABLE                   = IT_L_COBK
    EXCEPTIONS
      NO_MORE_TABLES          = 1
      TOO_MANY_FIELDS         = 2
      NAMETAB_NOT_VALID       = 3
      HANDLE_NOT_VALID        = 4
      OTHERS                  = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " DIS_LOG_FOR_POST_DOC
