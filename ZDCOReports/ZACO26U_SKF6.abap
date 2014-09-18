************************************************************************
* Program Name      : ZACO26U_SKF6
* Author            : Eun Hwa , Jung
* Creation Date     : 2003.11.05.
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No :  UD1K903690
* Addl Documentation:
* Description       : Create Process Ratio by Cost Center
*                      - > ZTCO_CC_PRO_QTY table create

*
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT ZACO26U_SKF6 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

** Table
TABLES : CSKS ,CBPR, ZTCO_CC_PRO_QTY .

** Internal Table

DATA: BEGIN OF IT_CBPR OCCURS 0,
*        KOKRS TYPE CBPR-KOKRS,
        PRZNR TYPE CBPR-PRZNR,
        OBJNR TYPE CBPR-OBJNR,
        KOSTL LIKE CBPR-KOSTL,
      END OF IT_CBPR.

* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0.
        INCLUDE STRUCTURE ZTCO_CC_PRO_QTY.
DATA : END OF IT_REPORT.


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

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
             P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
             P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS : S_KOSTL  FOR CSKS-KOSTL. " DEFAULT 'MXBXB1'.
PARAMETERS:      P_NCOAL LIKE GRPDYNP-NAME_COALL
                                       DEFAULT 'HMMA'.
SELECTION-SCREEN END OF BLOCK BL3.
SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.


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
* Read 'Business Process'  From CBPR table
  PERFORM READ_CBPR.
* Preparation of reporting  ( C/C + Business Process + SKF )
  PERFORM PRE_REPORTING.
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
*&      Form  READ_CBPR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CBPR.

* Set Validity Date (Start)
  DATA : LV_DATUM LIKE SY-DATUM.
  CLEAR LV_DATUM.
  CONCATENATE P_GJAHR P_PERID+1(2) '01' INTO LV_DATUM.

  CLEAR : IT_CBPR, IT_CBPR[].
  CLEAR CBPR.
  SELECT PRZNR OBJNR KOSTL
         INTO CORRESPONDING FIELDS OF TABLE IT_CBPR
         FROM CBPR
          FOR ALL ENTRIES IN IT_COSTCENTERLIST
           WHERE KOKRS = P_KOKRS
             AND DATAB <= LV_DATUM       "Validity Date (Start)
             AND KOSTL = IT_COSTCENTERLIST-COSTCENTER
             AND VERSION = P_VERSN.
  CLEAR  IT_CBPR.

  SORT IT_CBPR BY OBJNR.

ENDFORM.                    " READ_CBPR
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORTING.

  CLEAR : IT_REPORT, IT_REPORT[].
  LOOP AT IT_CBPR.

    MOVE-CORRESPONDING IT_CBPR TO IT_REPORT.
    IT_REPORT-GJAHR = P_GJAHR.
    IT_REPORT-ERDAT = SY-DATUM.
    IT_REPORT-ERZET = SY-UZEIT.
    IT_REPORT-ERNAM = SY-UNAME.
    APPEND IT_REPORT.
  ENDLOOP.
  CLEAR IT_REPORT.


ENDFORM.                    " READ_QTY

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
  SORT IT_REPORT BY GJAHR KOSTL PRZNR .
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
  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'GJAHR'  'X'            SPACE    SPACE
    SPACE        '4'      'Year'           SPACE    SPACE    SPACE.
*
*  PERFORM BUILD_FIELDCAT USING
*    'IT_REPORT' 'PERID'  'X'            SPACE    SPACE
*    SPACE        '6'      'Period'           SPACE    SPACE    SPACE.
*
  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'KOSTL'  'X'            SPACE    SPACE
    SPACE        '10'      'CostCenter'       SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'PRZNR'  'X'            SPACE    SPACE
    SPACE        '12'      'Biz Process'      SPACE    SPACE    SPACE.


** Value

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'T_QTY'  SPACE            SPACE    SPACE
    SPACE '15'      'Quantity'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'MEINH'  SPACE            SPACE    SPACE
     SPACE   '4'      'Unit'      'MEINH'   SPACE     SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'ERDAT'  SPACE            SPACE    SPACE
    SPACE '15'      'Date'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'ERZET'  SPACE            SPACE    SPACE
    SPACE '15'      'Time'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'ERNAM'  SPACE            SPACE    SPACE
    SPACE '15'      'Name'    SPACE    SPACE SPACE.


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
* update part !
    WHEN 'NEW'.
      PERFORM INSERT_TABLE.
    WHEN 'CHAN'.
      CALL TRANSACTION 'ZCOA34'.
  ENDCASE.
  CLEAR UCOMM.

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
  SKIP 1.

ENDFORM.
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


ENDFORM.                    " CHK_INPUT_VALUE
*&---------------------------------------------------------------------*
*&      Form  INSERT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_TABLE.

  PERFORM ENQUEUE_ZTCO_CCPP_QYT.

  LOOP AT IT_REPORT.
    MOVE-CORRESPONDING IT_REPORT TO ZTCO_CC_PRO_QTY.
* Do not check sy-subrc.
* If a failure occurs because of already existed data, just leave it.
* The old data should be reserved (Requested by FM)
    INSERT ZTCO_CC_PRO_QTY.
    CLEAR ZTCO_CC_PRO_QTY.

*       Consolidation management for the SKF
        CALL FUNCTION 'Z_FCO_MANAGEMENT_SKF'
          EXPORTING
            IM_PGMNO         =  'ZCOA37'            "SY-TCODE
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

  ENDLOOP.

    MESSAGE S000 WITH 'Successfully update'.

  PERFORM DEQUEUE_ZTCO_CCPP_QYT.

ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_CCPP_QYT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE_ZTCO_CCPP_QYT.

CALL FUNCTION 'ENQUEUE_EZ_ZTCO_CCPP_QYT'
 EXPORTING
   MODE_ZTCO_CC_PRO_QTY       = 'X'
   MANDT                      = SY-MANDT
   GJAHR                      = P_GJAHR
*   KOSTL                      =
*   PRZNR                      =
*   X_GJAHR                    = ' '
*   X_KOSTL                    = ' '
*   X_PRZNR                    = ' '
*   _SCOPE                     = '2'
*   _WAIT                      = ' '
*   _COLLECT                   = ' '
 EXCEPTIONS
   FOREIGN_LOCK               = 1
   SYSTEM_FAILURE             = 2
   OTHERS                     = 3
          .
IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

ENDFORM.                    " ENQUEUE_ZTCO_CC_PRO_QTY
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_ZTCO_CCPP_QYT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEQUEUE_ZTCO_CCPP_QYT.

CALL FUNCTION 'DEQUEUE_EZ_ZTCO_CCPP_QYT'
 EXPORTING
   MODE_ZTCO_CC_PRO_QTY       = 'X'
   MANDT                      = SY-MANDT
   GJAHR                      = P_GJAHR.
*   KOSTL                      =
*   PRZNR                      =
*   X_GJAHR                    = ' '
*   X_KOSTL                    = ' '
*   X_PRZNR                    = ' '
*   _SCOPE                     = '3'
*   _SYNCHRON                  = ' '
*   _COLLECT                   = ' '
          .

ENDFORM.                    " DEQUEUE_ZTCO_CCPP_QYT
