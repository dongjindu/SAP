************************************************************************
* Program Name      : ZACO54U_SKF17
* Author            : Eun Hwa , Jung
* Creation Date     : 2004.03.12
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K907742
* Addl Documentation:
* Description       : Consolidation management for the SKF


* the BDC structures for BATCH INPUT processing
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT ZACO54U_SKF17 MESSAGE-ID ZMCO.

** type-pools
TYPE-POOLS: SLIS.

** Table
TABLES : ZTCO_SKF_MANAGE.

DATA: BEGIN OF IT_DATA OCCURS 0,
        ZSORT(1),
        GUBUN(10),
        PGMNO(20),
        DESCR(80),
      END OF IT_DATA.

DATA : BEGIN OF IT_MANAGE OCCURS 0.
        INCLUDE STRUCTURE ZTCO_SKF_MANAGE .
DATA : END OF IT_MANAGE.

* for reporting
DATA : BEGIN OF IT_REPORT OCCURS 0,
*        GJAHR LIKE  COSL-GJAHR,
*        PERID LIKE  COEJL-PERBL,
        ZSORT(1),
        GUBUN(10),
        PGMNO(20),
        DESCR(80),
        STATUS(2),
        REMARK(20),
        DATE LIKE SY-DATUM,
        USER LIKE SY-UNAME,
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

DATA : GV_PERCOUNT       LIKE COSP-PERBL. "Period Counter
RANGES : RS_PERIOD FOR COSP-PERBL.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS :
             P_KOKRS LIKE CSKS-KOKRS MEMORY ID CAC OBLIGATORY,
             P_GJAHR LIKE COBK-GJAHR MEMORY ID GJR OBLIGATORY,
*             P_PERID LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_FRPER LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_TOPER LIKE COSP-PERBL MEMORY ID BPE OBLIGATORY,
             P_VERSN LIKE COBK-VERSN MEMORY ID KVT OBLIGATORY.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Global ALV Parameter
  GV_REPID = SY-REPID.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Calculating Period Count
  PERFORM CAL_PER_COUNT.
* Check input value
  PERFORM CHK_INPUT_VALUE.
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
  SORT IT_REPORT BY ZSORT.

  CLEAR IT_REPORT.

*  IT_SORT-FIELDNAME = 'GUBUN'.
*  IT_SORT-UP        = 'X'.
*  IT_SORT-EXPA      = 'X'.
*  IT_SORT-SUBTOT    = 'X'.
*  APPEND IT_SORT.

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
    'IT_REPORT' 'GUBUN'  'X'            SPACE    SPACE
    SPACE        '10'      'Section'   SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_REPORT' 'PGMNO'  'X'            SPACE    SPACE
    SPACE        '6'      'T-code'    SPACE    SPACE    SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'DESCR'  SPACE            SPACE    SPACE
   SPACE '65'      'Description'    SPACE    SPACE SPACE.


** Value
  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'STATUS'  SPACE            SPACE    SPACE
   SPACE '6'      'Status'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'REMARK'  SPACE            SPACE    SPACE
   SPACE '15'      'Remark'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_REPORT' 'DATE'  SPACE            SPACE    SPACE
   SPACE '10'      'Exe_Date'    SPACE    SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
  'IT_REPORT' 'USER'  SPACE            SPACE    SPACE
  SPACE '10'      'Exe_user'    SPACE    SPACE SPACE.



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
       IT_SORT                        = IT_SORT[]
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
                        I_SELFIELD TYPE SLIS_SELFIELD.

  DATA I_REC LIKE IT_REPORT.
  READ TABLE IT_REPORT INDEX I_SELFIELD-TABINDEX INTO I_REC.

  CASE UCOMM.
    WHEN 'POST'.
      CASE I_REC-PGMNO.
        WHEN ' '.
          MESSAGE S000(ZMCO) WITH 'Please select data'.
        WHEN 'ZCOA30'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA30'.
        WHEN 'ZCOA41'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA41'.
        WHEN 'ZCOA47'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA47'.
        WHEN 'ZCOA32'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA32'.
*        WHEN 'ZCOA38'.
*          IF I_REC-STATUS = 'O'.
*            MESSAGE I000(ZMCO) WITH 'Already Executed'.
*          ENDIF.
*          CALL TRANSACTION 'ZCOA38'.
        WHEN 'ZCOA33'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA33'.
        WHEN 'ZCOA35'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA35'.
        WHEN 'ZCOA37'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA37'.
        WHEN 'ZCOA40'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA40'.
        WHEN 'ZCOA42'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA42'.
        WHEN 'ZCOA39'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA39'.
        WHEN 'ZCOA43'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA43'.
        WHEN 'ZCOA44'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA44'.
        WHEN 'ZCOA45'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA45'.
        WHEN 'ZCOA46'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA46'.
        WHEN 'ZCOA56'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA56'.
        WHEN 'ZCOA57'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA57'.
        WHEN 'ZCOA58'.
          IF I_REC-STATUS = 'O'.
            MESSAGE I000(ZMCO) WITH 'Already Executed'.
          ENDIF.
          CALL TRANSACTION 'ZCOA58'.



      ENDCASE.
      CLEAR I_REC-PGMNO.

    WHEN 'REFH'.
      PERFORM PRE_REPORT_DATA.
      PERFORM PRE_REPORT_ADJ.
      MESSAGE S000(ZMCO) WITH 'Data Refreshed'.
      PERFORM CALL_ALV_LIST.

      IF SY-UCOMM = '&F03' OR SY-UCOMM = '&F15' .   " back or exit key
        LEAVE TO TRANSACTION 'ZCOA49'.
      ENDIF.

    WHEN 'GOTO'.
      CALL TRANSACTION 'ZCOA50'.

  ENDCASE.
  CLEAR UCOMM.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE : / 'Controlling Area           : '
            , P_KOKRS .
  WRITE : / 'Fiscal Year/Period/Version : '
            , P_GJAHR, '/', P_FRPER, '~', P_TOPER , '/', P_VERSN.

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
  IF P_FRPER > P_TOPER.
    MESSAGE E003(ZMCO) WITH P_FRPER P_TOPER.
  ENDIF.

  IF P_FRPER < 0 OR P_FRPER > 12.
    MESSAGE E007(ZMCO) WITH P_FRPER .
  ENDIF.

  IF P_TOPER < 0 OR P_TOPER > 12.
    MESSAGE E007(ZMCO) WITH P_TOPER.
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

  CLEAR : IT_DATA, IT_DATA[].
  CLEAR : IT_MANAGE, IT_MANAGE[].
  CLEAR : IT_REPORT, IT_REPORT[].

  PERFORM PRE_CREATE_LIST.

**--- CREATE RANGES FOR PERIOD.
  RS_PERIOD-SIGN = 'I'.
  RS_PERIOD-OPTION = 'BT'.
  RS_PERIOD-LOW = P_FRPER.
  RS_PERIOD-HIGH = P_TOPER.
  APPEND RS_PERIOD.   CLEAR RS_PERIOD.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MANAGE
    FROM ZTCO_SKF_MANAGE
     WHERE KOKRS = P_KOKRS
       AND GJAHR = P_GJAHR
       AND PERBL IN RS_PERIOD
       AND VERSN = P_VERSN.


  DATA : COUNT TYPE I.

  LOOP AT IT_DATA.
    MOVE-CORRESPONDING IT_DATA TO IT_REPORT.

    IF P_FRPER = P_TOPER.
      CLEAR IT_MANAGE.
      READ TABLE IT_MANAGE WITH KEY PGMNO = IT_DATA-PGMNO
                                    KOKRS = P_KOKRS
                                    GJAHR = P_GJAHR
                                    PERBL = P_FRPER
                                    VERSN = P_VERSN.
      IF SY-SUBRC = 0.
        IT_REPORT-STATUS = 'O'.
        IT_REPORT-REMARK = 'execute'.
        IT_REPORT-DATE = IT_MANAGE-ERDAT.
        IT_REPORT-USER = IT_MANAGE-ERNAM.
      ELSE.
        IT_REPORT-STATUS = 'X'.
        IT_REPORT-REMARK = 'Did not execute'.
      ENDIF.

      APPEND IT_REPORT.
      CLEAR  IT_REPORT.

    ELSE.

      CLEAR IT_MANAGE.
      CLEAR COUNT.
      SELECT COUNT( * )
          INTO COUNT
          FROM ZTCO_SKF_MANAGE
          WHERE PGMNO = IT_DATA-PGMNO
            AND KOKRS = P_KOKRS
            AND GJAHR = P_GJAHR
            AND PERBL IN RS_PERIOD
            AND VERSN = P_VERSN.

      IF COUNT = GV_PERCOUNT.
        IT_REPORT-STATUS = 'O'.
        IT_REPORT-REMARK = 'execute'.
      ELSE.
        IT_REPORT-STATUS = 'X'.
        IT_REPORT-REMARK = 'Did not execute'.
      ENDIF.

      APPEND IT_REPORT.
      CLEAR  IT_REPORT.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " PRE_REPORT_DATA
*&---------------------------------------------------------------------*
*&      Form  PRE_CREATE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_CREATE_LIST.

  IT_DATA-ZSORT = 'A'.
  IT_DATA-GUBUN = 'CCA_Plan'.
  IT_DATA-PGMNO = 'ZCOA41'.
  IT_DATA-DESCR = 'Create No. of Persons by Department'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'B'.
  IT_DATA-GUBUN = 'CCA_Plan'.
  IT_DATA-PGMNO = 'ZCOA47'.
  IT_DATA-DESCR =
                   'Create Activity Output for Semi-Direct Cost Center'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'C'.
  IT_DATA-GUBUN = 'CCA_Plan'.
  IT_DATA-PGMNO = 'ZCOA30'.
  IT_DATA-DESCR =
             'Create Receiver Quantity for Semi-Direct CCtr s Activity'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'D'.
  IT_DATA-GUBUN = 'CCA_Plan'.
  IT_DATA-PGMNO = 'ZCOA58'.
  IT_DATA-DESCR = 'Allocate maintenance activity qty(plan)'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'E'.
  IT_DATA-GUBUN = 'CCA_Actual'.
  IT_DATA-PGMNO = 'ZCOA32'.
  IT_DATA-DESCR = 'Create No. of Persons by Department'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'F'.
  IT_DATA-GUBUN = 'CCA_Actual'.
  IT_DATA-PGMNO = 'ZCOA39'.
  IT_DATA-DESCR = 'Create Support Working Hour for Reposting'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'G'.
  IT_DATA-GUBUN = 'CCA_Actual'.
  IT_DATA-PGMNO = 'ZCOA56'.
  IT_DATA-DESCR = 'Create/Allocate Semi-direct activity qty(actual)'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'H'.
  IT_DATA-GUBUN = 'CCA_Actual'.
  IT_DATA-PGMNO = 'ZCOA57'.
  IT_DATA-DESCR = 'Create/Allocate maintenance activity qty(actual)'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'I'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA33'.
  IT_DATA-DESCR =
                      'Create Machine Hour for Resource/Process Driver'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'J'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA37'.
  IT_DATA-DESCR = 'Create Process Ratio by Cost Center'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'K'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA35'.
  IT_DATA-DESCR = 'Create Man Hour for Resource/Process Driver'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'L'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA40'.
  IT_DATA-DESCR = 'Create Input Working Hours by Process'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'M'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA42'.
  IT_DATA-DESCR =
                      'Create Process Quantity related to QM Ispection'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'N'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA43'.
  IT_DATA-DESCR = 'Create No. of Engines for Engine Shop'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'O'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA44'.
  IT_DATA-DESCR = 'Create No. of Vehicles for Trim Shop'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'P'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA45'.
  IT_DATA-DESCR = 'Create Planned PM Working Hour'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.

  IT_DATA-ZSORT = 'Q'.
  IT_DATA-GUBUN = 'ABC_Actual'.
  IT_DATA-PGMNO = 'ZCOA46'.
  IT_DATA-DESCR = 'Create Emergency PM Working Hour'.
  APPEND IT_DATA.
  CLEAR  IT_DATA.


ENDFORM.                    " PRE_CREATE_LIST
*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_PER_COUNT.

  GV_PERCOUNT = P_TOPER - P_FRPER + 1.

ENDFORM.                    " CAL_PER_COUNT
