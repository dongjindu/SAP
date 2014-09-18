************************************************************************
* Program Name      : ZRPM04_BDCH_D
* Author            : Myoungho, Park
* Creation Date     : 2004.02.05.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :   Result of Monthly average breakdown time
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZRPM04_BDCH_B  NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPM.

TYPE-POOLS: SLIS.

TABLES: ZSPM_PARAM,    "//PM Parameters
        ZTPM_MONBD,    "//Monthly breakdown TIME
        T024I.         "//Shop(Maintenance planner groups)



**** Internal Tables for breakdown rate
DATA: IT_TIME      LIKE	ZTPM_MONBD  OCCURS 0 WITH HEADER LINE.

**** etc variables...
DATA:   WA_DATE      LIKE SY-DATUM,
        SPMON        LIKE S051-SPMON.

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

****************** SELECTION-SCREEN *****************************

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS :  S_SHOP  FOR ZSPM_PARAM-SHOP.
*PARAMETERS : P_MONTH LIKE ZSPM_PARAM-ZMONTH.
SELECT-OPTIONS :  S_MONTH FOR ZSPM_PARAM-ZMONTH NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK BLOCK1.
****************** SELECTION-SCREEN *****************************

INITIALIZATION.

  GV_REPID = SY-REPID.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = SY-DATUM
            DAYS      = 0
            MONTHS    = 2
            SIGNUM    = '-'
            YEARS     = 0
       IMPORTING
            CALC_DATE = WA_DATE.

*  P_MONTH = WA_DATE(6).

  S_MONTH-LOW = WA_DATE(6).
  APPEND S_MONTH.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_MONTH-LOW .
  PERFORM SELECT_MONTH.
  IF NOT SPMON IS INITIAL.
    S_MONTH-LOW = SPMON.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_MONTH-HIGH .
  PERFORM SELECT_MONTH.
  IF NOT SPMON IS INITIAL.
    S_MONTH-HIGH = SPMON.
  ENDIF.

****************** START-OF-SELECTION  ******************************
*********************************************************************
START-OF-SELECTION.

END-OF-SELECTION.

  PERFORM GET_DOWN_TIME.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

* Call ALV LIST
  PERFORM CALL_ALV_LIST.


*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ.
* Building Field Cat.
  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

* Key
  PERFORM BUILD_FIELDCAT USING
    'IT_TIME' 'SHOP'  'X'     SPACE SPACE
     SPACE    '10'     'Shop'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_TIME' 'AJAHR'  'X'     SPACE SPACE
     SPACE    '5'     'Year'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_TIME' 'ZMONTH'  'X'     SPACE SPACE
     SPACE    '5'     'Month'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_TIME' 'ZDOWNTIME'  'X'     SPACE SPACE
     SPACE    '20'     'Line Down Time'  SPACE SPACE SPACE.

*** Sort
  SORT IT_TIME BY SHOP AJAHR.
  CLEAR: IT_TIME.

  IT_SORT-FIELDNAME = 'SHOP'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'AJAHR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'ZMONTH'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.
  IT_SORT-FIELDNAME = 'ZDOWNTIME'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE'.
  APPEND WA_L_EVENT TO IT_EVENTS.
ENDFORM.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_LIST.
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = GV_REPID
            I_CALLBACK_PF_STATUS_SET = GV_STATUS
            I_CALLBACK_USER_COMMAND  = GV_USER_COMMAND
            IT_FIELDCAT              = IT_FIELDCAT[]
            IT_SORT                  = IT_SORT[]
            I_SAVE                   = 'A'
            IT_EVENTS                = IT_EVENTS
            IT_EVENT_EXIT            = IT_EVENT_EXIT  "
       TABLES
            T_OUTTAB                 = IT_TIME
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " CALL_ALV_LIST
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
*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE.

  WRITE  AT 20 'Result of Monthly Line Down Time'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Basic Month/Year    : ', S_MONTH-LOW,
                                  '~', S_MONTH-HIGH.
  WRITE : / 'Execution Date/Time : ', SY-DATUM, SY-UZEIT.

  SKIP.
ENDFORM.
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST'  EXCLUDING EXTAB. " OF PROGRAM 'ZAPM08_ANBD'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DOWN_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DOWN_TIME.
  CLEAR: IT_TIME,      IT_TIME[].

*  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TIME
*           FROM  ZTPM_MONBD
*           WHERE SHOP  IN S_SHOP
*           AND   AJAHR  = P_MONTH(4)
*           AND   ZMONTH = P_MONTH.

  IF S_MONTH-HIGH IS INITIAL.
    MOVE : S_MONTH-LOW TO S_MONTH-HIGH.
  ENDIF.

  EXEC SQL PERFORMING loop_output.
    SELECT  SHOP, AJAHR, ZMONTH, ZDOWNTIME, MEINS
           INTO :IT_TIME-SHOP,
                :IT_TIME-AJAHR,
                :IT_TIME-ZMONTH,
                :IT_TIME-ZDOWNTIME,
                :IT_TIME-MEINS
           FROM ZTPM_MONBD
              WHERE MANDT = :SY-MANDT
                AND CONCAT(AJAHR, ZMONTH) >= :S_MONTH-LOW
                AND CONCAT(AJAHR, ZMONTH) <= :S_MONTH-HIGH
  ENDEXEC.

ENDFORM.                    " GET_DOWN_TIME
*&---------------------------------------------------------------------*
*&      Form  SELECT_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MONTH.
  CLEAR : SPMON.
  CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
    EXPORTING
      ACTUAL_MONTH                     = SY-DATUM(6)
*   FACTORY_CALENDAR                 = ' '
*   HOLIDAY_CALENDAR                 = ' '
     LANGUAGE                         = SY-LANGU
     START_COLUMN                     = 8
     START_ROW                        = 5
   IMPORTING
     SELECTED_MONTH                   = SPMON
*   RETURN_CODE                      =
   EXCEPTIONS
     FACTORY_CALENDAR_NOT_FOUND       = 1
     HOLIDAY_CALENDAR_NOT_FOUND       = 2
     MONTH_NOT_FOUND                  = 3
     OTHERS                           = 4
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " SELECT_MONTH
*&---------------------------------------------------------------------*
*&      Form  loop_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOOP_OUTPUT.
  IF IT_TIME-SHOP IN S_SHOP.
    APPEND IT_TIME.
  ENDIF.
ENDFORM.                    " loop_output
