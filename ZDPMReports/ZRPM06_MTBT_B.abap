************************************************************************
* Program Name      : ZRPM06_MTBT_B
* Author            : Myoungho, Park
* Creation Date     : 2004.02.05.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :   Calculate monthly actual no. of breakdown
*                       Runs by background job by daily .
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZRPM06_MTBT_B NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPM.

TYPE-POOLS: SLIS.

TABLES: ZSPM_PARAM,    "//PM Parameters
        ZTPM_BDNO,    "//Monthly  Actual No. of Breakdown
        T024I.         "//Shop(Maintenance planner groups)


**** Internal Table for Shop
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZSPM_PARAM-SHOP,
      END OF IT_SHOP.

**** Internal Table for Month
DATA: BEGIN OF IT_MONTH OCCURS 0,
         ZMONTH LIKE SY-DATUM,
      END OF IT_MONTH.

**** Internal Tables for breakdown rate
DATA: IT_ZTPM_BDNO LIKE	ZTPM_BDNO OCCURS 0 WITH HEADER LINE,
      IT_BDNO      LIKE	ZTPM_BDNO  OCCURS 0 WITH HEADER LINE.

**** No. of breakdown
DATA: WA_BDNUM  TYPE I. "LIKE ZTPM_BDNO-ZBDNUM.

**** etc variables...
DATA:   WA_DATE      LIKE SY-DATUM,
        WA_MONTH_DAY LIKE SY-DATUM,
        CAL_DATE     LIKE SY-DATUM,
        WA_MONTH(2)  TYPE N,
        WA_COUNT     LIKE T5A4A-DLYMO.

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
PARAMETERS : P_MONTH LIKE ZSPM_PARAM-ZMONTH. "DEFAULT SY-DATUM(6).
SELECT-OPTIONS :  S_SHOP  FOR ZSPM_PARAM-SHOP.
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

  P_MONTH = WA_DATE(6).
*  WA_DATE  = SY-DATUM.


START-OF-SELECTION.

  SELECT  DISTINCT INGRP AS SHOP
          INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  T024I
          WHERE INGRP IN S_SHOP.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-002.
  ENDIF.


END-OF-SELECTION.

*** make selection condtion shop & pirod...
  PERFORM MAKE_SELECTION_ENTRY.

  PERFORM GET_NUM_BREAKDOWN.
  CHECK SY-SUBRC EQ 0.

  PERFORM SAVE_DATA.

  IF SY-BATCH EQ 'X'.
   LEAVE TO SCREEN 0.
  ENDIF.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

* Call ALV LIST
  PERFORM CALL_ALV_LIST.

*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_BDNO
           FROM  ZTPM_BDNO
           WHERE SHOP  IN S_SHOP
           AND   AJAHR  = P_MONTH(4)
           AND   ZMONTH = P_MONTH.

  LOOP AT IT_BDNO.
    READ TABLE IT_ZTPM_BDNO WITH KEY SHOP  = IT_BDNO-SHOP.
    IF SY-SUBRC EQ 0.
      MOVE:
            IT_ZTPM_BDNO-ERDAT TO	IT_BDNO-ERDAT,
            IT_ZTPM_BDNO-ERZET TO	IT_BDNO-ERZET,
            IT_ZTPM_BDNO-ERNAM TO	IT_BDNO-ERNAM.
      MOVE: SY-DATUM            TO  IT_BDNO-AEDAT,
            SY-UZEIT            TO  IT_BDNO-AEZET,
            SY-UNAME            TO  IT_BDNO-AENAM.
    ELSE.
      MOVE:
            SY-DATUM            TO	IT_BDNO-ERDAT,
            SY-UZEIT            TO	IT_BDNO-ERZET,
            SY-UNAME            TO	IT_BDNO-ERNAM,
            SY-DATUM            TO  IT_BDNO-AEDAT,
            SY-UZEIT            TO  IT_BDNO-AEZET,
            SY-UNAME            TO  IT_BDNO-AENAM.
    ENDIF.
    MODIFY IT_BDNO.
  ENDLOOP.

  MODIFY ZTPM_BDNO FROM TABLE IT_BDNO.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    COMMIT WORK.
    MESSAGE S000(ZMPM) WITH TEXT-005.
  ENDIF.

ENDFORM.                    " SAVE_DATA
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
    'IT_BDNO' 'SHOP'  'X'     SPACE SPACE
     SPACE    '10'     'Shop'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_BDNO' 'AJAHR'  'X'     SPACE SPACE
     SPACE    '5'     'Year'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_BDNO' 'ZMONTH'  'X'     SPACE SPACE
     SPACE    '5'     'Month'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_BDNO' 'ZBDNUM'  'X'     SPACE SPACE
     SPACE    '20'     'No. Line Down Time'  SPACE SPACE SPACE.

*** Sort
  SORT IT_BDNO BY SHOP AJAHR.
  CLEAR: IT_BDNO.

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
  IT_SORT-FIELDNAME = 'ZBDNUM'.
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
            T_OUTTAB                 = IT_BDNO
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

  WRITE  AT 20 'Result of Actual No. of Line Down Time'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Basic Month/Year    : ', P_MONTH.
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
*&      Form  MAKE_SELECTION_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_SELECTION_ENTRY.
  DATA: WA_CURR_MONTH LIKE SY-DATUM,
        WA_PREV_MONTH LIKE SY-DATUM.

*** make shop list....
  SELECT  DISTINCT INGRP AS SHOP
          INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
          FROM  T024I
          WHERE INGRP IN S_SHOP.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-002.
  ENDIF.

  CLEAR: WA_CURR_MONTH, WA_PREV_MONTH.
*** make current month first day for select condition
  CONCATENATE P_MONTH '01' INTO WA_CURR_MONTH.
  MOVE WA_CURR_MONTH TO IT_MONTH-ZMONTH.
  APPEND IT_MONTH.

*** make previous month first day for select condition
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = WA_CURR_MONTH
            DAYS      = 0
            MONTHS    = 1
            SIGNUM    = '-'
            YEARS     = 0
       IMPORTING
            CALC_DATE = WA_PREV_MONTH.
  MOVE WA_PREV_MONTH TO IT_MONTH-ZMONTH.
  APPEND IT_MONTH.

ENDFORM.                    " MAKE_SELECTION_ENTRY
*&---------------------------------------------------------------------*
*&      Form  GET_NUM_BREAKDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_NUM_BREAKDOWN.
  CLEAR: IT_BDNO,  IT_BDNO[].

**** Calculate No. of breakdown
  LOOP AT IT_MONTH.
    LOOP AT IT_SHOP.
      CLEAR: IT_BDNO, WA_BDNUM.

      PERFORM CAL_NUM_BREAKDOWN USING IT_SHOP-SHOP
                                      IT_MONTH-ZMONTH(6).

      MOVE : IT_SHOP-SHOP         TO IT_BDNO-SHOP.
      MOVE : IT_MONTH-ZMONTH(4)   TO IT_BDNO-AJAHR.
      MOVE : IT_MONTH-ZMONTH+4(2) TO IT_BDNO-ZMONTH.
      MOVE : WA_BDNUM             TO IT_BDNO-ZBDNUM.

      APPEND IT_BDNO.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_NUM_BREAKDOWN
*&---------------------------------------------------------------------*
*&      Form  CAL_NUM_BREAKDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SHOP  text
*      -->P_IT_MONTH_ZMONTH(6)  text
*----------------------------------------------------------------------*
FORM CAL_NUM_BREAKDOWN USING    P_SHOP
                                P_ZMONTH.

  DATA :  WA_S_DAY     LIKE SY-DATUM,
          WA_E_DAY     LIKE SY-DATUM.

*** Make Selection entry...
  CONCATENATE P_ZMONTH '01' INTO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.
  CASE P_SHOP.
    WHEN 'P10'.
      SELECT  COUNT( DISTINCT A~AUFNR )
             INTO WA_BDNUM
             FROM AFRU  AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
             WHERE B~INGRP = P_SHOP
             AND   A~AUERU EQ 'X'
             AND   A~STOKZ EQ ' '
             AND   A~STZHL EQ SPACE
             AND   B~STORT IN ('PS1', 'PS2')
             AND   B~AUSVN BETWEEN  WA_S_DAY    "//Start of Malfunction
                               AND  WA_E_DAY.
    WHEN 'P20' OR 'P30' OR 'P40'.
      SELECT  COUNT( DISTINCT A~AUFNR )
             INTO WA_BDNUM
             FROM AFRU  AS A
                  INNER JOIN VIQMEL AS B
                  ON A~AUFNR = B~AUFNR
             WHERE B~INGRP = P_SHOP
             AND   A~AUERU EQ 'X'
             AND   A~STOKZ EQ ' '
             AND   A~STZHL EQ SPACE
             AND   B~AUSVN BETWEEN  WA_S_DAY    "//Start of Malfunction
                               AND  WA_E_DAY.
    WHEN 'P50'.
      SELECT  COUNT( DISTINCT A~AUFNR )
         INTO WA_BDNUM
         FROM AFRU  AS A
              INNER JOIN VIQMEL AS B
              ON A~AUFNR = B~AUFNR
         WHERE B~INGRP = P_SHOP
         AND   A~AUERU EQ 'X'
         AND   A~STOKZ EQ ' '
         AND   A~STZHL EQ SPACE
         AND   B~STORT  = 'V00'
         AND   B~AUSVN BETWEEN  WA_S_DAY    "//Start of Malfunction
                           AND  WA_E_DAY.
  ENDCASE.


ENDFORM.                    " CAL_NUM_BREAKDOWN
