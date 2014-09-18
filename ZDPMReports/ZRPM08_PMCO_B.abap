************************************************************************
* Program Name      : ZRPM08_PMCO_B
* Author            : Myoungho Park
* Creation Date     : 2004.02.03.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Background job that computates
* Maintenance Cost and Unit per Cost for  Maintenance Cost Trend Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

************************************************************************

REPORT  ZRPM08_PMCO_B.

TYPE-POOLS: SLIS.

FIELD-SYMBOLS <WA_OBJCURR_VALUE>.

TABLES: ZSPM_PARAM,  "// PM Parameter
        T024I,       "//Shop(Maintenance planner groups)
        COSP.        "//Cost Totals for External Postings


**** Internal Table for Shop
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZSPM_PARAM-SHOP,
      END OF IT_SHOP.

*** Cost est number
*DATA : BEGIN OF IT_KALNR OCCURS 0,
*         KALNR LIKE CKMLHD-KALNR,
*       END OF IT_KALNR.

DATA: BEGIN OF IT_MONTH OCCURS 0,
        ZMONTH LIKE SY-DATUM,
      END OF IT_MONTH.

**** Actual Maintenance Cost
DATA : BEGIN OF IT_COST OCCURS 0.
        INCLUDE STRUCTURE ZTPM_PMCO.
DATA : END OF IT_COST.

**** Accumulation Actual Maintenance Cost
DATA :  BEGIN OF IT_ALL_COST OCCURS 0.
        INCLUDE STRUCTURE ZTPM_PMCO.
DATA : END OF IT_ALL_COST.

DATA: WA_DATE       LIKE SY-DATUM,
      WA_CURR_MONTH LIKE SY-DATUM,
      WA_PREV_MONTH LIKE SY-DATUM.

DATA: WA_CURR_AMOUNT LIKE MLCD-LBKUM,
      WA_PREV_AMOUNT LIKE MLCD-LBKUM,
      WA_LBKUM LIKE MLCD-LBKUM.

DATA : WA_OBJCURR_FIELD(20).
DATA : WA_SUM_ACTUAL TYPE WTGXXX.
DATA : WA_COST_UNIT LIKE ZTPM_PMCO-ZAUNITC.

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

PARAMETERS : P_MONTH LIKE ZSPM_PARAM-ZMONTH DEFAULT SY-DATUM(6).

SELECTION-SCREEN END OF BLOCK BLOCK1.
****************** SELECTION-SCREEN *****************************


******************* INITIALIZATION ********************************
*******************************************************************
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


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_MONTH .
  PERFORM SELECT_MONTH.

****************** START-OF-SELECTION  ******************************
*********************************************************************
START-OF-SELECTION.

END-OF-SELECTION.

  CLEAR : IT_ALL_COST, IT_ALL_COST[].

*** make selection condtion shop & pirod...
  PERFORM MAKE_SELECTION_ENTRY.

  LOOP AT IT_MONTH.

    CLEAR : IT_COST, IT_COST[].

    LOOP AT IT_SHOP.
      CLEAR : WA_COST_UNIT, WA_SUM_ACTUAL, WA_CURR_AMOUNT.

*** Get Amount of Produced Car...
      PERFORM GET_PRODUCED_CAR USING IT_SHOP-SHOP
                                     IT_MONTH-ZMONTH.

*** Get Actual Value of Mainteance Cost...
      PERFORM GET_ACTUAL_COST USING IT_SHOP-SHOP
                                    IT_MONTH-ZMONTH.

*** Get Actual Value of Mainteance Common Cost...
*      PERFORM GET_COMMON_COST USING IT_MONTH-ZMONTH.

****  Cost per Unit
****  = Actual Value of Mainteance Cost / Amount of Produced Car
*      PERFORM CAL_COST_PER_UNIT USING IT_MONTH-ZMONTH.

      IF NOT WA_CURR_AMOUNT IS INITIAL.
        WA_COST_UNIT = WA_SUM_ACTUAL / WA_CURR_AMOUNT.
      ENDIF.

      MOVE :  IT_SHOP-SHOP         TO IT_COST-SHOP,
              IT_MONTH-ZMONTH(4)   TO IT_COST-AJAHR,
              IT_MONTH-ZMONTH+4(2) TO IT_COST-ZMONTH,
              WA_SUM_ACTUAL        TO IT_COST-ZACOST,
              WA_COST_UNIT         TO IT_COST-ZAUNITC,
              'USD'                TO IT_COST-WAERS.
      APPEND IT_COST.
    ENDLOOP.

    PERFORM SAVE_DATA USING IT_MONTH-ZMONTH.

    APPEND LINES OF IT_COST TO IT_ALL_COST.
  ENDLOOP.

  IF SY-BATCH EQ 'X'.
    LEAVE TO SCREEN 0.
  ENDIF.

* Preparation of ALV
  PERFORM PRE_REPORT_ADJ.

* Call ALV LIST
  PERFORM CALL_ALV_LIST.
*&---------------------------------------------------------------------*
*&      Form  SELECT_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MONTH.
  DATA SPMON LIKE S051-SPMON.

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
  IF NOT SPMON IS INITIAL.
    P_MONTH = SPMON.
  ENDIF.

ENDFORM.                    " SELECT_MONTH
*&---------------------------------------------------------------------*
*&      Form  MAKE_SELECTION_ENTRY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_SELECTION_ENTRY.
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
*&      Form  GET_ACTUAL_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ACTUAL_COST USING  P_SHOP
                            P_MONTH.

  DATA : BEGIN OF IT_OBJECT OCCURS 0,
           AUFNR LIKE AUFM-AUFNR,
           SHKZG LIKE AUFM-SHKZG,
           DMBTR LIKE AUFM-DMBTR,
           BWART TYPE BWART,       "/Inserted by sllee : 05/14/2004
         END OF IT_OBJECT.

  DATA: WA_S_DAY LIKE SY-DATUM,
        WA_E_DAY LIKE SY-DATUM.

  DATA: BEGIN OF IT_OBJNR OCCURS 0,
          OBJNR LIKE AUFK-OBJNR,
          BUDAT LIKE AFRU-BUDAT,
        END OF IT_OBJNR.

  CLEAR: WA_SUM_ACTUAL.

  MOVE: P_MONTH TO WA_S_DAY.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
       EXPORTING
            DAY_IN            = WA_S_DAY
       IMPORTING
            LAST_DAY_OF_MONTH = WA_E_DAY.

*-/ Modified by sllee : requested by Mr. Yu : 05/14/2004 - Start
*     Add movement type '262' for return value

  SELECT  A~AUFNR A~SHKZG A~DMBTR  A~BWART
          INTO CORRESPONDING FIELDS OF TABLE IT_OBJECT
          FROM AUFM AS A  INNER JOIN VIAUFKST AS B
               ON  A~AUFNR = B~AUFNR
          WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
*          AND   A~BWART = '261'
          AND   A~BWART IN ('261', '262')
          AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
          AND   B~INGPR = P_SHOP.

*          break-point.

  LOOP AT IT_OBJECT.
*    IF IT_OBJECT-SHKZG  = 'S'.
*      IT_OBJECT-DMBTR = IT_OBJECT-DMBTR * ( -1 ).
*    ENDIF.

*        WA_SUM_ACTUAL = WA_SUM_ACTUAL + IT_OBJECT-DMBTR.

    CASE IT_OBJECT-BWART.
      WHEN '261'.
        WA_SUM_ACTUAL = WA_SUM_ACTUAL + IT_OBJECT-DMBTR.
      WHEN '262'.
        WA_SUM_ACTUAL = WA_SUM_ACTUAL - IT_OBJECT-DMBTR.
    ENDCASE.

  ENDLOOP.

*-/ Modified by sllee : requested by Mr. Yu : 05/14/2004 - End

*  SELECT OBJNR BUDAT
*         INTO CORRESPONDING FIELDS OF TABLE IT_OBJNR
*         FROM AFRU AS A
*            INNER JOIN VIAUFKST AS B
*            ON  A~AUFNR = B~AUFNR
*         WHERE A~BUDAT BETWEEN WA_S_DAY AND WA_E_DAY
*         AND   B~AUART IN ('PM01', 'PM02', 'PM03', 'PM04')
*         AND   B~INGPR = P_SHOP.
*  LOOP AT IT_OBJNR.
*    SELECT  SINGLE *
*            FROM COSP
*            WHERE OBJNR = IT_OBJNR-OBJNR
*            AND   GJAHR = IT_OBJNR-BUDAT(4)   "//
*            AND   WRTTP = '04'                "//
*            AND   VERSN = '000'.              "//
*    IF SY-SUBRC EQ 0.
*      CONCATENATE 'COSP-WOG0' IT_OBJNR-BUDAT+4(2)
*                  INTO  WA_OBJCURR_FIELD.
*
*      ASSIGN (WA_OBJCURR_FIELD) TO <WA_OBJCURR_VALUE>.
*      WA_SUM_ACTUAL = WA_SUM_ACTUAL + <WA_OBJCURR_VALUE>.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " GET_ACTUAL_COST
*&---------------------------------------------------------------------*
*&      Form  GET_PRODUCED_CAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_PRODUCED_CAR USING  P_SHOP
                             P_MONTH.

  SELECT SINGLE ZUNIT INTO WA_CURR_AMOUNT
         FROM  ZTPM_PUNIT
         WHERE SHOP   = P_SHOP
         AND   AJAHR  = P_MONTH(4)
         AND   ZMONTH = P_MONTH+4(2).


*  CLEAR: IT_KALNR[], IT_KALNR.
*
*  SELECT C~KALNR                "//Cost est number for cost est
*         INTO CORRESPONDING FIELDS OF TABLE IT_KALNR
*         FROM MARA AS A
*           INNER JOIN MBEW AS B
*           ON A~MATNR = B~MATNR
*              INNER JOIN CKMLHD AS C
*              ON  A~MATNR = C~MATNR
*              AND B~BWKEY = C~BWKEY
*              AND B~BWTAR = C~BWTAR
*         WHERE A~MTART = 'FERT'.
*
*  CLEAR : WA_CURR_AMOUNT, WA_PREV_AMOUNT.
*
*
*  LOOP AT IT_KALNR.
***** Current month produced car ...
*    CLEAR: WA_LBKUM.
*    SELECT SUM( LBKUM )
*           INTO WA_LBKUM
*           FROM MLCD
*           WHERE KALNR = IT_KALNR-KALNR
*           AND   BDATJ = P_MONTH(4)
*           AND   POPER = P_MONTH+4(2)
*           AND   CATEG = 'ZU' "//Category in the material ledger
*           AND   PTYP  = 'BF' "//Process category for procurement
*           AND   CURTP = '10'.
*
*    WA_CURR_AMOUNT = WA_CURR_AMOUNT + WA_LBKUM.
*
*  ENDLOOP.

ENDFORM.                    " GET_PRODUCED_CAR
*&---------------------------------------------------------------------*
*&      Form  GET_COMMON_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_COMMON_COST USING P_MONTH.

ENDFORM.                    " GET_COMMON_COST
*&---------------------------------------------------------------------*
*&      Form  CAL_COST_PER_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_COST_PER_UNIT USING P_MONTH.

ENDFORM.                    " CAL_COST_PER_UNIT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA USING P_MONTH.
  DATA: IT_ZTPM_PMCO LIKE ZTPM_PMCO OCCURS 0 WITH HEADER LINE.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_PMCO
           FROM  ZTPM_PMCO
           WHERE SHOP  IN S_SHOP
           AND   AJAHR  = P_MONTH(4)
           AND   ZMONTH = P_MONTH+4(2).

  LOOP AT IT_COST.
    READ TABLE IT_ZTPM_PMCO WITH KEY SHOP  = IT_COST-SHOP.
    IF SY-SUBRC EQ 0.
      MOVE: P_MONTH(4)          TO  IT_COST-AJAHR,
            P_MONTH+4(2)        TO  IT_COST-ZMONTH,
            IT_ZTPM_PMCO-ERDAT TO	IT_COST-ERDAT,
            IT_ZTPM_PMCO-ERZET TO	IT_COST-ERZET,
            IT_ZTPM_PMCO-ERNAM TO	IT_COST-ERNAM.
      MOVE: SY-DATUM            TO  IT_COST-AEDAT,
            SY-UZEIT            TO  IT_COST-AEZET,
            SY-UNAME            TO  IT_COST-AENAM.
    ELSE.
      MOVE: P_MONTH(4)          TO  IT_COST-AJAHR,
            P_MONTH+4(2)        TO  IT_COST-ZMONTH,
            SY-DATUM            TO	IT_COST-ERDAT,
            SY-UZEIT            TO	IT_COST-ERZET,
            SY-UNAME            TO	IT_COST-ERNAM,
            SY-DATUM            TO  IT_COST-AEDAT,
            SY-UZEIT            TO  IT_COST-AEZET,
            SY-UNAME            TO  IT_COST-AENAM.
    ENDIF.
    MODIFY IT_COST.
  ENDLOOP.

  MODIFY ZTPM_PMCO FROM TABLE IT_COST.
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
    'IT_ALL_COST' 'SHOP'  'X'     SPACE SPACE
     SPACE    '10'     'Shop'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_COST' 'AJAHR'  'X'     SPACE SPACE
     SPACE    '4'     'Year'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_COST' 'ZMONTH'  'X'     SPACE SPACE
     SPACE    '2'     'Month'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_COST' 'ZACOST'  'X'     SPACE SPACE
     SPACE    '21'     'Atc. Maintenance Cost'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_COST' 'ZAUNITC'  'X'     SPACE SPACE
     SPACE    '21'     'Cost per Unit'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ALL_COST' 'WAERS'  'X'     SPACE SPACE
      SPACE    '5'     'Currency'  SPACE SPACE SPACE.

*** Sort
  SORT IT_ALL_COST BY SHOP AJAHR ZMONTH.
  CLEAR: IT_ALL_COST.

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

  IT_SORT-FIELDNAME = 'ZACOST'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'ZAUNITC'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'WAERS'.
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
            T_OUTTAB                 = IT_ALL_COST
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

  WRITE  AT 20 'Result of Monthly Maintenance Cost'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Cost Analysis Month/Year : ', P_MONTH.
  WRITE : / 'Execution Date/Time      : ', SY-DATUM, SY-UZEIT.
  SKIP.
ENDFORM.
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST'  EXCLUDING EXTAB.
ENDFORM.
