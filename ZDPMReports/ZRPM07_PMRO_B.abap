************************************************************************
* Program Name      : ZRPM07_PMRO_B
* Author            : Myoungho Park
* Creation Date     : 2004.02.19.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Background job that computates
* Preventive / Post Maintenance Ratio for  Maintenance ratio report
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

************************************************************************

REPORT  ZRPM08_PMRO_B.

TYPE-POOLS: SLIS.

FIELD-SYMBOLS <WA_OBJCURR_VALUE>.

TABLES: ZSPM_PARAM,  "// PM Parameter
        T024I,       "//Shop(Maintenance planner groups)
        ZTPM_PMRO.   "//Preventive / Post Maintenance Ratio

**** Internal Table for Shop
DATA: BEGIN OF IT_SHOP OCCURS 0,
        SHOP LIKE ZSPM_PARAM-SHOP,
      END OF IT_SHOP.

DATA: BEGIN OF IT_MONTH OCCURS 0,
        ZMONTH LIKE SY-DATUM,
      END OF IT_MONTH.

**** Actual Maintenance PMRO
DATA : BEGIN OF IT_PMRO OCCURS 0.
        INCLUDE STRUCTURE ZTPM_PMRO.
DATA : END OF IT_PMRO.

**** Accumulation Actual Maintenance PMRO
DATA :  BEGIN OF IT_ALL_PMRO OCCURS 0.
        INCLUDE STRUCTURE ZTPM_PMRO.
DATA : END OF IT_ALL_PMRO.

DATA : IT_AFVGD LIKE ZSPM_PMRO_B OCCURS 0 WITH HEADER LINE.
DATA : IT_ALL_AFVGD LIKE ZSPM_PMRO_B OCCURS 0 WITH HEADER LINE.

DATA: WA_DATE       LIKE SY-DATUM,
      WA_CURR_MONTH LIKE SY-DATUM,
      WA_PREV_MONTH LIKE SY-DATUM.

DATA: WA_CURR_AMOUNT LIKE MLCD-LBKUM,
      WA_PREV_AMOUNT LIKE MLCD-LBKUM,
      WA_LBKUM LIKE MLCD-LBKUM.

DATA : WA_OBJCURR_FIELD(20).
DATA : WA_SUM_ACTUAL TYPE WTGXXX.
DATA : WA_PMRO_UNIT LIKE ZTPM_PMCO-ZAUNITC.

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

  CLEAR : IT_ALL_PMRO, IT_ALL_PMRO[].

*** make selection condtion shop & pirod...
  PERFORM MAKE_SELECTION_ENTRY.

  LOOP AT IT_MONTH.

    LOOP AT IT_SHOP.
      CLEAR : WA_PMRO_UNIT, WA_SUM_ACTUAL, WA_CURR_AMOUNT.
      CLEAR : IT_PMRO, IT_PMRO[].
      CLEAR : IT_AFVGD, IT_AFVGD[].

      CALL FUNCTION 'Z_FPM_MAINTENANCE_RATIO'
           EXPORTING
                I_SHOP      = IT_SHOP-SHOP
                I_AJAHR     = IT_MONTH-ZMONTH(4)
                I_ZMONTH    = IT_MONTH-ZMONTH+4(2)
           TABLES
                T_PMRO      = IT_PMRO
                T_ALL_AFVGD = IT_AFVGD.

      IF SY-SUBRC EQ 0.
        APPEND LINES OF IT_AFVGD TO IT_ALL_AFVGD.
        PERFORM SAVE_DATA USING IT_SHOP-SHOP
                                IT_MONTH-ZMONTH.
      ENDIF.
      APPEND LINES OF IT_PMRO TO IT_ALL_PMRO.
    ENDLOOP.
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
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA USING  P_SHOP
                      P_MONTH.

  DATA: IT_ZTPM_PMRO LIKE ZTPM_PMRO OCCURS 0 WITH HEADER LINE.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_ZTPM_PMRO
           FROM  ZTPM_PMRO
           WHERE SHOP   = P_SHOP
           AND   AJAHR  = P_MONTH(4)
           AND   ZMONTH = P_MONTH+4(2).

  LOOP AT IT_PMRO.
    READ TABLE IT_ZTPM_PMRO WITH KEY SHOP  = IT_PMRO-SHOP.
    IF SY-SUBRC EQ 0.
      MOVE: IT_ZTPM_PMRO-ERDAT TO	IT_PMRO-ERDAT,
            IT_ZTPM_PMRO-ERZET TO	IT_PMRO-ERZET,
            IT_ZTPM_PMRO-ERNAM TO	IT_PMRO-ERNAM.
      MOVE: SY-DATUM            TO  IT_PMRO-AEDAT,
            SY-UZEIT            TO  IT_PMRO-AEZET,
            SY-UNAME            TO  IT_PMRO-AENAM.
    ELSE.
      MOVE: SY-DATUM            TO	IT_PMRO-ERDAT,
            SY-UZEIT            TO	IT_PMRO-ERZET,
            SY-UNAME            TO	IT_PMRO-ERNAM,
            SY-DATUM            TO  IT_PMRO-AEDAT,
            SY-UZEIT            TO  IT_PMRO-AEZET,
            SY-UNAME            TO  IT_PMRO-AENAM.
    ENDIF.
    MODIFY IT_PMRO.
  ENDLOOP.

  MODIFY ZTPM_PMRO FROM TABLE IT_PMRO.
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
    'IT_ALL_PMRO' 'SHOP'  'X'     SPACE SPACE
     SPACE    '10'     'Shop'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_PMRO' 'AJAHR'  'X'     SPACE SPACE
     SPACE    '4'     'Year'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_PMRO' 'ZMONTH'  'X'     SPACE SPACE
     SPACE    '2'     'Month'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_ALL_PMRO' 'AUART'  'X'     SPACE SPACE
     SPACE    '10'     'Order type'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ALL_PMRO' 'ZPLAND'  'X'     SPACE SPACE
      SPACE    '10'     'Number of Planned'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ALL_PMRO' 'ZACTAL'  'X'     SPACE SPACE
      SPACE    '10'     'Number of Actual'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ALL_PMRO' 'ZPTIME'  'X'     SPACE SPACE
      SPACE    '15'     'Actual Working Time'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_ALL_PMRO' 'MEINS'  'X'     SPACE SPACE
       SPACE    '05'     'Unit'  SPACE SPACE SPACE.

*** Sort
  SORT IT_ALL_PMRO BY SHOP AJAHR ZMONTH.
  CLEAR: IT_ALL_PMRO.

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

  IT_SORT-FIELDNAME = 'AUART'.
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
            T_OUTTAB                 = IT_ALL_PMRO
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
*  WA_FIELDCAT-DO_SUM      = P_0103.
*  WA_FIELDCAT-CFIELDNAME  = P_0104.
*  WA_FIELDCAT-CTABNAME    = P_0105.
*  WA_FIELDCAT-OUTPUTLEN   = P_0106.
  WA_FIELDCAT-SELTEXT_L   = P_0107.
*  WA_FIELDCAT-DATATYPE    = P_0108.
*  WA_FIELDCAT-QFIELDNAME  = P_0109.
*  WA_FIELDCAT-QTABNAME    = P_0110.
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

  WRITE  AT 20 'Result of Monthly Maintenance Ratio'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Analysis Month/Year : ', P_MONTH.
  WRITE : / 'Execution Date/Time      : ', SY-DATUM, SY-UZEIT.
  SKIP.
ENDFORM.
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
FORM PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'BALVLIST'  EXCLUDING EXTAB.
ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       User Command for ALV List
*----------------------------------------------------------------------*
FORM USER_COMMAND  USING R_UCOMM
                         RS_SELFIELD TYPE SLIS_SELFIELD.

  CASE R_UCOMM.
    WHEN '&IC1'.
      CLEAR : R_UCOMM.
      CLEAR : IT_AFVGD, IT_AFVGD[].
      READ TABLE IT_ALL_PMRO INDEX RS_SELFIELD-TABINDEX.
      LOOP AT IT_ALL_AFVGD WHERE SHOP   = IT_ALL_PMRO-SHOP
                           AND   AJAHR  = IT_ALL_PMRO-AJAHR
                           AND   ZMONTH = IT_ALL_PMRO-ZMONTH
                           AND   AUART  = IT_ALL_PMRO-AUART.

        MOVE-CORRESPONDING IT_ALL_AFVGD TO IT_AFVGD.
        APPEND IT_AFVGD.
      ENDLOOP.
      PERFORM PRE_REPORT_ADJ_2.
      PERFORM CALL_ALV_DETAIL_LIST.
  ENDCASE.
ENDFORM.                 " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRE_REPORT_ADJ_2.
* Building Field Cat.
  CLEAR : GV_COL_POS, IT_FIELDCAT, IT_FIELDCAT[].

* Key

  PERFORM BUILD_FIELDCAT USING
    'IT_AFVGD' 'AUFNR'  'X'     SPACE SPACE
     SPACE    '12'     'Order Number'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
  'IT_AFVGD' 'VORNR'  'X'     SPACE SPACE
   SPACE    '10'     'Operation Number'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_AFVGD' 'ISDD'  'X'     SPACE SPACE
     SPACE    '10'     'Actual start date'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_AFVGD' 'ISDZ'  'X'     SPACE SPACE
     SPACE    '10'     'Actual start time'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
   'IT_AFVGD' 'IEDD'  'X'     SPACE SPACE
   SPACE    '10'     'Actual end date'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_AFVGD' 'IEDZ'  'X'     SPACE SPACE
     SPACE    '10'     'Actual end time'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
    'IT_AFVGD' 'ZPTIME'  'X'     SPACE SPACE
     SPACE    '10'     'Work time'  SPACE SPACE SPACE.

  PERFORM BUILD_FIELDCAT USING
      'IT_AFVGD' 'BEAZE'  'X'     SPACE SPACE
      SPACE    '5'     'Unit'  SPACE SPACE SPACE.


*** Sort
  SORT IT_AFVGD STABLE BY AUFNR VORNR  .
  CLEAR: IT_AFVGD.

  IT_SORT-FIELDNAME = 'AUFNR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

  IT_SORT-FIELDNAME = 'VORNR'.
  IT_SORT-UP        = 'X'.
  IT_SORT-EXPA      = 'X'.
  IT_SORT-SUBTOT    = 'X'.
  APPEND IT_SORT.

* Set Event
  DATA : WA_L_EVENT  TYPE SLIS_ALV_EVENT.
  WA_L_EVENT-NAME = SLIS_EV_TOP_OF_PAGE.
  WA_L_EVENT-FORM = 'BASIC_TOP_OF_PAGE2'.
  APPEND WA_L_EVENT TO IT_EVENTS.

ENDFORM.                    " PRE_REPORT_ADJ_2
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_DETAIL_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_DETAIL_LIST.
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
            T_OUTTAB                 = IT_AFVGD
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_ALV_DETAIL_LIST

*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
FORM BASIC_TOP_OF_PAGE2.

  WRITE  AT 20 'Detail List Monthly Maintenance Ratio'
         INVERSE COLOR 3.
  SKIP.
  WRITE : / 'Analysis Shop  : ', IT_ALL_PMRO-SHOP.
  WRITE : / 'Analysis Year  : ', IT_ALL_PMRO-AJAHR.
  WRITE : / 'Analysis Month : ', IT_ALL_PMRO-ZMONTH.
  WRITE : / 'Order Type     : ', IT_ALL_PMRO-AUART.

  SKIP.
ENDFORM.
