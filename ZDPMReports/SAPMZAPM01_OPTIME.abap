
************************************************************************
* Program Name      : SAPMZAPM01_OPTIME
* Author            : Myoungho Park
* Creation Date     : 2003.08.08.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  This Program is Input Operation Time
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZAPM01_OPTIME             .

TYPE-POOLS CXTAB .  "//Table_control Object type pool

**** Tables
TABLES: T024I,         "//Maintenance planner groups
*        T357,         "//Plant section
        ZSPM_PARAM,   "//Parameters
        ZSPM_OPTIME,  "//Operation Time structure
        ZTPM_OPTIME.  "//Operation Time Table

*** Shop internal table
DATA: BEGIN OF IT_SHOP OCCURS 0,
          SHOP  LIKE ZTPM_SHOP-SHOP,
          SHTXT LIKE ZTPM_SHOP-SHTXT,
      END OF IT_SHOP.

DATA: IT_SHOP_ALL LIKE IT_SHOP OCCURS 0 WITH HEADER LINE.
DATA: IT_TEMP LIKE ZTPM_OPTIME OCCURS 0 WITH HEADER LINE,
      IT_OPTIME LIKE ZSPM_OPTIME OCCURS 0 WITH HEADER LINE,
      IT_ZTPM_OPTIME LIKE ZTPM_OPTIME OCCURS 0 WITH HEADER LINE.

DATA: WA_PROGNAME LIKE SY-REPID,
      WA_DYNNUM LIKE SY-DYNNR.

DATA: WA_YEAR LIKE ZSPM_OPTIME-AJAHR,
      WA_TITLE(40).

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control
FIELD-SYMBOLS: <MONTH> .
FIELD-SYMBOLS: <INPUT_MONTH>.

*** Table control variables
CONTROLS: TC_0200 TYPE TABLEVIEW USING SCREEN 0200.

DATA : WA_TCNAME LIKE FELD-NAME. "table control name


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
*** Make Title & Status
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_HELP_SHOP  INPUT
*&---------------------------------------------------------------------*
*       Not used
*----------------------------------------------------------------------*
MODULE F4_HELP_SHOP INPUT.
  CLEAR: IT_SHOP_ALL, IT_SHOP_ALL[].

  WA_PROGNAME = SY-REPID.
  WA_DYNNUM   = SY-DYNNR.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_SHOP_ALL
           FROM  ZTPM_SHOP
           WHERE SPRAS   = SY-LANGU.

  IT_SHOP_ALL-SHOP  = '*'.
  IT_SHOP_ALL-SHTXT = 'ALL'.
  APPEND IT_SHOP_ALL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD    = 'SHOP'
            DYNPPROG    = WA_PROGNAME
            DYNPNR      = WA_DYNNUM
            DYNPROFIELD = 'ZSPM_OPTIME-SHOP'
            VALUE_ORG   = 'S'
       TABLES
            VALUE_TAB   = IT_SHOP_ALL.

ENDMODULE.                 " F4_HELP_SHOP  INPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Initial Values
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_0100 OUTPUT.
*** Year
  IF ZSPM_PARAM-AJAHR IS INITIAL.
    WA_YEAR = SY-DATUM(4).
    ZSPM_PARAM-AJAHR = WA_YEAR.
  ENDIF.
ENDMODULE.                 " INITIAL_VALUE_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

    WHEN 'ENTER'.
      CLEAR: SY-UCOMM.
*** Run progrma
      PERFORM EXEC_PROGRAM.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN '%EX'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

    WHEN 'RW'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Form  LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LEAVE_PROGRAM.
  LEAVE TO SCREEN 0.
ENDFORM.                    " LEAVE_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  EXEC_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXEC_PROGRAM.
  PERFORM CHECK_FIELDS.
  PERFORM SET_IT_SHOP.
  PERFORM SELECT_OPTIME_DATA.

  CALL SCREEN '0200'.
ENDFORM.                    " EXEC_PROGRAM
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       Make Table Control Title...
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_0200 OUTPUT.
  CONCATENATE ZSPM_PARAM-AJAHR TEXT-001 TEXT-002  INTO WA_TITLE.
ENDMODULE.                 " INITIAL_VALUE_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_OPTIME_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_OPTIME_DATA.

  CLEAR: IT_TEMP, IT_TEMP[],
         IT_OPTIME, IT_OPTIME[].
*** read Oeration Time
  IF ZSPM_OPTIME-SHOP = ' '.
    SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
             FROM  ZTPM_OPTIME
             WHERE AJAHR = ZSPM_PARAM-AJAHR
             ORDER BY SHOP AJAHR ZMONTH.
  ELSE.
    SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM  ZTPM_OPTIME
           WHERE SHOP  = ZSPM_PARAM-SHOP
           AND   AJAHR = ZSPM_PARAM-AJAHR
           ORDER BY SHOP AJAHR ZMONTH.
  ENDIF.

  IF SY-SUBRC EQ 0.

    LOOP AT IT_SHOP.
      LOOP AT IT_TEMP WHERE SHOP = IT_SHOP-SHOP.
        PERFORM SET_IT_OPTIME USING IT_TEMP-ZMONTH
                                    IT_TEMP-OPTIME.
      ENDLOOP.
      MOVE: IT_TEMP-SHOP  TO IT_OPTIME-SHOP,
            IT_TEMP-AJAHR TO IT_OPTIME-AJAHR,
            IT_TEMP-MAUEH    TO IT_OPTIME-MAUEH.
      APPEND IT_OPTIME.
    ENDLOOP.

  ELSE.
    LOOP AT IT_SHOP.
      MOVE: IT_SHOP-SHOP      TO IT_OPTIME-SHOP,
            ZSPM_OPTIME-AJAHR TO IT_OPTIME-AJAHR,
            'MIN'             TO IT_OPTIME-MAUEH.
      APPEND IT_OPTIME.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SELECT_OPTIME_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_OPTIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZMONTH  text
*      -->P_OPTIME  text
*----------------------------------------------------------------------*
FORM SET_IT_OPTIME USING    P_ZMONTH
                            P_OPTIME.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_OPTIME-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN (WA_MONTH) TO <MONTH>.

  MOVE :  P_OPTIME TO <MONTH>.

ENDFORM.                    " SET_IT_OPTIME
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0200 OUTPUT.
*** Internal Table -> Table Control
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_OPTIME INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_OPTIME TO ZSPM_OPTIME.
    SELECT  SINGLE INNAM AS SHTXT
            INTO  ZSPM_OPTIME-SHTXT
            FROM  T024I
            WHERE INGRP  = IT_OPTIME-SHOP.
*    SELECT SINGLE FING AS SHTXT INTO ZSPM_OPTIME-SHTXT
*         FROM T357
*         WHERE BEBER  = IT_OPTIME-SHOP.
  ELSE.
    CLEAR ZSPM_OPTIME.
  ENDIF.

ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0200 OUTPUT.
*--- Internal Table Lines No to Table Contral Lines No.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_OPTIME LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
*--- TABLE CONTROL -> INTERNAL TABLE
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  MOVE-CORRESPONDING ZSPM_OPTIME TO IT_OPTIME.

  MODIFY IT_OPTIME INDEX <TC>-CURRENT_LINE TRANSPORTING
                                             MAUEH
                                             MONTH01 MONTH02
                                             MONTH03 MONTH04
                                             MONTH05 MONTH06
                                             MONTH07 MONTH08
                                             MONTH09 MONTH10
                                             MONTH11 MONTH12.
ENDMODULE.                 " TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_it_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_SHOP.
  CLEAR: IT_SHOP, IT_SHOP[].

  IF ZSPM_PARAM-SHOP = ' '.
***  Select All Shop
    SELECT  DISTINCT INGRP AS SHOP INNAM AS SHTXT
            INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
            FROM  T024I
            WHERE INGRP LIKE 'P%'
            OR    INGRP LIKE 'U%'
            ORDER BY SHOP.

*    SELECT  BEBER AS SHOP
*            FING  AS SHTXT
*                  INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
*                  FROM  T357
*                  ORDER BY SHOP.
  ELSE.
    SELECT  DISTINCT INGRP AS SHOP INNAM AS SHTXT
        INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
        FROM  T024I
        WHERE INGRP = ZSPM_PARAM-SHOP
        ORDER BY SHOP.

*    SELECT BEBER AS SHOP
*           FING  AS SHTXT
*                  INTO CORRESPONDING FIELDS OF TABLE IT_SHOP
*                  FROM  T357
*                  WHERE BEBER  = ZSPM_PARAM-SHOP
*                  ORDER BY SHOP.
  ENDIF.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-003.
  ENDIF.

ENDFORM.                    " set_it_shop
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0200 OUTPUT.
*** Control Input / Display
  PERFORM MODIFY_SCREEN_INPUT.
ENDMODULE.                 " MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_INPUT.

  DATA : WA_CURRENT_DATE LIKE SY-DATUM,
         WA_1MONTH_BEFORE LIKE SY-DATUM.

  DATA : WA_INPUT_FLG(2) TYPE N.

  DATA : WA_INPUT_MONTH(30).

  WA_CURRENT_DATE  = SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            DATE      = WA_CURRENT_DATE
            DAYS      = 0
            MONTHS    = 1
            SIGNUM    = '-'
            YEARS     = 0
       IMPORTING
            CALC_DATE = WA_1MONTH_BEFORE.


  LOOP AT SCREEN .
    IF SCREEN-GROUP1 = 'GR1'.
      IF WA_CURRENT_DATE(4) = ZSPM_PARAM-AJAHR.
        CONCATENATE 'ZSPM_OPTIME-MONTH' WA_CURRENT_DATE+4(2)
                     INTO WA_INPUT_MONTH.
        ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
        IF SCREEN-NAME  = <INPUT_MONTH>.
          SCREEN-INPUT = 1.
        ENDIF.

        CONCATENATE 'ZSPM_OPTIME-MONTH' WA_1MONTH_BEFORE+4(2)
                     INTO WA_INPUT_MONTH.
        ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
        IF SCREEN-NAME  = <INPUT_MONTH>.
          SCREEN-INPUT = 1.
        ENDIF.
      ELSE.
        IF WA_1MONTH_BEFORE(4) = ZSPM_PARAM-AJAHR
           AND WA_1MONTH_BEFORE+4(2) = 12.
          CONCATENATE 'ZSPM_OPTIME-MONTH' WA_1MONTH_BEFORE+4(2)
               INTO WA_INPUT_MONTH.
          ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
          IF SCREEN-NAME  = <INPUT_MONTH>.
            SCREEN-INPUT = 1.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ZSPM_OPTIME-SHOP = SPACE.
        SCREEN-INPUT = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN_INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR: SY-UCOMM.
      PERFORM LEAVE_PROGRAM.

    WHEN 'SAVE'.
      CLEAR: SY-UCOMM.
      PERFORM SAVE_OPTIME_DATA.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
****make title & status..
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_OPTIME_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_OPTIME_DATA.
  DATA: WA_MONTH LIKE FELD-NAME,
        WA_COUNT(2) TYPE N. " LIKE ZTPM_OPTIME-ZMONTH.

  CLEAR: IT_ZTPM_OPTIME, IT_ZTPM_OPTIME[].

  LOOP AT IT_OPTIME.
    DO 12 TIMES.
      WA_COUNT = WA_COUNT + 1.

      CONCATENATE 'IT_OPTIME-MONTH' WA_COUNT INTO WA_MONTH.
      ASSIGN (WA_MONTH) TO <MONTH>.

      MOVE: IT_OPTIME-SHOP   TO IT_ZTPM_OPTIME-SHOP,
            ZSPM_PARAM-AJAHR TO IT_ZTPM_OPTIME-AJAHR,
            WA_COUNT         TO IT_ZTPM_OPTIME-ZMONTH,
            IT_OPTIME-MAUEH  TO IT_ZTPM_OPTIME-MAUEH,
            <MONTH>          TO IT_ZTPM_OPTIME-OPTIME.

      READ TABLE IT_TEMP WITH KEY SHOP   = IT_OPTIME-SHOP
                                  AJAHR  = IT_OPTIME-AJAHR
                                  ZMONTH = IT_OPTIME-ZMONTH.
      IF SY-SUBRC EQ 0.
        MOVE: IT_TEMP-ERDAT TO	IT_ZTPM_OPTIME-ERDAT,
              IT_TEMP-ERZET TO	IT_ZTPM_OPTIME-ERZET,
              IT_TEMP-ERNAM TO	IT_ZTPM_OPTIME-ERNAM.
        MOVE: SY-DATUM TO IT_ZTPM_OPTIME-AEDAT,
              SY-UZEIT TO IT_ZTPM_OPTIME-AEZET,
              SY-UNAME TO IT_ZTPM_OPTIME-AENAM.
      ELSE.
        MOVE: SY-DATUM TO	IT_ZTPM_OPTIME-ERDAT,
              SY-UZEIT TO	IT_ZTPM_OPTIME-ERZET,
              SY-UNAME TO	IT_ZTPM_OPTIME-ERNAM,
              SY-DATUM TO IT_ZTPM_OPTIME-AEDAT,
              SY-UZEIT TO IT_ZTPM_OPTIME-AEZET,
              SY-UNAME TO IT_ZTPM_OPTIME-AENAM.
      ENDIF.
      APPEND IT_ZTPM_OPTIME.
    ENDDO.
    CLEAR: WA_COUNT.
  ENDLOOP.

  MODIFY ZTPM_OPTIME FROM TABLE IT_ZTPM_OPTIME.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    MESSAGE S000(ZMPM) WITH TEXT-005.
  ENDIF.
ENDFORM.                    " SAVE_OPTIME_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_FIELDS.
***** Check Shop (Plant section)
  IF NOT ZSPM_PARAM-SHOP IS INITIAL.

    SELECT SINGLE *
           FROM  T024I
           WHERE INGRP = ZSPM_PARAM-SHOP.

*    SELECT SINGLE *
*                    FROM T357
*                    WHERE BEBER = ZSPM_PARAM-SHOP.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMPM) WITH TEXT-M03 ZSPM_PARAM-SHOP TEXT-006.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_FIELDS

*&---------------------------------------------------------------------*
*&      Module  MAUEH_TIME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MAUEH_TIME INPUT.
  TABLES: T006D.
  DATA: DIMID  LIKE T006D-DIMID.
  DATA: LMAUEH LIKE VIQMEL-MAUEH.
  DATA: BEGIN OF TMEINS OCCURS 0,
          MSEHI LIKE T006-MSEHI,
        END OF TMEINS.

  CLEAR T006D.
  T006D-DIMID = 'TIME'.
  CLEAR LMAUEH.

*--- Function fuer PF4 selektion--------------------------------------*
  CALL FUNCTION 'ME_VALUES_T006'
       EXPORTING
            I_T006D = T006D
       IMPORTING
            E_MEINS = LMAUEH
       TABLES
            T_MEINS = TMEINS.

  IF NOT ( LMAUEH IS INITIAL ).
    ZSPM_OPTIME-MAUEH = LMAUEH.
  ENDIF.


ENDMODULE.                 " MAUEH_TIME  INPUT
*&---------------------------------------------------------------------*
*&      Module  SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SELECT_YEAR INPUT.
  PERFORM SELECT_YEAR.
ENDMODULE.                 " SELECT_YEAR  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_YEAR.
  DATA: WA_SEL_YEAR LIKE  VVIS_SOPTI-SYEAR,
        WA_SEL_OK.
  CALL FUNCTION 'REAL_ESTATE_F4_YEAR'
   EXPORTING
*   I_YEAR              =
     I_POPUP_TITLE       = 'Select Year'
   IMPORTING
      E_YEAR              = WA_SEL_YEAR
      E_SEL_OK            = WA_SEL_OK
            .
  IF WA_SEL_OK EQ 'X'.
    ZSPM_PARAM-AJAHR = WA_SEL_YEAR.
  ENDIF.
ENDFORM.                    " SELECT_YEAR
