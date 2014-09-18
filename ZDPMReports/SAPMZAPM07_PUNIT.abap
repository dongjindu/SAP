************************************************************************
* Program Name      : SAPMZAPM07_PUNIT
* Author            : Myoungho Park
* Creation Date     : 2003.10.09.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  Input Number of Production Unit
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  SAPMZAPM07_PUNIT             .

TYPE-POOLS CXTAB .  "//Table_control Object type pool

TABLES: ZSPM_PARAM,
        ZSPM_PUNIT,  "//Operation Time sturcture
        ZTPM_PUNIT.  "//Operation Time table

*** internal table for shop list..
DATA: BEGIN OF IT_SHOP OCCURS 0,
          SHOP  LIKE ZTPM_SHOP-SHOP,
          SHTXT LIKE ZTPM_SHOP-SHTXT,
      END OF IT_SHOP.

DATA: IT_SHOP_ALL LIKE IT_SHOP OCCURS 0 WITH HEADER LINE.
DATA: IT_TEMP LIKE ZTPM_PUNIT OCCURS 0 WITH HEADER LINE,
      IT_PUNIT LIKE ZSPM_PUNIT OCCURS 0 WITH HEADER LINE,
      IT_ZTPM_PUNIT LIKE ZTPM_PUNIT OCCURS 0 WITH HEADER LINE.

DATA: WA_PROGNAME LIKE SY-REPID,
      WA_DYNNUM LIKE SY-DYNNR.

DATA: WA_YEAR LIKE ZSPM_PUNIT-AJAHR,
      WA_TITLE(40).

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control
FIELD-SYMBOLS: <MONTH> .
FIELD-SYMBOLS: <INPUT_MONTH>.

***** For Table Control
CONTROLS: TC_0200 TYPE TABLEVIEW USING SCREEN 0200.

DATA : WA_TCNAME LIKE FELD-NAME. "table control name


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_HELP_SHOP  INPUT
*&---------------------------------------------------------------------*
*       text
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
            DYNPROFIELD = 'ZSPM_PUNIT-SHOP'
            VALUE_ORG   = 'S'
       TABLES
            VALUE_TAB   = IT_SHOP_ALL.

ENDMODULE.                 " F4_HELP_SHOP  INPUT
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_0100 OUTPUT.
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
  PERFORM SET_IT_SHOP.
  PERFORM SELECT_PUNIT_DATA.

  CALL SCREEN '0200'.
ENDFORM.                    " EXEC_PROGRAM
*&---------------------------------------------------------------------*
*&      Module  INITIAL_VALUE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_0200 OUTPUT.
  CONCATENATE ZSPM_PUNIT-AJAHR TEXT-001 TEXT-002  INTO WA_TITLE.
ENDMODULE.                 " INITIAL_VALUE_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_PUNIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_PUNIT_DATA.

  CLEAR: IT_TEMP, IT_TEMP[],
         IT_PUNIT, IT_PUNIT[].

  IF ZSPM_PUNIT-SHOP = ' '.
    SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
             FROM  ZTPM_PUNIT
             WHERE AJAHR = ZSPM_PARAM-AJAHR
             ORDER BY SHOP AJAHR ZMONTH.
  ELSE.
    SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM  ZTPM_PUNIT
           WHERE SHOP  = ZSPM_PARAM-SHOP
           AND   AJAHR = ZSPM_PARAM-AJAHR
           ORDER BY SHOP AJAHR ZMONTH.
  ENDIF.

**--/ modify for internal table item : no-exit shop items  in ZTPM_PUNIT
**--    - sllee : 05/13/2004    - Start

  IF SY-SUBRC EQ 0.

    LOOP AT IT_SHOP.
      CLEAR IT_TEMP.

      READ TABLE IT_TEMP WITH KEY SHOP = IT_SHOP-SHOP.

      IF SY-SUBRC = 0.
        LOOP AT IT_TEMP WHERE SHOP = IT_SHOP-SHOP.
          PERFORM SET_IT_PUNIT USING IT_TEMP-ZMONTH
                                      IT_TEMP-ZUNIT.
        ENDLOOP.
        MOVE: IT_TEMP-SHOP  TO IT_PUNIT-SHOP,
              IT_TEMP-AJAHR TO IT_PUNIT-AJAHR,
              IT_TEMP-MEINS TO IT_PUNIT-MEINS.
        APPEND IT_PUNIT.
      ELSE.
        MOVE: IT_SHOP-SHOP      TO IT_PUNIT-SHOP,
              ZSPM_PUNIT-AJAHR  TO IT_PUNIT-AJAHR,
              'EA'              TO IT_PUNIT-MEINS.
        APPEND IT_PUNIT.
      ENDIF.

    ENDLOOP.

  ELSE.
    LOOP AT IT_SHOP.
      MOVE: IT_SHOP-SHOP      TO IT_PUNIT-SHOP,
            ZSPM_PUNIT-AJAHR  TO IT_PUNIT-AJAHR,
            'EA'              TO IT_PUNIT-MEINS.
      APPEND IT_PUNIT.
    ENDLOOP.
  ENDIF.

**--    - sllee : 05/13/2004    - End

ENDFORM.                    " SELECT_PUNIT_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_PUNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZMONTH  text
*      -->P_PUNIT  text
*----------------------------------------------------------------------*
FORM SET_IT_PUNIT USING    P_ZMONTH
                            P_PUNIT.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_PUNIT-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN (WA_MONTH) TO <MONTH>.

  MOVE :  P_PUNIT TO <MONTH>.

ENDFORM.                    " SET_IT_PUNIT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0200 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_PUNIT INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_PUNIT TO ZSPM_PUNIT.
    SELECT  SINGLE INNAM AS SHTXT
            INTO  ZSPM_PUNIT-SHTXT
            FROM  T024I
            WHERE INGRP  = IT_PUNIT-SHOP.
  ELSE.
    CLEAR ZSPM_PUNIT.
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

  DESCRIBE TABLE IT_PUNIT LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
*--- TABLE CONTROL ³»¿ëÀ» INTERNAL TABLE·Î ¿È±è.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  MOVE-CORRESPONDING ZSPM_PUNIT TO IT_PUNIT.

  MODIFY IT_PUNIT INDEX <TC>-CURRENT_LINE TRANSPORTING
                                             MEINS
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
*** GET SHOP TEXT....
  IF ZSPM_PARAM-SHOP = ' '.

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
    MESSAGE E000(ZMPM) WITH TEXT-M03 TEXT-M04.
  ENDIF.


ENDFORM.                    " set_it_shop
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0200 OUTPUT.
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
*  LOOP AT SCREEN .
*    IF SCREEN-GROUP1 = 'GR1'.
*      IF ZSPM_PUNIT-SHOP = SPACE.
*        SCREEN-INPUT = 0.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.

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
        CONCATENATE 'ZSPM_PUNIT-MONTH' WA_CURRENT_DATE+4(2)
                     INTO WA_INPUT_MONTH.
        ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
        IF SCREEN-NAME  = <INPUT_MONTH>.
          SCREEN-INPUT = 1.
        ENDIF.

        CONCATENATE 'ZSPM_PUNIT-MONTH' WA_1MONTH_BEFORE+4(2)
                     INTO WA_INPUT_MONTH.
        ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
        IF SCREEN-NAME  = <INPUT_MONTH>.
          SCREEN-INPUT = 1.
        ENDIF.
      ELSE.
        IF WA_1MONTH_BEFORE(4) = ZSPM_PARAM-AJAHR
           AND WA_1MONTH_BEFORE+4(2) = 12.
          CONCATENATE 'ZSPM_PUNIT-MONTH' WA_1MONTH_BEFORE+4(2)
               INTO WA_INPUT_MONTH.
          ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
          IF SCREEN-NAME  = <INPUT_MONTH>.
            SCREEN-INPUT = 1.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ZSPM_PUNIT-SHOP = SPACE.
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
      PERFORM SAVE_PUNIT_DATA.

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
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PUNIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_PUNIT_DATA.
  DATA: WA_MONTH LIKE FELD-NAME,
        WA_COUNT(2) TYPE N. " LIKE ZTPM_PUNIT-ZMONTH.

  CLEAR: IT_ZTPM_PUNIT, IT_ZTPM_PUNIT[].

  LOOP AT IT_PUNIT.
    DO 12 TIMES.
      WA_COUNT = WA_COUNT + 1.

      CONCATENATE 'IT_PUNIT-MONTH' WA_COUNT INTO WA_MONTH.
      ASSIGN (WA_MONTH) TO <MONTH>.

      MOVE: IT_PUNIT-SHOP   TO IT_ZTPM_PUNIT-SHOP,
            IT_PUNIT-AJAHR  TO IT_ZTPM_PUNIT-AJAHR,
            WA_COUNT         TO IT_ZTPM_PUNIT-ZMONTH,
            <MONTH>          TO IT_ZTPM_PUNIT-ZUNIT.

      READ TABLE IT_TEMP WITH KEY SHOP   = IT_PUNIT-SHOP
                                  AJAHR  = IT_PUNIT-AJAHR
                                  ZMONTH = IT_PUNIT-ZMONTH.
      IF SY-SUBRC EQ 0.
        MOVE: IT_TEMP-ERDAT TO	IT_ZTPM_PUNIT-ERDAT,
              IT_TEMP-ERZET TO	IT_ZTPM_PUNIT-ERZET,
              IT_TEMP-ERNAM TO	IT_ZTPM_PUNIT-ERNAM.
        MOVE: SY-DATUM TO IT_ZTPM_PUNIT-AEDAT,
              SY-UZEIT TO IT_ZTPM_PUNIT-AEZET,
              SY-UNAME TO IT_ZTPM_PUNIT-AENAM.
      ELSE.
        MOVE: SY-DATUM TO	IT_ZTPM_PUNIT-ERDAT,
              SY-UZEIT TO	IT_ZTPM_PUNIT-ERZET,
              SY-UNAME TO	IT_ZTPM_PUNIT-ERNAM,
              SY-DATUM TO IT_ZTPM_PUNIT-AEDAT,
              SY-UZEIT TO IT_ZTPM_PUNIT-AEZET,
              SY-UNAME TO IT_ZTPM_PUNIT-AENAM.
      ENDIF.
      APPEND IT_ZTPM_PUNIT.
    ENDDO.
    CLEAR: WA_COUNT.
  ENDLOOP.

  MODIFY ZTPM_PUNIT FROM TABLE IT_ZTPM_PUNIT.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    MESSAGE S000(ZMPM) WITH TEXT-005.
  ENDIF.
ENDFORM.                    " SAVE_PUNIT_DATA
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
