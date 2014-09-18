************************************************************************
* Program Name      : SAPMZAPM04_MONBD
* Author            : Myoungho Park
* Creation Date     : 2003.08.08.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  Input Planed Monthly Breakdown rate
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 2004.02.11. Myoungho Park
*
*
************************************************************************

REPORT  SAPMZAPM04_MONBD              .

TYPE-POOLS CXTAB .  "//Table_control Object type pool

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control
FIELD-SYMBOLS: <MONTH> .
FIELD-SYMBOLS: <INPUT_MONTH>.

TABLES: ZSPM_PARAM, "//Parameters
        ZSPM_BDMON, "//Monthly average breakdown rate
        ZTPM_MONBD. "//Monthly Breakdown Rate (Planed)


***** INTERANAL TABLE FOR SHOP LIST...
DATA: BEGIN OF IT_SHOP OCCURS 0,
          SHOP  LIKE ZSPM_PARAM-SHOP,
          SHTXT LIKE ZSPM_PARAM-SHTXT,
      END OF IT_SHOP.

DATA: IT_MONBD LIKE ZSPM_BDMON OCCURS 0 WITH HEADER LINE,
      IT_TEMP  LIKE ZTPM_PLANDBD OCCURS 0 WITH HEADER LINE,
      IT_ZTPM_PLANDBD LIKE ZTPM_PLANDBD OCCURS 0 WITH HEADER LINE.

DATA: WA_YEAR LIKE ZSPM_BDMON-AJAHR,
      WA_TITLE(40).

DATA: WA_SHTXT LIKE ZSPM_BDMON-SHTXT.

*** for TABLE CONTROL...
CONTROLS : TC_0200 TYPE TABLEVIEW USING SCREEN 0200.
DATA : WA_SEL.
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
*&      Module  INITIAL_VALUE_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_0100 OUTPUT.
***** DEFAULT VALUE FOR YEAR
  IF ZSPM_PARAM-AJAHR IS INITIAL.
    WA_YEAR = SY-DATUM(4).
    ZSPM_PARAM-AJAHR = WA_YEAR.
  ENDIF.
ENDMODULE.                 " INITIAL_VALUE_0100  OUTPUT
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
*** GET SHOP LIST..
  PERFORM SET_IT_SHOP.
****  SELECT DATA...
  PERFORM SELECT_DATA.

  CONCATENATE ZSPM_PARAM-AJAHR TEXT-001 INTO WA_TITLE.

  CLEAR: WA_SHTXT.

  SELECT SINGLE INNAM AS SHTXT INTO WA_SHTXT
          FROM  T024I
          WHERE INGRP = ZSPM_PARAM-SHOP.

*  SELECT SINGLE FING AS SHTXT INTO WA_SHTXT
*              FROM  T357
*              WHERE BEBER = ZSPM_PARAM-SHOP.

  CALL SCREEN '0200'.
ENDFORM.                    " EXEC_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA.
  CLEAR: IT_TEMP,  IT_TEMP[],
         IT_MONBD, IT_MONBD[].

  IF ZSPM_PARAM-SHOP = ' '.
    SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
             FROM  ZTPM_PLANDBD
             WHERE AJAHR = ZSPM_PARAM-AJAHR
             ORDER BY SHOP AJAHR ZMONTH.
  ELSE.
    SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
             FROM  ZTPM_PLANDBD
             WHERE SHOP    = ZSPM_PARAM-SHOP
             AND   AJAHR   = ZSPM_PARAM-AJAHR
             ORDER BY SHOP AJAHR ZMONTH.
  ENDIF.
  IF SY-SUBRC EQ 0.
    LOOP AT IT_SHOP.

      LOOP AT IT_TEMP WHERE SHOP = IT_SHOP-SHOP.

        PERFORM SET_IT_MONBD USING IT_TEMP-ZMONTH
                                   IT_TEMP-AVRATE.
      ENDLOOP.
      MOVE : ZSPM_PARAM-AJAHR TO IT_MONBD-AJAHR,
             IT_SHOP-SHOP     TO IT_MONBD-SHOP.

      APPEND IT_MONBD.
      CLEAR: IT_MONBD.
    ENDLOOP.
  ELSE.
    LOOP AT IT_SHOP.
      MOVE : ZSPM_PARAM-AJAHR  TO IT_MONBD-AJAHR,
             IT_SHOP-SHOP      TO IT_MONBD-SHOP.
      APPEND IT_MONBD.
      CLEAR: IT_MONBD.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SELECT_DATA
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
*&      Module  INITIAL_VALUE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_VALUE_0200 OUTPUT.

ENDMODULE.                 " INITIAL_VALUE_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0200 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_MONBD INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_MONBD TO ZSPM_BDMON.
    SELECT  SINGLE INNAM AS SHTXT
            INTO  ZSPM_BDMON-SHTXT
            FROM  T024I
            WHERE INGRP  = IT_MONBD-SHOP.
*    SELECT SINGLE FING AS SHTXT INTO ZSPM_BDMON-SHTXT
*            FROM T357
*            WHERE BEBER  = IT_MONBD-SHOP.
  ELSE.
    CLEAR ZSPM_BDMON.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0200 OUTPUT.
  PERFORM MODIFY_SCREEN_INPUT.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0200 OUTPUT.
*--- Internal Table Lines Number to Table Contral Lines Number.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_MONBD LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
*--- TABLE CONTROL content move to  INTERNAL TABLE.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  MOVE-CORRESPONDING ZSPM_BDMON TO IT_MONBD.

  MODIFY IT_MONBD INDEX <TC>-CURRENT_LINE TRANSPORTING
                                           MONTH01 MONTH02
                                           MONTH03 MONTH04
                                           MONTH05 MONTH06
                                           MONTH07 MONTH08
                                           MONTH09 MONTH10
                                           MONTH11 MONTH12.


ENDMODULE.                 " TABLE_CONTROL_INPUT_0200  INPUT
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
***   SAVE DATA...
      PERFORM SAVE_DATA.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: WA_MONTH    LIKE FELD-NAME,
        WA_COUNT(2) TYPE N.

  LOOP AT IT_MONBD.
    DO 12 TIMES.
      WA_COUNT = WA_COUNT + 1.

      CONCATENATE 'IT_MONBD-MONTH' WA_COUNT INTO WA_MONTH.
      ASSIGN (WA_MONTH) TO <MONTH>.

      MOVE: IT_MONBD-SHOP      TO IT_ZTPM_PLANDBD-SHOP,
            IT_MONBD-AJAHR     TO IT_ZTPM_PLANDBD-AJAHR,
            WA_COUNT           TO IT_ZTPM_PLANDBD-ZMONTH,
            <MONTH>            TO IT_ZTPM_PLANDBD-AVRATE.

      READ TABLE IT_TEMP  WITH KEY SHOP   = IT_ZTPM_PLANDBD-SHOP
                                   AJAHR  = IT_ZTPM_PLANDBD-AJAHR
                                   ZMONTH = IT_ZTPM_PLANDBD-ZMONTH.
      IF SY-SUBRC EQ 0.
        MOVE: IT_TEMP-ERDAT TO	IT_ZTPM_PLANDBD-ERDAT,
              IT_TEMP-ERZET TO	IT_ZTPM_PLANDBD-ERZET,
              IT_TEMP-ERNAM TO	IT_ZTPM_PLANDBD-ERNAM.
        MOVE: SY-DATUM      TO      IT_ZTPM_PLANDBD-AEDAT,
              SY-UZEIT      TO      IT_ZTPM_PLANDBD-AEZET,
              SY-UNAME      TO      IT_ZTPM_PLANDBD-AENAM.
      ELSE.
        MOVE: SY-DATUM     TO	     IT_ZTPM_PLANDBD-ERDAT,
              SY-UZEIT     TO	     IT_ZTPM_PLANDBD-ERZET,
              SY-UNAME     TO	     IT_ZTPM_PLANDBD-ERNAM,
              SY-DATUM     TO      IT_ZTPM_PLANDBD-AEDAT,
              SY-UZEIT     TO      IT_ZTPM_PLANDBD-AEZET,
              SY-UNAME     TO      IT_ZTPM_PLANDBD-AENAM.
      ENDIF.
      APPEND IT_ZTPM_PLANDBD.
    ENDDO.
    CLEAR: WA_COUNT.
  ENDLOOP.

  MODIFY ZTPM_PLANDBD FROM TABLE IT_ZTPM_PLANDBD.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMPM) WITH TEXT-M07.
  ELSE.
    COMMIT WORK.
    MESSAGE S000(ZMPM) WITH TEXT-M06.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_MONBD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_ZMONTH  text
*      -->P_IT_TEMP_AVRATE  text
*      -->P_APPEND  text
*      -->P_IT_MONBD  text
*----------------------------------------------------------------------*
FORM SET_IT_MONBD USING    P_ZMONTH
                           P_AVRATE.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_MONBD-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN (WA_MONTH) TO <MONTH>.
  MOVE :  P_AVRATE  TO <MONTH>.
ENDFORM.                    " SET_IT_MONBD
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
*&---------------------------------------------------------------------*
*&      Form  SET_IT_SHOP
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
ENDFORM.                    " SET_IT_SHOP
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_SCREEN_INPUT.
  LOOP AT SCREEN .
    IF SCREEN-GROUP1 = 'GR1'.
      IF ZSPM_BDMON-SHOP = SPACE.
        SCREEN-INPUT = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*  DATA : WA_CURRENT_DATE LIKE SY-DATUM,
*         WA_1MONTH_BEFORE LIKE SY-DATUM.
*
*  DATA : WA_INPUT_FLG(2) TYPE N.
*
*  DATA : WA_INPUT_MONTH(30).
*
*  WA_CURRENT_DATE  = SY-DATUM.
*
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*       EXPORTING
*            DATE      = WA_CURRENT_DATE
*            DAYS      = 0
*            MONTHS    = 1
*            SIGNUM    = '-'
*            YEARS     = 0
*       IMPORTING
*            CALC_DATE = WA_1MONTH_BEFORE.
*
*
*  LOOP AT SCREEN .
*    IF SCREEN-GROUP1 = 'GR1'.
*      IF WA_CURRENT_DATE(4) = ZSPM_PARAM-AJAHR.
*        CONCATENATE 'ZSPM_BDMON-MONTH' WA_CURRENT_DATE+4(2)
*                     INTO WA_INPUT_MONTH.
*        ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
*        IF SCREEN-NAME  = <INPUT_MONTH>.
*          SCREEN-INPUT = 1.
*        ENDIF.
*
*        CONCATENATE 'ZSPM_BDMON-MONTH' WA_1MONTH_BEFORE+4(2)
*                     INTO WA_INPUT_MONTH.
*        ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
*        IF SCREEN-NAME  = <INPUT_MONTH>.
*          SCREEN-INPUT = 1.
*        ENDIF.
*      ELSE.
*        IF WA_1MONTH_BEFORE(4) = ZSPM_PARAM-AJAHR
*           AND WA_1MONTH_BEFORE+4(2) = 12.
*          CONCATENATE 'ZSPM_BDMON-MONTH' WA_1MONTH_BEFORE+4(2)
*               INTO WA_INPUT_MONTH.
*          ASSIGN WA_INPUT_MONTH TO <INPUT_MONTH>.
*          IF SCREEN-NAME  = <INPUT_MONTH>.
*            SCREEN-INPUT = 1.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      IF ZSPM_BDMON-SHOP = SPACE.
*        SCREEN-INPUT = 0.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN_INPUT
