************************************************************************
* Program Name      : SAPMZAPM08_PMCO
* Author            : Myoungho Park
* Creation Date     : 2003.10.09.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  Input Planned Maintenance Cost
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZAPM06_PMCO                 .


TYPE-POOLS CXTAB .  "//Table_control Object type pool

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control
FIELD-SYMBOLS: <MONTH> .

TABLES: ZSPM_PARAM,
        ZSPM_PMCO,     "//Number of Planned Maintenance structure
        ZTPM_PLANDCO.  "//Number of Planned Maintenance tbale

DATA: IT_TEMP LIKE ZTPM_PLANDCO OCCURS 0 WITH HEADER LINE,
      IT_PMCO LIKE ZSPM_PMCO OCCURS 0 WITH HEADER LINE,
      IT_ZTPM_PMCO LIKE ZTPM_PLANDCO OCCURS 0 WITH HEADER LINE.


DATA: WA_YEAR LIKE ZSPM_PMCO-AJAHR,
      WA_TITLE(40),
      WA_ZMONTH LIKE ZTPM_PLAN-ZMONTH.

DATA: WA_SHOP  LIKE ZTPM_SHOP-SHOP,
      WA_SHTXT LIKE ZTPM_SHOP-SHTXT.

*** for table control
CONTROLS: TC_0200 TYPE TABLEVIEW USING SCREEN 0200.

DATA : WA_TCNAME LIKE FELD-NAME. "table control name
DATA : WA_SEL.

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
***** default value for year..
  IF ZSPM_PMCO-AJAHR IS INITIAL.
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
*&      Form  EXEC_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXEC_PROGRAM.
**** select data...
  PERFORM SELECT_DATA.

  CONCATENATE ZSPM_PARAM-AJAHR TEXT-001 INTO WA_TITLE.

  WA_SHOP = ZSPM_PARAM-SHOP.
**** get shop text...
  SELECT SINGLE SHTXT INTO WA_SHTXT
                FROM ZTPM_SHOP
                WHERE SHOP = ZSPM_PARAM-SHOP
                AND   SPRAS = SY-LANGU.

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
  CLEAR: IT_TEMP, IT_TEMP[],
         IT_PMCO, IT_PMCO[].

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM  ZTPM_PLANDCO
           WHERE SHOP    = ZSPM_PARAM-SHOP
           AND   AJAHR   = ZSPM_PARAM-AJAHR
           ORDER BY SHOP AJAHR ZMONTH.
  IF SY-SUBRC EQ 0.
**** Maintenance Cost
    LOOP AT IT_TEMP.
      PERFORM SET_IT_COST USING IT_TEMP-ZMONTH
                                IT_TEMP-ZPCOST.
    ENDLOOP.
    MOVE : IT_TEMP-SHOP         TO IT_PMCO-SHOP,
           'Maintenance Cost'    TO IT_PMCO-ZPMTYPE,
           ZSPM_PARAM-AJAHR      TO IT_PMCO-AJAHR,
           IT_TEMP-WAERS         TO IT_PMCO-WAERS.
    APPEND IT_PMCO.

**** Cost Per Uint
    LOOP AT IT_TEMP.
      PERFORM SET_IT_COST USING IT_TEMP-ZMONTH
                                IT_TEMP-ZPUNITC.
    ENDLOOP.
    MOVE : IT_TEMP-SHOP         TO IT_PMCO-SHOP,
           'Cost Per Uint'       TO IT_PMCO-ZPMTYPE,
           ZSPM_PARAM-AJAHR      TO IT_PMCO-AJAHR,
           IT_TEMP-WAERS         TO IT_PMCO-WAERS.
    APPEND IT_PMCO.

  ELSE.
    MOVE :  ZSPM_PARAM-SHOP       TO IT_PMCO-SHOP,
           'Maintenance Cost'     TO IT_PMCO-ZPMTYPE,
            ZSPM_PARAM-AJAHR      TO IT_PMCO-AJAHR,
            'USD'                 TO IT_PMCO-WAERS.
    APPEND IT_PMCO.

    MOVE : ZSPM_PARAM-SHOP       TO IT_PMCO-SHOP,
           'Cost Per Uint'       TO IT_PMCO-ZPMTYPE,
           ZSPM_PARAM-AJAHR      TO IT_PMCO-AJAHR,
           'USD'                 TO IT_PMCO-WAERS.
    APPEND IT_PMCO.
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
*&      Form  SET_IT_MTBT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_ZMONTH  text
*      -->P_IT_TEMP_ZPLAND_INS  text
*----------------------------------------------------------------------*
FORM SET_IT_MTBT USING   P_ZMONTH
                         P_VALUE.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_PMCO-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN (WA_MONTH) TO <MONTH>.
  MOVE :  P_VALUE  TO <MONTH>.
ENDFORM.                    " SET_IT_MTBT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0200 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_PMCO INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_PMCO TO ZSPM_PMCO.
  ELSE.
    CLEAR ZSPM_PMCO.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0200 OUTPUT.
  LOOP AT SCREEN .
    IF SCREEN-GROUP1 = 'GR1'.
      IF ZSPM_PMCO-ZPMTYPE EQ ' '.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
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

  DESCRIBE TABLE IT_PMCO LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  MOVE-CORRESPONDING ZSPM_PMCO TO IT_PMCO.

  MODIFY IT_PMCO INDEX <TC>-CURRENT_LINE TRANSPORTING
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
      PERFORM SAVE_PLAN_DATA.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PLAN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_PLAN_DATA.
  DATA: WA_MONTH    LIKE FELD-NAME,
        WA_COUNT(2) TYPE N.

  CLEAR: IT_ZTPM_PMCO, IT_ZTPM_PMCO[].

  LOOP AT IT_PMCO.
    DO 12 TIMES.
      WA_COUNT = WA_COUNT + 1.

      CONCATENATE 'IT_PMCO-MONTH' WA_COUNT INTO WA_MONTH.
      ASSIGN (WA_MONTH) TO <MONTH>.

      CONCATENATE IT_PMCO-AJAHR WA_COUNT INTO WA_ZMONTH.

      MOVE: ZSPM_PARAM-SHOP   TO IT_ZTPM_PMCO-SHOP,
            IT_PMCO-AJAHR     TO IT_ZTPM_PMCO-AJAHR,
            IT_PMCO-WAERS     TO IT_ZTPM_PMCO-WAERS,
            WA_COUNT          TO IT_ZTPM_PMCO-ZMONTH.

      IF IT_PMCO-ZPMTYPE = 'Maintenance Cost'.
        MOVE: <MONTH>      TO IT_ZTPM_PMCO-ZPCOST.
      ENDIF.

      READ TABLE IT_TEMP WITH KEY AJAHR  = IT_ZTPM_PMCO-AJAHR
                                  ZMONTH = IT_ZTPM_PMCO-ZMONTH.
      IF SY-SUBRC EQ 0.
        MOVE: IT_TEMP-ERDAT  TO	IT_ZTPM_PMCO-ERDAT,
              IT_TEMP-ERZET  TO	IT_ZTPM_PMCO-ERZET,
              IT_TEMP-ERNAM  TO	IT_ZTPM_PMCO-ERNAM.
        MOVE: SY-DATUM       TO      IT_ZTPM_PMCO-AEDAT,
              SY-UZEIT       TO      IT_ZTPM_PMCO-AEZET,
              SY-UNAME       TO      IT_ZTPM_PMCO-AENAM.
      ELSE.
        MOVE: SY-DATUM TO	IT_ZTPM_PMCO-ERDAT,
              SY-UZEIT TO	IT_ZTPM_PMCO-ERZET,
              SY-UNAME TO	IT_ZTPM_PMCO-ERNAM,
              SY-DATUM TO     IT_ZTPM_PMCO-AEDAT,
              SY-UZEIT TO     IT_ZTPM_PMCO-AEZET,
              SY-UNAME TO     IT_ZTPM_PMCO-AENAM.
      ENDIF.
      READ TABLE IT_ZTPM_PMCO WITH KEY AJAHR  = IT_ZTPM_PMCO-AJAHR
                                       ZMONTH = IT_ZTPM_PMCO-ZMONTH.
      IF SY-SUBRC NE 0.
        APPEND IT_ZTPM_PMCO .
      ELSE.
        MOVE: <MONTH>       TO IT_ZTPM_PMCO-ZPUNITC.
        MODIFY IT_ZTPM_PMCO INDEX SY-TABIX
                            TRANSPORTING ZPUNITC.
      ENDIF.
    ENDDO.
    CLEAR: WA_COUNT.
  ENDLOOP.

  MODIFY ZTPM_PLANDCO FROM TABLE IT_ZTPM_PMCO.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMPM) WITH TEXT-M07 TEXT-005.
  ELSE.
    COMMIT WORK.
    MESSAGE S000(ZMPM) WITH TEXT-005 TEXT-M06.
  ENDIF.
ENDFORM.                    " SAVE_PLAN_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_ZMONTH  text
*      -->P_IT_TEMP_ZPCOST  text
*----------------------------------------------------------------------*
FORM SET_IT_COST USING   P_ZMONTH
                          P_COST.
  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_PMCO-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN (WA_MONTH) TO <MONTH>.
  MOVE :  P_COST  TO <MONTH>.

ENDFORM.                    " SET_IT_COST
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
