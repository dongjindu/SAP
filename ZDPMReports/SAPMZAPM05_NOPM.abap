
************************************************************************
* Program Name      : SAPMZAPM05_NOPM
* Author            : Myoungho Park
* Creation Date     : 2003.09.23.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :  Input Number of Planed Preventive Maintenance
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  SAPMZAPM05_NOPM                 .


TYPE-POOLS CXTAB .  "//Table_control Object type pool

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control
FIELD-SYMBOLS: <MONTH> .

TABLES: ZSPM_PLAN, "//Number of Planned Maintenance
        ZTPM_PLAN. "//Number of Planned Maintenance

DATA: IT_TEMP LIKE ZTPM_PLAN OCCURS 0 WITH HEADER LINE,
      IT_PLAN LIKE ZSPM_PLAN OCCURS 0 WITH HEADER LINE,
      IT_ZTPM_PLAN LIKE ZTPM_PLAN OCCURS 0 WITH HEADER LINE.

DATA: WA_YEAR LIKE ZSPM_PLAN-AJAHR,
      WA_TITLE(40),
      WA_ZMONTH LIKE ZTPM_PLAN-ZMONTH.

DATA: WA_SHOP  LIKE ZTPM_SHOP-SHOP,
      WA_SHTXT LIKE ZTPM_SHOP-SHTXT.

*** FOR TABLE CONTROL....
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
*** DEFAULT VALUE FOR YEAR...
  IF ZSPM_PLAN-AJAHR IS INITIAL.
    WA_YEAR = SY-DATUM(4).
    ZSPM_PLAN-AJAHR = WA_YEAR.
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
****  EXECUTE PROGRAM..
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

  PERFORM SELECT_DATA.

  CONCATENATE ZSPM_PLAN-AJAHR TEXT-001 INTO WA_TITLE.

  WA_SHOP = ZSPM_PLAN-SHOP.

  SELECT SINGLE SHTXT INTO WA_SHTXT
                FROM ZTPM_SHOP
                WHERE SHOP = ZSPM_PLAN-SHOP
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
         IT_PLAN, IT_PLAN[].

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM  ZTPM_PLAN
           WHERE SHOP    = ZSPM_PLAN-SHOP
           AND   AJAHR   = ZSPM_PLAN-AJAHR
           ORDER BY SHOP AJAHR ZMONTH.
  IF SY-SUBRC EQ 0.
**** Routine Inspection
    LOOP AT IT_TEMP.
      PERFORM SET_IT_MAIN USING IT_TEMP-ZMONTH
                                IT_TEMP-ZPLAND_INS.
    ENDLOOP.
    MOVE : 'Routine Inspection'  TO IT_PLAN-ZPMTYPE,
           IT_TEMP-AJAHR         TO IT_PLAN-AJAHR.
    APPEND IT_PLAN.

**** Planned Task
    LOOP AT IT_TEMP.
      PERFORM SET_IT_MAIN USING IT_TEMP-ZMONTH
                                  IT_TEMP-ZPLAND_TSK.
    ENDLOOP.
    MOVE : 'Planned Task'   TO IT_PLAN-ZPMTYPE,
           IT_TEMP-AJAHR    TO IT_PLAN-AJAHR.
    APPEND IT_PLAN.
  ELSE.
    MOVE : 'Routine Inspection'  TO IT_PLAN-ZPMTYPE,
            ZSPM_PLAN-AJAHR        TO IT_PLAN-AJAHR.
    APPEND IT_PLAN.

    MOVE : 'Planned Task'   TO IT_PLAN-ZPMTYPE,
           ZSPM_PLAN-AJAHR    TO IT_PLAN-AJAHR.
    APPEND IT_PLAN.
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
FORM SET_IT_MAIN USING   P_ZMONTH
                         P_VALUE.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_PLAN-MONTH' P_ZMONTH INTO WA_MONTH.
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

  READ TABLE IT_PLAN INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_PLAN TO ZSPM_PLAN.
  ELSE.
    CLEAR ZSPM_PLAN.
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
      IF ZSPM_PLAN-ZPMTYPE EQ ' '.
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

  DESCRIBE TABLE IT_PLAN LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  MOVE-CORRESPONDING ZSPM_PLAN TO IT_PLAN.

  MODIFY IT_PLAN INDEX <TC>-CURRENT_LINE TRANSPORTING
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

  CLEAR: IT_ZTPM_PLAN, IT_ZTPM_PLAN[].

  LOOP AT IT_PLAN.
    DO 12 TIMES.
      WA_COUNT = WA_COUNT + 1.

      CONCATENATE 'IT_PLAN-MONTH' WA_COUNT INTO WA_MONTH.
      ASSIGN (WA_MONTH) TO <MONTH>.

      CONCATENATE IT_PLAN-AJAHR WA_COUNT INTO WA_ZMONTH.

      MOVE: WA_SHOP        TO IT_ZTPM_PLAN-SHOP,
            IT_PLAN-AJAHR  TO IT_ZTPM_PLAN-AJAHR,
            WA_COUNT       TO IT_ZTPM_PLAN-ZMONTH.

      IF IT_PLAN-ZPMTYPE = 'Routine Inspection'.
        MOVE: <MONTH>        TO IT_ZTPM_PLAN-ZPLAND_INS.
*      ELSE.
*        MOVE: <MONTH>        TO IT_ZTPM_PLAN-ZPLAND_TSK.
      ENDIF.

      READ TABLE IT_TEMP WITH KEY AJAHR  = IT_ZTPM_PLAN-AJAHR
                                  ZMONTH = IT_ZTPM_PLAN-ZMONTH.
      IF SY-SUBRC EQ 0.
        MOVE: IT_TEMP-ERDAT TO	IT_ZTPM_PLAN-ERDAT,
              IT_TEMP-ERZET TO	IT_ZTPM_PLAN-ERZET,
              IT_TEMP-ERNAM TO	IT_ZTPM_PLAN-ERNAM.
        MOVE: SY-DATUM      TO      IT_ZTPM_PLAN-AEDAT,
              SY-UZEIT      TO      IT_ZTPM_PLAN-AEZET,
              SY-UNAME      TO      IT_ZTPM_PLAN-AENAM.
      ELSE.
        MOVE: SY-DATUM TO	IT_ZTPM_PLAN-ERDAT,
              SY-UZEIT TO	IT_ZTPM_PLAN-ERZET,
              SY-UNAME TO	IT_ZTPM_PLAN-ERNAM,
              SY-DATUM TO     IT_ZTPM_PLAN-AEDAT,
              SY-UZEIT TO     IT_ZTPM_PLAN-AEZET,
              SY-UNAME TO     IT_ZTPM_PLAN-AENAM.
      ENDIF.
      READ TABLE IT_ZTPM_PLAN WITH KEY AJAHR  = IT_ZTPM_PLAN-AJAHR
                                       ZMONTH = IT_ZTPM_PLAN-ZMONTH.
      IF SY-SUBRC NE 0.
        APPEND IT_ZTPM_PLAN .
      ELSE.
        MOVE: <MONTH>        TO IT_ZTPM_PLAN-ZPLAND_TSK.
        MODIFY IT_ZTPM_PLAN INDEX SY-TABIX
                            TRANSPORTING ZPLAND_TSK.
      ENDIF.
    ENDDO.
    CLEAR: WA_COUNT.
  ENDLOOP.

  MODIFY ZTPM_PLAN FROM TABLE IT_ZTPM_PLAN.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    COMMIT WORK.
    MESSAGE S000(ZMPM) WITH TEXT-005.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " SAVE_PLAN_DATA
