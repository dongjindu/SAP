
************************************************************************
* Program Name      : SAPMZAPM02_MTBT
* Author            : Myoungho, Park
* Creation Date     : 2003.08.20.
* Specifications By : Myoungho, Park
* Development Request No :
* Addl Documentation:
* Description       : Input Planed MTTR / MTBF
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 03.08.20. Myoungho, Park
* 03.10.20.Myoungho, Park
*
************************************************************************


REPORT  SAPMZAPM02_MTBT.

TYPE-POOLS CXTAB .  "//Table_control Object type pool

FIELD-SYMBOLS: <TC>  TYPE CXTAB_CONTROL.  "for table control
FIELD-SYMBOLS: <MONTH> .

TABLES: ZSPM_PARAM,  "//parameters
*        T357,        "//plant section(shop)
        T024I,       "//planner groups(shop)
        ZSPM_MTBT,   "//Monthly MTTR/MTBF ( planed, actual )
        ZTPM_MTBT.   "//Monthly MTTR/MTBF ( planed, actual )

****  MTTR / MTBF
DATA: BEGIN OF IT_TYPE OCCURS 0,
          ZMTBT LIKE ZSPM_MTBT-ZMTBT,
      END OF IT_TYPE.


DATA: IT_TEMP LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE,
      IT_MTBT LIKE ZSPM_MTBT OCCURS 0 WITH HEADER LINE,
      IT_ZTPM_MTBT LIKE ZTPM_MTBT OCCURS 0 WITH HEADER LINE.

DATA: WA_YEAR LIKE ZSPM_MTBT-AJAHR,
      WA_TITLE(40).

DATA: WA_SHOP LIKE ZSPM_MTBT-SHOP,
      WA_SHTXT LIKE ZSPM_MTBT-SHTXT.

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
**** CHECK FIELDS
      PERFORM CHECK_FIELDS.
**** RUN PROGRAM
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

**** SELECT DATA...
  PERFORM SELECT_MTBT_DATA.

  CONCATENATE ZSPM_PARAM-AJAHR TEXT-001 INTO WA_TITLE.

  WA_SHOP = ZSPM_PARAM-SHOP.

  CALL SCREEN '0200'.
ENDFORM.                    " EXEC_PROGRAM

*&---------------------------------------------------------------------*
*&      Form  SELECT_MTBT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_MTBT_DATA.
  CLEAR: IT_TEMP, IT_TEMP[],
         IT_MTBT, IT_MTBT[],
         IT_TYPE, IT_TYPE[].

*** APPEND TYPE
  IT_TYPE-ZMTBT ='MTTR'.
  APPEND IT_TYPE.

  IT_TYPE-ZMTBT = 'MTBF'.
  APPEND IT_TYPE.

  SELECT * INTO  CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM  ZTPM_MTBT
           WHERE SHOP    = ZSPM_PARAM-SHOP
           AND   AJAHR   = ZSPM_PARAM-AJAHR
*           AND   ZAPTYPE = 'P'
           ORDER BY SHOP AJAHR ZMONTH.
  IF SY-SUBRC EQ 0.
    LOOP AT IT_TYPE.
      LOOP AT IT_TEMP WHERE ZMTBT = IT_TYPE-ZMTBT.
        PERFORM SET_IT_MTBT USING IT_TEMP-ZMONTH
                                  IT_TEMP-AVRATE.
      ENDLOOP.
      MOVE : IT_TEMP-ZMTBT    TO IT_MTBT-ZMTBT,
*             IT_TEMP-MAUEH    TO IT_MTBT-MAUEH,
             IT_TEMP-AJAHR    TO IT_MTBT-AJAHR.
      APPEND IT_MTBT.
    ENDLOOP.
  ELSE.
    LOOP AT IT_TYPE.
      MOVE : IT_TYPE-ZMTBT     TO IT_MTBT-ZMTBT,
*             'MIN'             TO IT_MTBT-MAUEH,
             ZSPM_MTBT-AJAHR   TO IT_MTBT-AJAHR.
      APPEND IT_MTBT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " SELECT_MTBT_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_IT_MTBT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_ZMONTH  text
*      -->P_IT_TEMP_MTBT  text
*----------------------------------------------------------------------*
FORM SET_IT_MTBT USING    P_ZMONTH
                          P_AVRATE.

  DATA: WA_MONTH LIKE FELD-NAME.

  CONCATENATE 'IT_MTBT-MONTH' P_ZMONTH INTO WA_MONTH.
  ASSIGN (WA_MONTH) TO <MONTH>.
  MOVE :  P_AVRATE  TO <MONTH>.
ENDFORM.                    " SET_IT_MTBT
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
*&      Module  TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_OUTPUT_0200 OUTPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  READ TABLE IT_MTBT INDEX <TC>-CURRENT_LINE .
  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING IT_MTBT TO ZSPM_MTBT.
  ELSE.
    CLEAR ZSPM_MTBT.
  ENDIF.
ENDMODULE.                 " TABLE_CONTROL_OUTPUT_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_LINES_0200 OUTPUT.
*--- Internal Table Lines Number to Table Contral Lines Number.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.                "not headerline

  DESCRIBE TABLE IT_MTBT LINES <TC>-LINES.
ENDMODULE.                 " TABLE_CONTROL_LINES_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TABLE_CONTROL_INPUT_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TABLE_CONTROL_INPUT_0200 INPUT.
  CONCATENATE 'TC_' SY-DYNNR INTO WA_TCNAME.
  ASSIGN (WA_TCNAME) TO <TC>.

  MOVE-CORRESPONDING ZSPM_MTBT TO IT_MTBT.

  MODIFY IT_MTBT INDEX <TC>-CURRENT_LINE TRANSPORTING
                                           ZMTBT
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
      PERFORM SAVE_MTBT_DATA.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_MTBT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_MTBT_DATA.
  DATA: WA_MONTH    LIKE FELD-NAME,
        WA_COUNT(2) TYPE N.

  CLEAR: IT_ZTPM_MTBT, IT_ZTPM_MTBT[].

  LOOP AT IT_MTBT.
    DO 12 TIMES.
      WA_COUNT = WA_COUNT + 1.

      CONCATENATE 'IT_MTBT-MONTH' WA_COUNT INTO WA_MONTH.
      ASSIGN (WA_MONTH) TO <MONTH>.

      MOVE: WA_SHOP           TO IT_ZTPM_MTBT-SHOP,
            ZSPM_PARAM-AJAHR  TO IT_ZTPM_MTBT-AJAHR,
            WA_COUNT          TO IT_ZTPM_MTBT-ZMONTH,
            IT_MTBT-ZMTBT     TO IT_ZTPM_MTBT-ZMTBT,
*            IT_MTBT-MAUEH    TO IT_ZTPM_MTBT-MAUEH,
*            'P'               TO IT_ZTPM_MTBT-ZAPTYPE,
            <MONTH>           TO IT_ZTPM_MTBT-AVRATE.

      READ TABLE IT_TEMP WITH KEY SHOP   = IT_MTBT-SHOP
                                  ZMTBT  = IT_MTBT-ZMTBT
                                  AJAHR  = IT_MTBT-AJAHR
                                  ZMONTH = IT_MTBT-ZMONTH.
      IF SY-SUBRC EQ 0.
        MOVE: IT_TEMP-ERDAT TO	IT_ZTPM_MTBT-ERDAT,
              IT_TEMP-ERZET TO	IT_ZTPM_MTBT-ERZET,
              IT_TEMP-ERNAM TO	IT_ZTPM_MTBT-ERNAM.
        MOVE: SY-DATUM TO IT_ZTPM_MTBT-AEDAT,
              SY-UZEIT TO IT_ZTPM_MTBT-AEZET,
              SY-UNAME TO IT_ZTPM_MTBT-AENAM.
      ELSE.
        MOVE: SY-DATUM TO	IT_ZTPM_MTBT-ERDAT,
              SY-UZEIT TO	IT_ZTPM_MTBT-ERZET,
              SY-UNAME TO	IT_ZTPM_MTBT-ERNAM,
              SY-DATUM TO IT_ZTPM_MTBT-AEDAT,
              SY-UZEIT TO IT_ZTPM_MTBT-AEZET,
              SY-UNAME TO IT_ZTPM_MTBT-AENAM.
      ENDIF.
      APPEND IT_ZTPM_MTBT.
    ENDDO.
    CLEAR: WA_COUNT.
  ENDLOOP.

  MODIFY ZTPM_MTBT FROM TABLE IT_ZTPM_MTBT.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000(ZMPM) WITH TEXT-004.
  ELSE.
    COMMIT WORK.
    MESSAGE S000(ZMPM) WITH TEXT-005.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.                    " SAVE_MTBT_DATA
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
*&      Form  CHECK_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_FIELDS.
  IF ZSPM_PARAM-SHOP IS INITIAL.
    MESSAGE E000(ZMPM) WITH TEXT-002.
  ENDIF.
***  CHECK SHOP (PLANT SECTION)
  IF NOT ZSPM_PARAM-SHOP IS INITIAL.
    SELECT SINGLE  INNAM INTO WA_SHTXT
                    FROM T024I
                    WHERE INGRP = ZSPM_PARAM-SHOP.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZMPM) WITH TEXT-M03 ZSPM_PARAM-SHOP TEXT-003.
    ENDIF.
  ENDIF.

  IF ZSPM_PARAM-AJAHR IS INITIAL.
    MESSAGE E000(ZMPM) WITH TEXT-006.
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
*    ZSPM_MTBT-MAUEH = LMAUEH.
  ENDIF.

ENDMODULE.                 " MAUEH_TIME  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_TABLE_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_TABLE_0200 OUTPUT.

ENDMODULE.                 " MODIFY_SCREEN_TABLE_0200  OUTPUT
