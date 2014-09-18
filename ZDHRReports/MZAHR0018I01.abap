*----------------------------------------------------------------------*
*   INCLUDE MZAHR0007I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE back_exit INPUT.
  IF save_flag = space AND sy-dyngr EQ '9100'.
    wa_title = 'Caution ! Input data will be lost'.
    PERFORM pop_up_message USING wa_title
                           CHANGING wa_answer  .
    IF wa_answer = 'J'.
      PERFORM save_data.
    ELSE.
      SET SCREEN 0. LEAVE SCREEN.
      CLEAR w_status.
    ENDIF.
  ELSE.
    SET SCREEN 0. LEAVE SCREEN.
    CLEAR w_status.
  ENDIF.

ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.


  CLEAR : ok_code, w_status.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'BUT1'.
      PERFORM init_screen_9100.
    WHEN 'BUT2'.
      PERFORM init_screen_9200.
  ENDCASE.
*
  CLEAR ok_code.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'EXEC'.
      PERFORM select_count_data.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'NEWE'.
      PERFORM insert_new_entry.
    WHEN 'DELE'.
      PERFORM data_delete_entry.
    WHEN 'CACL'.
      PERFORM calculation_entry.
    WHEN 'EXCL'.
  ENDCASE.
*
  CLEAR :ok_code.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
**&---------------------------------------------------------------------
*
**&      Module  GET_ZYEAR_VALUE  INPUT
**&---------------------------------------------------------------------
*
*MODULE get_zyear_value INPUT.
*  CLEAR it_years. REFRESH it_years.
**
*  CLEAR zthr_et02.
*  SELECT zval1 INTO zthr_et02-zval1
*    FROM zthr_et02 WHERE zmodl = '01'
*                      AND zgrup = '1000'.
*    it_years-zyear = zthr_et02-zval1(4).
*    APPEND it_years. CLEAR it_years.
*  ENDSELECT.
**
*  CLEAR it_field. REFRESH it_field.
**
*  it_field-tabname    = 'ZTHR_PCP03'.
*  it_field-fieldname  = 'ZYEAR'.
*  it_field-selectflag = 'X'.
*  APPEND it_field. CLEAR it_field.
**
*  CLEAR: w_fname, w_tabix, w_fldvl.
**
*  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
*       EXPORTING
*            selectfield  = w_fname
*       IMPORTING
*            ind          = w_tabix
*            select_value = w_fldvl
*       TABLES
*            fields       = it_field
*            full_table   = it_years.
**
*  w_zyear = w_fldvl.
*ENDMODULE.                 " GET_ZYEAR_VALUE  INPUT
**&---------------------------------------------------------------------
*
*&      Module  MODIFY_NEW_ENTRY  INPUT
*&---------------------------------------------------------------------*
MODULE modify_new_entry INPUT.

  CLEAR : w_int,wa_et03,f_flag.
  CHECK  w_falg_new = SPACE.
  CHECK it_et03-zdate GT '20040601'.
  MOVE-CORRESPONDING it_et03 TO wa_et03.
  CONCATENATE it_et03-zftime(4) '00' INTO wa_et03-zftime.
  READ TABLE it_et03 INDEX tc9100-current_line.
  IF sy-subrc = 0.
    PERFORM move_it_et03.
    MODIFY it_et03 INDEX tc9100-current_line.
    w_status = 'U'.
  ELSE.
    PERFORM move_it_et03.
    APPEND it_et03.
    w_status = 'U'.
  ENDIF.
  CLEAR : save_flag.
ENDMODULE.                 " MODIFY_NEW_ENTRY  INPUT
*&---------------------------------------------------------------------*
*&      Module  COST_CENTER_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cost_center_text INPUT.

*  SELECT sacha INTO w_sachz
*        FROM pa0001 UP TO 1 ROWS
*              WHERE pernr EQ w_pernr
*                AND endda EQ '99991231'.
*  ENDSELECT.
*
*  SELECT COUNT( * ) INTO  w_zhedc
*        FROM pa0001
*             WHERE sachz EQ  w_sachz .


  SELECT COUNT( * ) INTO  w_rc_head
        FROM pa0001
             WHERE kostl EQ  w_kostl.

  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.

  IF w_rc_head EQ 0.
    MESSAGE w001 WITH text-027.
    CLEAR w_kostl.
    REFRESH it_et03.CLEAR it_et03.
    REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
    DESCRIBE TABLE it_et03 LINES tc9100-lines.
    LEAVE TO SCREEN 9100.
  ELSE.
    REFRESH it_et03.CLEAR :  w_falg_new,it_et03.
    REFRESH CONTROL 'TC9100' FROM SCREEN 9100.
    DESCRIBE TABLE it_et03 LINES tc9100-lines.
    w_falg_new = 'X'.
    w_status = 'I'.
  ENDIF.
ENDMODULE.                 " COST_CENTER_TEXT  INPUT
*&---------------------------------------------------------------------*
*&      Module  COST_get_name  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cost_get_name INPUT.

  PERFORM get_data_employee USING w_pernr.

ENDMODULE.                 " COST_get_name  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_PERNR_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_pernr_value INPUT.
  DATA : BEGIN OF value_tab OCCURS 0,
           pernr LIKE pa0001-pernr,
           zctxt LIKE p1000-stext,
         END OF value_tab.
  DATA :   l_dynnr            LIKE sy-dynnr,
           l_repid            LIKE sy-repid.

  CLEAR : value_tab.
  REFRESH :  value_tab.

  SELECT zval1 INTO (zthr_et02-zval1)
         FROM zthr_et02 WHERE zmodl = '01'
                        AND zgrup = '1010' .

    PERFORM  numeric_check_pernr USING zthr_et02-zval1
                                 CHANGING value_tab-pernr.

    SELECT ename INTO value_tab-zctxt
                 FROM pa0001 UP TO 1 ROWS
                 WHERE pernr EQ value_tab-pernr
                   AND endda EQ '99991231'.
    ENDSELECT.

    APPEND value_tab. CLEAR value_tab.

  ENDSELECT .

  DELETE ADJACENT DUPLICATES FROM value_tab.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'PERNR'
            dynpprog        = l_repid
            dynpnr          = l_dynnr
            dynprofield     = 'W_PERNR'
            window_title    = 'Personal cost plan Adm'
            value_org       = 'S'
       TABLES
            value_tab       = value_tab
       EXCEPTIONS
            parameter_error = 1.

ENDMODULE.                 " GET_PERNR_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_etcode_value  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_etcode_value INPUT.
  DATA : BEGIN OF value_etcode OCCURS 0,
           zctxt LIKE zthr_et02-zctxt,
           zval1 LIKE zthr_et02-zval1,
         END OF value_etcode.
*  DATA :   l_dynnr            LIKE sy-dynnr,
*           l_repid            LIKE sy-repid.

  CLEAR : value_etcode.
  REFRESH :  value_etcode.

  SELECT zval1  zctxt INTO (value_etcode-zval1,value_etcode-zctxt)
         FROM zthr_et02 WHERE zmodl = '01'
                        AND zgrup = '1020' .

    APPEND value_etcode. CLEAR value_etcode.
  ENDSELECT .

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = ''
            dynpprog        = l_repid
            dynpnr          = l_dynnr
            dynprofield     = 'IT_ET03-ZETCODE'
            window_title    = 'ET Code list'
            value_org       = 'S'
       TABLES
            value_tab       = value_etcode
       EXCEPTIONS
            parameter_error = 1.

ENDMODULE.                 " get_etcode_value  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  CLEAR ok_code.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'EXEC'.
      PERFORM get_data.
    WHEN 'DOWN'.
      PERFORM download.
  ENDCASE.
ENDMODULE.                 " user_command_9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_COSTCENTER_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_costcenter_value INPUT.
  DATA: BEGIN OF dynpread OCCURS 3.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF dynpread.
  CLEAR : value_cost.
  REFRESH :  value_cost.

  REFRESH dynpfields. CLEAR dynpfields.
  dynpfields-fieldname = 'W_PERNR'.
  dynpfields-stepl = sy-stepl.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname     = sy-cprog
            dynumb     = '9100'
       TABLES
            dynpfields = dynpfields.

  READ TABLE dynpfields INDEX 1.
  MOVE  dynpfields-fieldvalue TO w_pernr.

  CHECK NOT w_pernr IS INITIAL."<> '00000000'
  IF r_b EQ 'X'.
    SELECT zrmrk INTO (zthr_et02-zrmrk)
           FROM zthr_et02 WHERE zmodl = '01'
                            AND zgrup = '1010'
                            AND zval1 = w_pernr+2(6).

      PERFORM cost_center_get USING zthr_et02-zrmrk .

    ENDSELECT .
  ELSEIF r_a EQ 'X'.
    PERFORM cost_center_get_tg USING w_pernr.
  ENDIF.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'KOSTL'
            dynpprog        = l_repid
            dynpnr          = l_dynnr
            dynprofield     = 'W_KOSTL'
            window_title    = 'Cost center'
            value_org       = 'S'
       TABLES
            value_tab       = value_cost
       EXCEPTIONS
            parameter_error = 1.


ENDMODULE.                 " GET_COSTCENTER_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_costcenter_value_table  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_costcenter_value_table INPUT.
  CLEAR : value_cost.
  REFRESH :  value_cost.
  IF r_b EQ 'X'.
    SELECT zrmrk INTO (zthr_et02-zrmrk)
            FROM zthr_et02 WHERE zmodl = '01'
                             AND zgrup = '1010'.

      PERFORM cost_center_get USING zthr_et02-zrmrk .

    ENDSELECT .
  ELSEIF r_a EQ 'X'.
    SELECT kostl  INTO value_cost
          FROM pa0001
               WHERE sachz NE space .
      PERFORM filled_costcenter_name USING value_cost-kostl
                               CHANGING  value_cost-zctxt.
      APPEND value_cost. CLEAR value_cost.
    ENDSELECT.
  ENDIF.
  SORT value_cost ASCENDING BY kostl.
  DELETE ADJACENT DUPLICATES FROM value_cost.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            retfield        = 'KOSTL'
            dynpprog        = l_repid
            dynpnr          = l_dynnr
            dynprofield     = 'W_KOSTL'
            window_title    = 'Cost center'
            value_org       = 'S'
       TABLES
            value_tab       = value_cost
       EXCEPTIONS
            parameter_error = 1.
ENDMODULE.                 " get_costcenter_value_table  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_data INPUT.
  CLEAR: w_falg_new, w_int.
  DESCRIBE TABLE it_et03 LINES w_int.
  IF w_int = 0.
    PERFORM select_count_data.
  ENDIF.
ENDMODULE.                 " GET_DATA  INPUT
