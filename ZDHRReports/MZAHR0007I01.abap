*----------------------------------------------------------------------*
*   INCLUDE MZAHR0007I01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE back_exit INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " BACK_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  PERFORM check_authority.
*
  CASE sy-ucomm.
    WHEN 'BUT1'.
      PERFORM init_screen_9100.
    WHEN 'BUT2'.
      PERFORM init_screen_9200.
    WHEN 'GOTO'.
      CALL TRANSACTION 'ZAHR0005'.
  ENDCASE.
*
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9100 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      PERFORM select_head_count_data.
    WHEN 'BUT1'.                       " close month
      PERFORM update_table_ach01.
    WHEN 'BUT3'.                       " copy month
      PERFORM copy_month.
      PERFORM update_table_ach01.
    WHEN 'BUT4'.                       " month status informaion
      CALL SCREEN 9110 STARTING AT 5 5
                       ENDING AT 40 15.
    WHEN 'NEWE'.
      PERFORM insert_new_entry.
    WHEN 'DELE'.
      PERFORM data_delete_entry.
    WHEN 'EXCL'.
      PERFORM excel_down_load_file.
  ENDCASE.
*
  CLEAR sy-ucomm.
ENDMODULE.                 " USER_COMMAND_9100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_WERKS_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE get_werks_value INPUT.
  CLEAR it_persa. REFRESH it_persa.
*
  CLEAR t500p.
  SELECT persa name1 INTO (t500p-persa, t500p-name1)
    FROM t500p WHERE molga = '10'.
    it_persa-werks = t500p-persa.
    it_persa-name1 = t500p-name1.
    APPEND it_persa. CLEAR it_persa.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'T500P'.
  it_field-fieldname  = 'PERSA'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'T500P'.
  it_field-fieldname  = 'NAME1'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_persa.
*
  w_werks = w_fldvl.
*
  CLEAR dynpfields. REFRESH dynpfields.

  READ TABLE it_persa INDEX w_tabix.
  dynpfields-fieldname   = 'W_NAME1'.
  dynpfields-fieldvalue  = it_persa-name1.
  APPEND dynpfields. CLEAR dynpfields.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpfields
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
ENDMODULE.                 " GET_WERKS_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE get_zyear_value INPUT.
  CLEAR it_years. REFRESH it_years.
*
  CLEAR zthr_pcp02.
  SELECT zval1 INTO zthr_pcp02-zval1
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1020'.
    it_years-zyear = zthr_pcp02-zval1(4).
    APPEND it_years. CLEAR it_years.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_PCP03'.
  it_field-fieldname  = 'ZYEAR'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_years.
*
  w_zyear = w_fldvl.
ENDMODULE.                 " GET_ZYEAR_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE get_zvers_value INPUT.
  CLEAR it_versn. REFRESH it_versn.
*
  CLEAR zthr_pcp02.
  SELECT zval1 zval3 INTO (zthr_pcp02-zval1, zthr_pcp02-zval3)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1030'
                      AND zval2 = w_zyear.
    it_versn-zvers = zthr_pcp02-zval1(2).
    APPEND it_versn. CLEAR it_versn.
  ENDSELECT.
*
  IF it_versn[] IS INITIAL.
    EXIT.
  ENDIF.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_PCP03'.
  it_field-fieldname  = 'ZVERS'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_versn.
*
  w_zvers = w_fldvl.
ENDMODULE.                 " GET_ZVERS_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_INTAB_AHC01  INPUT
*&---------------------------------------------------------------------*
MODULE modify_intab_ahc01 INPUT.
  CASE it_ahc01-stats.
    WHEN 'C' OR 'N'.
      it_ahc01-erdat = sy-datum.
      it_ahc01-erzet = sy-uzeit.
      it_ahc01-ernam = sy-uname.
    WHEN 'U'.
      it_ahc01-aedat = sy-datum.
      it_ahc01-aezet = sy-uzeit.
      it_ahc01-aenam = sy-uname.
  ENDCASE.
*
  MODIFY it_ahc01 INDEX tc9100-current_line.
ENDMODULE.                 " MODIFY_INTAB_AHC01  INPUT
*&---------------------------------------------------------------------*
*&      Module  SELECT_ITEMS  INPUT
*&---------------------------------------------------------------------*
MODULE select_items INPUT.
  MODIFY it_ahc01 INDEX tc9100-current_line.
ENDMODULE.                 " SELECT_ITEMS  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZSCST_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE get_zscst_value INPUT.
  DATA : z_zval2 LIKE zthr_pcp02-zval2,
        c_pernr LIKE w_pernr.

  CLEAR it_zscst. REFRESH it_zscst.
  CLEAR :z_zval2,c_pernr.

  REFRESH dynpfields. CLEAR dynpfields.
  dynpfields-fieldname = 'W_PERNR'.
  APPEND dynpfields.

  PERFORM dynp_values_read TABLES dynpfields.

  READ TABLE dynpfields WITH KEY fieldname = 'W_PERNR'.
  MOVE dynpfields-fieldvalue TO w_pernr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = w_pernr
       IMPORTING
            output = c_pernr.

  SELECT  zval2  INTO  z_zval2
          FROM  zthr_pcp02  UP TO 1  ROWS
           WHERE  zmodl EQ '02'
             AND  zgrup EQ '1005'
             AND  zval1 EQ c_pernr.
  ENDSELECT.
  IF z_zval2 <> space.
    PERFORM get_subcost_master.
  ELSE.
    PERFORM get_subcost.
  ENDIF.

  IF it_zscst[] IS INITIAL.
    MESSAGE i001 WITH  'Code not defind, user cost center'.
    EXIT.
  ENDIF.
  CLEAR it_field. REFRESH it_field.
  it_field-tabname    = 'CSKT'.
  it_field-fieldname  = 'KOSTL'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'CSKT'.
  it_field-fieldname  = 'KTEXT'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.
  CLEAR: w_fname, w_tabix, w_fldvl.
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_zscst.
*
  w_zscst = w_fldvl.
*
  CLEAR dynpfields. REFRESH dynpfields.
  READ TABLE it_zscst INDEX w_tabix.
  dynpfields-fieldname   = 'W_KNAME'.
  dynpfields-fieldvalue  = it_zscst-kname.
  APPEND dynpfields. CLEAR dynpfields.
  dynpfields-fieldname   = 'W_KOSTL'.
  dynpfields-fieldvalue  = w_kostl.
  APPEND dynpfields. CLEAR dynpfields.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpfields
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.

ENDMODULE.                 " GET_ZSCST_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_9200 INPUT.
  CASE sy-ucomm.
    WHEN 'EXEC'.
      PERFORM select_data_from_pcp05.
    WHEN 'BUT1'.
      PERFORM save_month_data.
    WHEN 'BUT2'.
      PERFORM save_year_data.
    WHEN 'BUT3'.
      PERFORM copy_month_data.
      PERFORM save_month_data.
    WHEN 'BUT4'.                         " month status informaion
      CALL SCREEN 9210 STARTING AT 5 5
                       ENDING AT 40 15.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_ZGRUP_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE get_zgrup_value INPUT.
  CLEAR it_group. REFRESH it_group.
*
  CLEAR zthr_pcp01.
  SELECT zgrup zgtxt INTO (zthr_pcp01-zgrup, zthr_pcp01-zgtxt)
    FROM zthr_pcp01 WHERE zmodl = '02'
                      AND zgrup IN ('1040', '1050').
    it_group-zgrup = zthr_pcp01-zgrup.
    it_group-zgtxt = zthr_pcp01-zgtxt.
    APPEND it_group. CLEAR it_group.
  ENDSELECT.
*
  CLEAR it_field. REFRESH it_field.
*
  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZGRUP'.
  it_field-selectflag = 'X'.
  APPEND it_field. CLEAR it_field.

  it_field-tabname    = 'ZTHR_PCP01'.
  it_field-fieldname  = 'ZGTXT'.
  it_field-selectflag = ' '.
  APPEND it_field. CLEAR it_field.
*
  CLEAR: w_fname, w_tabix, w_fldvl.
*
  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            selectfield  = w_fname
       IMPORTING
            ind          = w_tabix
            select_value = w_fldvl
       TABLES
            fields       = it_field
            full_table   = it_group.
*
  w_zgrup = w_fldvl.
*
  CLEAR dynpfields. REFRESH dynpfields.

  READ TABLE it_group INDEX w_tabix.
  dynpfields-fieldname   = 'W_ZGTXT'.
  dynpfields-fieldvalue  = it_group-zgtxt.
  APPEND dynpfields. CLEAR dynpfields.
*
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname               = sy-cprog
            dynumb               = sy-dynnr
       TABLES
            dynpfields           = dynpfields
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            undefind_error       = 7
            OTHERS               = 8.
*
  CLEAR zjobc. REFRESH zjobc.
  CLEAR zthr_pcp02.

  SELECT zval1 zval4 INTO (zthr_pcp02-zval1, zthr_pcp02-zval4)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = w_zgrup.
    CONDENSE zthr_pcp02-zval4.
    IF w_zgrup = '1040' .
      IF zthr_pcp02-zval4 = 'OT'.
        zjobc-sign = 'I'.
        zjobc-option = 'EQ'.
        zjobc-low = zthr_pcp02-zval1.
        APPEND zjobc. CLEAR zjobc.
      ENDIF.
    ELSE.
      zjobc-sign = 'I'.
      zjobc-option = 'EQ'.
      zjobc-low = zthr_pcp02-zval1.
      APPEND zjobc. CLEAR zjobc.
    ENDIF.
    CLEAR zthr_pcp02.
  ENDSELECT.

ENDMODULE.                 " GET_ZGRUP_VALUE  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LOOPC_PCP05  INPUT
*&---------------------------------------------------------------------*
MODULE get_loopc_pcp05 INPUT.
  w_index = w_topln + sy-stepl - 1.
  READ TABLE it_pcp05 INDEX w_index.
ENDMODULE.                 " GET_LOOPC_PCP05  INPUT
*&---------------------------------------------------------------------*
*&      Module  CALC_WORKING_HOURS  INPUT
*&---------------------------------------------------------------------*
MODULE calc_working_hours INPUT.
  it_pcp05-ztotm = it_pcp05-zreld * it_pcp05-zhedc *
                   ( it_pcp05-zprat / 100 ) * it_pcp05-zwktm.
*
  IF it_pcp05-stats = 'M'.
    it_pcp05-aedat = sy-datum.
    it_pcp05-aezet = sy-uzeit.
    it_pcp05-aenam = sy-uname.
  ENDIF.
*
  MODIFY it_pcp05 INDEX w_index.
ENDMODULE.                 " CALC_WORKING_HOURS  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_NEW_ENTRY  INPUT
*&---------------------------------------------------------------------*
MODULE modify_new_entry INPUT.
  CLEAR hrp1000.
  SELECT SINGLE short INTO hrp1000-short
   FROM hrp1000 WHERE plvar = '01'
                  AND otype = 'C'
                  AND istat = '1'
                  AND endda = '99991231'
                  AND langu = sy-langu
                  AND objid = it_ahc01-zjobc.
  IF sy-subrc NE 0.
    MESSAGE e001 WITH 'INPUT JOB CODE'.
  ENDIF.

  SELECT SINGLE zval1 INTO zthr_pcp02-zval1
FROM zthr_pcp02 WHERE zmodl = '02'
                  AND zgrup = '1250'
                  AND zval2 = it_ahc01-zperg
                  AND zval4 = it_ahc01-zjobc.
  IF sy-subrc NE 0.
*       message e001 with 'Not define code book 2-1250 '.
    MESSAGE s001 WITH 'Not define code book 2-1250 '.
    EXIT.
  ENDIF.
  it_ahc01-zjobk = hrp1000-short.

  IF it_ahc01-zscst <> space.
    SELECT SINGLE ktext INTO cskt-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = it_ahc01-zscst
                  AND datbi = '99991231'.
    IF sy-subrc = 0.
      it_ahc01-ktext = cskt-ktext.
    ELSE.
      MESSAGE e001 WITH 'INPUT SUB COST CENTER'.
    ENDIF.
  ELSE.
    MESSAGE e001 WITH 'INPUT SUB COST CENTER'.
  ENDIF.
*
  MODIFY it_ahc01 INDEX tc9100-current_line.
ENDMODULE.                 " MODIFY_NEW_ENTRY  INPUT
*&---------------------------------------------------------------------*
*&      Module  COST_CENTER_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cost_center_text INPUT.

  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.

ENDMODULE.                 " COST_CENTER_TEXT  INPUT
*&---------------------------------------------------------------------*
*&      Module  COST_get_name  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cost_get_name INPUT.
  DATA : l_zval1 LIKE zthr_pcp02-zval1.

  CALL FUNCTION 'HR_GET_EMPLOYEE_DATA'
       EXPORTING
            person_id             = w_pernr
            selection_begin       = sy-datum
            selection_end         = sy-datum
       IMPORTING
            personal_data         = it_perda
       EXCEPTIONS
            person_not_found      = 1
            no_active_integration = 2
            OTHERS                = 3.
*
  w_werks = it_perda-werks.
  CLEAR t500p.
  SELECT SINGLE name1 INTO t500p-name1
    FROM t500p WHERE persa = w_werks.
  w_name1 = t500p-name1.
  w_orgeh = it_perda-orgeh.
  w_kostl = it_perda-kostl.
  CLEAR cskt.
  SELECT SINGLE ktext INTO cskt-ktext
    FROM cskt WHERE spras = sy-langu
                AND kostl = w_kostl
                AND datbi = '99991231'.
  w_ktext = cskt-ktext.

  PACK w_pernr TO l_zval1.
  CONDENSE l_zval1.
  SELECT SINGLE  zval2 zval3 zval4 zval5
     INTO (zthr_pcp02-zval2, zthr_pcp02-zval3,
           zthr_pcp02-zval4, zthr_pcp02-zval5)
    FROM zthr_pcp02 WHERE zmodl = '02'
                      AND zgrup = '1000'
                      AND zval1 LIKE l_zval1.
  IF sy-subrc EQ 0 .
    w_close = zthr_pcp02-zval3.
    MOVE : zthr_pcp02-zval2  TO w_orgeh.
    CONDENSE w_orgeh.
    UNPACK w_orgeh TO w_orgeh.
    CLEAR:it_cost[], it_cost.
    PERFORM cost_center_get USING zthr_pcp02-zval4 .
    PERFORM cost_center_get USING zthr_pcp02-zval5 .

    DELETE it_cost WHERE zcost = '0000000000'.
  ENDIF.
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
           kostl LIKE pa0001-kostl,
           ktext LIKE cskt-ktext,
         END OF value_tab.
  DATA :   l_dynnr            LIKE sy-dynnr,
           l_repid            LIKE sy-repid.

  CLEAR : value_tab,w_zscst.
  REFRESH :  value_tab.

  SELECT zctxt zval1 INTO (zthr_pcp02-zctxt, zthr_pcp02-zval1)
         FROM zthr_pcp02 WHERE zmodl = '02'
                        AND zgrup = '1000' .

    MOVE :  zthr_pcp02-zctxt TO value_tab-zctxt.
    IF zthr_pcp02-zval1 CN '1234567890'.
      MOVE zthr_pcp02-zval1 TO value_tab-pernr.
    ELSE.
      UNPACK  zthr_pcp02-zval1 TO value_tab-pernr.
    ENDIF.

    SELECT SINGLE kostl INTO value_tab-kostl
                  FROM pa0001
                  WHERE pernr = value_tab-pernr .

    SELECT SINGLE ktext INTO value_tab-ktext
      FROM cskt WHERE spras = sy-langu
                  AND kostl = value_tab-kostl.

    APPEND value_tab. CLEAR value_tab.

  ENDSELECT .

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

  REFRESH it_zscst.
ENDMODULE.                 " GET_PERNR_VALUE  INPUT
