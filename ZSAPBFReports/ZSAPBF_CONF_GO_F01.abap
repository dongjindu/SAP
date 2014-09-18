*&---------------------------------------------------------------------*
*&  Include           ZSAPBF_CONF_GO_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100_EXT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100_ext OUTPUT.
  SET PF-STATUS 'G0100'. " excluding g_fcode.
  SET TITLEBAR 'T0100'.

  DESCRIBE TABLE it_plt_para LINES tc_0100-lines.

  IF tc_0100-lines IS INITIAL.
    PERFORM get_data.
  ENDIF.

  DESCRIBE TABLE it_plt_para LINES tc_0100-lines.

  PERFORM get_rzllitab.
ENDMODULE.                 " STATUS_0100_EXT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHANGE_TC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE change_tc OUTPUT.
  LOOP AT SCREEN.
    IF it_plt_para[] IS INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name = 'IT_PLT_PARA-WERKS' AND it_plt_para-check = 'X'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.
ENDMODULE.                 " CHANGE_TC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  DATA: ls_plant_para_setting TYPE zsapbf_plant_para_setting.
  CASE sy-ucomm.
    WHEN 'EXIT'.

      LEAVE PROGRAM.
    WHEN 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify INPUT.
  MODIFY it_plt_para FROM it_plt_para INDEX tc_0100-current_line.
ENDMODULE.                 " MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_ext INPUT.
  save_ok = ok_code.
  CLEAR ok_code.

  CASE save_ok.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'INST'.
      PERFORM insert_data.
    WHEN 'DETL'.
      PERFORM delete_data.
    WHEN 'CANC'.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100_EXT  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data .
  READ TABLE it_plt_para WITH KEY mark = 'X'.
  IF sy-subrc = 0.
    DELETE it_plt_para INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  INSERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_data .
  CLEAR it_plt_para. APPEND it_plt_para.
ENDFORM.                    " INSERT_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .
  DATA: lt_db_plt_para  TYPE TABLE OF zsapbf_plt_para,
        ls_db_plt_para TYPE zsapbf_plt_para.

  SORT it_plt_para BY werks.
  DELETE ADJACENT DUPLICATES FROM it_plt_para COMPARING werks.

  DATA ls_plt_para TYPE tt_plt_para.

  LOOP AT it_plt_para INTO ls_plt_para.

    SELECT SINGLE * INTO ls_db_plt_para
      FROM zsapbf_plt_para
     WHERE werks EQ ls_plt_para-werks
       AND tran_code EQ gv_tran_code.
    IF sy-subrc = 0.
      ls_db_plt_para-classname = ls_plt_para-classname.
      ls_db_plt_para-wp_quota  = ls_plt_para-wp_quota.
      ls_db_plt_para-aedat = sy-datlo.
      ls_db_plt_para-aezet = sy-timlo.
      ls_db_plt_para-aenam = sy-uname.
    ELSE.
      MOVE-CORRESPONDING ls_plt_para TO ls_db_plt_para.
      ls_db_plt_para-tran_code = gv_tran_code.
      ls_db_plt_para-aedat = sy-datlo.
      ls_db_plt_para-aezet = sy-timlo.
      ls_db_plt_para-aenam = sy-uname.
    ENDIF.
    APPEND ls_db_plt_para TO lt_db_plt_para.
  ENDLOOP.

  DELETE FROM zsapbf_plt_para WHERE tran_code EQ gv_tran_code.
  INSERT zsapbf_plt_para FROM TABLE lt_db_plt_para.
*  MODIFY zsapbf_plt_para FROM TABLE lt_db_plt_para.

  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s121(zsapbf_ppc0).
  ENDIF.
ENDFORM.                    " SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_F4HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_METH  text
*----------------------------------------------------------------------*
FORM get_f4help USING iv_tran TYPE zsapbf_lock_tran.
  DATA: BEGIN OF lt_value_tab OCCURS 0,
          method TYPE char2,
          description TYPE zsapbf_cust_para_meth_desc,
         END OF lt_value_tab.
*  DATA: lt_field_tab TYPE  STANDARD TABLE OF dfies,
  DATA: ls_return_tab TYPE ddshretval,
        lt_return_tab TYPE STANDARD TABLE OF ddshretval.
  IF iv_tran = lc_1.
    SELECT a~method
           b~description
     FROM zsapbf_cust_meth AS a INNER JOIN
          zsapbf_cus_metht AS b
       ON a~method = b~method
     INTO CORRESPONDING FIELDS OF TABLE lt_value_tab
    WHERE a~sins = lc_x
      AND b~spras = sy-langu.
  ELSEIF iv_tran = lc_2.
    SELECT a~method
          b~description
    FROM zsapbf_cust_meth AS a INNER JOIN
         zsapbf_cus_metht AS b
      ON a~method = b~method
    INTO CORRESPONDING FIELDS OF TABLE lt_value_tab
   WHERE a~tss1 = lc_x
     AND b~spras = sy-langu.
  ELSEIF iv_tran = lc_3.
    SELECT a~method
           b~description
     FROM zsapbf_cust_meth AS a INNER JOIN
          zsapbf_cus_metht AS b
       ON a~method = b~method
     INTO CORRESPONDING FIELDS OF TABLE lt_value_tab
    WHERE a~tss2 = lc_x
      AND b~spras = sy-langu.
  ENDIF.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'METHOD'                            "text-012
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'METHOD'                            "text-012
      stepl           = sy-stepl
      window_title    = 'METHOD'                            "text-012
      value_org       = lc_s
    TABLES
      value_tab       = lt_value_tab
*      field_tab       = lt_field_tab
      return_tab      = lt_return_tab
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " GET_F4HELP
*&---------------------------------------------------------------------*
*&      Form  DYNAMIC_SCREEN_GENERATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dynamic_screen_generate USING    iv_method      TYPE zsapbf_cust_para_meth "zsapbf_meth_par_single
                                      iv_tran        TYPE zsapbf_lock_tran
                                      ic_rpt         TYPE progname
                             CHANGING et_ui_maint    TYPE tt_ui_maint.
  DATA:BEGIN OF ls_code,
       text(120) TYPE c,
       END OF ls_code.
  DATA:BEGIN OF lt_code OCCURS 0,
       text(120) TYPE c,
       END OF lt_code.
  DATA: ls_ui_maint TYPE zsapbf_ui_maint,
        lt_ui_maint TYPE STANDARD TABLE OF zsapbf_ui_maint.

  DATA lv_ref_str      TYPE char12.
  DATA lv_ref_fld(60)  TYPE c.

  CASE iv_tran.
    WHEN lc_1."single step
      SELECT * FROM zsapbf_cust_meth AS a
               INNER JOIN  zsapbf_ui_maint AS b
               ON a~method = b~method
               INTO CORRESPONDING FIELDS OF TABLE lt_ui_maint
               WHERE a~method = iv_method
                 AND a~sins = lc_x.
    WHEN lc_2. " step 1 of two step
      SELECT * FROM zsapbf_cust_meth AS a
               INNER JOIN  zsapbf_ui_maint AS b
                ON a~method = b~method
                INTO CORRESPONDING FIELDS OF TABLE lt_ui_maint
                WHERE a~method = iv_method
                  AND a~tss1 = lc_x.
    WHEN lc_3.
      SELECT * FROM zsapbf_cust_meth AS a
               INNER JOIN  zsapbf_ui_maint AS b
               ON a~method = b~method
               INTO CORRESPONDING FIELDS OF TABLE lt_ui_maint
               WHERE a~method = iv_method
                 AND a~tss2 = lc_x.
  ENDCASE.
  et_ui_maint = lt_ui_maint.
  LOOP AT et_ui_maint INTO ls_ui_maint.
    CLEAR: lv_ref_str, ls_code.
    CONCATENATE lc_ref ls_ui_maint-fieldname INTO lv_ref_str.

    CONCATENATE lc_data             " DATA
                lv_ref_str          " ref_ parameter name
                lc_type             " TYPE
                ls_ui_maint-strname " structre name
                lc_symbol           " .
        INTO ls_code-text SEPARATED BY space.
    APPEND ls_code TO lt_code.
  ENDLOOP.

  LOOP AT et_ui_maint INTO ls_ui_maint.
    CLEAR: lv_ref_str, lv_ref_fld, ls_code.
    CONCATENATE lc_ref ls_ui_maint-fieldname INTO lv_ref_str.
    CONCATENATE lv_ref_str lc_hyphen ls_ui_maint-element INTO lv_ref_fld.

    IF ls_ui_maint-type = 'P'.
      CONCATENATE lc_parameter
               ls_ui_maint-fieldname
               lc_like
               lv_ref_fld
               lc_modif
               lc_symbol
          INTO ls_code-text SEPARATED BY space.
    ELSE.
      CONCATENATE lc_so
                  ls_ui_maint-fieldname
                  lc_for
                  lv_ref_fld
                  lc_modif
                  lc_symbol
            INTO ls_code-text SEPARATED BY space.
    ENDIF.
    APPEND ls_code TO lt_code.
  ENDLOOP.
*  LOOP AT lt_ui_maint INTO ls_ui_maint.
*    IF ls_ui_maint-type = lc_p.
*      CONCATENATE lc_parameter
*               ls_ui_maint-fieldname
*               lc_type
*               ls_ui_maint-element
*               lc_modif
*               lc_symbol
*          INTO ls_code-text SEPARATED BY space.
*    ELSE.
*      CONCATENATE lc_so
*                  ls_ui_maint-fieldname
*                  lc_for
*                  ls_ui_maint-element
*                  lc_modif
*                  lc_symbol
*            INTO ls_code-text SEPARATED BY space.
*    ENDIF.
*    APPEND ls_code TO lt_code.
*    CLEAR ls_code.
*  ENDLOOP.

  INSERT REPORT ic_rpt FROM lt_code.
  COMMIT WORK.
  GENERATE REPORT ic_rpt.
  COMMIT WORK.
ENDFORM.                    " DYNAMIC_SCREEN_GENERATE
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DYNAMIC_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_dynamic_screen USING ic_rpt TYPE progname.
  DATA:BEGIN OF lt_code OCCURS 2,
        text(120) TYPE c,
        END OF lt_code.

  INSERT REPORT ic_rpt FROM lt_code.
  COMMIT WORK.
  GENERATE REPORT ic_rpt.
  COMMIT WORK.

ENDFORM.                    " CLEAR_DYNAMIC_SCREEN
*&---------------------------------------------------------------------*
*&      Form  PARAM_DESCPT_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_UI_MAINT  text
*----------------------------------------------------------------------*
FORM param_descpt_get  USING  it_ui_maint TYPE tt_ui_maint.

* The section for customer add the description of the above new added screen elements.
  DATA: lt_itab TYPE STANDARD TABLE OF textpool,
        lv_fielddesc TYPE adge_fnd,
        ls_itab TYPE textpool.

  FIELD-SYMBOLS:
        <fs_ui_maint> TYPE zsapbf_ui_maint.
  READ TEXTPOOL sy-repid INTO lt_itab LANGUAGE sy-langu.

  LOOP AT it_ui_maint ASSIGNING <fs_ui_maint>.
    READ TABLE lt_itab INTO ls_itab WITH KEY id = lc_s
                                             key = <fs_ui_maint>-fieldname.
    IF sy-subrc = 0.
      lv_fielddesc+8 = <fs_ui_maint>-fielddesc.
      ls_itab-entry = lv_fielddesc.
      MODIFY lt_itab FROM ls_itab INDEX sy-tabix.
      CLEAR ls_itab.
    ELSE.
      lv_fielddesc+8 = <fs_ui_maint>-fielddesc.
      ls_itab-id = lc_s .
      ls_itab-key = <fs_ui_maint>-fieldname.
      ls_itab-entry = lv_fielddesc.
      APPEND ls_itab TO lt_itab.
      CLEAR ls_itab.
    ENDIF.
  ENDLOOP.
  INSERT textpool sy-repid FROM lt_itab LANGUAGE lc_en STATE lc_a.

ENDFORM.                    " PARAM_DESCPT_GET
*&---------------------------------------------------------------------*
*&      Form  PARAMETER_FILL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_UI_MAINT  text
*----------------------------------------------------------------------*
FORM parameter_fill USING it_ui_maint  TYPE tt_ui_maint
                 CHANGING et_ui_options TYPE zsapbf_tt_ui_option.

  DATA: ls_ui_maint TYPE zsapbf_ui_maint,
        lv_name TYPE char30.
  FIELD-SYMBOLS:
       <fs_field_name> TYPE ANY,
       <fs_table_name> TYPE ANY TABLE,
       <l_line> TYPE ANY.

  CHECK it_ui_maint[] IS NOT INITIAL.
  LOOP AT it_ui_maint INTO ls_ui_maint.

    IF ls_ui_maint-type = lc_p.
      ASSIGN (ls_ui_maint-fieldname) TO <fs_field_name>.
      ls_ui_option-name = ls_ui_maint-fieldname.
      ls_ui_option-sign = 'I'.
      ls_ui_option-option = 'EQ'.
      ls_ui_option-low = <fs_field_name>.
      ls_ui_option-type = lc_p.
      APPEND ls_ui_option TO et_ui_options.
    ENDIF.
    IF ls_ui_maint-type = lc_s.
*      ASSIGN (ls_ui_maint-fieldname) TO <fs_field_name>.
      CONCATENATE ls_ui_maint-fieldname '[]' INTO lv_name.
      ASSIGN (lv_name) TO <fs_table_name>.

      LOOP AT <fs_table_name> ASSIGNING <l_line>.
        MOVE-CORRESPONDING  <l_line> TO ls_ui_option.
        ls_ui_option-name = ls_ui_maint-fieldname.
        ls_ui_option-type = lc_s.
        APPEND ls_ui_option TO et_ui_options.
      ENDLOOP.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " PARAMETER_FILL
*&---------------------------------------------------------------------*
*&      Module  F4_CLASSNAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_classname INPUT.
  DATA: l_sel_index TYPE sy-tabix.

  PERFORM dynp_values_read USING 'IT_PLT_PARA-CLASSNAME' sy-subrc.
  READ TABLE it_f4hlp INDEX 1.

  REFRESH: it_help_value, it_valuetab, it_help_vtab, it_dynpfields.

  it_help_value-tabname      =  'RZLLITAB'.
  it_help_value-fieldname    =  'CLASSNAME'.
  it_help_value-selectflag   =  'X'.
  APPEND it_help_value.

  it_help_value-tabname      =  'RZLLITAB'.
  it_help_value-fieldname    =  'APPLSERVER'.
  it_help_value-selectflag   =  ' '.
  APPEND it_help_value.

  LOOP AT it_rzllitab.
    it_valuetab-value        =  it_rzllitab-classname.
    APPEND  it_valuetab.
    it_valuetab-value        =  it_rzllitab-applserver.
    APPEND  it_valuetab.
  ENDLOOP.


  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            titel                       = 'Logon/Server Group Name'
       IMPORTING
            index                       = g_sel_index
*           SELECT_VALUE                  =
       TABLES
            fields                      = it_help_value
            select_values               = it_help_vtab
            valuetab                    = it_valuetab.

  IF    g_sel_index  >  0.
    l_sel_index = g_sel_index.
    IF l_sel_index  = 1.
    ELSE.
      g_sel_index = ( l_sel_index * 2 ) - 1.
    ENDIF.

    READ TABLE it_valuetab INDEX g_sel_index.

    it_dynpfields-fieldname    = 'IT_PLT_PARA-CLASSNAME'.
    it_dynpfields-fieldvalue   = it_valuetab-value.
    it_dynpfields-stepl        = it_f4hlp-stepl.
    APPEND it_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname     = sy-cprog
        dynumb     = sy-dynnr
      TABLES
        dynpfields = it_dynpfields.

  ENDIF.
ENDMODULE.                 " F4_CLASSNAME  INPUT
*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0852   text
*      -->P_G_F4RC  text
*----------------------------------------------------------------------*
FORM dynp_values_read  USING
     fname LIKE dynpread-fieldname
     dv_rc LIKE sy-subrc.

  DATA: char_x(1) TYPE c VALUE 'X'.
  DATA g_f4dyn TYPE sydynnr.

  it_f4hlp-fieldname = fname.
  APPEND it_f4hlp.

*------- Nicht SY-DYNNR, SY-REPID direkt verwenden, gibt Probleme ------
  g_f4dyn = sy-dynnr.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'ZHPPO32080'
      dynumb               = g_f4dyn
      determine_loop_index = char_x
    TABLES
      dynpfields           = it_f4hlp
    EXCEPTIONS
      OTHERS               = 01.

  dv_rc = sy-subrc.
ENDFORM.                    " DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*&      Form  GET_PLT_SETTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_PLANT_PARA_SETTING  text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT * FROM zsapbf_plt_para
     INTO CORRESPONDING FIELDS OF TABLE it_plt_para
    WHERE tran_code = gv_tran_code.
ENDFORM.                    " GET_PLT_SETTING

*&---------------------------------------------------------------------*
*&      Form  GET_RZLLITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_PLANT_PARA_SETTING  text
*----------------------------------------------------------------------*
FORM get_rzllitab.
  REFRESH it_rzllitab.
  SELECT *
    INTO TABLE it_rzllitab
    FROM rzllitab
   WHERE grouptype EQ 'S'.

  DELETE ADJACENT DUPLICATES FROM it_rzllitab COMPARING classname applserver.

  SORT it_rzllitab BY classname applserver.
ENDFORM.                    " GET_RZLLITAB
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validate_data INPUT.
* Tim Msg 476831: Check Logon/Server Group
  READ TABLE it_plt_para INTO ls_plt_para INDEX tc_0100-current_line.

  READ TABLE it_rzllitab WITH KEY classname = ls_plt_para-classname.
  IF sy-subrc <> 0.
    " &1 is not a valid logon/server group
    MESSAGE e964(zsapbf_ppc0) WITH ls_plt_para-classname.
  ENDIF.
* END Tim Msg 476831
ENDMODULE.                 " VALIDATE_DATA  INPUT
