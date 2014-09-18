*----------------------------------------------------------------------*
*   INCLUDE YTEST_KGH02_I01                                            *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  user_command_1209  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands - Main Screen
*----------------------------------------------------------------------*
module user_command_1209 input.
  case ok_code.
*
    when 'RLS'.  "RELEASE.
      perform release_1209.
      clear ok_code.
*
    when 'SEL'.  "SEARCH.
      perform setup_parameter_1209.
      perform select_data_1209.
      clear ok_code.
*
    when 'ADD'.  "CREATE NEW DATA.
      clear ok_code.

      call screen '1210' starting at 10 3 ending at 70 22.
*
    when 'DLT'.  "DELETE.
      perform delete_data_1209.
      perform setup_parameter_1209.
      perform select_data_1209.
      clear ok_code.
*
    when 'EXL'.  "DOWNLOAD.
      perform make_file_1209.
      perform download_1209.
      clear ok_code.

    when 'SORTA'. "SORTING ASCENDING.
      perform sort_ascending_1209.
      clear ok_code.

    when 'SORTD'. "SORTING DESCENDING.
      perform sort_descending_1209.
      clear ok_code.
  endcase.
endmodule.                 " user_command_1209  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
module exit_1209 input.
  leave program.
endmodule.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_1210  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands - Sub Screen
*----------------------------------------------------------------------*
module user_command_1210 input.
  case ok_code.
    when 'BACK'.
      set screen '1209'.
      leave to screen 0.
      clear ok_code.
      clear WA_INIT_1210.
    when 'CLEAR'.
      clear it_new_app227.
      refresh it_new_app227.
      clear IT_ERROR_1210.
      refresh IT_ERROR_1210.
    when 'SAVE'.
      perform save_new_data_1210.
      clear ok_code.
  endcase.
endmodule.                 " user_command_1210  INPUT
*&---------------------------------------------------------------------*
*&      Module  BACK  INPUT
*&---------------------------------------------------------------------*
*       Setting a Command - Back
*----------------------------------------------------------------------*
module back input.
  leave to screen 0.
endmodule.                 " BACK  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_WO  INPUT
*&---------------------------------------------------------------------*
*       Modifying Internal Table with Table Control's Current Line
*----------------------------------------------------------------------*
module check_wo_1210 input.
  perform check_work_order_1210 using it_new_app227-forder.
  modify it_new_app227 index tc_new_app227-current_line.
*  if sy-subrc <> 0.
*    append it_new_app227.
*  endif.

endmodule.                 " CHECK_WO  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       Modifying Internal Table with Table Control's Current Line
*----------------------------------------------------------------------*
module modify_data_1209 input.
  modify it_app227 index tc_app227-current_line.
  if sy-subrc <> 0.
    append it_app227 .
  endif.

endmodule.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  worder_search_help  INPUT
*&---------------------------------------------------------------------*
*       Setting Search-Help Command - Work Order
*----------------------------------------------------------------------*
module worder_search_help_1209 input.
  clear reason_tab. refresh reason_tab.
  clear dynpfields. refresh dynpfields.
  dynpfields-fieldname = 'P_WORDER'.
  append dynpfields.
  call function 'DYNP_VALUES_READ'
       exporting
            dyname               = sy-repid
            dynumb               = sy-dynnr
            determine_loop_index = 'X'
       tables
            dynpfields           = dynpfields
       exceptions
            others               = 9.

  read table dynpfields with key fieldname = 'P_WORDER'.
* -- WORK ORDER SEARCH
  data: l_plant(04),
        l_model(06),
        l_worder(20).

  clear: l_plant, l_model, l_worder.
  concatenate p_plant '%' into l_plant.
  concatenate p_model '%' into l_model.
  concatenate p_worder '%' into l_worder.

  select distinct worder
    into reason_tab-code
    from ztpp_spec
    where plant like l_plant and
          model like l_model and
          worder like l_worder .
    append reason_tab.
  endselect.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
       exporting
            retfield    = 'CODE'
            dynpprog    = sy-cprog
            dynpnr      = sy-dynnr
            dynprofield = 'P_WORDER'
            value_org   = 'S'
       tables
            value_tab   = reason_tab.

  if sy-subrc  <> 0.
    message i000 with 'NOT FOUND....'.
  endif.
endmodule.                 " worder_search_help  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data_110  INPUT
*&---------------------------------------------------------------------*
*       Modifying Internal Table with Table Control's Current Line
*----------------------------------------------------------------------*
module modify_data_1210 input.
  modify it_new_app227 index tc_new_app227-current_line.

  if sy-subrc <> 0.
    append it_new_app227 .
  endif.

endmodule.                 " modify_data_110  INPUT
*&---------------------------------------------------------------------*
*&      Module  wo_c_search_help  INPUT
*&---------------------------------------------------------------------*
*       Setting Search-Help Command - Work Order
*----------------------------------------------------------------------*
module wo_c_search_help input.
  clear reason_tab. refresh reason_tab.
  clear dynpfields. refresh dynpfields.
  dynpfields-fieldname = 'IT_NEW_APP227-FORDER'.
  append dynpfields.
  call function 'DYNP_VALUES_READ'
       exporting
            dyname               = sy-repid
            dynumb               = sy-dynnr
            determine_loop_index = 'X'
       tables
            dynpfields           = dynpfields
       exceptions
            others               = 9.

  read table dynpfields with key fieldname = 'IT_NEW_APP227-FORDER'.
* -- WORK ORDER SEARCH
  select distinct matnr
    into reason_tab-code
    from mara
    where mtart = 'WOCL' .
    append reason_tab.
  endselect.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
       exporting
            retfield    = 'CODE'
            dynpprog    = sy-cprog
            dynpnr      = sy-dynnr
*            dynprofield = 'P_WORDER'
            value_org   = 'S'
       tables
            value_tab   = reason_tab.

  if sy-subrc  <> 0.
    message i000 with 'NOT FOUND....'.
  endif.

endmodule.                 " wo_c_search_help  INPUT
