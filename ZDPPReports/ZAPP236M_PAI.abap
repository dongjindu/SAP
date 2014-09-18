*----------------------------------------------------------------------*
***INCLUDE ZAPP236M_PAI .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit_100  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
module exit_100 input.

  leave to screen 0.

endmodule.                 " exit_100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands For Main Screen
*----------------------------------------------------------------------*
module user_command_2106 input.
  perform  key_field_attr_app236.
  if sy-ucomm eq 'ATTR'.
    clear  sy-ucomm. exit.
  endif.
  case st_key_app236-inqopt.
    when 'VEH'.
      perform  vehicle_number_search_app236.
    when 'VIN'.
      perform  vin_number_search_app236.
    when 'ENG'.
      perform  engine_number_search_app236.
    when 'TMN'.
      perform  tm_number_search_app236.
  endcase.

** For Table Control
  case sy-ucomm.
    when '219OPT'.
      perform  219_option_display.   "popup
    when 'AIRBAG'.
      perform  airbag_display.
    when 'ORDER'.
      perform  order_list_display.   "popup
*    WHEN 'CHASSI'.
*    WHEN 'APPR'.
*    WHEN 'CAP'.
    when 'EXCEL'.
      perform  export_to_excel.
  endcase.

  clear sy-ucomm.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_model_check  INPUT
*&---------------------------------------------------------------------*
*       Searching MODEL
*----------------------------------------------------------------------*
module field_model_check_app236 input.
*  select single model  into  st_app236-model
*    from  ztpp_veh_model
*    where  model eq  st_app236-model.
*  if sy-subrc ne 0.
*    message e000  with 'Model code Not found!'.
*  endif.

endmodule.                 " field_model_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_bodyno_check  INPUT
*&---------------------------------------------------------------------*
*       Checking Vehicle Master No.
*----------------------------------------------------------------------*
module field_bodyno_check_app236 input.
  check  not st_app236-model  is initial and
         not st_app236-bodyno is initial.

  concatenate  st_app236-model st_app236-bodyno  into  g_equnr_app236.

  perform  equi_master_check_app236  using  g_equnr_app236
                                            g_equichk_app236.
  if not g_equichk_app236 is initial.
    move   st_key_app236-inqopt   to  st_code_app236-inqopt.
    move   st_app236-model    to  st_code_app236-model.
    move   st_app236-bodyno   to  st_code_app236-bodyno.
    clear: st_app236, st_iss_app236.
    move-corresponding st_code_app236  to:  st_iss_app236, st_app236.
    message e000  with 'Vehicle Master Not found!'.
  endif.
endmodule.                 " field_bodyno_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_vin_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module field_vin_check_app236 input.


endmodule.                 " field_vin_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_engine_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module field_engine_check_app236 input.

endmodule.                 " field_engine_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  field_tm_check  INPUT
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
module field_tm_check_app236 input.

endmodule.                 " field_tm_check  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command - 219 Option List
*----------------------------------------------------------------------*
module user_command_0110 input.
  if sy-ucomm eq 'EXIT'.
    leave to screen  0.
  endif.
endmodule.                 " USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands - Order List
*----------------------------------------------------------------------*
module user_command_0120 input.

  case sy-ucomm.
    when 'EXIT'.
      leave to screen  0.
    when 'UPART'.
      perform  unique_part_display.
    when 'CPART'.
      perform  color_part_display.
  endcase.
endmodule.                 " USER_COMMAND_0120  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0130  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command - Air Bag List
*----------------------------------------------------------------------*
module user_command_0130 input.
  case sy-ucomm.
    when 'EXIT'.
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0130  INPUT

* INPUT MODULE FOR TABSTRIP 'SS2106': GETS ACTIVE TAB
module ss2106_active_tab_get input.
  ok_code = sy-ucomm.
  case ok_code.
    when c_ss2106-tab1.
      g_ss2106-pressed_tab = c_ss2106-tab1.
    when c_ss2106-tab2.
      g_ss2106-pressed_tab = c_ss2106-tab2.
    when c_ss2106-tab3.
      g_ss2106-pressed_tab = c_ss2106-tab3.
    when c_ss2106-tab4.
      g_ss2106-pressed_tab = c_ss2106-tab4.
    when c_ss2106-tab5.
      g_ss2106-pressed_tab = c_ss2106-tab5.
    when c_ss2106-tab6.
      g_ss2106-pressed_tab = c_ss2106-tab6.
    when c_ss2106-tab7.
      g_ss2106-pressed_tab = c_ss2106-tab7.
    when others.
*      DO NOTHING
  endcase.
endmodule.
