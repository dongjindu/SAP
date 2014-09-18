*----------------------------------------------------------------------*
***INCLUDE ZAPP236M_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar & Screen's Attributes.
*----------------------------------------------------------------------*
module status_2106 output.
  set pf-status '0100'.
  set titlebar  '100'.
  loop at screen.
    if G_ATTR_APP236 = '1'.
      if screen-group1 = 'VIN' or screen-group1 = 'ENG' or
         screen-group1 = 'TMN'.
        screen-input = 0.
      endif.
    endif.
    if G_ATTR_APP236 = '2'.
      if screen-group1 = 'MOD' or screen-group1 = 'ENG' or
         screen-group1 = 'TMN'.
        screen-input = 0.
      endif.
    endif.
    if G_ATTR_APP236 = '3'.
      if screen-group1 = 'MOD' or screen-group1 = 'VIN' or
         screen-group1 = 'TMN'.
        screen-input = 0.
      endif.
    endif.
    if G_ATTR_APP236 = '4'.
      if screen-group1 = 'MOD' or screen-group1 = 'ENG' or
         screen-group1 = 'VIN'.
        screen-input = 0.
      endif.
    endif.
    if screen-group1 eq 'DEA'.
      screen-input = 0.
    endif.
    modify screen.
  endloop.
  move  'HMMA'     to  st_key_app236-company.
  set cursor field G_CRSR_FLD_APP236.

  describe table IT_WIP_APP236  lines  wip_lines.
  tc100-lines = wip_lines.
endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  init_listbox  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Dropdown list boxes
*----------------------------------------------------------------------*
module init_listbox_app236 output.

  refresh  LIST_APP236.

  VALUE_APP236-key  = 'VEH'.
  VALUE_APP236-text = 'Veh.Number'.
  append VALUE_APP236 to LIST_APP236.

  VALUE_APP236-key  = 'VIN'.
  VALUE_APP236-text = 'VIN Number'.
  append VALUE_APP236 to LIST_APP236.

  VALUE_APP236-key  = 'ENG'.
  VALUE_APP236-text = 'Engine Number'.
  append VALUE_APP236 to LIST_APP236.

  VALUE_APP236-key  = 'TMN'.
  VALUE_APP236-text = 'T/M Number'.
  append VALUE_APP236 to LIST_APP236.

  name = 'ST_KEY_APP236-INQOPT'.

  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = LIST_APP236.

endmodule.                 " init_listbox  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*     Setting Status & Titlebar & The Number of Internal Table's lines
*----------------------------------------------------------------------*
module status_0110 output.
  set pf-status '0110'.
  set titlebar  '110' with st_app236-model.
  describe table IT_219_APP236  lines  tc110-lines.
endmodule.                 " STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar - Order List
*----------------------------------------------------------------------*
module status_0120 output.
  refresh  IT_FUNC_APP236.   clear IT_FUNC_APP236.
  if  g_part_APP236 = 'U'.
    move   'UPART'   to  IT_FUNC_APP236-fcode.
    append IT_FUNC_APP236.
  endif.
  if  g_part_APP236 = 'C'.
    move   'CPART'   to  IT_FUNC_APP236-fcode.
    append IT_FUNC_APP236.
  endif.
  if g_part_APP236 = 'U'.
    move   '/ Unique Parts'  to  g_parttit_APP236.
  elseif g_part_APP236 = 'C'.
    move   '/ Color Parts'   to  g_parttit_APP236.
  endif.

  set pf-status '0120'  excluding  IT_FUNC_APP236.
  set titlebar  '120' with  g_parttit_APP236.
  describe table IT_PART_APP236  lines tc120-lines.

endmodule.                 " STATUS_0120  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0130  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Total Data Amount - Air Bag List
*----------------------------------------------------------------------*
module status_0130 output.
  set pf-status '0130'.
*  SET TITLEBAR 'xxx'.
  set cursor  field  'EXIT'.
  describe table IT_ABAG_APP236 lines tc130-lines.
endmodule.                 " STATUS_0130  OUTPUT

* OUTPUT MODULE FOR TABSTRIP 'SS2106': SETS ACTIVE TAB
module ss2106_active_tab_set output.
  ss2106-activetab = g_ss2106-pressed_tab.
  case g_ss2106-pressed_tab.
    when c_ss2106-tab1.
      g_ss2106-subscreen = '2118'.
    when c_ss2106-tab2.
      g_ss2106-subscreen = '2119'.
    when c_ss2106-tab3.
      g_ss2106-subscreen = '2120'.
    when c_ss2106-tab4.
      g_ss2106-subscreen = '2121'.
    when c_ss2106-tab5.
      g_ss2106-subscreen = '2122'.
    when c_ss2106-tab6.
      g_ss2106-subscreen = '2123'.
    when c_ss2106-tab7.
      g_ss2106-subscreen = '2124'.
    when others.
*      DO NOTHING
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_2118  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Total Data Amount - 219 Option
*----------------------------------------------------------------------*
module status_2118 output.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  describe table IT_219_APP236  lines  tc_app236_01-lines.
endmodule.                 " STATUS_2118  OUTPUT
