*----------------------------------------------------------------------*
*   INCLUDE YTEST_KGH02_O01                                            *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters - Main Screen
*----------------------------------------------------------------------*
module initialization_1209 output.
  if WA_INIT_1209 is initial.
    perform set_parameter_1209.
    WA_INIT_1209 = 'X'.
  endif.
endmodule.                 " initialization  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1209  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar
*----------------------------------------------------------------------*
module status_1209 output.
  set pf-status 'STATUS100'.
  set titlebar 'SCREEN100'.

endmodule.                 " status_1209  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       Modifying Screen
*----------------------------------------------------------------------*
module modify_screen_1209 output.
  modify screen.
endmodule.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status_1210  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar
*----------------------------------------------------------------------*
module status_1210 output.
  set pf-status 'STATUS110'.
  set titlebar 'SCREEN110'.

endmodule.                 " status_1210  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen_110  OUTPUT
*&---------------------------------------------------------------------*
*       Modifying Screen - Sub Screen
*----------------------------------------------------------------------*
module modify_screen_1210 output.
*
  loop at screen.
    if ( screen-name = 'IT_NEW_APP227-FORDER' ) .
      read table IT_ERROR_1210
                 with key forder = it_new_app227-forder.
      if sy-subrc = 0.
        screen-intensified = 1.
      else.
        screen-intensified = 0.
      endif.
      modify screen.
    endif.
  endloop.
endmodule.                 " modify_screen_110  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INITIALIZATION02  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Initial Parameters - Sub Screen
*----------------------------------------------------------------------*
module initialization_1210 output.
  if WA_INIT_1210 is initial.
*   SETTING PARAMETERS.
    p_keycode = 'AALC'.  "KEY CODE
    p_erdat = sy-datum.  "DATE ON WHICH THE RECORD WAS CREATED
    p_erzet = sy-uzeit.  "ENTRY TIME
    p_ernam = sy-uname.  "NAME OF PERSON WHO CHANGED OBJECT
*   Setting internal fields.
    perform setting_internal_fields_1210.
    clear it_new_app227.
    refresh it_new_app227.

    WA_INIT_1210 = 'X'.
  endif.
endmodule.                 " INITIALIZATION02  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SETTING_TC  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Table Control's total line amount - Main Screen
*----------------------------------------------------------------------*
module setting_tc_1209 output.
  data wa_line1 type i.
*
  describe table it_app227 lines wa_line1.
  tc_app227-lines = wa_line1.

endmodule.                 " SETTING_TC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SETTING_TC_NEW_APP227  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Table Control's Total Line Amount - Sub Screen
*----------------------------------------------------------------------*
module setting_tc_new_app227 output.
  data wa_line type i.
*
  describe table it_new_app227 lines wa_line.
  tc_new_app227-lines = wa_line.

endmodule.                 " SETTING_TC_NEW_APP227  OUTPUT
