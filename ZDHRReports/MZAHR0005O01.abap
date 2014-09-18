*----------------------------------------------------------------------*
*   INCLUDE MZAHR0005O01                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_5000 OUTPUT.
  set pf-status 'PS5000'.
  set titlebar '500'.
*
  w_text1 = 'Code Book Maintenance : Module & Group'.
  w_text2 = 'Code Book Maintenance : code'.
ENDMODULE.                 " STATUS_5000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  case w_modes.
    when 0.   SET PF-STATUS 'PS9000' excluding it_stats.
    when 1.   SET PF-STATUS 'PS9000'.
  endcase.
*
  set titlebar '900'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
MODULE modify_screen OUTPUT.
  loop at screen.
    if screen-group1 = 'GR1'.
      screen-input = w_modes.
      modify screen.
    endif.
  endloop.
ENDMODULE.                 " modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9100 OUTPUT.
  case w_modes.
    when 0.   SET PF-STATUS 'PS9100' excluding it_stats.
    when 1.   SET PF-STATUS 'PS9100'.
  endcase.
*
  SET TITLEBAR '910'.
ENDMODULE.                 " STATUS_9100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LOOP_READ_mark  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LOOP_READ_mark OUTPUT.
  READ TABLE it_pcp02 INDEX TC9100-CURRENT_LINE.
    if    w_tc_index =  TC9100-CURRENT_LINE.
       it_pcp02-chkbx = 'X'.
    else.
       it_pcp02-chkbx = ''.
    endif.

ENDMODULE.                 " LOOP_READ_mark  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  LOOP_READ_mark_1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE LOOP_READ_mark_1 OUTPUT.
  READ TABLE it_pcp01 INDEX TC9000-CURRENT_LINE.
    if    w_tc_index =  TC9000-CURRENT_LINE.
       it_pcp01-chkbx = 'X'.
    else.
       it_pcp01-chkbx = ''.
    endif.


ENDMODULE.                 " LOOP_READ_mark_1  OUTPUT
