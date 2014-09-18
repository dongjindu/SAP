*----------------------------------------------------------------------*
***INCLUDE MZAPP903M_INPUT_PLANO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_9000 output.
  SET PF-STATUS 'APP903'.
  SET TITLEBAR 'APP903'.
endmodule.                 " STATUS_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_data output.
  if wa_init = space.
     wa_init = 'X'  .
     select * into corresponding fields of table it_data
       from ztpp_input_plan.
     sort it_data by status descending serial.
  endif.
endmodule.                 " init_data  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_9001 output.
  SET PF-STATUS 'APP903_9001'.
  SET TITLEBAR 'APP903_9001'.
endmodule.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  init_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_9001 output.
  if wa_point is initial.
     describe table it_data lines wa_point.
  endif.
endmodule.                 " init_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_data  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_data OUTPUT.
  it_data-tabix = tc_9000-current_line.
  modify it_data index tc_9000-current_line.
ENDMODULE.                 " DISPLAY_data  OUTPUT
