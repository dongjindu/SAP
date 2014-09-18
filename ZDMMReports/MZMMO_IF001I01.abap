*----------------------------------------------------------------------*
*   INCLUDE MZMMO_IF001I01                                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  cancel  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cancel INPUT.
  clear gv_first.
  leave to screen 0.
ENDMODULE.                 " cancel  INPUT
*&---------------------------------------------------------------------*
*&      Module  read_tc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE read_tc INPUT.
  modify it_sub index tc_list-current_line.
ENDMODULE.                 " read_tc  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  data: l_count type i.
  clear l_count.
  clear: ok_code.
  ok_code = sy-ucomm.
  clear sy-ucomm.

  case ok_code.
    when 'BACK'.
      leave to screen 0.
    when 'DIS'.
      loop at it_sub where l_mark = 'X'.
        move-corresponding it_sub to st_sub.
        l_count = sy-tabix.
      endloop.
      if l_count > 0.
        call screen 300.
      else.
        message s899(f2) with 'No Row Select'.
      endif.
    when 'CRT'.
      clear : st_sub.
      call screen 200.
  endcase.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  clear ok_code.
  ok_code = sy-ucomm.
  clear sy-ucomm.

  case ok_code.
    when 'BACK'.
      leave to screen 0.
    when 'SAVE'.
      insert ztmm_if002 from st_sub.
      if sy-subrc = 0.
        message s899(f2) with 'Create Success'.
        clear gv_first.
        leave to screen 0.
      endif.
  endcase.

ENDMODULE.                 " user_command_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  clear ok_code.
  ok_code = sy-ucomm.
  clear sy-ucomm.

  case ok_code.
    when 'BACK'.
      leave to screen 0.
    when 'SAVE'.
      update ztmm_if002 from st_sub.
      if sy-subrc = 0.
        message s899(f2) with 'Modify Success'.
        clear gv_first.
        leave to screen 0.
      endif.
  endcase.
ENDMODULE.                 " user_command_0300  INPUT
