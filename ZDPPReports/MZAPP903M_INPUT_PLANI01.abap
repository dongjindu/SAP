*----------------------------------------------------------------------*
***INCLUDE MZAPP903M_INPUT_PLANI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE ok_code.
    WHEN 'EXIT' OR 'CANC'  .
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
*          LEAVE TO SCREEN 0.
      LEAVE PROGRAM    .
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  MODIFY it_data INDEX tc_9000-current_line.
ENDMODULE.                 " MODIFY_data  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  data: l_flag                type c.

  sv_code = ok_code.
  CLEAR: ok_code.
  CASE sv_code.
    WHEN 'BACK'.
      perform check_save     .
      check wa_change = space.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      " Reloading Data from Vehicle Master
      PERFORM recalc_data.
    WHEN 'SAVE'.
      CLEAR: wa_init     .
      perform save_index .
      PERFORM save_data  using  l_flag.
    WHEN 'BLOCK'.
      PERFORM set_block  .
    WHEN 'TRANS'.
      PERFORM move_data  .
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  DATA: l_error.

  IF ok_code1 = 'OKAY'    .
    PERFORM check_point  USING l_error.
    IF l_error = 'X'     .
      MESSAGE i001(zmpp) WITH text-001.
      EXIT.
    ENDIF.
    it_temp[] = it_data[].
    INSERT LINES OF it_data FROM wa_sblock TO wa_eblock
               INTO it_temp INDEX wa_point .
    it_data[] = it_temp[].
    delete it_data FROM wa_sblock TO wa_eblock.
    LEAVE TO SCREEN 0   .
  ENDIF.
ENDMODULE.                 " USER_COMMAND_9001  INPUT
