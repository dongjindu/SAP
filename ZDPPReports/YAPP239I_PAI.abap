*----------------------------------------------------------------------*
*   INCLUDE YAPP239_PAI                                                *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       Setting Exit Command
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*    Modification of Internal Table with Table Control's Current line
*----------------------------------------------------------------------*
module modify_data input.
  modify it_app239 index tc_app239-current_line.
endmodule.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_0110  INPUT
*&---------------------------------------------------------------------*
*       Setting Commands
*----------------------------------------------------------------------*
module user_command_0110 input.
  case ok_code.
    when 'SEL'.
      clear ok_code.
      perform make_data.

    when 'EXL'.
      clear ok_code.
      perform download.

    when 'SORTA'. "SORTING ASCENDING.
      clear ok_code.
      perform sort_ascending.

    when 'SORTD'. "SORTING DESCENDING.
      clear ok_code.
      perform sort_descending.

  endcase.
endmodule.                 " user_command_0110  INPUT
