....
*&---------------------------------------------------------------------*
*&      Module  modify_data_APP244  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_APP244 INPUT.
  MODIFY IT_APP244 INDEX TC_APP244-current_line.

ENDMODULE.                 " modify_data_APP244  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_APP244  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_APP244 INPUT.
  CASE ok_code.
    WHEN 'SEA'.
      CLEAR ok_code.
      PERFORM search_data_app244.

    WHEN 'EXC'.
      CLEAR ok_code.
      PERFORM download_data_app244.

    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR ok_code.
*     Sort By Ascending
      PERFORM sort_ascending_APP244.

    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR ok_code.
*     Sort By Descending
      PERFORM sort_descending_APP244.

  ENDCASE.
ENDMODULE.                 " user_command_APP244  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
