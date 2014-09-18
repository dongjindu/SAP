....
*&---------------------------------------------------------------------*
*&      Module  modify_data_APP245  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_APP245 INPUT.
  MODIFY IT_APP245 INDEX TC_APP245-current_line.

ENDMODULE.                 " modify_data_APP245  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_APP245  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_APP245 INPUT.
  CASE ok_code.
    WHEN 'SEA'.
      CLEAR ok_code.
      PERFORM search_data_APP245.

    WHEN 'EXC'.
      CLEAR ok_code.
      PERFORM download_data_app245.

    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR ok_code.
*     Sort By Ascending
      PERFORM sort_ascending_APP245.

    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR ok_code.
*     Sort By Descending
      PERFORM sort_descending_APP245.

  ENDCASE.
ENDMODULE.                 " user_command_APP245  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
