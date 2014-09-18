....
*&---------------------------------------------------------------------*
*&      Module  modify_data_app240  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data_app240 INPUT.
  MODIFY it_app240 INDEX tc_app240-current_line.

ENDMODULE.                 " modify_data_app240  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_app240  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_app240 INPUT.
  CASE ok_code.
    WHEN 'SEA'.
      CLEAR ok_code.
      PERFORM search_data_APP240.

    WHEN 'EXC'.
      CLEAR ok_code.
      PERFORM download_data_APP240.

    WHEN 'SORTA'. "SORTING ASCENDING.
      CLEAR ok_code.
*     Sort By Ascending
      PERFORM sort_ascending_app240.

    WHEN 'SORTD'. "SORTING DESCENDING.
      CLEAR ok_code.
*     Sort By Descending
      PERFORM sort_descending_app240.

  ENDCASE.
ENDMODULE.                 " user_command_app240  INPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE PROGRAM.
ENDMODULE.                 " exit  INPUT
