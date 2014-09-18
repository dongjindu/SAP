*----------------------------------------------------------------------*
***INCLUDE MZVEMAILI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_8000 INPUT.
    CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'FIND'.
      PERFORM FIND_EMAIL.
    WHEN 'SAVEMAIL' OR 'SAVE'.
      PERFORM SAVE_EMAIL.
    endcase.
ENDMODULE.                 " USER_COMMAND_8000  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_lifnr  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lifnr INPUT.
  select single name1 into w_name
    from lfa1
     where lifnr = ZSQM_VEND_EMAIL-lifnr.
  if sy-subrc = 0.
  else.
     mESSAGE e008 WITH ZSQM_VEND_EMAIL-lifnr.
  endif.
*  PERFORM FIND_EMAIL.

ENDMODULE.                 " check_lifnr  INPUT
