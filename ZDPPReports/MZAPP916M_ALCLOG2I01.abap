*----------------------------------------------------------------------*
***INCLUDE MZAPP916M_ALCLOG2I01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  case ok_code.
    when 'EXIT'.
    WHEN 'CANC'.
      LEAVE PROGRAM.
  endcase.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_data INPUT.
  MODIFY ITPP_ALCLOG2 INDEX TC_9000-CURRENT_LINE.
ENDMODULE.                 " modify_data  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SV_CODE.
    when 'BACK'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      DELETE FROM ZTPP_ALCLOG2 CLIENT SPECIFIED WHERE MANDT =  SY-MANDT.
      MODIFY ZTPP_ALCLOG2 FROM TABLE ITPP_ALCLOG2 .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
