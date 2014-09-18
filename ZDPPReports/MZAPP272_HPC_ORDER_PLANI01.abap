*----------------------------------------------------------------------*
*   INCLUDE MZAPP272_HPC_ORDER_PLANI01                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  OKCODE_02 = OK_CODE.
  CASE OKCODE_02.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_APP272 INPUT.
  OKCODE_02 = OK_CODE.
  CLEAR OK_CODE.
  CASE OKCODE_02.
    WHEN 'ENTER'.
      PERFORM READ_PROCESS_APP272.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
