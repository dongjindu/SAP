*----------------------------------------------------------------------*
*   INCLUDE ZRSD01R_WO_DETAIL_ONLINE_PAI                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'BACK'.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'CANC'.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
