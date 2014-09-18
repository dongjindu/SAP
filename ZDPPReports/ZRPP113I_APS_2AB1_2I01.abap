*----------------------------------------------------------------------*
*   INCLUDE ZRPP113I_APS_2AB1_2I01                                     *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'INQ'.
      PERFORM SELECT_DATA.
    WHEN 'EXIT'.
       LEAVE PROGRAM.
    WHEN 'BACK'.
       LEAVE TO SCREEN 0.
    WHEN 'EXCL'.
      PERFORM DOWNLOAD_DATA.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
