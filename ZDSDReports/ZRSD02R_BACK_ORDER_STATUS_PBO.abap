*----------------------------------------------------------------------*
***INCLUDE ZRSD02R_BACK_ORDER_STATUS_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'RSD02'.
  SET TITLEBAR 'RSD02'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.
  IF CONTAINER IS INITIAL.
    CREATE OBJECT CONTAINER
        EXPORTING CONTAINER_NAME = 'CONTAINER'.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = CONTAINER.
  ENDIF.
ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
MODULE TRANSFER_DATA OUTPUT.
  GS_VARIANT-REPORT = SY-REPID.
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'ZSSD_BACK_OR_ST'
                 IS_VARIANT       = GS_VARIANT
                 I_SAVE           = 'A'
       CHANGING  IT_OUTTAB        = IT_BACK_OR.
ENDMODULE.                 " TRANSFER_DATA  OUTPUT
