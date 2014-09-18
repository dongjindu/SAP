*----------------------------------------------------------------------*
***INCLUDE ZRSD10R_ORDER_BALANCE_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'RSD10'.
  SET TITLEBAR 'RSD10'.
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

  IF CONTAINER2 IS INITIAL.
    CREATE OBJECT CONTAINER2
        EXPORTING CONTAINER_NAME = 'CONTAINER2'.

    CREATE OBJECT ALV_GRID2
        EXPORTING I_PARENT = CONTAINER2.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
MODULE TRANSFER_DATA OUTPUT.

  GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT2-REPORT = SY-REPID.

  PERFORM MAKE_HEADER.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'ZSSD_OR_DEVIATION'
                 IS_VARIANT       = GS_VARIANT
                 I_SAVE           = 'A'
                 IS_LAYOUT        = GS_LAYOUT
       CHANGING  IT_OUTTAB        = ITAB
                 IT_FIELDCATALOG  = GT_FIELDCAT_LVC[].

  CALL METHOD ALV_GRID2->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'ZSSD_OR_DEV_COUNT'
                 IS_VARIANT       = GS_VARIANT2
                 I_SAVE           = 'A'
       CHANGING  IT_OUTTAB        = ITAB2.

ENDMODULE.                 " TRANSFER_DATA  OUTPUT
