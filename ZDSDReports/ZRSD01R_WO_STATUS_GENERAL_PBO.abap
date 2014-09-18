*----------------------------------------------------------------------*
*   INCLUDE ZRSD01R_WO_STATUS_GENERAL_PBO                              *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

*  SET PF-STATUS 'ZRSD01R_1'.
*  SET TITLEBAR  'ZRSD01R_1'.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.

  IF CONTAINER1 IS INITIAL.
    CREATE OBJECT CONTAINER1
        EXPORTING CONTAINER_NAME = 'CONTAINER1'.

    CREATE OBJECT ALV_GRID1
        EXPORTING I_PARENT = CONTAINER1.
  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRANSFER_DATA OUTPUT.

  GS_VARIANT1-REPORT = SY-REPID.

  CALL METHOD ALV_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'ZSSD_WO_GENERAL'
                 IS_VARIANT       = GS_VARIANT1
                 I_SAVE           = 'A'
                 IS_LAYOUT        = GS_LAYOUT
       CHANGING  IT_OUTTAB        = ITAB1
                 IT_FIELDCATALOG  = GT_FIELDCAT_LVC[].

ENDMODULE.                 " TRANSFER_DATA  OUTPUT
