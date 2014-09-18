*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_PBO                                 *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.

  SET PF-STATUS 'ZASD04R'.
  SET TITLEBAR  'ZASD04R'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.

  CASE SY-DYNNR.
    WHEN '9000'.
      IF CONTAINER1 IS INITIAL.
        CREATE OBJECT G_APPLICATION.
        CREATE OBJECT CONTAINER1
            EXPORTING CONTAINER_NAME = 'CONTAINER1'.

        CREATE OBJECT ALV_GRID1
            EXPORTING I_PARENT = CONTAINER1.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRANSFER_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TRANSFER_DATA OUTPUT.

  GS_VARIANT1-REPORT = SY-REPID.

  CASE SY-DYNNR.
    WHEN '9000'.
      CALL METHOD ALV_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
           EXPORTING I_STRUCTURE_NAME = 'ZSPP_APP102'
                     IS_VARIANT       = GS_VARIANT1
                     I_SAVE           = 'A'
                     IS_LAYOUT        = GS_LAYOUT
           CHANGING  IT_OUTTAB        = ITAB1
                     IT_FIELDCATALOG  = GT_FIELDCAT_LVC[].

      SET HANDLER G_APPLICATION->HANDLE_DOUBLE_CLICK FOR ALV_GRID1.

  ENDCASE.

ENDMODULE.                 " TRANSFER_DATA  OUTPUT
