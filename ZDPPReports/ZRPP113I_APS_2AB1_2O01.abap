*----------------------------------------------------------------------*
*   INCLUDE ZRPP113I_APS_2AB1_2O01                                     *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT_DATA OUTPUT.
  PERFORM MAKE_DEFAULT_MONTH.
*   PERFORM MAKE_DROPDOWN_LIST_BOX.
ENDMODULE.                 " INIT_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
 SET PF-STATUS 'ST200'.
 SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
   IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
*    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_PVV02AB_OUT'.
    PERFORM ASSIGN_ITAB_TO_ALV.
   ELSE.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_PVV02AB_OUT'.
    PERFORM ASSIGN_ITAB_TO_ALV.

*    WA_STBL-ROW = 'X'.
*    WA_STBL-COL = 'X'.
*    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*      EXPORTING IS_STABLE = WA_STBL.
  ENDIF.
  IF GRID_CONTAINER_tot IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT_tot.
    PERFORM SET_ATTRIBUTES_ALV_GRID_tot.
*    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM EXCLUDE_TB_FUNCTIONS.
    PERFORM BUILD_FIELD_CATALOG_tot USING 'IT_PVV01RR_OUT'.
    PERFORM ASSIGN_ITAB_TO_ALV_TOT.
   ELSE.
    PERFORM BUILD_FIELD_CATALOG_TOT USING 'IT_PVV01RR_OUT'.
    PERFORM ASSIGN_ITAB_TO_ALV_TOT.

*    WA_STBL-ROW = 'X'.
*    WA_STBL-COL = 'X'.
*    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*      EXPORTING IS_STABLE = WA_STBL.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
