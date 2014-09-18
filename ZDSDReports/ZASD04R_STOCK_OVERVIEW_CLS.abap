*----------------------------------------------------------------------*
*   INCLUDE ZASD04R_STOCK_OVERVIEW_CLS                                 *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*   CLASS LCL_APPLICATION DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_APPLICATION DEFINITION.
  PUBLIC SECTION.
    METHODS HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW E_COLUMN.
ENDCLASS.
*---------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_APPLICATION IMPLEMENTATION.
*---------------------------------------------------------------------*

  METHOD HANDLE_DOUBLE_CLICK.
    DATA: SEL_WTAB LIKE ZSSD_STOCK_OV1.

    READ TABLE ITAB1 INDEX E_ROW-INDEX INTO SEL_WTAB.
    IF SY-SUBRC = 0.
      P_EX  = SEL_WTAB-EXTC.
      P_IT  = SEL_WTAB-INTC.
      LOOP AT LTAB INTO T_ITAB2 WHERE LGORT = SEL_WTAB-LGORT
                   AND   MATNR = SEL_WTAB-MATNR
                   AND   EXTC  = SEL_WTAB-EXTC
                   AND   INTC  = SEL_WTAB-INTC.
*        MOVE-CORRESPONDING LTAB TO T_ITAB2.
        APPEND T_ITAB2 TO ITAB2.
        CLEAR: LTAB, T_ITAB2.
      ENDLOOP.
   ENDIF.
      CALL SCREEN 9100.
    ENDMETHOD.
  ENDCLASS.
