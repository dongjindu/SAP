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

    DATA: SEL_WTAB LIKE ZSPP_APP102.
    CLEAR: LTAB1.  REFRESH: LTAB1.

    READ TABLE ITAB1 INDEX E_ROW-INDEX INTO SEL_WTAB.
    IF SY-SUBRC = 0.

       SELECT *
       INTO TABLE LTAB1
       FROM ZTPP_PP_LOG_DETA
       WHERE LOGKEY EQ SEL_WTAB-LOGKEY.

   ENDIF.
      PERFORM SUBMIT_PROCESS USING SEL_WTAB-PROGRAMM.

*      EXPORT LTAB1 TO DATABASE INDX(ZZ) ID VARIANT1.
*      SUBMIT (SEL_WTAB-PROGRAMM) WITH VARIANT1 EQ VARIANT1
*                                AND RETURN.
    ENDMETHOD.
  ENDCLASS.
