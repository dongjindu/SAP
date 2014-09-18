*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM44E_6030CLA                                          *
*----------------------------------------------------------------------*
TYPES: ty_func TYPE TABLE OF rsmpe-func.
TYPES: ty_ps(20).
TYPES: ty_tb(60).
TYPES: ty_title(80).
*---------------------------------------------------------------------*
*       CLASS lcl_ps DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_ps DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
                    IMPORTING im_ps      TYPE ty_ps
                              im_it_func TYPE ty_func
                              im_tb      TYPE ty_tb
                              im_title   TYPE ty_title.
  PRIVATE SECTION.
    DATA: ps    TYPE ty_ps,
          tb    TYPE ty_tb,
          title TYPE ty_title.
    DATA: it_func TYPE ty_func.  "Internal table
ENDCLASS.                    "lcl_ps DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_ps IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_ps IMPLEMENTATION.
  METHOD constructor.
    ps      = im_ps.
    tb      = im_tb.
    it_func = im_it_func.
    title   = im_title.
    SET PF-STATUS ps EXCLUDING it_func.
    SET TITLEBAR  tb WITH title.
  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_ps IMPLEMENTATION

************************************************************************
* CLASS LCL_H_ALV: Definition
*                  local class to define and handle own functions.
************************************************************************
CLASS lcl_h_alv DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
     handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
            IMPORTING e_object e_interactive,

     handle_menu_button
        FOR EVENT menu_button OF cl_gui_alv_grid
            IMPORTING e_object e_ucomm,

    handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.

  PRIVATE SECTION.
ENDCLASS.
****************************************************************
* CLASS LCL_H_ALV: Implementation
****************************************************************
CLASS lcl_h_alv IMPLEMENTATION.
  METHOD handle_toolbar.
* 2.At event TOOLBAR define a toolbar element of type 2 by using
*    event paramenter E_OBJECT. Remember its function code.
*.......
* Part I: Define a menu button including a function code that
*         is evaluated in 'HANDLE_MENU_BUTTON'
*.......

* append a separator to normal toolbar
    CLEAR  wa_toolbar.
    MOVE   3 TO wa_toolbar-butn_type.
    APPEND wa_toolbar TO e_object->mt_toolbar.

* append a menut o switch between detail levels.
    CLEAR wa_toolbar.
    MOVE 'FC01' TO wa_toolbar-function.
* --> This function code is evaluated in 'handle_menu_button'
    MOVE icon_execute_object TO wa_toolbar-icon.
    MOVE 'Printing'          TO wa_toolbar-quickinfo.
    MOVE 2                   TO wa_toolbar-butn_type.
    MOVE space               TO wa_toolbar-disabled.
    APPEND wa_toolbar        TO e_object->mt_toolbar.

  ENDMETHOD.
*--------------------------------------------------------------------
  METHOD handle_menu_button.
* ?3.At event MENU_BUTTON query your function code and define a
*     menu in the same way as a context menu.
*..........
* Part II: Evaluate 'e_ucomm' to see which menu button of the toolbar
*          has been clicked on.
*          Define then the corresponding menu.
*          The menu contains function codes that are evaluated
*          in 'handle_user_command'.
*...........

* handle own menubuttons
    IF e_ucomm = 'FC01'.
*/1
      CALL METHOD e_object->add_function
                  EXPORTING fcode   = 'DETA'
                            text    = 'Detailed Selected View'.
*/2
*      CALL METHOD e_object->add_function
*                  EXPORTING fcode   = 'SFPP'
*                            text    = 'Print Preview'.
    ENDIF.
  ENDMETHOD.
*---------------------------------------------------------------------
  METHOD handle_user_command.
* 4.At event USER_COMMAND query the function code of each function
* defined in step 3.
*.........
* Part III : Evaluate user command to invoke the corresponding
*            function.
*.........
*/ Get Selected rows
    PERFORM get_selected_rows
                     USING it_row.
*/ Get  lt_ztmm_6030_01_selected
    DATA: lt_ztmm_6030_01 LIKE it_ztmm_6030_01.
    PERFORM get_itable_selected
                     USING    it_row
                              it_ztmm_6030_01
                     CHANGING lt_ztmm_6030_01.
*/
    CASE e_ucomm.
      WHEN 'DETA'.     "Detailed Summarized View

* Set column header
        CLEAR: it_fcat[].
        PERFORM mask_columns TABLES it_fcat.
* Detailed Selected View Display
        PERFORM dsp_detailed_view
                     USING 'ZSMM_6030_01'
                           lt_ztmm_6030_01.
    ENDCASE.
  ENDMETHOD.                           "handle_user_command
ENDCLASS.
