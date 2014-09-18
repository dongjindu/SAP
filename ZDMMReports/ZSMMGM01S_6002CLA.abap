*----------------------------------------------------------------------*
*   INCLUDE ZSMMGM01S_6002CLA                                          *
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
                              im_title   TYPE ty_title..
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
* ?2.At event TOOLBAR define a toolbar element of type 2 by using
*    event paramenter E_OBJECT. Remember its function code.
*.......
* Part I: Define a menu button including a function code that
*         is evaluated in 'handle_MENU_BUTTON
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
    MOVE 'Printing' TO wa_toolbar-quickinfo.
    MOVE 2 TO wa_toolbar-butn_type.
    MOVE space TO wa_toolbar-disabled.
    APPEND wa_toolbar TO e_object->mt_toolbar.

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
      CALL METHOD e_object->add_function
                  EXPORTING fcode   = 'SFFC'
                            text    = 'Form Printing'.
      CALL METHOD e_object->add_function
                  EXPORTING fcode   = 'SFPP'
                            text    = 'Print Preview'.
    ENDIF.
  ENDMETHOD.
*---------------------------------------------------------------------
  METHOD handle_user_command.
* ?4.At event USER_COMMAND query the function code of each function
*     defined in step 3.
*.........
* Part III : Evaluate user command to invoke the corresponding
*            function.
*.........
    DATA: lt_rows TYPE lvc_t_row.

* get selected row
    CALL METHOD crv_GUI_alv_grid->get_selected_rows
             IMPORTING et_index_rows = lt_rows.

* Transfer the methods to the frontend using command FLUSH .
    CALL METHOD cl_gui_cfw=>flush.
    IF sy-subrc NE 0.
* add your handling, for example
      pgm = sy-repid.
      CALL FUNCTION 'POPUP_TO_INFORM'
           EXPORTING
                titel = pgm
                txt2  = sy-subrc
                txt1  = 'Error in Flush'(500).
    ENDIF.
* Make lt_zsmm_6002_01_sf

    DATA: ls_rows LIKE LINE OF lt_rows.
    DATA: lt_zsmm_6002_01_sf TYPE TABLE OF zsmm_6002_01.
    DATA: ls_zsmm_6002_01_sf LIKE LINE OF lt_zsmm_6002_01_sf.
    LOOP AT lt_rows INTO ls_rows.
      READ TABLE it_zsmm_6002_01 INTO wa_zsmm_6002_01
                                 INDEX ls_rows-index.
*      LOOP AT IT_zsmm_6002_01 INTO WA_zsmm_6002_01
*                              FROM ls_rows-index.
      APPEND wa_zsmm_6002_01 TO lt_zsmm_6002_01_sf.
*      ENDLOOP.
    ENDLOOP.
    SORT lt_zsmm_6002_01_sf BY rsnum.
    DELETE ADJACENT DUPLICATES FROM lt_zsmm_6002_01_sf
                               COMPARING rsnum.

    CLEAR: output_options, control_parameters.
    CASE e_ucomm.
      WHEN 'SFFC'.     "Form Printing
* set output options
*/Begin of Changed by Hakchin(20040128)
        IF sy-sysid = 'UQ1'.  "QAS System
          output_options-tddest        = 'LOCM'.
        ELSE.
          output_options-tddest        = 'LOCL'.
        ENDIF.
*/End of Changed by Hakchin(20040128)
*        output_options-tdimmed       = space.
        output_options-tdimmed       = 'X'.  "Print Immediately
        output_options-tdnewid       = 'X'.  "New Spool Request
*       control_parameters-preview   = 'X'.
        control_parameters-no_dialog = 'X'.   "Printing Dialog Box

* set color for company logo
        color = 'BMON'.

* determine the name of the generated function module
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
             EXPORTING
                  formname           = 'ZSFF_MMGI01'
             IMPORTING
                  fm_name            = func_module_name
             EXCEPTIONS
                  no_form            = 1
                  no_function_module = 2
                  OTHERS             = 3.
        CASE sy-subrc.
          WHEN 0.
*            PERFORM set_interface.

            control_parameters-no_close = space.

            LOOP AT lt_zsmm_6002_01_sf INTO ls_zsmm_6002_01_sf.
              PERFORM process_form USING    ls_zsmm_6002_01_sf-rsnum
                                   CHANGING return_code.
              IF return_code NE 0.
                EXIT.
              ENDIF.
*              control_parameters-no_open = 'X'.
              control_parameters-no_open = space.
              "If you want to reprint,
              IF return_code EQ 0.
                SKIP 2.
                WRITE:
                  'Function module executed', " Function module executed
                  40 func_module_name,
                  / 'Form used'(i03),         " Form used
                  40 'ZSFF_MMGI01'.           " Form
              ENDIF.
            ENDLOOP.

          WHEN OTHERS.
*            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDCASE.

      WHEN 'SFPP'.     "Print Preview
* set output options
*/Begin of Changed by Hakchin(20040128)
        IF sy-sysid = 'UQ1'.  "QAS System
          output_options-tddest        = 'LOCM'.
        ELSE.
          output_options-tddest        = 'LOCL'.
        ENDIF.
*/End of Changed by Hakchin(20040128)
*        output_options-tdimmed       = space.
        output_options-tdimmed       = 'X'.  "Print Immediately
        output_options-tdnewid       = 'X'.  "New Spool Request
        control_parameters-preview   = 'X'.
        control_parameters-no_dialog = 'X'.   "Printing Dialog Box

* set color for company logo
        color = 'BMON'.

* determine the name of the generated function module
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
             EXPORTING
                  formname           = 'ZSFF_MMGI01'
             IMPORTING
                  fm_name            = func_module_name
             EXCEPTIONS
                  no_form            = 1
                  no_function_module = 2
                  OTHERS             = 3.
        CASE sy-subrc.
          WHEN 0.
*            PERFORM set_interface.

            control_parameters-no_close = space.

            LOOP AT lt_zsmm_6002_01_sf INTO ls_zsmm_6002_01_sf.
              PERFORM process_form USING    ls_zsmm_6002_01_sf-rsnum
                                   CHANGING return_code.
              IF return_code NE 0.
                EXIT.
              ENDIF.
*              control_parameters-no_open = 'X'.
              control_parameters-no_open = space.
              "If you want to reprint,
              IF return_code EQ 0.
                SKIP 2.
                WRITE:
                  'Function module executed', " Function module executed
                  40 func_module_name,
                  / 'Form used'(i03),         " Form used
                  40 'ZSFF_MMGI01'.           " Form
              ENDIF.
            ENDLOOP.

          WHEN OTHERS.
*            MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDCASE.

    ENDCASE.
  ENDMETHOD.                           "handle_user_command
ENDCLASS.
