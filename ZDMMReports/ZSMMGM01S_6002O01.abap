*----------------------------------------------------------------------*
*   INCLUDE ZSMMGM01S_6002O01                                          *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.

* This object is needed to handle the ABAP Objects Events of Controls
  IF crv_h_alv IS INITIAL.
    CREATE OBJECT crv_h_alv.    "Handler of ALV
  ELSE.
  ENDIF.

* Set handler
  SET HANDLER crv_h_alv->handle_user_command
              crv_h_alv->handle_menu_button
              crv_h_alv->handle_toolbar FOR ALL INSTANCES.

* Custom Control
  cc_name = 'CC_0100'.
  IF crv_custom_container IS INITIAL.
    CREATE OBJECT crv_custom_container
      EXPORTING container_name = cc_name.

    CREATE OBJECT crv_gui_alv_grid
      EXPORTING i_parent = crv_custom_container.

* Set a titlebar for the grid control
    wa_layout-grid_title = 'Display for GI Request Slip'.
    wa_layout-sel_mode   = 'A'.     "Activate Row Selection

* Set column header
    PERFORM mask_columns TABLES it_fcat.

* Set Sort sequence and Prepare [Display several same value one time].
*    PERFORM make_IT_sort.

* Show ALV Control
    CALL METHOD crv_gui_alv_grid->set_table_for_first_display
      EXPORTING
*    I_BYPASSING_BUFFER            =
*    I_BUFFER_ACTIVE               =
*    I_CONSISTENCY_CHECK           =
        i_structure_name              = 'ZSMM_6002_01'
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
        is_layout                     = wa_layout   "Title
*    IS_PRINT                      =
*    it_special_groups             =   "For listbox
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
      CHANGING
        it_outtab                  = it_zsmm_6002_01 "field text change
        it_fieldcatalog            = it_fcat[]"For sort
*    IT_SORT                       =
*    IT_FILTER                     =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD crv_gui_alv_grid->refresh_table_display
      EXPORTING
*       IS_STABLE      =
        i_soft_refresh =  'X'
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.
ENDMODULE.                 " pbo_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
* Instanciate PF-STATUS & TITLEBAR.
  IF w_title IS INITIAL.
    w_title = 'Display Data Processing Log'.
  ENDIF.
  CREATE OBJECT crv_ps
    EXPORTING im_ps      = 'PS'                "PF-STATUS
              im_it_func = it_func             "Excluding func
              im_tb      = 'TB'                "TITLEBAR
              im_title   = w_title.            "TITLE
  CLEAR it_func.

* Dynamic Function Code Text
  dynftext  = 'Form Printing'.

ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_selected_rows  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_selected_rows OUTPUT.
  LOOP AT it_roid INTO wa_roid.
    wa_row-index = wa_roid-row_id.
    APPEND wa_row TO it_row.
  ENDLOOP.
  CALL METHOD crv_gui_alv_grid->set_selected_rows
     EXPORTING
        it_index_rows = it_row.
  CLEAR: it_row.
ENDMODULE.                 " set_selected_rows  OUTPUT
