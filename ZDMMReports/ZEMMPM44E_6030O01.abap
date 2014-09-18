*----------------------------------------------------------------------*
*   INCLUDE ZEMMPM44E_6030O01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  PERFORM ps_tb.
ENDMODULE.                 " status  OUTPUT
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
  IF crv_gui_custom_container IS INITIAL.
    CREATE OBJECT crv_gui_custom_container
      EXPORTING container_name = cc_name.

    CREATE OBJECT crv_gui_alv_grid
      EXPORTING i_parent = crv_gui_custom_container.

* Set Layout structure(ALV control)(Title, Row Seletion ...)
    PERFORM set_wa_layo.

* Set column header
    PERFORM mask_columns TABLES it_fcat.

* Set Sort sequence and Prepare [Display several same value one time].
    PERFORM make_it_sort.

* Show ALV Control
    CALL METHOD crv_gui_alv_grid->set_table_for_first_display
      EXPORTING
*    I_BYPASSING_BUFFER            =
*    I_BUFFER_ACTIVE               =
*    I_CONSISTENCY_CHECK           =
        i_structure_name              = 'ZSMM_6030_01'
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
        is_layout                     = wa_layo   "Title
*    IS_PRINT                      =
*    it_special_groups             =   "For listbox
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
      CHANGING
        it_outtab                  = it_ztmm_6030_01
        it_fieldcatalog            = it_fcat[]
        it_sort                    = it_sort
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
*&      Module  get_visiblelines  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_w_visiblelines OUTPUT.
  w_visiblelines = sy-loopc. "Screens, number of lines visible in table
ENDMODULE.                 " get_visiblelines  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  wa_to_sf  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE wa_to_sf OUTPUT.
  MOVE-CORRESPONDING wa_ztmm_6030_01 TO wa_ztmm_6030_01_st.
ENDMODULE.                 " wa_to_sf  OUTPUT
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
*&---------------------------------------------------------------------*
*&      Module  desc  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE desc OUTPUT.
*/Description of Material
  PERFORM desc_matnr USING sy-langu
                           p_matnr
                  CHANGING io_maktx.
*/Description of Vendor
  PERFORM desc_lifnr USING    p_lifnr
                     CHANGING io_desc_lifnr.
ENDMODULE.                 " desc  OUTPUT
