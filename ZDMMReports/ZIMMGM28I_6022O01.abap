*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM28I_6022O01                                          *
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
* Custom Control
  cc_name = 'CC_0100'.
  IF crv_custom_container IS INITIAL.
    CREATE OBJECT crv_custom_container
      EXPORTING container_name = cc_name.

    CREATE OBJECT crv_alv_grid
      EXPORTING i_parent = crv_custom_container.

* Set a titlebar for the grid control
    wa_layout-grid_title = 'Display Data Processing Log'.
*    wa_layout-sel_mode   = 'A'.     "Activate Row Selection

* Set column header
    PERFORM mask_columns TABLES it_fieldcat.

* Set Sort sequence and Prepare [Display several same value one time].
*    PERFORM make_IT_sort.

* Show ALV Control
    CALL METHOD crv_alv_grid->set_table_for_first_display
      EXPORTING
*    I_BYPASSING_BUFFER            =
*    I_BUFFER_ACTIVE               =
*    I_CONSISTENCY_CHECK           =
        i_structure_name              = 'ZTMM_6022_01'
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
        it_outtab                  = it_ztmm_6022_01 "field text change
        it_fieldcatalog            = it_fieldcat[]   "For sort
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
    CALL METHOD crv_alv_grid->refresh_table_display
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
