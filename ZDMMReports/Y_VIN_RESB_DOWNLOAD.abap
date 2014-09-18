REPORT y_vin_resb_download .

PARAMETERS: p_werks LIKE t001w-werks,
            p_atwrt LIKE ausp-atwrt,
            p_datum LIKE sy-datum,
            p_arbpl LIKE crhd-arbpl.

DATA: it_itab LIKE zspp_vin_info_for_nstl OCCURS 0 WITH HEADER LINE.

CALL FUNCTION 'Z_FPP_GET_NON_SUPPLY_TO_LINE'
     EXPORTING
          i_werks                  = p_werks
          i_atwrt                  = p_atwrt
          i_date                   = p_datum
          i_arbpl                  = p_arbpl
     TABLES
          t_supply_info            = it_itab
     EXCEPTIONS
          no_data_founded          = 1
          line_info_does_not_exist = 2
          etc_exception            = 3
          uph_info_does_not_exist  = 4
          OTHERS                   = 5.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

CALL FUNCTION 'DOWNLOAD'
     EXPORTING
          filename                = 'C:\TEMP\VIN_RESB.TXT'
          filetype                = 'DAT'
     TABLES
          data_tab                = it_itab
     EXCEPTIONS
          invalid_filesize        = 1
          invalid_table_width     = 2
          invalid_type            = 3
          no_batch                = 4
          unknown_error           = 5
          gui_refuse_filetransfer = 6
          customer_error          = 7
          OTHERS                  = 8.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
