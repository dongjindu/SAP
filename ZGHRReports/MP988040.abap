*----------------------------------------------------------------------*
*                                                                      *
*       Subroutines for infotype 9880                                  *
*                                                                      *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  attach_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM attach_upload.

  DATA: lt_files      TYPE filetable,
        ls_files      LIKE LINE OF lt_files,
        lv_rc         TYPE i,
        lv_filepath   TYPE string,
        lv_result     TYPE abap_bool,
        lv_cnt        TYPE i,
        lv_length     TYPE i.

  DATA: BEGIN OF lt_data OCCURS 0,
          buf(1024)   TYPE x,
        END OF lt_data.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = lt_files
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
*      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE lt_files INTO ls_files INDEX 1.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  lv_filepath = ls_files.

  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file                 = lv_filepath
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
*      not_supported_by_gui = 4
      OTHERS               = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  IF lv_result IS INITIAL.
*    MESSAGE 'FIle does not exist' type 'E'.
    MESSAGE e004(zghrm).
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename                = lv_filepath
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_length
    CHANGING
      data_tab                = lt_data[]
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
*      not_supported_by_gui    = 17
*      error_no_gui            = 18
      OTHERS                  = 19.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: gt_zghrlt0001, gt_zghrlt0001[].
  LOOP AT lt_data.
    CLEAR gt_zghrlt0001.
    gt_zghrlt0001-seqnr = sy-tabix.
    gt_zghrlt0001-zmcontent = lt_data-buf.
    IF lv_length > 1024.
      gt_zghrlt0001-length = 1024.
      SUBTRACT 1024 FROM lv_length.
    ELSE.
      gt_zghrlt0001-length = lv_length.
    ENDIF.
    APPEND gt_zghrlt0001.
  ENDLOOP.

  CLEAR: lt_files, ls_files.
  SPLIT lv_filepath AT '\' INTO TABLE lt_files.
*  lv_cnt = lines( lt_files ).
  DESCRIBE TABLE lt_files LINES lv_cnt.
  READ TABLE lt_files INTO ls_files INDEX lv_cnt.

  p9880-zmfilen = ls_files-filename.

ENDFORM.                    " attach_upload
*&---------------------------------------------------------------------*
*&      Form  attach_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM attach_display.

  DATA: lv_file     TYPE string.
  DATA: lv_temp_dir TYPE string.
  DATA: lv_execute  TYPE text255.
  DATA: lt_out_tab  TYPE STANDARD TABLE OF tbl1024 WITH HEADER LINE.
*  DATA: lt_out_tab  TYPE STANDARD TABLE OF blob WITH HEADER LINE.

*  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*    EXPORTING
*      buffer     = gs_zghrlt0001-zmcontent
*    TABLES
*      binary_tab = lt_out_tab.

*  LOOP AT gt_zghrlt0001.
*    CLEAR lt_out_tab.
*    lt_out_tab-line = gt_zghrlt0001-zmcontent.
*    APPEND lt_out_tab.
*  ENDLOOP.

  DATA: lv_raw_size          TYPE i,
        lv_binary_size       TYPE i.

  LOOP AT gt_zghrlt0001.
*    CLEAR lv_raw_size.
*    DESCRIBE FIELD gt_zghrlt0001-zmcontent LENGTH lv_raw_size.
    lv_binary_size = lv_binary_size + gt_zghrlt0001-length.

    CLEAR lt_out_tab.
    lt_out_tab-line = gt_zghrlt0001-zmcontent.
    APPEND lt_out_tab.
  ENDLOOP.

*  CALL METHOD cl_gui_frontend_services=>get_sapgui_workdir
*    CHANGING
*      sapworkdir            = lv_temp_dir
*    EXCEPTIONS
*      get_sapworkdir_failed = 1
*      cntl_error            = 2
*      error_no_gui          = 3
*      not_supported_by_gui  = 4
*      OTHERS                = 5.
  CALL METHOD cl_gui_frontend_services=>directory_get_current
    CHANGING
      current_directory            = lv_temp_dir
    EXCEPTIONS
      directory_get_current_failed = 1
      cntl_error                   = 2
*      error_no_gui                 = 3
*      not_supported_by_gui         = 4
      OTHERS                       = 5.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  IF lv_temp_dir IS INITIAL.
    lv_temp_dir = 'C:\Temp'.
  ENDIF.

  CONCATENATE lv_temp_dir '\' p9880-zmfilen INTO lv_file.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_binary_size
      filename                = lv_file
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_out_tab[]
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
*      not_supported_by_gui    = 22
*      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  CALL FUNCTION 'GUI_DOWNLOAD'
*       EXPORTING
*            BIN_FILESIZE            = binary_size
*            filename                = lv_file
*            filetype                = 'BIN'
*       TABLES
*            data_tab                = lt_out_tab[]
*       EXCEPTIONS
*            file_write_error        = 1
*            no_batch                = 2
*            gui_refuse_filetransfer = 3
*            invalid_type            = 4
*            no_authority            = 5
*            unknown_error           = 6
*            header_not_allowed      = 7
*            separator_not_allowed   = 8
*            filesize_not_allowed    = 9
*            header_too_long         = 10
*            dp_error_create         = 11
*            dp_error_send           = 12
*            dp_error_write          = 13
*            unknown_dp_error        = 14
*            access_denied           = 15
*            dp_out_of_memory        = 16
*            disk_full               = 17
*            dp_timeout              = 18
*            file_not_found          = 19
*            dataprovider_exception  = 20
*            control_flush_error     = 21
*            OTHERS                  = 22.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.





  CONCATENATE 'file://' lv_file INTO lv_execute.

  CALL FUNCTION 'CALL_BROWSER'
     EXPORTING
       url                    = lv_execute
       window_name            = 'Attached File'
*      new_window             = ' '
*      browser_type           =
*      contextstring          =
     EXCEPTIONS
       frontend_not_supported = 1
       frontend_error         = 2
       prog_not_found         = 3
       no_batch               = 4
       unspecified_error      = 5
       OTHERS                 = 6.
  IF sy-subrc <> 0.
* implement suitable error handling here
  ENDIF.

ENDFORM.                    " attach_display
*&---------------------------------------------------------------------*
*&      Form  save_attach
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_attach.

  DATA: lv_cnt     TYPE i.

  DESCRIBE TABLE gt_zghrlt0001 LINES lv_cnt.

*  IF lines( gt_zghrlt0001 ) > 0.
  IF lv_cnt > 0.

    IF p9880-adatanr IS INITIAL.
      CALL FUNCTION 'GUID_CREATE'
           IMPORTING
                ev_guid_32 = p9880-adatanr.
    ENDIF.

    LOOP AT gt_zghrlt0001.
      gt_zghrlt0001-adatanr = p9880-adatanr.
      gt_zghrlt0001-aedtm   = sy-datum.
      gt_zghrlt0001-uname   = sy-uname.
      MODIFY gt_zghrlt0001.
    ENDLOOP.

    MODIFY zghrlt0001 FROM TABLE gt_zghrlt0001.

  ENDIF.

ENDFORM.                    " save_attach
*&---------------------------------------------------------------------*
*&      Form  dele_attach
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dele_attach.

  DELETE FROM zghrlt0001 WHERE adatanr = p9880-adatanr.

ENDFORM.                    " dele_attach
