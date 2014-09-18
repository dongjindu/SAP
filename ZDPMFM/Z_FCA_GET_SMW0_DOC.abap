FUNCTION z_fca_get_smw0_doc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_OBJID) TYPE  C
*"     VALUE(I_OPEN) TYPE  C OPTIONAL
*"     VALUE(I_DOWN) TYPE  C OPTIONAL
*"     VALUE(I_FILEPATH) TYPE  C OPTIONAL
*"     VALUE(I_FILENAME) TYPE  C OPTIONAL
*"  TABLES
*"      T_FILE STRUCTURE  W3MIME OPTIONAL
*"  EXCEPTIONS
*"      OBJECT_ID_ERROR
*"      DOC_NOT_FOUND
*"----------------------------------------------------------------------



  DATA: if_factory     TYPE REF TO i_oi_document_factory,
        if_document    TYPE REF TO i_oi_document_proxy,
        if_link_server TYPE REF TO i_oi_link_server.

  DATA: l_retcode        TYPE t_oi_ret_string,
        l_doc_size       TYPE i,
        l_doc_type(80)   VALUE soi_doctype_word97_document,
        l_doc_format(80) TYPE c,
        l_filepath       TYPE string,
        l_filename       TYPE string.

*---// Init FACTORY
  IF if_factory IS INITIAL.
    CALL METHOD c_oi_factory_creator=>get_document_factory
      IMPORTING
        factory = if_factory
        retcode = l_retcode.
    IF l_retcode NE c_oi_errors=>ret_ok.
      EXIT.
    ENDIF.

    CALL METHOD if_factory->start_factory
      EXPORTING
        r3_application_name = i_filename
      IMPORTING
        retcode             = l_retcode.

    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

    CALL METHOD if_factory->get_link_server
      IMPORTING
        link_server = if_link_server
        retcode     = l_retcode.

    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.

    CALL METHOD if_link_server->start_link_server
      IMPORTING
        retcode = l_retcode.

    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.
  ENDIF.

*---// Get file
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      object_id        = i_objid
    IMPORTING
      data_size        = l_doc_size
      document_format  = l_doc_format
      document_type    = l_doc_type
    TABLES
      data_table       = t_file[]
    EXCEPTIONS
      object_not_found = 1
      internal_error   = 2
      OTHERS           = 3.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF l_doc_size IS INITIAL.
    RAISE doc_not_found.
  ENDIF.

  CALL METHOD if_factory->get_document_proxy
    EXPORTING
      document_type  = l_doc_type
    IMPORTING
      document_proxy = if_document
      retcode        = l_retcode.
  CALL METHOD c_oi_errors=>show_message
    EXPORTING
      type = 'E'.

*---// Open document
  IF i_open EQ 'X'.
    CALL METHOD if_document->open_document_from_table
      EXPORTING
        document_table = t_file[]
        document_size  = l_doc_size
      IMPORTING
        retcode        = l_retcode.
    CALL METHOD c_oi_errors=>show_message
      EXPORTING
        type = 'E'.
  ENDIF.

  CALL METHOD if_document->close_document
    EXPORTING
      do_save = 'X'
    IMPORTING
      retcode = l_retcode.

  IF i_down EQ 'X'.
    IF i_filepath IS INITIAL.
      MOVE: 'C:\' TO l_filepath.
    ELSE.
      MOVE: i_filepath TO l_filepath.
    ENDIF.

    IF i_filename IS INITIAL.
      CONCATENATE l_filepath i_objid INTO l_filename.
    ELSE.
      CONCATENATE l_filepath i_filename INTO l_filename.
    ENDIF.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = l_filename
        filetype                = 'BIN'
        BIN_FILESIZE            = l_doc_size
*        NO_AUTH_CHECK           = 'X'

      TABLES
        data_tab                = t_file[]
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
        OTHERS                  = 22.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDIF.
ENDFUNCTION.
