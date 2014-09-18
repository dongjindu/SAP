*----------------------------------------------------------------------*
*   INCLUDE ZTEST_SD_DEL_F01                                           *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form UPLOAD_PROCESS.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            codepage                = ' '
            filename                = p_file
            filetype                = p_filety
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            data_tab                = it_excl
      EXCEPTIONS
           conversion_error        = 1
           file_open_error         = 2
           file_read_error         = 3
           invalid_table_width     = 4
           invalid_type            = 5
           no_batch                = 6
           unknown_error           = 7
           gui_refuse_filetransfer = 8
           customer_error          = 9
           OTHERS                  = 10
            .
  CASE sy-subrc.
    WHEN 0.
      DATA l_text(132).
      CONCATENATE p_file ' DATA UPLOAD SUCCESS!!'
                  INTO l_text.
      WRITE: / l_text.
      SKIP.
    WHEN 2.
      MESSAGE e000 WITH 'FILE OPEN ERROR, FILE NO FOUND!'.
    WHEN 3.
      MESSAGE e000 WITH 'FILE READ ERROR'.
    WHEN OTHERS.
      MESSAGE e000 WITH 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
  ENDCASE.

endform.                    " UPLOAD_PROCESS
