*&---------------------------------------------------------------------*
*&  Include           ZRHR_APP_DOC_CREATE_BDCF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout_header .

  CLEAR: gt_title[], gs_title.

  gs_title-col_01 = text-h01.
  gs_title-col_02 = text-h02.
  gs_title-col_03 = text-h03.
  gs_title-col_04 = text-h04.
  gs_title-col_05 = text-h05.
  gs_title-col_06 = text-h06.
  gs_title-col_07 = text-h07.
  gs_title-col_08 = text-h08.
  gs_title-col_09 = text-h09.
  gs_title-col_10 = text-h10.
  gs_title-col_11 = text-h11.
  gs_title-col_12 = text-h12.
  gs_title-col_13 = text-h13.
  gs_title-col_14 = text-h14.

  APPEND gs_title TO gt_title.

ENDFORM.                    " LAYOUT_HEADER
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_layout .

  DATA: l_index(2)    TYPE n,
        l_field(20)   TYPE c.

  FIELD-SYMBOLS: <fs_value> TYPE any.

  CREATE OBJECT application 'excel.application'.
  SET PROPERTY OF application 'visible' = 1.

  CALL METHOD OF
      application
      'workbooks' = workbook.
  CALL METHOD OF
      workbook
      'Add'.

* create first excel sheet
  CALL METHOD OF
      application
      'Worksheets' = sheet
    EXPORTING
      #1           = 1.
  CALL METHOD OF
      sheet
      'Activate'.
  SET PROPERTY OF sheet 'Name' = 'Sheet1'.

  CLEAR gs_title.
  READ TABLE gt_title INTO gs_title
                      INDEX 1.
  CLEAR l_index.
  DO 12 TIMES.
    ADD 1 TO l_index.
    CALL METHOD OF
        sheet
        'Cells' = cells
      EXPORTING
        #1      = l_index.
    CONCATENATE 'GS_TITLE-COL_' l_index INTO l_field.
    ASSIGN (l_field) TO <fs_value>.
    SET PROPERTY OF cells 'Value' = <fs_value>.
    SET PROPERTY OF cells 'ColumnWidth' = 13.
    GET PROPERTY OF cells 'Interior' = color.
    SET PROPERTY OF color 'ColorIndex' = 33.
    GET PROPERTY OF cells 'Borders' = border.
    SET PROPERTY OF border 'LineStyle' = 1.
    GET PROPERTY OF cells 'Font' = bold.
    SET PROPERTY OF bold 'Bold' = 1.
    GET PROPERTY OF cells 'Font' = size.
    SET PROPERTY OF size 'Size' = 9.
  ENDDO.

* save excel speadsheet to particular filename
  CALL METHOD OF
      sheet
      'SaveAs'

    EXPORTING
      #1       = g_down_path  "filename
      #2       = 1.           "fileFormat

ENDFORM.                    " DOWNLOAD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_data .

  DATA: lt_intern             TYPE TABLE OF alsmex_tabline,
        ls_intern             LIKE LINE OF lt_intern,
        l_index               TYPE i,
        l_field_type          TYPE c,
        l_content             TYPE string,
        l_mcnt                TYPE i,
        l_datatype            TYPE dd01v-datatype,
        l_message             TYPE string,
        l_datum               TYPE sy-datum,
        ls_symsg              TYPE symsg,
        l_flag(1).

  FIELD-SYMBOLS: <fs_data>    TYPE any,
                 <fs_record>  TYPE any.

  CLEAR l_flag.

  IF p_fname CS '.XLS'.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_fname
        i_begin_col             = 1
        i_begin_row             = 2
        i_end_col               = c_col
        i_end_row               = c_row
      TABLES
        intern                  = lt_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CHECK lines( lt_intern ) > 0.

    CLEAR: record[].
    LOOP AT lt_intern INTO ls_intern.
      l_index = ls_intern-col.
      ASSIGN COMPONENT l_index OF STRUCTURE record TO <fs_record>.
      <fs_record> = ls_intern-value.

      AT END OF row.
        APPEND record.
        CLEAR: ls_intern, record.
      ENDAT.
    ENDLOOP.

*   data consistency check
    LOOP AT record.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE gs_record TO <fs_data>.
        IF sy-subrc = 0.
          ASSIGN COMPONENT sy-index OF STRUCTURE record TO <fs_record>.
          l_content = <fs_record>.
*          " Data Format Setting.
*          DESCRIBE FIELD <fs_data> TYPE l_field_type.
          CASE sy-index.
*            WHEN 'N' OR 'P' OR 'I' OR 'F' OR 'T'.
            WHEN 3 OR 4 OR 5 OR 6 OR 7 OR 8 OR 9 OR 10 OR 11 OR 12.
              TRANSLATE l_content USING ', . ~ ` ! @ # $ % ^ & * ( ) - _ + = < > ? : ; " '.
              CONDENSE l_content NO-GAPS.
              " Numeric Check.
              CALL FUNCTION 'NUMERIC_CHECK'
                EXPORTING
                  string_in = l_content
                IMPORTING
                  htype     = l_datatype.
              IF l_datatype <> 'NUMC'.
                l_flag = 'X'.
                MOVE-CORRESPONDING record TO gs_record.
                gs_record-error = 'E'.
                CONCATENATE '[' l_content ']' INTO ls_symsg-msgv1.
                CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                    msgid               = 'ZMHRPMS'
                    msgnr               = '013'
                    msgv1               = ls_symsg-msgv1
                  IMPORTING
                    message_text_output = l_message.
                IF 1 = 2.
                  MESSAGE e013.
                ENDIF.
                gs_record-etext = l_message.
                APPEND gs_record TO gt_record.
                EXIT.
              ELSE.
                <fs_data> = l_content.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = <fs_data>
                  IMPORTING
                    output = <fs_data>.
              ENDIF.

*            WHEN 'D'.
            WHEN 1 OR 2.
              " Effective Date check
*              CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*                EXPORTING
*                  input  = l_content
*                IMPORTING
*                  output = l_datum.
              l_datum = l_content.
              CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
                EXPORTING
                  date                      = l_datum
                EXCEPTIONS
                  plausibility_check_failed = 1
                  OTHERS                    = 2.
              IF sy-subrc <> 0.
                l_flag = 'X'.
                MOVE-CORRESPONDING record TO gs_record.
                gs_record-error = 'E'.
                CONCATENATE '[' l_content ']' INTO ls_symsg-msgv1.
                CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                    msgid               = 'ZMHRPMS'
                    msgnr               = '014'
                    msgv1               = ls_symsg-msgv1
                  IMPORTING
                    message_text_output = l_message.
                IF 1 = 2.
                  MESSAGE e014.
                ENDIF.
                gs_record-etext = l_message.
                APPEND gs_record TO gt_record.
                EXIT.

              ELSE.
                <fs_data> = l_content.
              ENDIF.

*            WHEN 'C' OR 'g'. " Char Or String.
*              <fs_data> = l_content.
          ENDCASE.
        ELSE.
          EXIT.
        ENDIF.

        CLEAR: l_content, l_field_type, l_message.
      ENDDO.

      IF l_flag IS INITIAL.
        APPEND gs_record TO gt_record.
        CLEAR: gs_record, record.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.                    " GET_FILE_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  CHECK lines( gt_record ) > 0.

  CALL FUNCTION 'ZFHR_APP_DOC_CREATE_BDC'
    TABLES
      it_record = gt_record.

  CLEAR: gs_record.
  LOOP AT gt_record INTO gs_record.
*   set flag
    IF gs_record-error = 'E'.
      gs_record-light = '@0A@'. " Red
    ELSE.
      gs_record-light = '@08@'. " Green
    ENDIF.

    MODIFY gt_record  FROM gs_record
                      INDEX sy-tabix
                      TRANSPORTING light.
  ENDLOOP.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_alv_100 .

  IF gr_cont IS INITIAL.
    CREATE OBJECT gr_cont
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = gr_cont.

    PERFORM set_layout.
    PERFORM set_fcat.

    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = st_layo
      CHANGING
        it_outtab                     = gt_record[]
        it_fieldcatalog               = it_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD gr_grid->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    " CREATE_ALV_100
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_layout .

  st_layo-zebra = 'X'.
  st_layo-sel_mode = 'D'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_FCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fcat .

  CLEAR: it_fcat[].
  it_fcat-fieldname = 'LIGHT'.
  it_fcat-coltext = text-t15.
  it_fcat-col_pos = 1.
  it_fcat-outputlen = 5.
  it_fcat-just = 'C'.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'BEGDA'.
  it_fcat-coltext = text-t01.
  it_fcat-col_pos = 2.
  it_fcat-outputlen = 10.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'ENDDA'.
  it_fcat-coltext = text-t02.
  it_fcat-col_pos = 3.
  it_fcat-outputlen = 10.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'APPEE'.
  it_fcat-coltext = text-t03.
  it_fcat-col_pos = 4.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'APPER'.
  it_fcat-coltext = text-t04.
  it_fcat-col_pos = 5.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'CORI1'.
  it_fcat-coltext = text-t05.
  it_fcat-col_pos = 6.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'EVAI2'.
  it_fcat-coltext = text-t06.
  it_fcat-col_pos = 7.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'CORI2'.
  it_fcat-coltext = text-t07.
  it_fcat-col_pos = 8.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'EVAI3'.
  it_fcat-coltext = text-t08.
  it_fcat-col_pos = 9.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'CORI3'.
  it_fcat-coltext = text-t09.
  it_fcat-col_pos = 10.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'APPRI'.
  it_fcat-coltext = text-t10.
  it_fcat-col_pos = 11.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'CORI4'.
  it_fcat-coltext = text-t11.
  it_fcat-col_pos = 12.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'HRTMI'.
  it_fcat-coltext = text-t12.
  it_fcat-col_pos = 13.
  it_fcat-no_zero = 'X'.
  it_fcat-outputlen = 8.
  APPEND it_fcat.CLEAR: it_fcat.

*  it_fcat-fieldname = 'ERROR'.
*  it_fcat-coltext = text-t13.
*  it_fcat-col_pos = 14.
*  it_fcat-outputlen = 6.
*  APPEND it_fcat.CLEAR: it_fcat.

  it_fcat-fieldname = 'ETEXT'.
  it_fcat-coltext = text-t14.
  it_fcat-col_pos = 14.
  it_fcat-outputlen = 60.
  APPEND it_fcat.CLEAR: it_fcat.

ENDFORM.                    " SET_FCAT
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_name .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = p_fname
      def_path         = 'c:\'
      mask             = ',*.xls;*xlsx,*.XLS;*.XLSX.'
      mode             = 'O'
      title            = ' '
    IMPORTING
      filename         = p_fname
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.                    " GET_FILE_NAME
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_excel .

  PERFORM layout_header.
  PERFORM get_download_path.
  PERFORM download_result.

ENDFORM.                    " DOWNLOAD_EXCEL
*&---------------------------------------------------------------------*
*&      Form  GET_DOWNLOAD_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_download_path .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = g_down_path
      def_path         = 'c:\'
      mask             = ',*.xls;*xlsx,*.XLS;*.XLSX.'
      mode             = 'O'
      title            = ' '
    IMPORTING
      filename         = g_down_path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF g_down_path IS INITIAL.
    MESSAGE e024 DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.                    " GET_DOWNLOAD_PATH
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_result .

  DATA: l_index(2)    TYPE n,
        l_row         TYPE int4,
        l_field(20)   TYPE c.

  FIELD-SYMBOLS: <fs_value> TYPE any.

  CREATE OBJECT application 'excel.application'.
  SET PROPERTY OF application 'visible' = 1.

  CALL METHOD OF
      application
      'workbooks' = workbook.
  CALL METHOD OF
      workbook
      'Add'.

* create first excel sheet
  CALL METHOD OF
      application
      'Worksheets' = sheet
    EXPORTING
      #1           = 1.
  CALL METHOD OF
      sheet
      'Activate'.
  SET PROPERTY OF sheet 'Name' = 'Sheet1'.

  CLEAR gs_title.
  READ TABLE gt_title INTO gs_title
                      INDEX 1.
  CLEAR l_index.
  DO 14 TIMES.
    ADD 1 TO l_index.
    CALL METHOD OF
        sheet
        'Cells' = cells
      EXPORTING
        #1      = l_index.
    CONCATENATE 'GS_TITLE-COL_' l_index INTO l_field.
    ASSIGN (l_field) TO <fs_value>.
    SET PROPERTY OF cells 'Value' = <fs_value>.
    IF l_index = 13.
      SET PROPERTY OF cells 'ColumnWidth' = 3.
    ELSEIF l_index = 14.
      SET PROPERTY OF cells 'ColumnWidth' = 40.
    ELSE.
      SET PROPERTY OF cells 'ColumnWidth' = 13.
    ENDIF.
    GET PROPERTY OF cells 'Interior' = color.
    SET PROPERTY OF color 'ColorIndex' = 33.
    GET PROPERTY OF cells 'Borders' = border.
    SET PROPERTY OF border 'LineStyle' = 1.
    GET PROPERTY OF cells 'Font' = bold.
    SET PROPERTY OF bold 'Bold' = 1.
    GET PROPERTY OF cells 'Font' = size.
    SET PROPERTY OF size 'Size' = 9.
  ENDDO.

  CLEAR: gs_record, l_row.
  l_row = 1.
  LOOP AT gt_record INTO gs_record.
    l_row = l_row + 1.
    DO 14 TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE gs_record TO <fs_value>.
      IF sy-subrc = 0.
        CALL METHOD OF
            sheet
            'Cells' = cells
          EXPORTING
            #1      = l_row
            #2      = sy-index.
        SET PROPERTY OF cells 'Value' = <fs_value>.
        GET PROPERTY OF cells 'Borders' = border.
        SET PROPERTY OF border 'LineStyle' = 1.
        GET PROPERTY OF cells 'Font' = size.
        SET PROPERTY OF size 'Size' = 9.
      ENDIF.
    ENDDO.
  ENDLOOP.

* save excel speadsheet to particular filename
  CALL METHOD OF
      sheet
      'SaveAs'

    EXPORTING
      #1       = g_down_path  "filename
      #2       = 1.           "fileFormat

ENDFORM.                    " DOWNLOAD_RESULT
