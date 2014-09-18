*----------------------------------------------------------------------*
*   INCLUDE YAPP223L_FOB                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Setting Status & Titlebar
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
*  SET TITLEBAR '100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  set_field
*&---------------------------------------------------------------------*
*       Setting a Parameter - Model
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_model.
  SELECT DISTINCT model name
    INTO (model_value-key , model_value-text)
    FROM ztpp_veh_model.
    APPEND model_value TO model_list.

  ENDSELECT.

ENDFORM.                    " set_field
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       Calling a Func. For Setting a Dropdown List Box
*----------------------------------------------------------------------*
*      -->P_MODEL_LIST  text
*----------------------------------------------------------------------*
FORM call_function_VRM USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.

ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  set_field02
*&---------------------------------------------------------------------*
*       Setting a Parameter - PART
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_PART.
  CLEAR: part_value-key, part_value-text.
  MOVE 'U' TO part_value-key.
  MOVE 'Unique Part' TO part_value-text.
  APPEND part_value TO part_list.
*
  CLEAR: part_value-key, part_value-text.
  MOVE 'C' TO part_value-key.
  MOVE 'Color Part' TO part_value-text.
  APPEND part_value TO part_list.

ENDFORM.                    " set_field02
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD03
*&---------------------------------------------------------------------*
*       Setting a Parameter - Part's Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_KEY.
  DATA l_count(03).
  DO 200 TIMES.
    l_count = l_count + 1.
    CLEAR: key_value-key, key_value-text.
    MOVE l_count TO key_value-key.
    MOVE l_count TO key_value-text.
    APPEND key_value TO key_list.
  ENDDO.
*

ENDFORM.                    " SET_FIELD03
*&---------------------------------------------------------------------*
*&      Form  check_essential_condition
*&---------------------------------------------------------------------*
*       Checking Essential Condition & Code Structure
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_essential_condition_1205.
  IF p_model IS INITIAL.
    MESSAGE s000 WITH 'Model needs to be filled'.
    EXIT.
  ENDIF.
  IF p_part IS INITIAL.
    MESSAGE s000 WITH 'Part needs to be filled'.
    EXIT.
  ENDIF.
  IF p_key IS INITIAL.
    MESSAGE s000 WITH 'Key needs to be filled'.
    EXIT.
  ENDIF.
  IF p_part = 'C' AND p_key > 50.
    MESSAGE s000 WITH 'The maxium key value is 50'.
    EXIT.
  ENDIF.
  IF p_part = 'U' AND p_key > 200.
    MESSAGE s000 WITH 'The maxium key value is 200'.
    EXIT.
  ENDIF.

* Full Code(ALC) = p_model+'_ALC_'+p_part+'_'+p_key.
  CONDENSE p_key.
  CONCATENATE p_model '_ALC_' p_part '_' p_key
    INTO p_full_code.

  CLEAR: wa_descriptions, it_column.
  REFRESH: wa_descriptions, it_column.

  CALL FUNCTION 'CARD_TABLE_READ_STRUCTURE'
    EXPORTING
      var_tab                    = p_full_code
*   CHANGE_NO                  =
*   DATE                       =
      language                   = 'E'
* IMPORTING
*   BASIC_DATA                 =
*   RETURN                     =
    TABLES
      descriptions               = wa_descriptions
      characteristics            = it_column
*   VALUE_ASSIGNMENT_ALT       =
    EXCEPTIONS
      error                      = 1
      OTHERS                     = 2 .
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  MOVE wa_descriptions-descript TO p_col_name.

  DATA: l_offset TYPE i.
  DATA: l_col_name(20).

  CLEAR: p_key_01, p_key_02, p_key_03, p_key_04, p_key_05,
         p_key_06, p_key_07, p_key_08, p_key_09, p_key_10,
         p_key_11, p_key_12, p_key_13, p_key_14, p_key_15,
         p_key_16, p_key_17, p_key_18, p_key_19, p_key_20.

  LOOP AT it_column.
    SEARCH it_column-charact FOR 'ALC'.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      CLEAR: l_offset, l_col_name.
      SEARCH it_column-charact FOR '219'.
      IF sy-subrc = 0.
        l_offset = sy-fdpos + 4.
        l_col_name = it_column-charact+l_offset.
      ELSE.
        l_col_name = it_column-charact+2.
      ENDIF.
      CASE sy-tabix.
        WHEN 3.
          p_key_01 = l_col_name.
        WHEN 4.
          p_key_02 = l_col_name.
        WHEN 5.
          p_key_03 = l_col_name.
        WHEN 6.
          p_key_04 = l_col_name.
        WHEN 7.
          p_key_05 = l_col_name.
        WHEN 8.
          p_key_06 = l_col_name.
        WHEN 9.
          p_key_07 = l_col_name.
        WHEN 10.
          p_key_08 = l_col_name.
        WHEN 11.
          p_key_09 = l_col_name.
        WHEN 12.
          p_key_10 = l_col_name.
        WHEN 13.
          p_key_11 = l_col_name.
        WHEN 14.
          p_key_12 = l_col_name.
        WHEN 15.
          p_key_13 = l_col_name.
        WHEN 16.
          p_key_14 = l_col_name.
        WHEN 17.
          p_key_15 = l_col_name.
        WHEN 18.
          p_key_16 = l_col_name.
        WHEN 19.
          p_key_17 = l_col_name.
        WHEN 20.
          p_key_18 = l_col_name.
        WHEN 21.
          p_key_19 = l_col_name.
        WHEN 22.
          p_key_20 = l_col_name.

      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " check_essential_condition
*&---------------------------------------------------------------------*
*&      Form  SEARCH_DATA
*&---------------------------------------------------------------------*
*       Searching Data & Setting Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_data_1205.

  CLEAR it_vtentries.
  REFRESH it_vtentries.

  CALL FUNCTION 'CARD_TABLE_READ_ENTRIES'
    EXPORTING
      var_table             = p_full_code
*     CHANGE_NO             =
*     DATE                  =
    TABLES
      var_tab_entries       = it_vtentries
    EXCEPTIONS
      error                 = 1
      OTHERS                = 2  .

*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

* DEFINE COLUMN.
  DATA: l_column TYPE vtentries-vtlineno.
  DATA: l_row TYPE vtentries-vtlineno.
  LOOP AT it_vtentries.
    IF sy-tabix = 1.  "First Row
      l_row = 1.
    ENDIF.

    IF l_row = it_vtentries-vtlineno.   "Past Row = Recent Row
      l_column = l_column + 1.  "Increase column number
*
      READ TABLE it_column WITH KEY charact =
                                    it_vtentries-vtcharact.
      IF sy-tabix <> l_column.  "If there is not a value of the column
        l_column = l_column + 1.
      ENDIF.
    ELSE.                               "Past Row <> Recent Row
      l_row = it_vtentries-vtlineno.  "Set Row
      l_column = 1.  "Reset Column
    ENDIF.

    CONDENSE l_column.
    MOVE l_column TO it_vtentries-column.
    MODIFY it_vtentries.

  ENDLOOP.

* SET THE INTERNAL TABLE
  CLEAR l_row.
  CLEAR: IT_APP223.
  REFRESH: IT_APP223.
  LOOP AT it_vtentries.
    AT NEW vtlineno.
      CLEAR IT_APP223.
      MOVE it_vtentries-vtlineno TO IT_APP223-line.
      MOVE it_vtentries-vtvalue TO IT_APP223-code.
      APPEND IT_APP223.
      l_row = l_row + 1.
    ENDAT.

    CASE it_vtentries-column.
      WHEN '1'.  "CODE
        MOVE it_vtentries-vtvalue TO IT_APP223-code.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '2'.  "DATE
*
        MOVE it_vtentries-vtvalue TO IT_APP223-date.
*
        MODIFY IT_APP223 INDEX l_row.
      WHEN '3'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_01.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '4'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_02.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '5'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_03.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '6'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_04.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '7'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_05.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '8'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_06.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '9'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_07.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '10'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_08.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '11'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_09.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '12'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_10.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '13'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_11.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '14'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_12.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '15'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_13.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '16'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_14.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '17'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_15.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '18'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_16.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '19'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_17.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '20'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_18.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '21'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_19.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '22'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_20.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '23'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_21.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '24'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_22.
        MODIFY IT_APP223 INDEX l_row.
      WHEN '25'.
        MOVE it_vtentries-vtvalue TO IT_APP223-col_23.
        MODIFY IT_APP223 INDEX l_row.
    ENDCASE.

  ENDLOOP.

  LOOP AT IT_APP223.
    CONCATENATE IT_APP223-col_01 IT_APP223-col_02
                IT_APP223-col_03 IT_APP223-col_04
                IT_APP223-col_05 IT_APP223-col_06
                IT_APP223-col_07 IT_APP223-col_08
                IT_APP223-col_09 IT_APP223-col_10
                IT_APP223-col_11 IT_APP223-col_12
                IT_APP223-col_13 IT_APP223-col_14
                IT_APP223-col_15 IT_APP223-col_16
                IT_APP223-col_17 IT_APP223-col_18
                IT_APP223-col_19 IT_APP223-col_20
      INTO IT_APP223-con_col .
    MODIFY IT_APP223.
  ENDLOOP.

ENDFORM.                    " SEARCH_DATA
*&---------------------------------------------------------------------*
*&      Form  set_excel
*&---------------------------------------------------------------------*
*       Setting Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_excel_1205.
*
  CLEAR IT_EXCEL_1205.
  REFRESH IT_EXCEL_1205.
  MOVE p_full_code TO IT_EXCEL_1205-code.
  APPEND IT_EXCEL_1205.

  CLEAR IT_EXCEL_1205.
  MOVE 'CODE' TO IT_EXCEL_1205-code.
  MOVE 'Full Key' TO IT_EXCEL_1205-con_col.
  MOVE 'DATE' TO IT_EXCEL_1205-date.
  READ TABLE it_column INDEX 3.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_01.
  ENDIF.
  READ TABLE it_column INDEX 4.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_02.
  ENDIF.
  READ TABLE it_column INDEX 5.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_03.
  ENDIF.
  READ TABLE it_column INDEX 6.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_04.
  ENDIF.
  READ TABLE it_column INDEX 7.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_05.
  ENDIF.
  READ TABLE it_column INDEX 8.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_06.
  ENDIF.
  READ TABLE it_column INDEX 9.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_07.
  ENDIF.
  READ TABLE it_column INDEX 10.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_08.
  ENDIF.
  READ TABLE it_column INDEX 11.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_09.
  ENDIF.
  READ TABLE it_column INDEX 12.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_10.
  ENDIF.
  READ TABLE it_column INDEX 13.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_11.
  ENDIF.
  READ TABLE it_column INDEX 14.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_12.
  ENDIF.
  READ TABLE it_column INDEX 15.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_13.
  ENDIF.
  READ TABLE it_column INDEX 16.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_14.
  ENDIF.
  READ TABLE it_column INDEX 17.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_15.
  ENDIF.
  READ TABLE it_column INDEX 18.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_16.
  ENDIF.
  READ TABLE it_column INDEX 19.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_17.
  ENDIF.
  READ TABLE it_column INDEX 20.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_18.
  ENDIF.
  READ TABLE it_column INDEX 21.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_19.
  ENDIF.
  READ TABLE it_column INDEX 22.
  IF sy-subrc = 0.
    MOVE it_column-charact TO IT_EXCEL_1205-col_20.
  ENDIF.
  APPEND IT_EXCEL_1205.

  LOOP AT IT_APP223.
    CLEAR IT_EXCEL_1205.
    MOVE-CORRESPONDING IT_APP223 TO IT_EXCEL_1205.
    APPEND IT_EXCEL_1205.
  ENDLOOP.

ENDFORM.                    " set_excel
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       Calling a Func. For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_func_download_1205.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'ALC Code.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       TABLES
            data_tab                = IT_EXCEL_1205
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " call_func_download
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ALC
*&---------------------------------------------------------------------*
*       Updating ALC Code
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_alc_1205.
  DATA: l_vtint.
  DATA: l_slnid TYPE cuvtln-slnid.  " INDEX

  CLEAR: it_table_header.
  REFRESH: it_table_header.
* Read Table of Variant table basic data
  PERFORM read_cuvtab.  " Variant table basic data
*
  READ TABLE it_table_header INDEX 1.
  LOOP AT IT_APP223 WHERE mark = 'X'.
    CLEAR: it_lines_old,
           it_lines_new,
           it_values_c_old,
           it_values_c_new,
           it_values_n_old,
           it_values_n_new .
    REFRESH: it_lines_old,
             it_lines_new,
             it_values_c_old,
             it_values_c_new,
             it_values_n_old,
             it_values_n_new .

    CLEAR l_slnid.
*   READ INDEX BY table_header-vtint and IT_APP223-line.
    PERFORM read_cuvtln USING it_table_header-vtint
                              IT_APP223-line
                              l_slnid.
*   Read old Data
    PERFORM read_old_data_1205 USING it_table_header-vtint
                                     l_slnid .
*   Create New Data
    PERFORM make_new_data TABLES IT_APP223
                          USING  it_table_header-vtint
                                 l_slnid .

*   Call a Func. For Updating Data
    CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
      EXPORTING
        table_header       = it_table_header
*   ECM_NUMBER         =
      TABLES
        lines_old          = it_lines_old
        lines_new          = it_lines_new
        values_c_old       = it_values_c_old
        values_c_new       = it_values_c_new
        values_n_old       = it_values_n_old
        values_n_new       = it_values_n_new .

  ENDLOOP.

ENDFORM.                    " UPDATE_ALC
*&---------------------------------------------------------------------*
*&      Form  MAKE_TABLE_HEADER
*&---------------------------------------------------------------------*
*       Reading Table - CUVTAB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cuvtab.
  DATA: wa_table_header TYPE cuvtab.
  CLEAR: wa_table_header, it_table_header.
  REFRESH it_table_header.

  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_table_header
    FROM cuvtab
    WHERE vtnam = p_full_code AND
          vtsta = '1'.
  APPEND wa_table_header TO it_table_header.

ENDFORM.                    " MAKE_TABLE_HEADER
*&---------------------------------------------------------------------*
*&      Form  READ_CUVTLN
*&---------------------------------------------------------------------*
*       Reading Table - CUVTLN
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cuvtln USING p_vtint
                       p_line
                       p_slnid.
  CLEAR p_slnid.
  SELECT SINGLE *
    INTO CORRESPONDING FIELDS OF wa_lines_1205
    FROM cuvtln
    WHERE vtint = p_vtint AND
          vtlin = p_line .

  APPEND wa_lines_1205 TO it_lines_new.
  APPEND wa_lines_1205 TO it_lines_old.
  MOVE wa_lines_1205-slnid TO p_slnid.

ENDFORM.                    " READ_CUVTLN
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending_1205.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(07) = 'IT_APP223'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT IT_APP223 ASCENDING BY (field_name01).
  ENDIF.

ENDFORM.                    " sort_ascending
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending_1205.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(07) = 'IT_APP223'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT IT_APP223 DESCENDING BY (field_name01).
  ENDIF.
*

ENDFORM.                    " sort_descending
*&---------------------------------------------------------------------*
*&      Form  MAKE_NEW_DATA
*&---------------------------------------------------------------------*
*       Setting Data Per Code Part Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_new_data TABLES p_IT_APP223 STRUCTURE IT_APP223
                   USING  p_vtint
                          p_slnid .
  DATA: l_date_d LIKE sy-datum,
        l_date_c(08).
  DATA original_date TYPE d.

  DATA l_tabix LIKE sy-tabix.
  LOOP AT it_column.
    CLEAR: it_values_n_new, it_values_c_new.
    l_tabix = sy-tabix.

    SEARCH it_column-charact FOR 'DATE'.
    IF sy-subrc = 0.   " If it is date type ...
      MOVE p_vtint TO it_values_n_new-vtint.
      MOVE p_slnid TO it_values_n_new-slnid.
      PERFORM call_function_conversion USING it_column-charact
                                             it_values_n_new-atinn.
      MOVE 1 TO it_values_n_new-vlcnt.
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
*     DATE CONTROL : IT_VALUES_N_NEW-VAL_FROM
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CLEAR: l_date_c.
      CALL 'DATE_CONV_EXT_TO_INT'
        ID 'DATEXT' FIELD p_IT_APP223-date
        ID 'DATINT' FIELD l_date_d.

*      PERFORM change_to_sys_date_type USING p_IT_APP223-date
*                                            l_date_c .
      move l_date_d to l_date_c.
      MOVE l_date_c TO it_values_n_new-val_from.

      MOVE '1' TO it_values_n_new-val_code.
      APPEND it_values_n_new .

    ELSE.
      CASE l_tabix.
        WHEN 1 .  "CODE
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-code TO it_values_c_new-valc.
          APPEND it_values_c_new.
*        WHEN 2 .  "DATE USUALLY
        WHEN 3 .                                            " 1ST KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_01 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 4 .                                            " 2ND KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_02 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 5 .                                            " 3RD KEY
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_03 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 6 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_04 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 7 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_05 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 8 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_06 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 9 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_07 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 10 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_08 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 11 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_09 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 12 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_10 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 13 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_11 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 14 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_12 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 15 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_13 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 16 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_14 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 17 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_15 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 18 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_16 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 19 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_17 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 20 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_18 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 21 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_19 TO it_values_c_new-valc.
          APPEND it_values_c_new.

        WHEN 22 .
          MOVE p_vtint TO it_values_c_new-vtint .
          MOVE p_slnid TO it_values_c_new-slnid .
          PERFORM call_function_conversion
                                       USING it_column-charact
                                             it_values_c_new-atinn.
          MOVE 1 TO it_values_c_new-vlcnt .
          MOVE p_IT_APP223-col_20 TO it_values_c_new-valc.
          APPEND it_values_c_new.

      ENDCASE.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " MAKE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  read_old_data
*&---------------------------------------------------------------------*
*       Reading Previous Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_old_data_1205 USING p_vtint    "
                              p_slnid .  "INDEX
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_values_n_old
    FROM cuvtab_valn
    WHERE vtint = p_vtint AND
          slnid = p_slnid .
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_values_c_old
    FROM cuvtab_valc
    WHERE vtint = p_vtint AND
          slnid = p_slnid .

ENDFORM.                    " read_old_data
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       Reading a Internal Characteristic
*----------------------------------------------------------------------*
*      -->P_IT_VTENTRIES_VTCHARACT  text
*      -->P_L_ATINN_N  text
*----------------------------------------------------------------------*
FORM call_function_conversion USING    p_char_c
                                       p_numb_n.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = p_char_c
       IMPORTING
            output = p_numb_n.

ENDFORM.                    " call_function_conversion
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       Deletion of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_1205.
  DATA: l_answer,
        l_count_i TYPE i,
        l_text(40),
        l_count_c(03).

  CLEAR l_count_i .
  LOOP AT IT_APP223 WHERE mark = 'X'.
    l_count_i = l_count_i + 1.
  ENDLOOP.

  WRITE l_count_i TO l_count_c .
  CONCATENATE 'Do you want to delete ' l_count_c 'EA data ? '
              INTO l_text.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            defaultoption  = 'Y'
            textline1      = l_text
            titel          = 'Delete'
            start_column   = 25
            start_row      = 6
            cancel_display = space
       IMPORTING
            answer         = l_answer.

  IF l_answer = 'J'.
    PERFORM delete_process_1205.
  ENDIF.

ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  DELETE_PROCESS
*&---------------------------------------------------------------------*
*       Deletion of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_process_1205.
  DATA: l_vtint.
  DATA: l_slnid TYPE cuvtln-slnid.  " INDEX

  CLEAR: it_table_header.
  REFRESH: it_table_header.
*
  PERFORM read_cuvtab.  " Variant table basic data
*
  READ TABLE it_table_header INDEX 1.
  LOOP AT IT_APP223 WHERE mark = 'X'.
*
    CLEAR l_slnid.

    PERFORM read_old_lines USING it_table_header-vtint.
    PERFORM set_new_lines USING it_table_header-vtint
                                IT_APP223-line   "LINE
                                l_slnid     .  "INDEX
    PERFORM read_old_data_1205 USING it_table_header-vtint
                                     l_slnid .

    CLEAR: it_values_c_new, it_values_n_new .
    REFRESH: it_values_c_new, it_values_n_new .

    CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
      EXPORTING
        table_header       = it_table_header
*   ECM_NUMBER         =
      TABLES
        lines_old          = it_lines_old
        lines_new          = it_lines_new
        values_c_old       = it_values_c_old
        values_c_new       = it_values_c_new
        values_n_old       = it_values_n_old
        values_n_new       = it_values_n_new .

  ENDLOOP.


ENDFORM.                    " DELETE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_OLD_LINES
*&---------------------------------------------------------------------*
*       Reading The Previous Lines
*----------------------------------------------------------------------*
*      -->P_IT_TABLE_HEADER_VTINT  text
*----------------------------------------------------------------------*
FORM read_old_lines USING    p_vtint.
  CLEAR: it_lines_old .
  REFRESH: it_lines_old .
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_lines_old
    FROM cuvtln
    WHERE vtint = p_vtint .

ENDFORM.                    " READ_OLD_LINES
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_LINES
*&---------------------------------------------------------------------*
*       Setting New Lines
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_new_lines USING  p_vtint
                          p_line
                          p_slnid     .

  DATA: l_vtlin TYPE cuvtln-vtlin.

  CLEAR: it_lines_new,  l_vtlin .
  REFRESH: it_lines_new .

  CLEAR p_slnid.
  SELECT SINGLE slnid
    INTO p_slnid
    FROM cuvtln
    WHERE vtint = p_vtint AND
          vtlin = p_line .

  SORT it_lines_old BY vtlin .
  LOOP AT it_lines_old.
    CLEAR it_lines_new.
    IF it_lines_old-slnid = p_slnid.
      CONTINUE.
    ELSE.
      l_vtlin = l_vtlin + 1.
      MOVE-CORRESPONDING it_lines_old TO it_lines_new.
      MOVE l_vtlin TO it_lines_new-vtlin .
      APPEND it_lines_new.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " SET_NEW_LINES
*&---------------------------------------------------------------------*
*&      Form  setting_p_full_code
*&---------------------------------------------------------------------*
*       Setting Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_parameters_1206.
* Full Code(ALC) = p_model+'_ALC_'+p_part+'_'+p_key.
  CONDENSE p_key.
  CONCATENATE p_model '_ALC_' p_part '_' p_key
    INTO p_full_code.

  CLEAR: wa_descriptions, it_column.
  REFRESH: wa_descriptions, it_column.

  IF ( p_model IS INITIAL ) OR
     ( p_part IS INITIAL ) OR
     ( p_key IS INITIAL ) .
    EXIT.
  ENDIF.

  CALL FUNCTION 'CARD_TABLE_READ_STRUCTURE'
    EXPORTING
      var_tab                    = p_full_code
*   CHANGE_NO                  =
*   DATE                       =
      language                   = 'E'
* IMPORTING
*   BASIC_DATA                 =
*   RETURN                     =
    TABLES
      descriptions               = wa_descriptions
      characteristics            = it_column
*   VALUE_ASSIGNMENT_ALT       =
    EXCEPTIONS
      error                      = 1
      OTHERS                     = 2 .
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.

  MOVE wa_descriptions-descript TO p_col_name.

  DATA: l_offset TYPE i.
  DATA: l_col_name(20).

  CLEAR: p_key_01, p_key_02, p_key_03, p_key_04, p_key_05,
         p_key_06, p_key_07, p_key_08, p_key_09, p_key_10,
         p_key_11, p_key_12, p_key_13, p_key_14, p_key_15,
         p_key_16, p_key_17, p_key_18, p_key_19, p_key_20.

  LOOP AT it_column.
    SEARCH it_column-charact FOR 'ALC'.
    IF sy-subrc = 0.
      CONTINUE.
    ELSE.
      CLEAR: l_offset, l_col_name.
      SEARCH it_column-charact FOR '219'.
      IF sy-subrc = 0.
        l_offset = sy-fdpos + 4.
        l_col_name = it_column-charact+l_offset.
      ELSE.
        l_col_name = it_column-charact+2.
      ENDIF.
      CASE sy-tabix.
        WHEN 3.
          p_key_01 = l_col_name.
        WHEN 4.
          p_key_02 = l_col_name.
        WHEN 5.
          p_key_03 = l_col_name.
        WHEN 6.
          p_key_04 = l_col_name.
        WHEN 7.
          p_key_05 = l_col_name.
        WHEN 8.
          p_key_06 = l_col_name.
        WHEN 9.
          p_key_07 = l_col_name.
        WHEN 10.
          p_key_08 = l_col_name.
        WHEN 11.
          p_key_09 = l_col_name.
        WHEN 12.
          p_key_10 = l_col_name.
        WHEN 13.
          p_key_11 = l_col_name.
        WHEN 14.
          p_key_12 = l_col_name.
        WHEN 15.
          p_key_13 = l_col_name.
        WHEN 16.
          p_key_14 = l_col_name.
        WHEN 17.
          p_key_15 = l_col_name.
        WHEN 18.
          p_key_16 = l_col_name.
        WHEN 19.
          p_key_17 = l_col_name.
        WHEN 20.
          p_key_18 = l_col_name.
        WHEN 21.
          p_key_19 = l_col_name.
        WHEN 22.
          p_key_20 = l_col_name.

      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " setting_p_full_code
*&---------------------------------------------------------------------*
*&      Form  CLEAR_IT_APP223_NEW
*&---------------------------------------------------------------------*
*       Initialization of Internal Table - IT_APP223_NEW
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_IT_APP223_NEW.
  CLEAR IT_APP223_NEW.
  REFRESH IT_APP223_NEW.

ENDFORM.                    " CLEAR_IT_APP223_NEW
*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*       Creation of New Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_new_data_1206.
  DATA: l_vtint.
  DATA: l_slnid TYPE cuvtln-slnid.  " INDEX

  CLEAR: it_table_header.
  REFRESH: it_table_header.
*
  PERFORM read_cuvtab.  " Variant table basic data
*
  READ TABLE it_table_header INDEX 1.
  LOOP AT IT_APP223_NEW.
    CLEAR: it_lines_old,
           it_lines_new,
           it_values_c_old,
           it_values_c_new,
           it_values_n_old,
           it_values_n_new .
    REFRESH: it_lines_old,
             it_lines_new,
             it_values_c_old,
             it_values_c_new,
             it_values_n_old,
             it_values_n_new .

    PERFORM make_new_line USING it_table_header-vtint
                                IT_APP223_NEW-line
                                l_slnid .

    PERFORM make_new_data TABLES IT_APP223_NEW
                          USING  it_table_header-vtint
                                 l_slnid .

    CALL FUNCTION 'CUVT_UPDATE_TABLE_CONTENT'
      EXPORTING
        table_header       = it_table_header
*   ECM_NUMBER         =
      TABLES
        lines_old          = it_lines_old
        lines_new          = it_lines_new
        values_c_old       = it_values_c_old
        values_c_new       = it_values_c_new
        values_n_old       = it_values_n_old
        values_n_new       = it_values_n_new .

  ENDLOOP.


ENDFORM.                    " CREATE_NEW_DATA
*&---------------------------------------------------------------------*
*&      Form  make_new_line
*&---------------------------------------------------------------------*
*       Creation of New Line
*----------------------------------------------------------------------*
*      -->P_IT_TABLE_HEADER_VTINT  text
*      -->P_L_SLNID  text
*----------------------------------------------------------------------*
FORM make_new_line USING    p_vtint
                            p_line
                            p_slnid.
  CLEAR: p_slnid, p_line .

  SELECT MAX( slnid ) MAX( vtlin )
    INTO (p_slnid , p_line)
    FROM cuvtln
    WHERE vtint = p_vtint.

  p_slnid = p_slnid + 1 .
  p_line = p_line + 1 .

  MOVE p_vtint TO it_lines_new-vtint .
  MOVE p_slnid TO it_lines_new-slnid .
  MOVE p_line  TO it_lines_new-vtlin .
  APPEND it_lines_new.

ENDFORM.                    " make_new_line
*&---------------------------------------------------------------------*
*&      Form  change_to_sys_date_type
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*      -->P_P_IT_APP223_DATE  text
*      -->P_L_DATE_C  text
*----------------------------------------------------------------------*
FORM change_to_sys_date_type USING    p_char10
                                      p_date_c.
  DATA: l_date_01(04),
        l_date_02(04),
        l_date_03(04),
        l_date_c(08),
        l_temp(08),
        l_offset TYPE i,
        l_pos TYPE i.

  CLEAR: l_date_01,
         l_date_02,
         l_date_03,
         l_date_c,
         l_temp,
         l_offset,
         l_pos.

  if p_char10 is initial .
    exit.
  endif.

  SEARCH p_char10 FOR '/'.
  MOVE sy-fdpos TO l_offset.
  l_pos = l_offset .
  MOVE p_char10(l_pos) TO l_date_01.
  l_pos = l_offset + 1.
  MOVE p_char10+l_pos TO l_temp.

  SEARCH l_temp FOR '/'.
  MOVE sy-fdpos TO l_offset.
  l_pos = l_offset .
  MOVE l_temp(l_pos) TO l_date_02.
  l_pos = l_offset + 1.
  MOVE l_temp+l_pos TO l_temp.
  MOVE l_temp TO l_date_03.

  CONCATENATE l_date_01 l_date_02 l_date_03
    INTO p_date_c.


ENDFORM.                    " change_to_sys_date_type
