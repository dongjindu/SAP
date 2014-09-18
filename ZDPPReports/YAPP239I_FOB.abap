*----------------------------------------------------------------------*
*   INCLUDE YAPP239L_FOB                                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_parameter
*&---------------------------------------------------------------------*
*       Setting Parameters to Search Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_parameter.
  CLEAR r_objek.
  REFRESH r_objek.
*
  LOOP AT it_objek.
    r_objek-sign = 'I'.
    r_objek-option = 'EQ'.
    r_objek-low = it_objek-objek.
    APPEND r_objek.
  ENDLOOP.

  DATA: l_char_c(20).
*
  CLEAR r_atinn.
  REFRESH r_atinn.
*
  r_atinn-sign = 'I'.
  r_atinn-option = 'EQ'.
  l_char_c = 'P_MI'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_OCN'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_EXT_COLOR'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_INT_COLOR'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_MODEL'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_BODY_SERIAL'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_MITU'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_MITU_DATE'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_SEQUENCE_SERIAL'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_SEQUENCE_DATE'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .
  APPEND r_atinn.
  l_char_c = 'P_WORK_ORDER'.
  PERFORM call_function_conversion USING l_char_c
                                         r_atinn-low .

  APPEND r_atinn.

ENDFORM.                    " set_parameter
*&---------------------------------------------------------------------*
*&      Form  make_data
*&---------------------------------------------------------------------*
*       Searching Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_data.
*
  DATA: l_lines TYPE i,
        l_num08(08) TYPE n.
*
  CLEAR it_app239.
  REFRESH it_app239.
*
  PERFORM select_objek.
  DESCRIBE TABLE it_objek LINES l_lines.
  IF l_lines < 1.
    MESSAGE s000 WITH 'THERE IS NO DATA'.
    EXIT.
  ENDIF.
*
  PERFORM set_parameter.
*
  CLEAR it_characteristic.
  REFRESH it_characteristic.
*
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_characteristic
    FROM ausp
    WHERE objek IN r_objek AND
          atinn IN r_atinn AND
          klart = '002' .

  DATA: l_tabix LIKE sy-tabix.
*
  LOOP AT it_characteristic.
*
    AT NEW objek.
      MOVE it_characteristic-objek TO it_app239-objek.
      APPEND it_app239.
    ENDAT.
*
    SELECT SINGLE atnam INTO it_characteristic-atnam
      FROM cabn
      WHERE atinn = it_characteristic-atinn.
    MODIFY it_characteristic.
*
    READ TABLE it_app239 WITH KEY objek = it_characteristic-objek.
    l_tabix = sy-tabix.
*
    CASE it_characteristic-atnam.
      WHEN 'P_MI'.
        MOVE it_characteristic-atwrt TO it_app239-mi.
      WHEN 'P_OCN'.
        MOVE it_characteristic-atwrt TO it_app239-ocn.
      WHEN 'P_EXT_COLOR'.
        MOVE it_characteristic-atwrt TO it_app239-ext_color.
      WHEN 'P_INT_COLOR'.
        MOVE it_characteristic-atwrt TO it_app239-int_color.
      WHEN 'P_MODEL'.
        MOVE it_characteristic-atwrt TO it_app239-model.
      WHEN 'P_BODY_SERIAL'.
        MOVE it_characteristic-atwrt TO it_app239-body_serial.
*      WHEN 'P_MITU'.
*        MOVE IT_CHARACTERISTIC-ATWRT TO IT_APP239-MITO.
      WHEN 'P_MITU_DATE'.
        MOVE: it_characteristic-atflv TO l_num08,
              l_num08                 TO it_app239-mitu_date.
      WHEN 'P_SEQUENCE_SERIAL'.
        MOVE it_characteristic-atwrt TO it_app239-sequence_serial.
      WHEN 'P_SEQUENCE_DATE'.
        MOVE: it_characteristic-atflv TO l_num08,
              l_num08                 TO it_app239-sequence_date.
      WHEN 'P_WORK_ORDER'.
        MOVE it_characteristic-atwrt TO it_app239-workorder.
    ENDCASE.
*
    MODIFY it_app239 INDEX l_tabix.
*
  ENDLOOP.
*

ENDFORM.                    " make_data
*&---------------------------------------------------------------------*
*&      Form  set_field_model
*&---------------------------------------------------------------------*
*       Searching Data To Set a Parameter - MODEL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_model.
  SELECT DISTINCT model name
    INTO (wa_model_value-key , wa_model_value-text)
    FROM ztpp_veh_model.
    APPEND wa_model_value TO it_model_list.

  ENDSELECT.

ENDFORM.                    " set_field_model
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm
*&---------------------------------------------------------------------*
*       Calling a Function For Making a Dropdown List Box
*----------------------------------------------------------------------*
*      -->P_IT_MODEL_LIST  text
*----------------------------------------------------------------------*
FORM call_function_vrm USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.

ENDFORM.                    " call_function_vrm
*&---------------------------------------------------------------------*
*&      Form  CHECK_OBJEK
*&---------------------------------------------------------------------*
*       Checking If there is a V/M No in The Table - AUSP.
*----------------------------------------------------------------------*
*      -->P_WA_OBJEK_L  text
*      -->P_WA_ATINN_L  text
*      -->P_WA_ATWRT_L  text
*----------------------------------------------------------------------*
FORM check_objek USING    p_objek
                          p_atinn
                          p_atwrt.
  SELECT SINGLE objek
    INTO p_objek
    FROM ausp
    WHERE objek = p_objek AND
          atinn = p_atinn AND
          atwrt = p_atwrt AND
          klart = '002'   .

ENDFORM.                    " CHECK_OBJEK
*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJEK
*&---------------------------------------------------------------------*
*       Separating V/M No. By Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_objek.
  DATA: l_objek_c TYPE ausp-objek,
        l_atwrt_c TYPE ausp-atwrt,
        l_atinn_n TYPE cabn-atinn.
  DATA: l_bodyno_c(10).
  DATA: lg_atinn_n TYPE ausp-atinn,
        lg_char_c(20).
  DATA: l_char_c(20).
  DATA: l_date_st_c(08),
        l_date_en_c(08),
        l_date_st_p TYPE ausp-atflv,
        l_date_en_p TYPE ausp-atflv.

  IF p_bodyser <> space.
    CONCATENATE p_model p_bodyser INTO p_bodyno.
    CONCATENATE p_bodyno '%' INTO l_bodyno_c.
  ELSE.
    CONCATENATE p_model '%' INTO l_bodyno_c.
  ENDIF.
*
  CLEAR it_objek.
  REFRESH it_objek.
*
* necessary parameter.
  lg_char_c = 'P_MITU'.
  CLEAR l_atinn_n.
  PERFORM call_function_conversion USING lg_char_c
                                         lg_atinn_n .
  SELECT DISTINCT objek  " CHECK MITO STATUS
    INTO l_objek_c
    FROM ausp
    WHERE objek LIKE l_bodyno_c AND
          atinn = lg_atinn_n AND
          atwrt = 'Y' AND
          klart = '002'.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
*   necessary parameter.
    l_char_c = 'P_MITU_DATE'.
    CLEAR: l_atinn_n, l_date_st_c, l_date_en_c,
           l_date_st_p, l_date_en_p.
    PERFORM call_function_conversion USING l_char_c
                                           l_atinn_n .
    MOVE: p_cdate_st  TO l_date_st_c,
          l_date_st_c TO l_date_st_p,
          p_cdate_en  TO l_date_en_c,
          l_date_en_c TO l_date_en_p.
    SELECT SINGLE objek  " CHECK MITU DATE.
      INTO l_objek_c
      FROM ausp
      WHERE objek = l_objek_c AND
            atinn = l_atinn_n AND
            klart = '002'     AND
            atflv BETWEEN l_date_st_p and l_date_en_p .
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
*   IF P_MODEL is NOT INITIAL ...
    IF p_model <> space.
      l_char_c = 'P_MODEL'.
      CLEAR l_atinn_n.
      PERFORM call_function_conversion USING l_char_c
                                             l_atinn_n .
      l_atwrt_c = p_model.
      PERFORM check_objek USING l_objek_c
                                l_atinn_n
                                l_atwrt_c.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.  " IF P_MODEL <> SPACE ...
*   IF P_BODYSER IS NOT INITIAL ...
    IF p_bodyser <> space.
      l_char_c = 'P_BODY_SERIAL'.
      CLEAR l_atinn_n.
      PERFORM call_function_conversion USING l_char_c
                                             l_atinn_n .
      l_atwrt_c = p_bodyser.
      PERFORM check_objek USING l_objek_c
                                l_atinn_n
                                l_atwrt_c.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   IF P_ORDERNO IS NOT INITIAL ...
    IF p_orderno <> space.
      l_char_c = 'P_WORK_ORDER'.
      CLEAR l_atinn_n.
      PERFORM call_function_conversion USING l_char_c
                                             l_atinn_n .
      l_atwrt_c = p_orderno.
      PERFORM check_objek USING l_objek_c
                                l_atinn_n
                                l_atwrt_c.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   IF P_EXT_COLOR IS NOT INITIAL ...
    IF p_ext_color <> space.
      l_char_c = 'P_EXT_COLOR'.
      CLEAR l_atinn_n.
      PERFORM call_function_conversion USING l_char_c
                                             l_atinn_n .
      l_atwrt_c = p_ext_color.
      PERFORM check_objek USING l_objek_c
                                l_atinn_n
                                l_atwrt_c.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   IF P_INT_COLOR IS NOT INITIAL ...
    IF p_int_color <> space.
      l_char_c = 'P_INT_COLOR'.
      CLEAR l_atinn_n.
      PERFORM call_function_conversion USING l_char_c
                                             l_atinn_n .
      l_atwrt_c = p_int_color.
      PERFORM check_objek USING l_objek_c
                                l_atinn_n
                                l_atwrt_c.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*
    APPEND l_objek_c TO it_objek.
*
  ENDSELECT.  " CHECK MITO STATUS

ENDFORM.                    " SELECT_OBJEK
*&---------------------------------------------------------------------*
*&      Form  call_function_conversion
*&---------------------------------------------------------------------*
*       Char's Name Conversion To Char's Value
*----------------------------------------------------------------------*
*      -->P_L_CHAR_C  text
*      -->P_L_MITU_N  text
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
*&      Form  download
*&---------------------------------------------------------------------*
*       Setting Internal Table's Header For download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download.
  CLEAR it_excel_239.
  REFRESH it_excel_239.
  MOVE 'Body Number' TO it_excel_239-objek.
  MOVE 'MITU Date'   TO it_excel_239-mitu_date.
  MOVE 'Body Serial' TO it_excel_239-body_serial.
  MOVE 'Model'       TO it_excel_239-model.
  MOVE 'Spec'        TO it_excel_239-mi.
  MOVE 'OCN'         TO it_excel_239-ocn.
  MOVE 'Work Order'  TO it_excel_239-workorder.
  MOVE 'External Color' TO it_excel_239-ext_color.
  MOVE 'Internal Color' TO it_excel_239-int_color.
  MOVE 'Sequence Serial' TO it_excel_239-sequence_serial.
  MOVE 'Sequence Date' TO it_excel_239-sequence_date.
  APPEND it_excel_239.

  LOOP AT it_app239.
    CLEAR it_excel_239.
    MOVE-CORRESPONDING it_app239 TO it_excel_239.
    APPEND it_excel_239.
  ENDLOOP.

  PERFORM call_function_download.

ENDFORM.                    " download
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION_DOWNLOAD
*&---------------------------------------------------------------------*
*       Calling A Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function_download.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'MITU STATUS.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       TABLES
            data_tab                = it_excel_239
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

ENDFORM.                    " CALL_FUNCTION_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP239'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app239 ASCENDING BY (field_name01).
  ENDIF.
*

ENDFORM.                    " sort_ascending
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP239'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app239 DESCENDING BY (field_name01).
  ENDIF.
*

ENDFORM.                    " sort_descending
