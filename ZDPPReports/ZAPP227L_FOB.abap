*----------------------------------------------------------------------*
*   INCLUDE YTEST_KGH02_F01                                            *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_parameter
*&---------------------------------------------------------------------*
*       Setting Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_parameter_1209.
* Company
  CLEAR p_company.
  p_company = 'HMMA'.

* Plant
  CLEAR : plant_list, plant_value.
  name = 'P_PLANT'.
  PERFORM set_field_PLANT_1209.
  PERFORM call_function_VRM USING plant_list.

* Model
  CLEAR : model_list, model_value.
  name = 'P_MODEL'.
  PERFORM set_field_MODEL_1209.
  PERFORM call_function_VRM USING model_list.

* Operation Date
  p_opdate = sy-datum.

* Operation Count
  p_opcount = 1.

* ORDER NO

ENDFORM.                    " set_parameter
*&---------------------------------------------------------------------*
*&      Form  set_field
*&---------------------------------------------------------------------*
*       Setting a Field For Dropdown List Box - PLANT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_PLANT_1209.
  CLEAR plant_value.
  MOVE '1' TO plant_value-key.
  MOVE '1st Plant' TO plant_value-text.
  APPEND plant_value TO plant_list.

  CLEAR plant_value.
  MOVE '2' TO plant_value-key.
  MOVE '2nd Plant' TO plant_value-text.
  APPEND plant_value TO plant_list.

  CLEAR plant_value.
  MOVE '3' TO plant_value-key.
  MOVE '3rd Plant' TO plant_value-text.
  APPEND plant_value TO plant_list.

  CLEAR plant_value.
  MOVE '4' TO plant_value-key.
  MOVE '4th Plant' TO plant_value-text.
  APPEND plant_value TO plant_list.

ENDFORM.                    " set_field

*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       Calling a Func. For Making Dropdown list box
*----------------------------------------------------------------------*
*      -->P_PLANT_LIST  text
*----------------------------------------------------------------------*
FORM call_function_VRM USING    p_list.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = p_list.

ENDFORM.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD02
*&---------------------------------------------------------------------*
*       Setting a Field For Dropdown List Box - MODEL
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_MODEL_1209.
  CLEAR: model_list, model_list[].
  SELECT DISTINCT model02 name
    INTO (model_value-key , model_value-text)
    FROM ztpp_veh_model .
    APPEND model_value TO model_list .
  ENDSELECT.

*  clear model_value.
*  move 'EM' to model_value-key.
*  move 'EF-SONATA' to model_value-text.
*  append model_value to model_list.
*
*  clear model_value.
*  move 'CM' to model_value-key.
*  move 'SANTAFE' to model_value-text.
*  append model_value to model_list.

ENDFORM.                    " SET_FIELD02
*&---------------------------------------------------------------------*
*&      Form  SETUP_PARAMETER
*&---------------------------------------------------------------------*
*       Setting Parameters when they are not empty.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setup_parameter_1209.
  IF p_plant <> space.
    CLEAR   r_plant.
    REFRESH r_plant.
    r_plant-option = 'EQ'.
    r_plant-sign   = 'I'.
    r_plant-low = p_plant.
    APPEND r_plant.
  ELSE.
    CLEAR  r_plant.
    REFRESH r_plant.
  ENDIF.

  IF p_model <> space.
    CLEAR r_model.
    REFRESH r_model.
    r_model-option = 'EQ'.
    r_model-sign = 'I'.
    r_model-low = p_model.
    APPEND r_model.
  ELSE.
    CLEAR r_model.
    REFRESH r_model.
  ENDIF.

ENDFORM.                    " SETUP_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Searching Data with parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_1209.
  DATA: l_extc(04),
        l_intc(04).
  CONCATENATE p_extc
              '%'
    INTO l_extc.
  CONCATENATE p_intc
              '%'
    INTO l_intc.

  SELECT *
    FROM ztpp_spec
    INTO CORRESPONDING FIELDS OF TABLE it_app227
    WHERE plant IN r_plant AND
          model IN r_model AND
          opdate = p_opdate AND
          opcount = p_opcount AND
          worder >= p_worder AND
          extc LIKE l_extc AND
          intc LIKE l_intc    .
  IF sy-subrc <> 0.
    MESSAGE s000 WITH 'There is no data. '.
  ENDIF.

  CLEAR p_tot_count.
  LOOP AT it_app227.
    p_tot_count = p_tot_count + it_app227-opcount.
  ENDLOOP.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_WORK_ORDER
*&---------------------------------------------------------------------*
*       Checking Work Order No. By MARA
*----------------------------------------------------------------------*
*      -->P_IT_NEW_APP227_WORDER  text
*----------------------------------------------------------------------*
FORM check_work_order_1210 USING p_worder.
  DATA p_matnr TYPE mara-matnr.

  SELECT SINGLE *
    FROM mara
    WHERE matnr = p_worder AND
          mtart = 'WOCL'     .

  IF sy-subrc <> 0.
    MOVE p_worder TO IT_ERROR_1210-forder.
    MOVE p_worder(14) TO IT_ERROR_1210-worder.
    MOVE p_worder+14(02) TO IT_ERROR_1210-extc.
    MOVE p_worder+16(02) TO IT_ERROR_1210-intc.
    MOVE p_worder TO IT_ERROR_1210-matnr.
    APPEND IT_ERROR_1210.
*    message e002 with 'THE WORK ORDER,'
*                      p_matnr
*                      ', IS NOT RESISTERED !'.
  ENDIF.

ENDFORM.                    " CHECK_WORK_ORDER
*&---------------------------------------------------------------------*
*&      Form  save_new_data
*&---------------------------------------------------------------------*
*       Creation of New Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_new_data_1210.
  DATA l_count TYPE i.
  DATA: l_tabix TYPE sy-tabix.
  LOOP AT it_new_app227.
    l_tabix = sy-tabix.
*   checking work_order_number.
    READ TABLE IT_ERROR_1210 WITH KEY forder =
                                   it_new_app227-forder.
    IF sy-subrc = 0 .
      CONTINUE.
    ENDIF.
*
    IF it_new_app227-opdate <> space  AND
       it_new_app227-opcount <> space AND
       it_new_app227-plant <> space   AND
       it_new_app227-model <> space   AND
       it_new_app227-forder <> space     .
      MOVE it_new_app227-forder(14) TO
        it_new_app227-worder.
      MOVE it_new_app227-forder+14(02) TO
        it_new_app227-extc.
      MOVE it_new_app227-forder+16(02) TO
        it_new_app227-intc.
      it_new_app227-mark = 'I'.
      MODIFY it_new_app227 INDEX l_tabix.

      MOVE-CORRESPONDING it_new_app227 TO ztpp_spec.
      MOVE p_keycode TO ztpp_spec-keycode.
      MOVE p_erdat TO ztpp_spec-erdat.
      MOVE p_erzet TO ztpp_spec-erzet.
      MOVE p_ernam TO ztpp_spec-ernam.
      MODIFY ztpp_spec.
*
      it_new_app227-check = 'X'.
      MODIFY it_new_app227.
      l_count = l_count + 1.
*
    ELSE.
*
      CONTINUE.
*
    ENDIF.
  ENDLOOP.

  MESSAGE i001 WITH 'The number of data created is '
                    l_count .

ENDFORM.                    " save_new_data
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       Deletion of Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_1209.
  DATA: l_wa_ztpp_spec TYPE ztpp_spec.
  LOOP AT it_app227 WHERE check = 'X'.
    MOVE-CORRESPONDING it_app227 TO l_wa_ztpp_spec.
    DELETE ztpp_spec FROM l_wa_ztpp_spec.
  ENDLOOP.
ENDFORM.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  release
*&---------------------------------------------------------------------*
*       Calling The Next Process
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM release_1209.
  DATA: l_return(05).

  LOOP AT it_app227 WHERE check = 'X'.
    CLEAR it_spec.
    MOVE-CORRESPONDING it_app227 TO it_spec.
    APPEND it_spec.
  ENDLOOP.
*
*
  DATA: lp_chk LIKE ztpp_spec-mark.
  RANGES: l_datum FOR sy-datum.
*
  lp_chk = 'X'.
*
  l_datum-option = 'EQ'.
  l_datum-sign = 'I'.
  l_datum-low = sy-datum.
  APPEND l_datum.
*
  EXPORT it_spec TO MEMORY ID 'SPEC'.
*
  SUBMIT zipp202i_ztppvs
       WITH p_chk EQ lp_chk
       WITH s_datum IN l_datum
       AND RETURN.
*
  l_return = sy-subrc.

ENDFORM.                    " release
*&---------------------------------------------------------------------*
*&      Form  make_file
*&---------------------------------------------------------------------*
*       Making Data For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_file_1209.
  CLEAR IT_EXCEL_1209.
  REFRESH IT_EXCEL_1209.
  MOVE 'KEY CODE' TO IT_EXCEL_1209-keycode.
  MOVE 'OPERATION DATE' TO IT_EXCEL_1209-opdate.
  MOVE 'OPERATION COUNT' TO IT_EXCEL_1209-opcount.
  MOVE 'MARKING' TO IT_EXCEL_1209-mark .
  MOVE 'PLANT' TO IT_EXCEL_1209-plant.
  MOVE 'MODEL CODE' TO IT_EXCEL_1209-model .
  MOVE 'WORK ORDER' TO IT_EXCEL_1209-worder .
  MOVE 'EXTERNAL COLOR' TO IT_EXCEL_1209-extc .
  MOVE 'INTERNAL COLOR' TO IT_EXCEL_1209-intc .
  MOVE 'LAST CHANGED ON' TO IT_EXCEL_1209-erdat .
  MOVE 'TIME LAST CHANGE MADE' TO IT_EXCEL_1209-erzet .
  MOVE 'PERSON CHANGING OBJ' TO IT_EXCEL_1209-ernam .
  APPEND IT_EXCEL_1209.

  LOOP AT it_app227.
    CLEAR IT_EXCEL_1209.
    MOVE-CORRESPONDING it_app227 TO IT_EXCEL_1209.
    APPEND IT_EXCEL_1209.
  ENDLOOP.

ENDFORM.                    " make_file
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       Calling a Function For Download
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM download_1209.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'LIST OF WORDER.XLS'
            filetype                = 'DAT'
            item                    = ' '
            filetype_no_change      = 'X'
            filetype_no_show        = 'X'
       TABLES
            data_tab                = IT_EXCEL_1209
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

ENDFORM.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  setting_internal_fields
*&---------------------------------------------------------------------*
*       Setting Parameters - Sub Screen
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_internal_fields_1210.
* Plant
  CLEAR : plant_list, plant_value.
  name = 'IT_NEW_APP227-PLANT'.
  PERFORM set_field_PLANT_1209.
  PERFORM call_function_VRM USING plant_list.

* Model
  CLEAR : model_list, model_value.
  name = 'IT_NEW_APP227-MODEL'.
  PERFORM set_field_MODEL_1209.
  PERFORM call_function_VRM USING model_list.

* Operation Date

* Operation Count

* ORDER NO


ENDFORM.                    " setting_internal_fields
*&---------------------------------------------------------------------*
*&      Form  SORT_ASCENDING
*&---------------------------------------------------------------------*
*       Sorting - Ascending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_ascending_1209.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP227'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app227 ASCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " SORT_ASCENDING
*&---------------------------------------------------------------------*
*&      Form  SORT_DESCENDING
*&---------------------------------------------------------------------*
*       Sorting - Descending
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sort_descending_1209.
  DATA: field_name01(40),
        offset01 TYPE i.
*
  GET CURSOR FIELD field_name01.
*
  IF field_name01(09) = 'IT_APP227'.
    SEARCH field_name01 FOR '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    SORT it_app227 DESCENDING BY (field_name01).
  ENDIF.
*
ENDFORM.                    " SORT_DESCENDING
