************************************************************************
* Program Name      : ZRPP_VEH_REPAIR_UPLOAD
* Creation Date     : 12/06/12
* Development Request No :
* Addl Documentation:
* Description       : Supplier to Supplier Complete
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zrpp_veh_repair_upload NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.

INCLUDE: <icon>.

CONSTANTS: c_sttime_weld LIKE sy-uzeit VALUE '063000',
           c_sttime_paint LIKE sy-uzeit VALUE '103000',
           c_sttime_ga LIKE sy-uzeit VALUE '143000',
           c_sttime_vpc LIKE sy-uzeit VALUE '183000'.

DATA: it_data TYPE STANDARD TABLE OF ztpp_veh_repair
                                    WITH HEADER LINE.

DATA: BEGIN OF it_0200 OCCURS 0,
      model LIKE ztpp_veh_repair-model,
      body_serial LIKE ztpp_veh_repair-body_serial,
      plant_no LIKE ztpp_veh_repair-plant_no,
      ref_date LIKE ztpp_veh_repair-ref_date,
      ref_time LIKE ztpp_veh_repair-ref_time ,
      part_cd LIKE ztpp_veh_repair-part_cd,
      part_cont LIKE ztpp_veh_repair-part_cont,
      END OF it_0200.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.


*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(5) text-001 FOR FIELD p_weld.
PARAMETERS: p_weld RADIOBUTTON GROUP grp.
SELECTION-SCREEN COMMENT 10(5) text-002  FOR FIELD p_paint.
PARAMETERS: p_paint RADIOBUTTON GROUP grp.
SELECTION-SCREEN COMMENT 20(3) text-003  FOR FIELD p_ga.
PARAMETERS: p_ga RADIOBUTTON GROUP grp.
SELECTION-SCREEN COMMENT 28(3) text-004  FOR FIELD p_vpc.
PARAMETERS: p_vpc RADIOBUTTON GROUP grp.
SELECTION-SCREEN END OF LINE.

PARAMETERS: p_fname LIKE rlgrap-filename OBLIGATORY
                         DEFAULT 'C:\TEMP\.xls'.
SELECTION-SCREEN END   OF BLOCK bl1.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM select_filename.

START-OF-SELECTION.
  PERFORM import_excel_file.
  PERFORM save_to_table.

*&---------------------------------------------------------------------*
*&      Form  SELECT_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_filename.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask          = '*.xls'
    CHANGING
      file_name     = p_fname
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
ENDFORM.                    " SELECT_FILENAME
*&---------------------------------------------------------------------*
*&      Form  IMPORT_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_excel_file.
  CASE 'X'.
    WHEN p_weld.
      PERFORM import_excel_file_weld.
    WHEN p_paint.
      PERFORM import_excel_file_paint.
    WHEN p_ga.
      PERFORM import_excel_file_ga.
    WHEN p_vpc.
      PERFORM import_excel_file_vpc.
  ENDCASE.

ENDFORM.                    " IMPORT_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  check_imported_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM check_imported_data.
*  LOOP AT it_DATA.
*   SELECT SINGLE * FROM dd07t WHERE domname    = 'ZSTGB'
*                               AND as4local   = 'A'
*                               AND domvalue_l = it_DATA-stgb.
*  IF sy-subrc NE 0.
*    MOVE: c_error  TO it_9000-icon,
*          text-m09 TO it_9000-zmsg.
*    EXIT.
*  ENDIF.

*    APPEND it_9000.
*  ENDLOOP.
*ENDFORM.                    " check_imported_data

*&---------------------------------------------------------------------*
*&      Form  IMPORT_EXCEL_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_excel_file_weld.
  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

  DATA: l_time LIKE sy-uzeit,
        l_date LIKE sy-datum,
        l_char8(8),
        l_idnex LIKE sy-tabix..

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 19
      i_end_row               = 40000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e009 WITH 'Upload Excel file Error'.
  ENDIF.

  SORT lt_excel BY row col.
  LOOP AT lt_excel.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lt_excel-value
        replacement       = 32
      IMPORTING
        outtext           = lt_excel-value
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      MESSAGE e997 WITH 'Special character checking error '
                        lt_excel-value.
    ENDIF.

    TRANSLATE lt_excel-value TO UPPER CASE.

    CASE lt_excel-col.
      WHEN  1.
        SPLIT  lt_excel-value AT space
         INTO:  it_data-model it_data-body_serial.
*        MOVE: lt_excel-value+0(3) TO it_data-model.
*        MOVE: lt_excel-value+4(6) TO it_data-body_serial.
      WHEN  2.
        MOVE: lt_excel-value TO it_data-part_cont.
      WHEN  3.
        MOVE: lt_excel-value TO it_data-ref_desc.
      WHEN  4.
        MOVE: lt_excel-value TO it_data-ref_loc.
      WHEN  6.
        MOVE: lt_excel-value TO it_data-inspector.
      WHEN  7.
        MOVE: lt_excel-value TO it_data-ref_compl.
      WHEN  8.
        MOVE: lt_excel-value TO it_data-resp1.
      WHEN 10.
        CONCATENATE lt_excel-value+6(4) lt_excel-value+0(2)
                     lt_excel-value+3(2) INTO l_char8.

*        MOVE: l_char8 TO it_data-soff_date.
        MOVE: l_char8 TO it_data-ref_date.
    ENDCASE.

    AT END OF row.
      APPEND it_data.
      CLEAR: it_data.
    ENDAT.
  ENDLOOP.

  DELETE it_data WHERE model EQ space.

  SORT it_data BY ref_date.
  l_time = c_sttime_weld.
  l_date = '29991299'.

  LOOP AT it_data.
    l_idnex = sy-tabix.
    IF l_date = it_data-ref_date.
      l_time = l_time + 1.
    ELSE.
      l_time = c_sttime_weld.
      l_date = it_data-ref_date..
    ENDIF.
    it_data-ref_time = l_time.
    it_data-plant_no = 'P001'.
    it_data-ref_main = it_data-rep_dept = 'WELD'.
    it_data-zresult = 'I'.
    it_data-zuser = sy-uname.
    it_data-zedat = sy-datum.
    it_data-zetim = sy-uzeit.
    MODIFY it_data INDEX l_idnex.
  ENDLOOP.
ENDFORM.                    " IMPORT_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  IMPORT_EXCEL_FILE_PAINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_excel_file_paint .
  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

  DATA: l_time LIKE sy-uzeit,
        l_date LIKE sy-datum,
        l_char8(8),
        l_idnex LIKE sy-tabix..

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 19
      i_end_row               = 40000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e009 WITH 'Upload Excel file Error'.
  ENDIF.

  SORT lt_excel BY row col.
  LOOP AT lt_excel.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lt_excel-value
        replacement       = 32
      IMPORTING
        outtext           = lt_excel-value
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      MESSAGE e997 WITH 'Special character checking error '
                        lt_excel-value.
    ENDIF.

    TRANSLATE lt_excel-value TO UPPER CASE.

    CASE lt_excel-col.
      WHEN  1.
        SPLIT  lt_excel-value AT space
         INTO:  it_data-model it_data-body_serial.
*        MOVE: lt_excel-value+0(3) TO it_data-model.
*        MOVE: lt_excel-value+4(6) TO it_data-body_serial
      WHEN  3.
        MOVE: lt_excel-value TO it_data-part_cont.
      WHEN  4.
        MOVE: lt_excel-value TO it_data-ref_desc.
** Added on 02/18/13
      WHEN 5.
          it_data-ref_compl = lt_excel-value+0(1).
** End on 02/18/13
      WHEN  6.
        MOVE: lt_excel-value TO it_data-ref_loc.
      WHEN  7.
        MOVE: lt_excel-value TO it_data-inspector.
      WHEN 10.
        CONCATENATE lt_excel-value+6(4) lt_excel-value+0(2)
                     lt_excel-value+3(2) INTO l_char8.

*        MOVE: l_char8 TO it_data-soff_date.
        MOVE: l_char8 TO it_data-ref_date.
    ENDCASE.

    AT END OF row.
      APPEND it_data.
      CLEAR: it_data.
    ENDAT.
  ENDLOOP.

  DELETE it_data WHERE model EQ space.

** Added on 02/18/13
  MOVE: 'B' TO it_data-ref_compl.
  MODIFY IT_DATA TRANSPORTING REF_COMPL
                 WHERE REF_COMPL = SPACE.
** End on 02/18/13

  SORT it_data BY ref_date.
  l_time = c_sttime_paint.
  l_date = '29991299'.

  LOOP AT it_data.
    l_idnex = sy-tabix.
    IF l_date = it_data-ref_date.
      l_time = l_time + 1.
    ELSE.
      l_time = c_sttime_paint.
      l_date = it_data-ref_date..
    ENDIF.
    it_data-ref_time = l_time.
    it_data-plant_no = 'P001'.
    it_data-ref_main = it_data-rep_dept = 'PAINT'.
    it_data-zresult = 'I'.
    it_data-zuser = sy-uname.
    it_data-zedat = sy-datum.
    it_data-zetim = sy-uzeit.
    MODIFY it_data INDEX l_idnex.
  ENDLOOP.
ENDFORM.                    " IMPORT_EXCEL_FILE_PAINT
*&---------------------------------------------------------------------*
*&      Form  IMPORT_EXCEL_FILE_GA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_excel_file_ga .
  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

  DATA: l_time LIKE sy-uzeit,
        l_date LIKE sy-datum,
        l_char8(8),
        l_idnex LIKE sy-tabix..

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 19
      i_end_row               = 40000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e009 WITH 'Upload Excel file Error'.
  ENDIF.

  SORT lt_excel BY row col.
  LOOP AT lt_excel.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lt_excel-value
        replacement       = 32
      IMPORTING
        outtext           = lt_excel-value
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      MESSAGE e997 WITH 'Special character checking error '
                        lt_excel-value.
    ENDIF.

    TRANSLATE lt_excel-value TO UPPER CASE.

    CASE lt_excel-col.
      WHEN  1.
        SPLIT  lt_excel-value AT space
         INTO:  it_data-model it_data-body_serial.
*        MOVE: lt_excel-value+0(3) TO it_data-model.
*        MOVE: lt_excel-value+4(6) TO it_data-body_serial.
      WHEN  2.
        MOVE: lt_excel-value TO it_data-part_cont.
      WHEN  3.
        MOVE: lt_excel-value TO it_data-ref_desc.
      WHEN  4.
        MOVE: lt_excel-value TO it_data-ref_loc.
      WHEN  5.
        MOVE: lt_excel-value TO it_data-inspector.
      WHEN  7.
        MOVE: lt_excel-value TO it_data-ref_main.
      WHEN  8.
        MOVE: lt_excel-value TO it_data-resp1.
      WHEN  9.
        MOVE: lt_excel-value TO it_data-resp2.
      WHEN 11.
        MOVE: lt_excel-value TO it_data-repairer.
      WHEN 12.
        MOVE: lt_excel-value TO it_data-categ2.
      WHEN 13.
        MOVE: lt_excel-value TO it_data-rep_mh.
      WHEN 17.
*        CONCATENATE lt_excel-value+6(4) lt_excel-value+0(2)
*                   lt_excel-value+3(2) INTO l_char8.
        CONCATENATE lt_excel-value+6(4) lt_excel-value+3(2)
                 lt_excel-value+0(2) INTO l_char8.
        MOVE: l_char8 TO it_data-soff_date.
        MOVE: l_char8 TO it_data-ref_date.
    ENDCASE.

    AT END OF row.
      APPEND it_data.
      CLEAR: it_data.
    ENDAT.
  ENDLOOP.

  DELETE it_data WHERE model EQ space.

  SORT it_data BY soff_date.
  l_time = c_sttime_ga.
  l_date = '29991299'.

  LOOP AT it_data.
    l_idnex = sy-tabix.
    IF l_date = it_data-soff_date.
      l_time = l_time + 1.
    ELSE.
      l_time = c_sttime_ga.
      l_date = it_data-soff_date..
    ENDIF.
    it_data-ref_time = l_time.
    it_data-plant_no = 'P001'.
    it_data-rep_dept = 'ASSEMBLY'.
    it_data-zresult = 'I'.
    it_data-zuser = sy-uname.
    it_data-zedat = sy-datum.
    it_data-zetim = sy-uzeit.
    MODIFY it_data INDEX l_idnex.
  ENDLOOP.
ENDFORM.                    " IMPORT_EXCEL_FILE_GA
*&---------------------------------------------------------------------*
*&      Form  IMPORT_EXCEL_FILE_VPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_excel_file_vpc .
  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

  DATA: l_time LIKE sy-uzeit,
        l_date LIKE sy-datum,
        l_char8(8),
        l_idnex LIKE sy-tabix..

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 19
      i_end_row               = 40000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE e009 WITH 'Upload Excel file Error'.
  ENDIF.

  SORT lt_excel BY row col.
  LOOP AT lt_excel.

    CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
      EXPORTING
        intext            = lt_excel-value
        replacement       = 32
      IMPORTING
        outtext           = lt_excel-value
      EXCEPTIONS
        invalid_codepage  = 1
        codepage_mismatch = 2
        internal_error    = 3
        cannot_convert    = 4
        fields_not_type_c = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      MESSAGE e997 WITH 'Special character checking error '
                        lt_excel-value.
    ENDIF.

    TRANSLATE lt_excel-value TO UPPER CASE.

    CASE lt_excel-col.
      WHEN  3.
        SPLIT  lt_excel-value AT space
         INTO:  it_data-model it_data-body_serial.
*        MOVE: lt_excel-value+0(3) TO it_data-model.
*        MOVE: lt_excel-value+4(6) TO it_data-body_serial.
      WHEN 6.
        CLEAR: l_char8.
        CONCATENATE lt_excel-value+6(4) lt_excel-value+0(2)
                    lt_excel-value+3(2) INTO l_char8.
        MOVE: l_char8 TO it_data-ref_date.
      WHEN  7.
        MOVE: lt_excel-value TO it_data-part_cont.
      WHEN  8.
        MOVE: lt_excel-value TO it_data-part_cd.
      WHEN  9.
        MOVE: lt_excel-value TO it_data-ref_desc.
      WHEN  10.
        MOVE: lt_excel-value TO it_data-ref_cd.
      WHEN  11.
        MOVE: lt_excel-value TO it_data-ref_loc.
      WHEN  17.
        MOVE: lt_excel-value TO it_data-shift.
      WHEN  18.
        MOVE: lt_excel-value TO it_data-inspector.


      WHEN 15.
        CLEAR: l_char8.
        CONCATENATE lt_excel-value+6(4) lt_excel-value+0(2)
                    lt_excel-value+3(2) INTO l_char8.
        MOVE: l_char8 TO it_data-soff_date.
    ENDCASE.

    AT END OF row.
      APPEND it_data.
      CLEAR: it_data.
    ENDAT.
  ENDLOOP.

  DELETE it_data WHERE model EQ space.

  SORT it_data BY soff_date.
  l_time = c_sttime_vpc.
  l_date = '29991299'.

  LOOP AT it_data.
    l_idnex = sy-tabix.
    IF l_date = it_data-soff_date.
      l_time = l_time + 1.
    ELSE.
      l_time = c_sttime_vpc.
      l_date = it_data-soff_date..
    ENDIF.
    it_data-ref_time = l_time.
    it_data-plant_no = 'P001'.
    it_data-ref_main = it_data-rep_dept = 'VPC'.
    it_data-zresult = 'I'.
    it_data-zuser = sy-uname.
    it_data-zedat = sy-datum.
    it_data-zetim = sy-uzeit.
    MODIFY it_data INDEX l_idnex.
  ENDLOOP.
ENDFORM.                    " IMPORT_EXCEL_FILE_VPC
*&---------------------------------------------------------------------*
*&      Form  SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_to_table .
  INSERT ztpp_veh_repair FROM TABLE it_data
   ACCEPTING DUPLICATE KEYS.
  IF sy-subrc = 0.
    COMMIT WORK.
    PERFORM display_result.
  ELSE.
    ROLLBACK WORK.
    PERFORM diaplay_error.
  ENDIF.
ENDFORM.                    " SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*&      Form  DIAPLAY_ERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM diaplay_error .
  DATA: lt_temp TYPE SORTED TABLE OF ztpp_veh_repair WITH HEADER LINE
        WITH UNIQUE KEY model body_serial plant_no ref_date ref_time.

  SELECT * INTO TABLE lt_temp
    FROM ztpp_veh_repair
    FOR ALL ENTRIES IN it_data
    WHERE model = it_data-model
      AND body_serial = it_data-body_serial
      AND plant_no = it_data-plant_no
      AND ref_date = it_data-ref_date
      AND ref_time = it_data-ref_time.


  LOOP AT it_data.
    READ TABLE lt_temp WITH KEY model = it_data-model
                                body_serial = it_data-body_serial
                                plant_no = it_data-plant_no
                                ref_date = it_data-ref_date
                                ref_time = it_data-ref_time.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_data TO it_0200.
      APPEND it_0200.
    ENDIF.
    CLEAR: it_0200, it_data.
  ENDLOOP.
  CALL SCREEN 0200.
ENDFORM.                    " DIAPLAY_ERROR
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_result.
  DATA: l_count TYPE i,
        l_date TYPE sy-datum.
  SORT it_data BY ref_date.
  CLEAR: l_count, l_date.

  WRITE: /1 'Data was successfully updated'.
  WRITE: /1 '============================='.

  WRITE: /1(10) 'Rep Date'.
  WRITE: 15(12) 'Total Count '.

  WRITE: /1(10) '--------'.
  WRITE: 15(12) '------------'.

  READ TABLE it_data INDEX 1.
  l_date = it_data-ref_date.
  LOOP AT it_data.
    IF l_date = it_data-ref_date.
      l_count = l_count + 1.
    ELSE.

      WRITE: /1(10) l_date MMDDYY.
      WRITE: 15(13) l_count.

      l_date = it_data-ref_date.
      l_count = 1.
    ENDIF.
  ENDLOOP.
  WRITE: /1(10) l_date MMDDYY.
  WRITE: 15(13) l_count.

ENDFORM.                    " DISPLAY_RESULT

*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_0200'.
    PERFORM assign_itab_to_alv TABLES it_0200.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-grid_title = 'Duplicate Data List'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                'S' 'MODEL'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Model',
                                  'E' 'OUTPUTLEN'   '5',

                                   'S' 'BODY_SERIAL'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Body No',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'PLANT_NO'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Source Plant',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'REF_DATE'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Rep Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'REF_TIME'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Ref Time',
                                    'E' 'OUTPUTLEN'   '8',

                                  'S' 'PART_CONT'       ' ',
                                  ' ' 'COLTEXT'     'Part Contenty',
                                  'E' 'OUTPUTLEN'   '40'.


ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv TABLES p_itab.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = p_itab[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
