************************************************************************
* Program Name      : ZACO58_SET_MATERIAL_PRICE
* Author            : Bae, Byung Sung
* Creation Date     : 2004.10.07.
* Specifications By : Bae, Byung Sung
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Set up material price for annual business plan
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zaco58_set_material_price .
INCLUDE: <icon>.


*---/// Defile Internal tables
" File contents
DATA: BEGIN OF it_file OCCURS 0,
        werks(4),                    "Plant
        matnr(18),                   "Material
        zplp3(14),                   "Price
        dmbtr(14),
        dduty(14),
        dfrgt(14),
        dcost(14),
        lifnr(10),                   "vendor
        zpld3(8),                    "Date(YYYYMMDD)
      END   OF it_file.

" Display Layout
DATA: it_9000 LIKE zsco_zcoa58_9000 OCCURS 0 WITH HEADER LINE.

" BAPI Return value
DATA: it_bapiret2   LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*---/// Global variable and structures
DATA: wa_head       LIKE bapimathead, "Header with control information
      wa_plant      LIKE bapi_marc  , "plant-specific material DATA
      wa_plantx     LIKE bapi_marcx ,
      wa_mbew       LIKE bapi_mbew  ,
      wa_mbewx      LIKE bapi_mbewx .

*---/// Constants
DATA: c_mark        VALUE 'X',
      c_ready(4)    VALUE icon_light_out,    "Ready
      c_bapi_err(4) VALUE icon_yellow_light, "BAPI call error
      c_exl_err(4)  VALUE icon_red_light,    "EXCEL file error
      c_success(4)  VALUE icon_green_light.  "Success

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : w_is_layout TYPE lvc_s_layo,
       w_variant   TYPE disvariant,          "for parameter IS_VARIANT
       w_fieldname LIKE LINE OF it_fieldcat,
       w_repid     LIKE sy-repid,
       w_cnt       TYPE i,                   "Field count
       w_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no.
ENDCLASS.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click
ENDCLASS.

TABLES: ztmm_analy.

*---/// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_peri LIKE  ztmm_analy-period.
PARAMETERS: p_file LIKE  rlgrap-filename OBLIGATORY
                         DEFAULT 'C:\TEMP\ABP_PRICE.TXT'.
SELECTION-SCREEN END   OF BLOCK bl1.
parameters: p_punit  as checkbox default ' '.
parameters: p_insert as checkbox default 'X'.

*---/// Possibel entry
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM select_file_name.

*---/// File upload
AT SELECTION-SCREEN.
  CHECK sy-ucomm = 'ONLI'.
  PERFORM file_upload.

*---/// Processing
START-OF-SELECTION.
  PERFORM set_it_9000.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  select_file_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_file_name.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            mask          = '*.TXT'
       CHANGING
            file_name     = p_file
       EXCEPTIONS
            mask_too_long = 1
            OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " select_file_name
*&---------------------------------------------------------------------*
*&      Form  FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM file_upload.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            filename                = p_file
            filetype                = 'DAT'
       TABLES
            data_tab                = it_file
       EXCEPTIONS
            conversion_error        = 1
            file_open_error         = 2
            file_read_error         = 3
            invalid_type            = 4
            no_batch                = 5
            unknown_error           = 6
            invalid_table_width     = 7
            gui_refuse_filetransfer = 8
            customer_error          = 9
            OTHERS                  = 10.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  READ TABLE it_file INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " FILE_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  set_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_it_9000.
  DATA: lw_zplp3(100),
        lw_decimal TYPE f.

  LOOP AT it_file.
    CLEAR: it_9000.

    MOVE: it_file-werks TO it_9000-werks,
          it_file-matnr TO it_9000-matnr,
          it_file-zplp3 TO it_9000-zplp3_file,
          it_file-dmbtr TO it_9000-dmbtr,
          it_file-dduty TO it_9000-dduty,
          it_file-dfrgt TO it_9000-dfrgt,
          it_file-dcost TO it_9000-dcost,
          it_file-lifnr TO it_9000-lifnr,
          it_file-zpld3 TO it_9000-zpld3.

    PERFORM numeric_check.

    CHECK it_9000-msg EQ ' '.

    SELECT SINGLE maktx INTO it_9000-maktx
      FROM makt
     WHERE matnr = it_9000-matnr
       AND spras = sy-langu.
    IF sy-subrc NE 0.
      MOVE: text-b02  TO it_9000-msg,
            c_exl_err TO it_9000-flag.
      APPEND it_9000.
      CONTINUE.
    ENDIF.

    SELECT SINGLE peinh INTO it_9000-peinh
      FROM mbew
    WHERE matnr = it_9000-matnr
      AND bwkey = it_9000-werks.
    IF sy-subrc NE 0.
      MOVE: text-b01  TO it_9000-msg,
            c_exl_err TO it_9000-flag.
      APPEND it_9000.
      CONTINUE.
    ENDIF.

    if p_punit = 'X'.
      it_9000-zplp3 = it_9000-zplp3_file * it_9000-peinh.
    else.
      it_9000-zplp3 = it_9000-zplp3_file.
    endif.

    IF it_9000-zplp3 EQ 0.
      MOVE: text-b04  TO it_9000-msg,
            c_exl_err TO it_9000-flag.
      APPEND it_9000.
      CONTINUE.
    ENDIF.

    MOVE: c_ready TO it_9000-flag.
    APPEND it_9000.
  ENDLOOP.
ENDFORM.                    " set_it_9000
*&---------------------------------------------------------------------*
*&      Form  NUMERIC_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM numeric_check.
  CALL FUNCTION 'CATS_NUMERIC_INPUT_CHECK'
       EXPORTING
            input      = it_file-zplp3
       IMPORTING
            output     = it_file-zplp3
       EXCEPTIONS
            no_numeric = 1
            OTHERS     = 2.
  IF sy-subrc <> 0.
    MOVE: text-b03  TO it_9000-msg,
          c_exl_err TO it_9000-flag.
    APPEND it_9000.
  ENDIF.
ENDFORM.                    " NUMERIC_CHECK
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_9000 OUTPUT.
  IF wc_container_9000 IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object_9000.
    PERFORM set_attributes_alv_grid_9000.
    PERFORM build_field_catalog USING 'IT_9000'.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv_9000.
    PERFORM sssign_event_9000.
  ENDIF.
ENDMODULE.                 " create_alv_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object_9000.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  CREATE OBJECT wc_container_9000
         EXPORTING container_name = wc_control_9000
         EXCEPTIONS
          cntl_error = 1
          cntl_system_error = 2
          create_error = 3
          lifetime_error = 4
          lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT wc_alv_9000
         EXPORTING i_parent      = wc_container_9000
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object_9000
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_9000.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_mark.   "/optimizes the column width
  w_is_layout-no_merging = c_mark.   "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0478   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  w_repid = sy-repid.
  lw_itab = p_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ZPLP3_FILE'  ' ',
                                  ' ' 'COLTEXT'     'Excel Price',
                                  'E' 'COL_POS'     '5'.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_9000.
  SORT it_9000 BY flag matnr.

  CALL METHOD wc_alv_9000->set_table_for_first_display
     EXPORTING i_structure_name = 'ZSCO_ZCOA58_9000'
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = it_9000[].
ENDFORM.                    " assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event_9000.
*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT event_receiver.

  SET HANDLER event_receiver->handle_double_click  FOR wc_alv_9000.
ENDFORM.                    " sssign_event_9000

*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1873   text
*      -->P_1874   text
*      -->P_1875   text
*----------------------------------------------------------------------*
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
      MESSAGE e000(zz) WITH 'Check filed catalog'.
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
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO w_cnt.
      p_fieldcat-col_pos = w_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_9000 USING p_column_name             "Column Name
                          ps_row_no  LIKE sy-tabix. "Numeric Row ID
  DATA : lw_sel_index LIKE sy-tabix.

  MOVE: ps_row_no TO lw_sel_index.

  READ TABLE it_9000 INDEX lw_sel_index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  SET PARAMETER ID 'MAT' FIELD it_9000-matnr.
  CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      CLEAR: sy-ucomm.
      PERFORM save_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_rtn.
  DATA: lw_total(4)      VALUE 0,
        lw_success(4)    VALUE 0,
        lw_index LIKE sy-tabix.

  "/Indexes of Selected Rows
  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CALL METHOD wc_alv_9000->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.

  LOOP AT lt_rows.
    READ TABLE it_9000 INDEX lt_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    MOVE: sy-tabix TO lw_index.

    CHECK it_9000-flag EQ c_ready OR
          it_9000-flag EQ c_bapi_err.

    CLEAR: wa_head, wa_plant, wa_plantx,
           wa_mbew, wa_mbewx.

    CLEAR: it_bapiret2, it_bapiret2[].

    wa_head-material     = it_9000-matnr.
    wa_head-cost_view    = 'X'.
    wa_plant-lot_size    = it_9000-peinh.
    wa_plantx-lot_size   = 'X'.
    wa_plant-plant       = it_9000-werks.
    wa_plantx-plant      = it_9000-werks.
    wa_mbew-val_area     = it_9000-werks.
    wa_mbew-plndprice3   = it_9000-zplp3.
    wa_mbew-plndprdate3  = it_9000-zpld3.
    wa_mbew-price_unit   = it_9000-peinh.
    wa_mbewx-val_area    = it_9000-werks.
    wa_mbewx-plndprice3  = 'X'.
    wa_mbewx-plndprdate3 = 'X'.
    wa_mbewx-price_unit  = 'X'.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
         EXPORTING
              headdata       = wa_head
              valuationdata  = wa_mbew
              valuationdatax = wa_mbewx
              plantdata      = wa_plant
              plantdatax     = wa_plantx
         TABLES
              returnmessages = it_bapiret2.

    LOOP AT it_bapiret2 WHERE type = 'E'
                           OR type = 'A'.
    ENDLOOP.

    IF sy-subrc EQ 0.
      MOVE: c_bapi_err  TO it_9000-flag,
            it_bapiret2 TO it_9000-msg.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
           EXPORTING
                wait = 'X'.

      MOVE: c_success   TO it_9000-flag,
            space       TO it_9000-msg.
      lw_success = lw_success + 1.

* save to zTable for history (Andy Choi)
      ztmm_analy-period    = p_peri.
      ztmm_analy-EKORG     = 'PU01'.
      ztmm_analy-matnr     = it_9000-matnr.
      ztmm_analy-werks     = it_9000-werks.
      ztmm_analy-peinh     = it_9000-peinh.
      ztmm_analy-waers     = 'USD'.
      ztmm_analy-wrbtr     = it_9000-dmbtr.
      ztmm_analy-wDUTY     = it_9000-dDUTY.
      ztmm_analy-wFRGT     = it_9000-dFRGT.
      ztmm_analy-wCOST     = it_9000-dCOST.

      ztmm_analy-dmbtr     = it_9000-dmbtr.
      ztmm_analy-DDUTY     = it_9000-DDUTY.
      ztmm_analy-DFRGT     = it_9000-DFRGT.
      ztmm_analy-DCOST     = it_9000-DCOST.

      ztmm_analy-lifnr     = it_9000-lifnr.
      ztmm_analy-source    = 'M'.
      ztmm_analy-BASE_D    = it_9000-zpld3.
      ztmm_analy-erdat     = sy-datum.
      ztmm_analy-ernam     = sy-uname.

      if p_insert = 'X'.
        INSERT ztmm_analy.
      else.
        MODIFY ztmm_analy.
      endif.
    ENDIF.

    lw_total = lw_total + 1.

    MODIFY it_9000 INDEX lw_index.
  ENDLOOP.

  PERFORM assign_itab_to_alv_9000.

  MESSAGE s000(zz) WITH text-m03 lw_total text-m04 lw_success.
ENDFORM.                    " SAVE_RTN
