REPORT zipp302u_upload_excel .
INCLUDE: <icon>.
TABLES: mara, t001w, t006, dd07t, cukb.

DATA: zsbm_302u_upload_excel_9000 LIKE zsbm_302u_upload_excel_9000.

*---// Internal Tables
DATA: it_9000 TYPE STANDARD TABLE OF zsbm_302u_upload_excel_9000
                                     WITH HEADER LINE.

DATA: it_bom_ecm LIKE ztbm_bom_ecm OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_bom OCCURS 0,
        mtno   LIKE   ztbm_bom_ecm-mtno,
        plnt   LIKE   ztbm_bom_ecm-plnt,
        usag   LIKE   ztbm_bom_ecm-usag,
        altn   LIKE   ztbm_bom_ecm-altn,
        pref   LIKE   ztbm_bom_ecm-pref,
        comp   LIKE   ztbm_bom_ecm-comp,
        suff   LIKE   ztbm_bom_ecm-suff,
        seqc   LIKE   ztbm_bom_ecm-seqc,
        datf   LIKE   ztbm_bom_ecm-datf,
        datt   LIKE   ztbm_bom_ecm-datt,
        qnty   LIKE   ztbm_bom_ecm-qnty,
        unit   LIKE   ztbm_bom_ecm-unit,
        eitm   LIKE   ztbm_bom_ecm-eitm,
        stgb   LIKE   ztbm_bom_ecm-stgb,
        dpid   LIKE   ztbm_bom_ecm-dpid,
        clpt   LIKE   ztbm_bom_ecm-clpt,
        upgn   LIKE   ztbm_bom_ecm-upgn,
        eono   LIKE   ztbm_bom_ecm-eono,
        zmode  LIKE   ztbm_bom_ecm-zmode,
      END   OF it_bom.

*---// Working area
DATA: wa_total   TYPE i,
      wa_success TYPE i,
      wa_ready   TYPE i,
      wa_error   TYPE i,
      wa_idoc    LIKE ztbm_bom_ecm-idoc.

*---// Constants
CONSTANTS: c_success(4) VALUE icon_green_light,
           c_ready(4)   VALUE icon_yellow_light,
           c_error(4)   VALUE icon_red_light,
           c_check      VALUE 'X'.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: w_container(50),
      w_control(50),
      w_alv(50),
      w_itab(50),
      w_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

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

CONSTANTS: c_structure(100) VALUE 'ZSBM_302U_UPLOAD_EXCEL_'.

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
*  PUBLIC SECTION.
*    METHODS:
*
*    handle_double_click
*        FOR EVENT double_click OF cl_gui_alv_grid
*            IMPORTING e_row
*                      e_column
*                      es_row_no.
*
*    handle_user_command
*        FOR EVENT user_command OF cl_gui_alv_grid
*            IMPORTING e_ucomm,
*
*    handle_data_changed
*        FOR EVENT data_changed OF cl_gui_alv_grid
*            IMPORTING er_data_changed
*                      e_onf4
*                      e_onf4_before
*                      e_onf4_after.
ENDCLASS.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_fname LIKE rlgrap-filename OBLIGATORY
                         DEFAULT 'C:\TEMP\BOM.xls'.
SELECTION-SCREEN END   OF BLOCK bl1.



AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM select_filename.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM import_excel_file.

START-OF-SELECTION.
  PERFORM check_imported_data.
  PERFORM count_rtn.
  PERFORM display_data.

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
  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SORT lt_excel BY row col.
  LOOP AT lt_excel.
    CASE lt_excel-col.
      WHEN  1.
        MOVE: lt_excel-value TO it_bom-mtno.
      WHEN  2.
        MOVE: lt_excel-value TO it_bom-plnt.
      WHEN  3.
        MOVE: lt_excel-value TO it_bom-usag.
      WHEN  4.
        MOVE: lt_excel-value TO it_bom-altn.
      WHEN  5.
        MOVE: lt_excel-value TO it_bom-pref.
      WHEN  6.
        MOVE: lt_excel-value TO it_bom-comp.
      WHEN  7.
        MOVE: lt_excel-value TO it_bom-suff.
      WHEN  8.
        MOVE: lt_excel-value TO it_bom-seqc.
      WHEN  9.
        MOVE: lt_excel-value TO it_bom-datf.
      WHEN 10.
        MOVE: lt_excel-value TO it_bom-datt.
      WHEN 11.
        MOVE: lt_excel-value TO it_bom-qnty.
      WHEN 12.
        MOVE: lt_excel-value TO it_bom-unit.
      WHEN 13.
        MOVE: lt_excel-value TO it_bom-eitm.
      WHEN 14.
        MOVE: lt_excel-value TO it_bom-stgb.
      WHEN 15.
        MOVE: lt_excel-value TO it_bom-dpid.
      WHEN 16.
        MOVE: lt_excel-value TO it_bom-clpt.
      WHEN 17.
        MOVE: lt_excel-value TO it_bom-upgn.
      WHEN 18.
        MOVE: lt_excel-value TO it_bom-eono.
      WHEN 19.
        MOVE: lt_excel-value TO it_bom-zmode.
    ENDCASE.

    AT END OF row.
      APPEND it_bom.
      CLEAR: it_bom.
    ENDAT.
  ENDLOOP.
  DELETE it_bom WHERE mtno EQ space.
ENDFORM.                    " IMPORT_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  check_imported_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_imported_data.
  LOOP AT it_bom.
    CLEAR: it_9000.
    MOVE-CORRESPONDING it_bom TO it_9000.

    MOVE: 'L'      TO it_9000-itca,
          1        TO it_9000-bqty,
          'EA'     TO it_9000-hunt,
          sy-uname TO it_9000-zuser,
          sy-datum TO it_9000-zsdat,
          sy-uzeit TO it_9000-zstim,
          sy-datum TO it_9000-zedat,
          sy-uzeit TO it_9000-zetim.

    PERFORM check_rtn.

    IF it_9000-icon EQ space.
      MOVE: c_ready TO it_9000-icon.
    ENDIF.

    APPEND it_9000.
  ENDLOOP.
ENDFORM.                    " check_imported_data
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  " Material code check
  SELECT SINGLE * FROM mara WHERE matnr = it_9000-mtno.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-icon,
          text-m02 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Plant check
  SELECT SINGLE * FROM t001w WHERE werks = it_9000-plnt.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-icon,
          text-m03 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Compontnt check
  SELECT SINGLE * FROM mara WHERE matnr = it_9000-comp.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-icon,
          text-m04 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Valid date check
  IF it_9000-datf > it_9000-datt.
    MOVE: c_error  TO it_9000-icon,
          text-m05 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Component quantity check
  IF it_9000-qnty <= 0.
    MOVE: c_error  TO it_9000-icon,
          text-m06 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Component UoM check
  SELECT SINGLE * FROM t006 WHERE msehi = it_9000-unit.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-icon,
          text-m07 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " End item type check
  SELECT SINGLE * FROM dd07t WHERE domname    = 'ZEITM'
                               AND as4local   = 'A'
                               AND domvalue_l = it_9000-eitm.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-icon,
          text-m08 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Structure type check
  SELECT SINGLE * FROM dd07t WHERE domname    = 'ZSTGB'
                               AND as4local   = 'A'
                               AND domvalue_l = it_9000-stgb.
  IF sy-subrc NE 0.
    MOVE: c_error  TO it_9000-icon,
          text-m09 TO it_9000-zmsg.
    EXIT.
  ENDIF.

  " Dependency Check
  IF   it_9000-clpt EQ 'C' OR
     ( it_9000-eitm EQ 'M' AND it_9000-dpid NE space ).
    SELECT SINGLE *
      FROM cukb
     WHERE knnam EQ it_9000-dpid.
    IF sy-subrc NE 0.
      MOVE: c_error  TO it_9000-icon,
            text-m10 TO it_9000-zmsg.
      EXIT.
    ENDIF.
  ENDIF.

  " Mode check
  IF NOT ( it_9000-zmode EQ 'U' OR it_9000-zmode EQ 'D' ).
    MOVE: c_error  TO it_9000-icon,
          text-m11 TO it_9000-zmsg.
    EXIT.
  ENDIF.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  SORT it_9000 BY mtno plnt usag altn pref comp suff dpid.
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
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  PERFORM create_alv_object USING sy-dynnr.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_alv_object USING p_dynnr.
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container.
  ASSIGN:      (w_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event.
  ELSE.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
  ENDIF.
ENDFORM.                    " create_alv_object
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_n_object USING p_dynnr.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO w_container,
               'WC_CONTROL_'   p_dynnr INTO w_control,
               'WC_ALV_'       p_dynnr INTO w_alv.

  ASSIGN: (w_container) TO <container>,
          (w_control)   TO <control>,
          (w_alv)       TO <alv>.

  CREATE OBJECT <container>
         EXPORTING container_name = <control>
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
  CREATE OBJECT <alv>
         EXPORTING i_parent      = <container>
                   i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9000.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  set_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.
ENDFORM.                    " set_fieldname
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.
  CASE p_dynnr.
    WHEN '9000'.
      PERFORM set_screen_fields_9000.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9000.
  SORT it_9000 BY mtno plnt usag altn pref comp suff dpid.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MTNO'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'PLNT'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'USAG'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ALTN'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'PREF'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'COMP'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'SUFF'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'DPID'        ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X'.
ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0984   text
*      -->P_0985   text
*      -->P_0986   text
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
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lw_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO w_alv,
               c_structure  p_dynnr      INTO w_structure,
               'IT_'        p_dynnr '[]' INTO w_itab.

  ASSIGN: (w_alv)       TO <alv>,
          (w_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
     EXPORTING i_structure_name = w_structure
               is_layout        = w_is_layout
               i_save           = w_save
               is_variant       = w_variant
               i_default        = space
     CHANGING  it_fieldcatalog  = it_fieldcat[]
               it_outtab        = <itab>.
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  sssign_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sssign_event.

ENDFORM.                    " sssign_event
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
    WHEN 'IMPORT'.
      CLEAR: sy-ucomm.
      PERFORM import_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  COUNT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_rtn.
  CLEAR: wa_total, wa_success, wa_ready, wa_error.

  LOOP AT it_9000.
    wa_total = wa_total + 1.

    CASE it_9000-icon.
      WHEN c_success.
        wa_success = wa_success + 1.
      WHEN c_ready.
        wa_ready   = wa_ready   + 1.
      WHEN c_error.
        wa_error   = wa_error   + 1.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " COUNT_RTN
*&---------------------------------------------------------------------*
*&      Form  IMPORT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM import_rtn.
  if wa_total eq wa_success.
    message e000(zz) with text-m16.
  endif.

  IF wa_total NE wa_ready.
    MESSAGE e000(zz) WITH text-m12.
  ENDIF.

  PERFORM get_interface_document_number.

  LOOP AT it_9000.
    CLEAR: it_bom_ecm.

    MOVE: wa_idoc  TO it_9000-idoc,
          sy-tabix TO it_9000-item,
          space    TO it_9000-zmsg.

    MOVE-CORRESPONDING it_9000 TO it_bom_ecm.

    APPEND it_bom_ecm.
    MODIFY it_9000.
  ENDLOOP.

  INSERT ztbm_bom_ecm FROM TABLE it_9000 ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m14.
  ELSE.
    COMMIT WORK AND WAIT.
    MOVE: c_success TO it_9000-icon,
          space     TO it_9000-zmsg.

    MODIFY it_9000 TRANSPORTING icon zmsg WHERE mtno >= space.

    MESSAGE s000(zz) WITH text-m15.
  ENDIF.

  PERFORM count_rtn.
ENDFORM.                    " IMPORT_RTN
*&---------------------------------------------------------------------*
*&      Form  get_interface_document_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_interface_document_number.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZPP_BOM_IF'
       IMPORTING
            number                  = wa_idoc
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
ENDFORM.                    " get_interface_document_number
