*&---------------------------------------------------------------------*
*& Program ID     : ZFIR_ASSET_MASS_CHANGE                             *
*& Program Name   : Asset mass change                                  *
*& Created by     : Furong                                             *
*& Created on     : 05/12/2014                                         *
*& Reference Pgm  : BDC AS02 by uploading excel                        *
*&                                                                     *
*& Modification Log                                                    *
*                                                                      *
*                                                                      *
*&=====================================================================*
REPORT zfir_asset_mass_change NO STANDARD PAGE HEADING
                             MESSAGE-ID zmfi.

TABLES: sscrfields.
INCLUDE <icon>.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

*-----/// Global variable
DATA: v_selected   TYPE   i.

DATA: BEGIN OF it_9000 OCCURS 0,
      anln1 LIKE anla-anln1,  "ASSET #
      anln2 LIKE anla-anln2,  " SUB ASSET #
      bukrs LIKE anla-bukrs,  "COMP CODE
      txa50 LIKE anla-txa50,   " additional asset
      gplab LIKE anla-gplab, " planned retirement date
      ord41 LIKE anla-ord41, " evaluation grp
      vmgli LIKE anla-vmgli, "property class key
      eigkz LIKE anla-eigkz,"propert indicator
      zmsg(100),
      END OF it_9000.

DATA: w_gjahr TYPE gjahr.

*-----/// Constants
CONSTANTS: c_true                           VALUE 'X',
           c_yes                            VALUE 'J',
           c_structure(100)                 VALUE 'IT_'.

*-----/// ALV Control : START

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: v_container(50),
      v_control(50),
      v_alv(50),
      v_itab(50),
*      v_ans,
      v_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO cl_gui_custom_container,
               <control>   TYPE        scrfname,
               <alv>       TYPE REF TO cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

* Global variable for ALV GRID
DATA : v_is_layout TYPE lvc_s_layo,
       v_variant   TYPE disvariant,          "for parameter IS_VARIANT
       v_fieldname LIKE LINE OF it_fieldname,
       v_repid     LIKE sy-repid,
       v_cnt       TYPE i,                   "Field count
       v_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
PARAMETERS:
 p_file  LIKE rlgrap-filename OBLIGATORY,
 p_filety LIKE rlgrap-filetype DEFAULT 'DAT' NO-DISPLAY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_wdate RADIOBUTTON GROUP grp DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(50) text-002 FOR FIELD p_wdate.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_wodate  RADIOBUTTON GROUP grp.
SELECTION-SCREEN COMMENT 3(50) text-001 FOR FIELD p_wodate.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.

PARAMETERS: p_bdc(1) DEFAULT ' '.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN FUNCTION KEY 2.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file .

AT SELECTION-SCREEN OUTPUT.
  PERFORM init_screen.

INITIALIZATION.
  PERFORM initial_value.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'FC01'.
    PERFORM get_template.
  ENDIF.
  IF sscrfields-ucomm = 'FC02'.
    PERFORM set_template.
  ENDIF.

START-OF-SELECTION.

  PERFORM upload_data.
  CALL SCREEN 9000.

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
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
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
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'POST'.
      CLEAR: sy-ucomm.
      PERFORM generate_rtn.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
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
  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container.
  ASSIGN:      (v_container)           TO   <container>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attributes_alv_grid USING p_dynnr.
    PERFORM build_field_catalog USING p_dynnr.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv USING p_dynnr.
    PERFORM sssign_event USING p_dynnr.
  ELSE.
    CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
    ASSIGN: (v_alv)       TO <alv>.
    CALL METHOD <alv>->refresh_table_display.

*    PERFORM set_attributes_alv_grid USING p_dynnr.
*    PERFORM build_field_catalog USING p_dynnr.
**    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
*    PERFORM assign_itab_to_alv USING p_dynnr.
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

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container,
               'WC_CONTROL_'   p_dynnr INTO v_control,
               'WC_ALV_'       p_dynnr INTO v_alv.

  ASSIGN: (v_container) TO <container>,
          (v_control)   TO <control>,
          (v_alv)       TO <alv>.

  CREATE OBJECT <container>
    EXPORTING
      container_name              = <control>
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT <alv>
    EXPORTING
      i_parent      = <container>
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
  CLEAR : v_is_layout, v_variant.

  v_is_layout-edit       = ' '.      "/Edit Mode Enable
  v_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  v_is_layout-language   = sy-langu. "/Language Key
  v_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
  v_is_layout-no_merging = 'X'.      "/Disable cell merging
  v_variant-report       = sy-repid.
  v_variant-username     = sy-uname.
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

  MOVE: sy-repid TO v_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = v_repid
      i_internal_tabname = lw_itab
      i_inclname         = v_repid
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

  REFRESH: it_fieldcat.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'ANLN1'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Asset No',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ANLN2'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Sub Asset',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'TXA50'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Addl Description',
                                  'E' 'OUTPUTLEN'   '60',

                                  'S' 'GPLAB'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Plnd Retire Date',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'ORD41'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Evaluation group1',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'VMGLI'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Class. Key',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'EIGKZ'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Property ind',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'ZMSG'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Message',
                                  'E' 'OUTPUTLEN'   '90'.

ENDFORM.                    " set_screen_fields_9000
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv,
               c_structure  p_dynnr      INTO v_structure,
               'IT_'        p_dynnr '[]' INTO v_itab.

  ASSIGN: (v_alv)       TO <alv>,
          (v_itab)      TO <itab>.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
*     i_structure_name = v_structure
      is_layout        = v_is_layout
      i_save           = v_save
      is_variant       = v_variant
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
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
FORM sssign_event USING p_dynnr.
  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

*--  Regist event for Edit
  IF sy-batch IS INITIAL.
    CALL METHOD <alv>->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
  ENDIF.
ENDFORM.                    " sssign_event
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1033   text
*      -->P_1034   text
*      -->P_1035   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : lv_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO v_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: v_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO lv_col.
  ASSIGN (lv_col) TO <fs>.
  MOVE   p_value  TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    IF p_fieldcat-col_pos IS INITIAL.
      ADD 1 TO v_cnt.
      p_fieldcat-col_pos = v_cnt.
    ENDIF.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  initial_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initial_value .

  MOVE: sy-datum(4)          TO w_gjahr,
        'c:\temp\asse mass.xlsx' TO p_file.

*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " initial_value
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_data .
  DATA: l_date LIKE sy-datum.

  DATA: BEGIN OF lt_excel OCCURS 0.
          INCLUDE STRUCTURE alsmex_tabline.
  DATA: END OF lt_excel.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 16
      i_end_row               = 40000
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    EXIT.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_9000, it_9000[].

  SORT lt_excel BY row col.
  LOOP AT lt_excel.
    CASE lt_excel-col.
      WHEN  1.
        MOVE: lt_excel-value TO it_9000-anln1.
      WHEN  2.
        MOVE: lt_excel-value TO it_9000-anln2.
      WHEN  3.
        MOVE: lt_excel-value TO it_9000-bukrs.
      WHEN  4.
        MOVE: lt_excel-value TO it_9000-txa50.
      WHEN  5.
        CONCATENATE lt_excel-value+6(4) lt_excel-value+0(2)
             lt_excel-value+3(2) INTO it_9000-gplab.
*        MOVE: lt_excel-value TO it_9000-gplab.
      WHEN  6.
        MOVE: lt_excel-value TO it_9000-ord41.
      WHEN  7.
        MOVE: lt_excel-value TO it_9000-vmgli.
      WHEN  8.
        MOVE: lt_excel-value TO it_9000-eigkz.
    ENDCASE.

    AT END OF row.
      APPEND it_9000..
    ENDAT.
  ENDLOOP.

  DELETE it_9000 WHERE anln1 EQ space.

  IF p_wdate IS NOT INITIAL.
    LOOP AT it_9000.
      IF it_9000-gplab IS INITIAL.
        MESSAGE e000(zz) WITH
             'Please input the date for ' it_9000-anln1.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  GENERATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_rtn .

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        l_index LIKE sy-tabix.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
*
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = sy-repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE lt_rows LINES l_line.
  IF l_line = 0.
    MESSAGE e001.
  ENDIF.
  IF p_bdc = 'A'.
    wa_opt-dismode = 'A'.
  ENDIF.

  LOOP AT lt_rows.
    READ TABLE it_9000 INDEX lt_rows-index.
    IF sy-subrc = 0.
      PERFORM generate_bdc.
      PERFORM posting_rtn USING lt_rows-index.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GENERATE_RTN
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc .
  DATA : l_date(8).

  CLEAR: it_bdc, it_bdc[].

*  CONCATENATE w_gjahr '0101' INTO l_date.
  WRITE: it_9000-gplab TO l_date.

  PERFORM dynpro USING:
    'X' 'SAPLAIST'    '0100',
    ' ' 'BDC_CURSOR' 'ANLA-ANLN1',
    ' ' 'BDC_OKCODE'  '/00',
    ' ' 'ANLA-ANLN1'  it_9000-anln1,
    ' ' 'ANLA-ANLN2'  it_9000-anln2,
    ' ' 'ANLA-BUKRS'  it_9000-bukrs,

    'X' 'SAPLAIST'    '1000',
    ' ' 'BDC_OKCODE'  '=TAB03',
    ' ' 'ANLA-TXA50'  it_9000-txa50.

  IF p_wdate = 'X'.    " with retirement date
    PERFORM dynpro USING:
    ' ' 'BDC_CURSOR' 'ANLA-GPLAB',
    ' ' 'ANLA-GPLAB'  l_date.
  ENDIF.

  PERFORM dynpro USING:
    'X' 'SAPLAIST'    '1000',
    ' ' 'BDC_OKCODE'  '=TAB05',
    ' ' 'BDC_CURSOR' 'ANLA-ORD41',
    ' ' 'ANLA-ORD41'  it_9000-ord41,

    'X' 'SAPLAIST'    '1000',
    ' ' 'BDC_OKCODE'  '=BUCH',
    ' ' 'BDC_CURSOR' 'ANLA-VMGLI',
    ' ' 'ANLA-VMGLI'  it_9000-vmgli,
    ' ' 'ANLA-EIGKZ'  it_9000-eigkz.

ENDFORM.                    " GENERATE_BDC
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1212   text
*      -->P_1213   text
*      -->P_1214   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-program,
          value TO it_bdc-dynpro,
          dynbegin TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE.
    CLEAR it_bdc.
    MOVE: name TO it_bdc-fnam,
          value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9001_MSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
    EXPORTING
      id      = sy-msgid
      mtype   = sy-msgty
      number  = sy-msgno
      par1    = sy-msgv1
      par2    = sy-msgv2
      par3    = sy-msgv3
      par4    = sy-msgv4
    IMPORTING
      msg_lin = lw_msg.

  MOVE: lw_msg TO pw_msg.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  posting_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_rtn USING p_index.
  DATA: l_msg   LIKE it_9000-zmsg.

  CLEAR: l_msg.

  CALL TRANSACTION 'AS02'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0.
    PERFORM get_err_msg USING l_msg.
    MOVE: l_msg TO it_9000-zmsg.

    MODIFY it_9000 INDEX p_index.   CLEAR it_9000.
  ELSE.
    MOVE: 'Successfully changed'  TO it_9000-zmsg.
    MODIFY it_9000 INDEX p_index.  CLEAR it_9000.
  ENDIF.
ENDFORM.                    " posting_rtn
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0040   text
*----------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename.

  DATA: tmp_filename LIKE rlgrap-filename.
*  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
*  DATA: fieldln TYPE i.
*  FIELD-SYMBOLS: <tmp_sym>.

*  fieldln = strlen( def_path ) - 1.
*  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
*  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
*    CLEAR <tmp_sym>.
*  ENDIF.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = sy-cprog
      dynpro_number = sy-dynnr
      field_name    = ' '
    IMPORTING
      file_name     = tmp_filename.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
    MESSAGE e000 WITH 'FILE SELECT WINDOW OPEN ERROR!'.
  ENDIF.

ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_screen.
 sscrfields-functxt_01 = text-003.
 if sy-uname = 'HIS20037'.
    sscrfields-functxt_02 = text-004.
  ENDIF.
  LOOP AT SCREEN.
    IF screen-name = 'P_BDC'.
      IF sy-uname+0(3) = 'HIS'.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SET_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_template .
  DATA: lw_key LIKE wwwdatatab.

  lw_key-relid = 'MI'.
  lw_key-objid = 'ZRFI_ASSET_MASS_CHANGE'.
  lw_key-tdate = sy-datum.
  lw_key-ttime = sy-uzeit.
  lw_key-text = 'Mass Change of Asset'.
  lw_key-devclass = 'ZDFI'.

  CALL FUNCTION 'UPLOAD_WEB_OBJECT'
    EXPORTING
      key           = lw_key
*     TEMP          = ' '
*     TEXT          =
*   IMPORTING
*     RC            =
  .
ENDFORM.                    " SET_TEMPLATE
*&---------------------------------------------------------------------*
*&      Form  GET_TEMPLATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_template .
  DATA: lt_doc_table      LIKE w3mime OCCURS 0 WITH HEADER LINE,
        lv_doc_size       TYPE i,
        lv_doc_type(80)   VALUE soi_doctype_excel97_sheet,
        lv_doc_format(80) TYPE c.

  DATA: lv_w3mime         TYPE w3mime,
        lv_xstring        TYPE xstring.

  DATA: lv_binary_tab     TYPE w3mimetabtype,
        lv_bin_size       TYPE i,
        lv_filename       TYPE string.


  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      object_id        = sy-cprog
    IMPORTING
      data_size        = lv_doc_size
      document_format  = lv_doc_format
      document_type    = lv_doc_type
    TABLES
      data_table       = lt_doc_table
    EXCEPTIONS
      object_not_found = 1
      internal_error   = 2
      OTHERS           = 3.

  CALL METHOD cl_gui_cfw=>flush .
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lv_doc_size NE 0.

    LOOP AT lt_doc_table INTO lv_w3mime.
      CONCATENATE lv_xstring lv_w3mime-line
       INTO lv_xstring IN BYTE MODE.
    ENDLOOP.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xstring
      IMPORTING
        output_length = lv_bin_size
      TABLES
        binary_tab    = lv_binary_tab.

    CALL FUNCTION 'WS_FILENAME_GET'
      EXPORTING
        def_filename     = sy-cprog
        def_path         = 'C:\'
        mask             = ',*.xls,*.xls;,*.*,*.*.'
        mode             = 'S'
        title            = 'Please select a file'
      IMPORTING
        filename         = lv_filename
      EXCEPTIONS
        inv_winsys       = 1
        no_batch         = 2
        selection_cancel = 3
        selection_error  = 4
        OTHERS           = 5.

    IF sy-subrc = 0.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize = lv_bin_size
          filename     = lv_filename
          filetype     = 'BIN'
        TABLES
*          data_tab     = lt_doc_table[].
          data_tab     = lv_binary_tab.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_TEMPLATE
