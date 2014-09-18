*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPR_DELAY_VEH_SIDE_TRACK
*& Program Name   : Side Track Management
*& Created by     :
*& Created on     :
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&
*&======================================================================


REPORT zppr_delay_veh_side_track NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.
INCLUDE: <icon>, ole2incl.
INCLUDE officeintegrationinclude.

TABLES: cawn, ztpp_side_track, zspp_side_track, ztpp_delay_sect.
TYPE-POOLS vrm.

DATA:  v_datum LIKE sy-datum,
       v_model LIKE ztpp_vm-model_code,
       v_model_con TYPE string,
       v_status LIKE ztpp_side_track-st_status,
       v_status_con TYPE string,
       v_zsecid LIKE ztpp_delay_sect-zsecid,
       v_zsecid_200 LIKE ztpp_delay_sect-zsecid.

DATA:  p_body LIKE ztsd_um-body_no.

RANGES: r_body  FOR ztsd_um-body_no,
        r_datum FOR sy-datum,
        r_rp    FOR ztpp_side_track-rp_cstatus,
        r_rp_b28    FOR ztpp_side_track-rp_cstatus.

DATA: g_body_ext(4)  VALUE icon_enter_more,
      g_datum_ext(4) VALUE icon_enter_more.

DATA: BEGIN OF it_0100 OCCURS 0.
        INCLUDE STRUCTURE ztpp_side_track.
DATA: progress   LIKE ztpp_status-progress,
      rp21_sdate LIKE sy-datum,
      section_txt(20),
      st_status_txt(6).
DATA: END OF it_0100.

DATA : wa_sect_b28   TYPE ztpp_delay_sect,
       it_sect     LIKE ztpp_delay_sect OCCURS 0 WITH HEADER LINE,
       it_sect_b28 LIKE ztpp_delay_sect OCCURS 0 WITH HEADER LINE.

DATA: it_vmaster  LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

DATA: w_okcode LIKE sy-ucomm.

DATA: it_list  TYPE vrm_values,
      st_value LIKE LINE OF it_list.

*DATA: w_save(1).

DATA: excelsheet      TYPE ole2_object,
      excel           TYPE ole2_object,
      workbooks       TYPE ole2_object,
      open_workbooks  TYPE ole2_object,
      close_workbooks TYPE ole2_object,
      sheet           TYPE ole2_object,
      cells           TYPE ole2_object,
      subrc           LIKE sy-subrc,
      exl_activate    TYPE ole2_object,
      exl_print       TYPE ole2_object,
      exl_preview     TYPE ole2_object,
      exl_quit        TYPE ole2_object,
      exl_saveas      TYPE ole2_object,
      e_work          TYPE ole2_object.

DATA: tmp_file LIKE rlgrap-filename,
     filename(50) VALUE 'C:\SapXls\Side_Track_Notice_Form.xls'.

DATA: if_factory     TYPE REF TO i_oi_document_factory,
      if_document    TYPE REF TO i_oi_document_proxy,
      if_link_server TYPE REF TO i_oi_link_server.


DATA: g_objid(40) TYPE c VALUE 'ZPP_SIDE_TRACK_NOTICE',
      g_filepath TYPE string VALUE 'C:\SAPXLS\',
      g_filename(50) TYPE c VALUE 'Side_Track_Notice_Form.xls'.

DATA: it_file LIKE w3mime OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_0100   TYPE        scrfname VALUE 'CC_0100_ALV',
      wc_alv_0100       TYPE REF TO cl_gui_alv_grid,
      wc_container_0100 TYPE REF TO cl_gui_custom_container.

* Interal tables for ALV GRID
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_filter       TYPE lvc_t_filt WITH HEADER LINE,
       it_rows         TYPE lvc_t_row  WITH HEADER LINE,
       it_row_no       TYPE lvc_t_roid WITH HEADER LINE.

* Global variable for ALV GRID
DATA : v_layout      TYPE lvc_s_layo,
       v_variant     TYPE disvariant,      "for parameter IS_VARIANT
       v_fieldname   LIKE LINE OF it_fieldname,
       v_repid       LIKE sy-repid,
       v_cnt         TYPE i,                   "Field count
       v_save        TYPE c   VALUE 'A'.       "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: v_container(100),
      v_control(100),
      v_alv(100),
      v_itab(100),
      v_structure LIKE dd02l-tabname.

FIELD-SYMBOLS: <container> TYPE REF TO   cl_gui_custom_container,
               <control>   TYPE          scrfname,
               <alv>       TYPE REF TO   cl_gui_alv_grid,
               <itab>      TYPE STANDARD TABLE.
*               <ITAB_OLD>  TYPE STANDARD TABLE.

CONSTANTS: c_structure(100) VALUE 'IT_'.

*-----/// ALV Control : END

DATA : g_okcode TYPE sy-ucomm.

DATA : g_type TYPE c,
       g_msg  TYPE bapireturn-message.
*
INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  DATA : extab  TYPE TABLE OF sy-ucomm.

  CLEAR : extab[], extab.

*-Authorization Check
  AUTHORITY-CHECK OBJECT 'Z:PP_PCALL'
           ID 'ACTVT' FIELD '01'.
  IF sy-subrc <> 0.
    APPEND 'CHANGE' TO extab.
    APPEND 'DELETE' TO extab.
    APPEND 'UPLOAD' TO extab.
  ENDIF.

  SET PF-STATUS '100' EXCLUDING extab.
  SET TITLEBAR  '100'.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  PERFORM create_alv_object USING sy-dynnr.
ENDMODULE.                 " CREATE_ALV_OBJECT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_ALV_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_alv_object  USING    p_dynnr.
  DATA : ls_stbl TYPE lvc_s_stbl .

  CONCATENATE: 'WC_CONTAINER_' p_dynnr INTO v_container.
  ASSIGN:      (v_container)           TO   <container>.
  CONCATENATE: 'WC_ALV_'       p_dynnr INTO v_alv.
  ASSIGN:      (v_alv)                 TO   <alv>.
  CONCATENATE: 'IT_'      p_dynnr '[]' INTO v_itab.
  ASSIGN:      (v_itab)                TO   <itab>.

  IF <container> IS INITIAL.          "/Not Created Control for ALV GRID
    PERFORM create_container_n_object USING p_dynnr.
    PERFORM set_attribute             USING p_dynnr.
    PERFORM build_field_catalog       USING p_dynnr.
    PERFORM assign_itab_to_alv        USING p_dynnr.
  ELSE.
    PERFORM build_field_catalog       USING p_dynnr.

    CALL METHOD <alv>->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat[].

    ls_stbl-row = 'X'.
    ls_stbl-col = 'X'.

    CALL METHOD <alv>->refresh_table_display
      EXPORTING
        is_stable = ls_stbl.

    CALL METHOD cl_gui_cfw=>flush.
  ENDIF.
ENDFORM.                    " CREATE_ALV_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
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
ENDFORM.                    "create_container_n_object

*----------------------------------------------------------------------*
FORM build_field_catalog USING p_dynnr.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  CALL METHOD <alv>->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcat[].

  PERFORM set_fieldname     USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
ENDFORM.                    "build_field_catalog
*----------------------------------------------------------------------*
FORM set_fieldname USING p_dynnr.

  DATA: lw_itab TYPE slis_tabname.
  DATA: l_datum(08).

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].



  sy-datum = sy-datum + 1.
  MOVE sy-datum TO l_datum.
  SET PARAMETER ID 'ALVBUFFER' FIELD l_datum.

  MOVE: sy-repid TO v_repid.
  CONCATENATE c_structure p_dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = v_repid
      i_internal_tabname = lw_itab
      i_inclname         = v_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

ENDFORM.                    "set_fieldname
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.

  CASE p_dynnr.
    WHEN '0100'.
      PERFORM set_screen_fields_0100.
    WHEN '0110'.

  ENDCASE.

ENDFORM.                    "set_screen_fields
*----------------------------------------------------------------------*
FORM set_screen_fields_0100.
  DATA: lv_adate     TYPE string,
        lv_adate_txt TYPE string.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                               'S' 'MODEL_CODE'   ' ',
                               ' ' 'COLTEXT'      'Model',
                               ' ' 'OUTPUTLEN'    '5',
                               ' ' 'FIX_COLUMN'   'X',
*                               ' ' 'HOTSPOT'      'X',
                               'E' 'EMPHASIZE'    'C100',

                               'S' 'BODY_NO'      ' ',
                               ' ' 'COLTEXT'      'Body No',
                               ' ' 'OUTPUTLEN'    '7',
                               ' ' 'FIX_COLUMN'   'X',
*                               ' ' 'HOTSPOT'      'X',
                               'E' 'EMPHASIZE'    'C100',


                               'S' 'EXTC'         ' ',
                               ' ' 'COLTEXT'      'EXT',
                               ' ' 'OUTPUTLEN'    '3',
                               ' ' 'FIX_COLUMN'   'X',
                               'E' 'EMPHASIZE'    'C100',

                               'S' 'INTC'         ' ',
                               ' ' 'COLTEXT'      'INT',
                               ' ' 'OUTPUTLEN'    '3',
                               ' ' 'FIX_COLUMN'   'X',
                               'E' 'EMPHASIZE'    'C100',

                               'S' 'SECTION_TXT'   ' ',
                               'E' 'COLTEXT'      'Section',

                               'S' 'RP_CSTATUS'       ' ',
                               ' ' 'COLTEXT'      'Current RP',
                               'E' 'OUTPUTLEN'    '5',

                               'S' 'PROGRESS'       ' ',
                               ' ' 'COLTEXT'      'RP Name',
                               'E' 'OUTPUTLEN'    '5',

                               'S' 'ZRETURN_DATE'       ' ',
                               ' ' 'COLTEXT'      'Return Date',
                               ' ' 'OUTPUTLEN'    '10',
                               'E' 'DATATYPE'     'DATS',

                              'S' 'ZRETURN_TIME'       ' ',
                               ' ' 'COLTEXT'      'Return Time',
                               ' ' 'OUTPUTLEN'    '8',
                               'E' 'DATATYPE'     'TIMS',

                               'S' 'ZETA'     ' ',
                               ' ' 'COLTEXT'      'ETA',
                               ' ' 'OUTPUTLEN'    '10',
                               'E' 'DATATYPE'     'DATS',

                               'S' 'ST_STATUS_TXT' ' ',
                               ' ' 'COLTEXT'      'Status',
                               'E' 'OUTPUTLEN'    '6',

                               'S' 'RP06_SERIAL'        ' ',
                               ' ' 'COLTEXT'      'RP06_SERIAL',
                               'E' 'OUTPUTLEN'    '8',

                               'S' 'ZLOCAT'        ' ',
                               ' ' 'COLTEXT'      'Where to',
                               'E' 'OUTPUTLEN'    '10',

                               'S' 'ZSTTXT'   ' ',
                               ' ' 'COLTEXT'      'Purpose',
                               'E' 'OUTPUTLEN'    '20',

                               'S' 'ZDEPT'       ' ',
                               ' ' 'COLTEXT'      'Dept',
                               'E' 'OUTPUTLEN'    '20',

                               'S' 'ZREQUSTOR'       ' ',
                               ' ' 'COLTEXT'      'Requested by',
                               'E' 'OUTPUTLEN'    '10',

                              'S' 'ZPHONE'       ' ',
                               ' ' 'COLTEXT'      'Phone',
                               'E' 'OUTPUTLEN'    '12',

                               'S' 'ERDAT'       ' ',
                               ' ' 'COLTEXT'      'Requested on',
                               'E' 'OUTPUTLEN'    '10',

                               'S' 'WORDER'   ' ',
                               ' ' 'COLTEXT'      'W/Order',
                               'E' 'OUTPUTLEN'    '14',

*                               'S' 'EXTC'           ' ',
*                               ' ' 'COLTEXT'      'Ext',
*                               'E' 'OUTPUTLEN'    '3',
*
*                               'S' 'INTC'          ' ',
*                               ' ' 'COLTEXT'      'Int',
*                               'E' 'OUTPUTLEN'    '3',

                               'S' 'URGENCY'     ' ',
                               ' ' 'COLTEXT'      'Urgency',
                               'E' 'OUTPUTLEN'    '2',

                               'S' 'URGCDATE'      ' ',
                               ' ' 'COLTEXT'      'Urgency Date',
                               ' ' 'OUTPUTLEN'    '10',
                               'E' 'DATATYPE'     'DATS',

                               'S' 'DEALER_NO'    ' ',
                               ' ' 'COLTEXT'      'Dealer No',
                               'E' 'OUTPUTLEN'    '9',

                               'S' 'FLET'        ' ',
                               ' ' 'COLTEXT'      'Fleet',
                               'E' 'OUTPUTLEN'    '5'.

ENDFORM.                    "set_screen_fields_0101
*----------------------------------------------------------------------*
FORM assign_itab_to_alv USING p_dynnr.
  CONCATENATE: c_structure  p_dynnr      INTO v_structure.

  CALL METHOD <alv>->set_table_for_first_display
    EXPORTING
*     i_structure_name = v_structure
      is_layout        = v_layout
      i_save           = v_save
      is_variant       = v_variant
      i_default        = space
    CHANGING
      it_fieldcatalog  = it_fieldcat[]
      it_filter        = it_filter[]
      it_sort          = it_sort[]
      it_outtab        = <itab>.
ENDFORM.                    "assign_itab_to_alv
*----------------------------------------------------------------------*
*FORM sssign_event USING p_dynnr.
*
*  DATA: lv_dynnr   LIKE   sy-dynnr.
*
*  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
*  ASSIGN: (v_alv)       TO <alv>.
*
**--  Regist event for Edit
*  CALL METHOD <alv>->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*  CREATE OBJECT event_receiver.
*  IF p_dynnr EQ '0100'.
*    SET HANDLER event_receiver->handle_double_click  FOR <alv>.
*    SET HANDLER event_receiver->handle_hotspot_click_0100  FOR <alv>.
*  ENDIF.
*
*  IF p_dynnr EQ '0110'.
*    SET HANDLER event_receiver->handle_hotspot_click  FOR <alv>.
*    SET HANDLER event_receiver->handle_toolbar        FOR <alv>.
*  ENDIF.
*ENDFORM.                    " sssign_event
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
      MESSAGE e000(zz) WITH 'Check filed catalog:' p_field.
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

ENDFORM.                    "setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_attribute USING p_dynnr.
  PERFORM set_layout                USING p_dynnr.
  PERFORM set_variant               USING p_dynnr.
  PERFORM set_sort_total_field      USING p_dynnr.
  PERFORM set_filter                USING p_dynnr.
ENDFORM.                    " SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_layout USING p_dynnr.
*  CALL METHOD <alv>->get_frontend_layout
*    IMPORTING
*      es_layout = v_layout.
*  IF v_layout IS INITIAL.
*  ENDIF.
  CASE p_dynnr.
    WHEN '0100' OR '0200'.
      CLEAR: v_layout.

      v_layout-edit       = ' '.         " Edit Mode Enable
      v_layout-sel_mode   = 'A'.           " mode for select col and row
      v_layout-language   = sy-langu.    " Language Key
*  v_layout-totals_bef = 'X'.         " Upper Total Line
*  v_layout-no_totline = 'X'.         " Disable Total Line
      v_layout-cwidth_opt = 'X'.         " optimizes the column width
*  v_layout-no_merging = 'X'.         " Disable cell merging
      v_layout-zebra      = 'X'.         " Emphasize C250
      v_layout-info_fname = 'ROW_COLOR'. " Line color field
    WHEN '0110'.
      CLEAR: v_layout.

      v_layout-edit       = ' '.         " Edit Mode Enable
      v_layout-sel_mode   = 'A'.           " mode for select col and row
      v_layout-language   = sy-langu.    " Language Key
*  v_layout-totals_bef = 'X'.         " Upper Total Line
*  v_layout-no_totline = 'X'.         " Disable Total Line
*      v_layout-cwidth_opt = 'X'.         " optimizes the column width
*  v_layout-no_merging = 'X'.         " Disable cell merging
      v_layout-zebra      = 'X'.         " Emphasize C250
      v_layout-info_fname = 'ROW_COLOR'. " Line color field
  ENDCASE.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_variant USING p_dynnr.
*  CALL METHOD <alv>->get_variant
*    IMPORTING
*      es_variant = v_variant.
*  IF v_layout IS INITIAL.
*  ENDIF.

  v_variant-report      = sy-repid.
  v_variant-handle      = space.
  v_variant-log_group   = space.
  v_variant-username    = space.
  v_variant-variant     = space.
  v_variant-text        = space.
  v_variant-dependvars  = space.
ENDFORM.                    " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_sort_total_field USING p_dynnr.

ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_FILTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_filter  USING    p_dynnr.
*  CALL METHOD <alv>->get_filter_criteria
*    IMPORTING
*      et_filter = it_filter[].
*
*  IF it_filter[] IS INITIAL.
*    CASE p_dynnr.
*      WHEN '0100'.
*        REFRESH it_filter.
*
*        " N/A
*      WHEN '0100'.
*        " N/A
*    ENDCASE.
*  ENDIF.
ENDFORM.                    " SET_FILTER
*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_0100 USING p_column_name             "Column Name
                          ps_row_no  LIKE sy-tabix. "Numeric Row ID
  DATA : lw_sel_index LIKE sy-tabix.

  MOVE: ps_row_no TO lw_sel_index.

  READ TABLE it_0100 INDEX lw_sel_index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CASE p_column_name.
    WHEN 'BODY_NO' OR 'MODEL_CODE'.
      SET PARAMETER ID 'ZMODEL' FIELD it_0100-model_code.
      SET PARAMETER ID 'ZBODY'  FIELD it_0100-body_no.

      CALL TRANSACTION 'ZPPR01500T'.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " DBL_CLICK_0100

*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM refresh_alv USING pv_dynnr.
  DATA : ls_stbl TYPE lvc_s_stbl .

  CONCATENATE: 'WC_ALV_'       pv_dynnr INTO v_alv.
  ASSIGN:      (v_alv)                 TO   <alv>.

  CHECK <alv> IS NOT INITIAL.

  ls_stbl-row = 'X'.
  ls_stbl-col = 'X'.

  PERFORM build_field_catalog       USING pv_dynnr.

  CALL METHOD <alv>->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcat[].

  CALL METHOD <alv>->refresh_table_display
    EXPORTING
      is_stable = ls_stbl.

  CALL METHOD cl_gui_cfw=>flush.

ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm, w_okcode.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      CLEAR: sy-ucomm, w_okcode.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      CLEAR: sy-ucomm, w_okcode.
      PERFORM inquiry_rtn.
      PERFORM refresh_alv USING '0100'.
    WHEN 'CREATE'.
      w_okcode = sy-ucomm.
      CLEAR: sy-ucomm.
      PERFORM create_rtn.
      CLEAR: w_okcode.
    WHEN 'CHANGE'.
      w_okcode = sy-ucomm.
      CLEAR: sy-ucomm.
      PERFORM modify_rtn.
      CLEAR: w_okcode.
    WHEN 'PRINT'.
      w_okcode = sy-ucomm.
      CLEAR: sy-ucomm.
      PERFORM print_rtn.
      CLEAR: w_okcode.
    WHEN 'DELETE'.
      w_okcode = sy-ucomm.
      CLEAR: sy-ucomm.
      PERFORM delete_rtn.
      CLEAR: w_okcode.
    WHEN 'BODY_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'BODY' 'ZTSD_UM-BODY_NO'.
    WHEN 'DATUM_EXT'.
      CLEAR: sy-ucomm.
      PERFORM click_extension USING 'DATUM' 'SY-DATUM'.
    WHEN 'UPLOAD'.
      w_okcode = sy-ucomm.
      CLEAR: sy-ucomm.
      PERFORM upload_rtn.
      CLEAR: w_okcode.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_LISTBOX_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_listbox_0100 OUTPUT.
  PERFORM set_model_0100.
  PERFORM set_section.
  PERFORM set_status.
ENDMODULE.                 " SET_LISTBOX_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_MODEL_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_model_0100 .
  REFRESH: it_list.

  SELECT atwrt AS key  atwtb AS text
    INTO TABLE it_list
    FROM cabn AS a
    INNER JOIN cawn AS b
    ON a~atinn = b~atinn
    INNER JOIN cawnt AS c
    ON b~atinn = c~atinn
    AND b~atzhl = c~atzhl
    WHERE atnam = 'P_MODEL'
      AND c~spras = 'EN'.

  st_value-key =   '*'.
  st_value-text =   ''.
  APPEND st_value TO it_list.

  SORT it_list BY key.
  DELETE ADJACENT DUPLICATES FROM it_list COMPARING key.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'V_MODEL'
      values = it_list.

ENDFORM.                    " SET_MODEL_0100
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
  MOVE: 'I' TO r_datum-sign,
        'BT' TO r_datum-option.

  CONCATENATE sy-datum(6) '01' INTO r_datum-low.
  MOVE: sy-datum TO r_datum-high.

  v_model =  '*'.
  v_status = 'I'.

  SELECT * INTO TABLE it_sect
  FROM ztpp_delay_sect.
ENDFORM.                    " INITIALIZATION


*&---------------------------------------------------------------------*
*&      Form  MODIFY_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_rtn .

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CLEAR: it_rows, it_rows[], it_row_no, it_row_no[].
  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = it_rows[]
      et_row_no     = it_row_no[].

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE it_rows INDEX 1.

  READ TABLE it_0100 INDEX it_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.
  MOVE-CORRESPONDING it_0100 TO zspp_side_track.

  CALL SCREEN 0200 STARTING AT 20  5
                   ENDING   AT 105 10.

  MODIFY it_0100 INDEX it_rows-index.

ENDFORM.                    " MODIFY_RTN


*&---------------------------------------------------------------------*
*&      Module  SET_SCREEN_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen_attr OUTPUT.

ENDMODULE.                 " SET_SCREEN_ATTR  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  INQUIRY_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM inquiry_rtn.
  DATA: l_object LIKE mara-matnr,
        l_index LIKE sy-tabix,
        l_model LIKE ztsd_um-model_code,
        l_body  LIKE ztsd_um-body_no.

  DATA: lt_temp LIKE TABLE OF ztpp_side_track WITH HEADER LINE.

  REFRESH: it_0100.
  CLEAR: it_0100, v_model_con, v_status_con, r_rp[], r_rp,
         wa_sect_b28, r_rp_b28[], r_rp_b28.

  IF v_model EQ '*'.
    CLEAR: v_model_con.
  ELSE.
    CONCATENATE 'A~MODEL_CODE = ''' v_model '''' INTO v_model_con.
  ENDIF.

  IF v_status IS NOT INITIAL.
    CONCATENATE 'A~ST_STATUS = ' '`' v_status '`' INTO v_status_con.
  ENDIF.

  SELECT model_code body_no zseq zrequstor zdept zphone zlocat
         zreturn_date zreturn_time zsttxt a~rp_cstatus a~tp_cstatus
         rp06_serial worder extc intc urgency urgcdate dealer_no
         alloc_date flet st_status
         a~ernam a~erdat a~erzet a~aenam a~aedat a~aezet
         b~progress a~st_status a~zeta
    INTO CORRESPONDING FIELDS OF TABLE it_0100
    FROM ztpp_side_track AS a LEFT OUTER JOIN ztpp_status AS b
                                ON b~id = a~tp_cstatus
    WHERE (v_model_con)
      AND (v_status_con)
      AND a~body_no    IN r_body
      AND a~erdat      IN r_datum.

*-Delete data with RP_CSTATUS
  IF v_zsecid IS NOT INITIAL.
    IF v_zsecid = 'VP'.
      READ TABLE it_sect WITH KEY  zsecid    = v_zsecid  "Not B28
                                   wo_nation = 'B28'
                                   zoper     = '<>'.
      IF sy-subrc = 0 AND it_sect-zrpfr IS NOT INITIAL
                      AND it_sect-zrpto IS NOT INITIAL.
        r_rp-sign = 'I'.
        r_rp-option = 'BT'.
        r_rp-low    = it_sect-zrpfr.
        r_rp-high   = it_sect-zrpto.
        APPEND r_rp.
      ELSE.
        MESSAGE e000 WITH 'Delay Section Master Not exist: ' v_zsecid.
      ENDIF.

      READ TABLE it_sect INTO wa_sect_b28      "B28
                            WITH KEY  zsecid    = v_zsecid
                                      wo_nation = 'B28'
                                      zoper     = '='.
      IF wa_sect_b28-zrpfr IS NOT INITIAL
              AND  wa_sect_b28-zrpto IS NOT INITIAL.
        r_rp_b28-sign = 'I'.
        r_rp_b28-option = 'BT'.
        r_rp_b28-low    = wa_sect_b28-zrpfr.
        r_rp_b28-high   = wa_sect_b28-zrpto.
        APPEND r_rp_b28.
      ELSE.
        MESSAGE e000 WITH 'Delay Section Master Not exist: ' v_zsecid.
      ENDIF.

      LOOP AT it_0100.
        IF it_0100-worder+9(3) = 'B28'.
          IF it_0100-rp_cstatus NOT IN r_rp_b28.
            DELETE it_0100.
          ENDIF.
        ELSE.
          IF it_0100-rp_cstatus NOT IN r_rp.
            DELETE it_0100.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.
      READ TABLE it_sect WITH KEY  zsecid    = v_zsecid.
      IF sy-subrc = 0 AND it_sect-zrpfr IS NOT INITIAL
                      AND it_sect-zrpto IS NOT INITIAL.
        r_rp-sign = 'I'.
        r_rp-option = 'BT'.
        r_rp-low    = it_sect-zrpfr.
        r_rp-high   = it_sect-zrpto.
        APPEND r_rp.

        DELETE it_0100 WHERE rp_cstatus NOT IN r_rp.
      ELSE.
        MESSAGE e000 WITH 'Delay Section Master Not exist: '  v_zsecid.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT it_0100.      "Input Text

    IF it_0100-st_status  = 'I'.
      it_0100-st_status_txt = 'Open'.
    ELSEIF it_0100-st_status  = 'F'.
      it_0100-st_status_txt = 'Closed'.
    ENDIF.

    LOOP AT it_sect.
      IF it_0100-worder+9(3) = 'B28'.
        IF  it_sect-zoper = '<>'.
          CONTINUE.
        ENDIF.
      ELSE.
        IF  it_sect-zoper = '='.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF it_0100-rp_cstatus >= it_sect-zrpfr AND
         it_0100-rp_cstatus <= it_sect-zrpto.
        it_0100-section_txt = it_sect-zsectx.
        EXIT.
      ENDIF.
    ENDLOOP.

    MODIFY it_0100 TRANSPORTING section_txt st_status_txt.
  ENDLOOP.

  SORT it_0100 BY erdat DESCENDING erzet DESCENDING model_code body_no.

  IF it_0100[] IS INITIAL.
    MESSAGE s000 WITH text-m02 DISPLAY LIKE 'S'.
  ENDIF.
ENDFORM.                    " INQUIRY_RTN
*&---------------------------------------------------------------------*
*&      Form  CREATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_rtn.
  CLEAR: zspp_side_track, it_0100.
  zspp_side_track-zreturn_date  = sy-datum + 1.
  zspp_side_track-zreturn_time  = sy-uzeit.
  zspp_side_track-st_status     = 'I'.

** oN 09/12/13
  zspp_side_track-zeta = zspp_side_track-zreturn_date.
** End addition

  CALL SCREEN 0200 STARTING AT 20  5
                   ENDING   AT 105 10.
ENDFORM.                    " CREATE_RTN
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  DATA : l_ucomm LIKE sy-ucomm.
  l_ucomm = g_okcode. CLEAR g_okcode.

  CASE l_ucomm.
    WHEN 'PRINT'.
      PERFORM print_side_track_note.
*      CLEAR: w_save.
*      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM save_0200_rtn.
*      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR: g_okcode.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_0200_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_0200_rtn .
  PERFORM save_data.

  LEAVE TO SCREEN 0.
ENDFORM.                    " SAVE_0200_RTN
*&---------------------------------------------------------------------*
*&      Module  SET_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_field OUTPUT.
  PERFORM set_data.
  PERFORM calculate_section CHANGING  v_zsecid_200.
*  PERFORM list_box_body_no.
ENDMODULE.                 " SET_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_data .

  IF w_okcode NE 'CREATE'.
    LOOP AT SCREEN.
      IF screen-name = 'ZSPP_SIDE_TRACK-MODEL_CODE' OR
         screen-name = 'ZSPP_SIDE_TRACK-BODY_NO'    OR
         screen-name = 'ZSPP_SIDE_TRACK-RP_CSTATUS' OR
         screen-name = 'ZSPP_SIDE_TRACK-ERDAT'      OR
         screen-name = 'ZSPP_SIDE_TRACK-RP06_SERIAL' OR
         screen-name = 'V_ZSECID_200'.
        screen-input = 0.
      ELSE.
        screen-input = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'ZSPP_SIDE_TRACK-ST_STATUS'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SET_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '200'.
  SET TITLEBAR  '100'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data .
  IF w_okcode = 'CREATE'.
    SELECT MAX( zseq ) INTO zspp_side_track-zseq
      FROM ztpp_side_track.

    zspp_side_track-zseq = zspp_side_track-zseq + 1.

    zspp_side_track-erdat = sy-datum.
    zspp_side_track-erzet = sy-uzeit.
    zspp_side_track-aedat = sy-datum.
    zspp_side_track-aezet = sy-uzeit.

    CLEAR: ztpp_side_track.
    MOVE-CORRESPONDING zspp_side_track TO ztpp_side_track.

    INSERT ztpp_side_track.
    IF sy-subrc = 0.
*      w_save = 'X'.
      COMMIT WORK.

      CLEAR: it_0100.
      MOVE-CORRESPONDING zspp_side_track TO it_0100.

      IF it_0100-st_status  = 'I'.
        it_0100-st_status_txt = 'Open'.
      ELSEIF it_0100-st_status  = 'F'.
        it_0100-st_status_txt = 'Closed'.
      ENDIF.

      LOOP AT it_sect.
        IF it_0100-worder+9(3) = 'B28'.
          IF  it_sect-zoper = '<>'.
            CONTINUE.
          ENDIF.
        ELSE.
          IF  it_sect-zoper = '='.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF it_0100-rp_cstatus >= it_sect-zrpfr AND
           it_0100-rp_cstatus <= it_sect-zrpto.
          it_0100-section_txt = it_sect-zsectx.
          EXIT.
        ENDIF.
      ENDLOOP.

      SELECT SINGLE progress INTO zspp_side_track-progress
        FROM ztpp_status
       WHERE id = zspp_side_track-tp_cstatus.

      APPEND it_0100.

      MESSAGE s000(zz) WITH 'Successfully Created'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000(zz) WITH 'Error in Creation'.
    ENDIF.
  ELSE.
    zspp_side_track-aedat = sy-datum.
    zspp_side_track-aezet = sy-uzeit.

    CLEAR: ztpp_side_track.
    MOVE-CORRESPONDING zspp_side_track TO ztpp_side_track.

    UPDATE ztpp_side_track.
    IF sy-subrc = 0.
*      w_save = 'X'.
      COMMIT WORK.

      CLEAR: it_0100.
      MOVE-CORRESPONDING zspp_side_track TO it_0100.

      IF it_0100-st_status  = 'I'.
        it_0100-st_status_txt = 'Open'.
      ELSEIF it_0100-st_status  = 'F'.
        it_0100-st_status_txt = 'Closed'.
      ENDIF.

      LOOP AT it_sect.
        IF it_0100-worder+9(3) = 'B28'.
          IF  it_sect-zoper = '<>'.
            CONTINUE.
          ENDIF.
        ELSE.
          IF  it_sect-zoper = '='.
            CONTINUE.
          ENDIF.
        ENDIF.

        IF it_0100-rp_cstatus >= it_sect-zrpfr AND
           it_0100-rp_cstatus <= it_sect-zrpto.
          it_0100-section_txt = it_sect-zsectx.
          EXIT.
        ENDIF.
      ENDLOOP.

      SELECT SINGLE progress INTO zspp_side_track-progress
        FROM ztpp_status
       WHERE id = zspp_side_track-tp_cstatus.

      MESSAGE s000(zz) WITH 'Successfully Modified'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000(zz) WITH 'Error in Modification'.
    ENDIF.
  ENDIF.

  SORT it_0100 BY erdat DESCENDING erzet DESCENDING model_code body_no.
ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_VM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_OBJEK  text
*----------------------------------------------------------------------*
FORM get_vm  USING p_object.
  DATA: l_object LIKE mara-matnr.

  l_object = p_object.
  CLEAR: it_vmaster, it_vmaster[].
  it_vmaster-atnam = 'P_RP06_SERIAL'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_DEALER_NO'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_RP21_SHOP_DATE'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_WORK_ORDER'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_EXT_COLOR'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_INT_COLOR'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_RP_STATUS'.
  APPEND it_vmaster.

  it_vmaster-atnam = 'P_STATUS'.
  APPEND it_vmaster.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object     = l_object
      mode       = 'R'
*     cmode      = '002'
    TABLES
      val_table  = it_vmaster
    EXCEPTIONS
      no_data    = 1
      error_mode = 2
      OTHERS     = 3.

  LOOP AT it_vmaster.
    CASE it_vmaster-atnam.
      WHEN 'P_RP06_SERIAL'.
        zspp_side_track-rp06_serial = it_vmaster-atwrt.
      WHEN 'P_DEALER_NO'.
        zspp_side_track-dealer_no = it_vmaster-atwrt.
      WHEN 'P_RP21_SHOP_DATE'.
        zspp_side_track-rp21_sdate = it_vmaster-atwrt.
      WHEN 'P_WORK_ORDER'.
        zspp_side_track-worder = it_vmaster-atwrt.
      WHEN 'P_EXT_COLOR'.
        zspp_side_track-extc = it_vmaster-atwrt.
      WHEN 'P_INT_COLOR'.
        zspp_side_track-intc = it_vmaster-atwrt.
      WHEN 'P_RP_STATUS'.
        zspp_side_track-rp_cstatus = it_vmaster-atwrt.
      WHEN 'P_STATUS'.
        zspp_side_track-tp_cstatus = it_vmaster-atwrt.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " GET_VM
*&---------------------------------------------------------------------*
*&      Module  DISPALY_BODY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE dispaly_body INPUT.
  DATA : BEGIN OF value_tab1 OCCURS 0,
         body_no LIKE ztpp_vm-body_no,
         END OF value_tab1.
  DATA : BEGIN OF scr_fields OCCURS 10.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF scr_fields.

  DATA: return_tab LIKE TABLE OF ddshretval WITH HEADER LINE.
  DATA: w_repid LIKE sy-repid,
        w_dynnr LIKE sy-dynnr.

  DATA : BEGIN OF l_dynfieldtab OCCURS 10.
          INCLUDE STRUCTURE dynpread.
  DATA : END OF l_dynfieldtab.

  w_repid = sy-repid.
  w_dynnr = sy-dynnr.

  MOVE 'ZSPP_SIDE_TRACK-BODY_NO' TO l_dynfieldtab-fieldname.
  APPEND l_dynfieldtab.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname                         = w_repid
      dynumb                         = w_dynnr
*       TRANSLATE_TO_UPPER             = ' '
*       REQUEST                        = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX           = ' '
    TABLES
      dynpfields                     = l_dynfieldtab
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA           = 1
*       INVALID_DYNPROFIELD            = 2
*       INVALID_DYNPRONAME             = 3
*       INVALID_DYNPRONUMMER           = 4
*       INVALID_REQUEST                = 5
*       NO_FIELDDESCRIPTION            = 6
*       INVALID_PARAMETER              = 7
*       UNDEFIND_ERROR                 = 8
*       DOUBLE_CONVERSION              = 9
*       STEPL_NOT_FOUND                = 10
*       OTHERS                         = 11
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  READ TABLE l_dynfieldtab INDEX 1.
*  it_0100-body_no = l_dynfieldtab-fieldvalue.

  SELECT DISTINCT body_no
  INTO TABLE value_tab1
   FROM ztpp_vm
   WHERE model_code = v_model.

  REFRESH return_tab.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BODY_NO'
      dynpprog        = w_repid
      dynpnr          = w_dynnr
*     dynprofield     = 'ZSPP_SIDE_TRACK-BODY_NO'
      window_title    = 'Body No'
      value_org       = 'S'
    TABLES
      value_tab       = value_tab1
      return_tab      = return_tab
    EXCEPTIONS
      parameter_error = 1.

  READ TABLE return_tab INDEX 1.
  IF sy-subrc EQ 0.

* Set values for Industry Type and Description.
    scr_fields-fieldname  = 'ZSPP_SIDE_TRACK-BODY_NO'.
    scr_fields-fieldvalue = return_tab-fieldval.
    APPEND scr_fields.

*  Update back Screen with Values
    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = scr_fields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
  ENDIF.

ENDMODULE.                 " DISPALY_BODY  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_VM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_vm_0200 INPUT.
  DATA: l_object LIKE mara-matnr.

  CHECK w_okcode = 'CREATE'.

  CONCATENATE zspp_side_track-model_code zspp_side_track-body_no
         INTO l_object.

  PERFORM get_vm USING l_object.

  SELECT SINGLE progress INTO zspp_side_track-progress
    FROM ztpp_status
   WHERE id = zspp_side_track-tp_cstatus.

  SELECT SINGLE urgency urgcdate flet
    INTO (zspp_side_track-urgency,
          zspp_side_track-urgcdate,
          zspp_side_track-flet)
    FROM ztsd_um
   WHERE model_code = it_0100-model_code
     AND body_no    = it_0100-body_no
     AND ( status = ' ' OR status = 'F' ).

  IF zspp_side_track-rp_cstatus IS INITIAL.
    g_msg = 'Body No not exist in VM'.
    g_type = 'E'.
    MESSAGE e000(zz) WITH g_msg DISPLAY LIKE g_type.
  ENDIF.

ENDMODULE.                 " DISPLAY_VM  INPUT
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_BODY_NO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM list_box_body_no .
  REFRESH it_list.

  SELECT body_no AS key
    INTO TABLE it_list
    FROM ztpp_vm
    WHERE model_code  = v_model.

  SORT it_list BY key.
  DELETE ADJACENT DUPLICATES FROM it_list COMPARING key.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'ZSPP_SIDE_TRACK-BODY_NO'
      values = it_list.
ENDFORM.                    " LIST_BOX_BODY_NO

*----------------------------------------------------------------------*
*  MODULE set_selection_condition INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE set_selection_condition INPUT.
  PERFORM set_ranges_first_line USING 'R_BODY'  r_body.
  PERFORM set_ranges_first_line USING 'R_DATUM' r_datum.
ENDMODULE.                 " SET_SELECTION_CONDITION  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2656   text
*      -->P_R_BODY  text
*----------------------------------------------------------------------*
FORM set_ranges_first_line  USING pv_range pv_range_val.
  DATA: l_range        TYPE string,
        l_range_sign   TYPE string,
        l_range_option TYPE string,
        l_range_low    TYPE string,
        l_range_high   TYPE string,
        l_button       TYPE string.

  FIELD-SYMBOLS: <lfs_range_it>     TYPE STANDARD TABLE,
                 <lfs_range_sign>,
                 <lfs_range_option>,
                 <lfs_range_low>,
                 <lfs_range_high>,
                 <lfs_button>.

  CONCATENATE: pv_range '[]'          INTO l_range,
               pv_range '-SIGN'       INTO l_range_sign,
               pv_range '-OPTION'     INTO l_range_option,
               pv_range '-LOW'        INTO l_range_low,
               pv_range '-HIGH'       INTO l_range_high,
               'G_' pv_range+2 '_EXT' INTO l_button.

  ASSIGN: (l_range)        TO <lfs_range_it>,
          (l_range_sign)   TO <lfs_range_sign>,
          (l_range_option) TO <lfs_range_option>,
          (l_range_low)    TO <lfs_range_low>,
          (l_range_high)   TO <lfs_range_high>,
          (l_button)       TO <lfs_button>.

  READ TABLE <lfs_range_it> INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    IF <lfs_range_high> IS INITIAL.
      IF <lfs_range_low> IS INITIAL.
        DELETE <lfs_range_it> INDEX 1.
      ELSEIF <lfs_range_low> CA '*+'.
        MOVE: 'I' TO <lfs_range_sign>, 'CP'
              TO <lfs_range_option>.
        MODIFY <lfs_range_it> INDEX 1 FROM pv_range_val.
      ELSE.
        MOVE: 'I' TO <lfs_range_sign>, 'EQ' TO <lfs_range_option>.
        MODIFY <lfs_range_it> INDEX 1 FROM pv_range_val.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <lfs_range_sign>, 'BT' TO <lfs_range_option>.
      MODIFY <lfs_range_it> INDEX 1 FROM pv_range_val.
    ENDIF.
  ELSE.
    IF <lfs_range_high> IS INITIAL.
      IF <lfs_range_low> IS INITIAL.
        " N/A
      ELSEIF <lfs_range_low> CA '*+'.
        MOVE: 'I' TO <lfs_range_sign>, 'CP' TO <lfs_range_option>.
        APPEND pv_range_val TO <lfs_range_it>.
      ELSE.
        MOVE: 'I' TO <lfs_range_sign>, 'EQ' TO <lfs_range_option>.
        APPEND pv_range_val TO <lfs_range_it>.
      ENDIF.
    ELSE.
      MOVE: 'I' TO <lfs_range_sign>, 'BT' TO <lfs_range_option>.
      APPEND pv_range_val TO <lfs_range_it>.
    ENDIF.
  ENDIF.

  READ TABLE <lfs_range_it> INDEX 1 TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    MOVE: icon_display_more TO <lfs_button>.
  ELSE.
    MOVE: icon_enter_more TO <lfs_button>.
  ENDIF.
ENDFORM.                    " SET_RANGES_FIRST_LINE
*&---------------------------------------------------------------------*
*&      Form  DELETE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_rtn .
  DATA: l_answer(1).

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CLEAR: it_rows, it_rows[], it_row_no, it_row_no[].
  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = it_rows[]
      et_row_no     = it_row_no[].

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE it_rows INDEX 1.

  READ TABLE it_0100 INDEX it_rows-index.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmation'
*     DIAGNOSE_OBJECT       = ' '
      text_question         = 'Are you sure to delete the data?'
      text_button_1         = 'YES'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'NO'
      icon_button_2         = 'ICON_CANCEL'
*     DEFAULT_BUTTON        = '1'
      display_cancel_button = ' '
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = ' '
*     START_ROW             = 6
*     POPUP_TYPE            =
    IMPORTING
      answer                = l_answer.

  IF l_answer = '1'.
    DELETE FROM ztpp_side_track
     WHERE model_code = it_0100-model_code
       AND body_no    = it_0100-body_no
       AND zseq       = it_0100-zseq.
    IF sy-subrc = 0.
      DELETE it_0100 INDEX it_rows-index.
      COMMIT WORK.
      MESSAGE s000(zz) WITH 'Data is deleted'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s000(zz) WITH 'Error in deletion'.
    ENDIF.
  ENDIF.

ENDFORM.                    " DELETE_RTN
*&---------------------------------------------------------------------*
*&      Form  CLICK_EXTENSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0962   text
*      -->P_0963   text
*----------------------------------------------------------------------*
FORM click_extension USING p_field p_ref.
  DATA: ls_ref      LIKE rstabfield,
        l_range_h(50),               " Body of ranges
        l_range_b(50),               " Header of ranges
        l_button(50).

  FIELD-SYMBOLS: <lfs_range_h>,
                 <lfs_range_b> TYPE STANDARD TABLE,
                 <lfs_button>.

  SPLIT p_ref AT '-' INTO ls_ref-tablename ls_ref-fieldname.

  CONCATENATE 'R_' p_field INTO l_range_h.
  ASSIGN (l_range_h) TO <lfs_range_h>.

  CONCATENATE 'R_' p_field '[]' INTO l_range_b.
  ASSIGN (l_range_b) TO <lfs_range_b>.

  CONCATENATE 'G_' p_field '_EXT' INTO l_button.
  ASSIGN (l_button) TO <lfs_button>.

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      tab_and_field = ls_ref
    TABLES
      range         = <lfs_range_b>
    EXCEPTIONS
      cancelled     = 1
      OTHERS        = 2.

  READ TABLE <lfs_range_b> INTO <lfs_range_h> INDEX 1.
  IF sy-subrc EQ 0.
    MOVE: icon_display_more TO <lfs_button>.
  ELSE.
    CLEAR: <lfs_range_h>.
    MOVE: icon_enter_more   TO <lfs_button>.
  ENDIF.
ENDFORM.                    " CLICK_EXTENSION
*&---------------------------------------------------------------------*
*&      Form  PRINT_SIDE_TRACK_NOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_side_track_note .
  DATA: l_retcode        TYPE t_oi_ret_string,
        l_doc_size       TYPE i,
        l_doc_type(80)   VALUE soi_doctype_word97_document,
        l_doc_format(80) TYPE c,
        l_filename       TYPE string.

*  DATA: l_answer(1).

  CHECK zspp_side_track-body_no IS NOT INITIAL.

*  IF w_save IS INITIAL.
*    CALL FUNCTION 'POPUP_TO_CONFIRM'
*      EXPORTING
*        titlebar              = 'Confirmation'
**       DIAGNOSE_OBJECT       = ' '
*        text_question     = 'Do you want to save Side Track Notice?'
*        text_button_1         = 'YES'
*        icon_button_1         = 'ICON_OKAY'
*        text_button_2         = 'NO'
*        icon_button_2         = 'ICON_CANCEL'
**       DEFAULT_BUTTON        = '1'
*        display_cancel_button = ' '
**       USERDEFINED_F1_HELP   = ' '
**       START_COLUMN          = ' '
**       START_ROW             = 6
**       POPUP_TYPE            =
*      IMPORTING
*        answer                = l_answer.
*
*    IF l_answer = '1'.
*      IF w_save IS INITIAL.
*        PERFORM save_data.
*      ENDIF.
*    ENDIF.
*  ENDIF.

*  CHECK w_save = 'X'.

** On 09/12/13
  PERFORM save_data.

*---// Init FACTORY
  CLEAR: if_factory, if_link_server.

  CALL METHOD c_oi_factory_creator=>get_document_factory
    IMPORTING
      factory = if_factory
      retcode = l_retcode.
  IF l_retcode NE c_oi_errors=>ret_ok.
    EXIT.
  ENDIF.

  CALL METHOD if_factory->start_factory
    EXPORTING
      r3_application_name = g_filename
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

  CALL METHOD if_link_server->stop_link_server
    IMPORTING
      retcode = l_retcode.

  CALL METHOD if_link_server->start_link_server
    IMPORTING
      retcode = l_retcode.

*  CALL METHOD c_oi_errors=>show_message
*    EXPORTING
*      type = 'E'.


*---// Get file
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      object_id        = g_objid
    IMPORTING
      data_size        = l_doc_size
      document_format  = l_doc_format
      document_type    = l_doc_type
    TABLES
      data_table       = it_file
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

  CALL METHOD if_document->close_document
    EXPORTING
      do_save = 'X'
    IMPORTING
      retcode = l_retcode.

  CONCATENATE g_filepath g_filename INTO l_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'
      bin_filesize            = l_doc_size
*     NO_AUTH_CHECK           = 'X'
    TABLES
      data_tab                = it_file[]
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
** End on 09/12/2013

  IF excel-header = space OR excel-handle = -1.
    CREATE OBJECT excel 'EXCEL.APPLICATION'.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE i009 WITH
    'Error occurred while opening excel'.
    subrc = '4'.
  ELSE.
    SET PROPERTY OF excel 'DisplayAlerts' = 0.
    SET PROPERTY OF  excel 'VISIBLE' = 0.
    CALL METHOD OF
        excel
        'WORKBOOKS' = workbooks.
    CALL METHOD OF
        workbooks
        'OPEN'    = open_workbooks
      EXPORTING
        #1        = filename.

    IF sy-subrc NE 0.
      MESSAGE i009 WITH
      'Error occurred while opening excel'.
      subrc = '4'.
    ENDIF.
  ENDIF.

  PERFORM excel_sheet USING 1 CHANGING subrc.
  PERFORM write_data.
  PERFORM excel_print.

  CALL METHOD OF
      excel
      'Quit'.
  FREE OBJECT excel.


  LEAVE TO SCREEN 0.
ENDFORM.                    " PRINT_SIDE_TRACK_NOTE
*---------------------------------------------------------------------*
*&      Form  EXCEL_SHEET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_sheet USING p_sheet CHANGING $subrc.

  IF sheet-handle > 0.
    FREE OBJECT sheet.
    sheet-handle = -1.
  ENDIF.

  CALL METHOD OF
      excel
      'Worksheets' = sheet
    EXPORTING
      #1           = p_sheet.

  IF sy-subrc NE 0. $subrc = sy-subrc. EXIT. ENDIF.

  CALL METHOD OF
      sheet
      'Activate'.

  IF sheet-handle > 0.
    FREE OBJECT sheet.
    sheet-handle = -1.
  ENDIF.

ENDFORM.                               " EXCEL_SHEET
*&---------------------------------------------------------------------*
*&      Form  write_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.

  DATA: l_date(10),
        l_body(9).

  DEFINE __fill_data.
    perform fill_cells using &1 &2 &3.
  END-OF-DEFINITION.

  WRITE: zspp_side_track-erdat   TO l_date MM/DD/YYYY.
  CONCATENATE zspp_side_track-model_code
              zspp_side_track-body_no INTO  l_body.
  __fill_data :  '8' 'J' zspp_side_track-zsttxt,
                 '10' 'N' zspp_side_track-zrequstor,
                 '10' 'AD' zspp_side_track-zdept,
                 '10' 'AN' zspp_side_track-zphone,
                 '12' 'H' l_date,
                 '12' 'AE' zspp_side_track-zlocat,
                 '15' 'L' zspp_side_track-rp06_serial,
                 '15' 'AG' l_body.

  WRITE: zspp_side_track-zreturn_date TO l_date MM/DD/YYYY.

  __fill_data :  '20' 'AD' l_date,
                 '20' 'AM' zspp_side_track-zreturn_time.

ENDFORM.                    " write_header
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*
FORM fill_cells USING i j val.

  CALL METHOD OF
      excel
      'CELLS' = cells
    EXPORTING
      #1      = i
      #2      = j.

  SET PROPERTY OF cells 'VALUE' = val.
  IF cells-handle > 0.
    FREE OBJECT cells.
  ENDIF.


ENDFORM.                    "fill_cells
*&---------------------------------------------------------------------*
*&      Form  EXCEL_PRINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_print .
  GET PROPERTY OF excel 'ACTIVEWORKBOOK' = e_work.
  CALL METHOD OF
      e_work
      'PRINTOUT'

    EXPORTING
      #1         = 1
      #2         = 1.
  IF sy-subrc = 0.
    MESSAGE s000(zz) WITH 'Successfully Print out'.
  ENDIF.
  FREE OBJECT e_work.

ENDFORM.                    " EXCEL_PRINT
*&---------------------------------------------------------------------*
*&      Form  SAVE_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_excel .
  IF sheet-handle > 0. FREE OBJECT sheet. ENDIF.

  CALL METHOD OF
      excel
      'Worksheets' = sheet
    EXPORTING
      #1           = 1.
  CALL METHOD OF
      sheet
      'Activate' = exl_activate.

  IF exl_activate-handle > 0.
    FREE OBJECT exl_activate.
  ENDIF.

  PERFORM get_file_name CHANGING subrc.

  CALL METHOD OF
      sheet
      'SAVEAS' = exl_saveas
    EXPORTING
      #1       = tmp_file.
  IF sy-subrc NE 0.
  ENDIF.

  FREE OBJECT : sheet, exl_saveas.
ENDFORM.                    " SAVE_EXCEL
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_SUBRC  text
*----------------------------------------------------------------------*

FORM get_file_name CHANGING p_subrc.

  DO 100 TIMES.

    tmp_file = 'C:\TEMP\$XLSTMP$$'.
    WRITE sy-datlo TO tmp_file+17(6) YYMMDD.
    tmp_file+23 = sy-timlo.
    CONCATENATE tmp_file '.xls' INTO tmp_file.
    CONDENSE tmp_file NO-GAPS.

    CALL FUNCTION 'WS_QUERY'
      EXPORTING
        filename       = tmp_file
        query          = 'FE'
      IMPORTING
        return         = subrc
      EXCEPTIONS
        inv_query      = 1
        no_batch       = 2
        frontend_error = 3
        OTHERS         = 4.

    IF subrc NE 1. EXIT. ENDIF.

*    PERFORM file_delete.
*
*    CALL FUNCTION 'WS_FILE_DELETE'
*         EXPORTING
*              file = tmp_file.
  ENDDO.

ENDFORM.                    " get_file_name
*&---------------------------------------------------------------------*
*&      Form  PRINT_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_rtn .
  DATA: l_retcode        TYPE t_oi_ret_string,
        l_doc_size       TYPE i,
        l_doc_type(80)   VALUE soi_doctype_word97_document,
        l_doc_format(80) TYPE c,
        l_filename       TYPE string.

  DATA: l_answer(1).

  CONCATENATE: 'WC_ALV_' sy-dynnr INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

  CLEAR: it_rows, it_rows[], it_row_no, it_row_no[].
  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = it_rows[]
      et_row_no     = it_row_no[].

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    v_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = v_repid
        txt2  = sy-subrc
        txt1  = 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE it_rows INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  READ TABLE it_0100 INDEX it_rows-index.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING it_0100 TO zspp_side_track.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmation'
*     DIAGNOSE_OBJECT       = ' '
      text_question         = 'Do you want to print Side Track Notice?'
      text_button_1         = 'YES'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'NO'
      icon_button_2         = 'ICON_CANCEL'
*     DEFAULT_BUTTON        = '1'
      display_cancel_button = ' '
*     USERDEFINED_F1_HELP   = ' '
*     START_COLUMN          = ' '
*     START_ROW             = 6
*     POPUP_TYPE            =
    IMPORTING
      answer                = l_answer.

  CHECK l_answer = '1'.

*---// Init FACTORY
  CLEAR: if_factory, if_link_server.

  CALL METHOD c_oi_factory_creator=>get_document_factory
    IMPORTING
      factory = if_factory
      retcode = l_retcode.
  IF l_retcode NE c_oi_errors=>ret_ok.
    EXIT.
  ENDIF.

  CALL METHOD if_factory->start_factory
    EXPORTING
      r3_application_name = g_filename
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

  CALL METHOD if_link_server->stop_link_server
    IMPORTING
      retcode = l_retcode.

  CALL METHOD if_link_server->start_link_server
    IMPORTING
      retcode = l_retcode.

*  CALL METHOD c_oi_errors=>show_message
*    EXPORTING
*      type = 'E'.


*---// Get file
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
    EXPORTING
      object_id        = g_objid
    IMPORTING
      data_size        = l_doc_size
      document_format  = l_doc_format
      document_type    = l_doc_type
    TABLES
      data_table       = it_file
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

  CALL METHOD if_document->close_document
    EXPORTING
      do_save = 'X'
    IMPORTING
      retcode = l_retcode.

  CONCATENATE g_filepath g_filename INTO l_filename.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_filename
      filetype                = 'BIN'
      bin_filesize            = l_doc_size
*     NO_AUTH_CHECK           = 'X'
    TABLES
      data_tab                = it_file[]
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

  IF excel-header = space OR excel-handle = -1.
    CREATE OBJECT excel 'EXCEL.APPLICATION'.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE i009 WITH
    'Error occurred while opening excel'.
    subrc = '4'.
  ELSE.
    SET PROPERTY OF excel 'DisplayAlerts' = 0.
    SET PROPERTY OF  excel 'VISIBLE' = 0.
    CALL METHOD OF
        excel
        'WORKBOOKS' = workbooks.

    CALL METHOD OF
        workbooks
        'OPEN'    = open_workbooks
      EXPORTING
        #1        = filename.

    IF sy-subrc NE 0.
      MESSAGE i009 WITH
      'Error occurred while opening excel'.
      subrc = '4'.
    ENDIF.
  ENDIF.

  PERFORM excel_sheet USING 1 CHANGING subrc.
  PERFORM write_data.


  PERFORM excel_print.

  CALL METHOD OF
      excel
      'Quit'.
  FREE OBJECT excel.
ENDFORM.                    " PRINT_RTN
*&---------------------------------------------------------------------*
*&      Form  SET_SECTION
*&---------------------------------------------------------------------*
FORM set_section .
  DATA: lt_section LIKE ztpp_delay_sect OCCURS 0 WITH HEADER LINE.

  REFRESH: it_list.

  SELECT DISTINCT zsecid zsectx zrpfr
    INTO CORRESPONDING FIELDS OF TABLE lt_section
    FROM ztpp_delay_sect.

  SORT lt_section BY zrpfr.

  LOOP AT lt_section.
    CLEAR: st_value.
    MOVE: lt_section-zsecid TO st_value-key,
          lt_section-zsectx TO st_value-text.
    APPEND st_value TO it_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'V_ZSECID'
      values = it_list.
ENDFORM.                    " SET_SECTION
*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
FORM set_status .
  REFRESH: it_list.

  st_value-key =   'I'.
  st_value-text =  'Open'.
  APPEND st_value TO it_list.

  st_value-key =   'F'.
  st_value-text =  'Closed'.
  APPEND st_value TO it_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'V_STATUS'
      values = it_list.
ENDFORM.                    " SET_STATUS

*&---------------------------------------------------------------------*
*&      Form  CALCULATE_SECTION
*&---------------------------------------------------------------------*
FORM calculate_section  CHANGING p_zsecid.
  DATA: l_nation LIKE ztsd_um-wo_nation.

  CLEAR : v_zsecid_200.

  CHECK zspp_side_track-rp_cstatus IS NOT INITIAL.

  SELECT SINGLE wo_nation INTO l_nation
  FROM ztsd_um
  WHERE model_code  = zspp_side_track-model_code
    AND body_no     = zspp_side_track-body_no.

  IF l_nation = 'B28'.
    SELECT SINGLE zsecid INTO v_zsecid_200
    FROM ztpp_delay_sect
    WHERE zoper     <> '<>'
      AND zrpfr <= zspp_side_track-rp_cstatus
      AND zrpto >= zspp_side_track-rp_cstatus.
  ELSE.
    SELECT SINGLE zsecid INTO v_zsecid_200
    FROM ztpp_delay_sect
    WHERE zoper     <> '='
      AND zrpfr <= zspp_side_track-rp_cstatus
      AND zrpto >= zspp_side_track-rp_cstatus.
  ENDIF.

ENDFORM.                    " CALCULATE_SECTION
*&---------------------------------------------------------------------*
*&      Form  UPLAOD_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_rtn .
  DATA: lw_key LIKE wwwdatatab.

  lw_key-relid = 'MI'.
  lw_key-objid = 'ZPP_SIDE_TRACK_NOTICE'.
  lw_key-tdate = sy-datum.
  lw_key-ttime = sy-uzeit.
  lw_key-text = 'Side Track Notice Form'.
  lw_key-devclass = 'ZDPP'.

  CALL FUNCTION 'UPLOAD_WEB_OBJECT'
    EXPORTING
      key           = lw_key
*     TEMP          = ' '
*     TEXT          =
*   IMPORTING
*     RC            =
            .
ENDFORM.                    " UPLAOD_RTN
