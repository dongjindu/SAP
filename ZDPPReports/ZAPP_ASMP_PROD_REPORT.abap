*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZAPP_ASMP_PROD_REPORT
*& Program Name   : AS&MP Production Report
*& Created by     : Bae Byung Sung
*& Created on     : 07.14.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.
*&
*&----------------------------------------------------------------------
* RP_LAST_DAY_OF_MONTHS
REPORT  zapp_asmp_prod_report MESSAGE-ID zz.
INCLUDE: <icon>.

DATA: BEGIN OF st_0100,
        werks   LIKE mseg-werks,
        model   LIKE makt-maktx,
        line    LIKE makt-maktx,
        matnr   LIKE mseg-matnr,
        year    TYPE p DECIMALS 0,
        jan     TYPE p DECIMALS 0,
        feb     TYPE p DECIMALS 0,
        mar     TYPE p DECIMALS 0,
        apr     TYPE p DECIMALS 0,
        may     TYPE p DECIMALS 0,
        jun     TYPE p DECIMALS 0,
        jul     TYPE p DECIMALS 0,
        aug     TYPE p DECIMALS 0,
        sep     TYPE p DECIMALS 0,
        oct     TYPE p DECIMALS 0,
        nov     TYPE p DECIMALS 0,
        dec     TYPE p DECIMALS 0,
        01      TYPE p DECIMALS 0,
        02      TYPE p DECIMALS 0,
        03      TYPE p DECIMALS 0,
        04      TYPE p DECIMALS 0,
        05      TYPE p DECIMALS 0,
        06      TYPE p DECIMALS 0,
        07      TYPE p DECIMALS 0,
        08      TYPE p DECIMALS 0,
        09      TYPE p DECIMALS 0,
        10      TYPE p DECIMALS 0,
        11      TYPE p DECIMALS 0,
        12      TYPE p DECIMALS 0,
        13      TYPE p DECIMALS 0,
        14      TYPE p DECIMALS 0,
        15      TYPE p DECIMALS 0,
        16      TYPE p DECIMALS 0,
        17      TYPE p DECIMALS 0,
        18      TYPE p DECIMALS 0,
        19      TYPE p DECIMALS 0,
        20      TYPE p DECIMALS 0,
        21      TYPE p DECIMALS 0,
        22      TYPE p DECIMALS 0,
        23      TYPE p DECIMALS 0,
        24      TYPE p DECIMALS 0,
        25      TYPE p DECIMALS 0,
        26      TYPE p DECIMALS 0,
        27      TYPE p DECIMALS 0,
        28      TYPE p DECIMALS 0,
        29      TYPE p DECIMALS 0,
        30      TYPE p DECIMALS 0,
        31      TYPE p DECIMALS 0,
        row_color(4),
        index(5) TYPE n,
      END   OF st_0100.

DATA: it_0100 LIKE st_0100 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_asmp_sum OCCURS 0,
        werks LIKE mseg-werks,
        model LIKE ztpp_asmppr-model,
        line  LIKE makt-maktx,
        matnr LIKE mseg-matnr,
        rdatu LIKE mseg-zbudat,
        menge LIKE mseg-menge,
        meins LIKE mseg-meins,
        bwart LIKE mseg-bwart,
      END   OF it_asmp_sum.

DATA: it_pgmi LIKE pgmi OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_month OCCURS 0,
        name(3),
        number(2) TYPE n,
        ext,
      END   OF it_month.

DATA: BEGIN OF it_field OCCURS 0,
        name(10),
        holiday,
      END   OF it_field.

DATA: v_werks   LIKE ztpp_asmp_sum-werks,
      v_year(4) TYPE n,
      v_prgrp   LIKE pgmi-prgrp,
      v_iferr   TYPE i.

RANGES: r_bwart FOR it_asmp_sum-bwart.
CONSTANTS: c_checked VALUE 'X'.

*-----/// LIST BOX DATA
TYPE-POOLS vrm.
DATA: list  TYPE vrm_values,
      value LIKE LINE OF list.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_0100   TYPE        scrfname VALUE 'CC_0100_ALV',
      wc_alv_0100       TYPE REF TO cl_gui_alv_grid,
      wc_container_0100 TYPE REF TO cl_gui_custom_container.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS lcl_event_receiver DEFINITION DEFERRED. "/ALV Event Handling

DATA : event_receiver TYPE REF TO lcl_event_receiver.

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
               <itab>      TYPE STANDARD TABLE,
               <itab_old>  TYPE STANDARD TABLE.

CONSTANTS: c_structure(100) VALUE 'ST_'.

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

*    METHODS:
*    handle_hotspot_click
*        FOR EVENT hotspot_click OF cl_gui_alv_grid
*            IMPORTING e_row_id
*                      e_column_id
*                      es_row_no.
*
*    METHODS:
*    handle_toolbar
*        FOR EVENT toolbar OF cl_gui_alv_grid
*            IMPORTING e_object e_interactive.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM dbl_click_0100 USING e_column-fieldname
                                 es_row_no-row_id.
  ENDMETHOD.                           "handle_double_click
*
*  METHOD handle_hotspot_click.
*    PERFORM hotspot_click      USING   e_row_id
*                                       e_column_id
*                                       es_row_no.
*  ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK
*
*  METHOD handle_toolbar.
*    PERFORM handle_toolbar USING e_object e_interactive.
*  ENDMETHOD.                    "handle_toolbar
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

INITIALIZATION.
  PERFORM set_default_field_lists.

START-OF-SELECTION.
*  PERFORM read_month_data.
*  PERFORM read_day_data USING sy-datum(6).
*  PERFORM get_model_n_line.

  CALL SCREEN 100.
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 100.
      SET PF-STATUS '100'.
      SET TITLEBAR  '100'.
  ENDCASE.

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
    PERFORM sssign_event              USING p_dynnr.
  ELSE.
*    PERFORM set_attribute             USING p_dynnr.
    PERFORM build_field_catalog       USING p_dynnr.
    PERFORM reassign_itab_to_alv        USING p_dynnr.
    PERFORM refresh_alv.
  ENDIF.

*  PERFORM set_attribute             USING p_dynnr.
*  PERFORM build_field_catalog       USING p_dynnr.
*  PERFORM assign_itab_to_alv        USING p_dynnr.
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

*  IF it_fieldcat[] IS INITIAL.
  PERFORM set_fieldname USING p_dynnr.
  PERFORM set_screen_fields USING p_dynnr.
*  ENDIF.
ENDFORM.                    "build_field_catalog
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

ENDFORM.                    "set_fieldname
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.

  CASE p_dynnr.
    WHEN '0100'.
      PERFORM set_screen_fields_0100.
  ENDCASE.

ENDFORM.                    "set_screen_fields
*----------------------------------------------------------------------*
FORM set_screen_fields_0100.
  DATA: lv_color(4),
        lv_length(2),
        lv_part(10),
        lv_type(10),
        lv_char(30).

  SPLIT v_prgrp AT '-' INTO lv_type lv_char.
  CONCATENATE lv_type 'Parts' INTO lv_part SEPARATED BY space.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MODEL'        ' ',
                                  ' ' 'COLTEXT'      'Model',
                                  ' ' 'OUTPUTLEN'    '6',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'LINE'         ' ',
                                  ' ' 'COLTEXT'      'Line',
                                  ' ' 'OUTPUTLEN'    '14',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'MATNR'        ' ',
                                  ' ' 'COLTEXT'      lv_part,
                                  ' ' 'OUTPUTLEN'    '15',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',
*
                                  'S' 'YEAR'         ' ',
                                  ' ' 'COLTEXT'      v_year,
                                  ' ' 'OUTPUTLEN'    '8',
                                  ' ' 'DO_SUM'       'X',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C300'.

  LOOP AT it_field.
    READ TABLE it_month WITH KEY name = it_field-name.
    IF sy-subrc EQ 0.            " Month
      MOVE: 'C500' TO lv_color,
            '6'   TO lv_length.
    ELSE.                        " Day
      CASE it_field-holiday.
        WHEN c_checked.
          MOVE: 'C600' TO lv_color.
        WHEN space.
          MOVE: 'C200' TO lv_color.
      ENDCASE.
      MOVE: '6' TO lv_length.
    ENDIF.

    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                    'S' it_field-name  ' ',
                                    ' ' 'COLTEXT'      it_field-name,
                                    ' ' 'OUTPUTLEN'    lv_length,
                                    ' ' 'DO_SUM'       'X',
                                    ' ' 'NO_ZERO'      'X',
                                    'E' 'EMPHASIZE'    lv_color.
  ENDLOOP.
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

  PERFORM set_selected_rows         USING p_dynnr.
ENDFORM.                    "assign_itab_to_alv
*----------------------------------------------------------------------*
FORM sssign_event USING p_dynnr.

  DATA: lv_dynnr   LIKE   sy-dynnr.

  CONCATENATE: 'WC_ALV_'    p_dynnr      INTO v_alv.
  ASSIGN: (v_alv)       TO <alv>.

*--  Regist event for Edit
  CALL METHOD <alv>->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CREATE OBJECT event_receiver.
  IF p_dynnr EQ '0100'.
    SET HANDLER event_receiver->handle_double_click  FOR <alv>.
  ENDIF.
ENDFORM.                    " sssign_event
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
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_sort_total_field USING p_dynnr.
  CALL METHOD <alv>->get_sort_criteria
    IMPORTING
      et_sort = it_sort[].

  IF it_sort[] IS INITIAL.
    CASE p_dynnr.
      WHEN '0100'.
        REFRESH it_sort.

        CLEAR : it_sort.
        it_sort-fieldname = 'MODEL'.
*        it_sort-subtot    = 'X'.
        APPEND it_sort.

        CLEAR : it_sort.
        it_sort-fieldname = 'LINE'.
        it_sort-comp      = 'X'.
*        it_sort-subtot    = 'X'.
*        IT_SORT-EXPA      = 'X'.
        APPEND it_sort.
    ENDCASE.
  ENDIF.
ENDFORM.                    " SET_SORT_TOTAL_FIELD

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
  CALL METHOD <alv>->get_frontend_layout
    IMPORTING
      es_layout = v_layout.

  IF v_layout IS INITIAL.
    CASE p_dynnr.
      WHEN '0100'.
*        v_layout-edit       = ' '.      "/Edit Mode Enable
        v_layout-sel_mode   = 'A'.      "/mode for select col and row
        v_layout-language   = sy-langu. "/Language Key
*        v_layout-cwidth_opt = 'X'.      "/optimizes the column width
*        v_layout-no_merging = 'X'.      "/Disable cell merging
        v_layout-zebra = 'X'.
*        v_layout-totals_bef = 'X'.
        v_layout-info_fname = 'ROW_COLOR'.
    ENDCASE.
  ENDIF.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DYNNR: Current Screen Number
*----------------------------------------------------------------------*
FORM set_variant USING p_dynnr.
  CALL METHOD <alv>->get_variant
    IMPORTING
      es_variant = v_variant.

*
  IF v_variant IS INITIAL.
    CASE p_dynnr.
      WHEN '0100'.
        v_variant-report      = sy-repid.
        v_variant-handle      = space.
        v_variant-log_group   = space.
        v_variant-username    = space.
        v_variant-variant     = space.
        v_variant-text        = space.
        v_variant-dependvars  = space.
    ENDCASE.
  ENDIF.
ENDFORM.                    " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_FILTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_filter  USING    p_dynnr.
  REFRESH it_filter.

  CALL METHOD <alv>->get_filter_criteria
    IMPORTING
      et_filter = it_filter[].

  IF it_filter[] IS INITIAL.
    CASE p_dynnr.
      WHEN '0100'.
        " N/A
      WHEN '0100'.
        " N/A
    ENDCASE.
  ENDIF.
ENDFORM.                    " SET_FILTER
*&---------------------------------------------------------------------*
*&      Form  SET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_selected_rows USING p_dynnr.
  CALL METHOD <alv>->set_selected_rows
    EXPORTING
      it_index_rows = it_rows[]
      it_row_no     = it_row_no[].
ENDFORM.                    " SET_SELECTED_ROWS
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
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      CLEAR: sy-ucomm.

      PERFORM refresh_rtn.
      PERFORM refresh_alv.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SET_DEFAULT_FIELD_LISTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_default_field_lists .
  DATA: lt_month_names LIKE t247 OCCURS 0 WITH HEADER LINE.

  DATA: lv_monthtxt  LIKE t247-ktx,
        lv_field_idx TYPE i,
        lv_daycnt    TYPE i,
        lv_day(2)    TYPE n,
        lv_datum     LIKE sy-datum.

  GET PARAMETER ID 'WRK' FIELD v_werks.
  SELECT SINGLE werks INTO v_werks
    FROM t001w
   WHERE werks = v_werks
     AND werks LIKE 'P*'.
  IF sy-subrc NE 0.
    MOVE: 'P001' TO v_werks.
  ENDIF.

  SELECT SINGLE matnr INTO v_prgrp
    FROM mara
   WHERE mtart = 'PROD'
     AND matnr LIKE '%PNL'.
*  MOVE: 'MV-PNL' TO v_prgrp.

  SELECT SINGLE COUNT(*) INTO v_iferr
    FROM ztpp_asmppr
   WHERE zresult = 'E'.

  MOVE: sy-datum(4) TO v_year.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language              = 'E'
    TABLES
      month_names           = lt_month_names
    EXCEPTIONS
      month_names_not_found = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH 'Calendar error'.
  ENDIF.

  CLEAR: it_month, it_month[],it_field, it_field[].

  LOOP AT lt_month_names.
    CLEAR: it_month.
    MOVE: lt_month_names-ktx TO it_month-name,
          lt_month_names-mnr TO it_month-number.
    APPEND it_month.

    CLEAR: it_field.
    MOVE: lt_month_names-ktx TO it_field-name.
    APPEND it_field.
  ENDLOOP.

  PERFORM set_day_field.
*  CALL FUNCTION 'ISP_GET_MONTH_NAME'
*    EXPORTING
*      date        = sy-datum
*      language    = 'E'
*    IMPORTING
*      shorttext   = lv_monthtxt
*    EXCEPTIONS
*      calendar_id = 1
*      date_error  = 2
*      not_found   = 3
*      wrong_input = 4
*      OTHERS      = 5.
*  IF sy-subrc <> 0.
*    MESSAGE e000 WITH text-m02 text-m01.
*  ENDIF.
*
*  READ TABLE it_month WITH KEY name = lv_monthtxt.
*  IF sy-subrc NE 0.
*    MESSAGE e000 WITH text-m02 text-m01.
*  ENDIF.
*
*  it_month-ext = 'X'.
*  MODIFY it_month INDEX sy-tabix.
*
*  READ TABLE it_field WITH KEY name = lv_monthtxt.
*  IF sy-subrc NE 0.
*    MESSAGE e000 WITH text-m02 text-m01.
*  ENDIF.
*
*  MOVE: sy-tabix TO lv_field_idx.
*
*  lv_daycnt = sy-datum+6(2).
*
*  DO lv_daycnt TIMES.
*    CLEAR: it_field.
*
*    MOVE: sy-index TO lv_day.
*
*    CONCATENATE v_year it_month-number lv_day INTO lv_datum.
*
*    PERFORM check_holiday USING lv_datum it_field-holiday.
*
*    lv_field_idx = lv_field_idx + 1.
*
*    MOVE: lv_day TO it_field-name.
*    INSERT it_field INDEX lv_field_idx.
*  ENDDO.
ENDFORM.                    " SET_DEFAULT_FIELD_LISTS
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
  DATA: lv_tabix     LIKE sy-tabix,
        lv_date      LIKE sy-datum,
        lv_lastday   LIKE sy-datum,
        lv_day(2)    TYPE n,
        lv_daycnt(2) TYPE n,
        lv_field_idx TYPE i,
        lv_datum     LIKE sy-datum,
        lv_month_sel(6),
        lv_extension.

  CHECK ps_row_no EQ 0.

  READ TABLE it_month WITH KEY name = p_column_name.

  CHECK sy-subrc EQ 0.

  CONCATENATE v_year it_month-number INTO lv_month_sel.

  IF lv_month_sel > sy-datum(6).
    MESSAGE i000 WITH text-m04.
    LEAVE TO SCREEN sy-dynnr.
  ENDIF.

  MOVE: sy-tabix TO lv_tabix.

  DELETE it_field WHERE name BETWEEN '01' AND '31'.

  CLEAR: it_0100.

  MODIFY it_0100 TRANSPORTING 01 02 03 04 05 06 07 08 09 10
                              11 12 13 14 15 16 17 18 19 20
                              21 22 23 24 25 26 27 28 29 30
                              31
                 WHERE model >= space.

  MOVE: it_month-ext TO lv_extension.

  CLEAR: it_month-ext.
  MODIFY it_month TRANSPORTING ext WHERE name >= space.

  CASE lv_extension.
    WHEN space.
      it_month-ext = c_checked.
      MODIFY it_month INDEX lv_tabix.

      CONCATENATE v_year it_month-number '01' INTO lv_date.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = lv_date
        IMPORTING
          last_day_of_month = lv_lastday
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.

      IF sy-subrc <> 0.
        MESSAGE e000 WITH text-m02 text-m01.
      ENDIF.

      READ TABLE it_field WITH KEY name = p_column_name.
      IF sy-subrc NE 0.
        MESSAGE e000 WITH text-m02 text-m01.
      ENDIF.

      MOVE: sy-tabix TO lv_field_idx.

      IF it_month-number EQ sy-datum+4(2).
        lv_daycnt = sy-datum+6(2).
      ELSE.
        lv_daycnt = lv_lastday+6(2).
      ENDIF.

      DO lv_daycnt TIMES.
        CLEAR: it_field.

        MOVE: sy-index TO lv_day.

        CONCATENATE v_year it_month-number lv_day INTO lv_datum.

        PERFORM check_holiday USING lv_datum it_field-holiday.

        lv_field_idx = lv_field_idx + 1.

        MOVE: lv_day TO it_field-name.
        INSERT it_field INDEX lv_field_idx.
      ENDDO.

      PERFORM read_day_data USING lv_datum(6).
    WHEN c_checked.
      CLEAR: it_month-ext.
      MODIFY it_month INDEX lv_tabix.
  ENDCASE.
ENDFORM.                    " DBL_CLICK_0100

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_month_data.
  DATA: lv_begin    LIKE sy-datum,
        lv_end      LIKE sy-datum,
        lv_month(2) TYPE n,
        lv_monthqty(50).

  FIELD-SYMBOLS: <fs_monthqty>.

  CLEAR: it_0100, it_0100[].

  CLEAR: r_bwart, r_bwart[].
  CASE v_prgrp.
    WHEN 'AS-PNL'.
      APPEND: 'IEQ601' TO r_bwart,
              'IEQ602' TO r_bwart.
    WHEN 'MP-PNL'.
      APPEND: 'IEQ101' TO r_bwart,
              'IEQ102' TO r_bwart,
              'IEQ309' TO r_bwart,
              'IEQ310' TO r_bwart.
    WHEN 'MV-PNL'.
      APPEND: 'IEQ101' TO r_bwart,
              'IEQ102' TO r_bwart.
  ENDCASE.


  CLEAR: it_pgmi, it_pgmi[].
  SELECT * INTO TABLE it_pgmi
    FROM pgmi
   WHERE werks = v_werks
     AND prgrp = v_prgrp.

  CHECK sy-subrc EQ 0.

  CLEAR: it_asmp_sum, it_asmp_sum[].
  DO 12 TIMES.
    MOVE: sy-index TO lv_month.

    CONCATENATE: v_year lv_month '01' INTO lv_begin,
                 v_year lv_month '31' INTO lv_end.

    PERFORM read_material_document USING lv_begin lv_end.
  ENDDO.

  LOOP AT it_asmp_sum.
    CLEAR: it_0100.

    READ TABLE it_month WITH KEY number = it_asmp_sum-rdatu+4(2).
    CONCATENATE 'IT_0100-' it_month-name INTO lv_monthqty.
    ASSIGN (lv_monthqty) TO <fs_monthqty>.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH text-m00 text-m01.
    ENDIF.

    IF it_asmp_sum-bwart+2(1) EQ '2' OR
       it_asmp_sum-bwart+2(1) EQ '0'.
      it_asmp_sum-menge = it_asmp_sum-menge * -1.
    ENDIF.

    READ TABLE it_0100 WITH KEY werks = v_werks
                                model = it_asmp_sum-model
                                line  = it_asmp_sum-line
                                matnr = it_asmp_sum-matnr.
    IF sy-subrc NE 0.
      MOVE: v_werks           TO it_0100-werks,
            it_asmp_sum-model TO it_0100-model,
            it_asmp_sum-line  TO it_0100-line,
            it_asmp_sum-matnr TO it_0100-matnr,
            it_asmp_sum-menge TO it_0100-year,
            it_asmp_sum-menge TO <fs_monthqty>.
      APPEND it_0100.
    ELSE.
      it_0100-year  = it_0100-year  + it_asmp_sum-menge.
      <fs_monthqty> = <fs_monthqty> + it_asmp_sum-menge.
      MODIFY it_0100 INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_HOLIDAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_DATUM  text
*----------------------------------------------------------------------*
FORM check_holiday USING pv_datum pv_holiday.
  CALL FUNCTION 'RH_GET_DATE_DAYNAME'
    EXPORTING
      langu               = 'E'
      date                = pv_datum
      calid               = 'HM'
    IMPORTING
      dayfree             = pv_holiday
    EXCEPTIONS
      no_langu            = 1
      no_date             = 2
      no_daytxt_for_langu = 3
      invalid_date        = 4
      OTHERS              = 5.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-m02 text-m01.
  ENDIF.
ENDFORM.                    " CHECK_HOLIDAY
*&---------------------------------------------------------------------*
*&      Module  SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_listbox OUTPUT.
  PERFORM set_werks.
  PERFORM set_prgrp.
ENDMODULE.                 " SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_werks .
  CLEAR : list, value.

  SELECT werks AS key
    INTO CORRESPONDING FIELDS OF TABLE list
    FROM t001w
    WHERE werks LIKE 'P*'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'V_WERKS'
      values = list.

ENDFORM.                    " SET_WERKS
*&---------------------------------------------------------------------*
*&      Form  READ_DAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_day_data USING pv_month.
  DATA: lv_begin LIKE sy-datum,
        lv_end   LIKE sy-datum,
        lv_dayqty(50).

  FIELD-SYMBOLS: <fs_dayqty>.

  CONCATENATE: pv_month '01' INTO lv_begin,
               pv_month '31' INTO lv_end.

  CLEAR: r_bwart, r_bwart[].
  CASE v_prgrp.
    WHEN 'AS-PNL'.
      APPEND: 'IEQ601' TO r_bwart,
              'IEQ602' TO r_bwart.
    WHEN 'MP-PNL'.
      APPEND: 'IEQ101' TO r_bwart,
              'IEQ102' TO r_bwart,
              'IEQ309' TO r_bwart,
              'IEQ310' TO r_bwart.
    WHEN 'MV-PNL'.
      APPEND: 'IEQ101' TO r_bwart,
              'IEQ102' TO r_bwart.
  ENDCASE.

  CLEAR: it_pgmi, it_pgmi[].
  SELECT * INTO TABLE it_pgmi
    FROM pgmi
   WHERE werks = v_werks
     AND prgrp = v_prgrp.

  CHECK sy-subrc EQ 0.

  CLEAR: it_asmp_sum, it_asmp_sum[].

  PERFORM read_material_document USING lv_begin lv_end.

  LOOP AT it_asmp_sum.
    CLEAR: it_0100.

    CONCATENATE 'IT_0100-' it_asmp_sum-rdatu+6(2) INTO lv_dayqty.
    ASSIGN (lv_dayqty) TO <fs_dayqty>.
    IF sy-subrc NE 0.
      MESSAGE e000 WITH text-m00 text-m01.
    ENDIF.

    IF it_asmp_sum-bwart+2(1) EQ '2' OR
       it_asmp_sum-bwart+2(1) EQ '0'.
      it_asmp_sum-menge = it_asmp_sum-menge * -1.
    ENDIF.

    READ TABLE it_0100 WITH KEY werks = v_werks
                                matnr = it_asmp_sum-matnr.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    <fs_dayqty> = <fs_dayqty> + it_asmp_sum-menge.
    MODIFY it_0100 INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " READ_DAY_DATA
*&---------------------------------------------------------------------*
*&      Form  REFRESH_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_rtn .
  PERFORM read_month_data.

  IF v_year EQ sy-datum(4).
    CLEAR: it_month-ext.
    MODIFY it_month TRANSPORTING ext WHERE name >= space.

    it_month-ext = 'X'.
    MODIFY it_month TRANSPORTING ext WHERE number = sy-datum+4(2).

    PERFORM set_day_field.

    PERFORM read_day_data USING sy-datum(6).
  ELSE.
    CLEAR: it_month-ext.
    MODIFY it_month TRANSPORTING ext WHERE name >= space.

    DELETE it_field WHERE name BETWEEN '01' AND '31'.
  ENDIF.

  PERFORM get_model_n_line.

  SORT it_0100 BY model line.
ENDFORM.                    " REFRESH_RTN
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv .
  DATA : l_stbl TYPE lvc_s_stbl .

  l_stbl-row = 'X'.
  l_stbl-col = 'X'.

  CALL METHOD <alv>->refresh_table_display
    EXPORTING
      is_stable = l_stbl.

  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  REASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM reassign_itab_to_alv USING p_dynnr.
  CONCATENATE: c_structure  p_dynnr      INTO v_structure.

  CALL METHOD <alv>->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcat[].

  CALL METHOD <alv>->set_sort_criteria
    EXPORTING
      it_sort = it_sort[].

  PERFORM set_selected_rows         USING p_dynnr.
ENDFORM.                    " REASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  SET_DAY_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DATUM(6)  text
*----------------------------------------------------------------------*
FORM set_day_field.
  DATA: lv_monthtxt  LIKE t247-ktx,
        lv_field_idx TYPE i,
        lv_daycnt    TYPE i,
        lv_day(2)    TYPE n,
        lv_datum     LIKE sy-datum.

  CALL FUNCTION 'ISP_GET_MONTH_NAME'
    EXPORTING
      date        = sy-datum
      language    = 'E'
    IMPORTING
      shorttext   = lv_monthtxt
    EXCEPTIONS
      calendar_id = 1
      date_error  = 2
      not_found   = 3
      wrong_input = 4
      OTHERS      = 5.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-m02 text-m01.
  ENDIF.

  READ TABLE it_month WITH KEY name = lv_monthtxt.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH text-m02 text-m01.
  ENDIF.

  it_month-ext = 'X'.
  MODIFY it_month INDEX sy-tabix.

  READ TABLE it_field WITH KEY name = lv_monthtxt.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH text-m02 text-m01.
  ENDIF.

  MOVE: sy-tabix TO lv_field_idx.

  lv_daycnt = sy-datum+6(2).

  DELETE it_field WHERE name >= '01'
                    AND name <= '31'.

  DO lv_daycnt TIMES.
    CLEAR: it_field.

    MOVE: sy-index TO lv_day.

    CONCATENATE v_year it_month-number lv_day INTO lv_datum.

    PERFORM check_holiday USING lv_datum it_field-holiday.

    lv_field_idx = lv_field_idx + 1.

    MOVE: lv_day TO it_field-name.
    INSERT it_field INDEX lv_field_idx.
  ENDDO.
ENDFORM.                    " SET_DAY_FIELD
*&---------------------------------------------------------------------*
*&      Form  GET_MODEL_N_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_model_n_line .
  LOOP AT it_0100.
    CASE v_prgrp.
      WHEN 'MV-PNL'.
        SELECT SINGLE matkl INTO it_0100-model
          FROM mara
         WHERE matnr = it_0100-matnr.

        it_0100-model = it_0100-model+3.

        SELECT SINGLE maktx INTO it_0100-line
          FROM makt
         WHERE spras = 'E'
           AND matnr = it_0100-matnr.
      WHEN OTHERS.
        SELECT SINGLE c~matkl d~maktx
          INTO (it_0100-model,it_0100-line)
        FROM mast AS a INNER JOIN stpo AS b
                               ON b~stlnr = a~stlnr
                       INNER JOIN mara AS c
                               ON c~matnr = b~idnrk
                  LEFT OUTER JOIN makt AS d
                               ON d~matnr = c~matnr
                              AND d~spras = 'E'
        WHERE a~matnr = it_0100-matnr
          AND a~werks = it_0100-werks
          AND a~stlan = '1'
          AND b~stlty = 'M'
          AND b~idnrk LIKE 'MV%'.

        it_0100-model = it_0100-model+3.
    ENDCASE.

    MODIFY it_0100.
  ENDLOOP.
ENDFORM.                    " GET_MODEL_N_LINE
*&---------------------------------------------------------------------*
*&      Form  SET_PRGRP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_prgrp .
  CLEAR : list, value.

  SELECT matnr AS key
    INTO CORRESPONDING FIELDS OF TABLE list
    FROM mara
   WHERE mtart = 'PROD'
     AND matnr LIKE '%PNL'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'V_PRGRP'
      values = list.
ENDFORM.                    " SET_PRGRP
*&---------------------------------------------------------------------*
*&      Form  READ_MATERIAL_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_BIGN  text
*      -->P_AND  text
*      -->P_LV_END  text
*----------------------------------------------------------------------*
FORM read_material_document USING pv_begin pv_end.
  SELECT a~zbudat AS rdatu a~bwart a~werks a~matnr
         erfmg AS menge mjahr mblnr zeile
    APPENDING CORRESPONDING FIELDS OF TABLE it_asmp_sum
    FROM mseg AS a
     FOR ALL ENTRIES IN it_pgmi
   WHERE a~zbudat BETWEEN pv_begin AND pv_end
     AND a~bwart  IN r_bwart
     AND a~werks  =  v_werks
     AND a~matnr  = it_pgmi-nrmit.
ENDFORM.                    " READ_MATERIAL_DOCUMENT
