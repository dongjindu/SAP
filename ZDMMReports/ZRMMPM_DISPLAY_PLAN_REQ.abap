************************************************************************
* Program Name      : ZRMMPM_DISPLAY_PLAN_REQ
* Author            : Byung Sung Bae
* Creation Date     : 2005.03.04.
* Specifications By : Byung Sung Bae
* Development Request No : UD1K914654
* Addl Documentation:
* Description       : Display Material Input Plan(EDI 866)
* Modification Logs
* Date            Developer        RequestNo      Description
* 08.13.2014      Victor     T-code has been deleted for APM
*
************************************************************************
REPORT zrmmpm_display_plan_req .
INCLUDE: <icon>.
TABLES: ztmm_parts_sch, dd03l.

DATA: zsmm_display_plan_9001 LIKE zsmm_display_plan_9001,
      zsmm_display_plan_9002 LIKE zsmm_display_plan_9001,
      zsmm_display_plan_9003 LIKE zsmm_display_plan_9003.

*---// Internal tables
DATA: it_9001 TYPE STANDARD TABLE OF zsmm_display_plan_9001
                                     WITH HEADER LINE.

DATA: it_9002 TYPE STANDARD TABLE OF zsmm_display_plan_9001
                                     WITH HEADER LINE.

DATA: it_9003 TYPE STANDARD TABLE OF zsmm_display_plan_9003
                                     WITH HEADER LINE.

DATA: BEGIN OF it_itab OCCURS 0.
        INCLUDE STRUCTURE ztmm_parts_sch.
DATA:   maktx   LIKE   makt-maktx,
      END   OF it_itab.

DATA: BEGIN OF it_2_hour OCCURS 0,
        bdter     LIKE   it_itab-bdter,
        dayidx(2) TYPE   n,
        index(2)  TYPE   n,
        dazet_f(14),
        dazet_t(14),
      END   OF it_2_hour.

DATA: BEGIN OF it_day OCCURS 0,
        week(2)    TYPE   n,
        bdter      LIKE   it_itab-bdter,
        daytxt(3),
        index(2)   TYPE   n,
        weekidx(2) TYPE   n,
      END   OF it_day.

*---// Work area : Global variables & Structures

*---// Constants
CONSTANTS : c_check                          VALUE 'X'.

*---// Ranges
RANGES: r_arbpl FOR it_itab-arbpl,
        r_rtype FOR it_itab-rtype,
        r_ptype FOR it_itab-ptype.

*---// For Listbox variable
TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

* FUNCTION CODES FOR TABSTRIP 'TAB_9000'
CONSTANTS: BEGIN OF c_tab_9000,
             tab1 LIKE sy-ucomm VALUE 'TAB_9000_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_9000_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_9000_FC3',
           END OF c_tab_9000.
* DATA FOR TABSTRIP 'TAB_9000'
CONTROLS:  tab_9000 TYPE TABSTRIP.
DATA:      BEGIN OF g_tab_9000,
             subscreen   LIKE sy-dynnr,
             prog        LIKE sy-repid VALUE 'ZRMMPM_DISPLAY_PLAN_REQ',
             pressed_tab LIKE sy-ucomm VALUE c_tab_9000-tab1,
           END OF g_tab_9000.
DATA:      ok_code LIKE sy-ucomm.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9001   TYPE        scrfname VALUE 'CC_9001_ALV',
      wc_alv_9001       TYPE REF TO cl_gui_alv_grid,
      wc_container_9001 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9002   TYPE        scrfname VALUE 'CC_9002_ALV',
      wc_alv_9002       TYPE REF TO cl_gui_alv_grid,
      wc_container_9002 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9003   TYPE        scrfname VALUE 'CC_9003_ALV',
      wc_alv_9003       TYPE REF TO cl_gui_alv_grid,
      wc_container_9003 TYPE REF TO cl_gui_custom_container.

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

CONSTANTS: c_structure(100) VALUE 'ZSMM_DISPLAY_PLAN_'.

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

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
*  METHOD  handle_data_changed.
*  ENDMETHOD.
*
*  METHOD  handle_user_command.
*  ENDMETHOD.
ENDCLASS.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001',
            p_shop AS LISTBOX VISIBLE LENGTH 13.
SELECT-OPTIONS: s_vspvb FOR it_itab-vspvb NO-EXTENSION NO INTERVALS.
PARAMETERS:     p_ptype LIKE it_itab-ptype AS LISTBOX VISIBLE LENGTH 8.
SELECT-OPTIONS: s_lifnr FOR it_itab-lifnr,
                s_matnr FOR it_itab-matnr.
SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_listbox.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM set_display_data.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  LOOP AT SCREEN.
    IF screen-name = 'P_WERKS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  set_listbox
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox.
  PERFORM set_listbox_shop.
ENDFORM.                    " set_listbox
*&---------------------------------------------------------------------*
*&      Form  set_listbox_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox_shop.
  CLEAR: name, value, list.

  name = 'P_SHOP'.

  MOVE: 'B'          TO value-key,
        'Body Shop'  TO value-text.
  APPEND value       TO list.

  MOVE: 'P'          TO value-key,
        'Paint Shop' TO value-text.
  APPEND value       TO list.

  MOVE: 'T'          TO value-key,
        'Trim Shop'  TO value-text.
  APPEND value       TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.
ENDFORM.                    " set_listbox_shop
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_shop.
  PERFORM check_ptype.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_shop.
  CHECK p_shop NE space.

  MOVE: 'I'  TO r_arbpl-sign,
        'CP' TO r_arbpl-option.
  CONCATENATE: p_shop '*' INTO r_arbpl-low.

  APPEND r_arbpl.
ENDFORM.                    " check_shop
*&---------------------------------------------------------------------*
*&      Form  check_ptype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ptype.
  CHECK p_ptype NE space.

  MOVE: 'I'     TO r_ptype-sign,
        'EQ'    TO r_ptype-option,
        p_ptype TO r_ptype-low.

  APPEND r_ptype.
ENDFORM.                    " check_ptype
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM display_progress_bar USING text-b01.

  PERFORM get_working_time.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
    FROM ztmm_parts_sch AS a INNER JOIN makt AS b
                                ON b~mandt = a~mandt
                               AND b~matnr = a~matnr
                               AND b~spras = sy-langu
   WHERE a~werks EQ p_werks
     AND a~arbpl IN r_arbpl
     AND a~vspvb IN s_vspvb
     AND a~rtype IN r_rtype
     AND a~ptype IN r_ptype
     AND a~lifnr IN s_lifnr
     AND a~matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN 9000.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " STATUS  OUTPUT
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
  CASE ok_code.
    WHEN 'BACK'.
      CLEAR sy-ucomm.
      LEAVE TO SCREEN 0.
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
    WHEN '9001'.
      PERFORM set_attributes_alv_9001.
    WHEN '9002'.
      PERFORM set_attributes_alv_9002.
    WHEN '9003'.
      PERFORM set_attributes_alv_9003.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9001.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9001
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9002.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_9002
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
*  -->  p1        text
*  <--  p2        text
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
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0849   text
*      -->P_0850   text
*      -->P_0851   text
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
*&      Form  set_screen_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM set_screen_fields USING p_dynnr.
  CASE p_dynnr.
    WHEN '9001'.
      PERFORM set_screen_fields_9001.
    WHEN '9002'.
      PERFORM set_screen_fields_9002.
    WHEN '9003'.
      PERFORM set_screen_fields_9003.
  ENDCASE.
ENDFORM.                    " set_screen_fields
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9001.
  DATA: lw_text(5),
        lw_qty(50).

  DATA: lw_2_hour LIKE it_2_hour.

  FIELD-SYMBOLS: <quantity>.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'LIFNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SHOP'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'VSPVB'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PTYPE'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MEINS'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'TOTAL'       ' ',
                                  ' ' 'COLTEXT'     'Total',
                                  'E' 'EMPHASIZE'   'C310'.

  SELECT * FROM dd03l WHERE tabname = 'ZSMM_DISPLAY_PLAN_9001'
                        AND ( fieldname LIKE 'QTY%' OR
                              fieldname LIKE 'SUM%' ).
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                    'S' dd03l-fieldname ' ',
                                    'E' 'NO_OUT'        'X'.
  ENDSELECT.

  LOOP AT it_2_hour.
    MOVE: it_2_hour TO lw_2_hour.

    AT NEW dayidx.
      CONCATENATE 'SUM' lw_2_hour-dayidx INTO lw_qty.
      CONCATENATE lw_2_hour-dazet_f+4(2) '/' lw_2_hour-dazet_f+6(2)
             INTO lw_text.
      PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                      'S' lw_qty        ' ',
                                      ' ' 'COLTEXT'     lw_text,
                                      ' ' 'QFIELDNAME'  'MEINS',
                                      'E' 'EMPHASIZE'   'C300'.

    ENDAT.

    CONCATENATE 'QTY' it_2_hour-index INTO lw_qty.
    CONCATENATE it_2_hour-dazet_f+8(2) ':' it_2_hour-dazet_f+10(2)
           INTO lw_text.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                    'S' lw_qty        ' ',
                                    ' ' 'COLTEXT'     lw_text,
                                    ' ' 'QFIELDNAME'  'MEINS',
                                    'E' 'EMPHASIZE'   'C000'.
  ENDLOOP.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                'S' 'QTYMT'       ' ',
                                ' ' 'COLTEXT'     'MITU',
                                ' ' 'QFIELDNAME'  'MEINS',
                                'E' 'EMPHASIZE'   'C300'.
ENDFORM.                    " set_screen_fields_9001
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
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
ENDFORM.                    " set_screen_fields_9002
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
*&      Form  set_screen_fields_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_fields_9002.
  DATA: lw_text(5),
        lw_qty(50).

  DATA: lw_day LIKE it_day.

  FIELD-SYMBOLS: <quantity>.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                               ' ' 'COLTEXT'     'Material Description',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SHOP'        ' ',
                                  ' ' 'COLTEXT'     'Shop',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'VSPVB'       ' ',
                                  ' ' 'COLTEXT'     'Propos.SA',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PTYPE'       ' ',
                                  ' ' 'COLTEXT'     'Pro',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'COLTEXT'     'Unit',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'TOTAL'       ' ',
                                  ' ' 'COLTEXT'     'Total',
                                  'E' 'EMPHASIZE'   'C310'.

  SELECT * FROM dd03l WHERE tabname = 'ZSMM_DISPLAY_PLAN_9001'
                        AND ( fieldname LIKE 'QTY%' OR
                              fieldname LIKE 'SUM%' ).
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                    'S' dd03l-fieldname ' ',
                                    'E' 'NO_OUT'        'X'.
  ENDSELECT.

  LOOP AT it_day.
    MOVE: it_day TO lw_day.

    AT NEW week.
      CONCATENATE 'SUM' lw_day-weekidx INTO lw_qty.
      CONCATENATE 'W'   lw_day-week    INTO lw_text.

      PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                      'S' lw_qty        ' ',
                                      ' ' 'COLTEXT'     lw_text,
                                      ' ' 'QFIELDNAME'  'MEINS',
                                      'E' 'EMPHASIZE'   'C300'.

    ENDAT.

    CONCATENATE 'QTY' it_day-index INTO lw_qty.
    CONCATENATE it_day-bdter+4(2) '/' it_day-bdter+6(2)
           INTO lw_text.
    PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                    'S' lw_qty        ' ',
                                    ' ' 'COLTEXT'     lw_text,
                                    ' ' 'QFIELDNAME'  'MEINS',
                                    'E' 'EMPHASIZE'   'C000'.
  ENDLOOP.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                'S' 'QTYMT'       ' ',
                                ' ' 'COLTEXT'     'MITU',
                                ' ' 'QFIELDNAME'  'MEINS',
                                'E' 'EMPHASIZE'   'C300'.
ENDFORM.                    " set_screen_fields_9002
*&---------------------------------------------------------------------*
*&      Form  set_display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_display_data.
  PERFORM display_progress_bar USING text-b02.

  PERFORM set_2hours_bucket.
  PERFORM set_working_day.

  LOOP AT it_itab.
    PERFORM set_9001_data.
    PERFORM set_9002_data.
    PERFORM set_9003_data.
  ENDLOOP.
ENDFORM.                    " set_display_data
*&---------------------------------------------------------------------*
*&      Form  set_9001_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_9001_data.
*---// Summary data by 2 hours
  DATA: lw_index TYPE i.

  CHECK it_itab-rtype EQ 'S' OR it_itab-rtype EQ 'M'.

  CLEAR: it_9001.

  READ TABLE it_9001 WITH KEY matnr = it_itab-matnr.
  IF sy-subrc EQ 0.
    MOVE: sy-tabix TO lw_index.

    PERFORM set_2_hour_quantity.

    MODIFY it_9001 INDEX lw_index.
  ELSE.
    MOVE: it_itab-lifnr    TO it_9001-lifnr,
          it_itab-matnr    TO it_9001-matnr,
          it_itab-maktx    TO it_9001-maktx,
          it_itab-ptype    TO it_9001-ptype,
          it_itab-arbpl(1) TO it_9001-shop,
          it_itab-vspvb    TO it_9001-vspvb,
          it_itab-meins    TO it_9001-meins.

    PERFORM set_2_hour_quantity.

    APPEND it_9001.
  ENDIF.
ENDFORM.                    " set_9001_da
*&---------------------------------------------------------------------*
*&      Form  set_2hours_bucket
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_2hours_bucket.
  DATA: lw_bdter LIKE sy-datum,
        lw_datum LIKE sy-datum,
        lw_uzeit LIKE sy-uzeit,
        lw_index TYPE i.

  SORT it_itab BY bdter datum uzeit matnr rtype..

  READ TABLE it_itab INDEX 1.

  MOVE: it_itab-bdter TO lw_bdter.
  CONCATENATE it_itab-uzeit(2) '0001' INTO lw_uzeit.

  DO.
    READ TABLE it_itab WITH KEY bdter = lw_bdter
                                rtype = 'S'
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      IF sy-index EQ 100.
        EXIT.
      ENDIF.

      lw_bdter = lw_bdter + 1.
      CONTINUE.
    ENDIF.

    lw_index = lw_index + 1.
    IF lw_index = 4.
      EXIT.
    ENDIF.

    MOVE: lw_bdter TO lw_datum.

    DO 12 TIMES.
      MOVE: lw_index TO it_2_hour-dayidx,
            lw_bdter TO it_2_hour-bdter.
      CONCATENATE lw_datum lw_uzeit INTO it_2_hour-dazet_f.

      lw_uzeit = lw_uzeit + 7200 - 1.
      IF lw_uzeit < 7200 AND lw_uzeit NE '000000'.
        lw_datum = lw_datum + 1.
      ENDIF.
      CONCATENATE lw_datum lw_uzeit INTO it_2_hour-dazet_t.

      APPEND it_2_hour.

      lw_uzeit = lw_uzeit + 1.
    ENDDO.

    lw_bdter = lw_bdter + 1.
  ENDDO.

  LOOP AT it_2_hour.
    LOOP AT it_itab WHERE bdter = it_2_hour-bdter
                      AND uzeit >= it_2_hour-dazet_f+8(6)
                      AND uzeit <= it_2_hour-dazet_t+8(6).
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      DELETE it_2_hour.
    ENDIF.
  ENDLOOP.

  LOOP AT it_2_hour.
    MOVE: sy-tabix TO it_2_hour-index.
    MODIFY it_2_hour.
  ENDLOOP.
ENDFORM.                    " set_2hours_bucket
*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_working_time.

ENDFORM.                    " get_working_time
*&---------------------------------------------------------------------*
*&      Form  SET_2_HOUR_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_2_hour_quantity.
  DATA: lw_dazet(14),
        lw_seq(2)   TYPE   n,
        lw_qty(50).

  FIELD-SYMBOLS: <quantity>.

  IF it_itab-rtype EQ 'M'.
    it_9001-qtymt = it_9001-qtymt + it_itab-bdmng.
  ELSE.
    CONCATENATE it_itab-datum it_itab-uzeit INTO lw_dazet.

    LOOP AT it_2_hour WHERE bdter = it_itab-bdter
                        AND dazet_f <= lw_dazet
                        AND dazet_t >= lw_dazet.
    ENDLOOP.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    MOVE: it_2_hour-index TO lw_seq.
    CONCATENATE 'IT_9001-QTY' lw_seq INTO lw_qty.
    ASSIGN (lw_qty) TO <quantity>.

    <quantity> = <quantity> + it_itab-bdmng.

    MOVE: it_2_hour-dayidx TO lw_seq.
    CONCATENATE 'IT_9001-SUM' lw_seq INTO lw_qty.
    ASSIGN (lw_qty) TO <quantity>.

    <quantity> = <quantity> + it_itab-bdmng.
  ENDIF.

  it_9001-total = it_9001-total + it_itab-bdmng.
ENDFORM.                    " SET_2_HOUR_QUANTITY
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


* OUTPUT MODULE FOR TABSTRIP 'TAB_9000': SETS ACTIVE TAB
MODULE tab_9000_active_tab_set OUTPUT.
  tab_9000-activetab = g_tab_9000-pressed_tab.
  CASE g_tab_9000-pressed_tab.
    WHEN c_tab_9000-tab1.
      g_tab_9000-subscreen = '9001'.
    WHEN c_tab_9000-tab2.
      g_tab_9000-subscreen = '9002'.
    WHEN c_tab_9000-tab3.
      g_tab_9000-subscreen = '9003'.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.

* INPUT MODULE FOR TABSTRIP 'TAB_9000': GETS ACTIVE TAB
MODULE tab_9000_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_9000-tab1.
      g_tab_9000-pressed_tab = c_tab_9000-tab1.
    WHEN c_tab_9000-tab2.
      g_tab_9000-pressed_tab = c_tab_9000-tab2.
    WHEN c_tab_9000-tab3.
      g_tab_9000-pressed_tab = c_tab_9000-tab3.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SET_9002_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_9002_data.
*---// Summary data by 21 Days
  DATA: lw_index TYPE i.

  CLEAR: it_9002.

  READ TABLE it_9002 WITH KEY matnr = it_itab-matnr.
  IF sy-subrc EQ 0.
    MOVE: sy-tabix TO lw_index.

    PERFORM set_21_days_quantity.

    MODIFY it_9002 INDEX lw_index.
  ELSE.
    MOVE: it_itab-lifnr    TO it_9002-lifnr,
          it_itab-matnr    TO it_9002-matnr,
          it_itab-maktx    TO it_9002-maktx,
          it_itab-ptype    TO it_9002-ptype,
          it_itab-arbpl(1) TO it_9002-shop,
          it_itab-vspvb    TO it_9002-vspvb,
          it_itab-meins    TO it_9002-meins.

    PERFORM set_21_days_quantity.

    APPEND it_9002.
  ENDIF.
ENDFORM.                    " SET_9002_DATA
*&---------------------------------------------------------------------*
*&      Form  set_working_day
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_working_day.
  DATA: lw_daytxt  LIKE hrvsched-daytxt,
        lw_week    LIKE scal-week,
        lw_weekidx TYPE i.

  LOOP AT it_itab WHERE rtype NE 'M'.
    MOVE: it_itab-bdter TO it_day-bdter.
    COLLECT it_day.
  ENDLOOP.

  SORT it_day BY bdter.

  LOOP AT it_day.
    MOVE: sy-tabix   TO it_day-index.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
         EXPORTING
              langu               = sy-langu
              date                = it_day-bdter
              calid               = 'HM'
         IMPORTING
              daytxt              = lw_daytxt
*              daynr               = lw_daynr
         EXCEPTIONS
              no_langu            = 1
              no_date             = 2
              no_daytxt_for_langu = 3
              invalid_date        = 4
              OTHERS              = 5.
    IF sy-subrc <> 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    CALL FUNCTION 'DATE_GET_WEEK'
         EXPORTING
              date         = it_day-bdter
         IMPORTING
              week         = lw_week
         EXCEPTIONS
              date_invalid = 1
              OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.


    TRANSLATE lw_daytxt TO UPPER CASE.

    MOVE: lw_daytxt(3) TO it_day-daytxt,
          lw_week      TO it_day-week.

    MODIFY it_day.
  ENDLOOP.

  READ TABLE it_day INDEX 1.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  LOOP AT it_day.
    AT NEW week.
      lw_weekidx = lw_weekidx + 1.
    ENDAT.

    MOVE: lw_weekidx TO it_day-weekidx.

    MODIFY it_day.
  ENDLOOP.
ENDFORM.                    " set_working_day
*&---------------------------------------------------------------------*
*&      Form  set_21_days_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_21_days_quantity.
  DATA: lw_dazet(14),
        lw_seq(2)   TYPE   n,
        lw_qty(50).

  FIELD-SYMBOLS: <quantity>.

  IF it_itab-rtype EQ 'M'.
    it_9002-qtymt = it_9002-qtymt + it_itab-bdmng.
  ELSE.
    CONCATENATE it_itab-datum it_itab-uzeit INTO lw_dazet.

    READ TABLE it_day WITH KEY bdter = it_itab-bdter
                      BINARY SEARCH.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    MOVE: it_day-index TO lw_seq.
    CONCATENATE 'IT_9002-QTY' lw_seq INTO lw_qty.
    ASSIGN (lw_qty) TO <quantity>.

    <quantity> = <quantity> + it_itab-bdmng.

    MOVE: it_day-weekidx TO lw_seq.
    CONCATENATE 'IT_9002-SUM' lw_seq INTO lw_qty.
    ASSIGN (lw_qty) TO <quantity>.

    <quantity> = <quantity> + it_itab-bdmng.
  ENDIF.

  it_9002-total = it_9002-total + it_itab-bdmng.
ENDFORM.                    " set_21_days_quantity
*&---------------------------------------------------------------------*
*&      Form  set_9003_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_9003_data.
  MOVE: it_itab-lifnr    TO it_9003-lifnr,
        it_itab-matnr    TO it_9003-matnr,
        it_itab-maktx    TO it_9003-maktx,
        it_itab-bdter    TO it_9003-bdter,
        it_itab-werks    TO it_9003-werks,
        it_itab-tprog    TO it_9003-tprog,
        it_itab-datum    TO it_9003-datum,
        it_itab-uzeit    TO it_9003-uzeit,
        it_itab-bdmng    TO it_9003-bdmng,
        it_itab-meins    TO it_9003-meins,
        it_itab-ptype    TO it_9003-ptype,
        it_itab-rtype    TO it_9003-rtype,
        it_itab-vspvb    TO it_9003-vspvb,
        it_itab-arbpl(1) TO it_9003-shop,
        it_itab-arbpl    TO it_9003-arbpl.

  APPEND it_9003.
ENDFORM.                    " set_9003_data
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_9003.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
endform.                    " set_attributes_alv_9003
*&---------------------------------------------------------------------*
*&      Form  set_screen_fields_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_screen_fields_9003.
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'LIFNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SHOP'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'VSPVB'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PTYPE'       ' ',
                                  'E' 'EMPHASIZE'   'C400'.
endform.                    " set_screen_fields_9003
*&---------------------------------------------------------------------*
*&      Form  display_progress_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_B01  text
*----------------------------------------------------------------------*
FORM display_progress_bar USING p_text.
  DATA: lw_text(50).

  MOVE: p_text TO lw_text.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            text = lw_text.
endform.                    " display_progress_bar
