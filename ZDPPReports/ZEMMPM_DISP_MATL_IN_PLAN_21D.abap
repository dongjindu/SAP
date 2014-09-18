REPORT zemmpm_disp_matl_in_plan_21d MESSAGE-ID zmmm.
* Modification Logs
* Date       Developer    RequestNo   Description
* *********************************************************************
* 12/19/2011 Valerian     UD1K953580  HMMA Engine Plant split
*                                     implementation
* *********************************************************************


INCLUDE: <icon>.

TABLES: mara.

DATA: zsmm_display_plan_21day_9000 LIKE zsmm_display_plan_21day_9000.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsmm_display_plan_21day_9000
                                     WITH HEADER LINE.
DATA: BEGIN OF it_day OCCURS 0,
        seq(3)    TYPE c,
        datum  LIKE sy-datum,
      END OF it_day.
*---// Constants
CONSTANTS : c_check                          VALUE 'X'.

*---// Ranges
RANGES: r_lgort FOR resb-lgort,
        r_profl FOR mara-profl,
        r_shop  FOR it_9000-shop,
        r_dispo FOR marc-dispo.

*---// For Listbox variable
TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.
DATA: wa_total(6).

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

CONSTANTS: c_structure(100) VALUE 'ZSMM_DISPLAY_PLAN_21DAY_'.

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
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001',
            p_dispo LIKE marc-dispo,
            p_shop AS LISTBOX VISIBLE LENGTH 13,
            p_lgort LIKE mard-lgort.
SELECT-OPTIONS: s_vspvb FOR it_9000-prvbe NO-EXTENSION NO INTERVALS.
PARAMETERS:     p_type(3)                 AS LISTBOX VISIBLE LENGTH 13.
SELECT-OPTIONS: s_lifnr FOR it_9000-lifnr,
                s_matnr FOR it_9000-matnr,
                s_maktx FOR it_9000-maktx.
SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_listbox.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM make_21days.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM display_rtn.

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
  PERFORM set_listbox_type.
ENDFORM.                    " set_listbox
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
*  LOOP AT SCREEN.
*    IF screen-name = 'P_WERKS'.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " screen_modify
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

  MOVE: 'E'          TO value-key,
        'Engine Shop'  TO value-text.
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
  PERFORM check_ptype.
  PERFORM check_shop.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_ptype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ptype.
  CLEAR:  r_lgort,  r_profl,
          r_lgort[],r_profl[],
          r_dispo,  r_dispo[].
  CASE p_type.
    WHEN 'JIS'.
      MOVE: 'I'    TO r_lgort-sign,
            'EQ'   TO r_lgort-option,
            'P500' TO r_lgort-low.
      APPEND r_lgort.
    WHEN 'JIT'.
      MOVE: 'I'    TO r_lgort-sign,
            'EQ'   TO r_lgort-option,
            'P400' TO r_lgort-low.
      APPEND r_lgort.
    WHEN 'K' OR 'V' OR 'M'.
      MOVE: 'I'    TO r_profl-sign,
            'EQ'   TO r_profl-option,
            p_type TO r_profl-low.
      APPEND r_profl.
  ENDCASE.

*
  IF NOT p_lgort IS INITIAL.
    MOVE: 'I'     TO r_lgort-sign,
          'EQ'   TO r_lgort-option,
          p_lgort  TO r_lgort-low.
    APPEND r_lgort.
  ENDIF.
* MRP controller
  IF NOT p_dispo IS INITIAL.
    MOVE: 'I'     TO r_dispo-sign,
           'EQ'   TO r_dispo-option,
           p_dispo  TO r_dispo-low.
    APPEND r_dispo.

  ENDIF.
ENDFORM.                    " check_ptype
*&---------------------------------------------------------------------*
*&      Form  check_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_shop.
* BEGIN OF UD1K953580
  IF ( P_SHOP = 'E' AND ( P_WERKS NE 'E001' OR P_WERKS NE 'E002' ) ) OR
     ( P_SHOP NE 'E' AND ( P_WERKS EQ 'E001' OR P_WERKS EQ 'E002' ) AND
       p_shop ne space ).
    MESSAGE E000 WITH 'Plant and Shop must be same'.
  ENDIF.

*  IF ( P_SHOP = 'E' AND P_WERKS NE 'E001' ) OR
*      ( P_SHOP NE 'E' AND P_WERKS EQ 'E001' and p_shop ne space ).
*    MESSAGE E000 WITH 'Plant and Shop must be same'.
*  ENDIF.
* END OF UD1K953580

  CHECK p_shop NE space.
  CLEAR: r_shop, r_shop[].
  MOVE: 'I'     TO r_shop-sign,
        'EQ'    TO r_shop-option,
        p_shop  TO r_shop-low.

  APPEND r_shop.
ENDFORM.                    " check_shop
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_9000
    FROM ztmm_parts_21day AS a INNER JOIN makt AS b
                                  ON a~matnr = b~matnr
                                 AND b~spras = sy-langu
   WHERE a~werks EQ p_werks
     AND a~shop  IN r_shop
     AND a~prvbe IN s_vspvb
     AND a~lgort IN r_lgort
     AND a~profl IN r_profl
     AND a~lifnr IN s_lifnr
     AND a~matnr IN s_matnr
     AND b~maktx IN s_maktx
     AND a~dispo IN r_dispo.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  LOOP AT it_9000.
    it_9000-sum = it_9000-d01 + it_9000-d02 + it_9000-d03 +
                  it_9000-d04 + it_9000-d05 + it_9000-d06 +
                  it_9000-d07 + it_9000-d08 + it_9000-d09 +
                  it_9000-d10 + it_9000-d11 + it_9000-d12 +
                  it_9000-d13 + it_9000-d14 + it_9000-d15 +
                  it_9000-d16 + it_9000-d17 + it_9000-d18 +
                  it_9000-d19 + it_9000-d20 + it_9000-d21.
    MODIFY it_9000.
  ENDLOOP.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  display_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_rtn.
  SORT it_9000 BY lifnr shop prvbe matnr.
  PERFORM set_seq.
  DESCRIBE TABLE it_9000 LINES wa_total.
  CALL SCREEN 9000.
ENDFORM.                    " display_rtn
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
    WHEN '9000'.
      PERFORM set_attributes_alv_9000.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_9100
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
ENDFORM.                    " set_attributes_alv_9100
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
  PERFORM change_header.
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
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'ITEMNO'      ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'LIFNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'LGORT'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PROFL'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SHOP'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'PRVBE'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SORTF'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MEINS'       ' ',
                                  'E' 'EMPHASIZE'   'C400'.
ENDFORM.                    " set_screen_fields_9000
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
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0836   text
*      -->P_0837   text
*      -->P_0838   text
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
*&      Form  set_listbox_type
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox_type.
  CLEAR: name, value, list.

  name = 'P_TYPE'.

  MOVE: 'JIS'   TO value-key,
        'JIS'   TO value-text.
  APPEND value  TO list.

  MOVE: 'JIT'   TO value-key,
        'JIT'   TO value-text.
  APPEND value  TO list.

  MOVE: 'K'     TO value-key,
        'KD'    TO value-text.
  APPEND value  TO list.

  MOVE: 'V'     TO value-key,
        'LP'    TO value-text.
  APPEND value  TO list.

  MOVE: 'M'     TO value-key,
        'MIP'   TO value-text.
  APPEND value  TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.
ENDFORM.                    " set_listbox_type
*&---------------------------------------------------------------------*
*&      Form  make_21days
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_21days.
  DATA: l_count(2) TYPE n.
  DATA: l_date   LIKE sy-datum.
  DATA: wa_kalid  LIKE kako-kalid.

* get the first date
  SELECT SINGLE erdat INTO l_date
   FROM ztmm_parts_21day
   WHERE erdat NE '00000000'.
  IF sy-subrc NE 0.
    MESSAGE e009 WITH 'Can not find the data creation date'.
  ENDIF.

* reading working calendar
  PERFORM read_shop_calid  USING wa_kalid.
* first is current inputed date
  l_count = '01'.
  CONCATENATE 'D' l_count INTO it_day-seq.
  it_day-datum = l_date.
  APPEND it_day.

  DO 20 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    CONCATENATE 'D' l_count INTO it_day-seq.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
  ENDDO.

ENDFORM.                    " make_21days
*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  change_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_header.
  DATA: wa_fc   LIKE LINE OF it_fieldname.
  DATA: wa_fieldcat  LIKE it_fieldcat .

  DATA: l_text(10).
  LOOP AT it_fieldname INTO wa_fc.

    READ TABLE it_day WITH KEY seq = wa_fc-fieldname.
    IF sy-subrc EQ 0.
      CLEAR: it_fieldcat.
      WRITE it_day-datum TO l_text USING EDIT MASK '__/__/____'.
      it_fieldcat-fieldname = wa_fc-fieldname.
      w_cnt = w_cnt + 1.
      it_fieldcat-col_pos = w_cnt.
      it_fieldcat-scrtext_m = l_text(5).
      it_fieldcat-colddictxt = 'M'.
      APPEND  it_fieldcat.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " change_header
*&---------------------------------------------------------------------*
*&      Form  SET_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_seq.
  DATA: item TYPE i.
  item = 1.
  LOOP AT it_9000.
    it_9000-itemno = item.
    MODIFY it_9000.
    item = item + 1.
  ENDLOOP.
ENDFORM.                    " SET_SEQ
