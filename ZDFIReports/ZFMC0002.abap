*&----------------------------------------------------------------------
*& Development ID : ZCFIF04
*& Program ID     : ZFMC0002
*& Program Name   : Budget Release Cancel
*& Created by     : Byung Sung Bae
*& Created on     : 07/28/2005
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*&
*&----------------------------------------------------------------------
REPORT  zfmc0002.

INCLUDE <icon>.

TABLES: fmci, fmfctr, bppe, tbpfe, aufk, fm01, tbp1c.

TYPE-POOLS zfmcm.
DATA: zsfm0006_9000 LIKE zsfm0006_9000.

*-----/// Constants
CONSTANTS:
           c_yes                            VALUE 'J'.
*---// Global variables
DATA: v_total(5)    TYPE n,
      v_ready(5)    TYPE n,
      v_error(5)    TYPE n,
      v_success(5)  TYPE n,
      v_selected(5) TYPE n,
      v_ans.

DATA: v_gjahr LIKE fmci-gjahr.

*---// Internal tables
DATA: it_9000 TYPE STANDARD TABLE OF zsfm0006_9000 WITH HEADER LINE.

DATA: it_9000_tar TYPE STANDARD TABLE OF zsfm0006_9000 WITH HEADER LINE.

DATA: BEGIN OF it_geber OCCURS 0,
        geber   LIKE   bppe-geber,
      END   OF it_geber.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

*---// Ranges
RANGES: r_profil FOR tbp1c-profil.

*---// Constants
CONSTANTS: c_available  TYPE i          VALUE 10,
           c_versn      LIKE bppe-versn VALUE '000',
           c_sgtxt      LIKE bpdy-sgtxt VALUE 'FM Reverse document'.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS cl_gui_cfw      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: wc_control_9000   TYPE        scrfname VALUE 'CC_9000_ALV',
      wc_alv_9000       TYPE REF TO cl_gui_alv_grid,
      wc_container_9000 TYPE REF TO cl_gui_custom_container.

DATA: v_container(50),
      v_control(50),
      v_alv(50),
      v_itab(50),
      v_structure LIKE dd02l-tabname.

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
DATA : v_is_layout TYPE lvc_s_layo,
       v_variant   TYPE disvariant,          "for parameter IS_VARIANT
       v_fieldname LIKE LINE OF it_fieldname,
       v_repid     LIKE sy-repid,
       v_cnt       TYPE i,                   "Field count
       v_save      TYPE c   VALUE 'A'.   "for Parameter I_SAVE

CONSTANTS: c_structure(100) VALUE 'ZSFM0006_'.

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
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
* class lcl_event_receiver (Implementation)
CLASS lcl_event_receiver IMPLEMENTATION.
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*---// Selection screen
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_fikrs LIKE fmci-fikrs    OBLIGATORY DEFAULT zfmcm_fm_area,
            p_gjahr LIKE fmci-gjahr    OBLIGATORY DEFAULT sy-datum(4),
            p_perio TYPE fc_tperi      OBLIGATORY DEFAULT sy-datum+4(2).
*            p_profil TYPE bp_profil    OBLIGATORY DEFAULT 'Q'.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t02.
SELECT-OPTIONS: s_geber  FOR bppe-geber,
                s_fictr  FOR fmfctr-fictr,
                s_fipex  FOR fmci-fipex.
SELECTION-SCREEN END OF BLOCK bl2.

INITIALIZATION.
  PERFORM initialization.
  SET PARAMETER ID 'FIK' FIELD zfmcm_fm_area.
*---// Input value check & Read data
AT SELECTION-SCREEN ON p_perio.
  PERFORM check_input_data.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM read_data.

START-OF-SELECTION.
  READ TABLE it_9000 INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m08.
  ENDIF.
  CALL SCREEN 9000.

*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data .

  DATA: lt_budget LIKE zsfm0008 OCCURS 0 WITH HEADER LINE.

  CLEAR v_gjahr.
  IF p_perio = 1.
    v_gjahr = p_gjahr - 1.
  ELSE.
    v_gjahr = p_gjahr.
  ENDIF.

  CALL FUNCTION 'Z_FM_GET_MONTHLY_BUDGET'
    EXPORTING
      i_fikrs            = p_fikrs
*      i_gjahr            = p_gjahr
      i_gjahr            = v_gjahr
    TABLES
      t_geber            = s_geber
      t_fictr            = s_fictr
      t_fipex            = s_fipex
      t_profil           = r_profil
      t_itab             = lt_budget
    EXCEPTIONS
      no_fm_area         = 1
      no_fund            = 2
      no_funds_center    = 3
      no_commitment_item = 4
      no_profile         = 5
      no_category        = 6
      no_original        = 7
      OTHERS             = 8.
  CASE sy-subrc.
    WHEN 1.
      MESSAGE e000(zz) WITH text-m02.
    WHEN 2.
      MESSAGE e000(zz) WITH text-m03.
    WHEN 3.
      MESSAGE e000(zz) WITH text-m04.
    WHEN 4.
      MESSAGE e000(zz) WITH text-m05.
    WHEN 5.
      MESSAGE e000(zz) WITH text-m06.
    WHEN 6.
      MESSAGE e000(zz) WITH text-m07.
    WHEN 7.
      MESSAGE e000(zz) WITH text-m08.
    WHEN 8.
      MESSAGE e000(zz) WITH text-m09.
  ENDCASE.

  SORT lt_budget BY geber fictr fipex ctgry.
  LOOP AT lt_budget WHERE   ctgry  EQ c_available
                      AND ( profil EQ 'M' OR
                            profil EQ 'Q' OR
                            profil EQ 'H' OR
                            profil EQ 'Y' OR
                            profil EQ 'F'   ).
    PERFORM check_budget_profile USING lt_budget.
*    PERFORM get_total USING lt_budget.
*--->> end of modi by zbyszek 20070417

*<<--- start of modi by zbyszek 20070417 (request from Rado)
    CHECK lt_budget-total > 0.
*--->> end of modi by zbyszek 20070417

    CLEAR: it_9000.
    MOVE-CORRESPONDING lt_budget TO it_9000.

    IF it_9000-total IS INITIAL.
      MOVE: icon_led_green  TO it_9000-icon.
      v_success = v_success + 1.
    ELSE.
      MOVE: icon_led_yellow TO it_9000-icon.
      v_ready = v_ready + 1.
    ENDIF.

    v_total = v_total + 1.

    APPEND it_9000.
  ENDLOOP.
*Amount zero line data delete " 20071102 insert
  DELETE it_9000 WHERE wtp01  = 0 AND
                     wtp02  = 0 AND
                     wtp03  = 0 AND
                     wtp04  = 0 AND
                     wtp05  = 0 AND
                     wtp06  = 0 AND
                     wtp07  = 0 AND
                     wtp08  = 0 AND
                     wtp09  = 0 AND
                     wtp10  = 0 AND
                     wtp11  = 0 AND
                     wtp12  = 0.

  READ TABLE it_9000 INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

ENDFORM.                    " read_data
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
    WHEN 'EXECUTE'.
      CLEAR: sy-ucomm.
      PERFORM execute_rtn.
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
  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'GEBER'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'FICTR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'FIPEX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ICON'        ' ',
                                  'E' 'KEY'         'X'.
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
      i_structure_name = v_structure
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
*      MESSAGE e000(zz) WITH 'Check filed catalog'.
      MESSAGE e000(zz) WITH text-m14.
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
*&      Form  EXECUTE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_rtn .
  PERFORM get_target_data USING sy-dynnr.
  PERFORM execute_fr51.
  PERFORM assign_itab_to_alv USING sy-dynnr.
ENDFORM.                    " EXECUTE_RTN
*&---------------------------------------------------------------------*
*&      Form  get_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_target_data USING pv_dynnr.
  "/Indexes of Selected Rows
  DATA: lt_rows   TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows

  CONCATENATE: 'WC_ALV_' pv_dynnr INTO v_alv.
  ASSIGN: (v_alv) TO <alv>.

  CALL METHOD <alv>->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows[]
      et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m10.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m11.
  ENDIF.

  PERFORM set_target_data TABLES lt_rows.
ENDFORM.                    " get_target_data
*&---------------------------------------------------------------------*
*&      Form  set_target_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ROWS  text
*----------------------------------------------------------------------*
FORM set_target_data TABLES pt_rows STRUCTURE lvc_s_row.
  CLEAR: v_selected.
  CLEAR: it_9000_tar, it_9000_tar[].

  LOOP AT pt_rows WHERE index NE 0.
    READ TABLE it_9000 INDEX pt_rows-index.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    v_selected = v_selected + 1.

    CHECK it_9000-icon EQ icon_led_yellow OR
          it_9000-icon EQ icon_led_red.

    MOVE it_9000 TO it_9000_tar.

    it_9000_tar-wtp01 = it_9000_tar-wtp01 * -1.
    it_9000_tar-wtp02 = it_9000_tar-wtp02 * -1.
    it_9000_tar-wtp03 = it_9000_tar-wtp03 * -1.
    it_9000_tar-wtp04 = it_9000_tar-wtp04 * -1.
    it_9000_tar-wtp05 = it_9000_tar-wtp05 * -1.
    it_9000_tar-wtp06 = it_9000_tar-wtp06 * -1.
    it_9000_tar-wtp07 = it_9000_tar-wtp07 * -1.
    it_9000_tar-wtp08 = it_9000_tar-wtp08 * -1.
    it_9000_tar-wtp09 = it_9000_tar-wtp09 * -1.
    it_9000_tar-wtp10 = it_9000_tar-wtp10 * -1.

    it_9000_tar-wtp11 = it_9000_tar-wtp11 * -1.
    it_9000_tar-wtp12 = it_9000_tar-wtp12 * -1.

    APPEND it_9000_tar.
  ENDLOOP.
ENDFORM.                    " set_target_data
*&---------------------------------------------------------------------*
*&      Form  check_budget_profile
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_budget_profile USING ps_budget STRUCTURE zsfm0008.
  DATA: lv_loop_cnt   TYPE i,
        lv_month(2)   TYPE n,
        lv_montx(2)   TYPE n,
        lv_times      TYPE sy-index,
        lv_amount(50).

  FIELD-SYMBOLS: <amount>.

  CASE ps_budget-profil.
    WHEN 'M'.
      lv_month    = p_perio.
    WHEN 'Q'.
      CASE p_perio.
        WHEN '001' OR '002' OR '003'.
          lv_month = '01'.
        WHEN '004' OR '005' OR '006'.
          lv_month = '04'.
        WHEN '007' OR '008' OR '009'.
          lv_month = '07'.
        WHEN '010' OR '011' OR '012'.
          lv_month = '10'.
      ENDCASE.
    WHEN 'H'.
      CASE p_perio.
        WHEN '001' OR '002' OR '003' OR '004' OR '005' OR '006'.
          lv_month = '01'.
        WHEN '007' OR '008' OR '009' OR '010' OR '011' OR '012'.
          lv_month = '07'.
      ENDCASE.
    WHEN 'Y' OR 'F'.
      lv_month = '01'.
  ENDCASE.

  IF v_gjahr = p_gjahr.
    lv_loop_cnt = 12 - lv_month + 1.
    lv_montx = lv_month.
  ELSE.
    lv_loop_cnt = 0.
    lv_montx    = 12.
  ENDIF.

  DO lv_loop_cnt TIMES.
    CONCATENATE 'PS_BUDGET-WTP' lv_month INTO lv_amount.
    ASSIGN (lv_amount) TO <amount>.

    CLEAR: <amount>.

    lv_month = lv_month + 1.
  ENDDO.

*>>-- 20070417 ZB Comment : changed the logic//
**    We don't need to consider Monthly -available budget
*> clear minus amount.
  lv_times = lv_montx.
  DO lv_times TIMES.
    CLEAR lv_amount.
    lv_montx = sy-index.
    CONCATENATE 'PS_BUDGET-WTP' lv_montx INTO lv_amount.
    ASSIGN (lv_amount) TO <amount>.
*    IF <amount> < 0.
*      CLEAR: <amount>.
*    ENDIF.
  ENDDO.
*<<-- 20070417 ZB

ENDFORM.                    " check_budget_profile
*&---------------------------------------------------------------------*
*&      Form  GET_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_total USING ps_budget STRUCTURE zsfm0008.
  ps_budget-total = ps_budget-wtp01 + ps_budget-wtp07 +
                    ps_budget-wtp02 + ps_budget-wtp08 +
                    ps_budget-wtp03 + ps_budget-wtp09 +
                    ps_budget-wtp04 + ps_budget-wtp10 +
                    ps_budget-wtp05 + ps_budget-wtp11 +
                    ps_budget-wtp06 + ps_budget-wtp12.
ENDFORM.                    " GET_TOTAL
*&---------------------------------------------------------------------*
*&      Form  execute_FR51
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_fr51 .

  DATA: text1(40),
        text2(40),
        titl(40).

  text1 = text-m13.
*  text2 = text-m14.
  titl  = text-m15.
  PERFORM popup_confirm   USING text1 text2 titl
                                'X'   'A'   v_ans.
  CHECK v_ans = c_yes.

  PERFORM get_fund.

  LOOP AT it_geber.
    PERFORM generate_bdc.
    PERFORM posting_rtn.
  ENDLOOP.
  LOOP AT it_9000_tar WHERE geber = ''.
    PERFORM generate_bdc_expence.
    PERFORM posting_rtn.
  ENDLOOP.

  PERFORM update_it_9000.
  PERFORM count_status.

  MESSAGE s000(zz) WITH v_selected text-m12.
ENDFORM.                    " execute_FR51
*&---------------------------------------------------------------------*
*&      Form  generate_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc .
  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0200',
     ' ' 'FMDY-FIKRS'   p_fikrs,              "FM Area
*     ' ' 'BPDY-GJAHR'   p_gjahr,              "Fiscal year
     ' ' 'BPDY-GJAHR'   v_gjahr,              "Fiscal year
     ' ' 'BPDY-VERSN'   c_versn,              "Version
     ' ' 'FMDY-FINCODE' it_geber-geber,       "Fund
     ' ' 'BDC_OKCODE'   '/00',

     'X' 'SAPLKBPB'     '0400',               "Detail screen
     ' ' 'BDC_OKCODE'   '=DOCH',              "Header

     'X' 'SAPLKBPB'     '0150',               "Header
     ' ' 'BPDY-SGTXT'   c_sgtxt,              "Text
     ' ' 'BDC_OKCODE'   '/00'.


  LOOP AT it_9000_tar WHERE geber = it_geber-geber.
    WRITE: it_9000_tar-wtp01 CURRENCY it_9000_tar-waers
                             TO lv_peri1_01(21),
           it_9000_tar-wtp02 CURRENCY it_9000_tar-waers
                             TO lv_peri1_02(21),
           it_9000_tar-wtp03 CURRENCY it_9000_tar-waers
                             TO lv_peri1_03(21),
           it_9000_tar-wtp04 CURRENCY it_9000_tar-waers
                             TO lv_peri1_04(21),
           it_9000_tar-wtp05 CURRENCY it_9000_tar-waers
                             TO lv_peri1_05(21),
           it_9000_tar-wtp06 CURRENCY it_9000_tar-waers
                             TO lv_peri1_06(21),
           it_9000_tar-wtp07 CURRENCY it_9000_tar-waers
                             TO lv_peri1_07(21),
           it_9000_tar-wtp08 CURRENCY it_9000_tar-waers
                             TO lv_peri1_08(21),
           it_9000_tar-wtp09 CURRENCY it_9000_tar-waers
                             TO lv_peri1_09(21),
           it_9000_tar-wtp10 CURRENCY it_9000_tar-waers
                             TO lv_peri1_10(21),
           it_9000_tar-wtp11 CURRENCY it_9000_tar-waers
                             TO lv_peri1_11(21),
           it_9000_tar-wtp12 CURRENCY it_9000_tar-waers
                             TO lv_peri1_12(21).
    PERFORM dynpro USING:
       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=INSL',              "Insert

       'X' 'SAPLKBPB'         '0400',               "Detail Screen
       ' ' 'FMDY-FICTR(01)'   it_9000_tar-fictr,    "Fund Center
       ' ' 'FMDY-FIPEX(01)'   it_9000_tar-fipex,    "Commitment Item
       ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
       ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
       ' ' 'BDC_OKCODE'       '=PERI',

       'X' 'SAPLKBPP'         '0600',               "Period Screen
       ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
       ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
       ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
       ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
       ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
       ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
       ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
       ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
       ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
       ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
       ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
       ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
       ' ' 'G_SCREEN_0600-DK' '0',                  "DK
       ' ' 'BDC_OKCODE'  '=CLOS'.
  ENDLOOP.

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',                 "Detail Screen
     ' ' 'BDC_OKCODE'       '=POST'.                "Save
ENDFORM.                    " generate_bdc
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
*&      Form  get_fund
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fund .
  CLEAR: it_geber, it_geber[].

  LOOP AT it_9000_tar WHERE geber NE space.
    MOVE: it_9000_tar-geber TO it_geber-geber.

    COLLECT it_geber.
  ENDLOOP.
ENDFORM.                    " get_fund
*&---------------------------------------------------------------------*
*&      Form  posting_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_rtn .
  CALL TRANSACTION 'FR51'  USING it_bdc
                           OPTIONS FROM wa_opt.
  IF sy-subrc NE 0 OR sy-msgno NE '043'.
    MOVE: icon_led_red TO it_9000_tar-icon.
    PERFORM get_err_msg USING it_9000_tar-zmsg.

    MODIFY it_9000_tar TRANSPORTING icon zmsg
                              WHERE geber = it_geber-geber.
  ELSE.
    MOVE: icon_led_green TO it_9000_tar-icon,
          sy-msgv1       TO it_9000_tar-docnr.
    MODIFY it_9000_tar TRANSPORTING icon docnr zmsg
                              WHERE geber = it_geber-geber.
  ENDIF.
ENDFORM.                    " posting_rtn
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000_ZMSG  text
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
*&      Form  update_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_it_9000.
  SORT it_9000 BY geber fictr fipex.

  LOOP AT it_9000_tar.
    READ TABLE it_9000 WITH KEY geber = it_9000_tar-geber
                                fictr = it_9000_tar-fictr
                                fipex = it_9000_tar-fipex
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    MOVE: it_9000_tar-icon  TO it_9000-icon,
          it_9000_tar-zmsg  TO it_9000-zmsg,
          it_9000_tar-docnr TO it_9000-docnr.

    MODIFY it_9000 INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " update_it_9000
*&---------------------------------------------------------------------*
*&      Form  count_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM count_status .
  CLEAR: v_total, v_ready, v_error, v_success.

  LOOP AT it_9000.
    CASE it_9000-icon.
      WHEN icon_led_yellow.
        v_ready   = v_ready   + 1.
      WHEN icon_led_red.
        v_error   = v_error   + 1.
      WHEN icon_led_green.
        v_success = v_success + 1.
    ENDCASE.

    v_total = v_total + 1.
  ENDLOOP.
ENDFORM.                    " count_status
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization .
*> 2005/10/18 Delete logic.
*  MOVE: 'I'   TO r_profil-sign,
*        'EQ'  TO r_profil-option.
*  MOVE: 'M'   TO r_profil-low. APPEND r_profil.
*  MOVE: 'Q'   TO r_profil-low. APPEND r_profil.
*  MOVE: 'H'   TO r_profil-low. APPEND r_profil.
*< 2005/10/18
*---// BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  popup_confirm
*&---------------------------------------------------------------------*
FORM popup_confirm USING   value(p_txt1)
                           value(p_txt2)
                           value(p_titl)
                           value(p_cancel_flg)
                           value(p_def_val)
                           p_ans.

  CLEAR p_ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      defaultoption  = p_def_val
      textline1      = p_txt1
      textline2      = p_txt2
      titel          = p_titl
      cancel_display = p_cancel_flg
    IMPORTING
      answer         = p_ans.

ENDFORM.                    " POPUP_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  check_input_data
*&---------------------------------------------------------------------*
FORM check_input_data .

  IF p_perio < 1 OR p_perio > 12.
    MESSAGE e000(zz) WITH text-e01.
  ENDIF.

ENDFORM.                    " check_input_data
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_expence
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_expence .
  DATA: lv_peri1_01(21), lv_peri1_02(21), lv_peri1_03(21),
        lv_peri1_04(21), lv_peri1_05(21), lv_peri1_06(21),
        lv_peri1_07(21), lv_peri1_08(21), lv_peri1_09(21),
        lv_peri1_10(21), lv_peri1_11(21), lv_peri1_12(21).

  CLEAR: it_bdc, it_bdc[].

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'     '0200',
     ' ' 'FMDY-FIKRS'   p_fikrs,              "FM Area
*     ' ' 'BPDY-GJAHR'   p_gjahr,              "Fiscal year
     ' ' 'BPDY-GJAHR'   v_gjahr,              "Fiscal year
     ' ' 'BPDY-VERSN'   c_versn,              "Version
     ' ' 'FMDY-FINCODE' it_9000_tar-geber,       "Fund
     ' ' 'BDC_OKCODE'   '/00',

     'X' 'SAPLKBPB'     '0400',               "Detail screen
     ' ' 'BDC_OKCODE'   '=DOCH',              "Header

     'X' 'SAPLKBPB'     '0150',               "Header
     ' ' 'BPDY-SGTXT'   c_sgtxt,              "Text
     ' ' 'BDC_OKCODE'   '/00'.


  WRITE: it_9000_tar-wtp01 CURRENCY it_9000_tar-waers
                           TO lv_peri1_01(21),
         it_9000_tar-wtp02 CURRENCY it_9000_tar-waers
                           TO lv_peri1_02(21),
         it_9000_tar-wtp03 CURRENCY it_9000_tar-waers
                           TO lv_peri1_03(21),
         it_9000_tar-wtp04 CURRENCY it_9000_tar-waers
                           TO lv_peri1_04(21),
         it_9000_tar-wtp05 CURRENCY it_9000_tar-waers
                           TO lv_peri1_05(21),
         it_9000_tar-wtp06 CURRENCY it_9000_tar-waers
                           TO lv_peri1_06(21),
         it_9000_tar-wtp07 CURRENCY it_9000_tar-waers
                           TO lv_peri1_07(21),
         it_9000_tar-wtp08 CURRENCY it_9000_tar-waers
                           TO lv_peri1_08(21),
         it_9000_tar-wtp09 CURRENCY it_9000_tar-waers
                           TO lv_peri1_09(21),
         it_9000_tar-wtp10 CURRENCY it_9000_tar-waers
                           TO lv_peri1_10(21),
         it_9000_tar-wtp11 CURRENCY it_9000_tar-waers
                           TO lv_peri1_11(21),
         it_9000_tar-wtp12 CURRENCY it_9000_tar-waers
                           TO lv_peri1_12(21).
  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',               "Detail Screen
     ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
     ' ' 'BDC_OKCODE'       '=INSL',              "Insert

     'X' 'SAPLKBPB'         '0400',               "Detail Screen
     ' ' 'FMDY-FICTR(01)'   it_9000_tar-fictr,    "Fund Center
     ' ' 'FMDY-FIPEX(01)'   it_9000_tar-fipex,    "Commitment Item
     ' ' 'BPDY-SPRED1(01)'  '0',                  "DK
     ' ' 'G_TABLECON_INFO-MARK(01)' 'X',          "Check
     ' ' 'BDC_OKCODE'       '=PERI',

     'X' 'SAPLKBPP'         '0600',               "Period Screen
     ' ' 'BPDY-PERI1(01)'   lv_peri1_01,          " 01
     ' ' 'BPDY-PERI1(02)'   lv_peri1_02,          " 02
     ' ' 'BPDY-PERI1(03)'   lv_peri1_03,          " 03
     ' ' 'BPDY-PERI1(04)'   lv_peri1_04,          " 04
     ' ' 'BPDY-PERI1(05)'   lv_peri1_05,          " 05
     ' ' 'BPDY-PERI1(06)'   lv_peri1_06,          " 06
     ' ' 'BPDY-PERI1(07)'   lv_peri1_07,          " 07
     ' ' 'BPDY-PERI1(08)'   lv_peri1_08,          " 08
     ' ' 'BPDY-PERI1(09)'   lv_peri1_09,          " 09
     ' ' 'BPDY-PERI1(10)'   lv_peri1_10,          " 10
     ' ' 'BPDY-PERI1(11)'   lv_peri1_11,          " 11
     ' ' 'BPDY-PERI1(12)'   lv_peri1_12,          " 12
     ' ' 'G_SCREEN_0600-DK' '0',                  "DK
     ' ' 'BDC_OKCODE'  '=CLOS'.

  PERFORM dynpro USING:
     'X' 'SAPLKBPB'         '0400',                 "Detail Screen
     ' ' 'BDC_OKCODE'       '=POST'.                "Save

ENDFORM.                    " generate_bdc_expence
