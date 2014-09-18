************************************************************************
* Program Name      : ZEMMPM28E_NSTL_FINAL
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.05.19.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K910554
* Addl Documentation:
* Description       : Daily Supply to Line (Non Supply to Line)
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.05.19.     Sung-Tae Lim     UD1K910554     Initial Coding
*
*
************************************************************************
REPORT zemmpm28e_nstl_final_bk01 .
INCLUDE <icon>.
TABLES: mara,
        ztmm_nstl_log.

DATA: zemmpm28e_nstl_final_9001 LIKE zemmpm28e_nstl_final_9001,
      zemmpm28e_nstl_final_9002 LIKE zemmpm28e_nstl_final_9002,
      zemmpm28e_nstl_final_9003 LIKE zemmpm28e_nstl_final_9003,
      zemmpm28e_nstl_final_9004 LIKE zemmpm28e_nstl_final_9004,
      zemmpm28e_nstl_final_9005 LIKE zemmpm28e_nstl_final_9005,
      zemmpm28e_nstl_final_9100 LIKE zemmpm28e_nstl_final_9002.

*---// Internal tables
DATA: it_worktime  LIKE zsmm_working_time OCCURS 0
                                          WITH HEADER LINE.

DATA: it_vin       LIKE zspp_vin_info_for_nstl   OCCURS 0
                                                 WITH HEADER LINE.

DATA: it_dvrt      LIKE ztmm_dvrt OCCURS 0 WITH HEADER LINE.

DATA: it_itab      LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.
DATA: it_itab_curr LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.

DATA: it_ztmm_nstl LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.
DATA: it_nstl_curr LIKE ztmm_nstl OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_nstl OCCURS 0.
        INCLUDE STRUCTURE ztmm_nstl.
DATA:   zsum        LIKE mseg-menge,           "Quantity sum
      END OF it_nstl.

DATA: BEGIN OF it_shift OCCURS 0,
        datum   LIKE   sy-datum,
        rpsta   LIKE   ztmm_nstl-rpsta,
        tprog   LIKE   kapa-tprog,
        zone(2),                              "Time zone
      END   OF it_shift.

DATA: it_master LIKE zemmpm28e_nstl_final_9005 OCCURS 0
                                               WITH HEADER LINE.

DATA: BEGIN OF it_bferr OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   t001w-werks,
        bwart   LIKE   affw-bwart,
        erfmg   LIKE   affw-erfmg,            "B/F error Qty
      END   OF it_bferr.

DATA: it_open_to LIKE zemmpm28e_nstl_final_9003 OCCURS 0
                                                WITH HEADER LINE.
DATA: BEGIN OF it_open_to_matnr OCCURS 0,
        werks   LIKE   marc-werks,
        matnr   LIKE   mara-matnr,
      END   OF it_open_to_matnr.

DATA: it_9001 TYPE STANDARD TABLE OF zemmpm28e_nstl_final_9001,
      it_9002 TYPE STANDARD TABLE OF zemmpm28e_nstl_final_9002,
      it_9003 TYPE STANDARD TABLE OF zemmpm28e_nstl_final_9003,
      it_9004 TYPE STANDARD TABLE OF zemmpm28e_nstl_final_9004,
      it_9005 TYPE STANDARD TABLE OF zemmpm28e_nstl_final_9005,
      it_9100 TYPE STANDARD TABLE OF zemmpm28e_nstl_final_9002.

DATA: w_9001  LIKE zemmpm28e_nstl_final_9001,
      w_9002  LIKE zemmpm28e_nstl_final_9002,
      w_9003  LIKE zemmpm28e_nstl_final_9003,
      w_9004  LIKE zemmpm28e_nstl_final_9004,
      w_9005  LIKE zemmpm28e_nstl_final_9005,
      w_9100  LIKE zemmpm28e_nstl_final_9002.

DATA: BEGIN OF bdc_tab OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdc_tab.

DATA: BEGIN OF wa_opt OCCURS 0.
        INCLUDE STRUCTURE ctu_params.
DATA: END OF wa_opt.

*---// Work area : Global variables & Structures
DATA : BEGIN OF w_order OCCURS 0,
         datum LIKE ztmm_dvrt-datum,
         matnr LIKE mara-matnr,
         uzeit LIKE ztmm_dvrt-uzeit,
         bdmng LIKE resb-bdmng,
         meins LIKE resb-meins,
         tprog LIKE ztmm_dvrt-tprog,
         wkord LIKE ztmm_dvrt_866-wkord,
       END OF w_order.

DATA: w_matnr_f     LIKE   mara-matnr,  "From material
      w_matnr_t     LIKE   mara-matnr,  "To material
      w_datum       LIKE   sy-datum,    "Current date
      w_uzeit       LIKE   sy-uzeit,    "Current time
      w_to_date     LIKE   sy-datum,    "Current shift date
      w_to_tprog    LIKE   kapa-tprog,  "Current shift
      w_next_date   LIKE   sy-datum,    "Next shift date
      w_next_tprog  LIKE   kapa-tprog,  "Next shift
      w_req_date    LIKE   sy-datum,    "Requirement end date
      w_curr_idx(2) TYPE   n,           "Current Time Zone
      w_last_idx(2) TYPE   n,           "Last Time Zone
      w_remain_idx(2) TYPE n,           "Remain Time Zone
      w_open_total    TYPE i,           "Total open T/O count
      w_open_success  TYPE i,           "Successfully deleted count
      w_open_failed   TYPE i,           "Delition open T/O failed count
      w_to_total      TYPE i,           "Total new T/O count
      w_to_success    TYPE i,           "Successfully created count
      w_to_stk_err    TYPE i,           "Stock is not available
      w_to_bdc_err    TYPE i,           "T/O creation failed
      p_arbpl         LIKE crhd-arbpl.
*---// Ranges

*---// For Listbox variable
TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

*---// Constants
CONSTANTS : c_day       TYPE i               VALUE 10,
            c_check                          VALUE 'X',
            c_time_plus TYPE t               VALUE '000001',
            c_mtart     LIKE mara-mtart      VALUE 'ROH',
            c_spptl     LIKE ztmm_mast-spptl VALUE 'N',
            c_max_zone  TYPE i               VALUE 20,    "Max time zone
            c_lgort     LIKE mseg-lgort      VALUE 'P400',
            c_hour      TYPE i               VALUE 60,
            c_icon_success(4)                VALUE icon_green_light,
            c_icon_fail(4)                   VALUE icon_red_light,
            c_icon_no_stock(4)               VALUE icon_yellow_light,
            c_icon_warning(4)                VALUE icon_yellow_light,
            c_interval  LIKE inri-nrrangenr  VALUE '09',
            c_object    LIKE inri-object     VALUE 'ZMMNRO0002'.

*---// For Tab Strip
* FUNCTION CODES FOR TABSTRIP 'TAB_9000'
CONSTANTS: BEGIN OF c_tab_9000,
             tab1 LIKE sy-ucomm VALUE 'TAB_9000_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_9000_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_9000_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAB_9000_FC4',
             tab5 LIKE sy-ucomm VALUE 'TAB_9000_FC5',
           END OF c_tab_9000.
* DATA FOR TABSTRIP 'TAB_9000'
CONTROLS:  tab_9000 TYPE TABSTRIP.
DATA:      BEGIN OF g_tab_9000,
             subscreen   LIKE sy-dynnr,
             prog       LIKE sy-repid VALUE 'ZEMMPM28E_NSTL_FINAL_BK01',
             pressed_tab LIKE sy-ucomm VALUE c_tab_9000-tab2,
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

DATA: wc_control_9004   TYPE        scrfname VALUE 'CC_9004_ALV',
      wc_alv_9004       TYPE REF TO cl_gui_alv_grid,
      wc_container_9004 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9005   TYPE        scrfname VALUE 'CC_9005_ALV',
      wc_alv_9005       TYPE REF TO cl_gui_alv_grid,
      wc_container_9005 TYPE REF TO cl_gui_custom_container.

DATA: wc_control_9100   TYPE        scrfname VALUE 'CC_9100_ALV',
      wc_alv_9100       TYPE REF TO cl_gui_alv_grid,
      wc_container_9100 TYPE REF TO cl_gui_custom_container.

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
  METHOD handle_double_click.
    PERFORM dbl_click_9000 USING e_column-fieldname
                                 es_row_no-row_id.

  ENDMETHOD.                           "handle_double_click

*  METHOD  handle_data_changed.
*  ENDMETHOD.
*
*  METHOD  handle_user_command.
*  ENDMETHOD.
ENDCLASS.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT : 1(60) text-t02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT : 1(60) text-t03.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-t04.
PARAMETERS : p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001',
             p_rp    LIKE ausp-atwrt OBLIGATORY AS LISTBOX
                                                VISIBLE LENGTH 14.
*             p_arbpl LIKE crhd-arbpl OBLIGATORY AS LISTBOX
*                                                VISIBLE LENGTH 14.
SELECTION-SCREEN ULINE.
PARAMETERS : p_shift  AS CHECKBOX USER-COMMAND z_uc,
             p_opento AS CHECKBOX DEFAULT 'X' MODIF ID gr1,
             p_tocrea AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN ULINE.
SELECT-OPTIONS : s_matnr FOR mara-matnr NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK bl2.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_listbox.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_rtn.
  PERFORM get_data.

START-OF-SELECTION.
  PERFORM insert_nstl_data.
  PERFORM read_master_data.
  PERFORM delete_open_to.
  PERFORM create_to.
  PERFORM display_screen.

*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  PERFORM check_matnr.
  PERFORM check_rp.
  PERFORM check_date_n_time.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.

  CLEAR: s_matnr.
  READ TABLE s_matnr INDEX 1.

  IF     s_matnr-low EQ ' ' AND s_matnr-high EQ ' '.
    w_matnr_t = 'ZZZZZZZZZZZZZZZZZ'.
  ELSEIF s_matnr-low EQ ' ' AND s_matnr-high NE ' '.
    w_matnr_t = s_matnr-high.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high EQ ' '.
    w_matnr_f = w_matnr_t = s_matnr-low.
  ELSEIF s_matnr-low NE ' ' AND s_matnr-high NE ' '.
    w_matnr_f = s_matnr-low. w_matnr_t = s_matnr-high.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_rp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rp.
*  IF NOT ( p_rp EQ '01' OR p_rp EQ '02' OR p_rp EQ '06' ).
*    SET CURSOR FIELD 'P_RP'.
*    MESSAGE e000(zz) WITH text-m02.
*  ENDIF.

  CASE p_rp.
    WHEN '01'.
      MOVE: 'B' TO p_arbpl.
    WHEN '02'.
      MOVE: 'P' TO p_arbpl.
    WHEN '06'.
      MOVE: 'T' TO p_arbpl.
  ENDCASE.
ENDFORM.                    " check_rp
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM get_working_time.
  PERFORM set_current_time_zone.
  PERFORM read_vehicle_master.
  PERFORM makt_dvrt_table.
  PERFORM read_requirement.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_working_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_working_time.
*---// Read working tiem from PP standard
  CLEAR : it_worktime, it_worktime[].

  PERFORM display_progress_bar USING text-b02.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
       EXPORTING
            i_datum              = w_datum
            i_day                = c_day
            i_arbpl              = p_arbpl
       IMPORTING
            e_date_curr          = w_to_date
            e_tprog_curr         = w_to_tprog
            e_date_next          = w_next_date
            e_tprog_next         = w_next_tprog
       TABLES
            t_working_time       = it_worktime
       EXCEPTIONS
            cannot_read_dayname  = 1
            incorrect_shift_info = 2
            incorrect_capa_info  = 3
            OTHERS               = 4.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e000(zz) WITH text-m04.
      WHEN 2.
        MESSAGE e000(zz) WITH text-m05.
      WHEN 3.
        MESSAGE e000(zz) WITH text-m06.
      WHEN 4.
        MESSAGE e000(zz) WITH text-m07.
    ENDCASE.
  ENDIF.

  READ TABLE it_worktime INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m08.
  ENDIF.

*---// Set worktime for display
  LOOP AT it_worktime.
    MOVE: it_worktime-datum      TO w_9004-datum,
          it_worktime-tprog      TO w_9004-tprog,
          it_worktime-index      TO w_9004-index,
          it_worktime-wofrm(8)   TO w_9004-wosdt,
          it_worktime-wofrm+8(6) TO w_9004-wostm,
          it_worktime-woend(8)   TO w_9004-woedt,
          it_worktime-woend+8(6) TO w_9004-woetm,
          it_worktime-brfrm+8(6) TO w_9004-bkstm,
          it_worktime-brend+8(6) TO w_9004-bketm.

    w_9004-opmin = it_worktime-opsec / 60.

    APPEND w_9004 TO it_9004.
  ENDLOOP.

*---// Select effective worktime
  IF p_shift EQ c_check.
    READ TABLE it_worktime WITH KEY datum = w_to_date
                                    tprog = w_to_tprog.
    IF sy-subrc EQ 0.
      DELETE it_worktime WHERE shidx < it_worktime-shidx.
    ENDIF.
  ELSE.
    READ TABLE it_worktime WITH KEY datum = w_to_date
                                    tprog = w_to_tprog.
    IF sy-subrc EQ 0.
      DELETE it_worktime WHERE shidx <= it_worktime-shidx.
    ENDIF.
  ENDIF.

*---// Set effective date & Shift
  LOOP AT it_worktime.
    READ TABLE it_shift WITH KEY datum = it_worktime-datum
                                 rpsta = p_rp
                                 tprog = it_worktime-tprog.
    IF sy-subrc EQ 0.
      MOVE: it_worktime-datum TO it_shift-datum,
            p_rp              TO it_shift-rpsta,
            it_worktime-tprog TO it_shift-tprog,
            it_worktime-index TO it_shift-zone.
      MODIFY it_shift INDEX sy-tabix.
    ELSE.
      MOVE: it_worktime-datum TO it_shift-datum,
            p_rp              TO it_shift-rpsta,
            it_worktime-tprog TO it_shift-tprog,
            it_worktime-index TO it_shift-zone.
      APPEND it_shift.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_working_time
*&---------------------------------------------------------------------*
*&      Form  check_date_n_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_date_n_time.
  MOVE: sy-datum TO w_datum,
        sy-uzeit TO w_uzeit.
ENDFORM.                    " check_date_n_time
*&---------------------------------------------------------------------*
*&      Form  read_vehicle_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vehicle_master.
  CLEAR: it_vin, it_vin[].

  PERFORM display_progress_bar USING text-b01.

  CALL FUNCTION 'Z_FPP_GET_NON_SUPPLY_TO_LINE'
       EXPORTING
            i_werks                  = p_werks
            i_atwrt                  = p_rp
            i_date                   = w_datum
            i_time                   = w_uzeit
            i_test                   = 'X'
            i_arbpl                  = p_arbpl
       TABLES
            t_supply_info            = it_vin
       EXCEPTIONS
            no_data_founded          = 1
            line_info_does_not_exist = 2
            etc_exception            = 3
            uph_info_does_not_exist  = 4
            OTHERS                   = 5.

  IF sy-subrc NE 0.
    CASE sy-subrc.
      WHEN 1.
        MESSAGE e000(zz) WITH text-m09.
      WHEN 2.
        MESSAGE e000(zz) WITH text-m10.
      WHEN 3.
        MESSAGE e000(zz) WITH text-m11.
      WHEN 4.
        MESSAGE e000(zz) WITH text-m12.
      WHEN 5.
        MESSAGE e000(zz) WITH text-m13.
    ENDCASE.
  ENDIF.

  DELETE it_vin WHERE rsnum EQ space.

  READ TABLE it_vin INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m09.
  ENDIF.
ENDFORM.                    " read_vehicle_master
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
ENDFORM.                    " display_progress_bar
*&---------------------------------------------------------------------*
*&      Form  makt_dvrt_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM makt_dvrt_table.
  PERFORM display_progress_bar USING text-b03.

*--- delete DVRT Table
  DELETE FROM ztmm_dvrt WHERE plnum >= space.

*--- insert DVRT Table
  LOOP AT it_vin.
    CLEAR: it_dvrt.

    MOVE-CORRESPONDING it_vin TO it_dvrt.
    MOVE : it_vin-p_rp06(8)   TO it_dvrt-datum,
           it_vin-p_rp06+8(6) TO it_dvrt-uzeit,
           sy-uname           TO it_dvrt-ernam,
           sy-datum           TO it_dvrt-erdat,
           sy-uzeit           TO it_dvrt-erzet,
           sy-uname           TO it_dvrt-aenam,
           sy-datum           TO it_dvrt-aedat,
           sy-uzeit           TO it_dvrt-aezet.

    APPEND it_dvrt.

*---// Set requirement end date
    IF it_vin-p_rp06(8) > w_req_date OR
       w_req_date IS INITIAL.
      MOVE: it_vin-p_rp06(8) TO w_req_date.
    ENDIF.
  ENDLOOP.

  INSERT ztmm_dvrt FROM TABLE it_dvrt ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m14.
  ENDIF.

  COMMIT WORK AND WAIT.
ENDFORM.                    " makt_dvrt_table
*&---------------------------------------------------------------------*
*&      Form  read_requirement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_requirement.
  PERFORM display_progress_bar USING text-b04.

  CASE p_rp.
    WHEN '01'.
      PERFORM read_rp01.
    WHEN '02'.
      PERFORM read_rp02.
    WHEN '06'.
      PERFORM read_rp06.
  ENDCASE.
ENDFORM.                    " read_requirement
*&---------------------------------------------------------------------*
*&      Form  APPEND_PLANNED_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_planned_order.
  DATA: lw_daytime(14),
        lw_timezone(2) TYPE n,
        lw_quantity(14).

  FIELD-SYMBOLS: <quantity>.

  CLEAR: it_itab.

  MOVE: w_order-matnr TO it_itab-matnr,
        w_order-meins TO it_itab-meins.

  CONCATENATE w_order-datum w_order-uzeit INTO lw_daytime.

  LOOP AT it_worktime WHERE wofrm <= lw_daytime
                        AND woend >= lw_daytime.
  ENDLOOP.
  IF sy-subrc NE 0.
    IF p_shift EQ c_check.
      MESSAGE e000(zz) WITH text-m01.
    ELSE.
      CLEAR: w_order.
      EXIT.
    ENDIF.
  ENDIF.

  MOVE: it_worktime-datum TO it_itab-datum,
        p_rp              TO it_itab-rpsta,
        it_worktime-tprog TO it_itab-kaptprog,
        it_worktime-index TO lw_timezone.

  CONCATENATE 'IT_ITAB-TIME' lw_timezone INTO lw_quantity.

  ASSIGN (lw_quantity) TO <quantity>.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m15 text-m16.
  ENDIF.

  MOVE: w_order-bdmng TO <quantity>.

  COLLECT it_itab.

  CLEAR: w_order.
ENDFORM.                    " APPEND_PLANNED_ORDER
*&---------------------------------------------------------------------*
*&      Form  READ_RP01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rp01.
  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
    SELECT /*+ ORDERED*/
           /*+ INDEX(C RESB________M)*/
           D.DATUM,  B.MATNR,  D.UZEIT,  C.BDMNG,  C.MEINS,
           D.TPROG,  D.WKORD
      INTO :W_ORDER
      FROM ZTMM_MAST A, MARA B,  RESB C,  ZTMM_DVRT D
     WHERE A.MANDT = :SY-MANDT
       AND A.WERKS = :P_WERKS
       AND A.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
       AND A.SPPTL = :C_SPPTL
       AND A.ZLINE LIKE 'B%'
       AND B.MANDT =  A.MANDT
       AND B.MATNR =  A.MATNR
       AND B.MTART =  :C_MTART
       AND C.MANDT =  B.MANDT
       AND C.MATNR =  B.MATNR
       AND C.WERKS =  :P_WERKS
       AND C.XLOEK =  ' '
       AND C.KZEAR >= ' '
*       AND C.BDTER BETWEEN :SY-DATUM AND :W_REQ_DATE
*       AND C.BDTER >= :SY-DATUM
       AND D.MANDT =  C.MANDT
       AND D.RSNUM =  C.RSNUM
  ENDEXEC.
ENDFORM.                                                    " READ_RP01
*&---------------------------------------------------------------------*
*&      Form  READ_RP02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rp06.
*  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
*    SELECT /*+ ORDERED*/
*           /*+ INDEX(C RESB________M)*/
*           D.DATUM,  C.MATNR,  D.UZEIT,  C.BDMNG,  C.MEINS,
*           D.TPROG
*      INTO :W_ORDER
*      FROM ZTMM_DVRT D, RESB C
*     WHERE D.MANDT = :SY-MANDT
*       AND C.MANDT = D.MANDT
*       AND C.RSNUM = D.RSNUM
*       AND C.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
*       AND C.WERKS =  :P_WERKS
*       AND C.XLOEK =  ' '
*       AND C.KZEAR >= ' '
*  ENDEXEC.


  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
    SELECT /*+ ORDERED*/
           /*+ INDEX(C RESB________M)*/
           D.DATUM,  B.MATNR,  D.UZEIT,  C.BDMNG,  C.MEINS,
           D.TPROG,  D.WKORD
      INTO :W_ORDER
      FROM ZTMM_MAST A, MARA B,  RESB C,  ZTMM_DVRT D
     WHERE A.MANDT = :SY-MANDT
       AND A.WERKS = :P_WERKS
       AND A.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
       AND A.SPPTL = :C_SPPTL
       AND NOT SUBSTR(A.ZLINE,1,1) IN ('B','P')
       AND B.MANDT =  A.MANDT
       AND B.MATNR =  A.MATNR
       AND B.MTART =  :C_MTART
       AND C.MANDT =  B.MANDT
       AND C.MATNR =  B.MATNR
       AND C.WERKS =  :P_WERKS
       AND C.XLOEK =  ' '
       AND C.KZEAR >= ' '
*       AND C.BDTER BETWEEN :SY-DATUM AND :W_REQ_DATE
*       AND C.BDTER >= :SY-DATUM
       AND D.MANDT =  C.MANDT
       AND D.RSNUM =  C.RSNUM
  ENDEXEC.

*  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
*    SELECT /*+ ORDERED*/
*           Z.DATUM,  X.MATNR,  Z.UZEIT,  Y.BDMNG,  Y.MEINS,
*           Z.TPROG
*           INTO :W_ORDER
*           FROM ZTMM_DVRT Z, RESB Y, MARA X, ZTMM_MAST W
**- Z & Y
*          WHERE Z.MANDT = :SY-MANDT
*            AND Y.MANDT = Z.MANDT
*            AND Y.RSNUM = Z.RSNUM
*            AND X.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
**- Y & X
*            AND X.MANDT = Y.MANDT
*            AND X.MATNR = Y.MATNR
*            AND X.MTART = :C_MTART
**- X & W
*            AND W.MANDT = X.MANDT
*            AND W.MATNR = X.MATNR
*            AND W.WERKS = Y.WERKS
*            AND W.SPPTL = :C_SPPTL
*            AND SUBSTR(W.ZLINE,1,1) != 'B'
*  ENDEXEC.

*  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
*    SELECT /*+ ORDERED*/
*           A.DATUM,  B.MATNR,  A.UZEIT,  B.BDMNG,  B.MEINS,
*           A.TPROG
*      INTO :W_ORDER
*      FROM ZTMM_DVRT A, RESB B, ZTMM_MAST C, MARA D
*     WHERE A.MANDT = :SY-MANDT
*       AND B.MANDT = A.MANDT
*       AND B.RSNUM = A.RSNUM
*       AND C.MANDT = B.MANDT
*       AND C.MATNR = B.MATNR
*       AND C.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
*       AND C.WERKS = B.WERKS
*       AND C.SPPTL = :C_SPPTL
*       AND NOT C.ZLINE LIKE 'B%'
*       AND D.MANDT = C.MANDT
*       AND D.MATNR = C.MATNR
*       AND D.MTART = :C_MTART
*  ENDEXEC.
ENDFORM.                                                    " READ_RP02
*&---------------------------------------------------------------------*
*&      Form  INSERT_NSTL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_nstl_data.
  PERFORM display_progress_bar USING text-b05.

  IF p_shift EQ c_check.                     "Current Shift
    PERFORM insert_nstl_current_shift.
  ELSE.
    PERFORM insert_nstl_normal.
  ENDIF.
ENDFORM.                    " INSERT_NSTL_DATA
*&---------------------------------------------------------------------*
*&      Form  insert_nstl_normal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_nstl_normal.
  PERFORM read_table_ztmm_nstl.
  PERFORM set_new_requirement.
  PERFORM update_table.
ENDFORM.                    " insert_nstl_normal
*&---------------------------------------------------------------------*
*&      Form  insert_nstl_current_shift
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_nstl_current_shift.
  PERFORM read_table_ztmm_nstl.
  PERFORM mix_current_shift_quantity.
  PERFORM set_new_requirement.

  APPEND LINES OF it_nstl_curr TO it_ztmm_nstl.

  PERFORM update_table.
ENDFORM.                    " insert_nstl_current_shift
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox.
  PERFORM set_listbox_rp.
  PERFORM set_listbox_arbpl.
ENDFORM.                    " SET_LISTBOX
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_ARBPL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox_arbpl.
  CLEAR: name, value, list.

  name = 'P_ARBPL'.

  MOVE: 'B'         TO value-key,
        'Body shop' TO value-text.
  APPEND value TO list.

  MOVE: 'P'         TO value-key,
        'Paint shop' TO value-text.
  APPEND value TO list.

  MOVE: 'T'         TO value-key,
        'Trim shop' TO value-text.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  set_listbox_rp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox_rp.
  CLEAR: name, value, list.

  name = 'P_RP'.

  MOVE: '01'      TO value-key,
        'BODY IN' TO value-text.
  APPEND value TO list.

  MOVE: '02'        TO value-key,
        'PAINT IN'  TO value-text.
  APPEND value      TO list.

  MOVE: '06'      TO value-key,
        'PBS OUT' TO value-text.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.
ENDFORM.                    " set_listbox_rp
*&---------------------------------------------------------------------*
*&      Form  READ_TABLE_ZTMM_NSTL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_table_ztmm_nstl.
  CLEAR: it_ztmm_nstl, it_ztmm_nstl[].

  SELECT * INTO TABLE it_ztmm_nstl
    FROM ztmm_nstl
     FOR ALL ENTRIES IN it_shift
   WHERE datum    EQ it_shift-datum
     AND rpsta    EQ it_shift-rpsta
     AND kaptprog EQ it_shift-tprog
     AND matnr    IN s_matnr.

  LOOP AT it_ztmm_nstl.
    MOVE: p_werks            TO it_open_to_matnr-werks,
          it_ztmm_nstl-matnr TO it_open_to_matnr-matnr.
    COLLECT it_open_to_matnr.
  ENDLOOP.
ENDFORM.                    " READ_TABLE_ZTMM_NSTL
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_REQUIREMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_new_requirement.
  DATA: lw_index(2)       TYPE   n,
        lw_time_itab(50),
        lw_time_nstl(50).

  FIELD-SYMBOLS: <time_itab>, <time_nstl>.

  SORT it_itab BY datum rpsta kaptprog matnr.
  LOOP AT it_ztmm_nstl.
    READ TABLE it_itab WITH KEY datum    = it_ztmm_nstl-datum
                                rpsta    = it_ztmm_nstl-rpsta
                                kaptprog = it_ztmm_nstl-kaptprog
                                matnr    = it_ztmm_nstl-matnr
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_ztmm_nstl.
      CONTINUE.
    ENDIF.

    DO.
      MOVE: sy-index TO lw_index.

      CONCATENATE: 'IT_ITAB-TIME'      lw_index INTO lw_time_itab,
                   'IT_ZTMM_NSTL-TIME' lw_index INTO lw_time_nstl.

      ASSIGN: (lw_time_itab) TO <time_itab>.
      IF sy-subrc NE 0. EXIT. ENDIF.

      ASSIGN: (lw_time_nstl) TO <time_nstl>.
      IF sy-subrc NE 0. EXIT. ENDIF.

      MOVE: <time_itab> TO <time_nstl>.
    ENDDO.

    MOVE: sy-uname TO it_ztmm_nstl-aenam,
          sy-datum TO it_ztmm_nstl-aedat,
          sy-uzeit TO it_ztmm_nstl-aezet.

    DELETE it_itab INDEX sy-tabix.
    MODIFY it_ztmm_nstl.
  ENDLOOP.

  LOOP AT it_itab.
    READ TABLE it_ztmm_nstl WITH KEY datum    = it_itab-datum
                                rpsta    = it_itab-rpsta
                                kaptprog = it_itab-kaptprog
                                matnr    = it_itab-matnr
                       BINARY SEARCH.

    CHECK sy-subrc NE 0.

    MOVE: it_itab  TO it_ztmm_nstl,
          sy-uname TO it_ztmm_nstl-ernam,
          sy-datum TO it_ztmm_nstl-erdat,
          sy-uzeit TO it_ztmm_nstl-erzet,
          sy-uname TO it_ztmm_nstl-aenam,
          sy-datum TO it_ztmm_nstl-aedat,
          sy-uzeit TO it_ztmm_nstl-aezet.

    APPEND it_ztmm_nstl.
  ENDLOOP.
ENDFORM.                    " SET_NEW_REQUIREMENT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  DATA: lt_ztmm_nstl LIKE it_ztmm_nstl OCCURS 0 WITH HEADER LINE.
  DATA: lw_mod TYPE i.

  LOOP AT it_shift.
    DELETE FROM ztmm_nstl WHERE datum    EQ it_shift-datum
                            AND rpsta    EQ p_rp
                            AND kaptprog EQ it_shift-tprog
                            AND matnr    IN s_matnr.
  ENDLOOP.

  LOOP AT it_ztmm_nstl.
    lw_mod = sy-tabix / 1000.
    MOVE: it_ztmm_nstl TO lt_ztmm_nstl.
    APPEND lt_ztmm_nstl.

*{   DELETE         UP2K900022                                        1
*\    IF lw_mod EQ 0.
*\    INSERT ztmm_nstl FROM TABLE lt_ztmm_nstl ACCEPTING DUPLICATE KEYS
    .
*\      IF sy-subrc NE 0.
*\        ROLLBACK WORK.
*\        MESSAGE e000(zz) WITH text-m18.
*\      ENDIF.
*}   DELETE

*{   DELETE         UP2K900022                                        3
*\      CLEAR: lt_ztmm_nstl, lt_ztmm_nstl[].
*\    ENDIF.
*}   DELETE
  ENDLOOP.
*{   INSERT         UP2K900022                                        2
  INSERT ztmm_nstl FROM TABLE lt_ztmm_nstl ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m18.
  ENDIF.

*}   INSERT

  COMMIT WORK AND WAIT.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  MIX_CURRENT_SHIFT_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mix_current_shift_quantity.
  DATA: lw_1t_qty_old(50),
        lw_1t_qty_new(50),
        lw_time_zone(2) TYPE n.

  FIELD-SYMBOLS: <1t_qty_old>,
                 <1t_qty_new>.

*---// Set current shift quantity
  LOOP AT it_ztmm_nstl WHERE datum    = w_to_date
                         AND rpsta    = p_rp
                         AND kaptprog = w_to_tprog.
    MOVE it_ztmm_nstl TO it_nstl_curr.

    APPEND it_nstl_curr.
    DELETE it_ztmm_nstl.
  ENDLOOP.

  LOOP AT it_itab WHERE datum    = w_to_date
                    AND rpsta    = p_rp
                    AND kaptprog = w_to_tprog.
    MOVE it_itab TO it_itab_curr.

    APPEND it_itab_curr.
    DELETE it_itab.
  ENDLOOP.

*---// Mix table quantity and Vehicle Master quantity FOR CURRENT SHIFT
*---// 01 ~ current time zone : Table quantity(old data)
*---// Remain time zone : Vehicle Master quantity(new data)
  SORT it_itab_curr BY datum rpsta kaptprog matnr.
  LOOP AT it_nstl_curr.
    CLEAR: it_itab_curr.
    READ TABLE it_itab_curr WITH KEY datum    = it_nstl_curr-datum
                                     rpsta    = it_nstl_curr-rpsta
                                     kaptprog = it_nstl_curr-kaptprog
                                     matnr    = it_nstl_curr-matnr.
    IF sy-subrc NE 0.
      DO w_remain_idx TIMES.
        lw_time_zone = w_curr_idx + sy-index.

        CONCATENATE 'IT_NSTL_CURR-TIME' lw_time_zone INTO lw_1t_qty_old.

        ASSIGN (lw_1t_qty_old) TO <1t_qty_old>.

        CLEAR: <1t_qty_old>.
      ENDDO.
    ELSE.
      DO w_remain_idx TIMES.
        lw_time_zone = w_curr_idx + sy-index.

       CONCATENATE: 'IT_ITAB_CURR-TIME' lw_time_zone INTO lw_1t_qty_new,
                    'IT_NSTL_CURR-TIME' lw_time_zone INTO lw_1t_qty_old.

        ASSIGN: (lw_1t_qty_old) TO <1t_qty_old>,
                (lw_1t_qty_new) TO <1t_qty_new>.

        MOVE: <1t_qty_new>       TO <1t_qty_old>.
      ENDDO.

      DELETE it_itab_curr INDEX sy-tabix.
    ENDIF.

    MODIFY it_nstl_curr.
  ENDLOOP.

  LOOP AT it_itab_curr.
    READ TABLE it_nstl_curr WITH KEY datum    = it_itab_curr-datum
                                     rpsta    = it_itab_curr-rpsta
                                     kaptprog = it_itab_curr-kaptprog
                                     matnr    = it_itab_curr-matnr
                            BINARY SEARCH.

    CHECK sy-subrc NE 0.

    DO w_curr_idx TIMES.
      MOVE sy-index TO lw_time_zone.

      CONCATENATE: 'IT_ITAB_CURR-TIME' lw_time_zone INTO lw_1t_qty_new.

      ASSIGN: (lw_1t_qty_new) TO <1t_qty_new>.

      CLEAR: <1t_qty_new>.
    ENDDO.

    MOVE: it_itab_curr  TO it_nstl_curr,
          sy-uname      TO it_nstl_curr-ernam,
          sy-datum      TO it_nstl_curr-erdat,
          sy-uzeit      TO it_nstl_curr-erzet,
          sy-uname      TO it_nstl_curr-aenam,
          sy-datum      TO it_nstl_curr-aedat,
          sy-uzeit      TO it_nstl_curr-aezet.

    APPEND it_nstl_curr.
  ENDLOOP.
ENDFORM.                    " MIX_CURRENT_SHIFT_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  set_current_time_zone
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_current_time_zone.
  DATA: lw_daytime(14),
        lw_date    LIKE   sy-datum,
        lw_tprog   LIKE   kapa-tprog.

*---// Get current time zone
  CONCATENATE sy-datum sy-uzeit INTO lw_daytime.

  LOOP AT it_worktime WHERE datum =  w_to_date
                        AND tprog =  w_to_tprog
                        AND wofrm <= lw_daytime
                        AND woend >= lw_daytime.
    MOVE: it_worktime-index TO w_curr_idx.

    w_remain_idx = c_max_zone - w_curr_idx.
  ENDLOOP.
  IF sy-subrc NE 0.
    LOOP AT it_worktime WHERE datum = w_to_date
                          AND tprog = w_to_tprog
                          AND woend < lw_daytime.
      MOVE: it_worktime-index TO w_curr_idx.

      w_remain_idx = c_max_zone - w_curr_idx.
    ENDLOOP.
  ENDIF.

*---// Get last time zone

  IF p_tocrea EQ c_check.
    IF p_shift EQ c_check.
      MOVE: w_to_date  TO lw_date,
            w_to_tprog TO lw_tprog.
    ELSE.
      MOVE: w_next_date  TO lw_date,
            w_next_tprog TO lw_tprog.
    ENDIF.

    READ TABLE it_shift WITH KEY datum = lw_date
                                 rpsta = p_rp
                                 tprog = lw_tprog.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m23.
    ENDIF.

    MOVE: it_shift-zone TO w_last_idx.
  ELSE.
    LOOP AT it_shift.
      IF it_shift-zone > w_last_idx.
        MOVE: it_shift-zone TO w_last_idx.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " set_current_time_zone
*&---------------------------------------------------------------------*
*&      Form  create_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_to.
  PERFORM display_progress_bar USING text-b06.

  PERFORM read_calculation_data.
  PERFORM calculate_feeding_cycle.
  PERFORM calculate_stock_n_others.

  CHECK p_tocrea EQ c_check.

  PERFORM generate_bdc_lt01.
ENDFORM.                    " create_to
*&---------------------------------------------------------------------*
*&      Form  read_calculation_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_calculation_data.
  PERFORM read_bf_error_quantity.
  PERFORM read_planned_quantity.
ENDFORM.                    " read_calculation_data
*&---------------------------------------------------------------------*
*&      Form  read_planned_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_planned_quantity.
  DATA: lw_time_zone(2) TYPE n,
        lw_time(50).

  FIELD-SYMBOLS: <time>.

  CASE p_rp.
    WHEN '01'.
      PERFORM read_planned_quantity_rp01.
    WHEN '02'.
      PERFORM read_planned_quantity_rp02.
    WHEN '06'.
      PERFORM read_planned_quantity_rp06.
  ENDCASE.

  LOOP AT it_nstl.
    DO w_curr_idx TIMES.
      MOVE: sy-index TO lw_time_zone.
      CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_time.
      ASSIGN (lw_time) TO <time>.

      CLEAR: <time>.
    ENDDO.

    DO c_max_zone TIMES.
      MOVE: sy-index TO lw_time_zone.
      CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_time.
      ASSIGN (lw_time) TO <time>.

      it_nstl-zsum = it_nstl-zsum + <time>.

    ENDDO.
    MODIFY it_nstl.
  ENDLOOP.
ENDFORM.                    " read_planned_quantity
*&---------------------------------------------------------------------*
*&      Form  read_current_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_stock_n_target_bin.
  DATA: BEGIN OF ls_stock,
          matnr   LIKE   mara-matnr,            "Material
          werks   LIKE   t001w-werks,           "Plant
          lgnum   LIKE   lqua-lgnum,            "Warehouse
          lgtyp   LIKE   pkhd-lgtyp,            "Stroage type
          lgpla   LIKE   pkhd-lgpla,            "Stroage bin
          gesme   LIKE   lqua-gesme,            "Stock
          meins   LIKE   lqua-meins,            "UoM
        END   OF ls_stock.

  LOOP AT it_master.
    CLEAR: ls_stock.

  SELECT SINGLE a~matnr a~werks a~lgnum a~lgtyp a~lgpla b~meins b~gesme
      INTO CORRESPONDING FIELDS OF ls_stock
      FROM pkhd AS a LEFT OUTER JOIN lqua AS b
                             ON a~mandt EQ b~mandt
                            AND a~matnr EQ b~matnr
                            AND a~lgtyp EQ b~lgtyp
                            AND a~lgpla EQ b~lgpla
                            AND b~bestq EQ space
     WHERE a~werks EQ it_master-werks
       AND a~matnr EQ it_master-matnr.
    IF sy-subrc EQ 0.
      MOVE: ls_stock-lgtyp TO it_master-nltyp,
            ls_stock-lgpla TO it_master-nlpla,
            ls_stock-gesme TO it_master-gesme,
            ls_stock-meins TO it_master-meins,
            ls_stock-lgnum TO it_master-lgnum.

      MODIFY it_master.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_current_stock
*&---------------------------------------------------------------------*
*&      Form  read_open_to_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_open_to_quantity.
  SELECT matnr stdat stuzt endat enuzt vsola altme werks a~lgnum
         vltyp vlpla nltyp nlpla a~tanum tapos bdatu bzeit
         pvqui nista
    INTO CORRESPONDING FIELDS OF TABLE it_open_to
    FROM ltap AS a INNER JOIN ltak AS b
                      ON a~mandt = b~mandt
                     AND a~lgnum = b~lgnum
                     AND a~tanum = b~tanum
     FOR ALL ENTRIES IN it_master
   WHERE a~lgnum EQ it_master-lgnum
     AND a~matnr EQ it_master-matnr
     AND a~werks EQ it_master-werks
     AND a~nltyp EQ it_master-nltyp
     AND a~nlpla EQ it_master-nlpla
     AND a~pquit EQ space.

  PERFORM set_open_to_status.
*  PERFORM check_current_zone_open_to.

  SORT it_open_to BY matnr stdat stuzt endat endat bdatu bzeit.
ENDFORM.                    " read_open_to_quantity
*&---------------------------------------------------------------------*
*&      Form  read_bf_error_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_bf_error_quantity.
  CLEAR: it_bferr, it_bferr[].

  SELECT matnr werks bwart erfmg
    INTO CORRESPONDING FIELDS OF TABLE it_bferr
    FROM affw
     FOR ALL ENTRIES IN it_master
   WHERE matnr EQ it_master-matnr
     AND werks EQ p_werks
     AND lgort EQ c_lgort
     AND bwart IN ('261','262').

  LOOP AT it_bferr.
    READ TABLE it_master WITH KEY matnr = it_bferr-matnr
                                  werks = it_bferr-werks
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    IF it_bferr-bwart EQ '262'.
      it_bferr-erfmg = it_bferr-erfmg * -1.
    ENDIF.

    it_master-bferr = it_master-bferr + it_bferr-erfmg.

    MODIFY it_master INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " read_bf_error_quantity
*&---------------------------------------------------------------------*
*&      Form  calculate_feeding_cycle
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_feeding_cycle.
*---// If current shift is checked, create T/O from next time zone.

  DATA: lw_cycle         TYPE i,
        lw_quantity      LIKE mseg-menge,
        lw_time_zone(2)  TYPE n,
        lw_cycle_zone(2) TYPE n,
        lw_req_qty(50),
        lw_cycle_qty(50).

  FIELD-SYMBOLS: <req_qty>, <cycle_qty>.

  LOOP AT it_nstl.
    READ TABLE it_master WITH KEY matnr = it_nstl-matnr
                                  werks = p_werks
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    lw_cycle = it_master-feed_cycle / c_hour.

    DO.
      MOVE: sy-index TO lw_time_zone.
      CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_req_qty.
      ASSIGN (lw_req_qty) TO <req_qty>.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      CHECK <req_qty> > 0.

      CLEAR: lw_quantity.

      DO lw_cycle TIMES.
        lw_cycle_zone = sy-index + lw_time_zone - 1.

        CONCATENATE 'IT_NSTL-TIME' lw_cycle_zone INTO lw_cycle_qty.
        ASSIGN (lw_cycle_qty) TO <cycle_qty>.

        lw_quantity = lw_quantity + <cycle_qty>.

        CLEAR: <cycle_qty>.
      ENDDO.

      MOVE: lw_quantity TO <req_qty>.

      MODIFY it_nstl.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " calculate_feeding_cycle
*&---------------------------------------------------------------------*
*&      Form  calculate_stock_n_others
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_stock_n_others.
  DATA: lw_eff_zone(2)  TYPE n,                "From Effective Time Zone
        lw_zone_cnt(2)  TYPE n,
        lw_time_zone(2) TYPE n,
        lw_req_qty(50),
        lw_src_stock    LIKE mseg-menge,       "Source bin current stock
        lw_tar_stock    LIKE mseg-menge,       "Target bin current stock
        lw_toqty        LIKE mseg-menge.

  FIELD-SYMBOLS: <req_qty>.

  LOOP AT it_nstl.
    READ TABLE it_master WITH KEY matnr = it_nstl-matnr
                                  werks = p_werks
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    PERFORM get_effective_time_zone USING lw_eff_zone lw_zone_cnt.

    MOVE: it_master-srcst TO lw_src_stock,
          it_master-gesme TO lw_tar_stock.

    DO lw_zone_cnt TIMES.
      lw_time_zone = sy-index + lw_eff_zone - 1.

      CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_req_qty.
      ASSIGN (lw_req_qty) TO <req_qty>.

      CHECK <req_qty> > 0.

      IF sy-index EQ 1.
        PERFORM calculate_to_quantity USING lw_toqty lw_tar_stock
                                            <req_qty>.
        PERFORM calculate_rounding_value USING lw_time_zone lw_toqty
                                               it_master-rdmng.
      ELSE.
        IF lw_tar_stock NE it_master-gesme."First To <= Target Bin Stock
          PERFORM calculate_to_qty_over_stock USING lw_toqty
                                                    lw_tar_stock
                                                    <req_qty>.
        ELSE.
          lw_toqty = <req_qty>.
        ENDIF.
        PERFORM calculate_rounding_value USING lw_time_zone lw_toqty
                                               it_master-rdmng.
      ENDIF.

      PERFORM set_to_data USING it_nstl it_master lw_toqty
                                lw_time_zone lw_src_stock.

      MOVE: lw_toqty TO <req_qty>.

      MODIFY it_nstl.
    ENDDO.
  ENDLOOP.
ENDFORM.                    " calculate_stock_n_others
*&---------------------------------------------------------------------*
*&      Form  calculate_rounding_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_rounding_value USING pw_time_zone pw_toqty pw_rdmng.
  DATA: lw_mod          TYPE i,
        lw_remain       LIKE mseg-menge,
        lw_gap          LIKE mseg-menge,
        lw_time_zone(2) TYPE n,
        lw_zone_cnt(2)  TYPE n,
        lw_eff_last(2)  TYPE n,                "Last Effective Time Zone
        lw_req_qty(50).

  FIELD-SYMBOLS: <req_qty>.

  lw_mod    = trunc( pw_toqty /   pw_rdmng ).
  lw_remain = pw_toqty MOD pw_rdmng.

  CHECK lw_remain NE 0.

  lw_gap = pw_rdmng - lw_remain.

  lw_zone_cnt = c_max_zone - pw_time_zone.

  DO lw_zone_cnt TIMES.
    lw_time_zone = sy-index + pw_time_zone.

    CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_req_qty.
    ASSIGN (lw_req_qty) TO <req_qty>.

    CHECK <req_qty> > 0.

    IF lw_gap > <req_qty>.
      IF sy-index EQ lw_zone_cnt.        "When last time zone
        pw_toqty = lw_mod * pw_rdmng + pw_rdmng.
        CLEAR: <req_qty>.
      ELSE.
        pw_toqty = pw_toqty + <req_qty>.
        lw_gap   = lw_gap   - <req_qty>.
        CLEAR: <req_qty>.
      ENDIF.
    ELSE.
      pw_toqty  = pw_toqty  + lw_gap.
      <req_qty> = <req_qty> - lw_gap.

      EXIT.
    ENDIF.
  ENDDO.

  PERFORM get_last_effective_time_zone USING lw_eff_last.
  IF lw_eff_last EQ pw_time_zone.
    pw_toqty = lw_mod * pw_rdmng + pw_rdmng.
  ENDIF.
ENDFORM.                    " calculate_rounding_value
*&---------------------------------------------------------------------*
*&      Form  read_planned_quantity_rp01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_planned_quantity_rp01.
  DATA: lw_datum        LIKE sy-datum,
        lw_tprog        LIKE kapa-tprog.


  IF p_shift EQ c_check.
    MOVE: w_to_date  TO lw_datum,
          w_to_tprog TO lw_tprog.
  ELSE.
    MOVE: w_next_date  TO lw_datum,
          w_next_tprog TO lw_tprog.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_nstl
    FROM ztmm_nstl AS a INNER JOIN ztmm_mast AS b
                           ON a~matnr EQ b~matnr
                          AND b~spptl EQ 'N'        "Non Supply To Line
   WHERE datum    EQ   lw_datum
     AND rpsta    EQ   p_rp
     AND kaptprog EQ   lw_tprog
     AND zline    LIKE 'B%'
     AND a~matnr  IN   s_matnr.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m19.
  ENDIF.

  CHECK p_shift EQ c_check.
ENDFORM.                    " read_planned_quantity_rp01
*&---------------------------------------------------------------------*
*&      Form  read_planned_quantity_rp06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_planned_quantity_rp06.
  DATA: lw_datum        LIKE sy-datum,
        lw_tprog        LIKE kapa-tprog.

  IF p_shift EQ c_check.
    MOVE: w_to_date  TO lw_datum,
          w_to_tprog TO lw_tprog.
  ELSE.
    MOVE: w_next_date  TO lw_datum,
          w_next_tprog TO lw_tprog.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_nstl
    FROM ztmm_nstl AS a INNER JOIN ztmm_mast AS b
                           ON a~matnr EQ b~matnr
                          AND b~spptl EQ 'N'      "Non Supply To Line
   WHERE datum    EQ   lw_datum
     AND rpsta    EQ   p_rp
     AND kaptprog EQ   lw_tprog
     AND NOT ( zline LIKE 'B%' OR zline LIKE 'P%' )
     AND a~matnr  IN   s_matnr.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m19.
  ENDIF.
ENDFORM.                    " read_planned_quantity_rp06
*&---------------------------------------------------------------------*
*&      Form  read_stl_master_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_stl_master_01.
  CLEAR: it_master, it_master[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_master
    FROM ztmm_mast AS a INNER JOIN makt AS b
                           ON a~mandt EQ b~mandt
                          AND a~matnr EQ b~matnr
                          AND b~spras EQ sy-langu
   WHERE a~werks EQ p_werks
     AND a~matnr IN s_matnr
     AND a~spptl EQ c_spptl
     AND a~zline LIKE 'B%'.
  IF sy-subrc NE 0.
*    MESSAGE e000(zz) WITH text-m20.
  ENDIF.

  SORT it_master BY werks matnr.
  SORT it_open_to_matnr BY werks matnr.

  LOOP AT it_master.
    READ TABLE it_open_to_matnr WITH KEY werks = it_master-werks
                                         matnr = it_master-matnr
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE: p_werks         TO it_open_to_matnr-werks,
            it_master-matnr TO it_open_to_matnr-matnr.
      COLLECT it_open_to_matnr.
    ENDIF.
  ENDLOOP.

  LOOP AT it_open_to_matnr.
    CLEAR: it_master.

    READ TABLE it_master WITH KEY werks = it_open_to_matnr-werks
                                  matnr = it_open_to_matnr-matnr
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      SELECT *
        APPENDING CORRESPONDING FIELDS OF TABLE it_master
        FROM ztmm_mast AS a INNER JOIN makt AS b
                               ON a~mandt EQ b~mandt
                              AND a~matnr EQ b~matnr
                              AND b~spras EQ sy-langu
       WHERE a~werks EQ p_werks
         AND a~matnr EQ it_open_to_matnr-matnr.
      IF sy-subrc NE 0.
        MOVE: it_open_to_matnr-werks TO it_master-werks,
              it_open_to_matnr-matnr TO it_master-matnr.
        APPEND it_master.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_stl_master_01
*&---------------------------------------------------------------------*
*&      Form  read_stl_master_06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_stl_master_06.
  CLEAR: it_master, it_master[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_master
    FROM ztmm_mast AS a INNER JOIN makt AS b
                           ON a~mandt EQ b~mandt
                          AND a~matnr EQ b~matnr
                          AND b~spras EQ sy-langu
   WHERE a~werks EQ p_werks
     AND a~matnr IN s_matnr
     AND a~spptl EQ c_spptl
     AND NOT ( a~zline LIKE 'B%' OR zline LIKE 'P%' ).
  IF sy-subrc NE 0.
*    MESSAGE e000(zz) WITH text-m20.
  ENDIF.

  SORT it_master BY werks matnr.
  SORT it_open_to_matnr BY werks matnr.

  LOOP AT it_master.
    READ TABLE it_open_to_matnr WITH KEY werks = it_master-werks
                                         matnr = it_master-matnr
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE: p_werks         TO it_open_to_matnr-werks,
            it_master-matnr TO it_open_to_matnr-matnr.
      COLLECT it_open_to_matnr.
    ENDIF.
  ENDLOOP.

  LOOP AT it_open_to_matnr.
    CLEAR: it_master.

    READ TABLE it_master WITH KEY werks = it_open_to_matnr-werks
                                  matnr = it_open_to_matnr-matnr
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      SELECT *
        APPENDING CORRESPONDING FIELDS OF TABLE it_master
        FROM ztmm_mast AS a INNER JOIN makt AS b
                               ON a~mandt EQ b~mandt
                              AND a~matnr EQ b~matnr
                              AND b~spras EQ sy-langu
       WHERE a~werks EQ p_werks
         AND a~matnr EQ it_open_to_matnr-matnr.
      IF sy-subrc NE 0.
        MOVE: it_open_to_matnr-werks TO it_master-werks,
              it_open_to_matnr-matnr TO it_master-matnr.
        APPEND it_master.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_stl_master_06
*&---------------------------------------------------------------------*
*&      Form  read_master_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_master_data.
  CASE p_rp.
    WHEN '01'.
      PERFORM read_stl_master_01.
    WHEN '02'.
      PERFORM read_stl_master_02.
    WHEN '06'.
      PERFORM read_stl_master_06.
  ENDCASE.

  READ TABLE it_master INDEX 1.
  IF sy-subrc NE 0.
    MOVE: 'dokdoneunwuriddang' TO it_master-matnr.
    APPEND it_master.
  ENDIF.

  SORT it_master BY werks matnr.

  PERFORM read_source_bin.
  PERFORM read_stock_n_target_bin.

  MOVE: 1 TO it_master-rdmng.

  MODIFY it_master TRANSPORTING rdmng WHERE rdmng EQ 0.

  SORT it_master BY werks matnr.
ENDFORM.                    " read_master_data
*&---------------------------------------------------------------------*
*&      Form  read_source_bin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_source_bin.
  DATA: BEGIN OF ls_mlgt,
          matnr   LIKE   mara-matnr,             "Material
          lgnum   LIKE   mlgt-lgnum,             "Warehouse No
          lgtyp   LIKE   mlgt-lgtyp,             "Source storage type
          lgpla   LIKE   mlgt-lgpla,             "Source storage bin
          rdmng   LIKE   mlgt-rdmng,             "Rounding value
          gesme   LIKE   lqua-gesme,             "Source bin stock
          meins   LIKE   mara-meins,             "Base UoM
        END   OF ls_mlgt.

  LOOP AT it_master.
    CLEAR: ls_mlgt.
    SELECT SINGLE a~matnr c~lgnum c~lgtyp c~lgpla
                  c~rdmng e~gesme a~meins
      INTO CORRESPONDING FIELDS OF ls_mlgt
      FROM mara AS a INNER JOIN mlgn AS b
                        ON a~mandt EQ b~mandt
                       AND a~matnr EQ b~matnr
                       AND b~lvorm EQ space
                     INNER JOIN mlgt AS c
                        ON b~mandt EQ c~mandt
                       AND b~matnr EQ c~matnr
                       AND b~lgnum EQ c~lgnum
                       AND c~lvorm EQ space
                     INNER JOIN t334t AS d
                        ON c~mandt EQ d~mandt
                       AND c~lgnum EQ d~lgnum
                       AND b~ltkze EQ d~lgtkz
                       AND d~kzear EQ 'A'
                      LEFT OUTER JOIN lqua AS e
                        ON c~mandt = e~mandt
                       AND c~matnr = e~matnr
                       AND c~lgtyp = e~lgtyp
                       AND c~lgpla = e~lgpla
                       AND e~bestq = space
       WHERE a~matnr EQ it_master-matnr
         AND a~lvorm EQ ' '
         AND ( c~lgtyp EQ d~lgty0 OR c~lgtyp EQ d~lgty1 OR
               c~lgtyp EQ d~lgty2 OR c~lgtyp EQ d~lgty3 OR
               c~lgtyp EQ d~lgty4 OR c~lgtyp EQ d~lgty5 OR
               c~lgtyp EQ d~lgty6 OR c~lgtyp EQ d~lgty7 OR
               c~lgtyp EQ d~lgty8 OR c~lgtyp EQ d~lgty9 OR
               c~lgtyp EQ d~lgt10 OR c~lgtyp EQ d~lgt11 OR
               c~lgtyp EQ d~lgt12 OR c~lgtyp EQ d~lgt13 OR
               c~lgtyp EQ d~lgt14 OR c~lgtyp EQ d~lgt15 OR
               c~lgtyp EQ d~lgt16 OR c~lgtyp EQ d~lgt17 OR
               c~lgtyp EQ d~lgt18 OR c~lgtyp EQ d~lgt19 OR
               c~lgtyp EQ d~lgt20 OR c~lgtyp EQ d~lgt21 OR
               c~lgtyp EQ d~lgt22 OR c~lgtyp EQ d~lgt23 OR
               c~lgtyp EQ d~lgt24 OR c~lgtyp EQ d~lgt25 OR
               c~lgtyp EQ d~lgt26 OR c~lgtyp EQ d~lgt27 OR
               c~lgtyp EQ d~lgt28 OR c~lgtyp EQ d~lgt29 ).
    IF sy-subrc EQ 0.
      MOVE: ls_mlgt-lgnum TO it_master-lgnum,
            ls_mlgt-lgtyp TO it_master-vltyp,
            ls_mlgt-lgpla TO it_master-vlpla,
            ls_mlgt-gesme TO it_master-srcst,
            ls_mlgt-rdmng TO it_master-rdmng,
            ls_mlgt-meins TO it_master-meins.

      MODIFY it_master.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_source_bin
*&---------------------------------------------------------------------*
*&      Form  delete_open_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_open_to.
  PERFORM display_progress_bar USING text-b07.

  PERFORM read_open_to_quantity.

  CHECK p_tocrea EQ c_check.

  PERFORM generate_bdc_lt15.
ENDFORM.                    " delete_open_to
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
  CASE p_shift.
    WHEN c_check.
      LOOP AT SCREEN.
        IF screen-group1 = 'GR1'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN space.
      LOOP AT SCREEN.
        IF screen-group1 = 'GR1'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

      MOVE: 'X' TO p_opento.
  ENDCASE.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  get_effective_time_zone
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_EFF_ZONE  text
*----------------------------------------------------------------------*
FORM get_effective_time_zone USING pw_eff_zone pw_zone_cnt.
  DATA: lw_time_zone(2) TYPE n,
        lw_time(50).

  FIELD-SYMBOLS: <time>.

  CLEAR: pw_eff_zone.

  DO c_max_zone TIMES.
    MOVE: sy-index TO lw_time_zone.
    CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_time.
    ASSIGN (lw_time) TO <time>.

    IF <time> > 0.
      MOVE: lw_time_zone TO pw_eff_zone.
      EXIT.
    ENDIF.
  ENDDO.

  pw_zone_cnt = c_max_zone - pw_eff_zone + 1.
ENDFORM.                    " get_effective_time_zone
*&---------------------------------------------------------------------*
*&      Form  set_it_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_NSTL  text
*      -->P_IT_MASTER  text
*      -->P_LW_TOQTY  text
*----------------------------------------------------------------------*
FORM set_to_data USING  pw_nstl    STRUCTURE it_nstl
                        pw_master  STRUCTURE it_master
                        pw_toqty   pw_time_zone pw_src_stock.

  CHECK pw_toqty NE 0.

  CLEAR: w_9002.

  READ TABLE it_worktime WITH KEY datum = pw_nstl-datum
                                  tprog = pw_nstl-kaptprog
                                  INDEX = pw_time_zone.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  CLEAR: w_9002.
  MOVE-CORRESPONDING: pw_nstl   TO w_9002,
                      pw_master TO w_9002.

  MOVE: it_worktime-wofrm(8)   TO w_9002-stdat,
        it_worktime-wofrm+8(6) TO w_9002-stuzt,
        it_worktime-woend(8)   TO w_9002-endat,
        it_worktime-woend+8(6) TO w_9002-enuzt,
        pw_nstl-meins          TO w_9002-altme,
        pw_src_stock           TO w_9002-srcst,
        pw_toqty               TO w_9002-vsola.

  pw_src_stock = pw_src_stock - pw_toqty.

  APPEND w_9002 TO it_9002.
ENDFORM.                    " set_it_to

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
    WHEN c_tab_9000-tab4.
      g_tab_9000-subscreen = '9004'.
    WHEN c_tab_9000-tab5.
      g_tab_9000-subscreen = '9005'.
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
    WHEN c_tab_9000-tab4.
      g_tab_9000-pressed_tab = c_tab_9000-tab4.
    WHEN c_tab_9000-tab5.
      g_tab_9000-pressed_tab = c_tab_9000-tab5.
    WHEN OTHERS.
*      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_alv_object OUTPUT.
  CONCATENATE: 'WC_CONTAINER_' sy-dynnr INTO w_container.
  ASSIGN:      (w_container)            TO   <container>.

  IF <container> IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_field_catalog.
*    PERFORM SET_SORT_TOTAL_FIELD TABLES IT_SORT
    PERFORM assign_itab_to_alv.
    PERFORM sssign_event.
  ENDIF.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM dbl_click_9000 USING    p_e_column_fieldname
                             p_es_row_no_row_id.

ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
*- Create Container('GRID_CONTAINER') with Custom Control on screen

  CONCATENATE: 'WC_CONTAINER_' sy-dynnr INTO w_container,
               'WC_CONTROL_'   sy-dynnr INTO w_control,
               'WC_ALV_'       sy-dynnr INTO w_alv.

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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  CASE sy-dynnr.
    WHEN '9001'.
      PERFORM set_attributes_alv_9001.
    WHEN '9002' OR '9100'.
      PERFORM set_attributes_alv_9002.
    WHEN '9003'.
      PERFORM set_attributes_alv_9003.
    WHEN '9004'.
      PERFORM set_attributes_alv_9004.
    WHEN '9005'.
      PERFORM set_attributes_alv_9005.
  ENDCASE.
ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog.
  CASE sy-dynnr.
    WHEN '9001'.
      PERFORM build_field_catalog_9001.
    WHEN '9002' OR '9100'.
      PERFORM build_field_catalog_9002.
    WHEN '9003'.
      PERFORM build_field_catalog_9003.
    WHEN '9004'.
      PERFORM build_field_catalog_9004.
    WHEN '9005'.
      PERFORM build_field_catalog_9005.
  ENDCASE.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog_9001.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  DATA: lw_time_zone(2) TYPE n,
        lw_time(50).

  PERFORM set_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'DATUM'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'RPSTA'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'KAPTPROG'    ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'GESME'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'BFERR'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'OPENTO'      ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZSUM'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'MEINS'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ERNAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'ERZET'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AENAM'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEDAT'       ' ',
                                  'E' 'NO_OUT'      'X',

                                  'S' 'AEZET'       ' ',
                                  'E' 'NO_OUT'      'X'.

  DO c_max_zone TIMES.
    MOVE: sy-index TO lw_time_zone.
    CONCATENATE 'TIME' lw_time_zone INTO lw_time.

    IF     sy-index <= w_curr_idx.
      PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' lw_time       ' ',
                                  'E' 'EMPHASIZE'   ' '.
    ELSEIF sy-index > w_curr_idx AND sy-index <= w_last_idx.
      PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' lw_time       ' ',
                                  'E' 'EMPHASIZE'   'X'.
    ELSEIF sy-index > w_last_idx.
      PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' lw_time       ' ',
                                  'E' 'NO_OUT'      'X'.
    ENDIF.
  ENDDO.
ENDFORM.                    " BUILD_FIELD_CATALOG_9001
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog_9002.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'NO_ZERO'     'X',

                                  'S' 'STDAT'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STUZT'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ENDAT'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ENUZT'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SRCST'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'VSOLA'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ALTME'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ICON'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZMSG'        ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'TANUM'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'TAPOS'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'LGNUM'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'VLTYP'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'VLPLA'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'NLTYP'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'NLPLA'       ' ',
                                  'E' 'EMPHASIZE'   ' ',

                                  'S' 'WORKS'       ' ',
                                  'E' 'EMPHASIZE'   ' '.
ENDFORM.                    " BUILD_FIELD_CATALOG_9002
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog_9003.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STDAT'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'STUZT'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ENDAT'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'ENUZT'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'VSOLA'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'NISTA'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'PVQUI'       ' ',
                                  'E' 'NO_OUT'      'X'.
ENDFORM.                    " BUILD_FIELD_CATALOG_9003
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog_9004.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'DATUM'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'TPROG'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'INDEX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'OPMIN'       ' ',
                                  'E' 'EDIT_MASK'   '__:__'.
ENDFORM.                    " BUILD_FIELD_CATALOG_9004
*&---------------------------------------------------------------------*
*&      Form  SET_fieldname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fieldname.
  DATA: lw_itab TYPE slis_tabname.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  MOVE: sy-repid TO w_repid.
  CONCATENATE 'ZEMMPM28E_NSTL_FINAL_' sy-dynnr INTO lw_itab.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = w_repid
            i_internal_tabname = lw_itab
            i_inclname         = w_repid
       CHANGING
            ct_fieldcat        = it_fieldname.
ENDFORM.                    " SET_fieldname
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_3884   text
*      -->P_3885   text
*      -->P_3886   text
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
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
* Display data
  CONCATENATE: 'WC_ALV_'               sy-dynnr INTO w_alv,
               'ZEMMPM28E_NSTL_FINAL_' sy-dynnr INTO w_structure,
               'IT_'                   sy-dynnr INTO w_itab.

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
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status OUTPUT.
  CASE sy-dynnr.
    WHEN '9000' OR '9100'.
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
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      CLEAR: sy-ucomm.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  get_last_effective_time_zone
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_EFF_LAST  text
*----------------------------------------------------------------------*
FORM get_last_effective_time_zone USING pw_eff_last.
  DATA: lw_time_zone(2) TYPE n,
        lw_req_qty(50).

  FIELD-SYMBOLS: <req_qty>.

  DO c_max_zone TIMES.
    lw_time_zone = c_max_zone - sy-index + 1.

    CONCATENATE 'IT_NSTL-TIME' lw_time_zone INTO lw_req_qty.
    ASSIGN (lw_req_qty) TO <req_qty>.

    IF <req_qty> > 0.
      MOVE: lw_time_zone TO pw_eff_last.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " get_last_effective_time_zone
*&---------------------------------------------------------------------*
*&      Form  calculate_to_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_TOQTY  text
*      -->P_<REQ_QTY>  text
*----------------------------------------------------------------------*
FORM calculate_to_quantity USING pw_toqty pw_tar_stock pw_req_qty.
  IF it_master-stock_check EQ c_check.
    CASE it_master-zmnmx.
      WHEN 'MAX'.
        pw_toqty =   it_master-lpmin - pw_tar_stock +
                     it_master-bferr - it_master-opento.
      WHEN 'MIN'.
        pw_toqty =  pw_req_qty -
                  ( pw_tar_stock    - it_master-bferr -
                    it_master-lpmin + it_master-opento ).
    ENDCASE.

    IF pw_toqty <= 0.
      pw_toqty = 0.
      pw_tar_stock = pw_tar_stock    - pw_req_qty -
                     it_master-bferr + it_master-opento.
    ENDIF.
  ELSE.
    pw_toqty = pw_req_qty.
  ENDIF.
ENDFORM.                    " calculate_to_quantity
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog_9005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_field_catalog_9005.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  PERFORM set_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                  'S' 'WERKS'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MATNR'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'MAKTX'       ' ',
                                  'E' 'KEY'         'X',

                                  'S' 'LGNUM'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'VLTYP'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'VLPLA'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'SRCST'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NLTYP'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'NLPLA'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'WORKS'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'GESME'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'BFERR'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZSUM'        ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'FEED_CYCLE'  ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'LPMIN'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'RDMNG'       ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'STOCK_CHECK' ' ',
                                  'E' 'EMPHASIZE'   'C400',

                                  'S' 'ZMNMX'       ' ',
                                  'E' 'EMPHASIZE'   'C400'.
ENDFORM.                    " build_field_catalog_9005
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_DATA_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_screen_data_9001 TABLES pt_nstl STRUCTURE ztmm_nstl.
  DATA: lw_gesme        LIKE it_master-gesme,
        lw_bferr        LIKE it_master-bferr,
        lw_opento       LIKE it_master-opento,
        lw_time_zone(2) TYPE n,
        lw_time(50).

  FIELD-SYMBOLS: <time>.

  CLEAR: it_9001, w_9001, pt_nstl.

  SORT pt_nstl BY matnr datum rpsta kaptprog.
  LOOP AT pt_nstl.
    CLEAR: w_9001, lw_gesme, lw_bferr, lw_opento, it_master.

    READ TABLE it_master WITH KEY matnr = pt_nstl-matnr
                                  werks = p_werks
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH text-m01.
    ENDIF.

    MOVE-CORRESPONDING pt_nstl TO w_9001.

    ON CHANGE OF pt_nstl-rpsta OR pt_nstl-matnr.
      MOVE: it_master-gesme  TO lw_gesme,
            it_master-opento TO lw_opento,
            it_master-bferr  TO lw_bferr.
    ENDON.

    MOVE: lw_gesme  TO w_9001-gesme,
          lw_opento TO w_9001-opento,
          lw_bferr  TO w_9001-bferr.

    DO c_max_zone TIMES.
      MOVE: sy-index TO lw_time_zone.
      CONCATENATE 'W_9001-TIME' lw_time_zone INTO lw_time.
      ASSIGN (lw_time) TO <time>.

      w_9001-zsum = w_9001-zsum + <time>.

    ENDDO.

    APPEND w_9001 TO it_9001.
  ENDLOOP.

  SORT it_9001 BY matnr datum rpsta kaptprog.
ENDFORM.                    " SET_SCREEN_DATA_9001
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_9001
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
ENDFORM.                    " SET_ATTRIBUTES_ALV_9001
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_9002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9002.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " SET_ATTRIBUTES_ALV_9002
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_9003
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9003.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " SET_ATTRIBUTES_ALV_9003
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_9004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9004.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " SET_ATTRIBUTES_ALV_9004
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_9005
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_9005.
  CLEAR : w_is_layout, w_variant.

  w_is_layout-edit       = ' '.      "/Edit Mode Enable
  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  w_is_layout-language   = sy-langu. "/Language Key
  w_is_layout-cwidth_opt = c_check.  "/optimizes the column width
  w_is_layout-no_merging = c_check.  "/Disable cell merging
  w_variant-report       = sy-repid.
  w_variant-username     = sy-uname.
ENDFORM.                    " SET_ATTRIBUTES_ALV_9005
*&---------------------------------------------------------------------*
*&      Module  set_listbox  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_listbox OUTPUT.
  PERFORM set_listbox.
ENDMODULE.                 " set_listbox  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  generate_bdc_lt15
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_lt15.
  CLEAR: w_open_total, w_open_failed, w_open_success.

  LOOP AT it_open_to.
    IF it_open_to-icon NE space.
      w_open_failed = w_open_failed + 1.
      w_open_total  = w_open_total  + 1.
      CONTINUE.
    ENDIF.

    REFRESH bdc_tab.

    PERFORM dynpro USING:
          'X' 'SAPML03T'              '0118',
          ' ' 'LTAK-TANUM'            it_open_to-tanum,
          ' ' 'RL03T-TAPOS'           it_open_to-tapos,
          ' ' 'LTAK-LGNUM'            it_open_to-lgnum,
          ' ' 'BDC_OKCODE'            '/00',

          'X' 'SAPML03T'              '120',
          ' ' 'LTAP1-KREUZ(1)'        'X',
          ' ' 'BDC_OKCODE'            '=BU'.

    CALL TRANSACTION 'LT15' USING  bdc_tab
                           OPTIONS FROM wa_opt.
    IF sy-subrc EQ 0 AND sy-msgno EQ '358'.
      MOVE: c_icon_success TO it_open_to-icon.
      w_open_success = w_open_success + 1.

      UPDATE ztmm_nstl_log
        SET: cancl = c_check
             aenam = sy-uname
             aedat = sy-datum
             aezet = sy-uzeit
       WHERE tanum = it_open_to-tanum
         AND cancl = space.
    ELSE.
      MOVE: c_icon_fail    TO it_open_to-icon.
      PERFORM get_err_msg USING it_open_to-zmsg.
      w_open_failed = w_open_failed + 1.
    ENDIF.

    w_open_total = w_open_total + 1.

    MODIFY it_open_to.
  ENDLOOP.
ENDFORM.                    " generate_bdc_lt15
*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0148   text
*      -->P_0149   text
*      -->P_0150   text
*----------------------------------------------------------------------*
FORM dynpro USING dynbegin name value.
  IF dynbegin = 'X'.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-program,
          value TO bdc_tab-dynpro,
          'X'   TO bdc_tab-dynbegin.
    APPEND bdc_tab.
  ELSE.
    CLEAR:  bdc_tab.
    MOVE: name  TO bdc_tab-fnam,
          value TO bdc_tab-fval.
    APPEND bdc_tab.
  ENDIF.
ENDFORM.                    " dynpro
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  wa_opt-defsize = 'X'.
  wa_opt-dismode = 'N'.
  wa_opt-updmode = 'S'.
ENDFORM.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  get_err_msg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OPEN_TO_ZMSG  text
*----------------------------------------------------------------------*
FORM get_err_msg USING pw_zmsg.
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
            msg_lin = pw_zmsg.
ENDFORM.                    " get_err_msg
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_lt1a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_lt01.
  DATA: lw_toqty(17),
        lw_stats   LIKE ztmm_nstl_log-stats,
        lw_subrc   LIKE sy-subrc,
        lw_logno_h LIKE ztmm_nstl_log-logno_h.

  PERFORM display_progress_bar USING text-b10.

  CLEAR: w_to_total, w_to_success, w_to_stk_err, w_to_bdc_err, w_9002.

  LOOP AT it_9002 INTO w_9002.
    CLEAR: lw_toqty, lw_subrc, lw_logno_h, lw_stats.

    w_to_total = w_to_total + 1.

*---// Get Log Document No
    PERFORM get_log_doc_no USING lw_logno_h lw_subrc.
    IF lw_subrc NE 0.
      MOVE: c_icon_fail TO w_9002-icon,
            text-m21    TO w_9002-zmsg.
      MODIFY it_9002 FROM w_9002.

      w_to_bdc_err = w_to_bdc_err + 1.
      CONTINUE.
    ENDIF.

*---// Get STL Master Info
    READ TABLE it_master WITH KEY matnr = w_9002-matnr
                                  werks = p_werks
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE: c_icon_fail TO w_9002-icon,
            text-m01    TO w_9002-zmsg.
      MODIFY it_9002 FROM w_9002.

      w_to_bdc_err = w_to_bdc_err + 1.
      CONTINUE.
    ENDIF.

*---// Source Bin Stock Check
    IF w_9002-srcst < w_9002-vsola.
      MOVE: c_icon_no_stock TO w_9002-icon,
            text-b08        TO w_9002-zmsg.
      MODIFY it_9002 FROM w_9002.

      w_to_stk_err = w_to_stk_err + 1.

      PERFORM update_log_table USING lw_logno_h 'E' space
                                     space lw_stats   lw_subrc.
      IF lw_subrc NE 0.
        CONCATENATE w_9002-zmsg text-b11 INTO w_9002-zmsg.
        MODIFY it_9002 FROM w_9002.
      ENDIF.

      CONTINUE.
    ENDIF.

*---// Set BDC Data
    REFRESH bdc_tab.

    WRITE: w_9002-vsola UNIT w_9002-altme TO lw_toqty.

    PERFORM dynpro USING:
          'X' 'SAPML03T'              '0101',
          ' ' 'LTAK-LGNUM'            w_9002-lgnum,
          ' ' 'LTAK-REFNR'            it_master-feedr,
          ' ' 'LTAK-BWLVS'            '850',
          ' ' 'LTAP-MATNR'            w_9002-matnr,
          ' ' 'RL03T-ANFME'           lw_toqty,
          ' ' 'LTAP-WERKS'            p_werks,
          ' ' 'LTAP-LGORT'            c_lgort,
          ' ' 'LTAP-BESTQ'            space,
          ' ' 'BDC_OKCODE'            '=TAE',

          'X' 'SAPML03T'              '102',
          ' ' 'RL03T-ANFME'           lw_toqty,
          ' ' 'LTAP-ALTME'            w_9002-altme,
          ' ' 'LTAP-VLTYP'            w_9002-vltyp,
          ' ' 'LTAP-VLPLA'            w_9002-vlpla,
          ' ' 'LTAP-NLTYP'            w_9002-nltyp,
          ' ' 'LTAP-NLPLA'            w_9002-nlpla,
          ' ' 'BDC_OKCODE'            '/00'.

    CALL TRANSACTION 'LT01' USING  bdc_tab
                           OPTIONS FROM wa_opt.
    IF sy-subrc EQ 0 AND sy-msgno EQ '016'.
      MOVE: sy-msgv1 TO w_9002-tanum,
            1        TO w_9002-tapos.

      PERFORM generate_bdc_lt1a USING lw_stats lw_logno_h lw_subrc.
    ELSE.
      MOVE: c_icon_fail    TO w_9002-icon.
      PERFORM get_err_msg USING w_9002-zmsg.
      w_to_bdc_err = w_to_bdc_err + 1.

      PERFORM update_log_table USING lw_logno_h 'E' sy-msgid
                                     sy-msgno   lw_stats lw_subrc.
      IF lw_subrc NE 0.
        CONCATENATE w_9002-zmsg text-b11 INTO w_9002-zmsg.
      ENDIF.
    ENDIF.


    MODIFY it_9002 FROM w_9002.
  ENDLOOP.
ENDFORM.                    " GENERATE_BDC_lt1a
*&---------------------------------------------------------------------*
*&      Form  update_log_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_log_table USING pw_logno_h pw_msgty pw_msgid pw_msgnr
                            pw_stats   pw_subrc.

  CLEAR: pw_stats.

  MOVE: pw_logno_h           TO ztmm_nstl_log-logno_h,
        w_9002-matnr         TO ztmm_nstl_log-matnr,
        w_9002-stdat         TO ztmm_nstl_log-sdate,
        w_9002-stuzt         TO ztmm_nstl_log-stime,
        w_9002-endat         TO ztmm_nstl_log-edate,
        w_9002-enuzt         TO ztmm_nstl_log-etime,
        w_9002-vsola         TO ztmm_nstl_log-tqty,
        it_master-gesme      TO ztmm_nstl_log-gesme,
        it_master-bferr      TO ztmm_nstl_log-bferr,
        it_master-opento     TO ztmm_nstl_log-vsola,
        it_master-lpmin      TO ztmm_nstl_log-lpmin,
        it_master-rdmng      TO ztmm_nstl_log-rdmng,
        it_master-meins      TO ztmm_nstl_log-meins,
        it_master-zline      TO ztmm_nstl_log-zline,
        it_master-works      TO ztmm_nstl_log-works,
        it_master-rh_lh      TO ztmm_nstl_log-rh_lh,
        it_master-dispo      TO ztmm_nstl_log-dispo,
        it_master-feed_cycle TO ztmm_nstl_log-feed_cycle,
        it_master-ztime      TO ztmm_nstl_log-ztime,
        it_master-feedr      TO ztmm_nstl_log-feedr,
        it_master-vltyp      TO ztmm_nstl_log-src_lgtyp,
        it_master-vlpla      TO ztmm_nstl_log-src_lgpla,
        it_master-nltyp      TO ztmm_nstl_log-des_lgtyp,
        it_master-nlpla      TO ztmm_nstl_log-des_lgpla,
        w_9002-zmsg          TO ztmm_nstl_log-messa,
        sy-tcode             TO ztmm_nstl_log-ztcode,
        sy-repid             TO ztmm_nstl_log-zprogramm,
        w_9002-tanum         TO ztmm_nstl_log-tanum,
        pw_msgty             TO ztmm_nstl_log-msgty,
        pw_msgid             TO ztmm_nstl_log-msgid,
        pw_msgnr             TO ztmm_nstl_log-msgnr,
        pw_stats             TO ztmm_nstl_log-stats,
        space                TO ztmm_nstl_log-cancl,
        sy-uname             TO ztmm_nstl_log-ernam,
        sy-datum             TO ztmm_nstl_log-erdat,
        sy-uzeit             TO ztmm_nstl_log-erzet,
        sy-uname             TO ztmm_nstl_log-aenam,
        sy-datum             TO ztmm_nstl_log-aedat,
        sy-uzeit             TO ztmm_nstl_log-aezet.

  INSERT ztmm_nstl_log.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    MOVE: sy-subrc TO pw_subrc.
  ENDIF.
ENDFORM.                    " update_log_table
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_LT1A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM generate_bdc_lt1a USING pw_stats pw_logno_h pw_subrc.
  DATA: lw_stdat(10),
        lw_stuzt(8),
        lw_endat(10),
        lw_enuzt(8).

*---// Change Feeding time in T/O
  REFRESH bdc_tab.

  WRITE: w_9002-stdat TO lw_stdat(10),
         w_9002-stuzt TO lw_stuzt(8),
         w_9002-endat TO lw_endat(10),
         w_9002-enuzt TO lw_enuzt(8).

  PERFORM dynpro USING:
        'X' 'SAPML03T'              '0126',
        ' ' 'LTAK-TANUM'            w_9002-tanum,
        ' ' 'LTAK-LGNUM'            w_9002-lgnum,
        ' ' 'BDC_OKCODE'            '/00',

        'X' 'SAPML03T'              '103',
        ' ' 'LTAK-STDAT'            lw_stdat,
        ' ' 'LTAK-STUZT'            lw_stuzt,
        ' ' 'LTAK-ENDAT'            lw_endat,
        ' ' 'LTAK-ENUZT'            lw_enuzt,
        ' ' 'BDC_OKCODE'            '=BU'.

  CALL TRANSACTION 'LT1A' USING  bdc_tab
                         OPTIONS FROM wa_opt.
  IF sy-subrc EQ 0 AND sy-msgno EQ space.
    MOVE: c_icon_success TO w_9002-icon,
          'C'            TO pw_stats.
    PERFORM get_err_msg USING w_9002-zmsg.
    w_to_success = w_to_success + 1.

    PERFORM update_log_table USING pw_logno_h 'S' sy-msgid
                                   sy-msgno   pw_stats pw_subrc.
    IF pw_subrc NE 0.
      CONCATENATE w_9002-zmsg text-b11 INTO w_9002-zmsg.
    ENDIF.

  ELSE.
    MOVE: c_icon_fail    TO w_9002-icon,
          text-b09       TO w_9002-zmsg,
          'H'            TO pw_stats.
    PERFORM get_err_msg USING w_9002-zmsg.
    w_to_bdc_err = w_to_bdc_err + 1.

    PERFORM update_log_table USING pw_logno_h 'E' sy-msgid
                                   sy-msgno   pw_stats pw_subrc.
    IF pw_subrc NE 0.
      CONCATENATE w_9002-zmsg text-b11 INTO w_9002-zmsg.
    ENDIF.
  ENDIF.
ENDFORM.                    " GENERATE_BDC_LT1A
*&---------------------------------------------------------------------*
*&      Form  GET_LOG_DOC_NO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_LOGNO_H  text
*----------------------------------------------------------------------*
FORM get_log_doc_no USING pw_logno_h pw_subrc.
  CLEAR : pw_logno_h.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = c_interval
            object                  = c_object
       IMPORTING
            number                  = pw_logno_h
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.

  MOVE sy-subrc TO pw_subrc.
ENDFORM.                    " GET_LOG_DOC_NO
*&---------------------------------------------------------------------*
*&      Form  READ_RP05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rp02.
  EXEC SQL PERFORMING APPEND_PLANNED_ORDER.
    SELECT /*+ ORDERED*/
           /*+ INDEX(C RESB________M)*/
           D.DATUM,  B.MATNR,  D.UZEIT,  C.BDMNG,  C.MEINS,
           D.TPROG,  D.WKORD
      INTO :W_ORDER
      FROM ZTMM_MAST A, MARA B,  RESB C,  ZTMM_DVRT D
     WHERE A.MANDT = :SY-MANDT
       AND A.WERKS = :P_WERKS
       AND A.MATNR BETWEEN :W_MATNR_F AND :W_MATNR_T
       AND A.SPPTL = :C_SPPTL
       AND A.ZLINE LIKE 'P%'
       AND B.MANDT =  A.MANDT
       AND B.MATNR =  A.MATNR
       AND B.MTART =  :C_MTART
       AND C.MANDT =  B.MANDT
       AND C.MATNR =  B.MATNR
       AND C.WERKS =  :P_WERKS
       AND C.XLOEK =  ' '
       AND C.KZEAR >= ' '
*       AND C.BDTER BETWEEN :SY-DATUM AND :W_REQ_DATE
*       AND C.BDTER >= :SY-DATUM
       AND D.MANDT =  C.MANDT
       AND D.RSNUM =  C.RSNUM
  ENDEXEC.
ENDFORM.                                                    " READ_RP05
*&---------------------------------------------------------------------*
*&      Form  read_stl_master_05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_stl_master_02.
  CLEAR: it_master, it_master[].

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_master
    FROM ztmm_mast AS a INNER JOIN makt AS b
                           ON a~mandt EQ b~mandt
                          AND a~matnr EQ b~matnr
                          AND b~spras EQ sy-langu
   WHERE a~werks EQ p_werks
     AND a~matnr IN s_matnr
     AND a~spptl EQ c_spptl
     AND a~zline LIKE 'P%'.
  IF sy-subrc NE 0.
*    MESSAGE e000(zz) WITH text-m20.
  ENDIF.

  SORT it_master BY werks matnr.
  SORT it_open_to_matnr BY werks matnr.

  LOOP AT it_master.
    READ TABLE it_open_to_matnr WITH KEY werks = it_master-werks
                                         matnr = it_master-matnr
                                BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE: p_werks         TO it_open_to_matnr-werks,
            it_master-matnr TO it_open_to_matnr-matnr.
      COLLECT it_open_to_matnr.
    ENDIF.
  ENDLOOP.

  LOOP AT it_open_to_matnr.
    CLEAR: it_master.

    READ TABLE it_master WITH KEY werks = it_open_to_matnr-werks
                                  matnr = it_open_to_matnr-matnr
                         BINARY SEARCH.
    IF sy-subrc NE 0.
      SELECT *
        APPENDING CORRESPONDING FIELDS OF TABLE it_master
        FROM ztmm_mast AS a INNER JOIN makt AS b
                               ON a~mandt EQ b~mandt
                              AND a~matnr EQ b~matnr
                              AND b~spras EQ sy-langu
       WHERE a~werks EQ p_werks
         AND a~matnr EQ it_open_to_matnr-matnr.
      IF sy-subrc NE 0.
        MOVE: it_open_to_matnr-werks TO it_master-werks,
              it_open_to_matnr-matnr TO it_master-matnr.
        APPEND it_master.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " read_stl_master_05
*&---------------------------------------------------------------------*
*&      Form  read_planned_quantity_rp05
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_planned_quantity_rp02.
  DATA: lw_datum        LIKE sy-datum,
        lw_tprog        LIKE kapa-tprog.

  IF p_shift EQ c_check.
    MOVE: w_to_date  TO lw_datum,
          w_to_tprog TO lw_tprog.
  ELSE.
    MOVE: w_next_date  TO lw_datum,
          w_next_tprog TO lw_tprog.
  ENDIF.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_nstl
    FROM ztmm_nstl AS a INNER JOIN ztmm_mast AS b
                           ON a~matnr EQ b~matnr
                          AND b~spptl EQ 'N'        "Non Supply To Line
   WHERE datum    EQ   lw_datum
     AND rpsta    EQ   p_rp
     AND kaptprog EQ   lw_tprog
     AND zline    LIKE 'P%'
     AND a~matnr  IN   s_matnr.
  IF sy-subrc NE 0.
    MESSAGE s000(zz) WITH text-m19.
  ENDIF.

  CHECK p_shift EQ c_check.
ENDFORM.                    " read_planned_quantity_rp05
*&---------------------------------------------------------------------*
*&      Form  prepare_display_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_screen.
  PERFORM set_screen_data_9001 TABLES it_ztmm_nstl.

  DELETE it_master WHERE matnr = 'dokdoneunwuriddang'.

  it_9003[] = it_open_to[].
  it_9005[] = it_master[].

  CLEAR: wc_container_9001,
         wc_container_9002,
         wc_container_9003,
         wc_container_9004,
         wc_container_9005,
         wc_container_9100.

  IF sy-batch EQ 'X'.
    it_9100[] = it_9002[].
    CALL SCREEN 9100.
  ELSE.
    CALL SCREEN 9000.
  ENDIF.
ENDFORM.                    " prepare_display_screen
*&---------------------------------------------------------------------*
*&      Form  calculate_to_qty_over_stock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_TOQTY  text
*      -->P_LW_TAR_STOCK  text
*      -->P_<REQ_QTY>  text
*----------------------------------------------------------------------*
FORM calculate_to_qty_over_stock USING pw_toqty pw_tar_stock pw_req_qty.
  CASE it_master-zmnmx.
    WHEN 'MAX'.
      pw_toqty =   it_master-lpmin - pw_tar_stock.
    WHEN 'MIN'.
      pw_toqty =  pw_req_qty -
                ( pw_tar_stock - it_master-lpmin ).
  ENDCASE.

  IF pw_toqty <= 0.
    pw_toqty = 0.
    pw_tar_stock = pw_tar_stock - pw_req_qty.
  ELSE.
    pw_tar_stock = it_master-gesme.
  ENDIF.
ENDFORM.                    " calculate_to_qty_over_stock
*&---------------------------------------------------------------------*
*&      Form  check_current_zone_open_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_current_zone_open_to.
  DATA: lw_to_time(14).

  CHECK p_opento EQ c_check.

  CLEAR: it_worktime.
  READ TABLE it_worktime WITH KEY datum =  w_to_date
                                  tprog =  w_to_tprog
                                  INDEX =  w_curr_idx.
  CHECK sy-subrc EQ 0.

  LOOP AT it_open_to.
    CONCATENATE it_open_to-stdat it_open_to-stuzt INTO lw_to_time.

    IF lw_to_time >= it_worktime-wofrm AND
       lw_to_time <= it_worktime-woend.
      MOVE: c_icon_warning TO it_open_to-icon,
            text-m22       TO it_open_to-zmsg.

      MODIFY it_open_to.

      READ TABLE it_master WITH KEY werks = p_werks
                                    matnr = it_open_to-matnr
                           BINARY SEARCH.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      it_master-opento = it_master-opento + it_open_to-vsola.
      MODIFY it_master INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_current_zone_open_to
*&---------------------------------------------------------------------*
*&      Form  SET_OPEN_TO_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_open_to_status.
  LOOP AT it_open_to.
    CASE it_open_to-pvqui.
      WHEN space.
        MOVE: icon_led_red TO it_open_to-qstvn.
      WHEN c_check.
        MOVE: icon_led_yellow TO it_open_to-qstvn,
              c_icon_fail     TO it_open_to-icon,
              text-m24        TO it_open_to-zmsg.
    ENDCASE.

    MODIFY it_open_to.

    PERFORM calculate_open_to_quantity.
  ENDLOOP.
ENDFORM.                    " SET_OPEN_TO_STATUS
*&---------------------------------------------------------------------*
*&      Form  calculate_open_to_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_open_to_quantity.
  DATA: lw_to_time(14).

  CASE it_open_to-pvqui.
    WHEN space.
      CHECK p_opento EQ c_check.

      CLEAR: it_worktime.
      READ TABLE it_worktime WITH KEY datum =  w_to_date
                                      tprog =  w_to_tprog
                                      INDEX =  w_curr_idx.
      CHECK sy-subrc EQ 0.
      CONCATENATE it_open_to-stdat it_open_to-stuzt INTO lw_to_time.

      IF lw_to_time >= it_worktime-wofrm AND
         lw_to_time <= it_worktime-woend.
        MOVE: c_icon_warning TO it_open_to-icon,
              text-m22       TO it_open_to-zmsg.

        MODIFY it_open_to.

        READ TABLE it_master WITH KEY werks = p_werks
                                      matnr = it_open_to-matnr
                             BINARY SEARCH.
        IF sy-subrc NE 0.
          MESSAGE e000(zz) WITH text-m01.
        ENDIF.

        it_master-opento = it_master-opento + it_open_to-vsola.
        MODIFY it_master INDEX sy-tabix.
      ENDIF.

    WHEN c_check.
      READ TABLE it_master WITH KEY werks = p_werks
                                    matnr = it_open_to-matnr
                           BINARY SEARCH.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      it_master-opento = it_master-opento + it_open_to-nista.
      MODIFY it_master INDEX sy-tabix.
  ENDCASE.
ENDFORM.                    " calculate_open_to_quantity
