************************************************************************
* Program Name      : ZAPP_PROD_PLAN
* Creation Date     : 10/13/2007
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 11/02/07        Furong           UD1K942044     Change/Dispaly mode
************************************************************************

REPORT zapp_prod_plan NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.
TABLES: ztpp_prod_actual.
TYPE-POOLS: slis, vrm.
*CONSTANTS:  C_CG_DATE TYPE D VALUE '20090531'.
CONSTANTS:  c_cg_date TYPE d VALUE '20100731'.
DATA: BEGIN OF it_tab_day OCCURS 0,
      model LIKE ztpp_prod_actual-model,
*      LABST LIKE MARD-LABST,
      seq(1),
      desc(10),
      type(15),
      sub LIKE ztpp_plan_day-util_01,
      total LIKE ztpp_plan_day-util_01,
      qtyd_01 LIKE ztpp_plan_day-util_01,
      qtyd_02 LIKE ztpp_plan_day-util_01,
      qtyd_03 LIKE ztpp_plan_day-util_01,
      qtyd_04 LIKE ztpp_plan_day-util_01,
      qtyd_05 LIKE ztpp_plan_day-util_01,
      qtyd_06 LIKE ztpp_plan_day-util_01,
      qtyd_07 LIKE ztpp_plan_day-util_01,
      qtyd_08 LIKE ztpp_plan_day-util_01,
      qtyd_09 LIKE ztpp_plan_day-util_01,
      qtyd_10 LIKE ztpp_plan_day-util_01,
      qtyd_11 LIKE ztpp_plan_day-util_01,
      qtyd_12 LIKE ztpp_plan_day-util_01,
      qtyd_13 LIKE ztpp_plan_day-util_01,
      qtyd_14 LIKE ztpp_plan_day-util_01,
      qtyd_15 LIKE ztpp_plan_day-util_01,
      qtyd_16 LIKE ztpp_plan_day-util_01,
      qtyd_17 LIKE ztpp_plan_day-util_01,
      qtyd_18 LIKE ztpp_plan_day-util_01,
      qtyd_19 LIKE ztpp_plan_day-util_01,
      qtyd_20 LIKE ztpp_plan_day-util_01,
      qtyd_21 LIKE ztpp_plan_day-util_01,
      qtyd_22 LIKE ztpp_plan_day-util_01,
      qtyd_23 LIKE ztpp_plan_day-util_01,
      qtyd_24 LIKE ztpp_plan_day-util_01,
      qtyd_25 LIKE ztpp_plan_day-util_01,
      qtyd_26 LIKE ztpp_plan_day-util_01,
      qtyd_27 LIKE ztpp_plan_day-util_01,
      qtyd_28 LIKE ztpp_plan_day-util_01,
      qtyd_29 LIKE ztpp_plan_day-util_01,
      qtyd_30 LIKE ztpp_plan_day-util_01,
      qtyd_31 LIKE ztpp_plan_day-util_01,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
     END OF it_tab_day.

DATA: BEGIN OF it_tab_month OCCURS 0,
      model LIKE ztpp_prod_actual-model,
*      LABST LIKE MARD-LABST,
      seq(1),
      desc(10),
      type(15),
      sub LIKE ztpp_plan_day-util_01,
      total LIKE ztpp_plan_day-util_01,
      qtyd_01 LIKE ztpp_plan_day-util_01,
      qtyd_02 LIKE ztpp_plan_day-util_01,
      qtyd_03 LIKE ztpp_plan_day-util_01,
      qtyd_04 LIKE ztpp_plan_day-util_01,
      qtyd_05 LIKE ztpp_plan_day-util_01,
      qtyd_06 LIKE ztpp_plan_day-util_01,
      qtyd_07 LIKE ztpp_plan_day-util_01,
      qtyd_08 LIKE ztpp_plan_day-util_01,
      qtyd_09 LIKE ztpp_plan_day-util_01,
      qtyd_10 LIKE ztpp_plan_day-util_01,
      qtyd_11 LIKE ztpp_plan_day-util_01,
      qtyd_12 LIKE ztpp_plan_day-util_01,
      qtyd_13 LIKE ztpp_plan_day-util_01,
      qtyd_14 LIKE ztpp_plan_day-util_01,
      qtyd_15 LIKE ztpp_plan_day-util_01,
      qtyd_16 LIKE ztpp_plan_day-util_01,
      qtyd_17 LIKE ztpp_plan_day-util_01,
      qtyd_18 LIKE ztpp_plan_day-util_01,
      qtyd_19 LIKE ztpp_plan_day-util_01,
      qtyd_20 LIKE ztpp_plan_day-util_01,
      qtyd_21 LIKE ztpp_plan_day-util_01,
      qtyd_22 LIKE ztpp_plan_day-util_01,
      qtyd_23 LIKE ztpp_plan_day-util_01,
      qtyd_24 LIKE ztpp_plan_day-util_01,
      qtyd_25 LIKE ztpp_plan_day-util_01,
      qtyd_26 LIKE ztpp_plan_day-util_01,
      qtyd_27 LIKE ztpp_plan_day-util_01,
      qtyd_28 LIKE ztpp_plan_day-util_01,
      qtyd_29 LIKE ztpp_plan_day-util_01,
      qtyd_30 LIKE ztpp_plan_day-util_01,
      qtyd_31 LIKE ztpp_plan_day-util_01,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
     END OF it_tab_month.

DATA: BEGIN OF it_rp OCCURS 0,
        rp(2),
      END OF it_rp.

DATA: BEGIN OF it_day OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day.

DATA: BEGIN OF it_month OCCURS 12,
        seq(2)  TYPE n,
        desc(3),
      END   OF it_month.

DATA: wa_plan LIKE it_tab_day,
      wa_actual LIKE it_tab_day,
      wa_gap LIKE it_tab_day,
      wa_hr LIKE it_tab_day,
      wa_util LIKE it_tab_day,
      wa_body LIKE it_tab_day,
      wa_trim LIKE it_tab_day,
      wa_paint LIKE it_tab_day,
      wa_signoff LIKE it_tab_day,
      wa_cgate LIKE it_tab_day,
      wa_mgate LIKE it_tab_day,
      wa_shipout LIKE it_tab_day,
      wa_util_body LIKE it_tab_day,
      wa_util_trim LIKE it_tab_day,
      wa_util_paint LIKE it_tab_day,
      wa_util_signoff LIKE it_tab_day,
      wa_util_cgate LIKE it_tab_day,
      wa_util_mgate LIKE it_tab_day,
      wa_util_shipout LIKE it_tab_day,
      wa_uph LIKE it_tab_day,
      wa_uph_b LIKE it_tab_day,
      wa_uph_p LIKE it_tab_day.

DATA: l_cn(2) TYPE n,
      l_text(30).
DATA : z_max_date LIKE sy-datum,
       z_beg_date LIKE sy-datum,
       z_max_week LIKE sy-datum,
       z_beg_week LIKE sy-datum,
       w_max_day TYPE p,
       w_max_day_cn(2) TYPE n,
       w_datum LIKE sy-datum,
       w_d-1 LIKE sy-datum,
       w_mtd LIKE sy-datum,
       w_uph(3),
       w_uph_ave TYPE zuph,
       w_uph_b_ave TYPE zuph,
       w_uph_p_ave TYPE zuph.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_no_data(1),
      w_model(3),
      w_month(6),
      w_year(4),
      w_flag(1),
      w_model_name(3).

DATA: xname    TYPE vrm_id,
      xlist    TYPE vrm_values,
      xvalue   LIKE LINE OF xlist.

DATA:  l_kalid LIKE kako-kalid.

DATA: wa_stbl  TYPE lvc_s_stbl.
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_tot  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_fname_tot    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_exclude      TYPE ui_functions,
       it_exclude_tot  TYPE ui_functions.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       wa_tot_layout TYPE lvc_s_layo,
       w_fieldname  LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
      wa_tot_save  TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_tot_variant TYPE disvariant.     "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: wa_custom_control_tot TYPE    scrfname VALUE 'ALV_CONTAINER_TOT',
      alv_grid_tot          TYPE REF TO cl_gui_alv_grid,
      grid_container_tot    TYPE REF TO cl_gui_custom_container.

FIELD-SYMBOLS : <fs01>, <fs02>, <fs03>, <fs_uph>.

**--- Constants
CONSTANTS : c_werks LIKE marc-werks VALUE 'E001'.

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define /implement event handler to handle event DATA_CHANGED.
*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed.

    DATA: error_in_data TYPE c.

ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
DATA :it_lvc  LIKE lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          w_qty(13),
          lvc_t_row TYPE lvc_t_row.

    error_in_data = space.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      CASE ls_good-fieldname.
* check if column Name1 of this row was changed
        WHEN 'QTYD_01' OR 'QTYD_02' OR 'QTYD_03' OR 'QTYD_04' OR
             'QTYD_05' OR 'QTYD_06' OR 'QTYD_07' OR 'QTYD_08' OR
             'QTYD_09' OR 'QTYD_10' OR 'QTYD_11' OR 'QTYD_12' OR
             'QTYD_13' OR 'QTYD_14' OR 'QTYD_15' OR 'QTYD_16' OR
             'QTYD_17' OR 'QTYD_18' OR 'QTYD_19' OR 'QTYD_20' OR
             'QTYD_21' OR 'QTYD_22' OR 'QTYD_23' OR 'QTYD_24' OR
             'QTYD_25' OR 'QTYD_26' OR 'QTYD_27' OR 'QTYD_28' OR
             'QTYD_29' OR 'QTYD_30' OR 'QTYD_31'.
          CALL METHOD er_data_changed->get_cell_value
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            IMPORTING
              e_value     = lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.
      ENDCASE.
    ENDLOOP.

*§7.Display application log if an error has occured.
    IF error_in_data EQ 'X'.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.                    "HANDLE_DATA_CHANGED

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  PERFORM set_days.
  PERFORM get_plan_data.
  PERFORM get_actual_data.
ENDFORM.                    "GET_DATA
*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_actual_data.

  DATA: l_line TYPE i.

*  DATA: LT_PROD_ACT LIKE TABLE OF ZTPP_PROD_ACTUAL
*        WITH HEADER LINE,
*        LT_PROD_ACT_TEMP LIKE TABLE OF ZTPP_PROD_ACTUAL
*        WITH HEADER LINE.

  DATA: lt_prod_act_temp LIKE TABLE OF ztpp_prod_actual
          WITH HEADER LINE.

*  DATA: BEGIN OF LT_PROD_ACT OCCURS 0.
*          INCLUDE STRUCTURE ZTPP_PROD_ACTUAL.
**  DATA: QTY_MGATE LIKE ZTPP_PROD_ACTUAL-QTY_VPCOUT,
*        END OF LT_PROD_ACT.

  DATA: lt_prod_act LIKE TABLE OF ztpp_prod_actual WITH HEADER LINE.

  DATA: w_rp LIKE it_rp-rp.

  CLEAR : it_tab_day.

  IF w_model_name = space.
    SELECT * INTO TABLE lt_prod_act_temp
       FROM ztpp_prod_actual
       WHERE prdt_date BETWEEN z_beg_date AND z_max_date.
  ELSE.
    SELECT * INTO TABLE lt_prod_act_temp
     FROM ztpp_prod_actual
     WHERE prdt_date BETWEEN z_beg_date AND z_max_date
       AND model = w_model_name.
  ENDIF.

  LOOP AT lt_prod_act_temp.
    IF lt_prod_act_temp-natn+3(2) = 'XX' OR
       lt_prod_act_temp-natn+3(2) = 'XY'.
      lt_prod_act-qty_signoff = 0.
    ELSE.
      lt_prod_act-qty_signoff = lt_prod_act_temp-qty_signoff.
    ENDIF.
    lt_prod_act-prdt_date = lt_prod_act_temp-prdt_date.
    lt_prod_act-qty_body = lt_prod_act_temp-qty_body.
    lt_prod_act-qty_paint = lt_prod_act_temp-qty_paint.
    lt_prod_act-qty_trim = lt_prod_act_temp-qty_trim.
    lt_prod_act-qty_cgate = lt_prod_act_temp-qty_cgate.
    lt_prod_act-qty_vpcout = lt_prod_act_temp-qty_vpcout.

** Changed by Furong on 08/04/10
** changed by I.G. Moon 06/20/09
*    IF LT_PROD_ACT_TEMP-NATN+3(2) EQ 'AA' OR
*       LT_PROD_ACT_TEMP-NATN+3(2) EQ 'AB'.
    IF lt_prod_act_temp-natn+3(1) EQ 'A'.
** end of change
** End of change on 08/04/10

      IF lt_prod_act_temp-dest+0(3) = 'B28'.
** Chnaged by Furong on 07/30/09
** Chnaged by Furong on 06/01/09
*        IF LT_PROD_ACT_TEMP-PRDT_DATE <= C_CG_DATE.
*          LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_CGATE.
*        ELSE.
*          LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_VPCOUT.
*        ENDIF.
* Chnaged by Furong on 07/28/10
        IF lt_prod_act_temp-prdt_date <= c_cg_date.
          lt_prod_act-qty_mgate = lt_prod_act_temp-qty_cgate.
        ELSE.
          lt_prod_act-qty_mgate = lt_prod_act_temp-qty_mgate.
        ENDIF.
*        LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_CGATE.
*** end of change on 07/28/10

*        LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_CGATE.
*** end of change on 07/30/09

      ELSE.
        lt_prod_act-qty_mgate = lt_prod_act_temp-qty_shipout.
      ENDIF.

** changed by I.G. Moon 06/20/09
    ELSE.
      lt_prod_act-qty_mgate = 0.
    ENDIF.
** end of change

    lt_prod_act-qty_shipout = lt_prod_act_temp-qty_shipout.
    COLLECT lt_prod_act.
    CLEAR: lt_prod_act.
  ENDLOOP.
  SORT lt_prod_act BY prdt_date.
  LOOP AT it_rp.
    w_rp = it_rp-rp.
    CLEAR: it_tab_day.
    CASE w_rp.
      WHEN '01'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_body.
            it_tab_day-total = it_tab_day-total + lt_prod_act-qty_body.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_BODY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.

** Changed by Furong on 11/21/08
*           CONCATENATE 'WA_UTIL_BODY-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            ENDIF.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_BODY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
** End of change

          ENDIF.
        ENDLOOP.
        it_tab_day-if = 'C210'.
        it_tab_day-desc = 'Body in'.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C210'.
        wa_body-if = 'C210'.
        wa_body-type = 'Working Hours'.
        wa_body-seq = '4'.
        wa_util_body-if = 'C210'.
        wa_util_body-type = 'Utilization'.
        wa_util_body-seq = '5'.
        IF wa_body-total > 0 AND w_uph_ave > 0.
*          WA_UTIL_BODY-TOTAL = IT_TAB_DAY-TOTAL /
*                               ( WA_BODY-TOTAL * W_UPH ) * 100.
          wa_util_body-total = it_tab_day-total /
                               ( wa_body-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_body-sub > 0  AND w_uph_ave > 0.
*          WA_UTIL_BODY-SUB = IT_TAB_DAY-SUB /
*                               ( WA_BODY-SUB * W_UPH ) * 100.
          wa_util_body-sub = it_tab_day-sub /
                               ( wa_body-sub * w_uph_ave ) * 100.

        ENDIF.
        APPEND wa_body TO it_tab_day.
        APPEND wa_util_body TO it_tab_day.
      WHEN '04'.
        it_tab_day-if = 'C410'.
        it_tab_day-desc = 'Paint out'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_paint.
            it_tab_day-total = it_tab_day-total
             + lt_prod_act-qty_paint.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_PAINT-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.

** Changed on 11/21/08 by Furong Wang
*            CONCATENATE 'WA_UTIL_PAINT-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            ENDIF.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_PAINT-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
** End of change

          ENDIF.
        ENDLOOP.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C410'.
        wa_paint-type = 'Working Hours'.
        wa_paint-seq = '4'.
        wa_paint-if = 'C410'.
        wa_util_paint-type = 'Utilization'.
        wa_util_paint-seq = '5'.
        wa_util_paint-if = 'C410'.
        IF wa_paint-total > 0  AND w_uph_ave > 0.
          wa_util_paint-total = it_tab_day-total /
                               ( wa_paint-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_paint-sub > 0  AND w_uph_ave > 0.
          wa_util_paint-sub = it_tab_day-sub /
                               ( wa_paint-sub * w_uph_ave ) * 100.
        ENDIF.
        APPEND wa_paint TO it_tab_day.
        APPEND wa_util_paint TO it_tab_day.

      WHEN '07'.
        it_tab_day-desc = 'Trim in'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_trim.
            it_tab_day-total = it_tab_day-total + lt_prod_act-qty_trim.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_TRIM-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.

** Changed on 11/21/08 by Furong Wang
*           CONCATENATE 'WA_UTIL_TRIM-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            ENDIF.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_TRIM-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_day-if = 'C210'.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C210'.
        wa_trim-if = 'C210'.
        wa_trim-type = 'Working Hours'.
        wa_trim-seq = '4'.
        wa_util_trim-if = 'C210'.
        wa_util_trim-type = 'Utilization'.
        wa_util_trim-seq = '5'.
        IF wa_trim-total > 0  AND w_uph_ave > 0.
          wa_util_trim-total = it_tab_day-total /
                               ( wa_trim-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_trim-sub > 0  AND w_uph_ave > 0.
          wa_util_trim-sub = it_tab_day-sub /
                               ( wa_trim-sub * w_uph_ave ) * 100.
        ENDIF.
        APPEND wa_trim TO it_tab_day.
        APPEND wa_util_trim TO it_tab_day.

      WHEN '18'.
        it_tab_day-desc = 'Sign off'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_signoff.
            it_tab_day-total = it_tab_day-total +
                               lt_prod_act-qty_signoff.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_SIGNOFF-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.


** Changed on 11/21/08 by Furong Wang
*           CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            endif.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
*              IF <FS03> > 0.
*                WA_UTIL_SIGNOFF-TOTAL = WA_UTIL_SIGNOFF-TOTAL + <FS03>.
*                IF IT_DAY-DATUM <= SY-DATUM.
*                   WA_UTIL_SIGNOFF-SUB = WA_UTIL_SIGNOFF-SUB + <FS03>.
*                ENDIF.
*              ENDIF.
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_day-if = 'C410'.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C410'.
        wa_signoff-type = 'Working Hours'.
        wa_signoff-seq = '4'.
        wa_signoff-if = 'C410'.
        wa_util_signoff-type = 'Utilization'.
        wa_util_signoff-seq = '5'.
        wa_util_signoff-if = 'C410'.
        IF wa_signoff-total > 0  AND w_uph_ave > 0.
          wa_util_signoff-total = it_tab_day-total /
                               ( wa_signoff-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_signoff-sub > 0  AND w_uph_ave > 0.
          wa_util_signoff-sub = it_tab_day-sub /
                               ( wa_signoff-sub * w_uph_ave ) * 100.
        ENDIF.
        APPEND wa_signoff TO it_tab_day.
        APPEND wa_util_signoff TO it_tab_day.

      WHEN '19'.
        it_tab_day-desc = 'C/Gate'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_cgate.
            it_tab_day-total = it_tab_day-total
             + lt_prod_act-qty_cgate.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_CGATE-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
** Changed on 11/21/08 by Furong Wang
*            CONCATENATE 'WA_UTIL_CGATE-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            ENDIF.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_CGATE-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_day-if = 'C210'.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C210'.
        wa_cgate-if = 'C210'.
        wa_cgate-type = 'Working Hours'.
        wa_cgate-seq = '4'.
        wa_util_cgate-if = 'C210'.
        wa_util_cgate-type = 'Utilization'.
        wa_util_cgate-seq = '5'.
        IF wa_cgate-total > 0  AND w_uph_ave > 0.
          wa_util_cgate-total = it_tab_day-total /
                               ( wa_cgate-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_cgate-sub > 0  AND w_uph_ave > 0.
          wa_util_cgate-sub = it_tab_day-sub /
                               ( wa_cgate-sub * w_uph_ave ) * 100.
        ENDIF.
        APPEND wa_cgate TO it_tab_day.
        APPEND wa_util_cgate TO it_tab_day.

      WHEN '23'.
        it_tab_day-desc = 'M/Gate'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
*            IF LT_PROD_ACT-DEST+0(3) = 'B28'.
*              <FS02> = LT_PROD_ACT-QTY_VPCOUT.
*         IT_TAB_DAY-TOTAL = IT_TAB_DAY-TOTAL + LT_PROD_ACT-QTY_VPCOUT
*     .
*            ELSE.
*              <FS02> = LT_PROD_ACT-QTY_SHIPOUT.
*         IT_TAB_DAY-TOTAL = IT_TAB_DAY-TOTAL + LT_PROD_ACT-QTY_SHIPOUT
*           .
*            ENDIF.
            <fs02> = lt_prod_act-qty_mgate.
            it_tab_day-total = it_tab_day-total
             + lt_prod_act-qty_mgate.

** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_MGATE-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
** Changed on 11/21/08 by Furong Wang
*            CONCATENATE 'WA_UTIL_CGATE-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            ENDIF.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_MGATE-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_day-if = 'C410'.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C410'.
        wa_mgate-if = 'C410'.
        wa_mgate-type = 'Working Hours'.
        wa_mgate-seq = '4'.
        wa_util_mgate-if = 'C410'.
        wa_util_mgate-type = 'Utilization'.
        wa_util_mgate-seq = '5'.
        IF wa_mgate-total > 0  AND w_uph_ave > 0.
          wa_util_mgate-total = it_tab_day-total /
                               ( wa_mgate-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_mgate-sub > 0  AND w_uph_ave > 0.
          wa_util_mgate-sub = it_tab_day-sub /
                               ( wa_mgate-sub * w_uph_ave ) * 100.
        ENDIF.
        APPEND wa_mgate TO it_tab_day.
        APPEND wa_util_mgate TO it_tab_day.

      WHEN '25'.
        it_tab_day-desc = 'Ship out'.
        LOOP AT it_day.
          READ TABLE lt_prod_act WITH KEY prdt_date = it_day-datum.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_shipout.
            it_tab_day-total = it_tab_day-total +
                               lt_prod_act-qty_shipout.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_SHIPOUT-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.

** Changed on 11/21/08 by Furong Wang
*           CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS03>.
*            IF SY-SUBRC = 0 AND <FS01> > 0.
*              <FS03> = ( <FS02> / ( <FS01> * W_UPH ) ) * 100.
*            ENDIF.
            CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs_uph>.
            CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0 AND  <fs_uph> > 0 .
              <fs03> = ( <fs02> / ( <fs01> *  <fs_uph> ) ) * 100.
            ENDIF.
** End of change on 11/21/08
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_day-if = 'C210'.
        it_tab_day-seq = '2'.
        it_tab_day-type = 'Actual'.
        it_tab_day-sub = it_tab_day-total.
        wa_actual = it_tab_day.
        APPEND it_tab_day.
        PERFORM cal_gap USING 'C210'.
        wa_shipout-type = 'Working Hours'.
        wa_shipout-seq = '4'.
        wa_shipout-if = 'C210'.
        wa_util_shipout-if = 'C210'.
        wa_util_shipout-type = 'Utilization'.
        wa_util_shipout-seq = '5'.
        IF wa_shipout-total > 0  AND w_uph_ave > 0.
          wa_util_shipout-total = it_tab_day-total /
                               ( wa_shipout-total * w_uph_ave ) * 100.
        ENDIF.
        IF wa_shipout-sub > 0  AND w_uph_ave > 0.
          wa_util_shipout-sub = it_tab_day-sub /
                               ( wa_shipout-sub * w_uph_ave ) * 100.
        ENDIF.
        APPEND wa_shipout TO it_tab_day.
        APPEND wa_util_shipout TO it_tab_day.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " get_act_data

*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_screen.
  LOOP AT SCREEN.
    IF screen-name = 'P_EXCEL'.
      screen-input = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM set_days                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_days.
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

  CLEAR: it_day, it_day[].
  IF w_month IS INITIAL.
    w_month = sy-datum+0(6).
    CONCATENATE sy-datum+0(6) '01' INTO z_beg_date.

  ELSE.
    CONCATENATE w_month '01' INTO z_beg_date.
  ENDIF.
  CALL FUNCTION 'HR_E_NUM_OF_DAYS_OF_MONTH'
    EXPORTING
      p_fecha        = z_beg_date
    IMPORTING
      number_of_days = w_max_day.
  .

  l_count = '01'.
  l_date = z_beg_date.
  z_max_date = z_beg_date + w_max_day - 1.

  WHILE l_count <= w_max_day.
    it_day-seq     = l_count.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
  ENDWHILE.

  IF NOT grid_container IS INITIAL.
    PERFORM build_field_catalog USING 'IT_TAB_DAY'.
    PERFORM assign_itab_to_alv.
  ENDIF.
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING p_l_kalid.
  SELECT SINGLE kalid INTO p_l_kalid
  FROM zvpp_capacity
 WHERE arbpl = 'T'   .
ENDFORM.                    " read_shop_calid
*---------------------------------------------------------------------*
*       FORM read_working_date                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PA_TYPE                                                       *
*  -->  PA_KALID                                                      *
*  -->  PA_WDATE                                                      *
*---------------------------------------------------------------------*
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
*&      Form  get_pir_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plan_data.
  DATA: lt_plan_day LIKE TABLE OF ztpp_plan_day WITH HEADER LINE,
        lt_plan_day_temp LIKE TABLE OF ztpp_plan_day WITH HEADER LINE,
        lt_emf LIKE TABLE OF ztpp_plan_day WITH HEADER LINE.
  DATA: wa_temp LIKE ztpp_plan_day.
  DATA: l_cn_body TYPE i,
        l_cn_paint TYPE i,
        l_cn_trim TYPE i,
        l_cn_signoff TYPE i,
        l_cn_cgate TYPE i,
        l_cn_mgate TYPE i,
        l_cn_shipout TYPE i,
        l_cn_uph TYPE i,
        l_cn_uph_b TYPE i,
        l_cn_uph_p TYPE i,
        l_index LIKE sy-tabix,
        l_next(1),
        l_next01(1),
        l_next04(1),
        l_next07(1),
        l_next18(1),
        l_next19(1),
        l_next23(1),
        l_next25(1).

  DATA: lw_xlist TYPE vrm_value.

*  CASE W_MODEL.
*    WHEN '1'.
*      W_MODEL_NAME ='EMF'.
*    WHEN '2'.
*      W_MODEL_NAME = 'CRA'.
*    WHEN '3'.
*      W_MODEL_NAME = 'C2F'.
*    WHEN OTHERS.
*      W_MODEL_NAME = SPACE.
*  ENDCASE.
  READ TABLE xlist INTO lw_xlist WITH KEY key = w_model.
  w_model_name = lw_xlist-text.
  IF w_model_name = 'ALL'.
    CLEAR w_model_name.
  ENDIF.

  IF w_model_name = space.
    SELECT * INTO TABLE lt_plan_day_temp
       FROM ztpp_plan_day
     WHERE prdt_date BETWEEN z_beg_date AND z_max_date.
    SORT lt_plan_day_temp BY prdt_date.
    LOOP AT lt_plan_day_temp.
      lt_plan_day = lt_plan_day_temp.
      CLEAR: lt_plan_day-model,lt_plan_day-cruser,lt_plan_day-crdate,
      lt_plan_day-crtime,lt_plan_day-chuser,lt_plan_day-chdate,
      lt_plan_day-chtime.
      COLLECT lt_plan_day.
    ENDLOOP.
    REFRESH: lt_plan_day_temp.
  ELSE.
    SELECT * INTO TABLE lt_plan_day
      FROM ztpp_plan_day
      WHERE prdt_date BETWEEN z_beg_date AND z_max_date
        AND model  = w_model_name.
** Cahnged by Furong on 04/20/10
** Changed on 05/08/13 to INF
*    IF W_MODEL_NAME <> 'EMF'.
    IF w_model_name <> 'C2F'.
** End of change ON 05/03/13
*    IF W_MODEL_NAME = 'CRA'.
** End of change
      SELECT * INTO TABLE lt_emf
        FROM ztpp_plan_day
       WHERE prdt_date BETWEEN z_beg_date AND z_max_date
** Changed on 05/08/13 to INF
*         AND MODEL  = 'EMF'.
        AND model  = 'C2F'.
** End of change ON 05/03/13
*      LOOP AT LT_PLAN_DAY.
*        READ TABLE LT_EMF WITH KEY PRDT_DATE = LT_PLAN_DAY-PRDT_DATE.
*        IF SY-SUBRC = 0.
*          LT_PLAN_DAY-UPH = LT_EMF-UPH.
*          MODIFY LT_PLAN_DAY.
*        ENDIF.
*        CLEAR: LT_PLAN_DAY.
*      ENDLOOP.
    ENDIF.
  ENDIF.

  CLEAR: it_tab_day.
  LOOP AT it_day.
    CLEAR: lt_plan_day.
    READ TABLE lt_plan_day WITH KEY prdt_date = it_day-datum.
    IF sy-subrc = 0.
      CONCATENATE 'IT_TAB_DAY-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-qty_plan.
        it_tab_day-total = it_tab_day-total + lt_plan_day-qty_plan.
        IF it_day-datum < sy-datum.
          it_tab_day-sub = it_tab_day-sub + lt_plan_day-qty_plan.
        ENDIF.
      ENDIF.

** Added by Furong on 11/21/08
** Get UPH
*      IF W_MODEL_NAME = 'EMF' OR  W_MODEL_NAME = ' '.
      IF w_model_name = 'C2F' OR  w_model_name = ' '.
        CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = lt_plan_day-uph.
          IF <fs01> > 0.
            l_cn_uph = l_cn_uph + 1.
            wa_uph-total = wa_uph-total + lt_plan_day-uph.
          ENDIF.
        ENDIF.
** Changed on 05/08/13 for Body and Paint UPH
        CONCATENATE 'WA_UPH_B-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = lt_plan_day-uph_b.
          IF <fs01> > 0.
            l_cn_uph_b = l_cn_uph_b + 1.
            wa_uph_b-total = wa_uph_b-total + lt_plan_day-uph_b.
          ENDIF.
        ENDIF.
        CONCATENATE 'WA_UPH_P-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = lt_plan_day-uph_p.
          IF <fs01> > 0.
            l_cn_uph_p = l_cn_uph_p + 1.
            wa_uph_p-total = wa_uph_p-total + lt_plan_day-uph_p.
          ENDIF.
        ENDIF.
** End on 05/08/13
      ELSE.
        READ TABLE lt_emf WITH KEY prdt_date = it_day-datum.
        IF sy-subrc = 0.
          CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            <fs01> = lt_emf-uph.
            IF <fs01> > 0.
              l_cn_uph = l_cn_uph + 1.
              wa_uph-total = wa_uph-total + lt_emf-uph.
            ENDIF.
          ENDIF.
** Changed on 05/08/13 for Body and Paint UPH
          CONCATENATE 'WA_UPH_B-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            <fs01> = lt_emf-uph_b.
            IF <fs01> > 0.
              l_cn_uph_b = l_cn_uph_b + 1.
              wa_uph_b-total = wa_uph_b-total + lt_emf-uph_b.
            ENDIF.
          ENDIF.
          CONCATENATE 'WA_UPH_P-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            <fs01> = lt_emf-uph_p.
            IF <fs01> > 0.
              l_cn_uph_p = l_cn_uph_p + 1.
              wa_uph_p-total = wa_uph_p-total + lt_emf-uph_p.
            ENDIF.
          ENDIF.
** End on 05/08/13
        ENDIF.
      ENDIF.
** End of addition
** Get work hours
      CONCATENATE 'WA_BODY-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_01.
        wa_body-total = wa_body-total + lt_plan_day-wk_hr_01.
        wa_body-sub = wa_body-total.
      ENDIF.

      CONCATENATE 'WA_PAINT-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_04.
        wa_paint-total = wa_paint-total + lt_plan_day-wk_hr_04.
        wa_paint-sub = wa_paint-total.
      ENDIF.

      CONCATENATE 'WA_TRIM-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_07.
        wa_trim-total = wa_trim-total + lt_plan_day-wk_hr_07.
        wa_trim-sub = wa_trim-total.
      ENDIF.

      CONCATENATE 'WA_SIGNOFF-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_18.
        wa_signoff-total = wa_signoff-total + lt_plan_day-wk_hr_18.
        wa_signoff-sub = wa_signoff-total.
      ENDIF.

      CONCATENATE 'WA_CGATE-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_19.
        wa_cgate-total = wa_cgate-total + lt_plan_day-wk_hr_19.
        wa_cgate-sub = wa_cgate-total.
      ENDIF.

      CONCATENATE 'WA_MGATE-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_23.
        wa_mgate-total = wa_mgate-total + lt_plan_day-wk_hr_23.
        wa_mgate-sub = wa_mgate-total.
      ENDIF.

      CONCATENATE 'WA_SHIPOUT-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-wk_hr_25.
        wa_shipout-total = wa_shipout-total + lt_plan_day-wk_hr_25.
        wa_shipout-sub = wa_shipout-total.
      ENDIF.
** Get untilization
      CONCATENATE 'WA_UTIL_BODY-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_01.
        IF <fs01> > 0.
          l_cn_body = l_cn_body + 1.
          wa_util_body-total = wa_util_body-total
           + lt_plan_day-util_01.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_PAINT-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_04.
        IF <fs01> > 0.
          l_cn_paint = l_cn_paint + 1.

          wa_util_paint-total = wa_util_paint-total +
                                lt_plan_day-util_04.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_TRIM-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_07.
        IF <fs01> > 0.
          l_cn_trim = l_cn_trim + 1.

          wa_util_trim-total = wa_util_trim-total
           + lt_plan_day-util_07.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_18.
        wa_util_signoff-total = wa_util_signoff-total +
                                lt_plan_day-util_18.
        IF <fs01> > 0.
          l_cn_signoff = l_cn_signoff + 1.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_CGATE-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_19.
        IF <fs01> > 0.
          l_cn_cgate = l_cn_cgate + 1.

          wa_util_cgate-total = wa_util_cgate-total
           + lt_plan_day-util_19.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_MGATE-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_23.
        IF <fs01> > 0.
          l_cn_mgate = l_cn_mgate + 1.

          wa_util_mgate-total = wa_util_mgate-total
           + lt_plan_day-util_23.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_day-util_25.
        IF <fs01> > 0.
          l_cn_shipout = l_cn_shipout + 1.

          wa_util_shipout-total = wa_util_shipout-total +
                                  lt_plan_day-util_25.
        ENDIF.
      ENDIF.
    ELSE.
** Added by Furong on 11/21/08
** Get UPH
** Changed by Furong on 04/21/10
*      IF W_MODEL_NAME <> 'EMF'.
      IF w_model_name <> 'C2F'.
*      IF W_MODEL_NAME = 'CRA'.
** End of changeon 04/21/10
        READ TABLE lt_emf WITH KEY prdt_date = it_day-datum.
        IF sy-subrc = 0.
          CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            <fs01> = lt_emf-uph.
            IF <fs01> > 0.
              l_cn_uph = l_cn_uph + 1.
              wa_uph-total = wa_uph-total + lt_emf-uph.
            ENDIF.
          ENDIF.
** Changed by Furong on 05/08/13
          CONCATENATE 'WA_UPH_B-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            <fs01> = lt_emf-uph_b.
            IF <fs01> > 0.
              l_cn_uph_b = l_cn_uph_b + 1.
              wa_uph_b-total = wa_uph_b-total + lt_emf-uph_b.
            ENDIF.
          ENDIF.
          CONCATENATE 'WA_UPH_P-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            <fs01> = lt_emf-uph_p.
            IF <fs01> > 0.
              l_cn_uph_p = l_cn_uph_p + 1.
              wa_uph_p-total = wa_uph_p-total + lt_emf-uph_p.
            ENDIF.
          ENDIF.
** End on 05/08/13
        ENDIF.
      ENDIF.
** End of addition
    ENDIF.
  ENDLOOP.
  it_tab_day-if = 'C310'.
  it_tab_day-desc = 'Plan'.
  it_tab_day-seq = '1'.
  APPEND it_tab_day.
  wa_plan = it_tab_day.

  wa_uph-sub = wa_uph-total =  wa_uph-total / l_cn_uph.
  w_uph_ave =  wa_uph-sub.

  wa_uph-desc = 'UPH(Trim)'.
  wa_uph-seq = '8'.
  APPEND wa_uph TO it_tab_day.


** Changed by Furong on 05/08/13
  IF  NOT l_cn_uph_b IS INITIAL.
    wa_uph_b-sub = wa_uph_b-total =  wa_uph_b-total / l_cn_uph_b.
  ENDIF.
  w_uph_b_ave =  wa_uph_b-sub.

  wa_uph_b-desc = 'UPH(Body)'.
  wa_uph_b-seq = '8'.
  APPEND wa_uph_b TO it_tab_day.

  IF  NOT l_cn_uph_p IS INITIAL.
    wa_uph_p-sub = wa_uph_p-total =  wa_uph_p-total / l_cn_uph_p.
  ENDIF.
  w_uph_p_ave =  wa_uph_p-sub.

  wa_uph_p-desc = 'UPH(Paint)'.
  wa_uph_p-seq = '8'.
  APPEND wa_uph_p TO it_tab_day.
** End on 05/08/13

  wa_util_body-sub = wa_util_body-total =  wa_util_body-total /
                                           l_cn_body.
  wa_util_paint-sub = wa_util_paint-total =  wa_util_paint-total /
                                            l_cn_paint.
  wa_util_trim-sub = wa_util_trim-total =  wa_util_trim-total /
                                           l_cn_trim.
  wa_util_signoff-sub = wa_util_signoff-total
   =  wa_util_signoff-total / l_cn_signoff.
  wa_util_cgate-sub = wa_util_cgate-total =  wa_util_cgate-total /
                                            l_cn_cgate.

  wa_util_mgate-sub = wa_util_mgate-total =  wa_util_mgate-total /
                                             l_cn_mgate.

  wa_util_shipout-sub = wa_util_shipout-total
   =  wa_util_shipout-total / l_cn_shipout.
ENDFORM.                    "GET_PLAN_DATA

INCLUDE zapp_prod_plan_pbo.

INCLUDE zapp_prod_plan_pai.
*&---------------------------------------------------------------------*
*&      Form  SET_WEEKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_weeks.
*  DATA: BEGIN OF LT_WEEK OCCURS 0,
*        SQDT LIKE SY-DATUM,
*        END OF LT_WEEK.
*  DATA: L_CN(2) TYPE N.
*
*  CLEAR: IT_MONTH, IT_MONTH[].
*  SELECT SQDT INTO TABLE LT_WEEK
*   FROM ZTPP_PMT07JB_A
*    WHERE GUBB EQ 'B'.
*
*  SORT LT_WEEK BY SQDT.
*  DELETE ADJACENT DUPLICATES FROM LT_WEEK COMPARING SQDT.
*
*  L_CN = '04'.
*  LOOP AT LT_WEEK.
*    IT_WEEK-SEQ = L_CN.
*    IT_WEEK-DATUM = LT_WEEK-SQDT.
*    APPEND IT_WEEK.
*    L_CN =  L_CN + 1.
*  ENDLOOP.
*
*  DESCRIBE TABLE IT_WEEK LINES L_CN.
*  READ TABLE IT_WEEK INDEX 1.
*  Z_BEG_WEEK = IT_WEEK-DATUM.
*
*  READ TABLE IT_WEEK INDEX L_CN.
*  Z_MAX_WEEK = IT_WEEK-DATUM.

ENDFORM.                    " SET_WEEKS
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line.
  DATA: lt_celltab TYPE lvc_t_styl,
        w_celltab TYPE lvc_s_styl,
        l_index TYPE i,
        l_mode TYPE raw4.

  LOOP AT it_tab_day.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF it_tab_day-seq = '1' OR it_tab_day-seq = '4'
** uph = 8
                            OR it_tab_day-seq = '8'.
      l_mode = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      l_mode = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    w_celltab-fieldname = 'DESC'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'TYPE'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'TOTAL'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'SUB'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'QTYD_01'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_02'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_03'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_04'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_05'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_06'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_07'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_08'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_09'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_10'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_11'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_12'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_13'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_14'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_15'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_16'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_17'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_18'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_19'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_20'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_21'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    w_celltab-fieldname = 'QTYD_22'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_23'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_24'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_25'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_26'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_27'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_28'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_29'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_30'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYD_31'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    INSERT LINES OF lt_celltab INTO TABLE it_tab_day-celltab.
    MODIFY it_tab_day INDEX l_index.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data.
  CLEAR: it_tab_day, it_tab_day[], it_tab_month, it_tab_month[].
  CLEAR: wa_plan, wa_actual, wa_gap, wa_body, wa_trim, wa_paint,
         wa_signoff, wa_cgate, wa_shipout, wa_util_body, wa_util_trim,
         wa_util_paint, wa_util_signoff, wa_util_cgate,wa_util_shipout,
         wa_hr, wa_util, wa_mgate,wa_util_mgate, wa_uph, w_uph_ave,
         wa_uph_b, w_uph_b_ave, wa_uph_p, w_uph_p_ave.
  CLEAR: w_refresh.

ENDFORM.                    " CLEAR_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_data.
  it_rp-rp = '01'.
  APPEND it_rp.
  it_rp-rp = '04'.
  APPEND it_rp.
  it_rp-rp = '07'.
  APPEND it_rp.
  it_rp-rp = '18'.
  APPEND it_rp.
  it_rp-rp = '19'.
  APPEND it_rp.
  it_rp-rp = '23'.
  APPEND it_rp.
  it_rp-rp = '25'.
  APPEND it_rp.

ENDFORM.                    " SET_DATA
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.
  PERFORM clear_data.

  PERFORM get_data.
  IF w_no_data = 'X'.
    CLEAR: w_no_data.
    EXIT.
  ENDIF.
  IF sy-tcode = 'ZAPP_PROD_PLAN_D'.
    PERFORM select_edit_line.
  ENDIF.
*  PERFORM PREPARE_DISPLAY.
ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  CAL_GAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_gap USING p_color.
  LOOP AT it_day.
    CONCATENATE 'WA_PLAN-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      CONCATENATE 'WA_ACTUAL-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs02>.
      IF sy-subrc = 0.
        CONCATENATE 'WA_GAP-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs03>.
        IF sy-subrc = 0.
          <fs03> = <fs02> - <fs01>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  wa_gap-if = p_color.
  wa_gap-total = wa_actual-total - wa_plan-total.
  wa_gap-sub = wa_actual-sub - wa_plan-sub.
  wa_gap-seq = '3'.
  wa_gap-type = 'Gap'.
  APPEND wa_gap TO it_tab_day.
ENDFORM.                    " CAL_GAP
*&---------------------------------------------------------------------*
*&      Form  GET_WORK_HOUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM GET_WORK_HOUR.
**  LOOP AT IT_DAY.
**    CONCATENATE 'WA_PLAN-QTYD_' IT_DAY-SEQ INTO L_TEXT.
**    ASSIGN (L_TEXT) TO <FS01>.
**    IF SY-SUBRC = 0.
**      CONCATENATE 'WA_ACTUAL-QTYD_' IT_DAY-SEQ INTO L_TEXT.
**      ASSIGN (L_TEXT) TO <FS02>.
**      IF SY-SUBRC = 0.
**        CONCATENATE 'WA_GAP-QTYD_' IT_DAY-SEQ INTO L_TEXT.
**        ASSIGN (L_TEXT) TO <FS03>.
**        IF SY-SUBRC = 0.
**          <FS03> = <FS02> - <FS01>.
**        ENDIF.
**      ENDIF.
**    ENDIF.
**  ENDLOOP.
**  WA_GAP-TOTAL = WA_ACTUAL-TOTAL - WA_PLAN-TOTAL.
**  WA_GAP-SUB = WA_ACTUAL-SUB - WA_PLAN-SUB.
**  WA_GAP-SEQ = '3'.
**  WA_GAP-TYPE = 'Gap'.
*  APPEND WA_GAP TO IT_TAB_DAY.
*
*ENDFORM.                    " GET_WORK_HOUR
*&---------------------------------------------------------------------*
*&      Form  RECAL_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM recal_qty.
*  DATA: LT_CELL TYPE LVC_T_CELL.
*  DATA: LW_CELL TYPE LVC_S_CELL.
*  DATA: WA_ROW TYPE LVC_S_ROW,
*        WA_COL TYPE LVC_S_COL,
*        WA_STBL TYPE LVC_S_STBL.
*  DATA: L_LINE TYPE I,
*        L_TEXT(35),
*        L_CN(2) TYPE N,
*        L_DW(2),
*        L_INDEX LIKE SY-TABIX,
*        L_QTY LIKE IT_OUTPUT-QTYD_01.
*
*  CALL METHOD ALV_GRID->GET_SELECTED_CELLS
*           IMPORTING ET_CELL = LT_CELL.
*  IF LT_CELL[] IS INITIAL.
*  ELSE.
*    READ TABLE LT_CELL INTO LW_CELL INDEX 1.
*    WA_ROW = LW_CELL-ROW_ID.
*    WA_COL = LW_CELL-COL_ID.
*  ENDIF.
*
*  CALL METHOD CL_GUI_CFW=>FLUSH.
*
*  IF SY-SUBRC NE 0.
*    W_REPID = SY-REPID.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = W_REPID
*              TXT2  = SY-SUBRC
*              TXT1  = 'Error found during flushing of ALV Grid Control'
*.
*    EXIT.
*  ENDIF.
*
*  L_CN = WA_COL-FIELDNAME+5(2).
*  L_DW = WA_COL-FIELDNAME+3(2).
*  IF L_CN < '00' OR L_CN > '31'.
*    MESSAGE E000(ZZ) WITH TEXT-M13.
*  ENDIF.
*  L_INDEX = WA_ROW-INDEX - 1.
*  READ TABLE IT_TAB_DAY INDEX L_INDEX.
*  IF SY-SUBRC NE 0.  "OR IT_OUTPUT-SEQ <> '1'.
*    MESSAGE E000(ZZ) WITH TEXT-M13.
*  ENDIF.
*  CONCATENATE 'IT_TAB_DAY-QTY' L_DW L_CN INTO L_TEXT.
*  ASSIGN (L_TEXT) TO <FS01>.
*  IF SY-SUBRC = 0.
*    L_QTY = <FS01>.
*    READ TABLE IT_OUTPUT INDEX WA_ROW-INDEX.
*    CONCATENATE 'IT_OUTPUT-QTY' L_DW L_CN INTO L_TEXT.
*    ASSIGN (L_TEXT) TO <FS-QTY>.
*    IF SY-SUBRC = 0.
*      <FS-QTY> = L_QTY.
*      MODIFY IT_OUTPUT INDEX WA_ROW-INDEX.
*      WA_STBL-ROW = 'X'.
*      WA_STBL-COL = 'X'.
*      CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
*       EXPORTING IS_STABLE = WA_STBL.
*
*      MESSAGE S000(ZZ) WITH 'Data successfully replaced'.
*    ENDIF.
*  ELSE.
*    MESSAGE S000(ZZ) WITH 'Replacement Error'.
*  ENDIF.
*
ENDFORM.                    " RECAL_QTY
*&---------------------------------------------------------------------*
*&      Form  SET_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_month.
  l_cn = '00'.
  REFRESH: it_month.
  WHILE l_cn < 12.
    l_cn = l_cn + 1.
    it_month-seq = l_cn.
    SELECT SINGLE ktx INTO it_month-desc
      FROM t247
      WHERE spras = 'EN'
       AND mnr = l_cn.
    APPEND it_month.
  ENDWHILE.
  IF w_year IS INITIAL.
    w_year = sy-datum+0(4).
    CONCATENATE sy-datum+0(4) '0101' INTO z_beg_date.
    CONCATENATE sy-datum+0(4) '1231' INTO z_max_date.
  ELSE.
    CONCATENATE w_year '0101' INTO z_beg_date.
    CONCATENATE w_year '1231' INTO z_max_date.
  ENDIF.
ENDFORM.                    " SET_month
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0792   text
*----------------------------------------------------------------------*
FORM build_field_catalog_300 USING  p_itab.
  DATA: lw_itab TYPE slis_tabname,
      lw_waers LIKE t001-waers,
      l_rqty(9),
      l_datum(8).


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

                                  'S' 'DESC'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'RP',
                                  'E' 'OUTPUTLEN'   '7'.


  PERFORM setting_fieldcat TABLES it_fieldcat USING :
                                     'S' 'TYPE'   ' ',
                                     ' ' 'KEY'         'X',
                                     ' ' 'COLTEXT'     'Element',
                                     'E' 'OUTPUTLEN'   '15',

                                     'S' 'TOTAL'   ' ',
                                     ' ' 'COLTEXT'     'Year Sum',
                                     ' ' 'DECIMALS'  '2',
                                     'E' 'OUTPUTLEN'   '10',

                                     'S' 'SUB'        ' ',
                                     ' ' 'COLTEXT'     'As of Mth',
                                     ' ' 'DECIMALS'  '2',
                                     'E' 'OUTPUTLEN'   '10'
                    .

  LOOP AT it_month.
    CONCATENATE 'QTYD_' it_month-seq INTO l_rqty.

    PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                   'S' l_rqty        ' ',
                                   ' ' 'COLTEXT'     it_month-desc,
                                   ' ' 'DECIMALS'  '2',
                                   'E' 'OUTPUTLEN'   '9'.
    CLEAR: l_rqty.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELD_CATALOG_300
