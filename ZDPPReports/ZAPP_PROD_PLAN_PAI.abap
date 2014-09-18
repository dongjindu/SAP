*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'INQ'.
      PERFORM select_data.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
*    WHEN 'CAL'.
*       PERFORM RECAL_STOCK.
    WHEN 'REF'.
      PERFORM refersh_data.
    WHEN 'SAVE'.
      IF sy-tcode = 'ZAPP_PROD_PLAN_D'.
        IF w_model_name = space.
          MESSAGE i001 WITH 'Cannot save data for all models'.
        ELSE.
          PERFORM save_data.
        ENDIF.
      ELSE.
        MESSAGE i001 WITH 'No Authorization to Access'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  DATA: lt_plan_day LIKE TABLE OF ztpp_plan_day WITH HEADER LINE.
  DATA: l_qty1(13),
        l_index LIKE sy-tabix.

  READ TABLE it_tab_day WITH KEY seq = '1'.
  lt_plan_day-hkcode = 'H'.
  lt_plan_day-plant = 'A1'.
  lt_plan_day-model = w_model_name.
  lt_plan_day-crdate = sy-datum.
  lt_plan_day-crtime = sy-uzeit.
  lt_plan_day-chdate = sy-datum.
  lt_plan_day-chtime = sy-uzeit.
  lt_plan_day-cruser = sy-uname.

  IF w_refresh IS INITIAL.
    MESSAGE i000 WITH 'Please refresh the data before saving the data'.
    EXIT.
  ENDIF.
  LOOP AT it_day.
    lt_plan_day-prdt_date = it_day-datum.

    CONCATENATE 'WA_PLAN-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-qty_plan = <fs01>.
    ENDIF.

** Changed by Furong on 11/21/08 for UPH
*    IF W_MODEL_NAME = 'EMF'.
    IF w_model_name = 'C2F'.
      CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        lt_plan_day-uph = <fs01>.
      ENDIF.

      CONCATENATE 'WA_UPH_B-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        lt_plan_day-uph_b = <fs01>.
      ENDIF.

      CONCATENATE 'WA_UPH_P-QTYD_' it_day-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        lt_plan_day-uph_p = <fs01>.
      ENDIF.
    ENDIF.
** End of change
    CONCATENATE 'WA_BODY-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_01 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_PAINT-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_04 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_TRIM-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_07 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_SIGNOFF-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_18 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_CGATE-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_19 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_MGATE-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_23 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_SHIPOUT-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-wk_hr_25 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_BODY-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_01 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_PAINT-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_04 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_TRIM-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_07 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_18 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_CGATE-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_19 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_MGATE-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_23 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' it_day-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_day-util_25 = <fs01>.
    ENDIF.

    APPEND lt_plan_day.
  ENDLOOP.

  MODIFY ztpp_plan_day FROM TABLE lt_plan_day.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s000(zz) WITH text-m16.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m17.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  refersh_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refersh_data.
  DATA: l_index LIKE sy-tabix,
        l_rp LIKE it_tab_day-desc,
        l_count TYPE i,
        l_ct_uph TYPE i,
        l_ct_uph_b TYPE i,
        l_ct_uph_p TYPE i.
*        L_SUB_CT TYPE I.

  w_refresh = 'X'.
  CLEAR: wa_plan, wa_actual, wa_gap, wa_body, wa_trim, wa_paint,
         wa_signoff, wa_cgate, wa_shipout, wa_util_body, wa_util_trim,
         wa_util_paint, wa_util_signoff, wa_util_cgate,
         wa_util_shipout,
         wa_hr, wa_util, wa_util_cgate, wa_cgate.

  LOOP AT it_tab_day.
    CASE it_tab_day-seq.
      WHEN '1'.
        l_index = sy-tabix.
        wa_plan = it_tab_day.
        CLEAR: wa_plan-total, wa_plan-sub.
        LOOP AT it_day.
          CONCATENATE 'WA_PLAN-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            wa_plan-total = wa_plan-total + <fs01>.
            IF it_day-datum < sy-datum.
              wa_plan-sub = wa_plan-sub + <fs01>.
            ENDIF.
          ENDIF.
        ENDLOOP.
        MODIFY it_tab_day FROM wa_plan INDEX l_index
        TRANSPORTING total sub.
      WHEN '8'.
        l_index = sy-tabix.
** Changed on 05/08/13 for BODY & PAINT UPH
*        WA_UPH = IT_TAB_DAY.
*        CLEAR: WA_UPH-TOTAL, WA_UPH-SUB, L_CT_UPH.
*        LOOP AT IT_DAY.
*          CONCATENATE 'WA_UPH-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS01>.
*          IF SY-SUBRC = 0 AND <FS01> > 0.
*            WA_UPH-TOTAL = WA_UPH-TOTAL + <FS01>.
*            L_CT_UPH = L_CT_UPH + 1.
**            IF IT_DAY-DATUM < SY-DATUM.
*            WA_UPH-SUB = WA_UPH-SUB + <FS01>.
**              L_SUB_CT = L_SUB_CT + 1.
**            ENDIF.
*          ENDIF.
*        WA_UPH-TOTAL = WA_UPH-TOTAL / L_CT_UPH.
*        WA_UPH-SUB = WA_UPH-SUB / L_CT_UPH.
*        W_UPH_AVE =  WA_UPH-SUB.
*        MODIFY IT_TAB_DAY FROM WA_UPH INDEX L_INDEX
*        TRANSPORTING TOTAL SUB.


        CASE it_tab_day-desc.
          WHEN 'UPH(Trim)'.
            wa_uph = it_tab_day.
            CLEAR: wa_uph-total, wa_uph-sub, l_ct_uph.
            LOOP AT it_day.
              CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
              ASSIGN (l_text) TO <fs01>.
              IF sy-subrc = 0 AND <fs01> > 0.
                wa_uph-total = wa_uph-total + <fs01>.
                l_ct_uph = l_ct_uph + 1.
*            IF IT_DAY-DATUM < SY-DATUM.
                wa_uph-sub = wa_uph-sub + <fs01>.
*              L_SUB_CT = L_SUB_CT + 1.
*            ENDIF.
              ENDIF.
            ENDLOOP.
            wa_uph-total = wa_uph-total / l_ct_uph.
            wa_uph-sub = wa_uph-sub / l_ct_uph.
            w_uph_ave =  wa_uph-sub.
            MODIFY it_tab_day FROM wa_uph INDEX l_index
            TRANSPORTING total sub.

          WHEN 'UPH(Body)'.
            wa_uph_b = it_tab_day.
            CLEAR: wa_uph_b-total, wa_uph_b-sub, l_ct_uph_b.
            LOOP AT it_day.

              CONCATENATE 'WA_UPH_B-QTYD_' it_day-seq INTO l_text.
              ASSIGN (l_text) TO <fs01>.
              IF sy-subrc = 0 AND <fs01> > 0.
                wa_uph_b-total = wa_uph_b-total + <fs01>.
                l_ct_uph_b = l_ct_uph_b + 1.
                wa_uph_b-sub = wa_uph_b-sub + <fs01>.
              ENDIF.
            ENDLOOP.
            wa_uph_b-total = wa_uph_b-total / l_ct_uph_b.
            wa_uph_b-sub = wa_uph_b-sub / l_ct_uph_b.
            w_uph_b_ave =  wa_uph_b-sub.
            MODIFY it_tab_day FROM wa_uph_b INDEX l_index
            TRANSPORTING total sub.
          WHEN 'UPH(Paint)'.
            wa_uph_p = it_tab_day.
            CLEAR: wa_uph_p-total, wa_uph_p-sub, l_ct_uph_p.
            LOOP AT it_day.
              CONCATENATE 'WA_UPH_P-QTYD_' it_day-seq INTO l_text.
              ASSIGN (l_text) TO <fs01>.
              IF sy-subrc = 0 AND <fs01> > 0.
                wa_uph_p-total = wa_uph_p-total + <fs01>.
                l_ct_uph_p = l_ct_uph_p + 1.
                wa_uph_p-sub = wa_uph_p-sub + <fs01>.
              ENDIF.
            ENDLOOP.
            wa_uph_p-total = wa_uph_p-total / l_ct_uph_p.
            wa_uph_p-sub = wa_uph_p-sub / l_ct_uph_p.
            w_uph_p_ave =  wa_uph_p-sub.
            MODIFY it_tab_day FROM wa_uph_p INDEX l_index
            TRANSPORTING total sub.
        ENDCASE.

** End on 05/08/13
      WHEN '2'.
        CLEAR: wa_actual, wa_gap, wa_hr, wa_util.
        l_index = sy-tabix + 1.
        wa_actual = it_tab_day.
        l_rp = it_tab_day-desc.
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
*                WA_actual-TOTAL = <FS02>.
                wa_gap-total = wa_gap-total + <fs03>.
                IF it_day-datum < sy-datum.
                  wa_gap-sub = wa_gap-sub + <fs03>.
*                  WA_actual-SUB = <FS02>.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
*        WA_GAP-TOTAL = WA_ACTUAL-TOTAL - WA_PLAN-TOTAL.
*        WA_GAP-SUB = WA_ACTUAL-SUB - WA_PLAN-SUB.
        MODIFY it_tab_day FROM wa_gap INDEX l_index TRANSPORTING
             total sub qtyd_01
             qtyd_02 qtyd_03 qtyd_04 qtyd_05 qtyd_06 qtyd_07 qtyd_08
             qtyd_09 qtyd_10 qtyd_11 qtyd_12 qtyd_13 qtyd_14 qtyd_15
             qtyd_16 qtyd_17 qtyd_18 qtyd_19 qtyd_20 qtyd_21 qtyd_22
             qtyd_23 qtyd_24 qtyd_25 qtyd_26 qtyd_27 qtyd_28 qtyd_29
             qtyd_30 qtyd_31.
      WHEN 4.
        l_index = sy-tabix.
        wa_hr = it_tab_day.
        CLEAR: wa_hr-total, wa_hr-sub.
        LOOP AT it_day.
          CONCATENATE 'WA_HR-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            IF <fs01> > 0.
              wa_hr-total = wa_hr-total + <fs01>.
              IF it_day-datum < sy-datum.
                wa_hr-sub = wa_hr-sub + <fs01>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
*        WA_HR-SUB = WA_HR-TOTAL.
        CLEAR: l_count.
        MODIFY it_tab_day FROM wa_hr INDEX l_index
        TRANSPORTING total sub.
        CASE l_rp.
          WHEN 'Body in'.
            wa_body = wa_hr.
          WHEN 'Paint out'.
            wa_paint = wa_hr.
          WHEN 'Trim in'.
            wa_trim = wa_hr.
          WHEN 'Sign off'.
            wa_signoff = wa_hr.
          WHEN 'C/Gate'.
            wa_cgate = wa_hr.
          WHEN 'M/Gate'.
            wa_mgate = wa_hr.
          WHEN 'Ship out'.
            wa_shipout = wa_hr.
        ENDCASE.
      WHEN 5.
        l_index = sy-tabix.
        wa_util = it_tab_day.
        CLEAR: wa_util-total, wa_util-sub.
        LOOP AT it_day.
          CONCATENATE 'WA_UTIL-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.

          CONCATENATE 'WA_ACTUAL-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs02>.
          CONCATENATE 'WA_HR-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs03>.
          CONCATENATE 'WA_UPH-QTYD_' it_day-seq INTO l_text.
          ASSIGN (l_text) TO <fs_uph>.
          IF sy-subrc = 0 AND <fs03> > 0 AND <fs_uph> > 0.
            <fs01> = ( <fs02> / ( <fs03> * <fs_uph> ) ) * 100.
          ENDIF.

*          IF <FS01> > 0.
*            WA_UTIL-TOTAL = WA_UTIL-TOTAL + <FS01>.
*            L_COUNT = L_COUNT + 1.
*            IF IT_DAY-DATUM <= SY-DATUM.
*              WA_UTIL-SUB = WA_UTIL-SUB + <FS01>.
*              L_SUB_CT = L_SUB_CT + 1.
*            ENDIF.
*          ENDIF.

        ENDLOOP.
** Changed by Furong on 12/07/07
        IF wa_hr-total > 0  AND w_uph_ave > 0.
          wa_util-total = wa_actual-total / ( wa_hr-total * w_uph_ave )
          *
                  100.
        ENDIF.
        IF wa_hr-sub > 0  AND w_uph_ave > 0.
          wa_util-sub = wa_actual-sub / ( wa_hr-sub * w_uph_ave ) * 100.
        ENDIF.
*        WA_UTIL-TOTAL = WA_UTIL-TOTAL / L_COUNT.
*        WA_UTIL-SUB = WA_UTIL-SUB / L_SUB_CT.
*        WA_UTIL-SUB = WA_UTIL-TOTAL.
** end of change
        CLEAR: l_count.
        MODIFY it_tab_day FROM wa_util INDEX l_index.
        CASE l_rp.
          WHEN 'Body in'.
            wa_util_body = wa_util.
          WHEN 'Paint out'.
            wa_util_paint = wa_util.
          WHEN 'Trim in'.
            wa_util_trim = wa_util.
          WHEN 'Sign off'.
            wa_util_signoff = wa_util.
          WHEN 'C/Gate'.
            wa_util_cgate = wa_util.
          WHEN 'M/Gate'.
            wa_util_mgate = wa_util.
          WHEN 'Ship out'.
            wa_util_shipout = wa_util.
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " refersh_data
*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE ok_code.
    WHEN 'INQ'.
      PERFORM select_data_300.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
*    WHEN 'CAL'.
*       PERFORM RECAL_STOCK.
    WHEN 'REF'.
      PERFORM refersh_data_300.
    WHEN 'SAVE'.
      IF sy-tcode = 'ZAPP_PROD_PLAN_M'.
        IF w_model_name = space.
          MESSAGE i001 WITH 'Cannot save data for all models'.
        ELSE.
          PERFORM save_data_300.
        ENDIF.
      ELSE.
        MESSAGE i001 WITH 'No Authorization to Access'.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_300.
  PERFORM clear_data.
  PERFORM set_month.
  PERFORM get_data_300.
  IF w_no_data = 'X'.
    CLEAR: w_no_data.
    EXIT.
  ENDIF.
  IF sy-tcode = 'ZAPP_PROD_PLAN_M'.
    PERFORM select_edit_line_300.
  ENDIF.
*  PERFORM PREPARE_DISPLAY.

ENDFORM.                    " SELECT_DATA_300
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_300.
*  PERFORM SET_MONTH.
  PERFORM get_plan_data_300.
  PERFORM get_actual_data_300.
ENDFORM.                    " GET_DATA_300
*&---------------------------------------------------------------------*
*&      Form  GET_PLAN_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plan_data_300.
  DATA: lt_plan_month LIKE TABLE OF ztpp_plan_month WITH HEADER LINE,
        lt_plan_month_temp LIKE TABLE OF ztpp_plan_month WITH HEADER
                                                              LINE.
  DATA: wa_temp LIKE ztpp_plan_month.

  DATA: l_cn_body TYPE i,
        l_cn_paint TYPE i,
        l_cn_trim TYPE i,
        l_cn_signoff TYPE i,
        l_cn_cgate TYPE i,
        l_cn_mgate TYPE i,
        l_cn_shipout TYPE i,
        l_cur_month(2),
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

  l_cur_month = sy-datum+4(2).
*  IF W_MODEL = 1.
*    W_MODEL_NAME ='EMF'.
*  ELSE.
*    W_MODEL_NAME = 'CRA'.
*  ENDIF.
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
    SELECT * INTO TABLE lt_plan_month_temp
     FROM ztpp_plan_month
     WHERE prdt_year = w_year.
*     loop at LT_PLAN_month_temp.
*     LT_PLAN_month = LT_PLAN_month_temp.
*     clear: LT_PLAN_month-model,LT_PLAN_month-CRUSER,
*            LT_PLAN_month-CRDATE, LT_PLAN_month-CRTIME,
*            LT_PLAN_month-CHUSER,LT_PLAN_month-CHDATE,
*            LT_PLAN_month-CHTIME .
*     collect LT_PLAN_month.
*   endloop.
    SORT lt_plan_month_temp BY prdt_month.
    LOOP AT lt_plan_month_temp.
      lt_plan_month = lt_plan_month_temp.
      CLEAR: lt_plan_month-model,lt_plan_month-cruser,
             lt_plan_month-crdate,
             lt_plan_month-crtime,lt_plan_month-chuser,
             lt_plan_month-chdate,lt_plan_month-chtime.
*      L_INDEX = SY-TABIX + 1.
*      READ TABLE LT_PLAN_MONTH_TEMP INTO WA_TEMP INDEX L_INDEX.
*      IF WA_TEMP-PRDT_MONTH = LT_PLAN_MONTH_TEMP-PRDT_MONTH.
*        IF WA_TEMP-UTIL_01 <> 0 AND LT_PLAN_MONTH_TEMP-UTIL_01 <> 0.
*          L_NEXT01 = 'X'.
*          LT_PLAN_MONTH-UTIL_01 = LT_PLAN_MONTH-UTIL_01 / 2.
*        ENDIF.
*        IF WA_TEMP-UTIL_04 <> 0 AND LT_PLAN_MONTH_TEMP-UTIL_04 <> 0.
*          L_NEXT04 = 'X'.
*          LT_PLAN_MONTH-UTIL_04 = LT_PLAN_MONTH-UTIL_04 / 2.
*        ENDIF.
*        IF WA_TEMP-UTIL_07 <> 0 AND LT_PLAN_MONTH_TEMP-UTIL_07 <> 0.
*          L_NEXT04 = 'X'.
*          LT_PLAN_MONTH-UTIL_07 = LT_PLAN_MONTH-UTIL_07 / 2.
*        ENDIF.
*        IF WA_TEMP-UTIL_18 <> 0 AND LT_PLAN_MONTH_TEMP-UTIL_18 <> 0.
*          L_NEXT18 = 'X'.
*          LT_PLAN_MONTH-UTIL_18 = LT_PLAN_MONTH-UTIL_18 / 2.
*        ENDIF.
*        IF WA_TEMP-UTIL_19 <> 0 AND LT_PLAN_MONTH_TEMP-UTIL_19 <> 0.
*          L_NEXT19 = 'X'.
*          LT_PLAN_MONTH-UTIL_19 = LT_PLAN_MONTH-UTIL_19 / 2.
*        ENDIF.
*        IF WA_TEMP-UTIL_25 <> 0 AND LT_PLAN_MONTH_TEMP-UTIL_25 <> 0.
*          L_NEXT25 = 'X'.
*          LT_PLAN_MONTH-UTIL_25 = LT_PLAN_MONTH-UTIL_25 / 2.
*        ENDIF.
*        L_NEXT = 'X'.
*      ELSEIF L_NEXT = 'X'.
*        IF L_NEXT01 = 'X'.
*          LT_PLAN_MONTH-UTIL_01 = LT_PLAN_MONTH-UTIL_01 / 2.
*          CLEAR: L_NEXT01.
*        ENDIF.
*        IF L_NEXT04 = 'X'.
*          LT_PLAN_MONTH-UTIL_04 = LT_PLAN_MONTH-UTIL_04 / 2.
*          CLEAR: L_NEXT04.
*        ENDIF.
*        IF L_NEXT07 = 'X'.
*          LT_PLAN_MONTH-UTIL_07 = LT_PLAN_MONTH-UTIL_07 / 2.
*          CLEAR: L_NEXT07.
*        ENDIF.
*        IF L_NEXT18 = 'X'.
*          LT_PLAN_MONTH-UTIL_18 = LT_PLAN_MONTH-UTIL_18 / 2.
*          CLEAR: L_NEXT18.
*        ENDIF.
*        IF L_NEXT19 = 'X'.
*          LT_PLAN_MONTH-UTIL_19 = LT_PLAN_MONTH-UTIL_19 / 2.
*          CLEAR: L_NEXT19.
*        ENDIF.
*        IF L_NEXT25 = 'X'.
*          LT_PLAN_MONTH-UTIL_25 = LT_PLAN_MONTH-UTIL_25 / 2.
*          CLEAR: L_NEXT25.
*        ENDIF.
*        CLEAR: L_NEXT.
*      ENDIF.
      COLLECT lt_plan_month.
    ENDLOOP.
    REFRESH: lt_plan_month_temp.
  ELSE.
    SELECT * INTO TABLE lt_plan_month
    FROM ztpp_plan_month
    WHERE prdt_year = w_year
      AND model  = w_model_name.
  ENDIF.
  CLEAR: it_tab_month.
  LOOP AT it_month.
    READ TABLE lt_plan_month WITH KEY prdt_month = it_month-seq.
    IF sy-subrc = 0.
      CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-qty_plan.
        it_tab_month-total = it_tab_month-total + lt_plan_month-qty_plan
                              .
        IF sy-datum+0(4) = w_year.
          IF it_month-seq <= l_cur_month.
            it_tab_month-sub = it_tab_month-sub + lt_plan_month-qty_plan
            .
          ENDIF.
        ELSE.
          it_tab_month-sub = it_tab_month-sub + lt_plan_month-qty_plan.
        ENDIF.
      ENDIF.
** Get work hours
      CONCATENATE 'WA_BODY-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_01.
        wa_body-total = wa_body-total + lt_plan_month-wk_hr_01.
        wa_body-sub = wa_body-total.
      ENDIF.

      CONCATENATE 'WA_PAINT-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_04.
        wa_paint-total = wa_paint-total + lt_plan_month-wk_hr_04.
        wa_paint-sub = wa_paint-total.
      ENDIF.

      CONCATENATE 'WA_TRIM-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_07.
        wa_trim-total = wa_trim-total + lt_plan_month-wk_hr_07.
        wa_trim-sub = wa_trim-total.
      ENDIF.

      CONCATENATE 'WA_SIGNOFF-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_18.
        wa_signoff-total = wa_signoff-total + lt_plan_month-wk_hr_18.
        wa_signoff-sub = wa_signoff-total.
      ENDIF.

      CONCATENATE 'WA_CGATE-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_19.
        wa_cgate-total = wa_cgate-total + lt_plan_month-wk_hr_19.
        wa_cgate-sub = wa_cgate-total.
      ENDIF.

      CONCATENATE 'WA_MGATE-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_23.
        wa_mgate-total = wa_mgate-total + lt_plan_month-wk_hr_23.
        wa_mgate-sub = wa_mgate-total.
      ENDIF.

      CONCATENATE 'WA_SHIPOUT-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-wk_hr_25.
        wa_shipout-total = wa_shipout-total + lt_plan_month-wk_hr_25.
        wa_shipout-sub = wa_shipout-total.
      ENDIF.
** Get untilization
      CONCATENATE 'WA_UTIL_BODY-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_01.
        IF <fs01> > 0.
          l_cn_body = l_cn_body + 1.
          wa_util_body-total = wa_util_body-total +
                               lt_plan_month-util_01.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_PAINT-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_04.
        IF <fs01> > 0.
          l_cn_paint = l_cn_paint + 1.

          wa_util_paint-total = wa_util_paint-total +
                                lt_plan_month-util_04.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_TRIM-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_07.
        IF <fs01> > 0.
          l_cn_trim = l_cn_trim + 1.

          wa_util_trim-total = wa_util_trim-total +
                               lt_plan_month-util_07.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_18.
        wa_util_signoff-total = wa_util_signoff-total +
                                lt_plan_month-util_18.
        IF <fs01> > 0.
          l_cn_signoff = l_cn_signoff + 1.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_CGATE-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_19.
        IF <fs01> > 0.
          l_cn_cgate = l_cn_cgate + 1.

          wa_util_cgate-total = wa_util_cgate-total +
                                lt_plan_month-util_19.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_MGATE-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_23.
        IF <fs01> > 0.
          l_cn_mgate = l_cn_mgate + 1.

          wa_util_mgate-total = wa_util_mgate-total +
                                lt_plan_month-util_23.
        ENDIF.
      ENDIF.

      CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        <fs01> = lt_plan_month-util_25.
        IF <fs01> > 0.
          l_cn_shipout = l_cn_shipout + 1.
          wa_util_shipout-total = wa_util_shipout-total +
                                  lt_plan_month-util_25.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  it_tab_month-if = 'C310'.
  it_tab_month-desc = 'Plan'.
  it_tab_month-seq = '1'.
  APPEND it_tab_month.
  wa_plan = it_tab_month.

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

ENDFORM.                    " GET_PLAN_DATA_300
*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_actual_data_300.
  DATA: l_line TYPE i.
  DATA: BEGIN OF lt_prod_act OCCURS 0,
        prdt_month(2).
          INCLUDE STRUCTURE ztpp_prod_actual.
*  DATA: QTY_MGATE LIKE ZTPP_PROD_ACTUAL-QTY_VPCOUT,
  DATA: END OF lt_prod_act.

  DATA: lt_prod_act_temp LIKE TABLE OF ztpp_prod_actual
        WITH HEADER LINE.
  DATA: w_rp LIKE it_rp-rp.

  CLEAR : it_tab_month.

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
    lt_prod_act-prdt_month = lt_prod_act_temp-prdt_date+4(2).

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

*** Changed by Furong on 07*29/09
** Changed by furong on 06/01/09
*        IF LT_PROD_ACT_TEMP-PRDT_DATE <= C_CG_DATE.
*          LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_CGATE.
*        ELSE.
*          LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_VPCOUT.
*        ENDIF.
** Changed by furong on 07/28/10
        IF lt_prod_act_temp-prdt_date <= c_cg_date.
          lt_prod_act-qty_mgate = lt_prod_act_temp-qty_cgate.
        ELSE.
          lt_prod_act-qty_mgate = lt_prod_act_temp-qty_mgate.
        ENDIF.
*        LT_PROD_ACT-QTY_MGATE = LT_PROD_ACT_TEMP-QTY_CGATE.
** End of change on 07/28/10

*** end of change on 07/29/09

** end of change
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
  ENDLOOP.

  SORT lt_prod_act BY prdt_month.
  LOOP AT it_rp.
    w_rp = it_rp-rp.
    CLEAR: it_tab_month.
    CASE w_rp.
      WHEN '01'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_body.
            it_tab_month-total = it_tab_month-total +
                                 lt_prod_act-qty_body.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_BODY-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_BODY-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
            ENDIF.
** End of change

          ENDIF.
        ENDLOOP.
        it_tab_month-if = 'C210'.
        it_tab_month-desc = 'Body in'.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C210'.
        wa_body-if = 'C210'.
        wa_body-type = 'Working Hours'.
        wa_body-seq = '4'.
        wa_util_body-if = 'C210'.
        wa_util_body-type = 'Utilization'.
        wa_util_body-seq = '5'.
        IF wa_body-total > 0.
          wa_util_body-total = it_tab_month-total /
                               ( wa_body-total * w_uph ) * 100.
        ENDIF.
        IF wa_body-sub > 0.
          wa_util_body-sub = it_tab_month-sub /
                               ( wa_body-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_body TO it_tab_month.
        APPEND wa_util_body TO it_tab_month.
      WHEN '04'.
        it_tab_month-if = 'C410'.
        it_tab_month-desc = 'Paint out'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_paint.
            it_tab_month-total = it_tab_month-total +
                                 lt_prod_act-qty_paint.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_PAINT-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_PAINT-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
            ENDIF.
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C410'.
        wa_paint-type = 'Working Hours'.
        wa_paint-seq = '4'.
        wa_paint-if = 'C410'.
        wa_util_paint-type = 'Utilization'.
        wa_util_paint-seq = '5'.
        wa_util_paint-if = 'C410'.
        IF wa_paint-total > 0.
          wa_util_paint-total = it_tab_month-total /
                               ( wa_paint-total * w_uph ) * 100.
        ENDIF.
        IF wa_paint-sub > 0.
          wa_util_paint-sub = it_tab_month-sub /
                               ( wa_paint-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_paint TO it_tab_month.
        APPEND wa_util_paint TO it_tab_month.
      WHEN '07'.
        it_tab_month-desc = 'Trim in'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_trim.
            it_tab_month-total = it_tab_month-total +
                                 lt_prod_act-qty_trim.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_TRIM-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_TRIM-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
            ENDIF.
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_month-if = 'C210'.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C210'.
        wa_trim-if = 'C210'.
        wa_trim-type = 'Working Hours'.
        wa_trim-seq = '4'.
        wa_util_trim-if = 'C210'.
        wa_util_trim-type = 'Utilization'.
        wa_util_trim-seq = '5'.
        IF wa_trim-total > 0.
          wa_util_trim-total = it_tab_month-total /
                               ( wa_trim-total * w_uph ) * 100.
        ENDIF.
        IF wa_trim-sub > 0.
          wa_util_trim-sub = it_tab_month-sub /
                               ( wa_trim-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_trim TO it_tab_month.
        APPEND wa_util_trim TO it_tab_month.
      WHEN '18'.
        it_tab_month-desc = 'Sign off'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_signoff.
            it_tab_month-total = it_tab_month-total +
                               lt_prod_act-qty_signoff.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_SIGNOFF-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' it_month-seq INTO l_text
                 .
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
*              IF <FS03> > 0.
*                WA_UTIL_SIGNOFF-TOTAL = WA_UTIL_SIGNOFF-TOTAL + <FS03>.
*                IF IT_DAY-DATUM <= SY-DATUM.
*                   WA_UTIL_SIGNOFF-SUB = WA_UTIL_SIGNOFF-SUB + <FS03>.
*                ENDIF.
*              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
        it_tab_month-if = 'C410'.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C410'.
        wa_signoff-type = 'Working Hours'.
        wa_signoff-seq = '4'.
        wa_signoff-if = 'C410'.
        wa_util_signoff-type = 'Utilization'.
        wa_util_signoff-seq = '5'.
        wa_util_signoff-if = 'C410'.
        IF wa_signoff-total > 0.
          wa_util_signoff-total = it_tab_month-total /
                               ( wa_signoff-total * w_uph ) * 100.
        ENDIF.
        IF wa_signoff-sub > 0.
          wa_util_signoff-sub = it_tab_month-sub /
                               ( wa_signoff-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_signoff TO it_tab_month.
        APPEND wa_util_signoff TO it_tab_month.

      WHEN '19'.
        it_tab_month-desc = 'C/Gate'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_cgate.
            it_tab_month-total = it_tab_month-total +
                                 lt_prod_act-qty_cgate.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_CGATE-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_CGATE-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
            ENDIF.
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_month-if = 'C210'.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C210'.
        wa_cgate-if = 'C210'.
        wa_cgate-type = 'Working Hours'.
        wa_cgate-seq = '4'.
        wa_util_cgate-if = 'C210'.
        wa_util_cgate-type = 'Utilization'.
        wa_util_cgate-seq = '5'.
        IF wa_cgate-total > 0.
          wa_util_cgate-total = it_tab_month-total /
                               ( wa_cgate-total * w_uph ) * 100.
        ENDIF.
        IF wa_cgate-sub > 0.
          wa_util_cgate-sub = it_tab_month-sub /
                               ( wa_cgate-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_cgate TO it_tab_month.
        APPEND wa_util_cgate TO it_tab_month.

      WHEN '23'.
        it_tab_month-desc = 'M/Gate'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.

*            IF LT_PROD_ACT-DEST+0(3) = 'B28'.
*            <FS02> = LT_PROD_ACT-QTY_VPCOUT.
*            IT_TAB_MONTH-TOTAL = IT_TAB_MONTH-TOTAL +
*                                 LT_PROD_ACT-QTY_VPCOUT.
*            ELSE.
*            <FS02> = LT_PROD_ACT-QTY_SHIPOUT.
*            IT_TAB_MONTH-TOTAL = IT_TAB_MONTH-TOTAL +
*                                 LT_PROD_ACT-QTY_SHIPOUT.
*
*            ENDIF.
            <fs02> = lt_prod_act-qty_mgate.
            it_tab_month-total = it_tab_month-total +
                                 lt_prod_act-qty_mgate.

** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_MGATE-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_MGATE-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
            ENDIF.
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_month-if = 'C410'.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C410'.
        wa_mgate-if = 'C410'.
        wa_mgate-type = 'Working Hours'.
        wa_mgate-seq = '4'.
        wa_util_mgate-if = 'C410'.
        wa_util_mgate-type = 'Utilization'.
        wa_util_mgate-seq = '5'.
        IF wa_mgate-total > 0.
          wa_util_mgate-total = it_tab_month-total /
                               ( wa_mgate-total * w_uph ) * 100.
        ENDIF.
        IF wa_mgate-sub > 0.
          wa_util_mgate-sub = it_tab_month-sub /
                               ( wa_mgate-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_mgate TO it_tab_month.
        APPEND wa_util_mgate TO it_tab_month.

      WHEN '25'.
        it_tab_month-desc = 'Ship out'.
        LOOP AT it_month.
          READ TABLE lt_prod_act WITH KEY prdt_month = it_month-seq.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB_MONTH-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            <fs02> = lt_prod_act-qty_shipout.
            it_tab_month-total = it_tab_month-total +
                               lt_prod_act-qty_shipout.
** Changed on 12/07/07 by Furong Wang
            CONCATENATE 'WA_SHIPOUT-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' it_month-seq INTO l_text
                 .
            ASSIGN (l_text) TO <fs03>.
            IF sy-subrc = 0 AND <fs01> > 0.
              <fs03> = ( <fs02> / ( <fs01> * w_uph ) ) * 100.
            ENDIF.
** End of change
          ENDIF.
        ENDLOOP.
        it_tab_month-if = 'C210'.
        it_tab_month-seq = '2'.
        it_tab_month-type = 'Actual'.
        it_tab_month-sub = it_tab_month-total.
        wa_actual = it_tab_month.
        APPEND it_tab_month.
        PERFORM cal_gap_300 USING 'C210'.
        wa_shipout-type = 'Working Hours'.
        wa_shipout-seq = '4'.
        wa_shipout-if = 'C210'.
        wa_util_shipout-if = 'C210'.
        wa_util_shipout-type = 'Utilization'.
        wa_util_shipout-seq = '5'.
        IF wa_shipout-total > 0.
          wa_util_shipout-total = it_tab_month-total /
                               ( wa_shipout-total * w_uph ) * 100.
        ENDIF.
        IF wa_shipout-sub > 0.
          wa_util_shipout-sub = it_tab_month-sub /
                               ( wa_shipout-sub * w_uph ) * 100.
        ENDIF.
        APPEND wa_shipout TO it_tab_month.
        APPEND wa_util_shipout TO it_tab_month.
    ENDCASE.
  ENDLOOP.

ENDFORM.                    " GET_ACTUAL_DATA_300
*&---------------------------------------------------------------------*
*&      Form  CAL_GAP_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1510   text
*----------------------------------------------------------------------*
FORM cal_gap_300 USING p_color.
  LOOP AT it_month.
    CONCATENATE 'WA_PLAN-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      CONCATENATE 'WA_ACTUAL-QTYD_' it_month-seq INTO l_text.
      ASSIGN (l_text) TO <fs02>.
      IF sy-subrc = 0.
        CONCATENATE 'WA_GAP-QTYD_' it_month-seq INTO l_text.
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
  APPEND wa_gap TO it_tab_month.

ENDFORM.                    " CAL_GAP_300
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line_300.
  DATA: lt_celltab TYPE lvc_t_styl,
       w_celltab TYPE lvc_s_styl,
       l_index TYPE i,
       l_mode TYPE raw4.

  LOOP AT it_tab_month.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF it_tab_month-seq = '1'.
*    OR IT_TAB_MONTH-SEQ = '4'.
*                            OR IT_TAB_MONTH-SEQ = '5'.
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
*    W_CELLTAB-FIELDNAME = 'QTYD_13'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_14'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_15'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_16'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_17'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_18'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_19'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_20'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_21'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'QTYD_22'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_23'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_24'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_25'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_26'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_27'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_28'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_29'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_30'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*    W_CELLTAB-FIELDNAME = 'QTYD_31'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.

    INSERT LINES OF lt_celltab INTO TABLE it_tab_month-celltab.
    MODIFY it_tab_month INDEX l_index.
  ENDLOOP.
ENDFORM.                    " SELECT_EDIT_LINE_300
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data_300.
  DATA: lt_plan_month LIKE TABLE OF ztpp_plan_month WITH HEADER LINE.
  DATA: l_index LIKE sy-tabix.

  READ TABLE it_tab_month WITH KEY seq = '1'.
  lt_plan_month-hkcode = 'H'.
  lt_plan_month-plant = 'A1'.
  lt_plan_month-prdt_year = w_year.
  lt_plan_month-model = w_model_name.
  lt_plan_month-crdate = sy-datum.
  lt_plan_month-crtime = sy-uzeit.
  lt_plan_month-chdate = sy-datum.
  lt_plan_month-chtime = sy-uzeit.
  lt_plan_month-cruser = sy-uname.

  IF w_refresh IS INITIAL.
    MESSAGE i000 WITH 'Please refresh the data before saving the data'.
    EXIT.
  ENDIF.
  LOOP AT it_month.
    lt_plan_month-prdt_month = it_month-seq.

    CONCATENATE 'WA_PLAN-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-qty_plan = <fs01>.
    ENDIF.

    CONCATENATE 'WA_BODY-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_01 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_PAINT-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_04 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_TRIM-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_07 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_SIGNOFF-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_18 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_CGATE-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_19 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_MGATE-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_23 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_SHIPOUT-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-wk_hr_25 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_BODY-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_01 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_PAINT-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_04 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_TRIM-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_07 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_SIGNOFF-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_18 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_CGATE-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_19 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_MGATE-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_23 = <fs01>.
    ENDIF.

    CONCATENATE 'WA_UTIL_SHIPOUT-QTYD_' it_month-seq INTO l_text.
    ASSIGN (l_text) TO <fs01>.
    IF sy-subrc = 0.
      lt_plan_month-util_25 = <fs01>.
    ENDIF.

    APPEND lt_plan_month.
  ENDLOOP.

  MODIFY ztpp_plan_month FROM TABLE lt_plan_month.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s000(zz) WITH text-m16.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m17.
  ENDIF.

ENDFORM.                    " SAVE_DATA_300
*&---------------------------------------------------------------------*
*&      Form  REFERSH_DATA_300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refersh_data_300.
  DATA: l_index LIKE sy-tabix,
        l_rp LIKE it_tab_month-desc,
        l_count TYPE i.
  DATA: l_fr_date LIKE sy-datum,
        l_to_date LIKE sy-datum.
  DATA: wa_plan_day_sum LIKE ztpp_plan_day. " WITH HEADER LINE.

  w_refresh = 'X'.
  CLEAR: wa_plan, wa_actual, wa_gap, wa_body, wa_trim, wa_paint,
         wa_signoff, wa_cgate, wa_shipout, wa_util_body, wa_util_trim,
         wa_util_paint, wa_util_signoff, wa_util_cgate,
         wa_util_shipout,
         wa_hr, wa_util,wa_mgate,wa_util_mgate.

  LOOP AT it_tab_month.
    CASE it_tab_month-seq.
      WHEN '1'.
        l_index = sy-tabix.
        wa_plan = it_tab_month.
        CLEAR: wa_plan-total, wa_plan-sub.
        LOOP AT it_month.
          CONCATENATE 'WA_PLAN-QTYD_' it_month-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            wa_plan-total = wa_plan-total + <fs01>.
            IF sy-datum+0(4) = w_year.
              IF it_month-seq <= sy-datum+4(2).
                wa_plan-sub = wa_plan-sub + <fs01>.
              ENDIF.
            ELSE.
              wa_plan-sub = wa_plan-sub + <fs01>.

            ENDIF.
          ENDIF.
        ENDLOOP.
        MODIFY it_tab_month FROM wa_plan INDEX l_index
        TRANSPORTING total sub.
      WHEN '2'.
        CLEAR: wa_actual, wa_gap, wa_hr, wa_util.
        l_index = sy-tabix + 1.
        wa_actual = it_tab_month.
        l_rp = it_tab_month-desc.
*        CLEAR: WA_ACTUAL-TOTAL, WA_ACTUAL-SUB.
        LOOP AT it_month.
          CONCATENATE 'WA_PLAN-QTYD_' it_month-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
          IF sy-subrc = 0.
            CONCATENATE 'WA_ACTUAL-QTYD_' it_month-seq INTO l_text.
            ASSIGN (l_text) TO <fs02>.
            IF sy-subrc = 0.
              CONCATENATE 'WA_GAP-QTYD_' it_month-seq INTO l_text.
              ASSIGN (l_text) TO <fs03>.
              IF sy-subrc = 0.
                <fs03> = <fs02> - <fs01>.
                wa_gap-total = wa_gap-total + <fs03>.
                IF sy-datum+0(4) = w_year.
                  IF it_month-seq <= sy-datum+4(2).
                    wa_gap-sub = wa_gap-sub + <fs03>.
                  ENDIF.
                ELSE.
                  wa_gap-sub = wa_gap-sub + <fs03>.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
*        WA_GAP-TOTAL = WA_ACTUAL-TOTAL - WA_PLAN-TOTAL.
*        WA_GAP-SUB = WA_ACTUAL-SUB - WA_PLAN-SUB.
        MODIFY it_tab_month FROM wa_gap INDEX l_index TRANSPORTING
             total sub qtyd_01
             qtyd_02 qtyd_03 qtyd_04 qtyd_05 qtyd_06 qtyd_07 qtyd_08
             qtyd_09 qtyd_10 qtyd_11 qtyd_12.
      WHEN 4.
*        L_INDEX = SY-TABIX.
*        WA_HR = IT_TAB_MONTH.
*        CLEAR: WA_HR-TOTAL, WA_HR-SUB.
*        LOOP AT IT_MONTH.
*          CONCATENATE 'WA_HR-QTYD_' IT_MONTH-SEQ INTO L_TEXT.
*          ASSIGN (L_TEXT) TO <FS01>.
*          IF SY-SUBRC = 0.
*            IF <FS01> > 0.
*              WA_HR-TOTAL = WA_HR-TOTAL + <FS01>.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*        WA_HR-SUB = WA_HR-TOTAL.
*        CLEAR: L_COUNT.
*        MODIFY IT_TAB_MONTH FROM WA_HR INDEX L_INDEX
*        TRANSPORTING TOTAL SUB.
*        CASE L_RP.
*          WHEN 'Body in'.
*            WA_BODY = WA_HR.
*          WHEN 'Paint out'.
*            WA_PAINT = WA_HR.
*          WHEN 'Trim in'.
*            WA_TRIM = WA_HR.
*          WHEN 'Sign off'.
*            WA_SIGNOFF = WA_HR.
*          WHEN 'C/Gate'.
*            WA_CGATE = WA_HR.
*          WHEN 'Ship out'.
*            WA_SHIPOUT = WA_HR.
*        ENDCASE.
** Changed by Furong on 01/08/08
        l_index = sy-tabix.
        CLEAR: it_tab_month-total, it_tab_month-sub.
        CASE l_rp.
          WHEN 'Body in'.
            wa_body = it_tab_month.
          WHEN 'Paint out'.
            wa_paint = it_tab_month.
          WHEN 'Trim in'.
            wa_trim = it_tab_month.
          WHEN 'Sign off'.
            wa_signoff = it_tab_month.
          WHEN 'C/Gate'.
            wa_cgate = it_tab_month.
          WHEN 'M/Gate'.
            wa_mgate = it_tab_month.
          WHEN 'Ship out'.
            wa_shipout = it_tab_month.
        ENDCASE.

        CLEAR: wa_hr-total, wa_hr-sub.
        LOOP AT it_month.
          CONCATENATE w_year it_month-seq '01' INTO l_fr_date.
          CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in                  = l_fr_date
           IMPORTING
             last_day_of_month       = l_to_date
*            EXCEPTIONS
*              DAY_IN_NO_DATE          = 1
*              OTHERS                  = 2
                    .
          IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
          ENDIF.
          IF w_model_name = space.
*            SELECT SUM( WK_HR_01 ) SUM( WK_HR_04 ) SUM( WK_HR_07 )
*                  SUM( WK_HR_18 ) SUM( WK_HR_19 ) SUM( WK_HR_25 )
*             INTO (WA_PLAN_DAY_SUM-WK_HR_01, WA_PLAN_DAY_SUM-WK_HR_04,
*                  WA_PLAN_DAY_SUM-WK_HR_07, WA_PLAN_DAY_SUM-WK_HR_18,
*                  WA_PLAN_DAY_SUM-WK_HR_19, WA_PLAN_DAY_SUM-WK_HR_25)
*            FROM ZTPP_PLAN_DAY
*            WHERE PRDT_DATE BETWEEN L_FR_DATE AND L_TO_DATE
*            GROUP BY HKCODE.

*              IF SY-SUBRC = 0.
            CASE l_rp.
              WHEN 'Body in'.
                SELECT SUM( wk_hr_01 )
                INTO wa_plan_day_sum-wk_hr_01
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   GROUP BY hkcode.

                  CONCATENATE 'WA_BODY-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_01.
                    wa_body-total = wa_body-total +
                                   wa_plan_day_sum-wk_hr_01.
                    wa_body-sub = wa_body-total.
                  ENDIF.
                ENDSELECT.
              WHEN 'Paint out'.
                SELECT SUM( wk_hr_04 )
                INTO wa_plan_day_sum-wk_hr_04
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   GROUP BY hkcode.

                  CONCATENATE 'WA_PAINT-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_04.
                    wa_paint-total = wa_paint-total
                             + wa_plan_day_sum-wk_hr_04.
                    wa_paint-sub = wa_paint-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'Trim in'.
                SELECT SUM( wk_hr_07 )
                INTO wa_plan_day_sum-wk_hr_07
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   GROUP BY hkcode.

                  CONCATENATE 'WA_TRIM-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_07.
                    wa_trim-total = wa_trim-total
                     + wa_plan_day_sum-wk_hr_07.
                    wa_trim-sub = wa_trim-total.
                  ENDIF.
                ENDSELECT.
              WHEN 'Sign off'.
                SELECT SUM( wk_hr_18 )
                INTO wa_plan_day_sum-wk_hr_18
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   GROUP BY hkcode.

                  CONCATENATE 'WA_SIGNOFF-QTYD_' it_month-seq INTO
                  l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_18.
                    wa_signoff-total = wa_signoff-total
                     + wa_plan_day_sum-wk_hr_18.
                    wa_signoff-sub = wa_signoff-total.
                  ENDIF.
                ENDSELECT.
              WHEN 'C/Gate'.
                SELECT SUM( wk_hr_19 )
                INTO wa_plan_day_sum-wk_hr_19
                FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   GROUP BY hkcode.

                  CONCATENATE 'WA_CGATE-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_19.
                    wa_cgate-total = wa_cgate-total +
                    wa_plan_day_sum-wk_hr_19.
                    wa_cgate-sub = wa_cgate-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'M/Gate'.
                SELECT SUM( wk_hr_23 )
                INTO wa_plan_day_sum-wk_hr_23
                FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   GROUP BY hkcode.

                  CONCATENATE 'WA_MGATE-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_23.
                    wa_mgate-total = wa_mgate-total +
                    wa_plan_day_sum-wk_hr_23.
                    wa_mgate-sub = wa_mgate-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'Ship out'.
                SELECT SUM( wk_hr_25 )
               INTO wa_plan_day_sum-wk_hr_25
                 FROM ztpp_plan_day
                 WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                  GROUP BY hkcode.

                  CONCATENATE 'WA_SHIPOUT-QTYD_' it_month-seq INTO
                  l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_25.
                    wa_shipout-total = wa_shipout-total +
                    wa_plan_day_sum-wk_hr_25.
                    wa_shipout-sub = wa_shipout-total.
                  ENDIF.
                ENDSELECT.
            ENDCASE.
          ELSE.
            CASE l_rp.
              WHEN 'Body in'.
                SELECT SUM( wk_hr_01 )
                INTO wa_plan_day_sum-wk_hr_01
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   AND model  = w_model_name
                   GROUP BY hkcode.

                  CONCATENATE 'WA_BODY-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_01.
                    wa_body-total = wa_body-total +
                                   wa_plan_day_sum-wk_hr_01.
                    wa_body-sub = wa_body-total.
                  ENDIF.
                ENDSELECT.
              WHEN 'Paint out'.
                SELECT SUM( wk_hr_04 )
                INTO wa_plan_day_sum-wk_hr_04
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   AND model  = w_model_name
                   GROUP BY hkcode.

                  CONCATENATE 'WA_PAINT-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_04.
                    wa_paint-total = wa_paint-total +
                    wa_plan_day_sum-wk_hr_04.
                    wa_paint-sub = wa_paint-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'Trim in'.
                SELECT SUM( wk_hr_07 )
                INTO wa_plan_day_sum-wk_hr_07
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   AND model  = w_model_name
                   GROUP BY hkcode.

                  CONCATENATE 'WA_TRIM-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_07.
                    wa_trim-total = wa_trim-total +
                    wa_plan_day_sum-wk_hr_07.
                    wa_trim-sub = wa_trim-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'Sign off'.
                SELECT SUM( wk_hr_18 )
                INTO wa_plan_day_sum-wk_hr_18
                  FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   AND model  = w_model_name
                   GROUP BY hkcode.

                  CONCATENATE 'WA_SIGNOFF-QTYD_' it_month-seq INTO
                  l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_18.
                    wa_signoff-total = wa_signoff-total +
                    wa_plan_day_sum-wk_hr_18.
                    wa_signoff-sub = wa_signoff-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'C/Gate'.
                SELECT SUM( wk_hr_19 )
                INTO wa_plan_day_sum-wk_hr_19
                FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   AND model  = w_model_name
                   GROUP BY hkcode.

                  CONCATENATE 'WA_CGATE-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_19.
                    wa_cgate-total = wa_cgate-total +
                    wa_plan_day_sum-wk_hr_19.
                    wa_cgate-sub = wa_cgate-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'M/Gate'.
                SELECT SUM( wk_hr_23 )
                INTO wa_plan_day_sum-wk_hr_23
                FROM ztpp_plan_day
                  WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                   AND model  = w_model_name
                   GROUP BY hkcode.

                  CONCATENATE 'WA_MGATE-QTYD_' it_month-seq INTO l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_23.
                    wa_mgate-total = wa_mgate-total +
                    wa_plan_day_sum-wk_hr_23.
                    wa_mgate-sub = wa_mgate-total.
                  ENDIF.
                ENDSELECT.

              WHEN 'Ship out'.
                SELECT SUM( wk_hr_25 )
               INTO wa_plan_day_sum-wk_hr_25
                 FROM ztpp_plan_day
                 WHERE prdt_date BETWEEN l_fr_date AND l_to_date
                  AND model  = w_model_name
                  GROUP BY hkcode.

                  CONCATENATE 'WA_SHIPOUT-QTYD_' it_month-seq INTO
                  l_text.
                  ASSIGN (l_text) TO <fs01>.
                  IF sy-subrc = 0.
                    <fs01> = wa_plan_day_sum-wk_hr_25.
                    wa_shipout-total = wa_shipout-total +
                    wa_plan_day_sum-wk_hr_25.
                    wa_shipout-sub = wa_shipout-total.
                  ENDIF.
                ENDSELECT.
            ENDCASE.
          ENDIF.
          CLEAR: wa_plan_day_sum.
        ENDLOOP.

        CASE l_rp.
          WHEN 'Body in'.
            MODIFY it_tab_month FROM wa_body INDEX l_index.
            wa_hr = wa_body.
          WHEN 'Paint out'.
            MODIFY it_tab_month FROM wa_paint INDEX l_index.
            wa_hr = wa_paint.
          WHEN 'Trim in'.
            MODIFY it_tab_month FROM wa_trim INDEX l_index.
            wa_hr = wa_trim.
          WHEN 'Sign off'.
            MODIFY it_tab_month FROM wa_signoff INDEX l_index.
            wa_hr = wa_signoff.
          WHEN 'C/Gate'.
            MODIFY it_tab_month FROM wa_cgate INDEX l_index.
            wa_hr = wa_cgate.
          WHEN 'M/Gate'.
            MODIFY it_tab_month FROM wa_mgate INDEX l_index.
            wa_hr = wa_mgate.
          WHEN 'Ship out'.
            MODIFY it_tab_month FROM wa_shipout INDEX l_index.
            wa_hr = wa_shipout.
        ENDCASE.

** End of change
      WHEN 5.
        l_index = sy-tabix.
        wa_util = it_tab_month.
        CLEAR: wa_util-total, wa_util-sub.
        LOOP AT it_month.
          CONCATENATE 'WA_UTIL-QTYD_' it_month-seq INTO l_text.
          ASSIGN (l_text) TO <fs01>.
** Changed by Furong on 12/07/07
          CONCATENATE 'WA_ACTUAL-QTYD_' it_month-seq INTO l_text.
          ASSIGN (l_text) TO <fs02>.
          CONCATENATE 'WA_HR-QTYD_' it_month-seq INTO l_text.
          ASSIGN (l_text) TO <fs03>.
          IF sy-subrc = 0 AND <fs03> > 0.
            <fs01> = ( <fs02> / ( <fs03> * w_uph ) ) * 100.
          ELSE.
            <fs01> = 0.
          ENDIF.
** end of change

*          IF SY-SUBRC = 0.
*            IF <FS01> > 0.
*              WA_UTIL-TOTAL = WA_UTIL-TOTAL + <FS01>.
*              L_COUNT = L_COUNT + 1.
*            ENDIF.
*          ENDIF.
        ENDLOOP.
** Changed by Furong on 12/07/07
        IF wa_hr-total > 0.
          wa_util-total = wa_actual-total / ( wa_hr-total * w_uph ) *
          100.
        ENDIF.
        IF wa_hr-sub > 0.
          wa_util-sub = wa_actual-sub / ( wa_hr-sub * w_uph ) * 100.
        ENDIF.
** End of change
*        WA_UTIL-TOTAL = WA_UTIL-TOTAL / L_COUNT.
*        WA_UTIL-SUB = WA_UTIL-TOTAL.
        CLEAR: l_count.
        MODIFY it_tab_month FROM wa_util INDEX l_index.
        CASE l_rp.
          WHEN 'Body in'.
            wa_util_body = wa_util.
          WHEN 'Paint out'.
            wa_util_paint = wa_util.
          WHEN 'Trim in'.
            wa_util_trim = wa_util.
          WHEN 'Sign off'.
            wa_util_signoff = wa_util.
          WHEN 'C/Gate'.
            wa_util_cgate = wa_util.
          WHEN 'M/Gate'.
            wa_util_mgate = wa_util.
          WHEN 'Ship out'.
            wa_util_shipout = wa_util.
        ENDCASE.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " REFERSH_DATA_300
