*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CALL METHOD alv_grid->check_changed_data.
  CALL METHOD cl_gui_cfw=>flush.

  CASE ok_code.
    WHEN 'INQ'.
      PERFORM select_data.
    WHEN 'EXIT'.
      CLEAR: w_new, w_refresh.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: w_new, w_refresh.
      LEAVE TO SCREEN 0.
    WHEN 'CAL'.
      IF sy-tcode = 'ZAPP_ASMP_PIR'.
        PERFORM recal_stock.
      ELSE.
        MESSAGE i001 WITH 'No Authorization to Access'.
      ENDIF.
    WHEN 'REF'.
      w_refresh = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      IF sy-tcode = 'ZAPP_ASMP_PIR'.
        PERFORM replace_pir_qty.
        PERFORM recal_stock.  "07.23.2014
        MESSAGE s000(zz) WITH 'Data successfully replaced'.
      ELSE.
        MESSAGE i001 WITH 'No Authorization to Access'.
      ENDIF.

*      PERFORM RECAL_STOCK.
    WHEN 'SAVE'.
      IF sy-tcode = 'ZAPP_ASMP_PIR'. " AND W_PRDT <> '2'.
        PERFORM save_data.
        PERFORM recal_stock.  "07.23.2014
        MESSAGE s000(zz) WITH text-m16.
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
  DATA: lt_asmp_pir LIKE TABLE OF ztpp_asmp_pir WITH HEADER LINE.
  DATA: l_cn(2) TYPE n,
        l_qty1(13),
        l_index LIKE sy-tabix,
        l_text(35).
  DATA: ls_output LIKE it_output.

  REFRESH lt_asmp_pir.
  LOOP AT it_output.
    l_index = sy-tabix - 2.
    IF it_output-seq = '3'.
      lt_asmp_pir-matnr = it_output-matnr.
      lt_asmp_pir-werks = w_werks.
      lt_asmp_pir-wdatu = z_beg_date.
      lt_asmp_pir-laeda = sy-datum.
      lt_asmp_pir-aenam = sy-uname.

      READ TABLE it_material WITH KEY matnr = it_output-matnr.
      IF sy-subrc = 0.
        lt_asmp_pir-bpart =  it_material-idnrk.
      ENDIF.

      READ TABLE it_output INTO ls_output INDEX l_index.
      LOOP AT it_day.
        CONCATENATE 'LS_OUTPUT-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          lt_asmp_pir-wmeng = <fs01>.
        ENDIF.
        CONCATENATE 'IT_OUTPUT-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          lt_asmp_pir-plnmg = <fs01>.
        ENDIF.
        lt_asmp_pir-entlu = '1'.
        lt_asmp_pir-pdatu = it_day-datum.
        APPEND lt_asmp_pir.
      ENDLOOP.

      LOOP AT it_week.
        CONCATENATE 'LS_OUTPUT-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          lt_asmp_pir-wmeng = <fs01>.
        ENDIF.
        CONCATENATE 'IT_OUTPUT-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          lt_asmp_pir-plnmg = <fs01>.
        ENDIF.
        lt_asmp_pir-entlu = '2'.
        lt_asmp_pir-pdatu = it_week-datum.
        APPEND lt_asmp_pir.
      ENDLOOP.
      CLEAR: lt_asmp_pir.
    ENDIF.
  ENDLOOP.
  MODIFY ztpp_asmp_pir FROM TABLE lt_asmp_pir.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s000(zz) WITH text-m16.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000(zz) WITH text-m17.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  REPLACE_PIR_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM replace_pir_qty.
  DATA: lt_cell TYPE lvc_t_cell.
  DATA: lw_cell TYPE lvc_s_cell.
  DATA: wa_row TYPE lvc_s_row,
        wa_col TYPE lvc_s_col,
        wa_stbl TYPE lvc_s_stbl.
  DATA: l_line TYPE i,
        l_text(35),
        l_cn(2) TYPE n,
        l_dw(2),
        l_index LIKE sy-tabix,
        l_qty LIKE it_output-qtyd_01.

  CALL METHOD alv_grid->get_selected_cells
    IMPORTING
      et_cell = lt_cell.
  IF lt_cell[] IS INITIAL.
  ELSE.
    READ TABLE lt_cell INTO lw_cell INDEX 1.
    wa_row = lw_cell-row_id.
    wa_col = lw_cell-col_id.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  l_cn = wa_col-fieldname+5(2).
  l_dw = wa_col-fieldname+3(2).
  IF l_cn < '00' OR l_cn > '21'.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  l_index = wa_row-index - 2.
  READ TABLE it_output INDEX l_index.
  IF sy-subrc NE 0 OR it_output-seq <> '1'.
    MESSAGE e000(zz) WITH text-m13.
  ENDIF.
  CONCATENATE 'IT_OUTPUT-QTY' l_dw l_cn INTO l_text.
  ASSIGN (l_text) TO <fs01>.
  IF sy-subrc = 0.
    l_qty = <fs01>.
    READ TABLE it_output INDEX wa_row-index.
    CONCATENATE 'IT_OUTPUT-QTY' l_dw l_cn INTO l_text.
    ASSIGN (l_text) TO <fs-qty>.
    IF sy-subrc = 0.
      <fs-qty> = l_qty.
      MODIFY it_output INDEX wa_row-index.
      wa_stbl-row = 'X'.
      wa_stbl-col = 'X'.
      CALL METHOD alv_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stbl.

      MESSAGE s000(zz) WITH 'Data successfully replaced'.
    ENDIF.
  ELSE.
    MESSAGE s000(zz) WITH 'Replacement Error'.
  ENDIF.
ENDFORM.                    " REPLACE_PIR_QTY
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_itab.
  DATA: l_qty TYPE i,
       l_cn(2) TYPE n,
       l_qty1 LIKE it_tab-qtyd_01,
       l_text(35),
       l_index LIKE sy-tabix.

  LOOP AT it_output.
    IF it_output-seq = '3'.
      l_index = sy-tabix.
      READ TABLE it_tab INDEX l_index.
      CLEAR : it_tab-mtd.

      l_cn = '00'.
      DO 21 TIMES.
        l_cn =  l_cn + 1.
        CONCATENATE 'IT_OUTPUT-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          l_qty = <fs01>.
          l_qty1 = l_qty.

        ENDIF.
        CONCATENATE 'IT_TAB-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = l_qty1.
          it_tab-mtd = it_tab-mtd + l_qty1.
        ENDIF.
      ENDDO.
      l_cn = '03'.
      DO 18 TIMES.
        l_cn =  l_cn + 1.
        CONCATENATE 'IT_OUTPUT-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          l_qty = <fs01>.
          l_qty1 = l_qty.
        ENDIF.
        CONCATENATE 'IT_TAB-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = l_qty1.
          it_tab-mtd = it_tab-mtd + l_qty1.
        ENDIF.
      ENDDO.
      MODIFY it_tab INDEX l_index..
    ENDIF.
  ENDLOOP.
ENDFORM.                    " UPDATE_ITAB


*&---------------------------------------------------------------------*
*&      Form  RECAL_STOCK
*&---------------------------------------------------------------------*
FORM recal_stock.
  PERFORM update_itab.
  PERFORM update_stock.
ENDFORM.                    " RECAL_STOCK

*&---------------------------------------------------------------------*
*&      Form  UPDATE_STOCK
*&---------------------------------------------------------------------*
FORM update_stock.
  DATA: wa_tab1 LIKE it_tab,
        wa_tab2 LIKE it_tab,
        wa_tab3 LIKE it_tab,
        l_index LIKE sy-tabix,
        l_curr LIKE sy-tabix,
        l_cn(2) TYPE n,
        l_text(30).

  FIELD-SYMBOLS: <l_order>, <l_stock>,<l_plan>.

  SORT it_tab BY matnr seq.
  LOOP AT it_tab INTO wa_tab2.

    IF wa_tab2-seq = '2'.

      CLEAR : lt_color[], lt_color, it_tab-cellcolor[].

      l_curr = sy-tabix.
      l_index = sy-tabix - 1.
      READ TABLE it_tab INTO wa_tab1 INDEX l_index.
      l_index  = l_index + 2.
      READ TABLE it_tab INTO wa_tab3 INDEX l_index.

      LOOP AT it_day FROM 2 .
        l_cn = it_day-seq - 1.
        CONCATENATE 'WA_TAB1-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_order>.
        IF sy-subrc = 0.
          CONCATENATE 'WA_TAB2-QTYD_'  l_cn INTO l_text.
          ASSIGN (l_text) TO <l_stock>.
          IF sy-subrc = 0.
            CONCATENATE 'WA_TAB3-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <l_plan>.
            IF sy-subrc = 0.
              CONCATENATE 'WA_TAB2-QTYD_' it_day-seq INTO l_text.
              ASSIGN (l_text) TO <fs-qty>.
              IF sy-subrc = 0.
                IF sy-tabix = 2.
                  <fs-qty> = <l_stock> + <l_plan> - <l_order>
                                                  - wa_tab1-past_due.
                ELSE.
                  <fs-qty> = <l_stock> + <l_plan> - <l_order>.
                ENDIF.

                PERFORM fill_cell_color USING   l_text+8(7) <fs-qty>
                                        CHANGING lt_color.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CONCATENATE 'WA_TAB1-QTYD_' w_max_day_cn INTO l_text.
      ASSIGN (l_text) TO <l_order>.
      IF sy-subrc = 0.
        CONCATENATE 'WA_TAB2-QTYD_'  w_max_day_cn INTO l_text.
        ASSIGN (l_text) TO <l_stock>.
        CONCATENATE 'WA_TAB3-QTYD_' w_max_day_cn INTO l_text.
        ASSIGN (l_text) TO <l_plan>.
        IF sy-subrc = 0.
          wa_tab2-qtyw_04 = <l_stock> + <l_plan> - <l_order>.

          PERFORM fill_cell_color USING   'QTYW_04'  wa_tab2-qtyw_04
                                  CHANGING lt_color.
        ENDIF.
      ENDIF.
      LOOP AT it_week FROM 2.
        l_cn = it_week-seq - 1.
        CONCATENATE 'WA_TAB1-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_order>.
        IF sy-subrc = 0.
          CONCATENATE 'WA_TAB2-QTYW_'  l_cn INTO l_text.
          ASSIGN (l_text) TO <l_stock>.
          IF sy-subrc = 0.
            CONCATENATE 'WA_TAB3-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <l_plan>.
            IF sy-subrc = 0.
              CONCATENATE 'WA_TAB2-QTYW_' it_week-seq INTO l_text.
              ASSIGN (l_text) TO <fs-qty>.
              IF sy-subrc = 0.
                <fs-qty> = <l_stock> + <l_plan> - <l_order>.

                PERFORM fill_cell_color USING   l_text+8(7) <fs-qty>
                                        CHANGING lt_color.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
*      IT_TAB-IF = 'C210'.
*    ENDIF.
      wa_tab2-cellcolor = lt_color.
      MODIFY it_tab FROM wa_tab2 INDEX l_curr.

    ENDIF.
  ENDLOOP.
  SORT it_tab BY matnr seq.
  PERFORM prepare_display.
  wa_stbl-row = 'X'.
  wa_stbl-col = 'X'.
  CALL METHOD alv_grid->refresh_table_display
    EXPORTING
      is_stable = wa_stbl.

  MESSAGE s000(zz) WITH 'Data recalculated'.
ENDFORM.                    " UPDATE_STOCK
