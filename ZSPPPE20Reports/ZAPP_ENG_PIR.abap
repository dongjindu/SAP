************************************************************************
* Program Name      : ZAPP_ENG_PIR
* Creation Date     : 07/27/2007
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zapp_eng_prdt_plan NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.
TABLES: ztpp_eng_pir.
TYPE-POOLS: slis, vrm.
DATA: BEGIN OF it_tab_temp OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      alc_vals LIKE ztpp_seq_sum02-alc_vals,
      END OF it_tab_temp.

DATA: BEGIN OF it_tab OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      matnr_s LIKE mara-matnr,
      labst LIKE mard-labst,
      seq(1),
      desc(15),
      mtd LIKE mdsm-bdmng,
      d-1 LIKE mdsm-bdmng,
      qtyd_01 LIKE mdsm-bdmng,
      qtyd_02 LIKE mdsm-bdmng,
      qtyd_03 LIKE mdsm-bdmng,
      qtyd_04 LIKE mdsm-bdmng,
      qtyd_05 LIKE mdsm-bdmng,
      qtyd_06 LIKE mdsm-bdmng,
      qtyd_07 LIKE mdsm-bdmng,
      qtyd_08 LIKE mdsm-bdmng,
      qtyd_09 LIKE mdsm-bdmng,
      qtyd_10 LIKE mdsm-bdmng,
      qtyd_11 LIKE mdsm-bdmng,
      qtyd_12 LIKE mdsm-bdmng,
      qtyd_13 LIKE mdsm-bdmng,
      qtyd_14 LIKE mdsm-bdmng,
      qtyd_15 LIKE mdsm-bdmng,
      qtyd_16 LIKE mdsm-bdmng,
      qtyd_17 LIKE mdsm-bdmng,
      qtyd_18 LIKE mdsm-bdmng,
      qtyd_19 LIKE mdsm-bdmng,
      qtyd_20 LIKE mdsm-bdmng,
      qtyd_21 LIKE mdsm-bdmng,
      qtyw_04 LIKE mdsm-bdmng,
      qtyw_05 LIKE mdsm-bdmng,
      qtyw_06 LIKE mdsm-bdmng,
      qtyw_07 LIKE mdsm-bdmng,
      qtyw_08 LIKE mdsm-bdmng,
      qtyw_09 LIKE mdsm-bdmng,
      qtyw_10 LIKE mdsm-bdmng,
      qtyw_11 LIKE mdsm-bdmng,
      qtyw_12 LIKE mdsm-bdmng,
      qtyw_13 LIKE mdsm-bdmng,
      qtyw_14 LIKE mdsm-bdmng,
      qtyw_15 LIKE mdsm-bdmng,
      qtyw_16 LIKE mdsm-bdmng,
      qtyw_17 LIKE mdsm-bdmng,
      qtyw_18 LIKE mdsm-bdmng,
      qtyw_19 LIKE mdsm-bdmng,
      qtyw_20 LIKE mdsm-bdmng,
      qtyw_21 LIKE mdsm-bdmng,
      meins LIKE mara-meins,
      total LIKE mdsm-bdmng,
      maktx LIKE makt-maktx,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
     END OF it_tab.

DATA: BEGIN OF it_output OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      matnr_s LIKE mara-matnr,
      labst LIKE mard-labst,
      seq(1),
      desc(15),
      mtd(13),
      d-1(13),
      qtyd_01(13),
      qtyd_02(13),
      qtyd_03(13),
      qtyd_04(13),
      qtyd_05(13),
      qtyd_06(13),
      qtyd_07(13),
      qtyd_08(13),
      qtyd_09(13),
      qtyd_10(13),
      qtyd_11(13),
      qtyd_12(13),
      qtyd_13(13),
      qtyd_14(13),
      qtyd_15(13),
      qtyd_16(13),
      qtyd_17(13),
      qtyd_18(13),
      qtyd_19(13),
      qtyd_20(13),
      qtyd_21(13),
      qtyw_04(13),
      qtyw_05(13),
      qtyw_06(13),
      qtyw_07(13),
      qtyw_08(13),
      qtyw_09(13),
      qtyw_10(13),
      qtyw_11(13),
      qtyw_12(13),
      qtyw_13(13),
      qtyw_14(13),
      qtyw_15(13),
      qtyw_16(13),
      qtyw_17(13),
      qtyw_18(13),
      qtyw_19(13),
      qtyw_20(13),
      qtyw_21(13),
*      MEINS LIKE MARA-MEINS,
      total LIKE mdsm-bdmng,
      maktx LIKE makt-maktx,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
     END OF it_output.

DATA: wa_tot1 LIKE it_tab,
      wa_tot2 LIKE it_tab,
      wa_tot3 LIKE it_tab.

DATA: BEGIN OF it_total OCCURS 0,
      werks LIKE marc-werks,
      matnr LIKE mara-matnr,
      matnr_s LIKE mara-matnr,
      labst LIKE mard-labst,
      seq(1),
      desc(15),
      mtd(13),
      d-1(13),
      qtyd_01(13),
      qtyd_02(13),
      qtyd_03(13),
      qtyd_04(13),
      qtyd_05(13),
      qtyd_06(13),
      qtyd_07(13),
      qtyd_08(13),
      qtyd_09(13),
      qtyd_10(13),
      qtyd_11(13),
      qtyd_12(13),
      qtyd_13(13),
      qtyd_14(13),
      qtyd_15(13),
      qtyd_16(13),
      qtyd_17(13),
      qtyd_18(13),
      qtyd_19(13),
      qtyd_20(13),
      qtyd_21(13),
      qtyw_04(13),
      qtyw_05(13),
      qtyw_06(13),
      qtyw_07(13),
      qtyw_08(13),
      qtyw_09(13),
      qtyw_10(13),
      qtyw_11(13),
      qtyw_12(13),
      qtyw_13(13),
      qtyw_14(13),
      qtyw_15(13),
      qtyw_16(13),
      qtyw_17(13),
      qtyw_18(13),
      qtyw_19(13),
      qtyw_20(13),
      qtyw_21(13),
*      MTD LIKE MDSM-BDMNG,
*      D-1 LIKE MDSM-BDMNG,
*      QTYD_01 LIKE MDSM-BDMNG,
*      QTYD_02 LIKE MDSM-BDMNG,
*      QTYD_03 LIKE MDSM-BDMNG,
*      QTYD_04 LIKE MDSM-BDMNG,
*      QTYD_05 LIKE MDSM-BDMNG,
*      QTYD_06 LIKE MDSM-BDMNG,
*      QTYD_07 LIKE MDSM-BDMNG,
*      QTYD_08 LIKE MDSM-BDMNG,
*      QTYD_09 LIKE MDSM-BDMNG,
*      QTYD_10 LIKE MDSM-BDMNG,
*      QTYD_11 LIKE MDSM-BDMNG,
*      QTYD_12 LIKE MDSM-BDMNG,
*      QTYD_13 LIKE MDSM-BDMNG,
*      QTYD_14 LIKE MDSM-BDMNG,
*      QTYD_15 LIKE MDSM-BDMNG,
*      QTYD_16 LIKE MDSM-BDMNG,
*      QTYD_17 LIKE MDSM-BDMNG,
*      QTYD_18 LIKE MDSM-BDMNG,
*      QTYD_19 LIKE MDSM-BDMNG,
*      QTYD_20 LIKE MDSM-BDMNG,
*      QTYD_21 LIKE MDSM-BDMNG,
*      QTYW_04 LIKE MDSM-BDMNG,
*      QTYW_05 LIKE MDSM-BDMNG,
*      QTYW_06 LIKE MDSM-BDMNG,
*      QTYW_07 LIKE MDSM-BDMNG,
*      QTYW_08 LIKE MDSM-BDMNG,
*      QTYW_09 LIKE MDSM-BDMNG,
*      QTYW_10 LIKE MDSM-BDMNG,
*      QTYW_11 LIKE MDSM-BDMNG,
*      QTYW_12 LIKE MDSM-BDMNG,
*      QTYW_13 LIKE MDSM-BDMNG,
*      QTYW_14 LIKE MDSM-BDMNG,
*      QTYW_15 LIKE MDSM-BDMNG,
*      QTYW_16 LIKE MDSM-BDMNG,
*      QTYW_17 LIKE MDSM-BDMNG,
*      QTYW_18 LIKE MDSM-BDMNG,
*      QTYW_19 LIKE MDSM-BDMNG,
*      QTYW_20 LIKE MDSM-BDMNG,
*      QTYW_21 LIKE MDSM-BDMNG,
*      MEINS LIKE MARA-MEINS,
      total LIKE mdsm-bdmng,
      maktx LIKE makt-maktx,
      if(4) TYPE c,
      celltab TYPE lvc_t_styl,
     END OF it_total.

DATA : BEGIN OF it_stock OCCURS 0,
       matnr LIKE mard-matnr,
       sum01 LIKE mdkp-sum01,
       END OF it_stock.

DATA: BEGIN OF it_day OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day.

DATA: BEGIN OF it_day_conv OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_day_conv.

DATA: BEGIN OF it_week OCCURS 21,
        seq(2)  TYPE n,
        datum   LIKE   sy-datum,
      END   OF it_week.

DATA : z_max_date LIKE sy-datum,
       z_beg_date LIKE sy-datum,
       z_max_week LIKE sy-datum,
       z_beg_week LIKE sy-datum,
       w_max_day_cn(2) TYPE n,
       w_datum LIKE sy-datum,
       w_prgrp LIKE pgmi-prgrp,
       w_d-1 LIKE sy-datum,
       w_mtd LIKE sy-datum.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i,
      w_no_data(1),
      w_prdt(1),
      w_flag(1).

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

FIELD-SYMBOLS : <fs01>, <fs-qty>.

**--- Constants

** for E002
*CONSTANTS : C_WERKS LIKE MARC-WERKS VALUE 'E001'.

DATA:  w_refresh(1),
       w_new(1) VALUE 'X'.

DATA : w_werks TYPE werks_d.  "MARC-WERKS.
*DATA  SAVE_DATA.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: g_event_receiver TYPE REF TO lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
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
             'QTYD_21'.
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
        WHEN 'QTYW_04' OR 'QTYW_05' OR 'QTYW_06' OR 'QTYW_07' OR
             'QTYW_08' OR 'QTYW_09' OR 'QTYW_10' OR 'QTYW_11' OR
             'QTYW_12' OR 'QTYW_13' OR 'QTYW_14' OR 'QTYW_15' OR
             'QTYW_16' OR 'QTYW_17' OR 'QTYW_18' OR 'QTYW_19' OR
             'QTYW_20' OR 'QTYW_21'.

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
  PERFORM get_req_data.
  IF w_prdt = '2'.
    PERFORM get_indel_data.
  ELSE.
    PERFORM get_pir_data.
  ENDIF.
ENDFORM.                    "GET_DATA
*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_req_data.
* make requirement qty -first line of 3 lines of it-tab.
* with stock qty and mtd and D-1 qty (from MSEG)
  DATA: it_seq_sum03 LIKE TABLE OF ztpp_seq_sum03 WITH HEADER LINE,
        it_seq_sum02 LIKE TABLE OF ztpp_seq_sum02 WITH HEADER LINE,
        lt_eng_stock LIKE TABLE OF ztpp_eng_stock WITH HEADER LINE,
        lt_stock LIKE TABLE OF it_stock WITH HEADER LINE.

  DATA: BEGIN OF it_resb OCCURS 0,
        matnr LIKE resb-matnr,
*        WERKS LIKE RESB-WERKS,
        bdter LIKE resb-bdter,
        bdmng LIKE resb-bdmng,
        END OF it_resb.

  DATA: BEGIN OF it_mdsm OCCURS 0,
        matnr LIKE mdsm-matnr,
*        WERKS LIKE RESB-WERKS,
        bdter LIKE mdsm-bdter,
        bdmng LIKE mdsm-bdmng,
        END OF it_mdsm.

  DATA: lt_resb LIKE TABLE OF it_resb WITH HEADER LINE,
        lt_mdsm LIKE TABLE OF it_mdsm WITH HEADER LINE.

  DATA: lt_pgmi LIKE TABLE OF pgmi WITH HEADER LINE.
  DATA: l_line TYPE i,
        l_cn(2) TYPE n,
        l_text(30).

  CLEAR : it_tab, it_tab[].
  CLEAR: it_tab_temp,it_tab_temp[], it_stock, it_stock[].
  IF w_prdt = '1'.
    w_prgrp = 'MIP-ENG'.
  ELSEIF w_prdt = '2'.
    w_prgrp = 'KD-ENG'.
  ELSE.
    w_prgrp = 'MIP-3C'.
  ENDIF.
  SELECT * INTO TABLE lt_pgmi
   FROM pgmi
   WHERE prgrp = w_prgrp
** for E002
*     AND WERKS = C_WERKS.
     AND werks = w_werks.

  LOOP AT lt_pgmi.
    it_tab_temp-matnr = lt_pgmi-nrmit.
    it_tab_temp-alc_vals = lt_pgmi-nrmit.
** for E002
*    IT_TAB_TEMP-WERKS = C_WERKS.
    it_tab_temp-werks = w_werks.
    APPEND it_tab_temp.
  ENDLOOP.

  REFRESH lt_pgmi.

  IF it_tab_temp[] IS INITIAL.
    w_no_data = 'X'.
    MESSAGE i000(zz) WITH  'No data found'.
    EXIT.
  ELSE.
    SELECT matnr sum01 INTO TABLE lt_stock
       FROM mdkp
       FOR ALL ENTRIES IN it_tab_temp
       WHERE matnr = it_tab_temp-matnr
*         AND PLWRK = IT_TAB_TEMP-WERKS
         AND dtart = 'MD'
         AND plscn = '   '.
*         AND DSDAT = Z_BEG_DATE.

    LOOP AT lt_stock.
      it_stock = lt_stock.
      COLLECT it_stock.
      CLEAR: it_stock, lt_stock.
    ENDLOOP.
    LOOP AT it_tab_temp.
      it_tab-matnr = it_tab_temp-matnr.
      it_tab-matnr_s = it_tab_temp-matnr.
      it_tab-desc = 'Veh Req'.
      SELECT SINGLE maktx INTO it_tab-maktx
      FROM makt
      WHERE matnr = it_tab_temp-matnr.

      READ TABLE it_stock WITH KEY matnr = it_tab_temp-matnr.
      IF sy-subrc = 0.
        it_tab-labst = it_stock-sum01.
      ENDIF.
*      IF L_LINE = 1.
*        IT_TAB-IF = 'C210'.
*      ENDIF.
      APPEND it_tab.
      CLEAR: it_tab_temp, it_tab.
*      IF L_LINE = 0.
*        L_LINE = 1.
*      ELSE.
*        L_LINE = 0.
*      ENDIF.
    ENDLOOP.

* ONLY FOR TESTING
*    W_D-1 = Z_BEG_DATE - 1.
*    W_MTD =  Z_BEG_DATE - 360.                              "07/01/07
    IF w_prdt = '3'.
** 3C
      SELECT matnr bdter bdmng INTO TABLE lt_resb
       FROM resb
       FOR ALL ENTRIES IN it_tab_temp
       WHERE matnr = it_tab_temp-matnr
** for E002
*         AND WERKS = 'E001'
       AND werks = w_werks
** End
         AND bdter BETWEEN z_beg_date AND z_max_date.

      SELECT matnr bdter bdmng INTO TABLE lt_mdsm
       FROM mdsm
       FOR ALL ENTRIES IN it_tab_temp
       WHERE plscn = '900'
         AND matnr = it_tab_temp-matnr
*         AND WERKS = 'E001'
         AND werks = w_werks
         AND bdter BETWEEN z_beg_week AND z_max_week.

      LOOP AT lt_resb.
        it_resb = lt_resb.
        COLLECT it_resb.
        CLEAR: lt_resb, it_resb.
      ENDLOOP.
      LOOP AT lt_mdsm.
        it_mdsm = lt_mdsm.
        COLLECT it_mdsm.
        CLEAR: lt_mdsm, it_mdsm.
      ENDLOOP.
      SORT it_resb BY matnr bdter.
      SORT it_mdsm BY matnr bdter.

      LOOP AT it_tab.
*        SELECT * INTO TABLE LT_MSEG
*         FROM MSEG
*         WHERE MATNR = IT_TAB-MATNR
*           AND WERKS = 'P001'
*           AND BWART BETWEEN '261' AND '262'
*           AND ZBUDAT BETWEEN   W_MTD AND W_D-1.
        SELECT * INTO TABLE lt_eng_stock
         FROM ztpp_eng_stock
         WHERE matnr = it_tab-matnr
*           AND WERKS = 'P001'
           AND bwart BETWEEN '261' AND '262'
           AND budat BETWEEN   w_mtd AND w_d-1.

        LOOP AT lt_eng_stock.
          IF lt_eng_stock-shkzg = 'S'.
            lt_eng_stock-menge = - lt_eng_stock-menge.
          ENDIF.
          it_tab-mtd = it_tab-mtd + lt_eng_stock-menge.
          IF lt_eng_stock-budat = w_d-1.
            it_tab-d-1 = it_tab-d-1 + lt_eng_stock-menge.
          ENDIF.
        ENDLOOP.

        REFRESH lt_eng_stock.

        LOOP AT it_day.
          READ TABLE it_resb WITH KEY matnr = it_tab-matnr
                                      bdter = it_day-datum BINARY SEARCH
.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs-qty>.
            IF sy-subrc = 0.
              <fs-qty> = it_resb-bdmng.
            ENDIF.
          ENDIF.
        ENDLOOP.
        LOOP AT it_week.
          READ TABLE it_mdsm WITH KEY matnr = it_tab-matnr
                                      bdter = it_week-datum
                                      BINARY SEARCH.
          IF sy-subrc = 0.
            CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
            ASSIGN (l_text) TO <fs-qty>.
            IF sy-subrc = 0.
              <fs-qty> = it_mdsm-bdmng.
            ENDIF.
          ENDIF.
        ENDLOOP.

        it_tab-seq = '1'.
        MODIFY it_tab.
      ENDLOOP.

    ELSE.

      PERFORM set_days_conver.

      SELECT * INTO TABLE it_seq_sum02
       FROM ztpp_seq_sum02
       FOR ALL ENTRIES IN it_tab_temp
       WHERE alc_vals = it_tab_temp-alc_vals.

      SELECT * INTO TABLE it_seq_sum03
        FROM ztpp_seq_sum03
        FOR ALL ENTRIES IN it_tab_temp
        WHERE alc_vals = it_tab_temp-alc_vals.


      LOOP AT it_tab.
*
*        SELECT * INTO TABLE LT_MSEG
*         FROM MSEG
*         WHERE MATNR = IT_TAB-MATNR
*           AND WERKS = 'P001'
*           AND BWART BETWEEN '261' AND '262'
*           AND ZBUDAT BETWEEN   W_MTD AND W_D-1.
*
*        LOOP AT LT_MSEG.
*          IF LT_MSEG-SHKZG = 'S'.
*            LT_MSEG-MENGE = - LT_MSEG-MENGE.
*          ENDIF.
*          IT_TAB-MTD = IT_TAB-MTD + LT_MSEG-MENGE.
*          IF LT_MSEG-ZBUDAT = W_D-1.
*            IT_TAB-D-1 = IT_TAB-D-1 + LT_MSEG-MENGE.
*          ENDIF.
*        ENDLOOP.

        SELECT * INTO TABLE lt_eng_stock
         FROM ztpp_eng_stock
         WHERE matnr = it_tab-matnr
*           AND WERKS = 'P001'
           AND bwart BETWEEN '261' AND '262'
           AND budat BETWEEN   w_mtd AND w_d-1.

        LOOP AT lt_eng_stock.
          IF lt_eng_stock-shkzg = 'S'.
            lt_eng_stock-menge = - lt_eng_stock-menge.
          ENDIF.
          it_tab-mtd = it_tab-mtd + lt_eng_stock-menge.
          IF lt_eng_stock-budat = w_d-1.
            it_tab-d-1 = it_tab-d-1 + lt_eng_stock-menge.
          ENDIF.
        ENDLOOP.

        REFRESH lt_eng_stock.

        READ TABLE it_seq_sum02 WITH KEY alc_vals = it_tab-matnr.
        IF sy-subrc = 0.
          l_cn = '01'.
          LOOP AT it_day.

** Furong on 02/10/12 for Engine split
            READ TABLE it_day_conv WITH KEY datum = it_day-datum.
            IF sy-subrc <> 0.
              CONTINUE.
            ENDIF.
            CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
            ASSIGN (l_text) TO <fs-qty>.
            CONCATENATE 'IT_SEQ_SUM02-D' l_cn INTO  l_text.
            l_cn = l_cn + 1.
*            CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*            ASSIGN (L_TEXT) TO <FS-QTY>.
*            CONCATENATE 'IT_SEQ_SUM02-D' IT_DAY-SEQ INTO  L_TEXT.
* End on 02/10/12
            ASSIGN (l_text) TO <fs01>.
            <fs-qty> = <fs01>.

          ENDLOOP.
        ENDIF.

        READ TABLE it_seq_sum03 WITH KEY alc_vals = it_tab-matnr.
        IF sy-subrc = 0.
          LOOP AT it_week.
            CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
            ASSIGN (l_text) TO <fs-qty>.
            CONCATENATE 'IT_SEQ_SUM03-W' it_week-seq INTO  l_text.
            ASSIGN (l_text) TO <fs01>.
            <fs-qty> = <fs01>.
          ENDLOOP.
        ENDIF.
        it_tab-seq = '1'.
        MODIFY it_tab.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_req_data

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

*  SELECT SINGLE KALID INTO L_KALID
*    FROM ZVPP_CAPACITY
*   WHERE ARBPL = 'T'   .

** Furong on 02/07/12 for E002
* SELECT SINGLE KALID INTO L_KALID
*    FROM ZVPP_CAPACITY
*   WHERE ARBPL = 'T'
*     AND WERKS = W_WERKS.

  IF w_werks = 'P001'.
    SELECT SINGLE kalid INTO l_kalid
       FROM zvpp_capacity
      WHERE arbpl = 'T'.
  ELSE.
    l_kalid = 'HE'.
  ENDIF.
** End on 02/07/12

*  PERFORM READ_SHOP_CALID  USING L_KALID.
** Furong on 02/08/12 for E002
*  SELECT MAX( SQDT ) INTO Z_MAX_DATE
*    FROM ZTPP_PMT07JB_A
*     WHERE GUBB = 'A'.

  z_max_date = z_beg_week.
** End on 02/08/12

  IF z_beg_date > z_max_date.
    z_max_date = z_beg_date.
  ENDIF.

  it_day-seq = 1.
  it_day-datum = z_beg_date.
  APPEND it_day.
  l_count = '01'.
  l_date = z_beg_date.

  WHILE l_date < z_max_date.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+' l_kalid  l_date.
    IF l_date >= z_max_date.
      EXIT.
    ELSE.
      it_day-seq     = l_count.
      it_day-datum   = l_date .
      APPEND it_day.  CLEAR: it_day.
    ENDIF.
  ENDWHILE.

*-< Victor 06.09.2014  fixed Daily period : only display 21 days
  DELETE it_day WHERE seq > 21.
*->

** Furong on 02/14/12 for E002
*  IF w_max_day_cn > 1.
*    w_max_day_cn = l_count - 1.
*  ELSE.
*    w_max_day_cn = l_count.
*  ENDIF.
  DESCRIBE TABLE it_day LINES w_max_day_cn.
** END ON 02/14/12
  w_d-1 = z_beg_date - 1.
  CONCATENATE w_d-1+0(6) '01' INTO w_mtd.
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  read_shop_calid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING p_l_kalid.

** Furong on 02/07/12
*  SELECT SINGLE KALID INTO P_L_KALID
*  FROM ZVPP_CAPACITY
* WHERE ARBPL = 'T'   .
  IF w_werks = 'P001'.
    SELECT SINGLE kalid INTO l_kalid
    FROM zvpp_capacity
    WHERE arbpl = 'T'.
  ELSE.
    l_kalid = 'HE'.
  ENDIF.
** End on 02/107/12


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
FORM get_pir_data.
* make PIR and stock data (secon and third line of it_tab)

  DATA: it_eng_pir LIKE TABLE OF ztpp_eng_pir WITH HEADER LINE,
        lt_eng_stock LIKE TABLE OF ztpp_eng_stock WITH HEADER LINE.
  DATA: l_matnr LIKE mara-matnr,
        l_cn(2) TYPE n,
        l_text(30).
  DATA: wa_tab1 LIKE it_tab,
        wa_tab3 LIKE it_tab.

  FIELD-SYMBOLS : <l_stock>, <l_pir>, <l_req>.

  SELECT * INTO TABLE it_eng_pir
   FROM ztpp_eng_pir
   FOR ALL ENTRIES IN it_tab_temp
   WHERE matnr  = it_tab_temp-matnr
     AND wdatu = w_datum.

  LOOP AT it_tab_temp.
    CLEAR: wa_tab1, wa_tab3, it_tab, it_eng_pir.
    REFRESH lt_eng_stock.

    READ TABLE it_tab WITH KEY matnr = it_tab_temp-matnr.
    wa_tab1 = it_tab.
    CLEAR: it_tab.

    LOOP AT it_day.
      READ TABLE it_eng_pir WITH KEY matnr = it_tab_temp-matnr
                                     pdatu = it_day-datum
                                     entlu = '1'.
      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = it_eng_pir-plnmg.
        ENDIF.
      ENDIF.
    ENDLOOP.
    LOOP AT it_week.
      READ TABLE it_eng_pir WITH KEY matnr = it_tab_temp-matnr
                                     pdatu = it_week-datum
                                     entlu = '2'.
      IF sy-subrc = 0.
        CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs01>.
        IF sy-subrc = 0.
          <fs01> = it_eng_pir-plnmg.
        ENDIF.
      ENDIF.
    ENDLOOP.

    it_tab-matnr = it_tab_temp-matnr.
    it_tab-seq = '3'.

*    SELECT * INTO TABLE LT_MSEG
*    FROM MSEG
*     WHERE MATNR = IT_TAB_TEMP-MATNR
*       AND WERKS = 'E001'
*       AND BWART BETWEEN '131' AND '132'
*       AND ZBUDAT BETWEEN  W_MTD AND W_D-1.
*   LOOP AT LT_MSEG.
*      IF LT_MSEG-SHKZG = 'H'.
*        LT_MSEG-MENGE = - LT_MSEG-MENGE.
*      ENDIF.
*      IT_TAB-MTD = IT_TAB-MTD + LT_MSEG-MENGE.
*      IF LT_MSEG-ZBUDAT = W_D-1.
*        IT_TAB-D-1 = IT_TAB-D-1 + LT_MSEG-MENGE.
*      ENDIF.
*    ENDLOOP.

    SELECT * INTO TABLE lt_eng_stock
      FROM ztpp_eng_stock
  WHERE matnr = it_tab_temp-matnr
*       AND WERKS = 'E001'
    AND bwart BETWEEN '131' AND '132'
    AND budat BETWEEN  w_mtd AND w_d-1.

    LOOP AT lt_eng_stock.
      IF lt_eng_stock-shkzg = 'H'.
        lt_eng_stock-menge = - lt_eng_stock-menge.
      ENDIF.
      it_tab-mtd = it_tab-mtd + lt_eng_stock-menge.
      IF lt_eng_stock-budat = w_d-1.
        it_tab-d-1 = it_tab-d-1 + lt_eng_stock-menge.
      ENDIF.
    ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
    it_tab-if = 'C210'.
*    ENDIF.
    it_tab-desc = 'Plan'.
    wa_tab3 = it_tab.
    APPEND it_tab.
    CLEAR: it_tab.

    it_tab-seq = '2'.
    it_tab-matnr = it_tab_temp-matnr.
    it_tab-qtyd_01 = wa_tab1-labst.

    LOOP AT it_day FROM 2 .
      l_cn = it_day-seq - 1.
      CONCATENATE 'WA_TAB1-QTYD_' l_cn INTO l_text.
      IF sy-subrc = 0.
        ASSIGN (l_text) TO <l_req>.
        CONCATENATE 'IT_TAB-QTYD_'  l_cn INTO l_text.
        ASSIGN (l_text) TO <l_stock>.
        CONCATENATE 'WA_TAB3-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_pir>.

        CONCATENATE 'IT_TAB-QTYD_' it_day-seq INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = <l_stock> + <l_pir> - <l_req>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE 'WA_TAB1-QTYD_' w_max_day_cn INTO l_text.
    ASSIGN (l_text) TO <l_req>.
    CONCATENATE 'IT_TAB-QTYD_'  w_max_day_cn INTO l_text.
    ASSIGN (l_text) TO <l_stock>.
    CONCATENATE 'WA_TAB3-QTYD_' w_max_day_cn INTO l_text.
    ASSIGN (l_text) TO <l_pir>.

    it_tab-qtyw_04 = <l_stock> + <l_pir> - <l_req>.

    LOOP AT it_week FROM 2.
      l_cn = it_week-seq - 1.
      CONCATENATE 'WA_TAB1-QTYW_' l_cn INTO l_text.
      IF sy-subrc = 0.
        ASSIGN (l_text) TO <l_req>.
        CONCATENATE 'IT_TAB-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_stock>.
        CONCATENATE 'WA_TAB3-QTYW_' l_cn INTO l_text.
        ASSIGN (l_text) TO <l_pir>.

        CONCATENATE 'IT_TAB-QTYW_' it_week-seq INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = <l_stock> + <l_pir> - <l_req>.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
*      IT_TAB-IF = 'C210'.
*    ENDIF.
    it_tab-desc = 'Stock'.
    APPEND it_tab.
    CLEAR: it_tab.
  ENDLOOP.

  SORT it_tab BY matnr seq.

ENDFORM.                    "GET_PIR_DATA

INCLUDE zapp_eng_pir_pbo.

INCLUDE zapp_eng_pir_pai.

*&---------------------------------------------------------------------*
*&      Form  BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_LINE  text
*      -->P_1971   text
*----------------------------------------------------------------------*
*FORM BUILD_COLOR_ALL USING p_line p_fname.
*  if p_line  = 1.
*    wa_color-color-col = 6.
*    wa_color-color-int = 1.
*    wa_color-fname = p_fname.
*    append wa_color to it_color.
*    clear wa_color.
*  endif.
*endform.                    " BUILD_COLOR_ALL
*&---------------------------------------------------------------------*
*&      Form  SET_WEEKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_weeks.
  DATA: BEGIN OF lt_week OCCURS 0,
        sqdt LIKE sy-datum,
        END OF lt_week.
  DATA: l_cn(2) TYPE n.

  CLEAR: it_week, it_week[].
  SELECT sqdt INTO TABLE lt_week
   FROM ztpp_pmt07jb_a
    WHERE gubb EQ 'B'.

  SORT lt_week BY sqdt.
  DELETE ADJACENT DUPLICATES FROM lt_week COMPARING sqdt.

  l_cn = '04'.
  LOOP AT lt_week.
    it_week-seq = l_cn.
    it_week-datum = lt_week-sqdt.
    APPEND it_week.
    l_cn =  l_cn + 1.
  ENDLOOP.

  DESCRIBE TABLE it_week LINES l_cn.
  READ TABLE it_week INDEX 1.
  z_beg_week = it_week-datum.

  READ TABLE it_week INDEX l_cn.
  z_max_week = it_week-datum.

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

  LOOP AT it_tab.
    l_index = sy-tabix.
    REFRESH lt_celltab.
    IF it_tab-seq = '3'.
      l_mode = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      l_mode = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.

    w_celltab-fieldname = 'MATNR_S'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'MAKTX'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'DESC'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'MTD'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'D-1'.
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

    w_celltab-fieldname = 'QTYW_04'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_05'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_06'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_07'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_08'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_09'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_10'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_11'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_12'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_13'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_14'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_15'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_16'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_17'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_18'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_19'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_20'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.
    w_celltab-fieldname = 'QTYW_21'.
    w_celltab-style = l_mode.
    INSERT w_celltab INTO TABLE lt_celltab.

    INSERT LINES OF lt_celltab INTO TABLE it_tab-celltab.
    MODIFY it_tab INDEX l_index.
  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_display.
  DATA: l_qty TYPE i,
        l_cn(2) TYPE n,
        l_qty1(13), " LIKE IT_TAB-QTYD_01.
        l_text(35).
  DATA: BEGIN OF lt_temp OCCURS 0,
        werks LIKE marc-werks,
        matnr LIKE mara-matnr,
        matnr_s LIKE mara-matnr,
        labst LIKE mard-labst,
        seq(1),
        desc(15),
        mtd TYPE i,
        d-1 TYPE i,
        qtyd_01 TYPE i,
        qtyd_02 TYPE i,
        qtyd_03 TYPE i,
        qtyd_04 TYPE i,
        qtyd_05 TYPE i,
        qtyd_06 TYPE i,
        qtyd_07 TYPE i,
        qtyd_08 TYPE i,
        qtyd_09 TYPE i,
        qtyd_10 TYPE i,
        qtyd_11 TYPE i,
        qtyd_12 TYPE i,
        qtyd_13 TYPE i,
        qtyd_14 TYPE i,
        qtyd_15 TYPE i,
        qtyd_16 TYPE i,
        qtyd_17 TYPE i,
        qtyd_18 TYPE i,
        qtyd_19 TYPE i,
        qtyd_20 TYPE i,
        qtyd_21 TYPE i,
        qtyw_04 TYPE i,
        qtyw_05 TYPE i,
        qtyw_06 TYPE i,
        qtyw_07 TYPE i,
        qtyw_08 TYPE i,
        qtyw_09 TYPE i,
        qtyw_10 TYPE i,
        qtyw_11 TYPE i,
        qtyw_12 TYPE i,
        qtyw_13 TYPE i,
        qtyw_14 TYPE i,
        qtyw_15 TYPE i,
        qtyw_16 TYPE i,
        qtyw_17 TYPE i,
        qtyw_18 TYPE i,
        qtyw_19 TYPE i,
        qtyw_20 TYPE i,
        qtyw_21 TYPE i,
        meins LIKE mara-meins,
        total LIKE mdsm-bdmng,
        maktx LIKE makt-maktx,
        if(4) TYPE c,
        celltab TYPE lvc_t_styl,
       END OF lt_temp.

  CLEAR: it_output, it_output[], it_total, it_total[],
         wa_tot1, wa_tot2, wa_tot3.
  LOOP AT it_tab.
    MOVE-CORRESPONDING it_tab TO it_output.
    l_qty = it_tab-mtd.
    it_output-mtd = l_qty.
    l_qty = it_tab-d-1.
    it_output-d-1 = l_qty.

    CASE it_tab-seq.
      WHEN '1'.
        wa_tot1-mtd = wa_tot1-mtd + it_tab-mtd.
        wa_tot1-d-1 = wa_tot1-d-1 + it_tab-d-1.
      WHEN '2'.
        wa_tot2-mtd = wa_tot2-mtd + it_tab-mtd.
        wa_tot2-d-1 = wa_tot2-d-1 + it_tab-d-1.
      WHEN '3'.
        wa_tot3-mtd = wa_tot3-mtd + it_tab-mtd.
        wa_tot3-d-1 = wa_tot3-d-1 + it_tab-d-1.
    ENDCASE.

    l_cn = '00'.
    DO 21 TIMES.
      l_cn =  l_cn + 1.
      CONCATENATE 'IT_TAB-QTYD_' l_cn INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        l_qty = <fs01>.
        l_qty1 = l_qty.

        CASE it_tab-seq.
          WHEN '1'.
            CONCATENATE 'WA_TOT1-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '2'.
            CONCATENATE 'WA_TOT2-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '3'.
            CONCATENATE 'WA_TOT3-QTYD_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
        ENDCASE.

        CONCATENATE 'IT_OUTPUT-QTYD_' l_cn INTO l_text.
        ASSIGN (l_text) TO <fs-qty>.
        IF sy-subrc = 0.
          <fs-qty> = l_qty1.
        ENDIF.
      ENDIF.
    ENDDO.
    l_cn = '03'.
    DO 18 TIMES.
      l_cn =  l_cn + 1.
      CONCATENATE 'IT_TAB-QTYW_' l_cn INTO l_text.
      ASSIGN (l_text) TO <fs01>.
      IF sy-subrc = 0.
        l_qty = <fs01>.
        l_qty1 = l_qty.
** total
*        CONCATENATE 'IT_TOTAL-QTYW_' L_CN INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS01>.
*        IF SY-SUBRC = 0.
*          <FS01> = <FS01> + L_QTY.
*        ENDIF.
**
        CASE it_tab-seq.
          WHEN '1'.
            CONCATENATE 'WA_TOT1-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '2'.
            CONCATENATE 'WA_TOT2-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
          WHEN '3'.
            CONCATENATE 'WA_TOT3-QTYW_' l_cn INTO l_text.
            ASSIGN (l_text) TO <fs01>.
            IF sy-subrc = 0.
              <fs01> = <fs01> + l_qty.
            ENDIF.
        ENDCASE.

      ENDIF.
      CONCATENATE 'IT_OUTPUT-QTYW_' l_cn INTO l_text.
      ASSIGN (l_text) TO <fs-qty>.
      IF sy-subrc = 0.
        <fs-qty> = l_qty1.
      ENDIF.
    ENDDO.
    APPEND it_output.
  ENDLOOP.
  wa_tot1-matnr_s = 'Total'.
  wa_tot1-desc = 'Veh Req'.
  wa_tot1-if = 'C410'.
  MOVE-CORRESPONDING wa_tot1 TO lt_temp.
  MOVE-CORRESPONDING lt_temp TO it_total.
  APPEND it_total.
  wa_tot2-desc = 'Stock'.
  wa_tot2-if = 'C410'.
  MOVE-CORRESPONDING wa_tot2 TO lt_temp.
  MOVE-CORRESPONDING lt_temp TO it_total.
  APPEND it_total.
  wa_tot3-desc = 'Plan'.
  wa_tot3-if = 'C410'.
  MOVE-CORRESPONDING wa_tot3 TO lt_temp.
  MOVE-CORRESPONDING lt_temp TO it_total.
  APPEND it_total.

*  MOVE-CORRESPONDING WA_TOT1 TO IT_TOTAL.
*  APPEND IT_TOTAL.
*  WA_TOT2-DESC = 'Stock'.
*  WA_TOT2-IF = 'C410'.
*  MOVE-CORRESPONDING WA_TOT2 TO IT_TOTAL.
*  APPEND IT_TOTAL.
*  WA_TOT3-DESC = 'Plan'.
*  WA_TOT3-IF = 'C410'.
*  MOVE-CORRESPONDING WA_TOT3 TO IT_TOTAL.
*  APPEND IT_TOTAL.
ENDFORM.                    " PREPARE_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data.
  REFRESH: it_tab_temp, it_tab, it_output, it_stock, it_total.
  CLEAR: it_tab_temp, it_tab, it_output, it_stock, it_total, wa_tot1,
         wa_tot2, wa_tot3.
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
  DATA: c_jobs(40) VALUE 'ZAPP903R_INPUT_PLAN',
      c_key1(18) VALUE 'TRIM_INPUT'.

  SELECT SINGLE dates INTO z_beg_date
    FROM ztpp_common_vals
   WHERE jobs = c_jobs
     AND key2 = c_key1.

  SELECT MAX( wdatu ) INTO w_datum
  FROM ztpp_eng_pir.

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
  IF sy-tcode = 'ZAPP_ENG_PIR'.
    PERFORM select_edit_line.
  ENDIF.
  PERFORM prepare_display.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DAYS_CONVER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_days_conver .
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.
  DATA: l_kalid_conv LIKE kako-kalid.

  CLEAR: it_day_conv, it_day_conv[].

  l_kalid_conv = 'HM'.

  it_day_conv-seq = 1.
  it_day_conv-datum = z_beg_date.
  APPEND it_day_conv.
  l_count = '01'.
  l_date = z_beg_date.

  WHILE l_date < z_max_date.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+' l_kalid_conv  l_date.
    IF l_date >= z_max_date.
      EXIT.
    ELSE.
      it_day_conv-seq     = l_count.
      it_day_conv-datum   = l_date .
      APPEND it_day_conv.  CLEAR: it_day_conv.
    ENDIF.
  ENDWHILE.

ENDFORM.                    " SET_DAYS_CONVER
