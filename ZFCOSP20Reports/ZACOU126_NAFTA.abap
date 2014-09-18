*----------------------------------------------------------------------
* Program ID        : ZACOU126_NAFTA
* Title             : [CO] Comparison report for unit costing
* Created on        : 06/29/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Comparison report between plan and actual cost
*----------------------------------------------------------------------
REPORT zacou126 MESSAGE-ID zmco.
INCLUDE : z_moon_alv_top,
          z_moon_alv_fnc,
          zacou126_nafta_top.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.

PARAMETERS     : p_kokrs LIKE ztco_shop_sum-kokrs OBLIGATORY
                 MEMORY ID cac,
                 p_bdatj LIKE ztco_shop_sum-bdatj OBLIGATORY
                 MEMORY ID bdtj,
                 p_poper LIKE ztco_shop_sum-poper OBLIGATORY
                 MEMORY ID popr.
PARAMETER      : p_ver TYPE ztcou102-ver.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_kalka FOR keko-kalka NO INTERVALS
                           OBLIGATORY MEMORY ID kka.
SELECTION-SCREEN END OF BLOCK bl2.


SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE text-003.
SELECT-OPTIONS :
                 s_artnr FOR ztco_shop_sum-artnr  MEMORY ID  mat,
                 s_compn FOR ztcou103-compn,
                 s_kstar FOR ztco_shop_sum-kstar MEMORY ID ka3,
                 s_elemt FOR ztco_shop_cc-elemt,
                 s_shop  FOR ztco_shop_sum-shop,
                 s_mtart FOR mara-mtart.
SELECTION-SCREEN END OF BLOCK bl3.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-010.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-011.
PARAMETER: p_fwng AS CHECKBOX DEFAULT 'X',
           p_unit AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.

PARAMETER: p_diff AS CHECKBOX DEFAULT 'X'.

INITIALIZATION.
  g_repid = sy-repid.

  __cls s_elemt.

  s_elemt-sign = 'I'.
  s_elemt-option = 'BT'.
  s_elemt-low = '10'.
  s_elemt-low = '10'.
  s_elemt-high = '50'.

  APPEND s_elemt.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM GET_DEFAULT_VARIANT_GET CHANGING P_LISTV.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_kalka-low.
  PERFORM popup_kalka USING s_kalka-low 'S_KALKA-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  PERFORM initialize_value.

  PERFORM read_tka01.
  CHECK g_error EQ space.

  __process 'Read Plan...' '10'.
  PERFORM get_plan.       " Read data from ZTCOU103(Cost Roll-Up)

  __process 'Read Actual...' '20'.
  PERFORM get_actual.

  __process 'Merge data...' '40'.
  PERFORM get_merged.     " Merge the data plan and atual

  __process 'Fill text...' '50'.
  PERFORM fill_text.      " Fill the description for material,vendor,etc

  __process 'Analysis Data...' '60'.
  PERFORM analysis_data.  " Fill the analysis data

  __process 'Prepare screen...' '70'.
  PERFORM get_output.     " prepare the data to ALV display

*--------------------------------------------------------------------*
END-OF-SELECTION.
  CHECK g_error EQ space .

  PERFORM set_list .

*--------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_actual.

  DATA: $strlen TYPE i.
  DATA: $ix LIKE sy-tabix.


  __cls : it_actual, it_actual_manu .

  SELECT  a~kokrs a~bdatj a~poper "a~kstar
          a~fevor a~llv_matnr
          a~bwkey a~bwtar a~kostl a~artnr a~par_werks
          a~kalst a~meeht a~stprs a~verpr a~peinh a~verid
          SUM( a~manu_amt ) AS manu_amt
          SUM( a~manu_qty ) AS manu_qty


          SUM( a~add_mbgbtr ) AS add_mbgbtr
          SUM( a~mbgbtr2 ) AS mbgbtr2


          SUM( a~single_qty ) AS single_qty
          INTO CORRESPONDING FIELDS OF TABLE it_actual
          FROM ztco_shop_sum AS a
          INNER JOIN mara AS b
          ON b~matnr EQ a~llv_matnr
          WHERE a~kokrs       EQ p_kokrs
            AND a~artnr       IN s_artnr
            AND a~bdatj       EQ p_bdatj
            AND a~poper       EQ p_poper
            AND a~typps       EQ 'M'
            AND a~kstar       IN s_kstar
            AND a~llv_matnr   IN s_compn
            AND a~shop        IN s_shop
            AND b~mtart       IN s_mtart
        GROUP BY A~KOKRS a~bdatj A~POPER a~kstar a~fevor a~llv_matnr
                 a~bwkey a~bwtar a~kostl a~artnr a~par_werks
                 a~kalst a~meeht a~stprs a~verpr a~peinh a~verid.

  SELECT  a~kokrs a~bdatj a~poper "a~kstar
          a~fevor a~llv_matnr
          a~bwkey a~bwtar a~kostl a~artnr a~par_werks
          a~kalst a~meeht a~stprs a~verpr
          a~peinh a~verid
*          SUM( B~MANU_AMT ) AS MANU_AMT
          SUM( b~gr_amt ) AS manu_amt

          SUM( a~add_mbgbtr ) AS add_mbgbtr
          SUM( a~mbgbtr2 ) AS mbgbtr2

          SUM( single_qty ) AS single_qty
          INTO CORRESPONDING FIELDS OF TABLE it_actual_manu
          FROM ztco_shop_sum AS a
          INNER JOIN ztco_shop_cc AS b
          ON  b~kokrs EQ a~kokrs
          AND b~bdatj EQ a~bdatj
          AND b~poper EQ a~poper
          AND b~typps EQ a~typps
          AND b~kstar EQ a~kstar
          AND b~resou EQ a~resou
          AND b~aufnr EQ a~aufnr
          INNER JOIN mara AS c
          ON  c~matnr EQ a~llv_matnr
        WHERE a~kokrs       EQ p_kokrs
          AND a~artnr       IN s_artnr
          AND a~bdatj       EQ p_bdatj
          AND a~poper       EQ p_poper
          AND a~typps       EQ 'M'
          AND a~kstar       IN s_kstar
          AND a~llv_matnr   IN s_compn
          AND a~shop        IN s_shop
          AND a~fevor       NE space
          AND b~elemt       IN s_elemt
          AND c~mtart       IN s_mtart
        GROUP BY A~KOKRS a~bdatj A~POPER a~kstar a~fevor
                a~llv_matnr a~bwkey a~bwtar a~kostl
                a~artnr a~par_werks a~kalst
                a~meeht a~stprs a~verpr a~peinh a~verid.

  SORT it_actual_manu BY kokrs bdatj poper llv_matnr verid.
  "RESOU.

  LOOP AT it_actual WHERE fevor NE space.
    READ TABLE it_actual_manu WITH KEY  kokrs = it_actual-kokrs
                                        bdatj = it_actual-bdatj
                                        poper = it_actual-poper
*                                        kstar = it_actual-kstar
                                        llv_matnr = it_actual-llv_matnr
                                        verid = it_actual-verid
					      BINARY SEARCH .
    IF sy-subrc EQ 0.
      it_actual-manu_amt = it_actual_manu-manu_amt.
      it_actual-manu_qty = it_actual_manu-manu_qty.
      it_actual-$net_a = it_actual_manu-manu_amt. " for Debuging
      MODIFY it_actual TRANSPORTING manu_amt $net_a.
    ENDIF.
  ENDLOOP.

  PERFORM get_product_gr.

  LOOP AT it_actual.
    $ix = sy-tabix.
    READ TABLE it_ckmlmv003 WITH KEY matnr = it_actual-artnr
                                     verid_nd = it_actual-verid
        BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_actual INDEX $ix.
    ENDIF.
  ENDLOOP.


  LOOP AT it_actual.
    $ix = sy-tabix.
*          SUM( A~ADD_MBGBTR ) AS ADD_MBGBTR
*          SUM( A~MBGBTR2 ) AS MBGBTR2
    it_actual-manu_qty = it_actual-manu_qty -
     ( it_actual-add_mbgbtr + it_actual-mbgbtr2 ).
    MODIFY  it_actual INDEX $ix.
  ENDLOOP.

  LOOP AT it_actual.
    $ix = sy-tabix.

    IF it_actual-llv_matnr+5(1) = 'M'.
      it_color-matnr1 = it_actual-llv_matnr.
      it_actual-llv_matnr = it_actual-llv_matnr+(12).
      it_color-matnr2 = it_actual-llv_matnr.
      COLLECT it_color.
      MODIFY it_actual INDEX $ix TRANSPORTING llv_matnr.
    ELSE.
      $strlen = strlen( it_actual-llv_matnr ).
      IF $strlen > 11.
        CASE $strlen.
          WHEN 12 OR 13.
            it_color-matnr1 = it_actual-llv_matnr.
            it_actual-llv_matnr = it_actual-llv_matnr+(10).
            it_color-matnr2 = it_actual-llv_matnr.
            COLLECT it_color.
          WHEN 14 OR 15.
            it_color-matnr1 = it_actual-llv_matnr.
            it_actual-llv_matnr = it_actual-llv_matnr+(12).
            it_color-matnr2 = it_actual-llv_matnr.
            COLLECT it_color.
        ENDCASE.
        MODIFY it_actual INDEX $ix TRANSPORTING llv_matnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM actual_collect.

  __process 'Gather text...' '30'.
  PERFORM get_text_data.  " Read description for material, vendor,etc

  SORT it_actual BY llv_matnr.

  LOOP AT it_mat_txt WHERE NOT bismt IS initial.
    $ix = sy-tabix.
    READ TABLE it_actual WITH KEY llv_matnr = it_mat_txt-matnr
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_color-matnr1 = it_actual-llv_matnr.
      it_actual-llv_matnr = it_mat_txt-bismt.
      it_color-matnr2 = it_mat_txt-bismt.
      COLLECT it_color.
      MODIFY it_actual INDEX $ix TRANSPORTING llv_matnr.
    ENDIF.

  ENDLOOP.

  PERFORM actual_collect.

  LOOP AT it_actual.
    $ix = sy-tabix.
    PERFORM get_pp_grqty CHANGING it_actual-pp_gr_qty.
    CHECK it_actual-pp_gr_qty <> 0.
    PERFORM calculate_unit.
    MODIFY it_actual INDEX $ix.
  ENDLOOP.

  loop at it_ztcou103.
    $ix = sy-tabix.
    READ TABLE it_ckmlmv003 WITH KEY matnr = it_ztcou103-artnr
                                     verid_nd = it_ztcou103-verid
        BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_ztcou103 INDEX $ix.
    ENDIF.
  endloop.


ENDFORM.                    " GET_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  READ_TKA01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tka01.

  CLEAR tka01.

  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE s038 WITH p_kokrs.
    g_error = true.
  ENDIF.

ENDFORM.                    " READ_TKA01
*&---------------------------------------------------------------------*
*&      Form  GET_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_plan.
  __cls : it_ztcou103.


*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCOU103
*     FROM ZTCOU103 AS A
*     INNER JOIN MARA AS B
*     ON B~MATNR EQ A~COMPN
*    WHERE KOKRS       EQ P_KOKRS
*      AND BDATJ       EQ P_BDATJ
*      AND KALKA       IN S_KALKA
*      AND POPER       EQ P_POPER
*      AND ARTNR       IN S_ARTNR
*      AND TYPPS       EQ 'M'
*      AND KSTAR       IN S_KSTAR
*      AND COMPN       IN S_COMPN
*      AND VER         EQ P_VER
*      AND B~MTART     IN S_MTART.

  SELECT
kokrs
klvar
bdatj
poper
artnr
verid
refdt
werks
compn
indx
kstar
reqqt AS menge
meeht
hwaer
verpr
peinh
gpreis
amtdt  AS duty
amtft  AS frg
amtot  AS oth
total  AS wertn
*WERTN
typps
strat
lifnr
infnr
splnt
upgvc
bklas
a~meins
tvers
losgr
mkalk
btyp
misch_verh
verid
  INTO CORRESPONDING FIELDS OF TABLE it_ztcou103
     FROM ztco_ck11 AS a
     INNER JOIN mara AS b
     ON b~matnr EQ a~compn
    WHERE kokrs       EQ p_kokrs
      AND klvar       EQ 'ZUNF'
      AND bdatj       EQ p_bdatj
      AND poper       EQ p_poper
      AND artnr       IN s_artnr
      AND typps       EQ 'M'
      AND kstar       IN s_kstar
      AND compn       IN s_compn
      AND b~mtart     IN s_mtart.

  DATA: $strlen TYPE i.
  DATA: $ix LIKE sy-tabix.

  LOOP AT it_ztcou103.
    $ix = sy-tabix.
    $strlen = strlen( it_ztcou103-compn ).
    IF it_ztcou103-compn+5(1) = 'M'.
      "    it_ztcou103-artnr+13(1) ne space.
      it_color-matnr1 = it_ztcou103-compn.
      it_ztcou103-compn = it_ztcou103-compn+(12).
      it_color-matnr2 = it_ztcou103-compn.
      COLLECT it_color.
      MODIFY it_ztcou103 INDEX $ix TRANSPORTING compn.
    ELSE.
      IF $strlen > 11.
        CASE $strlen.
          WHEN 12 OR 13.
            it_color-matnr1 = it_ztcou103-compn.
            it_ztcou103-compn = it_ztcou103-compn+(10).
            it_color-matnr2 = it_ztcou103-compn.
            COLLECT it_color.
          WHEN 14 OR 15.
            it_color-matnr1 = it_ztcou103-compn.
            it_ztcou103-compn = it_ztcou103-compn+(12).
            it_color-matnr2 = it_ztcou103-compn.
            COLLECT it_color.
        ENDCASE.
        MODIFY it_ztcou103 INDEX $ix TRANSPORTING compn.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA $it_ztcou103 LIKE it_ztcou103 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_ztcou103.
    $it_ztcou103 = it_ztcou103.
    COLLECT $it_ztcou103.
  ENDLOOP.

  __cls it_ztcou103.
  it_ztcou103[] = $it_ztcou103[].

ENDFORM.                    " GET_PLAN
*&---------------------------------------------------------------------*
*&      Form  GET_MERGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_merged.
  DATA: l_actual_exist TYPE i.
  DATA $verid TYPE verid.

  __cls it_row_tab.

  SORT it_actual BY kokrs bdatj poper
                    artnr verid llv_matnr par_werks.

  LOOP AT it_ztcou103 .
    $verid = it_ztcou103-verid.
    AT NEW artnr.
      READ TABLE it_ckmlmv003 WITH KEY matnr = it_ztcou103-artnr
                                       verid_nd = $verid
          BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_actual_exist = 1.
      ELSE.
        l_actual_exist = 0.
      ENDIF.
    ENDAT.

    IF l_actual_exist = 1.
      MOVE-CORRESPONDING it_ztcou103 TO it_row_tab.
      MOVE : it_ztcou103-menge  TO it_row_tab-pln_qty,
             it_ztcou103-gpreis TO it_row_tab-pln_unp,
             it_ztcou103-wertn TO it_row_tab-pln_amt.
      APPEND it_row_tab.
    ENDIF.

  ENDLOOP.

  DATA $ix LIKE sy-tabix.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    READ TABLE it_actual WITH KEY kokrs = it_row_tab-kokrs
                                  bdatj = it_row_tab-bdatj
                                  poper = it_row_tab-poper
                                  artnr = it_row_tab-artnr
                                  verid = it_row_tab-verid
                                  llv_matnr = it_row_tab-compn
*                                  KSTAR = IT_ROW_TAB-KSTAR
                                  par_werks = it_row_tab-werks
                                  BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_actual-chk = true.
      MODIFY it_actual INDEX sy-tabix TRANSPORTING chk.
      PERFORM it_row_tab_with_actual USING it_row_tab-compn.
    ELSE.
      it_row_tab-miss_act = true.
    ENDIF.
    MODIFY it_row_tab INDEX $ix TRANSPORTING act_qty act_amt
                               manu_qty manu_amt pp_gr_qty miss_act
                                   compn_a.

  ENDLOOP.

  CLEAR it_row_tab.

  SORT it_row_tab BY kokrs bdatj poper
                     artnr verid compn.
  DATA $ix2 LIKE sy-tabix.

* for Following Part
  DATA : tot_pln_qty LIKE it_row_tab-pln_qty,
         tot_pln_amt LIKE it_row_tab-pln_amt,
         tot_act_qty LIKE it_row_tab-act_qty,
         tot_act_amt LIKE it_row_tab-act_amt.

  LOOP AT it_actual WHERE chk = false.

    $ix = sy-tabix.

    READ TABLE it_ckmlmv003 WITH KEY matnr = it_actual-artnr
                                     verid_nd = it_actual-verid
        BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    from_matnr = it_actual-llv_matnr .
    CLEAR to_matnr .
    PERFORM get_following_part USING from_matnr
                            CHANGING to_matnr.
    IF to_matnr NE space.
      READ TABLE it_row_tab WITH KEY kokrs = it_actual-kokrs
                                     bdatj = it_actual-bdatj
                                     poper = it_actual-poper
                                     artnr = it_actual-artnr
                                     verid =  it_actual-verid
                                     compn = to_matnr
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        $ix2 = sy-tabix.

        it_actual-chk = true.
        MODIFY it_actual INDEX $ix TRANSPORTING chk.

*********************// Following Part //*******************
******
******   Planned Qty1 =
******                                   Actual Qty 1
******         Planned Qty       x     ------------------
******                                  Total Actual Qty
******
******   Planned Qty2 =
******                                   Actual Qty 2
******         Planned Qty       x     ------------------
******                                  Total Actual Qty
******
************************************************************
        tot_pln_qty = it_row_tab-pln_qty .
        tot_pln_amt = it_row_tab-pln_amt .

        tot_act_qty = it_row_tab-act_qty + it_actual-act_qty .
        tot_act_amt = it_row_tab-act_amt + it_actual-act_amt .

*     " Original Planned Quantity & Amount
        it_row_tab-pln_qty = tot_pln_qty *
                             it_row_tab-act_qty / tot_act_qty.
        it_row_tab-pln_amt = tot_pln_amt *
*                             IT_ROW_TAB-ACT_AMT / TOT_ACT_AMT.
                             it_row_tab-act_qty / tot_act_qty.

        MODIFY it_row_tab INDEX $ix2 TRANSPORTING pln_qty pln_amt .

*     " New Planned Quantity & Amount
        MOVE-CORRESPONDING it_actual TO it_row_tab.

        it_row_tab-pln_qty = tot_pln_qty *
                             it_actual-act_qty / tot_act_qty.
        it_row_tab-pln_amt = tot_pln_amt *
*                             IT_ACTUAL-ACT_AMT / TOT_ACT_AMT.
                             it_actual-act_qty / tot_act_qty.
        it_row_tab-compn_a = from_matnr.
        it_row_tab-miss_act = false.
        APPEND it_row_tab.
        CLEAR it_row_tab.

*        MODIFY IT_ROW_TAB INDEX $IX2 TRANSPORTING ACT_QTY ACT_AMT
*                                   MANU_QTY MANU_AMT PP_GR_QTY MISS_ACT
*                                       COMPN_A.

      ELSE.
        MOVE-CORRESPONDING it_actual TO it_row_tab.
        it_row_tab-compn_a = it_actual-llv_matnr.
        it_row_tab-miss_pln = true.
        APPEND it_row_tab.CLEAR it_row_tab.
      ENDIF.
    ELSE.
      MOVE-CORRESPONDING it_actual TO it_row_tab.
      it_row_tab-compn_a = it_actual-llv_matnr.
      it_row_tab-miss_pln = true.
      APPEND it_row_tab.CLEAR it_row_tab.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE it_row_tab LINES sy-tabix.
  IF sy-tabix EQ 0.
    g_error = true.
    MESSAGE s000 WITH 'Could not find data.'.
  ENDIF.

  DATA : BEGIN OF it_upgvc_c_sum  OCCURS 0,
              artnr   TYPE matnr,
              verid LIKE ckmlmv001-verid_nd,
              upgvc           LIKE ztcou103-upgvc,
              compn           LIKE ztcou103-compn,
              pln_qty         LIKE ztcou103-menge,
         END OF it_upgvc_c_sum.

  DATA : BEGIN OF it_compn_sum  OCCURS 0,
              artnr   TYPE matnr,
              verid LIKE ckmlmv001-verid_nd,
              compn           LIKE ztcou103-compn,
              pln_qty         LIKE ztcou103-menge,
         END OF it_compn_sum.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO :
            it_upgvc_c_sum,
            it_compn_sum.

    COLLECT : it_upgvc_c_sum,
              it_compn_sum.

  ENDLOOP.

  SORT : it_upgvc_c_sum BY artnr verid upgvc compn,
         it_compn_sum BY artnr verid compn .

  LOOP AT it_row_tab.
    $ix = sy-tabix.

* default {
    it_row_tab-act_s_qty = it_row_tab-act_qty.
    it_row_tab-act_s_amt = it_row_tab-act_amt.

    IF it_row_tab-act_s_qty <> 0.
      it_row_tab-act_s_unp = it_row_tab-act_s_amt
                              / it_row_tab-act_s_qty / 1000.
    ENDIF.

    it_row_tab-t_pln_q = it_row_tab-pln_qty . " default.
* }

*   distribute ... if component was used by more than one upgvc

    READ TABLE it_compn_sum WITH KEY artnr = it_row_tab-artnr
                                     verid = it_row_tab-verid
                                     compn = it_row_tab-compn
                                     BINARY SEARCH.

    IF sy-subrc EQ 0.
*   {
      READ TABLE it_upgvc_c_sum WITH KEY  artnr = it_row_tab-artnr
                                          verid =  it_row_tab-verid
                                          upgvc = it_row_tab-upgvc
                                          compn = it_row_tab-compn
                                          BINARY SEARCH.
      IF sy-subrc EQ 0.
*     {
        it_row_tab-t_pln_q = it_row_tab-pln_qty.

        IF it_upgvc_c_sum-pln_qty <> it_compn_sum-pln_qty.
*       {
          it_row_tab-act_s_qty =
     it_row_tab-act_qty * it_row_tab-pln_qty / it_compn_sum-pln_qty.

          it_row_tab-act_s_amt =
     it_row_tab-act_amt * it_row_tab-pln_qty / it_compn_sum-pln_qty.

          IF it_row_tab-act_s_qty <> 0.
            it_row_tab-act_s_unp = it_row_tab-act_s_amt
                                    / it_row_tab-act_s_qty.
          ENDIF.
*       }
        ENDIF.
*     }
      ENDIF.
*     }
    ENDIF.

    it_row_tab-dif_qty = it_row_tab-act_s_qty - it_row_tab-pln_qty.
    it_row_tab-dif_amt = it_row_tab-act_s_amt - it_row_tab-pln_amt.

   MODIFY it_row_tab INDEX $ix TRANSPORTING t_pln_q act_s_qty act_s_amt
                                                              act_s_unp
                                                        dif_qty dif_amt.
  ENDLOOP.

ENDFORM.                    " GET_MERGED
*&---------------------------------------------------------------------*
*&      Form  get_product_gr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_product_gr.

  DATA : BEGIN OF it_proc_gr OCCURS 0,
          par_werks LIKE ztco_shop_sum-bwkey,
          artnr     LIKE ztco_shop_sum-artnr,
          aufnr     LIKE ztco_shop_sum-aufnr,
          verid     LIKE ztco_shop_sum-verid,
          bdatj     LIKE ztco_shop_sum-bdatj,
          poper     LIKE ztco_shop_sum-poper,
        END OF it_proc_gr.

  DATA : it_ckmlmv003_temp LIKE it_ckmlmv003 OCCURS 0 WITH HEADER LINE.

  LOOP AT it_actual.
    MOVE-CORRESPONDING it_actual TO it_proc_gr.
    APPEND it_proc_gr. CLEAR it_proc_gr.
  ENDLOOP.

  SORT it_proc_gr.
  DELETE ADJACENT DUPLICATES FROM it_proc_gr.

  CHECK NOT it_proc_gr[] IS INITIAL.

** read GR data
  SELECT  a~bwkey a~matnr a~verid_nd
          c~aufnr
          b~out_menge
          b~meinh
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003_temp
    FROM ckmlmv001 AS a
    INNER JOIN ckmlmv003 AS b
       ON a~kalnr    =  b~kalnr_bal
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = b~kalnr_in
     FOR ALL ENTRIES IN it_proc_gr
   WHERE a~werks    =  it_proc_gr-par_werks
     AND a~matnr    =  it_proc_gr-artnr
     AND a~btyp     =  'BF'
     AND a~bwkey    =  it_proc_gr-par_werks
     AND b~gjahr    =  p_bdatj
     AND b~perio    =  p_poper
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05'.
*    and c~loekz    = space. "Not deleted.

  LOOP AT it_ckmlmv003_temp.
    MOVE-CORRESPONDING it_ckmlmv003_temp TO it_ckmlmv003.
    CLEAR: "it_ckmlmv003-verid_nd,
           it_ckmlmv003-aufnr,
           it_ckmlmv003-meinh.
    COLLECT it_ckmlmv003. CLEAR it_ckmlmv003.
  ENDLOOP.

  DELETE it_ckmlmv003 WHERE out_menge EQ 0.

  SORT it_ckmlmv003 BY matnr verid_nd.

ENDFORM.                    " get_product_gr
*&---------------------------------------------------------------------*
*&      Form  GET_PP_GRQTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_DISPLAY_PP_GRQTY  text
*----------------------------------------------------------------------*
FORM get_pp_grqty CHANGING p_pp_grqty TYPE ckml_outmenge.

  CLEAR it_ckmlmv003.
  READ TABLE it_ckmlmv003 WITH KEY matnr = it_actual-artnr
                                   verid_nd = it_actual-verid
      BINARY SEARCH.

  CHECK : sy-subrc EQ 0,
          it_ckmlmv003-out_menge > 0.

  p_pp_grqty = it_ckmlmv003-out_menge.

*  CLEAR : P_PP_GRQTY.
*
*  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*       EXPORTING
*            INPUT                = IT_CKMLMV003-OUT_MENGE
*            UNIT_IN              = IT_CKMLMV003-MEINH
*            UNIT_OUT             = IT_CKMLMV003-MEINH
*       IMPORTING
*            OUTPUT               = P_PP_GRQTY
*       EXCEPTIONS
*            CONVERSION_NOT_FOUND = 1
*            DIVISION_BY_ZERO     = 2
*            INPUT_INVALID        = 3
*            OUTPUT_INVALID       = 4
*            OVERFLOW             = 5
*            TYPE_INVALID         = 6
*            UNITS_MISSING        = 7
*            UNIT_IN_NOT_FOUND    = 8
*            UNIT_OUT_NOT_FOUND   = 9.
*
*  IF P_PP_GRQTY = 0.
*    BREAK-POINT.
*  ENDIF.

ENDFORM.                    " pp_grqty
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_UNIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_unit.
  it_actual-pp_gr_qty    = it_actual-pp_gr_qty.
  it_actual-act_qty      = it_actual-manu_qty / it_actual-pp_gr_qty.
  it_actual-act_amt      = it_actual-manu_amt / it_actual-pp_gr_qty.
  it_actual-single_qty   = it_actual-single_qty / it_actual-pp_gr_qty.
  it_actual-manu_qty     = it_actual-manu_qty.
ENDFORM.                    " CALCULATE_UNIT
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_output.

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    gt_out-waers = 'USD'.

    IF p_diff EQ true.
      IF gt_out-dif_qty IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND gt_out.
  ENDLOOP.

ENDFORM.                    " GET_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_vendor_from_102
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_text_data.

  __cls : it_material,
          it_vnd_mat,
          it_vendor,
          it_mat_txt.

* get material list to gather information

  SORT it_ztcou103 BY kokrs	bdatj poper artnr.

  LOOP AT it_ztcou103.
    it_material-werks = it_ztcou103-werks.
    AT NEW artnr.
      it_material-matnr = it_ztcou103-artnr.
      it_material-mtype = 'P'.
      APPEND it_material.
    ENDAT.

    it_material-matnr = it_ztcou103-upgvc.
    it_material-mtype = 'U'.
    APPEND it_material.

    it_material-matnr = it_ztcou103-compn.
    it_material-mtype = 'M'.
    APPEND it_material.

    CLEAR it_material.
  ENDLOOP.

  SORT it_actual BY kokrs bdatj poper artnr.
  LOOP AT it_actual.
    it_material-werks = it_actual-par_werks.
    AT NEW artnr.
      it_material-matnr = it_actual-artnr. it_material-mtype = 'P'.
      APPEND it_material.
    ENDAT.
    it_material-matnr = it_actual-llv_matnr.
    it_material-mtype = 'M'.
    APPEND it_material.
    CLEAR it_material.
  ENDLOOP.

  SORT it_material.
  DELETE ADJACENT DUPLICATES FROM it_material COMPARING matnr.

  CHECK NOT it_material[] IS INITIAL.

  it_material-werks = 'P001'.
  MODIFY it_material TRANSPORTING werks WHERE werks IS initial.
  DELETE it_material WHERE matnr IS initial.

* 1 - step 1 ( vendor )
* get vendor from Costing Result table
*FIXME... UA; no entry in 102. U1
  SELECT matnr lifnr werks
    INTO TABLE it_vnd_mat
    FROM ztcou102
     FOR ALL ENTRIES IN it_material
   WHERE kokrs EQ  p_kokrs
     AND bdatj EQ  p_bdatj
     AND poper EQ  p_poper
     AND kalka LIKE 'U%'     "IN  S_KALKA
     AND matnr EQ  it_material-matnr.
*    AND WERKS EQ  IT_MATERIAL-WERKS.

* Hard Code {
  it_vnd_mat-matnr = 'R16N'.  "*R16N Engine
  it_vnd_mat-lifnr = 'SEF9'.
  APPEND it_vnd_mat.
* }

  PERFORM get_missed_vendor.

  LOOP AT it_vnd_mat.
    it_vendor-lifnr = it_vnd_mat-lifnr.
    it_vendor-werks = it_vnd_mat-werks.
    COLLECT it_vendor.
  ENDLOOP.

  SELECT lifnr name1
    INTO TABLE it_vndr_name
    FROM lfa1
     FOR ALL ENTRIES  IN it_vendor
   WHERE lifnr = it_vendor-lifnr .

* 2 - step 2 ( material )
* get material text

  PERFORM get_matl_info TABLES it_mat_txt
                               it_material
                         USING ' '.

*-get MIP information

  DATA: BEGIN OF lt_marc OCCURS 0,
          matnr LIKE marc-matnr,
          fevor LIKE marc-fevor,
        END OF lt_marc.

  SELECT matnr fevor INTO TABLE lt_marc
     FROM marc
     FOR ALL ENTRIES IN it_mat_txt
     WHERE matnr = it_mat_txt-matnr
       AND fevor <> space.

  SORT lt_marc BY matnr.

  DATA: l_idx LIKE sy-tabix.
  LOOP AT it_mat_txt.
    l_idx = sy-tabix.

    READ TABLE lt_marc WITH KEY matnr = it_mat_txt-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      it_mat_txt-fevor = lt_marc-fevor.  "production scheduler
    ELSE.
      CLEAR it_mat_txt-fevor.
    ENDIF.

    MODIFY it_mat_txt INDEX l_idx TRANSPORTING fevor.
  ENDLOOP.

*  DATA : $IT_MATERIAL LIKE IT_MATERIAL OCCURS 0 WITH HEADER LINE,
*         $IX LIKE SY-TABIX.
*
*  SORT IT_MAT_TXT BY MATNR.
*
*  LOOP AT IT_ROW_TAB WHERE UPGVC EQ SPACE.
*    $IX = SY-TABIX.
*    READ TABLE IT_MAT_TXT WITH KEY MATNR = IT_ROW_TAB-COMPN
*                               BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      IT_ROW_TAB-UPGVC = IT_MAT_TXT-MATKL.
*      MODIFY IT_ROW_TAB INDEX $IX TRANSPORTING UPGVC.
*      $IT_MATERIAL-MATNR = IT_ROW_TAB-UPGVC.
*      $IT_MATERIAL-WERKS = IT_ROW_TAB-WERKS.
*      APPEND $IT_MATERIAL.
*      CLEAR $IT_MATERIAL.
*    ENDIF.
*
*  ENDLOOP.
*  SORT $IT_MATERIAL.
*  DELETE ADJACENT DUPLICATES FROM $IT_MATERIAL.
*
*  PERFORM GET_MATL_INFO TABLES IT_MAT_TXT
*                               $IT_MATERIAL
*                        USING 'X'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  fill_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_text.

  SORT:   it_vnd_mat BY matnr,
          it_vndr_name BY lifnr,
          it_mat_txt BY matnr.

  DATA $ix LIKE sy-tabix.

  LOOP AT it_row_tab.
    $ix = sy-tabix.

    READ TABLE it_mat_txt WITH KEY matnr = it_row_tab-upgvc
                               BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_row_tab-upgvc_t = it_mat_txt-maktg.
    ENDIF.

    IF NOT it_row_tab-compn IS INITIAL.
      READ TABLE it_mat_txt WITH KEY matnr = it_row_tab-compn
                                 BINARY SEARCH.
    ELSE.
      READ TABLE it_mat_txt WITH KEY matnr = it_row_tab-compn_a
                                 BINARY SEARCH.
    ENDIF.

    IF sy-subrc EQ 0.
      it_row_tab-compn_t = it_mat_txt-maktg.
    ENDIF.

    READ TABLE it_vnd_mat WITH KEY matnr = it_row_tab-compn
                                 BINARY SEARCH.

    IF sy-subrc EQ 0.
      it_row_tab-lifnr = it_vnd_mat-lifnr.
      READ TABLE it_vndr_name WITH KEY lifnr = it_row_tab-lifnr
                                   BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_row_tab-vndr_name = it_vndr_name-name1.
      ENDIF.
    ENDIF.

    MODIFY it_row_tab INDEX $ix TRANSPORTING lifnr vndr_name
                                             upgvc_t compn_t.

  ENDLOOP.

ENDFORM.                    " fill_text
*&---------------------------------------------------------------------*
*&      Form  SET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_list.

  CHECK g_error IS INITIAL.

  PERFORM init_alv_parm.

***   Initialization fieldcatalog   ***
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM sort_build        USING gt_sort[].

  PERFORM alv_events_get    USING:  'P', 'U', 'T', ''.
  PERFORM alv_grid_display  TABLES  gt_out USING ''.

ENDFORM.                    " SET_LIST
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_alv_parm.

  __cls   :  gt_fieldcat, gt_sort, gt_events, gt_listheader,
             gt_sp_group.

  CLEAR   :  gs_layout.

  gs_layout-colwidth_optimize = 'X'.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos       TYPE i.

  __cls ft_fieldcat.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :

    'X'  'ARTNR'   'FSC'              18  'CHAR' '' '',
    'X'  'UPGVC'   'UPG-VC'           18  'CHAR' '' '',
    'X'  'UPGVC_T' 'UPG-VC'           18  'CHAR' '' '',
    'X'  'COMPN'   'Plan Comp.'       18  'CHAR' '' '',
    'X'  'COMPN_T' 'Description'      20  'CHAR' '' '',
    'X'  'COMPN_A' 'Act. Comp.'       18  'CHAR' '' '',
*    ' '  'KSTAR'   'Cost Elem.'       10  'CHAR' '' '',
    ' '  'LIFNR'   'Vendor'           10  'CHAR' '' '',
    ' '  'VNDR_NAME' 'Name'           20  'CHAR' '' '',

    ' '  'PLN_QTY' 'Plan Qty'         13  'QUAN' 'C40' '', "'MEEHT',
    ' '  'PLN_UNP' 'Plan U/P'         13  'CURR' 'C40' 'WAERS',
    ' '  'PLN_AMT' 'Plan Amt'         13  'CURR' 'C40' 'WAERS',

    ' '  'ACT_S_QTY' 'Actual Qty'     13  'QUAN' 'C50' '', "'MEEHT',
    ' '  'ACT_S_UNP' 'Actual U/P'     13  'CURR' 'C50' 'WAERS',
    ' '  'ACT_S_AMT' 'Actual Amt'     13  'CURR' 'C50' 'WAERS',

    ' '  'DIF_QTY' 'Diff. Qty'        13  'QUAN' 'C60' '', "'MEEHT',
    ' '  'DIF_AMT' 'Diff. Amt'        13  'CURR' 'C60' 'WAERS',

    ' '  'PV' 'PV'             13  'QUAN' 'C70' '', "'MEEHT',
    ' '  'QV' 'QV'             13  'QUAN' 'C70' '', "'MEEHT',
    ' '  'RV' 'RV'             13  'QUAN' 'C70' '', "'MEEHT',
    ' '  'OV' 'OV'             13  'QUAN' 'C70' '', "'MEEHT',

    ' '  'MANU_QTY' 'G.Act.Qty'     13  'QUAN' '' '', "'MEEHT',
    ' '  'MANU_AMT' 'G.Act.Amt'     13  'CURR' '' 'WAERS',
    ' '  'PP_GR_QTY' 'PP G/R '      13  'QUAN' '' '', "'MEEHT',
    ' '  'MEEHT'    'UOM'       3  'CHAR' '' '',

    ' '  'MISS_PLN' 'NP'        1  'CHAR' '' '',
    ' '  'MISS_ACT' 'NA'        1  'CHAR' '' '',
    ' '  'VERID'  'Ver'         4  'CHAR' '' ''.

  PERFORM change_fieldcat USING ft_fieldcat[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING    ft_sort TYPE slis_t_sortinfo_alv.

  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-comp      = &5.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
             'ARTNR'        ' ' 'X' 'X' 'X',
             'UPGVC'        ' ' 'X' 'X' 'X',
             'UPGVC_T'      ' ' 'X' 'X' 'X',
             'COMPN'        ' ' 'X' 'X' 'X',
             'COMPN_T'      ' ' 'X' 'X' 'X',
             'KSTAR'        ' ' 'X' 'X' 'X',
             'LIFNR'        ' ' 'X' 'X' 'X',
             'VNDR_NAME'    ' ' 'X' 'X' 'X'.

ENDFORM.                    " SORT_BUILD

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA l_text(60).
  DATA s_text(60).

  REFRESH gt_listheader.

  l_text = 'Actual Unit Cost.'.
  CONCATENATE p_bdatj '/' p_poper INTO s_text.
  PERFORM set_header_line USING:
          'P' 'H' ''                 l_text       '',
          'P' 'S' 'Controlling Area' p_kokrs      '',
          'P' 'S' 'Fisical Year/Preiod'     s_text      ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = gt_listheader.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM pf_status_set USING  ft_extab TYPE slis_t_extab.
  SET PF-STATUS '100' EXCLUDING ft_extab.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM user_command USING fp_ucomm LIKE sy-ucomm
                        fs       TYPE slis_selfield.
  CLEAR : g_error.
*  FIELD-SYMBOLS : <LS_LINE>, <MATNR>.
*
*  CASE FP_UCOMM.
*
*  ENDCASE.
*
*  PERFORM TOP_OF_PAGE.
*  CALL FUNCTION 'REUSE_ALV_GRID_LAYOUT_INFO_SET'
*       EXPORTING
*            IS_LAYOUT   = GS_LAYOUT
*            IT_FIELDCAT = GT_FIELDCAT
*            IT_SORT     = GT_SORT.
*
*  FS-REFRESH    = 'X'.
*  FS-ROW_STABLE = 'X'.
*  FS-COL_STABLE = 'X'.

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FT_FIELDCAT[]  text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM change_fieldcat USING    pt_fieldcat TYPE slis_t_fieldcat_alv.

  LOOP AT pt_fieldcat INTO gs_fieldcat.
    CASE gs_fieldcat-fieldname.

      WHEN 'PLN_QTY' OR 'ACT_S_QTY' OR 'DIF_QTY'.
        gs_fieldcat-no_zero = 'X'.
      WHEN  'PLN_UNP'   OR 'PLN_AMT'   OR 'ACT_S_UNP' OR 'ACT_S_AMT'
              OR 'DIF_AMT'.
        gs_fieldcat-no_zero = 'X'.
      WHEN  'PV'.
        gs_fieldcat-seltext_l = 'PV = ( AQ * AP ) - ( AQ * PP )'.
        gs_fieldcat-no_zero = 'X'.
      WHEN  'QV'.
        gs_fieldcat-seltext_l = 'QV = ( AQ - PQ ) * PP'.
        gs_fieldcat-no_zero = 'X'.
      WHEN  'RV'.
        gs_fieldcat-seltext_l = 'RV = ( AQ - PQ ) * PP'.
        gs_fieldcat-no_zero = 'X'.
      WHEN  'OV'.
        gs_fieldcat-seltext_l = 'OV = Diff. - PV - QV - RV'.
        gs_fieldcat-no_zero = 'X'.
    ENDCASE.

    MODIFY pt_fieldcat FROM gs_fieldcat.

  ENDLOOP.

ENDFORM.                    " CHANGE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ANALYSIS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM analysis_data.

* AQ = Actual Quantity
* AP = Actual Price
* PP = Planed Price
* PQ = Planed Quantity
* PV = ( AQ * AP ) - ( AQ * PP )
* QV = ( AQ - PQ ) * PP
* RV = ( AQ - PQ ) * PP
* OV = Diff. - PV - QV - RV

  DATA $ix LIKE sy-tabix.

  LOOP AT it_row_tab.
    $ix = sy-tabix.
    it_row_tab-pv =
      it_row_tab-act_amt - ( it_row_tab-act_qty * it_row_tab-pln_unp ).

    IF it_row_tab-miss_act EQ space AND it_row_tab-miss_pln EQ space.
      it_row_tab-qv =
       ( it_row_tab-act_qty - it_row_tab-pln_qty ) * it_row_tab-pln_unp.
    ELSEIF it_row_tab-miss_act = space OR it_row_tab-miss_pln = space.
      it_row_tab-rv =
       ( it_row_tab-act_qty - it_row_tab-pln_qty ) * it_row_tab-pln_unp.
    ENDIF.

    it_row_tab-ov = it_row_tab-dif_amt
                    - it_row_tab-pv - it_row_tab-qv - it_row_tab-rv.

    IF p_unit = 'X'.

      MODIFY it_row_tab INDEX $ix TRANSPORTING pv qv rv ov.

    ELSE.      "total value
      it_row_tab-pln_qty  = it_row_tab-pln_qty * it_row_tab-pp_gr_qty.
      it_row_tab-pln_amt  = it_row_tab-pln_amt * it_row_tab-pp_gr_qty.
      it_row_tab-act_qty  = it_row_tab-act_qty * it_row_tab-pp_gr_qty.
      it_row_tab-act_amt  = it_row_tab-act_amt * it_row_tab-pp_gr_qty.
      it_row_tab-dif_qty  = it_row_tab-dif_qty * it_row_tab-pp_gr_qty.
      it_row_tab-dif_amt  = it_row_tab-dif_amt * it_row_tab-pp_gr_qty.
      it_row_tab-pv       = it_row_tab-pv      * it_row_tab-pp_gr_qty.
      it_row_tab-qv       = it_row_tab-qv      * it_row_tab-pp_gr_qty.
      it_row_tab-rv       = it_row_tab-rv      * it_row_tab-pp_gr_qty.
      it_row_tab-ov       = it_row_tab-ov      * it_row_tab-pp_gr_qty.

      MODIFY it_row_tab INDEX $ix TRANSPORTING pln_qty pln_amt
                                               act_qty act_amt
                                               dif_qty dif_amt
                                               pv qv rv ov.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " ANALYSIS_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MATL_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MAT_TXT  text
*      -->P_IT_MATERIAL  text
*----------------------------------------------------------------------*
FORM get_matl_info TABLES   p_it_mat_txt STRUCTURE it_mat_txt
                            p_it_material STRUCTURE it_material
                   USING    p_append.

  IF p_append EQ true.
    SELECT a~matnr b~werks
           a~raube b~fevor
           a~mtart a~matkl a~bismt
           b~vspvb b~prctr c~maktg
      APPENDING TABLE p_it_mat_txt
      FROM mara AS a
      INNER JOIN marc AS b
         ON a~matnr = b~matnr
      INNER JOIN makt AS c
         ON c~matnr = a~matnr
        AND c~spras = sy-langu
      FOR ALL ENTRIES  IN p_it_material
     WHERE b~matnr    = p_it_material-matnr .
*     AND B~WERKS    = P_IT_MATERIAL-WERKS.
  ELSE.
    SELECT a~matnr b~werks
           a~raube b~fevor
           a~mtart a~matkl a~bismt
           b~vspvb b~prctr c~maktg
      INTO TABLE p_it_mat_txt
      FROM mara AS a
      INNER JOIN marc AS b
         ON a~matnr = b~matnr
      INNER JOIN makt AS c
         ON c~matnr = a~matnr
        AND c~spras = sy-langu
      FOR ALL ENTRIES  IN p_it_material
     WHERE b~matnr    = p_it_material-matnr .
*     AND B~WERKS    = P_IT_MATERIAL-WERKS.
  ENDIF.

ENDFORM.                    " GET_MATL_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_FOLLOWING_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FROM_MATNR  text
*      <--P_TO_MATNR  text
*----------------------------------------------------------------------*
FORM get_following_part USING    p_from_matnr
                        CHANGING p_to_matnr.
  CHECK p_fwng EQ true.
  SELECT SINGLE tmatnr INTO p_to_matnr
    FROM ztcou105
   WHERE kokrs = p_kokrs
     AND fmatnr = p_from_matnr.

ENDFORM.                    " GET_FOLLOWING_PART
*&---------------------------------------------------------------------*
*&      Form  it_row_tab_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM it_row_tab_with_actual USING p_act_compn.

  it_row_tab-compn_a = p_act_compn.

  it_row_tab-act_qty = it_actual-act_qty.
  it_row_tab-act_amt = it_actual-act_amt.

  it_row_tab-manu_qty = it_actual-manu_qty.
  it_row_tab-manu_amt = it_actual-manu_amt.

*  IT_ROW_TAB-GR_QTY = IT_ACTUAL-GR_QTY.
  it_row_tab-pp_gr_qty = it_actual-pp_gr_qty.
  it_row_tab-miss_act = false.

ENDFORM.                    " it_row_tab_success
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_value.
  CLEAR g_error.
  __cls it_color.

ENDFORM.                    " INITIALIZE_VALUE
*&---------------------------------------------------------------------*
*&      Form  get_missed_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_missed_vendor.

  DATA  it_vnd_mat_miss LIKE it_vnd_mat OCCURS 0 WITH HEADER LINE.

  SORT it_vnd_mat BY matnr.

  LOOP AT it_material WHERE mtype = 'M'.

    READ TABLE it_vnd_mat WITH KEY matnr = it_material-matnr
                          BINARY SEARCH.
    IF sy-subrc NE 0.
      it_vnd_mat_miss-matnr = it_material-matnr.
      it_vnd_mat_miss-werks = it_material-werks.
      APPEND it_vnd_mat_miss.
      CLEAR it_vnd_mat_miss.
    ENDIF.

  ENDLOOP.

  it_vnd_mat_miss-werks = 'P001'.
  MODIFY it_vnd_mat_miss TRANSPORTING werks WHERE werks IS initial.
  DELETE it_vnd_mat_miss WHERE matnr IS initial.


* determine vendor from source of supply
* {
  DATA:  lf_bqpim LIKE bqpim,
         lf_bqpex LIKE bqpex.

  lf_bqpim-bstyp = 'B'.          "Purchase requisition
  lf_bqpim-pstyp = '0'.          "Item category in PO
  lf_bqpim-vorga = 'B'.          "Transaction/event
  lf_bqpim-bqpra = '1'.          "Selection of source with price
  lf_bqpim-noaus = space.        "No box listing sources of supply
  lf_bqpim-liste = 'X'.                                     "553647
  lf_bqpim-beskz = 'F'.         "External procurement
  lf_bqpim-msgno = 'X'.         "keine Nachricht
  lf_bqpim-noquu = 'X'.         "Do not update quota arrangement
  lf_bqpim-novrt = 'X'.         "Do not search for outline agreement
  lf_bqpim-novrt_ord = 'X'.     "Do not search for order ???
  lf_bqpim-nomei = 'X'.         "Always use order unit from info
  lf_bqpim-matnl = 'X'.         "Do not read material
  lf_bqpim-noqum = 'X'.         "Selection after plan quotation
  CONCATENATE p_bdatj p_poper+1(2) '15' INTO lf_bqpim-nedat.

  LOOP AT it_vnd_mat_miss.
    lf_bqpim-werks = it_vnd_mat_miss-werks.
    lf_bqpim-matnr = it_vnd_mat_miss-matnr.

    CALL FUNCTION 'ME_SEARCH_SOURCE_OF_SUPPLY'
         EXPORTING
              comim = lf_bqpim
         IMPORTING
              comex = lf_bqpex.

    IF sy-subrc = 0.
      it_vnd_mat-matnr = it_vnd_mat_miss-matnr.
      it_vnd_mat-lifnr = lf_bqpex-flief.
      it_vnd_mat-werks = it_vnd_mat_miss-werks.
      APPEND it_vnd_mat.
    ENDIF.
  ENDLOOP.
* }


ENDFORM.                    " get_missed_vendor
*&---------------------------------------------------------------------*
*&      Form  actual_collect
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actual_collect.

  DATA $it_actual LIKE it_actual OCCURS 0 WITH HEADER LINE.
*// collect //                   {

  LOOP AT it_actual.
    $it_actual = it_actual.
    COLLECT $it_actual.
  ENDLOOP.
  __cls it_actual.
  it_actual[] = $it_actual[].
*                                 }

ENDFORM.                    " actual_collect
