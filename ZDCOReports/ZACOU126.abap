*----------------------------------------------------------------------
* Program ID        : ZACOU126
* Title             : [CO] Comparison report for unit costing
* Created on        : 06/29/2007
* Created by        : I.G.MOON
* Specifications By : Andy Choi
* Description       : Comparison report between plan and actual cost
*----------------------------------------------------------------------
* Date        Developer  Request    Description
* 10/11/2010  Valerian   UD1K949919 Add 'Material Level II', 'Material
*                                   Level I', Description and Category
*                                   to Rep.Layout
*----------------------------------------------------------------------

REPORT ZACOU126 MESSAGE-ID ZMCO.
INCLUDE : Z_MOON_ALV_TOP,
          Z_MOON_ALV_FNC,
          ZACOU126_TOP.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.

PARAMETERS     : P_KOKRS LIKE ZTCO_SHOP_SUM-KOKRS OBLIGATORY
                 MEMORY ID CAC,
                 P_BDATJ LIKE ZTCO_SHOP_SUM-BDATJ OBLIGATORY
                 MEMORY ID BDTJ,
                 P_POPER LIKE ZTCO_SHOP_SUM-POPER OBLIGATORY
                 MEMORY ID POPR.
PARAMETER      : P_VER TYPE ZTCOU102-VER.
SELECTION-SCREEN END OF BLOCK BL1.

SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS S_KALKA FOR KEKO-KALKA NO INTERVALS
                           OBLIGATORY MEMORY ID KKA.
SELECTION-SCREEN END OF BLOCK BL2.


SELECTION-SCREEN BEGIN OF BLOCK BL3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS :
                 S_ARTNR FOR ZTCO_SHOP_SUM-ARTNR  MEMORY ID  MAT,
                 S_COMPN FOR ZTCOU103-COMPN,
                 S_KSTAR FOR ZTCO_SHOP_SUM-KSTAR MEMORY ID KA3,
                 S_ELEMT FOR ZTCO_SHOP_CC-ELEMT,
                 S_SHOP  FOR ZTCO_SHOP_SUM-SHOP,
                 S_MTART FOR MARA-MTART.
SELECTION-SCREEN END OF BLOCK BL3.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-010.
PARAMETER P_VARI TYPE SLIS_VARI.
SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE TEXT-011.
PARAMETER: P_FWNG AS CHECKBOX DEFAULT 'X',
           P_UNIT AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B5.

PARAMETER: p_diff AS CHECKBOX DEFAULT 'X'.

INITIALIZATION.
  G_REPID = SY-REPID.

  __CLS S_ELEMT.

  S_ELEMT-SIGN = 'I'.
  S_ELEMT-OPTION = 'BT'.
  S_ELEMT-LOW = '10'.
  S_ELEMT-LOW = '10'.
  S_ELEMT-HIGH = '50'.

  APPEND S_ELEMT.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*  PERFORM GET_DEFAULT_VARIANT_GET CHANGING P_LISTV.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_KALKA-LOW.
  PERFORM POPUP_KALKA USING S_KALKA-LOW 'S_KALKA-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM ALV_VARIANT_F4 CHANGING P_VARI.

*--------------------------------------------------------------------*
START-OF-SELECTION.
*--------------------------------------------------------------------*

  PERFORM INITIALIZE_VALUE.

  PERFORM READ_TKA01.
  CHECK G_ERROR EQ SPACE.

  __PROCESS 'Read Plan...' '10'.
  PERFORM GET_PLAN.       " Read data from ZTCOU103(Cost Roll-Up)

  __PROCESS 'Read Actual...' '20'.
  PERFORM GET_ACTUAL.

  __PROCESS 'Merge data...' '40'.
  PERFORM GET_MERGED.     " Merge the data plan and atual

  __PROCESS 'Fill text...' '50'.
  PERFORM FILL_TEXT.      " Fill the description for material,vendor,etc

  __PROCESS 'Analysis Data...' '60'.
  PERFORM ANALYSIS_DATA.  " Fill the analysis data

  __PROCESS 'Prepare screen...' '70'.
  PERFORM GET_OUTPUT.     " prepare the data to ALV display

*--------------------------------------------------------------------*
END-OF-SELECTION.
  CHECK G_ERROR EQ SPACE .
  PERFORM SET_LIST .

*--------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ACTUAL.
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

  LOOP AT it_ztcou103.
    $ix = sy-tabix.
    READ TABLE it_ckmlmv003 WITH KEY matnr = it_ztcou103-artnr
                                     verid_nd = it_ztcou103-verid
        BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_ztcou103 INDEX $ix.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM SHOW_PROGRESS USING    PF_TEXT
                            VALUE(PF_VAL).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PF_VAL
            TEXT       = PF_TEXT.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  READ_TKA01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_TKA01.

  CLEAR TKA01.

  SELECT SINGLE * FROM TKA01
                 WHERE KOKRS = P_KOKRS.
  IF SY-SUBRC <> 0.
    MESSAGE S038 WITH P_KOKRS.
    G_ERROR = TRUE.
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
FORM GET_PLAN.
  __CLS : IT_ZTCOU103.


  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCOU103
     FROM ZTCOU103 AS A
     INNER JOIN MARA AS B
     ON B~MATNR EQ A~COMPN
    WHERE KOKRS       EQ P_KOKRS
      AND BDATJ       EQ P_BDATJ
      AND KALKA       IN S_KALKA
      AND POPER       EQ P_POPER
      AND ARTNR       IN S_ARTNR
*      AND TYPPS       EQ 'M'
      AND KSTAR       IN S_KSTAR
      AND COMPN       IN S_COMPN
      AND VER         EQ P_VER
      AND B~MTART     IN S_MTART.

  DATA: $STRLEN TYPE I.
  DATA: $IX LIKE SY-TABIX.


* by ig.moon 11/17/2009 {
*  LOOP AT IT_ZTCOU103.
*    $IX = SY-TABIX.
*    $STRLEN = STRLEN( IT_ZTCOU103-COMPN ).
*    IF $STRLEN > 11.
*      CASE $STRLEN.
*        WHEN 12 OR 13.
*          IT_COLOR-MATNR1 = IT_ZTCOU103-COMPN.
*          IT_ZTCOU103-COMPN = IT_ZTCOU103-COMPN+(10).
*          IT_COLOR-MATNR2 = IT_ZTCOU103-COMPN.
*          COLLECT IT_COLOR.
*        WHEN 14 OR 15.
*          IT_COLOR-MATNR1 = IT_ZTCOU103-COMPN.
*          IT_ZTCOU103-COMPN = IT_ZTCOU103-COMPN+(12).
*          IT_COLOR-MATNR2 = IT_ZTCOU103-COMPN.
*          COLLECT IT_COLOR.
*      ENDCASE.
*      MODIFY IT_ZTCOU103 INDEX $IX TRANSPORTING COMPN.
*    ENDIF.
*  ENDLOOP.

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
* }
  DATA $IT_ZTCOU103 LIKE IT_ZTCOU103 OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_ZTCOU103.
    $IT_ZTCOU103 = IT_ZTCOU103.
    COLLECT $IT_ZTCOU103.
  ENDLOOP.

  __CLS IT_ZTCOU103.
  IT_ZTCOU103[] = $IT_ZTCOU103[].

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
FORM GET_PRODUCT_GR.

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
FORM GET_PP_GRQTY CHANGING P_PP_GRQTY TYPE CKML_OUTMENGE.

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
FORM CALCULATE_UNIT.
  IT_ACTUAL-PP_GR_QTY    = IT_ACTUAL-PP_GR_QTY.
  IT_ACTUAL-ACT_QTY      = IT_ACTUAL-MANU_QTY / IT_ACTUAL-PP_GR_QTY.
  IT_ACTUAL-ACT_AMT      = IT_ACTUAL-MANU_AMT / IT_ACTUAL-PP_GR_QTY.
  IT_ACTUAL-SINGLE_QTY   = IT_ACTUAL-SINGLE_QTY / IT_ACTUAL-PP_GR_QTY.
  IT_ACTUAL-MANU_QTY     = IT_ACTUAL-MANU_QTY.
ENDFORM.                    " CALCULATE_UNIT
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_OUTPUT.

  __cls gt_out.

  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    gt_out-waers = 'USD'.

    IF p_diff EQ true.
      IF gt_out-dif_qty IS INITIAL.
        CONTINUE.
      ENDIF.
    ENDIF.

    perform get_mat_level.                                  "UD1K949919

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
FORM GET_TEXT_DATA.

  __CLS : IT_MATERIAL,
          IT_VND_MAT,
          IT_VENDOR,
          IT_MAT_TXT.

* get material list to gather information

  SORT IT_ZTCOU103 BY KOKRS	BDATJ POPER ARTNR.

  LOOP AT IT_ZTCOU103.
    IT_MATERIAL-WERKS = IT_ZTCOU103-WERKS.
    AT NEW ARTNR.
      IT_MATERIAL-MATNR = IT_ZTCOU103-ARTNR.
      IT_MATERIAL-MTYPE = 'P'.
      APPEND IT_MATERIAL.
    ENDAT.

    IT_MATERIAL-MATNR = IT_ZTCOU103-UPGVC.
    IT_MATERIAL-MTYPE = 'U'.
    APPEND IT_MATERIAL.

    IT_MATERIAL-MATNR = IT_ZTCOU103-COMPN.
    IT_MATERIAL-MTYPE = 'M'.
    APPEND IT_MATERIAL.

    CLEAR IT_MATERIAL.
  ENDLOOP.

  SORT IT_ACTUAL BY KOKRS BDATJ POPER ARTNR.
  LOOP AT IT_ACTUAL.
    IT_MATERIAL-WERKS = IT_ACTUAL-PAR_WERKS.
    AT NEW ARTNR.
      IT_MATERIAL-MATNR = IT_ACTUAL-ARTNR. IT_MATERIAL-MTYPE = 'P'.
      APPEND IT_MATERIAL.
    ENDAT.
    IT_MATERIAL-MATNR = IT_ACTUAL-LLV_MATNR.
    IT_MATERIAL-MTYPE = 'M'.
    APPEND IT_MATERIAL.
    CLEAR IT_MATERIAL.
  ENDLOOP.

  SORT IT_MATERIAL.
  DELETE ADJACENT DUPLICATES FROM IT_MATERIAL COMPARING MATNR.

  CHECK NOT IT_MATERIAL[] IS INITIAL.

  IT_MATERIAL-WERKS = 'P001'.
  MODIFY IT_MATERIAL TRANSPORTING WERKS WHERE WERKS IS INITIAL.
  DELETE IT_MATERIAL WHERE MATNR IS INITIAL.

* 1 - step 1 ( vendor )
* get vendor from Costing Result table
*FIXME... UA; no entry in 102. U1
  SELECT MATNR LIFNR WERKS
    INTO TABLE IT_VND_MAT
    FROM ZTCOU102
     FOR ALL ENTRIES IN IT_MATERIAL
   WHERE KOKRS EQ  P_KOKRS
     AND BDATJ EQ  P_BDATJ
     AND POPER EQ  P_POPER
     AND KALKA LIKE 'U%'     "IN  S_KALKA
     AND MATNR EQ  IT_MATERIAL-MATNR.
*    AND WERKS EQ  IT_MATERIAL-WERKS.

* Hard Code {
  IT_VND_MAT-MATNR = 'R16N'.  "*R16N Engine
  IT_VND_MAT-LIFNR = 'SEF9'.
  APPEND IT_VND_MAT.
* }

  PERFORM GET_MISSED_VENDOR.

  LOOP AT IT_VND_MAT.
    IT_VENDOR-LIFNR = IT_VND_MAT-LIFNR.
    IT_VENDOR-WERKS = IT_VND_MAT-WERKS.
    COLLECT IT_VENDOR.
  ENDLOOP.

  SELECT LIFNR NAME1
    INTO TABLE IT_VNDR_NAME
    FROM LFA1
     FOR ALL ENTRIES  IN IT_VENDOR
   WHERE LIFNR = IT_VENDOR-LIFNR .

* 2 - step 2 ( material )
* get material text

  PERFORM GET_MATL_INFO TABLES IT_MAT_TXT
                               IT_MATERIAL
                         USING ' '.

*-get MIP information

  DATA: BEGIN OF LT_MARC OCCURS 0,
          MATNR LIKE MARC-MATNR,
          FEVOR LIKE MARC-FEVOR,
        END OF LT_MARC.

  SELECT MATNR FEVOR INTO TABLE LT_MARC
     FROM MARC
     FOR ALL ENTRIES IN IT_MAT_TXT
     WHERE MATNR = IT_MAT_TXT-MATNR
       AND FEVOR <> SPACE.

  SORT LT_MARC BY MATNR.

  DATA: L_IDX LIKE SY-TABIX.
  LOOP AT IT_MAT_TXT.
    L_IDX = SY-TABIX.

    READ TABLE LT_MARC WITH KEY MATNR = IT_MAT_TXT-MATNR BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_MAT_TXT-FEVOR = LT_MARC-FEVOR.  "production scheduler
    ELSE.
      CLEAR IT_MAT_TXT-FEVOR.
    ENDIF.

    MODIFY IT_MAT_TXT INDEX L_IDX TRANSPORTING FEVOR.
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
FORM FILL_TEXT.

  SORT:   IT_VND_MAT BY MATNR,
          IT_VNDR_NAME BY LIFNR,
          IT_MAT_TXT BY MATNR.

  DATA $IX LIKE SY-TABIX.

  LOOP AT IT_ROW_TAB.
    $IX = SY-TABIX.

    READ TABLE IT_MAT_TXT WITH KEY MATNR = IT_ROW_TAB-UPGVC
                               BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_ROW_TAB-UPGVC_T = IT_MAT_TXT-MAKTG.
    ENDIF.

    IF NOT IT_ROW_TAB-COMPN IS INITIAL.
      READ TABLE IT_MAT_TXT WITH KEY MATNR = IT_ROW_TAB-COMPN
                                 BINARY SEARCH.
    ELSE.
      READ TABLE IT_MAT_TXT WITH KEY MATNR = IT_ROW_TAB-COMPN_A
                                 BINARY SEARCH.
    ENDIF.

    IF SY-SUBRC EQ 0.
      IT_ROW_TAB-COMPN_T = IT_MAT_TXT-MAKTG.
    ENDIF.

    READ TABLE IT_VND_MAT WITH KEY MATNR = IT_ROW_TAB-COMPN
                                 BINARY SEARCH.

    IF SY-SUBRC EQ 0.
      IT_ROW_TAB-LIFNR = IT_VND_MAT-LIFNR.
      READ TABLE IT_VNDR_NAME WITH KEY LIFNR = IT_ROW_TAB-LIFNR
                                   BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        IT_ROW_TAB-VNDR_NAME = IT_VNDR_NAME-NAME1.
      ENDIF.
    ENDIF.

    MODIFY IT_ROW_TAB INDEX $IX TRANSPORTING LIFNR VNDR_NAME
                                             UPGVC_T COMPN_T.

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
FORM SET_LIST.

  CHECK G_ERROR IS INITIAL.

  PERFORM INIT_ALV_PARM.

***   Initialization fieldcatalog   ***
  PERFORM FIELDCAT_INIT     USING GT_FIELDCAT[].
  PERFORM SORT_BUILD        USING GT_SORT[].

  PERFORM ALV_EVENTS_GET    USING:  'P', 'U', 'T', ''.
  PERFORM ALV_GRID_DISPLAY  TABLES  GT_OUT USING ''.

ENDFORM.                    " SET_LIST
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV_PARM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_ALV_PARM.

  __CLS   :  GT_FIELDCAT, GT_SORT, GT_EVENTS, GT_LISTHEADER,
             GT_SP_GROUP.

  CLEAR   :  GS_LAYOUT.

  GS_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

*   Set variant
  GV_REPID = GS_VARIANT-REPORT = SY-REPID.
  GS_VARIANT-VARIANT = P_VARI.

ENDFORM.                    " INIT_ALV_PARM
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING FT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV .

  DATA: L_POS       TYPE I.

  __CLS FT_FIELDCAT.

  DEFINE __CATALOG.
    L_POS = L_POS + 1.
    CLEAR GS_FIELDCAT.
    GS_FIELDCAT-COL_POS       = L_POS.
    GS_FIELDCAT-KEY           = &1.
    GS_FIELDCAT-FIELDNAME     = &2.
    GS_FIELDCAT-SELTEXT_M     = &3.        " Column heading
    GS_FIELDCAT-OUTPUTLEN     = &4.        " Column width
    GS_FIELDCAT-DATATYPE      = &5.        " Data type
    GS_FIELDCAT-EMPHASIZE     = &6.
    GS_FIELDCAT-CFIELDNAME    = &7.
    APPEND GS_FIELDCAT TO  FT_FIELDCAT.
  END-OF-DEFINITION.

  __CATALOG :

    'X'  'ARTNR'   'FSC'              18  'CHAR' '' '',
    'X'  'UPGVC'   'UPG-VC'           18  'CHAR' '' '',
    'X'  'UPGVC_T' 'UPG-VC'           18  'CHAR' '' '',
    'X'  'COMPN'   'Plan Comp.'       18  'CHAR' '' '',
    'X'  'COMPN_T' 'Description'      20  'CHAR' '' '',
    'X'  'COMPN_A' 'Act. Comp.'       18  'CHAR' '' '',
    ' '  'KSTAR'   'Cost Elem.'       10  'CHAR' '' '',
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
    ' '  'VERID'  'Ver'         4  'CHAR' '' '',
    ' '  'MATNR2' 'Level II'   18  'CHAR' '' '',            "UD1K949919
    ' '  'MATNR1' 'Level I'    18  'CHAR' '' '',            "UD1K949919
    ' '  'MAKTX'  'Mat.Description' 40 'CHAR' '' '',        "UD1K949919
    ' '  'ZCATX'  'Categ.for MCVS'   8 'CHAR' '' ''.        "UD1K949919

  PERFORM CHANGE_FIELDCAT USING FT_FIELDCAT[] .

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM SORT_BUILD USING    FT_SORT TYPE SLIS_T_SORTINFO_ALV.

  DEFINE SORT_TAB.
    CLEAR GS_SORT.
    GS_SORT-FIELDNAME = &1.
    GS_SORT-SPOS      = &2.
    GS_SORT-UP        = &3.
    GS_SORT-GROUP     = &4.
    GS_SORT-COMP      = &5.
    APPEND GS_SORT TO FT_SORT.
  END-OF-DEFINITION.

  SORT_TAB :
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
FORM TOP_OF_PAGE.
  DATA L_TEXT(60).
  DATA S_TEXT(60).

  REFRESH GT_LISTHEADER.

  L_TEXT = 'Actual Unit Cost.'.
  CONCATENATE P_BDATJ '/' P_POPER INTO S_TEXT.
  PERFORM SET_HEADER_LINE USING:
          'P' 'H' ''                 L_TEXT       '',
          'P' 'S' 'Controlling Area' P_KOKRS      '',
          'P' 'S' 'Fisical Year/Preiod'     S_TEXT      ''.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = GT_LISTHEADER.

ENDFORM.                    "top_of_page
*---------------------------------------------------------------------*
*       FORM PF_STATUS_SET
*---------------------------------------------------------------------*
FORM PF_STATUS_SET USING  FT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS '100' EXCLUDING FT_EXTAB.
ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING FP_UCOMM LIKE SY-UCOMM
                        FS       TYPE SLIS_SELFIELD.
  CLEAR : G_ERROR.
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
FORM CHANGE_FIELDCAT USING    PT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  LOOP AT PT_FIELDCAT INTO GS_FIELDCAT.
    CASE GS_FIELDCAT-FIELDNAME.

      WHEN 'PLN_QTY' OR 'ACT_S_QTY' OR 'DIF_QTY'.
        GS_FIELDCAT-NO_ZERO = 'X'.
      WHEN  'PLN_UNP'   OR 'PLN_AMT'   OR 'ACT_S_UNP' OR 'ACT_S_AMT'
              OR 'DIF_AMT'.
        GS_FIELDCAT-NO_ZERO = 'X'.
      WHEN  'PV'.
        GS_FIELDCAT-SELTEXT_L = 'PV = ( AQ * AP ) - ( AQ * PP )'.
        GS_FIELDCAT-NO_ZERO = 'X'.
      WHEN  'QV'.
        GS_FIELDCAT-SELTEXT_L = 'QV = ( AQ - PQ ) * PP'.
        GS_FIELDCAT-NO_ZERO = 'X'.
      WHEN  'RV'.
        GS_FIELDCAT-SELTEXT_L = 'RV = ( AQ - PQ ) * PP'.
        GS_FIELDCAT-NO_ZERO = 'X'.
      WHEN  'OV'.
        GS_FIELDCAT-SELTEXT_L = 'OV = Diff. - PV - QV - RV'.
        GS_FIELDCAT-NO_ZERO = 'X'.
    ENDCASE.

    MODIFY PT_FIELDCAT FROM GS_FIELDCAT.

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
FORM ANALYSIS_DATA.

* AQ = Actual Quantity
* AP = Actual Price
* PP = Planed Price
* PQ = Planed Quantity
* PV = ( AQ * AP ) - ( AQ * PP )
* QV = ( AQ - PQ ) * PP
* RV = ( AQ - PQ ) * PP
* OV = Diff. - PV - QV - RV

  DATA $IX LIKE SY-TABIX.

  LOOP AT IT_ROW_TAB.
    $IX = SY-TABIX.
    IT_ROW_TAB-PV =
      IT_ROW_TAB-ACT_AMT - ( IT_ROW_TAB-ACT_QTY * IT_ROW_TAB-PLN_UNP ).

    IF IT_ROW_TAB-MISS_ACT EQ SPACE AND IT_ROW_TAB-MISS_PLN EQ SPACE.
      IT_ROW_TAB-QV =
       ( IT_ROW_TAB-ACT_QTY - IT_ROW_TAB-PLN_QTY ) * IT_ROW_TAB-PLN_UNP.
    ELSEIF IT_ROW_TAB-MISS_ACT = SPACE OR IT_ROW_TAB-MISS_PLN = SPACE.
      IT_ROW_TAB-RV =
       ( IT_ROW_TAB-ACT_QTY - IT_ROW_TAB-PLN_QTY ) * IT_ROW_TAB-PLN_UNP.
    ENDIF.

    IT_ROW_TAB-OV = IT_ROW_TAB-DIF_AMT
                    - IT_ROW_TAB-PV - IT_ROW_TAB-QV - IT_ROW_TAB-RV.

    IF P_UNIT = 'X'.

      MODIFY IT_ROW_TAB INDEX $IX TRANSPORTING PV QV RV OV.

    ELSE.      "total value
      IT_ROW_TAB-PLN_QTY  = IT_ROW_TAB-PLN_QTY * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-PLN_AMT  = IT_ROW_TAB-PLN_AMT * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-ACT_QTY  = IT_ROW_TAB-ACT_QTY * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-ACT_AMT  = IT_ROW_TAB-ACT_AMT * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-DIF_QTY  = IT_ROW_TAB-DIF_QTY * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-DIF_AMT  = IT_ROW_TAB-DIF_AMT * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-PV       = IT_ROW_TAB-PV      * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-QV       = IT_ROW_TAB-QV      * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-RV       = IT_ROW_TAB-RV      * IT_ROW_TAB-PP_GR_QTY.
      IT_ROW_TAB-OV       = IT_ROW_TAB-OV      * IT_ROW_TAB-PP_GR_QTY.
      MODIFY IT_ROW_TAB INDEX $IX TRANSPORTING PLN_QTY PLN_AMT
                                               ACT_QTY ACT_AMT
                                               DIF_QTY DIF_AMT
                                               PV QV RV OV.
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
FORM GET_MATL_INFO TABLES   P_IT_MAT_TXT STRUCTURE IT_MAT_TXT
                            P_IT_MATERIAL STRUCTURE IT_MATERIAL
                   USING    P_APPEND.

  IF P_APPEND EQ TRUE.
    SELECT A~MATNR B~WERKS
           A~RAUBE B~FEVOR
           A~MTART A~MATKL A~BISMT
           B~VSPVB B~PRCTR C~MAKTG
      APPENDING TABLE P_IT_MAT_TXT
      FROM MARA AS A
      INNER JOIN MARC AS B
         ON A~MATNR = B~MATNR
      INNER JOIN MAKT AS C
         ON C~MATNR = A~MATNR
        AND C~SPRAS = SY-LANGU
      FOR ALL ENTRIES  IN P_IT_MATERIAL
     WHERE B~MATNR    = P_IT_MATERIAL-MATNR .
*     AND B~WERKS    = P_IT_MATERIAL-WERKS.
  ELSE.
    SELECT A~MATNR B~WERKS
           A~RAUBE B~FEVOR
           A~MTART A~MATKL A~BISMT
           B~VSPVB B~PRCTR C~MAKTG
      INTO TABLE P_IT_MAT_TXT
      FROM MARA AS A
      INNER JOIN MARC AS B
         ON A~MATNR = B~MATNR
      INNER JOIN MAKT AS C
         ON C~MATNR = A~MATNR
        AND C~SPRAS = SY-LANGU
      FOR ALL ENTRIES  IN P_IT_MATERIAL
     WHERE B~MATNR    = P_IT_MATERIAL-MATNR .
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
FORM GET_FOLLOWING_PART USING    P_FROM_MATNR
                        CHANGING P_TO_MATNR.
  CHECK P_FWNG EQ TRUE.
  SELECT SINGLE TMATNR INTO P_TO_MATNR
    FROM ZTCOU105
   WHERE KOKRS = P_KOKRS
     AND FMATNR = P_FROM_MATNR.

ENDFORM.                    " GET_FOLLOWING_PART
*&---------------------------------------------------------------------*
*&      Form  it_row_tab_success
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM IT_ROW_TAB_WITH_ACTUAL USING P_ACT_COMPN.

  IT_ROW_TAB-COMPN_A = P_ACT_COMPN.

  IT_ROW_TAB-ACT_QTY = IT_ACTUAL-ACT_QTY.
  IT_ROW_TAB-ACT_AMT = IT_ACTUAL-ACT_AMT.

  IT_ROW_TAB-MANU_QTY = IT_ACTUAL-MANU_QTY.
  IT_ROW_TAB-MANU_AMT = IT_ACTUAL-MANU_AMT.

*  IT_ROW_TAB-GR_QTY = IT_ACTUAL-GR_QTY.
  IT_ROW_TAB-PP_GR_QTY = IT_ACTUAL-PP_GR_QTY.
  IT_ROW_TAB-MISS_ACT = FALSE.

ENDFORM.                    " it_row_tab_success
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZE_VALUE.
  CLEAR G_ERROR.
  __CLS IT_COLOR.

ENDFORM.                    " INITIALIZE_VALUE
*&---------------------------------------------------------------------*
*&      Form  get_missed_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MISSED_VENDOR.

  DATA  IT_VND_MAT_MISS LIKE IT_VND_MAT OCCURS 0 WITH HEADER LINE.

  SORT IT_VND_MAT BY MATNR.

  LOOP AT IT_MATERIAL WHERE MTYPE = 'M'.

    READ TABLE IT_VND_MAT WITH KEY MATNR = IT_MATERIAL-MATNR
                          BINARY SEARCH.
    IF SY-SUBRC NE 0.
      IT_VND_MAT_MISS-MATNR = IT_MATERIAL-MATNR.
      IT_VND_MAT_MISS-WERKS = IT_MATERIAL-WERKS.
      APPEND IT_VND_MAT_MISS.
      CLEAR IT_VND_MAT_MISS.
    ENDIF.

  ENDLOOP.

  IT_VND_MAT_MISS-WERKS = 'P001'.
  MODIFY IT_VND_MAT_MISS TRANSPORTING WERKS WHERE WERKS IS INITIAL.
  DELETE IT_VND_MAT_MISS WHERE MATNR IS INITIAL.


* determine vendor from source of supply
* {
  DATA:  LF_BQPIM LIKE BQPIM,
         LF_BQPEX LIKE BQPEX.

  LF_BQPIM-BSTYP = 'B'.          "Purchase requisition
  LF_BQPIM-PSTYP = '0'.          "Item category in PO
  LF_BQPIM-VORGA = 'B'.          "Transaction/event
  LF_BQPIM-BQPRA = '1'.          "Selection of source with price
  LF_BQPIM-NOAUS = SPACE.        "No box listing sources of supply
  LF_BQPIM-LISTE = 'X'.                                     "553647
  LF_BQPIM-BESKZ = 'F'.         "External procurement
  LF_BQPIM-MSGNO = 'X'.         "keine Nachricht
  LF_BQPIM-NOQUU = 'X'.         "Do not update quota arrangement
  LF_BQPIM-NOVRT = 'X'.         "Do not search for outline agreement
  LF_BQPIM-NOVRT_ORD = 'X'.     "Do not search for order ???
  LF_BQPIM-NOMEI = 'X'.         "Always use order unit from info
  LF_BQPIM-MATNL = 'X'.         "Do not read material
  LF_BQPIM-NOQUM = 'X'.         "Selection after plan quotation
  CONCATENATE P_BDATJ P_POPER+1(2) '15' INTO LF_BQPIM-NEDAT.

  LOOP AT IT_VND_MAT_MISS.
    LF_BQPIM-WERKS = IT_VND_MAT_MISS-WERKS.
    LF_BQPIM-MATNR = IT_VND_MAT_MISS-MATNR.

    CALL FUNCTION 'ME_SEARCH_SOURCE_OF_SUPPLY'
         EXPORTING
              COMIM = LF_BQPIM
         IMPORTING
              COMEX = LF_BQPEX.

    IF SY-SUBRC = 0.
      IT_VND_MAT-MATNR = IT_VND_MAT_MISS-MATNR.
      IT_VND_MAT-LIFNR = LF_BQPEX-FLIEF.
      IT_VND_MAT-WERKS = IT_VND_MAT_MISS-WERKS.
      APPEND IT_VND_MAT.
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
FORM ACTUAL_COLLECT.

  DATA $IT_ACTUAL LIKE IT_ACTUAL OCCURS 0 WITH HEADER LINE.
*// collect //                   {

  LOOP AT IT_ACTUAL.
    $IT_ACTUAL = IT_ACTUAL.
    COLLECT $IT_ACTUAL.
  ENDLOOP.
  __CLS IT_ACTUAL.
  IT_ACTUAL[] = $IT_ACTUAL[].
*                                 }

ENDFORM.                    " actual_collect
*&---------------------------------------------------------------------*
*&      Form  get_mat_level
*&---------------------------------------------------------------------*
*       Get material level
*----------------------------------------------------------------------*
form get_mat_level.
  select single ZTCO_UPG~MATNR ZTCO_UPG~WRKTS
                ZTCO_UPG~MAKTX ZTCO_UPG~ZCATX
    into (gt_out-matnr2, gt_out-matnr1, gt_out-maktx, gt_out-zcatx)
    from mara join ztco_upg
                on mara~wrkst = ztco_upg~matnr
   where mara~matnr = gt_out-compn_a.
endform.                    " get_mat_level
