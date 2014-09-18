*----------------------------------------------------------------------
* Program ID        : ZACOU106
* Title             : [CO] Calculate Variances
* Created on        : 08/31/2006
* Created by        : Michelle Jeong
* Specifications By : Andy Choi
* Description       : Calculate variances program
*                     using by table ZTCOU104, ZTCOU102, ZTCOU105
*                                    ZTCOU116
*                     save result to table ZTCOU106.
*----------------------------------------------------------------------
*FIXME - ANDY
* - valuation class should be used...for historical data

REPORT zacou106 NO STANDARD PAGE HEADING MESSAGE-ID zmco.

INCLUDE zacoui00.
INCLUDE zacou106_top.

DEFINE __process.
  perform show_message using &1 &2.
END-OF-DEFINITION.
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
*----------------------------------------------------------------------
* Selet-Options & Parameters
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
PARAMETERS: p_kokrs LIKE keko-kokrs OBLIGATORY MEMORY ID cac,
            p_year  LIKE keko-bdatj OBLIGATORY MEMORY ID bdtj,
            p_poper LIKE ztcou106-poper OBLIGATORY MEMORY ID popr,
            p_kalka LIKE ztcou104-kalka OBLIGATORY MEMORY ID kka.
PARAMETERS: p_abp AS CHECKBOX.
*PARAMETERS: p_act AS CHECKBOX.
"(1) type c AS LISTBOX VISIBLE LENGTH 25 OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b0.
SELECT-OPTIONS: s_id FOR ztcou104-id MATCHCODE OBJECT zid,
                s_matnr FOR ztcou106-compn.

PARAMETERS: p_ref NO-DISPLAY. "  AS CHECKBOX.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.

* for combobox
TYPE-POOLS: vrm.
DATA: gt_mod TYPE vrm_values,
      gw_mod LIKE LINE OF gt_mod.

DATA:   wa_tab(72) TYPE c,
        atab LIKE STANDARD TABLE OF wa_tab WITH NON-UNIQUE
                  DEFAULT KEY INITIAL SIZE 5.

DATA iztcou106 LIKE ztcou106 OCCURS 0 WITH HEADER LINE.
*---------------------------------------------------------------------*I
INITIALIZATION.
* for combo box
  gw_mod-key  = 'P'.  gw_mod-text = 'Previous period'.
  APPEND gw_mod TO gt_mod.
  gw_mod-key  = 'A'.  gw_mod-text = 'ABP cost'.
  APPEND gw_mod TO gt_mod.
*  gw_mod-key  = 'B'.  gw_mod-text = 'Base cost'.
*  APPEND gw_mod TO gt_mod.
*  p_abp = 'P'.
*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = 'p_abp'
            values = gt_mod.

*----------------------------------------------------------------------
* AT SELECTION-SCREEN
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kalka.
* Possible entry for Costing type
  PERFORM popup_kalka USING p_kalka 'P_KALKA'.

*----------------------------------------------------------------------
* Start of selection
*----------------------------------------------------------------------
START-OF-SELECTION.
* Case of ABP, setting the period as 013.
  IF p_abp = 'X'.   "ABP
    gv_poper = '000'.
    gv_pkalka = 'BP'.
    p_poper = '000'.
  ELSE.
    gv_poper = p_poper.
    gv_pkalka = p_kalka.
  ENDIF.

  PERFORM get_data.

*----------------------------------------------------------------------
* End of selection
*----------------------------------------------------------------------
END-OF-SELECTION.
  CLEAR gv_cnt.

*  UD1K940601 by IG.MOON 5/21/2007
*  describe table gt_out lines gv_cnt.
*  if gv_cnt = 0.
*    message s000 with 'No data found.'.
*    exit.
*  else.
*    perform save_data.
*  endif.

  READ TABLE gt_out INDEX 1.

  IF sy-subrc NE 0.

*    DELETE FROM ztcou106 WHERE kokrs = p_kokrs
*                           AND bdatj = p_year
*                           AND kalka = p_kalka
*                           AND id IN s_id
*                           AND poper = p_poper.


    MESSAGE s000 WITH 'No data found.'.
  ELSE.
    DESCRIBE TABLE gt_out LINES gv_cnt.

    IF p_test = 'X'.
      PERFORM display_result.
    ELSE.
      PERFORM save_data.
    ENDIF.
  ENDIF.

* end

*&---------------------------------------------------------------------
*&      Form  GET_DATA
*&---------------------------------------------------------------------
FORM get_data.
  DATA: l_cnt   TYPE i,
        l_answer VALUE 'J'.

  CLEAR:   gt_102, gt_103, gt_104,
           gt_a018, gt_out, l_cnt.
  REFRESH: gt_102, gt_103, gt_104,
           gt_a018, gt_out.

* Get data from Variance Analysis Code
*  IF p_ref = 'X' ."space.
**   Check data whether in the table ZTCOU106
*    PERFORM check_ztcou106 USING l_cnt.
*
** Get data from Variance Analysis Code in the table ZTCOU104
*    IF l_cnt > 0.
*      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
*        EXPORTING
*          textline1 = 'Table ZTCOU106 has data. Do you want delete?'
*          titel     = 'Check!'
*          cancel_display = 'X'
*       IMPORTING
*         answer = l_answer.
*    ENDIF.
*
*    IF l_answer = 'J'.
*      PERFORM get_data_from_ztcou104.
*    ENDIF.
*
*  ELSE.

  PERFORM get_data_from_ztcou104.

*  ENDIF.


  IF NOT gt_104[] IS INITIAL.
*   IF p_abp = space.
      PERFORM get_data_from_ztcou102.
*   ENDIF.

    PERFORM create_structure.

    PERFORM calc_difference.
  ENDIF.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM save_data.
  DATA: l_seq TYPE zseq1,
        l_cnt TYPE i.

  DATA: locked.
  DATA $flag.

  __cls atab.
  CONCATENATE 'LOCK' p_poper+1(2) '^eq^''X''' INTO wa_tab.
  REPLACE '^' WITH ' ' INTO : wa_tab , wa_tab.
  APPEND wa_tab TO atab.

  CLEAR: l_cnt, l_seq.

  SELECT MAX( seq ) INTO l_seq
    FROM ztcou106.

  __cls iztcou106.

  SELECT * FROM ztcou106 INTO TABLE iztcou106
                       WHERE kokrs = p_kokrs
                         AND bdatj = p_year
                         AND kalka = p_kalka
                         AND id IN s_id
                         AND poper = p_poper.

  SORT iztcou106 BY kokrs bdatj kalka id poper.

  LOOP AT iztcou106.

    AT NEW poper.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.

    SELECT SINGLE * INTO *ztcou104lock
          FROM ztcou104lock
    WHERE kokrs EQ iztcou106-kokrs
      AND bdatj EQ iztcou106-bdatj
      AND kalka EQ iztcou106-kalka
      AND id    EQ iztcou106-id
      AND (atab).

    IF sy-subrc EQ 0.
    ELSE.
      DELETE FROM ztcou106 WHERE kokrs = iztcou106-kokrs
                             AND bdatj = iztcou106-bdatj
                             AND kalka = iztcou106-kalka
                             AND id    = iztcou106-id
                             AND poper = p_poper.
    ENDIF.
  ENDLOOP.

  COMMIT WORK.

  WRITE: /'------------------------------------------------------',
         /'Date:', sy-datum, 'Time:', sy-uzeit, 'User:', sy-uname,
         /'------------------------------------------------------',
         /'Controling area:', p_kokrs,
         /'Piscal year:    ', p_year,
         /'Period:         ' , p_poper,
         /'Costing Type:   ', p_kalka.
  WRITE: /'------------------------------------------------------'.

  SORT gt_out BY kokrs bdatj kalka id poper.

  LOOP AT gt_out.

    AT NEW poper.
      $flag = true.
    ENDAT.

    CLEAR ztcou106.
    l_seq = l_seq + 1.

    IF $flag EQ true.
      PERFORM check_lock USING gt_out-kokrs
                               gt_out-bdatj
                               gt_out-kalka
                               gt_out-id
                               p_poper
                      CHANGING locked.
      CLEAR $flag.
    ENDIF.

    IF locked EQ true.
      WRITE:/ gt_out-id    COLOR COL_NEGATIVE,
              gt_out-upgvc COLOR COL_NEGATIVE,
              gt_out-compn COLOR COL_NEGATIVE,
              gt_out-poper COLOR COL_NEGATIVE,':lock'.
    ELSE.
      MOVE-CORRESPONDING gt_out TO ztcou106.
      ztcou106-kzust1 = gt_out-kzust.
      ztcou106-wertn1 = gt_out-dwertn.
      ztcou106-seq = l_seq.
      ztcou106-aedat = sy-datum.
      ztcou106-aenam = sy-uname.
      INSERT ztcou106.
      IF sy-subrc = 0.
        WRITE:/ ztcou106-id    COLOR COL_POSITIVE,
                ztcou106-upgvc COLOR COL_POSITIVE,
                ztcou106-compn COLOR COL_POSITIVE,
                ztcou106-poper COLOR COL_POSITIVE.
        l_cnt = l_cnt + 1.
      ELSE.
        WRITE:/ ztcou106-id    COLOR COL_NEGATIVE,
                ztcou106-upgvc COLOR COL_NEGATIVE,
                ztcou106-compn COLOR COL_NEGATIVE,
                ztcou106-poper COLOR COL_NEGATIVE,':error'.
      ENDIF.

    ENDIF.
  ENDLOOP.

  WRITE: /'------------------------------------------------------',
         /,/'* Saved records:', l_cnt,
         /'------------------------------------------------------'.

ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  SPLIT_REASON
*&---------------------------------------------------------------------*
*       Get SPLIT  code
*----------------------------------------------------------------------*
FORM split_reason.
* <Reason Code>
*  E4: Usage Change (EU4, ED4, EE4), U:Up D:Down E:Equal
*  V1: Vendor/Source Change (VU1, VD1, VE1)
*  M1: MIP Reason Code Change (MU1, MD1, ME1)
*  K1: KD Reason Code Change (KU1, KD1, KE1)
*     KD: when same country key as ABP LDC Rate table ZTCOU116
*     MIP: when Costing Result table ZTCOU102 has no data
*  P1: Price Change (PU1, PD1, PE1)

* UD1K941594 - by IG.MOON 9/17/2007
* Add classification
* {
*    V : source change
*    S : spec. change
*    Q : quantity change
*    P : price change
* }

* Source / Vendor Variance
  IF NOT gt_all-lifnr IS INITIAL AND NOT gt_all-plifnr IS INITIAL AND
     gt_all-lifnr <> gt_all-plifnr.

    gt_outwk-zrclss = 'V'.
    PERFORM src_change.
  ENDIF.

* use current vendor
  gt_outwk-lifnr = gt_all-lifnr.
  IF gt_outwk-lifnr IS INITIAL.
    gt_outwk-lifnr = gt_all-plifnr.  "FIXME
  ENDIF.

* Spec/Qty Variance
  IF gt_all-menge = 0 OR gt_all-pmenge = 0. "Spec Var
*        AND GT_ALL-$FLAG EQ SPACE.
    gt_outwk-zrclss = 'S'.
    PERFORM spec_change.

  ELSEIF ( gt_all-menge <> gt_all-pmenge )  "Qty Var
        AND gt_all-$flag EQ space.

    gt_outwk-zrclss = 'Q'.
    PERFORM usage_change.
  ENDIF.

* Price Variance
  IF  ( gt_all-menge <> 0 AND gt_all-pmenge <> 0
*       AND gt_all-wertn <> 0
*       AND gt_all-pwertn <> 0
        AND gt_all-wertn <> gt_all-pwertn )
  OR gt_all-$flag EQ 'X'.

    gt_outwk-zrclss = 'P'.
    PERFORM price_change.
  ENDIF.

  DATA $dwertn LIKE gt_outwk-dwertn.

* check total
  LOOP AT gt_outwk.
* by ig.moon 6/11/2008 {
*    AT END OF compn.
*      SUM.
*    ENDAT.
    AT LAST.
      SUM.
      $dwertn = gt_outwk-dwertn.
    ENDAT.
* }
  ENDLOOP.

  DATA: l_rounding LIKE gt_outwk-dwertn.
  l_rounding = gt_all-wertn - gt_all-pwertn - $dwertn. "gt_outwk-dwertn.
  IF l_rounding <> 0.
    PERFORM fill_header_data.
    PERFORM get_kzust USING 'Z' 'R'.  "Rounding Error!!!
    gt_outwk-dwertn = l_rounding.
    APPEND gt_outwk.
  ENDIF.

* return result to global internal table.
  APPEND LINES OF gt_outwk TO gt_out.

ENDFORM.                    " SPLIT_REASON
*&---------------------------------------------------------------------*
*&      Form  SPEC_CHANGE
*&---------------------------------------------------------------------*
*       Spec Variance : Usage, Part Change (E4)
*----------------------------------------------------------------------*
* <Spec change>
* (No following)
* 1. Old & New part ... Part: New Part
*                (1) Prv.qty: 0
*                (2) Prv.unit price: New part unit price
*                (3) Curr.qty: New part qty
*                (4) Curr.unit price: New part unit price
*                (5) Diff.qty: (3)-(1)
*                (6) Diff.price: (5)*(2): Diff.qty * Prv.unit price
*                (7) Reason: EU4
* (existing following)
* 1. Old &New part ... Part: New Part
*                (1) Prv.qty: 0
*                (2) Prv.unit price: old part unit price
*                (3) Curr.qty: New part qty
*                (4) Curr.unit price: New part unit price
*                (5) Diff.qty: (3)-(1)
*                (6) Diff.price: (5)*(2): Diff.qty * Prv.unit price
*                (7) Reason: EU4
* 2. New part ... Part: New Part
*                (1) Prv.qty: 0
*                (2) Prv.unit price: old part unit price
*                (3) Curr.qty: New part qty
*                (4) Curr.unit price: New part unit price
*                (5) Diff.qty: (3)-(1)
*                (6) Diff.price: (5)*((4)-(2)): Diff.qty * Diff.UPR
*                (7) Reason: ED4
*----------------------------------------------------------------------*
* (Example)
*  <Spec change>
*  Part      : A  -> B
*  Qty       : 5  -> 4
*  Unit price: $5 -> $3
*----------------------------------------------------------------------*
*  RSN  Part  P.qty  P.UPR  C.qty  C.UPR  Diff.qty  Diff.price
*----------------------------------------------------------------------*
*  (no follwing)
*  ED4  A     5      5      0      0       -5       -5*5 = -25
*  EU4  B     0      3      4      3        4        4*3 = 12
*  ED4  B     0      3      4      3        4        4*(3-3) = 0
*
*  (existing follwing)
*  ED4  A     5      5      0      0       -5       -5*5 = -25
*  EU4  B     0      5      4      3        4        4*5 = 20
*  KD4  B     0      5      4      3        4        4*(3-5) = -8
*----------------------------------------------------------------------*
FORM spec_change.

** ...No following
  IF gt_all-$flag EQ space.
    PERFORM get_no_following.

* ...Existing following
  ELSE.
    PERFORM get_following.
  ENDIF.

ENDFORM.                    " SPEC_CHANGE
*&---------------------------------------------------------------------*
*&      Form  SRC_CHANGE
*&---------------------------------------------------------------------*
*       Source / Vendor Variance(V1)
*----------------------------------------------------------------------*
* If change vendor, create 2 records by old vendor and new vendor.
* 1. Old vendor... Vendor: old vendor
*                  (1) Prv.qty: old vendor qty
*                  (2) Prv.unit price: old vendor unit price
*                  (3) Curr.qty: 0
*                  (4) Curr.unit price: old vendor unit price
*                  (5) Diff.qty: (3) - (1): Curr.qty - Prv.qty
*                  (6) Diff.price:(5) * (2): Diff.qty * old vendor UPR
*                  (7) Reason: VD1
* 2. New vendor... Vendor: new vendor
*                  (1) Prv.qty: 0
*                  (2) Prv.unit price: old vendor unit price
*                  (3) Curr.qty: old vendor qty
*                  (4) Curr.unit price: old vendor unit price
*                  (5) Diff.qty: (3) - (1): Curr.qty - Prv.qty
*                  (6) Diff.price:(5) * (2): Diff.qty * old vendor UPR
*                  (7) Reason: VU1
*----------------------------------------------------------------------*
* (Example)
*  Part      : A
*  Vendor    : #1 -> #2
*  Qty       : 5  -> 4
*  Unit price: $5 -> $3
*----------------------------------------------------------------------*
*  RSN Vendor  P.qty  P.UPR  C.qty  C.UPR  Diff.qty  Diff.price
*----------------------------------------------------------------------*
*  VD1 #1      5      $5     0      $5     -5         -5*5 = -25$
*  VU1 #2      0      $5     5      $5      5          5*5 =25$
*----------------------------------------------------------------------*
FORM src_change.

* 1. Old Vendor
  gt_outwk-lifnr  = gt_all-plifnr.                    " Vendor
  gt_outwk-pmenge = gt_all-pmenge.                    " Prv.qty
  gt_outwk-pwertn = gt_all-pgpreis.                   " Prv.unit price
  gt_outwk-kpein  = gt_all-kpein.                     " Prv.unit price
  gt_outwk-meeht  = gt_all-meeht.                     " Prv.unit price
  CLEAR gt_outwk-menge.                               " Curr.qty
  gt_outwk-wertn  = gt_all-pgpreis.                   " Curr.unitprice
  gt_outwk-dmenge = gt_outwk-menge - gt_outwk-pmenge.   " Diff.qty
  gt_outwk-dwertn = gt_outwk-dmenge * gt_all-pgpreis / gt_all-kpein.
  " Diff.price

  IF gt_outwk-dwertn <> 0.
    PERFORM get_kzust USING 'V' '1'.                " Reason: VD1
    APPEND gt_outwk.
  ENDIF.

* 2. New Vendor
  gt_outwk-lifnr  = gt_all-lifnr.                     " Vendor

  CLEAR gt_outwk-pmenge.                              " Prv.qty

* Prv.unit price: Prv.unit price: same the case of old vendor
  gt_outwk-menge = gt_all-pmenge.                     " Curr.qty
* Curr.unit price: Prv.unit price: same the case of old vendor
  gt_outwk-dmenge = gt_outwk-menge - gt_outwk-pmenge.   " Diff.qty
  gt_outwk-dwertn = gt_outwk-dmenge * gt_all-pgpreis / gt_all-kpein.
  " Diff.price

  IF gt_outwk-dwertn <> 0.
    PERFORM get_kzust USING 'V' '1'.                " Reason: VU1
    APPEND gt_outwk.
  ENDIF.

ENDFORM.                    " SRC_CHANGE
*&---------------------------------------------------------------------*
*&      Form  PRICE_CHANGE
*&---------------------------------------------------------------------*
*       Get Price Change
*----------------------------------------------------------------------*
*  (1) Prv.qty: 0
*  (2) Prv.unit price: changed unit price
*  (3) Curr.qty: Curr.qty
*  (4) Curr.unit price: Curr.unit price
*  (5) Diff.qty: (3) - (1): Curr.qty - Prv.qty
*  (6) Diff.price: (5) * ( (4) - (2) ) : Diff.qty * Diff.unit price
*  (7) Reason: PU4 / PD4 / PE4 (Diff.price > 0:U, < 0:D, = 0:E)
*----------------------------------------------------------------------*
* (Example)
*  <Price change>
*  Part      : A
*  Qty       : 5  -> 4
*  Unit price: $5 -> $3
*----------------------------------------------------------------------*
*  RSN  P.qty  P.UPR  C.qty  C.UPR  Diff.qty  Diff.price
*----------------------------------------------------------------------*
*  P%4  0      3      4      3      4          0
*----------------------------------------------------------------------*
* only follwing
FORM price_change.
  DATA: l_w1 TYPE zwertn1,
        l_w2 TYPE zwertn1,
        l_w3 TYPE zwertn1,
        l_w4 TYPE zwertn1,
        l_wt TYPE zwertn1,
        l_di TYPE zwertn1,
        l_wertn(15) TYPE p DECIMALS 6.

  IF gt_all-menge = 0 AND gt_outwk-wertn = 0.
    IF gt_all-$flag NE space.
      EXIT.
    ENDIF.
  ENDIF.

* vendor.... FIXME
  gt_outwk-lifnr = gt_all-lifnr.
  IF gt_outwk-lifnr IS INITIAL.
    gt_outwk-lifnr = gt_all-plifnr.
  ENDIF.

  DATA $gpreis LIKE gt_all-gpreis.
  DATA $kpein LIKE gt_all-kpein.

* Old & New part
*  CLEAR gt_outwk-PMENGE.                            " Prv.qty
  IF NOT gt_all-$gpreis IS INITIAL.
    $gpreis = gt_all-$gpreis.
    $kpein =  gt_all-kpein.
  ELSE.
    $gpreis = gt_all-pgpreis.
    $kpein =  gt_all-pkpein.
  ENDIF.

* If current qty is exist, set unit as current unit
* else, set unit as prvious unit
*  IF GT_ALL-MENGE <> 0.
  gt_outwk-kpein  = gt_all-kpein.      " Condition pricing unit
  gt_outwk-meeht  = gt_all-meeht.      " Price qty unit
*  ELSE.
*    gt_outwk-KPEIN  = GT_ALL-PKPEIN.
*    gt_outwk-MEEHT  = GT_ALL-PMEEHT.
*  ENDIF.

  gt_outwk-pmenge = gt_all-pmenge.
  gt_outwk-menge = gt_all-menge.                     " Prv.

  IF $kpein IS INITIAL.
    BREAK-POINT.
  ENDIF.

  gt_outwk-pwertn = gt_outwk-menge * $gpreis / $kpein.     "
  gt_outwk-wertn  = gt_outwk-menge * gt_all-gpreis / gt_outwk-kpein.
  gt_outwk-dwertn = gt_outwk-wertn - gt_outwk-pwertn.

  CHECK gt_outwk-dwertn <> 0.

  CASE 'X'.

    WHEN gt_all-mip.
      PERFORM get_kzust USING 'M' '1'.
      APPEND gt_outwk.

    WHEN gt_all-kd.
      PERFORM get_kzust USING 'K' '1'.
      APPEND gt_outwk.

    WHEN OTHERS.
* 12/13 {
      IF p_abp EQ 'X'.
        PERFORM get_kzust USING 'Z' 'A'.
        APPEND gt_outwk.
        EXIT.
      ENDIF.
*  }
      IF gt_all-$flag EQ 'X' AND gt_all-$compn NE space.
        READ TABLE gt_102 WITH KEY kalka = p_kalka
                                   bdatj = gt_104-cyear
                                   poper = gt_104-cpoper
                                   matnr = gt_all-$compn
                               BINARY SEARCH.
      ELSE.
        READ TABLE gt_102 WITH KEY kalka = p_kalka
                                   bdatj = gt_104-cyear
                                   poper = gt_104-cpoper
                                   matnr = gt_all-compn
                               BINARY SEARCH.
      ENDIF.

      IF sy-subrc <> 0.
        PERFORM get_kzust USING 'P' '4'.
        APPEND gt_outwk.

      ELSE.
        CLEAR: l_w1, l_w2, l_w3, l_w4, l_di, l_wt.
*       calculate price as converted by UoM

*       RSN1$
        PERFORM convert_unit USING gt_102-wertn1
                                   gt_102-pmeht
                                   gt_all-meeht
                            CHANGING gt_102-wertn1.
        l_w1 = gt_102-wertn1 * gt_102-qta / 100.

*       RSN2$
        PERFORM convert_unit USING gt_102-wertn2
                                   gt_102-pmeht
                                   gt_all-meeht
                            CHANGING gt_102-wertn2.
        l_w2 = gt_102-wertn2 * gt_102-qta / 100.

*       RSN1$ of 2nd vendor
        PERFORM convert_unit USING gt_102-wertn1_v2
                                   gt_102-pmeht
                                   gt_all-meeht
                            CHANGING gt_102-wertn1_v2.
        l_w3 = gt_102-wertn1_v2 * gt_102-qta_v2 / 100.

*       RSN2$ of 2nd vendor
        PERFORM convert_unit USING gt_102-wertn2_v2
                             gt_102-pmeht
                             gt_all-meeht
                      CHANGING gt_102-wertn2_v2.
        l_w4 = gt_102-wertn2_v2 * gt_102-qta_v2 / 100.

*       Diff.price
        l_di = gt_outwk-dwertn.

        IF l_w1 <> 0.
          gt_outwk-kzust  = gt_102-kzust1.       " Reason
          gt_outwk-dwertn = gt_outwk-menge * l_w1 / gt_outwk-kpein
.

          l_wt = l_wt + gt_outwk-dwertn.
          APPEND gt_outwk.
        ENDIF.

        IF l_w2 <> 0.
          gt_outwk-kzust  = gt_102-kzust2.
          gt_outwk-dwertn = gt_outwk-menge * l_w2 / gt_outwk-kpein
.

          l_wt = l_wt + gt_outwk-dwertn.
          APPEND gt_outwk.
        ENDIF.

        IF l_w3 <> 0.
          gt_outwk-kzust  = gt_102-kzust1_v2.
          gt_outwk-dwertn = gt_outwk-menge * l_w3 / gt_outwk-kpein
.

          l_wt = l_wt + gt_outwk-dwertn.
          APPEND gt_outwk.
        ENDIF.

        IF l_w4 <> 0.
          gt_outwk-kzust  = gt_102-kzust2_v2.
          gt_outwk-dwertn = gt_outwk-menge * l_w4 / gt_outwk-kpein
.

          l_wt = l_wt + gt_outwk-dwertn.
          APPEND gt_outwk.
        ENDIF.
      ENDIF.

*   Rounding
      IF l_wt <> l_di.
        gt_outwk-dwertn = l_di - l_wt.
        PERFORM get_kzust USING 'P' '4'.
        APPEND gt_outwk.
      ENDIF.

  ENDCASE.

ENDFORM.                    " PRICE_CHANGE
*&---------------------------------------------------------------------*
*&      Form  GET_KZUST
*&---------------------------------------------------------------------*
*       Get Reason
*----------------------------------------------------------------------*
* Reason: 1st digit of reason + Diff.op(E,U,D) + 2nd digit of reason
FORM get_kzust USING   p1
                       p3.
  DATA l2.
  CLEAR l2.

  IF gt_outwk-dwertn = 0.
    l2 = 'E'.
  ELSEIF gt_outwk-dwertn < 0.
    l2 = 'D'.
  ELSE.
    l2 = 'U'.
  ENDIF.

  IF p_abp = 'X'.
    CONCATENATE 'Z' l2 'A' INTO gt_outwk-kzust.
  ELSE.
    CONCATENATE p1 l2 p3 INTO gt_outwk-kzust.
  ENDIF.

ENDFORM.                    " GET_KZUST
*&---------------------------------------------------------------------*
*&      Form  USAGE_CHANGE
*&---------------------------------------------------------------------*
*       Append data when change usage
*----------------------------------------------------------------------*
* (1) Prv.qty: previous period qty
* (2) Prv.unit price: previous period unit price
* (3) Curr.qty: current period qty
* (4) Curr.unit price: previous period unit price
* (5) Diff.qty: (3) - (1) : Curr.qty - Prv.qty
* (6) Diff.price: (5) * (2) :Diff.qty * Prv.unit UPR
* (7) Reason: EU4 / ED4 / EE4
*----------------------------------------------------------------------*
*  <Usage change>
*  Part      : A
*  Qty       : 5  -> 4
*  Unit price: $5 -> $3
*----------------------------------------------------------------------*
*  RSN Vendor  P.qty  P.UPR  C.qty  C.UPR  Diff.qty  Diff.price
*----------------------------------------------------------------------*
*  ED4 #2      5      $5     4      $5     -1         -1*5 = -5$
*----------------------------------------------------------------------*
FORM usage_change.

  gt_outwk-pmenge = gt_all-pmenge.                     " Prv.qty
  gt_outwk-pwertn = gt_all-pgpreis.                    " Prv.unit price
  gt_outwk-menge  = gt_all-menge.                      " Curr.qty
  gt_outwk-kpein  = gt_all-kpein.              " Condition pricing unit
  gt_outwk-meeht  = gt_all-meeht.                      " Price qty unit
  gt_outwk-wertn  = gt_all-pgpreis.                    " Curr.UPR
  gt_outwk-dmenge = gt_outwk-menge - gt_outwk-pmenge.      " Diff.qty

* Diff.price
  gt_outwk-dwertn = gt_outwk-dmenge * gt_all-pgpreis / gt_all-kpein.

  IF gt_outwk-dwertn <> 0.
    PERFORM get_kzust USING 'E' '4'.                 " Reason: E%4
    APPEND gt_outwk.
  ENDIF.

ENDFORM.                    " USAGE_CHANGE
*&---------------------------------------------------------------------*
*&      Form  GET_NO_FOLLOWING
*&---------------------------------------------------------------------*
*       Append data when change spec: no followng
*----------------------------------------------------------------------*
* (No following)
*  Old & New part ... Part: New Part
*  (1) Prv.qty: 0
*  (2) Prv.unit price: New part unit price
*  (3) Curr.qty: New part qty
*  (4) Curr.unit price: New part unit price
*  (5) Diff.qty: (3)-(1)
*  (6) Diff.price: (5)*(2): Diff.qty * Prv.unit price
*  (7) Reason: EU4
*----------------------------------------------------------------------*
*  <Spec change>
*  Part      : A  -> B
*  Qty       : 5  -> 4
*  Unit price: $5 -> $3
*----------------------------------------------------------------------*
*  RSN  Part  P.qty  P.UPR  C.qty  C.UPR  Diff.qty  Diff.price
*----------------------------------------------------------------------*
*  ED4  A     5      5      0      0       -5         -5*5 = -25
*  EU4  B     0      3      4      3       4           4*3 = 12
*  ED4  B     0      3      4      3       4           4*(3-3) = 0
*----------------------------------------------------------------------*
FORM get_no_following.
  DATA: l_valuation_price TYPE zgpreis.

* New & Old part
  gt_outwk-compn = gt_all-compn.             " Part

  gt_outwk-pmenge = gt_all-pmenge.           " Prv.qty
  gt_outwk-pwertn = gt_all-pgpreis.          " Prv.Amt

  gt_outwk-menge  = gt_all-menge.             " Curr Qty
  gt_outwk-wertn  = gt_all-gpreis.            " Curr.Amt

  IF gt_all-menge <> 0.
    gt_outwk-kpein  = gt_all-kpein.          " Condition pricing unit
    gt_outwk-meeht  = gt_all-meeht.          " Price qty unit
  ELSE.
    gt_outwk-kpein  = gt_all-pkpein.         " Prv.price unit
    gt_outwk-meeht  = gt_all-pmeeht.         " Prv.Price qty unit
  ENDIF.

  IF gt_all-pgpreis = 0.
    l_valuation_price = gt_all-gpreis.
  ELSE.
    l_valuation_price = gt_all-pgpreis.
  ENDIF.


  IF gt_all-menge <> 0.  "NEW part
    gt_outwk-dmenge = gt_all-menge.          " Diff.qty
    gt_outwk-dwertn = gt_outwk-dmenge *
                        l_valuation_price  / gt_all-kpein.
    " Diff.price
  ELSE.
    gt_outwk-dmenge = - gt_all-pmenge.       " Diff.qty
    gt_outwk-dwertn = gt_outwk-dmenge *
                        l_valuation_price / gt_all-pkpein.
    " Diff.price
  ENDIF.

  IF gt_outwk-dwertn <> 0.
    PERFORM get_kzust USING 'E' '4'.       " Reason
    APPEND gt_outwk.
  ENDIF.

ENDFORM.                    " GET_NO_FOLLOWING
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_ZTCOU104
*&---------------------------------------------------------------------*
FORM get_data_from_ztcou104.
  DATA: l_artnr TYPE artnr.

  IF p_abp = 'X'.   "ABP             "  If choose ABP
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_104
      FROM ztcou104
     WHERE kokrs = p_kokrs
       AND bdatj = p_year
       AND kalka = p_kalka
       AND id IN s_id
       AND zabp_fsc <> space.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_104
      FROM ztcou104
     WHERE kokrs = p_kokrs
       AND bdatj = p_year
       AND kalka = p_kalka
       AND id IN s_id.
  ENDIF.


  DATA: n(2)  TYPE n,
        i(2)  TYPE n,
        lv_fsc(18),
        l_idx LIKE sy-tabix.
  FIELD-SYMBOLS <fs1> TYPE ANY.

  LOOP AT gt_104.
    l_idx = sy-tabix.
*--- BASE - ABP
    IF p_abp = 'X'.

      IF gt_104-base_year IS INITIAL.
        gt_104-cyear = p_year - 1.
        gt_104-cpoper = '012'.
      ELSE.
        gt_104-cyear  = gt_104-base_year.
        gt_104-cpoper = gt_104-base_poper.
      ENDIF.
      gt_104-cfsc   = gt_104-zbase_fsc.
      gt_104-kalka = p_kalka.

      gt_104-pyear  = gt_104-bdatj.
      gt_104-ppoper = '001'.
      gt_104-pfsc   = gt_104-zabp_fsc.
      gt_104-pkalka = 'BP'.
    ELSE.
*--- determine current
      gt_104-cyear   = p_year.            " Currnet Fiscal year
      gt_104-cpoper  = p_poper.           " Currnet Period
      n = p_poper.
      CONCATENATE 'GT_104-FSC' n INTO lv_fsc.
      ASSIGN (lv_fsc) TO <fs1>.
      IF <fs1> IS INITIAL.
        DELETE gt_104 INDEX l_idx. CONTINUE.  "exclude if blank
      ENDIF.
      gt_104-cfsc = <fs1>.

*--- determine previous
      WHILE n > 0.
        n = n - 1.
        IF n > 0.
          CONCATENATE 'GT_104-FSC' n INTO lv_fsc.
          ASSIGN (lv_fsc) TO <fs1>.
          IF <fs1> <> space.
            EXIT.
          ENDIF.
        ENDIF.
      ENDWHILE.

      IF n = 0.  "if Jan or starting, use BASE

        IF gt_104-base_year IS INITIAL.
          gt_104-pyear = p_year - 1.
          gt_104-ppoper = '012'.
        ELSE.
          gt_104-pyear  = gt_104-base_year.
          gt_104-ppoper = gt_104-base_poper.
        ENDIF.

        gt_104-pfsc   = gt_104-zbase_fsc.

      ELSE.
        gt_104-pyear  = p_year.            " Currnet Fiscal year
        gt_104-ppoper = n.                 " Currnet Period
        gt_104-pfsc   = <fs1>.
      ENDIF.
      gt_104-pkalka = gt_104-kalka.
    ENDIF.

    MODIFY gt_104 INDEX l_idx
           TRANSPORTING pyear ppoper pfsc pkalka cyear cpoper cfsc.
  ENDLOOP.
  SORT gt_104 BY id.

* check if 103 exist
  LOOP AT gt_104.

    SELECT SINGLE artnr INTO l_artnr FROM ztcou103
                 WHERE kokrs = p_kokrs
                   AND bdatj = gt_104-pyear
                   AND kalka = gt_104-pkalka
                   AND poper = gt_104-ppoper
                   AND artnr = gt_104-pfsc.
    IF sy-subrc <> 0.
      DELETE gt_104 INDEX sy-tabix.
      MESSAGE s100 WITH '**No unit costing in current:'
                   gt_104-id gt_104-pyear gt_104-ppoper.
      CONTINUE.
    ENDIF.

    SELECT SINGLE artnr INTO l_artnr FROM ztcou103
                 WHERE kokrs = p_kokrs
                   AND bdatj = gt_104-cyear
                   AND kalka = p_kalka
                   AND poper = gt_104-cpoper
                   AND artnr = gt_104-cfsc.
    IF sy-subrc <> 0.
      DELETE gt_104 INDEX sy-tabix.
      MESSAGE s100 WITH '**No unit costing to compare:'
                   gt_104-id gt_104-cyear gt_104-cpoper.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_DATA_FROM_ZTCOU104
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_FROM_ZTCOU102
*&---------------------------------------------------------------------*
*       Get data from Cost Result
*----------------------------------------------------------------------*
FORM get_data_from_ztcou102.

  LOOP AT gt_104.
*---- current
    SELECT bdatj poper kalka matnr a~werks profl a~lifnr land1
           grspr as wertn pmeht peinh
           qta    wertn_v1  kzust1      wertn1    kzust2    wertn2
           qta_v2 wertn_v2  kzust1_v2   wertn1_v2 kzust2_v2 wertn2_v2
           ekgrp
      APPENDING CORRESPONDING FIELDS OF TABLE gt_102
      FROM ztcou102 AS a
      LEFT JOIN lfa1 AS b
        ON b~lifnr = a~lifnr
      WHERE a~kokrs = p_kokrs
       AND a~bdatj =  gt_104-cyear
       AND a~poper =  gt_104-cpoper
       AND a~kalka =  p_kalka
       AND a~ver   = '00'
       AND a~matnr IN s_matnr.

*-- previous
    SELECT bdatj poper kalka matnr a~werks profl a~lifnr land1
           grspr as wertn pmeht peinh
           qta    wertn_v1  kzust1      wertn1    kzust2    wertn2
           qta_v2 wertn_v2  kzust1_v2   wertn1_v2 kzust2_v2 wertn2_v2
           ekgrp
    APPENDING CORRESPONDING FIELDS OF TABLE gt_102
    FROM ztcou102 AS a
    LEFT JOIN lfa1 AS b
      ON b~lifnr = a~lifnr
    WHERE a~kokrs = p_kokrs
     AND a~bdatj =  gt_104-pyear
     AND a~poper =  gt_104-ppoper
     AND a~kalka = gv_pkalka
     AND a~ver   = '00'
     AND a~matnr IN s_matnr.
  ENDLOOP.

  SORT gt_102 BY kalka bdatj poper matnr werks.
  DELETE ADJACENT DUPLICATES FROM gt_102
         COMPARING kalka bdatj poper matnr werks.


ENDFORM.                    " GET_DATA_FROM_ZTCOU102
*&---------------------------------------------------------------------*
*&      Form  CHECK_ZTCOU106
*&---------------------------------------------------------------------*
FORM check_ztcou106 CHANGING p_cnt TYPE i.
  SELECT COUNT(*) INTO p_cnt
    FROM ztcou106
    WHERE kokrs = p_kokrs
      AND bdatj = p_year
      AND kalka = p_kalka
      AND id IN s_id
      AND poper = p_poper.

ENDFORM.                    " CHECK_ZTCOU106
*&---------------------------------------------------------------------*
*&      Form  GET_FOLLOWING
*&---------------------------------------------------------------------*
*  Append data when change spec: existing followng
*----------------------------------------------------------------------*
* (existing following)
* Old & New part ... Part: New Part
*                (1) Prv.qty: 0
*                (2) Prv.unit price: old part unit price
*                (3) Curr.qty: New part qty
*                (4) Curr.unit price: New part unit price
*                (5) Diff.qty: (3)-(1)
*                (6) Diff.price: (5)*(2) / UoM
*                    : Diff.qty * Prv.unit price/Condition pricing unit
*                (7) Reason: EU4
*----------------------------------------------------------------------*
* (Example)
*  <Spec change>
*  Part      : A  -> B
*  Qty       : 5  -> 4
*  Unit price: $5 -> $3
*----------------------------------------------------------------------*
*  RSN  Part  P.qty  P.UPR  C.qty  C.UPR  Diff.qty  Diff.price
*----------------------------------------------------------------------*
*  ED4  A     5      5      0      0       -5       -5*5 = -25
*  EU4  B     0      5      4      3        4        4*5 = 20
*  KD4  B     0      5      4      3        4        4*(3-5) = -8
*----------------------------------------------------------------------*
FORM get_following.

  DATA $gpreis LIKE gt_all-gpreis.

* Old & New part
*  CLEAR gt_outwk-PMENGE.                            " Prv.qty
  IF NOT gt_all-$gpreis IS INITIAL.
    $gpreis = gt_all-$gpreis.
  ELSE.
    $gpreis = gt_all-pgpreis.
  ENDIF.

* If current qty is exist, set unit as current unit
* else, set unit as prvious unit
  IF gt_all-menge <> 0.
    gt_outwk-kpein  = gt_all-kpein.      " Condition pricing unit
    gt_outwk-meeht  = gt_all-meeht.      " Price qty unit
  ELSE.
    gt_outwk-kpein  = gt_all-pkpein.
    gt_outwk-meeht  = gt_all-pmeeht.
  ENDIF.

  gt_outwk-pmenge = gt_all-pmenge.
  gt_outwk-menge = gt_all-menge.                     " Prv.
  gt_outwk-dmenge = gt_outwk-menge - gt_outwk-pmenge.

  gt_outwk-pwertn = gt_outwk-pmenge * $gpreis / gt_outwk-kpein.
  gt_outwk-wertn = gt_outwk-menge * $gpreis / gt_outwk-kpein.

  gt_outwk-dwertn = gt_outwk-wertn - gt_outwk-pwertn.

  IF gt_outwk-dwertn <> 0.
    PERFORM get_kzust USING 'E' '4'.               " Reason
    APPEND gt_outwk.
  ENDIF.

ENDFORM.                    " GET_FOLLOWING
*&---------------------------------------------------------------------*
*&      Form  create_compare_structure
*&---------------------------------------------------------------------*
*       Get Current data
*----------------------------------------------------------------------*
FORM create_compare_structure  USING fkalka fyear fpoper ffsc.
  CLEAR: gv_rc.

*BUFFERING---
* Search from internal table GT_103
  READ TABLE gt_103 WITH KEY kalka = fkalka
                                  bdatj = fyear
                                  poper = fpoper
                                  artnr = ffsc
                           BINARY SEARCH.

* If it has no data, append internal table GT_103
* as select from table ZTCOU103
  IF sy-subrc <> 0.
    SELECT bdatj poper kalka artnr compn kstar
           upgvc menge meeht gpreis wertn peinh splnt bwdat stkkz losgr
     APPENDING TABLE gt_103
     FROM ztcou103
    WHERE kokrs = p_kokrs
      AND bdatj = fyear
      AND poper = fpoper
      AND kalka = fkalka
      AND artnr = ffsc.

    SORT gt_103 BY kalka bdatj poper artnr.
    READ TABLE gt_103 WITH KEY kalka = fkalka
                                    bdatj = fyear
                                    poper = fpoper
                                    artnr = ffsc
                             BINARY SEARCH.
  ENDIF.

* Create internal table gt_103a & gt_103c
* by current data from internal table GT_103
  IF sy-subrc <> 0.
    __process '***Unit costing not found:' ffsc.
    gv_rc = 8.
    EXIT.
  ENDIF.

  LOOP AT gt_103 FROM sy-tabix.
    IF  gt_103-kalka = fkalka AND
        gt_103-bdatj = fyear  AND
        gt_103-poper = fpoper AND
        gt_103-artnr = ffsc.

      CHECK gt_103-compn IN s_matnr.

      MOVE-CORRESPONDING gt_103 TO gt_103a.
      gt_103a-id = gt_104-id.
      APPEND gt_103a.
      CLEAR gt_103a.

*-----Compact gt_103a
      gt_103c-id    = gt_104-id.        " ID
      gt_103c-upgvc = gt_103-upgvc.     " UPG
      gt_103c-kstar = gt_103-kstar.     " Cost Element
      gt_103c-compn = gt_103-compn.     " Curr.Component

      APPEND gt_103c. CLEAR gt_103c.

    ELSE.
      EXIT.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " create_compare_structure
*&---------------------------------------------------------------------*
*&      Form  calc_difference
*&---------------------------------------------------------------------*
*       Get applicable component
*----------------------------------------------------------------------*
FORM calc_difference.
  CLEAR: gt_ztcou116, gt_ztcou105, gt_all.
  REFRESH: gt_ztcou116, gt_ztcou105, gt_all.

  SORT gt_103 BY kalka bdatj poper artnr upgvc compn.

* Check Spec Change Info
  PERFORM get_ztcou105.
* Create Internal table for LDC Rate
  PERFORM get_gt_ztcou116.

* Create Internal table GT_ALL to calculate price
  PERFORM make_gt_all.

* UD1K941594 by IG.MOON 9/17/2007 {
  PERFORM merge_gt_all.
* }

  LOOP AT gt_all.
    CLEAR: gt_outwk.
    REFRESH: gt_outwk.


    PERFORM fill_header_data.

    PERFORM split_reason.
  ENDLOOP.


ENDFORM.                    " calc_difference
*&---------------------------------------------------------------------*
*&      Form  make_GT_ALL
*&---------------------------------------------------------------------*
* Create Internal table GT_ALL for calculate price
*----------------------------------------------------------------------*
FORM make_gt_all.
  DATA: lv_rc LIKE sy-subrc.

  LOOP AT gt_103c.
    gt_all-bdatj = p_year.                " Year
    gt_all-kalka = p_kalka.               " Costing Type

    gt_all-id    = gt_103c-id.              " ID
    gt_all-upgvc = gt_103c-upgvc.           " UPG
    gt_all-compn = gt_103c-compn.           " Component
    gt_all-kstar = gt_103c-kstar.           " Cost element

    READ TABLE gt_104 WITH KEY id = gt_103c-id BINARY SEARCH.

    PERFORM get_curr USING lv_rc.         " Current  Data
    PERFORM get_prv  USING lv_rc.         " Previous Data

*   Unit conversion: if unit
*   If current qty is exist,
*     previous price
*     = prv.unit price * curr.cond.price unit / prv.cond.price unit
*     previous cond.price unit = curr.cond.price unit
    IF gt_all-menge <> 0 AND
       gt_all-kpein <> gt_all-pkpein.
      IF gt_all-pkpein = 0.
        gt_all-pgpreis = gt_all-pgpreis.
      ELSE.
        gt_all-pgpreis = gt_all-pgpreis *
                         gt_all-kpein / gt_all-pkpein.
      ENDIF.
      gt_all-pkpein  = gt_all-kpein.
    ENDIF.

    APPEND gt_all.
    CLEAR gt_all.
  ENDLOOP.

ENDFORM.                    " make_GT_ALL
*&---------------------------------------------------------------------*
*&      Form  create_structure
*&---------------------------------------------------------------------*
* Create Internal table gt_103c for collect UPG & component
*                       gt_103a for price by UPG & component
*----------------------------------------------------------------------*
FORM create_structure.
  REFRESH: gt_103a.                                         "gt_103c,

  LOOP AT gt_104.
* Current period data
    PERFORM create_compare_structure
            USING p_kalka
                  gt_104-cyear
                  gt_104-cpoper
                  gt_104-cfsc.

    IF gv_rc = 0.
* Previous period data
      PERFORM create_compare_structure
              USING gv_pkalka
                    gt_104-pyear
                    gt_104-ppoper
                    gt_104-pfsc.
    ENDIF.
  ENDLOOP.

*-Product + Component
  SORT gt_103c BY id upgvc compn kstar.
  DELETE ADJACENT DUPLICATES FROM gt_103c
         COMPARING id upgvc compn kstar.

*-Costing Type + Year+Period + Product + UPGVC + Component + Details...
  SORT gt_103a BY kalka bdatj poper artnr upgvc compn kstar.
  DELETE ADJACENT DUPLICATES FROM gt_103a
        COMPARING kalka bdatj poper artnr upgvc compn kstar.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_GT_ZTCOU116
*&---------------------------------------------------------------------*
*       Create Internal table for LDC Rate
*----------------------------------------------------------------------*
FORM get_gt_ztcou116.

  SELECT kokrs bdatj land1
    INTO TABLE gt_ztcou116
    FROM ztcou116
*   where bdatj between p_year and gv_bdatj.
   WHERE kokrs = p_kokrs
     AND bdatj = p_year
     AND ver   = 0.

  SORT gt_ztcou116 BY kokrs bdatj land1.

ENDFORM.                    " GET_GT_ZTCOU116
*&---------------------------------------------------------------------*
*&      Form  GET_CURR
*&---------------------------------------------------------------------*
*       Get current period price
*----------------------------------------------------------------------*
FORM get_curr USING l_rc LIKE sy-subrc.
** Get current data from internal table gt_103a
  READ TABLE gt_103a WITH KEY kalka = gt_104-kalka
                           bdatj = gt_104-cyear
                           poper = gt_104-cpoper
                           artnr = gt_104-cfsc
                           upgvc = gt_103c-upgvc
                           compn = gt_103c-compn
                           kstar = gt_103c-kstar
                     BINARY SEARCH.
  IF sy-subrc <> 0.
    l_rc = 1.
  ELSE.
    gt_all-wertn  = gt_103a-wertn.          " AMT
    gt_all-menge  = gt_103a-menge.          " Qty
    gt_all-meeht  = gt_103a-meeht.          " UoM
    gt_all-gpreis = gt_103a-gpreis.         " Price
    gt_all-kpein  = gt_103a-peinh.          " Price unit

*   Case of MIP, search data from internal table GT_103
    IF gt_103a-stkkz = 'X'.
      gt_all-mip = 'X'.                    " MIP
    ELSE.
*     Get price & unit & vendor
      PERFORM move_to_gt_all USING    gt_104-kalka
                                      gt_104-cyear
                                      gt_104-cpoper
                                      gt_all-compn
                             CHANGING gt_all-lifnr
                                      gt_all-kd
* 9/2007 by IG.MOON
                                      gt_all-ekgrp_c
                                      gt_all-okzust_c.
*
    ENDIF.
    l_rc = 0.
  ENDIF.

ENDFORM.                    " GET_CURR
*&---------------------------------------------------------------------*
*&      Form  GET_PRV
*&---------------------------------------------------------------------*
*       Get previous period price
*----------------------------------------------------------------------*
FORM get_prv USING l_rc LIKE sy-subrc.
  DATA: l_amt(15) TYPE p DECIMALS 6,
        l_matnr   TYPE matnr.

  CLEAR gv_matnr.
*  GT_ALL-ppoper = gv_ppoper.

* Get previous data from internal table gt_103a
  READ TABLE gt_103a WITH KEY kalka = gt_104-pkalka
                           bdatj = gt_104-pyear
                           poper = gt_104-ppoper
                           artnr = gt_104-pfsc
                           upgvc = gt_103c-upgvc
                           compn = gt_103c-compn
                           kstar = gt_103c-kstar
                     BINARY SEARCH.
  IF sy-subrc = 0.
* No current, Previous only
    IF l_rc <> 0.
      gt_all-meeht = gt_103a-meeht.  "default UoM
    ENDIF.
    gt_all-pmeeht = gt_103a-meeht.

    PERFORM get_prv_price.

* Prev: None, Curr: New
  ELSE.
*---NEED BUFFERING : FIXME ANDY!!!
    PERFORM get_info_record.

    IF gt_all-pgpreis = 0.
*   Determine unit price from alt.part#
      READ TABLE gt_ztcou105 WITH KEY tmatnr = gt_all-compn
      BINARY SEARCH.

      IF sy-subrc = 0.
        READ TABLE gt_103a WITH KEY kalka = gt_104-pkalka
                                 bdatj = gt_104-pyear
                                 poper = gt_104-ppoper
                                 artnr = gt_104-pfsc
                                 upgvc = gt_all-upgvc
                                 compn = gt_ztcou105-fmatnr
                                 kstar = gt_103c-kstar
                             BINARY SEARCH.
        IF sy-subrc = 0.
          PERFORM get_prv_price.
        ELSE.
          gv_matnr = gt_ztcou105-fmatnr.
          PERFORM get_info_record.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.

* determine vendor/country
  IF gt_all-mip = space AND gt_all-pgpreis <> 0.  " Not MIP
    PERFORM move_to_gt_all USING    gt_104-pkalka
                                    gt_104-pyear
                                    gt_104-ppoper
                                    gt_all-compn
                           CHANGING gt_all-plifnr
                                    gt_all-kd
* 9/2007 by IG.MOON
                                    gt_all-ekgrp_p
                                    gt_all-okzust_p.
*
  ENDIF.

ENDFORM.                    " GET_PRV
*&---------------------------------------------------------------------*
*&      Form  convert_amt
*&---------------------------------------------------------------------*
*       UoM conversion
*----------------------------------------------------------------------*
*      -->P_PKPEIN   Price of UoM
*      -->P_PMEHT    Input UoM
*      -->P_MEEHT    Output UoM
*      <--P_PGPREIS  Price or Qty
*----------------------------------------------------------------------*
FORM convert_amt USING    p_pkpein TYPE kpein
                          p_pmeht  TYPE meins
                          p_meeht  TYPE meins
                 CHANGING p_gpreis.

  DATA l_output(10) TYPE p DECIMALS 6.

  CLEAR l_output.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
       EXPORTING
            input                = p_pkpein
            unit_in              = p_pmeht
            unit_out             = p_meeht
       IMPORTING
            output               = l_output
       EXCEPTIONS
            conversion_not_found = 1
            division_by_zero     = 2
            input_invalid        = 3
            output_invalid       = 4
            overflow             = 5
            type_invalid         = 6
            units_missing        = 7
            unit_in_not_found    = 8
            unit_out_not_found   = 9.

  IF sy-subrc <> 0.
    BREAK-POINT.
  ENDIF.

* UoM conversion
  p_gpreis = l_output.

ENDFORM.                    " convert_amt
*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_GT_ALL
*&---------------------------------------------------------------------*
*       Move to internal Table GT_ALL
*----------------------------------------------------------------------*
*      -->PA_KALKA   Costing type
*      -->PA_BDATJ   Year
*      -->PA_POPER   Period
*      -->PA_COMPN   Component
*      <--PA_LIFNR   Vendor
*      <--PA_KD      KD
*----------------------------------------------------------------------*
FORM move_to_gt_all USING    pa_kalka  TYPE ck_kalka
                             pa_bdatj  TYPE bdatj
                             pa_poper  TYPE poper
                             pa_compn  TYPE idnrk
                    CHANGING pa_lifnr  TYPE lifnr
                             pa_kd
                             pa_ekgrp
                             pa_okzust.

  DATA: l_amt(15) TYPE p DECIMALS 6,
        l_matnr   TYPE matnr.

* If it has following material, search data by the following material
* else search data by componet of internal table gt_103a
  IF gv_matnr IS INITIAL.
    l_matnr = pa_compn.
  ELSE.
    l_matnr = gv_matnr.
  ENDIF.

* Search data from internale table GT_102
  READ TABLE gt_102 WITH KEY kalka = pa_kalka
                                  bdatj = pa_bdatj
                                  poper = pa_poper
                                  matnr = l_matnr
                         BINARY SEARCH.

* If it has no data select data from table ZTCOU102
  IF sy-subrc <> 0.
    SELECT SINGLE
           bdatj poper kalka matnr a~werks profl a~lifnr land1
           wertn pmeht peinh
           qta    wertn_v1  kzust1      wertn1    kzust2    wertn2
           qta_v2 wertn_v2  kzust1_v2   wertn1_v2 kzust2_v2 wertn2_v2
           ekgrp
      INTO gt_102
      FROM ztcou102 AS a
      JOIN lfa1 AS b
        ON b~lifnr = a~lifnr
*     where bdatj =  pa_bdatj
     WHERE a~kokrs EQ p_kokrs
       AND a~bdatj =  pa_bdatj
       AND a~poper <= pa_poper
       AND a~kalka =  pa_kalka
       AND a~matnr = l_matnr.
  ENDIF.

  IF sy-subrc = 0.
*   Purchasing item
    pa_lifnr = gt_102-lifnr.  " Vendor

*   Purchasing Group
    pa_ekgrp = gt_102-ekgrp.

*   Original Reason
    pa_okzust = gt_102-kzust1.

*   KD: when same country key as ABP LDC Rate table ZTCOU116
    READ TABLE gt_ztcou116 WITH KEY kokrs = p_kokrs
                                    bdatj = p_year
                                    land1 = gt_102-land1
                           BINARY SEARCH.
    IF sy-subrc = 0.
      pa_kd = 'X'.                   " KD
    ENDIF.

  ENDIF.

ENDFORM.                    " MOVE_TO_GT_ALL
*&---------------------------------------------------------------------*
*&      Form  get_info_record
*&---------------------------------------------------------------------*
*       search info-record
*----------------------------------------------------------------------*
FORM get_info_record.
  DATA: l_kmein  TYPE kmein,         " UoM
        l_gpreis TYPE zgpreis,       " Price
        l_peinh  TYPE peinh,         " Price Unit
        l_pmeht  TYPE pmeht,         " Price quantity unit
        l_kpein  TYPE kpein,         " Condition pricing unit
        l_matnr  TYPE matnr,         " Material number
        l_not_found TYPE i.

  CLEAR: l_matnr, l_gpreis, l_peinh, l_pmeht, l_kpein, l_not_found.

  IF gv_matnr IS INITIAL.
    l_matnr = gt_103a-compn.
  ELSE.
    l_matnr = gv_matnr.
  ENDIF.

  READ TABLE gt_102 WITH KEY kalka = gv_pkalka
                                  bdatj = gt_104-pyear
                                  poper = gt_104-ppoper
                                  matnr = l_matnr
                         BINARY SEARCH.

  IF sy-subrc = 0.
    gt_all-pkpein  = gt_102-peinh.
    IF gt_102-pmeht = gt_all-meeht.
      gt_all-pgpreis = gt_102-wertn.
    ELSE.
      PERFORM convert_unit USING gt_102-wertn
                                 gt_102-pmeht
                                 gt_all-meeht
                        CHANGING gt_all-pwertn.
    ENDIF.

  ELSE.
    l_not_found = 1.
*---exception---  FIXME ANDY
*    SELECT SINGLE LIFNR WERTN PEINH PMEHT
*      INTO (GT_102-LIFNR, GT_102-WERTN,
*            GT_102-PEINH, GT_102-PMEHT)
*      FROM ZTCOU102
*     WHERE KOKRS = P_KOKRS
*       AND BDATJ = P_YEAR
*       AND POPER = P_POPER
*       AND KALKA = P_KALKA
*       AND MATNR = L_MATNR.

    SELECT SINGLE lifnr wertn peinh pmeht
      INTO (gt_102-lifnr, gt_102-wertn,
            gt_102-peinh, gt_102-pmeht)
      FROM ztcou102
     WHERE kokrs = p_kokrs
       AND bdatj = gt_104-pyear
       AND poper = gt_104-ppoper
       AND kalka = gv_pkalka
       AND matnr = l_matnr.
    IF sy-subrc = 0.
      gt_all-pkpein  = gt_102-peinh.
      PERFORM convert_unit USING gt_102-wertn
                                 gt_102-pmeht
                                 gt_all-meeht
                        CHANGING gt_all-pgpreis.
      CLEAR: l_not_found.
    ENDIF.
  ENDIF.

  IF l_not_found = 1. " Do not need!
*    PERFORM GET_INFO USING L_MATNR.
  ENDIF.

ENDFORM.                    " get_info_record
*&---------------------------------------------------------------------*
*&      Form  GET_ZTCOU105
*&---------------------------------------------------------------------*
*       Get following material
*----------------------------------------------------------------------*
FORM get_ztcou105.
  REFRESH gt_ztcou105.
  CLEAR gt_ztcou105.

  SELECT tmatnr fmatnr INTO TABLE gt_ztcou105
    FROM ztcou105
   FOR ALL ENTRIES IN gt_103a
   WHERE kokrs = p_kokrs
     AND tmatnr = gt_103a-compn.

  SORT gt_ztcou105 BY tmatnr.

ENDFORM.                    " GET_ZTCOU105
*&---------------------------------------------------------------------*
*&      Form  CONVERT_UNIT
*&---------------------------------------------------------------------*
FORM convert_unit USING    p_input
                           p_meeht1 TYPE meins
                           p_meeht2 TYPE meins
                  CHANGING p_output.

  DATA l_output(10) TYPE p DECIMALS 6.

  CLEAR l_output.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
       EXPORTING
            input                = p_input
            unit_in              = p_meeht1
            unit_out             = p_meeht2
       IMPORTING
            output               = l_output
       EXCEPTIONS
            conversion_not_found = 1
            division_by_zero     = 2
            input_invalid        = 3
            output_invalid       = 4
            overflow             = 5
            type_invalid         = 6
            units_missing        = 7
            unit_in_not_found    = 8
            unit_out_not_found   = 9.

* UoM conversion
  p_output = l_output.

ENDFORM.                    " CONVERT_UNIT
*&---------------------------------------------------------------------*
*&      Form  GET_INFO
*&---------------------------------------------------------------------*
FORM get_info USING p_matnr TYPE matnr.
  TYPES: BEGIN OF ty_lifnr,
           lifnr TYPE lifnr,
         END OF ty_lifnr.

  TYPES: BEGIN OF ty_a018,
           matnr TYPE matnr,
           lifnr TYPE lifnr,
           datbi TYPE kodatbi,
           datab TYPE kodatab,
           knumh TYPE knumh,
         END OF ty_a018.

  TYPES: BEGIN OF ty_konp,
           kschl TYPE kscha,
           kbetr TYPE kbetr_kond,
           kpein TYPE kpein,
           kmein TYPE kmein,
           kzust TYPE kzust,
         END OF ty_konp.

  DATA: lt_lifnr TYPE TABLE OF ty_lifnr WITH HEADER LINE,
        lt_a018  TYPE TABLE OF ty_a018  WITH HEADER LINE,
        lt_konp  TYPE TABLE OF ty_konp  WITH HEADER LINE,
        l_date   TYPE sydatum,       " Date
        l_kbter  TYPE kbetr_kond,    " Rate
        l_kmein  TYPE kmein,         " UoM
        l_gpreis TYPE zgpreis,
        l_peinh  TYPE peinh,
        l_pmeht  TYPE pmeht,
        l_kpein  TYPE kpein,
        l_kzust  TYPE kzust,
        l_cnt    TYPE i.

  RANGES r_lifnr FOR a018-lifnr.

  CLEAR: lt_lifnr, r_lifnr, lt_a018.
  REFRESH: lt_lifnr, r_lifnr, lt_a018.

* Get vendor ranges
  SELECT lifnr INTO TABLE lt_lifnr
    FROM eina
     WHERE matnr = p_matnr.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  r_lifnr-sign = 'I'.
  r_lifnr-option = 'EQ'.

  LOOP AT lt_lifnr.
    r_lifnr-low = lt_lifnr-lifnr.
    APPEND r_lifnr.
  ENDLOOP.

* Get date: previous month of Valuation date
  CLEAR l_date.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
       EXPORTING
            date      = gt_103a-bwdat
            days      = '00'
            months    = '01'
            signum    = '-'
            years     = '00'
       IMPORTING
            calc_date = l_date.

* Get condition no.
  SELECT matnr lifnr datbi datab knumh
    INTO TABLE lt_a018
    FROM a018
   WHERE kappl = 'M'                " Purchasing
     AND kschl = 'PB00'             " ZTIR = PB00
     AND lifnr IN r_lifnr
     AND matnr = p_matnr
     AND ekorg = c_ekorg
     AND esokz = '0'                " Standard
     AND datab =< l_date
     AND datbi >= l_date.

  IF sy-subrc = 0.
    SORT lt_a018 BY datab  DESCENDING.

    CLEAR: l_kmein, l_kbter, l_kpein, l_cnt.

    READ TABLE lt_a018 INDEX 1.

    SELECT konp~kschl konp~kbetr konp~kpein konp~kmein konh~kzust
      INTO TABLE lt_konp
      FROM konp
      INNER JOIN konh
         ON konh~knumh = konp~knumh
     WHERE konp~knumh = lt_a018-knumh
       AND konp~kappl = 'M'
     AND ( konp~kschl = 'PB00' OR        " Gross price
           konp~kschl = 'ZTIR' ).

    READ TABLE lt_konp WITH KEY kschl = 'PB00'.
    IF sy-subrc = 0.
      l_kmein = lt_konp-kmein.
    ENDIF.

    LOOP AT lt_konp.
      l_kbter = l_kbter + lt_konp-kbetr.
      l_kpein = l_kpein + lt_konp-kpein.
      l_cnt   = l_cnt + 1.
      l_kzust = lt_konp-kzust.
    ENDLOOP.

    IF l_cnt <> 0.
      l_kpein = l_kpein / l_cnt.
    ENDIF.

    gt_all-pkpein  = l_kpein.
    gt_all-plifnr  = lt_a018-lifnr.

    IF l_kmein = gt_all-pmeeht.
      gt_all-pgpreis = l_kbter.
    ELSE.
      PERFORM convert_unit USING l_kbter
                                 l_kmein
                                 gt_all-meeht
                        CHANGING gt_all-pgpreis.
    ENDIF.

*-- append back to 102 table
    gt_102-bdatj = gt_104-pyear.
    gt_102-poper = gt_104-ppoper.
    gt_102-kalka = gt_104-pkalka.

    gt_102-matnr = p_matnr.
    gt_102-lifnr = lt_a018-lifnr.
    gt_102-wertn = l_kbter.
    gt_102-pmeht = l_kmein.
    gt_102-peinh = l_kpein.
    gt_102-qta      = 100.
    gt_102-wertn_v1 = l_kbter.
    gt_102-kzust1   = l_kzust.
    APPEND gt_102.

    SORT gt_102 BY kalka bdatj poper matnr.
  ENDIF.

ENDFORM.                    " GET_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_PRV_PRICE
*&---------------------------------------------------------------------*
FORM get_prv_price.
  gt_all-pwertn = gt_103a-wertn.         " Prv.Price
  gt_all-pkpein = gt_103a-peinh.         " Price unit

  IF gt_103a-meeht <> gt_all-meeht.
    PERFORM convert_unit USING gt_103a-menge
                               gt_103a-meeht
                               gt_all-meeht
                      CHANGING gt_all-pmenge.

    PERFORM convert_unit USING gt_103a-gpreis
                               gt_103a-meeht
                               gt_all-meeht
                         CHANGING gt_all-pgpreis.
  ELSE.
    gt_all-pmenge  = gt_103a-menge.      " Prv.Qty
    gt_all-pgpreis = gt_103a-gpreis.     " Unit Price
    gt_all-pmeeht  = gt_103a-meeht.      " Prv.UoM
  ENDIF.

  gt_all-mip = gt_103a-stkkz.             " MIP

ENDFORM.                    " GET_PRV_PRICE
*&---------------------------------------------------------------------*
*&      Form  merge_gt_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_gt_all.

  DATA $ix LIKE sy-tabix.
  DATA from_matnr LIKE gt_all-compn. " for Following Parts.
  DATA to_matnr LIKE gt_all-compn. " for Following Parts.
  DATA : $flag(1).
  DATA $gt_all LIKE gt_all OCCURS 1 WITH HEADER LINE.

  LOOP AT gt_all.
    $ix = sy-tabix.
    from_matnr = gt_all-compn .
    CLEAR to_matnr .

    CHECK gt_all-pmenge <> 0.

    PERFORM get_following_part USING from_matnr
                            CHANGING to_matnr.
    CHECK to_matnr NE space.

    CLEAR $flag.

    $gt_all = gt_all.

    PERFORM check_following_upg TABLES gt_all
                                       $gt_all
                                USING  to_matnr
                             CHANGING $flag.

    IF $flag EQ 'X'.
      gt_all-$compn = to_matnr.
      gt_all-$flag = $flag.
      MODIFY gt_all INDEX $ix TRANSPORTING $compn $flag.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " merge_gt_all
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

  SELECT SINGLE tmatnr INTO p_to_matnr
    FROM ztcou105
   WHERE kokrs = p_kokrs
     AND fmatnr = p_from_matnr.

ENDFORM.                    " GET_FOLLOWING_PART
*&---------------------------------------------------------------------*
*&      Form  check_following_upg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ALL  text
*      -->P_TO_MATNR  text
*      -->P_GT_ALL_UPGVC  text
*      <--P_$FLAG  text
*----------------------------------------------------------------------*
FORM check_following_upg TABLES   p_gt_all STRUCTURE gt_all
                                  p_$gt_all  STRUCTURE gt_all
                         USING    p_to_matnr
                         CHANGING p_flag.

  DATA $ix LIKE sy-tabix.

  LOOP AT p_gt_all WHERE id = p_$gt_all-id
                     AND upgvc(5) = p_$gt_all-upgvc(5).
    $ix = sy-tabix.
    CHECK p_gt_all-compn EQ p_to_matnr.
    CHECK p_gt_all-menge <> 0.
    p_gt_all-$flag  = 'X'.
    p_gt_all-$gpreis = p_$gt_all-pgpreis.
    MODIFY p_gt_all INDEX $ix TRANSPORTING $flag $gpreis.
    p_flag = 'X'.
    EXIT.
  ENDLOOP.

ENDFORM.                    " check_following_upg
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGE
*&---------------------------------------------------------------------*
FORM show_message USING    pf_text1
                           pf_text2.
  DATA: l_text(256) TYPE c.

  CONCATENATE pf_text1 ' ' pf_text2 INTO l_text.

  MESSAGE s000 WITH pf_text1 pf_text2.

*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*       EXPORTING
*            PERCENTAGE = PF_VAL
*            TEXT       = L_TEXT.

ENDFORM.                    " SHOW_MESSAGE
*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_sp_group TYPE slis_t_sp_group_alv,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV
*&---------------------------------------------------------------------*
*&      Form  display_result
*&---------------------------------------------------------------------*
FORM display_result.

  PERFORM field_setting TABLES gt_fieldcat USING :
'ID'	   'ID'            '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'POPER'   'Period'        '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGVC'   'UPG-VC'        '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'COMPN'   'BOM component' '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KSTAR'   'CstElement'    '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LIFNR'   'Vendor'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'EKGRP'   'PurGrp'        '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PROFL'   'LP/KD/MIP'     '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'DMENGE'  'Diff.Qty'      '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'DWERTN'  'Diff.Value'    '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'ZRCLSS'  'Class'         '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KZUST1'  'Rsn 1'         '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'WERTN1'  'Rsn 1$'        '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KZUST2'  'Rsn 2'         '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'WERTN2'  'Rsn 2$'        '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KZUST3'  'Rsn 3'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'WERTN3'  'Rsn 3$'        '15' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KZUST'   'RsnCd'         '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OKZUST'  'Org RSN'       '03' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PMENGE'  'Prev Qty'      '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PWERTN'  'Prev Info'     '15' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MENGE'   'Quantity'      '15' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'WERTN'   'Overall$'      '15' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KPEIN'   'PrcUnit'       '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MEEHT'   'UoM'           '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = gt_out
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.


ENDFORM.                    " display_result
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  fill_header_data
*&---------------------------------------------------------------------*
FORM fill_header_data.

*   MOVE-CORRESPONDING gt_all TO gt_out.
  gt_outwk-kokrs = p_kokrs.   " Controling area
  gt_outwk-kalka = p_kalka.
  gt_outwk-bdatj = p_year.
  gt_outwk-poper = p_poper.  "gv_poper.

*   ID infomation
  READ TABLE gt_104 WITH KEY id = gt_all-id BINARY SEARCH.
  gt_outwk-id    = gt_all-id.

  gt_outwk-upgvc = gt_all-upgvc.
  gt_outwk-compn = gt_all-compn.
  gt_outwk-kstar = gt_all-kstar.
  gt_outwk-profl = gt_all-profl.

  gt_outwk-okzust  = gt_all-okzust_c.

* by IG.MOON 9/2007
  IF gt_all-ekgrp_c NE space.
    gt_outwk-ekgrp = gt_all-ekgrp_c.
  ELSE.
    gt_outwk-ekgrp = gt_all-ekgrp_p.
  ENDIF.
*
ENDFORM.                    " fill_header_data
*&---------------------------------------------------------------------*
*&      Form  check_lock
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_KOKRS  text
*      -->P_GT_OUT_BDATJ  text
*      -->P_GT_OUT_KALKA  text
*      -->P_GT_OUT_ID  text
*      -->P_P_POPER  text
*      <--P_LOCKED  text
*----------------------------------------------------------------------*
FORM check_lock USING    p_kokrs
                         p_bdatj
                         p_kalka
                         p_id
                         p_poper
                CHANGING p_locked.

  CLEAR p_locked.

  SELECT SINGLE * FROM ztcou106
                       WHERE kokrs = p_kokrs
                         AND bdatj = p_bdatj
                         AND kalka = p_kalka
                         AND id    = p_id
                         AND poper = p_poper.

  IF sy-subrc EQ 0.
    SELECT SINGLE * INTO *ztcou104lock
          FROM ztcou104lock
    WHERE kokrs EQ p_kokrs
      AND bdatj EQ p_bdatj
      AND kalka EQ p_kalka
      AND id    EQ p_id
      AND (atab).
    IF sy-subrc EQ 0.
      p_locked = true.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_lock
