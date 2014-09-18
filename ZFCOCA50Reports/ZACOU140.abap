*----------------------------------------------------------------------
* Program ID        : ZACOU140
* Title             : [CO] Create Costing Itemization
* Created on        : 04/11/2008
* Created by        : IG.MOON
* Specifications By : Andy Choi
*----------------------------------------------------------------------
*
* Mixed costing notes
*   - if mixed ratio change after cost estimates,
*     new mixed ratio will be used.
*
* TO-DO
*   . color part problem, use alt.color for module??? UA???
*
REPORT zacou_ck11 NO STANDARD PAGE HEADING LINE-SIZE 125
                  MESSAGE-ID zmco.

INCLUDE zacoui00_new.
INCLUDE zacou140_top.

DATA: l_cousertype(3) TYPE c.
*----------------------------------------------------------------------*
* Select-Options & Parameters
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
PARAMETERS: p_kokrs LIKE keko-kokrs OBLIGATORY MEMORY ID cac,
            p_year  LIKE keko-bdatj OBLIGATORY MEMORY ID bdtj,
            p_poper LIKE keko-poper OBLIGATORY MEMORY ID popr.
PARAMETERS p_klvar LIKE keko-klvar OBLIGATORY MEMORY ID krt.

SELECTION-SCREEN END OF BLOCK b0.

* Block: Costing types
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
SELECT-OPTIONS: s_artnr FOR keko-matnr.           " Material
SELECT-OPTIONS: s_kalst FOR keko-kalst.           " Level

SELECTION-SCREEN END OF BLOCK b1.


PARAMETER: p_test  AS CHECKBOX DEFAULT 'X'.
PARAMETER: p_mix      AS CHECKBOX.  "Use mixed costing
PARAMETER: p_ref      AS CHECKBOX.

PARAMETER: p_bom2(1) TYPE c NO-DISPLAY.

SELECT-OPTIONS: s_bkmat FOR keko-matnr.           " break-point


*Item category; default = M, I
SELECT-OPTIONS: s_typps FOR ckis-typps NO-DISPLAY.


DATA: p_mold(1) TYPE c.  "old module bom???

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  PERFORM check_screen.

INITIALIZATION.
  GET PARAMETER ID 'ZCOLV1' FIELD l_cousertype.
  s_typps = 'IEQM'.  APPEND s_typps.
  s_typps = 'IEQI'.  APPEND s_typps.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_kokrs.
  PERFORM get_t030.

  PERFORM get_products.  " Get FSC / MIP / Module
  PERFORM get_mixing_ratio.

* rollup using from level 1...
  PERFORM roll_up.

*----------------------------------------------------------------------*
* End of selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF p_test = 'X'.
    PERFORM display_result.
  ELSE.
    PERFORM save_roll_up.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_products
*&---------------------------------------------------------------------*
*       Get Costing Information
*&---------------------------------------------------------------------*
FORM get_products.

  DATA lt_keko TYPE TABLE OF ty_ckiuser WITH HEADER LINE.

  __cls : f_ckiuser,lt_keko. ", F_CKIUSER1.

  " KALAID ; naming rule = costing type(2) + M/F

* index: KLVAR/BDATJ/POPER/MATNR
  SELECT

      a~bzobj a~kalnr a~kalka
      a~kadky a~tvers a~bwvar
      a~matnr a~werks a~bwkey
      a~bwtar a~kokrs a~kadat
      a~aldat a~verid a~stlan
      a~stalt a~losgr a~meins
      a~disst
      a~kalst a~mgtyp a~bwdat
      a~poper a~bdatj a~btyp
      a~misch_verh a~bwvar_ba a~klvar
      a~mkalk a~baltkz a~kalaid
      a~kokrs a~hwaer

  INTO CORRESPONDING FIELDS OF TABLE lt_keko
    FROM keko AS a
    JOIN marc AS b
      ON b~matnr EQ a~matnr
     AND b~werks EQ a~werks
   WHERE a~klvar EQ p_klvar
     AND a~bdatj = p_year
     AND a~poper = p_poper
     AND a~matnr IN s_artnr
     AND a~sobes <> '7'         "exclude stock trf.
     AND b~fevor <> ''
     AND ( b~beskz = 'E' OR b~beskz = 'X' )
     AND ( a~kalst >= 1 AND a~kalst IN s_kalst )
     and A~KOKRS = P_KOKRS.

  IF sy-subrc = 0.
*   Stock Trf.
    LOOP AT lt_keko.

      IF lt_keko-mkalk = 'X'. CONTINUE.  ENDIF.
      IF lt_keko-baltkz = 'X'.
        MOVE-CORRESPONDING lt_keko TO f_mixed.
        APPEND f_mixed.
      ENDIF.

      MOVE-CORRESPONDING lt_keko TO f_ckiuser.
      IF f_ckiuser-kalaid IS INITIAL.
        f_ckiuser-kalaid = 'ZZZZZZZZ' .
      ENDIF.

      APPEND f_ckiuser.
      CLEAR f_ckiuser.
    ENDLOOP.
  ENDIF.

  SORT f_ckiuser.
  DELETE ADJACENT DUPLICATES FROM f_ckiuser.

  SORT f_ckiuser BY disst DESCENDING
                    kalaid kalst ASCENDING .    " Sort by level

  DESCRIBE TABLE f_ckiuser LINES sy-subrc.
  IF sy-subrc = 0.
    MESSAGE s000 WITH 'No data found.'.
    EXIT.
  ENDIF.

ENDFORM.                    " get_products
*&---------------------------------------------------------------------*
*&      Form  GET_T_KIS1
*&---------------------------------------------------------------------*
*       Get FSC Items
*----------------------------------------------------------------------*
FORM get_t_kis1.

  DATA $ix LIKE sy-tabix.

  __cls t_kis1.

  SELECT a~kalka a~kadky a~tvers a~bwvar a~posnr a~typps a~kstar
         a~matnr a~gpreis
         a~peinh
         a~menge a~meeht a~ukaln
         b~kalnr a~werks a~baugr a~wertn c~fevor
         d~pvprs AS verpr
    INTO CORRESPONDING FIELDS OF TABLE t_kis1
       FROM ckis AS a
            INNER JOIN ckmlhd AS b
               ON b~matnr EQ a~matnr
              AND b~bwkey EQ a~werks
            INNER JOIN marc AS c
               ON c~matnr EQ a~matnr
              AND c~werks EQ a~werks
* { ? by - ig.moon
         INNER JOIN ckmlcr AS d
            ON d~kalnr = b~kalnr
           AND d~bdatj = p_year
           AND d~poper = p_poper
           AND d~untper = '000'
           AND d~curtp = '10'
* }
       WHERE a~lednr = '00'             " Standard ledger
         AND a~bzobj = f_ckiuser-bzobj
         AND a~kalnr = f_ckiuser-kalnr
         AND a~kalka = f_ckiuser-kalka
         AND a~kadky = f_ckiuser-kadky
         AND a~tvers = gc_tvers
         AND a~bwvar = f_ckiuser-bwvar
         AND ( a~typps = 'M' OR a~typps = 'I' ).

ENDFORM.                    " GET_T_KIS1
*&---------------------------------------------------------------------*
*&      Form  GET_GT_KEKO
*&---------------------------------------------------------------------*
*       Get Component (inc. FSC)
*----------------------------------------------------------------------*
FORM get_gt_keko.

  __cls gt_keko.

  SELECT kalnr werks bwdat stlan sobes sowrk kalst
    INTO CORRESPONDING FIELDS OF TABLE gt_keko
       FROM keko
       FOR ALL ENTRIES IN t_kis1
       WHERE bzobj = '0'
         AND kalnr = t_kis1-ukaln
         AND kalka = t_kis1-kalka
         AND tvers = t_kis1-tvers
         AND kadky = t_kis1-kadky
         AND bwvar = t_kis1-bwvar.

  SORT gt_keko BY kalnr.

ENDFORM.                    " GET_KEKO
*&---------------------------------------------------------------------*
*&      Form  GET_GT_BOM
*&---------------------------------------------------------------------*
*       Read BOM explosion
*----------------------------------------------------------------------*
FORM get_gt_bom.
  CHECK f_ckiuser-stlan <> space.

  DATA l_capid TYPE capid.

  CLEAR  : stb, gt_bom, l_capid.
  REFRESH: stb, gt_bom.

  SELECT SINGLE a~capid INTO l_capid
    FROM tck19a AS a
    JOIN tck03 AS b
      ON b~aufkz = a~aufkz
   WHERE b~klvar = f_ckiuser-klvar
     AND b~kalka = f_ckiuser-kalka
     AND b~bwvar = f_ckiuser-bwvar.

  IF p_bom2 = 'X'.
* Notes: 729663  (refer LCKSAF0B)
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              capid = l_capid
              datuv = f_ckiuser-aldat
              mtnrv = f_ckiuser-matnr
              werks = f_ckiuser-werks
              stlan = f_ckiuser-stlan
              stlal = f_ckiuser-stalt
              mmory = '1'  "Memory use On(1)
              sanka = 'X'  "Only Costing Relevency(inc.Phantom)
              ftrel = 'X'  "stop explosion not relevant to production
              aumgb = ' '  "calculate scrap
              mdmps = ' '  "Limited multi-lvl- explode phantom at least
              mehrs = 'X'  "Multi-level explosion "Notes 729663 !!!
              rndkz = ' '  "Round off: ' '=always, '1'=never,
              emeng = 1  "Required quantity
         TABLES
              stb   = stb.
  ELSE.
    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              capid = l_capid
              datuv = f_ckiuser-aldat
              mtnrv = f_ckiuser-matnr
              werks = f_ckiuser-werks
              stlan = f_ckiuser-stlan
              stlal = f_ckiuser-stalt
              mmory = '1'  "Memory use On(1)
              sanka = ' '  "Only Costing Relevency(inc.Phantom)
              ftrel = ' '  "stop explosion not relevant to production
              aumgb = ' '  "calculate scrap
              mdmps = 'X'  "Limited multi-lvl- explode phantom at least
              mehrs = 'X'  "Multi-level explosion "Notes 729663 !!!
              rndkz = ' '  "Round off: ' '=always, '1'=never,
              emeng = 1  "Required quantity
         TABLES
              stb   = stb.
  ENDIF.

  LOOP AT stb.
    MOVE-CORRESPONDING stb TO gt_bom.
    APPEND gt_bom.
  ENDLOOP.

  SORT gt_bom BY idnrk.

ENDFORM.                    " GET_GT_BOM
*&---------------------------------------------------------------------*
*&      Form  FILL_ITAB
*&---------------------------------------------------------------------*
*       Itemization
*----------------------------------------------------------------------*
FORM fill_itab.
  DATA l_idx TYPE sytabix.

  CLEAR l_idx.

*  LOOP AT t_kis1.
*    l_idx = sy-tabix.
*
*    t_kis1-kalst = f_ckiuser-kalst.       " Level
*    t_kis1-bwdat = f_ckiuser-bwdat.       " Valuation date
*
*    MODIFY t_kis1 INDEX l_idx TRANSPORTING kalst bwdat.
*  ENDLOOP.

  REFRESH itab.

  LOOP AT t_kis1.
    CHECK t_kis1-typps IN s_typps.
    CLEAR itab.
*   move header info
    PERFORM move_f_ckiuser_to_itab USING f_ckiuser.

*   get component detail
    PERFORM get_compn_info USING t_kis1.

    "default key; in case of UPG blank
    CONCATENATE itab-artnr itab-werks itab-compn
           INTO itab-key.

    APPEND itab. CLEAR itab.

  ENDLOOP.

ENDFORM.                    " FILL_ITAB
*&---------------------------------------------------------------------*
*&      Form  get_upg_prepare
*&---------------------------------------------------------------------*
*       Get Item info
*----------------------------------------------------------------------*
FORM get_upg_prepare.
  CHECK f_ckiuser-stlan <> space.

  DATA l_index TYPE sytabix.
  DATA stkkz TYPE stkkz.
  SORT: gt_bom BY stufe index posnr ASCENDING hdnfo DESCENDING,
        itab BY compn posnr.

  LOOP AT gt_bom WHERE dumps = space. " excluding Phantom
    READ TABLE itab WITH KEY compn = gt_bom-idnrk
                             upgvc = space
                             indx  = 0
                             chk   = space.
    IF sy-subrc = 0.
      l_index = sy-tabix.

      stkkz = itab-stkkz.
      MOVE-CORRESPONDING gt_bom TO itab.
      itab-matkl = gt_bom-matmk.
      itab-indx  = gt_bom-index.
      itab-chk = 'X'.
      MOVE stkkz TO itab-stkkz.

      MODIFY itab INDEX l_index.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_upg_prepare
*&---------------------------------------------------------------------*
*&      Form  GET_UPG
*&---------------------------------------------------------------------*
*       Get UPG
*----------------------------------------------------------------------*
FORM get_upg.
  CHECK f_ckiuser-stlan <> space.

  DATA : l_key(60),
         ln  TYPE i,
         $ix TYPE i.

* get UPG
  SORT : gt_bom BY index stufe ASCENDING,
* UD1K940727  by IG.MOON
         itab BY indx.
* end of UD1K940727

  LOOP AT gt_bom.

    IF gt_bom-stufe = 1.
      w_upg = gt_bom.
    ENDIF.

    READ TABLE itab WITH KEY indx = gt_bom-index BINARY SEARCH.
    $ix = sy-tabix.

    IF sy-subrc = 0.
      itab-stgb  = gt_bom-stgb.

*      IF p_kalka = 'M1' AND p_mold = space.
**---- Module BOM (UPG is derived from BOM component user field)
*        CONCATENATE itab-artnr itab-werks itab-upgvc itab-compn
*               INTO itab-key.
*        MODIFY itab INDEX $ix TRANSPORTING key stgb.
*        "WHERE INDX = GT_BOM-INDEX.
*
*      ELSE.
*---- General BoM
      ln = strlen( w_upg-idnrk ).
      IF ln = 9.
        CONCATENATE w_upg-idnrk(7) '0' w_upg-idnrk+7(2) INTO itab-upgvc.
      ELSE.
        itab-upgvc = w_upg-idnrk.
      ENDIF.
      CONCATENATE itab-artnr itab-werks itab-upgvc itab-compn
             INTO itab-key.
      MODIFY itab INDEX $ix TRANSPORTING key stgb upgvc.
      "WHERE INDX = GT_BOM-INDEX.
*      ENDIF.

    ELSE.
**    skip phantom line.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " GET_UPG
*&---------------------------------------------------------------------*
*&      Form  SAVE_ROLL_UP
*&---------------------------------------------------------------------*
FORM save_roll_up.
  DATA: l_cnt TYPE i,
        l_text1(60),
        l_text2(60),
        l_chk.

  CLEAR: l_cnt, l_text1, l_text2, l_chk.

*  IF p_kalka <> 'BP'.
*    CLEAR p_ver.
*  ENDIF.

  IF p_ref = 'X'.
    DELETE FROM ztco_ck11
     WHERE kokrs = f_ckiuser-kokrs
       AND klvar = p_klvar
       AND bdatj = f_ckiuser-bdatj
       AND poper = f_ckiuser-poper.
    COMMIT WORK.
  ENDIF.

  itab_all-aedat = sy-datum.
  itab_all-aenam = sy-uname.
  itab_all-cputm = sy-uzeit.

  MODIFY itab_all TRANSPORTING aedat aenam cputm WHERE kokrs = p_kokrs.
*  insert ztco_ck11.

 SORT itab_all BY kokrs klvar bdatj poper artnr verid refdt werks compn
        .
  DATA: l_cnt1  TYPE i,
        l_cnt2 TYPE i.

  CLEAR: l_cnt1, l_cnt2.

  DATA: l_idx TYPE i.

  DATA $flag .
  LOOP AT itab_all.

    AT NEW artnr.
      $flag = 'X'.
      CLEAR l_idx .
    ENDAT.

    ADD 1 TO l_idx .
    IF $flag EQ 'X'.
      CLEAR $flag.
      IF p_ref NE 'X'.
        DELETE FROM ztco_ck11
         WHERE kokrs = itab_all-kokrs
           AND klvar = itab_all-klvar
           AND bdatj = itab_all-bdatj
           AND poper = itab_all-poper
           AND artnr = itab_all-artnr.
        COMMIT WORK.
      ENDIF.
    ENDIF.
    MOVE-CORRESPONDING itab_all TO ztco_ck11.
    ztco_ck11-indx = l_idx.

    INSERT ztco_ck11.

    IF sy-subrc = 0.
      l_cnt1 = l_cnt1 + 1.
    ELSE.
      WRITE:/ 'Error saving:',
              itab_all-artnr, itab_all-compn, itab_all-indx.
    ENDIF.

    AT END OF artnr.
      COMMIT WORK.
    ENDAT.

  ENDLOOP.

  COMMIT WORK.

  DESCRIBE TABLE itab_all LINES l_cnt2.

  l_chk = 'X'.
  SET PARAMETER ID 'ZRU' FIELD l_chk.

  WRITE: /'------------------------------------------------------',
         /'Date:', sy-datum, 'Time:', sy-uzeit, 'User:', sy-uname,
         /'------------------------------------------------------',
         /'Controling area:', p_kokrs,
         /'Piscal year:    ', p_year,
         /'Period:         ', p_poper,
         /,
         /'Saved', l_cnt1, 'records.',
         /'------------------------------------------------------'.

ENDFORM.                    " SAVE_ROLL_UP
*&---------------------------------------------------------------------*
*&      Form  MOVE_F_CKIUSER_TO_ITAB
*&---------------------------------------------------------------------*
*       Move Header Info
*----------------------------------------------------------------------*
FORM move_f_ckiuser_to_itab USING f_ckiuser STRUCTURE f_ckiuser.

  itab-klvar = f_ckiuser-klvar.   " Costing type
  itab-kokrs = f_ckiuser-kokrs.   " Controlling Area
  itab-bdatj = f_ckiuser-bdatj.   " Posting year
  itab-poper = f_ckiuser-poper.   " Posting period
  itab-werks = f_ckiuser-werks.   " Plant
  itab-artnr = f_ckiuser-matnr.   " Product number
* itab-kalnr = f_ckiuser-kalnr.   " Cost estimate number
  itab-kadky = f_ckiuser-kadky.   " Costing date (key)
  itab-tvers = f_ckiuser-tvers.   " Costing version
* itab-bwvar = f_ckiuser-bwvar.   " Valuation Variant in Costing
  itab-verid = f_ckiuser-verid.   " Production version

  itab-losgr = f_ckiuser-losgr.   " Costing lot size
  itab-meins = f_ckiuser-meins.   " Costing lot size

  itab-stlan = f_ckiuser-stlan.   " BoM Usg
  itab-kalst = f_ckiuser-kalst.   " Level
  itab-bwdat = f_ckiuser-bwdat.   " Valuation date
  itab-hwaer = f_ckiuser-hwaer.

*Mixed costing
  itab-mkalk      = f_ckiuser-baltkz.
  itab-btyp       = f_ckiuser-btyp.
  itab-misch_verh = f_ckiuser-misch_verh.

  itab-stalt = f_ckiuser-stalt.
  itab-aldat = f_ckiuser-aldat.

*  IF itab-kalka = 'BP'.
*    itab-ver = p_ver.             " BP Version
*  ENDIF.

ENDFORM.                    " MOVE_F_CKIUSER_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_COMPN_INFO
*&---------------------------------------------------------------------*
*       Get component detail
*----------------------------------------------------------------------*
FORM get_compn_info USING p_ckis STRUCTURE t_kis1.
  DATA lv_logsr TYPE menge_kpf.

  IF itab-splnt IS INITIAL.
    itab-splnt = t_kis1-werks.
  ENDIF.

  itab-indx  = t_kis1-indx.
  itab-compn = t_kis1-matnr.         " Component
  itab-typps = t_kis1-typps.
  itab-posnr = t_kis1-posnr.         " BOM Item No.
  itab-reqqt = t_kis1-menge.         " Qty
  itab-meeht = t_kis1-meeht.         " UoM
  itab-kstar = t_kis1-kstar.

  itab-verpr = t_kis1-verpr.

  itab-mtart = t_kis1-mtart.
  itab-matkl = t_kis1-matkl.

* valuation class
  SELECT SINGLE b~bklas  INTO itab-bklas
        FROM ckmlrunperiod AS a
          INNER JOIN ckmlmv011 AS b
           ON  b~laufid = a~run_id
        INNER JOIN ckmlhd AS c
          ON c~kalnr EQ b~kalnr
        WHERE a~gjahr = f_ckiuser-bdatj
          AND a~poper = f_ckiuser-poper
          AND c~matnr = itab-compn
          AND c~bwkey = f_ckiuser-werks .

  IF sy-subrc NE 0.
    SELECT SINGLE bklas INTO itab-bklas
              FROM mbew
        WHERE matnr = itab-compn
          AND bwkey = f_ckiuser-werks .
  ENDIF.

* get price
  PERFORM get_price_end_item.               " case of end item

* determine vendor
  IF t_kis1-fevor IS INITIAL.  "Not production relevant...
    CLEAR: itab-stkkz.
    PERFORM determine_vendor.
  ELSE.
    itab-stkkz = 'X'.
  ENDIF.


ENDFORM.                    " GET_COMPN_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_PRICE_END_ITEM
*&---------------------------------------------------------------------*
*       Get price of end item
*----------------------------------------------------------------------*
FORM get_price_end_item.
* price$
* price unit; default price unit = 1
  itab-peinh = t_kis1-peinh.
  IF t_kis1-peinh = space OR
     t_kis1-peinh = 0 OR
     t_kis1-peinh > 999.

    SELECT SINGLE peinh INTO itab-peinh FROM mbew
     WHERE matnr = itab-compn AND bwkey = itab-werks.

    itab-gpreis = 0.
  ELSE.
    itab-peinh = t_kis1-peinh.
    itab-gpreis = t_kis1-gpreis.

  ENDIF.

  itab-wertn = t_kis1-wertn.

ENDFORM.                    " GET_PRICE_END_ITEM
*---------------------------------------------------------------------*
*       FORM ROLL_UP                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM roll_up.
  DATA: $mixed TYPE i.

* Rollup
  LOOP AT f_ckiuser.

    PERFORM get_product_info USING  f_ckiuser
                                    CHANGING $mixed.
*---skip rollup on mixed costing + parent product
    IF $mixed = 8. CONTINUE. ENDIF.

*    IF P_CHANGE EQ SPACE.

    MESSAGE s000 WITH 'Processing...' f_ckiuser-matnr.

    PERFORM get_t_kis1.             " FSC Items
*    ENDIF.

    IF NOT t_kis1[] IS INITIAL.
      PERFORM get_gt_bom.             " Read BOM explosion
      PERFORM fill_itab.              " Fill to ITAB

      PERFORM get_upg_prepare.          " Item info (BOM)
      PERFORM get_upg.                " UPG from higher level(BOM)

      PERFORM move_to_itab_all.

    ENDIF.

  ENDLOOP.


ENDFORM.                    " roll_up
*&---------------------------------------------------------------------*
*&      Form  get_t030
*&---------------------------------------------------------------------*
FORM get_t030.

  SELECT SINGLE ktopl INTO tka01-ktopl
    FROM tka01
     WHERE kokrs = p_kokrs.

* pool table
  REFRESH it_t030.

  SELECT bklas konts
    INTO TABLE it_t030
    FROM t030
      WHERE ktopl = tka01-ktopl
        AND ktosl = 'GBB'
        AND bwmod = '0001'
        AND komok = 'VBR'.

  SORT it_t030 BY bklas.

ENDFORM.                                                    " get_t030
*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_ITAB_ALL
*&---------------------------------------------------------------------*
FORM move_to_itab_all.
  DATA: l_menge TYPE menge_pos,     " Qty
        l_wertn TYPE zwertn,        " Price
        l_duty  TYPE zduty1,        " Duty
        l_frg   TYPE zfrg1,         " Freight
        l_oth   TYPE zoth1.         " Other
  DATA $flag.
  CLEAR: l_menge, l_wertn, l_duty, l_frg, l_oth.

  SORT itab BY key.

  LOOP AT itab.
    MOVE-CORRESPONDING itab TO itab_all.

    l_menge = l_menge + itab-reqqt.
    l_wertn = l_wertn + itab-wertn.

*    l_duty  = l_duty + itab-duty.
*    l_frg   = l_frg  + itab-frg.
*    l_oth   = l_oth  + itab-oth.

    IF itab-upgvc IS INITIAL AND f_ckiuser-btyp <> 'BB'.
      WRITE:/ '***UPG not found:', itab-artnr, itab-compn.
    ENDIF.

    AT END OF key.
      $flag = 'X'.
    ENDAT.
    CHECK $flag EQ 'X'.
    CLEAR $flag.
    itab_all-reqqt = l_menge * gv_ratio.
    itab_all-wertn = l_wertn * gv_ratio.
    itab_all-total = itab_all-wertn .
    " + itab_all-duty + itab_all-frg + itab_all-oth.


*    itab_all-duty  = l_duty * gv_ratio.
*    itab_all-frg   = l_frg * gv_ratio.
*    itab_all-oth   = l_oth * gv_ratio..


    itab_all-aedat = sy-datum.
    itab_all-aenam = sy-uname.
    itab_all-cputm = sy-uzeit.

    APPEND itab_all.

**--- MIP summary
*    v_ztcou103-artnr = itab-artnr.
*    v_ztcou103-wertn = itab_all-wertn.
**    v_ztcou103-duty  = itab_all-duty.
**    v_ztcou103-frg   = itab_all-frg.
**    v_ztcou103-oth   = itab_all-oth.
*    COLLECT v_ztcou103.  "ANDY

    CLEAR: itab_all,
           l_menge, l_wertn, l_duty, l_frg, l_oth.
  ENDLOOP.

ENDFORM.                    " MOVE_TO_ITAB_ALL
*&---------------------------------------------------------------------*
*&      Form  process_module_color
*&---------------------------------------------------------------------*
FORM process_module_color.

*  DATA: lw_int_key(3),         " Internal Key Color
*        $index LIKE sy-tabix.
*
*  CHECK p_kalka = 'M1' AND p_mold = 'X'.
*
*  LOOP AT itab WHERE stgb = 'U'.
*    $index = sy-tabix.
*    lw_int_key = f_ckiuser-matnr+10(3).
*    SELECT COUNT( * ) INTO sy-dbcnt FROM ztmm_cp_color
*                                    WHERE copit   EQ f_ckiuser-matnr
*                                      AND inkey EQ lw_int_key
*                                      AND submt EQ itab-compn
*                                      AND datab <  f_ckiuser-aldat
*                                      AND datbi >= f_ckiuser-aldat.
*    IF sy-subrc <> 0.
*      DELETE itab INDEX $index.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " process_module_color
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
FORM check_screen.
  IF l_cousertype <> 'ADM'.
    LOOP AT SCREEN.
      IF screen-group1 = 'DIS'.
        screen-invisible = '1'.
        screen-active    = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " check_screen
*&---------------------------------------------------------------------*
*&      Form  get_end_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_end_date.

  SELECT SINGLE lmona INTO tka01-lmona FROM tka01
    WHERE kokrs = p_kokrs.

  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_gjahr        = p_year
            i_periv        = tka01-lmona
            i_poper        = p_poper
       IMPORTING
            e_date         = end_date
       EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.

  CONCATENATE p_year p_poper+1(2) '01' INTO start_date.

ENDFORM.                    " get_end_date

*//////////////////////////////////////////////

*///////////////////////////////////////////// moon
FORM unit_convert USING    p_matnr
                           p_input
                           p_unit_out  "p_to_u
                  CHANGING p_output
                           p_unit_in. "p_from_u.

  DATA $p_result_qty.  " TYPE bstmg.

  IF p_unit_in = p_unit_out.
    p_output = p_input.
  ELSE.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
         EXPORTING
              input                = p_input
              unit_in              = p_unit_in  "from_u
              unit_out             = p_unit_out  "to_u
         IMPORTING
              output               = p_output  "$p_result_qty
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
* check ALT UoM
      DATA : l_umrez_f TYPE umrez,
             l_umrez_t TYPE umrez.

      CLEAR : l_umrez_f,l_umrez_t.

      SELECT SINGLE umrez INTO :
                l_umrez_f FROM marm
               WHERE matnr = p_matnr
               AND meinh = p_unit_in,  "p_from_u,

                l_umrez_t FROM marm
               WHERE matnr = p_matnr
               AND meinh = p_unit_out. "p_to_u.

      IF l_umrez_f <> 0 AND  l_umrez_t <> 0.
        p_output  = p_input * ( l_umrez_f / l_umrez_t ).
        p_unit_in = p_unit_out. "p_to_u.

      ELSE.
* error
        p_output = 0.  "result_qty = 0.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " UNIT_CONVERT
*&---------------------------------------------------------------------*
*&      Form  get_mixing_ratio
*&---------------------------------------------------------------------*
FORM get_mixing_ratio.

  DESCRIBE TABLE f_mixed LINES sy-tabix.
  CHECK sy-tabix > 0.

  SELECT a~werks a~matnr a~btyp a~kalnr
         b~misch_verh
    INTO TABLE gt_mixed
    FROM ckmlmv001 AS a
    INNER JOIN ckmlmv003 AS b
       ON b~werks = a~werks
      AND b~matnr = a~matnr
      AND b~kalnr_in = a~proc_kalnr
    FOR ALL ENTRIES IN f_mixed
     WHERE a~werks = f_mixed-werks
       AND a~matnr = f_mixed-matnr
       AND b~mgtyp = f_mixed-mgtyp
       AND b~gjahr = f_mixed-bdatj
       AND b~perio = f_mixed-poper.

  SORT gt_mixed BY matnr kalnr.

ENDFORM.                    " get_mixing_ratio
*&---------------------------------------------------------------------*
*&      ALV
*&---------------------------------------------------------------------*
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
'ARTNR'      'Product number'    '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'VERID'      'PPver'             '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'WERKS'      'Plant'             '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KSTAR'      'Cost element'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGVC'      'UPG-VC'            '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STKKZ'      'Assy'              '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'COMPN'      'BOM component'     '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LIFNR'      'Vendor'            '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MKALK'      'Mixed'             '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'TYPPS'      'IC'                '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'REQQT'      'Quantity'          '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'GPREIS'     'Unit Price'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PEINH'      'Price unit'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MEEHT'      'UoM'               '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'WERTN'      'Overall value'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'LOSGR'      'Lotsize'           '13' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
*'DUTY'       'Duty'              '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
*'FRG'        'Freight'           '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
*'OTH'        'Other'             '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'SPLNT'      'SuppPlant'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'BTYP'       'Pcat'              '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MISCH_VERH' 'Mix%'              '04' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'AUSSS'      'AssyScrap%'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KAUSF'      'CompScrap%'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'BAUSF'      'AssyScrap%'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KALST'      'CstLvl'            '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'BWDAT'      'Valuation date'    '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STLAN'      'BOM usage'         '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.


  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = itab_all
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
*&      Form  get_product_info
*&---------------------------------------------------------------------*
FORM get_product_info USING f_ckiuser STRUCTURE f_ckiuser
                             CHANGING l_rc LIKE sy-subrc.
  DATA: l_ssr TYPE ck_crossr.

  gv_ratio = 1.
  CLEAR l_rc.

*---skip rollup on mixed costing + parent product
  READ TABLE gt_mixed WITH KEY matnr = f_ckiuser-matnr BINARY SEARCH.
  IF sy-subrc = 0 AND f_ckiuser-baltkz = space.
    l_rc = 8. "CONTINUE.
  ENDIF.

  IF f_ckiuser-baltkz = 'X'.  "mixed costing
    IF p_mix = space.
      gv_ratio = 1.

    ELSE.
      l_ssr = 0.
      LOOP AT gt_mixed WHERE matnr = f_ckiuser-matnr.
        l_ssr = l_ssr + gt_mixed-misch_verh.
      ENDLOOP.

      CLEAR gt_mixed.
      READ TABLE gt_mixed WITH KEY matnr = f_ckiuser-matnr
                                   kalnr = f_ckiuser-kalnr
                          BINARY SEARCH.
      IF sy-subrc EQ 0.
        gv_ratio = gt_mixed-misch_verh / l_ssr.   "TEMP
      ELSE.
        gv_ratio = 0.
      ENDIF.
    ENDIF.
  ENDIF.

** determine old module bom.
*  IF p_kalka = 'M1' AND f_ckiuser-matnr+5(2) = 'M1'.
*    CLEAR p_mold.
*  ELSE.
*    p_mold = 'X'.
*  ENDIF.

ENDFORM.                    " get_product_info
*&---------------------------------------------------------------------*
*&      Form  determine_vendor
*&---------------------------------------------------------------------*
FORM determine_vendor.
  DATA:  lf_bqpim LIKE bqpim,
         lf_bqpex LIKE bqpex,
         l_peinh  TYPE peinh.
  DATA: BEGIN OF it_lifnr OCCURS 21,
          lifnr TYPE lifnr,
          land1 TYPE land1,
        END   OF it_lifnr.
  DATA: l_lifnr TYPE lifnr,
        l_land1 TYPE land1.
  DATA $ix LIKE sy-tabix.

  IF f_ckiuser-kalka NE 'UN'.

    lf_bqpim-matnr = t_kis1-matnr.

    lf_bqpim-werks = t_kis1-werks.
    lf_bqpim-nemng = abs( t_kis1-menge ). "PriceUnit
    lf_bqpim-meins = t_kis1-meeht. "UoM
    lf_bqpim-lmein = t_kis1-meeht. "Units of Measure in WM
    lf_bqpim-nedat = itab_all-bwdat. "Valid date on
    lf_bqpim-bstyp = 'B'.          "Purchase requisition
    lf_bqpim-pstyp = '0'.          "Item category in PO
    lf_bqpim-vorga = 'B'.          "Transaction/event
    lf_bqpim-bqpra = '1'.          "Selection of source with price
    lf_bqpim-noaus = space.        "No box listing sources of supply
    lf_bqpim-liste = 'X'.                                   "553647
    lf_bqpim-beskz = 'F'.         "External procurement
    lf_bqpim-msgno = 'X'.         "keine Nachricht
    lf_bqpim-noquu = 'X'.         "Do not update quota arrangement
    lf_bqpim-novrt = 'X'.         "Do not search for outline agreement
    lf_bqpim-novrt_ord = 'X'.     "Do not search for order ???
    lf_bqpim-nomei = 'X'.         "Always use order unit from info
*lf_bqpim-usequ = uf_mack2-usequ.   "Quotation arrange usage (Space)
    lf_bqpim-matnl = 'X'.         "Do not read material
    lf_bqpim-noqum = 'X'.         "Selection after plan quotation
    CALL FUNCTION 'ME_SEARCH_SOURCE_OF_SUPPLY'
         EXPORTING
              comim = lf_bqpim
         IMPORTING
              comex = lf_bqpex.
    itab-lifnr = lf_bqpex-flief.
    itab-infnr = lf_bqpex-infnr.

  ELSE.
*-- NAFTA purpose
    CLEAR : it_lifnr, it_lifnr[], l_lifnr, l_land1.

    SELECT a~lifnr b~land1 INTO TABLE it_lifnr
                 FROM ztcou137 AS a
                 INNER JOIN lfa1 AS b
                    ON a~lifnr = b~lifnr
                WHERE a~bukrs EQ p_kokrs
                  AND a~matnr EQ itab-compn
                  AND a~zdtfr <= f_ckiuser-bwdat
                  AND a~zdtto >= f_ckiuser-bwdat .

*---multiple vendor - take KD vendor
    READ TABLE it_lifnr INDEX 2.
    IF sy-subrc EQ 0.
      LOOP AT it_lifnr.
        IF it_lifnr-land1 <> 'US'.
          l_lifnr = it_lifnr-lifnr.
          l_land1 = it_lifnr-land1.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF l_lifnr EQ space.
      READ TABLE it_lifnr INDEX 1.
      IF sy-subrc EQ 0.
        l_lifnr = it_lifnr-lifnr.
        l_land1 = it_lifnr-land1.
      ENDIF.
    ENDIF.

    itab-lifnr = l_lifnr.
    itab-land1 = l_land1.
  ENDIF.


ENDFORM.                    " determine_vendor
