*----------------------------------------------------------------------
* Program ID        : ZACOU103_new
* Title             : [CO] Cost Roll-Up - new
* Created on        : 06/11/2007
* Created by        : IG.MOON
* Specifications By : Andy Choi
* Description       : Cost Roll-Up program.
*                     recalculate costing and save itemization.
*----------------------------------------------------------------------
*
* Mixed costing notes
*   - if mixed ratio change after cost estimates,
*     new mixed ratio will be used.
*
* TO-DO
*   . color part problem, use alt.color for module??? UA???
*
*----------------------------------------------------------------------
* Modifications Log
* Date       Developer Request ID    Description
*
* 11/28/2011 Valerian  UQ1K900307    Fix Unit Conversion routine that
*                                    Cause ABAP Dump
* 11/29/2011 Valerian  UQ1K900355    Fix Conversion Logic
*----------------------------------------------------------------------
REPORT zacou103 NO STANDARD PAGE HEADING LINE-SIZE 125
                MESSAGE-ID zmco.

INCLUDE zacoui00_new.
INCLUDE zacou103_new_top.

DATA: l_cousertype(3) TYPE c.
*----------------------------------------------------------------------*
* Select-Options & Parameters
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-001.
PARAMETERS: p_kokrs LIKE keko-kokrs OBLIGATORY MEMORY ID cac,
            p_year  LIKE keko-bdatj OBLIGATORY MEMORY ID bdtj,
            p_poper LIKE keko-poper OBLIGATORY MEMORY ID popr.
PARAMETERS p_kalka LIKE keko-kalka  OBLIGATORY MEMORY ID kka.

* Roll-up only changed materials
PARAMETERS:p_change AS CHECKBOX DEFAULT ' '.
PARAMETER: p_test   AS CHECKBOX DEFAULT 'X'.
PARAMETER: p_ref    AS CHECKBOX.

SELECT-OPTIONS: s_kalst FOR keko-kalst.           " Level

SELECTION-SCREEN END OF BLOCK b0.


* Block: Target Version
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-006.
PARAMETER: p_ver TYPE ztcou102-ver.
*          P_MODE TYPE CHAR01 NO-DISPLAY.  "Locked???
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-003.
PARAMETER: p_ua     AS CHECKBOX DEFAULT ' '.
PARAMETER: p_gr     AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

* Block: Costing types
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-002.
PARAMETERS:    p_sing AS CHECKBOX DEFAULT ' '.
SELECT-OPTIONS: s_artnr FOR keko-matnr.           " Material
*                S_LIFNR FOR CKIS-LIFNR,           " Vendor
*                S_MATNR FOR CKIS-MATNR.           " Component
SELECTION-SCREEN END OF BLOCK b3.



*Batch update
*ARAMETER: p_batch TYPE char01 NO-DISPLAY MODIF ID rp.

* Price from Cost Est,
PARAMETER: p_src      AS CHECKBOX.

*PARAMETER: P_BREAK AS CHECKBOX    MODIF ID DIS.
PARAMETER: p_bom2  TYPE c NO-DISPLAY.

SELECT-OPTIONS: s_bkmat FOR keko-matnr.           " break-point


*Item category; default = M, I
SELECT-OPTIONS: s_typps FOR ckis-typps NO-DISPLAY.


DATA: p_mold(1) TYPE c.  "old module bom???

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  PERFORM check_screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kalka.
  PERFORM popup_kalka USING p_kalka 'P_KALKA'.

INITIALIZATION.
  GET PARAMETER ID 'ZCOLV1' FIELD l_cousertype.
  s_typps = 'IEQM'.  APPEND s_typps.
  s_typps = 'IEQI'.  APPEND s_typps.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_kokrs.

  PERFORM get_gt_100 USING p_kalka.
  CLEAR g_error.

* UD1K941492 by ig.moon 8/28/07
* {
  IF p_ua = 'X'.
    ua_kalka = 'UA'.
    p_kalka = 'U1'.
    PERFORM prep_for_map.
*    __e_exit.
  ELSE.
*}
    IF p_change EQ 'X'.
      PERFORM get_changed_comp.
      IF g_error NE space.
        MESSAGE s000 WITH 'No data found.'.
        EXIT.
      ENDIF.
    ELSE.
      PERFORM get_all_comp.
    ENDIF.

  ENDIF.

  PERFORM get_products.  " Get FSC / MIP / Module
  PERFORM get_mixing_ratio.

  DESCRIBE TABLE f_ckiuser LINES sy-subrc.
  IF sy-subrc = 0.
    MESSAGE s000 WITH 'No data found.'.
    EXIT.
  ENDIF.

*  describe table s_artnr lines sy-subrc.
*  if sy-subrc > 0. P_SING = 'X'. endif.

  PERFORM get_t030.


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


  DESCRIBE TABLE gt_missing LINES sy-tabix.
  IF sy-tabix > 0.
    PERFORM display_missing_log.
  ENDIF.


*  LOOP AT ITAB_ALL WHERE KSTAR IS INITIAL.
*    WRITE: / ITAB_ALL-KALNR, ITAB_ALL-COMPN,
*             ITAB_ALL-UPGVC, ITAB_ALL-MENGE.
*  ENDLOOP.


*&---------------------------------------------------------------------*
*&      Form  get_products
*&---------------------------------------------------------------------*
*       Get Costing Information
*&---------------------------------------------------------------------*
FORM get_products.

  DATA lt_keko TYPE TABLE OF ty_ckiuser WITH HEADER LINE.

  __cls : f_ckiuser,lt_keko. ", F_CKIUSER1.

  CASE p_kalka.

    WHEN 'M1'. " * module costing
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_keko
        FROM keko
       FOR ALL ENTRIES IN gt_100
       WHERE matnr = gt_100-matnr
         AND werks = gt_100-werks
         AND matnr IN s_artnr
         AND kalka EQ p_kalka
         AND tvers = gc_tvers
         AND bdatj = p_year
         AND poper = p_poper
         AND kokrs = p_kokrs.

    WHEN OTHERS.
      " KALAID ; naming rule = costing type(2) + M/F
      IF NOT rt_ztcou103[] IS INITIAL AND p_change EQ 'X'.

* by IG.MOON 10/11/2007 {
*        SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_KEKO
*          FROM KEKO AS A
*          JOIN MARC AS B
*          ON B~MATNR EQ A~MATNR
*          AND B~WERKS EQ A~WERKS
*          FOR ALL ENTRIES IN RT_ZTCOU103
*         WHERE ( A~MATNR IN S_ARTNR AND A~MATNR EQ RT_ZTCOU103-ARTNR )
*           AND A~KALKA EQ P_KALKA
*           AND A~TVERS = GC_TVERS
*           AND A~BDATJ = P_YEAR
*           AND A~POPER = P_POPER
*           AND A~SOBES <> '7'         "exclude stock trf.
*           AND B~FEVOR <> ''
*           AND ( B~BESKZ = 'E' OR B~BESKZ = 'X' )
*           AND ( A~KALST >= 1 AND KALST IN S_KALST )
*           AND A~KOKRS = P_KOKRS.

        SELECT

            a~bzobj a~kalnr a~kalka
            a~kadky a~tvers a~bwvar
            a~matnr a~werks a~bwkey
            a~bwtar a~kokrs a~kadat
            a~aldat a~verid a~stlan
            a~stalt a~losgr a~disst
            a~kalst a~mgtyp a~bwdat
            a~poper a~bdatj a~btyp
            a~misch_verh a~bwvar_ba a~klvar
            a~mkalk a~baltkz a~kalaid

        INTO CORRESPONDING FIELDS OF TABLE lt_keko
          FROM keko AS a
          JOIN marc AS b
          ON b~matnr EQ a~matnr
          AND b~werks EQ a~werks
          FOR ALL ENTRIES IN rt_ztcou103
         WHERE ( a~matnr IN s_artnr AND a~matnr EQ rt_ztcou103-artnr )
           AND a~kalka EQ p_kalka
           AND a~tvers = gc_tvers
           AND a~bdatj = p_year
           AND a~poper = p_poper
           AND a~sobes <> '7'         "exclude stock trf.
           AND b~fevor <> ''
           AND ( b~beskz = 'E' OR b~beskz = 'X' )
           AND ( a~kalst >= 1 AND kalst IN s_kalst )
           AND a~kokrs = p_kokrs.
* }

      ELSE.

* by IG.MOON 10/11/2007 {
*        SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_KEKO
*          FROM KEKO AS A
*          JOIN MARC AS B
*          ON B~MATNR EQ A~MATNR
*          AND B~WERKS EQ A~WERKS
*         WHERE A~MATNR IN S_ARTNR
*           AND A~KALKA EQ P_KALKA
*           AND A~TVERS = GC_TVERS
*           AND A~BDATJ = P_YEAR
*           AND A~POPER = P_POPER
*           AND A~SOBES <> '7'         "exclude stock trf.
*           AND B~FEVOR <> ''
*           AND ( B~BESKZ = 'E' OR B~BESKZ = 'X' )
*           AND ( A~KALST >= 1 AND A~KALST IN S_KALST )
*           AND A~KOKRS = P_KOKRS.

        SELECT

            a~bzobj a~kalnr a~kalka
            a~kadky a~tvers a~bwvar
            a~matnr a~werks a~bwkey
            a~bwtar a~kokrs a~kadat
            a~aldat a~verid a~stlan
            a~stalt a~losgr a~disst
            a~kalst a~mgtyp a~bwdat
            a~poper a~bdatj a~btyp
            a~misch_verh a~bwvar_ba a~klvar
            a~mkalk a~baltkz a~kalaid

        INTO CORRESPONDING FIELDS OF TABLE lt_keko
          FROM keko AS a
          JOIN marc AS b
          ON b~matnr EQ a~matnr
          AND b~werks EQ a~werks
         WHERE a~matnr IN s_artnr
           AND a~kalka EQ p_kalka
           AND a~tvers = gc_tvers
           AND a~bdatj = p_year
           AND a~poper = p_poper
           AND a~sobes <> '7'         "exclude stock trf.
           AND b~fevor <> ''
           AND ( b~beskz = 'E' OR b~beskz = 'X' )
           AND ( a~kalst >= 1 AND a~kalst IN s_kalst )
           AND a~kokrs = p_kokrs.
* }
      ENDIF.

  ENDCASE.

  IF sy-subrc = 0.
*   Stock Trf.
    LOOP AT lt_keko.
      READ TABLE v_db103 WITH KEY artnr = lt_keko-matnr BINARY SEARCH.
      IF sy-subrc = 0 AND v_db103-lock = 'X'.
        CONTINUE.
      ENDIF.

*---- CHECK MIXED COSTING
*      IF LT_KEKO-BALTKZ = 'X' AND LT_KEKO-BTYP = 'BB'.  "Purchasing
*         CONTINUE.
*      ENDIF.
*Mixed costing; FIXME
      IF lt_keko-mkalk = 'X'. CONTINUE.  ENDIF.
      IF lt_keko-baltkz = 'X'.
        MOVE-CORRESPONDING lt_keko TO f_mixed.
        APPEND f_mixed.

*        if lt_keko-btyp = 'BF'. continue. endif.
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

ENDFORM.                    " get_products
*&---------------------------------------------------------------------*
*&      Form  GET_T_KIS1
*&---------------------------------------------------------------------*
*       Get FSC Items
*----------------------------------------------------------------------*
FORM get_t_kis1.

  DATA $ix LIKE sy-tabix.

  __cls t_kis1.

  IF ua_kalka EQ 'UA'.

*    SELECT a~kalka a~kadky a~tvers a~bwvar a~posnr a~typps a~kstar
*           a~matnr a~gpreis
    SELECT a~kalka a~kadky a~tvers a~bwvar
*           a~posnr
           a~typps a~kstar
           a~upgvc
*           a~INDX
           a~kalst
           a~stlan
           a~compn AS matnr
           a~werks
           a~bwdat             "fixme - should be end of month.
           a~gpreis
*          e~losgr AS peinh
           a~peinh
           a~menge a~meeht
           a~stkkz
           a~wertn
*          a~ukaln
           c~kalnr
           c~bwkey
           d~pvprs AS verpr
      INTO CORRESPONDING FIELDS OF TABLE t_kis1
*        FROM ckis AS a
         FROM ztcou103 AS a
         JOIN marc AS e
           ON e~matnr EQ a~compn
          AND e~werks EQ a~werks
         INNER JOIN ckmlhd AS c
          ON c~matnr EQ a~compn
         AND c~bwkey EQ a~werks
         INNER JOIN ckmlcr AS d
            ON d~kalnr = c~kalnr
           AND d~bdatj = p_year
           AND d~poper = p_poper
           AND d~untper = '000'
           AND d~curtp = '10'
         WHERE a~kokrs = p_kokrs
           AND a~bdatj = p_year
           AND a~kalka = f_ckiuser-kalka
           AND a~poper = p_poper
           AND a~artnr = f_ckiuser-matnr.

*         WHERE a~lednr = '00'             " Standard ledger
*           AND a~bzobj = f_ckiuser-bzobj
*           AND a~kalnr = f_ckiuser-kalnr
*           AND a~kalka = f_ckiuser-kalka
*           AND a~kadky = f_ckiuser-kadky
*           AND a~tvers = gc_tvers
*           AND a~bwvar = f_ckiuser-bwvar
*           AND ( a~typps = 'M' OR a~typps = 'I' ).
* }

*    IF sy-subrc EQ 0.
*      LOOP AT t_kis1.
*        $ix = sy-tabix.
*        t_kis1-gpreis = t_kis1-verpr.
*        t_kis1-wertn  = t_kis1-menge * t_kis1-verpr.
*        MODIFY t_kis1 INDEX $ix TRANSPORTING gpreis wertn.
*      ENDLOOP.
*    ENDIF.

*    PERFORM get_102_for_ua.  "delete this line...

  ELSE.

    SELECT a~kalka a~kadky a~tvers a~bwvar a~posnr a~typps a~kstar
           a~matnr a~gpreis
           a~peinh
           a~menge a~meeht a~ukaln
           b~kalnr a~werks a~baugr a~wertn
      INTO CORRESPONDING FIELDS OF TABLE t_kis1
         FROM ckis AS a
         INNER JOIN ckmlhd AS b
          ON b~matnr EQ a~matnr
         AND b~bwkey EQ a~werks
         WHERE a~lednr = '00'             " Standard ledger
           AND a~bzobj = f_ckiuser-bzobj
           AND a~kalnr = f_ckiuser-kalnr
           AND a~kalka = f_ckiuser-kalka
           AND a~kadky = f_ckiuser-kadky
           AND a~tvers = gc_tvers
           AND a~bwvar = f_ckiuser-bwvar
           AND ( a~typps = 'M' OR a~typps = 'I' ).

  ENDIF.

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
*&      Form  GT_CKIS
*&---------------------------------------------------------------------*
*       Component Item (exc. FSC)
*----------------------------------------------------------------------*
FORM gt_ckis.
  IF NOT gt_keko[] IS INITIAL.

    __cls gt_ckis.

* UD1K941492 by ig.moon 8/28/07
* {
    IF ua_kalka EQ 'UA'.

      SELECT a~kalnr AS kalnr
             a~kstar AS kstar
             a~hrkft AS hrkft
             a~wertn AS wertn
             a~ukaln AS ukaln
             b~verpr AS verpr
             a~matnr AS matnr
        INTO CORRESPONDING FIELDS OF TABLE  gt_ckis
           FROM ckis AS a
           LEFT OUTER JOIN mbew AS b
             ON  b~kaln1 EQ a~kalnr
            AND  b~matnr EQ a~matnr
           FOR ALL ENTRIES IN gt_keko
           WHERE a~lednr = '00'
             AND a~bzobj = f_ckiuser-bzobj
         AND ( a~kalnr <> f_ckiuser-kalnr AND a~kalnr = gt_keko-kalnr )
             AND a~kalka = f_ckiuser-kalka
             AND a~kadky = f_ckiuser-kadky
             AND a~tvers = gc_tvers
             AND a~bwvar = f_ckiuser-bwvar
             AND ( a~typps = 'M' OR a~typps = 'I' ).
* }

*      SELECT KALNR HRKFT WERTN KSTAR UKALN
*        INTO CORRESPONDING FIELDS OF TABLE  GT_CKIS
*           FROM CKIS
*         FOR ALL ENTRIES IN GT_KEKO
*           WHERE LEDNR = '00'
*             AND BZOBJ = F_CKIUSER-BZOBJ
*       AND ( KALNR <> F_CKIUSER-KALNR AND KALNR = GT_KEKO-KALNR )
*             AND KALKA = F_CKIUSER-KALKA
*             AND KADKY = F_CKIUSER-KADKY
*             AND TVERS = GC_TVERS
*             AND BWVAR = F_CKIUSER-BWVAR
*             AND ( TYPPS = 'M' OR TYPPS = 'I' ).

    ELSE.
      SELECT kalnr hrkft wertn kstar ukaln
        INTO CORRESPONDING FIELDS OF TABLE  gt_ckis
           FROM ckis
         FOR ALL ENTRIES IN gt_keko
           WHERE lednr = '00'
             AND bzobj = f_ckiuser-bzobj
       AND ( kalnr <> f_ckiuser-kalnr AND kalnr = gt_keko-kalnr )
             AND kalka = f_ckiuser-kalka
             AND kadky = f_ckiuser-kadky
             AND tvers = gc_tvers
             AND bwvar = f_ckiuser-bwvar
             AND ( typps = 'M' OR typps = 'I' ).
    ENDIF.

    SORT gt_ckis BY kalnr hrkft.

  ENDIF.

ENDFORM.                    " GT_CKIS
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
*&      Form  GET_ITEM_INFO
*&---------------------------------------------------------------------*
*       Get Item info
*----------------------------------------------------------------------*
FORM get_item_info.
  CHECK f_ckiuser-stlan <> space.

  DATA l_index TYPE sytabix.

  SORT: gt_bom BY stufe index posnr ASCENDING hdnfo DESCENDING,
        itab BY compn posnr.

  LOOP AT gt_bom WHERE dumps = space. " excluding Phantom
    READ TABLE itab WITH KEY compn = gt_bom-idnrk
                             upgvc = space
                             indx = 0
                             chk = space.
    IF sy-subrc = 0.
      l_index = sy-tabix.

      MOVE-CORRESPONDING gt_bom TO itab.
      itab-indx  = gt_bom-index.
      itab-chk = 'X'.

      IF p_kalka = 'M1' AND p_mold = space.
        itab-upgvc = gt_bom-upgn.
        MODIFY itab INDEX l_index TRANSPORTING indx chk upgvc.
      ELSE.
        MODIFY itab INDEX l_index TRANSPORTING indx chk.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_ITEM_INFO
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

      IF p_kalka = 'M1' AND p_mold = space.
*---- Module BOM (UPG is derived from BOM component user field)
        CONCATENATE itab-artnr itab-werks itab-upgvc itab-compn
               INTO itab-key.
        MODIFY itab INDEX $ix TRANSPORTING key stgb.
        "WHERE INDX = GT_BOM-INDEX.

      ELSE.
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
      ENDIF.

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

  IF p_kalka <> 'BP'.
    CLEAR p_ver.
  ENDIF.

  IF p_change NE 'X'.  "changed part only processing
    IF p_ref = 'X'.
      PERFORM delete_103_all.
    ELSE.
      PERFORM delele_103_indv.
    ENDIF.
  ENDIF.

  itab_all-aedat = sy-datum.
  itab_all-aenam = sy-uname.
  itab_all-cputm = sy-uzeit.

  MODIFY itab_all TRANSPORTING aedat aenam cputm WHERE kokrs = p_kokrs.

  DATA: l_cnt1  TYPE i,
        l_cnt2 TYPE i.

  CLEAR: l_cnt1, l_cnt2.

  DATA: l_idx LIKE ztcou103-indx.
  LOOP AT itab_all.
    AT NEW artnr.
      CLEAR l_idx.
    ENDAT.

    IF p_change NE 'X'.  "changed part only processing
      l_idx = l_idx + 1.
      itab_all-indx = l_idx.
      MOVE-CORRESPONDING itab_all TO ztcou103.
* UD1K941492 by ig.moon 8/28/07
* {
      IF ua_kalka EQ 'UA'.
        ztcou103-kalka = ua_kalka.
      ENDIF.
* }
      INSERT ztcou103.
    ELSE.
      SELECT SINGLE * FROM ztcou103
      WHERE kokrs EQ itab_all-kokrs
        AND bdatj EQ itab_all-bdatj
        AND kalka EQ itab_all-kalka
        AND poper EQ itab_all-poper
        AND artnr EQ itab_all-artnr
        AND ver   EQ itab_all-ver
        AND werks EQ itab_all-werks
        AND compn EQ itab_all-compn
        AND indx  EQ itab_all-indx.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING itab_all TO ztcou103.
        UPDATE ztcou103.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.
      l_cnt1 = l_cnt1 + 1.
    ELSE.
      WRITE:/ 'Error saving:',
              itab_all-artnr, itab_all-compn, itab_all-indx.
    ENDIF.

    AT END OF artnr.
      UPDATE ztcou100 SET rstat = 'X'
                          aedat = sy-datum
                          aenam = sy-uname
            WHERE kokrs = p_kokrs
              AND kalka = p_kalka
              AND bdatj = p_year
              AND poper = p_poper
              AND matnr = itab_all-artnr.

      IF sy-subrc <> 0.  "internal entry
        READ TABLE f_ckiuser WITH KEY matnr = itab_all-artnr.
        ztcou100-kokrs = p_kokrs.
        ztcou100-kalka = p_kalka.
        ztcou100-bdatj = p_year.
        ztcou100-poper = p_poper.
        ztcou100-matnr = itab_all-artnr.
        ztcou100-werks = f_ckiuser-werks.
        ztcou100-stlan = f_ckiuser-stlan.
        ztcou100-stlal = f_ckiuser-stalt.
        ztcou100-verid = f_ckiuser-verid.
        ztcou100-aldat = f_ckiuser-aldat.
        ztcou100-bwdat = f_ckiuser-bwdat.
        ztcou100-rstat = 'X'.
        ztcou100-aedat = sy-datum.
        ztcou100-aenam = sy-uname.
        INSERT ztcou100.
      ENDIF.
    ENDAT.

  ENDLOOP.

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

*- save roll up status to 102
  IF NOT gt_102[] IS INITIAL.
    gt_102-roldt = sy-datum.
    gt_102-rolnm = sy-uname.
    gt_102-roltm = sy-uzeit.

    IF p_change EQ 'X'.
      gt_102-stat = 'R'.
      MODIFY gt_102 TRANSPORTING stat roldt rolnm roltm
             WHERE stat = 'C'.
    ELSE.
      gt_102-stat = 'R'.
      MODIFY gt_102 TRANSPORTING stat roldt rolnm roltm
             WHERE stat = 'C'.
      gt_102-stat = 'O'.
      MODIFY gt_102 TRANSPORTING stat roldt rolnm roltm
             WHERE stat <> 'C'.
    ENDIF.

    MODIFY ztcou102 FROM TABLE gt_102.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " SAVE_ROLL_UP
*&---------------------------------------------------------------------*
*&      Form  MOVE_F_CKIUSER_TO_ITAB
*&---------------------------------------------------------------------*
*       Move Header Info
*----------------------------------------------------------------------*
FORM move_f_ckiuser_to_itab USING f_ckiuser STRUCTURE f_ckiuser.

  itab-kalka = f_ckiuser-kalka.   " Costing type
  itab-kokrs = f_ckiuser-kokrs.   " Controlling Area
  itab-bdatj = f_ckiuser-bdatj.   " Posting year
  itab-poper = f_ckiuser-poper.   " Posting period
  itab-werks = f_ckiuser-werks.   " Plant
  itab-artnr = f_ckiuser-matnr.   " Product number
  itab-kalnr = f_ckiuser-kalnr.   " Cost estimate number
  itab-kadky = f_ckiuser-kadky.   " Costing date (key)
  itab-tvers = f_ckiuser-tvers.   " Costing version
  itab-bwvar = f_ckiuser-bwvar.   " Valuation Variant in Costing
  itab-verid = f_ckiuser-verid.   " Production version
  itab-losgr = f_ckiuser-losgr.   " Costing lot size

  itab-stlan = f_ckiuser-stlan.   " BoM Usg
  itab-kalst = f_ckiuser-kalst.   " Level
  itab-bwdat = f_ckiuser-bwdat.   " Valuation date

*Mixed costing
  itab-mkalk      = f_ckiuser-baltkz.
  itab-btyp       = f_ckiuser-btyp.
  itab-misch_verh = f_ckiuser-misch_verh.

  IF itab-kalka = 'BP'.
    itab-ver = p_ver.             " BP Version
  ENDIF.

ENDFORM.                    " MOVE_F_CKIUSER_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_COMPN_INFO
*&---------------------------------------------------------------------*
*       Get component detail
*----------------------------------------------------------------------*
FORM get_compn_info USING p_ckis STRUCTURE t_kis1.
  DATA lv_logsr TYPE menge_kpf.

* case of stock transfer
  IF NOT t_kis1-ukaln IS INITIAL.
    w_ukaln = t_kis1-ukaln.
    PERFORM get_stock_trnsf.
  ELSE.
* UD1K941149 - by IG.MOON
    w_ukaln = t_kis1-kalnr.
*
    itab-splnt = t_kis1-werks.
  ENDIF.

  IF itab-splnt IS INITIAL.
    itab-splnt = t_kis1-werks.
  ENDIF.

*  MOVE-CORRESPONDING t_kis1 TO itab.

* 12/18 {
  itab-indx = t_kis1-indx.         " Component
* }

  itab-compn = t_kis1-matnr.         " Component
  itab-typps = t_kis1-typps.
  itab-posnr = t_kis1-posnr.         " BOM Item No.
  itab-menge = t_kis1-menge.         " Qty
  itab-meeht = t_kis1-meeht.         " UoM
  itab-peinh = t_kis1-peinh.

  IF NOT s_bkmat[] IS INITIAL AND itab-compn IN s_bkmat.
    BREAK-POINT.                                           "#EC NOBREAK
  ENDIF.

*   price unit; default price unit = 1
  IF t_kis1-peinh = space OR
     t_kis1-peinh = 0 OR
     t_kis1-peinh > 999.
    SELECT SINGLE peinh INTO itab-peinh
      FROM mbew
     WHERE matnr = itab-compn
       AND bwkey = itab-werks.
  ELSE.
    itab-peinh = t_kis1-peinh.
  ENDIF.
  IF itab-peinh = 0.
    itab-peinh = 1.
  ENDIF.

**   get price

*-mixed costing; self item - purchasing item
  IF f_ckiuser-baltkz = 'X' AND f_ckiuser-matnr = t_kis1-matnr.
    PERFORM get_price_end_item.               " case of end item

*-normal costing
  ELSE.
    CLEAR v_ztcou103.
    READ TABLE v_ztcou103 WITH KEY artnr = t_kis1-matnr.

    IF sy-subrc <> 0.
      IF p_sing EQ 'X'.
*check existing MIP
        READ TABLE v_db103 WITH KEY artnr = t_kis1-matnr BINARY SEARCH.
        v_ztcou103 = v_db103.
      ENDIF.
    ENDIF.

*   ...MIP
    IF sy-subrc = 0.
      PERFORM get_price_mip USING t_kis1.       " MIP
    ELSE.
*   ...End Part
*///////////////////////////////////////////////////////////////
      PERFORM get_price_end_item.               " case of end item
*///////////////////////////////////////////////////////////////
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_COMPN_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_STOCK_TRNSF
*&---------------------------------------------------------------------*
FORM get_stock_trnsf.

  READ TABLE gt_keko WITH KEY kalnr = w_ukaln BINARY SEARCH.
*  ITAB-SPLNT = GT_KEKO-WERKS.    " Def.Supply plant

* case of stock transfer, get source plant data
  IF gt_keko-sobes = '7'. " Stock Trf
    itab-splnt = gt_keko-sowrk.   " Trf From Plant

* get detail info from supply plant
    READ TABLE gt_ckis INTO w_ckis WITH KEY kalnr = t_kis1-ukaln
                                   BINARY SEARCH.
    IF sy-subrc = 0.
      w_ukaln = w_ckis-ukaln.
    ENDIF.

    READ TABLE gt_keko WITH KEY kalnr = w_ukaln BINARY SEARCH.
    IF sy-subrc <> 0.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE gt_keko
           FROM keko
           WHERE bzobj = f_ckiuser-bzobj
             AND kalnr = w_ukaln
             AND kalka = f_ckiuser-kalka
             AND kadky = f_ckiuser-kadky
             AND tvers = f_ckiuser-tvers
             AND bwvar = f_ckiuser-bwvar
             AND klvar = f_ckiuser-klvar
             AND werks = itab-splnt.
      SORT gt_keko BY kalnr.
*    ENDIF.
*
*    READ TABLE GT_CKIS WITH KEY KALNR = W_UKALN BINARY SEARCH.
*    IF SY-SUBRC <> 0.
      SELECT kalnr kstar hrkft wertn ukaln
        APPENDING CORRESPONDING FIELDS OF TABLE gt_ckis
           FROM ckis
           WHERE lednr = '00'             "Standard ledger
             AND bzobj = f_ckiuser-bzobj
             AND kalnr = w_ukaln
             AND kalka = f_ckiuser-kalka
             AND kadky = f_ckiuser-kadky
             AND tvers = f_ckiuser-tvers
             AND bwvar = f_ckiuser-bwvar
             AND ( typps = 'M' OR typps = 'I' ). "FIXME; V
      SORT gt_ckis BY kalnr.
    ENDIF.

** replace with origin source
*    READ TABLE GT_KEKO WITH KEY KALNR = W_UKALN BINARY SEARCH.
*    T_KIS1-STLAN = GT_KEKO-STLAN.
  ENDIF.

ENDFORM.                    " GET_STOCK_TRNSF
*&---------------------------------------------------------------------*
*&      Form  GET_PRICE_END_ITEM
*&---------------------------------------------------------------------*
*       Get price of end item
*----------------------------------------------------------------------*
FORM get_price_end_item.
* price$
  CLEAR gv_changed.

  IF p_src = 'X'.
    PERFORM get_price_from_ckis USING w_ukaln.
  ELSE.
* by IG.MOON 8/24/2007 {
*    IF ua_kalka NE 'UA'.
    PERFORM get_changed_price USING t_kis1-matnr.
*    ENDIF.
  ENDIF.

ENDFORM.                    " GET_PRICE_END_ITEM
*&---------------------------------------------------------------------*
*&      Form  get_price_mip
*&---------------------------------------------------------------------*
*       Get price of upper level item
*----------------------------------------------------------------------*
FORM get_price_mip USING p_ckis STRUCTURE t_kis1.
  itab-gpreis = v_ztcou103-wertn.

  itab-wertn = itab-menge * v_ztcou103-wertn  / itab-peinh.
  itab-duty =  itab-menge * v_ztcou103-duty   / itab-peinh.
  itab-frg =   itab-menge * v_ztcou103-frg    / itab-peinh.
  itab-oth =   itab-menge * v_ztcou103-oth    / itab-peinh.

  itab-kstar = t_kis1-kstar.      " v_ztcou103-kstar.
  itab-stkkz = 'X'.               " Assy

ENDFORM.                    " get_price_mip

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
      IF p_change EQ 'X'.
        PERFORM get_upg_from_ztcou103.

      ELSEIF ua_kalka = 'UA'.
        PERFORM fill_itab_ua.

      ELSE.
        PERFORM get_gt_keko.            " Component (inc. FSC)
        PERFORM gt_ckis.                " Component Item (exc. FSC)

        PERFORM get_gt_bom.             " Read BOM explosion
        PERFORM fill_itab.              " Fill to ITAB

        PERFORM get_item_info.          " Item info (BOM)
        PERFORM get_upg.                " UPG from higher level(BOM)

        PERFORM process_module_color.   " Old Module logic.

      ENDIF.

*     PERFORM CALC_MIP_SUM.           " MIP
      PERFORM move_to_itab_all.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " roll_up
****&-------------------------------------------------------------------
*-
**-
****
****&      Form  calc_mip_sum
****&-------------------------------------------------------------------
*-
**-
****
***FORM CALC_MIP_SUM.
***  DATA: L_WERTN TYPE ZWERTN,
***        L_DUTY  TYPE ZDUTY1,
***        L_FRG   TYPE ZFRG1,
***        L_OTH   TYPE ZOTH1.
***
***
***  CLEAR: L_WERTN, L_DUTY, L_FRG, L_OTH.
***
***  LOOP AT ITAB.
***    L_WERTN = L_WERTN + ITAB-WERTN.
***    L_DUTY = L_DUTY + ITAB-DUTY.
***    L_FRG = L_FRG + ITAB-FRG.
***    L_OTH = L_OTH + ITAB-OTH.
***  ENDLOOP.
***
**** MIP
***  V_ZTCOU103-ARTNR = F_CKIUSER-MATNR.
***  V_ZTCOU103-WERTN = L_WERTN.
****  V_ZTCOU103-STKKZ = 'X'.               "Assy
****  V_ZTCOU103-STLAN = F_CKIUSER-STLAN.
***
***  V_ZTCOU103-DUTY  = L_DUTY.
***  V_ZTCOU103-FRG   = L_FRG.
***  V_ZTCOU103-OTH   = L_OTH.
***
**** temp
****  V_ZTCOU103-MENGE   = ITAB-MENGE.
***
****  APPEND V_ZTCOU103.
***  COLLECT V_ZTCOU103.  "ANDY
***
***
***ENDFORM.                    " calc_mip_sum
*&---------------------------------------------------------------------*
*&      Form  GET_CHANGED_PRICE
*&---------------------------------------------------------------------*
FORM get_changed_price USING p_matnr TYPE matnr.
  DATA: l_text(70).

  CLEAR gt_102.
  READ TABLE gt_102 WITH KEY matnr = p_matnr
       BINARY SEARCH.

  IF sy-subrc <> 0.
    READ TABLE gt_bom WITH KEY idnrk = p_matnr BINARY SEARCH.

    IF sy-subrc EQ 0.
      IF gt_bom-dumps EQ space.
        gt_missing-artnr = f_ckiuser-matnr.
        gt_missing-matnr = p_matnr.
        gt_missing-upgvc = gt_ztcou103-upgvc.
        APPEND gt_missing.
      ENDIF.
    ENDIF.

*    concatenate 'Missing in 102; ' p_matnr into g_txt.
*    call function 'FI_PROGRESS_INDICATOR'
*         EXPORTING
*              text = g_txt.

*    IF P_BREAK = 'X'.
*      BREAK-POINT.                                         "#EC NOBREAK
*    ENDIF.

    PERFORM get_price_from_ckis USING w_ukaln.
  ELSE.

    DATA: l_output(10) TYPE p DECIMALS 3,
          l_convsn     TYPE i.

    CLEAR: l_output, l_convsn.
*if 102 has problem, then 0...
    IF gt_102-peinh = 0.
      gt_102-peinh = 1.
    ENDIF.


*////////////////////////////////////////////////

*    IF GT_102-PMEHT = ITAB-MEEHT.
*      L_OUTPUT = 1.
*      L_CONVSN = 1.
*    ELSE.
*
*      L_CONVSN = 100. "//// ???????????? ////
*
*      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
*           EXPORTING
*                INPUT                = GT_102-PEINH
*                UNIT_IN              = GT_102-PMEHT
*                UNIT_OUT             = ITAB-MEEHT
*           IMPORTING
*                OUTPUT               = L_OUTPUT
*           EXCEPTIONS
*                CONVERSION_NOT_FOUND = 1
*                DIVISION_BY_ZERO     = 2
*                INPUT_INVALID        = 3
*                OUTPUT_INVALID       = 4
*                OVERFLOW             = 5
*                TYPE_INVALID         = 6
*                UNITS_MISSING        = 7
*                UNIT_IN_NOT_FOUND    = 8
*                UNIT_OUT_NOT_FOUND   = 9.
*      IF SY-SUBRC <> 0.
*        BREAK-POINT.                                       "#EC NOBREAK
*      ENDIF.
*
*    ENDIF.

* by IG.MOON 9/5/2007 {
*
** On 04/08/14
    IF gt_102-pmeht <> itab-meeht.
      PERFORM unit_convert USING  p_matnr
                                  gt_102-peinh
                                  gt_102-pmeht
                         CHANGING l_output
                                  itab-meeht.
    ELSE.
      l_output = 1.
    ENDIF.
    IF   l_output = 0.
      CONCATENATE 'Unit Conv Err' p_matnr gt_102-pmeht
            itab-meeht INTO l_text SEPARATED BY space.
      MESSAGE e000 WITH l_text.
    ENDIF.

*    gt_102-peinh = l_output.
    l_output = l_output * gt_102-peinh.
    l_convsn = gt_102-peinh.

*   Unit price
    itab-gpreis = ( gt_102-wertn +
                    gt_102-duty + gt_102-frg + gt_102-oth )."/ L_OUTPUT.
    itab-peinh  = gt_102-peinh.

*   if different price of end-item
    gv_changed = 'X'.

*   Price
    itab-wertn = itab-menge * itab-gpreis / l_output.

*   Duty
    itab-duty = itab-menge * gt_102-duty /  l_output.

*   Freight
    itab-frg = itab-menge * gt_102-frg /  l_output.

*   Other
    itab-oth = itab-menge * gt_102-oth /  l_output.

*    IF gt_102-pmeht <> itab-meeht.
*      PERFORM unit_convert USING  p_matnr
*                                  gt_102-peinh
*                                  gt_102-pmeht
*                         CHANGING gt_102-peinh
*                                  itab-meeht.
*    ENDIF.
*
*    l_output = gt_102-peinh.
*    l_convsn = gt_102-peinh.
** }
*
**////////////////////////////////////////////////
*
**   Unit price
*    itab-gpreis = ( gt_102-wertn +
*                    gt_102-duty + gt_102-frg + gt_102-oth )."/ L_OUTPUT.
*    itab-peinh  = gt_102-peinh.
*
**   if different price of end-item
*    gv_changed = 'X'.
*
**   Price
*    itab-wertn = itab-menge * itab-gpreis / itab-peinh.
*
**   Duty
*    itab-duty = itab-menge * gt_102-duty / itab-peinh.
*
**   Freight
*    itab-frg = itab-menge * gt_102-frg / itab-peinh.
*
**   Other
*    itab-oth = itab-menge * gt_102-oth / itab-peinh.
** End on 04/08/14

*FIXME!!!!! - ANDY !!!
    READ TABLE it_t030 WITH KEY bklas = gt_102-bklas.
    IF sy-subrc = 0.
      itab-kstar = it_t030-konts.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_CHANGED_PRICE
*&---------------------------------------------------------------------*
*&      Form  get_price_from_ckis
*&---------------------------------------------------------------------*
FORM get_price_from_ckis USING f_kaln.
  DATA l_wertn TYPE ck_kwt.

  CLEAR : l_wertn .

  LOOP AT gt_ckis WHERE kalnr = f_kaln.
    CASE gt_ckis-hrkft.
      WHEN 'KD-D'.
        itab-duty = t_kis1-menge * gt_ckis-wertn / itab-peinh.
      WHEN 'KD-F'.
        itab-frg = t_kis1-menge * gt_ckis-wertn / itab-peinh.
      WHEN 'KD-O'.
        itab-oth = t_kis1-menge * gt_ckis-wertn / itab-peinh.
      WHEN OTHERS.
        l_wertn = t_kis1-menge * gt_ckis-wertn / itab-peinh.
    ENDCASE.
    itab-wertn = l_wertn + itab-duty + itab-frg + itab-oth.
  ENDLOOP.

* UD1K941163 - by IG.MOON {
  CHECK itab-wertn IS INITIAL.
  itab-wertn = t_kis1-wertn.
* }
ENDFORM.                    " get_price_from_ckis
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
*&      Form  GET_GT_100
*&---------------------------------------------------------------------*
FORM get_gt_100 USING $p_kalka.

  __cls gt_100.

  SELECT * INTO TABLE gt_100
    FROM ztcou100
   WHERE kokrs = p_kokrs
     AND kalka EQ $p_kalka
     AND bdatj = p_year
     AND poper = p_poper.
  IF sy-dbcnt > 0.
*    READ TABLE GT_100 WITH KEY LSTAT = SPACE.
*    IF SY-SUBRC <> 0.
*      P_MODE = 'L'.
*    ENDIF.
    SORT gt_100 BY matnr.
  ENDIF.

  RANGES: lr_matnr FOR ztcou100-matnr.
  lr_matnr-option = 'EQ'.
  lr_matnr-sign   = 'I'.

  LOOP AT gt_100 WHERE lstat = 'X'.
    lr_matnr-low = gt_100-matnr. APPEND lr_matnr.
    gt_locked-artnr = gt_100-matnr. APPEND gt_locked.
  ENDLOOP.


  DATA $locked LIKE gt_locked OCCURS 0 WITH HEADER LINE.
  DATA $line_cnt TYPE i.
  DATA $lock_cnt TYPE i.

  DO.
    MOVE $lock_cnt TO $line_cnt.
    LOOP AT gt_locked.
      $locked-artnr = gt_locked-artnr.
      APPEND $locked.
    ENDLOOP.

    SORT $locked.
    DELETE ADJACENT DUPLICATES FROM $locked.
    DESCRIBE TABLE $locked LINES $lock_cnt .

    IF $lock_cnt EQ $line_cnt.
      EXIT.
    ENDIF.

    SELECT a~compn APPENDING TABLE gt_locked
       FROM ztcou103 AS a
       FOR ALL ENTRIES IN $locked
       WHERE a~kokrs = p_kokrs
         AND a~kalka EQ $p_kalka
         AND a~bdatj = p_year
         AND a~poper = p_poper
         AND a~artnr EQ $locked-artnr
         AND a~stkkz = 'X'.

  ENDDO.

  SORT gt_locked.
  DELETE ADJACENT DUPLICATES FROM gt_locked.

*  select a~compn appending table gt_locked
*     from ztcou103 as a
*     WHERE a~KOKRS = P_KOKRS
*       AND a~KALKA EQ $P_KALKA
*       AND a~BDATJ = P_YEAR
*       AND a~POPER = P_POPER
*       AND a~artnr in lr_matnr
*       AND a~stkkz = 'X'.


* get locked cost info.
  SELECT artnr SUM( wertn ) SUM( duty ) SUM( frg ) SUM( oth )
     INTO TABLE v_db103
     FROM ztcou103
     WHERE kokrs = p_kokrs
       AND bdatj = p_year
       AND kalka = $p_kalka
       AND poper = p_poper
     GROUP BY artnr.
  SORT v_db103 BY artnr.

  DATA: l_idx LIKE sy-tabix.
  LOOP AT gt_locked.
    READ TABLE v_db103 WITH KEY artnr = gt_locked-artnr
                       BINARY SEARCH.
    l_idx = sy-tabix.
    IF sy-subrc = 0.
      v_db103-lock = 'X'.
      MODIFY v_db103 INDEX l_idx TRANSPORTING lock.
    ENDIF.
  ENDLOOP.

* single level processing.
  IF p_sing EQ 'X'.
    v_ztcou103[] = v_db103[].
  ELSE.
*-- move locked costing data
    LOOP AT v_db103 WHERE lock = 'X'.
      APPEND v_db103 TO v_ztcou103.
    ENDLOOP.
  ENDIF.


ENDFORM.                    " GET_GT_100
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

  IF p_test = space.
    DATA $line LIKE sy-tabix.
    DESCRIBE TABLE itab LINES $line.
    WRITE:/ f_ckiuser-matnr, '----->', $line.
  ENDIF.

  LOOP AT itab.
    MOVE-CORRESPONDING itab TO itab_all.

    l_menge = l_menge + itab-menge.
    l_wertn = l_wertn + itab-wertn.
    l_duty  = l_duty + itab-duty.
    l_frg   = l_frg + itab-frg.
    l_oth   = l_oth + itab-oth.

    IF itab-upgvc IS INITIAL AND f_ckiuser-btyp <> 'BB'.
*     MESSAGE S000 WITH '***UPG error:'  ITAB-ARTNR ITAB-COMPN.
      WRITE:/ '***UPG not found:', itab-artnr, itab-compn.
    ENDIF.

    AT END OF key.
      $flag = 'X'.
    ENDAT.
    CHECK $flag EQ 'X'.
    CLEAR $flag.
    itab_all-menge = l_menge * gv_ratio.
    itab_all-wertn = l_wertn * gv_ratio.
    itab_all-duty  = l_duty * gv_ratio.
    itab_all-frg   = l_frg * gv_ratio.
    itab_all-oth   = l_oth * gv_ratio..
    APPEND itab_all.

*--- MIP summary
    v_ztcou103-artnr = itab-artnr.
    v_ztcou103-wertn = itab_all-wertn.
    v_ztcou103-duty  = itab_all-duty.
    v_ztcou103-frg   = itab_all-frg.
    v_ztcou103-oth   = itab_all-oth.
    COLLECT v_ztcou103.  "ANDY

    CLEAR: itab_all, v_ztcou103,
           l_menge, l_wertn, l_duty, l_frg, l_oth.
  ENDLOOP.

ENDFORM.                    " MOVE_TO_ITAB_ALL
*&---------------------------------------------------------------------*
*&      Form  process_module_color
*&---------------------------------------------------------------------*
FORM process_module_color.

  DATA: lw_int_key(3),         " Internal Key Color
        $index LIKE sy-tabix.

  CHECK p_kalka = 'M1' AND p_mold = 'X'.

  LOOP AT itab WHERE stgb = 'U'.
    $index = sy-tabix.
    lw_int_key = f_ckiuser-matnr+10(3).
    SELECT COUNT( * ) INTO sy-dbcnt FROM ztmm_cp_color
                                    WHERE copit   EQ f_ckiuser-matnr
                                      AND inkey EQ lw_int_key
                                      AND submt EQ itab-compn
                                      AND datab <  f_ckiuser-aldat
                                      AND datbi >= f_ckiuser-aldat.
    IF sy-subrc <> 0.
      DELETE itab INDEX $index.
    ENDIF.
  ENDLOOP.

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
*&      Form  get_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_changed_comp.

  __cls : gt_102, gt_ztcou103, rt_ztcou103 .

  SELECT * INTO TABLE gt_102
           FROM ztcou102 WHERE  kokrs EQ p_kokrs
                           AND  bdatj EQ p_year
                           AND  poper EQ p_poper
                           AND  kalka EQ p_kalka
                           AND  ver   EQ p_ver
                           AND  stat  EQ 'C'.

  IF sy-subrc EQ 0.
    SORT gt_102 BY matnr.
    SELECT * INTO TABLE gt_ztcou103
       FROM ztcou103
       FOR ALL ENTRIES IN gt_102
                     WHERE  kokrs EQ p_kokrs
                       AND  bdatj EQ p_year
                       AND  kalka EQ p_kalka
                       AND  poper EQ p_poper
                       AND  ver   EQ p_ver
*                       AND  WERKS EQ GT_102-WERKS
                       AND  compn EQ gt_102-matnr.

    IF sy-subrc EQ 0.
      SORT gt_ztcou103 BY artnr.
      rt_ztcou103[] = gt_ztcou103[].
      DELETE ADJACENT DUPLICATES FROM rt_ztcou103
                      COMPARING artnr.
    ENDIF.
  ELSE.
    g_error = 'X'.
    EXIT.
  ENDIF.

  DATA : line1 LIKE sy-tabix,
         line2 LIKE sy-tabix.

  DO.
    DESCRIBE TABLE gt_ztcou103 LINES line1.
    SORT gt_ztcou103 BY compn.

    LOOP AT rt_ztcou103.
      READ TABLE gt_ztcou103 WITH KEY compn = rt_ztcou103-artnr
                             BINARY SEARCH.
      IF sy-subrc NE 0.
        SELECT * APPENDING TABLE gt_ztcou103
           FROM ztcou103
                         WHERE  kokrs EQ p_kokrs
                           AND  bdatj EQ p_year
                           AND  kalka EQ p_kalka
                           AND  poper EQ p_poper
                           AND  compn EQ rt_ztcou103-artnr
                           AND  ver   EQ p_ver.
      ENDIF.
    ENDLOOP.

    DESCRIBE TABLE gt_ztcou103 LINES line2.
    IF  line1 = line2.
      EXIT.
    ELSE.
      __cls rt_ztcou103.
      rt_ztcou103[] = gt_ztcou103[].
      SORT rt_ztcou103 BY artnr.
      DELETE ADJACENT DUPLICATES FROM rt_ztcou103
                      COMPARING artnr.
    ENDIF.

  ENDDO.

ENDFORM.                    " get_changed
*&---------------------------------------------------------------------*
*&      Form  GET_UPG_FROM_ZTCOU103
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_upg_from_ztcou103.

  __cls itab.

  SORT t_kis1 BY matnr.

  LOOP AT gt_ztcou103 WHERE artnr = f_ckiuser-matnr.

    READ TABLE t_kis1 WITH KEY matnr = gt_ztcou103-compn
                               BINARY SEARCH.
    CHECK sy-subrc EQ 0.

    MOVE-CORRESPONDING gt_ztcou103 TO itab.
* Roll-Up changed
    PERFORM get_compn_info_changed USING t_kis1.

    CONCATENATE itab-artnr itab-werks itab-upgvc itab-compn
                                     INTO itab-key.

    APPEND itab. CLEAR itab.

  ENDLOOP.

ENDFORM.                    " GET_UPG_FROM_ZTCOU103
*&---------------------------------------------------------------------*
*&      Form  GET_COMPN_INFO_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_KIS1  text
*----------------------------------------------------------------------*
FORM get_compn_info_changed USING p_ckis STRUCTURE t_kis1.

  DATA lv_logsr TYPE menge_kpf.

* case of stock transfer
  IF NOT t_kis1-ukaln IS INITIAL.
    w_ukaln = t_kis1-ukaln.
    PERFORM get_stock_trnsf.
  ELSE.
    itab-splnt = t_kis1-werks.
  ENDIF.

  IF itab-splnt IS INITIAL.
    itab-splnt = t_kis1-werks.
  ENDIF.

  IF NOT s_bkmat[] IS INITIAL AND itab-compn IN s_bkmat.
    BREAK-POINT.                                           "#EC NOBREAK
  ENDIF.

*   price unit; default price unit = 1
  IF t_kis1-peinh = space OR
     t_kis1-peinh = 0 OR
     t_kis1-peinh > 999.
    SELECT SINGLE peinh INTO itab-peinh
      FROM mbew
     WHERE matnr = itab-compn
       AND bwkey = itab-werks.
  ELSE.
    itab-peinh = t_kis1-peinh.
  ENDIF.

  IF itab-peinh = 0.
    itab-peinh = 1.
  ENDIF.

*     get price
  READ TABLE v_ztcou103 WITH KEY artnr = t_kis1-matnr.
*     ...MIP
  IF sy-subrc = 0.
    IF p_change EQ 'X'.
      " Roll Up only changed materials
      PERFORM get_price_mip_change USING t_kis1.       " MIP
    ELSE.
      PERFORM get_price_mip USING t_kis1.       " MIP
    ENDIF.
  ELSE.
*     ...End Part
    PERFORM get_price_end_item.               " case of end item

  ENDIF.

ENDFORM.                    " GET_COMPN_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_PRICE_MIP_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_KIS1  text
*----------------------------------------------------------------------*
FORM get_price_mip_change USING p_ckis STRUCTURE t_kis1.

  DATA : $old_gpreis LIKE gt_ztcou103-gpreis,
         $new_gpreis LIKE gt_ztcou103-gpreis.

  READ TABLE gt_ztcou103 WITH KEY artnr = t_kis1-matnr.

  IF sy-subrc EQ 0.
    $old_gpreis = gt_ztcou103-gpreis.

    $new_gpreis = v_ztcou103-wertn. "/ V_ZTCOU103-MENGE.

*FIXME *? /?
*    ITAB-GPREIS = GT_ZTCOU103-MENGE * ( $NEW_GPREIS - $OLD_GPREIS )
**                 * GT_ZTCOU103-LOSGR
*                  + ITAB-GPREIS.
    itab-gpreis = ( $new_gpreis - $old_gpreis )
                   + itab-gpreis.

    itab-wertn = itab-menge *
            ( v_ztcou103-wertn - gt_ztcou103-wertn ) / itab-peinh
               + itab-wertn.
    itab-duty =  itab-menge *
            ( v_ztcou103-duty  - gt_ztcou103-duty ) / itab-peinh
               + itab-duty .
    itab-frg =   itab-menge *
            ( v_ztcou103-frg   - gt_ztcou103-frg  ) / itab-peinh
               + itab-frg .
    itab-oth =   itab-menge *
            ( v_ztcou103-oth   - gt_ztcou103-oth )  / itab-peinh
               + itab-oth .

    itab-kstar = t_kis1-kstar.      " v_ztcou103-kstar.
    itab-stkkz = 'X'.               " Assy

  ELSE.
    gt_missing-artnr = f_ckiuser-matnr.
    gt_missing-matnr = t_kis1-matnr.
    gt_missing-upgvc = gt_ztcou103-upgvc.
    APPEND gt_missing.
  ENDIF.

ENDFORM.                    " get_price_mip
*&---------------------------------------------------------------------*
*&      Form  prep_for_map
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prep_for_map.
* by ig.moon 01/15/2008 {
  DATA : BEGIN OF it_ckmlmv003 OCCURS 0,
           bwkey      LIKE ckmlmv001-bwkey,
           matnr      LIKE ckmlmv001-matnr,
           aufnr      LIKE ckmlmv013-aufnr,
           verid_nd   LIKE ckmlmv001-verid_nd,
           meinh      LIKE ckmlmv003-meinh,
           out_menge  LIKE ckmlmv003-out_menge,
         END OF  it_ckmlmv003.

  SELECT  b~bwkey b~matnr b~verid_nd
          c~aufnr
          a~out_menge
          a~meinh
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv003
    FROM
    ( ( ckmlmv003 AS a
    INNER JOIN ckmlmv001 AS b
       ON b~kalnr = a~kalnr_bal )
    INNER JOIN ckmlmv013 AS c
       ON c~kalnr_proc = a~kalnr_in )
   WHERE a~mgtyp EQ '00001'
     AND a~gjahr EQ p_year
     AND a~perio EQ p_poper
     AND a~matnr IN s_artnr
     AND b~btyp  =  'BF'
     AND c~flg_wbwg = 'X'
     AND c~autyp = '05' .

  DATA: $ix TYPE sy-tabix.
  LOOP AT it_ckmlmv003.
    $ix = sy-tabix.
    SELECT SINGLE * FROM ztcou103
                 WHERE  kokrs EQ p_kokrs
                   AND  bdatj EQ p_year
                   AND  kalka EQ p_kalka
                   AND  poper EQ p_poper
                   AND  ver   EQ p_ver
                   AND  artnr EQ it_ckmlmv003-matnr .

    IF sy-subrc NE 0.
*      message s000 with 'Please Roll-Up the U1 type first.'.
      g_error = 'X'.
*      exit.
      WRITE:/ 'Warning:', it_ckmlmv003-matnr,' has no unit costing'.
      DELETE it_ckmlmv003 INDEX $ix.
    ENDIF.
  ENDLOOP.
* }

  PERFORM get_end_date.

  __cls $gt_ztcou103.

  SELECT DISTINCT compn splnt
  INTO TABLE $gt_ztcou103
  FROM ztcou103
    WHERE kokrs EQ p_kokrs
    AND bdatj EQ p_year
    AND kalka EQ p_kalka
    AND poper EQ p_poper
    AND artnr IN s_artnr
    AND ver   EQ p_ver
    AND compn IN s_bkmat.

  SORT $gt_ztcou103 BY matnr.


  __cls gt_102.

  SELECT *
*  VER MATNR BKLAS LIFNR PEINH PMEHT WERTN DUTY FRG OTH STAT
    INTO TABLE gt_102
      FROM ztcou102
     FOR ALL ENTRIES IN $gt_ztcou103
     WHERE kokrs = p_kokrs
       AND bdatj = p_year
       AND poper = p_poper
       AND kalka = 'U1'
       AND ver = p_ver
       AND matnr = $gt_ztcou103-matnr. "matnr IN s_bkmat ).

  SORT gt_102 BY matnr.

* get MLCD/EKBZ to get actual cost
  PERFORM get_102_map.

ENDFORM.                    " prep_for_map
*&---------------------------------------------------------------------*
*&      Form  GET_102_FOR_UA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_102_for_ua.
*  EXIT.
*
*  DATA: BEGIN OF $matnr OCCURS 0,
*           matnr  LIKE t_kis1-matnr,
*           werks  LIKE t_kis1-werks,
*        END OF $matnr.
*
*  DATA: BEGIN OF it_mlcd OCCURS 0,
*         matnr  LIKE ckmlhd-matnr,
*         kalnr  LIKE mlcd-kalnr,
*         bdatj  LIKE mlcd-bdatj,
*         poper  LIKE mlcd-poper,
*	  categ  LIKE mlcd-categ,
*         ptyp   LIKE mlcd-ptyp,
*         lbkum  LIKE mlcd-lbkum,
*         meins  LIKE mlcd-meins,
*         salk3  LIKE mlcd-salk3,
*         estprd LIKE mlcd-estprd,
*         estkdm LIKE mlcd-estkdm,
*        END OF it_mlcd.
*
*  DATA : BEGIN OF $ekbz OCCURS 0,
*           belnr           LIKE   ekbz-belnr,
*           buzei           LIKE   ekbz-buzei,
*           matnr           LIKE   ekpo-matnr,
*           kschl           LIKE   ekbz-kschl,
*           menge           LIKE   ekpo-menge,
*           dmbtr           LIKE   ekbz-dmbtr,   " Amount
*           shkzg           LIKE   ekbz-shkzg,
*           bewtp           LIKE   ekbz-bewtp,
*           meins           LIKE   ekpo-meins,
*           werks           LIKE   ekpo-werks,
*           ebeln           LIKE   ekbz-ebeln,
*           ebelp           LIKE   ekbz-ebelp,
*         END   OF  $ekbz.
*
*  DATA $$ekbz LIKE $ekbz OCCURS 0 WITH HEADER LINE.
*  DATA $it_mlcd LIKE it_mlcd OCCURS 0 WITH HEADER LINE.
*  DATA $ix LIKE sy-tabix.
*
*
*  LOOP AT t_kis1 WHERE typps EQ 'M'.
*    $matnr-matnr = t_kis1-matnr.
*    $matnr-werks = t_kis1-werks.
*    COLLECT $matnr.
*  ENDLOOP.
*
*  SORT $matnr.
*
** Due to incorrect maintenance of MRP information in material master
** this program will collect all plant information.
**  LOOP AT $MATNR.
**    $IX = SY-TABIX.
**    READ TABLE $GT_ZTCOU103 WITH KEY ARTNR = F_CKIUSER-MATNR
**                                     MATNR = $MATNR-MATNR
**                                     BINARY SEARCH.
**    IF SY-SUBRC EQ 0.
**      $MATNR-WERKS = $GT_ZTCOU103-SPLNT.
**      MODIFY $MATNR INDEX $IX TRANSPORTING WERKS.
**    ENDIF.
**  ENDLOOP.
*
*  CHECK NOT $matnr[] IS INITIAL.
*
*  SELECT a~matnr b~kalnr b~bdatj b~poper b~categ b~ptyp b~lbkum b~meins
*         b~salk3 b~estprd b~estkdm
*    INTO CORRESPONDING FIELDS OF TABLE it_mlcd
*    FROM ckmlhd AS a
*     JOIN mlcd AS b
*        ON a~kalnr = b~kalnr
*    FOR ALL ENTRIES IN $matnr
*     WHERE a~matnr = $matnr-matnr
**       AND A~BWKEY = $MATNR-WERKS
*       AND b~bdatj = p_year
*       AND b~poper = p_poper
*       AND (
*             ( b~categ = 'ZU' AND b~ptyp >= 'BB'
*               AND b~categ = 'ZU' AND b~ptyp < 'BC' ) OR
*             ( b~categ = 'VP' AND b~ptyp = 'DC' ) " OR
**             "( B~CATEG = 'VN' AND B~PTYP = 'VU' )
*             ) .
*
*  LOOP AT it_mlcd.
*    $it_mlcd = it_mlcd.
*    CLEAR $it_mlcd-ptyp.
*    COLLECT $it_mlcd.
*  ENDLOOP.
*
*  __cls : it_mlcd.
*
*  it_mlcd[] = $it_mlcd[].
*  SORT it_mlcd BY matnr categ.
*
*  SELECT  b~belnr b~buzei
*          a~matnr b~kschl
*          b~menge
*          b~dmbtr
*          b~shkzg b~bewtp a~meins b~lifnr a~werks b~ebeln b~ebelp
*  INTO CORRESPONDING FIELDS OF TABLE $ekbz
*  FROM    ekpo AS  a  INNER JOIN ekbz AS b
*  ON      a~ebeln      EQ    b~ebeln
*  AND     a~ebelp      EQ    b~ebelp
*  FOR ALL ENTRIES IN $matnr
*  WHERE   a~matnr    EQ    $matnr-matnr
*  AND     b~bewtp    EQ    'M'
*  AND     b~budat BETWEEN start_date AND end_date.
*
**  SELECT  A~MATNR B~KSCHL
**          B~MENGE
**          B~DMBTR
**          B~SHKZG B~BEWTP A~MEINS B~LIFNR A~WERKS B~EBELN B~EBELP
**  INTO CORRESPONDING FIELDS OF TABLE $EKBZ
**  FROM    EKPO AS  A  INNER JOIN EKBZ AS B
**  ON      A~EBELN      EQ    B~EBELN
**  AND     A~EBELP      EQ    B~EBELP
**  WHERE   A~MATNR    EQ    'Z86463LFILM'
**  AND     B~BEWTP    EQ    'M'
**  AND     B~BUDAT BETWEEN START_DATE AND END_DATE.
*
*  LOOP AT $ekbz.
*    MOVE $ekbz TO $$ekbz .
*    IF $ekbz-shkzg EQ 'H'.
*      $$ekbz-menge = - $$ekbz-menge.
*      $$ekbz-dmbtr = - $$ekbz-dmbtr.
*    ENDIF.
*    CLEAR : $$ekbz-shkzg,$$ekbz-bewtp,$$ekbz-werks,
*            $$ekbz-belnr,$$ekbz-buzei,
*            $$ekbz-ebeln,$$ekbz-ebelp.
*    COLLECT $$ekbz.
*  ENDLOOP.
*
*  __cls $ekbz.
*  $ekbz[] = $$ekbz[].
*
*  SORT $ekbz BY matnr kschl.
*
** get other import expenses from ML
*
*
*  DATA : $lbkum LIKE mlcd-lbkum,
*         $receipt TYPE kkb_ml_pabwe.
*
*  LOOP AT t_kis1.
*    $ix = sy-tabix.
*    CLEAR : $lbkum,$receipt,gt_102.
*
*    READ TABLE gt_102 WITH KEY matnr = t_kis1-matnr BINARY SEARCH.
*
**   Gross Price From GR
*    CLEAR t_kis1-$flag .
*
*    READ TABLE it_mlcd WITH KEY matnr = t_kis1-matnr
*                                categ = 'ZU'
*                                BINARY SEARCH.
*    IF sy-subrc EQ 0 AND it_mlcd-lbkum <> 0 .
*      $lbkum         = it_mlcd-lbkum.
*      $receipt       = it_mlcd-salk3 + it_mlcd-estprd + it_mlcd-estkdm.
*      t_kis1-$gpreis = $receipt / $lbkum.
*
**     other import cost
*      READ TABLE it_mlcd WITH KEY matnr = t_kis1-matnr
*                                  categ = 'VP'
*                                  BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        t_kis1-$oth = gt_102-oth + ( it_mlcd-estprd / $lbkum ).
*        "critical error.....
*        IF t_kis1-$oth > t_kis1-$gpreis. BREAK-POINT. ENDIF.
*      ENDIF.
*
**     Freight
*      READ TABLE $ekbz WITH KEY matnr = t_kis1-matnr
*                                kschl = 'FRA1'
*                                BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        IF it_mlcd-meins <> $ekbz-meins.
*          PERFORM unit_convert        USING    it_mlcd-matnr
*                                               it_mlcd-meins
*                                      CHANGING $ekbz-menge
*                                               $ekbz-meins.
*        ENDIF.
*        t_kis1-$frg = $ekbz-dmbtr / $ekbz-menge.
*      ENDIF.
*
**     Duty
*      READ TABLE $ekbz WITH KEY matnr = t_kis1-matnr
*                                kschl = 'ZOA1'
*                                BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        IF it_mlcd-meins <> $ekbz-meins.
*          PERFORM unit_convert USING  it_mlcd-matnr
*                                      it_mlcd-meins
*                                      CHANGING $ekbz-menge
*                                               $ekbz-meins.
*        ENDIF.
*        t_kis1-$duty = $ekbz-dmbtr / $ekbz-menge.
*      ENDIF.
*
*      t_kis1-gpreis = t_kis1-$gpreis * t_kis1-peinh.
*
*
*    ELSE.
*      t_kis1-$flag = 'D'.
*      READ TABLE gt_102 WITH KEY matnr = t_kis1-matnr BINARY SEARCH.
*
*      t_kis1-gpreis  = t_kis1-verpr.
*      t_kis1-$gpreis = t_kis1-verpr.
*      t_kis1-$frg    = gt_102-frg.
*      t_kis1-$duty   = gt_102-duty.
*      t_kis1-$oth    = gt_102-oth.
*
*    ENDIF.
*
*    MODIFY t_kis1 INDEX $ix TRANSPORTING
*                  gpreis $gpreis $frg $duty $oth $flag.
*
*  ENDLOOP.
*
*ENDFORM.                    " GET_102_FOR_UA
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
                           p_unit_in  "p_from_u.            "UQ1K900355
*                          p_unit_out "p_to_u               "UQ1K900355
                  CHANGING p_output
                           p_unit_out."p_to_u               "UQ1K900355
*                          p_unit_in. "p_from_u.            "UQ1K900355

  DATA $p_result_qty.  " TYPE bstmg.
** On 04/08/14
*  DATA l_output LIKE gt_102-peinh.                          "UQ1K900307
  DATA l_output TYPE p DECIMALS 3.
** End on 04/08/14

  IF p_unit_in = p_unit_out.
    p_output = p_input.
  ELSE.

    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
      EXPORTING
        input                = p_input
        unit_in              = p_unit_in  "from_u
        unit_out             = p_unit_out  "to_u
      IMPORTING
        output               = l_output                     "UQ1K900307
*       output               = p_output  "$p_result_qty
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
    ELSE.                                                   "UQ1K900307
      p_output = l_output.                                  "UQ1K900307
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
*&      Form  GET_ALL_COMP
*&---------------------------------------------------------------------*
FORM get_all_comp.
  __cls gt_102.

  SELECT *
*  VER MATNR BKLAS LIFNR PEINH PMEHT WERTN DUTY FRG OTH STAT
    INTO TABLE gt_102
      FROM ztcou102
     WHERE kokrs = p_kokrs
       AND bdatj = p_year
       AND poper = p_poper
       AND kalka EQ p_kalka
       AND ver = p_ver.

  SORT gt_102 BY matnr.

ENDFORM.                    " GET_ALL_COMP
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
'WERKS'      'Plant'             '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KSTAR'      'Cost element'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'UPGVC'      'UPG-VC'            '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'COMPN'      'BOM component'     '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MKALK'      'Mixed'             '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'TYPPS'      'IC'                '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MENGE'      'Quantity'          '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MEEHT'      'UoM'               '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'STKKZ'      'Assy'              '01' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'GPREIS'     'Unit Price'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'WERTN'      'Overall value'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'DUTY'       'Duty'              '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'FRG'        'Freight'           '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OTH'        'Other'             '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PEINH'      'Price unit'        '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'SPLNT'      'SuppPlant'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'BTYP'       'Pcat'              '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MISCH_VERH' 'Mix%'              '04' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'AUSSS'      'AssyScrap%'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'KAUSF'      'CompScrap%'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'BAUSF'      'AssyScrap%'        '05' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'VERID'      'PPver'             '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KALST'      'CstLvl'            '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LOSGR'      'Lotsize'           '13' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
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
*&      Form  display_missing_log
*&---------------------------------------------------------------------*
FORM display_missing_log.

  LOOP AT gt_missing.
    AT FIRST.
      WRITE:/ '*** Missing Component ***'.
    ENDAT.
    WRITE:/ gt_missing-matnr, '-', gt_missing-artnr, gt_missing-upgvc.
  ENDLOOP.

ENDFORM.                    " display_missing_log
*&---------------------------------------------------------------------*
*&      Form  fill_itab_ua
*&---------------------------------------------------------------------*
FORM fill_itab_ua.

  REFRESH itab.

  LOOP AT t_kis1.
    IF NOT s_bkmat[] IS INITIAL AND t_kis1-matnr IN s_bkmat.
      BREAK-POINT.                                         "#EC NOBREAK
    ENDIF.

    CLEAR itab.
**   move header info
    PERFORM move_f_ckiuser_to_itab USING f_ckiuser.

*   get component detail
    PERFORM get_compn_info_ua USING t_kis1.


    "default key; in case of UPG blank
    CONCATENATE itab-artnr itab-werks itab-compn
           INTO itab-key.

    APPEND itab. CLEAR itab.

  ENDLOOP.

ENDFORM.                    " fill_itab_ua
*&---------------------------------------------------------------------*
*&      Form  get_compn_info_ua
*&---------------------------------------------------------------------*
FORM get_compn_info_ua USING p_ckis STRUCTURE t_kis1.

  itab-kstar = t_kis1-kstar.         " cost element
  itab-stkkz = t_kis1-stkkz.         " assy' indicator
  itab-typps = t_kis1-typps.         "

  itab-upgvc = t_kis1-upgvc.         " UPG
  itab-compn = t_kis1-matnr.         " UPG
  itab-posnr = t_kis1-posnr.         " BOM Item No.
  itab-menge = t_kis1-menge.         " Qty

  itab-meeht = t_kis1-meeht.         " UoM
  itab-peinh = t_kis1-peinh.


*check existing MIP
  CLEAR v_ztcou103.
  READ TABLE v_ztcou103 WITH KEY artnr = t_kis1-matnr.
  IF sy-subrc <> 0.
    IF p_sing EQ 'X'.
      READ TABLE v_db103 WITH KEY artnr = t_kis1-matnr BINARY SEARCH.
      v_ztcou103 = v_db103.
    ENDIF.
  ENDIF.

*   ...MIP
  IF sy-subrc = 0.
    itab-gpreis = v_ztcou103-wertn.
    itab-wertn = itab-menge * v_ztcou103-wertn  / itab-peinh.
    itab-duty =  itab-menge * v_ztcou103-duty   / itab-peinh.
    itab-frg =   itab-menge * v_ztcou103-frg    / itab-peinh.
    itab-oth =   itab-menge * v_ztcou103-oth    / itab-peinh.

  ELSE.
*   ...End Part
    CLEAR gt_102.
    READ TABLE gt_102 WITH KEY matnr = t_kis1-matnr BINARY SEARCH.
    IF sy-subrc <> 0 AND t_kis1-matnr IN s_bkmat. BREAK-POINT. ENDIF.

    IF gt_102-grspr = 0.
      itab-gpreis = t_kis1-verpr.
    ELSE.
      itab-gpreis = gt_102-grspr.
    ENDIF.
    itab-wertn = t_kis1-menge * itab-gpreis / t_kis1-peinh.
    itab-frg   = t_kis1-menge * gt_102-frg  / t_kis1-peinh.
    itab-duty  = t_kis1-menge * gt_102-duty / t_kis1-peinh.
    itab-oth   = t_kis1-menge * gt_102-oth  / t_kis1-peinh.
  ENDIF.

ENDFORM.                    " get_compn_info_ua
*&---------------------------------------------------------------------*
*&      Form  get_product_info
*&---------------------------------------------------------------------*
FORM get_product_info USING f_ckiuser STRUCTURE f_ckiuser
                             CHANGING l_rc LIKE sy-subrc.

  gv_ratio = 1.
  CLEAR l_rc.

*---skip rollup on mixed costing + parent product
  READ TABLE gt_mixed WITH KEY matnr = f_ckiuser-matnr BINARY SEARCH.
  IF sy-subrc = 0 AND f_ckiuser-baltkz = space.
    l_rc = 8. "CONTINUE.
  ENDIF.

  IF f_ckiuser-baltkz = 'X'.  "mixed costing
    CLEAR gt_mixed.
    READ TABLE gt_mixed WITH KEY matnr = f_ckiuser-matnr
                                 kalnr = f_ckiuser-kalnr
                        BINARY SEARCH.
    IF sy-subrc EQ 0.
      gv_ratio = gt_mixed-misch_verh / 100.   "TEMP
    ELSE.
      gv_ratio = 0.
    ENDIF.
  ENDIF.

* determine old module bom.
  IF p_kalka = 'M1' AND f_ckiuser-matnr+5(2) = 'M1'.
    CLEAR p_mold.
  ELSE.
    p_mold = 'X'.
  ENDIF.

ENDFORM.                    " get_product_info
*&---------------------------------------------------------------------*
*&      Form  get_102_map
*&---------------------------------------------------------------------*
FORM get_102_map.

  DATA: BEGIN OF it_mlcd OCCURS 0,
         matnr  LIKE ckmlhd-matnr,
         kalnr  LIKE mlcd-kalnr,
         bdatj  LIKE mlcd-bdatj,
         poper  LIKE mlcd-poper,
     categ  LIKE mlcd-categ,
         ptyp   LIKE mlcd-ptyp,
         lbkum  LIKE mlcd-lbkum,
         meins  LIKE mlcd-meins,
         peinh  LIKE ckmlcr-peinh,
         salk3  LIKE mlcd-salk3,
         estprd LIKE mlcd-estprd,
         estkdm LIKE mlcd-estkdm,
         mstprd LIKE mlcd-mstprd,
         mstkdm LIKE mlcd-mstkdm,
        END OF it_mlcd.

  DATA : BEGIN OF $ekbz OCCURS 0,
           belnr           LIKE   ekbz-belnr,
           buzei           LIKE   ekbz-buzei,
           matnr           LIKE   ekpo-matnr,
           kschl           LIKE   ekbz-kschl,
           menge           LIKE   ekpo-menge,
           dmbtr           LIKE   ekbz-dmbtr,   " Amount
           shkzg           LIKE   ekbz-shkzg,
           bewtp           LIKE   ekbz-bewtp,
           meins           LIKE   ekpo-meins,
           werks           LIKE   ekpo-werks,
           ebeln           LIKE   ekbz-ebeln,
           ebelp           LIKE   ekbz-ebelp,
         END   OF  $ekbz.

  DATA $$ekbz LIKE $ekbz OCCURS 0 WITH HEADER LINE.
  DATA $it_mlcd LIKE it_mlcd OCCURS 0 WITH HEADER LINE.
  DATA $ix LIKE sy-tabix.

* Due to incorrect maintenance of MRP information in material master
* this program will collect all plant information.
*  LOOP AT $MATNR.
*    $IX = SY-TABIX.
*    READ TABLE $GT_ZTCOU103 WITH KEY ARTNR = F_CKIUSER-MATNR
*                                     MATNR = $MATNR-MATNR
*                                     BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      $MATNR-WERKS = $GT_ZTCOU103-SPLNT.
*      MODIFY $MATNR INDEX $IX TRANSPORTING WERKS.
*    ENDIF.
*  ENDLOOP.


* read MLCD
  REFRESH it_mlcd.

  IF p_gr = 'X'.
    SELECT a~matnr b~kalnr b~bdatj b~poper b~categ b~ptyp b~lbkum b~meins
           c~peinh b~salk3 b~estprd b~estkdm
      INTO CORRESPONDING FIELDS OF TABLE it_mlcd
      FROM ckmlhd AS a
       JOIN mlcd AS b
          ON a~kalnr = b~kalnr
       JOIN ckmlcr AS c
          ON b~kalnr = c~kalnr
         AND b~bdatj = c~bdatj
         AND b~poper = c~poper
         AND b~untper = c~untper
         AND b~curtp = c~curtp

      FOR ALL ENTRIES IN gt_102
       WHERE a~matnr = gt_102-matnr
*       AND A~BWKEY = $MATNR-WERKS   "all valuation class...??? yes
         AND b~bdatj = p_year
         AND b~poper = p_poper
         AND ( ( b~categ = 'ZU' AND
                 ( b~ptyp >= 'BB' AND b~ptyp < 'BC' ) )
            OR ( b~categ = 'VP' AND b~ptyp = 'DC' ) ).
*             "( B~CATEG = 'VN' AND B~PTYP = 'VU' )
  ELSE.
    SELECT a~matnr b~kalnr b~bdatj b~poper b~categ b~ptyp b~lbkum b~meins
           c~peinh b~salk3 b~estprd b~estkdm
      INTO CORRESPONDING FIELDS OF TABLE it_mlcd
      FROM ckmlhd AS a
       JOIN mlcd AS b
          ON a~kalnr = b~kalnr
       JOIN ckmlcr AS c
          ON b~kalnr = c~kalnr
         AND b~bdatj = c~bdatj
         AND b~poper = c~poper
         AND b~untper = c~untper
         AND b~curtp = c~curtp

      FOR ALL ENTRIES IN gt_102
       WHERE a~matnr = gt_102-matnr
*      AND A~BWKEY = $MATNR-WERKS   "all valuation class...??? yes
         AND b~bdatj = p_year
         AND b~poper = p_poper
         AND ( b~categ = 'VN' AND b~ptyp = 'VF' ).
  ENDIF.

  LOOP AT it_mlcd.
    $it_mlcd = it_mlcd.
    CLEAR: $it_mlcd-ptyp, $it_mlcd-categ. COLLECT $it_mlcd.
  ENDLOOP.

  __cls : it_mlcd.
  it_mlcd[] = $it_mlcd[].
  SORT it_mlcd BY matnr categ.


* read EKBZ - only KD purchasing...
  SELECT  a~matnr a~werks a~meins
          c~kschl             "condition type
          c~shkzg             "dr/cr
          c~menge
          c~dmbtr
          c~bewtp c~lifnr
          c~ebeln c~ebelp
          c~belnr c~buzei
    INTO CORRESPONDING FIELDS OF TABLE $ekbz
    FROM    ekpo AS  a
    INNER JOIN ekko AS b
       ON   a~ebeln = b~ebeln
    INNER JOIN lfa1 AS k
       ON   b~lifnr = k~lifnr
    INNER JOIN ekbz AS c
       ON   a~ebeln    EQ    c~ebeln
      AND   a~ebelp    EQ    c~ebelp
    FOR ALL ENTRIES IN gt_102
    WHERE   a~matnr    EQ    gt_102-matnr
      AND   k~land1    <>   t001-land1
      AND   c~bewtp    EQ    'M'
      AND   c~budat BETWEEN start_date AND end_date.

*  WHERE   A~MATNR    EQ    'Z86463LFILM'

  LOOP AT $ekbz.
    MOVE $ekbz TO $$ekbz .
    IF $ekbz-shkzg EQ 'H'.
      $$ekbz-menge = - $$ekbz-menge.
      $$ekbz-dmbtr = - $$ekbz-dmbtr.
    ENDIF.
    CLEAR : $$ekbz-shkzg,$$ekbz-bewtp,$$ekbz-werks,
            $$ekbz-belnr,$$ekbz-buzei,$$ekbz-ebeln,$$ekbz-ebelp.
    COLLECT $$ekbz.
  ENDLOOP.

  __cls $ekbz.
  $ekbz[] = $$ekbz[].
  SORT $ekbz BY matnr kschl.



  DATA : $lbkum LIKE mlcd-lbkum,
         $value TYPE kkb_ml_pabwe.

  LOOP AT gt_102.
    $ix = sy-tabix.
    CLEAR : $lbkum,$value.

*   READ TABLE gt_102 WITH KEY matnr = t_kis1-matnr BINARY SEARCH.

*   Gross Price From GR
    CLEAR t_kis1-$flag .

*  change UoM of ML to 102 unit (purchasing unit)??? fixme
    READ TABLE it_mlcd WITH KEY matnr = gt_102-matnr
*                                categ = 'ZU'
                                BINARY SEARCH.
    IF sy-subrc EQ 0 AND it_mlcd-lbkum <> 0 .
      $lbkum         = it_mlcd-lbkum.

      IF p_gr = 'X'.
        $value       = it_mlcd-salk3 + it_mlcd-estprd + it_mlcd-estkdm.
      ELSE.
        $value       = it_mlcd-salk3 + it_mlcd-estprd + it_mlcd-mstprd
                                     + it_mlcd-estkdm + it_mlcd-mstkdm.
      ENDIF.

      gt_102-grspr = ( $value / $lbkum ) * gt_102-peinh.

*     other import cost
      READ TABLE it_mlcd WITH KEY matnr = gt_102-matnr
                                  categ = 'VP'
                                  BINARY SEARCH.
      IF sy-subrc EQ 0.
        gt_102-oth = gt_102-oth +
        ( it_mlcd-estprd / $lbkum ) * gt_102-peinh.
      ENDIF.
*-- not found on MLCD ; no purchasing..
    ELSE.
      CLEAR gt_102-grspr.
    ENDIF.

*   Freight
    READ TABLE $ekbz WITH KEY matnr = gt_102-matnr
                              kschl = 'FRA1'
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF gt_102-pmeht <> $ekbz-meins.
        PERFORM unit_convert        USING    gt_102-matnr
                                             $ekbz-menge
                                             gt_102-pmeht
                                    CHANGING $ekbz-menge
                                             $ekbz-meins.
      ENDIF.
      gt_102-frg = ( $ekbz-dmbtr / $ekbz-menge ) * gt_102-peinh.
    ENDIF.

*     Duty
    READ TABLE $ekbz WITH KEY matnr = gt_102-matnr
                              kschl = 'ZOA1'
                              BINARY SEARCH.
    IF sy-subrc EQ 0.
      IF gt_102-pmeht <> $ekbz-meins.
        PERFORM unit_convert USING  gt_102-matnr
                                    $ekbz-menge
                                    gt_102-pmeht
                             CHANGING $ekbz-menge
                                      $ekbz-meins.
      ENDIF.
      gt_102-duty = ( $ekbz-dmbtr / $ekbz-menge ) * gt_102-peinh.
    ENDIF.



    MODIFY gt_102 INDEX $ix TRANSPORTING
                  grspr frg duty oth.  " $flag.

  ENDLOOP.

ENDFORM.                    " get_102_map
*&---------------------------------------------------------------------*
*&      Form  delete_103_all
*&---------------------------------------------------------------------*
FORM delete_103_all.
  DATA: BEGIN OF lt_103 OCCURS 0,
          artnr TYPE matnr,
        END OF lt_103.

  SELECT DISTINCT artnr INTO TABLE lt_103 FROM ztcou103
   WHERE kokrs = p_kokrs
     AND bdatj = p_year
     AND kalka = p_kalka
     AND poper = p_poper
     AND ver   = p_ver.

  LOOP AT lt_103.
    CLEAR ztcou100.
    SELECT SINGLE * FROM ztcou100
       WHERE kokrs = p_kokrs
         AND kalka = p_kalka
         AND bdatj = p_year
         AND poper = p_poper
         AND matnr = lt_103-artnr.

    IF ztcou100-lstat = space.
      DELETE FROM ztcou103
         WHERE kokrs = p_kokrs
           AND bdatj = p_year
           AND kalka = p_kalka
           AND poper = p_poper
           AND ver   = p_ver
           AND artnr = lt_103-artnr.

      UPDATE ztcou100 SET rstat = ' '
                          aedat = sy-datum
                          aenam = sy-uname
            WHERE kokrs = p_kokrs
              AND kalka = p_kalka
              AND bdatj = p_year
              AND poper = p_poper
              AND matnr = lt_103-artnr.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " delete_103_all
*&---------------------------------------------------------------------*
*&      Form  delele_103_indv
*&---------------------------------------------------------------------*
FORM delele_103_indv.

  LOOP AT f_ckiuser.
* UD1K941492 by ig.moon 8/28/07
* {
    IF ua_kalka EQ 'UA'.
      f_ckiuser-kalka = ua_kalka.
    ENDIF.
* }
    DELETE FROM ztcou103
     WHERE kalka = f_ckiuser-kalka
       AND kokrs = f_ckiuser-kokrs
       AND bdatj = f_ckiuser-bdatj
       AND poper = f_ckiuser-poper
       AND kalka = f_ckiuser-kalka
       AND ver   = p_ver
       AND artnr = f_ckiuser-matnr
       AND werks = f_ckiuser-werks.

    UPDATE ztcou100 SET rstat = ' '
                        aedat = sy-datum
                        aenam = sy-uname
          WHERE kokrs = p_kokrs
            AND kalka = f_ckiuser-kalka
            AND bdatj = f_ckiuser-bdatj
            AND poper = f_ckiuser-poper
            AND matnr = f_ckiuser-matnr.
    COMMIT WORK.
  ENDLOOP.

ENDFORM.                    " delele_103_indv
