*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU0Z_LDC                                               *
*----------------------------------------------------------------------*

*read BOM explosion
* << Modify by Michelle on 02/10/2006
CLEAR l_capid.
SELECT SINGLE a~capid INTO l_capid
  FROM tck19a AS a
  JOIN tck03 AS b
    ON b~aufkz = a~aufkz
 WHERE b~klvar = f_ckiuser-klvar
   AND b~kalka = f_ckiuser-kalka
   AND b~bwvar = f_ckiuser-bwvar.
* Modify by Michelle on 02/10/2006 >>

*call function 'CS_BOM_EXPL_MAT_V2'
*     exporting
*          capid = l_capid
*          datuv = f_ckiuser-aldat
*          mtnrv = f_ckiuser-matnr
*          werks = f_ckiuser-werks
*          stlan = f_ckiuser-stlan
*          stlal = f_ckiuser-stalt
*          mehrs = 'X'  "Multi-level explosion
*          mmory = '1'  "Memory use On(1)
*          sanka = 'X'  "Only Costing Relevency(inc.Phantom)
*     tables
*          stb   = stb.


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

LOOP AT stb.
  MOVE-CORRESPONDING stb TO lt_bom. APPEND lt_bom.
ENDLOOP.

LOOP AT t_kis1 WHERE typps = 'M'.
  CLEAR itab.
  MOVE-CORRESPONDING t_kis1 TO itab.


  itab-kokrs  = f_ckiuser-kokrs.
  itab-klvar  = f_ckiuser-klvar.
  itab-artnr  = f_ckiuser-matnr.
  itab-werks  = f_ckiuser-werks.
  itab-bdatj  = f_ckiuser-bdatj.
  itab-poper  = f_ckiuser-poper.
  itab-stalt  = f_ckiuser-stalt. "Alt.BOM
  itab-bwdat  = f_ckiuser-bwdat.
  itab-aldat  = f_ckiuser-aldat.
  itab-verid  = f_ckiuser-verid.
  itab-compn  = t_kis1-matnr.
  itab-reqqt  = t_kis1-menge.

* get component detail
*  if not t_kis1-ukaln is initial.
*    w_ukaln = t_kis1-ukaln.
**---Plant!!!
*    read table lt_keko with key kalnr = w_ukaln  binary search.
*    itab-splnt = lt_keko-werks.  "Def.Supply plant
*
**---Stock transfer, get source plant data
*    if lt_keko-sobes = '7'. "Stock Trf
*      itab-splnt = lt_keko-sowrk.   "Trf From Plant
*
**.... get detail info from supply plant
*      read table lt_ckis into w_ckis with key kalnr = t_kis1-ukaln.
*      w_ukaln    = w_ckis-ukaln.
*      itab-strat = w_ckis-strat.   "Pricing Strategy
*    endif.
*  endif.

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

*  case t_kis1-strat.
*    when 'L'.  "Info-Record
  IF itab-lifnr IS INITIAL AND
     itab-bklas NE '7900'  AND
     f_ckiuser-kalka NE 'UN'.


*lf_bqpim-infnr = uf_kalktab-infnr. "blank
*lf_bqpim-ekorg = uf_kalktab-ekorg. "blank
*lf_bqpim-lifnr = uf_kalktab-lifnr. "blank
*lf_bqpim-cuobj = uf_kalktab-cuobj. "blank

    lf_bqpim-matnr = t_kis1-matnr.

    lf_bqpim-werks = t_kis1-werks.                          "P001
    lf_bqpim-nemng = abs( t_kis1-menge ). "PriceUnit
    lf_bqpim-meins = t_kis1-meeht. "UoM
    lf_bqpim-lmein = t_kis1-meeht. "Units of Measure in WM
    lf_bqpim-nedat = t_kis1-steas. "Valid date on
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
  ENDIF.

* by ig.moon 03/17/2008 {
**  SELECT SINGLE pvprs a~kalnr INTO (itab-verpr,itab-kalnr)
**      FROM ckmlhd AS a
**      INNER JOIN ckmlcr AS b
**        ON b~kalnr EQ a~kalnr
**      INNER JOIN ckmlpp AS c
**         ON c~kalnr  = b~kalnr
**        AND c~bdatj  = b~bdatj
**        AND c~poper  = b~poper
**        AND c~untper = b~untper
**      WHERE a~matnr = itab-compn
**        AND a~bwkey = f_ckiuser-werks
**        AND b~bdatj  = f_ckiuser-bdatj
**        AND b~poper  = f_ckiuser-poper
**        AND b~untper = space
**        AND b~curtp  = '10' .
**
**  IF sy-subrc NE 0.
**    SELECT SINGLE verpr kaln1
**    INTO (itab-verpr,itab-kalnr)
**              FROM mbew
**        WHERE matnr = itab-compn
**          AND bwkey = f_ckiuser-werks .
**  ENDIF.

  CALL FUNCTION 'Z_CO_GET_MAP_IG'
       EXPORTING
            matnr = itab-compn
            poper = f_ckiuser-poper
            bdatj = f_ckiuser-bdatj
       IMPORTING
            verpr = itab-verpr
            peinh = itab-peinh
            retro = sy-locdb.

  IF sy-subrc NE 0.
  ELSE.
  ENDIF.

  SELECT SINGLE kaln1
  INTO itab-kalnr FROM mbew
      WHERE matnr = itab-compn
        AND bwkey = f_ckiuser-werks .

* }



  APPEND itab.
  CLEAR itab.

ENDLOOP.



*
*

CLEAR : ir_prtyp[], ir_prtyp,
        ir_curtp[], ir_curtp.
ir_prtyp = 'IEQV'.
APPEND ir_prtyp.
ir_curtp = 'IEQ10'.
APPEND ir_curtp.

*get vendor name ; except NAFTA costing
IF f_ckiuser-kalka NE 'UN'.
  SELECT lifnr land1 name1 INTO CORRESPONDING FIELDS OF TABLE t_lfa1
                   FROM lfa1
                   FOR ALL ENTRIES IN itab
                   WHERE lifnr = itab-lifnr.
  SORT t_lfa1 BY lifnr.
ENDIF.

*get Item info
SORT lt_bom BY stufe index posnr ASCENDING hdnfo DESCENDING. "SAP
SORT itab BY compn posnr.

LOOP AT lt_bom WHERE dumps = space.
  READ TABLE itab WITH KEY compn = lt_bom-idnrk
                           upgvc = space
                           indx = 0
                           chk = space.

  IF sy-subrc = 0.
    l_index = sy-tabix.
    MOVE-CORRESPONDING lt_bom TO itab.
    itab-indx  = lt_bom-index.

    READ TABLE t_lfa1 WITH KEY lifnr = itab-lifnr
    BINARY SEARCH.
    IF sy-subrc = 0.
      itab-name1 = t_lfa1-name1.
      itab-land1 = t_lfa1-land1.
    ENDIF.

    itab-chk = 'X'.

    IF f_ckiuser-kalka = 'M1'.
      itab-upgvc = lt_bom-upgn.
    ENDIF.

    MODIFY itab INDEX l_index.

  ENDIF.
ENDLOOP.

IF f_ckiuser-kalka NE 'M1'.

  REFRESH: gt_ldc, gt_a902.
  CLEAR : gt_ldc, gt_a902.

  SELECT kokrs bdatj ver land1 matnr fra1 zoth zoti
    INTO TABLE gt_ldc
    FROM ztcou116
   WHERE kokrs = f_ckiuser-kokrs
     AND bdatj = f_ckiuser-bdatj
     AND ver = '00'.

  SELECT stawn kbetr INTO TABLE gt_a902
    FROM a902 AS a
    JOIN konp AS b
      ON a~knumh = b~knumh
   WHERE a~kappl = 'M'
     AND a~kschl = 'ZOA1'.

  SORT gt_ldc BY land1 matnr.
  SORT gt_a902 BY stawn.

* Modified on 10.12.2006 by Michelle >>

***get UPG
**sort lt_bom by index stufe ascending.

* get UPG
  SORT : lt_bom BY index stufe ASCENDING,
         itab BY indx,
         lt_upg BY idnrk .

  LOOP AT lt_bom.
    IF lt_bom-stufe = 1.
      w_upg = lt_bom.
    ENDIF.

    READ TABLE itab WITH KEY indx = lt_bom-index  BINARY SEARCH.

    IF sy-subrc = 0.
      l_cnt = sy-tabix.

      itab-mtart = lt_bom-mtart.
      itab-stgb  = lt_bom-stgb.

      ln = strlen( w_upg-idnrk ).
      IF ln = 9.
        CONCATENATE w_upg-idnrk(7) '0' w_upg-idnrk+7(2) INTO itab-upgvc.
      ELSE.
        itab-upgvc = w_upg-idnrk.
      ENDIF.

      READ TABLE lt_upg WITH KEY idnrk = w_upg-idnrk BINARY SEARCH.
      IF sy-subrc EQ 0.
        itab-upgtx = lt_upg-maktx.
      ENDIF.


*+ for LDC {

      IF f_ckiuser-kalka EQ 'U1'.
* Freight, Other
        READ TABLE gt_ldc WITH KEY land1 = itab-land1
                                   matnr = itab-compn.
        IF sy-subrc <> 0.
          READ TABLE gt_ldc WITH KEY land1 = itab-land1.
        ENDIF.

        IF sy-subrc = 0.

          itab-amtft = itab-wertn * ( gt_ldc-fra1 / 100 ).   " Freight
          itab-amtot = itab-wertn * ( ( gt_ldc-zoth  + gt_ldc-zoti )
                             / 100 ).    " Other

* Duty
          READ TABLE gt_a902 WITH KEY stawn = itab-stawn BINARY SEARCH.

          IF sy-subrc = 0.
            $p_duty = '2.5' * 10.
            IF $p_duty < gt_a902-kbetr.
              gt_a902-kbetr = $p_duty.
            ENDIF.

            itab-amtdt = itab-wertn * ( gt_a902-kbetr / 1000 ).

          ELSE.
            CLEAR itab-amtdt.
          ENDIF.
        ENDIF.

        IF  itab-mtart EQ 'HALB'.
          CLEAR : lt_kalnr[], lt_kalnr.
          CLEAR : it_prkeph_fsc_temp[], it_prkeph_fsc_temp.

          lt_kalnr-kalnr  = itab-kalnr.
          APPEND lt_kalnr.

          CALL FUNCTION 'MLCCS_READ_PR'
               EXPORTING
                    i_use_buffer            = space
                    i_bdatj_1               = itab-bdatj
                    i_poper_1               = itab-poper
               IMPORTING
                    et_prkeph               = it_prkeph_fsc_temp
               TABLES
                    it_kalnr                = lt_kalnr
                    ir_prtyp                = ir_prtyp
                    ir_curtp                = ir_curtp
               EXCEPTIONS
                    no_data_found           = 1
                    input_data_inconsistent = 2
                    OTHERS                  = 3.

          it_prkeph[] = it_prkeph_fsc_temp[].

          READ TABLE  it_prkeph
                      WITH KEY kkzst = space.
          IF sy-subrc EQ 0.
            itab-verpr =
            it_prkeph-kst001 +
            it_prkeph-kst003 +
            it_prkeph-kst005 +
            it_prkeph-kst007 +
            it_prkeph-kst009.
          ENDIF.

        ENDIF.

      ENDIF.                                                " when 'U1'

      itab-total = itab-wertn + itab-amtdt + itab-amtft + itab-amtot .
* }

      MODIFY itab INDEX l_cnt.

    ELSE.
      l_cnt = 0.
    ENDIF.

  ENDLOOP.

ENDIF.                                                      " not M1
