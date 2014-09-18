FUNCTION z_fpp_hma_new_wo.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IDOCNUM) LIKE  EDIDC-DOCNUM OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BAPIRETURN
*"      HEADER STRUCTURE  ZPOSEG1
*"----------------------------------------------------------------------
  DATA : l_return LIKE LINE OF return,
         w_header LIKE LINE OF header.

  DATA : lv_docnum LIKE idocnum.

*  DATA : LT_WOSUM LIKE TABLE OF ZTPP_WOSUM.
  DATA : lt_ksbohmm LIKE TABLE OF ztpp_ksbohmm WITH HEADER LINE.
  DATA : lv_objek  LIKE ausp-objek,
         lv_objekc LIKE ausp-objek,
         lt_s219   LIKE TABLE OF zv_cabn WITH HEADER LINE.

  RANGES : r_atnam  FOR cabn-atnam,
            r_atnamc FOR cabn-atnam.
  lv_docnum = idocnum.
**#0. Set Range For 'P_219%' .
  r_atnam-low = 'P_219*'.
  r_atnam-option = 'CP'.
  r_atnam-sign = 'I'.
  APPEND r_atnam.

  SORT header BY prdod natn dist wkexc wkinc.

  SELECT
    wo_ser
    nation
    dealer
    extc
    intc
    moye
    bmdl
    ocnn
    vers
    initqty
    modqty
    req_date
    crt_date
    chg_date
    dest
    lcnt
    lcno
    regn
    orzn
    clsr
    flet
    maor
    spec
    s219
  INTO CORRESPONDING FIELDS OF TABLE lt_ksbohmm
    FROM ztpp_ksbohmm
    FOR ALL ENTRIES IN header
    WHERE wo_ser = header-prdod
      AND nation = header-natn
      AND dealer = header-dist
      AND zmode  = ''
      AND zresult = ''.

  CLEAR :  lt_ksbohmm-initqty , lt_ksbohmm-modqty.
  MODIFY lt_ksbohmm TRANSPORTING initqty modqty
  WHERE extc = '***' AND intc = '***'.


  CLEAR : lt_ksbohmm.

  LOOP AT header INTO w_header .

    READ TABLE lt_ksbohmm WITH KEY
      wo_ser	          =	w_header-prdod
      nation	          =	w_header-natn
      dealer	          =	w_header-dist
      extc	          =	w_header-wkexc
      intc	          =	w_header-wkinc.
    IF sy-subrc EQ 0.
      DELETE TABLE lt_ksbohmm.
    ENDIF.
    					
    lt_ksbohmm-wo_ser	  =	w_header-prdod.
    lt_ksbohmm-nation	  =	w_header-natn.
    lt_ksbohmm-dealer	  =	w_header-dist.
    lt_ksbohmm-extc	         =	w_header-wkexc.
    lt_ksbohmm-intc	         =	w_header-wkinc.
    lt_ksbohmm-moye	         =	w_header-mdyr.
*    LT_KSBOHMM-BMDL	   =	W_HEADER-MDINX. "Commented Victor 06.29.2011
*    LT_KSBOHMM-OCNN	   =	W_HEADER-OCCN.
*    LT_KSBOHMM-VERS           =   W_HEADER-VERS.

    lt_ksbohmm-initqty        =	w_header-ioqty.
    lt_ksbohmm-modqty         =	w_header-moqty.
    lt_ksbohmm-req_date       =	w_header-rdd.

    lt_ksbohmm-crt_date       =	w_header-crdat.

*   LT_KSBOHMM-ZSDAT          =	W_HEADER-CRDAT.

    lt_ksbohmm-crt_date       =	w_header-crdat.
    lt_ksbohmm-chg_date       =	w_header-aedat.

    lt_ksbohmm-dest	         =	w_header-destn.
    lt_ksbohmm-lcnt	         =	w_header-lccnt.
    lt_ksbohmm-lcno	         =	w_header-lcldl.
    lt_ksbohmm-flet	         =	w_header-fltfg.

    CONCATENATE w_header-woups  w_header-natn w_header-dist
    INTO lv_objek.
    AT END OF dist.
      CLEAR : lt_ksbohmm-vers , lt_ksbohmm-s219,
              lt_ksbohmm-ocnn , lt_ksbohmm-bmdl.
      PERFORM get_eord_inform USING : 'V' lv_objek lt_ksbohmm-vers,
                                      'C' lv_objekc lt_ksbohmm-ocnn,
                                      'M' lv_objekc lt_ksbohmm-bmdl,
                                      'S' lv_objek lt_ksbohmm-s219.
    ENDAT.

    APPEND lt_ksbohmm.
  ENDLOOP.

*  DATA : LT_TEMP LIKE TABLE OF LT_KSBOHMM WITH HEADER LINE.

  DATA : lt_ksbohmm_h LIKE TABLE OF lt_ksbohmm WITH HEADER LINE.
  CLEAR : lt_ksbohmm_h , lt_ksbohmm_h[].

  SORT lt_ksbohmm BY wo_ser nation dealer extc intc .

  LOOP AT lt_ksbohmm
      WHERE extc NE '***' AND intc NE '***'.

    lt_ksbohmm-extc = '***'.
    lt_ksbohmm-intc = '***'.

    COLLECT lt_ksbohmm INTO lt_ksbohmm_h.
  ENDLOOP.

  CLEAR : lt_ksbohmm.
  APPEND  LINES OF lt_ksbohmm_h TO lt_ksbohmm.


  PERFORM conversion_output USING lv_docnum.
  lt_ksbohmm-zmsg      = lv_docnum."'From IDOC Data'.
  lt_ksbohmm-zsdat     = sy-datum.
  lt_ksbohmm-zstim     = sy-uzeit.
  lt_ksbohmm-zuser     = sy-uname.

  MODIFY lt_ksbohmm
  TRANSPORTING zmsg zsdat zstim zuser WHERE zuser EQ ''.

**#1 Delete Data wo_ser is null.
  DELETE FROM ztpp_ksbohmm WHERE wo_ser IS null.
  DELETE ztpp_ksbohmm FROM TABLE lt_ksbohmm.

  SORT lt_ksbohmm BY wo_ser nation dealer extc intc .
**#2 Duplicated date delete
  DELETE ADJACENT DUPLICATES FROM lt_ksbohmm
  COMPARING wo_ser nation dealer extc intc .



  INSERT ztpp_ksbohmm
    FROM TABLE lt_ksbohmm ACCEPTING DUPLICATE KEYS .

  IF sy-subrc <> 0 .
    UPDATE ztpp_po_idoc SET
      status = 'E'
       erdat = sy-datum
       ertim = sy-uzeit
       uname = sy-uname
      WHERE docnum = lv_docnum
      AND status = ''.

  ELSE.
    LOOP AT lt_ksbohmm.
      UPDATE ztpp_po_idoc SET
      status = 'S'
       erdat = sy-datum
       ertim = sy-uzeit
       uname = sy-uname
      WHERE docnum = lv_docnum
        AND status = ''.
    ENDLOOP.

  ENDIF.

  PERFORM get_ksbo USING l_return.
  return = l_return.
  APPEND return.

*  CHECK L_RETURN-CODE IS INITIAL.
*  CLEAR : RETURN , RETURN[].
*  PERFORM PRE_CHECKING_DATA  ." USING L_RETURN.
*  PERFORM PRE_CHECKING_DATA2 ." USING L_RETURN.
*  PERFORM BDC_PROCESSING   .
*  PERFORM WRITE_SUCCESS.
*  PERFORM WRITE_END.
*  IF WA_EMPTY IS INITIAL.                                   "UD1K913223
*    UPDATE ZTPP_COMMON_VALS SET:
*    DATES = WA_DATE
*    ITEM1 = WA_DATE2
*     WHERE JOBS
*     = 'ZAPP703C_WORKORDER_IF' .
*  ENDIF.
*  PERFORM WRITE_VIN_SPEC.

*  PERFORM DELETE_JOBLIST.

ENDFUNCTION.
