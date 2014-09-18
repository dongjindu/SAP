FUNCTION z_fpp_hma_new_wo_if.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IDOCNUM) LIKE  EDIDC-DOCNUM OPTIONAL
*"     VALUE(FLAG) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      RETURN STRUCTURE  BAPIRETURN
*"      HEADER STRUCTURE  ZPOSEG1
*"----------------------------------------------------------------------
  DATA : l_return LIKE LINE OF return,
         w_header LIKE LINE OF header.

  DATA : lv_docnum LIKE idocnum.

*  DATA : LT_WOSUM LIKE TABLE OF ZTPP_WOSUM.
  DATA : lt_ksbohmm LIKE TABLE OF ztpp_ksbohmm_if WITH HEADER LINE.
  DATA : lv_objek  LIKE ausp-objek,
         lv_objekc LIKE ausp-objek,
         lt_s219   LIKE TABLE OF zv_cabn WITH HEADER LINE.
  DATA : lt_sodata LIKE TABLE OF ztsd_sodata WITH HEADER LINE.

  RANGES : r_atnam  FOR cabn-atnam,
            r_atnamc FOR cabn-atnam.
  gv_docnum = lv_docnum = idocnum.

**#0. Set Range For 'P_219%' .
  r_atnam-low = 'P_219*'.
  r_atnam-option = 'CP'.
  r_atnam-sign = 'I'.
  APPEND r_atnam.

  SORT header BY prdod natn dist wkexc wkinc.

  DELETE FROM ztpp_ksbohmm_if WHERE wo_ser NE ''.
  DELETE FROM ztpp_ksbohmm_if WHERE wo_ser EQ ''.

  CLEAR :  lt_ksbohmm.

  SORT header BY prdod natn dist wkexc wkinc.
  LOOP AT header INTO w_header .

    READ TABLE lt_ksbohmm WITH KEY
      wo_ser            = w_header-prdod
      nation            = w_header-natn
      dealer            = w_header-dist
      extc                  = w_header-wkexc
      intc                  = w_header-wkinc.

    IF sy-subrc EQ 0.
      DELETE TABLE lt_ksbohmm.
      lt_ksbohmm-extc = '***'.
      lt_ksbohmm-intc = '***'.
      DELETE TABLE lt_ksbohmm.
    ENDIF.

    lt_ksbohmm-wo_ser      =  w_header-prdod.
    lt_ksbohmm-nation      =  w_header-natn.
    lt_ksbohmm-dealer      =  w_header-dist.
    lt_ksbohmm-extc      =  w_header-wkexc.
    lt_ksbohmm-intc      =  w_header-wkinc.
    lt_ksbohmm-moye      =  w_header-mdyr.
*    lt_ksbohmm-bmdl       =  w_header-mdinx. "Commented Victor 06.29.11
*    lt_ksbohmm-ocnn       =  w_header-occn.

    lt_ksbohmm-initqty        =	w_header-ioqty.
    lt_ksbohmm-modqty         =	w_header-moqty.
    lt_ksbohmm-req_date       =	w_header-rdd.

    lt_ksbohmm-crt_date       =	w_header-crdat.

    lt_ksbohmm-crt_date       =	w_header-crdat.
    lt_ksbohmm-chg_date       =	w_header-aedat.

    lt_ksbohmm-dest          =  w_header-destn.
    lt_ksbohmm-lcnt          =  w_header-lccnt.
    lt_ksbohmm-lcno          =  w_header-lcldl.
    lt_ksbohmm-flet          =  w_header-fltfg.

    CONCATENATE w_header-woups  w_header-natn w_header-dist
    INTO lv_objek.

    CONCATENATE w_header-woups  w_header-natn w_header-dist
    w_header-wkexc w_header-wkinc
    INTO lv_objekc.

*-  MC, OCN, Version, 219 -> From Ausp "Victor 06.29.2011
    CLEAR : lt_ksbohmm-vers , lt_ksbohmm-s219,
            lt_ksbohmm-bmdl,  lt_ksbohmm-ocnn.
    PERFORM get_eord_inform USING : 'V' lv_objekc lt_ksbohmm-vers,
                                    'C' lv_objekc lt_ksbohmm-ocnn,
                                    'M' lv_objekc lt_ksbohmm-bmdl,
                                    'S' lv_objek  lt_ksbohmm-s219.
    APPEND lt_ksbohmm.
  ENDLOOP.

  DATA : lt_ksbohmm_h LIKE TABLE OF lt_ksbohmm WITH HEADER LINE.
  CLEAR : lt_ksbohmm_h , lt_ksbohmm_h[].
  SORT lt_ksbohmm BY wo_ser nation dealer extc intc.

  LOOP AT lt_ksbohmm WHERE extc NE '***'
                       AND intc NE '***'.

    lt_ksbohmm-extc = '***'.
    lt_ksbohmm-intc = '***'.
    CLEAR : lt_ksbohmm-lcnt.
    COLLECT lt_ksbohmm INTO lt_ksbohmm_h.
  ENDLOOP.

  CLEAR : lt_ksbohmm.
  SORT lt_ksbohmm_h BY wo_ser nation dealer bmdl DESCENDING
                                            s219 DESCENDING
                                            ocnn DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_ksbohmm_h
         COMPARING wo_ser nation dealer.
  APPEND  LINES OF lt_ksbohmm_h TO lt_ksbohmm.

  PERFORM conversion_output USING lv_docnum.
  lt_ksbohmm-zmsg      = lv_docnum."'From IDOC Data'.
  lt_ksbohmm-zsdat     = sy-datum.
  lt_ksbohmm-zstim     = sy-uzeit.
  lt_ksbohmm-zuser     = sy-uname.

  MODIFY lt_ksbohmm
  TRANSPORTING zmsg zsdat zstim zuser WHERE zuser EQ ''.

**#1 Delete Data wo_ser is null.
  DELETE FROM ztpp_ksbohmm_if WHERE wo_ser IS NULL.
  DELETE ztpp_ksbohmm_if FROM TABLE lt_ksbohmm.

  SORT lt_ksbohmm BY wo_ser nation dealer extc intc .
**#2 Duplicated date delete
  DELETE ADJACENT DUPLICATES FROM lt_ksbohmm
  COMPARING wo_ser nation dealer extc intc .

*--< Victor 06.30.2011
  CLEAR : lt_sodata[], lt_sodata.
  LOOP AT lt_ksbohmm WHERE extc  <> '***'.
    MOVE-CORRESPONDING lt_ksbohmm TO lt_sodata.

    CONCATENATE '20' lt_sodata-wo_ser+1(2) INTO lt_sodata-poyear.
    lt_sodata-pomonth  =  lt_sodata-wo_ser+3(2).

*    lt_sodata-REGIONN
*    lt_sodata-PORT
    CLEAR : w_header.
    READ TABLE header INTO w_header WITH KEY
                        prdod	          =	lt_ksbohmm-wo_ser
                        natn            = lt_ksbohmm-nation
                        dist            = lt_ksbohmm-dealer
                        wkexc	          =	lt_ksbohmm-extc
                        wkinc	          =	lt_ksbohmm-intc.
    IF sy-subrc = 0.
      lt_sodata-woups    =  w_header-woups.
    ENDIF.

    lt_sodata-ordqty =  lt_ksbohmm-modqty.
    lt_sodata-p_flag  =  'S'.

    APPEND lt_sodata.
    CLEAR : lt_sodata.
  ENDLOOP.

  INSERT ztsd_sodata FROM TABLE lt_sodata ACCEPTING DUPLICATE KEYS.
*-->

  INSERT ztpp_ksbohmm_if
    FROM TABLE lt_ksbohmm ACCEPTING DUPLICATE KEYS .

  IF sy-subrc <> 0 .
    UPDATE ztpp_po_idoc SET
        status = 'E'
        erdat = sy-datum
        ertim = sy-uzeit
        uname = sy-uname
    WHERE docnum = idocnum
      AND status = ''.


    l_return-type = 'E'.
    l_return-code = idocnum.
    l_return-message =
              ' [ZTPP_KSBOHMM_IF] Interface Data insert Failed '  .
    APPEND l_return TO return.

    EXIT.

  ENDIF.

  PERFORM get_ksbo USING l_return.

  return = l_return.

  IF NOT l_return IS INITIAL.
    APPEND  l_return TO return.
  ENDIF.

  CHECK l_return-code IS INITIAL.
  CLEAR : return .

  PERFORM pre_checking_data  ." USING L_RETURN.
  PERFORM pre_checking_data2 ." USING L_RETURN.
  PERFORM bdc_processing   .
  PERFORM write_success USING lv_docnum l_return-message.

*  PERFORM WRITE_SUCCESS.
  IF sy-subrc = 0 .
    l_return-type = 'S'.
    l_return-code = idocnum.
    l_return-message = 'ENDPO'.
    APPEND l_return TO return.
  ENDIF.

*  PERFORM WRITE_END.
  IF wa_empty IS INITIAL.                                   "UD1K913223
    UPDATE ztpp_common_vals SET:
        dates = wa_date
        item1 = wa_date2
     WHERE jobs = 'ZAPP703C_WORKORDER_IF' .
  ENDIF.

ENDFUNCTION.
*FUNCTION Z_FPP_HMA_NEW_WO_IF.
**"---------------------------------------------------------------------
*-
**"*"Local interface:
**"  IMPORTING
**"     VALUE(IDOCNUM) LIKE  EDIDC-DOCNUM OPTIONAL
**"  TABLES
**"      RETURN STRUCTURE  BAPIRETURN
**"      HEADER STRUCTURE  ZPOSEG1
**"---------------------------------------------------------------------
*-
*  DATA : L_RETURN LIKE LINE OF RETURN,
*         W_HEADER LIKE LINE OF HEADER.
*
*  DATA : LV_DOCNUM LIKE IDOCNUM.
*
**  DATA : LT_WOSUM LIKE TABLE OF ZTPP_WOSUM.
*  DATA : LT_KSBOHMM LIKE TABLE OF ZTPP_KSBOHMM_IF WITH HEADER LINE.
*  DATA : LV_OBJEK  LIKE AUSP-OBJEK,
*         LV_OBJEKC LIKE AUSP-OBJEK,
*         LT_S219   LIKE TABLE OF ZV_CABN WITH HEADER LINE.
*
*  RANGES : R_ATNAM  FOR CABN-ATNAM,
*            R_ATNAMC FOR CABN-ATNAM.
*  GV_DOCNUM = LV_DOCNUM = IDOCNUM.
*
***#0. Set Range For 'P_219%' .
*  R_ATNAM-LOW = 'P_219*'.
*  R_ATNAM-OPTION = 'CP'.
*  R_ATNAM-SIGN = 'I'.
*  APPEND R_ATNAM.
*
*  SORT HEADER BY PRDOD NATN DIST WKEXC WKINC.
*
*  SELECT
*    WO_SER
*    NATION
*    DEALER
*    EXTC
*    INTC
*    MOYE
*    BMDL
*    OCNN
*    VERS
*    INITQTY
*    MODQTY
*    REQ_DATE
*    CRT_DATE
*    CHG_DATE
*    DEST
*    LCNT
*    LCNO
*    REGN
*    ORZN
*    CLSR
*    FLET
*    MAOR
*    SPEC
*    S219
*  INTO CORRESPONDING FIELDS OF TABLE LT_KSBOHMM
*    FROM ZTPP_KSBOHMM_IF
*    FOR ALL ENTRIES IN HEADER
*    WHERE WO_SER = HEADER-PRDOD
*      AND NATION = HEADER-NATN
*      AND DEALER = HEADER-DIST
*      AND ZMODE  = ''
*      AND ZRESULT = ''.
*
*  CLEAR :  LT_KSBOHMM-INITQTY , LT_KSBOHMM-MODQTY.
*  MODIFY LT_KSBOHMM TRANSPORTING INITQTY MODQTY
*  WHERE EXTC = '***' AND INTC = '***'.
*
*
*  CLEAR : LT_KSBOHMM.
*
*  SORT HEADER BY PRDOD NATN DIST WKEXC WKINC.
*
*  LOOP AT HEADER INTO W_HEADER .
*
*    READ TABLE LT_KSBOHMM WITH KEY
*      WO_SER	          =	W_HEADER-PRDOD
*      NATION	          =	W_HEADER-NATN
*      DEALER	          =	W_HEADER-DIST
*      EXTC	                =	W_HEADER-WKEXC
*      INTC	                =	W_HEADER-WKINC.
*    IF SY-SUBRC EQ 0.
*      DELETE TABLE LT_KSBOHMM.
*    ENDIF.
*
*    LT_KSBOHMM-WO_SER       =  W_HEADER-PRDOD.
*    LT_KSBOHMM-NATION       =  W_HEADER-NATN.
*    LT_KSBOHMM-DEALER       =  W_HEADER-DIST.
*    LT_KSBOHMM-EXTC       =  W_HEADER-WKEXC.
*    LT_KSBOHMM-INTC       =  W_HEADER-WKINC.
*    LT_KSBOHMM-MOYE       =  W_HEADER-MDYR.
*    LT_KSBOHMM-BMDL       =  W_HEADER-MDINX.
*    LT_KSBOHMM-OCNN       =  W_HEADER-OCCN.
*
*    LT_KSBOHMM-INITQTY        =  W_HEADER-IOQTY.
*    LT_KSBOHMM-MODQTY         =  W_HEADER-MOQTY.
*    LT_KSBOHMM-REQ_DATE       =  W_HEADER-RDD.
*
*    LT_KSBOHMM-CRT_DATE       =  W_HEADER-CRDAT.
*
*    LT_KSBOHMM-CRT_DATE       =  W_HEADER-CRDAT.
*    LT_KSBOHMM-CHG_DATE       =  W_HEADER-AEDAT.
*
*    LT_KSBOHMM-DEST           =  W_HEADER-DESTN.
*    LT_KSBOHMM-LCNT           =  W_HEADER-LCCNT.
*    LT_KSBOHMM-LCNO           =  W_HEADER-LCLDL.
*    LT_KSBOHMM-FLET           =  W_HEADER-FLTFG.
*
*    CONCATENATE W_HEADER-WOUPS  W_HEADER-NATN W_HEADER-DIST
*    INTO LV_OBJEK.
*
*    CONCATENATE W_HEADER-WOUPS  W_HEADER-NATN W_HEADER-DIST
*    W_HEADER-WKEXC W_HEADER-WKINC
*    INTO LV_OBJEKC.
*
*    CLEAR : LT_KSBOHMM-VERS , LT_KSBOHMM-S219.
*    PERFORM GET_EORD_INFORM USING : 'V' LV_OBJEKC LT_KSBOHMM-VERS,
*                                    'S' LV_OBJEK  LT_KSBOHMM-S219.
*    APPEND LT_KSBOHMM.
*  ENDLOOP.
*
**  DATA : LT_TEMP LIKE TABLE OF LT_KSBOHMM WITH HEADER LINE.
*  DATA : LT_KSBOHMM_H LIKE TABLE OF LT_KSBOHMM WITH HEADER LINE.
*  CLEAR : LT_KSBOHMM_H , LT_KSBOHMM_H[].
*  SORT LT_KSBOHMM BY WO_SER NATION DEALER EXTC INTC .
*
*  LOOP AT LT_KSBOHMM
*      WHERE EXTC NE '***' AND INTC NE '***'.
*
*    LT_KSBOHMM-EXTC = '***'.
*    LT_KSBOHMM-INTC = '***'.
*
*    COLLECT LT_KSBOHMM INTO LT_KSBOHMM_H.
*  ENDLOOP.
*
*  CLEAR : LT_KSBOHMM.
*  APPEND  LINES OF LT_KSBOHMM_H TO LT_KSBOHMM.
*
*  PERFORM CONVERSION_OUTPUT USING LV_DOCNUM.
*  LT_KSBOHMM-ZMSG      = LV_DOCNUM."'From IDOC Data'.
*  LT_KSBOHMM-ZSDAT     = SY-DATUM.
*  LT_KSBOHMM-ZSTIM     = SY-UZEIT.
*  LT_KSBOHMM-ZUSER     = SY-UNAME.
*
*  MODIFY LT_KSBOHMM
*  TRANSPORTING ZMSG ZSDAT ZSTIM ZUSER WHERE ZUSER EQ ''.
*
***#1 Delete Data wo_ser is null.
*  DELETE FROM ZTPP_KSBOHMM_IF WHERE WO_SER IS NULL.
*  DELETE ZTPP_KSBOHMM_IF FROM TABLE LT_KSBOHMM.
*
*  SORT LT_KSBOHMM BY WO_SER NATION DEALER EXTC INTC .
***#2 Duplicated date delete
*  DELETE ADJACENT DUPLICATES FROM LT_KSBOHMM
*  COMPARING WO_SER NATION DEALER EXTC INTC .
*
*
*
*  INSERT ZTPP_KSBOHMM_IF
*    FROM TABLE LT_KSBOHMM ACCEPTING DUPLICATE KEYS .
*
*  IF SY-SUBRC <> 0 .
*    UPDATE ZTPP_PO_IDOC SET
*    STATUS = 'E'
*     ERDAT = SY-DATUM
*     ERTIM = SY-UZEIT
*     UNAME = SY-UNAME
*    WHERE DOCNUM = IDOCNUM
*      AND STATUS = ''.
*
*
*    L_RETURN-TYPE = 'E'.
*    L_RETURN-CODE = IDOCNUM.
*    L_RETURN-MESSAGE =
*    ' [ZTPP_KSBOHMM_IF] Interface Data insert Failed '  .
*    APPEND L_RETURN TO RETURN.
*
*    EXIT.
*
*  ENDIF.
*
*  PERFORM GET_KSBO USING L_RETURN.
*  RETURN = L_RETURN.
*
*  IF NOT L_RETURN IS INITIAL.
*    APPEND  L_RETURN TO RETURN.
*  ENDIF.
*
*  CHECK L_RETURN-CODE IS INITIAL.
*  CLEAR : RETURN .
*
*  PERFORM PRE_CHECKING_DATA  ." USING L_RETURN.
*  PERFORM PRE_CHECKING_DATA2 ." USING L_RETURN.
*  PERFORM BDC_PROCESSING   .
*
*  IF SY-SUBRC = 0 .
*    L_RETURN-TYPE = 'S'.
*    L_RETURN-CODE = IDOCNUM.
*    L_RETURN-MESSAGE = 'ENDPO'.
*    APPEND L_RETURN TO RETURN.
*  ENDIF.
**  PERFORM WRITE_END.
*  IF WA_EMPTY IS INITIAL.                                   "UD1K913223
*    UPDATE ZTPP_COMMON_VALS SET:
*    DATES = WA_DATE
*    ITEM1 = WA_DATE2
*     WHERE JOBS
*     = 'ZAPP703C_WORKORDER_IF' .
*  ENDIF.
**  PERFORM WRITE_SUCCESS USING LV_DOCNUM L_RETURN-MESSAGE.
**  IF SY-SUBRC = 0 .
**    L_RETURN-TYPE = 'S'.
**    L_RETURN-CODE = IDOCNUM.
**    APPEND L_RETURN TO RETURN.
**  ENDIF.
***  PERFORM WRITE_END.
**  IF WA_EMPTY IS INITIAL.
*"UD1K913223
**    UPDATE ZTPP_COMMON_VALS SET:
**    DATES = WA_DATE
**    ITEM1 = WA_DATE2
**     WHERE JOBS
**     = 'ZAPP703C_WORKORDER_IF' .
**  ENDIF.
**
*  PERFORM DELETE_JOBLIST.
*ENDFUNCTION.
