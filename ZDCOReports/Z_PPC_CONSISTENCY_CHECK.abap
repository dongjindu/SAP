REPORT Z_PPC_CONSISTENCY_CHECK line-size 155 .

*The Purpose of this report is to correct any discrepancies that had
*occurred between PPC and Material document tables. Prior to running
*this report for a particular date and material, ensure all
*confirmations are processed by PPCGO for this date and material.


TYPE-POOLS: PPCPR.

tables : ppc1_all,
         mseg,
         mkpf,
         ppc_head, ppc_conf_mat,
         ppc_mat_det,
         ppc_mat.


* Selection screen

SELECT-OPTIONS: SO_DATE  FOR ppc1_all-postdate,
                so_matnr for ppc1_all-matnr,
                SO_WERKS FOR ppc1_all-werks,
                so_locn  for ppc1_all-lgort,
                so_amty for mseg-bwart.


Parameters: no_ofdoc type i obligatory,
            pos_mty type mseg-bwart obligatory,
            neg_mty type mseg-bwart obligatory,
            test_flg as checkbox default 'X'.

data:  it_complist type ppc_t_apocomplist_ext ,
       LT_IMSEG LIKE IMSEG OCCURS 0 with header line,
       ls_imseg like imseg.

data:  LS_IMKPF LIKE IMKPF,
       LS_EMKPF LIKE EMKPF.

DATA: LT_EMSEG LIKE EMSEG OCCURS 0.

DATA: LS_DOC TYPE PPCPR_TYPE_TAB_BELNR.
data: ls_doc_line type PPCPR_TYPE_BELNR.


data : CHAR_MINUS TYPE C VALUE '-',
       CF_GMOVE_WE    LIKE PPC_MAT-GMOVE_IND VALUE 1,
       CF_GMOVE_WE_CO LIKE PPC_MAT-GMOVE_IND VALUE 2,
       CF_GMOVE_WE_BY LIKE PPC_MAT-GMOVE_IND VALUE 3,
       CHARF TYPE C VALUE 'F',
       CHARX TYPE C VALUE 'X',
       CHARS TYPE C VALUE 'S',
       CHARH TYPE C VALUE 'H',
       CHAR0 TYPE C VALUE '0',
       CHARE TYPE c VALUE 'E',
       CHARQ TYPE c VALUE 'Q',
       c_flg_no_reversal VALUE ' ',
       c_flg_reversal VALUE 'X',
       c_gmove_ind_0 VALUE '0'.


data:  CF_TCODE_PPCGO LIKE SY-TCODE VALUE 'PPCGO',
       CF_GMOVE_WA    LIKE PPC_MAT-GMOVE_IND VALUE '0'.
*
data : begin of lt_info_tab occurs 0,
        budat     like mkpf-budat,
        matnr     like ppc1_all-matnr,
        aufnr     like QRP002-AUFNR,
        werks     like ppc1_all-werks,
        lgort     like ppc1_all-lgort,
        charg     like ppc1_all-komp_charg,
        mat_kdauf     like PPC_APOCOMPLIST_EXT-kdauf,
        mat_kdpos     like PPC_APOCOMPLIST_EXT-kdpos,
        ps_psp_pnr like imseg-ps_psp_pnr,
        lgnum    like PPC_APOCOMPLIST_EXT-LGNUM,
        LGTYP    like PPC_APOCOMPLIST_EXT-LGTYP,
        lgpla    like PPC_APOCOMPLIST_EXT-LGPLA,
        berkz    like PPC_APOCOMPLIST_EXT-berkz,
        sobkz    like mseg-sobkz,
        lifnr    like mseg-lifnr,
        kunnr    like mseg-kunnr,
        KZVBR    like mseg-kzvbr,
        kzbws    like mseg-kzbws,
        kzech    like imseg-kzech,
        kzbew    like mseg-kzbew,
        aufps    like imseg-aufps,
        xwait    like imseg-xwait,
        prvbe    like ppc1_all-prvbe,
        ppc_qty  like mseg-menge,
        mseg_qty like mseg-menge,
        diff     like mseg-menge,
        MEINS like MSEG-MEINS,
        BWART like MSEG-BWART,
        SHKZG LIKE MSEG-SHKZG,
        mat_doc_notfound type c,
        do_notprocess type c,
       end of lt_info_tab.

* Material document table - MKPF & MSEG
data: begin of mat_doc_itab occurs 0.
data: BUDAT like MKPF-BUDAT .
data: MATNR like MSEG-MATNR .
data: ANLN1 like MSEG-ANLN1 .
data: ANLN2 like MSEG-ANLN2 .
data: APLZL like MSEG-APLZL .
data: AUFNR like MSEG-AUFNR .
data: AUFPL like MSEG-AUFPL .
data: BERKZ like MSEG-BERKZ .
data: BKTXT like MKPF-BKTXT .
data: BLDAT like MKPF-BLDAT .
data: BPMNG like MSEG-BPMNG .
data: BPRME like MSEG-BPRME .
data: BSTME like MSEG-BSTME .
data: BSTMG like MSEG-BSTMG .
data: BUKRS like MSEG-BUKRS .
data: BWART like MSEG-BWART .
data: BWTAR like MSEG-BWTAR .
data: CHARG like MSEG-CHARG .
data: CPUDT like MKPF-CPUDT .
data: CPUTM like MKPF-CPUTM .
data: DMBTR like MSEG-DMBTR .
data: EBELN like MSEG-EBELN .
data: EBELP like MSEG-EBELP .
data: ERFME like MSEG-ERFME .
data: ERFMG like MSEG-ERFMG .
data: EXBWR like MSEG-EXBWR .
data: EXVKW like MSEG-EXVKW .
data: GRUND like MSEG-GRUND .
data: KDAUF like MSEG-KDAUF .
data: KDEIN like MSEG-KDEIN .
data: KDPOS like MSEG-KDPOS .
data: KOSTL like MSEG-KOSTL .
data: KUNNR like MSEG-KUNNR .
data: KZBEW like MSEG-KZBEW .
data: KZVBR like MSEG-KZVBR .
data: KZZUG like MSEG-KZZUG .
data: LGORT like MSEG-LGORT .
data: lgnum like MSEG-LGNUM .
data: LGTYP like MSEG-LGTYP .
data: LGPLA like MSEG-LGPLA .
data: LIFNR like MSEG-LIFNR .
data: MAT_KDAUF like MSEG-KDAUF.
data: MAT_KDPOS like MSEG-KDPOS.
data: MBLNR like MKPF-MBLNR .
data: MEINS like MSEG-MEINS .
data: MENGE like MSEG-MENGE .
data: MJAHR like MKPF-MJAHR .
data: NPLNR like MSEG-NPLNR .
data: PS_PSP_PNR like MSEG-PS_PSP_PNR .
data: RSNUM like MSEG-RSNUM .
data: RSPOS like MSEG-RSPOS .
data: SHKZG like MSEG-SHKZG .
data: SOBKZ like MSEG-SOBKZ .
data: USNAM like MKPF-USNAM .
data: VGART like MKPF-VGART .
data: VKWRT like MSEG-VKWRT .
data: WAERS like MSEG-WAERS .
data: WERKS like MSEG-WERKS .
data: XAUTO like MSEG-XAUTO .
data: ZEILE like MSEG-ZEILE .
data: end of mat_doc_itab.

*
data: begin of mat_doc_process occurs 0.
data: BUDAT like MKPF-BUDAT .
data: MATNR like MSEG-MATNR .
data: AUFNR like MSEG-AUFNR .
data: BLDAT like MKPF-BLDAT .
data: BUKRS like MSEG-BUKRS .
data: BWART like MSEG-BWART .
data: DMBTR like MSEG-DMBTR .
data: ERFME like MSEG-ERFME .
data: ERFMG like MSEG-ERFMG .
data: EXBWR like MSEG-EXBWR .
data: EXVKW like MSEG-EXVKW .
data: GRUND like MSEG-GRUND .
data: lgnum like MSEG-LGNUM .
data: LGTYP like MSEG-LGTYP .
data: LGPLA like MSEG-LGPLA .
data: BERKZ like MSEG-BERKZ .
data: LIFNR like MSEG-LIFNR .
data: LGORT like MSEG-LGORT .
data: MEINS like MSEG-MEINS .
data: MENGE like MSEG-MENGE .
data: MJAHR like MKPF-MJAHR .
data: SHKZG like MSEG-SHKZG .
data: WAERS like MSEG-WAERS .
data: WERKS like MSEG-WERKS .
data: CHARG like MSEG-CHARG .
data: KDAUF like MSEG-KDAUF .
data: KDPOS like MSEG-KDPOS .
data: MAT_KDAUF like MSEG-MAT_KDAUF.
data: MAT_KDPOS like MSEG-MAT_KDPOS.
data: SOBKZ like MSEG-SOBKZ.
data: KUNNR like MSEG-KUNNR.
data: KZECH like IMSEG-KZECH.
data: AUFPS like MSEG-AUFPS.
data: KZBEW like MSEG-KZBEW.
data: XWAIT like IMSEG-XWAIT.
data: end of mat_doc_process.

*Records that need to be processed using PPCGO
data: begin of lt_ppcgo occurs 0,
       matnr like ppc1_all-matnr,
       budat like ppc1_all-postdate,
      end of lt_ppcgo.

data: ls_ppc_qty like mseg-menge,
      ls_mseg_qty like mseg-menge,
      lv_cnt      type i,
      lv_chg_of_dt type c,
      lv_new_budat like mkpf-budat,
      lv_old_budat like mkpf-budat,
      lv_doc_lines type i,
      lv_version type   PPC_CONF_DATA_AGGR-version,
      lm_wmrel     type c,
      lv_found_ppcgo type c.

* Details from PPC Tables
DATA: BEGIN OF lt_ppc_tab,
       budat      like ppc_head-postdate,
       mat_number like ppc_material_components-mat_number,
       plant      like ppc_material_components-plant,
       storage_loc like ppc_material_components-storage_loc,
       batch       like ppc_material_components-batch,
       supply_area like ppc_material_components-supply_area,
       SPECIAL_STOCK like ppc_material_components-SPECIAL_STOCK,
       SALES_DOC  like ppc_material_components-SALES_DOC,
       SALES_DOC_ITEM like ppc_material_components-SALES_DOC_ITEM,
       WBS_ELEM  like ppc_material_components-WBS_ELEM,
       COSTING_NUM like ppc_material_components-COSTING_NUM,
       SPECIAL_STOCK_VAL like ppc_material_components-SPECIAL_STOCK_VAL,
       CONSUMPT_POSTING  like ppc_material_components-CONSUMPT_POSTING,
       ACCASS_CATEGORY  like ppc_material_components-ACCASS_CATEGORY,
       DEBIT_CREDIT_IND like ppc_material_components-DEBIT_CREDIT_IND,
       MOVEMENT_TYPE    like ppc_material_components-MOVEMENT_TYPE,
       QUANTITY         like ppc_material_components-QUANTITY,
       UNIT_OF_MEASURE  like ppc_material_components-UNIT_OF_MEASURE,
       GMOVE_IND        like ppc_material_components-GMOVE_IND,
       flg_synch        like ppc_head-flg_synch,
       flg_asynch       like ppc_head-flg_asynch,
       accassobj like ppc_head-accassobj,
       head_sales_doc TYPE kdauf,
       head_sales_doc_item TYPE kdpos,
       head_wbs_elem TYPE ps_psp_pnr,
       orderid          like ppc_head-orderid,
       prvbe            like ppc_mat-prvbe,
       lgnum            like PPC_APOCOMPLIST_EXT-lgnum,
       lgtyp            like PPC_APOCOMPLIST_EXT-lgtyp,
       lgpla            like PPC_APOCOMPLIST_EXT-lgpla,
       berkz            like PPC_APOCOMPLIST_EXT-berkz,
       aufnr            like QRP002-AUFNR,
       lifnr            like ppc_mat_det-lifnr,
       kunnr            like ppc_mat_det-kunnr,
      END OF lt_ppc_tab.

DATA: ls_ext_rev_mat_comp like lt_ppc_tab occurs 0 with header line,
      ls_ext_mat_comp like lt_ppc_tab,
      lt_material_components like lt_ppc_tab occurs 0 with header line,
      lt_rev_mat_components like lt_ppc_tab occurs 0 with header line,
      ls_material_components like lt_ppc_tab,
      ls_rev_mat_components like lt_ppc_tab occurs 0 with header line.

*Top of page

top-of-page.
  perform print_head.

*Start of selection.

START-OF-SELECTION.

* Data Prepare - from PPC and Material document tables
  PERFORM DATA_PREPARE tables lt_material_components
                              lt_rev_mat_components
                               mat_doc_itab.


  LOOP AT lt_material_components.
*Check if this record is processed by PPCGO, if not collect that in an
*internal table and display the contents of this internal table. This
*contents however should be processed by PPCGO.
    if ( lt_material_components-FLG_ASYNCH is initial or
  lt_material_components-FLG_SYNCH is initial )
  .
      lt_ppcgo-matnr = lt_material_components-mat_number.
      lt_ppcgo-budat = lt_material_components-budat.
      append lt_ppcgo.
      clear lt_ppcgo.
      lv_found_ppcgo = CHARX.
      continue.
    endif.

* Once a record is found to be processed by PPCGO, do not
* contiue with the other part of this loop.
    check lv_found_ppcgo ne charx.

* Suche einen entsprechenden Stornoeintrag
*-----------------------------------------------------------------------
    READ TABLE lt_rev_mat_components WITH KEY
      accassobj = lt_material_components-accassobj
      mat_number = lt_material_components-mat_number
      plant = lt_material_components-plant
      storage_loc = lt_material_components-storage_loc
      batch = lt_material_components-batch
      supply_area = lt_material_components-supply_area
      special_stock = lt_material_components-special_stock
      sales_doc = lt_material_components-sales_doc
      sales_doc_item = lt_material_components-sales_doc_item
      wbs_elem = lt_material_components-wbs_elem
      costing_num = lt_material_components-costing_num
      special_stock_val = lt_material_components-special_stock_val
      consumpt_posting = lt_material_components-consumpt_posting
      accass_category = lt_material_components-accass_category
    INTO ls_rev_mat_components.


* Stornoeintrag wurde gefunden
    IF sy-subrc EQ 0.
      IF lt_material_components-quantity >
         lt_rev_mat_components-quantity.
        lt_material_components-quantity =
        lt_material_components-quantity -
        lt_rev_mat_components-quantity.
      ELSE.
        ls_material_components-quantity = 0.
      ENDIF.
    ENDIF.


* Get cost collecter from ACCASSOBJ
    perform get_costcoll using lt_material_components-ACCASSOBJ
                               lt_material_components-aufnr.


* Determination of Storage location
    if not lt_material_components-gmove_ind = CF_GMOVE_WA and
               lt_material_components-storage_loc is initial.
      perform determine_version using lt_material_components-ORDERID
                                       lv_version.
     perform determine_stor_loc using lt_material_components-mat_number
                                        lt_material_components-plant
                                       lt_material_components-gmove_ind
                                        lv_version
                                     lt_material_components-storage_loc.

    endif.

* WM data
    IF lt_material_components-gmove_ind = cf_gmove_wa.
      perform fill_wm_data using lt_material_components lm_wmrel .
    endif.

    lt_info_tab-budat        = lt_material_components-budat.
    lt_info_tab-matnr        = lt_material_components-mat_number.
    lt_info_tab-ppc_qty      = lt_material_components-quantity.
    lt_info_tab-meins        = lt_material_components-UNIT_OF_MEASURE.
    lt_info_tab-aufnr        = lt_material_components-aufnr.
    lt_info_tab-werks        = lt_material_components-plant.
    lt_info_tab-lgort        = lt_material_components-storage_loc.
    lt_info_tab-prvbe        = lt_material_components-supply_area.
    lt_info_tab-charg        = lt_material_components-batch.
    lt_info_tab-mat_kdauf    = lt_material_components-sales_doc.
    lt_info_tab-mat_kdpos    = lt_material_components-sales_doc_item.
    lt_info_tab-ps_psp_pnr   = lt_material_components-wbs_elem.
    lt_info_tab-lgnum        = lt_material_components-LGNUM.
    lt_info_tab-LGTYP        = lt_material_components-LGTYP.
    lt_info_tab-lgpla        = lt_material_components-LGPLA.
    lt_info_tab-berkz        = lt_material_components-BERKZ.
    lt_info_tab-sobkz        = lt_material_components-SPECIAL_STOCK.
    lt_info_tab-kunnr        = lt_material_components-kunnr.
    lt_info_tab-lifnr        = lt_material_components-lifnr.
    lt_info_tab-kzvbr        =  lt_material_components-consumpt_posting.
    lt_info_tab-kzbws        = lt_material_components-special_stock_val.

    IF NOT lt_material_components-LGNUM IS INITIAL OR
       NOT lt_material_components-LGTYP IS INITIAL OR
       NOT lt_material_components-LGPLA IS INITIAL OR
       NOT lt_material_components-BERKZ IS INITIAL.
      MOVE CHAR_MINUS TO LT_INFO_TAB-KZECH.
    ENDIF.

    IF lt_material_components-GMOVE_IND = CF_GMOVE_WE      OR
       lt_material_components-GMOVE_IND = CF_GMOVE_WE_CO.
      MOVE: '0001' TO lt_info_tab-AUFPS.

      IF NOT lt_material_components-AUFNR IS INITIAL.
        MOVE CHARF TO lt_info_tab-KZBEW.
      ELSE.
        CLEAR lt_info_tab-KZBEW.
      ENDIF.
    ELSE.
      MOVE: CHARX TO lt_info_tab-XWAIT.
      CLEAR lt_info_tab-KZBEW.
    ENDIF.

    collect lt_info_tab.

    clear: lt_info_tab,
           lv_version.
  endloop.


*Records yet to be processed by PPCGO. These records should not be
*corrected using this report. These are not discrepancies. Just
*inform the user and stop further processing.

  if not lt_ppcgo[] is initial.
    write: / 'There are few records that are not processed by PPCGO'.
    write: / 'Process these records using PPCGO first'.
    write: /5 'Material number',  50 'Posting date'.
    loop at lt_ppcgo.
      write: /5 lt_ppcgo-matnr, 50 lt_ppcgo-budat.
    endloop.
    exit.       " do not proceed further.
  endif.

  sort mat_doc_itab by budat matnr.

* Calculate the Difference and fill the value for BWART and
* SHKZG.
  loop at lt_info_tab.

    clear: ls_mseg_qty,
           mat_doc_itab.

    loop at mat_doc_itab where budat          = lt_info_tab-budat
                         and   matnr          = lt_info_tab-matnr
                         and   aufnr          = lt_info_tab-aufnr
                         and   werks          = lt_info_tab-werks
                         and   lgort          = lt_info_tab-lgort
                         and   charg          = lt_info_tab-charg
                         and   meins          = lt_info_tab-meins
                         and   mat_kdauf      = lt_info_tab-mat_kdauf
                         and   mat_kdpos      = lt_info_tab-mat_kdpos
                         and   ps_psp_pnr     = lt_info_tab-ps_psp_pnr
                         and   lgnum          = lt_info_tab-lgnum
                         and   LGTYP          = lt_info_tab-lgtyp
                         and   lgpla          = lt_info_tab-lgpla
                         and   berkz          = lt_info_tab-berkz.

      if mat_doc_itab-shkzg = charh.
        ls_mseg_qty = ls_mseg_qty + mat_doc_itab-menge.
      elseif mat_doc_itab-shkzg = chars.
        ls_mseg_qty = ls_mseg_qty - mat_doc_itab-menge.
      endif.
    endloop.
*Some times exact matching record could not be determined from the
*material document table(all condition in the where clause may not
*have satisfied). In that case, posting of the document is done
*manually.
    if sy-subrc <> 0.
      lt_info_tab-mat_doc_notfound = charx.
      modify lt_info_tab.
      continue.
    endif.
    lt_info_tab-mseg_qty = ls_mseg_qty.
* Calculate the difference in Quantity between PPC and Mat documents.

    lt_info_tab-diff = lt_info_tab-ppc_qty - ls_mseg_qty.

* Based on the difference, decide Movement type and Debit/Credit Ind.
    if lt_info_tab-diff < 0.
      lt_info_tab-bwart = neg_mty.
      lt_info_tab-shkzg = chars.
      MODIFY lt_INFO_TAB.
    elseif lt_info_tab-diff > 0.
      lt_info_tab-bwart = pos_mty.
      lt_info_tab-shkzg = charh.
      modify lt_info_tab.
    elseif lt_info_tab-diff = 0.
* Delete these records(because for a difference of 0, nothing to post).

      delete lt_info_tab.                     " don't need to process

      delete mat_doc_itab where budat          = lt_info_tab-budat
                          and   matnr          = lt_info_tab-matnr
                          and   aufnr          = lt_info_tab-aufnr
                          and   werks          = lt_info_tab-werks
                          and   lgort          = lt_info_tab-lgort
                          and   charg          = lt_info_tab-charg
                          and   meins          = lt_info_tab-meins
                          and   mat_kdauf      = lt_info_tab-mat_kdauf
                          and   mat_kdpos      = lt_info_tab-mat_kdpos
                          and   ps_psp_pnr     = lt_info_tab-ps_psp_pnr
                          and   lgnum          = lt_info_tab-lgnum
                          and   LGTYP          = lt_info_tab-lgtyp
                          and   lgpla          = lt_info_tab-lgpla
                          and   berkz          = lt_info_tab-berkz.

    endif.
    clear: lt_info_tab,
           mat_doc_itab.
  endloop.

*Now lt_info_table will have all the necesary value. Using this
*we can decide upon the discrepencies between PPC and Mat document
*tables.
*Prepare the main Process table for Posting documents.

  sort mat_doc_itab by budat matnr.

  loop at lt_info_tab where mat_doc_notfound <> charx.


* Create one entry for every combination of material,posting date,
* cost collecter,plant,storage loc,batch,supplier area,sales doc,
* sales item.
* with the quantity to be posted. Value for the SHKZG and BWART are
* determined  based on the difference in value between the
* the quantity from PPC1_ALL and the quantity from MSEG. Other details
* are copied from mat_doc_itab & lt_info_tab.

      MOVE-corresponding lt_info_tab to mat_doc_process.
      mat_doc_process-bwart = lt_info_tab-bwart.
      if lt_info_tab-diff < 0.
        mat_doc_process-erfmg = abs( lt_info_tab-diff ).
        mat_doc_process-shkzg = lt_info_tab-shkzg.
      elseif lt_info_tab-diff > 0.
        mat_doc_process-erfmg = lt_info_tab-diff.
        mat_doc_process-shkzg = lt_info_tab-shkzg.
      ENDIF.
      append mat_doc_process.
      clear mat_doc_process.

* Delete this record from mat_doc_itab.

      delete mat_doc_itab where matnr       = lt_info_tab-matnr      and
                                budat       = lt_info_tab-budat      and
                                aufnr       = lt_info_tab-aufnr      and
                                werks       = lt_info_tab-werks      and
                                lgort       = lt_info_tab-lgort      and
                                charg       = lt_info_tab-charg      and
                                meins       = lt_info_tab-meins      and
                                mat_kdauf   = lt_info_tab-mat_kdauf  and
                                mat_kdpos   = lt_info_tab-mat_kdpos  and
                                ps_psp_pnr  = lt_info_tab-ps_psp_pnr and
                                lgnum       = lt_info_tab-lgnum      and
                                LGTYP       = lt_info_tab-lgtyp      and
                                lgpla       = lt_info_tab-lgpla      and
                                berkz       = lt_info_tab-berkz.


  endloop.


* Do not process posting, if text_flg = X.
  if test_flg <> 'X'."test_flg = X - Test flag - No Posting of documents

*
    sort mat_doc_process by budat matnr.

    loop at mat_doc_process.

      at end of budat.
        lv_chg_of_dt = charx.
      endat.

      lv_cnt = lv_cnt + 1.

*Create a set of records for processing. This will either based on
*selection parameter - no_ofdoc, entered in the selection screen or
*change in date.
      clear lt_imseg.
      move-corresponding mat_doc_process to lt_imseg.
      append lt_imseg.


* Check which condition is satisfied - number of documents or Change in
* date.

      if ( lv_cnt =  no_ofdoc or lv_chg_of_dt = charx ).

        ls_imkpf-budat = mat_doc_process-budat.
        ls_imkpf-bldat = mat_doc_process-bldat.
        ls_imkpf-usnam = sy-uname.

        PERFORM GM_CREATE TABLES  LT_IMSEG
                                  LT_EMSEG
                            USING LS_IMKPF
                                  LS_EMKPF
                                  LS_DOC.
        PERFORM GM_POST USING LS_EMKPF.
        refresh lt_imseg.
        clear: lt_imseg,
               lv_cnt,
               lv_chg_of_dt,
               ls_imkpf.
      endif.
    endloop.
  endif.

* Report output

  if not lt_info_tab[] is initial.

    Write: /10 'Date',
           22 'Material',
           50 'PPC1_ALL Count',
           75 'MSEG count',
           96 'Difference'.
    skip.
    loop at lt_info_tab where mat_doc_notfound <> charx.
      check lt_info_tab-ppc_qty <> lt_info_tab-mseg_qty.
      write : /10 lt_info_tab-budat,
               22 lt_info_tab-matnr,
               45 lt_info_tab-ppc_qty,
               70 lt_info_tab-mseg_qty,
               90 lt_info_tab-diff.
    endloop.

    skip 1.
* Display document numbers.
    describe table ls_doc lines lv_doc_lines.
    if not ls_doc[] is initial.

      write : /'Total number of Documents posted: ' , lv_doc_lines.
      write : /5 'Document numbers:'.
      write: / sy-uline(20).
      loop at ls_doc into ls_doc_line.
        write : /5 ls_doc_line-mblnr.
      endloop.

    endif.
  else.

    write :/'No Discrepancies exists for the selection criteria'.
  endif.



* Should be corrected Manually.
* Entries that are found in PPC* but not in Material document
* table(MSEG)
  loop at lt_info_tab where mat_doc_notfound = charx.
    if sy-tabix = 1.
Write : / 'Entries found in PPC tables(Backflush),',
          'but not in Material document tables:'.
      write : / sy-uline(76).
      write:  /2 'Date',
               14 'Material',
               34 'Cost coll',
               48 'Plant',
               54 'St.Loc.',
               61 'Batch',
               72 'Sales ord',
               86 'Sal.ord.itm',
               99 'Proj.',
               110 'Warehouse No.',
               125 'Sto.ty',
               133 'Sto.Bin',
               145 'Mat.stg.Ind'.


    endif.

    write: /2 lt_info_tab-budat,
            14 lt_info_tab-matnr,
            34 lt_info_tab-aufnr,
            48 lt_info_tab-werks,
            54 lt_info_tab-lgort,
            61 lt_info_tab-charg,
            72 lt_info_tab-mat_kdauf,
            86 lt_info_tab-mat_kdpos,
            99 lt_info_tab-ps_psp_pnr,
           110 lt_info_tab-lgnum,
           125 lt_info_tab-lgtyp,
           133 lt_info_tab-lgpla,
           145 lt_info_tab-berkz.

  endloop.

  skip 2.

* Entries that are found in Material document but not in PPC*
  loop at mat_doc_itab.
    if sy-tabix = 1.
          Write : / 'Entries found in Material document tables,',
          'but not in PPC tables:'.
      write : / sy-uline(65).
      write:  /2 'Date',
               14 'Material',
               34 'Cost coll',
               48 'Plant',
               54 'St.Loc.',
               61 'Batch',
               72 'Sales ord',
               86 'Sal.ord.item',
               99 'Proj',
               110 'Warehouse No.',
               125 'Sto.ty',
               133 'Sto.Bin',
               145 'Mat.stg.Ind'.

    endif.

    write: /2 mat_doc_itab-budat,
            14 mat_doc_itab-matnr,
            34 mat_doc_itab-aufnr,
            48 mat_doc_itab-werks,
            54 mat_doc_itab-lgort,
            61 mat_doc_itab-charg,
            72 mat_doc_itab-kdauf,
            86 mat_doc_itab-kdpos,
            99 mat_doc_itab-PS_PSP_PNR,
           110 mat_doc_itab-lgnum,
           125 mat_doc_itab-lgtyp,
           133 mat_doc_itab-lgpla,
           145 mat_doc_itab-berkz.

  endloop.



*&---------------------------------------------------------------------*
*&      Form  GM_CREATE
*&---------------------------------------------------------------------*
* Create Goods movements
*----------------------------------------------------------------------*
*      -->P_LT_IMSEG  text
*      -->P_LT_EMSEG  text
*      -->P_LS_IMKPF  text
*      -->P_LS_EMKPF  text
*      -->P_LS_DOC  text
*----------------------------------------------------------------------*
FORM GM_CREATE TABLES   P_LT_IMSEG type ppcpr_type_tab_imseg
                        P_LT_EMSEG type ppcpr_type_tab_emseg
               USING    P_LS_IMKPF STRUCTURE IMKPF
                        P_LS_EMKPF STRUCTURE EMKPF
                        P_LS_DOC   TYPE PPCPR_TYPE_TAB_BELNR.



  DATA: LS_MDOC TYPE PPCPR_TYPE_BELNR.


  CLEAR P_LS_EMKPF.
  CLEAR P_LT_EMSEG.
  REFRESH P_LT_EMSEG.

  check not p_lt_imseg[] is initial.

  CALL FUNCTION 'MB_CREATE_GOODS_MOVEMENT'
       EXPORTING
            IMKPF = P_LS_IMKPF
            XALLP = SPACE
            XALLR = CHARX
            CTCOD = CF_TCODE_PPCGO
       IMPORTING
            EMKPF = P_LS_EMKPF
       TABLES
            EMSEG = P_LT_EMSEG
            IMSEG = P_LT_IMSEG.


  IF NOT P_LS_EMKPF-MBLNR IS INITIAL.
    LS_MDOC-MBLNR = P_LS_EMKPF-MBLNR.
    LS_MDOC-MJAHR = P_LS_EMKPF-MJAHR.
    APPEND LS_MDOC TO P_LS_DOC.
  ENDIF.


ENDFORM.                    " GM_CREATE

*&---------------------------------------------------------------------*
*&      Form  GM_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_EMKPF  text
*----------------------------------------------------------------------*
FORM GM_POST USING    P_LS_EMKPF STRUCTURE EMKPF.

  IF NOT P_LS_EMKPF-MBLNR IS INITIAL.
    CALL FUNCTION 'MB_POST_GOODS_MOVEMENT'
         EXCEPTIONS
              OTHERS = 0.
  ENDIF.

* Commit.
  commit work.
ENDFORM.                    " GM_POST

*&---------------------------------------------------------------------*
*&      Form  DATA_PREPARE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_COMPLIST  text
*----------------------------------------------------------------------*
FORM DATA_PREPARE TABLES   lt_material_components
                           lt_rev_mat_components
                           mat_doc_itab .


  data:  begin of ls_sel_tab occurs 0.
  data:  postdate like ppc1_all-postdate.
  data:  ACCASSOBJ like ppc1_all-ACCASSOBJ.
  data:  KOMP_CHARG like ppc1_all-KOMP_CHARG.
          INCLUDE STRUCTURE  PPC_APOCOMPLIST_EXT.
  data:   end of ls_sel_tab.

  SELECT
     t1~ACCASSOBJ
     t1~postdate
     t2~matnr
     t2~werks
     t2~lgort
     t2s~charg
     t2~prvbe
     t2~gmove_ind
     t2~sobkz
     t2~kzbws
     t2~kzvbr
     t2s~kdauf
     t2s~kdpos
     t2s~pspnr
     t2s~calcnr
     t2s~lifnr
     t2s~kunnr
     SUM( t4~confquant )
     t4~confunit
     t1~kdauf
     t1~kdpos
     t1~pspnr
     t1~flg_synch
     t1~flg_asynch
     t1~orderid
  INTO
      (ls_ext_mat_comp-ACCASSOBJ,
       ls_ext_mat_comp-budat,
      ls_ext_mat_comp-mat_number,
      ls_ext_mat_comp-plant,
      ls_ext_mat_comp-storage_loc,
      ls_ext_mat_comp-batch,
      ls_ext_mat_comp-supply_area,
      ls_ext_mat_comp-gmove_ind,
      ls_ext_mat_comp-special_stock,
      ls_ext_mat_comp-special_stock_val,
      ls_ext_mat_comp-consumpt_posting,
      ls_ext_mat_comp-sales_doc,
      ls_ext_mat_comp-sales_doc_item,
      ls_ext_mat_comp-wbs_elem,
      ls_ext_mat_comp-costing_num,
      ls_ext_mat_comp-lifnr,
      ls_ext_mat_comp-kunnr,
      ls_ext_mat_comp-quantity,
      ls_ext_mat_comp-unit_of_measure,
      ls_ext_mat_comp-head_sales_doc,
      ls_ext_mat_comp-head_sales_doc_item,
      ls_ext_mat_comp-head_wbs_elem,
      ls_ext_mat_comp-flg_synch,
      ls_ext_mat_comp-flg_asynch,
      ls_ext_mat_comp-orderid)
FROM
    ( ( ppc_head AS t1 INNER JOIN ppc_conf_mat AS t4
        ON t1~headid = t4~headid )
      INNER JOIN ppc_mat_det AS t2s
        ON t4~accid = t2s~accid )
      INNER JOIN ppc_mat AS t2
        ON t2s~matid = t2~matid
WHERE
      t1~postdate in so_date AND
      t2~matnr in so_matnr AND
      t2~werks in so_werks and
      t2~lgort in so_locn and
      t1~flg_reversal EQ c_flg_no_reversal
GROUP BY
    t1~ACCASSOBJ
    t1~postdate
    t2~matnr
    t2~werks
    t2~lgort
    t2s~charg
    t2~prvbe
    t2~gmove_ind
    t2~sobkz
    t2~kzbws
    t2~kzvbr
    t2s~kdauf
    t2s~kdpos
    t2s~pspnr
    t2s~calcnr
    t2s~lifnr
    t2s~kunnr
    t4~confunit
    t1~kdauf
    t1~kdpos
    t1~pspnr
    t1~flg_synch
    t1~flg_asynch
    t1~orderid
    %_HINTS ORACLE '&SUBSTITUTE LITERALS&'
            MSSQLNT '&SUBSTITUTE LITERALS&'
            DB2     '&SUBSTITUTE LITERALS&'.


* Bestimme die Kontierung auf Komponentenebene             "AMF_CONVERT
    IF NOT ls_ext_mat_comp-special_stock IS INITIAL AND
       NOT ls_ext_mat_comp-head_sales_doc IS INITIAL AND
           ls_ext_mat_comp-sales_doc IS INITIAL.
      MOVE ls_ext_mat_comp-head_sales_doc TO
           ls_ext_mat_comp-sales_doc.
    ENDIF.
    IF NOT ls_ext_mat_comp-special_stock IS INITIAL AND
       NOT ls_ext_mat_comp-head_sales_doc_item IS INITIAL AND
           ls_ext_mat_comp-sales_doc_item IS INITIAL.
      MOVE ls_ext_mat_comp-head_sales_doc_item TO
           ls_ext_mat_comp-sales_doc_item.
    ENDIF.
    IF NOT ls_ext_mat_comp-special_stock IS INITIAL AND
       NOT ls_ext_mat_comp-head_wbs_elem IS INITIAL AND
           ls_ext_mat_comp-wbs_elem IS INITIAL.
      MOVE ls_ext_mat_comp-head_wbs_elem TO
             ls_ext_mat_comp-wbs_elem.
    ENDIF.

    APPEND ls_ext_mat_comp TO lt_material_components.
    CLEAR ls_ext_mat_comp.

  ENDSELECT.


* Selektiere die Ausschußmeldungen
  SELECT
      t1~accassobj
      t1~postdate
      t2~matnr
      t2~werks
      t2~lgort
      t2s~charg
      t2~prvbe
      t2~gmove_ind
      t2~sobkz
      t2~kzbws
      t2~kzvbr
      t2s~kdauf
      t2s~kdpos
      t2s~pspnr
      t2s~calcnr
      t2s~lifnr
      SUM( t4~confquant )
      t4~confunit
      t1~kdauf
      t1~kdpos
      t1~pspnr
      t1~flg_synch
      t1~flg_asynch
      t1~orderid
    INTO
      (ls_ext_rev_mat_comp-accassobj,
       ls_ext_rev_mat_comp-budat,
       ls_ext_rev_mat_comp-mat_number,
       ls_ext_rev_mat_comp-plant,
       ls_ext_rev_mat_comp-storage_loc,
       ls_ext_rev_mat_comp-batch,
       ls_ext_rev_mat_comp-supply_area,
       ls_ext_rev_mat_comp-gmove_ind,
       ls_ext_rev_mat_comp-special_stock,
       ls_ext_rev_mat_comp-special_stock_val,
       ls_ext_rev_mat_comp-consumpt_posting,
       ls_ext_rev_mat_comp-sales_doc,
       ls_ext_rev_mat_comp-sales_doc_item,
       ls_ext_rev_mat_comp-wbs_elem,
       ls_ext_rev_mat_comp-costing_num,
       ls_ext_rev_mat_comp-lifnr,
       ls_ext_rev_mat_comp-quantity,
       ls_ext_rev_mat_comp-unit_of_measure,
       ls_ext_rev_mat_comp-head_sales_doc,
       ls_ext_rev_mat_comp-head_sales_doc_item,
       ls_ext_rev_mat_comp-head_wbs_elem,
       ls_ext_rev_mat_comp-flg_synch,
       ls_ext_rev_mat_comp-flg_asynch,
       ls_ext_rev_mat_comp-orderid)

  FROM
      ( ( ppc_head AS t1 INNER JOIN ppc_conf_mat AS t4
          ON t1~headid = t4~headid )
        INNER JOIN ppc_mat_det AS t2s
          ON t4~accid = t2s~accid )
        INNER JOIN ppc_mat AS t2
          ON t2s~matid = t2~matid
  WHERE
        t1~postdate in so_date AND
        t2~matnr in so_matnr AND
        t2~werks in so_werks AND
        t2~lgort in so_locn AND
        t1~flg_reversal EQ c_flg_reversal
  GROUP BY
      t1~accassobj
      t1~postdate
      t2~matnr
      t2~werks
      t2~lgort
      t2s~charg
      t2~prvbe
      t2~gmove_ind
      t2~sobkz
      t2~kzbws
      t2~kzvbr
      t2s~kdauf
      t2s~kdpos
      t2s~pspnr
      t2s~calcnr
      t2s~lifnr
      t4~confunit
      t1~kdauf
      t1~kdpos
      t1~pspnr
      t1~flg_synch
      t1~flg_asynch
      t1~orderid
      %_HINTS ORACLE '&SUBSTITUTE LITERALS&'
              MSSQLNT '&SUBSTITUTE LITERALS&'
              DB2     '&SUBSTITUTE LITERALS&'.


* Bestimme die Kontierung auf Komponentenebene             "AMF_CONVERT
    IF NOT ls_ext_rev_mat_comp-special_stock IS INITIAL AND
         NOT ls_ext_rev_mat_comp-head_sales_doc IS INITIAL AND
             ls_ext_rev_mat_comp-sales_doc IS INITIAL.
      MOVE ls_ext_rev_mat_comp-head_sales_doc TO
           ls_ext_rev_mat_comp-sales_doc.
    ENDIF.
    IF NOT ls_ext_rev_mat_comp-special_stock IS INITIAL AND
         NOT ls_ext_rev_mat_comp-head_sales_doc_item IS INITIAL AND
             ls_ext_rev_mat_comp-sales_doc_item IS INITIAL.
      MOVE ls_ext_rev_mat_comp-head_sales_doc_item TO
           ls_ext_rev_mat_comp-sales_doc_item.
    ENDIF.
    IF NOT ls_ext_rev_mat_comp-special_stock IS INITIAL AND
         NOT ls_ext_rev_mat_comp-head_wbs_elem IS INITIAL AND
             ls_ext_rev_mat_comp-wbs_elem IS INITIAL.
      MOVE ls_ext_rev_mat_comp-head_wbs_elem TO
           ls_ext_rev_mat_comp-wbs_elem.
    ENDIF.

    APPEND ls_ext_rev_mat_comp TO lt_rev_mat_components.
    CLEAR ls_ext_rev_mat_comp.

  ENDSELECT.


  select
  MSEG~ANLN1
  MSEG~ANLN2
  MSEG~APLZL
  MSEG~AUFNR
  MSEG~AUFPL
  MSEG~BERKZ
  MKPF~BKTXT
  MKPF~BLDAT
  MSEG~BPMNG
  MSEG~BPRME
  MSEG~BSTME
  MSEG~BSTMG
  MKPF~BUDAT
  MSEG~BUKRS
  MSEG~BWART
  MSEG~BWTAR
  MSEG~CHARG
  MKPF~CPUDT
  MKPF~CPUTM
  MSEG~DMBTR
  MSEG~EBELN
  MSEG~EBELP
  MSEG~ERFME
  MSEG~ERFMG
  MSEG~EXBWR
  MSEG~EXVKW
  MSEG~GRUND
  MSEG~KDAUF
  MSEG~KDEIN
  MSEG~KDPOS
  MSEG~KOSTL
  MSEG~KUNNR
  MSEG~KZBEW
  MSEG~KZVBR
  MSEG~KZZUG
  MSEG~LGORT
  MSEG~LGNUM
  MSEG~LGTYP
  MSEG~LGPLA
  MSEG~LIFNR
  MSEG~MAT_KDAUF
  MSEG~MAT_KDPOS
  MSEG~MATNR
  MKPF~MBLNR
  MSEG~MEINS
  MSEG~MENGE
  MKPF~MJAHR
  MSEG~NPLNR
  MSEG~PS_PSP_PNR
  MSEG~RSNUM
  MSEG~RSPOS
  MSEG~SHKZG
  MSEG~SOBKZ
  MKPF~USNAM
  MKPF~VGART
  MSEG~VKWRT
  MSEG~WAERS
  MSEG~WERKS
  MSEG~XAUTO
  MSEG~ZEILE
  into corresponding fields of table mat_doc_itab
  from mkpf inner join mseg
  on mkpf~mblnr = mseg~mblnr and mkpf~mjahr = mseg~mjahr
  where
  MKPF~BUDAT in SO_DATE
  and
  MSEG~BWART in SO_AMTY
  and
  MSEG~LGORT in SO_LOCN
  and
  MSEG~MATNR in SO_MATNR
  and
  MSEG~WERKS in SO_WERKS
  .
  if sy-subrc ne 0.
write :/'No Material documents exists for the given selection criteria.'
    .
  endif.

ENDFORM.                    " DATA_PREPARE

*&---------------------------------------------------------------------*
*&      Form  PRINT_HEAD
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PRINT_HEAD.

  if test_flg = 'X'.
    Write : 'Mode: Test  - No posting of Material documents'.
  else.
    Write : 'Mode: Posting - Posting of Material documents'.
  endif.
  skip.
  Write :/ 'Date ' , SY-DATUM,  100 'Time', SY-UZEIT.
  skip.
  write : / sy-uline(155).

ENDFORM.                    " PRINT_HEAD

*&---------------------------------------------------------------------*
*&      Form  GET_COSTCOLL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SEL_TAB_ACCASSOBJ  text
*      -->P_COSTCOLL  text
*----------------------------------------------------------------------*
FORM GET_COSTCOLL USING    P_SEL_TAB_ACCASSOBJ
                           P_COSTCOLL type QRP002-AUFNR.
  CALL FUNCTION 'QRP_QRP002_READ'
       EXPORTING
*            IF_AUFNR   =
            IF_CC_GUID = P_SEL_TAB_ACCASSOBJ
      IMPORTING
            EF_AUFNR   = P_COSTCOLL
*            EF_CC_GUID =
      EXCEPTIONS
           NOT_FOUND  = 1
           OTHERS     = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " GET_COSTCOLL

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_VERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SEL_TAB_ORDERID  text
*      -->P_LV_VERSION  text
*----------------------------------------------------------------------*
FORM DETERMINE_VERSION USING    P_SEL_TAB_ORDERID
                                P_LV_VERSION.

  select single version into p_lv_version from ppc_ord_inf
                       where  ORDERID =  P_SEL_TAB_ORDERID.


ENDFORM.                    " DETERMINE_VERSION

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_STOR_LOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SEL_TAB_MATNR  text
*      -->P_SEL_TAB_WERKS  text
*      -->P_SEL_TAB_GMOVE_IND  text
*      -->P_LV_VERSION  text
*      -->P_SEL_TAB_LGORT  text
*----------------------------------------------------------------------*
FORM DETERMINE_STOR_LOC USING    P_SEL_TAB_MATNR
                                 P_SEL_TAB_WERKS
                                 P_SEL_TAB_GMOVE_IND
                                 P_LV_VERSION
                                 P_SEL_TAB_LGORT.


* data declaration
  DATA: LS_MAT_DATA LIKE MT61D,
        LS_MKAL LIKE MKAL.

* for assembly: get storage location from production version
  IF P_SEL_TAB_GMOVE_IND = CF_GMOVE_WE.
    CALL FUNCTION 'PPC1DC_VERSION_READ'
         EXPORTING
              IF_MATNR          = P_SEL_TAB_MATNR
              IF_WERKS          = P_SEL_TAB_WERKS
              IF_VERSION        = p_lv_VERSION
         IMPORTING
              ES_MKAL           = LS_MKAL
         EXCEPTIONS
              VERSION_NOT_FOUND = 1.
    IF SY-SUBRC = 0.
      IF NOT LS_MKAL-ALORT IS INITIAL.
        P_SEL_TAB_LGORT = LS_MKAL-ALORT.
        EXIT.
      ENDIF.
    ENDIF.
* if not found: try material master
  ENDIF.

  PERFORM MATERIAL_READ USING P_SEL_TAB_MATNR
                              P_SEL_TAB_WERKS
                              LS_MAT_DATA.
  MOVE LS_MAT_DATA-LGPRO TO P_SEL_TAB_LGORT.



ENDFORM.                    " DETERMINE_STOR_LOC

*&---------------------------------------------------------------------*
*&      Form  MATERIAL_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_SEL_TAB_MATNR  text
*      -->P_P_SEL_TAB_WERKS  text
*      -->P_LS_MAT_DATA  text
*----------------------------------------------------------------------*
FORM MATERIAL_READ USING    P_SEL_TAB_MATNR
                            P_SEL_TAB_WERKS
                            P_LS_MAT_DATA.

* Data Declaration
  DATA: LS_MTCOM LIKE MTCOM.
  DATA: LS_MAT_DATA LIKE MT61D.
  STATICS: LT_MAT_DATA LIKE MT61D OCCURS 0.
  DATA: LS_MARA LIKE MARA.
  DATA: LS_MARC LIKE MARC.

* bereits gelesen?
  READ TABLE LT_MAT_DATA INTO LS_MAT_DATA
                          WITH KEY MATNR = P_SEL_TAB_MATNR
                                   WERKS = P_SEL_TAB_WERKS.
  IF SY-SUBRC = 0.
    P_LS_MAT_DATA = LS_MAT_DATA.
  ELSE.
* gepuffertes Lesen auf Mat.stammdaten
* a) MARA
    MOVE 'MARA '     TO LS_MTCOM-KENNG.
    MOVE P_SEL_TAB_MATNR TO LS_MTCOM-MATNR.
    CALL FUNCTION 'MATERIAL_READ'
         EXPORTING
              SCHLUESSEL = LS_MTCOM  " Materialstammschlüsselfelder
         IMPORTING
              MATDATEN   = LS_MARA  " Matstammview
         EXCEPTIONS
              OTHERS     = 01.
    IF SY-SUBRC <> 0.
*      MESSAGE E001(RM) WITH P_SEL_TAB_MATNR 'MARA'
*      RAISING HEAD_ERROR.
    ENDIF.
    MOVE-CORRESPONDING LS_MARA TO LS_MAT_DATA.
* a) MARC
    CLEAR LS_MTCOM.
    MOVE 'MARC '     TO LS_MTCOM-KENNG.
    MOVE P_SEL_TAB_MATNR  TO LS_MTCOM-MATNR.
    MOVE P_SEL_TAB_WERKS  TO LS_MTCOM-WERKS.
    CALL FUNCTION 'MATERIAL_READ'
         EXPORTING
              SCHLUESSEL = LS_MTCOM  " Materialstammschlüsselfelder
         IMPORTING
              MATDATEN   = LS_MARC  " Matstammview
         EXCEPTIONS
              OTHERS     = 01.
    IF SY-SUBRC <> 0.
*      MESSAGE E001(RM) WITH P_SEL_TAB_MATNR 'MARC'
*      RAISING HEAD_ERROR.
    ENDIF.
    MOVE-CORRESPONDING LS_MARC TO LS_MAT_DATA.
    APPEND LS_MAT_DATA TO LT_MAT_DATA.
    P_LS_MAT_DATA = LS_MAT_DATA.
  ENDIF.

ENDFORM.                    " MATERIAL_READ


*&---------------------------------------------------------------------*
*&      Form  FILL_WM_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_WM_DATA using p_lt_material_components like lt_ppc_tab
                        ef_wmrel type c.


*--> Datendeklaration
* Tabelle mit WM-Daten
  STATICS: ST_WMDATA TYPE PPCPR_TYPE_TAB_WMDATA.
* Schlüsselfelder für WM-Daten
  DATA: LS_WMDATA_KEY TYPE PPCPR_TYPE_WMDATA_KEY.
* Arbeitsstruktur für WM-Daten
  DATA: LS_WMDATA TYPE PPCPR_TYPE_WMDATA.
* Schnittstellenstruktur ins WM
  DATA: LS_LRESB LIKE LRESB.
* als Tabelle:
  DATA: LT_LRESB LIKE LS_LRESB OCCURS 0.
* Fehler bei WM-Aufruf?
  DATA: LF_PROTO LIKE RLCPP-PROTO.


  CLEAR EF_WMREL.
*--> Prüfen, ob Lagerort WM-relevant ist
  PERFORM WM_RELEV_CHECK USING p_lt_material_components-storage_loc
                               p_lt_material_components-plant
                               EF_WMREL.
  CHECK EF_WMREL = CHARX.

  MOVE p_lt_material_components-mat_number TO LS_WMDATA_KEY-MATNR.
  MOVE p_lt_material_components-plant TO LS_WMDATA_KEY-WERKS.
  MOVE p_lt_material_components-storage_loc TO LS_WMDATA_KEY-LGORT.
  MOVE p_lt_material_components-supply_area TO LS_WMDATA_KEY-PRVBE.
  MOVE p_lt_material_components-DEBIT_CREDIT_IND TO LS_WMDATA_KEY-SHKZG.
  MOVE p_lt_material_components-SPECIAL_STOCK    TO LS_WMDATA_KEY-SOBKZ.
  MOVE p_lt_material_components-UNIT_OF_MEASURE TO LS_WMDATA_KEY-LAGME.
  READ TABLE ST_WMDATA WITH KEY LS_WMDATA_KEY INTO LS_WMDATA.
*--> WM-Daten bereits gelesen
  IF SY-SUBRC = 0.
    p_lt_material_components-LGNUM = LS_WMDATA-LGNUM.
    p_lt_material_components-LGTYP = LS_WMDATA-LGTYP.
    p_lt_material_components-LGPLA = LS_WMDATA-LGPLA.
    p_lt_material_components-BERKZ = LS_WMDATA-BERKZ.
  ELSE.
*--> WM-Daten neu ermitteln
    LS_LRESB-MATNR = p_lt_material_components-MAT_number.
    LS_LRESB-WERKS = p_lt_material_components-plant.
    LS_LRESB-LGORT = p_lt_material_components-storage_loc.
    LS_LRESB-CHARG = p_lt_material_components-batch.
    LS_LRESB-SOBKZ = p_lt_material_components-SPECIAL_STOCK.
    LS_LRESB-BDMNG = p_lt_material_components-QUANTITY.
    LS_LRESB-MEINS = p_lt_material_components-UNIT_OF_MEASURE.
    LS_LRESB-ERFMG = p_lt_material_components-QUANTITY.
    LS_LRESB-ERFME = p_lt_material_components-UNIT_OF_MEASURE.
    LS_LRESB-AUFNR = p_lt_material_components-AUFNR.
    LS_LRESB-PRVBE = p_lt_material_components-supply_area.
    LS_LRESB-UMREN = 1.
    LS_LRESB-UMREZ = 1.
    LS_LRESB-LIFNR = p_lt_material_components-LIFNR.
    LS_LRESB-BWART = p_lt_material_components-MOVEMENT_TYPE.



* falls Kundeneinzelbestand: fülle  kdauf, kdpos
    IF p_lt_material_components-SPECIAL_STOCK EQ CHARE.
      LS_LRESB-KDAUF = p_lt_material_components-SALES_DOC.
      LS_LRESB-KDPOS = p_lt_material_components-SALES_DOC_ITEM.
    ENDIF.
* falls Projektbestand: fülle pspel
    IF p_lt_material_components-SPECIAL_STOCK EQ CHARQ.
      LS_LRESB-PSPEL = p_lt_material_components-WBS_ELEM.
    ENDIF.
    "    MOVE AM61B-ARBID     TO LS_LRESB-OBJID.
    LS_LRESB-PRVBE = p_lt_material_components-supply_area.
    LS_LRESB-XWAOK = CHARX.            " Hart gesetzt
    LS_LRESB-SELKZ = CHARX.            " Hart gesetzt FAUF - Logik
    APPEND LS_LRESB TO LT_LRESB.
    " WM Init
    CALL FUNCTION 'L_WMPP_INTERFACE_FROM_PP'
         EXPORTING
              PREPA = ' '
              VIEW_ = ' '
              CHNGE = ' '
              SIMUL = ' '
              FCALL = ' '
              SCALL = CHARX
              INITT = CHARX
         IMPORTING
              PROTO = LF_PROTO
         TABLES
              LRES  = LT_LRESB.
* Daten übernehmen
    READ TABLE LT_LRESB INDEX 1 INTO LS_LRESB.
* in die Statics-Tabelle
    MOVE-CORRESPONDING LS_WMDATA_KEY TO LS_WMDATA.
    LS_WMDATA-LGNUM = LS_LRESB-LGNUM.
    LS_WMDATA-LGTYP = LS_LRESB-LGTYP.
    LS_WMDATA-LGPLA = LS_LRESB-LGPLA.
    LS_WMDATA-BERKZ = LS_LRESB-BERKZ.
    APPEND LS_WMDATA TO ST_WMDATA.
* in die Komponenten-Tabelle
    p_lt_material_components-LGNUM = LS_LRESB-LGNUM.
    p_lt_material_components-LGTYP = LS_LRESB-LGTYP.
    p_lt_material_components-LGPLA = LS_LRESB-LGPLA.
    p_lt_material_components-BERKZ = LS_LRESB-BERKZ.
  ENDIF.


ENDFORM.                    " FILL_WM_DATA

*&---------------------------------------------------------------------*
*&      Form  WM_RELEV_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LT_MATERIAL_COMPONENTS_STORA  text
*      -->P_P_LT_MATERIAL_COMPONENTS_PLANT  text
*      -->P_EF_WMREL  text
*----------------------------------------------------------------------*
FORM WM_RELEV_CHECK USING    IF_LGORT
                             IF_WERKS
                             P_EF_WMREL.
*--> Datendeklaration
  STATICS: ST_WM_RELEV TYPE PPCPR_TYPE_TAB_WMREL.
  DATA: LS_WM_RELEV TYPE PPCPR_TYPE_WMREL.

*--> Initialisierung
  CLEAR P_EF_WMREL.

  CHECK NOT IF_WERKS IS INITIAL.
  CHECK NOT IF_LGORT IS INITIAL.

  READ TABLE ST_WM_RELEV INTO LS_WM_RELEV
                         WITH KEY WERKS = IF_WERKS
                                  LGORT = IF_LGORT.
  IF SY-SUBRC <> 0.
* Prüfung für Werk, Lagerort noch nicht erfolgt
    CALL FUNCTION 'L_WMPP_RELEVANCE_CHECK'
         EXPORTING
              WERKS       = IF_WERKS
              LGORT       = IF_LGORT
         IMPORTING
              WM_RELEVANT = P_EF_WMREL.
    LS_WM_RELEV-WERKS = IF_WERKS.
    LS_WM_RELEV-LGORT = IF_LGORT.
    LS_WM_RELEV-WMREL = P_EF_WMREL.
    APPEND LS_WM_RELEV TO ST_WM_RELEV.
  ELSE.
    P_EF_WMREL = LS_WM_RELEV-WMREL.
  ENDIF.



ENDFORM.                    " WM_RELEV_CHECK
