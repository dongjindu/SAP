************************************************************************
* Program Name      : ZACO16U_ABIS_NEW
* Author            : Jung hye sun
* Creation Date     : 04/11/2006
* Specifications By : Andy Choi
* Pattern           : Report 1-1
* Development Request No: UD1K903655
* Add documentation :
* Description       : Allocate the costs in Internal order- P001, E001
*                     to PCC by the rate of PCC cost -> Changed
* the BDC structures for BATCH INPUT processing
************************************************************************
REPORT zaco16u_abis_new MESSAGE-ID zmco.

* For TOP include
INCLUDE zaco16lnew_1top.
*INCLUDE zaco16l_1top.
* For Sub-Routine
INCLUDE zaco16lnew_f001.
*INCLUDE zaco16l_f001.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
**// Mod. By Hyung Jin Youn 2004.08.05
* Do not use Default Values
* KSTAR, I/O, Material Type
*  PERFORM SELECT_INIT.
**// End of Mod.
* Set Material Type for Semi-Finished product and FSC
  PERFORM set_mtype_fk.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

* information
  PERFORM confirm_message.

AT SELECTION-SCREEN OUTPUT.
* Check Post+Report/Report
  PERFORM ind_post_n_report.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  message e000 with 'Program no more available'.

  CONCATENATE  p_gjahr p_perio+1(2) '01' INTO g_first_date.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = g_first_date
       IMPORTING
            last_day_of_month = g_last_date.

* Controlling Area Information
  PERFORM read_tka01.
* Enqueue
  PERFORM enqueue.

  IF p_post = space.
* Confirmation
    PERFORM conf_replace_data.
* KSTAR - ALPHA_NUMERIC
    PERFORM alpha_kstar.
* Read I/O data from CO document
*    PERFORM read_io_data.

* Read storage location data from MARD/MARDH
* X905, X551
    PERFORM read_storage_location_data.

** Read standard price CKMLMV011/CKMLCR
*    PERFORM read_standard_price.


* Read Costs from B/F
    PERFORM read_b_f_data.

*// Mod. by Hyung Jin Youn 2004.04.19
* Cal. ratio by Qty. Base not By Cost Base
* Cal. Total material cost and the ratio for parents materials,
*      The cost ratio of child materials
* PERFORM CAL_COST_RATIO.
    PERFORM cal_cost_ratio_02.
    PERFORM cal_qty_ratio_02.
*// End of Mod.

* Making itab to be posted
    PERFORM make_pcc_post_table.
* Adjustment
    PERFORM adjustment_data.

* Confirmation
    PERFORM conf_replace_data.

* Update result to CBO table
    PERFORM update_data_to_table .
  ENDIF.

* Posting
  PERFORM post_data.
* Let system Dequeue


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  read_storage_location_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_storage_location_data.

  PERFORM select_mard_mardh.

* Read standard price CKMLMV011/CKMLCR
  PERFORM read_standard_price.

  PERFORM modify_it_mard.

ENDFORM.                    " read_storage_location_data
*&---------------------------------------------------------------------*
*&      Form  select_mard_mardh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_mard_mardh.
  DATA : l_idx LIKE sy-tabix.
  DATA : l_mardh LIKE mard.
  SELECT  *
    INTO CORRESPONDING FIELDS OF TABLE it_mard
    FROM mard
*   WHERE ( lgort = 'X551' OR  lgort = 'X905' )
    WHERE ( lgort = 'P400' OR  lgort = '9999' )
     AND matnr  IN s_matnr.

  LOOP AT it_mard.
    IF it_mard-lgort = 'P400' .
      it_mard-lgort = 'X551'.
    ELSE.
      it_mard-lgort = 'X905'.
    ENDIF.
    MODIFY it_mard. CLEAR it_mard.
  ENDLOOP.
  LOOP AT it_mard.
    l_idx = sy-tabix.
*----- get from current value
    IF it_mard-lfgja < p_gjahr
    OR ( it_mard-lfgja = p_gjahr AND it_mard-lfmon <= p_perio ).
*----- get from history
    ELSE.
      CLEAR l_mardh.
      SELECT * INTO l_mardh
       FROM mardh
          WHERE matnr = it_mard-matnr
            AND werks = it_mard-werks
            AND lgort = it_mard-lgort
           AND lfgja  = p_gjahr
           AND lfmon <= p_perio
       ORDER BY lfgja DESCENDING
                lfmon DESCENDING.
        EXIT.
      ENDSELECT.
      IF sy-subrc <> 0.
        SELECT * INTO l_mardh
         FROM mardh
            WHERE matnr = it_mard-matnr
            AND werks = it_mard-werks
            AND lgort = it_mard-lgort
             AND lfgja < p_gjahr
         ORDER BY lfgja DESCENDING
                  lfmon DESCENDING.
          EXIT.
        ENDSELECT.
      ENDIF.
      IF sy-subrc = 0 .
        it_mard-werks = l_mardh-werks.
        it_mard-labst = l_mardh-labst.
        MODIFY it_mard INDEX l_idx.
      ELSE.
*       DELETE it_mard INDEX l_idx.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.                    " select_mard_mardh
*&---------------------------------------------------------------------*
*&      Form  modify_it_mard
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_it_mard.

* Get for Cost element
  PERFORM select_costelement.
  SORT it_mbew  BY matnr bwkey.
  SORT it_t030  BY bklas.

  LOOP AT it_mard.
    CASE it_mard-lgort.
      WHEN 'X551'.
        it_mard-stype = 'OS&D' .
      WHEN 'X905'.
        it_mard-stype = 'KEYIN'  .
    ENDCASE.

*Production scheduler
    CLEAR it_mard-fevor.
    SELECT SINGLE fevor INTO it_mard-fevor
     FROM marc
    WHERE matnr = it_mard-matnr
      AND werks = it_mard-werks.

*Unit
    CLEAR it_mard-meins.
    SELECT SINGLE meins INTO it_mard-meins
     FROM mara
    WHERE matnr = it_mard-matnr.

*Cost element
    PERFORM get_costelement CHANGING it_mard-kstar.

*Std prcie
    CLEAR it_ckmlmv011.
    READ TABLE it_ckmlmv011 WITH KEY matnr = it_mard-matnr
                                     werks = it_mard-werks
                                 BINARY SEARCH.
    CLEAR it_ckmlcr.
    READ TABLE it_ckmlcr WITH KEY kalnr = it_ckmlmv011-kalnr
                              BINARY SEARCH.
    it_mard-stprs = it_ckmlcr-stprs.
    MODIFY it_mard. CLEAR it_mard.
  ENDLOOP.

ENDFORM.                    " modify_it_mard
*&---------------------------------------------------------------------*
*&      Form  read_standard_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_standard_price.

  CHECK NOT it_mard[] IS INITIAL.
*  LOOP AT it_mard.
*    CLEAR it_mard-kalnr.
*    SELECT single kalnr INTO it_mard-kalnr
*      FROM ckmlmv011
*     WHERE matnr = it_mard-matnr
*       AND bwkey = it_mard-werks.
*      MODIFY it_mard. CLEAR it_mard.
*    ENDLOOP.
*
*    SELECT  *
*      INTO CORRESPONDING FIELDS OF TABLE it_ckmlcr
*      FROM ckmlcr
*       FOR ALL ENTRIES IN it_mard
*     WHERE kalnr = it_mard-kalnr
*       AND bdatj = p_gjahr
*       AND poper = p_perio
*       AND untper = space
*       AND curtp  = '10'
*       AND vprsv  = 'S'.

  SELECT kalnr matnr  werks
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlmv011_temp
    FROM ckmlmv011
     FOR ALL ENTRIES IN it_mard
   WHERE matnr = it_mard-matnr
     AND bwkey = it_mard-werks.


  LOOP AT it_ckmlmv011_temp.
    MOVE-CORRESPONDING it_ckmlmv011_temp TO it_ckmlmv011.
    COLLECT it_ckmlmv011. CLEAR it_ckmlmv011.
  ENDLOOP.

  CHECK NOT it_ckmlmv011[] IS INITIAL.
  SELECT  *
    INTO CORRESPONDING FIELDS OF TABLE it_ckmlcr
    FROM ckmlcr
     FOR ALL ENTRIES IN it_ckmlmv011
   WHERE kalnr = it_ckmlmv011-kalnr
     AND bdatj = p_gjahr
     AND poper = p_perio
     AND untper = space
     AND curtp  = '10'
     AND vprsv  = 'S'.

  SORT it_ckmlmv011 BY matnr werks.
  SORT it_ckmlcr BY kalnr.


ENDFORM.                    " read_standard_price
*&---------------------------------------------------------------------*
*&      Form  select_costelement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_costelement.
  DATA : l_kstar TYPE kstar,
         l_ktopl LIKE t001-ktopl.

  CHECK NOT it_mard[] IS INITIAL.
  SELECT matnr bwkey bklas
    INTO CORRESPONDING FIELDS OF TABLE it_mbew
    FROM mbew
     FOR ALL ENTRIES IN it_mard
   WHERE matnr = it_mard-matnr
     AND bwkey = it_mard-werks.

  CHECK NOT it_mbew[] IS INITIAL.
  CLEAR l_ktopl.
  SELECT SINGLE ktopl INTO l_ktopl FROM t001
    WHERE bukrs = p_kokrs.

  SELECT bklas ktopl konts INTO CORRESPONDING FIELDS OF TABLE it_t030
    FROM t030
    FOR ALL ENTRIES IN it_mbew
  WHERE ktosl = 'GBB'
    AND komok = 'VBR'
    AND bklas = it_mbew-bklas
    AND ktopl = l_ktopl.



ENDFORM.                    " select_costelement
*&---------------------------------------------------------------------*
*&      Form  move_mard_to_it_post
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_mard_to_it_post.
  DATA : lv_sum_qty LIKE it_io_coep-mbgbtr.
  DATA : lv_res_qty LIKE it_io_coep-mbgbtr.
  DATA : lw_fevor   LIKE marc-fevor.


  MOVE:" it_mard-waers  TO it_post-waers,
        it_mard-kstar  TO it_post-kstar,
        it_mard-matnr  TO it_post-matnr,
        it_mard-werks  TO it_post-werks,
        it_mard-meins  TO it_post-meinb,
        it_mard-stype  TO it_post-stype,
        'A'            TO it_post-type.

*  read PCC order
  CLEAR it_pcc_mat.
  READ TABLE it_pcc_mat WITH KEY objnr  = it_tot_rate-objnr.
  it_post-pcc_aufnr = it_pcc_mat-aufnr.
*  parentes rate
  it_post-rate_child = it_tot_rate-kstar_rate.

  IF it_mard-meins EQ 'EA'.
    cal_by_qty rate_child.
  ELSE.
    it_post-mbgbtr     = it_mard-labst * it_post-rate_child.
    it_post-chg_wkgbtr = it_mard-labst * it_mard-stprs *
                         it_post-rate_child.
  ENDIF.
  APPEND it_post.
  CLEAR  it_post.

ENDFORM.                    " move_mard_to_it_post
*&---------------------------------------------------------------------*
*&      Form  get_costelement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_costelement CHANGING p_kstar.
  DATA : l_kstar TYPE kstar.
  DATA : l_bklas TYPE bklas.

  SELECT SINGLE bklas INTO l_bklas
    FROM mbew
   WHERE matnr = it_mard-matnr
     AND bwkey = it_mard-werks.

  CLEAR it_mbew.
  READ TABLE it_mbew WITH KEY matnr = it_mard-matnr
                              bwkey = it_mard-werks
                          BINARY SEARCH.

  CLEAR it_t030.
  READ TABLE it_t030 WITH KEY bklas = it_mbew-bklas
                          BINARY SEARCH.

  p_kstar = it_t030-konts.

ENDFORM.                    " get_costelement
*&---------------------------------------------------------------------*
*&      Form  POST_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_sap.
  DATA : l_date TYPE datum.
  PERFORM get_define_posting_type.

  PERFORM posting_mb1a_1.
*  PERFORM posting_mb1a.
  PERFORM posting_mfbf.
  PERFORM posting_ppcvar.

ENDFORM.                    " POST_SAP
*&---------------------------------------------------------------------*
*&      Form  GET_DEFINE_POSTING_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_define_posting_type.

  DATA : BEGIN OF it_marc OCCURS 0,
           matnr LIKE marc-matnr,
           werks LIKE marc-werks,
           sauft LIKE marc-sauft,
           sfepr LIKE marc-sfepr,
           lgpro LIKE marc-lgpro,
         END OF it_marc.


  CHECK NOT it_post_fin[] IS INITIAL.
  SELECT matnr werks sauft sfepr
    INTO CORRESPONDING FIELDS OF TABLE it_marc
    FROM marc
     FOR ALL ENTRIES IN it_post_fin
   WHERE matnr = it_post_fin-matnr.

  LOOP AT it_post_fin.
    CLEAR it_marc .
    READ TABLE it_marc WITH KEY werks = it_post_fin-werks
                                matnr = it_post_fin-matnr.
    it_post_fin-lgort = it_marc-lgpro.
    IF it_post_fin-lgort IS INITIAL.
      it_post_fin-lgort = 'P400'.
    ENDIF.
* Repititive manufacturing
* Posting MB1A :BAPI_PRODORDCONF_CREATE_TT
    IF it_marc-sauft = ' '.
      PERFORM get_bwart CHANGING it_mb1a-bwart
                                 it_mb1a-reas.

      MOVE-CORRESPONDING it_post_fin TO it_mb1a.
      COLLECT it_mb1a.
      CLEAR it_mb1a.
* Posting MFBF :BAPI_REPMANCONF1_CREATE_MTS
    ELSEIF it_marc-sauft = 'X' AND it_marc-sfepr <> 'VEHI'.
      MOVE-CORRESPONDING it_post_fin TO it_mfbf.
      COLLECT it_mfbf.
      CLEAR it_mfbf.
* Posting PPCVAR :
    ELSEIF it_marc-sauft = 'X' AND it_marc-sfepr = 'VEHI'.
      MOVE-CORRESPONDING it_post_fin TO it_ppcvar.
      if it_ppcvar-mbgbtr <  0 .
        it_ppcvar-flg_reversal = 'X'.
      endif.
      COLLECT it_ppcvar.
      CLEAR it_ppcvar.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " GET_DEFINE_POSTING_TYPE
*&---------------------------------------------------------------------*
*&      Form  POSTING_MB1A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_mb1a.
* The number of Item Records should not be greater than 200
  DATA : it_post_idx LIKE sy-tabix.
  DATA : lv_mod      LIKE sy-tabix.
  DATA : lv_div      LIKE sy-tabix.
  DATA : lv_from_idx LIKE sy-tabix,
         lv_to_idx   LIKE sy-tabix,
         lv_skip.


  CHECK NOT it_mb1a[] IS INITIAL.
* Make one document per 200 records
  SORT it_mb1a .

* Read the number of Index of "IT_POST"
  DESCRIBE TABLE it_mb1a LINES it_post_idx.
  lv_div = it_post_idx  DIV  200.
  lv_mod = it_post_idx  MOD  200.
  IF lv_mod > 0.
    lv_div = lv_div + 1.
  ENDIF.

  DO lv_div TIMES.
* Check Index of IT_POST
* Cal. MOD. DIV.
    lv_to_idx   =  sy-index * 200 .
    lv_from_idx =  lv_to_idx - 199.
* From
    CLEAR lv_skip.
    CLEAR it_post_fin. READ TABLE it_post_fin INDEX lv_from_idx.
    IF sy-subrc <> 0. lv_skip = 'X'. ENDIF.
* TO
    CLEAR it_post_fin. READ TABLE it_post_fin INDEX lv_to_idx.
    IF sy-subrc <> 0. lv_to_idx = lv_from_idx + lv_mod - 1 . ENDIF.

    IF lv_skip <> 'X'.
* Run Post FM
      PERFORM call_post_mb1a USING  lv_from_idx lv_to_idx .
    ENDIF.
  ENDDO.

ENDFORM.                    " POSTING_MB1A
*&---------------------------------------------------------------------*
*&      Form  POSTING_MFBF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_mfbf.
  CHECK NOT it_mfbf[] IS INITIAL.
  LOOP AT it_mfbf.
    PERFORM append_bapi_structure_mfbf.
    PERFORM gi_posting_mfbf .
  ENDLOOP.

ENDFORM.                    " POSTING_MFBF
*&---------------------------------------------------------------------*
*&      Form  POSTING_PPCVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_ppcvar.

  CHECK NOT it_ppcvar[] IS INITIAL.
  PERFORM read_resource_data.

  SORT it_ppcvar BY pcc_aufnr werks lgort.

  LOOP AT it_ppcvar.
    ON CHANGE OF it_ppcvar-flg_reversal
              OR it_ppcvar-pcc_aufnr
              OR it_ppcvar-werks
              OR it_ppcvar-lgort.
* Creation of PPC Header
      PERFORM create_ppc_header.
    ENDON.
* Creation of PPC Item  - with WA_PPC_HEAD-HEADID
    CLEAR it_apoactlists.
    it_apoactlists-headid = wa_ppc_head-headid.

*    clear it_ppc_act_mod .
*    read table  it_ppc_act_mod with key cost_center = it_di_post-kostl
*                                        acttype     = it_di_post-lstar.
*    if sy-subrc <> 0.
*      continue.
*    endif.
*
** Resource Info
*    it_apoactlists-resource_guid  = it_ppc_act_mod-resource_guid.
*    it_apoactlists-mode_guid      = it_ppc_act_mod-mode_guid.
* Value
    it_apoactlists-duration_var = abs( it_ppcvar-mbgbtr ).
* DURATION_VAR / Delta
    it_apoactlists-delta_duration_var = it_apoactlists-duration_var.
* DELTA_DURATION_FIX
    it_apoactlists-durunit = it_ppcvar-meinb.

    APPEND it_apoactlists.
    CLEAR  it_apoactlists.
    CLEAR it_ppcvar.
  ENDLOOP.


  PERFORM gi_posting_ppcvar .

*    PERFORM append_bapi_structure_PPCVAR.
ENDFORM.                    " POSTING_PPCVAR
*&---------------------------------------------------------------------*
*&      Form  append_bapi_structure_mb1a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_bapi_structure_mb1a.
  DATA:  l_werks_1(1).
  DATA : l_bwart LIKE mseg-bwart.


* Detail.
  MOVE : it_mb1a-matnr  TO it_goodsmvt_item-material,
         it_mb1a-werks  TO it_goodsmvt_item-plant,
         it_mb1a-pcc_aufnr TO it_goodsmvt_item-orderid,
         it_mb1a-lgort  TO it_goodsmvt_item-stge_loc,
         it_mb1a-bwart  TO it_goodsmvt_item-move_type,
         it_mb1a-mbgbtr TO it_goodsmvt_item-entry_qnt,
         it_mb1a-reas   TO it_goodsmvt_item-move_reas.
  CONCATENATE it_mb1a-matnr ';' it_mb1a-stype
           INTO it_goodsmvt_item-item_text.


  APPEND it_goodsmvt_item. CLEAR it_goodsmvt_item.

ENDFORM.                    " append_bapi_structure_mb1a
*&---------------------------------------------------------------------*
*&      Form  gi_posting_mb1a
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gi_posting_mb1a.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
       EXPORTING
            goodsmvt_header  = w_goodsmvt_header
            goodsmvt_code    = w_goodsmvt_code
       IMPORTING
            goodsmvt_headret = w_goodsmvt_headret
            materialdocument = w_materialdocument
            matdocumentyear  = w_matdocumentyear
       TABLES
            goodsmvt_item    = it_goodsmvt_item
            return           = it_return.

  READ TABLE it_return WITH KEY type = 'E'.
  BREAK-POINT.
  IF sy-subrc EQ 0.
    ROLLBACK WORK.
*    concatenate it_return-message 'hlw' into it_return-message.
*    MOVE : c_red             TO it_write_temp-linecolor,
*           it_return-message TO it_write_temp-messa,
*           'E' TO it_write_temp-gi_status,
*           ' ' TO it_write_temp-processed.
*    MODIFY it_write_temp TRANSPORTING linecolor messa
*                                      gi_status
*                                WHERE matnr NE space.
*    MODIFY it_itab FROM it_write_temp
*                          TRANSPORTING gi_status processed
*                          WHERE processed = 'Y'.

  ELSE.
    COMMIT WORK AND WAIT.
*    MOVE : c_green            TO it_write_temp-linecolor,
*           it_return-message  TO it_write_temp-messa,
*           w_materialdocument TO it_write_temp-mblnr,
**           w_matdocumentyear  TO it_write_temp-mjahr,
*           'S' TO it_write_temp-gi_status,
*           ' ' TO it_write_temp-processed.
*    MODIFY it_write_temp TRANSPORTING mblnr linecolor messa
*                                      gi_status
*                                      where matnr ne space.
*    MODIFY it_itab FROM it_write_temp
*                         TRANSPORTING gi_status processed
*                                      mblnr
*                         WHERE processed = 'Y'.
  ENDIF.

  CLEAR : it_goodsmvt_item[].

ENDFORM.                    " gi_posting_mb1a
*&---------------------------------------------------------------------*
*&      Form  append_bapi_structure_mfbf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_bapi_structure_mfbf.
*& Ref. BFLUSHFLAG-BACKFLTYPE = '01'
*&                              '02' Z : Reporting Point Backflush
*&                                     : Goods issue for reporting point
*&                              '10' A : Only activity posting
*&                              '11' U : Unplanned consumption message
*&                              '12' V : Scrap Message
*&                              '20'
*------> YIELD QTY
  CLEAR : bflushdatagen,
          bflushflags.
  bflushflags-bckfltype       = '01'         .    "Backflushing type

  bflushdatagen-materialnr    = it_mfbf-matnr.    "Material number
  bflushdatagen-prodplant     = it_mfbf-werks .   "Plant
  bflushdatagen-prodversion   = '01'          .   "Production version

  bflushdatagen-postdate      = g_last_date .     "Posting date
  bflushdatagen-docdate       = sy-datum      .   "Document date
  bflushdatagen-unitofmeasure = it_mfbf-meinb .   "UoM

  CONCATENATE text-010 sy-repid sy-uname sy-datum sy-uzeit
           INTO bflushdatagen-docheadertxt.        "Doc header text


  IF it_mfbf-mbgbtr > 0.
    bflushdatagen-backflquant   = it_mfbf-mbgbtr.  "Yield Qty
  ENDIF.


ENDFORM.                    " append_bapi_structure_mfbf
*&---------------------------------------------------------------------*
*&      Form  gi_posting_mfbf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gi_posting_mfbf.

*-----> BAPI FOR BACKFLUSH
  CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'
       EXPORTING
            bflushflags   = bflushflags
            bflushdatagen = bflushdatagen
            bflushdatamts = bflushdatamts
       IMPORTING
            confirmation  = wa_confirmation
            return        = return
       TABLES
            serialnr      = it_serialnr.

  PERFORM call_return_message .   "  USING L_CONFIRMATION .


ENDFORM.                    " gi_posting_mfbf
*&---------------------------------------------------------------------*
*&      Form  CALL_RETURN_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_return_message.
  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
       EXPORTING
            type   = return-type
            cl     = return-id
            number = return-number
            par1   = return-message_v1
            par2   = return-message_v2
            par3   = return-message_v3
            par4   = return-message_v4
       IMPORTING
            return = return.

*  IF NOT PA_CONFIRM IS INITIAL.
  IF NOT wa_confirmation IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK' .
  ENDIF.
ENDFORM.                    " call_return_message
*&---------------------------------------------------------------------*
*&      Form  append_bapi_structure_PPCVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_bapi_structure_ppcvar.
*  clear : it_apoheads,     it_apoheads[].
*  clear : it_apocomplists, it_apocomplists[].
*  clear : it_apoactlists,  it_apoactlists[].
*
*  clear it_ppc_act_mod.
*
*  sort it_post_fin.
*
** Building Posting Tabs
*  loop at it_post_fin where wrong_ppc ne 'X'.
*    on change of it_post_fin-gjahr
*              or it_post_fin-perid
*              or it_post_fin-matnr
*              or it_post_fin-werks .
*
** Creation of PPC Header
*      perform create_ppc_header.
*    endon.
*
** Creation of PPC Item  - with WA_PPC_HEAD-HEADID
*    clear it_apoactlists.
*    it_apoactlists-headid = wa_ppc_head-headid.
*
*    clear it_ppc_act_mod .
*    read table  it_ppc_act_mod with key cost_center = it_post_fin-kostl
*                                        acttype     = it_post_fin-lstar
*  .
*    if sy-subrc <> 0.
*      continue.
*    endif.
*
** Resource Info
*    it_apoactlists-resource_guid  = it_ppc_act_mod-resource_guid.
*    it_apoactlists-mode_guid      = it_ppc_act_mod-mode_guid.
** Value
*    it_apoactlists-duration_var = abs( it_post_fin-varquan ).
** DURATION_VAR / Delta
*    it_apoactlists-delta_duration_var = it_apoactlists-duration_var.
** DELTA_DURATION_FIX
*    it_apoactlists-durunit = it_post_fin-meinh.
*
*    append it_apoactlists.
*    clear  it_apoactlists.
*    clear it_post_fin.
*  endloop.


ENDFORM.                    " append_bapi_structure_PPCVAR
*&---------------------------------------------------------------------*
*&      Form  gi_posting_PPCVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gi_posting_ppcvar.
* Call posting FM
  CLEAR : it_return, it_return[].
  CALL FUNCTION 'BAPI_MNFCTCONFRCVR_RECEIVE'
    IMPORTING
      return                = it_return
    TABLES
      it_apoheads           = it_apoheads
*     IT_APOCOMPLISTS       =
      it_apoactlists        = it_apoactlists.


ENDFORM.                    " gi_posting_PPCVAR
*&---------------------------------------------------------------------*
*&      Form  create_ppc_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_ppc_header.

* Clear Header Information
  CLEAR   wa_ppc_head.
  CLEAR : it_ppc_heads, it_ppc_heads[].

* Header ID
  CALL FUNCTION 'GUID_CREATE'
       IMPORTING
            ev_guid_32 = wa_ppc_head-headid.

* ... fill additional information
  GET TIME STAMP FIELD wa_ppc_head-conf_time.
  MOVE g_last_date  TO wa_ppc_head-pstng_date.
  MOVE sy-uname     TO wa_ppc_head-conf_username.

** Posting Ind. (Reversal Ind.)
  wa_ppc_head-flg_reversal = it_ppcvar-flg_reversal.

  MOVE '3' TO wa_ppc_head-flg_info_dest.    "separate variances posting
  APPEND wa_ppc_head TO it_ppc_heads.

* Header Tab.
  LOOP AT it_ppc_heads .
    MOVE-CORRESPONDING it_ppc_heads TO it_apoheads .
* MAT Infor
    it_apoheads-head_matnr = it_ppcvar-matnr.
    it_apoheads-prodplant  = it_ppcvar-werks.
*    it_apoheads-version    = it_ppcvar-verid.

    APPEND it_apoheads .
    CLEAR  it_apoheads .
    CLEAR  it_ppc_heads.
  ENDLOOP.


ENDFORM.                    " create_ppc_header
*&---------------------------------------------------------------------*
*&      Form  call_post_MB1A
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_FROM_IDX  text
*      -->P_LV_TO_IDX  text
*----------------------------------------------------------------------*
FORM call_post_mb1a USING  p_lv_fr
                           p_lv_to.
*  DATA:  l_werks_1(1).
*  DATA : l_bwart LIKE mseg-bwart.
*
** Header.
*  CLEAR : w_goodsmvt_header, w_goodsmvt_code, w_goodsmvt_headret,
*          w_materialdocument, w_matdocumentyear,
*          it_return, it_return[].
*
*
*  w_goodsmvt_header-pstng_date = g_last_date.
*  w_goodsmvt_header-doc_date   = sy-datum.
**  w_goodsmvt_code-gm_code = c_gm_code
*  CONCATENATE text-010 sy-repid
*            sy-uname sy-datum sy-uzeit
*       INTO wa_l_doc_header-doc_hdr_tx
*
*  LOOP AT it_mb1a  FROM p_lv_fr TO  p_lv_to.
*    PERFORM append_bapi_structure_mb1a.
*    PERFORM gi_posting_mb1a .
*  ENDLOOP.

ENDFORM.                    " call_post_MB1A
*&---------------------------------------------------------------------*
*&      Form  posting_mb1a_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_mb1a_1.
  SORT it_mb1a BY werks lgort bwart.
  LOOP AT it_mb1a.
    PERFORM append_bapi_structure_mb1a.
    AT END OF bwart.
      PERFORM append_bapi_mb1a_header.
      PERFORM gi_posting_mb1a .
    ENDAT.
  ENDLOOP.


ENDFORM.                    " posting_mb1a_1
*&---------------------------------------------------------------------*
*&      Form  GET_PRODUCT_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_product_order USING p_aufnr.
* Get production order 3 steps
* 1) Start   date in input month and
*    Finish  date in input month .
* 2) Start   date in input month and
*    Finish  date not in input month .
* 3) Start   date not in input month and
*    Finish  date in input month .

*1)
  CLEAR p_aufnr.
  SELECT SINGLE aufnr INTO p_aufnr
    FROM caufv                         "aufk
   WHERE pkosa = it_post-pcc_aufnr
     AND autyp = '10' "<- Production order
     AND ( gstri BETWEEN g_first_date AND g_last_date )
     AND ( gltri BETWEEN g_first_date AND g_last_date ) .

  IF sy-subrc <> 0 .
*2)
    CLEAR p_aufnr.
    SELECT SINGLE aufnr INTO p_aufnr
      FROM caufv                         "aufk
     WHERE pkosa = it_post-pcc_aufnr
       AND autyp = '10'                   "<- Production order
       AND ( gstri BETWEEN g_first_date AND g_last_date )
       AND ( gltri = space ) .

    IF sy-subrc <> 0 .
      CLEAR p_aufnr.
*3)
      SELECT SINGLE aufnr INTO p_aufnr
        FROM caufv                         "aufk
       WHERE pkosa = it_post-pcc_aufnr
         AND autyp = '10'                   "<- Production order
         AND gstri <=  g_first_date
         AND ( gltri BETWEEN g_first_date AND g_last_date ) .
    ENDIF.
  ENDIF.



ENDFORM.                    " GET_PRODUCT_ORDER
*&---------------------------------------------------------------------*
*&      Form  get_bwart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_MB1A_BWART  text
*----------------------------------------------------------------------*
FORM get_bwart CHANGING p_bwart
                        p_reas.
  IF it_mb1a-mbgbtr > 0 .
    IF it_mb1a-lgort = 'X551'.
      p_bwart = '551'.
    ELSE.
      p_bwart = '905'.
    ENDIF.
  ELSE.
    IF it_mb1a-lgort = 'X551'.
      p_bwart = '552'.
    ELSE.
      p_bwart = '906'.
    ENDIF.
  ENDIF.

  IF p_bwart = '551'.
    p_reas = p_os_rs.
  ELSE.
    p_reas = p_key_rs.
  ENDIF.
ENDFORM.                    " get_bwart
*&---------------------------------------------------------------------*
*&      Form  APPEND_BAPI_STRUCTURE_MB1A_HEA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_bapi_mb1a_header.
* Header.
  CLEAR : w_goodsmvt_header, w_goodsmvt_code, w_goodsmvt_headret,
          w_materialdocument, w_matdocumentyear,
          it_return, it_return[].
  w_goodsmvt_header-pstng_date = g_last_date.
  w_goodsmvt_header-doc_date   = sy-datum.
  w_goodsmvt_code-gm_code      = '03' ."c_gm_code.
  CONCATENATE text-010 sy-repid
            sy-uname sy-datum sy-uzeit
       INTO w_goodsmvt_header-header_txt.

ENDFORM.                    " APPEND_BAPI_STRUCTURE_MB1A_HEA
*&---------------------------------------------------------------------*
*&      Form  READ_RESOURCE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resource_data.
  CLEAR : it_resguid16,        it_resguid16[].
  CLEAR : if_modeid16.
  CLEAR : it_act_raw,          it_act_raw[].
  CLEAR : it_ppc_show_ext_act, it_ppc_show_ext_act[].

  RANGES: so_budat FOR  sy-datum.
  so_budat-option = 'BT'.
  so_budat-sign   = 'I'.
  so_budat-low    = g_first_date.
  so_budat-high   = g_last_date.
  APPEND so_budat.

  CALL FUNCTION 'PPC1DC_ACTS_SELECT'
    EXPORTING
      it_resguids      = it_resguid16
      if_modeguid      = if_modeid16
    TABLES
     ir_budat          = so_budat
*     ir_uname          = so_uname
*     ir_rptid          = so_rptid
*     ir_actid          = so_actid
     et_acts_ext       = it_act_raw
*   ET_HEADIDS        =
            .

  CALL FUNCTION 'PPC1RT_ACT_RAW_CONVERT'
       TABLES
            it_act_raw  = it_act_raw
            et_acts_ext = it_ppc_show_ext_act.

* Delete redundant records
  DELETE it_ppc_show_ext_act WHERE cost_center EQ space
                                OR acttype     EQ space.

  SORT it_ppc_show_ext_act BY cost_center acttype.
  DELETE ADJACENT DUPLICATES FROM it_ppc_show_ext_act
                  COMPARING cost_center acttype.

* Clear IT_PPC_ACT_MOD
  CLEAR : it_ppc_act_mod, it_ppc_act_mod[].

  LOOP AT it_ppc_show_ext_act. "where acttype = p_lstar.
    LOOP AT it_act_raw
                WHERE actid = it_ppc_show_ext_act-actid.
      MOVE-CORRESPONDING it_ppc_show_ext_act TO it_ppc_act_mod.
      MOVE-CORRESPONDING it_act_raw          TO it_ppc_act_mod.
      APPEND it_ppc_act_mod.
      CLEAR  it_ppc_act_mod.
      CLEAR  it_act_raw.
    ENDLOOP.
    CLEAR it_ppc_show_ext_act.
  ENDLOOP.
ENDFORM.                    " READ_RESOURCE_DATA
