*&---------------------------------------------------------------------*
*&  Include           ZAFI_RECLAIM_PROC_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data.

  DATA: l_index LIKE sy-tabix.
  DATA: $it_data LIKE it_data OCCURS 0 WITH HEADER LINE.
  DATA $vndr LIKE it_data-vndr.

*     UD1K940683 by IG.MOON 5/29/2007
*  select * into table it_data from ZTFI_RECLAIM_DAT
*     where
*            CORP eq 'A1' and
*            TYPE in ('','E').

  IF r_01 = 'X'.
    SELECT    corp doex vndr issu vdgb carc natn opcd
              SUM( bjpt ) AS bjpt
              SUM( bjla ) AS bjla
              SUM( bjmi ) AS bjmi
              SUM( ojpt ) AS ojpt
              SUM( ojla ) AS ojla
              SUM( ojmi ) AS ojmi
       INTO CORRESPONDING FIELDS OF TABLE it_data
       FROM ztfi_reclaim_dat
       WHERE  corp EQ 'A1'
         AND  doex IN s_doex
         AND  vndr IN s_vndr
         AND  issu IN s_issu
         AND  vdgb IN s_vdgb
* UD1K940839 by IG.MOON
         AND  natn IN s_natn
* end of UD1K940839
         AND  carc IN s_carc
        AND  type IN ('','E')
        GROUP BY CORP doex VNDR issu vdgb carc natn opcd.
*      end of UD1K940683

    LOOP AT it_data.

      $it_data = it_data.

      IF $it_data-vndr EQ 'SEF9'.
        IF $it_data-opcd CP '80BA16*' OR $it_data-opcd CP '90BA03*'.
          $it_data-vndr = 'YONG'.
        ENDIF.
      ENDIF.

      CLEAR $it_data-opcd.
      COLLECT $it_data.

    ENDLOOP.

    CLEAR : it_data[],it_data.
    it_data[] = $it_data[].

  ELSE.
    SELECT    corp doex vndr issu vdgb carc natn opcd
                SUM( bjpt ) AS bjpt
                SUM( bjla ) AS bjla
                SUM( bjmi ) AS bjmi
                SUM( ojpt ) AS ojpt
                SUM( ojla ) AS ojla
                SUM( ojmi ) AS ojmi
         INTO CORRESPONDING FIELDS OF TABLE it_data
         FROM ztfi_reclaim_dat
         WHERE  corp EQ 'A1'
           AND  doex IN s_doex
           AND  vndr IN s_vndr
           AND  issu IN s_issu
           AND  vdgb IN s_vdgb
* UD1K940839 by IG.MOON
           AND  natn IN s_natn
* end of UD1K940839
           AND  carc IN s_carc
*          AND  type = 'S'      " -UD1K942469
         AND  type IN ('S','D','M')  " +UD1K942469
          GROUP BY CORP doex VNDR issu vdgb carc natn opcd.
*      end of UD1K940683

    LOOP AT it_data.

      $it_data = it_data.

      IF $it_data-vndr EQ 'SEF9'.
        IF $it_data-opcd CP '80BA16*' OR $it_data-opcd CP '90BA03*'.
          $it_data-vndr = 'YONG'.
        ENDIF.
      ENDIF.

      CLEAR $it_data-opcd.
      COLLECT $it_data.

    ENDLOOP.

    CLEAR : it_data[],it_data.
    it_data[] = $it_data[].

    LOOP AT it_data.

      l_index = sy-tabix.
      IF it_data-vndr EQ 'YONG'.
        $vndr = 'SEF9'.

        SELECT SINGLE message msgrvs type INTO
            (it_data-message, it_data-msgrvs, it_data-type)
          FROM ztfi_reclaim_dat
            WHERE  corp EQ it_data-corp
              AND  doex EQ it_data-doex
              AND  vndr EQ $vndr
              AND  issu EQ it_data-issu
              AND  vdgb EQ it_data-vdgb
              AND  natn EQ it_data-natn
              AND  carc EQ it_data-carc
              AND  ( opcd LIKE '80BA16%' OR opcd LIKE '90BA03%' ).

        MODIFY it_data INDEX l_index.

      ELSEIF it_data-vndr EQ 'SEF9'.

        SELECT SINGLE message msgrvs type INTO
            (it_data-message, it_data-msgrvs, it_data-type)
          FROM ztfi_reclaim_dat
            WHERE  corp EQ it_data-corp
              AND  doex EQ it_data-doex
              AND  vndr EQ it_data-vndr
              AND  issu EQ it_data-issu
              AND  vdgb EQ it_data-vdgb
              AND  natn EQ it_data-natn
              AND  carc EQ it_data-carc
           AND  ( NOT opcd LIKE '80BA16%' AND NOT opcd LIKE '90BA03%' ).

        MODIFY it_data INDEX l_index.

      ELSE.

** Changed by Furong on 01/28/08
*      SELECT SINGLE message msgrvs INTO
*         (it_data-message, it_data-msgrvs)
        SELECT SINGLE message msgrvs type INTO
            (it_data-message, it_data-msgrvs, it_data-type)
** End of chnage
          FROM ztfi_reclaim_dat
            WHERE  corp EQ it_data-corp
              AND  doex EQ it_data-doex
              AND  vndr EQ it_data-vndr
              AND  issu EQ it_data-issu
              AND  vdgb EQ it_data-vdgb
              AND  natn EQ it_data-natn
              AND  carc EQ it_data-carc.
        MODIFY it_data INDEX l_index.

      ENDIF.

    ENDLOOP.

  ENDIF.

* UD1K940843 by IG.MOON 6/15/2007
  PERFORM get_all_country_code. " <--- get iso code from kna1
* end of UD1K940843

* Populate Final Internal Table.
  LOOP AT it_data.

* Customer
    it_output-kunnr =  it_data-vndr.
    it_final-kunnr  =  it_data-vndr.

    it_final-issu   =  it_data-issu.
    it_output-issu   =  it_data-issu.

* UD1K940839 by IG.MOON 6/15/2007
* //////////////////////////////////////////////////////////
*    IT_FINAL-RONO   = IT_OUTPUT-RONO   =  IT_DATA-RONO.
    it_final-natn   =  it_output-natn   =  it_data-natn.

    IF it_final-natn IS INITIAL.
      PERFORM set_country_by_hard_code. " <--- 'US' 'CA' 'PR'
    ELSE.
      READ TABLE gt_ctry WITH KEY kunnr = it_final-natn BINARY SEARCH.
      IF sy-subrc EQ 0.
        it_final-cty  = it_output-cty = gt_ctry-land1.
      ELSE.
        PERFORM set_country_by_hard_code.
      ENDIF.
    ENDIF.
* end of UD1K940839

    it_output-amount =  it_data-bjpt + it_data-bjla +
                        it_data-bjmi.

    it_final-sub_tot1 = it_output-amount.
    IF it_data-carc IS INITIAL.
      it_output-model = space.
      it_final-model = space.
    ELSE.
      CONCATENATE 'WTY_' it_data-carc INTO it_output-model.
      CONCATENATE 'WTY_' it_data-carc INTO it_final-model.
    ENDIF.
*    COLLECT IT_OUTPUT.

* ALV DISPLAY Internal Table
    it_final-bjpt  = it_data-bjpt.
    it_final-bjla  = it_data-bjla.
    it_final-bjmi  = it_data-bjmi.

    it_final-sub_tot2 = it_data-ojpt + it_data-ojla +
                        it_data-ojmi.
    it_final-ojpt  = it_data-ojpt.
    it_final-ojla  = it_data-ojla.
    it_final-ojmi  = it_data-ojmi.
    it_final-msg  = it_data-message.
    it_final-msgrvs  = it_data-msgrvs.

    COLLECT it_final.

*    IT_OUTPUT-AMOUNT = IT_FINAL-SUB_TOT1 - IT_FINAL-SUB_TOT2.
*    COLLECT IT_OUTPUT.
*
  ENDLOOP.

  LOOP AT it_final.
    it_final-rc_amt = it_final-sub_tot1. " - IT_FINAL-SUB_TOT2.
    MODIFY it_final INDEX sy-tabix.
    MOVE-CORRESPONDING it_final TO it_output.
    APPEND it_output.CLEAR it_output.
  ENDLOOP.

  SORT it_output BY kunnr.

  SELECT matnr prodh INTO TABLE gt_prodh
     FROM mvke
     FOR ALL ENTRIES IN it_output
     WHERE matnr = it_output-model.
  SORT gt_prodh BY matnr.
ENDFORM.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_structures
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bapi_structures USING f_mode.

  DATA : line_no TYPE i,
         amt LIKE it_output-amount.

  LOOP AT it_output.
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
         IMPORTING
              own_logical_system = gd_documentheader-obj_sys.

* fill header
    AT NEW kunnr.

      REFRESH: it_accountreceivable, it_accountgl, it_accounttax,
               it_criteria, it_valuefield, it_currencyamount,
               it_return, it_salesorder, it_salesamount.
      REFRESH: wt_output.

      CLEAR: it_accountreceivable, it_accountgl, it_accounttax,
               it_criteria, it_valuefield, it_currencyamount,
               it_return, it_salesorder, it_salesamount.
      CLEAR: wt_output.

*     OBJ_TYPE = 'IDOC' -> OBJ_KEY is unique
*     OBJ_TYPE = 'BKPFF' -> OBJ_KEY is self doc #
      gd_documentheader-obj_type   = 'BKPFF'.
      gd_documentheader-obj_key    = '$'. "IDOC -> Unique ref.#
* GD_DOCUMENTHEADER-OBJ_SYS    = FUNKTION OWN_LOGICAL_SYSTEM_GET
      gd_documentheader-username   = sy-uname.
      gd_documentheader-header_txt = p_head.
* GD_DOCUMENTHEADER-OBJ_KEY_R  =
      gd_documentheader-comp_code  = p_bukrs.
      gd_documentheader-fisc_year  = p_gjahr.
      gd_documentheader-doc_date   = p_bldat. "'19990303'.
      gd_documentheader-pstng_date = p_budat. "'19990303'.

*      UD1K940683 by IG.MOON 5/29/2007
*      GD_DOCUMENTHEADER-FIS_PERIOD = P_MONAT.
*      end of UD1K940683

      gd_documentheader-doc_type   = 'RS'.
    ENDAT.

    APPEND it_output TO wt_output.
    line_no = line_no + 1.
    amt = amt + it_output-amount.

* fill GL (line 2)
*    concatenate 'WTY_'  it_output-model into it_output-model.
    it_accountgl-itemno_acc     = line_no.

    IF it_output-issu CP '*WW*' OR it_output-issu CP '*WA*' OR
       it_output-issu CP '*WR*' OR it_output-issu CP '*WF*'.
      it_accountgl-gl_account     = p_hkont1.
    ELSE.
      it_accountgl-gl_account     = p_hkont2.
    ENDIF.

    it_accountgl-item_text      = p_item.
    CONCATENATE it_output-cty '-' it_output-model
           INTO it_accountgl-ref_key_1.
    CONCATENATE it_output-cty '-' it_output-model
           INTO it_accountgl-ref_key_3.


* by ig.moon 8/21/2009 {

    IF it_output-kunnr EQ 'YONG' OR
       it_output-kunnr EQ 'CKDP'.
      CONCATENATE it_output-kunnr it_output-issu
       INTO it_accountgl-alloc_nmbr.
    ELSE.
      it_accountgl-alloc_nmbr = it_output-issu .
    ENDIF.

* }

* by IG.MOON 3/6/2009

*    IF IT_OUTPUT-KUNNR EQ 'CKDP'.
*      CONCATENATE IT_OUTPUT-KUNNR IT_OUTPUT-ISSU
*       INTO IT_ACCOUNTGL-ALLOC_NMBR.
*    ELSE.
*      IT_ACCOUNTGL-ALLOC_NMBR = IT_OUTPUT-ISSU .
*    ENDIF.

*   IT_ACCOUNTGL-MATERIAL       = it_output-model.
    APPEND it_accountgl.
    CLEAR it_accountgl.

    it_currencyamount-itemno_acc   = line_no.
    it_currencyamount-curr_type    = '00'.
    it_currencyamount-currency     = 'USD'.
    it_currencyamount-amt_doccur   = it_output-amount * -1.
*   IT_CURRENCYAMOUNT-EXCH_RATE_V  = '0.5'.
    APPEND it_currencyamount.
    CLEAR it_currencyamount.

    it_criteria-itemno_acc = line_no.
    it_criteria-fieldname  = 'VTWEG'.
    it_criteria-character  = '40'.    "DstCh=40 warranty
    APPEND it_criteria.
    CLEAR it_criteria.

    it_criteria-itemno_acc = line_no.
    it_criteria-fieldname  = 'PRODH'.
    READ TABLE gt_prodh WITH KEY matnr = it_output-model BINARY SEARCH.
    it_criteria-character  = gt_prodh-prodh.
    APPEND it_criteria.
    CLEAR it_criteria.

    it_criteria-itemno_acc = line_no.
    it_criteria-fieldname  = 'PAPH1'.
    it_criteria-character  = '00001'.  "Vehociel Product
    APPEND it_criteria.
    CLEAR it_criteria.

    it_criteria-itemno_acc = line_no.
    it_criteria-fieldname  = 'KMLAND'.
    it_criteria-character  = it_output-cty.
    APPEND it_criteria.
    CLEAR it_criteria.

* WITHOUT THIS; PA IS NOT POSTED.
    it_valuefield-itemno_acc = line_no.
*    IF IT_OUTPUT-ISSU CP '*WW*'.
    IF it_output-issu CP '*WW*' OR it_output-issu CP '*WA*' OR
       it_output-issu CP '*WR*' OR it_output-issu CP '*WF*'.
      it_valuefield-fieldname  = 'VV495'.
    ELSE.
      it_valuefield-fieldname  = 'VV500'.
    ENDIF.
*   IT_VALUEFIELD-CURR_TYPE  = '10'.   "NO CURRENCY KEY->DUMP
    it_valuefield-currency   = 'USD'.
    it_valuefield-amt_valcom = it_output-amount * -1.
    APPEND it_valuefield.
    CLEAR it_valuefield.

* CALL BAPI to POST RECLAIM DOCUMENT for every customer
    AT END OF kunnr.
* fill AR (line 1)
      line_no = line_no + 1.

      it_accountreceivable-itemno_acc = line_no.

      IF it_output-kunnr EQ 'CKDP'.
        it_accountreceivable-customer   = 'SBC3'.
        it_accountreceivable-alloc_nmbr   = 'CKDP'.
      ELSEIF it_output-kunnr EQ 'YONG'.
        it_accountreceivable-customer   = 'SEF9'.
        it_accountreceivable-alloc_nmbr   = 'YONG'.
      ELSE.
        it_accountreceivable-customer   = it_output-kunnr.
      ENDIF.

      it_accountreceivable-item_text  = p_item.
      APPEND it_accountreceivable.
      CLEAR it_accountreceivable.
*      IT_CURRENCYAMOUNT-ITEMNO_ACC   = line_no.
*      IT_CURRENCYAMOUNT-CURR_TYPE    = '00'.
*      IT_CURRENCYAMOUNT-CURRENCY     = 'USD'.
*      IT_CURRENCYAMOUNT-AMT_DOCCUR   = 100.
*      IT_CURRENCYAMOUNT-EXCH_RATE_V  = '0.5'.
*      APPEND IT_CURRENCYAMOUNT.


      it_currencyamount-itemno_acc   = line_no.
      it_currencyamount-curr_type    = '00'.
      it_currencyamount-currency     = 'USD'.
      it_currencyamount-amt_doccur   = amt.
*     IT_CURRENCYAMOUNT-EXCH_RATE_V  = '0.5'.
      APPEND it_currencyamount.
      CLEAR it_currencyamount.
      CLEAR : line_no, amt.
      PERFORM call_bapi USING f_mode.
    ENDAT.
  ENDLOOP.

* Update TABle with status.
  IF f_mode = 'P'.

    delete it_rec_posted where kunnr <> 'SBC3' and          "UD1K955778
                               kunnr <> 'CKDP'.             "UD1K955778

    modify ztfi_rec_posted from table it_rec_posted.        "UD1K953781

*    MODIFY ZTFI_RECLAIM_DAT FROM TABLE IT_DATA.

    LOOP AT it_data.

      READ TABLE it_output WITH KEY issu = it_data-issu
                                    natn = it_data-natn.
      CHECK sy-subrc EQ 0.

      IF it_data-vndr EQ 'YONG'.
        it_data-vndr = 'SEF9'.

        CLEAR *ztfi_reclaim_dat.
        SELECT * FROM ztfi_reclaim_dat
               WHERE  corp EQ it_data-corp
                 AND  doex EQ it_data-doex
                 AND  vndr EQ it_data-vndr
                 AND  issu EQ it_data-issu
                 AND  vdgb EQ it_data-vdgb
                 AND  natn EQ it_data-natn
                 AND  carc EQ it_data-carc.

          IF  ztfi_reclaim_dat-opcd CP '80BA16*' OR
              ztfi_reclaim_dat-opcd CP '90BA03*'.
             *ztfi_reclaim_dat = ztfi_reclaim_dat.
             *ztfi_reclaim_dat-type = it_data-type.
             *ztfi_reclaim_dat-message = it_data-message.
             *ztfi_reclaim_dat-msgrvs = it_data-msgrvs.
             *ztfi_reclaim_dat-aenam = it_data-aenam.
             *ztfi_reclaim_dat-aedat = it_data-aedat.
             *ztfi_reclaim_dat-aezet = it_data-aezet.
             *ztfi_reclaim_dat-fiscal_yr = it_data-fiscal_yr.
             *ztfi_reclaim_dat-posting_dt = it_data-posting_dt.
            ztfi_reclaim_dat = *ztfi_reclaim_dat.
            UPDATE ztfi_reclaim_dat.
          ENDIF.
        ENDSELECT.

      ELSEIF it_data-vndr EQ 'SEF9'.

        CLEAR *ztfi_reclaim_dat.
        SELECT * FROM ztfi_reclaim_dat
               WHERE  corp EQ it_data-corp
                 AND  doex EQ it_data-doex
                 AND  vndr EQ it_data-vndr
                 AND  issu EQ it_data-issu
                 AND  vdgb EQ it_data-vdgb
                 AND  natn EQ it_data-natn
                 AND  carc EQ it_data-carc.

          IF  ztfi_reclaim_dat-opcd CP '80BA16*' OR
              ztfi_reclaim_dat-opcd CP '90BA03*'.
          ELSE.
             *ztfi_reclaim_dat = ztfi_reclaim_dat.
             *ztfi_reclaim_dat-type = it_data-type.
             *ztfi_reclaim_dat-message = it_data-message.
             *ztfi_reclaim_dat-msgrvs = it_data-msgrvs.
             *ztfi_reclaim_dat-aenam = it_data-aenam.
             *ztfi_reclaim_dat-aedat = it_data-aedat.
             *ztfi_reclaim_dat-aezet = it_data-aezet.
             *ztfi_reclaim_dat-fiscal_yr = it_data-fiscal_yr.
             *ztfi_reclaim_dat-posting_dt = it_data-posting_dt.
            ztfi_reclaim_dat = *ztfi_reclaim_dat.
            UPDATE ztfi_reclaim_dat.
          ENDIF.
        ENDSELECT.

                                                            "UD1K955778
*     ELSEIF it_data-vndr EQ 'SBC3'.                        "UD1K951780
*     Do not modify ztfi_reclaim_dat                        "UD1K951780

      ELSE.

        UPDATE ztfi_reclaim_dat SET type = it_data-type
                                    message = it_data-message
                                    msgrvs = it_data-msgrvs
                                    aenam = it_data-aenam
                                    aedat = it_data-aedat
                                    aezet = it_data-aezet
                                    fiscal_yr  = it_data-fiscal_yr
                                    posting_dt = it_data-posting_dt
               WHERE  corp EQ it_data-corp
                 AND  doex EQ it_data-doex
                 AND  vndr EQ it_data-vndr
                 AND  issu EQ it_data-issu
                 AND  vdgb EQ it_data-vdgb
                 AND  natn EQ it_data-natn
                 AND  carc EQ it_data-carc.
      ENDIF.

*      MODIFY IT_DATA  TRANSPORTING TYPE MESSAGE AENAM AEDAT AEZET
*                                    WHERE VNDR = WT_OUTPUT-KUNNR
*                                      AND ISSU  = WT_OUTPUT-ISSU.
*

    ENDLOOP.

    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDFORM.                    " fill_bapi_structures
*&---------------------------------------------------------------------*
*&      Form  GET_COUNTRY_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_all_country_code.

  DATA $gt_ctry LIKE gt_ctry OCCURS 0 WITH HEADER LINE.
  __cls gt_ctry.

  LOOP AT it_data.
    $gt_ctry-kunnr = it_data-natn.
    APPEND $gt_ctry.CLEAR $gt_ctry.
  ENDLOOP.

  SORT $gt_ctry BY kunnr.
  DELETE ADJACENT DUPLICATES FROM $gt_ctry COMPARING kunnr.

  SELECT kunnr land1
  INTO TABLE gt_ctry
  FROM kna1
  FOR ALL ENTRIES IN $gt_ctry
  WHERE kunnr EQ $gt_ctry-kunnr.

  SORT gt_ctry BY kunnr.

ENDFORM.                    " GET_COUNTRY_CODE
*&---------------------------------------------------------------------*
*&      Form  set_country
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_country_by_hard_code.
  CASE it_data-doex.
    WHEN 'A'.
      it_final-cty  = it_output-cty = 'PR'.
    WHEN 'D'.
      it_final-cty  = it_output-cty = 'US'.
    WHEN 'E'.
      it_final-cty  = it_output-cty = 'CA'.
  ENDCASE.
ENDFORM.                    " set_country
*&---------------------------------------------------------------------*
*&      Form  call_BAPI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi USING f_mode.
*  DATA: $OBJ_KEY LIKE  BAPIACHE01-OBJ_KEY,
*       OBJ_SYS LIKE BAPIACHE01-OBJ_SYS,
*       OBJ_TYPE LIKE BAPIACHE01-OBJ_TYPE.
  DATA: l_carc LIKE it_data-carc.

  IF f_mode = 'S'.
    CALL FUNCTION 'BAPI_ACC_BILLING_CHECK'
         EXPORTING
              documentheader    = gd_documentheader
              customercpd       = gd_customercpd
         TABLES
              accountreceivable = it_accountreceivable
              accountgl         = it_accountgl
              accounttax        = it_accounttax
              criteria          = it_criteria
              valuefield        = it_valuefield
              currencyamount    = it_currencyamount
              return            = it_return
              salesorder        = it_salesorder
              salesamount       = it_salesamount.
  ELSE.
    CLEAR $obj_key.
    CALL FUNCTION 'BAPI_ACC_BILLING_POST'
         EXPORTING
              documentheader    = gd_documentheader
              customercpd       = gd_customercpd
         IMPORTING
* UD1K940910
              obj_type          = obj_type
              obj_key           = $obj_key
              obj_sys           =  obj_sys
* end of UD1K940910
         TABLES
              accountreceivable = it_accountreceivable
              accountgl         = it_accountgl
              accounttax        = it_accounttax
              criteria          = it_criteria
              valuefield        = it_valuefield
              currencyamount    = it_currencyamount
              return            = it_return
              salesorder        = it_salesorder
              salesamount       = it_salesamount.
  ENDIF.

** CLEAR VARIABLES
  LOOP AT it_return WHERE  type EQ 'S'.
    w_accdoc = it_return-message_v2+0(10).
  ENDLOOP.

  IF sy-subrc EQ 0 .
*    CLEAR : IT_CRITERIA[],  IT_VALUEFIELD[],  GD_DOCUMENTHEADER,
*            GD_CUSTOMERCPD,  IT_ACCOUNTRECEIVABLE[], IT_ACCOUNTGL[],
*            IT_ACCOUNTTAX[], IT_CURRENCYAMOUNT[], IT_RETURN[],
*            IT_VALUEFIELD[],IT_SALESORDER[],IT_SALESAMOUNT[].
    COMMIT WORK.
    CONCATENATE obj_type $obj_key obj_sys INTO w_msgrvs.
    LOOP AT wt_output.

      READ TABLE it_final WITH KEY kunnr = wt_output-kunnr
                                   cty   = wt_output-cty
                                   model = wt_output-model
                                   issu  = wt_output-issu.

*      IT_FINAL-MSG = $OBJ_KEY. "' ok'.
      it_final-msg = w_accdoc.
      it_final-msgrvs = w_msgrvs.
      MODIFY it_final TRANSPORTING msg msgrvs
                                  WHERE kunnr = wt_output-kunnr
                                   AND cty   = wt_output-cty
                                   AND model = wt_output-model
                                   AND issu  = wt_output-issu.

      move-corresponding it_final to it_rec_posted.         "UD1K953781

      IF f_mode = 'S'.
      ELSE.
        it_data-type = 'S'.
*        IT_DATA-MESSAGE = $OBJ_KEY. "' Posted'.
        it_data-message = w_accdoc.
        it_data-aenam = sy-uname.                           "UD1K930698
        it_data-aedat = sy-datum.                           "UD1K930698
        it_data-aezet = sy-uzeit.                           "UD1K930698
        it_data-msgrvs = w_msgrvs.
        it_data-fiscal_yr  = p_gjahr.                     " +UD1K942469
        it_data-posting_dt = p_budat.                     " +UD1K942469

        l_carc = wt_output-model+4(2).

        MODIFY it_data  TRANSPORTING type message msgrvs aenam
*                                     aedat aezet        " -UD1K942469
                                     aedat aezet fiscal_yr posting_dt
                                                         " +UD1K942469
                                      WHERE vndr = wt_output-kunnr
                                        AND issu  = wt_output-issu
                                        AND carc = l_carc
                                        AND natn = wt_output-natn.
* BEGIN OF UD1K953781
      it_rec_posted-type  = it_data-type.
      it_rec_posted-budat = it_data-posting_dt.
      it_rec_posted-cpudt = it_data-aedat.
      it_rec_posted-cputm = it_data-aezet.
      it_rec_posted-usnam = it_data-aenam.
* END OF UD1K953781
      ENDIF.

      append it_rec_posted.                                 "UD1K953781

    ENDLOOP.
  ELSE.
    LOOP AT wt_output.
      READ TABLE it_final WITH KEY kunnr = wt_output-kunnr
                                   issu  = wt_output-issu.
      it_final-msg = it_return-message.

      MODIFY it_final TRANSPORTING msg WHERE kunnr = wt_output-kunnr
                                         AND cty   = wt_output-cty
                                         AND model = wt_output-model
                                         AND issu  = wt_output-issu.
      IF f_mode = 'S'.
      ELSE.
        it_data-type = 'E'.
        it_data-message = it_return-message.
        it_data-aenam = sy-uname.                           "UD1K930698
        it_data-aedat = sy-datum.                           "UD1K930698
        it_data-aezet = sy-uzeit.                           "UD1K930698
        MODIFY it_data  TRANSPORTING type message aenam aedat aezet
                                      WHERE vndr = wt_output-kunnr
                                        AND issu  = wt_output-issu.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " call_BAPI
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_selected_rows.


  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: l_line TYPE i,
        w_repid LIKE sy-repid.

  clear: ty_rows, ty_rows[], ty_row,  ty_roid, ty_roid[].

      call method g_grid1->get_selected_rows
        importing
          et_index_rows = ty_rows       "ALV INDEX INFO
          et_row_no     = ty_roid.      "ALV ROW INFO
***
***      loop at ty_rows into ty_row.
***        read table gt_grid  index ty_row.

***  CALL METHOD alv_grid->get_selected_rows
***           IMPORTING et_index_rows = lt_rows[]
***                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    w_repid = sy-repid.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
  REFRESH  it_output. CLEAR it_output.
  READ TABLE ty_rows into ty_row  INDEX 1.

* If  rows are selected
  IF sy-subrc EQ 0.
    LOOP AT ty_rows into ty_row WHERE index NE 0.
      READ TABLE it_final INDEX ty_row-index.
      IF sy-subrc EQ 0.
*-Validation for Manually created records. They can't be reveresed
*-Start of +UD1K942469
        IF sy-ucomm = 'REVERSAL'.
          IF it_final-msg = text-001.
            CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                 EXPORTING
                      textline1 = text-006.
            CONTINUE.
          ELSEIF it_final-msg = text-005.
            CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                 EXPORTING
                      textline1 = text-007.
            CONTINUE.
          ENDIF.
        ENDIF.
*-End of +UD1K942469
* Customer
        it_output-kunnr =  it_final-kunnr.
        it_output-cty = it_final-cty.
        it_output-issu = it_final-issu.
        it_output-natn = it_final-natn.

*        IT_OUTPUT-AMOUNT =  IT_FINAL-BJPT + IT_FINAL-BJLA +
*                            IT_FINAL-BJMI.
        it_output-amount =  it_final-rc_amt.
        it_output-model = it_final-model.

        it_output-msg =  it_final-msg.
        it_output-msgrvs = it_final-msgrvs.

*          COLLECT IT_OUTPUT.
        APPEND it_output.
        CLEAR it_output.
      ENDIF.
    ENDLOOP.

  ELSE.
* No Rows are selected.
*    LOOP AT IT_FINAL.
*      IT_OUTPUT-KUNNR =  IT_FINAL-KUNNR.
*      IT_OUTPUT-CTY = IT_FINAL-CTY.
*      IT_OUTPUT-ISSU = IT_FINAL-ISSU.
*
**      IT_OUTPUT-AMOUNT =  IT_FINAL-BJPT + IT_FINAL-BJLA +
**                          IT_FINAL-BJMI.
*      IT_OUTPUT-AMOUNT =  IT_FINAL-RC_AMT.
*      IT_OUTPUT-MODEL = IT_FINAL-MODEL.
**      COLLECT IT_OUTPUT.
*      APPEND IT_OUTPUT.
*      CLEAR IT_OUTPUT.
*    ENDLOOP.

  ENDIF.

  SORT it_output BY kunnr.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  BAKUP_OLD_RECORDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bakup_old_records.

  DATA : mod TYPE i, b_cnt TYPE i.
  DATA   i_ztfi_reclaim_bak LIKE ztfi_reclaim_bak
         OCCURS 0 WITH HEADER LINE.

  DELETE FROM ztfi_reclaim_bak
                  WHERE bkdat EQ sy-datum.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE
           i_ztfi_reclaim_bak FROM
           ztfi_reclaim_dat .

  COMMIT WORK.

  i_ztfi_reclaim_bak-bkdat = sy-datum.
  i_ztfi_reclaim_bak-bktim = sy-uzeit.

  MODIFY i_ztfi_reclaim_bak TRANSPORTING bkdat bktim
                                    WHERE bkdat IS initial.

  LOOP AT i_ztfi_reclaim_bak.
    ADD 1 TO b_cnt.
    mod = b_cnt MOD 1000.
    MOVE-CORRESPONDING i_ztfi_reclaim_bak TO ztfi_reclaim_bak.
    INSERT ztfi_reclaim_bak.
    IF mod EQ 1. COMMIT WORK. ENDIF.
  ENDLOOP.

ENDFORM.                    " BAKUP_OLD_RECORDS
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
FORM pop_up USING    p_text
            CHANGING p_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            titel          = 'Confirm!'
            cancel_display = 'X'
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  INIT_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_button.

  WRITE: icon_packing AS ICON TO bkup,
         'Backup'  TO sscrfields-functxt_01.

ENDFORM.                    " INIT_BUTTON
*&---------------------------------------------------------------------*
*&      Form  REVERSAL_BAPI_POST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reversal_bapi_post.
  DATA: reversal LIKE bapiacrev.
*  DATA : LINE_NO TYPE I,
*         AMT LIKE IT_OUTPUT-AMOUNT.
  DATA: l_msg(60),
        l_num_ranger LIKE inri-nrrangenr.
  DATA  $flag.
  DATA: BEGIN OF lt_rvs OCCURS 0,
        msg LIKE it_output-msg,
        msgrvs LIKE it_output-msgrvs,
        END OF lt_rvs.

  LOOP AT it_output.
    MOVE-CORRESPONDING it_output TO lt_rvs.
    COLLECT lt_rvs.
    move-corresponding it_output to it_rec_posted.          "UD1K953781
    append it_rec_posted. clear it_rec_posted.              "UD1K953781
    CLEAR: it_output, lt_rvs.
  ENDLOOP.

  l_num_ranger = '66'.
  LOOP AT lt_rvs.
    reversal-obj_type = lt_rvs-msgrvs+0(5).
*   CONCATENATE 'R' LT_RVS-MSG+1(9) INTO REVERSAL-OBJ_KEY.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr                   = l_num_ranger
        object                        = 'RF_BELEG'
*       QUANTITY                      = '1'
        subobject                     = 'H201'
        toyear                        = p_gjahr
*       IGNORE_BUFFER                 = ' '
     IMPORTING
       number                        = w_accdoc_rvs
*       QUANTITY                      =
*       RETURNCODE                    =
*     EXCEPTIONS
*       INTERVAL_NOT_FOUND            = 1
*       NUMBER_RANGE_NOT_INTERN       = 2
*       OBJECT_NOT_FOUND              = 3
*       QUANTITY_IS_0                 = 4
*       QUANTITY_IS_NOT_1             = 5
*       INTERVAL_OVERFLOW             = 6
*       BUFFER_OVERFLOW               = 7
*       OTHERS                        = 8
              .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    reversal-obj_key = w_accdoc_rvs.

    reversal-obj_sys = lt_rvs-msgrvs+23(10).
    reversal-obj_key_r = lt_rvs-msgrvs+5(18).
    reversal-pstng_date = p_budat.
    reversal-fis_period = p_budat+4(2).
    reversal-comp_code = p_bukrs.
    reversal-reason_rev = p_reason.
    reversal-ac_doc_no = lt_rvs-msg+0(10).
    w_accdoc =  reversal-ac_doc_no.

*CALL FUNCTION 'BAPI_ACC_BILLING_REV_CHECK'
*  EXPORTING
*    REVERSAL       = REVERSAL
*
*  TABLES
*    RETURN         = IT_RETURN
*          .
*
    REFRESH it_return.

    CALL FUNCTION 'BAPI_ACC_BILLING_REV_POST'
         EXPORTING
              reversal = reversal
         IMPORTING
              obj_type = obj_type
              obj_key  = $obj_key
              obj_sys  = obj_sys
         TABLES
              return   = it_return.

    LOOP AT it_return WHERE  type EQ 'S'.
      w_accdoc_rvs = it_return-message_v2+0(10).
    ENDLOOP.

    IF sy-subrc EQ 0 .
*    CLEAR : IT_CRITERIA[],  IT_VALUEFIELD[],  GD_DOCUMENTHEADER,
*            GD_CUSTOMERCPD,  IT_ACCOUNTRECEIVABLE[], IT_ACCOUNTGL[],
*            IT_ACCOUNTTAX[], IT_CURRENCYAMOUNT[], IT_RETURN[],
*            IT_VALUEFIELD[],IT_SALESORDER[],IT_SALESAMOUNT[].
      COMMIT WORK.
      LOOP AT it_final.
        IF it_final-msg =  w_accdoc.

*        IT_FINAL-MSG = W_ACCDOC_RVS.
          it_final-msg = ' '.
          MODIFY it_final. " TRANSPORTING MSG.
        ENDIF.
      ENDLOOP.
      LOOP AT it_data.
        IF it_data-message = w_accdoc.
*        IT_DATA-MESSAGE = $OBJ_KEY. "' Posted'.
          it_data-type = ' '.
*       IT_DATA-MESSAGE = W_ACCDOC_RVS.
          it_data-message = ' '.
          it_data-aenam = sy-uname.                         "UD1K930698
          it_data-aedat = sy-datum.                         "UD1K930698
          it_data-aezet = sy-uzeit.                         "UD1K930698

          MODIFY it_data. "  TRANSPORTING TYPE MESSAGE AENAM
*                                     AEDAT AEZET
*                                      WHERE VNDR = IT_OUTPUT-KUNNR
*                                        AND ISSU  = IT_OUTPUT-ISSU
*                                        and MESSAGE = W_ACCDOC.
        ENDIF.
      ENDLOOP.
    ELSE.
      READ TABLE it_return INDEX 1.
      l_msg = it_return-message.
      READ TABLE it_return INDEX 2.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel         = 'Error in reveral'
          txt1          = l_msg
          txt2          = it_return-message
*         TXT3          = ' '
*         TXT4          = ' '
                .
      CLEAR $flag.
      IF it_return-message CP '*Document was already reversed*'.
        $flag = 'X'.
        LOOP AT it_data.
          IF it_data-message = w_accdoc.
            it_data-type = ' '.
            it_data-message = ' '.
           it_data-aenam = sy-uname.                        "UD1K930698
           it_data-aedat = sy-datum.                        "UD1K930698
           it_data-aezet = sy-uzeit.                        "UD1K930698
            MODIFY it_data.
          ENDIF.
        ENDLOOP.
      ENDIF.

*IT_RETURN-MESSAGE_V1.
*      LOOP AT IT_OUTPUT.
*        READ TABLE IT_FINAL WITH KEY KUNNR = IT_OUTPUT-KUNNR
*                                     ISSU  = IT_OUTPUT-ISSU.
*        IT_FINAL-MSG = IT_RETURN-MESSAGE.
*        MODIFY IT_FINAL TRANSPORTING MSG WHERE KUNNR = IT_OUTPUT-KUNNR
*                                           AND CTY   = IT_OUTPUT-CTY
*                                           AND MODEL = IT_OUTPUT-MODEL
*                                           AND ISSU  = IT_OUTPUT-ISSU.
*
*        IT_DATA-TYPE = 'E'.
*        IT_DATA-MESSAGE = IT_RETURN-MESSAGE.
*        IT_DATA-AENAM = SY-UNAME.                           "UD1K930698
*        IT_DATA-AEDAT = SY-DATUM.                           "UD1K930698
*        IT_DATA-AEZET = SY-UZEIT.                           "UD1K930698
*        MODIFY IT_DATA  TRANSPORTING TYPE MESSAGE AENAM AEDAT AEZET
*                                      WHERE VNDR = IT_OUTPUT-KUNNR
*                                        AND ISSU  = IT_OUTPUT-ISSU.
*      ENDLOOP.
    ENDIF.

  ENDLOOP.

  delete it_rec_posted where kunnr <> 'SBC3' and            "UD1K955778
                             kunnr <> 'CKDP'.               "UD1K955778

  delete ztfi_rec_posted from table it_rec_posted.          "UD1K953781

* Update TABle with status.
*  IF F_MODE = 'P'.
*  MODIFY ZTFI_RECLAIM_DAT FROM TABLE IT_DATA.

  LOOP AT it_data.
    READ TABLE it_output WITH KEY issu = it_data-issu
                                  natn = it_data-natn.
    CHECK sy-subrc EQ 0.

    IF it_data-message IS INITIAL OR $flag EQ 'X'.

      IF it_data-vndr EQ 'YONG'.
        it_data-vndr = 'SEF9'.

        CLEAR *ztfi_reclaim_dat.
        SELECT * FROM ztfi_reclaim_dat
               WHERE  corp EQ it_data-corp
                 AND  doex EQ it_data-doex
                 AND  vndr EQ it_data-vndr
                 AND  issu EQ it_data-issu
                 AND  vdgb EQ it_data-vdgb
                 AND  natn EQ it_data-natn
                 AND  carc EQ it_data-carc.

          IF  ztfi_reclaim_dat-opcd CP '80BA16*' OR
              ztfi_reclaim_dat-opcd CP '90BA03*'.
             *ztfi_reclaim_dat = ztfi_reclaim_dat.
             *ztfi_reclaim_dat-type = it_data-type.
             *ztfi_reclaim_dat-message = it_data-message.
             *ztfi_reclaim_dat-aenam = it_data-aenam.
             *ztfi_reclaim_dat-aedat = it_data-aedat.
             *ztfi_reclaim_dat-aezet = it_data-aezet.
            ztfi_reclaim_dat = *ztfi_reclaim_dat.
            UPDATE ztfi_reclaim_dat.
          ENDIF.
        ENDSELECT.

      ELSEIF  it_data-vndr EQ 'SEF9'.
        CLEAR *ztfi_reclaim_dat.
        SELECT * FROM ztfi_reclaim_dat
               WHERE  corp EQ it_data-corp
                 AND  doex EQ it_data-doex
                 AND  vndr EQ it_data-vndr
                 AND  issu EQ it_data-issu
                 AND  vdgb EQ it_data-vdgb
                 AND  natn EQ it_data-natn
                 AND  carc EQ it_data-carc.

          IF  ztfi_reclaim_dat-opcd CP '80BA16*' OR
              ztfi_reclaim_dat-opcd CP '90BA03*'.
          ELSE.
             *ztfi_reclaim_dat = ztfi_reclaim_dat.
             *ztfi_reclaim_dat-type = it_data-type.
             *ztfi_reclaim_dat-message = it_data-message.
             *ztfi_reclaim_dat-aenam = it_data-aenam.
             *ztfi_reclaim_dat-aedat = it_data-aedat.
             *ztfi_reclaim_dat-aezet = it_data-aezet.
            ztfi_reclaim_dat = *ztfi_reclaim_dat.
            UPDATE ztfi_reclaim_dat.
          ENDIF.
        ENDSELECT.
      ELSE.
        UPDATE ztfi_reclaim_dat SET type = it_data-type
                                    message = it_data-message
                                    aenam = it_data-aenam
                                    aedat = it_data-aedat
                                    aezet = it_data-aezet
               WHERE  corp EQ it_data-corp
                 AND  doex EQ it_data-doex
                 AND  vndr EQ it_data-vndr
                 AND  issu EQ it_data-issu
                 AND  vdgb EQ it_data-vdgb
                 AND  natn EQ it_data-natn
                 AND  carc EQ it_data-carc.
      ENDIF.
    ENDIF.

*      MODIFY IT_DATA  TRANSPORTING TYPE MESSAGE AENAM AEDAT AEZET
*                                    WHERE VNDR = WT_OUTPUT-KUNNR
*                                      AND ISSU  = WT_OUTPUT-ISSU.
*
  ENDLOOP.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " REVERSAL_BAPI_POST
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       Update table
*----------------------------------------------------------------------*
*      -->FMODE Status field
*      -->W_MSG Message
*----------------------------------------------------------------------*
FORM update_table USING fmode w_msg.
  LOOP AT it_output.

    READ TABLE it_final WITH KEY kunnr = it_output-kunnr
                                 cty   = it_output-cty
                                 model = it_output-model
                                 issu  = it_output-issu.
    IF sy-ucomm = 'POST_UNFLA'.
*      IF it_final-msg <> text-001.
      IF it_final-msg CP '*Posted*'.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSEIF sy-ucomm = 'UNDELE'.
      IF it_final-msg <> text-005.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF sy-subrc = 0.

      it_final-msg = w_msg.
      MODIFY it_final TRANSPORTING msg
                                  WHERE kunnr = it_output-kunnr AND
                                        cty   = it_output-cty   AND
                                        model = it_output-model AND
                                        issu  = it_output-issu.
*      IF sy-ucomm = 'POST_FLAG' OR sy-ucomm = 'DELE'.
      DELETE it_final WHERE kunnr = it_output-kunnr AND
                            cty   = it_output-cty   AND
                            model = it_output-model AND
                            issu  = it_output-issu.
*      ENDIF.
    ENDIF.
    it_data-type       = fmode.
    it_data-message    = w_msg.

    MODIFY it_data  TRANSPORTING type message
                                 WHERE vndr = it_output-kunnr AND
                                       issu = it_output-issu  AND
                                       natn = it_output-natn.
  ENDLOOP.

  LOOP AT it_data.

    IF it_data-vndr EQ 'YONG'.
      it_data-vndr = 'SEF9'.

      CLEAR *ztfi_reclaim_dat.
      SELECT * FROM ztfi_reclaim_dat
             WHERE  corp EQ it_data-corp
               AND  doex EQ it_data-doex
               AND  vndr EQ it_data-vndr
               AND  issu EQ it_data-issu
               AND  vdgb EQ it_data-vdgb
               AND  natn EQ it_data-natn
               AND  carc EQ it_data-carc.

        IF  ztfi_reclaim_dat-opcd CP '80BA16*' OR
            ztfi_reclaim_dat-opcd CP '90BA03*'.
           *ztfi_reclaim_dat = ztfi_reclaim_dat.
           *ztfi_reclaim_dat-type = it_data-type.
           *ztfi_reclaim_dat-message = it_data-message.
          ztfi_reclaim_dat = *ztfi_reclaim_dat.
          UPDATE ztfi_reclaim_dat.
        ENDIF.
      ENDSELECT.

    ELSEIF it_data-vndr EQ 'SEF9'.

      CLEAR *ztfi_reclaim_dat.
      SELECT * FROM ztfi_reclaim_dat
             WHERE  corp EQ it_data-corp
               AND  doex EQ it_data-doex
               AND  vndr EQ it_data-vndr
               AND  issu EQ it_data-issu
               AND  vdgb EQ it_data-vdgb
               AND  natn EQ it_data-natn
               AND  carc EQ it_data-carc.

        IF  ztfi_reclaim_dat-opcd CP '80BA16*' OR
            ztfi_reclaim_dat-opcd CP '90BA03*'.
        ELSE.
           *ztfi_reclaim_dat = ztfi_reclaim_dat.
           *ztfi_reclaim_dat-type = it_data-type.
           *ztfi_reclaim_dat-message = it_data-message.
          ztfi_reclaim_dat = *ztfi_reclaim_dat.
          UPDATE ztfi_reclaim_dat.
        ENDIF.
      ENDSELECT.

    ELSE.
      UPDATE ztfi_reclaim_dat SET type    = it_data-type
                                  message = it_data-message
                                  WHERE corp EQ it_data-corp AND
                                        doex EQ it_data-doex AND
                                        vndr EQ it_data-vndr AND
                                        issu EQ it_data-issu AND
                                        vdgb EQ it_data-vdgb AND
                                        natn EQ it_data-natn AND
                                        carc EQ it_data-carc.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " update_table
*===================================================================*
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_object_0100 .

  create object g_c_container1
    exporting
*      parent                      =
      container_name              = 'G_C_CONTAINER1'
*      style                       =
*      lifetime                    = lifetime_default
*      repid                       =
*      dynnr                       =
*      no_autodef_progid_dynnr     =
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      others                      = 6
      .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  create object g_grid1
    exporting
*      I_SHELLSTYLE      = 0
*      I_LIFETIME        =
      i_parent          = g_c_container1
      i_appl_events     = 'X'
*      I_PARENTDBG       =
*      I_APPLOGPARENT    =
*      I_GRAPHICSPARENT  =
*      I_NAME            =
*      I_FCAT_COMPLETE   = SPACE
    exceptions
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      others            = 5
      .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CREATE_OBJECT_0100
*&---------------------------------------------------------------------*
*&      Form  GET_GRID_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT_LVC  text
*      -->P_0029   text
*----------------------------------------------------------------------*
form get_grid_fieldcat  using pt_fieldcat  type lvc_t_fcat
                              pv_grid_stru type any.

  data:    lt_fieldcat  type slis_t_fieldcat_alv,
           lv_fieldcat  type lvc_t_fcat,
           ls_fieldcat  type lvc_s_fcat,
           lz_fieldcat  type lvc_s_fcat.



* 1) Get Tree Field Catalog : ??? ??? Layout? ??
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_internal_tabname     = pv_grid_stru
      i_inclname             = sy-repid
    changing
      ct_fieldcat            = lt_fieldcat[]
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.
  if sy-subrc = 0.
    perform transfer_slis_to_lvc changing lt_fieldcat pt_fieldcat[].
  endif.



*DATA:    text1 LIKE ZCMS1901_RESULT-ST_YEART,
*         text2 like ZCMS1901_RESULT-PROG_RESULTST,
  clear ls_fieldcat.

  loop at pt_fieldcat into ls_fieldcat.

    case ls_fieldcat-fieldname.
      when 'CTY'.                "Cntry
        ls_fieldcat-scrtext_l  = 'Cntry'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 4.
        ls_fieldcat-col_pos    = 1.
        ls_fieldcat-key        = 'X'.

      when 'ISSU'.              "Issue#
        ls_fieldcat-scrtext_l  = 'Issue#'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 10.
        ls_fieldcat-col_pos    = 2.
        ls_fieldcat-key        = 'X'.
*        ls_fieldcat-edit       = 'X'.
        ls_fieldcat-key        = 'X'.

      when 'NATN'.              "Dest
        ls_fieldcat-scrtext_l  = 'Dest'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 5.
        ls_fieldcat-col_pos    = 3.
*        ls_fieldcat-edit       = 'X'.
        ls_fieldcat-key        = 'X'.

      when 'KUNNR'.              "Vndr.
        ls_fieldcat-scrtext_l  = 'Vendor.'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 12.
        ls_fieldcat-col_pos    = 4.
        ls_fieldcat-key        = 'X'.

      when 'MODEL'.              "Model
        ls_fieldcat-scrtext_l  = 'Model'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 8.
        ls_fieldcat-col_pos    = 5.
        ls_fieldcat-key        = 'X'.

      when 'RC_AMT'.             "Reclaim Amt.
        ls_fieldcat-scrtext_l  = 'Reclaim Amt.'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 6.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.
        ls_fieldcat-edit       = 'X'.

      when 'BJPT'.               "Material $
        ls_fieldcat-scrtext_l  = 'Material $'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 7.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.
*        ls_fieldcat-do_sum     = 'X'.

      when 'BJLA'.               "Labor $
        ls_fieldcat-scrtext_l  = 'Labor $'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 8.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'BJMI'.               "OutSource $
        ls_fieldcat-scrtext_l  = 'OutSource $'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 9.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'SUB_TOT1'.           "Claim Sub Total
        ls_fieldcat-scrtext_l  = 'Claim Sub Total'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 10.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'OJPT'.               "NC Material $
        ls_fieldcat-scrtext_l  = 'NC Material $'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 11.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'OJLA'.               "NC Labor $
        ls_fieldcat-scrtext_l  = 'NC Labor $'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 12.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'OJMI'.               "NC OutSource $
        ls_fieldcat-scrtext_l  = 'NC OutSource $'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 13.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'SUB_TOT2'.           "NC Sub Total
        ls_fieldcat-scrtext_l  = 'NC Sub Total'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 16.
        ls_fieldcat-col_pos    = 14.
        ls_fieldcat-just       = 'R'.
        ls_fieldcat-cfieldname = 'WAERS'.

      when 'MSG'.                "Message
        ls_fieldcat-scrtext_l  = 'Message'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 50.
        ls_fieldcat-col_pos    = 15.
        ls_fieldcat-emphasize  = 'C300'.
*        ls_fieldcat-style      = '00000010'.

***      when 'RC_AMT'.             "Reclaim Amt.
***        ls_fieldcat-scrtext_l  = 'Reclaim Amt.'.
***        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
***        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
***        ls_fieldcat-outputlen  = 14.
***        ls_fieldcat-col_pos    = 15.
***        ls_fieldcat-just       = 'R'.
***        ls_fieldcat-cfieldname = 'WAERS'.

      when 'WAERS'.              "Currency
        ls_fieldcat-scrtext_l  = 'Curr'.
        ls_fieldcat-reptext    = ls_fieldcat-scrtext_s =
        ls_fieldcat-scrtext_m  = ls_fieldcat-scrtext_l.
        ls_fieldcat-outputlen  = 05.
        ls_fieldcat-col_pos    = 16.
        ls_fieldcat-no_out     = 'X'.

      when others.
        ls_fieldcat-no_out = 'X'.
    endcase.

    modify pt_fieldcat from ls_fieldcat.
    clear: ls_fieldcat.

  endloop.

endform.                    " GET_GRID_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_SLIS_TO_LVC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_FIELDCAT  text
*      <--P_PT_FIELDCAT[]  text
*----------------------------------------------------------------------*
form transfer_slis_to_lvc  changing pt_field    type slis_t_fieldcat_alv
                       pt_fieldcat type lvc_t_fcat.

  data : lt_fieldcat    type kkblo_t_fieldcat.
*
  call function 'REUSE_ALV_TRANSFER_DATA'
    exporting
      it_fieldcat = pt_field
    importing
      et_fieldcat = lt_fieldcat.
*
  call function 'LVC_TRANSFER_FROM_KKBLO'
    exporting
      it_fieldcat_kkblo = lt_fieldcat
    importing
      et_fieldcat_lvc   = pt_fieldcat.

endform.                    " TRANSFER_SLIS_TO_LVC
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_TB_EXCLUDE[]  text
*----------------------------------------------------------------------*
form exclude_tb_functions1  using   pt_exclude type ui_functions.

  clear pt_exclude[].
  clear gs_tb_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_check.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_auf.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_average.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_back_classic.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_abc.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_chain.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_crbatch.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_crweb.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_call_line_ITEMs.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_master_data.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_more.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_call_report.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_call_xint.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_call_xxl.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_col_invisible.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_col_optimize.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_current_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_data_save.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_delete_filter.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_deselect_all.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_detail.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
*  -------------------------"[??? ?? ?? ?? ?? ???]
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_excl_all.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
*------------------------------------------------------
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expcrdata.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expcrdesig.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expcrtempl.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_expmdb.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_extend.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_f4.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_filter.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_find.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_fix_columns.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_graph.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_help.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_info.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_load_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_loc_copy.
* APPEND GS_TB_EXCLUDE TO pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_html.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  append gs_tb_exclude to pt_exclude.
*  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
*  APPEND gs_tb_exclude TO pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_maintain_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_maximum.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_minimum.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_print.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
*  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_print_back.
*  APPEND GS_TB_EXCLUDE TO pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_print_prev.
  append gs_tb_exclude to pt_exclude.
*  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_refresh.
*  APPEND gs_tb_exclude TO pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_reprep.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_save_variant.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_select_all.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_send.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_separator.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sort.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sort_asc.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sort_dsc.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_subtot.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_sum.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_to_office.
  append gs_tb_exclude to pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_to_rep_tree.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_unfix_columns.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_views.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_view_crystal.
  append gs_tb_exclude to pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_view_excel.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
* GS_TB_EXCLUDE = cl_gui_alv_grid=>mc_fc_view_grid.
* APPEND GS_TB_EXCLUDE TO Pt_exclude.
  gs_tb_exclude = cl_gui_alv_grid=>mc_fc_word_processor.
  append gs_tb_exclude to pt_exclude.


endform.                    " EXCLUDE_TB_FUNCTIONS1
*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
form handle_toolbar  using  pr_object type ref
                                          to cl_alv_event_toolbar_set
                            p_interactive type  char01.
*  check g_status  = c_change.

  data: ls_toolbar  type stb_button.

*  clear ls_toolbar.
*  ls_toolbar-function   = 'INS_ROW'.
*  ls_toolbar-butn_type  = 0.
*  ls_toolbar-icon       = icon_insert_row.
*  ls_toolbar-quickinfo  =
*  ls_toolbar-text       =  '??'.
*  append ls_toolbar to pr_object->mt_toolbar.
*
*  clear ls_toolbar.
*  ls_toolbar-function   = 'DEL_ROW'.
*  ls_toolbar-butn_type  = 0.
*  ls_toolbar-icon       = icon_delete_row.
*  ls_toolbar-quickinfo  =
*  ls_toolbar-text       = '??'.
*  append ls_toolbar to pr_object->mt_toolbar.

*  CLEAR ls_toolbar.
*  MOVE 3 TO ls_toolbar-butn_type.
*  APPEND ls_toolbar TO pr_object->mt_toolbar.

endform.                    " HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
form handle_user_command  using    p_ucomm.

*  case p_ucomm.
*    when 'INS_ROW'.
*      perform insert_row.
*
*    when 'DEL_ROW'.
*      perform delete_row.
*
*  endcase.

endform.                    " HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*      -->P_E_ONF4  text
*      -->P_E_ONF4_BEFORE  text
*      -->P_E_ONF4_AFTER  text
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
form handle_data_changed  using   rr_data_changed  type ref to
                                        cl_alv_changed_data_protocol
                            p_onf4 p_onf4_before p_onf4_after p_ucomm.

  data : ls_mod_cells type lvc_s_modi,
         ls_cells     type lvc_s_modi,
         l_hkont      like bsis-hkont.

  data: lt_modi_cell  like table of gs_modi_cell,
        ls_modi_cell  like gs_modi_cell.
  clear : gs_modi_cell.

*  loop at rr_data_changed->mt_good_cells into gs_modi_cell.
*    ls_modi_cell-row_id = gs_modi_cell-row_id.
*
*    case gs_modi_cell-fieldname.
*      when  'ZGL_ACCT'.  "??? ????? ?
*
*        clear: it_skat, l_hkont.
** G/L ALPHA Conversion
*        l_hkont = gs_modi_cell-value.
*        call function 'CONVERSION_EXIT_ALPHA_INPUT'
*          exporting
*            input  = l_hkont
*          importing
*            output = l_hkont
*          exceptions
*            others = 1.
*        read table it_skat with key  saknr  = l_hkont.
*        ls_modi_cell-fieldname  = 'ZGL_ACCT'.
*        ls_modi_cell-value      = gs_modi_cell-value.
*        append ls_modi_cell to lt_modi_cell.
*
*        ls_modi_cell-fieldname  = 'ZGL_ACCTT'.
*        ls_modi_cell-value      = it_skat-txt20.
*        append ls_modi_cell to lt_modi_cell.
*
*      when  'ZKOSTL'.  "CCTR? ????? ?
*
*        clear it_cskt.
*        read table it_cskt with key  kostl  = gs_modi_cell-value.
*        ls_modi_cell-fieldname  = 'ZKOSTL'.
*        ls_modi_cell-value      = gs_modi_cell-value.
*        append ls_modi_cell to lt_modi_cell.
*
*        ls_modi_cell-fieldname  = 'ZKOSTLT'.
*        ls_modi_cell-value      = it_cskt-ktext.
*        append ls_modi_cell to lt_modi_cell.
*
*      when  'ZFUND'.  "??? ????? ?
*
*        clear it_fmfint.
*        read table it_fmfint with key  fincode  = gs_modi_cell-value.
*        ls_modi_cell-fieldname  = 'ZFUND'.
*        ls_modi_cell-value      = gs_modi_cell-value.
*        append ls_modi_cell to lt_modi_cell.
*
*        ls_modi_cell-fieldname  = 'ZFUNDT'.   "???
*        ls_modi_cell-value      = it_fmfint-bezeich.
*        append ls_modi_cell to lt_modi_cell.
*
*      when  'ZFUND_CENTER'.  "????? ????? ?
*
*        clear it_fmfctrt.
*        read table it_fmfctrt with key  fictr  = gs_modi_cell-value.
*        ls_modi_cell-fieldname  = 'ZFUND_CENTER'.
*        ls_modi_cell-value      = gs_modi_cell-value.
*        append ls_modi_cell to lt_modi_cell.
*
*        ls_modi_cell-fieldname  = 'ZFUND_CENTERT'.   "???? ???
*        ls_modi_cell-value      = it_fmfctrt-bezeich.
*        append ls_modi_cell to lt_modi_cell.


*      when others.
*
*    endcase.

*  endloop.

  loop at lt_modi_cell  into ls_modi_cell.
    call method rr_data_changed->modify_cell
      exporting
        i_row_id    = ls_modi_cell-row_id
*              i_tabix     =
        i_fieldname = ls_modi_cell-fieldname
        i_value     = ls_modi_cell-value
        .
  endloop.

  perform refresh_grid  using g_grid1 'X'.

endform.                    " HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  HANDLE_DATA_CHANGED_FIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_MODIFIED  text
*      -->P_ET_GOOD_CELLS  text
*----------------------------------------------------------------------*
form handle_data_changed_fin  using    p_modified type char01
                                       pt_good_cells  type lvc_t_modi.
  check p_modified  is not initial.

  field-symbols:  <ls_tab> like gt_grid.

  clear : gs_modi_cell.

  loop at pt_good_cells into gs_modi_cell.
    read table gt_grid index gs_modi_cell-row_id assigning <ls_tab>.
    if sy-subrc = 0.
      <ls_tab>-changed = 'X'.
    endif.

    g_changed = 'X'.

  endloop.


  perform refresh_grid  using g_grid1 'X'.

endform.                    " HANDLE_DATA_CHANGED_FIN
*&---------------------------------------------------------------------*
*&      Form  HANDLE_F4_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SENDER  text
*      -->P_E_FIELDNAME  text
*      -->P_E_FIELDVALUE  text
*      -->P_ES_ROW_NO  text
*      -->P_ER_EVENT_DATA  text
*      -->P_ET_BAD_CELLS  text
*      -->P_E_DISPLAY  text
*----------------------------------------------------------------------*
form handle_f4_grid  using  sender   type ref to cl_gui_alv_grid
                            p_fieldname   type lvc_fname
                            p_fieldvalue  type lvc_value
                            p_row_no      type lvc_s_roid
                            p_event_data  type ref to cl_alv_event_data
                            p_bad_cells   type lvc_t_modi
                            p_display     type char01.

  data:    ls_modi        type lvc_s_modi,
           ls_grid        like gt_grid.
  data:    lt_return      type ddshretval occurs 0 with header line.

  data:    lv_value       like help_info-fldvalue.

  field-symbols: <lt_f4>  type lvc_t_modi.

* ? GRID DATA ??
  clear gt_grid.
  read table gt_grid index p_row_no-row_id.

  check sy-subrc eq 0.
* ? GRID ?? ??? ??
  assign p_event_data->m_data->* to <lt_f4>.

*//
* ? FIELD ? SEARCH HELP ??.
*  case p_fieldname.
*    when 'ZGL_ACCT'.
*      perform onf4_zprosgbn   using lv_value
*                                    p_fieldname.
*      if not lv_value is initial.
*        clear ls_modi.
*        ls_modi-fieldname = p_fieldname.
*        ls_modi-row_id    = p_row_no-row_id.
*        ls_modi-value     = lv_value.
*        append ls_modi to <lt_f4>.
*        p_event_data->m_event_handled = 'X'.
*      endif.
*    when 'ZKOSTL'.
*      perform onf4_zprosgbn   using lv_value
*                                   p_fieldname.
*      if not lv_value is initial.
*        clear ls_modi.
*        ls_modi-fieldname = p_fieldname.
*        ls_modi-row_id    = p_row_no-row_id.
*        ls_modi-value     = lv_value.
*        append ls_modi to <lt_f4>.
*        p_event_data->m_event_handled = 'X'.
*      endif.
*
*  endcase.
*//

*  PERFORM refresh_grid  USING g_grid1 'X'.


endform.                    " HANDLE_F4_GRID
*&---------------------------------------------------------------------*
*&      Form  ONF4_ZPROSGBN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_VALUE  text
*      -->P_P_FIELDNAME  text
*----------------------------------------------------------------------*
form onf4_zprosgbn  using pv_value type any
                           pv_fieldname.

  data:    lt_fields    like help_value occurs 0 with header line.
  clear:   pv_value.

***  data:    lv_selectfield like help_info-fieldname,
***           lv_selectvalue like  help_info-fldvalue,
***           lv_idx       like sy-tabix.
***
***  data : begin of lt_stu occurs 0,
***         key      like zcmt4013-peryr,
***         text     like zcms1901_result-st_yeart,
***         end   of lt_stu.
***
***  data : begin of lt_stu1 occurs 0,
***         key      like zcmt4013-perid,
***         text     like zcms1901_result-prog_resultst,
***         end   of lt_stu1.

  data: return_values like ddshretval occurs 0 with header line.

  case pv_fieldname.
*    when 'ZGL_ACCT'.
*      call function 'F4IF_FIELD_VALUE_REQUEST'
*        exporting
*          tabname    = 'ZDFT301'
*          fieldname  = 'ZGL_ACCT'
*          searchhelp = 'ZDF_SH_HKONT'   "SEARCHHELP
*        tables
*          return_tab = return_values
*        exceptions
*          others     = 5.
*      pv_value = return_values-fieldval.
*
*    when 'ZKOSTL'.
*      call function 'F4IF_FIELD_VALUE_REQUEST'
*        exporting
*          tabname    = 'CSKS'
*          fieldname  = 'KOSTL'
*          searchhelp = 'TRAC_KOSTL'   "SEARCHHELP
*        tables
*          return_tab = return_values
*        exceptions
*          others     = 5.
*      pv_value = return_values-fieldval.
*
*    when 'ZFUND'.
*      call function 'F4IF_FIELD_VALUE_REQUEST'
*        exporting
*          tabname    = 'CSKS'
*          fieldname  = 'ZFUND'
*          searchhelp = 'TRAC_KOSTL'   "SEARCHHELP
*        tables
*          return_tab = return_values
*        exceptions
*          others     = 5.
*      pv_value = return_values-fieldval.


  endcase.

endform.                    " ONF4_ZPROSGBN
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_layout_grid1 .

  clear: gs_layout_lvc, gs_variant.

  gs_layout_lvc-sel_mode  = 'A'.
  gs_layout_lvc-box_fname = 'MARK' .
* Edit
  gs_layout_lvc-edit           = space.
  gs_layout_lvc-edit_mode      = space.
  gs_layout_lvc-zebra     = 'X'.

*  gs_layout_lvc-cwidth_opt  = 'X'.

  gs_variant-report = sy-repid.
  gs_variant-handle = 1.
  gs_variant-username = sy-uname.
*  gs_variant-variant = p_layout.

endform.                    " SET_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*&      Form  SETTING_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form setting_dropdown .

* dropdown
  refresh gt_dropdowna. clear gs_dropdowna.
*  REFRESH:  gt_text.

*  PERFORM get_com_code_tab  TABLES gt_text_ser_tp
*                            USING c_grp_ser_tp.


*//  LOOP AT GT_TEXT.
*    gs_dropdowna-handle     = 1.
*    gs_dropdowna-int_value  = GT_TEXT-COM_CD.
*    gs_dropdowna-value      = GT_TEXT-COM_NM.
*    APPEND gs_dropdowna TO gt_dropdowna.
*    CLEAR gs_dropdowna.
*  ENDLOOP.
*
*
*  IF gt_dropdowna  IS INITIAL.
*    gs_dropdowna-handle = 1.
*    APPEND gs_dropdowna TO gt_dropdowna.
*    CLEAR gs_dropdowna.
*//  ENDIF.


  call method g_grid1->set_drop_down_table
    exporting
*      it_drop_down = gt_dropdown.
      it_drop_down_alias  = gt_dropdowna.

endform.                    " SETTING_DROPDOWN
*&---------------------------------------------------------------------*
*&      Form  CHANGE_PART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RR_DATA_CHANGED  text
*      -->P_LS_MOD_CELLS  text
*      -->P_0590   text
*----------------------------------------------------------------------*
form change_part  using    rr_data_changed type ref to
                           cl_alv_changed_data_protocol
                           rs_mod_cells type lvc_s_modi
                           p_name.

  data : l_stat(20)  type c,
         l_value type lvc_value,
         l_txt(10).

*  data : lv_amount type bseg-wrbtr,
*         lv_degre type zpst1012-degre.

*  case p_name.
*    when 'ZGL_ACCT'.
*      read table gt_grid index rs_mod_cells-row_id.
*      read table it_skat with key saknr = gt_grid-zgl_acct.
*      gt_grid-zgl_acct  = it_skat-saknr.
*      gt_grid-zgl_acctt = it_skat-txt20.
*      modify gt_grid index rs_mod_cells-row_id
*             transporting zgl_acct zgl_acctt .
*
*  endcase.

endform.                    " CHANGE_PART
*&---------------------------------------------------------------------*
*&      Form  INSERT_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_row .

  data: l_row type i,
        lv_lines   type  i,
        lv_buzei   type  buzei.

  field-symbols:  <lt_tab>  type table,
                  <ls_tab>.

  assign gt_grid[] to <lt_tab>.
  check <lt_tab> is assigned.


* ??(default? ???) ??
*  l_row = LINES( <lt_tab> ).
*  ADD 1 TO l_row.
  l_row = 1.

  describe table gt_grid lines lv_lines.
  lv_buzei = 1 + lv_lines.
*  INSERT INITIAL LINE INTO  <lt_tab> INDEX l_row.
*  READ TABLE <lt_tab> INDEX l_row ASSIGNING <ls_tab>.
*
***  insert initial line into gt_grid index l_row.
***  gt_grid-bukrs = p_bukrs.
***  gt_grid-buzei = lv_buzei.
***  gt_grid-gjahr = p_gjahr.
***  gt_grid-waers = gc_waers.
***  gt_grid-twaer = gt_hd-twaer.
***  gt_grid-hkont = '0000112200'.    "???
***  gt_grid-txt20 = '???'.
***  gt_grid-blart = gt_hd-blart.
***  gt_grid-bldat = sy-datum.
***  gt_grid-budat = sy-datum.
***  gt_grid-bschl = '31'.
***  gt_grid-kokrs = p_bukrs.        "??????.
***
***  perform find_hkont_text  using    gt_grid-hkont
***                           changing gt_grid-txt20.
***
***
***  modify gt_grid index l_row transporting bukrs gjahr waers
***                 bldat budat bschl twaer hkont blart txt20
***                 buzei kokrs.


* refresh grid
  perform refresh_grid  using g_grid1  'X'.


* ??? ?? ??? ????? ?
  perform set_current_cell  using g_grid1 l_row.

endform.                    " INSERT_ROW
*&---------------------------------------------------------------------*
*&      Form  DELETE_ROW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_row .

  data: lv_tabix   like  sy-tabix.
*  clear: gt_save, gt_save[], lv_tabix.

  field-symbols:  <lt_tab>  type table,
                  <ls_tab>  like gt_grid.

  perform get_select_row_grid using g_grid1.

  check gt_sels[] is not initial.

  assign gt_grid[] to <lt_tab>.
  check <lt_tab> is assigned.

  loop at gt_sels into gs_sels.
    read table <lt_tab> index gs_sels-index assigning <ls_tab> casting.
    check sy-subrc = 0.

    gt_grid-mark = 'X'.
    modify gt_grid index gs_sels-index transporting mark.
*
  endloop.

*  loop at gt_grid where mark = 'X'.
*    lv_tabix = sy-tabix.
*    move-corresponding gt_grid to gt_save.
*    check gt_save-belnr is initial.  "
*    append gt_save.
*    clear: gt_save.
*    delete  gt_grid index lv_tabix.
*
*  endloop.

*  delete zfit4004  from table gt_save.  "delete
*  clear: gt_save, gt_save[].

  perform refresh_grid  using g_grid1 'X'.

endform.                    " DELETE_ROW
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
form double_click  using    p_row
                            p_column
                            p_row_no.

  check p_row is not initial.
*  clear: wa_list.
*  read table gt_grid index p_row  into wa_list.
*  check sy-subrc = 0.

  case p_column.
    when 'BELNR'.          "?? ????.
*      check wa_list-belnr is not initial.
*      set parameter id 'BUK' field p_bukrs.
*      set parameter id 'BLN' field wa_list-belnr.
*      set parameter id 'GJR' field p_gjahr.
*
*      call transaction 'FB03' and skip first screen.

*    when 'BELN2'.         "?? ????
*      check wa_list-beln2 is not initial.
*      set parameter id 'BUK' field p_bukrs.
*      set parameter id 'BLN' field wa_list-beln2.
*      set parameter id 'GJR' field p_gjahr.
*
*      call transaction 'FB03' and skip first screen.
  endcase.

endform.                    " DOUBLE_CLICK
