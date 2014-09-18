*&---------------------------------------------------------------------*
*&  Include           ZIPP_PT_WSTOCK_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GET_VEHICLE_MODEL_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_PGMI  text
*----------------------------------------------------------------------*
FORM get_vehicle_model_code  TABLES pt_pgmi  STRUCTURE it_pgmi.

  DATA :
    l_datub      TYPE rc29l-datub,      "Date 'valid to'
    lt_matcat    TYPE TABLE OF cscmat  WITH HEADER LINE.


  CLEAR : pt_pgmi.
  LOOP AT pt_pgmi.

    REFRESH : lt_matcat[].

    l_datub = sy-datum.         "Date 'valid to'
    PERFORM call_bom_function TABLES pt_pgmi
                                     lt_matcat
                               USING l_datub.

    IF lt_matcat[] IS INITIAL.
*     "try once again
      l_datub = '99991231'.     "Date 'valid to'
      PERFORM call_bom_function TABLES pt_pgmi
                                       lt_matcat
                                 USING l_datub.
    ENDIF.

    IF NOT lt_matcat[] IS INITIAL.
      SORT lt_matcat BY matnr  DESCENDING.

      CLEAR : lt_matcat.
      READ TABLE lt_matcat  INDEX 1.  "the first data
      IF sy-subrc = 0.
        pt_pgmi-zmo01 = lt_matcat-matnr+5(2).   "model code (char 2)

        MODIFY pt_pgmi TRANSPORTING zmo01.
      ENDIF.
    ENDIF.

    CLEAR : pt_pgmi.
  ENDLOOP.

ENDFORM.                    " GET_VEHICLE_MODEL_CODE

*&---------------------------------------------------------------------*
*&      Form  CALL_BOM_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_PGMI  text
*      -->PT_MATCAT  text
*----------------------------------------------------------------------*
FORM call_bom_function    TABLES pt_pgmi     STRUCTURE it_pgmi
                                 pt_matcat   STRUCTURE cscmat
                           USING p_datub     TYPE      rc29l-datub.

  DATA :
    l_datuv      TYPE rc29l-datuv,      "Date 'valid from'
    lt_wultb     TYPE TABLE OF stpov    WITH HEADER LINE,
    lt_equicat   TYPE TABLE OF cscequi  WITH HEADER LINE,
    lt_kndcat    TYPE TABLE OF cscknd   WITH HEADER LINE,
    lt_matcat    TYPE TABLE OF cscmat   WITH HEADER LINE,
    lt_stdcat    TYPE TABLE OF cscstd   WITH HEADER LINE,
    lt_tplcat    TYPE TABLE OF csctpl   WITH HEADER LINE.


* "initialize result
  CLEAR   : pt_matcat.
  REFRESH : pt_matcat[].


  CLEAR : l_datuv.        "Date 'valid from'
  l_datuv = sy-datum.

  CALL FUNCTION 'CS_WHERE_USED_MAT'
    EXPORTING
*    "datub : Date 'valid to'
      datub                      = p_datub    "if no data '99991231'
*    "datuv : Date 'valid from'
      datuv                      = l_datuv
      matnr                      = pt_pgmi-nrmit
      stlan                      = '1'        "BOM usage
      werks                      = pt_pgmi-werks
    TABLES
      wultb                      = lt_wultb
      equicat                    = lt_equicat
      kndcat                     = lt_kndcat
      matcat                     = lt_matcat
      stdcat                     = lt_stdcat
      tplcat                     = lt_tplcat
    EXCEPTIONS
      call_invalid               = 1
      material_not_found         = 2
      no_where_used_rec_found    = 3
      no_where_used_rec_selected = 4
      no_where_used_rec_valid    = 5
      OTHERS                     = 6.

  IF NOT lt_matcat[] IS INITIAL.
    pt_matcat[] = lt_matcat[].
  ENDIF.

ENDFORM.                    " CALL_BOM_FUNCTION

*&---------------------------------------------------------------------*
*&      Form  GET_BILL_OF_LADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_PGMI  text
*      -->PT_BL  text
*----------------------------------------------------------------------*
FORM get_bill_of_lading  TABLES pt_pgmi         STRUCTURE it_pgmi
                                pt_bl           STRUCTURE it_bl
                          USING p_inputted_date TYPE      sy-datum.

  RANGES : lt_zfetd FOR sy-datum.

* "initialize result
  CLEAR   : pt_bl.
  REFRESH : pt_bl[].


  IF NOT pt_pgmi[] IS INITIAL.

    CLEAR   : lt_zfetd.
    REFRESH : lt_zfetd[].
    lt_zfetd-sign   = 'I'.
    lt_zfetd-option = 'BT'.
    CONCATENATE p_inputted_date+0(6) '01'
                INTO lt_zfetd-low.            "the first day
    lt_zfetd-high   = p_inputted_date.        "inputted date
    APPEND lt_zfetd.

*   "B/L (Bill of Lading)
    SELECT *
      FROM ztbl   AS a INNER JOIN
           ztblit AS b    ON a~zfblno = b~zfblno
      INTO CORRESPONDING FIELDS OF TABLE pt_bl
       FOR ALL ENTRIES IN pt_pgmi
     WHERE ( a~lifnr = c_sbc3 OR      "Hyundai Motor Company
             a~lifnr = c_h27z    )    "WIA Auto.Engine(Shandong)Co
       AND a~zfetd IN lt_zfetd        "(01 ~ inputted date)
       AND b~matnr = pt_pgmi-nrmit.   "matnr
  ENDIF.

  SORT : pt_bl   BY zfblno zfblit,
         pt_pgmi BY nrmit.      "matnr

  CLEAR : pt_bl.
  LOOP AT pt_bl.

    CLEAR : pt_pgmi.
    READ TABLE pt_pgmi WITH KEY nrmit = pt_bl-matnr BINARY SEARCH.
    IF sy-subrc = 0.
      pt_bl-prgrp = pt_pgmi-prgrp.
      pt_bl-zmo01 = pt_pgmi-zmo01.

      MODIFY pt_bl  TRANSPORTING prgrp          "product group
                                 zmo01.         "model code(char 2)
    ENDIF.

    CLEAR : pt_bl.
  ENDLOOP.

ENDFORM.                    " GET_BILL_OF_LADING


*&---------------------------------------------------------------------*
*&      Form  GET_QTY_SHIP_N_TRANSIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_qty_ship_n_transit   USING p_inputted_date TYPE sy-datum.

* "initialize result
  CLEAR   : it_wstock_qty.
  REFRESH : it_wstock_qty[].    "Shipping Qty +  In-Transit Qty


  CLEAR   : it_pgmi.
  REFRESH : it_pgmi[].
  SELECT *
    FROM pgmi
    INTO CORRESPONDING FIELDS OF TABLE it_pgmi
   WHERE pgtyp = ''
     AND ( prgrp = c_kd_eng OR  "ENGINE ASSY - CKD
           prgrp = c_kd_tm    ) "CKD TRANSMISSION
     AND werks = 'P001'.        "Hyundai Car Assembly, AL

* "model code(char 2)
  PERFORM get_vehicle_model_code  TABLES it_pgmi.

* "YYYYMMDDhhmmss
  CLEAR : g_date_time.
  CONCATENATE sy-datum sy-uzeit INTO g_date_time.

* [Shipping Qty +  In-Transit Qty]
* Shipping Qty
  PERFORM get_shipping_qty    TABLES it_wstock_qty
                               USING p_inputted_date.
* In-Transit Qty
  PERFORM get_in_transit_qty  TABLES it_wstock_qty
                               USING p_inputted_date.


ENDFORM.                    " GET_QTY_SHIP_N_TRANSIT

*&---------------------------------------------------------------------*
*&      Form  GET_CURRENT_INTRANSIT_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_PGMI  text
*      -->PT_CURR_TRANSIT_QTY  text
*----------------------------------------------------------------------*
FORM get_current_intransit_qty
                 TABLES pt_pgmi              STRUCTURE it_pgmi
                        pt_curr_transit_qty  STRUCTURE it_curr_transit.

  DATA : l_lfdat TYPE lfdat,
         l_days  TYPE i.

* "initialize result
  CLEAR   : pt_curr_transit_qty.
  REFRESH : pt_curr_transit_qty[].


  IF NOT pt_pgmi[] IS INITIAL.

    CLEAR : l_lfdat, l_days.
    l_days  = 700.              "(about) number of days for two years
    l_lfdat = sy-datum - l_days.

    SELECT a~kunde  a~parvw  a~lfdat  "Vendor, Delivery date
           b~wbstk  b~vbtyp
           c~wbsta
           e~vbeln  e~posnr           "Delivery
           e~matnr  e~lfimg  e~meins  "material, Qty
      FROM vlkpa AS a
           INNER JOIN  vbuk AS b   ON a~vbeln = b~vbeln
           INNER JOIN  vbup AS c   ON b~vbeln = c~vbeln
           INNER JOIN  likp AS d   ON b~vbeln = d~vbeln
           INNER JOIN  lips AS e   ON d~vbeln = e~vbeln AND
                                      c~posnr = e~posnr
      INTO CORRESPONDING FIELDS OF TABLE pt_curr_transit_qty
       FOR ALL ENTRIES IN pt_pgmi
     WHERE ( a~kunde =  c_sbc3 OR     "Hyundai Motor Company
             a~kunde =  c_h27z    )   "WIA Auto.Engine(Shandong)Co
       AND a~parvw   =  'LF'          "(LF <-- 'VN' Vendor)
       AND a~lfdat   >= l_lfdat
       AND ( b~wbstk =  'A' OR  b~wbstk = 'B' )
       AND b~vbtyp   =  '7'           "Delivery/shipping notification
       AND ( c~wbsta =  'A' OR  c~wbsta = 'B' )
       AND e~matnr   = pt_pgmi-nrmit. "matnr

    SORT pt_curr_transit_qty  BY vbeln posnr.   "Delivery

  ENDIF.

* for Binary Search
  SORT : pt_pgmi BY nrmit."matnr

  CLEAR : pt_curr_transit_qty.
  LOOP AT pt_curr_transit_qty.

    CLEAR : pt_pgmi.
    READ TABLE pt_pgmi WITH KEY nrmit = pt_curr_transit_qty-matnr
                                BINARY SEARCH.
    IF sy-subrc = 0.
      pt_curr_transit_qty-prgrp = pt_pgmi-prgrp.
      pt_curr_transit_qty-zmo01 = pt_pgmi-zmo01.

      MODIFY pt_curr_transit_qty TRANSPORTING prgrp "product group
                                              zmo01."model code(char2)
    ENDIF.

    CLEAR : pt_curr_transit_qty.
  ENDLOOP.


ENDFORM.                    " GET_CURRENT_INTRANSIT_QTY


*&---------------------------------------------------------------------*
*&      Form  GET_PAST_INTRANSIT_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_PAST_TRANSIT_QTY  text
*----------------------------------------------------------------------*
FORM get_past_intransit_qty
                 TABLES pt_past_transit_qty  STRUCTURE it_pt_wstock.

* "initialize result
  CLEAR   : pt_past_transit_qty.
  REFRESH : pt_past_transit_qty[].

  SELECT *
    FROM ztpp_pt_wstock
    INTO CORRESPONDING FIELDS OF TABLE  pt_past_transit_qty
   WHERE mkr_cd       = 'HMC'           "Maker
     AND prdn_plnt_cd = 'HVA1'         "used Plant Code
     AND cls_scn_cd   = 'D'             "Classification Code
     AND crtn_yymm    = sy-datum+0(6).   "Basic Month
ENDFORM.                    " GET_PAST_INTRANSIT_QTY



*&---------------------------------------------------------------------*
*&      Form  GET_SHIPPING_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_WSTOCK_SHIPQTY  text
*----------------------------------------------------------------------*
FORM get_shipping_qty
                    TABLES pt_wstock_shipqty  STRUCTURE it_wstock_qty
                     USING p_inputted_date    TYPE      sy-datum.
  DATA :
    lt_wstock    LIKE it_wstock_qty OCCURS 0  WITH HEADER LINE.


* initialize result
  CLEAR   : it_bl,   lt_wstock.
  REFRESH : it_bl[], lt_wstock[].


* "get B/L (Bill of Lading)
  PERFORM get_bill_of_lading  TABLES it_pgmi
                                     it_bl      "B/L
                               USING p_inputted_date.
* -----   ---
  CHECK : NOT it_bl[]  IS INITIAL.
* -----   ---
  SORT : it_bl   BY zfblno zfblit.


  CLEAR : it_bl.
  LOOP AT it_bl.

    CLEAR : lt_wstock.
    lt_wstock-mip_cd   = it_bl-matnr.
    lt_wstock-mkr_cd   = 'HMC'.

*   "Shop Code ('SHOP_SCN_CD')
    CASE it_bl-prgrp.
      WHEN c_kd_eng.
        lt_wstock-shop_scn_cd = 'E'.  "Engine
      WHEN c_kd_tm.
        lt_wstock-shop_scn_cd = 'G'.  "T/M
    ENDCASE.

*   "PT Plant Code ('PT_PRDN_PLNT_CD' = SPACE(default))
    CASE lt_wstock-shop_scn_cd.
      WHEN 'E'.                 "Engine
        CASE it_bl-lifnr.
          WHEN c_sbc3.             "Hyundai Motor Company
            lt_wstock-pt_prdn_plnt_cd = 'HE11'.
          WHEN c_h27z.             "WIA Automotive Engine (Shandong) Co
            lt_wstock-pt_prdn_plnt_cd = 'REW3'.
        ENDCASE.

      WHEN 'G'.                 "T/M
        IF it_bl-lifnr = c_sbc3.   "Hyundai Motor Company
          lt_wstock-pt_prdn_plnt_cd   = 'HT11'.
        ENDIF.
    ENDCASE.

*   "PT Line Code ('MIP_LN_CD' = SPACE(default))
    CASE lt_wstock-shop_scn_cd.
      WHEN 'E'.                 "Engine
        CASE it_bl-lifnr.
          WHEN c_sbc3.             "Hyundai Motor Company
            lt_wstock-mip_ln_cd = 'ANU100'.
          WHEN c_h27z.             "WIA Automotive Engine (Shandong) Co
            lt_wstock-mip_ln_cd = 'REW32'.
        ENDCASE.

      WHEN 'G'.                 "T/M
        IF it_bl-lifnr = c_sbc3.   "Hyundai Motor Company
          lt_wstock-mip_ln_cd   = '8ZA00'.
        ENDIF.
    ENDCASE.

    lt_wstock-mip_tyma_cd      = ''.          "PT Type Code
    lt_wstock-usf_scn_cd       = 'C'.         "Use
    lt_wstock-wned_sqlt_scn_cd = 'A'.         "Assy Classification
    lt_wstock-prdn_plnt_cd     = 'HVA1'.      "used Plant Code

    lt_wstock-prdn_vehl_cd     = it_bl-zmo01. "Model Code(char 2)
    lt_wstock-pno              = it_bl-matnr. "Parts No.
*   "Classification Code : 'E' (shipping qty)
*   "                      'D' (in-transit qty)
    lt_wstock-cls_scn_cd       = 'E'.         "Classification Code
    lt_wstock-crtn_yymm        = w_yymm.      "Basic Month
*   "YYYYMMDDhhmmss
    lt_wstock-createdate       = g_date_time. "Date and Time

*   "Basic Month Production QTY  [B/L quantity]
    lt_wstock-m0_prdn_prd_qty  = it_bl-blmenge.
    lt_wstock-d0_prdn_prd_qty  = 0.           "Last Day Production QTY

*   "Day total by ZTBL-ZFETD (E.T.D)  [B/L quantity]
    IF NOT it_bl-zfetd IS INITIAL.
      CLEAR : g_text, g_field.
      CONCATENATE 'D'  it_bl-zfetd+6(2)  '_PRDN_PRD_QTY'  INTO  g_text.
      CONCATENATE 'LT_WSTOCK-' g_text  INTO g_field.
      ASSIGN (g_field)     TO <fs>.
      MOVE   it_bl-blmenge TO <fs>.
    ENDIF.

*   -------
    COLLECT lt_wstock.
*   -------
    CLEAR : it_bl.
  ENDLOOP.

  IF NOT lt_wstock[] IS INITIAL.
    APPEND  LINES OF lt_wstock  TO pt_wstock_shipqty.
  ENDIF.


ENDFORM.                    " GET_SHIPPING_QTY

*&---------------------------------------------------------------------*
*&      Form  GET_IN_TRANSIT_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PT_WSTOCK_TRANSITQTY  text
*----------------------------------------------------------------------*
FORM get_in_transit_qty
                TABLES pt_wstock_transitqty  STRUCTURE it_wstock_qty
                 USING p_inputted_date       TYPE      sy-datum.

  DATA: lt_wstock    LIKE it_wstock_qty OCCURS 0  WITH HEADER LINE.

  DATA: l_last_day LIKE sy-datum.

* initialize result
  CLEAR   : it_curr_transit,   it_past_transit,   lt_wstock.
  REFRESH : it_curr_transit[], it_past_transit[], lt_wstock[].


* in-transit Qty (Current & Past)
  l_last_day = sy-datum - 1.
  IF p_inputted_date = l_last_day.
    PERFORM get_current_intransit_qty TABLES it_pgmi
                                             it_curr_transit.
  ENDIF.

  PERFORM get_past_intransit_qty  TABLES it_past_transit.

* -----   ---
  CHECK : NOT it_curr_transit[] IS INITIAL
          OR
          NOT it_past_transit[] IS INITIAL.
* -----   ---


* in-transit Qty (Current)
  CLEAR : it_curr_transit.
  LOOP AT it_curr_transit.

    CLEAR : lt_wstock.
    lt_wstock-mip_cd   = it_curr_transit-matnr.
    lt_wstock-mkr_cd   = 'HMC'.

*   "Shop Code ('SHOP_SCN_CD')
    CASE it_curr_transit-prgrp.
      WHEN c_kd_eng.
        lt_wstock-shop_scn_cd = 'E'.  "Engine
      WHEN c_kd_tm.
        lt_wstock-shop_scn_cd = 'G'.  "T/M
    ENDCASE.

*   "PT Plant Code ('PT_PRDN_PLNT_CD' = SPACE(default))
    CASE lt_wstock-shop_scn_cd.
      WHEN 'E'.                 "Engine
        CASE it_curr_transit-kunde.
          WHEN c_sbc3.             "Hyundai Motor Company
            lt_wstock-pt_prdn_plnt_cd = 'HE11'.
          WHEN c_h27z.             "WIA Automotive Engine(Shandong)Co
            lt_wstock-pt_prdn_plnt_cd = 'REW3'.
        ENDCASE.

      WHEN 'G'.                 "T/M
        IF it_curr_transit-kunde = c_sbc3.   "Hyundai Motor Company
          lt_wstock-pt_prdn_plnt_cd   = 'HT11'.
        ENDIF.
    ENDCASE.

*   "PT Line Code ('MIP_LN_CD' = SPACE(default))
    CASE lt_wstock-shop_scn_cd.
      WHEN 'E'.                    "Engine
        CASE it_curr_transit-kunde.
          WHEN c_sbc3.                "Hyundai Motor Company
            lt_wstock-mip_ln_cd = 'ANU100'.
          WHEN c_h27z.                "WIA Automotive Engine(Shandong)Co
            lt_wstock-mip_ln_cd = 'REW32'.
        ENDCASE.

      WHEN 'G'.                    "T/M
        IF it_curr_transit-kunde = c_sbc3.   "Hyundai Motor Company
          lt_wstock-mip_ln_cd   = '8ZA00'.
        ENDIF.
    ENDCASE.

    lt_wstock-mip_tyma_cd      = ''.            "PT Type Code
    lt_wstock-usf_scn_cd       = 'C'.           "Use
    lt_wstock-wned_sqlt_scn_cd = 'A'.           "Assy Classification
    lt_wstock-prdn_plnt_cd     = 'HVA1'.        "used Plant Code

    lt_wstock-prdn_vehl_cd     = it_curr_transit-zmo01."Model Code(char2)
    lt_wstock-pno              = it_curr_transit-matnr."Parts No.
*   "Classification Code       ( 'D'  in-transit qty)
*   "                          ( 'E'  shipping qty)
    lt_wstock-cls_scn_cd       = 'D'.           "Classification Code
    lt_wstock-crtn_yymm        = w_yymm.        "Basic Month
*   "YYYYMMDDhhmmss
    lt_wstock-createdate       = g_date_time.   "Date and Time

    lt_wstock-d0_prdn_prd_qty  = 0.             "Last Day Production QTY

    CLEAR : g_text, g_field.
    CONCATENATE 'D' p_inputted_date+6(2)
                '_PRDN_PRD_QTY'        INTO   g_text.
    CONCATENATE 'LT_WSTOCK-' g_text    INTO   g_field.
    ASSIGN (g_field)                   TO     <fs>.
    MOVE   it_curr_transit-lfimg       TO     <fs>.

*   -------
    COLLECT lt_wstock.
*   -------

    CLEAR : it_curr_transit.
  ENDLOOP.


* in-transit Qty (Past)
  CLEAR : it_past_transit.
  LOOP AT it_past_transit.

    CLEAR : lt_wstock.
    MOVE-CORRESPONDING it_past_transit   TO   lt_wstock.
*   "YYYYMMDDhhmmss
    MOVE g_date_time   TO lt_wstock-createdate.     "Date and Time

    IF p_inputted_date = l_last_day.
      CLEAR : g_text, g_field.
      CONCATENATE 'D' p_inputted_date+6(2)
                  '_PRDN_PRD_QTY'        INTO   g_text.
      CONCATENATE 'LT_WSTOCK-' g_text    INTO   g_field.
      ASSIGN (g_field)                   TO     <fs>.
      CLEAR: <fs>.
    ENDIF.

    CLEAR :
      lt_wstock-mandt,
      lt_wstock-ifresult,
      lt_wstock-iffailmsg,
      lt_wstock-zuser,
      lt_wstock-zsdat,
      lt_wstock-zstim,
      lt_wstock-zedat,
      lt_wstock-zetim,
      lt_wstock-zbdat,
      lt_wstock-zbtim,
      lt_wstock-zbnam,
      lt_wstock-zmode,
      lt_wstock-zresult,
      lt_wstock-zmsg.
*   -------
    COLLECT lt_wstock.
*   =======

    CLEAR : it_past_transit.
  ENDLOOP.


* in-transit Qty (Current & Past)
  IF NOT lt_wstock[] IS INITIAL.
    APPEND  LINES OF lt_wstock  TO pt_wstock_transitqty.
  ENDIF.


ENDFORM.                    " GET_IN_TRANSIT_QTY
*&---------------------------------------------------------------------*
*&      Form  READ_DISPOSAL_CLASS
*&---------------------------------------------------------------------*
FORM READ_DISPOSAL_CLASS  USING    p_vmno   p_char
                          CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '002'       AND
          ca~atnam = p_char  .
ENDFORM.                    " READ_DISPOSAL_CLASS
