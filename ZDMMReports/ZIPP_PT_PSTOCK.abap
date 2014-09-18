************************************************************************
* Program Name      : ZIPP_PT_PSTOCK
* Author            : Furong Wang
* Creation Date     : 07/02/2010
* Specifications By : Daniel Kim
* Development Request No :
* Addl Documentation:
* Description       : PT: Send Production Stock
* Modification Logs
* Date       Developer    RequestNo    Description
* 08/17/10   Daniel       UD1K949664   Assign plant code according model
* 08/18/10   Daniel       UD1K949668   Model GD is IN
* 08.13.2013 Victor   Read Engine code from Body# level, not from W/O
* 05.15.2014 Victor  If that vehicle is scrap or disposal, skip
*              that vehicle and continue to check for other vehicles.
*********************************************************************

REPORT zipp_pt_pstock NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.
TABLES: pgmi, mseg, t001w.

DATA : w_msgtxt(100),
       w_result(1),
       w_yymm(6).

CONSTANTS: c_dest(10) VALUE 'WMHR01'.   "Interface Destination.

DATA: BEGIN OF it_pt_pstock OCCURS 0.
        INCLUDE STRUCTURE ztpp_pt_pstock.
DATA: END OF it_pt_pstock.
DATA: BEGIN OF it_eng OCCURS 0,
        plnum LIKE resb-plnum,
        matnr LIKE mara-matnr,
      END   OF it_eng.

*-< Victor 11.04.2011 for G/I : Trim In
DATA : BEGIN OF it_vehicle OCCURS 0,
      trim_date  LIKE ztpp_rpid-rp07_sdate,
*        pac        LIKE ztpp_wohd-u_001,
      model_code LIKE ztpp_vm-model_code,
      body_no    LIKE ztpp_vm-body_no,
      wo_serial  LIKE ztpp_vm-wo_serial,
      wo_nation  LIKE ztpp_vm-wo_nation,
      wo_dealer  LIKE ztpp_vm-wo_dealer,
      vehicle    TYPE ausp-objek,
      qty        TYPE i ,
      wk_order   TYPE zwkor,
      END OF it_vehicle.
DATA : BEGIN OF it_vehicle_tmp OCCURS 0,
      trim_date  LIKE ztpp_rpid-rp07_sdate,
*        pac        LIKE ztpp_wohd-u_001,
      model_code LIKE ztpp_vm-model_code,
      body_no    LIKE ztpp_vm-body_no,
      wo_serial  LIKE ztpp_vm-wo_serial,
      wo_nation  LIKE ztpp_vm-wo_nation,
      wo_dealer  LIKE ztpp_vm-wo_dealer,
      vehicle    TYPE ausp-objek,
      qty        TYPE i ,
      wk_order   TYPE ausp-objek,
      END OF it_vehicle_tmp.
DATA : it_ausp_engine LIKE ausp OCCURS 0 WITH HEADER LINE.
DATA : it_ausp_plan   LIKE ausp OCCURS 0 WITH HEADER LINE.
DATA : it_ausp_engine_cd LIKE ausp OCCURS 0 WITH HEADER LINE.
*DATA : it_ausp_tm     LIKE ausp OCCURS 0 WITH HEADER LINE.
*->

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_prgrp FOR pgmi-prgrp,
                s_matnr FOR pgmi-nrmit.
*                s_bwart for mseg-bwart.
SELECT-OPTIONS: s_werks FOR t001w-werks DEFAULT 'E001' TO 'P001'
                                                      OBLIGATORY.
PARAMETERS: p_date LIKE sy-datum.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_send AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.
  PERFORM get_data.

  PERFORM send_data.

END-OF-SELECTION.

*---------------------------------------------------------------------*
*       FORM GET_DATA                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM get_data.

  FIELD-SYMBOLS: <fs_gr>, <fs_gi>, <fs_stk>,
                 <fs_tran>, <fs_tran_pre>, <fs_inb>..

  DATA: BEGIN OF lt_matnr OCCURS 0,
       matnr LIKE mseg-matnr,
       END OF lt_matnr.

  DATA : lv_atinn      LIKE ausp-atinn.
  DATA : l_vehicle     TYPE ausp-objek,
         l_atwrt       TYPE ausp-atwrt.

  DATA: BEGIN OF it_porder OCCURS 0,
          bodyno(10),
          plnum LIKE resb-plnum,
        END   OF it_porder.

  DATA : BEGIN OF it_engine_tmp OCCURS 0,
          objek LIKE ausp-objek,
          atwrt LIKE ausp-objek,
        END OF it_engine_tmp.

  DATA: BEGIN OF lt_mseg_gr OCCURS 0,
    matnr LIKE mseg-matnr,
     mblnr LIKE mseg-mblnr,
     zeile LIKE mseg-zeile,
     menge LIKE mseg-menge,
     zbudat LIKE mseg-zbudat,
     bwart LIKE mseg-bwart, " 11242010 add field by sjlee
     END OF lt_mseg_gr.

  DATA: BEGIN OF lt_mseg_gi OCCURS 0,
      matnr LIKE mseg-matnr,
      mblnr LIKE mseg-mblnr,
      zeile LIKE mseg-zeile,
      menge LIKE mseg-menge,
      zbudat LIKE mseg-zbudat,
      bwart LIKE mseg-bwart,
      END OF lt_mseg_gi.

  DATA: BEGIN OF lt_inb OCCURS 0,
        matnr LIKE mseg-matnr,
        mblnr LIKE mseg-mblnr,
        zeile LIKE mseg-zeile,
        menge LIKE mseg-menge,
        zbudat LIKE mseg-zbudat,
        bwart LIKE mseg-bwart,
        END OF lt_inb.

  DATA: wa_gr LIKE ztpp_pt_pstock,
        wa_gi LIKE ztpp_pt_pstock,
        wa_stk LIKE ztpp_pt_pstock,
        wa_tran LIKE ztpp_pt_pstock,
        wa_tran_pre LIKE ztpp_pt_pstock,
        wa_inb LIKE ztpp_pt_pstock.

  DATA: l_date_c(8),
        l_time_c(6),
         l_date_from LIKE sy-datum,
         l_date LIKE sy-datum,
         l_cn(02) TYPE n,
         l_qty_gr LIKE mseg-menge,
         l_qty_gi LIKE mseg-menge,
         l_qty_stk LIKE mseg-menge,
         l_tot_gr LIKE mseg-menge,
         l_tot_gi LIKE mseg-menge,
         l_tot_stk LIKE mseg-menge,
*         L_OPEN_STK LIKE MSEG-MENGE,
         l_text(40),
         l_bwart LIKE mseg-bwart,
         l_year LIKE mbew-lfgja,
         l_period LIKE mbew-lfmon,
         l_qty_tran LIKE mseg-menge,
         l_tot_tran LIKE mseg-menge,
         l_qty_inb LIKE mseg-menge,
         l_tot_inb LIKE mseg-menge.

  l_date_c = p_date.
  w_yymm = l_date_c+0(6).


  CONCATENATE  l_date_c+0(6) '01' INTO  l_date_c.
  l_date_from =  l_date_c.

  SELECT nrmit AS matnr INTO TABLE lt_matnr
    FROM pgmi
    WHERE prgrp IN s_prgrp
      AND nrmit IN s_matnr.

  IF sy-subrc <> 0.
    MESSAGE e001 WITH 'No Data'.
    EXIT.
  ENDIF.

*  SELECT MATNR MBLNR ZEILE MENGE ZBUDAT
*     INTO TABLE LT_MSEG_GR
*     FROM MSEG
*     FOR ALL ENTRIES IN LT_MATNR
*     WHERE ZBUDAT BETWEEN L_DATE_FROM AND P_DATE
*       AND MATNR = LT_MATNR-MATNR
*       AND WERKS = 'E001'
*       AND ( BWART = '131' OR  BWART = '132' ).

*# 11242010 modify Query by sjlee +

  RANGES : gr_bwart FOR mseg-bwart .
  gr_bwart-option = 'EQ'.
  gr_bwart-sign   = 'I'.
  gr_bwart-low    = '131'. APPEND gr_bwart.
  gr_bwart-low    = '132'. APPEND gr_bwart.

  SELECT matnr mblnr zeile menge zbudat bwart
      INTO TABLE lt_mseg_gr
      FROM mseg
      FOR ALL ENTRIES IN lt_matnr
      WHERE zbudat BETWEEN l_date_from AND p_date
        AND matnr = lt_matnr-matnr
** For E002
*        AND WERKS = 'E001'
        AND werks IN  s_werks
** End
        AND bwart IN gr_bwart
   %_HINTS ORACLE 'INDEX (MSEG "MSEG~M")'.  "Addition
*# 11242010 -

*  SELECT MATNR MBLNR ZEILE MENGE ZBUDAT
*      INTO TABLE LT_MSEG_GR
*      FROM MSEG
*      FOR ALL ENTRIES IN LT_MATNR
*      WHERE ZBUDAT BETWEEN L_DATE_FROM AND P_DATE
*        AND MATNR = LT_MATNR-MATNR
*        AND WERKS = 'E001'
*        AND BWART = '131'.
*
*  SELECT MATNR MBLNR ZEILE MENGE ZBUDAT
*      APPENDING TABLE LT_MSEG_GR
*      FROM MSEG
*      FOR ALL ENTRIES IN LT_MATNR
*      WHERE ZBUDAT BETWEEN L_DATE_FROM AND P_DATE
*        AND MATNR = LT_MATNR-MATNR
*        AND WERKS = 'E001'
*        AND BWART = '132'.

  SORT lt_mseg_gr BY matnr zbudat.


*# 11242010 modify Query by sjlee +
  RANGES : gi_bwarte FOR mseg-bwart,
           gi_bwartp FOR mseg-bwart.

  gi_bwarte-option =
  gi_bwartp-option = 'EQ' .

  gi_bwarte-sign =
  gi_bwartp-sign = 'I'.

  gi_bwarte-low = '601' . APPEND gi_bwarte.
  gi_bwarte-low = '602' . APPEND gi_bwarte.

  gi_bwartp-low = '261' . APPEND gi_bwartp.
  gi_bwartp-low = '262' . APPEND gi_bwartp.


*- Engine which was sold  to KMMG
  SELECT matnr mblnr zeile menge zbudat bwart
    INTO TABLE lt_mseg_gi
    FROM mseg
         FOR ALL ENTRIES IN lt_matnr
    WHERE zbudat BETWEEN l_date_from AND p_date
       AND matnr = lt_matnr-matnr
** for e002
*      AND WERKS = 'E001'
      AND werks IN s_werks
** end
      AND bwart IN gi_bwarte
    %_HINTS ORACLE 'INDEX (MSEG "MSEG~M")'.  "Addition


** Changed on 02/28/12
  SELECT matnr mblnr zeile menge zbudat bwart
        INTO TABLE lt_inb
        FROM mseg
        FOR ALL ENTRIES IN lt_matnr
        WHERE zbudat BETWEEN l_date_from AND p_date
          AND matnr = lt_matnr-matnr
          AND ( bwart = '601' OR  bwart = '602' )
    %_HINTS ORACLE 'INDEX (MSEG "MSEG~M")'.  "Addition

  SORT lt_inb BY matnr zbudat.
** End on 02/28/12

*--<Use  Trim In date for G/I 01.12.2012 Victor
*  SELECT matnr mblnr zeile menge zbudat bwart
*    APPENDING TABLE lt_mseg_gi
*    FROM mseg
*             FOR ALL ENTRIES IN lt_matnr
*    WHERE zbudat BETWEEN l_date_from AND p_date
*       AND matnr = lt_matnr-matnr
*      AND werks = 'P001'
*      AND bwart IN gi_bwartp.

  CLEAR : it_vehicle_tmp[], it_vehicle[], it_ausp_engine[].

  SELECT  a~rp07_sdate AS trim_date  a~model_code a~body_no
          b~wo_serial b~wo_nation b~wo_dealer  " b~u_001 AS pac
   INTO CORRESPONDING FIELDS OF TABLE it_vehicle
  FROM ztpp_rpid AS a INNER JOIN ztpp_vm AS b
                    ON a~model_code =  b~model_code
                   AND a~body_no    =  b~body_no
  WHERE a~rp07_sdate  >= l_date_from
    AND a~rp07_sdate <=  p_date.
*    AND ( b~usg_car     = 'P' OR b~usg_car     = 'T' ).


*  LOOP AT IT_VEHICLE.
*    CONCATENATE  IT_VEHICLE-WO_SERIAL  IT_VEHICLE-WO_NATION
*                  IT_VEHICLE-WO_DEALER INTO IT_VEHICLE-WK_ORDER.
*    MODIFY IT_VEHICLE.
*  ENDLOOP.

  it_vehicle_tmp[] =  it_vehicle[].
*  SORT it_vehicle_tmp BY wk_order.
*  DELETE ADJACENT DUPLICATES FROM it_vehicle_tmp COMPARING wk_order.
  LOOP AT it_vehicle_tmp.
    CLEAR : it_vehicle_tmp-vehicle.
    CONCATENATE it_vehicle_tmp-model_code it_vehicle_tmp-body_no
                                            INTO it_vehicle_tmp-vehicle.
*    it_vehicle_tmp-wk_order = it_vehicle_tmp-wk_order+0(14).
    MODIFY it_vehicle_tmp.
  ENDLOOP.

*-Read Engine code from Body # 08.13.2013 Victor
  IF it_vehicle_tmp[] IS NOT INITIAL.

*-  Get Engine Serial
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'P_ENGINE_NO'
      IMPORTING
        output = lv_atinn.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_engine
    FROM ausp
      FOR ALL ENTRIES IN  it_vehicle_tmp
    WHERE objek =   it_vehicle_tmp-vehicle
      AND atinn =   lv_atinn
      AND klart =   '002'.
    SORT it_ausp_engine BY objek.

    IF it_ausp_engine[] IS NOT INITIAL.
      CLEAR : it_engine_tmp[], it_engine_tmp.
      LOOP AT it_ausp_engine.
        it_engine_tmp-objek = it_ausp_engine-objek.
        it_engine_tmp-atwrt = it_ausp_engine-atwrt.
        APPEND  it_engine_tmp.
      ENDLOOP.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'EN_ITEM_CODE'
        IMPORTING
          output = lv_atinn.
      SELECT * INTO TABLE it_ausp_engine_cd
      FROM ausp
        FOR ALL ENTRIES IN it_engine_tmp
      WHERE objek = it_engine_tmp-atwrt
        AND atinn = lv_atinn
        AND klart =   '002'.

      SORT it_ausp_engine_cd BY objek.
    ENDIF.

*-  Get Planned Order Engine code
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = 'P_PLAN_ORDER'
      IMPORTING
        output = lv_atinn.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_plan
    FROM ausp
      FOR ALL ENTRIES IN  it_vehicle_tmp
    WHERE objek =   it_vehicle_tmp-vehicle
      AND atinn =   lv_atinn
      AND klart =   '002'.
    SORT it_ausp_plan BY objek.

    LOOP AT it_ausp_plan.
      MOVE: it_ausp_plan-objek TO it_porder-bodyno,
            it_ausp_plan-atwrt TO it_porder-plnum.
      APPEND it_porder.
    ENDLOOP.

    SELECT DISTINCT a~plnum a~matnr
      INTO CORRESPONDING FIELDS OF TABLE it_eng
      FROM resb AS a INNER JOIN stpo AS b
                        ON b~idnrk = a~matnr
                       AND b~lkenz = ''
                       AND b~stlty = 'M'
                       AND b~posnr = '8000'
                       AND b~zinfo = 'ENG'
       FOR ALL ENTRIES IN it_porder
     WHERE a~plnum = it_porder-plnum.

    SORT it_porder BY bodyno.
    SORT it_eng BY plnum.
  ENDIF.

  LOOP AT it_vehicle.
    CLEAR :  it_ausp_engine, it_ausp_engine_cd.

    CONCATENATE it_vehicle-model_code it_vehicle-body_no
                                            INTO l_vehicle.
*    CLEAR : it_ausp_tm.

    it_vehicle-qty  =  1.

    READ TABLE it_ausp_engine WITH KEY objek  =  l_vehicle
                              BINARY SEARCH.

    IF sy-subrc = 0 AND it_ausp_engine-atwrt IS NOT INITIAL.
*-    Read Engine code
      CLEAR: it_ausp_engine_cd.
      READ TABLE it_ausp_engine_cd WITH KEY
                                      objek = it_ausp_engine-atwrt
                                      BINARY SEARCH.
      IF sy-subrc EQ 0 AND
         it_ausp_engine_cd-atwrt IS NOT INITIAL.
        lt_mseg_gi-matnr  = it_ausp_engine_cd-atwrt.
      ELSE.
*-      When Engine code is empty
        CLEAR : it_porder, it_eng.
        READ TABLE it_porder WITH KEY bodyno  = l_vehicle
                                       BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE it_eng WITH KEY plnum = it_porder-plnum
                            BINARY SEARCH.
          IF sy-subrc = 0.
            lt_mseg_gi-matnr  = it_eng-matnr.
          ELSE.
            MESSAGE e000 WITH 'No Engine code:' l_vehicle
                              it_ausp_engine-atwrt.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
*-< If that vehicle is scrap/disposal, skip and continue : 05.15.2014
      CLEAR : l_atwrt.
      PERFORM read_disposal_class USING l_vehicle 'P_USAGE_CAR'
                        CHANGING l_atwrt.
      IF l_atwrt = 'S' OR l_atwrt = 'D'.
        CONTINUE.
      ENDIF.
*->
*-    When Engine Serial is emtpy
      CLEAR : it_porder, it_eng.
      READ TABLE it_porder WITH KEY bodyno  = l_vehicle
                                     BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE it_eng WITH KEY plnum = it_porder-plnum
                          BINARY SEARCH.
        IF sy-subrc = 0.
          lt_mseg_gi-matnr  = it_eng-matnr.
        ELSE.
          MESSAGE e000 WITH 'No Engine code:' l_vehicle.
        ENDIF.
      ENDIF.
    ENDIF.

    CHECK lt_mseg_gi-matnr IN s_matnr[].    "Not improt Engine

    lt_mseg_gi-bwart  = '261'.   "hard coding for calculation below
*    lt_mseg_gi-matnr   = it_ausp_engine-atwrt.
    lt_mseg_gi-menge   = it_vehicle-qty.
    lt_mseg_gi-zbudat  = it_vehicle-trim_date.
    COLLECT lt_mseg_gi.    CLEAR lt_mseg_gi.

  ENDLOOP.
*-->

*  SELECT MATNR MBLNR ZEILE MENGE ZBUDAT BWART
*    INTO TABLE LT_MSEG_GI
*    FROM MSEG
*         FOR ALL ENTRIES IN LT_MATNR
*    WHERE ZBUDAT BETWEEN L_DATE_FROM AND P_DATE
*       AND MATNR = LT_MATNR-MATNR
*      AND WERKS = 'E001'
*      AND BWART = '601'.
*
*  SELECT MATNR MBLNR ZEILE MENGE ZBUDAT BWART
*    APPENDING TABLE LT_MSEG_GI
*    FROM MSEG
*             FOR ALL ENTRIES IN LT_MATNR
*    WHERE ZBUDAT BETWEEN L_DATE_FROM AND P_DATE
*       AND MATNR = LT_MATNR-MATNR
*      AND WERKS = 'P001'
*      AND BWART = '261'.

*# 11242010 -

  SORT lt_mseg_gi BY matnr zbudat.

*  LOOP AT LT_MSEG_GR.
*    LT_MATNR-MATNR = LT_MSEG_GR.
*    COLLECT LT_MATNR.
*  ENDLOOP.
*
*  LOOP AT LT_MSEG_GI.
*    LT_MATNR-MATNR = LT_MSEG_GI.
*    COLLECT LT_MATNR.
*  ENDLOOP.

  l_date_c = p_date.

  l_year = l_date_c+0(4).
  l_period = l_date_c+4(2).
  IF l_period > 1.
    l_period = l_period - 1.
  ELSE.
    l_year = l_year - 1.

*# 11222010 change Period before one month by. sjlee req. Daniel +
*    L_PERIOD = '01'.
    l_period = '12'.
*# 11222010 -

  ENDIF.

  l_date_c = sy-datum.
  l_time_c = sy-uzeit.
*# 11222010 Add Sort delete Duplicate material code sjlee +
  SORT lt_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_matnr.
*# 11222010 -

  LOOP AT lt_matnr.

    CLEAR: l_tot_gr, l_tot_gi, l_tot_stk, l_qty_stk,
            l_tot_tran,l_tot_inb.
    CLEAR: wa_gr, wa_gi, wa_stk, wa_tran,wa_tran_pre,
           wa_inb, it_pt_pstock.

    SELECT SUM( lbkum ) INTO l_tot_stk
      FROM mbewh
      WHERE matnr = lt_matnr-matnr
        AND lfgja = l_year
        AND lfmon = l_period.

    IF sy-subrc <> 0.
      SELECT SUM( lbkum ) INTO l_tot_stk
       FROM mbew
       WHERE matnr = lt_matnr-matnr
         AND lfgja <= l_year
         AND lfmon <= l_period.
    ENDIF.

    it_pt_pstock-mip_cd = lt_matnr-matnr.
    it_pt_pstock-mkr_cd = 'H201'.
    it_pt_pstock-shop_scn_cd = 'E'.
    it_pt_pstock-pt_prdn_plnt_cd = 'HEA1'.

    it_pt_pstock-usf_scn_cd = 'V'.
    it_pt_pstock-wned_sqlt_scn_cd = 'A'.

    PERFORM read_normal_class USING lt_matnr-matnr 'EN_VEH_MODEL'
                                CHANGING  it_pt_pstock-prdn_vehl_cd.

* by Daniel on 08/18/10 {
    IF it_pt_pstock-prdn_vehl_cd = 'GD'.
      it_pt_pstock-prdn_vehl_cd = 'IN'.
    ENDIF.
* }

    it_pt_pstock-pno = lt_matnr-matnr.
    it_pt_pstock-crtn_yymm = w_yymm.

    PERFORM read_normal_class USING lt_matnr-matnr 'EN_SPC14'
                                CHANGING  it_pt_pstock-mip_ln_cd.

    CONCATENATE l_date_c l_time_c INTO it_pt_pstock-createdate.
    it_pt_pstock-changedate = it_pt_pstock-createdate.

    MOVE-CORRESPONDING it_pt_pstock TO wa_gr.
    MOVE-CORRESPONDING it_pt_pstock TO wa_gi.
    MOVE-CORRESPONDING it_pt_pstock TO wa_stk.

** Furong on 02/28/12
    MOVE-CORRESPONDING it_pt_pstock TO wa_tran.
    MOVE-CORRESPONDING it_pt_pstock TO wa_inb.
    SELECT SINGLE * INTO wa_tran_pre
     FROM ztpp_pt_pstock
     WHERE mip_cd = lt_matnr-matnr
       AND cls_scn_cd ='D'
       AND crtn_yymm = w_yymm.
** End on 02/28/12

    l_date = l_date_from.
    l_cn = '01'.
    WHILE l_date <= p_date.
      CLEAR: l_qty_gr, l_qty_gi, l_text,l_bwart.

      CONCATENATE 'WA_GR-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_gr>.

      CONCATENATE 'WA_GI-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_gi>.

      CONCATENATE 'WA_STK-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_stk>.


** Furong on 02/28/12
      CONCATENATE 'WA_TRAN-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_tran>.
      CONCATENATE 'WA_INB-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_inb>.

** In transit stock
      CLEAR: l_qty_tran, l_qty_inb.
      IF l_date = p_date.
        SELECT SINGLE labst INTO l_qty_tran
        FROM mard
        WHERE werks = 'E001'
          AND lgort = 'E302'
          AND matnr = lt_matnr-matnr.
      ELSE.
        CONCATENATE 'WA_TRAN_PRE-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
        ASSIGN (l_text) TO <fs_tran_pre>.
        l_qty_tran = <fs_tran_pre>.
      ENDIF.
      <fs_tran> = l_qty_tran.
      l_tot_tran =  l_tot_tran + l_qty_tran.

** inbound delivery
*      LOOP AT lt_lips WHERE matnr = lt_matnr-matnr
*                        AND lfdat = l_date.
*        l_qty_inb = l_qty_inb + lt_lips-lgmng.
*      ENDLOOP.
      LOOP AT lt_inb WHERE matnr = lt_matnr-matnr
                        AND zbudat = l_date.
        IF lt_inb-bwart EQ '602' .
          lt_inb-menge = lt_inb-menge * -1.
        ENDIF.
        l_qty_inb = l_qty_inb + lt_inb-menge.
      ENDLOOP.

      IF l_qty_inb > 0.
        <fs_inb> = l_qty_inb.
        l_tot_inb = l_tot_inb + l_qty_inb.
      ENDIF.
** End pn 02/28/12

** Production
      LOOP AT lt_mseg_gr WHERE matnr = lt_matnr
                          AND zbudat = l_date.

* by Daniel on 01/05/2011 {
        IF lt_mseg_gr-bwart EQ '132'.
          lt_mseg_gr-menge = lt_mseg_gr-menge * -1.
        ENDIF.
* }

*# 11242010 by sjlee +
        IF lt_mseg_gr-bwart EQ '602' OR
           lt_mseg_gr-bwart EQ '262' .
          lt_mseg_gr-menge = lt_mseg_gr-menge * -1.
        ENDIF.
*# 11242010 -
        l_qty_gr = l_qty_gr + lt_mseg_gr-menge.
      ENDLOOP.

* by Daniel on 01/07/11 {
*      IF L_QTY_GR > 0.
*        WRITE L_QTY_GR TO <FS_GR> NO-ZERO DECIMALS 0.
      <fs_gr> = l_qty_gr.
      l_tot_gr = l_tot_gr + l_qty_gr.
*      ENDIF.
* }

** GI
      LOOP AT lt_mseg_gi WHERE matnr = lt_matnr
                          AND zbudat = l_date.
*# 11242010 by sjlee +
        IF lt_mseg_gi-bwart EQ '602' OR
           lt_mseg_gi-bwart EQ '262' .
          lt_mseg_gi-menge = lt_mseg_gi-menge * -1.
        ENDIF.
*# 11242010 -
        l_qty_gi = l_qty_gi + lt_mseg_gi-menge.
        l_bwart =  lt_mseg_gi-bwart.
      ENDLOOP.

* by Daniel on 01/07/11 {
*      IF L_QTY_GI > 0.
*        WRITE L_QTY_GI TO <FS_GI> NO-ZERO DECIMALS 0.
      <fs_gi> = l_qty_gi.

* by Daniel on 03/10/11 {
*        IF L_BWART = '261'.
*          WA_GI-PRDN_PLNT_CD = 'HVA1'.
*        ELSE.
*          WA_GI-PRDN_PLNT_CD = 'KVA1'.
*        ENDIF.
*        L_TOT_GI = L_TOT_GI + L_QTY_GI.
*     ENDIF.
      IF l_bwart = '261' OR l_bwart = '262'.
        wa_gi-prdn_plnt_cd = 'HVA1'.
      ELSEIF l_bwart = '601' OR l_bwart = '602'.
        wa_gi-prdn_plnt_cd = 'KVA1'.
      ELSE.
      ENDIF.
      l_tot_gi = l_tot_gi + l_qty_gi.

*}
* }

** Stock
*# 11222010 Add add Last Stock by sjlee req. Daniel +
      IF NOT l_tot_stk IS INITIAL.
        l_qty_stk = l_tot_stk.
      ENDIF.
*# 11222010 -

      l_qty_stk = l_qty_stk + l_qty_gr - l_qty_gi.

* by Daniel on 01/07/11 {
*     IF L_QTY_STK > 0.
*      WRITE L_QTY_STK TO <FS_STK> NO-ZERO DECIMALS 0.
      <fs_stk> = l_qty_stk.
*     ENDIF.
* }

      l_tot_stk  =  l_tot_stk + l_qty_gr - l_qty_gi.

*      <FS_STK> = L_TOT_STK.
*      WRITE L_QTY_STK TO <FS_STK> NO-ZERO DECIMALS 0.
*      <FS_STK> = L_QTY_STK.
      l_cn = l_cn + 1.
      l_date = l_date + 1.

    ENDWHILE.

*    WA_GR-M0_PRDN_PRD_QTY = L_TOT_GR.
*    WRITE L_TOT_GR TO WA_GR-M0_PRDN_PRD_QTY NO-ZERO DECIMALS 0.
    wa_gr-prdn_plnt_cd = 'HVA1'.
    wa_gr-m0_prdn_prd_qty = l_tot_gr.
    wa_gr-cls_scn_cd = 'A'.

*    WA_GI-M0_PRDN_PRD_QTY = L_TOT_GI.
*    WRITE L_TOT_GI TO WA_GI-M0_PRDN_PRD_QTY NO-ZERO DECIMALS 0.
    wa_gi-m0_prdn_prd_qty = l_tot_gi.
    wa_gi-cls_scn_cd = 'B'.

*    WA_STK-M0_PRDN_PRD_QTY = L_TOT_STK.
    wa_stk-cls_scn_cd = 'C'.

* by Daniel on 08/17/10 {
* by Daniel on 08/18/10 {
*    IF WA_STK-CLS_SCN_CD = 'C'.
*      IF WA_STK-PRDN_VEHL_CD = 'GD'.
*        WA_STK-PRDN_VEHL_CD = 'IN'.
*      ENDIF.

    SELECT COUNT(*)
    FROM ztpp_model_conv
    WHERE bmdl = wa_stk-prdn_vehl_cd.

    IF sy-subrc <> 0.
      wa_stk-prdn_plnt_cd = 'KVA1'.
    ELSE.
      wa_stk-prdn_plnt_cd = 'HVA1'.
    ENDIF.
*    ENDIF.
* }
* }

    IF l_tot_gr <> 0.
      APPEND wa_gr TO it_pt_pstock.
    ENDIF.
    IF l_tot_gi <> 0.
      APPEND wa_gi TO it_pt_pstock.
    ENDIF.
    IF l_tot_stk <> 0.
      IF l_tot_gr = 0 AND l_tot_gi = 0.

        l_date = l_date_from.
        l_cn = '01'.
        WHILE l_date <= p_date.
          CLEAR: l_text.
          CONCATENATE 'WA_STK-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
          ASSIGN (l_text) TO <fs_stk>.

*          WRITE L_TOT_STK TO <FS_STK> NO-ZERO DECIMALS 0.
          <fs_stk> = l_tot_stk.
          l_cn = l_cn + 1.
          l_date = l_date + 1.
        ENDWHILE.
      ENDIF.
      APPEND wa_stk TO it_pt_pstock.

    ELSEIF l_tot_gi <> 0 OR  l_tot_gr <> 0.
      APPEND wa_stk TO it_pt_pstock.
    ENDIF.

** Furong on 02/24/12
    wa_tran-cls_scn_cd = 'D'.
    wa_tran-m0_prdn_prd_qty = '0'.
    wa_tran-d0_prdn_prd_qty = '0'.
    wa_tran-prdn_plnt_cd = 'KVA1'.
    IF l_tot_tran <> 0.
      APPEND wa_tran TO it_pt_pstock.
    ENDIF.

    wa_inb-cls_scn_cd = 'E'.
    wa_inb-m0_prdn_prd_qty = '0'.
    wa_inb-d0_prdn_prd_qty = '0'.
    wa_inb-prdn_plnt_cd = 'KVA1'.
    IF l_tot_inb <> 0.
      APPEND wa_inb TO it_pt_pstock.
    ENDIF.
** End on 02/24/12

  ENDLOOP.

** Changed by Park On 11/18/13
  DELETE it_pt_pstock WHERE prdn_plnt_cd = 'KVA1'
                        AND ( cls_scn_cd EQ 'D' OR cls_scn_cd EQ 'E').
** End of change 11/18/13

ENDFORM.                    "GET_DATA


*---------------------------------------------------------------------*
*       FORM SEND_DATA                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM send_data.
  DATA: it_save LIKE TABLE OF ztpp_pt_pstock WITH HEADER LINE,
        it_send LIKE TABLE OF ztpp_pt_pstock WITH HEADER LINE,
        l_flag(1),
        l_yymm(6),
        l_date_c(8).

  l_date_c = p_date.
  l_yymm =  l_date_c+0(6).

  IF it_pt_pstock[] IS INITIAL.
    MESSAGE i000 WITH 'No data to be sent'.
    EXIT.
  ENDIF.
  IF p_send IS INITIAL.
    LOOP AT it_pt_pstock.
      MOVE-CORRESPONDING it_pt_pstock TO it_save.
      it_save-zsdat = sy-datum.
      it_save-zstim = sy-uzeit.
      APPEND it_save.
      CLEAR: it_save.
    ENDLOOP.

    DELETE FROM ztpp_pt_pstock WHERE crtn_yymm = l_yymm.
    INSERT ztpp_pt_pstock FROM TABLE it_save.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Database table update error'.
    ENDIF.
  ELSE.

** Change by Furong on 04/16/12
    it_send[] =  it_pt_pstock[].
    DELETE it_send WHERE cls_scn_cd = 'D' OR
                         cls_scn_cd = 'E'.

    CALL FUNCTION 'Z_FPP_PT_PSTOCK'
      DESTINATION c_dest
      IMPORTING
        flag                  = w_result
      TABLES
        i_data                = it_send
      EXCEPTIONS
        communication_failure = 1  MESSAGE w_msgtxt
        system_failure        = 2  MESSAGE w_msgtxt.

*    CALL FUNCTION 'Z_FPP_PT_PSTOCK'
*      DESTINATION c_dest
*      IMPORTING
*        flag                  = w_result
*      TABLES
*        i_data                = it_pt_pstock
*      EXCEPTIONS
*        communication_failure = 1  MESSAGE w_msgtxt
*        system_failure        = 2  MESSAGE w_msgtxt.

** End on 04/16/12

    IF sy-subrc = 0.
      IF w_result = 'S'.
        l_flag = 'S'.
        w_msgtxt = 'Data successfully sent out'.
        MESSAGE i001 WITH w_msgtxt.

      ELSE.
        l_flag = 'E'.
        w_msgtxt =  'Data unsuccessfully sent out'.
        MESSAGE i001 WITH w_msgtxt.
      ENDIF.
    ELSE.
      l_flag = 'E'.
      MESSAGE i001 WITH w_msgtxt.
    ENDIF.

    LOOP AT it_pt_pstock.
      MOVE-CORRESPONDING it_pt_pstock TO it_save.
      it_save-zresult = l_flag.
      it_save-zmsg = w_msgtxt.
      it_save-zsdat = sy-datum.
      it_save-zstim = sy-uzeit.
      it_save-zedat = sy-datum.
      it_save-zetim = sy-uzeit.
      APPEND it_save.
      CLEAR: it_save.
    ENDLOOP.

    DELETE FROM ztpp_pt_pstock WHERE crtn_yymm = l_yymm.
    INSERT ztpp_pt_pstock FROM TABLE it_save.
    IF sy-subrc = 0.
      COMMIT WORK.
      IF l_flag EQ 'E'.
        MESSAGE e001 WITH w_msgtxt.
      ENDIF.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Table saving error'.
    ENDIF.
  ENDIF.
ENDFORM.                    "SEND_DATA
*&---------------------------------------------------------------------*
*&      Form  read_normal_class
*&---------------------------------------------------------------------*
FORM read_normal_class USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '001'       AND   " material
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK_OBJEK  text
*      -->P_0461   text
*      <--P_L_ATFLV_TEMP  text
*----------------------------------------------------------------------*
FORM read_normal_class_atflv USING  p_vmno  p_char
                             CHANGING p_value.
  SELECT SINGLE au~atflv
      INTO p_value
      FROM ausp AS au
        INNER JOIN cabn AS ca ON au~atinn = ca~atinn
      WHERE objek = p_vmno      AND
            klart = '002'       AND
            ca~atnam = p_char  .
ENDFORM.                    " READ_NORMAL_CLASS_ATFLV
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  p_date = sy-datum - 1.

*  S_MATNR-LOW = 'AU*'.
*  S_MATNR-SIGN = 'I'.
*  S_MATNR-OPTION = 'EQ'.
*  APPEND S_MATNR.
*
*  S_MATNR-LOW = 'AW*'.
*  S_MATNR-SIGN = 'I'.
*  S_MATNR-OPTION = 'EQ'.
*  APPEND S_MATNR.


ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  READ_DISPOSAL_CLASS
*&---------------------------------------------------------------------*
FORM read_disposal_class  USING    p_vmno   p_char
                          CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_vmno      AND
          klart = '002'       AND
          ca~atnam = p_char  .
ENDFORM.                    " READ_DISPOSAL_CLASS
