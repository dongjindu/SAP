************************************************************************
* Program Name      : ZIPP_PT_WSTOCK
* Author            : Furong Wang
* Creation Date     : 07/02/2010
* Specifications By : Daniel Kim
* Development Request No :
* Addl Documentation:
* Description       : PT: Send Production Stock
* Modification Logs
* Date       Developer    RequestNo    Description
* 08/18/10   Daniel       UD1K949670   get engine & trans material
* 01.12.2012 Victor        Use  Trim In date for G/I
* 04/09/12   Furong       Copy from UP2 (ignore changes we made *
*                         previous) requested by Lee
**********************************************************************
* 08.13.2013 Victor   Read Engine code from Body# level, not from W/O
* 10.21.2013 CH.Jeong     UD1K959015    Add quantity
*                                          of shipping & in-transit *
* 05.15.2014 Victor  If that vehicle is scrap or disposal, skip
*              that vehicle and continue to check for other vehicles.
*********************************************************************

REPORT zipp_pt_wstock NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmpp.

INCLUDE : zipp_pt_wstock_top,
          zipp_pt_wstock_f01.

*** from ::: move  to  "INCLUDE zipp_pt_wstock_TOP"
*TABLES: pgmi, mseg.
*
*DATA : w_msgtxt(100),
*       w_result(1),
*       w_yymm(6).
*
*CONSTANTS:
** "Interface Destination.
*  c_dest(10)  TYPE c            VALUE 'WMHR01',
*  c_kd_eng    TYPE pgmi-prgrp   VALUE 'KD-ENG',
*  c_kd_tm     TYPE pgmi-prgrp   VALUE 'KD-TM',
*  c_sbc3      TYPE lifnr        VALUE 'SBC3',  "Hyundai Motor Company
*  c_h27z      TYPE lifnr        VALUE 'H27Z'.  "WIA Auto.Engine(Shandong)Co
*
*DATA: BEGIN OF it_pt_wstock OCCURS 0.
*        INCLUDE STRUCTURE ztpp_pt_wstock.
*DATA: END OF it_pt_wstock.
*** to ::: move  to  "INCLUDE zipp_pt_wstock_TOP"


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
DATA : it_ausp_tm     LIKE ausp OCCURS 0 WITH HEADER LINE.
DATA : it_ausp_plan   LIKE ausp OCCURS 0 WITH HEADER LINE.
DATA : it_ausp_engine_cd LIKE ausp OCCURS 0 WITH HEADER LINE.
*->

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
SELECT-OPTIONS: s_prgrp FOR pgmi-prgrp,         "Product Group
                s_matnr FOR pgmi-nrmit.         "Material Number
*                s_bwart for mseg-bwart.

PARAMETERS: p_date LIKE sy-datum.               "Date

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_send AS CHECKBOX,       "Send to Interface
            p_chk  AS CHECKBOX.       "Get Select STPO

SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  PERFORM init_data.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_selection.

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

  FIELD-SYMBOLS: <fs_gr>, <fs_gi>, <fs_stk>.

  DATA: BEGIN OF lt_matnr OCCURS 0,
       matnr LIKE mseg-matnr,
       END OF lt_matnr.

  DATA : lv_atinn LIKE ausp-atinn.
  DATA : l_vehicle     TYPE ausp-objek,
         l_atwrt    TYPE ausp-atwrt.

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
     bwart LIKE mseg-bwart,
     END OF lt_mseg_gr.

  DATA: BEGIN OF lt_invoice_gr OCCURS 0,
      matnr LIKE mseg-matnr,
      menge LIKE mseg-menge,
      zbudat LIKE mseg-zbudat,
      invoice LIKE mseg-mblnr,
      END OF lt_invoice_gr.

  DATA: BEGIN OF lt_invno OCCURS 0,
       matnr LIKE mseg-matnr,
       invoice LIKE mseg-mblnr,
       END OF lt_invno.

  DATA: BEGIN OF lt_mseg_gi OCCURS 0,
      matnr LIKE mseg-matnr,
      mblnr LIKE mseg-mblnr,
      zeile LIKE mseg-zeile,
      menge LIKE mseg-menge,
      zbudat LIKE mseg-zbudat,
      bwart LIKE mseg-bwart,
      END OF lt_mseg_gi.

  DATA: wa_gr LIKE ztpp_pt_wstock,
        wa_gi LIKE ztpp_pt_wstock,
        wa_stk LIKE ztpp_pt_wstock.


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
         l_kdmat LIKE lips-kdmat.

* by Daniel on 08/18/10 {
  DATA: $strlen TYPE i.
* }

* by Daniel on 02/10/11 {
  DATA: BEGIN OF lt_eord OCCURS 0,
        matnr LIKE eord-matnr,
        lifnr LIKE eord-lifnr,
        END OF lt_eord.
* }


  l_date_c = p_date.
  w_yymm = l_date_c+0(6).


  CONCATENATE  l_date_c+0(6) '01' INTO  l_date_c.
  l_date_from =  l_date_c.

* by Daniel on 08/18/10 {
*  SELECT NRMIT AS MATNR INTO TABLE LT_MATNR
*    FROM PGMI
*    WHERE PRGRP IN S_PRGRP
*      AND NRMIT IN S_MATNR.

  IF p_chk EQ 'X'.
* by Daniel on 02/10/11 {
*    DATA : LT_STPO LIKE TABLE OF STPO.
    DATA: BEGIN OF lt_stpo OCCURS 0,
          idnrk LIKE stpo-idnrk,
          zinfo LIKE stpo-zinfo,
          END OF lt_stpo.
* }


    SELECT idnrk zinfo INTO CORRESPONDING FIELDS OF TABLE lt_stpo
      FROM   stpo
       WHERE lkenz EQ ''
         AND stlty EQ 'M'
         AND posnr EQ '8000'
         AND ( zinfo EQ 'ENG'
            OR zinfo EQ 'TM' ).

    CHECK NOT lt_stpo[] IS INITIAL.

    SELECT matnr INTO CORRESPONDING FIELDS OF TABLE lt_matnr
      FROM marc
      FOR ALL ENTRIES IN lt_stpo
       WHERE matnr EQ lt_stpo-idnrk
         AND beskz = 'F'
         AND werks = 'P001'
         AND dispo NE 'ME1'
         AND mmsta = '12'.
  ELSE.

    SELECT matnr INTO TABLE lt_matnr
      FROM mara
      WHERE matnr IN s_matnr
        AND mtart = 'ROH'.
  ENDIF.

* by Daniel on 02/10/11 {
  SELECT matnr lifnr INTO CORRESPONDING FIELDS OF TABLE lt_eord
    FROM eord
     FOR ALL ENTRIES IN lt_matnr
   WHERE matnr EQ lt_matnr-matnr.
* }

  IF sy-subrc = 0.

    LOOP AT lt_matnr.
      $strlen = strlen( lt_matnr-matnr ).

      IF $strlen > 4.
        DELETE lt_matnr INDEX sy-tabix.
      ENDIF.

      CLEAR $strlen.
    ENDLOOP.

    CLEAR: lt_matnr.
* }

    SELECT matnr mblnr zeile menge zbudat bwart
       INTO TABLE lt_mseg_gr
       FROM mseg
       FOR ALL ENTRIES IN lt_matnr
       WHERE zbudat BETWEEN l_date_from AND p_date
         AND matnr = lt_matnr-matnr
*       AND WERKS = 'E001'
         AND ( bwart = '101' OR  bwart = '102' ).

    SORT lt_mseg_gr BY matnr zbudat.

*--<Use  Trim In date for G/I 01.12.2012 Victor
*-   commented : original source
*    SELECT matnr mblnr zeile menge zbudat bwart
*      INTO TABLE lt_mseg_gi
*      FROM mseg
*           FOR ALL ENTRIES IN lt_matnr
*      WHERE zbudat BETWEEN l_date_from AND p_date
*         AND matnr = lt_matnr-matnr
**      AND WERKS = 'E001'
*        AND bwart = '261'.
*
*    SELECT matnr mblnr zeile menge zbudat bwart
*       APPENDING TABLE lt_mseg_gi
*       FROM mseg
*            FOR ALL ENTRIES IN lt_matnr
*       WHERE zbudat BETWEEN l_date_from AND p_date
*          AND matnr = lt_matnr-matnr
**      AND WERKS = 'E001'
*         AND bwart = '262'.

    CLEAR : it_vehicle_tmp[], it_vehicle[], it_ausp_engine[],
            it_ausp_tm[].

    SELECT  a~rp07_sdate AS trim_date  a~model_code a~body_no
            b~wo_serial b~wo_nation b~wo_dealer  " b~u_001 AS pac
     INTO CORRESPONDING FIELDS OF TABLE it_vehicle
    FROM ztpp_rpid AS a INNER JOIN ztpp_vm AS b
                      ON a~model_code =  b~model_code
                     AND a~body_no    =  b~body_no
    WHERE a~rp07_sdate  >= l_date_from
      AND a~rp07_sdate <=  p_date.
*      AND ( b~usg_car     = 'P' OR b~usg_car     = 'T' ).

    LOOP AT it_vehicle.
      CONCATENATE  it_vehicle-wo_serial  it_vehicle-wo_nation
                    it_vehicle-wo_dealer INTO it_vehicle-wk_order.
      CONCATENATE it_vehicle-model_code it_vehicle-body_no
                                              INTO it_vehicle-vehicle.
      MODIFY it_vehicle.
    ENDLOOP.

    it_vehicle_tmp[] =  it_vehicle[].
    SORT it_vehicle_tmp BY wk_order.
    DELETE ADJACENT DUPLICATES FROM it_vehicle_tmp COMPARING wk_order.
    LOOP AT it_vehicle_tmp.
      it_vehicle_tmp-wk_order = it_vehicle_tmp-wk_order+0(14).
      MODIFY it_vehicle_tmp.
    ENDLOOP.

    IF it_vehicle_tmp[] IS NOT INITIAL.
*-    transmission material code
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'P_ALC_U_2'
        IMPORTING
          output = lv_atinn.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_tm
      FROM ausp
        FOR ALL ENTRIES IN  it_vehicle_tmp
      WHERE objek =   it_vehicle_tmp-wk_order
        AND atinn =   lv_atinn
        AND klart =   '001'.
    ENDIF.

*-  Read Engine code from Body # 08.13.2013 Victor
    IF it_vehicle[] IS NOT INITIAL.
*-    Get Engine Serial
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'P_ENGINE_NO'
        IMPORTING
          output = lv_atinn.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_engine
      FROM ausp
        FOR ALL ENTRIES IN  it_vehicle
      WHERE objek =   it_vehicle-vehicle
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

*-    Get Planned Order Engine code
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          input  = 'P_PLAN_ORDER'
        IMPORTING
          output = lv_atinn.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ausp_plan
      FROM ausp
        FOR ALL ENTRIES IN  it_vehicle
      WHERE objek =   it_vehicle-vehicle
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
      CLEAR : it_ausp_tm, it_ausp_engine, it_ausp_engine_cd.

      CONCATENATE it_vehicle-model_code it_vehicle-body_no
                                              INTO l_vehicle.
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
*-        When Engine code is empty
          CLEAR : it_porder, it_eng.
          READ TABLE it_porder WITH KEY bodyno  = l_vehicle
                                         BINARY SEARCH.
          IF sy-subrc = 0.
            READ TABLE it_eng WITH KEY plnum = it_porder-plnum
                              BINARY SEARCH.
            IF sy-subrc = 0.
              lt_mseg_gi-matnr  = it_eng-matnr.
            ELSE.
              MESSAGE e000 WITH 'No Engine code:'  l_vehicle
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

      lt_mseg_gi-menge   = it_vehicle-qty.
      lt_mseg_gi-zbudat  = it_vehicle-trim_date.
      COLLECT lt_mseg_gi.    CLEAR lt_mseg_gi.

      READ TABLE it_ausp_tm WITH KEY objek  = it_vehicle-wk_order.
      IF sy-subrc <> 0.
        MESSAGE e000 WITH 'T/M is not exist: ' l_vehicle.
      ENDIF.
      lt_mseg_gi-matnr   = it_ausp_tm-atwrt.
      lt_mseg_gi-menge   = it_vehicle-qty.
      lt_mseg_gi-zbudat  = it_vehicle-trim_date.
      COLLECT lt_mseg_gi.   CLEAR : lt_mseg_gi.
    ENDLOOP.
*-->

    SORT lt_mseg_gi BY matnr zbudat.

  ENDIF.
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

  LOOP AT lt_matnr.
    CLEAR: l_tot_gr, l_tot_gi, l_tot_stk, l_qty_stk.
    CLEAR: wa_gr, wa_gi, wa_stk, it_pt_wstock.

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

    it_pt_wstock-mip_cd = lt_matnr-matnr.

* by Daniel on 02/10/11 {
* get vendor code for manufacturer information
    READ TABLE lt_eord WITH KEY matnr = lt_matnr-matnr.
    IF lt_eord-lifnr = 'ALVY'.
      it_pt_wstock-mkr_cd = 'H210'.
    ELSEIF lt_eord-lifnr = 'H27Z'.
      it_pt_wstock-mkr_cd = 'H135'.
    ELSE.
      it_pt_wstock-mkr_cd = 'H109'.
    ENDIF.

*    IT_PT_WSTOCK-MKR_CD = 'H201'.

* get shop code based on type of material
    READ TABLE lt_stpo WITH KEY idnrk = lt_matnr-matnr.

    IF sy-subrc = 0.
      IF lt_stpo-zinfo = 'TM'.
        it_pt_wstock-shop_scn_cd = 'G'.
      ELSE.
        it_pt_wstock-shop_scn_cd = 'E'.
      ENDIF.
    ENDIF.

* by Daniel on 02/14/11 {
*    IT_PT_WSTOCK-PT_PRDN_PLNT_CD = 'HEA1'.
    IF it_pt_wstock-mkr_cd = 'H210'.
      it_pt_wstock-pt_prdn_plnt_cd = 'RTP2'.
    ELSEIF it_pt_wstock-mkr_cd = 'H109'.
      it_pt_wstock-pt_prdn_plnt_cd = 'RTP1'.
    ELSEIF it_pt_wstock-mkr_cd = 'H135'.
      it_pt_wstock-pt_prdn_plnt_cd = 'REW3'.
    ENDIF.
* }
* }



    it_pt_wstock-usf_scn_cd = 'V'.
    it_pt_wstock-wned_sqlt_scn_cd = 'A'.

* by Daniel on 08/18/10 {
*    PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_VEH_MODEL'
*                                CHANGING  IT_PT_WSTOCK-PRDN_VEHL_CD.
* }

    it_pt_wstock-pno = lt_matnr-matnr.
    it_pt_wstock-crtn_yymm = w_yymm.

    PERFORM read_normal_class USING lt_matnr-matnr 'EN_SPC14'
                                CHANGING  it_pt_wstock-mip_ln_cd.

    CONCATENATE l_date_c l_time_c INTO it_pt_wstock-createdate.
    it_pt_wstock-changedate = it_pt_wstock-createdate.

    MOVE-CORRESPONDING it_pt_wstock TO wa_gr.
    MOVE-CORRESPONDING it_pt_wstock TO wa_gi.
    MOVE-CORRESPONDING it_pt_wstock TO wa_stk.

    l_date = l_date_from.
    l_cn = '01'.
    WHILE l_date <= p_date.
      CLEAR: l_qty_gr, l_qty_gi, l_text, l_bwart.

      CONCATENATE 'WA_GR-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_gr>.

      CONCATENATE 'WA_GI-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_gi>.

      CONCATENATE 'WA_STK-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
      ASSIGN (l_text) TO <fs_stk>.

** Production
      LOOP AT lt_mseg_gr WHERE matnr = lt_matnr
                          AND zbudat = l_date.
        IF lt_mseg_gr-bwart EQ '102' .
          lt_mseg_gr-menge = lt_mseg_gr-menge * -1.
        ENDIF.
        l_qty_gr = l_qty_gr + lt_mseg_gr-menge.
      ENDLOOP.
      IF l_qty_gr > 0.

*        WRITE L_QTY_GR TO <FS_GR> NO-ZERO DECIMALS 0.
        <fs_gr> = l_qty_gr.
        l_tot_gr = l_tot_gr + l_qty_gr.
      ENDIF.
** GI
      LOOP AT lt_mseg_gi WHERE matnr = lt_matnr
                          AND zbudat = l_date.
*        IF lt_mseg_gi-bwart EQ '262' . "commented by Victor 01.12.2012
*          lt_mseg_gi-menge = lt_mseg_gi-menge * -1.
*        ENDIF.
        l_qty_gi = l_qty_gi + lt_mseg_gi-menge.
*        l_bwart =  lt_mseg_gi-bwart.  "commented by Victor 01.12.2012
      ENDLOOP.

      IF l_qty_gi > 0.
*        WRITE L_QTY_GI TO <FS_GI> NO-ZERO DECIMALS 0.
        <fs_gi> = l_qty_gi.
*        IF l_bwart = '261'.  "commented by Victor 01.12.2012
*          wa_gi-prdn_plnt_cd = 'HVA1'.
*        ELSE.
*          wa_gi-prdn_plnt_cd = 'KVA1'.
*        ENDIF.

        wa_gi-prdn_plnt_cd = 'HVA1'.
        l_tot_gi = l_tot_gi + l_qty_gi.
      ENDIF.
** Stock
*# 11222010 Add add Last Stock by sjlee req. Daniel +
      IF NOT l_tot_stk IS INITIAL.
        l_qty_stk = l_tot_stk.
      ENDIF.
*# 11222010 -

      l_qty_stk = l_qty_stk + l_qty_gr - l_qty_gi.
      IF l_qty_stk > 0.
*      WRITE L_QTY_STK TO <FS_STK> NO-ZERO DECIMALS 0.
        <fs_stk> = l_qty_stk.
      ENDIF.
      l_tot_stk  =  l_tot_stk + l_qty_gr - l_qty_gi.

**      WRITE L_QTY_STK TO <FS_STK> NO-ZERO DECIMALS 0.
*      <FS_STK> = L_QTY_STK.
      l_cn = l_cn + 1.
      l_date = l_date + 1.

    ENDWHILE.

*    WRITE L_TOT_GR TO WA_GR-M0_PRDN_PRD_QTY NO-ZERO DECIMALS 0.
** Added on 08/29/13 by Furong
    wa_gr-prdn_plnt_cd = 'HVA1'.
** End on 08/29/13
    wa_gr-m0_prdn_prd_qty = l_tot_gr.
    wa_gr-cls_scn_cd = 'A'.

*    WRITE L_TOT_GI TO WA_GI-M0_PRDN_PRD_QTY NO-ZERO DECIMALS 0.
    wa_gi-m0_prdn_prd_qty = l_tot_gi.
    wa_gi-cls_scn_cd = 'B'.

*    WA_STK-M0_PRDN_PRD_QTY = L_TOT_STK.
    wa_stk-cls_scn_cd = 'C'.

* by Daniel on 08/18/10 {
    wa_stk-prdn_plnt_cd = 'HVA1'.
* }

*    APPEND WA_GR TO IT_PT_wSTOCK.
    IF l_tot_gi <> 0.
      APPEND wa_gi TO it_pt_wstock.
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
      APPEND wa_stk TO it_pt_wstock.

    ELSEIF l_tot_gi <> 0 OR  l_tot_gr <> 0.
      APPEND wa_stk TO it_pt_wstock.
    ENDIF.

  ENDLOOP.

  LOOP AT lt_mseg_gr.
    CLEAR: l_kdmat.
    lt_invoice_gr-matnr = lt_mseg_gr-matnr.
    lt_invoice_gr-zbudat = lt_mseg_gr-zbudat.
    lt_invoice_gr-menge = lt_mseg_gr-menge.

    SELECT SINGLE kdmat INTO l_kdmat
      FROM mkpf AS b
      INNER JOIN lips AS c
      ON b~xblnr = c~vbeln
      WHERE mblnr = lt_mseg_gr-mblnr.

    lt_invoice_gr-invoice = l_kdmat+0(10).
    COLLECT lt_invoice_gr.
    lt_invno-matnr = lt_invoice_gr-matnr.
    lt_invno-invoice = lt_invoice_gr-invoice.
    COLLECT lt_invno.
  ENDLOOP.

  SORT lt_invoice_gr BY matnr invoice zbudat.
** GR
  LOOP AT lt_matnr.

    LOOP AT lt_invno WHERE matnr = lt_matnr-matnr.
      CLEAR: l_tot_gr.
      CLEAR: wa_gr, it_pt_wstock.

      it_pt_wstock-mip_cd = lt_matnr-matnr.



* by Daniel on 02/10/11 {
*      IT_PT_WSTOCK-MKR_CD = 'H201'.
*      IT_PT_WSTOCK-SHOP_SCN_CD = 'E'.

* get vendor code for manufacturer information
      READ TABLE lt_eord WITH KEY matnr = lt_matnr-matnr.
      IF lt_eord-lifnr = 'ALVY'.
        it_pt_wstock-mkr_cd = 'H210'.
      ELSEIF lt_eord-lifnr = 'H27Z'.
        it_pt_wstock-mkr_cd = 'H135'.
      ELSE.
        it_pt_wstock-mkr_cd = 'H109'.
      ENDIF.

* get shop code based on type of material
      READ TABLE lt_stpo WITH KEY idnrk = lt_matnr-matnr.

      IF sy-subrc = 0.
        IF lt_stpo-zinfo = 'TM'.
          it_pt_wstock-shop_scn_cd = 'G'.
        ELSE.
          it_pt_wstock-shop_scn_cd = 'E'.
        ENDIF.
      ENDIF.

* by Daniel on 02/14/11 {
*    IT_PT_WSTOCK-PT_PRDN_PLNT_CD = 'HEA1'.
      IF it_pt_wstock-mkr_cd = 'H210'.
        it_pt_wstock-pt_prdn_plnt_cd = 'RTP2'.
      ELSEIF it_pt_wstock-mkr_cd = 'H109'.
        it_pt_wstock-pt_prdn_plnt_cd = 'RTP1'.
      ELSEIF it_pt_wstock-mkr_cd = 'H135'.
        it_pt_wstock-pt_prdn_plnt_cd = 'REW3'.
      ENDIF.
* }
* }




      it_pt_wstock-usf_scn_cd = 'V'.
      it_pt_wstock-wned_sqlt_scn_cd = 'A'.

* by Daniel on 08/18/10 {
*      PERFORM READ_NORMAL_CLASS USING LT_MATNR-MATNR 'EN_VEH_MODEL'
*                                  CHANGING  IT_PT_WSTOCK-PRDN_VEHL_CD.
* }

      it_pt_wstock-pno = lt_matnr-matnr.

      it_pt_wstock-invoice_cd = lt_invno-invoice.

      PERFORM read_normal_class USING lt_matnr-matnr 'EN_SPC14'
                                CHANGING  it_pt_wstock-mip_ln_cd.


      it_pt_wstock-crtn_yymm = w_yymm.

      CONCATENATE l_date_c l_time_c INTO it_pt_wstock-createdate.
      it_pt_wstock-changedate = it_pt_wstock-createdate.

      MOVE-CORRESPONDING it_pt_wstock TO wa_gr.

      l_date = l_date_from.
      l_cn = '01'.
      WHILE l_date <= p_date.
        CLEAR: l_qty_gr.

        CONCATENATE 'WA_GR-D' l_cn '_PRDN_PRD_QTY' INTO l_text.
        ASSIGN (l_text) TO <fs_gr>.

        LOOP AT lt_invoice_gr WHERE matnr = lt_invno-matnr
                            AND invoice = lt_invno-invoice
                            AND zbudat = l_date.

          l_qty_gr = l_qty_gr + lt_invoice_gr-menge.
        ENDLOOP.
        IF l_qty_gr > 0.

*          WRITE L_QTY_GR TO <FS_GR> NO-ZERO DECIMALS 0.
          <fs_gr> = l_qty_gr.
          l_tot_gr = l_tot_gr + l_qty_gr.
        ENDIF.
        l_cn = l_cn + 1.
        l_date = l_date + 1.

      ENDWHILE.

*      WRITE L_TOT_GR TO WA_GR-M0_PRDN_PRD_QTY NO-ZERO DECIMALS 0.
** Added on 08/29/13 by Furong
      wa_gr-prdn_plnt_cd = 'HVA1'.
** End on 085/29/13
      wa_gr-m0_prdn_prd_qty = l_tot_gr.
      wa_gr-cls_scn_cd = 'A'.

      IF l_tot_gr <> 0.
        APPEND wa_gr TO it_pt_wstock.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

** On 08/29/13
** sending PLANT(PRDN_PLNT_CD) and MODEL(PRDN_VEHL_CD).
  LOOP AT it_pt_wstock.
    CLEAR: it_pt_wstock-prdn_vehl_cd.

    SELECT SINGLE au~atwrt INTO it_pt_wstock-prdn_vehl_cd
      FROM ausp AS au INNER JOIN cabn AS ca
                              ON au~atinn = ca~atinn
      WHERE objek = it_pt_wstock-mip_cd
        AND klart = '001'
        AND ca~atnam = 'EN_VEH_MODEL'.

    IF it_pt_wstock-prdn_vehl_cd = 'GD'.
      it_pt_wstock-prdn_vehl_cd = 'IN'.
    ENDIF.

    MODIFY it_pt_wstock.
  ENDLOOP.
** On 08/29/13

*# 11232010 Add Logic ZTPP_PT_PSTOCK by sjlee +
  DATA : lt_pstock LIKE TABLE OF ztpp_pt_pstock WITH HEADER LINE,
         lv_spmon  LIKE s006-spmon.

  lv_spmon = p_date+0(6).
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_pstock
    FROM ztpp_pt_pstock
    WHERE crtn_yymm EQ lv_spmon
** Furong on 04/09/12
*      AND cls_scn_cd EQ 'B'
*      AND  prdn_plnt_cd = 'HVA1'.
        AND ( cls_scn_cd EQ 'B' AND  prdn_plnt_cd = 'HVA1' ).

** Changed by Park On 11/18/13
*            OR cls_scn_cd EQ 'D' AND  prdn_plnt_cd = 'KVA1'
*            OR cls_scn_cd EQ 'E' AND  prdn_plnt_cd = 'KVA1' ).
** End of change 11/18/13

** End on 04/09/12

  CLEAR it_pt_wstock.
  LOOP AT lt_pstock .
    MOVE-CORRESPONDING lt_pstock TO it_pt_wstock.
    APPEND it_pt_wstock.
  ENDLOOP.

* CH.Jeong on 10/21/2013 : Quantity (of Shipping & In-Transit)
  PERFORM get_qty_ship_n_transit   USING p_date.

  IF NOT it_wstock_qty[] IS INITIAL.
    APPEND  LINES OF it_wstock_qty  TO it_pt_wstock.
  ENDIF.
* End on 10/21/2013


*# 11232010 -
  SORT it_pt_wstock BY mip_cd.

ENDFORM.                    "GET_DATA


*---------------------------------------------------------------------*
*       FORM SEND_DATA                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM send_data.
  DATA: it_save LIKE TABLE OF ztpp_pt_wstock WITH HEADER LINE,
        l_flag(1),
        l_yymm(6),
        l_date_c(8).

  l_date_c = p_date.
  l_yymm =  l_date_c+0(6).

  IF it_pt_wstock[] IS INITIAL.
    MESSAGE i000 WITH 'No data'.
    EXIT.
  ENDIF.
  IF p_send IS INITIAL.
    LOOP AT it_pt_wstock.
      MOVE-CORRESPONDING it_pt_wstock TO it_save.
      it_save-zsdat = sy-datum.
      it_save-zstim = sy-uzeit.
      APPEND it_save.
      CLEAR: it_save.
    ENDLOOP.

    DELETE FROM ztpp_pt_wstock WHERE crtn_yymm = l_yymm.
    INSERT ztpp_pt_wstock FROM TABLE it_save ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
      MESSAGE i000 WITH 'Database table update error'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'Z_FPP_PT_WSTOCK'
      DESTINATION c_dest
      IMPORTING
        flag                  = w_result
      TABLES
        i_data                = it_pt_wstock
      EXCEPTIONS
        communication_failure = 1  MESSAGE w_msgtxt
        system_failure        = 2  MESSAGE w_msgtxt.

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

    LOOP AT it_pt_wstock.
      MOVE-CORRESPONDING it_pt_wstock TO it_save.
      it_save-zresult = l_flag.
      it_save-zmsg = w_msgtxt.
      it_save-zsdat = sy-datum.
      it_save-zstim = sy-uzeit.
      it_save-zedat = sy-datum.
      it_save-zetim = sy-uzeit.
      APPEND it_save.
      CLEAR: it_save.
    ENDLOOP.

    DELETE FROM ztpp_pt_wstock WHERE crtn_yymm = l_yymm.
    INSERT ztpp_pt_wstock FROM TABLE it_save ACCEPTING DUPLICATE KEYS.
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

** On 08/29/13
*  UPDATE ztpp_pt_wstock
*  SET prdn_plnt_cd    = 'HVA1'
*  WHERE prdn_plnt_cd EQ ''.
** end
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
*&      Form  CHECK_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_selection .
  IF p_date >= sy-datum.
    MESSAGE e000(zz) WITH 'Date should be before today'.
  ENDIF.

ENDFORM.                    " CHECK_SELECTION
