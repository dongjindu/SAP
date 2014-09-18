*----------------------------------------------------------------------*
*   INCLUDE ZXRSAU01                                                   *
*----------------------------------------------------------------------*

TABLES : coss, cosp, covp, pa0007, pa0001, ztco_mh_ws, t001p,
         ztebpp_deal_conv, mkpf, bkpf, likp, ekkn, ztbdiv, ztbkpf,
         qmfe, qmur, qmel, jest, ckmlmv001, ckmlmv003, ckmlmv013,
         rbkp, ce4h201_acct, ausp, qmih, plaf, ppc_rp, vbrp, konp, mseg,
         vrsd, d010inc, ztco_mh_dws, tstc, pnwtyh, pvwty, pnwtyv, konv,
         pnodtx, pnodid, posvid, edid4, edidc, ppc_comat, ztsd_um, t510,
         ztpp_repair_rate, ztpp_repair_mh, ztpp_veh_repair, ztfi_war_post,
         ckmlpp, ckmlcr, ckmlhd, ztfi_war_sales, zvbw_ppc_head_rp,
         mbewh.


DATA : l_mc05q00ntf LIKE mc05q00ntf OCCURS 0 WITH HEADER LINE, "QM noti
       l_zoxud10164 LIKE zoxud10164 OCCURS 0 WITH HEADER LINE, "QM noti
       l_zoxud10005 LIKE zoxud10005 OCCURS 0 WITH HEADER LINE, "PO Hist
       l_zoxud10006 LIKE zoxud10006 OCCURS 0 WITH HEADER LINE, "PO Hist2
       l_zoxud10008 LIKE zoxud10008 OCCURS 0 WITH HEADER LINE, "PO Cost
       l_mc02m_0itm LIKE mc02m_0itm OCCURS 0 WITH HEADER LINE, "PO ITEM
       l_mc02m_0scl LIKE mc02m_0scl OCCURS 0 WITH HEADER LINE, "PO SCL
       l_zamc02m_0scl LIKE zamc02m_0scl OCCURS 0 WITH HEADER LINE, "Ap
       l_zoxud10024 LIKE zoxud10024 OCCURS 0 WITH HEADER LINE,
       l_zoxud10020 LIKE zoxud10020 OCCURS 0 WITH HEADER LINE, "manifest
       l_zoxud10036 LIKE zoxud10036 OCCURS 0 WITH HEADER LINE,
       l_mc13vd0itm LIKE mc13vd0itm OCCURS 0 WITH HEADER LINE,
       l_zoxud10047 LIKE zoxud10047 OCCURS 0 WITH HEADER LINE, "vm
       l_zoxud10056 LIKE zoxud10056 OCCURS 0 WITH HEADER LINE, "Info-Rec
       l_zoxud10058 LIKE zoxud10058 OCCURS 0 WITH HEADER LINE, "Info-R D
       l_zoxud10065 LIKE zoxud10065 OCCURS 0 WITH HEADER LINE, "Unitcost
       l_zoxud10073 LIKE zoxud10073 OCCURS 0 WITH HEADER LINE, "shopcost
       l_zoxud10174 LIKE zoxud10174 OCCURS 0 WITH HEADER LINE, "shop sum
       l_kkbw_ccs_ds  LIKE kkbw_ccs_ds,  "Actual Cost Component Split
       l_kkbw_act_is1 LIKE kkbw_act_is1, "Actual Costing/Material Ledger
       l_mc03bf0 LIKE mc03bf0 OCCURS 0 WITH HEADER LINE, "mseg
       l_zoxud10134 LIKE zoxud10134 OCCURS 0 WITH HEADER LINE, "Hr Short
       l_zoxud10135 LIKE zoxud10135 OCCURS 0 WITH HEADER LINE, "Dy Short
       l_zoxud10136 LIKE zoxud10136 OCCURS 0 WITH HEADER LINE, "VRSD
       l_zoxud10097 LIKE zoxud10097 OCCURS 0 WITH HEADER LINE, "ERS stat
       l_zoxud10053 LIKE zoxud10053 OCCURS 0 WITH HEADER LINE, "MAT Ledg
       l_zoxud10002 LIKE zoxud10002 OCCURS 0 WITH HEADER LINE, "campaign
       l_zoxud10155 LIKE zoxud10155 OCCURS 0 WITH HEADER LINE, "CPZP
       l_zoxud10095 LIKE zoxud10095 OCCURS 0 WITH HEADER LINE,
       l_zoxud10106 LIKE zoxud10106 OCCURS 0 WITH HEADER LINE, "monthly claim
       l_zoxud10190 LIKE zoxud10190 OCCURS 0 WITH HEADER LINE, "material repair
       l_zoxud10192 LIKE zoxud10192 OCCURS 0 WITH HEADER LINE, "vehicle repair
       l_zoxud10196 LIKE zoxud10196 OCCURS 0 WITH HEADER LINE, "vehicle repair (CO)
       l_zoxud10197 LIKE zoxud10197 OCCURS 0 WITH HEADER LINE, "vehicle repair (EWS)
       l_zoxud10182 LIKE zoxud10182 OCCURS 0 WITH HEADER LINE,
       l_zoxud10183 LIKE zoxud10183 OCCURS 0 WITH HEADER LINE,
       l_zoxud10163 LIKE zoxud10163 OCCURS 0 WITH HEADER LINE,
       l_zoxud10203 LIKE zoxud10203 OCCURS 0 WITH HEADER LINE,
       l_zoxud10208 LIKE zoxud10208 OCCURS 0 WITH HEADER LINE.

*       l_ZOXUD10173 like ZOXUD10173 occurs 0 with header line. "219 master
*       l_ZOXUD10003 like ZOXUD10003 occurs 0 with header line. "ZPAY
DATA:  l_zoxud10159 LIKE zoxud10159 OCCURS 0 WITH HEADER LINE.
DATA:  l_zoxud10157 LIKE zoxud10157 OCCURS 0 WITH HEADER LINE. "ERP Operation KPI
DATA:  l_zoxud10001 LIKE zoxud10001 OCCURS 0 WITH HEADER LINE. "Dealer Allocation
DATA:  l_zoxud10181 LIKE zoxud10181 OCCURS 0 WITH HEADER LINE. "Campaign Master
DATA:  l_zoxud10022 LIKE zoxud10022 OCCURS 0 WITH HEADER LINE. "UM
DATA:  l_zoxud10130 LIKE zoxud10130 OCCURS 0 WITH HEADER LINE. "Est. sign-off vs alloc.
DATA:  l_zoxud10193 LIKE zoxud10193 OCCURS 0 WITH HEADER LINE. "OS&D Monitoring

DATA:  l_zoxud10213 LIKE zoxud10213 OCCURS 0 WITH HEADER LINE. "FSC WIP

DATA BEGIN OF lt_jlg_tbl OCCURS 50.
        INCLUDE STRUCTURE tbtc5.
DATA END OF lt_jlg_tbl.
DATA l_stepcnt(3) TYPE n.

DATA $tprog(4).


DATA: BEGIN OF lt_ppc_ord OCCURS 0,
        orderid LIKE ppc_head-orderid,
        reppoint_ext LIKE zvbw_ppc_head_rp-reppoint_ext,
      END OF lt_ppc_ord.

DATA: BEGIN OF lt_ppc_ord_all OCCURS 0,
        orderid LIKE ppc_head-orderid,
        reppoint_ext LIKE zvbw_ppc_head_rp-reppoint_ext,
      END OF lt_ppc_ord_all.

DATA: BEGIN OF lt_ppc_ord_rp_cnt OCCURS 0,
        orderid      LIKE ppc_head-orderid,
        spmon        TYPE spmon,
        reppoint_ext LIKE zvbw_ppc_head_rp-reppoint_ext,
        confquant    LIKE zvbw_ppc_head_rp-confquant,
      END OF lt_ppc_ord_rp_cnt.

DATA: BEGIN OF lt_ppc_ord_rp_cnt2 OCCURS 0,
        orderid      LIKE ppc_head-orderid,
        reppoint_ext LIKE zvbw_ppc_head_rp-reppoint_ext,
        confquant    LIKE zvbw_ppc_head_rp-confquant,
      END OF lt_ppc_ord_rp_cnt2.


DATA: lt_bw_ppc_head_rp LIKE zoxud10213 OCCURS 0 WITH HEADER LINE. "FSC WIP


DATA BEGIN OF lt_zoxud10065 OCCURS 0.       "Unit Cost Temp
        INCLUDE STRUCTURE l_zoxud10065.
DATA END OF lt_zoxud10065.

DATA BEGIN OF lt_zoxud10065_mip2 OCCURS 0.  "Unit Cost Temp
        INCLUDE STRUCTURE l_zoxud10065.
DATA END OF lt_zoxud10065_mip2.

DATA BEGIN OF lt_zoxud10174_mip1 OCCURS 0.       "shop Cost Temp
        INCLUDE STRUCTURE l_zoxud10174.
DATA END OF lt_zoxud10174_mip1.

DATA BEGIN OF lt_zoxud10174_mip2 OCCURS 0.  "shop Cost Temp
        INCLUDE STRUCTURE l_zoxud10174.
DATA END OF lt_zoxud10174_mip2.


DATA : l_lndwntime LIKE qmel-lndwntime,
       l_objnr LIKE jest-objnr.
DATA : lf_bqpim LIKE bqpim,
       lf_bqpex LIKE bqpex.
DATA: l_pnguid TYPE pnodid-pnguid.

DATA l_year  LIKE mbewh-lfgja.
DATA l_month LIKE mbewh-lfmon.
DATA l_poper LIKE ckmlcr-poper.
DATA l_date  LIKE sy-datum.
DATA l_knumh LIKE konh-knumh.
DATA l_matnr LIKE mbewh-matnr.
DATA l_plant LIKE mbewh-bwkey.
DATA l_bklas LIKE mbewh-bklas.
DATA l_vprsv LIKE ckmlcr-vprsv.
DATA l_fiscper  LIKE l_kkbw_ccs_ds-fiscper.
DATA l_manu_amt LIKE zoxud10174-manu_amt.
DATA l_sum_amt LIKE zoxud10174-manu_amt.
DATA l_sum_amt2 LIKE zoxud10174-manu_amt.
DATA l_manu_qty LIKE zoxud10174-manu_qty.
DATA: l_peinh LIKE mbew-peinh.
DATA: l_def_desc LIKE ztpp_repair_mh-def_desc.
DATA: l_bdatj TYPE bdatj,
      l_cdate TYPE sy-datum,
      l_mdate TYPE sy-datum,
      emonths TYPE i,
      l_fiscper1 LIKE l_kkbw_ccs_ds-fiscper.


DATA : l_zoxud10010 LIKE zoxud10010 OCCURS 0 WITH HEADER LINE."Prod Qty
DATA : l_dtfigl_4 LIKE dtfigl_4 OCCURS 0 WITH HEADER LINE.

* begin of PPC
DATA:  l_zoxud10039 LIKE zoxud10039 OCCURS 0 WITH HEADER LINE,
       l_zoxud10043 LIKE zoxud10043 OCCURS 0 WITH HEADER LINE.

DATA:  BEGIN OF i_ckmlmv013 OCCURS 0,
        pmatn LIKE ckmlmv013-pmatn,
        prwrk LIKE ckmlmv013-prwrk,
        verid LIKE ckmlmv013-verid,
        aufnr LIKE ckmlmv013-aufnr,
       END OF i_ckmlmv013.

DATA:  BEGIN OF i_plaf OCCURS 0,
        plnum LIKE plaf-plnum,
        pedtr LIKE plaf-pedtr,
       END OF i_plaf.

DATA:  BEGIN OF i_rp OCCURS 0,
        reppoint LIKE ppc_rp-reppoint,
        reppoint_ext LIKE ppc_rp-reppoint_ext,
       END OF i_rp.

* end fo PPC

DATA : l_icctrcsta1 LIKE icctrcsta1,
       l_icctrcst LIKE icctrcst,
       l_ptdw_pws LIKE ptdw_pws,
       l_ptdw_times LIKE ptdw_times,
       l_hrms_biw_pp1 LIKE hrms_biw_pp1,
       l_hrms_biw_py1 LIKE hrms_biw_py1,
       l_zoxud10070 LIKE zoxud10070,
       l_pa0001 LIKE pa0001 OCCURS 0 WITH HEADER LINE.

DATA : l_vrgng LIKE covp-vrgng,       "CO business transaction
       l_schkz LIKE pa0007-schkz,
       l_werks LIKE pa0001-werks,
       l_btrtl LIKE pa0001-btrtl,
       l_mosid LIKE t001p-mosid,
       l_anzsh LIKE ztco_mh_ws-anzsh,
       l_sdate LIKE sy-datum,
       l_edate LIKE sy-datum.

DATA : l_belnr LIKE bkpf-belnr,
       l_gjahr LIKE bkpf-gjahr,
       l_kzust LIKE ztmm_reval_rsn-kzust.

DATA : BEGIN OF lt_0co_pc_act_1 OCCURS 0.
        INCLUDE STRUCTURE l_kkbw_act_is1.
DATA : END OF lt_0co_pc_act_1.

* HBL and Port
DATA: BEGIN OF lt_ztbl_hbl OCCURS 0,
        bukrs LIKE ztbl-bukrs,
        frbnr LIKE l_zoxud10008-frbnr,
        zfsprtc LIKE ztbl-zfsprtc,
        zfaprtc LIKE ztbl-zfaprtc,
        zfbldt  LIKE ztbl-zfbldt,
        zfblno  LIKE ztbl-zfblno,
      END OF lt_ztbl_hbl.

* Reason Code
DATA: BEGIN OF lt_ztblit_hbl OCCURS 0,
        zfblno   LIKE ztblit-zfblno,
        ebeln   LIKE ztblit-ebeln,
        ebelp   LIKE ztblit-ebelp,
        inirsn  LIKE ztblit-inirsn,
        hmmarsn LIKE ztblit-hmmarsn,
        finrsn  LIKE ztblit-finrsn,
      END OF lt_ztblit_hbl.


* GL Customer and Vendor
DATA: BEGIN OF lt_cust_ven OCCURS 0,
        belnr LIKE bsik-belnr,
        gjahr LIKE bsik-gjahr,
        lifnr LIKE bsik-lifnr,
        kunnr LIKE bsid-kunnr,
        vbund LIKE bsik-vbund,
      END OF lt_cust_ven.

* FI Document Types
DATA: BEGIN OF lt_doc_type OCCURS 0,
        blart LIKE t003-blart,
        xkoad LIKE t003-xkoad,
        xkoak LIKE t003-xkoak,
      END OF lt_doc_type.

* Customer Trading Partner
DATA: BEGIN OF lt_cust_tp OCCURS 0,
        kunnr LIKE kna1-kunnr,
        vbund LIKE kna1-vbund,
      END OF lt_cust_tp.

* Vendor Trading partner
DATA: BEGIN OF lt_ven_tp OCCURS 0,
        lifnr LIKE lfa1-lifnr,
        vbund LIKE lfa1-vbund,
      END OF lt_ven_tp.

DATA:  BEGIN OF lt_ven_mat OCCURS 0,
         lifnr LIKE a018-lifnr,
         matnr LIKE a018-matnr,
       END OF lt_ven_mat.

DATA: BEGIN OF lt_eina OCCURS 0,
        matnr LIKE mara-matnr,
        lifnr LIKE lfa1-lifnr,
        ekgrp LIKE ekko-ekgrp,
        urzdt LIKE eina-urzdt,
        land1 LIKE lfa1-land1,
      END OF lt_eina.

DATA: BEGIN OF lt_a018 OCCURS 0,
        knumh LIKE a018-knumh,
        datab LIKE a018-datab,
        datbi LIKE a018-datbi,
        ekorg LIKE a018-ekorg,
      END OF lt_a018.

DATA: BEGIN OF lt_remark OCCURS 0,
        matnr   LIKE ztmm_h_short_eis-matnr,
        remark  LIKE ztmm_h_short_eis-remark,
        to_date LIKE ztmm_h_short_eis-to_date,
        to_time LIKE ztmm_h_short_eis-to_time,
      END OF lt_remark.

DATA: BEGIN OF lt_stg_range OCCURS 0,
        fieldname LIKE ztbw_stg_ranges-fieldname,
        low       LIKE ztbw_stg_ranges-low,
        high      LIKE ztbw_stg_ranges-high,
      END OF lt_stg_range.

* Repair Paint
DATA:  BEGIN OF i_rppt OCCURS 0,
        model LIKE ztpp_veh_repair-model,
        body_serial LIKE ztpp_veh_repair-body_serial,
        ref_date LIKE ztpp_veh_repair-ref_date,
        ref_compl LIKE ztpp_veh_repair-ref_compl,
        cnt TYPE i,
       END OF i_rppt.
DATA: wa_rppt LIKE i_rppt.

RANGES: r_matnr FOR ztmm_hour_short-matnr,
        r_mtart FOR ztmm_hour_short-mtart,
        r_matkl FOR ztmm_hour_short-matkl,
        r_dispo FOR ztmm_hour_short-dispo,
        r_lifnr FOR ztmm_hour_short-lifnr.
DATA : l_sign(1), l_option(2).
DATA : l_tabix LIKE sy-tabix.
DATA : l_tabix1 LIKE sy-tabix.
DATA : l_tabix2 LIKE sy-tabix.
DATA : l_tabname LIKE dd02d-tabname.
DATA : wa_ftab(72) TYPE c,
       l_ftab    LIKE TABLE OF wa_ftab.

DATA : l_bvalt  LIKE l_zoxud10053-bvalt,
       l_pmatn  LIKE l_zoxud10053-pmatn,
       l_prwrk  LIKE l_zoxud10053-prwrk,
       l_verid  LIKE ckmlmv013-verid.

DATA : l_ebeln  LIKE ekko-ebeln,
       l_lifnr  LIKE ekko-lifnr.

DATA l_jobname LIKE ztbw_op_kpi_job-start_jobname.
RANGES r_date FOR sy-datum.

* eleminating CR LF
FIELD-SYMBOLS: <cr>, <lf>, <crsp>.
DATA : l_hex_cr TYPE xstring. " VALUE '0A'.
DATA : l_hex_lf TYPE xstring. " VALUE '0D'.
DATA x_crsp     TYPE xstring.
DATA :    s_crsp       TYPE string.

l_hex_cr = '0A'.
l_hex_lf = '0D'.

x_crsp = '0A20'.

IF sy-saprl < '700'.
  ASSIGN l_hex_cr TO <cr>.  " CASTING.
  ASSIGN l_hex_lf TO <lf>.
  ASSIGN x_crsp TO <crsp>.
ELSE.
  CALL FUNCTION 'ECATT_CONV_XSTRING_TO_STRING'
    EXPORTING
      im_xstring = x_crsp  " variable type string
    IMPORTING
      ex_string  = s_crsp. " variable type xstring
ENDIF.

*DATA : XKTOKS  LIKE SKA1-KTOKS.
*DATA : I_IOBJACCT LIKE IOBJACCT.
*DATA : A_TABIX LIKE SY-TABIX.


CASE i_datasource.

  WHEN '1CL_OEQU019'.
    LOOP AT c_t_data INTO l_zoxud10208.
      l_tabix = sy-tabix.

      CLEAR ztebpp_deal_conv.
      SELECT SINGLE *
        FROM ztebpp_deal_conv
       WHERE  old_dealer = l_zoxud10208-p_dist_code002.
      IF sy-subrc EQ 0.
        l_zoxud10208-new_dealer = ztebpp_deal_conv-new_dealer.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10208 INDEX l_tabix.

    ENDLOOP.

  WHEN '1CL_OEQU007'.
    LOOP AT c_t_data INTO l_zoxud10070.
      l_tabix = sy-tabix.

      CLEAR ztebpp_deal_conv.
      SELECT SINGLE *
        FROM ztebpp_deal_conv
       WHERE  old_dealer = l_zoxud10070-p_dist_code002.
      IF sy-subrc EQ 0.
        l_zoxud10070-new_dealer = ztebpp_deal_conv-new_dealer.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10070 INDEX l_tabix.

    ENDLOOP.

  WHEN '1CL_OEQU014'.
    LOOP AT c_t_data INTO l_zoxud10047.
      l_tabix = sy-tabix.

      CLEAR ztebpp_deal_conv.
      SELECT SINGLE *
        FROM ztebpp_deal_conv
       WHERE  old_dealer = l_zoxud10047-p_dist_code002.
      IF sy-subrc EQ 0.
        l_zoxud10047-new_dealer = ztebpp_deal_conv-new_dealer.
      ENDIF.

      IF l_zoxud10047-p_engine_no005 NE space.
        CLEAR ausp.
        SELECT SINGLE atwrt
          INTO (l_zoxud10047-en_item_code001)
          FROM ausp
         WHERE objek = l_zoxud10047-p_engine_no005
           AND atinn = '0000003583'.

        IF sy-subrc NE 0.
          CLEAR ausp.
          SELECT SINGLE atwrt
            INTO (l_zoxud10047-en_item_code001)
            FROM ausp
           WHERE objek = l_zoxud10047-p_work_order001
             AND atinn = '0000002510'.
        ENDIF.
      ENDIF.

      DATA: $atflv LIKE ausp-atflv,
            $i TYPE i, $date(8).

      CLEAR: ausp, $atflv, $date.
      SELECT SINGLE atflv
        INTO $atflv
        FROM ausp
       WHERE objek = l_zoxud10047-p_work_order001
         AND atinn = '0000002801'.
      $i =  $atflv. $date = $i.
      l_zoxud10047-p_wo_create_020 = $date.

      CLEAR: ausp, $atflv, $date.
      SELECT SINGLE atflv
        INTO $atflv
        FROM ausp
       WHERE objek = l_zoxud10047-p_work_order001
         AND atinn = '0000002803'.
      $i =  $atflv. $date = $i.
      l_zoxud10047-p_wo_modi_da019 = $date.

      MODIFY c_t_data FROM l_zoxud10047 INDEX l_tabix.

    ENDLOOP.

****** fields Add-on Data Source .
  WHEN '0CO_OM_CCA_9'.
    LOOP AT c_t_data INTO l_icctrcsta1.
*      L_TABIX = SY-TABIX.
*
*      SELECT SINGLE VRGNG INTO L_VRGNG
*        FROM COVP
*       WHERE  KOKRS = L_ICCTRCSTA1-KOKRS
*         AND  BELNR = L_ICCTRCSTA1-BELNR
*         AND  BUZEI = L_ICCTRCSTA1-BUZEI.
*
*      L_ICCTRCSTA1-VRGNG = L_VRGNG.
*
**
*      MODIFY C_T_DATA FROM L_ICCTRCSTA1 INDEX L_TABIX.

    ENDLOOP.

  WHEN '0CO_OM_CCA_1'.
    LOOP AT c_t_data INTO l_icctrcst.

    ENDLOOP.

  WHEN '0HR_PT_1'.
    LOOP AT c_t_data INTO l_ptdw_pws.
      l_tabix = sy-tabix.
      CLEAR pa0007.
*      BREAK-POINT.
* to get Administrator for Time Recording
      SELECT SINGLE sachz INTO l_ptdw_pws-sachz
        FROM pa0001
       WHERE  pernr = l_ptdw_pws-pernr
         AND  begda <= l_ptdw_pws-kday
         AND  endda >= l_ptdw_pws-kday.
* to get work schedule
      SELECT SINGLE schkz INTO l_ptdw_pws-schkz
        FROM pa0007
       WHERE  pernr = l_ptdw_pws-pernr
         AND  begda <= l_ptdw_pws-kday
         AND  endda >= l_ptdw_pws-kday.

* to get Personnel Subarea Grouping for Work Schedules
      SELECT SINGLE mosid INTO l_mosid
       FROM t001p
      WHERE werks = l_ptdw_pws-pers_area_wp
        AND btrtl = l_ptdw_pws-pers_subarea_wp.
*

* to get shift information
* new DWS
*      DATA $tprog(4).
      CALL FUNCTION 'Z_CO_GET_DWS_IG'
        EXPORTING
          schkz                          = l_ptdw_pws-schkz
          datum                          = l_ptdw_pws-kday
        IMPORTING
          tprog                          = $tprog
        EXCEPTIONS
          not_found_work_schedule_rules  = 1
          invalid_date                   = 2
          not_found_period_work_schedule = 3
          OTHERS                         = 4.



      l_ptdw_pws-schkz = $tprog.
      CLEAR ztco_mh_dws.
      SELECT SINGLE *
        FROM ztco_mh_dws
       WHERE mosid = l_mosid
*         AND schkz = $tprog
         AND schkz = l_ptdw_pws-schkz
         AND zsdat <= l_ptdw_pws-kday
         AND zedat >= l_ptdw_pws-kday.
      IF sy-subrc EQ 0.
        l_ptdw_pws-anzsh = ztco_mh_dws-zshif.
      ENDIF.

*
      MODIFY c_t_data FROM l_ptdw_pws INDEX l_tabix.

    ENDLOOP.

  WHEN '0HR_PT_2'.
    LOOP AT c_t_data INTO l_ptdw_times.
      l_tabix = sy-tabix.
      CLEAR pa0007.
*      BREAK-POINT.
* to get Administrator for Time Recording
      SELECT SINGLE sachz INTO l_ptdw_times-sachz
        FROM pa0001
       WHERE  pernr = l_ptdw_times-pernr
         AND  begda <= l_ptdw_times-kday
         AND  endda >= l_ptdw_times-kday.
* to get work schedule
      SELECT SINGLE schkz INTO l_ptdw_times-schkz
        FROM pa0007
       WHERE  pernr = l_ptdw_times-pernr
         AND  begda <= l_ptdw_times-kday
         AND  endda >= l_ptdw_times-kday.

* to get Personnel Subarea Grouping for Work Schedules
      SELECT SINGLE mosid INTO l_mosid
       FROM t001p
      WHERE werks = l_ptdw_times-pers_area_wp
        AND btrtl = l_ptdw_times-pers_subarea_wp.
*
* to get shift information
* new DWS
      CALL FUNCTION 'Z_CO_GET_DWS_IG'
        EXPORTING
          schkz                          = l_ptdw_times-schkz
          datum                          = l_ptdw_times-kday
        IMPORTING
          tprog                          = $tprog
        EXCEPTIONS
          not_found_work_schedule_rules  = 1
          invalid_date                   = 2
          not_found_period_work_schedule = 3
          OTHERS                         = 4.


      l_ptdw_times-schkz = $tprog.
      CLEAR ztco_mh_dws.
      SELECT SINGLE *
        FROM ztco_mh_dws
       WHERE mosid = l_mosid
*         AND schkz = $tprog
         AND schkz = l_ptdw_times-schkz
         AND zsdat <= l_ptdw_times-kday
         AND zedat >= l_ptdw_times-kday.
      IF sy-subrc EQ 0.
        l_ptdw_times-anzsh = ztco_mh_dws-zshif.
      ENDIF.
*
*to get pay scale info.
      SELECT SINGLE trfar trfgb trfgr trfst
      INTO (l_ptdw_times-trfar, l_ptdw_times-trfgb, l_ptdw_times-trfgr, l_ptdw_times-trfst)
      FROM pa0008
       WHERE  pernr = l_ptdw_times-pernr
         AND  begda <= l_ptdw_times-kday
         AND  endda >= l_ptdw_times-kday.
*
      MODIFY c_t_data FROM l_ptdw_times INDEX l_tabix.

    ENDLOOP.

  WHEN '0HR_PY_PP_1'.    "Auditing Information for Posting Transfer
    LOOP AT c_t_data INTO l_hrms_biw_pp1.
      l_tabix = sy-tabix.
      CLEAR pa0007. CLEAR pa0001. REFRESH l_pa0001.
*      BREAK-POINT.
* to get work schedule
      SELECT SINGLE schkz INTO l_hrms_biw_pp1-schkz
        FROM pa0007
       WHERE  pernr = l_hrms_biw_pp1-pernr
         AND  begda <= l_hrms_biw_pp1-fpend
         AND  endda >= l_hrms_biw_pp1-fpend.

* to get Personnel Subarea Grouping for Work Schedules
      SELECT SINGLE mosid INTO l_mosid
       FROM t001p
      WHERE werks = l_hrms_biw_pp1-werks
        AND btrtl = l_hrms_biw_pp1-btrtl.
*
* to get shift information
      SELECT SINGLE anzsh INTO l_hrms_biw_pp1-anzsh
        FROM ztco_mh_ws
       WHERE schkz = l_hrms_biw_pp1-schkz
         AND mosid = l_mosid.
*
* to get job code
*      SELECT SINGLE STELL INTO l_HRMS_BIW_PP1-STELL
*        FROM pa0001
*       WHERE  pernr = l_HRMS_BIW_PP1-pernr
*         AND  begda <= l_HRMS_BIW_PP1-FPEND
*         AND  endda >= l_HRMS_BIW_PP1-FPEND.
      l_sdate = l_hrms_biw_pp1-fpend - 13.
      SELECT * INTO TABLE l_pa0001 FROM pa0001
       WHERE  pernr = l_hrms_biw_pp1-pernr
         AND  begda <= l_sdate
         AND  endda >= l_hrms_biw_pp1-fpend.
      IF sy-subrc EQ 0.                                " No change
        SELECT SINGLE stell INTO l_hrms_biw_pp1-stell
          FROM pa0001
         WHERE  pernr = l_hrms_biw_pp1-pernr
           AND  begda <= l_hrms_biw_pp1-fpend
           AND  endda >= l_hrms_biw_pp1-fpend.
      ELSE.
        REFRESH l_pa0001.
        SELECT * INTO TABLE l_pa0001 FROM pa0001
         WHERE  pernr = l_hrms_biw_pp1-pernr
           AND  endda > l_sdate.
        IF sy-subrc EQ 0.
          CLEAR l_pa0001.
          READ TABLE l_pa0001 WITH KEY
                        plans = l_hrms_biw_pp1-plans
                        orgeh = l_hrms_biw_pp1-orgeh
                        kostl = l_hrms_biw_pp1-kostl.
          IF sy-subrc EQ 0.
            l_hrms_biw_pp1-stell = l_pa0001-stell.
          ELSE.
            SELECT SINGLE stell INTO l_hrms_biw_pp1-stell
              FROM pa0001
             WHERE  pernr = l_hrms_biw_pp1-pernr
               AND  begda <= l_hrms_biw_pp1-fpend
               AND  endda >= l_hrms_biw_pp1-fpend.
          ENDIF.
        ELSE.
          SELECT SINGLE stell INTO l_hrms_biw_pp1-stell
            FROM pa0001
           WHERE  pernr = l_hrms_biw_pp1-pernr
             AND  begda <= l_hrms_biw_pp1-fpend
             AND  endda >= l_hrms_biw_pp1-fpend.
        ENDIF.
      ENDIF.

      MODIFY c_t_data FROM l_hrms_biw_pp1 INDEX l_tabix.

    ENDLOOP.

  WHEN '0HR_PY_1'.
    LOOP AT c_t_data INTO l_hrms_biw_py1.
      l_tabix = sy-tabix.

*      break-point.
*      SELECT SINGLE VRGNG INTO L_VRGNG
*        FROM COVP
*       WHERE  KOKRS = L_ICCTRCSTA1-KOKRS
*         AND  BELNR = L_ICCTRCSTA1-BELNR
*         AND  BUZEI = L_ICCTRCSTA1-BUZEI.
*
*      L_ICCTRCSTA1-VRGNG = L_VRGNG.
*
**
*      MODIFY C_T_DATA FROM L_ICCTRCSTA1 INDEX L_TABIX.

    ENDLOOP.

  WHEN 'ZVBW_QMEL'.
    LOOP AT c_t_data INTO l_zoxud10164.
      l_tabix = sy-tabix.

*begin of gathering the downtime
      CLEAR qmel.
      SELECT SINGLE lndwntime INTO l_lndwntime
        FROM qmel
       WHERE  qmnum = l_zoxud10164-qmnum.
      IF sy-subrc EQ 0.
        l_zoxud10164-lndwntime = l_lndwntime.
      ENDIF.
*end of gathering the downtime

*begin of deletion flag
      CLEAR jest.
      SELECT SINGLE *
        FROM jest
       WHERE objnr = l_zoxud10164-objnr
         AND stat = 'I0076'
         AND inact <> 'X'.
      IF sy-subrc EQ 0.
        l_zoxud10164-kzloesch = 'X'.
      ENDIF.
*end of deletion flag

*begin of ppm user status flag
      DATA: l_inact LIKE jest-inact.
      CLEAR l_inact.

      CASE l_zoxud10164-qmart.
        WHEN 'Q3'.
          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10164-objnr
           AND stat = 'E0008'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10164-zchkppm = 'X'.
          ENDIF.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10164-objnr
           AND stat = 'E0004'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10164-zsyswsn = 'X'.
          ENDIF.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10164-objnr
           AND stat = 'I0072'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10164-zsysstat = 'X'.
          ENDIF.

        WHEN 'Q4'.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10164-objnr
           AND stat = 'E0001'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10164-zchkppm = 'X'.
          ENDIF.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10164-objnr
           AND stat = 'I0072'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10164-zsysstat = 'X'.
          ENDIF.

        WHEN OTHERS.

      ENDCASE.

*end of ppm user status flag

      IF l_zoxud10164-lifnum EQ ' '.
        lf_bqpim-matnr = l_zoxud10164-matnr.
*lf_bqpim-cuobj = l_ZOXUD10164-cuobj. "blank
        lf_bqpim-werks = l_zoxud10164-mawerk.               "P001
*      LF_BQPIM-NEMNG = ABS( T_KIS1-MENGE ). "PriceUnit
*      LF_BQPIM-MEINS = T_KIS1-MEEHT. "UoM
*      LF_BQPIM-LMEIN = T_KIS1-MEEHT. "Units of Measure in WM
        lf_bqpim-nedat = l_zoxud10164-erdat. "Creation date on
        lf_bqpim-bstyp = 'B'.          "Purchase requisition
        lf_bqpim-pstyp = '0'.          "Item category in PO
        lf_bqpim-vorga = 'B'.          "Transaction/event
        lf_bqpim-bqpra = '1'.          "Selection of source with price
        lf_bqpim-noaus = space.        "No box listing sources of supply
        lf_bqpim-liste = 'X'.                               "553647
        lf_bqpim-beskz = 'F'.         "External procurement
        lf_bqpim-msgno = 'X'.         "keine Nachricht
        lf_bqpim-noquu = 'X'.         "Do not update quota arrangement
        lf_bqpim-novrt = 'X'.   "Do not search for outlineagreement
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

        l_zoxud10164-lifnum = lf_bqpex-flief.
*      ITAB-INFNR = LF_BQPEX-INFNR.

      ENDIF.

* Begin of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMFE
      CLEAR qmfe.
      SELECT SINGLE otkat otgrp oteil fekat fegrp fecod
      INTO (l_zoxud10164-otkat,
            l_zoxud10164-otgrp,
            l_zoxud10164-oteil,
            l_zoxud10164-fekat,
            l_zoxud10164-fegrp,
            l_zoxud10164-fecod)
       FROM qmfe
       WHERE qmnum = l_zoxud10164-qmnum
         AND fenum = '0001'.
* End of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMFE

* Begin of to get URKAT, URGRP, URCOD, URTXT from table
* QMUR
      CLEAR qmur.
      SELECT SINGLE urkat urgrp urcod urtxt
      INTO (l_zoxud10164-urkat,
            l_zoxud10164-urgrp,
            l_zoxud10164-urcod,
            l_zoxud10164-urtxt)
       FROM qmur
       WHERE qmnum = l_zoxud10164-qmnum
         AND fenum = '0001'
         AND urnum = '0001'.
* End of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMUR

* Begin of to get AUSVN,AUZTV from table
* QMIH
      CLEAR qmih.
      SELECT SINGLE ausvn auztv
      INTO (l_zoxud10164-ausvn,
            l_zoxud10164-auztv)
       FROM qmih
       WHERE qmnum = l_zoxud10164-qmnum.
* End of to get AUSVN,AUZTV from table
* QMIH

      MODIFY c_t_data FROM l_zoxud10164 INDEX l_tabix.

    ENDLOOP.

  WHEN '2LIS_05_Q0NOTIF'.
    LOOP AT c_t_data INTO l_mc05q00ntf.
      l_tabix = sy-tabix.

      SELECT SINGLE lndwntime INTO l_lndwntime
        FROM qmel
       WHERE  qmnum = l_mc05q00ntf-qmnum.

      l_mc05q00ntf-lndwntime = l_lndwntime.

      IF l_mc05q00ntf-lifnum EQ ' '.
        lf_bqpim-matnr = l_mc05q00ntf-matnr.
*lf_bqpim-cuobj = l_mc05q00ntf-cuobj. "blank
        lf_bqpim-werks = l_mc05q00ntf-mawerk.               "P001
*      LF_BQPIM-NEMNG = ABS( T_KIS1-MENGE ). "PriceUnit
*      LF_BQPIM-MEINS = T_KIS1-MEEHT. "UoM
*      LF_BQPIM-LMEIN = T_KIS1-MEEHT. "Units of Measure in WM
        lf_bqpim-nedat = l_mc05q00ntf-erdat. "Creation date on
        lf_bqpim-bstyp = 'B'.          "Purchase requisition
        lf_bqpim-pstyp = '0'.          "Item category in PO
        lf_bqpim-vorga = 'B'.          "Transaction/event
        lf_bqpim-bqpra = '1'.          "Selection of source with price
        lf_bqpim-noaus = space.        "No box listing sources of SUPPLY
        lf_bqpim-liste = 'X'.                               "553647
        lf_bqpim-beskz = 'F'.         "External procurement
        lf_bqpim-msgno = 'X'.         "keine Nachricht
        lf_bqpim-noquu = 'X'.         "Do not update quota arrangement
        lf_bqpim-novrt = 'X'.   "Do not search for outlineagreement
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

        l_mc05q00ntf-lifnum = lf_bqpex-flief.
*      ITAB-INFNR = LF_BQPEX-INFNR.


      ENDIF.

*
      MODIFY c_t_data FROM l_mc05q00ntf INDEX l_tabix.

    ENDLOOP.

*Begin of PO History for BW ****
  WHEN 'Z_MM_ZEKBE_EKKO' OR 'Z_MM_ZEKBEH_EKKO'.
    LOOP AT c_t_data INTO l_zoxud10005.
      l_tabix = sy-tabix.
* GR Posting Date, Inbound Delivery, BoL#, ASN#
      CLEAR: bkpf-belnr, bkpf-gjahr, bkpf-xblnr,
             rbkp-bktxt, rbkp-rmwwr.

      IF l_zoxud10005-vgabe EQ '1'.         "GR#
        bkpf-belnr = l_zoxud10005-belnr.
        bkpf-gjahr = l_zoxud10005-gjahr.
      ELSEIF l_zoxud10005-vgabe EQ '2'
          OR l_zoxud10005-vgabe EQ '3'.
* Invoice Document Type & BoL#
        IF l_zoxud10005-belnr NE space.
          SELECT SINGLE blart xblnr bktxt rmwwr
            INTO (l_zoxud10005-zzblart, bkpf-xblnr,
                  rbkp-bktxt, rbkp-rmwwr)
            FROM rbkp
           WHERE belnr = l_zoxud10005-belnr
             AND gjahr = l_zoxud10005-gjahr.
        ENDIF.
        IF l_zoxud10005-lfbnr NE space.   "Reference#
          bkpf-belnr = l_zoxud10005-lfbnr.
          bkpf-gjahr = l_zoxud10005-lfgja.
        ENDIF.
* Check Gross Amount
        IF rbkp-rmwwr EQ 0.
          l_zoxud10005-dmbtr = 0.
          l_zoxud10005-wrbtr = 0.
        ENDIF.
* Revaluation Reason Code
        IF l_zoxud10005-vgabe EQ '3'
          AND l_zoxud10005-belnr NE space.
          IF l_belnr = l_zoxud10005-belnr
            AND l_gjahr = l_zoxud10005-gjahr.
            l_zoxud10005-zzkzust = l_kzust.
          ELSE.
            l_belnr = l_zoxud10005-belnr.
            l_gjahr = l_zoxud10005-gjahr.
            CLEAR l_kzust.
            SELECT SINGLE kzust INTO l_zoxud10005-zzkzust
              FROM ztmm_reval_rsn
             WHERE belnr = l_zoxud10005-belnr
               AND gjahr = l_zoxud10005-gjahr.
            IF sy-subrc EQ 0.
              l_kzust = l_zoxud10005-zzkzust.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF bkpf-belnr EQ space.
        l_zoxud10005-zzbolnr = bkpf-xblnr.
      ELSE.
        SELECT SINGLE a~budat a~xblnr b~borgr_grp a~frbnr  "b~bolnr
                      a~bktxt
                      b~traid b~verur c~kdmat
                      c~lgmng
          INTO (l_zoxud10005-zzbudat,
                l_zoxud10005-zzxblnr,
                l_zoxud10005-zzborgr_grp,
                l_zoxud10005-zzbolnr,
                l_zoxud10005-zzbktxt,
                l_zoxud10005-zztraid,
                l_zoxud10005-zzverur,
                l_zoxud10005-zzkdmat,
                l_zoxud10005-zzlgmng)
          FROM mkpf AS a LEFT OUTER JOIN likp AS b
            ON b~vbeln = a~xblnr
               LEFT OUTER JOIN lips AS c
            ON c~vbeln = a~xblnr
           AND vgbel = l_zoxud10005-ebeln
           AND vgpos = l_zoxud10005-ebelp
         WHERE a~mblnr = bkpf-belnr
           AND a~mjahr = bkpf-gjahr.
        IF l_zoxud10005-shkzg EQ 'H'.
          l_zoxud10005-zzlgmng = l_zoxud10005-zzlgmng * '-1'.
        ENDIF.
      ENDIF.

* B/L, Trans Method, ETD
      IF NOT l_zoxud10005-zzxblnr IS INITIAL.
        SELECT SINGLE zfhblno zfvia zfetd
          INTO (l_zoxud10005-zzfhblno, l_zoxud10005-zzfvia,
                l_zoxud10005-zzfetd)
          FROM ztbl
         WHERE bukrs = 'H201'
           AND zfhblno = l_zoxud10005-zzverur(24).
      ENDIF.

* Standard Item & GR Non Valuated Item
      IF l_zoxud10005-pstyp EQ '0' AND l_zoxud10005-weunb EQ 'X'
        AND l_zoxud10005-vgabe EQ '1' AND l_zoxud10005-dmbtr EQ 0.
        l_zoxud10005-dmbtr = l_zoxud10005-netpr * l_zoxud10005-menge
                           / l_zoxud10005-peinh.
        l_zoxud10005-wrbtr = l_zoxud10005-netpr * l_zoxud10005-menge
                           / l_zoxud10005-peinh.
      ENDIF.

      TRANSLATE l_zoxud10005-zzborgr_grp TO UPPER CASE.

* G/L Account, Order no ...
      IF l_zoxud10005-knttp EQ space.
        l_zoxud10005-zzfipos = l_zoxud10005-fipos.
        l_zoxud10005-zzfistl = l_zoxud10005-fistl.
        l_zoxud10005-zzgeber = l_zoxud10005-geber.
      ELSE.
        IF ekkn-ebeln NE  l_zoxud10005-ebeln
          OR ekkn-ebelp NE  l_zoxud10005-ebelp.
          CLEAR ekkn.

          ekkn-ebeln = l_zoxud10005-ebeln.
          ekkn-ebelp = l_zoxud10005-ebelp.

          PERFORM get_ekkn USING ekkn-ebeln ekkn-ebelp l_zamc02m_0scl.

        ENDIF.
        MOVE-CORRESPONDING l_zamc02m_0scl TO  l_zoxud10005.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10005 INDEX l_tabix.
    ENDLOOP.


*End of PO History for BW ****

*Begin of PO Delivery Cost for BW ****
  WHEN 'Z_MM_ZEKBZ_EKKO' OR 'Z_MM_ZEKBZH_EKKO'.
    LOOP AT c_t_data INTO l_zoxud10008.
      l_tabix = sy-tabix.
* Title Code
      IF l_zoxud10008-bewtp = 'M' AND
        l_zoxud10008-kschl EQ 'FRA1'.
        IF  ztbdiv-ebeln EQ l_zoxud10008-ebeln
          AND ztbdiv-ebelp EQ l_zoxud10008-ebelp
          AND bkpf-belnr   EQ l_zoxud10008-belnr.
        ELSE.
          ztbdiv-ebeln = l_zoxud10008-ebeln.
          ztbdiv-ebelp = l_zoxud10008-ebelp.
          bkpf-belnr   = l_zoxud10008-belnr.

          CLEAR: ztbdiv-zfcstgrp, ztbdiv-zfcd,
                 ztbdiv-zuonr, ztbkpf-xblnr.

          SELECT SINGLE a~zfcstgrp a~zfcd a~zuonr b~xblnr
            INTO (ztbdiv-zfcstgrp, ztbdiv-zfcd,
                  ztbdiv-zuonr, ztbkpf-xblnr)
            FROM ztbdiv AS a
            INNER JOIN ztbkpf AS b
               ON a~belnr    = b~belnr
              AND a~bukrs = b~bukrs
              AND a~gjahr = b~gjahr
            WHERE a~ebeln     = l_zoxud10008-ebeln
              AND a~ebelp     = l_zoxud10008-ebelp
              AND a~cond_type = l_zoxud10008-kschl
              AND b~zfacdo    = l_zoxud10008-belnr.
          IF sy-subrc NE 0.
            SELECT SINGLE a~zfcstgrp a~zfcd a~zuonr b~xblnr
              INTO (ztbdiv-zfcstgrp, ztbdiv-zfcd,
                    ztbdiv-zuonr, ztbkpf-xblnr)
              FROM ztbdiv AS a
              INNER JOIN ztbkpf AS b
                 ON a~belnr    = b~belnr
                AND a~bukrs = b~bukrs
                AND a~gjahr = b~gjahr
              WHERE a~ebeln     = l_zoxud10008-ebeln
                AND a~ebelp     = l_zoxud10008-ebelp
                AND a~cond_type = l_zoxud10008-kschl
                AND b~budat     = l_zoxud10008-budat
                AND a~dmbtr     = l_zoxud10008-dmbtr.
            IF sy-subrc NE 0. "AND l_zoxud10008-shkzg = 'H'."Credit Memo
              CLEAR rbkp-kidno.
              SELECT SINGLE kidno INTO rbkp-kidno
                FROM rbkp
               WHERE belnr = l_zoxud10008-belnr
                 AND gjahr = l_zoxud10008-gjahr.
              IF rbkp-kidno NE space.
                SELECT SINGLE a~zfcstgrp a~zfcd a~zuonr b~xblnr
                  INTO (ztbdiv-zfcstgrp, ztbdiv-zfcd,
                        ztbdiv-zuonr, ztbkpf-xblnr)
                  FROM ztbdiv AS a
                 INNER JOIN ztbkpf AS b
                    ON a~belnr     = b~belnr
                   AND a~bukrs     = b~bukrs
                   AND a~gjahr     = b~gjahr
                 WHERE a~ebeln     = l_zoxud10008-ebeln
                   AND a~ebelp     = l_zoxud10008-ebelp
                   AND a~cond_type = l_zoxud10008-kschl
                   AND a~zuonr     = rbkp-kidno(18).
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        l_zoxud10008-zfcstgrp = ztbdiv-zfcstgrp.
        l_zoxud10008-zfcd     = ztbdiv-zfcd.
        l_zoxud10008-frbnr    = ztbdiv-zuonr.  "BOL No
        l_zoxud10008-xblnr    = ztbkpf-xblnr.  "Reference No
      ELSEIF l_zoxud10008-kschl EQ 'ZOA1'.
        l_zoxud10008-zfcstgrp = '006'.
        l_zoxud10008-zfcd     = '001'.  "Duty
      ENDIF.

* Invoice Document Type
      IF ( l_zoxud10008-vgabe EQ '2'
         OR l_zoxud10008-vgabe EQ '3' )
         AND l_zoxud10008-belnr NE space.

        CLEAR: bkpf-xblnr, rbkp-bktxt, rbkp-rmwwr.
        SELECT SINGLE blart xblnr bktxt rmwwr
          INTO (l_zoxud10008-zzblart, bkpf-xblnr,
                rbkp-bktxt, rbkp-rmwwr)
          FROM rbkp
         WHERE belnr = l_zoxud10008-belnr
           AND gjahr = l_zoxud10008-gjahr.
* BoL#
        IF l_zoxud10008-frbnr EQ space.
          l_zoxud10008-frbnr = bkpf-xblnr.
        ENDIF.
* Arrival port and Loading Port
        IF l_zoxud10008-frbnr NE space.

          READ TABLE lt_ztbl_hbl WITH KEY bukrs = l_zoxud10008-bukrs
                                          frbnr = l_zoxud10008-frbnr
                                         BINARY SEARCH.
          IF sy-subrc EQ 0.
            l_zoxud10008-zzfsprtc = lt_ztbl_hbl-zfsprtc.
            l_zoxud10008-zzfaprtc = lt_ztbl_hbl-zfaprtc.
            l_zoxud10008-zzfbldt  = lt_ztbl_hbl-zfbldt.
          ELSE.
            l_tabix1 = sy-tabix.
            CLEAR lt_ztbl_hbl-zfblno.
            SELECT SINGLE zfsprtc zfaprtc zfbldt zfblno
              INTO (l_zoxud10008-zzfsprtc, l_zoxud10008-zzfaprtc,
                    l_zoxud10008-zzfbldt, lt_ztbl_hbl-zfblno)
              FROM ztbl
             WHERE zfhblno = l_zoxud10008-frbnr
               AND bukrs   = l_zoxud10008-bukrs.

            lt_ztbl_hbl-bukrs   = l_zoxud10008-bukrs.
            lt_ztbl_hbl-frbnr   = l_zoxud10008-frbnr.
            lt_ztbl_hbl-zfsprtc = l_zoxud10008-zzfsprtc.
            lt_ztbl_hbl-zfaprtc = l_zoxud10008-zzfaprtc.
            lt_ztbl_hbl-zfbldt  = l_zoxud10008-zzfbldt.
            INSERT lt_ztbl_hbl INDEX l_tabix1.
          ENDIF.
*  reason for airfreight
          READ TABLE lt_ztblit_hbl WITH KEY
                                           zfblno  = lt_ztbl_hbl-zfblno
                                           ebeln   = l_zoxud10008-ebeln
                                           ebelp   = l_zoxud10008-ebelp
                                                  BINARY SEARCH.
          IF sy-subrc EQ 0.
            l_zoxud10008-zzinirsn  = lt_ztblit_hbl-inirsn.
            l_zoxud10008-zzhmmarsn = lt_ztblit_hbl-hmmarsn.
            l_zoxud10008-zzfinrsn  = lt_ztblit_hbl-finrsn.
          ELSE.
            l_tabix1 = sy-tabix.
            SELECT SINGLE inirsn hmmarsn finrsn
              INTO (l_zoxud10008-zzinirsn, l_zoxud10008-zzhmmarsn,
                    l_zoxud10008-zzfinrsn)
              FROM ztblit
             WHERE zfblno  = lt_ztbl_hbl-zfblno
               AND ebeln   = l_zoxud10008-ebeln
               AND ebelp   = l_zoxud10008-ebelp.

            lt_ztblit_hbl-zfblno  = lt_ztbl_hbl-zfblno.
            lt_ztblit_hbl-ebeln   = l_zoxud10008-ebeln.
            lt_ztblit_hbl-inirsn  = l_zoxud10008-ebelp.
            lt_ztblit_hbl-inirsn  = l_zoxud10008-zzinirsn.
            lt_ztblit_hbl-hmmarsn = l_zoxud10008-zzhmmarsn.
            lt_ztblit_hbl-finrsn  = l_zoxud10008-zzfinrsn.

            INSERT lt_ztblit_hbl INDEX l_tabix1.
          ENDIF.

        ENDIF.
* Check Gross Amount
        IF  l_zoxud10008-zzblart EQ 'RI'
          AND l_zoxud10008-kschl EQ 'ZOA1'
          AND ( bkpf-xblnr EQ '01'
             OR bkpf-xblnr EQ '06'
             OR bkpf-xblnr EQ '08' ) .   "Exception - Pass
        ELSEIF rbkp-rmwwr EQ 0.          "Reset Amount
          l_zoxud10008-dmbtr = 0.
          l_zoxud10008-wrbtr = 0.
        ENDIF.
* Reference#
        IF l_zoxud10008-zzblart EQ 'RI'
          AND l_zoxud10008-kschl EQ 'ZOA1'.
          CONCATENATE bkpf-xblnr '-' rbkp-bktxt
                 INTO l_zoxud10008-frbnr.
        ENDIF.
      ENDIF.

* G/L Account, Order no ...
      IF l_zoxud10008-knttp EQ space.
        l_zoxud10008-zzfipos = l_zoxud10008-fipos.
        l_zoxud10008-zzfistl = l_zoxud10008-fistl.
        l_zoxud10008-zzgeber = l_zoxud10008-geber.
      ELSE.
        IF ekkn-ebeln NE l_zoxud10008-ebeln
          OR ekkn-ebelp NE l_zoxud10008-ebelp.
          CLEAR ekkn.

          ekkn-ebeln = l_zoxud10008-ebeln.
          ekkn-ebelp = l_zoxud10008-ebelp.

          PERFORM get_ekkn USING ekkn-ebeln ekkn-ebelp l_zamc02m_0scl.

        ENDIF.
        MOVE-CORRESPONDING l_zamc02m_0scl TO l_zoxud10008.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10008 INDEX l_tabix.
    ENDLOOP.
*End of PO Delivery Cost for BW ****

*Begin of PO Item for BW ****
  WHEN '2LIS_02_ITM' .
    LOOP AT c_t_data INTO l_mc02m_0itm.
      l_tabix = sy-tabix.
* G/L Account, Order no ...
      IF l_mc02m_0itm-knttp NE space.
        IF ekkn-ebeln NE l_mc02m_0itm-ebeln
          OR ekkn-ebelp NE l_mc02m_0itm-ebelp.
          CLEAR ekkn.

          ekkn-ebeln = l_mc02m_0itm-ebeln.
          ekkn-ebelp = l_mc02m_0itm-ebelp.

          PERFORM get_ekkn USING ekkn-ebeln ekkn-ebelp l_zamc02m_0scl.

        ENDIF.

        MOVE-CORRESPONDING l_zamc02m_0scl TO l_mc02m_0itm.

        MODIFY c_t_data FROM l_mc02m_0itm INDEX l_tabix.

      ENDIF.

    ENDLOOP.
*End of PO Item for BW ****


*Begin of PO Schedule line for BW ****
  WHEN '2LIS_02_SCL' .
    LOOP AT c_t_data INTO l_mc02m_0scl.
      l_tabix = sy-tabix.

* G/L Account, Order no ...
      IF l_mc02m_0scl-knttp NE space.
        IF ekkn-ebeln NE l_mc02m_0scl-ebeln
          OR ekkn-ebelp NE l_mc02m_0scl-ebelp.
          CLEAR ekkn.
          ekkn-ebeln = l_mc02m_0scl-ebeln.
          ekkn-ebelp = l_mc02m_0scl-ebelp.

          PERFORM get_ekkn USING ekkn-ebeln ekkn-ebelp l_zamc02m_0scl.
        ENDIF.

        MOVE-CORRESPONDING l_zamc02m_0scl TO l_mc02m_0scl.

        MODIFY c_t_data FROM l_mc02m_0scl INDEX l_tabix.

      ENDIF.

    ENDLOOP.
*End of PO Schedule line for BW ****


* Begin of Unit Cost
  WHEN 'ZVBW_CKMLMV003'.
    LOOP AT c_t_data INTO l_zoxud10010.
      l_tabix = sy-tabix.

      CLEAR ckmlmv001.
      SELECT SINGLE *
        FROM ckmlmv001
       WHERE kalnr = l_zoxud10010-kalnr_bal
         AND btyp = 'BF'. " production
      IF sy-subrc <> 0.
        DELETE c_t_data INDEX l_tabix.
      ENDIF.

    ENDLOOP.
* End of Unit Cost

  WHEN '0FI_GL_4'.

    REFRESH lt_doc_type.
    SELECT blart xkoad xkoak
      INTO CORRESPONDING FIELDS OF TABLE lt_doc_type
      FROM t003
     WHERE xkoak EQ 'X' OR xkoad EQ 'X'.
    SORT lt_doc_type.

    REFRESH lt_cust_tp. "Customer Trading partner
    SELECT kunnr vbund
      INTO TABLE lt_cust_tp
      FROM kna1
     WHERE vbund > space.
    SORT lt_cust_tp.

    REFRESH lt_ven_tp.  "Vendor Trading partner
    SELECT lifnr vbund
      INTO TABLE lt_ven_tp
      FROM lfa1
     WHERE vbund > space.
    SORT lt_ven_tp.

    LOOP AT c_t_data INTO l_dtfigl_4.
      l_tabix = sy-tabix.

      IF l_dtfigl_4-paobjnr NE space.
        CLEAR ce4h201_acct.
        SELECT SINGLE paobjnr kndnr artnr kmland prodh
          INTO (l_dtfigl_4-paobjnr,
                l_dtfigl_4-kndnr,
                l_dtfigl_4-artnr,
                l_dtfigl_4-kmland,
                l_dtfigl_4-prodh)
          FROM ce4h201_acct
         WHERE aktbo = 'X'
           AND paobjnr = l_dtfigl_4-paobjnr.
      ENDIF.

* H Customer, Vendor and Trading partner
      READ TABLE lt_doc_type WITH KEY blart = l_dtfigl_4-blart
                                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        READ TABLE lt_cust_ven WITH KEY belnr = l_dtfigl_4-belnr
                                  gjahr = l_dtfigl_4-gjahr
                                 BINARY SEARCH.
        IF sy-subrc EQ 0.
          l_dtfigl_4-zzlifnr = lt_cust_ven-lifnr.
          l_dtfigl_4-zzkunnr = lt_cust_ven-kunnr.
          l_dtfigl_4-zzvbund = lt_cust_ven-vbund.
        ELSE.
          l_tabix1 = sy-tabix.

          IF lt_doc_type-xkoak EQ 'X'.      "Vendor
*            IF l_dtfigl_4-statusps EQ 'O'.
*              l_tabname = 'BSIK'.
*            ELSE.
*              l_tabname = 'BSAK'.
*            ENDIF.

            SELECT SINGLE lifnr
              INTO l_dtfigl_4-zzlifnr
              FROM bsak
             WHERE belnr = l_dtfigl_4-belnr
               AND gjahr = l_dtfigl_4-gjahr.
            IF sy-subrc NE 0.
              SELECT SINGLE lifnr
                INTO l_dtfigl_4-zzlifnr
                FROM bsik
               WHERE belnr = l_dtfigl_4-belnr
                 AND gjahr = l_dtfigl_4-gjahr.
            ENDIF.
            IF l_dtfigl_4-zzlifnr NE space.
              READ TABLE lt_ven_tp WITH KEY lifnr = l_dtfigl_4-zzlifnr
                                        BINARY SEARCH.
              IF sy-subrc EQ 0.
                l_dtfigl_4-zzvbund = lt_ven_tp-vbund.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lt_doc_type-xkoad EQ 'X'       "Customer
            AND l_dtfigl_4-zzlifnr EQ space.
*            IF l_dtfigl_4-statusps EQ 'O'.
*              l_tabname = 'BSID'.
*            ELSE.
*              l_tabname = 'BSAD'.
*            ENDIF.

            SELECT SINGLE kunnr
              INTO l_dtfigl_4-zzkunnr
              FROM bsad
             WHERE belnr = l_dtfigl_4-belnr
               AND gjahr = l_dtfigl_4-gjahr.
            IF sy-subrc NE 0.
              SELECT SINGLE kunnr
                INTO l_dtfigl_4-zzkunnr
                FROM bsid
               WHERE belnr = l_dtfigl_4-belnr
                 AND gjahr = l_dtfigl_4-gjahr.
            ENDIF.

            IF l_dtfigl_4-zzkunnr NE space.
              READ TABLE lt_cust_tp WITH KEY kunnr = l_dtfigl_4-zzkunnr
                                         BINARY SEARCH.
              IF sy-subrc EQ 0.
                l_dtfigl_4-zzvbund = lt_cust_tp-vbund.
              ENDIF.
            ENDIF.
          ENDIF.

          lt_cust_ven-belnr = l_dtfigl_4-belnr.
          lt_cust_ven-gjahr = l_dtfigl_4-gjahr.
          lt_cust_ven-lifnr = l_dtfigl_4-zzlifnr.
          lt_cust_ven-kunnr = l_dtfigl_4-zzkunnr.
          lt_cust_ven-vbund = l_dtfigl_4-zzvbund.
          INSERT lt_cust_ven INDEX l_tabix1.
        ENDIF.
      ENDIF.

      IF l_dtfigl_4-zzlifnr EQ space AND l_dtfigl_4-ebeln NE space.
        IF l_ebeln EQ l_dtfigl_4-ebeln.
          l_dtfigl_4-zzlifnr = l_lifnr.
        ELSE.
          SELECT SINGLE lifnr INTO l_dtfigl_4-zzlifnr
            FROM ekko
           WHERE ebeln = l_dtfigl_4-ebeln.
          l_ebeln = l_dtfigl_4-ebeln.
          l_lifnr = l_dtfigl_4-zzlifnr.
        ENDIF.
      ENDIF.

*      REPLACE ALL OCCURRENCES OF <cr> IN l_dtfigl_4-sgtxt WITH space .
*      REPLACE ALL OCCURRENCES OF <cr> IN l_dtfigl_4-bktxt WITH space .

      IF sy-saprl < '700'.
        TRANSLATE l_dtfigl_4-sgtxt USING <crsp>.
        TRANSLATE l_dtfigl_4-bktxt USING <crsp>.
      ELSE.
        TRANSLATE l_dtfigl_4-sgtxt USING s_crsp.
        TRANSLATE l_dtfigl_4-bktxt USING s_crsp.
      ENDIF.

      MODIFY c_t_data FROM l_dtfigl_4 INDEX l_tabix.
    ENDLOOP.

  WHEN 'ZVBW_AFRU'.
    LOOP AT c_t_data INTO l_zoxud10024.
      l_tabix = sy-tabix.

      CLEAR qmel.
      SELECT SINGLE qmnum qmart INTO (l_zoxud10024-qmnum,
                                      l_zoxud10024-qmart)
        FROM qmel
       WHERE aufnr = l_zoxud10024-aufnr.

      CLEAR qmih.
      SELECT SINGLE equnr INTO l_zoxud10024-equnr
        FROM qmih
       WHERE qmnum = l_zoxud10024-qmnum.

      MODIFY c_t_data FROM l_zoxud10024 INDEX l_tabix.

    ENDLOOP.

  WHEN '1CL_OEQU013'. " Manifest of Vehicle Master
    LOOP AT c_t_data INTO l_zoxud10020.
      l_tabix = sy-tabix.

      IF l_zoxud10020-p_engine_no005 NE space.
        CLEAR ausp.
        SELECT SINGLE atwrt
          INTO (l_zoxud10020-en_item_code001)
          FROM ausp
         WHERE objek = l_zoxud10020-p_engine_no005
           AND atinn = '0000003583'.
        IF sy-subrc EQ 0.
          MODIFY c_t_data FROM l_zoxud10020 INDEX l_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  WHEN 'ZTCO_ABISPOST'. " CO OTHER GI Allocation
    LOOP AT c_t_data INTO l_zoxud10036.
      l_tabix = sy-tabix.

      IF l_zoxud10036-pcc_aufnr NE space.
        CLEAR ckmlmv013.
        SELECT SINGLE verid
          INTO (l_zoxud10036-verid)
          FROM ckmlmv013
         WHERE aufnr = l_zoxud10036-pcc_aufnr.
        IF sy-subrc EQ 0.
          MODIFY c_t_data FROM l_zoxud10036 INDEX l_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

*PPC
  WHEN 'ZVBW_PPC1_ALL'. " PPC components

    REFRESH: i_ckmlmv013, i_plaf, i_rp.

    SELECT pmatn prwrk verid aufnr
    INTO CORRESPONDING FIELDS OF TABLE i_ckmlmv013
    FROM ckmlmv013.
    SORT i_ckmlmv013.

    SELECT plnum pedtr
    INTO CORRESPONDING FIELDS OF TABLE i_plaf
    FROM plaf.
    SORT i_plaf.

    SELECT reppoint reppoint_ext
    INTO CORRESPONDING FIELDS OF TABLE i_rp
    FROM ppc_rp.
    SORT i_rp.

    LOOP AT c_t_data INTO l_zoxud10039.
      l_tabix = sy-tabix.
*      BREAK-POINT.
      IF l_zoxud10039-orderid NE space.
        CLEAR i_plaf.
        READ TABLE i_plaf WITH KEY plnum = l_zoxud10039-orderid
                          BINARY SEARCH.
        IF sy-subrc EQ 0.
*      BREAK-POINT.
          l_zoxud10039-pedtr = i_plaf-pedtr.
        ENDIF.
      ENDIF.

      CLEAR i_ckmlmv013.
      READ TABLE i_ckmlmv013 WITH KEY pmatn = l_zoxud10039-materialnr
                                      prwrk = l_zoxud10039-plant
                                      verid = l_zoxud10039-version
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10039-aufnr = i_ckmlmv013-aufnr.
      ENDIF.

      CLEAR i_rp.
      READ TABLE i_rp WITH KEY reppoint = l_zoxud10039-reppoint
                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10039-reppoint_ext = i_rp-reppoint_ext.
      ENDIF.

      IF l_zoxud10039-kdauf EQ space AND l_zoxud10039-confquant EQ 0.
        l_zoxud10039-orderid = space.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10039 INDEX l_tabix.

    ENDLOOP.
*PPC

*PPC Header
  WHEN 'ZVBW_PPC_HEAD'. " PPC Header

    REFRESH: i_ckmlmv013, i_plaf, i_rp.

    SELECT pmatn prwrk verid aufnr
    INTO CORRESPONDING FIELDS OF TABLE i_ckmlmv013
    FROM ckmlmv013.
    SORT i_ckmlmv013.

    SELECT plnum pedtr
    INTO CORRESPONDING FIELDS OF TABLE i_plaf
    FROM plaf.
    SORT i_plaf.

    SELECT reppoint reppoint_ext
    INTO CORRESPONDING FIELDS OF TABLE i_rp
    FROM ppc_rp.
    SORT i_rp.

    LOOP AT c_t_data INTO l_zoxud10043.
      l_tabix = sy-tabix.
*      BREAK-POINT.
      IF l_zoxud10043-orderid NE space.
        CLEAR i_plaf.
        READ TABLE i_plaf WITH KEY plnum = l_zoxud10043-orderid
                          BINARY SEARCH.
        IF sy-subrc EQ 0.
*      BREAK-POINT.
          l_zoxud10043-pedtr = i_plaf-pedtr.
        ENDIF.
      ENDIF.

      CLEAR i_ckmlmv013.
      READ TABLE i_ckmlmv013 WITH KEY pmatn = l_zoxud10043-materialnr
                                      prwrk = l_zoxud10043-plant
                                      verid = l_zoxud10043-version
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10043-aufnr = i_ckmlmv013-aufnr.
      ENDIF.

      CLEAR i_rp.
      READ TABLE i_rp WITH KEY reppoint = l_zoxud10043-reppoint
                      BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10043-reppoint_ext = i_rp-reppoint_ext.
      ENDIF.

      IF l_zoxud10043-kdauf EQ space AND l_zoxud10043-confquant EQ 0.
        l_zoxud10043-orderid = space.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10043 INDEX l_tabix.

    ENDLOOP.
*PPC Header

* Date 06/16/2008 by Kerry Lee
* Actual Costing/Material Ledger: Cost
* Valuation Class Correction from mbewh
*  WHEN '0CO_PC_ACT_10'.
*    LOOP AT c_t_data INTO l_kkbw_ccs_ds.
*      l_tabix = sy-tabix.
*
*      IF    l_matnr    EQ l_kkbw_ccs_ds-material
*        AND l_plant    EQ l_kkbw_ccs_ds-plant
*        AND l_fiscper  EQ l_kkbw_ccs_ds-fiscper.
*        l_kkbw_ccs_ds-val_class = l_bklas.
*      ELSE.
*        l_year  = l_kkbw_ccs_ds-fiscper(4).
*        l_month = l_kkbw_ccs_ds-fiscper+5(2).
*
*        l_fiscper = l_kkbw_ccs_ds-fiscper.
*        l_matnr   = l_kkbw_ccs_ds-material.
*        l_plant   = l_kkbw_ccs_ds-plant.
*        l_bklas   = l_kkbw_ccs_ds-val_class.
*
*        DO 2 TIMES.
*          SELECT SINGLE bklas    " lfgja lfmon
**          INTO (mbewh-lfgja, mbewh-lfmon, mbewh-bklas)
*            INTO l_bklas
*            FROM mbewh
*           WHERE matnr = l_matnr
*             AND bwkey = l_plant
*             AND lfgja = l_year
*             AND lfmon = ( SELECT MAX( lfmon ) FROM mbewh
*                            WHERE matnr = l_matnr
*                              AND bwkey = l_plant
*                              AND lfgja = l_year
*                              AND lfmon <= l_month ).
*          IF sy-subrc EQ 0.
*            l_kkbw_ccs_ds-val_class = l_bklas.
*            EXIT.
*          ELSE.
*            l_year  = l_year - 1.
*            l_month = '12'.
*          ENDIF.
*        ENDDO.
*      ENDIF.
*
*      MODIFY c_t_data FROM l_kkbw_ccs_ds INDEX l_tabix.
*    ENDLOOP.
*
  WHEN '0CO_PC_ACT_1'.

    LOOP AT c_t_data INTO l_kkbw_act_is1.
      l_tabix = sy-tabix.

*      IF    l_matnr    EQ l_kkbw_act_is1-material
*        AND l_plant    EQ l_kkbw_act_is1-plant
*        AND l_fiscper  EQ l_kkbw_act_is1-fiscper.
*        l_kkbw_act_is1-val_class = l_bklas.
*      ELSE.
*        l_year  = l_kkbw_act_is1-fiscper(4).
*        l_month = l_kkbw_act_is1-fiscper+5(2).
*
*        l_fiscper = l_kkbw_act_is1-fiscper.
*        l_matnr   = l_kkbw_act_is1-material.
*        l_plant   = l_kkbw_act_is1-plant.
*        l_bklas   = l_kkbw_act_is1-val_class.
*
*        DO 2 TIMES.
*          SELECT SINGLE bklas    " lfgja lfmon
**          INTO (mbewh-lfgja, mbewh-lfmon, mbewh-bklas)
*            INTO l_bklas
*            FROM mbewh
*           WHERE matnr = l_matnr
*             AND bwkey = l_plant
*             AND lfgja = l_year
*             AND lfmon = ( SELECT MAX( lfmon ) FROM mbewh
*                            WHERE matnr = l_matnr
*                              AND bwkey = l_plant
*                              AND lfgja = l_year
*                              AND lfmon <= l_month ).
*          IF sy-subrc EQ 0.
*            l_kkbw_act_is1-val_class = l_bklas.
*            EXIT.
*          ELSE.
*            l_year  = l_year - 1.
*            l_month = '12'.
*          ENDIF.
*        ENDDO.
*      ENDIF.

** Price control indicator
*      IF    l_matnr    EQ l_kkbw_act_is1-material
*        AND l_plant    EQ l_kkbw_act_is1-plant
*        AND l_fiscper  EQ l_kkbw_act_is1-fiscper.
*        l_kkbw_act_is1-zzvprsv = l_vprsv.
*      ELSE.
*        l_year  = l_kkbw_act_is1-fiscper(4).
*        l_poper = l_kkbw_act_is1-fiscper+4(3).
*
*        l_fiscper = l_kkbw_act_is1-fiscper.
*        l_matnr   = l_kkbw_act_is1-material.
*        l_plant   = l_kkbw_act_is1-plant.
*
*        SELECT SINGLE vprsv    " Price control indicator
*          INTO l_kkbw_act_is1-zzvprsv
*          FROM ckmlhd AS a
*         INNER JOIN ckmlcr AS b
*            ON a~kalnr = b~kalnr
*           AND b~bdatj = l_year
*           AND b~poper = l_poper
*         WHERE a~matnr = l_kkbw_act_is1-material
*           AND a~bwkey = l_kkbw_act_is1-plant.
*
*        l_vprsv = l_kkbw_act_is1-zzvprsv.
*      ENDIF.

*      MODIFY c_t_data FROM l_kkbw_act_is1 INDEX l_tabix.

* Beginning from Previous Ending
      IF l_kkbw_act_is1-categ EQ 'EB'.
        lt_0co_pc_act_1 = l_kkbw_act_is1.
        CONCATENATE lt_0co_pc_act_1-fiscper(4)
                    lt_0co_pc_act_1-fiscper+5(2) '20'
               INTO l_date.
        l_date = l_date + 15.
        CONCATENATE l_date(4) '0' l_date+4(2)
               INTO lt_0co_pc_act_1-fiscper.   "Previous Period
        lt_0co_pc_act_1-categ   = 'ZB'.        "Previous Ending
        APPEND lt_0co_pc_act_1.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_0co_pc_act_1.
** Get Price control
*      l_year  = lt_0co_pc_act_1-fiscper(4).
*      l_poper = lt_0co_pc_act_1-fiscper+4(3).
*
*      SELECT SINGLE vprsv    " Price control indicator
*        INTO l_vprsv
*        FROM ckmlhd AS a
*       INNER JOIN ckmlcr AS b
*          ON a~kalnr = b~kalnr
*         AND b~bdatj = l_year
*         AND b~poper = l_poper
*       WHERE a~matnr = lt_0co_pc_act_1-material
*         AND a~bwkey = lt_0co_pc_act_1-plant.
*      IF sy-subrc EQ 0.
*        lt_0co_pc_act_1-zzvprsv = l_vprsv.
*      ENDIF.
      c_t_data = lt_0co_pc_act_1.
      APPEND c_t_data.
    ENDLOOP.

    REFRESH lt_0co_pc_act_1.

  WHEN '2LIS_13_VDITM'.
* Get Vehicle Number
    LOOP AT c_t_data INTO l_mc13vd0itm.
      l_tabix = sy-tabix.

      l_mc13vd0itm-zzvehicle = l_mc13vd0itm-vgbel.
      IF   l_mc13vd0itm-fkart EQ 'ZVL2'   "Credit Memo
        OR l_mc13vd0itm-fkart EQ 'ZVG2'   "Debit Memo
        OR l_mc13vd0itm-fkart EQ 'S1'     "Cancel. Invoice (S1)
        OR l_mc13vd0itm-fkart EQ 'S2'.    "Cancel of Cred Memo
*       AND l_MC13VD0ITM-VGTYP NE 'M'.
        CLEAR vbrp.
        SELECT SINGLE vgbel
          INTO vbrp-vgbel
          FROM vbrp
         WHERE vbeln = l_mc13vd0itm-vgbel
           AND posnr = l_mc13vd0itm-vgpos
           AND vgtyp = 'J'.
        IF sy-subrc EQ 0.
          l_mc13vd0itm-zzvehicle = vbrp-vgbel.
        ENDIF.
      ENDIF.
      MODIFY c_t_data FROM l_mc13vd0itm INDEX l_tabix.
    ENDLOOP.

* Info-Record
  WHEN 'ZBW_KONH_KONP' OR 'ZBW_KONH_KONP_DELTA'.

    LOOP AT c_t_data.
      IF i_datasource = 'ZBW_KONH_KONP'.
        MOVE c_t_data TO l_zoxud10056.
      ELSE.
        MOVE c_t_data TO l_zoxud10058.
        MOVE-CORRESPONDING l_zoxud10058 TO l_zoxud10056.
      ENDIF.

      l_tabix = sy-tabix.
      l_zoxud10056-zzlifnr = l_zoxud10056-vakey(10).
      l_zoxud10056-zzmatnr = l_zoxud10056-vakey+10(18).

      lt_ven_mat-lifnr = l_zoxud10056-zzlifnr.
      lt_ven_mat-matnr = l_zoxud10056-zzmatnr.
      COLLECT lt_ven_mat.

      IF i_datasource = 'ZBW_KONH_KONP'.
        MODIFY c_t_data FROM l_zoxud10056 INDEX l_tabix.
      ELSE.
        MOVE-CORRESPONDING l_zoxud10056 TO l_zoxud10058.
        MODIFY c_t_data FROM l_zoxud10058 INDEX l_tabix.
      ENDIF.
    ENDLOOP.

    READ TABLE lt_ven_mat INDEX 1.
    IF sy-subrc EQ 0.
      SELECT matnr a~lifnr ekgrp a~urzdt c~land1
      INTO TABLE lt_eina
      FROM eina AS a
     INNER JOIN eine AS b
        ON a~infnr =  b~infnr
     INNER JOIN lfa1 AS c
        ON a~lifnr = c~lifnr
      FOR ALL ENTRIES IN lt_ven_mat
     WHERE a~matnr =  lt_ven_mat-matnr
       AND a~lifnr =  lt_ven_mat-lifnr
       AND a~loekz =  ' '
       AND b~werks =  ' '
       AND b~loekz =  ' '.

      SORT lt_eina.

      SELECT knumh datab datbi ekorg
        INTO CORRESPONDING FIELDS OF TABLE lt_a018
        FROM a018
         FOR ALL ENTRIES IN lt_ven_mat
       WHERE kappl =  'M'
         AND kschl =  'PB00'     "ZTIR = PB00
         AND esokz =  '0'
         AND matnr = lt_ven_mat-matnr
         AND lifnr = lt_ven_mat-lifnr.

      SORT lt_a018.
    ENDIF.

    LOOP AT c_t_data.   " INTO l_zoxud10056.
      IF i_datasource = 'ZBW_KONH_KONP'.
        MOVE c_t_data TO l_zoxud10056.
      ELSE.
        MOVE c_t_data TO l_zoxud10058.
        MOVE-CORRESPONDING l_zoxud10058 TO l_zoxud10056.
      ENDIF.

      l_tabix = sy-tabix.
      READ TABLE lt_eina WITH KEY matnr = l_zoxud10056-zzmatnr
                                  lifnr = l_zoxud10056-zzlifnr
                              BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE  c_t_data INDEX l_tabix. CONTINUE.
      ENDIF.

      READ TABLE lt_a018 WITH KEY knumh = l_zoxud10056-knumh
                              BINARY SEARCH.
      IF sy-subrc NE 0.
        DELETE  c_t_data INDEX l_tabix. CONTINUE.
      ENDIF.

      l_zoxud10056-zzekgrp = lt_eina-ekgrp.
      l_zoxud10056-zzurzdt = lt_eina-urzdt.

      l_zoxud10056-datab     = lt_a018-datab.
      l_zoxud10056-datbi     = lt_a018-datbi.
      l_zoxud10056-zzekorg   = lt_a018-ekorg.

      IF lt_eina-land1 EQ 'KR' OR lt_eina-land1 EQ 'CN'.
        l_date = l_zoxud10056-datab - 1.   "To date for prior price
        CLEAR konp.

        SELECT SINGLE knumh
          INTO konp-knumh
          FROM a018
         WHERE kappl =  'M'
           AND kschl =  'PB00'     "ZTIR = PB00
           AND esokz =  '0'
           AND matnr = l_zoxud10056-zzmatnr
           AND lifnr = l_zoxud10056-zzlifnr
           AND ekorg = l_zoxud10056-zzekorg
           AND datbi = l_date.

        IF sy-subrc EQ 0.
          SELECT SINGLE kbetr
            INTO konp-kbetr
            FROM konp
           WHERE knumh = konp-knumh
             AND kschl = l_zoxud10056-kschl001.

          IF sy-subrc EQ 0.
            IF l_zoxud10056-kbetr EQ konp-kbetr.      "Equal
              l_zoxud10056-kzust = 'KE1'.
            ELSEIF l_zoxud10056-kbetr > konp-kbetr.   "Up
              l_zoxud10056-kzust = 'KU1'.
            ELSE.
              l_zoxud10056-kzust = 'KD1'.
            ENDIF.
          ELSE.
            l_zoxud10056-kzust = 'KE1'.
          ENDIF.
        ELSE.
          l_zoxud10056-kzust = 'KE1'.
        ENDIF.
      ENDIF.

      IF     l_zoxud10056-kzust EQ 'ZLC'.   "Ignore
        l_zoxud10056-kzust        = space.
      ELSEIF l_zoxud10056-kzust(1) EQ 'X'.
        l_zoxud10056-zzrgrp2      = l_zoxud10056-kzust+1(2).
*        " UDEX = 'X'.
      ELSE.
        l_zoxud10056-zzrgrp2(1)   = l_zoxud10056-kzust(1).
        l_zoxud10056-zzrgrp2+1(1) = l_zoxud10056-kzust+2(1).
*        " UDEX = l_zoxud10056-KZUST+1(1).
      ENDIF.

* if Tire Vendor is not exist
      IF l_zoxud10056-lifnr EQ space.
        l_zoxud10056-lifnr = l_zoxud10056-zzlifnr.
      ENDIF.

      IF i_datasource = 'ZBW_KONH_KONP'.
        MODIFY c_t_data FROM l_zoxud10056 INDEX l_tabix.
      ELSE.
        MOVE-CORRESPONDING l_zoxud10056 TO l_zoxud10058.
        MODIFY c_t_data FROM l_zoxud10058 INDEX l_tabix.
      ENDIF.
    ENDLOOP.

  WHEN 'ZBW_CO_UNIT_COST'.
    REFRESH: lt_zoxud10065,  lt_zoxud10065_mip2.

    LOOP AT c_t_data INTO l_zoxud10065.
      l_tabix = sy-tabix.
      CLEAR: l_zoxud10065-zzmip1, l_zoxud10065-zzmip2,
             l_zoxud10065-losgr,
             l_zoxud10065-ekgrp,  l_zoxud10065-profl,
             l_zoxud10065-wertn2, l_zoxud10065-peinh2,
             l_zoxud10065-pmeht,  l_zoxud10065-lifnr,
             l_zoxud10065-kzust1, l_zoxud10065-verpr,
             l_zoxud10065-stprs.
* Info Price
      SELECT SINGLE ekgrp profl wertn peinh AS peinh2 pmeht
                    lifnr kzust1 verpr stprs
*          INTO CORRESPONDING FIELDS OF lt_zoxud10065
        INTO (l_zoxud10065-ekgrp,  l_zoxud10065-profl,
              l_zoxud10065-wertn2, l_zoxud10065-peinh2,
              l_zoxud10065-pmeht,  l_zoxud10065-lifnr,
              l_zoxud10065-kzust1, l_zoxud10065-verpr,
              l_zoxud10065-stprs)
        FROM ztcou102
       WHERE kokrs = l_zoxud10065-kokrs
         AND bdatj = l_zoxud10065-bdatj
         AND poper = l_zoxud10065-poper
         AND kalka = l_zoxud10065-kalka
         AND ver   = l_zoxud10065-ver
         AND matnr = l_zoxud10065-compn.

      IF l_zoxud10065-stkkz = 'X'.
        lt_zoxud10065 = l_zoxud10065.
        lt_zoxud10065-zzmip1 = l_zoxud10065-compn.
* MIP 1
        SELECT a~indx  a~kstar  a~upgvc  a~compn a~menge
               a~meeht a~stkkz  a~wertn  a~duty  a~frg
               a~oth   a~peinh  a~losgr
               a~aedat a~aenam  a~cputm
               b~ekgrp b~profl b~wertn b~peinh as b~peinh2 b~pmeht
               b~lifnr b~kzust1 b~verpr b~stprs
*          INTO CORRESPONDING FIELDS OF lt_zoxud10065
            INTO (lt_zoxud10065-indx,  lt_zoxud10065-kstar,
                 lt_zoxud10065-upgvc, lt_zoxud10065-compn,
                 lt_zoxud10065-menge, lt_zoxud10065-meeht,
                 lt_zoxud10065-stkkz, lt_zoxud10065-wertn,
                 lt_zoxud10065-duty,  lt_zoxud10065-frg,
                 lt_zoxud10065-oth,
                 lt_zoxud10065-peinh, lt_zoxud10065-losgr,
                 lt_zoxud10065-aedat, lt_zoxud10065-aenam,
                 lt_zoxud10065-cputm,
                 lt_zoxud10065-ekgrp, lt_zoxud10065-profl,
                 lt_zoxud10065-wertn2, lt_zoxud10065-peinh2,
                 lt_zoxud10065-pmeht,
                 lt_zoxud10065-lifnr, lt_zoxud10065-kzust1,
                 lt_zoxud10065-verpr, lt_zoxud10065-stprs)
          FROM ztcou103 AS a LEFT OUTER JOIN ztcou102 AS b
            ON a~kokrs = b~kokrs
           AND a~bdatj = b~bdatj
           AND a~poper = b~poper
           AND a~kalka = b~kalka
           AND a~ver   = b~ver
           AND a~compn = b~matnr
         WHERE a~kokrs = l_zoxud10065-kokrs
           AND a~bdatj = l_zoxud10065-bdatj
           AND a~kalka = l_zoxud10065-kalka
           AND a~poper = l_zoxud10065-poper
           AND a~artnr = l_zoxud10065-compn.
*           AND stkkz = space.

          lt_zoxud10065-wertn = lt_zoxud10065-wertn
                              / lt_zoxud10065-losgr
                              * l_zoxud10065-menge.
          lt_zoxud10065-menge = lt_zoxud10065-menge
                              * l_zoxud10065-menge.

          APPEND lt_zoxud10065.
        ENDSELECT.
* MIP 2
        LOOP AT lt_zoxud10065 WHERE stkkz = 'X'.
          lt_zoxud10065_mip2 = lt_zoxud10065.
          SELECT a~artnr as a~zzmip2
                 a~indx  a~kstar a~upgvc a~compn a~menge
                 a~meeht a~stkkz a~wertn a~duty  a~frg a~oth
                 a~peinh a~losgr
                 a~aedat a~aenam  a~cputm
               b~ekgrp b~profl b~wertn b~peinh as b~peinh2 b~pmeht
               b~lifnr b~kzust1 b~verpr b~stprs
*            INTO CORRESPONDING FIELDS OF lt_zoxud10065_mip2
            INTO (lt_zoxud10065_mip2-zzmip2,
                 lt_zoxud10065_mip2-indx,  lt_zoxud10065_mip2-kstar,
                 lt_zoxud10065_mip2-upgvc, lt_zoxud10065_mip2-compn,
                 lt_zoxud10065_mip2-menge, lt_zoxud10065_mip2-meeht,
                 lt_zoxud10065_mip2-stkkz, lt_zoxud10065_mip2-wertn,
                 lt_zoxud10065_mip2-duty,  lt_zoxud10065_mip2-frg,
                 lt_zoxud10065_mip2-oth,
                 lt_zoxud10065_mip2-peinh, lt_zoxud10065_mip2-losgr,
                 lt_zoxud10065_mip2-aedat, lt_zoxud10065_mip2-aenam,
                 lt_zoxud10065_mip2-cputm,
                 lt_zoxud10065_mip2-ekgrp, lt_zoxud10065_mip2-profl,
                 lt_zoxud10065_mip2-wertn2, lt_zoxud10065_mip2-peinh2,
                 lt_zoxud10065_mip2-pmeht,
                 lt_zoxud10065_mip2-lifnr, lt_zoxud10065_mip2-kzust1,
                 lt_zoxud10065_mip2-verpr, lt_zoxud10065_mip2-stprs)
          FROM ztcou103 AS a LEFT OUTER JOIN ztcou102 AS b
            ON a~kokrs = b~kokrs
           AND a~bdatj = b~bdatj
           AND a~poper = b~poper
           AND a~kalka = b~kalka
           AND a~ver   = b~ver
           AND a~compn = b~matnr
           WHERE a~kokrs = lt_zoxud10065-kokrs
             AND a~bdatj = lt_zoxud10065-bdatj
             AND a~kalka = lt_zoxud10065-kalka
             AND a~poper = lt_zoxud10065-poper
             AND a~stkkz = space
             AND a~artnr = lt_zoxud10065-compn.
            lt_zoxud10065_mip2-wertn = lt_zoxud10065_mip2-wertn
                                     / lt_zoxud10065_mip2-losgr
                                     * lt_zoxud10065-menge.
            lt_zoxud10065_mip2-menge = lt_zoxud10065_mip2-menge
                                     * lt_zoxud10065-menge.
            APPEND lt_zoxud10065_mip2.
          ENDSELECT.
          DELETE lt_zoxud10065.
        ENDLOOP.


** MIP 1
*        SELECT indx  kstar  upgvc  compn menge
*               meeht stkkz  wertn  duty  frg   oth   peinh losgr
*               aedat aenam  cputm
*          INTO CORRESPONDING FIELDS OF lt_zoxud10065
*          FROM ztcou103
*         WHERE kokrs = l_zoxud10065-kokrs
*           AND bdatj = l_zoxud10065-bdatj
*           AND kalka = l_zoxud10065-kalka
*           AND poper = l_zoxud10065-poper
*           AND artnr = l_zoxud10065-compn.
**           AND stkkz = space.
*          APPEND lt_zoxud10065.
*        ENDSELECT.
* MIP 2
*        LOOP AT lt_zoxud10065 WHERE stkkz = 'X'.
*          lt_zoxud10065_mip2 = lt_zoxud10065.
*          SELECT artnr AS zzmip2
*                 indx  kstar  upgvc  compn menge
*                 meeht stkkz  wertn  duty  frg   oth   peinh losgr
*                 aedat aenam  cputm
*            INTO CORRESPONDING FIELDS OF lt_zoxud10065_mip2
*            FROM ztcou103
*           WHERE kokrs = lt_zoxud10065-kokrs
*             AND bdatj = lt_zoxud10065-bdatj
*             AND kalka = lt_zoxud10065-kalka
*             AND poper = lt_zoxud10065-poper
*             AND stkkz = space
*             AND artnr = lt_zoxud10065-compn.
*            lt_zoxud10065_mip2-wertn = lt_zoxud10065_mip2-wertn
*                                     / lt_zoxud10065_mip2-losgr
*                                     * lt_zoxud10065-menge.
*            APPEND lt_zoxud10065_mip2.
*          ENDSELECT.
*          DELETE lt_zoxud10065.
*        ENDLOOP.


        DELETE c_t_data INDEX l_tabix.
      ELSE.

        MODIFY c_t_data FROM l_zoxud10065 INDEX l_tabix.
      ENDIF.
    ENDLOOP.


    APPEND LINES OF lt_zoxud10065 TO c_t_data.

    APPEND LINES OF lt_zoxud10065_mip2 TO c_t_data.

*    loop at lt_zoxud10065.
*      append c_t_data from lt_zoxud10065.
*    endloop.

  WHEN 'ZBW_CO_SHOP_PLN'.
* Get Vehicle Number
    LOOP AT c_t_data INTO l_zoxud10073.
      l_tabix = sy-tabix.
      CONCATENATE l_zoxud10073-bdatj l_zoxud10073-poper+1(2) '01'
             INTO l_date.
      SELECT SINGLE lifnr INTO l_zoxud10073-zzlifnr
        FROM ztcou137
       WHERE bukrs = 'H201'
         AND matnr = l_zoxud10073-llv_matnr
         AND zdtfr <= l_date
         AND zdtto >= l_date.
      MODIFY c_t_data FROM l_zoxud10073 INDEX l_tabix.
    ENDLOOP.

  WHEN '2LIS_03_BF'.
* enhance mseg
*    BREAK-POINT.
    LOOP AT c_t_data INTO l_mc03bf0.
      l_tabix = sy-tabix.

      CLEAR mseg.
      SELECT SINGLE kzzug kzvbr xauto bualt bpmng bprme
                    lfbja lfbnr lfpos sjahr smbln smblp
                    elikz gjahr weunb bwlvs xblvs vschn
                    dypla weanz fipos bstmg bstme emlif
                    pprctr fistl matbf bustm bustw mengu
                    wertu lbkum salk3 vprsv urzei anln1
                    anln2 kzstr sgtxt

        INTO (l_mc03bf0-kzzug, l_mc03bf0-kzvbr, l_mc03bf0-xauto,
              l_mc03bf0-bualt, l_mc03bf0-bpmng, l_mc03bf0-bprme,

              l_mc03bf0-lfbja, l_mc03bf0-lfbnr, l_mc03bf0-lfpos,
              l_mc03bf0-sjahr, l_mc03bf0-smbln, l_mc03bf0-smblp,

              l_mc03bf0-elikz, l_mc03bf0-gjahr, l_mc03bf0-weunb,
              l_mc03bf0-bwlvs, l_mc03bf0-xblvs, l_mc03bf0-vschn,

              l_mc03bf0-dypla, l_mc03bf0-weanz, l_mc03bf0-fipos,
              l_mc03bf0-bstmg, l_mc03bf0-bstme, l_mc03bf0-emlif,

              l_mc03bf0-pprctr, l_mc03bf0-fistl, l_mc03bf0-matbf,
              l_mc03bf0-bustm, l_mc03bf0-bustw, l_mc03bf0-mengu,

              l_mc03bf0-wertu, l_mc03bf0-lbkum, l_mc03bf0-salk3,
              l_mc03bf0-vprsv, l_mc03bf0-urzei, l_mc03bf0-anln1,

              l_mc03bf0-anln2, l_mc03bf0-kzstr, l_mc03bf0-sgtxt)
        FROM mseg
       WHERE mblnr = l_mc03bf0-mblnr
         AND mjahr = l_mc03bf0-mjahr
         AND zeile = l_mc03bf0-zeile.

      CLEAR mkpf.
      SELECT SINGLE cpudt cputm usnam vgart blart blaum
                    xblnr frbnr wever tcode2 bfwms
        INTO (l_mc03bf0-cpudt, l_mc03bf0-cputm, l_mc03bf0-usnam,
              l_mc03bf0-vgart, l_mc03bf0-blart, l_mc03bf0-blaum,
              l_mc03bf0-xblnr, l_mc03bf0-frbnr, l_mc03bf0-wever,
              l_mc03bf0-tcode2, l_mc03bf0-bfwms)
      FROM mkpf
      WHERE mblnr = l_mc03bf0-mblnr
        AND mjahr = l_mc03bf0-mjahr.

      IF sy-saprl < '700'.
        TRANSLATE l_mc03bf0-sgtxt USING <crsp>.
        TRANSLATE l_mc03bf0-sgtxt USING <crsp>.
      ELSE.
        TRANSLATE l_mc03bf0-sgtxt USING s_crsp.
        TRANSLATE l_mc03bf0-sgtxt USING s_crsp.
      ENDIF.

      MODIFY c_t_data FROM l_mc03bf0 INDEX l_tabix.
    ENDLOOP.

  WHEN 'ZBW_MM_HOUR_SHORT'.
* Get Remark for Hourly shortage
    REFRESH: lt_remark, r_matnr, r_mtart, r_matkl, r_dispo, r_lifnr.

    l_tabname = 'ZTMM_H_SHORT_EIS'.
    SELECT matnr remark to_date to_time
      INTO TABLE lt_remark
      FROM (l_tabname)
     WHERE to_date >= sy-datum
        OR to_date EQ '00000000'.

    LOOP AT lt_remark.
      l_tabix = sy-tabix.
      IF lt_remark-to_date EQ sy-datum.
        IF lt_remark-to_time EQ space.
          lt_remark-to_time = '240000'.
        ENDIF.
        IF lt_remark-to_time < sy-uzeit.
          DELETE lt_remark INDEX l_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT lt_remark.

* Get Excluding Condition
    l_tabname = 'ZTBW_STG_RANGES'.
    SELECT fieldname low high INTO TABLE lt_stg_range
      FROM (l_tabname).

    LOOP AT lt_stg_range.

      TRANSLATE lt_stg_range-low USING '*^'.
      SEARCH lt_stg_range-low FOR '^'.
      IF sy-subrc EQ 0.
        l_option = 'CP'.
        TRANSLATE lt_stg_range-low USING '^*'.
      ELSEIF lt_stg_range-high IS INITIAL.
        l_option = 'EQ'.
      ELSE.
        l_option = 'BT'.
      ENDIF.
      l_sign = 'I'.

      CASE lt_stg_range-fieldname.
        WHEN 'MATNR'.
          r_matnr-sign   = l_sign.
          r_matnr-option = l_option.
          r_matnr-low    = lt_stg_range-low.
          r_matnr-high   = lt_stg_range-high.
          APPEND r_matnr.
        WHEN 'MTART'.
          r_mtart-sign   = l_sign.
          r_mtart-option = l_option.
          r_mtart-low    = lt_stg_range-low.
          r_mtart-high   = lt_stg_range-high.
          APPEND r_mtart.
        WHEN 'MATKL'.
          r_matkl-sign   = l_sign.
          r_matkl-option = l_option.
          r_matkl-low    = lt_stg_range-low.
          r_matkl-high   = lt_stg_range-high.
          APPEND r_matkl.
        WHEN 'DISPO'.
          r_dispo-sign   = l_sign.
          r_dispo-option = l_option.
          r_dispo-low    = lt_stg_range-low.
          r_dispo-high   = lt_stg_range-high.
          APPEND r_dispo.
        WHEN 'LIFNR'.
          r_lifnr-sign   = l_sign.
          r_lifnr-option = l_option.
          r_lifnr-low    = lt_stg_range-low.
          r_lifnr-high   = lt_stg_range-high.
          APPEND r_lifnr.
      ENDCASE.
    ENDLOOP.

    LOOP AT c_t_data INTO l_zoxud10134.
      l_tabix = sy-tabix.

      READ TABLE r_matnr INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10134-matnr IN r_matnr.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_mtart INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10134-mtart IN r_mtart.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_matkl INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10134-matkl IN r_matkl.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_dispo INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10134-dispo IN r_dispo.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_lifnr INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10134-lifnr IN r_lifnr.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.


      READ TABLE lt_remark WITH KEY matnr = l_zoxud10134-matnr
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10134-remark = lt_remark-remark.
      ENDIF.
      MODIFY c_t_data FROM l_zoxud10134 INDEX l_tabix.
    ENDLOOP.

  WHEN 'ZBW_MM_DAILY_SHORT'.
* Get Remark for Hourly shortage
    REFRESH: lt_remark, r_matnr, r_mtart, r_matkl, r_dispo, r_lifnr.

    l_tabname = 'ZTMM_D_SHORT_EIS'.
    SELECT matnr remark to_date to_time
      INTO TABLE lt_remark
      FROM (l_tabname)
     WHERE to_date >= sy-datum
        OR to_date EQ '00000000'.

    LOOP AT lt_remark.
      l_tabix = sy-tabix.
      IF lt_remark-to_date EQ sy-datum.
        IF lt_remark-to_time EQ space.
          lt_remark-to_time = '240000'.
        ENDIF.
        IF lt_remark-to_time < sy-uzeit.
          DELETE lt_remark INDEX l_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.
    SORT lt_remark.

* Get Excluding Condition
    l_tabname = 'ZTBW_STG_RANGESD'.
    SELECT fieldname low high INTO TABLE lt_stg_range
      FROM (l_tabname).

    LOOP AT lt_stg_range.

      TRANSLATE lt_stg_range-low USING '*^'.
      SEARCH lt_stg_range-low FOR '^'.
      IF sy-subrc EQ 0.
        l_option = 'CP'.
        TRANSLATE lt_stg_range-low USING '^*'.
      ELSEIF lt_stg_range-high IS INITIAL.
        l_option = 'EQ'.
      ELSE.
        l_option = 'BT'.
      ENDIF.
      l_sign = 'I'.

      CASE lt_stg_range-fieldname.
        WHEN 'MATNR'.
          r_matnr-sign   = l_sign.
          r_matnr-option = l_option.
          r_matnr-low    = lt_stg_range-low.
          r_matnr-high   = lt_stg_range-high.
          APPEND r_matnr.
        WHEN 'MTART'.
          r_mtart-sign   = l_sign.
          r_mtart-option = l_option.
          r_mtart-low    = lt_stg_range-low.
          r_mtart-high   = lt_stg_range-high.
          APPEND r_mtart.
        WHEN 'MATKL'.
          r_matkl-sign   = l_sign.
          r_matkl-option = l_option.
          r_matkl-low    = lt_stg_range-low.
          r_matkl-high   = lt_stg_range-high.
          APPEND r_matkl.
        WHEN 'DISPO'.
          r_dispo-sign   = l_sign.
          r_dispo-option = l_option.
          r_dispo-low    = lt_stg_range-low.
          r_dispo-high   = lt_stg_range-high.
          APPEND r_dispo.
        WHEN 'LIFNR'.
          r_lifnr-sign   = l_sign.
          r_lifnr-option = l_option.
          r_lifnr-low    = lt_stg_range-low.
          r_lifnr-high   = lt_stg_range-high.
          APPEND r_lifnr.
      ENDCASE.
    ENDLOOP.

    LOOP AT c_t_data INTO l_zoxud10135.
      l_tabix = sy-tabix.

      READ TABLE r_matnr INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10135-matnr IN r_matnr.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_mtart INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10135-mtart IN r_mtart.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_matkl INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10135-matkl IN r_matkl.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_dispo INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10135-dispo IN r_dispo.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

      READ TABLE r_lifnr INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10135-lifnr IN r_lifnr.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.


      READ TABLE lt_remark WITH KEY matnr = l_zoxud10135-matnr
                             BINARY SEARCH.
      IF sy-subrc EQ 0.
        l_zoxud10135-remark = lt_remark-remark.
      ENDIF.
      MODIFY c_t_data FROM l_zoxud10135 INDEX l_tabix.
    ENDLOOP.

*abap cts
  WHEN 'ZVBW_VRSD'.
    LOOP AT c_t_data INTO l_zoxud10136.
      l_tabix = sy-tabix.

      l_zoxud10136-master = l_zoxud10136-objname.

      CLEAR d010inc.
      SELECT SINGLE *
        FROM d010inc
       WHERE  include = l_zoxud10136-objname.
      IF sy-subrc EQ 0.
        l_zoxud10136-master = d010inc-master.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10136 INDEX l_tabix.

    ENDLOOP.

*ERP statistics
  WHEN 'Z_ZTHRAPPUSGE'.

    LOOP AT c_t_data INTO l_zoxud10097.
      l_tabix = sy-tabix.

      CLEAR tstc.
      SELECT SINGLE *
      FROM tstc
      WHERE pgmna = l_zoxud10097-tcode.

* get t-code using program name
      IF sy-subrc EQ 0 AND tstc-tcode NE l_zoxud10097-tcode.
        l_zoxud10097-tcode = tstc-tcode.
      ENDIF.

* to remove cr lf
      IF sy-saprl < '700'.
        TRANSLATE l_zoxud10097-account USING <crsp>.
      ELSE.
        TRANSLATE l_zoxud10097-account USING s_crsp.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10097 INDEX l_tabix.

    ENDLOOP.

  WHEN 'ZBW_CO_SHOP_SUM'.
    REFRESH: lt_zoxud10174_mip1,  lt_zoxud10174_mip2.

    LOOP AT c_t_data INTO l_zoxud10174.
      l_tabix = sy-tabix.

      CLEAR: l_zoxud10174-zzmip1, l_zoxud10174-zzmip2,
             l_zoxud10174-zzmip1_bwkey,
             l_zoxud10174-zzmip1_manuqty,
             l_zoxud10174-zzmip2_manuqty,
             l_zoxud10174-zzmip1_grqty,
             l_zoxud10174-zzmip2_grqty.


      IF l_zoxud10174-kstar EQ '0000540300'
        AND l_zoxud10174-typps EQ 'M'.
* Amount from Shop CC
        SELECT SUM( manu_amt ) SUM( wkgbtr2 )    "PPC Var
          INTO (l_zoxud10174-manu_amt, l_zoxud10174-wkgbtr2)
          FROM ztco_shop_cc
         WHERE kokrs = l_zoxud10174-kokrs
           AND bdatj = l_zoxud10174-bdatj
           AND poper = l_zoxud10174-poper
           AND typps = l_zoxud10174-typps
           AND kstar = l_zoxud10174-kstar
           AND resou = l_zoxud10174-resou
           AND elemt BETWEEN '010' AND '050'
           AND aufnr = l_zoxud10174-aufnr.
* MIP 1
        CLEAR: l_manu_amt, l_sum_amt, l_sum_amt2.

*        SELECT SUM( manu_amt ) INTO l_sum_amt
*          FROM zbw_co_shop_sum
*         WHERE kokrs = l_zoxud10174-kokrs
*           AND bdatj = l_zoxud10174-bdatj
*           AND poper = l_zoxud10174-poper
*           AND typps = l_zoxud10174-typps
*           AND artnr = l_zoxud10174-llv_matnr.

        SELECT * INTO CORRESPONDING FIELDS OF lt_zoxud10174_mip1
           FROM zbw_co_shop_sum
          WHERE kokrs = l_zoxud10174-kokrs
            AND bdatj = l_zoxud10174-bdatj
            AND poper = l_zoxud10174-poper
            AND typps = l_zoxud10174-typps
            AND artnr = l_zoxud10174-llv_matnr.

          CLEAR lt_zoxud10174_mip1-zzmip2.
          lt_zoxud10174_mip1-artnr  = l_zoxud10174-artnr.
          lt_zoxud10174_mip1-verid  = l_zoxud10174-verid.
          lt_zoxud10174_mip1-zzmip1 = l_zoxud10174-llv_matnr.
          lt_zoxud10174_mip1-zzmip1_bwkey  = l_zoxud10174-bwkey.
          lt_zoxud10174_mip1-zzartnr_werks = l_zoxud10174-par_werks.
          lt_zoxud10174_mip1-zzmip1_manuqty       "MIP1 MANU_QTY
                                      = l_zoxud10174-manu_qty.
          lt_zoxud10174_mip1-zzmip2_manuqty       "MIP2 MANU_QTY
                                      = lt_zoxud10174_mip1-manu_qty.

          SELECT SUM( zf_lbkum ) INTO lt_zoxud10174_mip1-zzmip1_grqty
            FROM ztco_ml_summary
           WHERE kokrs = l_zoxud10174-kokrs
             AND bdatj = l_zoxud10174-bdatj
             AND poper = l_zoxud10174-poper
             AND matnr = lt_zoxud10174_mip1-zzmip1.
*             AND bwkey = lt_zoxud10174_mip1-zzmip1_bwkey.

* Manu Amt, Qty, PPC VAR Amt, Qty, ADD Qty
*          l_manu_amt = lt_zoxud10174_mip1-manu_amt.
*          lt_zoxud10174_mip1-manu_amt = l_zoxud10174-manu_amt
*                           * lt_zoxud10174_mip1-manu_amt / l_sum_amt.
*          lt_zoxud10174_mip1-manu_qty = lt_zoxud10174_mip1-manu_qty
*                           * lt_zoxud10174_mip1-manu_amt / l_manu_amt.
*          lt_zoxud10174_mip1-wkgbtr2 = lt_zoxud10174_mip1-wkgbtr2
*                           * lt_zoxud10174_mip1-manu_amt / l_manu_amt.
*          lt_zoxud10174_mip1-mbgbtr2 = lt_zoxud10174_mip1-mbgbtr2
*                           * lt_zoxud10174_mip1-manu_amt / l_manu_amt.
*          lt_zoxud10174_mip1-mbgbtr  = lt_zoxud10174_mip1-mbgbtr
*                           * lt_zoxud10174_mip1-manu_amt / l_manu_amt.
*

          APPEND lt_zoxud10174_mip1.

        ENDSELECT.
* MIP 2
        LOOP AT lt_zoxud10174_mip1
                           WHERE kokrs  = l_zoxud10174-kokrs
                             AND bdatj  = l_zoxud10174-bdatj
                             AND poper  = l_zoxud10174-poper
                             AND zzmip1 = l_zoxud10174-llv_matnr
                             AND artnr  = l_zoxud10174-artnr
                             AND verid  = l_zoxud10174-verid.
          l_tabix1 = sy-tabix.

          IF lt_zoxud10174_mip1-kstar EQ '0000540300'.      "MIP2

*            SELECT SUM( manu_amt ) INTO l_sum_amt2
*               FROM zbw_co_shop_sum
*              WHERE kokrs = lt_zoxud10174_mip1-kokrs
*                AND bdatj = lt_zoxud10174_mip1-bdatj
*                AND poper = lt_zoxud10174_mip1-poper
*                AND typps = lt_zoxud10174_mip1-typps
*                AND artnr = lt_zoxud10174_mip1-llv_matnr.

            SELECT * INTO CORRESPONDING FIELDS OF lt_zoxud10174_mip2
               FROM zbw_co_shop_sum
              WHERE kokrs = lt_zoxud10174_mip1-kokrs
                AND bdatj = lt_zoxud10174_mip1-bdatj
                AND poper = lt_zoxud10174_mip1-poper
                AND typps = lt_zoxud10174_mip1-typps
                AND artnr = lt_zoxud10174_mip1-llv_matnr.

              lt_zoxud10174_mip2-zzmip1 = l_zoxud10174-llv_matnr.
              lt_zoxud10174_mip2-zzmip2 = lt_zoxud10174_mip1-llv_matnr.
              lt_zoxud10174_mip2-artnr  = l_zoxud10174-artnr.
              lt_zoxud10174_mip2-verid  = l_zoxud10174-verid.
              lt_zoxud10174_mip2-zzmip1_bwkey  = l_zoxud10174-bwkey.
              lt_zoxud10174_mip2-zzartnr_werks = l_zoxud10174-par_werks.

              lt_zoxud10174_mip2-zzmip1_manuqty
                                    = lt_zoxud10174_mip1-zzmip1_manuqty.
              lt_zoxud10174_mip2-zzmip2_manuqty
                                    = lt_zoxud10174_mip1-zzmip2_manuqty.

              lt_zoxud10174_mip2-zzmip1_grqty
                                    = lt_zoxud10174_mip1-zzmip1_grqty.

              SELECT SINGLE zf_lbkum
                INTO lt_zoxud10174_mip2-zzmip2_grqty
                FROM ztco_ml_summary
               WHERE kokrs = l_zoxud10174-kokrs
                 AND bdatj = l_zoxud10174-bdatj
                 AND poper = l_zoxud10174-poper
                 AND matnr = lt_zoxud10174_mip2-zzmip2
                 AND bwkey = lt_zoxud10174_mip2-bwkey.

*              SELECT SUM( manu_amt ) INTO lt_zoxud10174_mip2-manu_amt
*                FROM ztco_shop_cc
*               WHERE kokrs = lt_zoxud10174_mip2-kokrs
*                 AND bdatj = lt_zoxud10174_mip2-bdatj
*                 AND poper = lt_zoxud10174_mip2-poper
*                 AND typps = lt_zoxud10174_mip2-typps
*                 AND kstar = lt_zoxud10174_mip2-kstar
*                 AND resou = lt_zoxud10174_mip2-resou
*                 AND elemt BETWEEN '010' AND '050'
*                 AND aufnr = lt_zoxud10174_mip2-aufnr.

*              l_manu_amt = lt_zoxud10174_mip2-manu_amt.
*              lt_zoxud10174_mip2-manu_amt = lt_zoxud10174_mip1-manu_amt
*                           * lt_zoxud10174_mip2-manu_amt / l_sum_amt2.
*              lt_zoxud10174_mip2-manu_qty = lt_zoxud10174_mip2-manu_qty
*                             * lt_zoxud10174_mip2-manu_amt / l_manu_amt
*.
*              lt_zoxud10174_mip2-wkgbtr2  = lt_zoxud10174_mip2-wkgbtr2
*                             * lt_zoxud10174_mip2-manu_amt / l_manu_amt
*.
*              lt_zoxud10174_mip2-mbgbtr2 = lt_zoxud10174_mip2-mbgbtr2
*                             * lt_zoxud10174_mip2-manu_amt / l_manu_amt
*.
*              lt_zoxud10174_mip2-mbgbtr  = lt_zoxud10174_mip2-mbgbtr
*                             * lt_zoxud10174_mip2-manu_amt / l_manu_amt
*.
*
              APPEND lt_zoxud10174_mip2.
            ENDSELECT.
            IF sy-subrc EQ 0.
              DELETE lt_zoxud10174_mip1 INDEX l_tabix1.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF sy-subrc EQ 0.
          DELETE c_t_data INDEX l_tabix.
          CONTINUE.
        ENDIF.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10174 INDEX l_tabix.

    ENDLOOP.

    APPEND LINES OF lt_zoxud10174_mip1 TO c_t_data.
    APPEND LINES OF lt_zoxud10174_mip2 TO c_t_data.

  WHEN 'ZVBW_PNODID'.

*    DATA: l_pnguid TYPE pnodid-pnguid,
    DATA: o_pnguid TYPE pnodid-pnguid,
          n_pnguid TYPE pnodid-pnguid,
          l_pvguid TYPE posvid-pvguid,
          l_pname TYPE pnodid-pname,
          l_quant TYPE pvwty-quant,
          len TYPE i.

    DATA: BEGIN OF i_obj OCCURS 0,
           pvguid LIKE posvid-pvguid,
          END OF i_obj.

    LOOP AT c_t_data INTO l_zoxud10002.
      l_tabix = sy-tabix.

      o_pnguid = l_zoxud10002-pnguid.

*get object number
      len = strlen( l_zoxud10002-pname ) - 5.
      l_pname = l_zoxud10002-pname+5(len).
      CLEAR l_pnguid.
      SELECT SINGLE pnguid INTO l_pnguid
      FROM pnodid
      WHERE pname = l_pname.

* pnwtyh

      CLEAR pnwtyh.
      SELECT SINGLE *
        FROM pnwtyh
       WHERE pnguid = l_pnguid.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING pnwtyh TO l_zoxud10002.
      ENDIF.

* pnwtyv
      CLEAR pnwtyv.
      SELECT SINGLE *
        FROM pnwtyv
       WHERE pnguid = o_pnguid.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING pnwtyv TO l_zoxud10002.
      ENDIF.

* posvid
      REFRESH i_obj.

      SELECT pvguid INTO TABLE i_obj
      FROM posvid
      WHERE pnguid = o_pnguid.

* PNODTX
      CLEAR pnodtx.
      SELECT SINGLE pntext INTO l_zoxud10002-pntext
      FROM pnodtx
      WHERE pnguid = l_pnguid
        AND spras = 'E'.

* pvwty

      LOOP AT i_obj.
        CLEAR pvwty.
        SELECT SINGLE *
        FROM pvwty
        WHERE pvguid = i_obj-pvguid.
        IF sy-subrc EQ 0.
          CASE pvwty-herst.
            WHEN 'HMMA'.
              l_zoxud10002-zownrat = pvwty-quant.
            WHEN 'HMC'.
              l_zoxud10002-zhqrat = pvwty-quant.
            WHEN 'VENDOR'.
              l_zoxud10002-zsuprat = pvwty-quant.
          ENDCASE.
*        l_ZOXUD10002-ZOWNRAT = l_quant.
        ENDIF.
      ENDLOOP.

*konv
*Labor
      SELECT SINGLE kwert waers
      INTO (l_zoxud10002-zsbll, l_zoxud10002-waers)
      FROM konv
      WHERE knumv = l_zoxud10002-knumv
        AND kschl = 'LABO'.


*PART
      SELECT SINGLE kwert waers
      INTO (l_zoxud10002-zsbpp, l_zoxud10002-waers)
      FROM konv
      WHERE knumv = l_zoxud10002-knumv
        AND kschl = 'PART'.

*SUBL
      SELECT SINGLE kwert waers
      INTO (l_zoxud10002-zsbss, l_zoxud10002-waers)
      FROM konv
      WHERE knumv = l_zoxud10002-knumv
        AND kschl = 'SUBL'.
*

      MODIFY c_t_data FROM l_zoxud10002 INDEX l_tabix.

    ENDLOOP.


* to fix ASN issue
  WHEN 'Z_ZVBW_EDIDC'.
    DATA: l_sdata LIKE edid4-sdata.

    LOOP AT c_t_data INTO l_zoxud10159.

      l_tabix = sy-tabix.

      IF l_zoxud10159-credat GE '20110706'. "GCS go-live date

        CLEAR: edid4, l_sdata.
        SELECT SINGLE sdata INTO l_sdata
          FROM edid4
         WHERE docnum = l_zoxud10159-docnum
*         AND SEGNUM = '000001'
           AND segnam = 'E1ADHDR'. " header

        IF sy-subrc EQ 0 AND l_sdata(9) NE 'ALEAUD100'. "Error
          "Success ALEAUD000.
          DELETE c_t_data INDEX l_tabix.
        ELSE.
          CLEAR: edid4, l_sdata.
          SELECT SINGLE sdata INTO l_sdata
            FROM edid4
           WHERE docnum = l_zoxud10159-docnum
*           AND SEGNUM = '000001'
             AND segnam = 'E1STATE'. " detail
          IF sy-subrc EQ 0.
            l_zoxud10159-zasntext = l_sdata+26(60).
            MODIFY c_t_data FROM l_zoxud10159 INDEX l_tabix.
          ENDIF.
        ENDIF.

      ENDIF.

*      modify C_T_DATA from l_zoxud10159 index L_TABIX.

    ENDLOOP.
* end of fix ASN issue

* Material Ledger Qty
  WHEN 'Z_MM_MLCD_CKMLHD'.
    LOOP AT c_t_data INTO l_zoxud10053.

      l_tabix = sy-tabix.

      CLEAR: l_zoxud10053-pmatn, l_zoxud10053-prwrk, l_zoxud10053-verid.

      IF l_zoxud10053-categ EQ 'VN'.
        IF l_bvalt EQ l_zoxud10053-bvalt.
          l_zoxud10053-pmatn = l_pmatn.
          l_zoxud10053-prwrk = l_prwrk.
          l_zoxud10053-verid = l_verid.
        ELSE.
          SELECT SINGLE pmatn prwrk verid
            INTO (l_zoxud10053-pmatn, l_zoxud10053-prwrk, l_zoxud10053-verid)
            FROM ckmlmv013
           WHERE  kalnr_proc = l_zoxud10053-bvalt.

          l_bvalt = l_zoxud10053-bvalt.
          l_pmatn = l_zoxud10053-pmatn.
          l_prwrk = l_zoxud10053-prwrk.
          l_verid = l_zoxud10053-verid.
        ENDIF.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10053 INDEX l_tabix.


    ENDLOOP.

*CPZP
  WHEN 'ZBW_CPZP'.
    LOOP AT c_t_data INTO l_zoxud10155.
      l_tabix = sy-tabix.

      IF l_zoxud10155-f_objnr(2) EQ 'MK'.
        CLEAR ppc_comat.
        SELECT SINGLE *
          FROM ppc_comat
         WHERE  objid = l_zoxud10155-f_objnr+4(10).
        IF sy-subrc EQ 0.
          l_zoxud10155-matnr = ppc_comat-matnr.
          l_zoxud10155-werks = ppc_comat-werks.
* get standard price
          IF l_zoxud10155-werks IS INITIAL.
            l_zoxud10155-werks = 'P001'.
          ENDIF.

*          DATA: l_peinh LIKE mbew-peinh.
          CLEAR: l_peinh.

          SELECT SINGLE stprs peinh INTO (l_zoxud10155-stprs, l_peinh)
          FROM mbew
          WHERE bwkey = l_zoxud10155-werks
            AND matnr = l_zoxud10155-matnr.
          IF sy-subrc EQ 0.
            l_zoxud10155-stprs = l_zoxud10155-stprs / l_peinh.
          ENDIF.
        ENDIF.
*
        MODIFY c_t_data FROM l_zoxud10155 INDEX l_tabix.
      ELSE.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  WHEN 'ZBW_V_OP'.

    CLEAR r_date.
    r_date-sign   = 'I'.
    r_date-option = 'EQ'.

    LOOP AT c_t_data INTO l_zoxud10157.
      l_tabix = sy-tabix.

      r_date-low = l_zoxud10157-enddate.
      COLLECT r_date.

      IF l_zoxud10157-st_stepcount > space.
        CALL FUNCTION 'BP_JOBLOG_READ'
          EXPORTING
            client                = sy-mandt
            jobcount              = l_zoxud10157-jobcount
            joblog                = l_zoxud10157-joblog
            jobname               = l_zoxud10157-jobname
          TABLES
            joblogtbl             = lt_jlg_tbl
          EXCEPTIONS
            cant_read_joblog      = 1
            jobcount_missing      = 2
            joblog_does_not_exist = 3
            joblog_is_empty       = 4
            joblog_name_missing   = 5
            jobname_missing       = 6
            job_does_not_exist    = 7
            OTHERS                = 8.
        IF sy-subrc EQ 0.
          l_stepcnt = l_zoxud10157-st_stepcount.
          READ TABLE lt_jlg_tbl WITH KEY msgno = '550'
                                         msgv1 = l_stepcnt.
          IF sy-subrc EQ 0.      " Start time
            l_tabix1 = sy-tabix.
            l_zoxud10157-strtdate = lt_jlg_tbl-enterdate.
            l_zoxud10157-strttime = lt_jlg_tbl-entertime.
          ENDIF.



*          IF L_ZOXUD10157-ED_STEPCOUNT EQ SPACE.
*            L_TABIX1 = L_TABIX1 + 1.
*          ELSE.
*            l_stepcnt = L_ZOXUD10157-ED_STEPCOUNT + 1.
*            read table LT_JLG_TBL with key MSGNO = '550'
*                                           MSGV1 = l_stepcnt.
*            IF SY-SUBRC EQ 0.
*              L_TABIX1 = SY-TABIX + 1.
*            ENDIF.
*          ENDIF.
*
          IF l_zoxud10157-ed_stepcount EQ space.
            l_stepcnt = l_stepcnt + 1.
          ELSE.
            l_stepcnt = l_zoxud10157-ed_stepcount + 1.
          ENDIF.

          READ TABLE lt_jlg_tbl WITH KEY msgno = '550'
                                         msgv1 = l_stepcnt.
          IF sy-subrc NE 0.
            DESCRIBE TABLE lt_jlg_tbl LINES l_tabix1.
            READ TABLE lt_jlg_tbl INDEX l_tabix1.  "End Time
          ENDIF.
          l_zoxud10157-enddate = lt_jlg_tbl-enterdate.
          l_zoxud10157-endtime = lt_jlg_tbl-entertime.
        ENDIF.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10157 INDEX l_tabix.

    ENDLOOP.

*    r_date-low  = '20111115'.
*    r_date-high = '20111129'.
*    r_date-option = 'BT'.
*    APPEND R_DATE.

    CLEAR l_zoxud10157.
    SELECT start_jobname AS jobname
           st_stepcount ed_stepcount t_strttime t_endtime
           t_durtime    job_desc     end_jobname
      INTO CORRESPONDING FIELDS OF l_zoxud10157
      FROM ztbw_op_kpi_job
     WHERE start_jobname LIKE '%*'.

      l_jobname = l_zoxud10157-jobname.
      TRANSLATE l_jobname USING '*%'.

      IF r_date IS NOT INITIAL.
        SELECT sdluname strtdate enddate status
               MIN( strttime ) AS strttime
               MAX( endtime )  AS endtime
               " JOBCOUNT JOBGROUP STEPCOUNT JOBNAME JOBLOG
               INTO CORRESPONDING FIELDS OF l_zoxud10157
          FROM tbtco
         WHERE jobname LIKE l_jobname
           AND enddate IN r_date
           AND status = 'F'
         GROUP BY sdluname strtdate enddate status.
          APPEND l_zoxud10157 TO c_t_data.
        ENDSELECT.
      ENDIF.
    ENDSELECT.


  WHEN '1CL_OEQU017'. " Dealer Allocation
    LOOP AT c_t_data INTO l_zoxud10001.
      l_tabix = sy-tabix.

      CLEAR ztsd_um.
      IF ( l_zoxud10001-p_natn_code001 EQ 'B28' ) OR
         ( l_zoxud10001-p_natn_code001 EQ 'B06' AND l_zoxud10001-p_rp18_shop_068 GE '20120816' ).

        SELECT SINGLE dealer_dt status INTO (l_zoxud10001-dealer_dt, l_zoxud10001-status)
        FROM ztsd_um
        WHERE model_code = l_zoxud10001-equnr(3)
          AND body_no = l_zoxud10001-equnr+3(6)
*          AND dealer_dt IS NOT NULL
          AND ( status = ' ' OR status EQ 'F').

*        if sy-subrc eq 0.
        IF  l_zoxud10001-dealer_dt IS INITIAL. "no dealer date
          IF l_zoxud10001-p_rp25_shop_043 IS NOT INITIAL.
            l_zoxud10001-alloc_dt = l_zoxud10001-p_rp25_shop_043.
          ELSEIF l_zoxud10001-p_rp27_shop_053 IS NOT INITIAL.
            l_zoxud10001-alloc_dt = l_zoxud10001-p_rp27_shop_053.
          ENDIF.
        ELSE.
          IF l_zoxud10001-p_rp19_shop_065 GE l_zoxud10001-dealer_dt.
            l_zoxud10001-alloc_dt = l_zoxud10001-p_rp19_shop_065.
          ELSE.
            l_zoxud10001-alloc_dt = l_zoxud10001-dealer_dt.
          ENDIF.
        ENDIF.
*        endif.

      ELSE.
        IF l_zoxud10001-p_rp_status082 GE '21'.
* to get earliest shopdate RP21 or Higher
          DATA:  BEGIN OF i_rpdt OCCURS 0,
                  rpdt LIKE zoxud10001-p_rp21_shop_008,
                 END OF i_rpdt.

          REFRESH i_rpdt.
          IF l_zoxud10001-p_rp21_shop_008 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp21_shop_008.
            APPEND i_rpdt.
          ENDIF.
          IF l_zoxud10001-p_rp22_shop_031 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp22_shop_031.
            APPEND i_rpdt.
          ENDIF.
          IF l_zoxud10001-p_rp23_shop_034 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp23_shop_034.
            APPEND i_rpdt.
          ENDIF.
          IF l_zoxud10001-p_rp24_shop_040 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp24_shop_040.
            APPEND i_rpdt.
          ENDIF.
          IF l_zoxud10001-p_rp25_shop_043 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp25_shop_043.
            APPEND i_rpdt.
          ENDIF.
          IF l_zoxud10001-p_rp26_shop_050 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp26_shop_050.
            APPEND i_rpdt.
          ENDIF.
          IF l_zoxud10001-p_rp27_shop_053 IS NOT INITIAL.
            i_rpdt-rpdt = l_zoxud10001-p_rp27_shop_053.
            APPEND i_rpdt.
          ENDIF.

          SORT i_rpdt.
          READ TABLE i_rpdt INDEX 1.
          IF sy-subrc EQ 0.
            l_zoxud10001-alloc_dt = i_rpdt-rpdt.
          ENDIF.

        ENDIF.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10001 INDEX l_tabix.

*      IF l_ZOXUD10001- NE space.
*        CLEAR ausp.
*        SELECT SINGLE atwrt
*          INTO (l_zoxud10020-en_item_code001)
*          FROM ausp
*         WHERE objek = l_zoxud10020-p_engine_no005
*           AND atinn = '0000003583'.
*        IF sy-subrc EQ 0.

*        ENDIF.
*      ENDIF.
    ENDLOOP.


  WHEN 'ZVBW_PNWTYH'.

    LOOP AT c_t_data INTO l_zoxud10181.
      l_tabix = sy-tabix.

* pnwtyv
      CLEAR l_pnguid.
      CLEAR pnwtyv.
      SELECT SINGLE pnguid knumv
      INTO (l_pnguid, l_zoxud10181-knumv)
        FROM pnwtyv
       WHERE header_guid = l_zoxud10181-pnguid.

* pvwty

      CLEAR pvwty.
      SELECT SINGLE quant INTO l_zoxud10181-zownrat
      FROM pvwty
      WHERE version_guid = l_pnguid
        AND herst = 'HMMA'.

      CLEAR pvwty.
      SELECT SINGLE quant INTO l_zoxud10181-zhqrat
      FROM pvwty
      WHERE version_guid = l_pnguid
        AND herst = 'HMC'.

      CLEAR pvwty.
      SELECT SINGLE quant INTO l_zoxud10181-zsuprat
      FROM pvwty
      WHERE version_guid = l_pnguid
        AND herst = 'VENDOR'.

*konv
*Labor
      SELECT SINGLE kwert waers
      INTO (l_zoxud10181-zsbll, l_zoxud10181-waers)
      FROM konv
      WHERE knumv = l_zoxud10181-knumv
        AND kschl = 'LABO'.

*PART
      SELECT SINGLE kwert waers
      INTO (l_zoxud10181-zsbpp, l_zoxud10181-waers)
      FROM konv
      WHERE knumv = l_zoxud10181-knumv
        AND kschl = 'PART'.

*SUBL
      SELECT SINGLE kwert waers
      INTO (l_zoxud10181-zsbss, l_zoxud10181-waers)
      FROM konv
      WHERE knumv = l_zoxud10181-knumv
        AND kschl = 'SUBL'.
*
      MODIFY c_t_data FROM l_zoxud10181 INDEX l_tabix.

    ENDLOOP.

  WHEN 'Z_ZTSD_UM'.
    LOOP AT c_t_data INTO l_zoxud10022.
      l_tabix = sy-tabix.

      IF  l_zoxud10022-status NE 'F' AND  l_zoxud10022-status NE ' '.
        DELETE c_t_data INDEX l_tabix.
      ENDIF.

    ENDLOOP.

*rate for labor cost reports
  WHEN 'Z_PA0008'.
    LOOP AT c_t_data INTO l_zoxud10095.
      l_tabix = sy-tabix.

      CASE l_zoxud10095-cpind.
        WHEN 'S'. "salary
          IF l_zoxud10095-bet01 NE 0 AND l_zoxud10095-divgv NE 0.
            l_zoxud10095-zrate = l_zoxud10095-bet01 / l_zoxud10095-divgv.
          ENDIF.
        WHEN 'T'. "hourly
          SELECT SINGLE betrg INTO l_zoxud10095-zrate
          FROM t510
          WHERE molga = '10'
            AND trfar = l_zoxud10095-trfar
            AND trfgb = l_zoxud10095-trfgb
            AND trfkz = '1'
            AND trfgr = l_zoxud10095-trfgr
            AND trfst = l_zoxud10095-trfst
            AND endda >= l_zoxud10095-endda
            AND begda <= l_zoxud10095-begda.
        WHEN OTHERS.

      ENDCASE.

      MODIFY c_t_data FROM l_zoxud10095 INDEX l_tabix.

    ENDLOOP.

*monthly claim
  WHEN 'ZTSD_ACM_H'.
    LOOP AT c_t_data INTO l_zoxud10106.
      l_tabix = sy-tabix.

* to remove cr lf
      IF sy-saprl < '700'.
        TRANSLATE l_zoxud10106-zdesc USING <crsp>.
      ELSE.
        TRANSLATE l_zoxud10106-zdesc USING s_crsp.
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10106 INDEX l_tabix.

    ENDLOOP.

  WHEN 'ZVBW_QMEL_REPAIR'.
    LOOP AT c_t_data INTO l_zoxud10190.
      l_tabix = sy-tabix.

*begin of deletion flag
      CLEAR jest.
      SELECT SINGLE *
        FROM jest
       WHERE objnr = l_zoxud10190-objnr
         AND stat = 'I0076'
         AND inact <> 'X'.
      IF sy-subrc EQ 0.
        l_zoxud10190-kzloesch = 'X'.
      ENDIF.
*end of deletion flag

*begin of ppm user status flag
*      DATA: l_inact LIKE jest-inact.
      CLEAR l_inact.

      CLEAR jest.
      SELECT SINGLE *
      FROM jest
     WHERE objnr = l_zoxud10190-objnr
       AND stat = 'E0008'
       AND inact <> 'X'.
      IF sy-subrc EQ 0.
        l_zoxud10190-zchkppm = 'X'.
      ENDIF.

      CLEAR jest.
      SELECT SINGLE *
      FROM jest
     WHERE objnr = l_zoxud10190-objnr
       AND stat = 'E0004'
       AND inact <> 'X'.
      IF sy-subrc EQ 0.
        l_zoxud10190-zsyswsn = 'X'.
      ENDIF.

      CLEAR jest.
      SELECT SINGLE *
      FROM jest
     WHERE objnr = l_zoxud10190-objnr
       AND stat = 'I0072'
       AND inact <> 'X'.
      IF sy-subrc EQ 0.
        l_zoxud10190-zsysstat = 'X'.
      ENDIF.

*end of ppm user status flag

**      IF l_zoxud10164-lifnum EQ ' '.
**        lf_bqpim-matnr = l_zoxud10164-matnr.
**lf_bqpim-cuobj = l_ZOXUD10164-cuobj. "blank
**        lf_bqpim-werks = l_zoxud10164-mawerk.               "P001
**      LF_BQPIM-NEMNG = ABS( T_KIS1-MENGE ). "PriceUnit
**      LF_BQPIM-MEINS = T_KIS1-MEEHT. "UoM
**      LF_BQPIM-LMEIN = T_KIS1-MEEHT. "Units of Measure in WM
**        lf_bqpim-nedat = l_zoxud10164-erdat. "Creation date on
**        lf_bqpim-bstyp = 'B'.          "Purchase requisition
**        lf_bqpim-pstyp = '0'.          "Item category in PO
**        lf_bqpim-vorga = 'B'.          "Transaction/event
**        lf_bqpim-bqpra = '1'.          "Selection of source with price
**        lf_bqpim-noaus = space.        "No box listing sources of supply
**        lf_bqpim-liste = 'X'.                               "553647
**        lf_bqpim-beskz = 'F'.         "External procurement
**        lf_bqpim-msgno = 'X'.         "keine Nachricht
**        lf_bqpim-noquu = 'X'.         "Do not update quota arrangement
**        lf_bqpim-novrt = 'X'.   "Do not search for outlineagreement
**        lf_bqpim-novrt_ord = 'X'.     "Do not search for order ???
**        lf_bqpim-nomei = 'X'.         "Always use order unit from info
**lf_bqpim-usequ = uf_mack2-usequ.   "Quotation arrange usage (Space)
**        lf_bqpim-matnl = 'X'.         "Do not read material
**        lf_bqpim-noqum = 'X'.         "Selection after plan quotation
**        CALL FUNCTION 'ME_SEARCH_SOURCE_OF_SUPPLY'
**          EXPORTING
**            comim = lf_bqpim
**          IMPORTING
**            comex = lf_bqpex.
**
**        l_zoxud10164-lifnum = lf_bqpex-flief.
**      ITAB-INFNR = LF_BQPEX-INFNR.
**
**      ENDIF.

* Begin of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMFE
      CLEAR qmfe.
      SELECT SINGLE otkat otgrp oteil fekat fegrp fecod
      INTO (l_zoxud10190-otkat,
            l_zoxud10190-otgrp,
            l_zoxud10190-oteil,
            l_zoxud10190-fekat,
            l_zoxud10190-fegrp,
            l_zoxud10190-fecod)
       FROM qmfe
       WHERE qmnum = l_zoxud10190-qmnum
         AND fenum = '0001'.
* End of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMFE

* Begin of to get URKAT, URGRP, URCOD, URTXT from table
* QMUR
      CLEAR qmur.
      SELECT SINGLE urkat urgrp urcod urtxt
      INTO (l_zoxud10190-urkat,
            l_zoxud10190-urgrp,
            l_zoxud10190-urcod,
            l_zoxud10190-urtxt)
       FROM qmur
       WHERE qmnum = l_zoxud10190-qmnum
         AND fenum = '0001'
         AND urnum = '0001'.
* End of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMUR

* Begin of to get AUSVN,AUZTV from table
* QMIH
      CLEAR qmih.
      SELECT SINGLE ausvn auztv
      INTO (l_zoxud10190-ausvn,
            l_zoxud10190-auztv)
       FROM qmih
       WHERE qmnum = l_zoxud10190-qmnum.
* End of to get AUSVN,AUZTV from table
* QMIH

* get standard price
      IF l_zoxud10190-mawerk IS INITIAL.
        l_zoxud10190-mawerk = 'P001'.
      ENDIF.

*      DATA: l_peinh LIKE mbew-peinh.
      CLEAR l_peinh.

      SELECT SINGLE stprs peinh INTO (l_zoxud10190-stprs, l_peinh)
      FROM mbew
      WHERE bwkey = l_zoxud10190-mawerk
        AND matnr = l_zoxud10190-matnr.
      IF sy-subrc EQ 0.
        l_zoxud10190-stprs = l_zoxud10190-stprs / l_peinh.
      ENDIF.
*

      MODIFY c_t_data FROM l_zoxud10190 INDEX l_tabix.

    ENDLOOP.


*Vehicle Repair (M/H)
  WHEN 'ZTPP_VEH_REPAIR'.

    REFRESH i_rppt.

    SELECT model body_serial ref_date ref_compl COUNT(*) AS cnt
    INTO CORRESPONDING FIELDS OF TABLE i_rppt
    FROM ztpp_veh_repair
    WHERE rep_mh = 0
    GROUP BY model body_serial ref_date ref_compl.

    SORT i_rppt BY model body_serial ref_date ref_compl.


    LOOP AT c_t_data INTO l_zoxud10192.
      l_tabix = sy-tabix.

*get repair rate
      CLEAR ztpp_repair_rate.
      SELECT SINGLE *
      FROM ztpp_repair_rate
      WHERE model = l_zoxud10192-model
        AND plant_no = l_zoxud10192-plant_no
        AND shop_cd = l_zoxud10192-ref_main
        AND datbi =< l_zoxud10192-ref_date
        AND datab >= l_zoxud10192-ref_date.

*get repair rate 2nd
      IF sy-subrc NE 0.
        CLEAR ztpp_repair_rate.
        SELECT SINGLE *
        FROM ztpp_repair_rate
        WHERE model = l_zoxud10192-model
          AND plant_no = l_zoxud10192-plant_no
          AND shop_cd = l_zoxud10192-rep_dept
          AND datbi =< l_zoxud10192-ref_date
          AND datab >= l_zoxud10192-ref_date.

*get repair rate 3rd
        IF sy-subrc NE 0.
          CLEAR ztpp_repair_rate.
          SELECT SINGLE *
          FROM ztpp_repair_rate
          WHERE plant_no = l_zoxud10192-plant_no
            AND shop_cd = l_zoxud10192-rep_dept
            AND datbi =< l_zoxud10192-ref_date
            AND datab >= l_zoxud10192-ref_date.

*get repair rate 4th
          IF sy-subrc NE 0.
            CLEAR ztpp_repair_rate.
            SELECT SINGLE *
            FROM ztpp_repair_rate
            WHERE plant_no = l_zoxud10192-plant_no
              AND shop_cd = l_zoxud10192-ref_main
              AND datbi =< l_zoxud10192-ref_date
              AND datab >= l_zoxud10192-ref_date.
          ENDIF.

        ENDIF.

      ENDIF.

*get repair m/h
      DATA: l_mh LIKE ztpp_repair_mh-std_mh.

      CLEAR ztpp_repair_mh.


      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = l_zoxud10192-part_cont
*         in_cp             = g_in_cp
*         inter_cp          = g_inter_cp
          replacement       = 32
        IMPORTING
          outtext           = l_zoxud10192-part_cont
*         outused           = ol
*         outoverflow       = ov
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          cannot_convert    = 3
          fields_not_type_c = 4.

*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.

      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = l_zoxud10192-ref_desc
*         in_cp             = g_in_cp
*         inter_cp          = g_inter_cp
          replacement       = 32
        IMPORTING
          outtext           = l_zoxud10192-ref_desc
*         outused           = ol
*         outoverflow       = ov
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          cannot_convert    = 3
          fields_not_type_c = 4.

      SELECT SINGLE *
      FROM ztpp_repair_mh
      WHERE part_cont = l_zoxud10192-part_cont
        AND def_desc = l_zoxud10192-ref_desc.

      l_zoxud10192-shop = ztpp_repair_mh-shop.
      l_zoxud10192-reseve_01 = ztpp_repair_mh-reseve_01.
      l_zoxud10192-reseve_02 = ztpp_repair_mh-reseve_02.

*        l_zoxud10192-rep_mh = ztpp_repair_mh-std_mh.

* if actual M/H > 0 from IQIS
      IF l_zoxud10192-rep_mh > 0.
        l_zoxud10192-hrep_mh = l_zoxud10192-rep_mh.
        l_zoxud10192-zmode = 'A'. " Actual M/H from IQIS

** if there is no STD M/H master
*        IF l_zoxud10192-reseve_01 = ' '.
*          l_zoxud10192-reseve_01 = l_zoxud10192-rep_dept.
*        ENDIF.
*        case l_zoxud10192-rep_dept.
*        when 'PAINT' or 'WELD'.
*        l_zoxud10192-reseve_01 = l_zoxud10192-rep_dept.
*        when others.
*        endcase.

      ELSE.
        l_zoxud10192-hrep_mh = ztpp_repair_mh-std_mh.

* special logic for paint shop using No. of records by vin/date/repair type
        IF l_zoxud10192-rep_dept = 'PAINT'.
          READ TABLE i_rppt INTO wa_rppt
                      WITH KEY   model = l_zoxud10192-model
                                 body_serial = l_zoxud10192-body_serial
                                 ref_date = l_zoxud10192-ref_date
                                 ref_compl = l_zoxud10192-ref_compl
                                 BINARY SEARCH.
          IF sy-subrc EQ 0.

            CLEAR l_def_desc.

            IF wa_rppt-cnt = 1.
              l_def_desc = '1'.
            ELSEIF wa_rppt-cnt = 2.
              l_def_desc = '2'.
            ELSE. " >=3
              l_def_desc = '3+'.
            ENDIF.

*get special m/h for paint shop
            SELECT SINGLE *
            FROM ztpp_repair_mh
            WHERE shop = 'PAINT'
              AND part_cd = wa_rppt-ref_compl
              AND def_desc = l_def_desc.

            IF sy-subrc EQ 0 AND wa_rppt-cnt NE 0.
              l_zoxud10192-reseve_01 = 'PAINT'.
              l_zoxud10192-hrep_mh = ztpp_repair_mh-std_mh / wa_rppt-cnt.
            ENDIF.

          ENDIF.
        ENDIF.

        IF l_zoxud10192-hrep_mh > 0.
          l_zoxud10192-zmode = 'S'. " Standard M/H from SAP
        ENDIF.
      ENDIF.

* if there is no STD M/H master
      IF l_zoxud10192-reseve_01 = ' '.
        l_zoxud10192-reseve_01 = l_zoxud10192-rep_dept.
      ENDIF.

      l_zoxud10192-rep_lab_amt = l_zoxud10192-hrep_mh / 60 * ztpp_repair_rate-rep_lab_rate.
      l_zoxud10192-rep_util_amt = l_zoxud10192-hrep_mh / 60 * ztpp_repair_rate-util_rate.
      l_zoxud10192-rep_submat_amt = l_zoxud10192-hrep_mh / 60 * ztpp_repair_rate-sub_mat_rate.

      MODIFY c_t_data FROM l_zoxud10192 INDEX l_tabix.

    ENDLOOP.

*Vehicle Repair (H/C)
  WHEN 'ZTPP_VEH_REPAIR_01'.

    REFRESH i_rppt.

    SELECT model body_serial ref_date ref_compl COUNT(*) AS cnt
    INTO CORRESPONDING FIELDS OF TABLE i_rppt
    FROM ztpp_veh_repair
    WHERE rep_mh = 0
    GROUP BY model body_serial ref_date ref_compl.

    SORT i_rppt BY model body_serial ref_date ref_compl.

    DATA: l_day(1).

    LOOP AT c_t_data INTO l_zoxud10196.
      l_tabix = sy-tabix.
* only GA
      IF l_zoxud10196-rep_dept = 'ASSEMBLY' OR l_zoxud10196-rep_dept = 'VPC'
        OR l_zoxud10196-rep_dept = 'PAINT' OR l_zoxud10196-rep_dept = 'WELD'
        OR l_zoxud10196-rep_dept = 'STAMPING' OR l_zoxud10196-rep_dept = 'ENGINE'.
*      IF l_zoxud10196-rep_dept = 'ASSEMBLY' OR l_zoxud10196-rep_dept = 'VPC'
*        OR l_zoxud10196-rep_dept = 'PAINT' OR l_zoxud10196-rep_dept = 'WELD'
*        OR ( l_zoxud10196-ref_date >= '20130218' and
*             ( l_zoxud10196-rep_dept = 'STAMPING' or l_zoxud10196-rep_dept = 'ENGINE' ) ).

* change day to end of the day of week (sunday)
        CALL FUNCTION 'DATE_COMPUTE_DAY'
          EXPORTING
            date = l_zoxud10196-ref_date
          IMPORTING
            day  = l_day.

        l_day = 7 - l_day.
        l_zoxud10196-ref_date = l_zoxud10196-ref_date + l_day.
*

*get repair rate
        CLEAR ztpp_repair_rate.
        SELECT SINGLE *
        FROM ztpp_repair_rate
        WHERE model = l_zoxud10196-model
          AND plant_no = l_zoxud10196-plant_no
          AND shop_cd = l_zoxud10196-ref_main
          AND datbi =< l_zoxud10196-ref_date
          AND datab >= l_zoxud10196-ref_date.

*get repair rate 2nd
        IF sy-subrc NE 0.
          CLEAR ztpp_repair_rate.
          SELECT SINGLE *
          FROM ztpp_repair_rate
          WHERE model = l_zoxud10196-model
            AND plant_no = l_zoxud10196-plant_no
            AND shop_cd = l_zoxud10196-rep_dept
            AND datbi =< l_zoxud10196-ref_date
            AND datab >= l_zoxud10196-ref_date.

*get repair rate 3rd
          IF sy-subrc NE 0.
            CLEAR ztpp_repair_rate.
            SELECT SINGLE *
            FROM ztpp_repair_rate
            WHERE plant_no = l_zoxud10196-plant_no
              AND shop_cd = l_zoxud10196-rep_dept
              AND datbi =< l_zoxud10196-ref_date
              AND datab >= l_zoxud10196-ref_date.

*get repair rate 4th
            IF sy-subrc NE 0.
              CLEAR ztpp_repair_rate.
              SELECT SINGLE *
              FROM ztpp_repair_rate
              WHERE plant_no = l_zoxud10196-plant_no
                AND shop_cd = l_zoxud10196-ref_main
                AND datbi =< l_zoxud10196-ref_date
                AND datab >= l_zoxud10196-ref_date.
            ENDIF.

          ENDIF.

        ENDIF.


*get repair m/h
*      DATA: l_mh LIKE ztpp_repair_mh-std_mh.

        CLEAR ztpp_repair_mh.


        CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
          EXPORTING
            intext            = l_zoxud10196-part_cont
*           in_cp             = g_in_cp
*           inter_cp          = g_inter_cp
            replacement       = 32
          IMPORTING
            outtext           = l_zoxud10196-part_cont
*           outused           = ol
*           outoverflow       = ov
          EXCEPTIONS
            invalid_codepage  = 1
            codepage_mismatch = 2
            cannot_convert    = 3
            fields_not_type_c = 4.

*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.

        CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
          EXPORTING
            intext            = l_zoxud10196-ref_desc
*           in_cp             = g_in_cp
*           inter_cp          = g_inter_cp
            replacement       = 32
          IMPORTING
            outtext           = l_zoxud10196-ref_desc
*           outused           = ol
*           outoverflow       = ov
          EXCEPTIONS
            invalid_codepage  = 1
            codepage_mismatch = 2
            cannot_convert    = 3
            fields_not_type_c = 4.

        SELECT SINGLE *
        FROM ztpp_repair_mh
        WHERE part_cont = l_zoxud10196-part_cont
          AND def_desc = l_zoxud10196-ref_desc.


        l_zoxud10196-h_rpmxhr = ztpp_repair_rate-n_w_days * ztpp_repair_rate-wh_p_shift *
                                ztpp_repair_rate-shift * ztpp_repair_rate-rep_h_ct + ztpp_repair_rate-n_prod_ot.

        l_zoxud10196-h_replabm = l_zoxud10196-h_rpmxhr * ztpp_repair_rate-rep_lab_rate.
        l_zoxud10196-h_rputilm = l_zoxud10196-h_rpmxhr * ztpp_repair_rate-util_rate.
        l_zoxud10196-h_rpsubmx = l_zoxud10196-h_rpmxhr * ztpp_repair_rate-sub_mat_rate.

* if actual M/H > 0 from IQIS
        IF l_zoxud10196-rep_mh > 0.
          l_zoxud10196-hrep_mh = l_zoxud10196-rep_mh.
          l_zoxud10196-zmode = 'A'. " Actual M/H from IQIS
** if there is no STD M/H master
*          IF l_zoxud10196-reseve_01 = ' '.
*            l_zoxud10196-reseve_01 = l_zoxud10196-rep_dept.
*          ENDIF.

        ELSE.
          l_zoxud10196-hrep_mh = ztpp_repair_mh-std_mh.

* special logic for paint shop using No. of records by vin/date/repair type
          IF l_zoxud10196-rep_dept = 'PAINT'.
            READ TABLE i_rppt INTO wa_rppt
                        WITH KEY   model = l_zoxud10196-model
                                   body_serial = l_zoxud10196-body_serial
                                   ref_date = l_zoxud10196-ref_date
                                   ref_compl = l_zoxud10196-ref_compl
                                   BINARY SEARCH.
            IF sy-subrc EQ 0.

              CLEAR l_def_desc.

              IF wa_rppt-cnt = 1.
                l_def_desc = '1'.
              ELSEIF wa_rppt-cnt = 2.
                l_def_desc = '2'.
              ELSE. " >=3
                l_def_desc = '3+'.
              ENDIF.

*get special m/h for paint shop
              SELECT SINGLE *
              FROM ztpp_repair_mh
              WHERE shop = 'PAINT'
                AND part_cd = wa_rppt-ref_compl
                AND def_desc = l_def_desc.

              IF sy-subrc EQ 0 AND wa_rppt-cnt NE 0.
                l_zoxud10196-reseve_01 = 'PAINT'.
                l_zoxud10196-hrep_mh = ztpp_repair_mh-std_mh / wa_rppt-cnt.
              ENDIF.

            ENDIF.
          ENDIF.

          IF l_zoxud10196-hrep_mh > 0.
            l_zoxud10196-zmode = 'S'. " Standard M/H from IQIS
          ENDIF.
        ENDIF.

        l_zoxud10196-h_rpmhhr = l_zoxud10196-hrep_mh / 60.

        l_zoxud10196-reseve_01 = ztpp_repair_mh-reseve_01.
        l_zoxud10196-reseve_02 = ztpp_repair_mh-reseve_02.

* if there is no STD M/H master
        IF l_zoxud10196-reseve_01 = ' '.
          l_zoxud10196-reseve_01 = l_zoxud10196-rep_dept.
        ENDIF.

        l_zoxud10196-rep_lab_amt = l_zoxud10196-hrep_mh / 60 * ztpp_repair_rate-rep_lab_rate.
        l_zoxud10196-rep_util_amt = l_zoxud10196-hrep_mh / 60 * ztpp_repair_rate-util_rate.
        l_zoxud10196-rep_submat_amt = l_zoxud10196-hrep_mh / 60 * ztpp_repair_rate-sub_mat_rate.

        MODIFY c_t_data FROM l_zoxud10196 INDEX l_tabix.
      ELSE.
        DELETE c_t_data INDEX l_tabix.
      ENDIF.
    ENDLOOP.

*Vehicle Repair (H/C) - Will be deleted.
  WHEN 'ZTPP_VEH_REPAIR_02'.

    REFRESH i_rppt.

    SELECT model body_serial ref_date ref_compl COUNT(*) AS cnt
    INTO CORRESPONDING FIELDS OF TABLE i_rppt
    FROM ztpp_veh_repair
    WHERE rep_mh = 0
    GROUP BY model body_serial ref_date ref_compl.

    SORT i_rppt BY model body_serial ref_date ref_compl.

    CLEAR: l_day.

    LOOP AT c_t_data INTO l_zoxud10197.
      l_tabix = sy-tabix.

* change day to end of the day of week (sunday)
      CALL FUNCTION 'DATE_COMPUTE_DAY'
        EXPORTING
          date = l_zoxud10197-ref_date
        IMPORTING
          day  = l_day.

      l_day = 7 - l_day.
      l_zoxud10197-ref_date = l_zoxud10197-ref_date + l_day.
*

*get repair rate
      CLEAR ztpp_repair_rate.
      SELECT SINGLE *
      FROM ztpp_repair_rate
      WHERE model = l_zoxud10197-model
        AND plant_no = l_zoxud10197-plant_no
        AND shop_cd = l_zoxud10197-ref_main
        AND datbi =< l_zoxud10197-ref_date
        AND datab >= l_zoxud10197-ref_date.

*get repair m/h
*      DATA: l_mh LIKE ztpp_repair_mh-std_mh.

      CLEAR ztpp_repair_mh.


      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = l_zoxud10197-part_cont
*         in_cp             = g_in_cp
*         inter_cp          = g_inter_cp
          replacement       = 32
        IMPORTING
          outtext           = l_zoxud10197-part_cont
*         outused           = ol
*         outoverflow       = ov
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          cannot_convert    = 3
          fields_not_type_c = 4.

*IF sy-subrc <> 0.
** Implement suitable error handling here
*ENDIF.

      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = l_zoxud10197-ref_desc
*         in_cp             = g_in_cp
*         inter_cp          = g_inter_cp
          replacement       = 32
        IMPORTING
          outtext           = l_zoxud10197-ref_desc
*         outused           = ol
*         outoverflow       = ov
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          cannot_convert    = 3
          fields_not_type_c = 4.

      SELECT SINGLE *
      FROM ztpp_repair_mh
      WHERE part_cont = l_zoxud10197-part_cont
        AND def_desc = l_zoxud10197-ref_desc.

*    IF sy-subrc EQ 0.
*to convert hours
* if actual M/H > 0 from IQIS
      IF l_zoxud10197-rep_mh > 0.
        l_zoxud10197-hrep_mh = l_zoxud10197-rep_mh.
        l_zoxud10197-zmode = 'A'. " Actual M/H from IQIS
      ELSE.
        l_zoxud10197-hrep_mh = ztpp_repair_mh-std_mh.

* special logic for paint shop using No. of records by vin/date/repair type
        IF l_zoxud10192-rep_dept = 'PAINT'.
          READ TABLE i_rppt INTO wa_rppt
                      WITH KEY   model = l_zoxud10192-model
                                 body_serial = l_zoxud10192-body_serial
                                 ref_date = l_zoxud10192-ref_date
                                 ref_compl = l_zoxud10192-ref_compl.
          IF sy-subrc EQ 0.

            CLEAR l_def_desc.

            IF wa_rppt-cnt = 1.
              l_def_desc = '1'.
            ELSEIF wa_rppt-cnt = 2.
              l_def_desc = '2'.
            ELSE. " >=3
              l_def_desc = '3+'.
            ENDIF.

*get special m/h for paint shop
            SELECT SINGLE *
            FROM ztpp_repair_mh
            WHERE shop = 'PAINT'
              AND part_cd = wa_rppt-ref_compl
              AND def_desc = l_def_desc.

            IF sy-subrc EQ 0 AND wa_rppt-cnt NE 0.
              l_zoxud10192-hrep_mh = ztpp_repair_mh-std_mh / wa_rppt-cnt.
            ENDIF.

          ENDIF.
        ENDIF.

        IF l_zoxud10197-hrep_mh > 0.
          l_zoxud10197-zmode = 'S'. " Standard M/H from IQIS
        ENDIF.
      ENDIF.

      l_zoxud10197-h_rpmhhr = l_zoxud10197-hrep_mh / 60.

      l_zoxud10197-reseve_01 = ztpp_repair_mh-reseve_01.
      l_zoxud10197-reseve_02 = ztpp_repair_mh-reseve_02.

      l_zoxud10197-rep_lab_amt = l_zoxud10197-hrep_mh / 60 * ztpp_repair_rate-rep_lab_rate.
      l_zoxud10197-rep_util_amt = l_zoxud10197-hrep_mh / 60 * ztpp_repair_rate-util_rate.
      l_zoxud10197-rep_submat_amt = l_zoxud10197-hrep_mh / 60 * ztpp_repair_rate-sub_mat_rate.

      l_zoxud10197-h_rpmxhr = ztpp_repair_rate-n_w_days * ztpp_repair_rate-wh_p_shift *
                              ztpp_repair_rate-shift * ztpp_repair_rate-rep_h_ct + ztpp_repair_rate-n_prod_ot.

      l_zoxud10197-h_replabm = l_zoxud10197-h_rpmxhr * ztpp_repair_rate-rep_lab_rate.
      l_zoxud10197-h_rputilm = l_zoxud10197-h_rpmxhr * ztpp_repair_rate-util_rate.
      l_zoxud10197-h_rpsubmx = l_zoxud10197-h_rpmxhr * ztpp_repair_rate-sub_mat_rate.

*    ENDIF.

      MODIFY c_t_data FROM l_zoxud10197 INDEX l_tabix.

    ENDLOOP.

* Est. Sign-off plan vs. allocation
  WHEN 'ZVBW_UM_SP'.

    DATA: l_ustatus LIKE ztsd_um-status.
    DATA: l_uzvin LIKE ztsd_um-zvin.

    LOOP AT c_t_data INTO l_zoxud10130.

      l_tabix = sy-tabix.

*      CLEAR: l_ustatus, l_uzvin.

      SELECT SINGLE wo_serial wo_nation wo_dealer
                    wo_extc   wo_intc   intno
                    zvin urgency urgcdate
                    status ship_out model_code
                    body_no devno baanr
                    carno wo_dealer1 dealer_dt
                    plan_date oser invno
                    flet flet_vm_udate flet_vm_utime
        INTO (l_zoxud10130-wo_serial, l_zoxud10130-wo_nation, l_zoxud10130-wo_dealer,
              l_zoxud10130-wo_extc, l_zoxud10130-wo_intc, l_zoxud10130-intno,
              l_zoxud10130-uzvin, l_zoxud10130-urgency, l_zoxud10130-urgcdate,
              l_zoxud10130-ustatus, l_zoxud10130-ship_out, l_zoxud10130-model_code,
              l_zoxud10130-body_no, l_zoxud10130-devno, l_zoxud10130-baanr,
              l_zoxud10130-carno, l_zoxud10130-wo_dealer1, l_zoxud10130-dealer_dt,
              l_zoxud10130-plan_date, l_zoxud10130-oser, l_zoxud10130-invno,
              l_zoxud10130-flet, l_zoxud10130-flet_vm_udate, l_zoxud10130-flet_vm_utime)
        FROM ztsd_um
       WHERE zvin = l_zoxud10130-zvin
         AND wo_nation = l_zoxud10130-work_order+9(3)
         AND status = 'F'.

      IF sy-subrc NE 0.
        SELECT SINGLE wo_serial wo_nation wo_dealer
                  wo_extc   wo_intc   intno
                  zvin urgency urgcdate
                  status ship_out model_code
                  body_no devno baanr
                  carno wo_dealer1 dealer_dt
                  plan_date oser invno
                  flet flet_vm_udate flet_vm_utime
      INTO (l_zoxud10130-wo_serial, l_zoxud10130-wo_nation, l_zoxud10130-wo_dealer,
            l_zoxud10130-wo_extc, l_zoxud10130-wo_intc, l_zoxud10130-intno,
            l_zoxud10130-uzvin, l_zoxud10130-urgency, l_zoxud10130-urgcdate,
            l_zoxud10130-ustatus, l_zoxud10130-ship_out, l_zoxud10130-model_code,
            l_zoxud10130-body_no, l_zoxud10130-devno, l_zoxud10130-baanr,
            l_zoxud10130-carno, l_zoxud10130-wo_dealer1, l_zoxud10130-dealer_dt,
            l_zoxud10130-plan_date, l_zoxud10130-oser, l_zoxud10130-invno,
            l_zoxud10130-flet, l_zoxud10130-flet_vm_udate, l_zoxud10130-flet_vm_utime)
      FROM ztsd_um
     WHERE zvin = l_zoxud10130-zvin
       AND wo_nation = l_zoxud10130-work_order+9(3)
       AND status = ' '.
      ENDIF.

      IF l_zoxud10130-body_no NE '000000' AND l_zoxud10130-body_no NE ' '.
        CONCATENATE l_zoxud10130-model_code l_zoxud10130-body_no INTO l_zoxud10130-h_vm_vs.
      ENDIF.

      IF l_zoxud10130-model_code EQ ' ' OR l_zoxud10130-model_code IS INITIAL.
        CONCATENATE l_zoxud10130-mi(2) 'F' INTO l_zoxud10130-model_code.
        l_zoxud10130-wo_nation = l_zoxud10130-work_order+9(3).
        l_zoxud10130-wo_dealer = l_zoxud10130-work_order+12(2).
      ENDIF.

      MODIFY c_t_data FROM l_zoxud10130 INDEX l_tabix.

    ENDLOOP.

  WHEN 'ZBW_ZTFI_WAR_POST'. " FI War Post
    LOOP AT c_t_data INTO l_zoxud10182.
      l_tabix = sy-tabix.

      CLEAR ztfi_war_post.
      SELECT SINGLE sal_qty
        INTO (l_zoxud10182-sal_qty)
        FROM ztfi_war_post
       WHERE bukrs = l_zoxud10182-bukrs
         AND gjahr = l_zoxud10182-gjahr
         AND monat = l_zoxud10182-monat
         AND matnr = l_zoxud10182-matnr
         AND m_gjahr = l_zoxud10182-m_gjahr
         AND s_gjahr = l_zoxud10182-s_gjahr
         AND s_monat = l_zoxud10182-s_monat.
      IF sy-subrc EQ 0.
        l_zoxud10182-meins = 'EA'.
        MODIFY c_t_data FROM l_zoxud10182 INDEX l_tabix.
      ENDIF.

      IF sy-saprl < '700'.
        TRANSLATE l_zoxud10182-descr USING <crsp>.
      ELSE.
        TRANSLATE l_zoxud10182-descr USING s_crsp.
      ENDIF.

    ENDLOOP.

  WHEN 'ZTFI_WAR_SALES'. " FI War Sales
    LOOP AT c_t_data INTO l_zoxud10203.
      l_tabix = sy-tabix.

      CLEAR ztfi_war_sales.
      SELECT SINGLE sal_qty
        INTO (l_zoxud10203-sal_qty)
        FROM ztfi_war_sales
       WHERE bukrs = l_zoxud10203-bukrs
         AND s_gjahr = l_zoxud10203-s_gjahr
         AND monat = l_zoxud10203-monat
         AND land1 = l_zoxud10203-land1
         AND model = l_zoxud10203-model
         AND m_gjahr = l_zoxud10203-m_gjahr
         AND matnr = l_zoxud10203-matnr.

      IF sy-subrc EQ 0.
        l_zoxud10203-meins = 'EA'.
        MODIFY c_t_data FROM l_zoxud10203 INDEX l_tabix.
      ENDIF.

    ENDLOOP.

  WHEN 'ZBW_CKMLPP'. " CKML


* Get excluding condition
    REFRESH: r_matnr, r_mtart, r_matkl, r_dispo, r_lifnr, lt_stg_range.

* Get Excluding Condition
    l_tabname = 'ZTBW_OVS_RANGES'.
    SELECT fieldname low high INTO TABLE lt_stg_range
      FROM (l_tabname).

    LOOP AT lt_stg_range.

      TRANSLATE lt_stg_range-low USING '*^'.
      SEARCH lt_stg_range-low FOR '^'.
      IF sy-subrc EQ 0.
        l_option = 'CP'.
        TRANSLATE lt_stg_range-low USING '^*'.
      ELSEIF lt_stg_range-high IS INITIAL.
        l_option = 'EQ'.
      ELSE.
        l_option = 'BT'.
      ENDIF.
      l_sign = 'I'.

      CASE lt_stg_range-fieldname.
        WHEN 'MATNR'.
          r_matnr-sign   = l_sign.
          r_matnr-option = l_option.
          r_matnr-low    = lt_stg_range-low.
          r_matnr-high   = lt_stg_range-high.
          APPEND r_matnr.
        WHEN 'MTART'.
          r_mtart-sign   = l_sign.
          r_mtart-option = l_option.
          r_mtart-low    = lt_stg_range-low.
          r_mtart-high   = lt_stg_range-high.
          APPEND r_mtart.
        WHEN 'MATKL'.
          r_matkl-sign   = l_sign.
          r_matkl-option = l_option.
          r_matkl-low    = lt_stg_range-low.
          r_matkl-high   = lt_stg_range-high.
          APPEND r_matkl.
        WHEN 'DISPO'.
          r_dispo-sign   = l_sign.
          r_dispo-option = l_option.
          r_dispo-low    = lt_stg_range-low.
          r_dispo-high   = lt_stg_range-high.
          APPEND r_dispo.
        WHEN 'LIFNR'.
          r_lifnr-sign   = l_sign.
          r_lifnr-option = l_option.
          r_lifnr-low    = lt_stg_range-low.
          r_lifnr-high   = lt_stg_range-high.
          APPEND r_lifnr.
      ENDCASE.
    ENDLOOP.

    LOOP AT c_t_data INTO l_zoxud10183.
      l_tabix = sy-tabix.

*get material
      CLEAR ckmlhd.
      SELECT SINGLE matnr bwkey INTO (l_zoxud10183-matnr, l_zoxud10183-bwkey)
      FROM ckmlhd
      WHERE kalnr = l_zoxud10183-kalnr.

      READ TABLE r_matnr INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10183-matnr IN r_matnr.
*        DELETE c_t_data INDEX l_tabix.
*        CONTINUE.
        l_zoxud10183-zflag = 'E'.
      ENDIF.

*get valuation class
      CLEAR mbewh.
      SELECT SINGLE bklas INTO l_zoxud10183-bklas
      FROM mbewh
      WHERE matnr = l_zoxud10183-matnr
        AND bwkey = l_zoxud10183-bwkey
        AND lfgja = l_zoxud10183-bdatj
        AND lfmon = l_zoxud10183-poper.

      READ TABLE r_matnr INDEX 1.
      IF sy-subrc EQ 0 AND l_zoxud10183-matnr IN r_matnr.
*        DELETE c_t_data INDEX l_tabix.
*        CONTINUE.
        l_zoxud10183-zflag = 'E'.

      ENDIF.

*      READ TABLE r_mtart INDEX 1.
*      IF sy-subrc EQ 0 AND l_zoxud10134-mtart IN r_mtart.
*        DELETE c_t_data INDEX l_tabix.
*        CONTINUE.
*      ENDIF.
*
*      READ TABLE r_matkl INDEX 1.
*      IF sy-subrc EQ 0 AND l_zoxud10134-matkl IN r_matkl.
*        DELETE c_t_data INDEX l_tabix.
*        CONTINUE.
*      ENDIF.
*
*      READ TABLE r_dispo INDEX 1.
*      IF sy-subrc EQ 0 AND l_zoxud10134-dispo IN r_dispo.
*        DELETE c_t_data INDEX l_tabix.
*        CONTINUE.
*      ENDIF.
*
*      READ TABLE r_lifnr INDEX 1.
*      IF sy-subrc EQ 0 AND l_zoxud10134-lifnr IN r_lifnr.
*        DELETE c_t_data INDEX l_tabix.
*        CONTINUE.
*      ENDIF.

      CLEAR: l_fiscper, l_fiscper1.
      CONCATENATE l_zoxud10183-bdatj l_zoxud10183-poper INTO l_fiscper.

      CLEAR: l_bdatj, l_poper.
      EXEC SQL.
        select max( bdatj||poper )
          into :l_fiscper1
          from ckmlpp
         where MANDT =  :SY-MANDT
           AND bdatj <= :l_zoxud10183-bdatj
           AND to_char(bdatj)||to_char(poper) <= :l_fiscper
           AND kalnr = :l_zoxud10183-kalnr
           AND ( zukumo <> 0 OR vnkumo <> 0 )
      ENDEXEC.

      IF NOT l_fiscper1 IS INITIAL.
        l_bdatj = l_fiscper1(4).
        l_poper = l_fiscper1+5(2).

        CLEAR: l_mdate, l_sdate.
        CONCATENATE l_bdatj l_poper+1(2) '01' INTO l_mdate.
        CONCATENATE l_zoxud10183-bdatj l_zoxud10183-poper+1(2) '01' INTO l_cdate.

        CLEAR: emonths.
        CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          EXPORTING
            i_date_from = l_mdate
            i_date_to   = l_cdate
          IMPORTING
*           e_days      =
            e_months    = emonths
*           e_years     =
          .
        l_zoxud10183-znnsob = emonths.

      ENDIF.

*get amount
      CLEAR ckmlcr.
      SELECT SINGLE *
        INTO CORRESPONDING FIELDS OF l_zoxud10183
        FROM ckmlcr
       WHERE kalnr = l_zoxud10183-kalnr
         AND bdatj = l_zoxud10183-bdatj
         AND poper = l_zoxud10183-poper
         AND untper = l_zoxud10183-untper.
*      IF sy-subrc EQ 0.
      MODIFY c_t_data FROM l_zoxud10183 INDEX l_tabix.
*      ENDIF.

    ENDLOOP.

*ASN Detail
  WHEN 'ZVBW_ZMMT0102'.

    DATA: l_seq LIKE l_zoxud10163-ifseq.
    CLEAR l_seq.

    LOOP AT c_t_data INTO l_zoxud10163.
      l_tabix = sy-tabix.

      SELECT MIN( ifseq ) INTO l_seq
      FROM zmmt0102
      WHERE ifkey = l_zoxud10163-ifkey
        AND ifp03 = l_zoxud10163-ifp03.
*
      IF l_zoxud10163-ifseq EQ l_seq. " if 1st occurred records by ship id
        MODIFY c_t_data FROM l_zoxud10163 INDEX l_tabix.
      ELSE.
        DELETE c_t_data INDEX l_tabix.
        CONTINUE.
      ENDIF.

    ENDLOOP.

  WHEN 'ZVBW_QMEL_MM'.
    LOOP AT c_t_data INTO l_zoxud10193.
      l_tabix = sy-tabix.

*begin of gathering the downtime
      CLEAR qmel.
      SELECT SINGLE lndwntime INTO l_lndwntime
        FROM qmel
       WHERE  qmnum = l_zoxud10193-qmnum.
      IF sy-subrc EQ 0.
        l_zoxud10193-lndwntime = l_lndwntime.
      ENDIF.
*end of gathering the downtime

*begin of deletion flag
      CLEAR jest.
      SELECT SINGLE *
        FROM jest
       WHERE objnr = l_zoxud10193-objnr
         AND stat = 'I0076'
         AND inact <> 'X'.
      IF sy-subrc EQ 0.
        l_zoxud10193-kzloesch = 'X'.
      ENDIF.
*end of deletion flag

*begin of ppm user status flag
      CLEAR l_inact.

      CASE l_zoxud10193-qmart.
        WHEN 'Q3'.
          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10193-objnr
           AND stat = 'E0008'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10193-zchkppm = 'X'.
          ENDIF.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10193-objnr
           AND stat = 'E0004'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10193-zsyswsn = 'X'.
          ENDIF.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10193-objnr
           AND stat = 'I0072'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10193-zsysstat = 'X'.
          ENDIF.

        WHEN 'Q4'.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10193-objnr
           AND stat = 'E0001'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10193-zchkppm = 'X'.
          ENDIF.

          CLEAR jest.
          SELECT SINGLE *
          FROM jest
         WHERE objnr = l_zoxud10193-objnr
           AND stat = 'I0072'
           AND inact <> 'X'.
          IF sy-subrc EQ 0.
            l_zoxud10193-zsysstat = 'X'.
          ENDIF.

        WHEN OTHERS.

      ENDCASE.

*end of ppm user status flag

      IF l_zoxud10193-lifnum EQ ' '.
        lf_bqpim-matnr = l_zoxud10193-matnr.
*lf_bqpim-cuobj = l_ZOXUD10193-cuobj. "blank
        lf_bqpim-werks = l_zoxud10193-mawerk.               "P001
*      LF_BQPIM-NEMNG = ABS( T_KIS1-MENGE ). "PriceUnit
*      LF_BQPIM-MEINS = T_KIS1-MEEHT. "UoM
*      LF_BQPIM-LMEIN = T_KIS1-MEEHT. "Units of Measure in WM
        lf_bqpim-nedat = l_zoxud10193-erdat. "Creation date on
        lf_bqpim-bstyp = 'B'.          "Purchase requisition
        lf_bqpim-pstyp = '0'.          "Item category in PO
        lf_bqpim-vorga = 'B'.          "Transaction/event
        lf_bqpim-bqpra = '1'.          "Selection of source with price
        lf_bqpim-noaus = space.        "No box listing sources of supply
        lf_bqpim-liste = 'X'.                               "553647
        lf_bqpim-beskz = 'F'.         "External procurement
        lf_bqpim-msgno = 'X'.         "keine Nachricht
        lf_bqpim-noquu = 'X'.         "Do not update quota arrangement
        lf_bqpim-novrt = 'X'.   "Do not search for outlineagreement
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

        l_zoxud10193-lifnum = lf_bqpex-flief.
*      ITAB-INFNR = LF_BQPEX-INFNR.

      ENDIF.

* Begin of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMFE
      CLEAR qmfe.
      SELECT SINGLE otkat otgrp oteil fekat fegrp fecod
      INTO (l_zoxud10193-otkat,
            l_zoxud10193-otgrp,
            l_zoxud10193-oteil,
            l_zoxud10193-fekat,
            l_zoxud10193-fegrp,
            l_zoxud10193-fecod)
       FROM qmfe
       WHERE qmnum = l_zoxud10193-qmnum
         AND fenum = '0001'.
* End of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMFE

* Begin of to get URKAT, URGRP, URCOD, URTXT from table
* QMUR
      CLEAR qmur.
      SELECT SINGLE urkat urgrp urcod urtxt
      INTO (l_zoxud10193-urkat,
            l_zoxud10193-urgrp,
            l_zoxud10193-urcod,
            l_zoxud10193-urtxt)
       FROM qmur
       WHERE qmnum = l_zoxud10193-qmnum
         AND fenum = '0001'
         AND urnum = '0001'.
* End of to get OTKAT, OTGRP, OTEIL, FEKAT, FEGRP, FECOD from table
* QMUR

* Begin of to get AUSVN,AUZTV from table
* QMIH
      CLEAR qmih.
      SELECT SINGLE ausvn auztv
      INTO (l_zoxud10193-ausvn,
            l_zoxud10193-auztv)
       FROM qmih
       WHERE qmnum = l_zoxud10193-qmnum.
* End of to get AUSVN,AUZTV from table
* QMIH

* get standard price
      IF l_zoxud10193-mawerk IS INITIAL.
        l_zoxud10193-mawerk = 'P001'.
      ENDIF.

*      DATA: l_peinh LIKE mbew-peinh.
      CLEAR l_peinh.

      SELECT SINGLE stprs peinh INTO (l_zoxud10193-stprs, l_peinh)
      FROM mbewh
      WHERE bwkey = l_zoxud10193-mawerk
        AND matnr = l_zoxud10193-matnr
        AND lfgja = l_zoxud10193-qmdat(4)
        AND lfmon = l_zoxud10193-qmdat+4(2).
      IF sy-subrc EQ 0.
        l_zoxud10193-stprs = l_zoxud10193-stprs / l_peinh.
      ELSE.

        SELECT SINGLE stprs peinh INTO (l_zoxud10193-stprs, l_peinh)
        FROM mbew
        WHERE bwkey = l_zoxud10193-mawerk
          AND matnr = l_zoxud10193-matnr.
        IF sy-subrc EQ 0.
          l_zoxud10193-stprs = l_zoxud10193-stprs / l_peinh.
        ENDIF.

      ENDIF.


*



      MODIFY c_t_data FROM l_zoxud10193 INDEX l_tabix.

    ENDLOOP.

  WHEN 'ZVBW_PPC_HEAD_RP'.        "FSC WIP

    REFRESH: lt_ppc_ord, lt_bw_ppc_head_rp.

    LOOP AT c_t_data INTO l_zoxud10213.     "Delete non WIP
      lt_ppc_ord_all-orderid = l_zoxud10213-orderid.
      COLLECT lt_ppc_ord_all.
    ENDLOOP.

    LOOP AT lt_ppc_ord_all.
      CLEAR: zvbw_ppc_head_rp, lt_ppc_ord_rp_cnt, lt_bw_ppc_head_rp.

      SELECT *
        FROM zvbw_ppc_head_rp
       WHERE orderid = lt_ppc_ord_all-orderid
         AND ( flg_gr_head  EQ 'X' OR flg_scrap EQ 'X' )
       order by postdate.

        IF zvbw_ppc_head_rp-flg_reversal EQ 'X'.
          zvbw_ppc_head_rp-confquant = zvbw_ppc_head_rp-confquant * -1.
        ENDIF.

        lt_ppc_ord_rp_cnt-confquant    = lt_ppc_ord_rp_cnt-confquant
                                       + zvbw_ppc_head_rp-confquant.
      ENDSELECT.

      CHECK lt_ppc_ord_rp_cnt-confquant > 0.

      l_tabix     = sy-tabix.
      l_date      = zvbw_ppc_head_rp-postdate.
      l_date+6(2) = '01'.

      CLEAR: zvbw_ppc_head_rp, lt_ppc_ord_rp_cnt.
      SELECT *
        FROM zvbw_ppc_head_rp
       WHERE orderid = lt_ppc_ord_all-orderid
         AND postdate     < l_date.
        IF zvbw_ppc_head_rp-flg_reversal EQ 'X'.
          zvbw_ppc_head_rp-confquant = zvbw_ppc_head_rp-confquant * -1.
        ENDIF.

        lt_ppc_ord_rp_cnt-confquant    = lt_ppc_ord_rp_cnt-confquant
                                       + zvbw_ppc_head_rp-confquant.
      endselect.
      IF lt_ppc_ord_rp_cnt-confquant EQ 0.
        lt_ppc_ord-orderid = lt_ppc_ord_all-orderid.
        COLLECT lt_ppc_ord.
      ENDIF.
    ENDLOOP.

    SORT lt_ppc_ord.
    CLEAR lt_ppc_ord_rp_cnt.

    LOOP AT c_t_data INTO l_zoxud10213.
      l_tabix     = sy-tabix.
      READ TABLE lt_ppc_ord WITH KEY orderid = l_zoxud10213-orderid
                                     BINARY SEARCH.
      IF sy-subrc EQ 0.
        DELETE c_t_data INDEX l_tabix. CONTINUE.
      ENDIF.

      l_date      = l_zoxud10213-postdate.
      l_date+6(2) = '01'.

      IF lt_ppc_ord_rp_cnt-orderid NE l_zoxud10213-orderid.

        REFRESH: lt_ppc_ord_rp_cnt, lt_ppc_ord_rp_cnt2.
        SELECT *
          FROM zvbw_ppc_head_rp
         WHERE orderid = l_zoxud10213-orderid.
          lt_ppc_ord_rp_cnt-orderid      = zvbw_ppc_head_rp-orderid.
          lt_ppc_ord_rp_cnt-spmon        = zvbw_ppc_head_rp-postdate(6).
          lt_ppc_ord_rp_cnt-reppoint_ext = zvbw_ppc_head_rp-reppoint_ext.
          lt_ppc_ord_rp_cnt-confquant    = zvbw_ppc_head_rp-confquant.
          IF zvbw_ppc_head_rp-flg_reversal EQ 'X'.
            lt_ppc_ord_rp_cnt-confquant = lt_ppc_ord_rp_cnt-confquant * -1.
          ENDIF.
          COLLECT lt_ppc_ord_rp_cnt.

          MOVE-CORRESPONDING lt_ppc_ord_rp_cnt TO lt_ppc_ord_rp_cnt2.
          COLLECT lt_ppc_ord_rp_cnt2.
        ENDSELECT.

        delete lt_ppc_ord_rp_cnt where confquant eq 0.
        SORT lt_ppc_ord_rp_cnt BY SPMON DESCENDING REPPOINT_EXT DESCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_ppc_ord_rp_cnt COMPARING ORDERID SPMON.

        delete lt_ppc_ord_rp_cnt2 where confquant eq 0.
        SORT lt_ppc_ord_rp_cnt2.

      ENDIF.


      IF l_zoxud10213-flg_gr_head EQ 'X' OR l_zoxud10213-flg_scrap EQ 'X'.

        l_zoxud10213-confquant = l_zoxud10213-confquant * -1.

      ELSE.
        CLEAR: lt_ppc_ord_rp_cnt, lt_ppc_ord_rp_cnt2.
        READ TABLE lt_ppc_ord_rp_cnt WITH KEY orderid = l_zoxud10213-orderid
                                               reppoint_ext = l_zoxud10213-reppoint_ext.
        IF lt_ppc_ord_rp_cnt-confquant eq 0.
          CLEAR l_zoxud10213-confquant.
        else.
          READ TABLE lt_ppc_ord_rp_cnt2 WITH KEY orderid = l_zoxud10213-orderid
                                               reppoint_ext = l_zoxud10213-reppoint_ext
                                               BINARY SEARCH.
          if lt_ppc_ord_rp_cnt2-confquant eq 0.
            CLEAR l_zoxud10213-confquant.
          else.
            LOOP AT lt_ppc_ord_rp_cnt WHERE spmon < l_zoxud10213-postdate(6).
              " reppoint_ext < l_zoxud10213-reppoint_ext
              lt_bw_ppc_head_rp  = l_zoxud10213.
              lt_bw_ppc_head_rp-reppoint_ext = lt_ppc_ord_rp_cnt-reppoint_ext.
              if l_zoxud10213-flg_reversal eq ' '.
                lt_bw_ppc_head_rp-confquant = lt_bw_ppc_head_rp-confquant * -1.
              endif.
              APPEND lt_bw_ppc_head_rp.
              EXIT.
            ENDLOOP.
          endif.
        ENDIF.

      ENDIF.

      IF l_zoxud10213-flg_reversal EQ 'X'.
        l_zoxud10213-confquant = l_zoxud10213-confquant * -1.
      ENDIF.

      IF l_zoxud10213-confquant EQ 0.
        DELETE c_t_data INDEX l_tabix.
      ELSE.
        MODIFY c_t_data FROM l_zoxud10213 INDEX l_tabix.
      ENDIF.


    ENDLOOP.

    LOOP AT lt_bw_ppc_head_rp.
      append lt_bw_ppc_head_rp to C_T_DATA.
    ENDLOOP.

ENDCASE.
