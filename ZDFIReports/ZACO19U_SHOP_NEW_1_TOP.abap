*&---------------------------------------------------------------------*
*&  Include           ZACO19U_SHOP_NEW_TOP
*&---------------------------------------------------------------------*

*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
TABLES : mara,mbew,mbewh,marc,
         aufk, afru,
         ztco_shop_sum_1, ztco_shop_cc_1,
         keko,keph, t001w, tckh4, tj01, kkbcs_out, tck03,
         crhd,plpo, plko, mkal, ckhs, covp,
         cosl,cosp, coss, ccss, cost, cosb, hcstruktur, kkb_head,
         mlcd,ckmlhd, ckmlmv001, ckmlcr, kkb_ml_pos,
         cobk,coep, ccr1t, csla,
         cospa_key, cosla_key, cossa_key, cosba_key,
         tka01,
         zsco_shopcost_001,
         zsco_shopcost_003,
         zsco_shopcost_key,
         qrp_s_wip_scrap,
         qrp_quantities,
         ztco_abispost, ztco_mhpcpost,bapicstgva,bapimateri,
         bapiplant,bapivalidi,bapicstgst,bapicostingversion,
         ckmlprkeko,ckmlprkeph .

*Structure
TABLES: zsco_shopcost_c.
*OBJNR  Object number
*TYPPS  Item category
*KSTAR  Cost element
*RESOU  Resource
*MATNR  Material number
*KOSTL  Cost Center
*LSTAR  Activity Type
*WERKS  Plant
*ARTNR  Product number
*VERID  Production version

*-----------------------------------------------------------------*
* Constants
*------------------------------------------------------------------*
CONSTANTS : c_bpl(4) VALUE 'ZPC1',
            c_std(4) VALUE 'PPC1',
            c_act(4) VALUE 'PPC1'.
CONSTANTS : c_fsc(4)       VALUE 'FSC',
            c_halb(4)      VALUE 'SEMI',
            c_fsc_plant(4) VALUE 'KVA1'.
*            c_rp_plnnr(8)  VALUE 'RP'.

*CONSTANTS : c_cat_m(1)     VALUE 'M',
*            c_cat_e(1)     VALUE 'E',
*            c_cat_v(1)     VALUE 'V'.
CONSTANTS :
            c_typ_round(1)      VALUE 'U',
            c_typ_wip(1)        VALUE 'W'.

*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*

DATA: it_unitpc  LIKE bapi0012_actprices OCCURS 0 WITH HEADER LINE,
      it_unitpp  LIKE bapi0012_actprices OCCURS 0 WITH HEADER LINE.
DATA: it_bapiret2 LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

*Previous period variable
DATA: l_prev_perd LIKE  ckmlpp-poper,
      l_prev_year LIKE  ckmlpp-bdatj.


* type Pools for Variable Ratio Table
TYPE-POOLS: gseth .


DATA : it_nodes  TYPE gseth_node_tab
                 WITH HEADER LINE ,
       it_values TYPE gseth_val_tab
                 WITH HEADER LINE .

DATA : g_batch(1).

*message
DATA: gt_msg(256) TYPE c.

* Part Price
DATA: i_mbew LIKE mbewh OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
*   Macro                                                              *
*----------------------------------------------------------------------*
DEFINE def_co_sum_tab.
  data : begin of it_cos&1a occurs 0.
  data :  beweg like kkbcs_out-beweg.
          include structure cos&1a.
  data : end of it_cos&1a.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
* TYPE-POOL
TYPE-POOLS:
  ccs00,
  ccs01,
  ckmv0,
  ckmv3,
  vrm,
  ckmd,
  slis,
  ckru0.


** Internal Tables
RANGES : r_budat FOR cobk-budat.

* Temp. Table for Main ITAB
* actual unit price; 5 digit; data element = BASE_PRICE_SCALE

DATA : BEGIN OF it_shop_sum OCCURS 0.
        INCLUDE STRUCTURE ztco_shop_sum_1. "it_ztco_shopcost_a2.
*WIP
DATA :  vmpec   LIKE mbew-vmpei, "current period (WIP)
        vmstc   LIKE mbew-vmstp, "
        vmpep   LIKE mbew-vmpei, "previous period (WIP)
        vmstp   LIKE mbew-vmstp,

*//Modify..04/27/2011..T00020..
        wip_f      TYPE zcoamt4,
        wip_pf     TYPE zcoamt4,
        wip_qty_f  LIKE cpzp-istmn,
        wip_pqty_f LIKE cpzp-istmn,
*//
*data :  control_cost   type awkgr,
*        target_cost    type awkgr,
END OF  it_shop_sum.
DATA : w_shopcost LIKE it_shop_sum.


* Actual - Cost Component
DATA : it_res_ccsum   LIKE ztco_shop_sum_1  OCCURS 0 WITH HEADER LINE.
DATA : it_shop_cc     LIKE ztco_shop_cc_1   OCCURS 0 WITH HEADER LINE.
DATA : it_shop_cc_adj LIKE ztco_shop_cc_1   OCCURS 0 WITH HEADER LINE.

DATA:
  lt_shop_cc TYPE SORTED TABLE OF ztco_shop_cc_1 WITH HEADER LINE
             WITH UNIQUE KEY typps kstar resou elemt aufnr artnr.

DATA : it_tckh3  LIKE tckh3  OCCURS 0 WITH HEADER LINE.
DATA : it_mlkeph LIKE mlkeph OCCURS 0 WITH HEADER LINE.

TYPES:
  BEGIN OF ty_elemt,
    elnum TYPE ck_el_hv,
    elemt TYPE ck_element,
    eltyp,
  END OF ty_elemt.
DATA it_elemt TYPE HASHED TABLE OF ty_elemt WITH HEADER LINE
              WITH UNIQUE KEY elnum.
*EL_HV  CK_EL_HV
*EL_HF  CK_EL_HF
*//Modify..04/01/2011..T00020..
DATA : BEGIN OF gt_multi OCCURS 0,
         kjahr    LIKE mlkeph-kjahr,
         belnr    LIKE mlkeph-belnr,
         posnr    LIKE mlkeph-posnr,
         elemt    LIKE tckh3-elemt,
         amt_f    LIKE ztco_shop_cc-multi_amt_f,
         mamt_f   LIKE ztco_shop_cc-multi_mamt_f ,
         samt_f   LIKE ztco_shop_cc-multi_samt_f,
       END OF gt_multi.

TYPES:
  BEGIN OF ty_cc_amt,
*    gubun(15),
    kalnr    TYPE ck_kalnr,
    bdatj    TYPE bdatj,
    poper    TYPE poper,
    kkzst    TYPE ck_stufe,
    elemt    LIKE it_shop_cc-elemt,
    dmbtr    LIKE it_shop_cc-wip_amt,
    dmbtr_f  LIKE it_shop_cc-wip_amt_f,
  END OF ty_cc_amt.
DATA: gt_cc_amt_new TYPE SORTED TABLE OF ty_cc_amt WITH HEADER LINE
                    WITH UNIQUE KEY kalnr bdatj poper kkzst elemt.
DATA: gt_cc_amt_cur TYPE SORTED TABLE OF ty_cc_amt WITH HEADER LINE
                    WITH UNIQUE KEY kalnr bdatj poper kkzst elemt.

DATA : BEGIN OF fsc_std_cc_new OCCURS 0 ,
        kalnr    TYPE ck_kalnr,
        kkzst    TYPE ck_stufe,
        artnr    LIKE it_shop_cc-artnr,
        elemt    LIKE it_shop_cc-elemt,
        dmbtr    LIKE it_shop_cc-wip_amt,
        dmbtr_f  LIKE it_shop_cc-wip_amt_f,
       END OF fsc_std_cc_new.
*//

DATA : BEGIN OF gt_cc_amt OCCURS 0 ,
        elemt    LIKE it_shop_cc-elemt,
        dmbtr    LIKE it_shop_cc-wip_amt,
        dmbtr_f  LIKE it_shop_cc-wip_amt_f,
       END OF gt_cc_amt.

DATA : BEGIN OF fsc_std_cc OCCURS 0 ,
        artnr    LIKE it_shop_cc-artnr,
        elemt    LIKE it_shop_cc-elemt,
        dmbtr    LIKE it_shop_cc-wip_amt,
        dmbtr_f  LIKE it_shop_cc-wip_amt_f,
       END OF fsc_std_cc.

DATA : BEGIN OF fsc_std_ccsum OCCURS 0 ,
        artnr    LIKE it_shop_cc-artnr,
        dmbtr    LIKE it_shop_cc-wip_amt,
        dmbtr_f  LIKE it_shop_cc-wip_amt_f,

*//Modify..04/27/2011..T00020..
        dmbtr_p  LIKE it_shop_cc-wip_amt,
        dmbtr_pf LIKE it_shop_cc-wip_amt_f,
*//
       END OF fsc_std_ccsum.

* Commented out by Han Moon
**//Modify..04/27/2011..T00020..
*DATA : BEGIN OF fsc_std_cp OCCURS 0 ,
*        artnr    LIKE it_shop_cc-artnr,
*        elemt    LIKE it_shop_cc-elemt,
*        dmbtr    LIKE it_shop_cc-wip_amt,
*        dmbtr_f  LIKE it_shop_cc-wip_amt_f,
*       END OF fsc_std_cp.
**//

DATA : BEGIN OF it_cc_wip_scale OCCURS 0 ,
        aufnr        LIKE it_shop_cc-aufnr,
        artnr        LIKE it_shop_cc-artnr,
        elemt        LIKE it_shop_cc-elemt,
        gr_amt       LIKE it_shop_cc-gr_amt,
        gr_amt_f     LIKE it_shop_cc-gr_amt_f,
        single_amt   LIKE it_shop_cc-single_amt,
        single_amt_f LIKE it_shop_cc-single_amt_f,
        multi_amt    LIKE it_shop_cc-multi_amt,
        multi_amt_f  LIKE it_shop_cc-multi_amt_f,
        wip_amt      LIKE it_shop_cc-wip_amt,
        wip_amt_f    LIKE it_shop_cc-wip_amt_f,
        wip_pamt     LIKE it_shop_cc-wip_pamt,
        wip_pamt_f   LIKE it_shop_cc-wip_pamt_f,
       END OF it_cc_wip_scale.
DATA : BEGIN OF it_cc_round OCCURS 0 ,
        aufnr        LIKE it_shop_cc-aufnr,
        artnr        LIKE it_shop_cc-artnr,
        elemt        LIKE it_shop_cc-elemt,
        gr_amt       LIKE it_shop_cc-gr_amt,
        gr_amt_f     LIKE it_shop_cc-gr_amt_f,
        single_amt   LIKE it_shop_cc-single_amt,
        single_amt_f LIKE it_shop_cc-single_amt_f,
        multi_amt    LIKE it_shop_cc-multi_amt,
        multi_amt_f  LIKE it_shop_cc-multi_amt_f,
*        wip_amt      like it_shop_cc-wip_amt,
*        wip_amt_f    like it_shop_cc-wip_amt_f,
*        wip_pamt     like it_shop_cc-wip_pamt,
*        wip_pamt_f   like it_shop_cc-wip_pamt_f,
       END OF it_cc_round.

*target---
DATA : BEGIN OF it_ml_scale OCCURS 0 ,
        aufnr        LIKE it_shop_cc-aufnr,
        artnr        LIKE it_shop_cc-artnr,
        elemt        LIKE it_shop_cc-elemt,
        gr_amt       LIKE it_shop_cc-wip_amt,
        gr_amt_f     LIKE it_shop_cc-wip_amt_f,
        diff_amt     LIKE it_shop_cc-wip_amt,
        diff_amt_f   LIKE it_shop_cc-wip_amt_f,
       END OF it_ml_scale.

DATA : BEGIN OF it_obj_sum_wip OCCURS 0 ,
        objnr    LIKE it_shop_sum-objnr,
*        typps    like it_shop_cc-typps,
*        kstar    like it_shop_cc-kstar,
        wip_pamt LIKE it_shop_cc-wip_pamt,
        wip_amt  LIKE it_shop_cc-wip_amt,

*//Modify..04/27/2011..T00020..
        wip_pf   LIKE it_shop_cc-wip_pamt,
        wip_f    LIKE it_shop_cc-wip_amt,
*//
       END OF it_obj_sum_wip.

TYPES: BEGIN OF tt_obj_value,
        objnr       LIKE aufk-objnr,
        kstar       TYPE kstar     , "Cost Element
        typps       TYPE typps,
        resou(25)   TYPE c,

        amount      LIKE cosb-wkg001,
        quantity    LIKE cosb-meg001,

        amount2     LIKE cosb-wkg001,
        quantity2   LIKE cosb-meg001,

        matnr       TYPE matnr,
        werks       TYPE werks_d,
        kostl       LIKE onrkl-kostl,
        lstar       LIKE onrkl-lstar,
       END OF tt_obj_value.

TYPES tt_obj_tbl TYPE STANDARD TABLE OF tt_obj_value WITH DEFAULT KEY.


DATA : it_obj_scrap TYPE tt_obj_tbl WITH HEADER LINE,
       it_obj_wip   TYPE tt_obj_tbl WITH HEADER LINE.

DATA : BEGIN OF it_cosba OCCURS 0.
        INCLUDE STRUCTURE cosba.
DATA : END OF it_cosba.

TYPES:
       t_ckmllacr_type     TYPE STANDARD TABLE OF ckmllacr
                            WITH KEY kalnr poper bdatj untper curtp.

DATA : lt_kalnr            TYPE ckmv0_laobj_tbl WITH HEADER LINE,
       lt_kalnr2           TYPE ckmv0_laobj_tbl WITH HEADER LINE,
       it_prkeko_temp      TYPE mlccs_t_prkeko,
       it_prkeph_temp      TYPE mlccs_t_prkeph,
       it_prkeko_fsc_temp  TYPE mlccs_t_prkeko,
       it_prkeph_fsc_temp  TYPE mlccs_t_prkeph,
       it_ckmllahd         LIKE ckmllahd OCCURS 0 WITH HEADER LINE,
       ef_ckmllahd         TYPE ckmllahd,
       lt_ckmlprkeko_temp  TYPE mlccs_t_prkeko,
       lt_ckmlprkeph_temp  TYPE mlccs_t_prkeph,
       lt_ckmllacr         TYPE t_ckmllacr_type.
TYPES:
  BEGIN OF ty_ckobject,
    kalnr TYPE ckmlhd-kalnr,
    matnr TYPE marc-matnr,
    werks TYPE marc-werks,
    beskz TYPE marc-beskz,
    sobsl TYPE marc-sobsl,
  END OF ty_ckobject.

DATA: it_ckobject TYPE HASHED TABLE OF ty_ckobject WITH HEADER LINE
                  WITH UNIQUE KEY kalnr.

DATA: it_prkeko TYPE HASHED TABLE OF ckmlprkeko WITH HEADER LINE
                WITH UNIQUE KEY kalnr bdatj poper.
DATA: it_prkeph TYPE HASHED TABLE OF ckmlprkeph WITH HEADER LINE
                WITH UNIQUE KEY kalnr bdatj poper kkzst.

*DATA: it_prkeko_fsc   LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
*DATA: it_prkeph_fsc   LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
DATA: it_ckmlprkeko   LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
DATA: it_ckmlprkeph   LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
DATA: it_ckmllacr     LIKE ckmllacr   OCCURS 0 WITH HEADER LINE.

*DATA: it_prkeko_p     LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
*DATA: it_prkeph_p     LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
DATA: it_ckmlprkeko_p LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
DATA: it_ckmlprkeph_p LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
DATA: it_ckmllacr_p   LIKE ckmllacr   OCCURS 0 WITH HEADER LINE.

*//Modify..03/24/2011..T00020..
*DATA: it_ztco0025     LIKE ztco0025   OCCURS 0 WITH HEADER LINE.

* Commented out by Han Moon
**//Modify..04/27/2011..T00020..
*DATA: it_prkeko_fsc_p LIKE ckmlprkeko OCCURS 0 WITH HEADER LINE.
*DATA: it_prkeph_fsc_p LIKE ckmlprkeph OCCURS 0 WITH HEADER LINE.
*//

DATA: ld_wa_db_price LIKE ckmllacr,
      ld_wa_db_keph  LIKE ckmlprkeph,
      ld_wa_db_keko  LIKE ckmlprkeko.

DATA : BEGIN OF it_act OCCURS 0,
        kostl LIKE onrkl-kostl,
        lstar LIKE onrkl-lstar,
        objnr LIKE onrkl-objnr,
        kalnr LIKE ckmllahd-kalnr,
       END OF it_act.


* For Cost Components
TYPES:
  BEGIN OF ty_kstar,
    kstar LIKE ztco_shop_cc-kstar,
    elemt LIKE ztco_shop_cc-elemt,
    katyp LIKE cskb-katyp,
  END OF ty_kstar.
DATA it_kstar TYPE HASHED TABLE OF ty_kstar WITH HEADER LINE
              WITH UNIQUE KEY kstar.

*DATA : BEGIN OF IT_CSKB  OCCURS 0.
*DATA :  ELEMT LIKE TCKH2-ELEMT,
*        KSTAR LIKE CSKB-KSTAR,
*        KATYP like cskb-KATYP.
*DATA : END OF IT_CSKB .


*data : begin of temp_shop occurs 0,
*       artnr like ztco_shop_sum-artnr,
*       aufnr like ztco_shop_sum-aufnr,
*       werks like ztco_shop_sum-par_werks,
*       verid like ztco_shop_sum-verid,
*       objnr like ztco_shop_sum-objnr,
*       end of temp_shop.

DATA : it_ckmlkeph LIKE ckmlkeph OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_ckmlmv001 OCCURS 0,
        kaln1      LIKE mbew-kaln1,
        kalnr      LIKE ckmlmv001-kalnr,
        proc_kalnr LIKE ckmlmv001-proc_kalnr,  "proc.process
        pmatn_nd   LIKE ckmlmv001-pmatn_nd,
        verid_nd   LIKE ckmlmv001-verid_nd,
       END OF it_ckmlmv001.

FIELD-SYMBOLS: <f_field> ,<f_field2> .

DATA : g_scale     TYPE p DECIMALS 6,
       g_item_qty  TYPE p DECIMALS 6,
       g_std(1),
       g_round(1).

*prev.shop data
DATA: it_prevshop LIKE ztco_shop_sum OCCURS 0 WITH HEADER LINE.
DATA: g_pr_lfgja  LIKE ckmlpp-bdatj,
      g_pr_lfmon  LIKE ckmlpp-poper.

* Temp. Container for IC
DATA : BEGIN OF it_sc_m OCCURS 0.
        INCLUDE STRUCTURE it_shop_sum.
DATA : END OF it_sc_m.
DATA : BEGIN OF it_sc_v OCCURS 0.
        INCLUDE STRUCTURE it_shop_sum.
DATA : END OF it_sc_v.
DATA : BEGIN OF it_sc_e OCCURS 0.
        INCLUDE STRUCTURE it_shop_sum.
DATA : END OF it_sc_e.

* KEKO
DATA : BEGIN OF it_ckikekokey OCCURS 0.
        INCLUDE STRUCTURE ckikekokey.
DATA : bdatj TYPE bdatj,
       poper TYPE poper.
DATA : kadat LIKE keko-kadat,
       bidat LIKE keko-bidat,
       matnr LIKE keko-matnr,
       werks LIKE keko-werks,
       bwkey LIKE keko-bwkey,
       bwtar LIKE keko-bwtar.
DATA : END OF it_ckikekokey.
*  For CK13n Cost Components
DATA : BEGIN OF it_kkb_split_ck13n OCCURS 0.
DATA : bdatj TYPE bdatj,
       poper TYPE poper.
DATA : matnr LIKE keko-matnr,
       werks LIKE keko-werks,
       bwkey LIKE keko-bwkey,
       bwtar LIKE keko-bwtar,
       elemt LIKE kkb_split-elemt,
       w000  LIKE kkb_split-w000.
DATA : END OF it_kkb_split_ck13n.

*Inventory acct
DATA: i_t030 LIKE t030 OCCURS 0 WITH HEADER LINE.

*  For MLCCS
DATA : it_keph_mlcd  TYPE ccs01_t_keph_mlcd
                     WITH HEADER LINE .
DATA : it_kkb_split  LIKE STANDARD TABLE OF kkb_split
                     WITH HEADER LINE .
*  For Valuation Info. of Materials

DATA: BEGIN OF it_cokey OCCURS 0,
        hrkft   LIKE cokey-hrkft,
        matnr   LIKE cokey-matnr,
        werks   LIKE cokey-werks,
        hkgrp   LIKE cokey-hkgrp,
        subkey2 LIKE cokey-subkey2,
      END OF it_cokey.

*data : begin of it_mat occurs 100,
*        matnr like macku-matnr,
*        werks like marc-werks,
*        bwkey like macku-bwkey,
*        bwtar like macku-bwtar,
*        meins like mara-meins,   "Base UoM
*        raube like mara-raube,   "Shop code
*        fevor like marc-fevor,   "Production scheduler
*
*        mtart like macku-mtart,
*        profil like marc-profil,  "DI B/F profile
*        sauft  like marc-sauft,   "REM indicator
*        beskz like marc-beskz,   "Procurement Type
*        sobsl like marc-sobsl,   "Special procurement type
*        vspvb like marc-vspvb,   "Proposed Supply Area
*        kaln1 like mbew-kaln1,
*        bklas like mbew-bklas,
*        kstar like coep-kstar,   "consumption account
*
*        peinh like mbew-peinh, "Price unit
*        stprs like mbew-stprs, "Standard price
*        verpr type base_price_scale,  "Periodic unit price
*        lfgja like mbew-lfgja,
*        lfmon like mbew-lfmon,
*
**WIP
*        vmpec like mbew-vmpei, "current period (WIP)
*        vmstc like mbew-vmstp,
*        vmpep like mbew-vmpei, "previous period (WIP)
*        vmstp like mbew-vmstp,
*
*        kalst     like ckmlmv011-kalst,   "Costing Level
*        abrechdat like ckmlhd-abrechdat,  "Date of last price det.
**        "Cost est number for cost est w/o qty structure
*       end of it_mat.

TYPES: BEGIN OF tt_mat.
        INCLUDE STRUCTURE zsco_shop_mat_1.
*TYPES:   PRCTR LIKE MARC-PRCTR,  "profit center
**       fevor like marc-fevor,
*        RAUBE LIKE MARA-RAUBE,
TYPES: END OF tt_mat.

DATA: it_mat TYPE HASHED TABLE OF tt_mat WITH HEADER LINE
             WITH UNIQUE KEY matnr werks.

DATA: it_mat_tmp TYPE tt_mat OCCURS 0 WITH HEADER LINE.

*data : begin of it_fsc_mat occurs 100,
*        objnr  like coep-objnr,
*        aufnr  like aufk-aufnr,
*        verid  like mkal-verid,
*        categ(3) type c,          "DI, REM(MTS), MTO
*        klvarp like afko-klvarp.  "costing variant for planned cost
*        include structure it_mat.
*data :  proc_kalnr like ckmlmv001-proc_kalnr,
*       end of it_fsc_mat.
DATA : BEGIN OF it_fsc_mat OCCURS 0.
        INCLUDE STRUCTURE zsco_shop_fsc_mat_1.
DATA:  END OF it_fsc_mat.

DATA: gt_ztcou100 LIKE ztcou100 OCCURS 0 WITH HEADER LINE.

DATA: tper_kalnr TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE.

TYPES: BEGIN OF tt_ckmlcr,
          bdatj   LIKE ckmlcr-bdatj,   "Modify..04/07/2011..T00020
          poper   LIKE ckmlcr-poper,   "Modify..04/07/2011..T00020
          kalnr   LIKE ckmlcr-kalnr,
          peinh   LIKE ckmlcr-peinh,
          vprsv   LIKE ckmlcr-vprsv,
          stprs   LIKE ckmlcr-stprs,  "STD
          pvprs   LIKE ckmlcr-pvprs,  "MAP
          abprd_o LIKE ckmlcr-abprd_o, "price diff
          abkdm_o LIKE ckmlcr-abkdm_o,
          zuprd_o LIKE ckmlcr-zuprd_o,
          zukdm_o LIKE ckmlcr-zukdm_o,

          abkumo  LIKE ckmlpp-abkumo, "begin inv.
          zukumo  LIKE ckmlpp-zukumo, "GR
        END OF tt_ckmlcr.

DATA: BEGIN OF t_ckmlpp OCCURS 0.
        INCLUDE STRUCTURE ckmlpp.
DATA: END OF t_ckmlpp.

DATA: BEGIN OF t_ckmlcr OCCURS 0.
        INCLUDE STRUCTURE ckmlcr.
DATA: END OF t_ckmlcr.


DATA : BEGIN OF it_mbewh OCCURS 0,
        matnr    LIKE mbewh-matnr,
        bwkey    LIKE mbewh-bwkey,
        bwtar    LIKE mbewh-bwtar,
        peinh    LIKE mbewh-peinh,
        stprs    LIKE mbewh-stprs,
        verpr    LIKE mbewh-verpr,
        bklas    LIKE mbewh-bklas,
        lfgja    LIKE mbewh-lfgja,
        lfmon    LIKE mbewh-lfmon,
       END OF it_mbewh .


DATA : BEGIN OF it_obj_gr OCCURS 0,
        objnr     LIKE coep-objnr,
        werks     LIKE coep-werks,
        matnr     LIKE coep-matnr,
        grqty     LIKE coep-mbgbtr, "Positive Sign
       END OF it_obj_gr.
DATA : BEGIN OF it_fsc_gr OCCURS 0,
        werks     LIKE coep-werks,
        matnr     LIKE coep-matnr,
        lotsize   TYPE ck_losgr,    "Lot Size
        grqty     LIKE coep-mbgbtr, "Positive Sign
       END OF it_fsc_gr.

DATA : it_log LIKE ztco_batch_log OCCURS 0 WITH HEADER LINE.


*MTO - scrap
DATA : BEGIN OF it_mto_scrap OCCURS 0,
        objnr  LIKE coep-objnr,

        aufnr  LIKE afru-aufnr,
        smeng LIKE afru-smeng,    "Plan Qty
        lmnga LIKE afru-lmnga,    "Yield Qty
        xmnga LIKE afru-xmnga,    "Scrap Qty
        rmnga LIKE afru-rmnga,    "Rework Qty
       END OF it_mto_scrap.

* ZVCO_RP1 (Report Point Linkage)
*DATA : it_zvco_rp1       LIKE STANDARD TABLE OF zvco_rp1
*                         WITH HEADER LINE .
* For BAPI
DATA : it_costcenterlist LIKE STANDARD TABLE OF bapi0012_cclist
                         WITH HEADER LINE.
DATA : it_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.
DATA : BEGIN  OF it_cctr  OCCURS 0,
        shop  LIKE ztco_shopcost_a2-shop,
        kostl LIKE csks-kostl.
DATA : END    OF it_cctr.


* CO Summary Tables...
*def_co_sum_tab l.
*def_co_sum_tab p.
*def_co_sum_tab s.
*def_co_sum_tab b.

DATA : BEGIN OF it_cosla OCCURS 0.
DATA :  beweg LIKE kkbcs_out-beweg.
        INCLUDE STRUCTURE cosla.
DATA : END OF it_cosla.

TYPES: BEGIN OF tt_categ,                                   " OCCURS 0,
         objnr LIKE covp-objnr,
         kstar LIKE covp-kstar,
         vrgng LIKE covp-vrgng,
         beweg LIKE kkbcs_out-beweg,
       END OF tt_categ.
DATA : it_categ TYPE HASHED TABLE OF tt_categ WITH HEADER LINE
                WITH UNIQUE KEY objnr kstar vrgng.

DATA : BEGIN OF it_gi_misc OCCURS 0,
        objnr	TYPE j_objnr,
        matnr     LIKE coep-matnr,
        kstar     LIKE coep-kstar,
        add_wkgbtr LIKE ztco_shop_sum-add_wkgbtr,
        add_mbgbtr LIKE ztco_shop_sum-add_mbgbtr,
       END OF  it_gi_misc.

DATA : BEGIN OF it_doc_round OCCURS 0,
        artnr TYPE matnr,
        belnr LIKE mlit-belnr,
        posnr LIKE mlit-posnr,
       END OF it_doc_round.

* For COEP
DATA : BEGIN OF it_covp OCCURS 0,
*       belnr  LIKE coep-belnr,
*       buzei  TYPE buzei,
       objnr  TYPE j_objnr   ,
       kstar  TYPE kstar     ,
       werks  TYPE werks_d   ,
       matnr  TYPE matnr     ,

       parob  TYPE parob     ,  "Partner Obj
       hrkft  TYPE co_subkey ,  "CO key subnumber
       vrgng  TYPE co_vorgang,  "CO business transaction
       beknz  TYPE beknz     ,  "Dr/Cr
       meinb  LIKE covp-meinb,  "Unit of measure

       wkgbtr	TYPE covp-wkgbtr,  "Amt
       mbgbtr	LIKE covp-mbgbtr,  "Qty

       sgtxt    LIKE coep-sgtxt,   "To get material info

       awtyp    LIKE covp-awtyp,
*      beweg    LIKE kkbcs_out-beweg,   "Biz Transaction on Manuf.Orders
       refbn    LIKE covp-refbn,   "Reference Document Number
       aworg    LIKE covp-aworg,   "Reference organisational units

      END OF  it_covp.

*Summarized lineitem
DATA : BEGIN OF it_coep OCCURS 0,
       objnr  TYPE j_objnr   ,
       typps      TYPE typps,
       kstar  TYPE kstar     ,
       werks  TYPE werks_d   ,
       matnr  TYPE matnr     ,
       kostl      TYPE kostl     ,
       lstar      TYPE lstar     ,

       parob      TYPE parob     ,
       hrkft  TYPE co_subkey ,  "CO key subnumber
       vrgng  TYPE co_vorgang,  "CO business transaction
       beknz  TYPE beknz     ,  "Dr/Cr
*      meinh  TYPE co_meinh  ,  "Unit of measure
       meinb  LIKE covp-meinb,  "Unit of measure
       wkgbtr	TYPE wkgxxx    ,
       mbgbtr	TYPE mbgxxx    ,
       scrap_amt  TYPE wkgxxx    ,  "OS&D
       scrap_qty  TYPE mbgxxx    ,

*//Modify..03/28/2011..T00020..
       keyin_amt  TYPE wkgxxx    ,  "OS&D
       keyin_qty  TYPE mbgxxx    ,
       osnd_amt   TYPE wkgxxx    ,  "OS&D
       osnd_qty   TYPE mbgxxx    ,
*//
       END OF  it_coep.

DATA : BEGIN OF it_coep_self OCCURS 0,
       objnr  TYPE j_objnr   ,
       werks  TYPE werks_d   ,
       matnr  TYPE matnr     ,

       meinb  LIKE covp-meinb,  "Unit of measure
       wkgbtr	TYPE wkgxxx    ,
       mbgbtr	TYPE mbgxxx    ,
       END OF  it_coep_self.


*COEP-MH
DATA : BEGIN OF it_mts_mh OCCURS 0,
       objnr  TYPE j_objnr   ,
       kstar  TYPE kstar     ,
       parob  TYPE parob     ,  "Partner Obj
       refbn      LIKE covp-refbn,  "Reference Document Number
       aworg      LIKE covp-aworg,  "Reference organisational units
       meinb  LIKE covp-meinb,  "Unit of measure
       mbgbtr	TYPE mbgxxx    ,  "Qty
       wkgbtr	TYPE wkgxxx    ,  "Amt
      END OF it_mts_mh.



* For Scrap Wip
DATA : BEGIN OF it_wip_scrap OCCURS 0.
* type QRP_T_WIP_SCRAP
DATA : gjahr         LIKE qrp_s_wip_scrap-gjahr,
       period        LIKE qrp_s_wip_scrap-period,
       objnr         LIKE qrp_s_wip_scrap-objnr,
       kostl         LIKE csks-kostl,
       lstar         LIKE csla-lstar,
       material      LIKE qrp_s_wip_scrap-material,
       bwkey         LIKE qrp_s_wip_scrap-bwkey,
       bwtar         LIKE qrp_s_wip_scrap-bwtar,
       wip_quantity  LIKE qrp_s_wip_scrap-wip_quantity,
       actual_scrap  LIKE qrp_s_wip_scrap-actual_scrap,
       target_qty    LIKE qrp_s_wip_scrap-target_qty,
       variance_qty  LIKE cpzp-istmn,
       unit          LIKE qrp_s_wip_scrap-unit .
DATA : END OF it_wip_scrap.

DATA: BEGIN OF it_wip_calculated OCCURS 0,
       gjahr         LIKE qrp_s_wip_scrap-gjahr,
       period        LIKE qrp_s_wip_scrap-period,
       objnr         LIKE qrp_s_wip_scrap-objnr,
       kostl         LIKE csks-kostl,
       lstar         LIKE csla-lstar,
       material      LIKE qrp_s_wip_scrap-material,
       bwkey         LIKE qrp_s_wip_scrap-bwkey,
       bwtar         LIKE qrp_s_wip_scrap-bwtar,
      END   OF it_wip_calculated.

* For Additional Issue
DATA : BEGIN OF it_ztco_abispost OCCURS 0.
        INCLUDE STRUCTURE ztco_abispost.
DATA : END OF  it_ztco_abispost.

* For M/H
DATA : BEGIN OF it_mhsum OCCURS 0,
*           gjahr   type gjahr,
*           perid   type perid,
           objnr   LIKE coep-objnr,
*           matnr   LIKE mara-matnr,
*           werks   LIKE t001w-werks,
*           aufnr   LIKE aufk-aufnr,
           kostl   LIKE csks-kostl,
           lstar   LIKE csla-lstar,
*          elemt   LIKE tckh3-elemt,
           meinh   LIKE t006-msehi,
           varmn   LIKE cpzp-varmn,
           varamt  TYPE wkgxxx    ,  "Amt
        END OF  it_mhsum.

* Unit Price from KSBT
DATA : BEGIN OF it_ccr1t OCCURS 0.
*DATA :   gjahr   LIKE cost-gjahr,
*         periode LIKE ccr1t-periode,
*         lednr   LIKE cost-lednr,
*         versn   LIKE cost-versn,
*         tarkz   LIKE cost-tarkz.
DATA :   objnr   LIKE cost-objnr,
         kostl   LIKE csks-kostl,
         lstar   LIKE csla-lstar,
         tkexxx  LIKE ccr1t-tkexxx,
         tkgxxx  LIKE ccr1t-tkgxxx,
         tkfxxx  LIKE ccr1t-tkfxxx,
         toexxx  LIKE ccr1t-toexxx,
         togxxx  LIKE ccr1t-togxxx,
         tofxxx  LIKE ccr1t-tofxxx.
*        INCLUDE STRUCTURE   CCR1T.
DATA : END OF   it_ccr1t.

* For Default Unit For AT
DATA : BEGIN OF it_csla OCCURS 0,
*         POPER       LIKE CCSS-BUPER,
          lstar       LIKE csla-lstar,
          datbi       LIKE csla-datbi,
          datab       LIKE csla-datab,
          vksta       LIKE csla-vksta,
          leinh       LIKE csla-leinh,
          leinh_out   LIKE csla-leinh,
          denominator(16) TYPE p DECIMALS 6,
          numerator(16)   TYPE p DECIMALS 6.
DATA : END OF   it_csla.
* For Fractions (PCC Variance data)
DATA :  BEGIN OF it_frpcc OCCURS  0.
DATA :    typps     LIKE ckis-typps.
DATA :    perio     LIKE coep-perio,
          objnr     LIKE coep-objnr,
          gjahr     LIKE coep-gjahr,
*         WRTTP     Like Coep-WRTTP,
          versn     LIKE coep-versn,
          kstar     LIKE coep-kstar,
          twaer     LIKE coep-twaer.
DATA :    wkgbtr    LIKE coep-wkgbtr, "PCC Total Actual
          wip_amt   LIKE cosb-wkg001,
          scrap_amt LIKE cosb-wkg001.
DATA :    add_wkgbtr LIKE ztco_shopcost_a2-add_wkgbtr.
DATA :  END OF it_frpcc.

* For Fractions (ML Manufacture Amt Variance data)
DATA :  BEGIN OF it_frml OCCURS  0.
* DATA :    TYPPS     LIKE CKIS-TYPPS.
DATA :    bdatj     LIKE coep-gjahr,
          poper     LIKE coep-perio.
DATA :    objnr     LIKE coep-objnr,
          versn     LIKE coep-versn,
          elemt     LIKE ztco_shopcost_a2-elemt,
*         KSTAR     LIKE COEP-KSTAR,
          hwaer     LIKE coep-twaer.
DATA :    manu_amt  LIKE ztco_shopcost_a2-manu_amt.
DATA :  END OF it_frml.

** Global Vriables
DATA : gv_record_type LIKE ztco_shopcost_a2-record_type VALUE 'A'.
DATA : gv_verwe  LIKE plko-verwe.
DATA : gv_tarkz  LIKE cokl-tarkz.
DATA : gv_freig  LIKE keko-freig.
DATA:  w_perbi   LIKE covja-perbi.

** Constant
CONSTANTS : c_gp_wrttp(2)       VALUE '04',
            c_gp_beknz(1)       VALUE 'S'.

RANGES: r_beknz FOR coep-beknz,
        r_kstar FOR coep-kstar.

DATA: gv_shop_sum_idx LIKE sy-tabix.

*data : gs_quantities type qrp_quantities occurs 0 with header line.
DATA : gs_quantities TYPE zqrp_quantities OCCURS 0 WITH HEADER LINE.

DATA : g_frdat LIKE sy-datum,
       g_todat LIKE sy-datum.


DATA  :  p_versn LIKE rkpln-versn,
         p_tvers LIKE keko-tvers.

DATA: BEGIN OF it_wip_sap   OCCURS 0,
        objnr   LIKE aufk-objnr,
        kstar   LIKE kv012-kstar,
        parob   LIKE kv012-parob,
        ckmatnr LIKE kv012-ckmatnr,
        ckwerks LIKE kv012-ckwerks,
        meinh   LIKE kv012-meinh,
        megbtr  LIKE kv012-megbtr,
        wkgbtr  LIKE kv012-wkgbtr,

        pegbtr  LIKE kv012-megbtr,
        pkgbtr  LIKE kv012-wkgbtr,

*//Modify..04/27/2011..T00020..
        mefbtr  LIKE kv012-mefbtr,
        wkfbtr  LIKE kv012-wkfbtr,

        pefbtr  LIKE kv012-mefbtr,
        pkfbtr  LIKE kv012-wkfbtr,
*//
      END OF it_wip_sap.

DATA: BEGIN OF it_wip_pre   OCCURS 0,
        objnr   LIKE aufk-objnr,
        kstar   LIKE kv012-kstar,
        matnr   LIKE kv012-ckmatnr,
        kostl   TYPE kostl,
        lstar   TYPE lstar,
        pegbtr  LIKE kv012-megbtr,
        pkgbtr  LIKE kv012-wkgbtr,
      END OF it_wip_pre.

DATA: BEGIN OF ls_wip_qty OCCURS 0.
        INCLUDE STRUCTURE zqrp_quantities.
*       objnr(22)       TYPE c,
*       kaln1           LIKE ckmlhd-kalnr,
*       sobkz           LIKE ckmlhd-sobkz,
*       kzbws           LIKE ckmlhd-kzbws,
*       act_objnr       LIKE qrp_quantities-act_objnr,
*       kstar           TYPE kstar     , "Cost Element
*       wip_quantity    LIKE cpzp-istmn, "based on components
*       actual_scrap    LIKE cpzp-istmn, "based on components
*       planned_scrap   LIKE cpzp-istmn, "based on components
*       variance_qty    LIKE cpzp-istmn, "based on components
*       actual_qty_stpc LIKE cpzp-istmn, "Act.Qty after RP
*       target_qty      LIKE cpzp-istmn, "based on components
*       prev_wip        LIKE cpzp-istmn, "based on components
*       curr_wip        LIKE cpzp-istmn, "based on components
*       apo_var_qty     LIKE cpzp-istmn, "based on components
*       apo_input_qty   LIKE cpzp-istmn, "based on components
*       apo_output_qty  LIKE cpzp-istmn, "based on components
*       unit            LIKE cpzp-meinh,

*DATA:  apo_meins       LIKE cpzp-meinh, "UOM
DATA:  bwkey           LIKE ckmlhd-bwkey,
       bwtar           LIKE ckmlhd-bwtar,
       vbeln           LIKE ckmlhd-vbeln,
       posnr           LIKE ckmlhd-posnr,
       kostl           LIKE csksz-kostl,
       lstar           LIKE cslaz-lstar,
       material        LIKE mara-matnr,
       END OF ls_wip_qty.

DATA: BEGIN OF lt_wipvalue OCCURS 0,
      objnr            LIKE aufk-objnr,
      kstar            TYPE kstar     , "Cost Element
      typps            TYPE typps,
      resou(25)        TYPE c,

      act_objnr        LIKE qrp_quantities-act_objnr,
      wip_quantity     LIKE cpzp-istmn, "based on components
*      curr_wip         LIKE cpzp-istmn, "based on components
      prev_wip         LIKE cpzp-istmn, "based on components

      actual_scrap     LIKE cpzp-istmn, "based on components
      planned_scrap    LIKE cpzp-istmn, "based on components
      variance_qty     LIKE cpzp-istmn, "based on components
      actual_qty_stpc  LIKE cpzp-istmn, "Act.Qty after RP
      target_qty       LIKE cpzp-istmn, "based on components
      apo_var_qty      LIKE cpzp-istmn, "based on components
      apo_input_qty    LIKE cpzp-istmn, "based on components
      apo_output_qty   LIKE cpzp-istmn, "based on components

      unit             LIKE cpzp-meinh, "unit
*      apo_meins        LIKE cpzp-meinh, "UOM
      material         LIKE mara-matnr,
      bwkey            LIKE ckmlhd-bwkey,
      kostl            LIKE csksz-kostl,
      lstar            LIKE cslaz-lstar,"Activity Type

      unitp            TYPE base_price_scale, "Unit Price$
      peinh            LIKE mbew-peinh, "price unit 1/10

      wip_amt          LIKE kkbcs_out-sollkost_g,  "WIP ending
      wip_pamt         LIKE kkbcs_out-sollkost_g,  "WIP opening
      var_amt          LIKE kkbcs_out-sollkost_g,  "Variance
      scrap_amt        LIKE kkbcs_out-sollkost_g,
*      gr_amt           LIKE kkbcs_out-sollkost_g,
*      gr_qty           LIKE cpzp-istmn,
*      WKGBTR           LIKE coep-WKGBTR,
*      MBGBTR           LIKE coep-MBGBTR,

*     settle           LIKE kkbcs_out-sollkost_g,  "Settlement
*     comp_lstar      LIKE cslaz-lstar,"Activity Type???

*//Modify..04/27/2011..T00020..
      wip_quantity_f   LIKE cpzp-istmn, "based on components
      prev_wip_f       LIKE cpzp-istmn, "based on components
      wip_f            LIKE kkbcs_out-sollkost_g,  "WIP ending
      wip_pf           LIKE kkbcs_out-sollkost_g,  "WIP opening
*//

      END OF lt_wipvalue.

DATA: BEGIN OF lt_ck13n OCCURS 0,
      werks            LIKE coep-werks,
      matnr            LIKE coep-matnr,
      lotsize          TYPE ck_losgr,   "Lot Size

      kstar            TYPE kstar     , "Cost Element
      resou(25)        TYPE c,
      material         LIKE mara-matnr,
      plant            LIKE ckmlhd-bwkey,
      kostl            LIKE csksz-kostl,
      lstar            LIKE cslaz-lstar,"Activity Type

      meeht            LIKE coep-meinb,
      unitp            TYPE base_price_scale, "Unit Price$
      peinh            LIKE mbew-peinh, "price unit 1/10

      gr_amt           LIKE kkbcs_out-sollkost_g,
      gr_qty           LIKE cpzp-istmn,
      END OF lt_ck13n.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.
*---- ALV

DATA: BEGIN OF t_mlite OCCURS 0,
        paobjnr   LIKE mlco-paobjnr,
*        lights type n,
*        coltab type slis_t_specialcol_alv,
        vnprd_ea LIKE ckmlcr-vnprd_ea,
        vnkdm_ea LIKE ckmlcr-vnkdm_ea,
        vnprd_ma LIKE ckmlcr-vnprd_ma,
        vnkdm_ma LIKE ckmlcr-vnkdm_ma,
        ebprd_ea LIKE ckmlcr-ebprd_ea,
        ebkdm_ea LIKE ckmlcr-ebkdm_ea,
        ebprd_ma LIKE ckmlcr-ebprd_ma,
        ebkdm_ma LIKE ckmlcr-ebkdm_ma,
        vn_dif LIKE cki_doc_ml-vn_dif,
        eb_dif LIKE cki_doc_ml-eb_dif,
        multilevel_dif LIKE cki_doc_ml-multilevel_dif,
        singlelevel_dif LIKE cki_doc_ml-singlelevel_dif,
        sum_prdif LIKE cki_doc_ml-sum_prdif,
        sum_krdif LIKE cki_doc_ml-sum_krdif,
        sum_dif LIKE cki_doc_ml-sum_dif,
        price_new LIKE cki_doc_ml-price_new,
        price_old LIKE cki_doc_ml-price_old,
        stapr_new LIKE cki_mr21_0250-newstapr,
        stapr_old LIKE cki_mr21_0250-stapr,
        vprsv_new LIKE mlcrp-vprsv_new,
        peinh_new LIKE mlcrp-peinh_new,
        vprsv_old LIKE mlcr-vprsv_old,
        peinh_old LIKE mlcr-peinh,
        curtp_text LIKE cki_ml_cty-text.
*{   INSERT         PA8K031745                                        5
*       include data from MLCR, MLITMB and MLCO
DATA:   exbwr     LIKE mlcr-exbwr,
        bnbtr     LIKE mlcr-bnbtr,
        vkwrt     LIKE mlcr-vkwrt,
        vkwra     LIKE mlcr-vkwra,
        exvkw     LIKE mlcr-exvkw,
        vksal_old LIKE mlcr-vksal_old.
*       INCLUDE STRUCTURE MLCO_DATA.
*       MLCO_DATA contains also LIFNR!
DATA:   gjahr     LIKE mlco-gjahr,
        kokrs     LIKE mlco-kokrs,
        sakto     LIKE mlco-sakto,
        geber     LIKE mlco-geber,
        fistl     LIKE mlco-fistl,
        fipos     LIKE mlco-fipos,
        fkber     LIKE mlco-fkber,
*       paobjnr   like mlco-paobjnr,
        prctr     LIKE mlco-prctr,
        pprctr    LIKE mlco-pprctr,
        pargb     LIKE mlco-pargb,
        parbu     LIKE mlco-parbu,
        xskst     LIKE mlco-xskst,
        kostl     LIKE mlco-kostl,
        xsauf     LIKE mlco-xsauf,
        aufnr     LIKE mlco-aufnr,
        xspro     LIKE mlco-xspro,
        ps_psp_pnr LIKE mlco-ps_psp_pnr,
        nplnr     LIKE mlco-nplnr,
        xserg     LIKE mlco-xserg,
        lifnr_mm  LIKE mlco-lifnr,
        ebeln     LIKE mlco-ebeln,
        kunnr     LIKE mlco-kunnr,
        kdauf     LIKE mlco-kdauf,
        kstrg     LIKE mlco-kstrg,
        anln1     LIKE mlco-anln1,
        anln2     LIKE mlco-anln2,
        prznr     LIKE mlco-prznr,
        lstar     LIKE mlco-lstar,
        menge     LIKE mlppf-menge,
        act_objnr LIKE qrp_quantities-act_objnr.
**      include structure CI_COBL.
        INCLUDE STRUCTURE mlitmb_data.
*}   INSERT
        INCLUDE STRUCTURE mlite.
DATA: END OF t_mlite.

DATA :  l_objnr TYPE j_objnr,
        l_space TYPE c.

DATA:   return     TYPE bapiret2 OCCURS 0.

*----------------------------------------------------------------------*
*   MACRO                                                              *
*----------------------------------------------------------------------*
DEFINE scr_sum_data.
  clear : it_cos&1a, it_cos&1a[].
  loop at it_lt_cos&1a.
    clear lv_kkb_beweg.

    call function 'Z_K_KKB_BEWEG_SET'
      exporting
        i_cos&1a = it_lt_cos&1a
      importing
        e_beweg  = lv_kkb_beweg
      exceptions
        no_input = 1
        others   = 2.

    if sy-subrc <> 0.
      if p_debug = 'X'.
        break-point.
      endif.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
*C-Cost,F-Activity,L-Misc,P-GR,X-Settle
      if lv_kkb_beweg ca 'CFLP'.
        move-corresponding it_lt_cos&1a to it_cos&1a.
        it_cos&1a-beweg = lv_kkb_beweg.
        collect  it_cos&1a.
        clear    it_cos&1a.
      endif.
    endif.
    clear it_lt_cos&1a.
  endloop.
END-OF-DEFINITION.

DEFINE cal_koat.
* Read Unit Price By CostComp
  data : lv_datum like sy-datum.
  clear it_koat_p_&1.
  sort it_ccr1t by gjahr periode objnr.

  sort it_csla by
            lstar
            leinh
            datab.
  clear it_csla.

  loop at it_ccr1t.
    loop at it_koat_p_&1
                  where gjahr = it_ccr1t-gjahr
                    and poper = it_ccr1t-periode
                    and kostl = it_ccr1t-kostl
                    and lstar = it_ccr1t-lstar.
* Get the Last Day of Period
      clear lv_datum.
      call function 'LAST_DAY_IN_PERIOD_GET'
        exporting
          i_gjahr              = it_koat_p_&1-gjahr
*         I_MONMIT             = 00
          i_periv              = tka01-lmona
          i_poper              = it_koat_p_&1-poper
        importing
          e_date               = lv_datum
        exceptions
          input_false          = 1
          t009_notfound        = 2
          t009b_notfound       = 3
          others               = 4.

      if sy-subrc <> 0.
        if p_debug = 'X'.
          break-point.
        endif.
        message id sy-msgid type sy-msgty number sy-msgno
                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      clear it_csla.
      loop at it_csla where  lstar = it_koat_p_&1-lstar
                        and  datab =< lv_datum.
        if sy-subrc = 0.
          exit.
        endif.
      endloop.

* Cal.
      it_koat_p_&1-tkgxxx
        = (
          it_ccr1t-tkgxxx * ( it_csla-denominator / it_csla-numerator )
          )
        * it_koat_p_&1-cp_%.
* Modify
      modify it_koat_p_&1.
      clear  it_koat_p_&1.
    endloop.
    clear  it_ccr1t.
  endloop.
END-OF-DEFINITION.




*&----------------------------------------------------------------------
RANGES:
  bapi_cstgva   FOR bapicstgva-low,                         "'PPC1'
  bapi_matnr    FOR bapimateri-low,
  bapi_plant    FOR bapiplant-low,
  bapi_valid    FOR bapivalidi-low,
  bapi_cstgstat FOR bapicstgst-low,  "'FR'
  bapi_cstgvrsn FOR bapicostingversion-low.

TYPES: gt_amt TYPE p DECIMALS 6.

DATA:  gv_sap_err TYPE char1.

CONSTANTS:
      y_selection(6) TYPE c VALUE 'SELECT'.

*&----------------------------------------------------------------------
* Tracking Point ( First Digit is Shop code )
TYPES:
  BEGIN OF ty_tp,
    prvbe TYPE marc-vspvb,
    tpoint TYPE ztpp0001-key2,
  END OF ty_tp.
DATA it_tp TYPE HASHED TABLE OF ty_tp WITH HEADER LINE
           WITH UNIQUE KEY prvbe.
