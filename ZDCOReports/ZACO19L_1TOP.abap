*----------------------------------------------------------------------*
*   INCLUDE ZACO19L_1TOP                                               *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
include zlzgco_global_formto1.

* For Global Value in CO - SHOP
include zlzgco_global_formto2.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
* TYPE-POOL
type-pools:
  ccs00,
  ccs01,
  ckmv0,
  ckmv3,
  vrm,
  ckmd,
  slis,
  ckru0.

** Tables
tables : keko, keph, t001w, tckh4, tj01, kkbcs_out, tck03.
tables : crhd, plpo, plko, mkal, ckhs.
tables : cosl, cosp, coss, ccss, cost, cosb, hcstruktur, kkb_head.
tables : mlcd, ckmlhd, ckmlmv001, ckmlcr, kkb_ml_pos.
tables : cobk, coep, ccr1t, csla.
tables : cospa_key, cosla_key, cossa_key, cosba_key.
tables : ztco_shopcost_at.
tables : zvco_rp1, tka01,
         zsco_shopcost_001,
         zsco_shopcost_key,
         qrp_s_wip_scrap,
         qrp_quantities.
tables : ztco_abispost, ztco_mhpcpost.

** Internal Tables
* Main ITAB
data : it_ztco_shopcost_at
                         like standard table of ztco_shopcost_at
                         with header line .
* Temp. Table for Main ITAB
data : begin of it_tmp_shopcost occurs 5000.
        include structure it_ztco_shopcost_at.
data : end of  it_tmp_shopcost.
* Temp. Container for IC
data : begin of it_sc_m occurs 0.
        include structure it_tmp_shopcost.
data : end of it_sc_m.
data : begin of it_sc_v occurs 0.
        include structure it_tmp_shopcost.
data : end of it_sc_v.
data : begin of it_sc_e occurs 0.
        include structure it_tmp_shopcost.
data : end of it_sc_e.
* KEKO
data : begin of it_ckikekokey occurs 0.
        include structure ckikekokey.
data : bdatj type bdatj,
       poper type poper.
data : kadat like keko-kadat,
       bidat like keko-bidat,
       matnr like keko-matnr,
       werks like keko-werks,
       bwkey like keko-bwkey,
       bwtar like keko-bwtar.
data : end of it_ckikekokey.
*  For CK13n Cost Components
data : begin of it_kkb_split_ck13n occurs 100.
data : bdatj type bdatj,
       poper type poper.
data : matnr like keko-matnr,
       werks like keko-werks,
       bwkey like keko-bwkey,
       bwtar like keko-bwtar,
       elemt like kkb_split-elemt,
       w000  like kkb_split-w000.
data : end of it_kkb_split_ck13n.

*  For MLCCS
data : it_keph_mlcd  type ccs01_t_keph_mlcd
                     with header line .
data : it_kkb_split  like standard table of kkb_split
                     with header line .
*  For Valuation Info. of Materials
data : begin of it_mat occurs 100,
        matnr like macku-matnr,
        werks like marc-werks,
        bwkey like macku-bwkey,
        bwtar like macku-bwtar,
        mtart like macku-mtart,
        beskz like marc-beskz,   "Procurement Type
        sobsl like marc-sobsl,   "Special procurement type
        vspvb like marc-vspvb,   "Proposed Supply Area
        kalnr like ckmlhd-kalnr,
*        "Cost est number for cost est w/o qty structure
       end of it_mat.
data : begin of it_fsc_mat occurs 500.
        include structure it_mat.
data :  aufnr  like aufk-aufnr,
        objnr  like coep-objnr,
        verid  like mkal-verid,
        par_proc_kalnr like ckmlmv001-proc_kalnr.
data : end of it_fsc_mat.
* ZVCO_RP1 (Report Point Linkage)
data : it_zvco_rp1       like standard table of zvco_rp1
                         with header line .
* Plant Info.
data : it_t001w like standard table of t001w
                with header line .
* For BAPI
data : it_costcenterlist like standard table of bapi0012_cclist
                         with header line.
data : it_return         like standard table of bapiret2
                         with header line.
data : begin  of it_cctr  occurs 0,
        shop  like ztco_shopcost_at-shop,
        kostl like csks-kostl.
data : end    of it_cctr.
* Temp. Table for Main ITAB - Press / Engine
data : begin of it_tmp_pe occurs 0,
        llv_matnr like mara-matnr ,
        werks     like t001w-werks,
        bwkey     like mbew-bwkey ,
        bwtar     like mbew-bwtar .
data : end of   it_tmp_pe .
* KSBT Costing Rate BY KOSTL LSTAR
data : begin of it_koat_p occurs 0.
data :  gjahr    like ccss-gjahr,
        poper    like ccss-buper,
        kostl    like csks-kostl,
        lstar    like csla-lstar,
        elemt    like kkb_split-elemt,
        w000     like kkb_split-w000 ,
        waers    like kkb_split-waers,
        total    like kkb_split-w000 ,
        cp_%(16) type p decimals 7,
        tkgxxx   like ccr1t-tkgxxx.
*        LEINH    like CSLA-LEINH.
data : end of it_koat_p.
data : it_koat_p_5 like standard table of it_koat_p
                   with header line .
data : it_koat_p_1 like standard table of it_koat_p
                   with header line .
data : begin of it_kostl_lstar_pct occurs 0.
data :  from_per like cobk-perab,
        to_per   like cobk-perab,
        objnr    like coomco-objnr,
        kadky    like sy-datum ,
        bidat    like sy-datum .
        include structure it_koat_p.
data : end of it_kostl_lstar_pct.
data : begin of it_coomco occurs 0.
        include structure coomco.
data :  poper    like ccss-buper,
        from_per like cobk-perab,
        to_per   like cobk-perab,
        kostl    like csks-kostl,
        lstar    like csla-lstar.
data : end of it_coomco.
* CO Summary Tables
def_co_sum_tab l.
def_co_sum_tab p.
def_co_sum_tab s.
def_co_sum_tab b.
* For COEP
data : begin of it_coep occurs 10000.
data :  beweg like kkbcs_out-beweg.
        include structure zsco_shopcost_003.
data :  sgtxt like coep-sgtxt.
data : end of  it_coep.
* For Scrap Wip
data : begin of it_wip_scrap occurs 1000.
* type QRP_T_WIP_SCRAP
data : gjahr         like qrp_s_wip_scrap-gjahr,
       period        like qrp_s_wip_scrap-period,
       objnr         like qrp_s_wip_scrap-objnr,
       kostl         like csks-kostl,
       lstar         like csla-lstar,
       material      like qrp_s_wip_scrap-material,
       bwkey         like qrp_s_wip_scrap-bwkey,
       bwtar         like qrp_s_wip_scrap-bwtar,
       wip_quantity  like qrp_s_wip_scrap-wip_quantity,
       actual_scrap  like qrp_s_wip_scrap-actual_scrap,
       target_qty    like qrp_s_wip_scrap-target_qty,
       unit          like qrp_s_wip_scrap-unit .
data : end of it_wip_scrap.
data: begin of it_wip_calculated occurs 0,
       gjahr         like qrp_s_wip_scrap-gjahr,
       period        like qrp_s_wip_scrap-period,
       objnr         like qrp_s_wip_scrap-objnr,
       kostl         like csks-kostl,
       lstar         like csla-lstar,
       material      like qrp_s_wip_scrap-material,
       bwkey         like qrp_s_wip_scrap-bwkey,
       bwtar         like qrp_s_wip_scrap-bwtar,
      end   of it_wip_calculated.

* For Additional Issue
data : begin of it_ztco_abispost occurs 0.
        include structure ztco_abispost.
data : end of  it_ztco_abispost.
* For M/H
*DATA : BEGIN OF IT_ZTCO_MHPCPOST OCCURS 0.
*        INCLUDE STRUCTURE ZTCO_MHPCPOST.
*DATA : END OF  IT_ZTCO_MHPCPOST.
data : begin of it_mhsum occurs 100,
           gjahr   type gjahr,
           perid   type perid,
           matnr   like mara-matnr,
           werks   like t001w-werks,
           aufnr   like aufk-aufnr,
           kostl   like csks-kostl,
           lstar   like csla-lstar,
           elemt   like tckh3-elemt,
           meinh   like t006-msehi,
           varquan like ztco_mhpcpost-varquan,
        end of  it_mhsum.
* Unit Price from KSBT
data : begin of it_ccr1t occurs 0.
data :   gjahr   like cost-gjahr,
         periode like ccr1t-periode,
         lednr   like cost-lednr,
         versn   like cost-versn,
         tarkz   like cost-tarkz.
data :   objnr   like cost-objnr,
         kostl   like csks-kostl,
         lstar   like csla-lstar.
data :   tkexxx  like ccr1t-tkexxx,
         tkgxxx  like ccr1t-tkgxxx.
*        INCLUDE STRUCTURE   CCR1T.
data : end of   it_ccr1t.
* For Default Unit For AT
data : begin of it_csla occurs 0,
*         POPER       LIKE CCSS-BUPER,
          lstar       like csla-lstar,
          datbi       like csla-datbi,
          datab       like csla-datab,
          leinh       like csla-leinh,
          leinh_out   like csla-leinh,
          denominator(16) type p decimals 5,
          numerator(16)   type p decimals 5.
data : end of   it_csla.
* For Fractions (PCC Variance data)
data :  begin of it_frpcc occurs 100.
data :    typps     like ckis-typps.
data :    perio     like coep-perio,
          objnr     like coep-objnr,
          gjahr     like coep-gjahr,
*         WRTTP     Like Coep-WRTTP,
          versn     like coep-versn,
          kstar     like coep-kstar,
          twaer     like coep-twaer.
data :    wkgbtr    like coep-wkgbtr, "PCC Total Actual
          wip_amt   like cosb-wkg001,
          scrap_amt like cosb-wkg001.
data :    add_wkgbtr like ztco_shopcost_at-add_wkgbtr.
data :  end of it_frpcc.
* For Fractions (ML Manufacture Amt Variance data)
data :  begin of it_frml occurs 100.
* DATA :    TYPPS     LIKE CKIS-TYPPS.
data :    bdatj     like coep-gjahr,
          poper     like coep-perio.
data :    objnr     like coep-objnr,
          versn     like coep-versn,
          elemt     like ztco_shopcost_at-elemt,
*         KSTAR     LIKE COEP-KSTAR,
          hwaer     like coep-twaer.
data :    manu_amt  like ztco_shopcost_at-manu_amt.
data :  end of it_frml.

** Global Vriables
data : gv_record_type like ztco_shopcost_at-record_type.
data : gv_verwe like plko-verwe.
data : gv_tarkz like cokl-tarkz.
data : gv_freig like keko-freig.
data:  w_perbi like covja-perbi.
** Constant
constants : c_gp_wrttp(2)       value '04',
            c_gp_beknz(1)       value 'S'.


*----------------------------------------------------------------------*
*   MACRO                                                              *
*----------------------------------------------------------------------*
define scr_sum_data.
  clear : it_cos&1a, it_cos&1a[].
  loop at it_lt_cos&1a.
    clear lv_kkb_beweg.

    call function 'K_KKB_BEWEG_SET'
         exporting
              i_cos&1a = it_lt_cos&1a
         importing
              e_beweg  = lv_kkb_beweg
         exceptions
              no_input = 1
              others   = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    else.
      if lv_kkb_beweg ca 'CFL'.
        move-corresponding it_lt_cos&1a to it_cos&1a.
        it_cos&1a-beweg = lv_kkb_beweg.
        collect  it_cos&1a.
        clear    it_cos&1a.
      endif.
    endif.
    clear it_lt_cos&1a.
  endloop.
end-of-definition.

define cal_koat.
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
end-of-definition.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
* General Info.
parameters : p_kokrs like csks-kokrs   memory id cac  obligatory.
*PARAMETERS : P_KLVAR LIKE CKI64A-KLVAR MEMORY ID KRT  MODIF ID DIV,
parameters :  p_versn like rkpln-versn  default '000'  modif id div,
              p_tvers like keko-tvers   default '04'   no-display.
parameters :  p_elehk like tckh4-elehk  default 'H1'   modif id div.
selection-screen skip 1.

* Costing Type
selection-screen begin of block bl2 with frame title text-002.
selection-screen begin of line.
selection-screen comment  1(13)  text-031. "Actual
selection-screen position 15.
parameters : p_act as checkbox default 'X' modif id div.
*PARAMETERS : P_BPL RADIOBUTTON GROUP RA01
*             USER-COMMAND  CTY.
*SELECTION-SCREEN COMMENT  25(13) TEXT-032. "Standard Plan
*SELECTION-SCREEN POSITION 39.
*PARAMETERS : P_STD RADIOBUTTON GROUP RA01.
*SELECTION-SCREEN COMMENT  49(13) TEXT-033. "Actual
*SELECTION-SCREEN POSITION 63.
*PARAMETERS : P_ACT RADIOBUTTON GROUP RA01.
selection-screen end of line.
selection-screen end of block bl2.
selection-screen end of block bl1.

selection-screen begin of block bl3 with frame title text-003.
* Posted Yr.
parameters : p_bdatj like keko-bdatj memory id bdtj obligatory.
* periods
selection-screen begin of line.
selection-screen comment  1(30) text-021. "From
selection-screen position 33.
parameters: p_perab like covja-perab memory id vpe
            modif id per obligatory.
*SELECTION-SCREEN COMMENT  52(3) TEXT-022. "To
*SELECTION-SCREEN POSITION 58.
*PARAMETERS: P_PERBI LIKE COVJA-PERBI MEMORY ID BPE
*            MODIF ID PER OBLIGATORY.
selection-screen end of line.
selection-screen end of block bl3.

* Option
selection-screen begin of block bl4 with frame title text-004.
select-options : s_matnr for keko-matnr memory id mat.
*PARAMETERS : P_DEL AS CHECKBOX.
selection-screen end of block bl4.
