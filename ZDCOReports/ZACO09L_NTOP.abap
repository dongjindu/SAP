*----------------------------------------------------------------------*
*   INCLUDE ZACO09L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For Global Value in CO
include zlzgco_global_formto1.

* For Global Value in CO For SHOP
include zlzgco_global_formto2.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
tables : keko, keph,   ckhs,  ckis,  tckh3, tckh2, cskb ,tckh4, coomco,
         khsk,mara.
tables : cki64a, covja, rkpln.
tables : tck03,  tck05.
tables : t001w,  macku, marc.
tables : zvco_rp1, tka01, ztco_shopcost, ztco_shop_pln,ztco_shop_pln_cc,
         zsco_shopcost_001, zsco_shopcost_key.
tables : crhd, plpo, plko, mkal.

** Internal Tables
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
       end of it_mat.
data : begin of it_fsc_mat occurs 500.
        include structure it_mat.
data : end of it_fsc_mat.
* Main ITAB
*data : it_ztco_shopcost  like standard table of ztco_shopcost
*                         with header line .

data : begin of it_ztco_shopcost occurs 5000.
        include structure ztco_shopcost.
data :  lot_size like  ztco_shop_pln-lot_size,
        kalst    like  ztco_shop_pln-kalst.
data : end of  it_ztco_shopcost.

* Temp. Table for Main ITAB
data : begin of it_tmp_shopcost occurs 5000.
        include structure it_ztco_shopcost.
data : end of  it_tmp_shopcost.
* Temp. Table for Main ITAB - add on
data : begin of it_add_tmp_scost occurs 100.
        include structure it_ztco_shopcost.
data : end of   it_add_tmp_scost .
* Temp. Table for Main ITAB - Press/Engine
data : begin of it_tmp_pren occurs 100.
        include structure it_ztco_shopcost.
data : end of   it_tmp_pren .
* Temp. Table for Main ITAB - Press / Engine


data : it_shop_pln    like ztco_shop_pln    occurs 0 with header line.
data : it_shop_pln_cc like ztco_shop_pln_cc occurs 0 with header line.

data : begin of it_tmp_pe occurs 0,
        llv_matnr like mara-matnr ,
        werks     like t001w-werks,
        bwkey     like mbew-bwkey ,
        bwtar     like mbew-bwtar .
data : end of   it_tmp_pe .
* KEKO
data : begin of it_ckikekokey occurs 0.
        include structure ckikekokey.
data : kadat like keko-kadat,
       bidat like keko-bidat,
       matnr like keko-matnr,
       werks like keko-werks,
       bwkey like keko-bwkey,
       bwtar like keko-bwtar,
       kalst like keko-kalst,
       LOSGR LIKE KEKO-LOSGR.
data : end of it_ckikekokey.
* Itemization
data : begin of it_kis1 occurs 0.
        include structure kis1.
data : LOSGR LIKE KEKO-LOSGR.
data : end of it_kis1.

data : it_khs1  like standard table of khs1
                with header line .
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
        shop  like it_ztco_shopcost-shop,
        kostl like csks-kostl.
data : end    of it_cctr.
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
        cp_%(16) type p decimals 7.
data : end of it_koat_p.
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
data : record_num type i.
** Global Vriables
data : gv_record_type like ztco_shopcost-record_type.
data : gv_verwe like plko-verwe.
data : gv_tarkz like cokl-tarkz.
data : gv_freig like keko-freig.


*----------------------------------------------------------------------*
*   DEFINITION                                                *
*----------------------------------------------------------------------*
define screen_period_d.
* Period Check No Longer than To_period
* P_PERBI
*Issue # 20041201-001 requested by HS CHO
*Changed by wskim,on 01112005
*-----Start
*  DELETE   &1
*   WHERE   BDATJ =  P_BDATJ
*     AND   (   POPER <  P_PERAB
*          OR   POPER >  P_PERBI ).
*----End
end-of-definition.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
* General Info.
parameters : p_kokrs like csks-kokrs   memory id cac  obligatory.
parameters : p_klvar like cki64a-klvar memory id krt  modif id div,
             p_versn like rkpln-versn  memory id kvs  modif id div,
             p_tvers like keko-tvers   default '01'   modif id div.
parameters : p_elehk like tckh4-elehk  default 'H1'   modif id div.

selection-screen skip 1.

* Costing Type
selection-screen begin of block bl2 with frame title text-002.
selection-screen begin of line.
selection-screen comment  1(13)  text-032. "Business Plan
selection-screen position 15.
parameters : p_std radiobutton group ra01
             user-command  cty.
selection-screen comment  25(13) text-031. "Standard Plan
selection-screen position 39.
parameters : p_bpl radiobutton group ra01.
*SELECTION-SCREEN COMMENT  49(13) TEXT-033. "Actual
*SELECTION-SCREEN POSITION 63.
*PARAMETERS : P_ACT RADIOBUTTON GROUP RA01.
selection-screen end of line.
parameters: p_freig like keko-freig default 'X'.
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
