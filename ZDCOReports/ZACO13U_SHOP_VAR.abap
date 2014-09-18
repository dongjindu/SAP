*&---------------------------------------------------------------------*
*& Report  ZACO13U_SHOP_VAR
*& Analysys Shop Cost Variance
*&---------------------------------------------------------------------*
report zaco13u_shop_var  line-count 65
                         no standard page heading message-id zmco .

************************************************************************
* Date      Developer     Request      Description
* 02/08/07  Manju         UD1K930672   Program bug fix
* 02/09/07  Manju         UD1K930682   Program bug fix
************************************************************************

*----------------------------------------------------------------------*
*   INCLUDE ZACO13U_SHOP_VAR_TOP                                       *
*----------------------------------------------------------------------*

tables : tka01,
         ztco_shop_var, ztco_shop_var_cc,
         ztco_shop_link,
         ztco_shop_sum.


data : it_shop_sum_t   like ztco_shop_sum    occurs 0 with header line,
       it_shop_cc_t    like ztco_shop_cc     occurs 0 with header line,
       it_shop_sum     like ztco_shop_sum    occurs 0 with header line,
       it_shop_cc      like ztco_shop_cc     occurs 0 with header line,
       it_shop_pln     like ztco_shop_pln    occurs 0 with header line,
       it_shop_pln_cc  like ztco_shop_pln_cc occurs 0 with header line,
       it_shop_var     like ztco_shop_var    occurs 0 with header line,
       it_shop_var_temp like ztco_shop_var   occurs 0 with header line,
       it_shop_var_cc  like ztco_shop_var_cc occurs 0 with header line,
       it_shop_link like ztco_shop_link occurs 0 with header line.

data : begin of it_wip occurs 0,
         plant    like ztco_wip-plant,
         matnr    like ztco_wip-matnr,
         outqty   like ztco_wip-outqty,
       end of it_wip.

data : it_fsc like it_wip occurs 0 with header line.
data : g_pln(1).
ranges: r_artnr for ztco_shop_link-abp_matnr.  "PLN + ACT

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
* General Info.
parameters :     p_kokrs like csks-kokrs   memory id cac  obligatory.
parameters :     p_bdatj like ztco_shop_sum-bdatj obligatory
                 memory id bdtj.
parameters :     p_poper like ztco_shop_sum-poper obligatory
                 memory id vpe modif id per.
select-options : s_artnr for ztco_shop_sum-artnr.
select-options : s_typps for ztco_shop_sum-typps.
select-options : s_kstar for ztco_shop_sum-kstar.
select-options : s_resou for ztco_shop_sum-resou.

selection-screen end of block bl1.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
* Controlling Area Information
  perform read_tka01.

* Read data (Actual)
  perform read_ztco_shop_sum.

* Read data (PLAN)
  perform read_ztco_shop_pln.

* Combine Plan/Actual
  perform combine_plan_actual.

* Cal. data
  perform cal_data.

  perform delete_ztco_var .

  perform update_ztco_var.


*&---------------------------------------------------------------------*
*&      Form  READ_TKA01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_tka01.
  clear tka01.
  select single * from tka01
                 where kokrs = p_kokrs.
  if sy-subrc <> 0.
    message e038 with p_kokrs.
  endif.
endform.                    " READ_TKA01
*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_SHOP_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztco_shop_sum.

  data : l_yymm like ztco_wip-yymm.

  select * into corresponding fields of table it_shop_cc_t
    from ztco_shop_cc
   where kokrs = p_kokrs
     and bdatj = p_bdatj
     and poper = p_poper
     and typps in s_typps
     and kstar in s_kstar
     and artnr in s_artnr
     and resou in s_resou.
*    and resou <> '' .         "WHY?

  loop at it_shop_cc_t into it_shop_cc.
    clear: it_shop_cc-aufnr.

*           it_shop_cc-ERDAT,
*           it_shop_cc-ERZET,
*           it_shop_cc-ERNAM,
*           it_shop_cc-AEDAT,
*           it_shop_cc-AEZET,
*           it_shop_cc-AENAM.
    collect it_shop_cc.
  endloop.
  refresh it_shop_cc_t.

  select * into corresponding fields of table it_shop_sum_t
    from ztco_shop_sum
   where kokrs = p_kokrs
     and bdatj = p_bdatj
     and poper = p_poper
     and typps in s_typps
     and kstar in s_kstar
     and artnr in s_artnr
     and resou in s_resou.
*    and resou <> ''.            "WHY??

  loop at it_shop_sum_t.
    it_shop_sum = it_shop_sum_t.
    clear: it_shop_sum-aufnr,
           it_shop_sum-objnr,
           it_shop_sum-verid,

           it_shop_sum-erdat,
           it_shop_sum-erzet,
           it_shop_sum-ernam,
           it_shop_sum-aedat,
           it_shop_sum-aezet,
           it_shop_sum-aenam,
           it_shop_sum-peinh.
    collect it_shop_sum.
  endloop.

  check not it_shop_sum[] is initial.

  data: l_idx like sy-tabix.
  sort it_shop_sum_t by artnr par_werks resou.
  loop at it_shop_sum.
    l_idx = sy-tabix.

    clear it_shop_sum_t.
    read table it_shop_sum_t with key artnr     = it_shop_sum-artnr
                                      par_werks = it_shop_sum-par_werks
                                      resou     = it_shop_sum-resou
                             binary search.
    it_shop_sum-peinh = it_shop_sum_t-peinh.
    modify it_shop_sum index l_idx transporting peinh.

    it_fsc-matnr = it_shop_sum-artnr.
    it_fsc-plant = it_shop_sum-par_werks.
    append it_fsc. clear it_fsc.
    clear it_shop_sum.
  endloop.

  refresh it_shop_sum_t.

  sort it_fsc.
  delete adjacent duplicates from it_fsc.

*  CONCATENATE p_bdatj p_poper+1(2) INTO l_yymm.
*
*  SELECT plant matnr outqty
*         INTO  CORRESPONDING FIELDS OF TABLE it_wip
*     FROM ztco_wip
*      FOR ALL ENTRIES IN it_fsc
*        WHERE gubun = 'F'
*          AND matnr = it_fsc-matnr
*          AND plant = it_fsc-plant
*          AND yymm  = l_yymm .


* read data
  data: ll_wip like it_wip occurs 0 with header line.
  select a~bwkey a~matnr b~out_menge
    into table ll_wip
    from ckmlmv001 as a
   inner join ckmlmv003 as b
      on a~kalnr    =  b~kalnr_bal
      for all entries in it_fsc
   where a~bwkey    =  it_fsc-plant
     and a~matnr    =  it_fsc-matnr
     and a~btyp     =  'BF'
     and b~gjahr    =  p_bdatj
     and b~perio    =  p_poper.

  refresh it_wip.
  loop at ll_wip.
    it_wip = ll_wip.
    collect it_wip.
  endloop.

endform.                    " READ_ZTCO_SHOP_sum

*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_SHOP_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztco_shop_pln.
* GET ABP costing

  check not it_fsc[] is initial.

  select * into corresponding fields of table it_shop_link
   from ztco_shop_link
    for all entries in it_fsc
  where kokrs     = p_kokrs
    and bdatj     = p_bdatj
    and fsc_matnr = it_fsc-matnr.

  sort it_shop_link by fsc_matnr.

  refresh r_artnr.
  r_artnr-sign = 'I'.
  r_artnr-option = 'EQ'.

  loop at it_fsc.
    read table it_shop_link with key fsc_matnr = it_fsc-matnr
               binary search.
    if sy-subrc = 0.
      r_artnr-low = it_shop_link-abp_matnr.
      append r_artnr.
    else.
      r_artnr-low = it_fsc-matnr.
      append r_artnr.
    endif.
  endloop.
  delete adjacent duplicates from r_artnr.

  select * into corresponding fields of table it_shop_pln_cc
     from ztco_shop_pln_cc
    where kokrs = p_kokrs
      and bdatj = p_bdatj
      and poper = p_poper
      and klvar = 'ZPCP'
      and artnr in r_artnr.

  select * into corresponding fields of table it_shop_pln
     from ztco_shop_pln
    where kokrs = p_kokrs
      and bdatj = p_bdatj
      and poper = p_poper
      and klvar = 'ZPCP'
      and artnr in r_artnr.


endform.                    " READ_ZTCO_SHOP_pln
*&---------------------------------------------------------------------*
*&      Form  CAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_data.
  sort it_wip         by plant matnr.
  sort it_shop_link   by fsc_matnr.
  sort it_shop_pln    by artnr.
  sort it_shop_pln_cc by artnr typps kstar resou elemt.
  sort it_shop_sum    by artnr typps kstar resou.

  perform make_it_shop_var_cc.

  perform make_it_shop_var.


endform.                    " CAL_DATA
*&---------------------------------------------------------------------*
*&      Form  get_base_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_base_data.
  data : l_matnr type matnr.

  move-corresponding it_shop_cc to it_shop_var_cc.

  it_shop_var_cc-fsc_matnr = it_shop_cc-artnr.

*     Shop Sum
  clear it_shop_sum.
  read table it_shop_sum with key artnr =  it_shop_cc-artnr
                                  typps = it_shop_cc-typps
                                  kstar = it_shop_cc-kstar
                                  resou = it_shop_cc-resou
                                  binary search.


*     Gr Qty
  clear it_wip.
  read table it_wip with key matnr = it_shop_sum-artnr
                             binary search.

*     ABP
  clear : it_shop_link, l_matnr.
  read table  it_shop_link with key fsc_matnr = it_shop_cc-artnr
                                    binary search.
  if sy-subrc = 0.
    l_matnr = it_shop_link-abp_matnr.
  else.
    l_matnr = it_shop_cc-artnr.
  endif.
*     Plan
  clear it_shop_pln_cc.
  read table it_shop_pln_cc with key
                                 artnr     = l_matnr
                                 typps     = it_shop_cc-typps
                                 kstar     = it_shop_cc-kstar
                                 resou     = it_shop_cc-resou
                                 elemt     = it_shop_cc-elemt
                                 binary search.
  if sy-subrc = 0 .
    g_pln = 'X'.
  else.
    g_pln = ''.
  endif.

endform.                    " get_base_data
*&---------------------------------------------------------------------*
*&      Form  move_shop_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_shop_var_cc.

  it_shop_var_cc-werks        =  it_shop_sum-par_werks.
  it_shop_var_cc-shop         =  it_shop_sum-shop.
  it_shop_var_cc-frozen_amt   =  it_wip-outqty *
                                 it_shop_pln_cc-wertn.
  it_shop_var_cc-frozen_amt_f =  it_wip-outqty *
                                 it_shop_pln_cc-wrtfx.

  it_shop_var_cc-std_amt      =  it_shop_cc-gr_amt.
  it_shop_var_cc-std_amt_f    =  it_shop_cc-gr_amt_f.

  it_shop_var_cc-cont_amt     =  it_shop_cc-manu_amt.
  it_shop_var_cc-cont_amt_f   =  it_shop_cc-manu_amt_f.

  it_shop_var_cc-target_amt   =  it_shop_cc-target_amt.
  it_shop_var_cc-target_amt_f =  it_shop_cc-target_amt_f.


endform.                    " move_shop_var_cc
*&---------------------------------------------------------------------*
*&      Form  cal_input_diff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_input_diff using p_act_price
                          p_act_price_f.


  data : l_act_price   type p decimals 4,
         l_act_price_f type p decimals 4.

*     Diff. : Input
  it_shop_var_cc-inpdif       =  it_shop_var_cc-cont_amt -
                                 it_shop_var_cc-target_amt.
  it_shop_var_cc-inpdif_f     =  it_shop_var_cc-cont_amt_f -
                                 it_shop_var_cc-target_amt_f.

  if it_shop_sum-target_qty <>  0 .
*         price diff
    it_shop_var_cc-prcdif   = it_shop_var_cc-cont_amt -
       ( it_shop_var_cc-target_amt   * it_shop_sum-manu_qty /
         it_shop_sum-target_qty ) .
    it_shop_var_cc-prcdif_f = it_shop_var_cc-cont_amt_f -
       ( it_shop_var_cc-target_amt_f * it_shop_sum-manu_qty /
         it_shop_sum-target_qty ) .

  endif.
*  if Target Qty * Control Qty = 0  : Spec Diff
*  elseif Target Qty * Control Qty <> 0 : Qty Diff
  if it_shop_sum-target_qty = 0 or it_shop_sum-manu_qty = 0 .
*   Spec Diff
    if it_shop_sum-peinh <> 0 .
      it_shop_var_cc-sp1dif   = ( it_shop_sum-manu_qty -
                                 it_shop_sum-target_qty ) *
                                  p_act_price / it_shop_sum-peinh.

      it_shop_var_cc-sp1dif_f = ( it_shop_sum-manu_qty -
                                 it_shop_sum-target_qty ) *
                                 p_act_price_f / it_shop_sum-peinh.

    endif.
  else.
*   Qty Diff
    it_shop_var_cc-usgdif   = it_shop_var_cc-target_amt *
  ( ( it_shop_sum-manu_qty   / it_shop_sum-target_qty ) - 1 ).
    it_shop_var_cc-usgdif_f = it_shop_var_cc-target_amt_f *
     ( ( it_shop_sum-manu_qty / it_shop_sum-target_qty ) - 1 ).
  endif.


  it_shop_var_cc-othdif   = it_shop_var_cc-inpdif -
                            it_shop_var_cc-prcdif -
                            it_shop_var_cc-usgdif -
                            it_shop_var_cc-sp1dif.

  it_shop_var_cc-othdif_f = it_shop_var_cc-inpdif_f -
                            it_shop_var_cc-prcdif_f -
                            it_shop_var_cc-usgdif_f -
                            it_shop_var_cc-sp1dif_f.


endform.                    " cal_input_diff
*&---------------------------------------------------------------------*
*&      Form  cal_output_diff
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form  cal_output_diff using p_act_price
                            p_act_price_f.
*     Diff. : output
  it_shop_var_cc-outdif       =  it_shop_var_cc-target_amt -
                                 it_shop_var_cc-std_amt.
  it_shop_var_cc-outdif_f     =  it_shop_var_cc-target_amt_f -
                                 it_shop_var_cc-std_amt_f.
*   Sepc Diff
  if it_shop_sum-target_qty =  0 or it_shop_sum-gr_qty = 0 .
    if it_shop_sum-peinh <> 0 .
      it_shop_var_cc-sp2dif   = ( it_shop_sum-target_qty -
                                  it_shop_sum-gr_qty ) *
                                  p_act_price / it_shop_sum-peinh.
    endif.

    if it_shop_sum-peinh <> 0 .
      it_shop_var_cc-sp2dif_f = ( it_shop_sum-target_qty  -
                                  it_shop_sum-gr_qty ) *
                                  p_act_price_f / it_shop_sum-peinh.
    endif.

  endif.
  it_shop_var_cc-oth2dif   = it_shop_var_cc-outdif -
                           it_shop_var_cc-sp2dif.

  it_shop_var_cc-oth2dif_f = it_shop_var_cc-outdif_f -
                           it_shop_var_cc-sp2dif_f.

endform.                    " cal_output_diff
*&---------------------------------------------------------------------*
*&      Form  cal_diff_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form cal_diff_qty.

  if it_shop_var-target_qty   = 0 or it_shop_var-cont_qty = 0 .
    it_shop_var-spdif_q  = it_shop_var-cont_qty -
                               it_shop_var-target_qty.
  else.
    it_shop_var-usgdif_q   = it_shop_var-cont_qty -
                                it_shop_var-target_qty.
  endif.

  if it_shop_var-target_qty   = 0 or it_shop_var-std_qty = 0 .
    it_shop_var-sp2dif_q   =  it_shop_var-target_qty -
                                 it_shop_var-std_qty .
  endif.

  it_shop_var-sp3dif_q      = it_shop_var-std_qty -
                                it_shop_var-frozen_qty.

  if g_pln = ''.
    clear :  it_shop_var-spdif_q,it_shop_var-sp3dif_q  .
  endif.

endform.                    " cal_diff_qty
*&---------------------------------------------------------------------*
*&      Form  make_it_shop_var_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_it_shop_var_cc.
  data : l_act_price   type p decimals 4,
         l_act_price_f type p decimals 4.


  loop at it_shop_cc.

    perform get_base_data.

    perform move_shop_var_cc.

*---actual unit price can be different due to ML restriction : ANDY
    if it_shop_sum-manu_qty <> 0 .
      l_act_price   = it_shop_cc-manu_amt   / it_shop_sum-manu_qty.
      l_act_price_f = it_shop_cc-manu_amt_f / it_shop_sum-manu_qty.
    else.
      l_act_price   = it_shop_sum-VERPR.
      l_act_price_f = 0.
    endif.

    perform cal_input_diff using l_act_price
                                 l_act_price_f.

*valuation with STD price??? - FIXME ANDY
    perform cal_output_diff using l_act_price
                                  l_act_price_f.

    if g_pln = 'X'.
      it_shop_var_cc-sp3dif  = it_shop_var_cc-std_amt -
                               it_shop_var_cc-frozen_amt.

      it_shop_var_cc-sp3dif_f  = it_shop_var_cc-std_amt_f -
                                 it_shop_var_cc-frozen_amt_f.
    else.
      clear : it_shop_var_cc-sp3dif,it_shop_var_cc-sp3dif_f.
    endif.
    collect it_shop_var_cc.
    clear it_shop_var_cc.

  endloop.


endform.                    " make_it_shop_var_cc
*&---------------------------------------------------------------------*
*&      Form  make_it_shop_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_it_shop_var.

  loop at it_shop_var_cc.
    move-corresponding it_shop_var_cc to it_shop_var_temp.
    collect it_shop_var_temp. clear it_shop_var_temp.
  endloop.

  sort it_shop_var_temp by fsc_matnr typps kstar resou.

  loop at it_shop_sum.

    perform get_base_data2.

    perform move_shop_var.

    perform cal_diff_qty.

    collect it_shop_var. clear it_shop_var.
  endloop.


endform.                    " make_it_shop_var
*&---------------------------------------------------------------------*
*&      Form  move_shop_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_shop_var.
  move-corresponding it_shop_sum to it_shop_var.
  it_shop_var-werks           =  it_shop_sum-par_werks.
  it_shop_var-fsc_matnr       =  it_shop_sum-artnr.

  it_shop_var-frozen_qty      =  it_wip-outqty *
                                 it_shop_pln-menge.
  it_shop_var-std_qty         =  it_shop_sum-gr_qty.
  it_shop_var-cont_qty        =  it_shop_sum-manu_qty.
  it_shop_var-cont_qty        =  it_shop_sum-manu_qty.
  it_shop_var-target_qty      =  it_shop_sum-target_qty.

*  it_shop_var-frozen_amt      =  it_shop_var_temp-frozen_amt .

*  it_shop_var-std_amt         =  it_shop_var_temp-std_amt.
*  it_shop_var-target_amt      =  it_shop_var_temp-target_amt.
*  it_shop_var-cont_amt        =  it_shop_var_temp-cont_amt.
  it_shop_var-std_amt         =  it_shop_sum-gr_amt.
  it_shop_var-target_amt      =  it_shop_sum-target_amt.
  it_shop_var-cont_amt        =  it_shop_sum-control_amt.

* input variance
  it_shop_var-inpdif          =  it_shop_var_temp-inpdif.
  it_shop_var-prcdif          =  it_shop_var_temp-prcdif.
  it_shop_var-usgdif          =  it_shop_var_temp-usgdif.
  it_shop_var-sp1dif          =  it_shop_var_temp-sp1dif.
  it_shop_var-othdif          =  it_shop_var_temp-othdif.
* output variance
  it_shop_var-outdif          =  it_shop_var_temp-outdif.
  it_shop_var-sp2dif          =  it_shop_var_temp-sp2dif.
  it_shop_var-oth2dif         =  it_shop_var_temp-oth2dif.

  if g_pln = 'X'.
    it_shop_var-sp3dif          =  it_shop_var_temp-sp3dif.
  else.
    clear : it_shop_var-sp3dif .
  endif.


endform.                    " move_shop_var
*&---------------------------------------------------------------------*
*&      Form  get_base_data2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_base_data2.
  data : l_matnr type matnr.
  clear it_shop_var_temp.
  read table it_shop_var_temp with key
                                fsc_matnr    = it_shop_sum-artnr
                                    typps    = it_shop_sum-typps
                                    kstar    = it_shop_sum-kstar
                                    resou    = it_shop_sum-resou
                                  binary search.

  clear it_wip.
  read table it_wip with key matnr = it_shop_sum-artnr
                             binary search.

* ABP
  clear  it_shop_link.
  read table  it_shop_link with key fsc_matnr = it_shop_sum-artnr
                                    binary search.
  if sy-subrc = 0.
    l_matnr = it_shop_link-abp_matnr.
  else.
    l_matnr = it_shop_sum-artnr.
  endif.

* Plan
  clear it_shop_pln.
  read table it_shop_pln with key artnr     = l_matnr
                                  binary search.

  if sy-subrc = 0 .
    g_pln = 'X'.
  else.
    clear g_pln.
  endif.

endform.                    " get_base_data2
*&---------------------------------------------------------------------*
*&      Form  update_ztco_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ztco_var.
  loop at it_shop_var.
* LOG
    it_shop_var-erdat = sy-datum.
    it_shop_var-erzet = sy-uzeit.
    it_shop_var-ernam = sy-uname.
* CURKY
    clear ztco_shop_var.
    move-corresponding  it_shop_var to ztco_shop_var.

    ztco_shop_var-kokrs = p_kokrs.
    ztco_shop_var-bdatj = p_bdatj.
    ztco_shop_var-poper = p_poper.

    insert ztco_shop_var .
*    IF sy-subrc <> 0.
**      WRITE : / ztco_shopcost_a2 .
*      BREAK-POINT.
*      MESSAGE e044.
*    ENDIF.
    clear it_shop_var.
  endloop.

  commit work.

  loop at it_shop_var_cc.
* LOG
    it_shop_var_cc-erdat = sy-datum.
    it_shop_var_cc-erzet = sy-uzeit.
    it_shop_var_cc-ernam = sy-uname.
* CURKY
    clear ztco_shop_var_cc.
    move-corresponding  it_shop_var_cc to ztco_shop_var_cc.

    ztco_shop_var_cc-kokrs = p_kokrs.
    ztco_shop_var_cc-bdatj = p_bdatj.
    ztco_shop_var_cc-poper = p_poper.

    insert ztco_shop_var_cc .
    if sy-subrc <> 0.
      break-point.
      message e044.
    endif.
    clear it_shop_var_cc.
  endloop.

  if sy-subrc = 0 .
    message s009.
  endif.

  commit work.

endform.                    " update_ztco_var
*&---------------------------------------------------------------------*
*&      Form  delete_ztco_var
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_ztco_var.
  delete from ztco_shop_var
     where
              kokrs        =  p_kokrs
          and bdatj        =  p_bdatj
          and poper        =  p_poper
          and fsc_matnr in s_artnr.

  delete from ztco_shop_var_cc
     where
              kokrs        =  p_kokrs
          and bdatj        =  p_bdatj
          and poper        =  p_poper
          and fsc_matnr in s_artnr.


endform.                    " delete_ztco_var
*&---------------------------------------------------------------------*
*&      Form  combine_plan_actual
*&---------------------------------------------------------------------*
form combine_plan_actual.
  data : l_matnr type matnr.


  sort it_shop_pln    by artnr.
  sort it_shop_pln_cc by artnr.
  sort it_shop_sum    by artnr typps kstar resou.
  sort it_shop_cc     by artnr typps kstar resou elemt.

  loop at it_fsc.
    perform get_abp_matnr using    it_fsc-matnr
                          changing l_matnr.

    loop at it_shop_pln  where artnr = l_matnr.
      read table it_shop_sum with key
                                 artnr     = it_shop_pln-artnr
                                 typps     = it_shop_pln-typps
                                 kstar     = it_shop_pln-kstar
                                 resou     = it_shop_pln-resou
                            binary search.
      if sy-subrc <> 0.
        it_shop_sum_t-mandt  = sy-mandt.                     "UD1K930682
        it_shop_sum_t-kokrs  = p_kokrs.
        it_shop_sum_t-bdatj  = p_bdatj.
        it_shop_sum_t-poper  = p_poper.

        it_shop_sum_t-artnr = it_shop_pln-artnr.
        it_shop_sum_t-typps = it_shop_pln-typps.
        it_shop_sum_t-kstar = it_shop_pln-kstar.
        it_shop_sum_t-resou = it_shop_pln-resou.
        append it_shop_sum_t.
      endif.
    endloop.

    loop at it_shop_pln_cc where artnr = l_matnr.
      read table it_shop_cc with key
                                 artnr     = it_shop_pln_cc-artnr
                                 typps     = it_shop_pln_cc-typps
                                 kstar     = it_shop_pln_cc-kstar
                                 resou     = it_shop_pln_cc-resou
                                 elemt     = it_shop_pln_cc-elemt
                            binary search.
      if sy-subrc <> 0.
        it_shop_cc_t-mandt  = sy-mandt.                     "UD1K930682
        it_shop_cc_t-kokrs  = p_kokrs.
        it_shop_cc_t-bdatj  = p_bdatj.
        it_shop_cc_t-poper  = p_poper.

        it_shop_cc_t-artnr = it_shop_pln_cc-artnr.
        it_shop_cc_t-typps = it_shop_pln_cc-typps.
        it_shop_cc_t-kstar = it_shop_pln_cc-kstar.
        it_shop_cc_t-resou = it_shop_pln_cc-resou.
        it_shop_cc_t-elemt = it_shop_pln_cc-elemt.
        append it_shop_cc_t.
      endif.
    endloop.

  endloop.


  append lines of it_shop_cc_t   to it_shop_cc.
  append lines of it_shop_sum_t  to it_shop_sum.

endform.                    " combine_plan_actual
*&---------------------------------------------------------------------*
*&      Form  get_abp_matnr
*&---------------------------------------------------------------------*
form get_abp_matnr using    f_mat
                   changing abp_matnr.

  read table  it_shop_link with key fsc_matnr = f_mat
                                    binary search.
  if sy-subrc = 0.
    abp_matnr = it_shop_link-abp_matnr.
  else.
    abp_matnr = f_mat.
  endif.

endform.                    " get_abp_matnr
