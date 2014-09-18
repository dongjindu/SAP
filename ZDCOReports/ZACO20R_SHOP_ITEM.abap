*--------------------------------------------------------------------*
* Prgram  : ZACO20R_SHOP_ITEM                                        *
* Date    :
* Author  : Itemization
* Spec    : Andy Choi
*--------------------------------------------------------------------*
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  01/12/2012  Valerian  UD1K953687  HMMA Engine Plant split
*                                    implementation
*----------------------------------------------------------------------
report  zaco20r_shop_item message-id zmco no standard page heading.


tables : ztco_shop_sum,
         tka01,
         a018,
         mara.

data : it_shop_sum like table of ztco_shop_sum with header line.
data : begin of it_shop_cc  occurs 0.
        include structure ztco_shop_cc.
data :   verid like ztco_shop_sum-verid,
       end of it_shop_cc.

data : begin of it_display occurs 0,
       poper like ztco_shop_sum-poper,
       artnr like ztco_shop_sum-artnr,
       typps like ztco_shop_sum-typps,
       kstar like ztco_shop_sum-kstar,
       resou like ztco_shop_sum-resou,

       shop  like ztco_shop_sum-shop,

       wkgbtr       like ztco_shop_sum-wkgbtr,
       wkgbtr2      like ztco_shop_sum-wkgbtr2,
       add_wkgbtr   like ztco_shop_sum-wkgbtr,
       wip_amt      like ztco_shop_sum-wip_amt,
       wip_pamt     like ztco_shop_sum-wip_pamt,
       scrap_amt    like ztco_shop_sum-scrap_amt,
       manu_amt     like ztco_shop_sum-manu_amt,
       gr_amt       like ztco_shop_sum-gr_amt,
       single_amt   like ztco_shop_sum-single_amt,
       multi_amt    like ztco_shop_sum-multi_amt,
       multi_samt   like ztco_shop_sum-multi_samt ,
       multi_mamt   like ztco_shop_sum-multi_mamt,

       MBGBTR       like ztco_shop_sum-MBGBTR,
       MBGBTR2      like ztco_shop_sum-MBGBTR2,
       ADD_MBGBTR   like ztco_shop_sum-ADD_MBGBTR,
       WIP_QTY      like ztco_shop_sum-WIP_QTY,
       gr_qty       like ztco_shop_sum-gr_qty,    "Component GR
       single_qty   like ztco_shop_sum-single_qty,
       manu_qty     like ztco_shop_sum-manu_qty,
       pp_grqty     like ztco_shop_sum-gr_qty,    "Vehicle GR

       maktg        like makt-maktg,

       llv_matnr like ztco_shop_sum-llv_matnr,
       kostl     like ztco_shop_sum-kostl,
       lstar     like ztco_shop_sum-lstar,
       par_werks like ztco_shop_sum-par_werks,
       bwkey     like ztco_shop_sum-bwkey,

       stprs     like ztco_shop_sum-stprs,
       verpr     like ztco_shop_sum-verpr,
       peinh     like ztco_shop_sum-peinh,
       meeht     like ztco_shop_sum-meeht,

       aufnr     like ztco_shop_sum-aufnr,
       verid     like ztco_shop_sum-verid,

       lifnr like ekko-lifnr,
       name1 like lfa1-name1.

data : end of it_display .

data : begin of it_ckmlmv003 occurs 0,
         bwkey      like ckmlmv001-bwkey,
         matnr      like ckmlmv001-matnr,
         aufnr      like ckmlmv013-aufnr,
         verid_nd   like ckmlmv001-verid_nd,
         meinh      like ckmlmv003-meinh,
         out_menge  like ckmlmv003-out_menge,
       end of  it_ckmlmv003.

data : begin of it_mara occurs 0,
         werks type werks_d,
         matnr type matnr,
       end of it_mara.

data : begin of it_marc occurs 0,
         matnr      like marc-matnr,
         werks      like marc-werks,

         raube      like mara-raube, "shop
         fevor      like marc-fevor, "PP schedule
         mtart      like mara-mtart,
         matkl      like mara-matkl,
         vspvb      like marc-vspvb,
         prctr      like marc-prctr,
         maktg      like makt-maktg,
       end of  it_marc.


data : begin of it_s013  occurs 0,
         matnr like s013-matnr,
         lifnr like s013-lifnr,
         name1 like lfa1-name1,
         spmon like s013-spmon,
       end of it_s013 .

data : begin of it_ekbe  occurs 0,
         matnr type matnr,
         lifnr like ekko-lifnr,
         budat like ekbe-budat,
       end of it_ekbe .

data : begin of it_lfa1  occurs 0,
         lifnr like lfa1-lifnr,
         name1 like lfa1-name1,
       end of it_lfa1 .

*--------------------------------------------------------------------*
* INCLUDE
*--------------------------------------------------------------------*
include zco_alv_top.
include zco_alv_form.


*--------------------------------------------------------------------*
* INITIALIZATION.
*--------------------------------------------------------------------*
initialization.
*  perform init_variant.

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
  selection-screen begin of block bl1 with frame title text-001.

  parameters     : p_kokrs like ztco_shop_sum-kokrs memory id cac,
                   p_bdatj like ztco_shop_sum-bdatj memory id bdtj.
  select-options : s_poper for ztco_shop_sum-poper  memory id popr,
                   s_artnr for ztco_shop_sum-artnr  memory id  mat,
                   s_verid for ztco_shop_sum-verid,
                   s_typps for ztco_shop_sum-typps,
                   s_kstar for ztco_shop_sum-kstar memory id ka3,
                   s_resou for ztco_shop_sum-resou,
                   s_SHOP  for ztco_shop_sum-SHOP,
                   s_fevor for ztco_shop_sum-fevor.

  selection-screen end of block bl1.
*  select-options : s_aufnr for ztco_shop_sum-aufnr  memory id anr,
*                   s_verid for ztco_shop_sum-verid.
  parameters: p_shop   as checkbox default ' '.  "derive shop from mater

* Reporting Options
  selection-screen begin of block bl2 with frame title text-002.
  selection-screen begin of line.
  selection-screen comment (15) text-004.
  selection-screen position 33.
*--(scale)
  parameters: p_trunc like rfpdo1-ts70skal  default '0'.
  selection-screen comment 35(1) text-005.
*--(decimals)
  parameters: p_decl like rfpdo1-ts70skal  default '0'.
  selection-screen end of line.

*  selection-screen skip 1.
  selection-screen begin of block bl3 with frame title text-003.
  parameters : p_unit  radiobutton group ra01.    "Unit price
  parameters : p_sum   radiobutton group ra01.    "Amt

  selection-screen end of block bl3.


  selection-screen skip 1.


  parameters: p_lifnr as checkbox default ' '.
  select-options: s_lifnr for a018-lifnr.
  selection-screen skip 1.
  parameters: p_vsum   as checkbox default 'X'.  "Sum all version
  parameters: p_matgrp  as checkbox default 'X'. "Sum by material group

  selection-screen end of block bl2.

*--------------------------------------------------------------------*
start-of-selection.
*--------------------------------------------------------------------*

* Select SHOP Summary
  perform read_tka01.
  perform get_table.

* AMT displayed per unit
* IF P_UNIT = 'X'.
  perform get_product_gr.
* ENDIF.

  perform define_mara.
  perform get_material_group.

* Vendor displayed
  if p_lifnr = 'X'.
    if 1 = 2.
      perform get_lifnr_for_part.
    endif.
    perform get_lifnr_for_part_new.
    perform get_lifnr_name.

    it_s013-matnr = 'R16N'.  "*R16N Engine
    it_s013-lifnr = 'SEF9'.
    append it_s013.

  endif.

  perform make_data.

*--------------------------------------------------------------------*
end-of-selection.
*--------------------------------------------------------------------*
*-- Data Display
  perform display_data.

*&---------------------------------------------------------------------*
*&      Form  GET_TABLE
*&---------------------------------------------------------------------*
form get_table .

  perform get_shop_cost.

endform.                    " GET_TABLE
*&---------------------------------------------------------------------*
*&      Form  GET_SHOP_COST
*&---------------------------------------------------------------------*
form get_shop_cost.

  select  *  into table it_shop_sum
     from ztco_shop_sum
    where kokrs       =  p_kokrs
      and bdatj       =  p_bdatj
      and poper       in s_poper
      and typps       in s_typps
      and kstar       in s_kstar
      and resou       in s_resou
      and artnr       in s_artnr
      and verid       in s_verid
      and shop        in s_shop
      and fevor       in s_fevor.

*  describe table it_shop_sum lines sy-index.
*  check sy-index > 0.

endform.                    " GET_SHOP_COST
*&---------------------------------------------------------------------*
*&      Form  MAKE_DATA
*&---------------------------------------------------------------------*
form make_data .

  sort it_marc by matnr werks.
  sort it_shop_sum by kokrs bdatj poper aufnr resou kstar.

  sort it_s013 by matnr.
  sort it_lfa1 by lifnr.
  sort it_shop_cc.
  sort it_ckmlmv003 by aufnr.

  loop at it_shop_sum.
    clear it_display.
    move-corresponding it_shop_sum to it_display.
*   clear it_display-poper.

    perform fill_shop_sum_info.
*      perform fill_basic_info.

*   Version SUM
    if p_vsum = 'X'.
      clear: it_display-verid, it_display-aufnr.
    endif.

    collect it_display.
  endloop.


  data: l_idx type i.

  sort it_ckmlmv003 by matnr verid_nd.
  loop at it_display.
    l_idx = sy-tabix.

    perform get_pp_grqty changing it_display-pp_grqty.
    check it_display-pp_grqty > 0.

    if p_unit = 'X'.
      perform calculate_unit.
    endif.

    modify it_display index l_idx.
  endloop.

  sort it_display by artnr.

*  IF p_unit = 'X'.
*    PERFORM recalculate_per_unit.
*  ENDIF.
endform.                    " MAKE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
form display_data .

*-- ALV layout
  perform alv_set_layout   using  space  space
                                  space  space.

*-- Event
  perform alv_get_event    using  gt_events.

*-- Fieldcategory
  perform alv_get_fieldcat tables gt_fieldcat using 'DISPLAY'.
  perform alv_chg_fieldcat tables gt_fieldcat.

*-- Sort
  perform set_sort         tables gt_alv_sort.

*-- Top of page
*  PERFORM SET_TOP_PAGE.

*-- Display
  perform alv_grid_display tables it_display.


endform.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_CHG_FIELDCAT
*&---------------------------------------------------------------------*
form alv_chg_fieldcat tables pt_fieldcat type slis_t_fieldcat_alv.

  perform alv_chg_fieldcat_1 tables pt_fieldcat.

endform.                    " ALV_CHG_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  ALV_CHG_FIELDCAT_1
*&---------------------------------------------------------------------*
form alv_chg_fieldcat_1 tables pt_fieldcat type slis_t_fieldcat_alv.

  read table it_display index 1.

  loop at pt_fieldcat into gs_fieldcat.
    clear :  gs_fieldcat-key, gs_fieldcat-no_out.

    if gs_fieldcat-col_pos < 8.
      gs_fieldcat-key     = 'X'.
    endif.

    if gs_fieldcat-cfieldname = 'WAERS'.
      gs_fieldcat-do_sum = 'X'.
    endif.

    if gs_fieldcat-datatype = 'CURR'.
      gs_fieldcat-datatype = 'DEC'.
      gs_fieldcat-round    = p_trunc.

      if not p_decl is initial.
        gs_fieldcat-decimals_out = p_decl.
      endif.
    endif.

    case gs_fieldcat-fieldname.
      when 'RESOU'.  " Resource
        set_fieldcat gs_fieldcat text-t07 .
      when 'VERPR'.  " moving avg
        set_fieldcat gs_fieldcat text-t38 .
      when 'WKGBTR'.  " Current (Overall)
        set_fieldcat gs_fieldcat text-t23 .
      when 'WKGBTR2'.  " Current var. (Overall)
        set_fieldcat gs_fieldcat text-t24 .
      when 'ADD_WKGBTR'.  " Current  Additive (Overall)
        set_fieldcat gs_fieldcat text-t25 .
      when 'WIP_AMT'.  " WIP - begining (Overall)
        set_fieldcat gs_fieldcat text-t27 .
      when 'WIP_PAMT'." WIP - ending (Overall)
        set_fieldcat gs_fieldcat text-t28 .
      when 'SCRAP_AMT'.  " Scrap (Overall)
        set_fieldcat gs_fieldcat text-t29 .
      when 'MANU_AMT'.  " Manufacturing (Overall)
        set_fieldcat gs_fieldcat text-t30 .
      when 'GR_AMT'.  " GR (Overall)
        set_fieldcat gs_fieldcat text-t31.
      when 'SINGLE_AMT'.  " Material ledger - single (Overall)
        set_fieldcat gs_fieldcat text-t32.
      when 'MULTI_AMT'.  " Material ledger - multi (Overall)
        set_fieldcat gs_fieldcat text-t33.
      when 'MULTI_SAMT'.  " Material ledger - multi-single (Overall)
        set_fieldcat gs_fieldcat text-t34.
      when 'MULTI_MAMT'.  " Material ledger - multi-multi (Overall)
        set_fieldcat gs_fieldcat text-t35.

      when 'MANU_QTY'.  " Manu_Qty
        set_fieldcat gs_fieldcat text-t53 .
      when 'GR_QTY'.    " Comp STD Qty
        set_fieldcat gs_fieldcat text-t54 .
      when 'SINGLE_QTY'. " Single-level diff.
        set_fieldcat gs_fieldcat text-t55.
      when 'PP_GRQTY'.    " GR Qty
        set_fieldcat gs_fieldcat text-t56 .
    endcase.

    clear: gs_fieldcat-cfieldname,
           gs_fieldcat-ctabname.
    modify pt_fieldcat from gs_fieldcat.
  endloop.

endform.
*&--------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&--------------------------------------------------------------------*
form pf_status_set using it_extab type slis_t_extab .

  set pf-status 'STANDARD'.  "  EXCLUDING IT_EXTAB.

endform.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM ALV_user_command
*---------------------------------------------------------------------*
form user_command using p_ucomm     like sy-ucomm
                        ps_selfield type slis_selfield.


endform.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  SET_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_ALV_SORT  text
*----------------------------------------------------------------------*
form set_sort tables  pt_alv_sort structure gs_alv_sort.

*
*  clear : gs_alv_sort,  gt_alv_sort[].
*
*  case 'X'.
*    when p_but1.
*      gs_alv_sort-spos      = '1'.
*      gs_alv_sort-fieldname = 'LIFNR'.
*      gs_alv_sort-tabname   = 'IT_DISPLAY'.
*      gs_alv_sort-up        = 'X'.
*      gs_alv_sort-subtot    = 'X'.
*      append gs_alv_sort  to gt_alv_sort.
*
*    when p_but2.
*      gs_alv_sort-spos      = '1'.
*      gs_alv_sort-fieldname = 'SAKTO'.
*      gs_alv_sort-tabname   = 'IT_DISPLAY'.
*      gs_alv_sort-up        = 'X'.
*      gs_alv_sort-subtot    = 'X'.
*      append gs_alv_sort  to gt_alv_sort.
*
*    when p_but3.
*      gs_alv_sort-spos      = '1'.
*      gs_alv_sort-fieldname = 'BELNR'.
*      gs_alv_sort-tabname   = 'IT_DISPLAY'.
*      gs_alv_sort-up        = 'X'.
*      gs_alv_sort-subtot    = 'X'.
*      append gs_alv_sort  to gt_alv_sort.
*  endcase.
endform.                    " SET_SORT
*&---------------------------------------------------------------------*
*&      Form  SET_TOP_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_top_page.
  clear : gs_line.
  gs_line-typ  = 'H'.
  gs_line-info = text-h01.
  append gs_line to gt_list_top_of_page .

*  CLEAR : gs_line.
*  APPEND INITIAL LINE TO gt_list_top_of_page .

*  append_top :
*      'S' text-h02 s_kokrs-low s_kokrs-high,
*      'S' text-h03 p_bdatj-low p_bdatj-high,
*      'S' text-h04 s_poper-low s_poper-high,
*      'S' text-h13 s_shop-low  s_shop-high,
*      'S' text-h05 s_aufnr-low s_aufnr-high,
*      'S' text-h06 s_artnr-low s_artnr-high,
*      'S' text-h07 s_matnr-low s_matnr-high,
*      'S' text-h08 s_typps-low s_typps-high,
*      'S' text-h09 s_kstar-low s_kstar-high,
*      'S' text-h10 s_elemt-low s_elemt-high,
*      'S' text-h11 s_kostl-low s_kostl-high,
*      'S' text-h12 s_lstar-low s_lstar-high.

endform.                    " SET_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  get_product_gr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_product_gr.
  data : begin of it_proc_gr occurs 0,
          par_werks like it_shop_sum-bwkey,
          artnr     like it_shop_sum-artnr,
          aufnr     like it_shop_sum-aufnr,
          verid     like it_shop_sum-verid,
          bdatj     like it_shop_sum-bdatj,
          poper     like it_shop_sum-poper,
        end of it_proc_gr.

  data : it_ckmlmv003_temp like it_ckmlmv003 occurs 0 with header line.


  loop at it_shop_sum.
    move-corresponding it_shop_sum to it_proc_gr.
    append it_proc_gr. clear it_proc_gr.
  endloop.
  sort it_proc_gr.
  delete adjacent duplicates from it_proc_gr.

* read GR data
  select  a~bwkey a~matnr a~verid_nd
          c~aufnr
          b~out_menge b~meinh
    into corresponding fields of table it_ckmlmv003_temp
    from ckmlmv001 as a
    inner join ckmlmv003 as b
       on a~kalnr    =  b~kalnr_bal
    inner join ckmlmv013 as c
       on c~KALNR_PROC = b~kalnr_IN
     for all entries in it_proc_gr
   where a~bwkey    =  it_proc_gr-par_werks
     and a~matnr    =  it_proc_gr-artnr
     and a~verid_nd =  it_proc_gr-verid
     and a~btyp     =  'BF'
     and b~gjahr    =  p_bdatj
     and b~perio    in s_poper
**     and c~flg_wbwg = 'X'.   "goods movement
***    and c~loekz    = space. "Not deleted.
     AND C~FLG_WBWG = 'X'
     AND C~AUTYP = '05'.

  loop at it_ckmlmv003_temp.
    move-corresponding it_ckmlmv003_temp to it_ckmlmv003.

    if p_vsum = 'X'.
      clear: it_ckmlmv003-verid_nd,
             it_ckmlmv003-aufnr.
    endif.

    collect it_ckmlmv003. clear it_ckmlmv003.
  endloop.

endform.                    " get_product_gr
*&---------------------------------------------------------------------*
*&      Form  recalculate_per_unit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form recalculate_per_unit.
*  data : l_pp_grqty like  it_ckmlmv003-out_menge.
*
*  loop at it_display.
*    clear it_ckmlmv003.
*
*    if p_vsum = 'X'.
*      read table it_ckmlmv003 with key bwkey    = it_display-par_werks
*                                       matnr    = it_display-artnr.
*    else.
*      read table it_ckmlmv003 with key bwkey    = it_display-par_werks
*                                       matnr    = it_display-artnr
*                                       verid_nd = it_display-verid.
*    endif.
*
*    clear : l_pp_grqty.
*    l_pp_grqty =  it_ckmlmv003-out_menge.
*
*    if l_pp_grqty <>  0 .
*      it_display-wkgbtr     = it_display-wkgbtr      / l_pp_grqty.
*      it_display-wkgbtr2    = it_display-wkgbtr2     / l_pp_grqty.
*      it_display-wkgbtr     = it_display-wkgbtr      / l_pp_grqty.
*      it_display-wkgbtr2    = it_display-wkgbtr2     / l_pp_grqty.
*      it_display-wip_amt    = it_display-wip_amt     / l_pp_grqty.
*      it_display-wip_pamt   = it_display-wip_pamt    / l_pp_grqty.
*      it_display-scrap_amt  = it_display-scrap_amt   / l_pp_grqty.
*      it_display-manu_amt   = it_display-manu_amt    / l_pp_grqty.
*      it_display-gr_amt     = it_display-gr_amt      / l_pp_grqty.
*      it_display-single_amt = it_display-single_amt  / l_pp_grqty.
*      it_display-multi_amt  = it_display-multi_amt   / l_pp_grqty.
*      it_display-multi_samt = it_display-multi_samt  / l_pp_grqty.
*      it_display-multi_mamt = it_display-multi_mamt  / l_pp_grqty.
*      it_display-misc       = it_display-misc        / l_pp_grqty.
*
*
*
*      it_display-wkgbtr_f     = it_display-wkgbtr_f      / l_pp_grqty.
*      it_display-wkgbtr2_f    = it_display-wkgbtr2_f     / l_pp_grqty.
*      it_display-wkgbtr_f     = it_display-wkgbtr_f      / l_pp_grqty.
*      it_display-wkgbtr2_f    = it_display-wkgbtr2_f     / l_pp_grqty.
*      it_display-wip_amt_f    = it_display-wip_amt_f     / l_pp_grqty.
*      it_display-wip_pamt_f   = it_display-wip_pamt_f    / l_pp_grqty.
*      it_display-scrap_amt_f  = it_display-scrap_amt_f   / l_pp_grqty.
*      it_display-manu_amt_f   = it_display-manu_amt_f    / l_pp_grqty.
*      it_display-gr_amt_f     = it_display-gr_amt_f      / l_pp_grqty.
*      it_display-single_amt_f = it_display-single_amt_f / l_pp_grqty.
*      it_display-multi_amt_f  = it_display-multi_amt_f   / l_pp_grqty.
*      it_display-multi_samt_f = it_display-multi_samt_f / l_pp_grqty.
*      it_display-multi_mamt_f = it_display-multi_mamt_f / l_pp_grqty.
*      it_display-misc_f       = it_display-misc_f        / l_pp_grqty.
*
*      it_display-pp_grqty       = l_pp_grqty.
*      modify it_display. clear it_display.
*    endif.
*  endloop.
*endform.                    " recalculate_per_unit
*&---------------------------------------------------------------------*
*&      Form  get_lifnr_for_part
*&---------------------------------------------------------------------*
*       take too long db selection
*&---------------------------------------------------------------------*
form get_lifnr_for_part.
  data : l_lifnr like ekko-lifnr.
  data : l_fdate type datum,
         l_last_date like ekbe-budat.

  data : l_ok(1),
         l_cnt type i.
  ranges : r_ven_matnr for s013-matnr.
  ranges : r_ven_matnr2 for s013-matnr.
  data : l_spmon like s013-spmon.

  data : begin of it_ekbe occurs 0,
         matnr like ekbe-matnr,
         lifnr like ekko-lifnr,
         budat like ekbe-budat,
         end of it_ekbe.


  loop at it_marc where mtart cs 'ROH' .
    check it_marc-matnr <> 'R16N'.
    r_ven_matnr-sign   = 'I'.
    r_ven_matnr-option = 'EQ'.
    r_ven_matnr-low    = it_marc-matnr.
    collect r_ven_matnr. clear r_ven_matnr.
  endloop.
  read table s_poper.
*  CONCATENATE  s_poper-low+1(2) '/' p_bdatj INTO l_spmon.
  concatenate  p_bdatj s_poper-low+1(2)  p_bdatj into l_spmon.

* For getting VENDOR
* step 1) select s013 (LIS Table)
* step 2) select ekbe (PO history)

  select distinct lifnr matnr
    into table it_s013
    from s013
    for all entries in it_marc
   where spmon <= l_spmon
     and matnr = it_marc-matnr
*    AND matnr IN r_ven_matnr
   group by lifnr matnr.
*  order by spmon descending.

* Delete r_ven_matnr's matnr if it getted vendor from s013
  sort it_s013 by matnr.

*MULTIPLE VENDOR... SELECT FIRST ONE!!! FIXME LATER
  loop at r_ven_matnr.
    clear it_s013.
    loop at it_s013 where matnr = r_ven_matnr-low.
      l_ok = 'X'.
      l_cnt = l_cnt + 1.
      if l_cnt > 1.
        clear l_ok.
        exit.
      endif.
    endloop.

    if l_ok = ''.
      r_ven_matnr2 = r_ven_matnr.
      append r_ven_matnr2.
      delete it_s013 where matnr = r_ven_matnr-low.
    endif.
    clear : l_ok, l_cnt.
  endloop.

  sort r_ven_matnr2 by low.
  delete adjacent duplicates from r_ven_matnr2.

  check not r_ven_matnr2[] is initial.

*  Get vendor from ekbe
  read table s_poper index 1.
  if s_poper-high = ''.
    s_poper-high = s_poper-low.
  endif.

  concatenate p_bdatj s_poper-high+1(2) '01' into l_fdate.
  call function 'LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = l_fdate
       IMPORTING
            last_day_of_month = l_last_date
       EXCEPTIONS
            day_in_no_date    = 1
            others            = 2.

*FIXME
  ranges: r_werks for ekbe-werks.
  r_werks-option = 'EQ'.
  r_werks-sign   = 'I'.
  r_werks-low = 'E001'. append r_werks.
  r_werks-low = 'E002'. append r_werks.                     "UD1K953687
  r_werks-low = 'P001'. append r_werks.

  select distinct a~matnr b~lifnr max( a~budat )
  appending  corresponding fields of table it_ekbe
    from ekbe as a
   inner join ekko as b
      on a~ebeln = b~ebeln
   where a~werks    in r_werks
     and a~budat    <= l_last_date
     and a~bwart    = '101'
     and a~matnr    in r_ven_matnr2
     and a~bewtp    =  'E'
    group by a~matnr b~lifnr.
*  SELECT DISTINCT a~matnr b~lifnr MAX( a~budat )
*  APPENDING  CORRESPONDING FIELDS OF TABLE it_ekbe
*    FROM ekbe AS a
*   INNER JOIN ekko AS b
*      ON a~ebeln = b~ebeln
*   for all entries in r_ven_matnr2
*   WHERE a~werks    in r_werks
*     AND a~budat    <= l_last_date
*     AND a~bwart    = '101'
*     and a~matnr    = r_ven_matnr2-low
*     AND a~bewtp    =  'E'
*    GROUP by a~matnr b~lifnr.

  loop at it_ekbe.
    move-corresponding it_ekbe to it_s013.
    collect it_s013. clear it_s013.
  endloop.


endform.                    " get_lifnr_for_part
*&---------------------------------------------------------------------*
*&      Form  convert_to_material_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_DISPLAY_LLV_MATNR  text
*      <--P_IT_DISPLAY_RESOU  text
*----------------------------------------------------------------------*
form convert_to_material_group changing p_llv_matnr
                                    p_resou
                                    p_subrc.

  clear it_marc.
  read table it_marc with key matnr = it_display-llv_matnr
                              werks = it_display-bwkey
                              binary search.

  if sy-subrc = 0 .
    if p_shop = 'X'.

      CALL FUNCTION 'Z_CO_SHOP_DETERMINE'
           EXPORTING
                F_TYPPS = 'M'
                F_PRCTR = it_marc-prctr
                F_FEVOR = it_marc-fevor
                F_WERKS = it_marc-werks
                F_RAUBE = it_marc-raube
           IMPORTING
                E_SHOP  = it_display-shop.

* by ig.moon 11/19/2009 {
   if it_display-shop is initial or it_marc-prctr is initial.
     select single prctr into it_display-shop
     from marc where MATNR eq it_marc-matnr
                 and WERKS eq it_marc-werks.
   endif.
* }

*      if it_shop_sum-par_werks = 'E001'.
*        it_display-shop = 'MXEX'.
*
*      else.
*        case it_marc-fevor.
*          when 'SPB' or 'SPD' or 'SPP'.
*            it_display-shop = 'MXSX'.
*          when 'SEA' or 'SEC'.
*            it_display-shop = 'MXEX'.
*
*          when others.
*            case it_marc-raube.
*              when 10.
*                it_display-shop = 'MXSX'.
*              when 11.
*                it_display-shop = 'MXBX'.
*              when 12.
*                it_display-shop = 'MXPX'.
*              when 13.
*                it_display-shop = 'MXTX'.
*              when 14.
*                it_display-shop = 'MXEX'.
*              when others.
*                it_display-shop = space. "'MXTX'.
*            endcase.
*        endcase.
*      endif.


    endif.

*   replace materia => material group and SUM by material group
*   replace resouce => Proposed Supply Area and SUM by Supply area
    p_llv_matnr = it_marc-matkl.
    p_resou     = it_marc-vspvb.
    p_subrc = 0.

  else.
    p_subrc = 4.
  endif.

endform.                    " convert_to_material_group
*&---------------------------------------------------------------------*
*&      Form  get_material_group
*&---------------------------------------------------------------------*
form get_material_group.

  check not it_mara[] is initial.
  select a~matnr b~werks
         a~raube b~fevor
         a~mtart a~matkl b~vspvb b~prctr c~maktg
    into table it_marc
    from mara as a
    inner join marc as b
       on a~matnr = b~matnr
    inner join makt as c
       on c~matnr = a~matnr
      and c~spras = sy-langu
    for all entries  in it_mara
   where b~matnr    = it_mara-matnr
     and b~werks    = it_mara-werks.

*-get MIP information
  data: begin of lt_marc occurs 0,
          matnr like marc-matnr,
          fevor like marc-fevor,
        end of lt_marc.
  select matnr fevor into table lt_marc
     from marc
     for all entries in it_marc
     where matnr = it_marc-matnr
       and fevor <> space.
  sort lt_marc by matnr.

  data: l_idx like sy-tabix.
  loop at it_marc.
    l_idx = sy-tabix.

    read table lt_marc with key matnr = it_marc-matnr.
    if sy-subrc = 0.
      it_marc-fevor = lt_marc-fevor.  "production scheduler
    else.
      clear it_marc-fevor.
    endif.

    modify it_marc index l_idx transporting fevor.
  endloop.

endform.                    " get_material_group
*&---------------------------------------------------------------------*
*&      Form  define_mara
*&---------------------------------------------------------------------*
form define_mara.

  loop at it_shop_sum.
    check it_shop_sum-llv_matnr <> space.

    it_mara-werks = it_shop_sum-bwkey.
    it_mara-matnr = it_shop_sum-llv_matnr.
    append it_mara. clear it_mara.
  endloop.

  sort it_mara.
  delete adjacent duplicates from it_mara.

endform.                    " define_mara
*&---------------------------------------------------------------------*
*&      Form  get_lifnr_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_lifnr_name.

  select lifnr name1
    into corresponding fields of table it_lfa1
    from lfa1
     for all entries  in it_s013
   where lifnr = it_s013-lifnr .


endform.                    " get_lifnr_name
*&---------------------------------------------------------------------*
*&      Form  calculate_unit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_unit.
*  data : l_pp_grqty like  it_display-pp_grqty.         "5 digit
  data : l_pp_grqty type i.


*  CLEAR IT_CKMLMV003.
*  if p_vsum = 'X'.
*    read table it_ckmlmv003 with key bwkey    = it_display-par_werks
*                                     matnr    = it_display-artnr.
*  else.
*    read table it_ckmlmv003 with key bwkey    = it_display-par_werks
*                                     matnr    = it_display-artnr
*                                     verid_nd = it_display-verid.
*  endif.
*  clear : l_pp_grqty.

  l_pp_grqty =  it_display-pp_grqty / 1000.

*FIXME
  if l_pp_grqty = 0.
    l_pp_grqty = 1.
    break-point.
  endif.

*decimal 4 amount, decimal 5 qty
  if l_pp_grqty <>  0 .
    it_display-wkgbtr     = it_display-wkgbtr      / l_pp_grqty.
    it_display-wkgbtr2    = it_display-wkgbtr2     / l_pp_grqty.
    it_display-add_wkgbtr = it_display-add_wkgbtr  / l_pp_grqty.
    it_display-wip_amt    = it_display-wip_amt     / l_pp_grqty.
    it_display-wip_pamt   = it_display-wip_pamt    / l_pp_grqty.
    it_display-scrap_amt  = it_display-scrap_amt   / l_pp_grqty.
    it_display-manu_amt   = it_display-manu_amt    / l_pp_grqty.
    it_display-gr_amt     = it_display-gr_amt      / l_pp_grqty.
    it_display-single_amt = it_display-single_amt  / l_pp_grqty.
    it_display-multi_amt  = it_display-multi_amt   / l_pp_grqty.
    it_display-multi_samt = it_display-multi_samt  / l_pp_grqty.
    it_display-multi_mamt = it_display-multi_mamt  / l_pp_grqty.


    it_display-manu_qty     = it_display-manu_qty      / l_pp_grqty.
    it_display-gr_qty       = it_display-gr_qty        / l_pp_grqty.
    it_display-single_qty   = it_display-single_qty    / l_pp_grqty.

  endif.

endform.                    " calculate_unit
*&---------------------------------------------------------------------*
*&      Form  pp_grqty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_pp_grqty changing p_pp_grqty type ckml_outmenge.
  clear it_ckmlmv003.
  if p_vsum = 'X'.
    read table it_ckmlmv003 with key matnr    = it_display-artnr.
  else.
    read table it_ckmlmv003 with key matnr    = it_display-artnr
                                     verid_nd = it_display-verid.
  endif.

  check it_ckmlmv003-out_menge > 0.
  clear : p_pp_grqty.
*  p_pp_grqty = it_ckmlmv003-out_menge .
*
*  p_pp_grqty = it_ckmlmv003-out_menge * 100.
*
  call function 'UNIT_CONVERSION_SIMPLE'
       EXPORTING
            input                = it_ckmlmv003-out_menge
            unit_in              = it_ckmlmv003-meinh
            unit_out             = it_ckmlmv003-meinh
       IMPORTING
            output               = p_pp_grqty
       EXCEPTIONS
            conversion_not_found = 1
            division_by_zero     = 2
            input_invalid        = 3
            output_invalid       = 4
            overflow             = 5
            type_invalid         = 6
            units_missing        = 7
            unit_in_not_found    = 8
            unit_out_not_found   = 9.

  if p_pp_grqty = 0.
    break-point.
  endif.
endform.                    " pp_grqty
*&---------------------------------------------------------------------*
*&      Form  determine_vendor
*&---------------------------------------------------------------------*
form determine_vendor.

  clear it_s013.
  read table it_s013 with key matnr = it_display-llv_matnr
                     binary search.
  if sy-subrc = 0.
    it_display-lifnr = it_s013-lifnr.
    clear it_lfa1.
    read table it_lfa1 with key lifnr = it_display-lifnr
                       binary search.
    it_display-name1 = it_lfa1-name1.
  endif.

endform.                    " determine_vendor
*&---------------------------------------------------------------------*
*&      Form  fill_shop_sum_info
*&---------------------------------------------------------------------*
form fill_shop_sum_info.
  data : l_llv_matnr like it_shop_sum-llv_matnr,
         l_resou     like it_shop_cc-resou,
         l_subrc     like sy-subrc.

  clear it_shop_sum.
*  read table it_shop_sum with key kokrs =  it_shop_cc-kokrs
*                                  bdatj =  it_shop_cc-bdatj
*                                  poper =  it_shop_cc-poper
*                                  aufnr =  it_shop_cc-aufnr
*                                  resou =  it_shop_cc-resou
*                                  kstar =  it_shop_cc-kstar
*                              binary search.
*  if sy-subrc = 0.
  it_display-par_werks  = it_shop_sum-par_werks.
  it_display-aufnr      = it_shop_sum-aufnr.
  it_display-verid      = it_shop_sum-verid.

  it_display-shop       = it_shop_sum-shop.
  it_display-bwkey      = it_shop_sum-bwkey.
  it_display-llv_matnr  = it_shop_sum-llv_matnr.
  it_display-kostl      = it_shop_sum-kostl.
  it_display-lstar      = it_shop_sum-lstar.
  it_display-meeht      = it_shop_sum-meeht.
*    it_display-PEINH      = it_shop_sum-peinh.
*    it_display-STPRS      = it_shop_sum-STPRS.
*    it_display-VERPR      = it_shop_sum-VERPR.

  it_display-gr_qty     = it_shop_sum-gr_qty.
  it_display-single_qty = it_shop_sum-single_qty.
  it_display-manu_qty   = it_shop_sum-manu_qty.

  if it_display-llv_matnr <> space.
*   display vendor of parts
    if p_lifnr = 'X'.
      perform determine_vendor.
    endif.

    clear : l_llv_matnr, l_resou, l_subrc.
    perform convert_to_material_group changing l_llv_matnr
                                               l_resou
                                               l_subrc .

    if p_matgrp = 'X' and l_subrc = 0.
      it_display-llv_matnr = l_llv_matnr.
      it_display-resou     = l_resou.
      clear: it_display-peinh, it_display-stprs, it_display-verpr.
    else.
      it_display-maktg = it_marc-maktg.
    endif.

  elseif it_shop_cc-typps = 'E'.

  endif.
*  endif.

endform.                    " fill_shop_sum_info
*&---------------------------------------------------------------------*
*&      Form  get_lifnr_for_part_new
*&---------------------------------------------------------------------*
form get_lifnr_for_part_new.

  data: w_datum_f   like   sy-datum,
        w_datum_t   like   sy-datum.
  constants: c_ekorg like ekko-ekorg  value 'PU01'.

  call function 'LAST_DAY_IN_PERIOD_GET'
       EXPORTING
            i_periv        = tka01-lmona
            i_gjahr        = p_bdatj
            i_poper        = s_poper-low
       IMPORTING
            e_date         = w_datum_t
       EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            others         = 4.

  select lifnr matnr
    into corresponding fields of table it_s013
    from a018 as a
     for all entries in it_marc
   where kappl =  'M'
     and kschl =  'PB00'     "ZTIR = PB00
     and lifnr in s_lifnr
     and matnr = it_marc-matnr
     and ekorg =  c_ekorg
     and esokz =  '0'
     and datbi >= w_datum_t
     and datab <= w_datum_t.

endform.                    " get_lifnr_for_part_new
*&---------------------------------------------------------------------*
*&      Form  read_tka01
*&---------------------------------------------------------------------*
form read_tka01.

  clear tka01.
  select single * from tka01
                 where kokrs = p_kokrs.
  if sy-subrc <> 0.
    message e038 with p_kokrs.
  endif.

endform.                    " read_tka01
*&---------------------------------------------------------------------*
*&      Form  fill_basic_info
*&---------------------------------------------------------------------*
*form fill_basic_info.
*    clear it_ckmlmv003.
*    read table it_ckmlmv003 with key aufnr = it_display-aufnr
*               binary search.
*
*    it_display-par_werks  = it_ckmlmv003-bwkey.
*    it_display-aufnr      = it_ckmlmv003-aufnr.
*    it_display-verid      = it_ckmlmv003-verid_nd.
*
*endform.                    " fill_basic_info
