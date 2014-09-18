*&--------------------------------------------------------------------
*& REPORT                 : ZACO92A_STMH
*& Creation Date          : 01/12/2005
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            : Standard M/H each MIP Material FSC
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
report zaco92a_stmh message-id  zmco.

tables: ztco_shop_sum.

RANGES R_BWKEY FOR T001K-BWKEY.
DATA : BEGIN OF IT_PRODQTY OCCURS 0,
         BWKEY      LIKE CKMLMV001-BWKEY,
         MATNR      LIKE CKMLMV001-MATNR,
         VERID_ND   LIKE CKMLMV001-VERID_ND,
         MEINH      LIKE CKMLMV003-MEINH,
         OUT_MENGE  LIKE CKMLMV003-OUT_MENGE,
         PERIO      LIKE CKMLMV003-PERIO,
       END OF  IT_PRODQTY.

data : begin of it_shop_sum occurs 0,
*         kokrs        like ztco_shop_sum-kokrs,
*         bdatj        like ztco_shop_sum-bdatj,
*         aufnr        LIKE ztco_SHOP_SUM-aufnr,
         kalst        like ztco_shop_sum-kalst,   "self level
         kalst_c      like ztco_shop_sum-kalst,   "component level

         artnr        like ztco_shop_sum-artnr,
         shop         like ztco_shop_sum-shop,
         typps        like ztco_shop_sum-typps,
         bwkey        like ztco_shop_sum-bwkey,
         par_werks    like ztco_shop_sum-par_werks,
         llv_matnr    like ztco_shop_sum-llv_matnr,
*         kstar        like ztco_shop_sum-kstar,
         kostl        like ztco_shop_sum-kostl,
         lstar        like ztco_shop_sum-lstar,
         meeht        like ztco_shop_sum-meeht,

         mbgbtr       like ztco_shop_sum-mbgbtr,
         mbgbtr2      like ztco_shop_sum-mbgbtr2,
         add_mbgbtr   like ztco_shop_sum-add_mbgbtr,
         wip_qty      like ztco_shop_sum-wip_qty,
         wip_pqty     like ztco_shop_sum-wip_pqty,
         scrap_qty    like ztco_shop_sum-scrap_qty,
         manu_qty     like ztco_shop_sum-manu_qty,
         gr_qty       like ztco_shop_sum-gr_qty,
*        poper        like ztco_shop_sum-poper,
       end of it_shop_sum.

* Acutal
data: it_shop_act like it_shop_sum occurs 0 with header line.

*data : begin of it_shop_act occurs 0,
*         kokrs        like ztco_shop_sum-kokrs,
*         bdatj        like ztco_shop_sum-bdatj,
*         aufnr        like ztco_shop_sum-aufnr,
*         artnr        like ztco_shop_sum-artnr,
*         shop         like ztco_shop_sum-shop,
*         typps        like ztco_shop_sum-typps,
*         bwkey        like ztco_shop_sum-bwkey,
*         par_werks    like ztco_shop_sum-par_werks,
*         llv_matnr    like ztco_shop_sum-llv_matnr,
*         kstar        like ztco_shop_sum-kstar,
*         kostl        like ztco_shop_sum-kostl,
*         lstar        like ztco_shop_sum-lstar,
*         kalst        like ztco_shop_sum-kalst,
*         mbgbtr       like ztco_shop_sum-mbgbtr,
*         mbgbtr2      like ztco_shop_sum-mbgbtr2,
*         add_mbgbtr   like ztco_shop_sum-add_mbgbtr,
*         wip_qty      like ztco_shop_sum-wip_qty,
*         wip_pqty     like ztco_shop_sum-wip_pqty,
*         scrap_qty    like ztco_shop_sum-scrap_qty,
*         manu_qty     like ztco_shop_sum-manu_qty,
*         gr_qty       like ztco_shop_sum-gr_qty,
*         poper        like ztco_shop_sum-poper,
*       end of it_shop_act.

data :begin of it_shop_pp occurs 0,
         shop         like ztco_shop_sum-shop,
         kostl        like ztco_shop_sum-kostl,
         mbgbtr       like ztco_shop_sum-mbgbtr,
         mbgbtr2      like ztco_shop_sum-mbgbtr2,
         add_mbgbtr   like ztco_shop_sum-add_mbgbtr,
         wip_qty      like ztco_shop_sum-wip_qty,
         wip_pqty     like ztco_shop_sum-wip_pqty,
         menge        like ztco_shop_pln-menge,
      end of it_shop_pp.

data :begin of it_shop_pp_pln occurs 0,
         shop         like ztco_shop_sum-shop,
         kostl        like ztco_shop_sum-kostl,
         artnr        like ztco_shop_sum-artnr,
         mbgbtr       like ztco_shop_sum-mbgbtr,
         mbgbtr2      like ztco_shop_sum-mbgbtr2,
         add_mbgbtr   like ztco_shop_sum-add_mbgbtr,
         wip_qty      like ztco_shop_sum-wip_qty,
         wip_pqty     like ztco_shop_sum-wip_pqty,
         menge        like ztco_shop_pln-menge,
      end of it_shop_pp_pln.


data :begin of it_mara occurs 0,
         matnr like mara-matnr,
         mtart like mara-mtart,
         kalst like ckmlmv011-kalst, "costing level
      end of it_mara.

data :begin of it_marc occurs 0,
         matnr like marc-matnr,
         werks like marc-werks,
         vspvb like marc-vspvb,
      end of it_marc.

data :begin of it_plpo occurs 0,
         usr00 like plpo-usr00,
         usr01 like plpo-usr01,
      end of it_plpo.

data :begin of it_gr_wip occurs 0,
*         gubun    like ztco_wip-gubun,
         plant    like ztco_wip-plant,
         matnr    like ztco_wip-matnr,
*         yymm     like ztco_wip-yymm,
         rppoint  like ztco_wip-rppoint,
         workct   like ztco_wip-workct,
         inqty    like ztco_wip-inqty,
         outqty   like ztco_wip-outqty,
         bwqty    like ztco_wip-bwqty,
         ewqty    like ztco_wip-ewqty,
      end of it_gr_wip.



data : begin of it_csks  occurs 0,
         abtei like csks-abtei,
         kostl like csks-kostl,
       end of it_csks.

data : begin of it_shop_hr  occurs 0,
         shop  like ztco_shop_sum-shop,
         kostl like    ztco_mha-kostl,

         anzhl like    ztco_mha-anzhl,     "
         menge like    ztco_mha-anzhl,
         shop_rate     type p decimals 6,
         total_rate    type p decimals 6,
         os_rate       type p decimals 6,
         total_os_rate type p decimals 6,
         semi_dir      type p decimals 4,
         semi_dir1     type p decimals 4,
         semi_dir2     type p decimals 4,
         pc            type p decimals 4,
         pc1           type p decimals 4,
         pc2           type p decimals 4,
         qc            type p decimals 4,
         qc1           type p decimals 4,
         qc2           type p decimals 4,
         pm            type p decimals 4,
         pm1           type p decimals 4,
         pm2           type p decimals 4,
         in_dir        type p decimals 4,
         admin         type p decimals 4,
         os_dir        type p decimals 4,
         os_semi       type p decimals 4,
         os_semi1      type p decimals 4,
         os_semi2      type p decimals 4,
         os_pc         type p decimals 4,
         os_pc1        type p decimals 4,
         os_pc2        type p decimals 4,
         os_qc         type p decimals 4,
         os_qc1        type p decimals 4,
         os_qc2        type p decimals 4,
         os_pm         type p decimals 4,
         os_pm1        type p decimals 4,
         os_pm2        type p decimals 4,
         os_in         type p decimals 4,
         os_admin      type p decimals 4,
       end of it_shop_hr.

data : begin of it_shop2  occurs 0,
         shop  like ztco_shop_sum-shop,
         anzhl like ztco_mha-anzhl,
         menge like ztco_mha-anzhl,
       end of it_shop2.

data : g_total_anzhl type p decimals 4.

data : begin of it_semi  occurs 0,
         shop  like ztco_shop_sum-shop,
         anzhl like ztco_mha-anzhl,
         menge like ztco_mha-anzhl,
       end of it_semi.
data : it_pc like it_semi occurs 0 with header line,
       it_qc like it_semi occurs 0 with header line,
       it_pm like it_semi occurs 0 with header line.

data : begin of it_in  occurs 0,
         anzhl like ztco_mha-anzhl,
         menge like ztco_mha-anzhl,
       end of it_in.

data : begin of it_admin occurs 0,
         anzhl like ztco_mha-anzhl,
         menge like ztco_mha-anzhl,
       end of it_admin.

data : it_mhv       like ztco_mhv  occurs 0 with header line.
data : it_mhv2      like it_mhv    occurs 0 with header line.
data : it_mhv_base  like ztco_mhv  occurs 0 with header line.
data : it_ztco_mhv  like ztco_mhv  occurs 0 with header line.
data : it_shop_pln  like ztco_shop_pln occurs 0 with header line.

data : begin of it_tka03_temp occurs 0,
         kostl like ztco_mhv-kostl,
         stagr like tka03-stagr,
         bezei like tkt03-bezei,
       end of it_tka03_temp.

data : begin of it_tka03 occurs 0,
         type(2),
         kostl like ztco_mhv-kostl,
         stagr like tka03-stagr,
       end of it_tka03.

data : it_cosr_skf like cosr occurs 0 with header line.

data : begin of it_skf occurs 0,
         type(2),
         kostl like ztco_mhv-kostl,
         stagr like tka03-stagr,
         sme000 like cosr-sme001,
       end of it_skf.


data : it_mhv_pln       like ztco_mhv_pln  occurs 0 with header line.
data : it_mhv_pln2      like it_mhv_pln    occurs 0 with header line.
data : it_mhv_pln_base  like ztco_mhv_pln  occurs 0 with header line.
data : it_ztco_mhv_pln  like ztco_mhv_pln  occurs 0 with header line.


ranges: r_semi    for  csks-kostl,
        r_in      for  csks-kostl,
        r_admin   for  csks-kostl,
        r_pc_cc   for  csks-kostl,
        r_qc_cc   for  csks-kostl,
        r_pm_cc   for  csks-kostl,
        r_pc      for  csks-kostl,
        r_qc      for  csks-kostl,
        r_pm      for  csks-kostl.

data : g_ktopl like t001-ktopl.

data  :begin of it_ccgrp occurs 0,
        group     like sethier-shortname,
        low       like setvalues-from,
        high      like setvalues-to,
       end of it_ccgrp.

data  :begin of it_mha_hr occurs 0,
        kostl    like ztco_mha-kostl,
        chnge    like ztco_mha-chnge,
        anzhl    like ztco_mha-anzhl,  "Hr man hour
        netmh    like ztco_mha-netmh,  "Hr net hour
        menge    like ztco_mha-netmh,  "OS man hour
        objnr    like cosr-objnr,
       end of it_mha_hr.

data  :begin of it_dist occurs 0,
        kostl    like ztco_mha-kostl,
        persn    type RECNPERCENT,
       end of it_dist.

*"Out sourcing
data  :begin of it_mha_os occurs 0,
        kostl    type kostl,
        menge    like ztco_mha-anzhl,
       end of it_mha_os.

data  :begin of it_mha_abs occurs 0,
        kostl    like ztco_mha-kostl,
        chnge    like ztco_mha-chnge,
        anzhl    like ztco_mha-anzhl,
        netmh    like ztco_mha-netmh,
        objnr    like cosr-objnr,
        menge    like ztco_mha-netmh,       "Out sourcing
       end of it_mha_abs.

data : it_cosr_exc like cosr occurs 0 with header line.
data : it_cosr_os  like cosr occurs 0 with header line.



data  :begin of it_mha occurs 0,
        kostl        like ztco_mha-kostl,
        hr_mh        like ztco_mha-anzhl,
        menge        like ztco_mha-anzhl,
        del_mh       like ztco_mha-anzhl,
        del_rat      type p decimals 6,
        netmh        like ztco_mha-netmh,
        net_rat      type p decimals 6,
        semi_dir     like ztco_mha-netmh,
        semi_rat     type p decimals 6,
        pc           like ztco_mha-netmh,
        pc_rat       type p decimals 6,
        qc           like ztco_mha-netmh,
        qc_rat       type p decimals 6,
        pm           like ztco_mha-netmh,
        pm_rat       type p decimals 6,
        in_dir       like ztco_mha-netmh,
        in_rat       type p decimals 6,
        admin        like ztco_mha-netmh,
        admin_rat    type p decimals 6,
        os_dir       like ztco_mha-netmh,
        os_dir_rat   type p decimals 6,
        os_semi      like ztco_mha-netmh,
        os_semi_rat  type p decimals 6,
        os_pc        like ztco_mha-netmh,
        os_pc_rat    type p decimals 6,
        os_qc        like ztco_mha-netmh,
        os_qc_rat    type p decimals 6,
        os_pm        like ztco_mha-netmh,
        os_pm_rat    type p decimals 6,
        os_in        like ztco_mha-netmh,
        os_in_rat    type p decimals 6,
        os_admin     like ztco_mha-netmh,
        os_admin_rat type p decimals 6,
       end of it_mha.

*data : begin of it_plaf occurs 0,
*       perio(6),
*       matnr like mara-matnr,
*       gsmng like plaf-gsmng,
*       end of it_plaf.
data : it_plaf like ztco_shop_plh occurs 0 with header line.


data : it_plaf_temp like plaf occurs 0 with header line.

data : it_alt like ztco_mhv_alt occurs 0 with header line.

field-symbols : <f_field>.

*Data definition
data : w_int type i.


constants: c_cs010 like cosr-stagr value 'CS010',  "MH
           c_cs011 like cosr-stagr value 'CS011',  "MH exc.%
           c_cs012 like cosr-stagr value 'CS012'.  "Temp MH

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
* General Info.
parameters : p_kokrs like csks-kokrs   memory id cac  obligatory
               default 'H201'.
* Posted Yr.
selection-screen begin of line.
selection-screen comment  1(30) text-021. "From
selection-screen position 33.
parameters : p_bdatj like keko-bdatj memory id bdtj obligatory.
selection-screen position 38.
parameters: p_poper like covja-perab memory id vpe
            modif id per obligatory ."DEFAULT '10'.
selection-screen end of line.

selection-screen skip 1.
parameters: p_ksmip like coep-kstar obligatory  default '540300'.
parameters: p_ksmh  like coep-kstar obligatory  default '836001'.
parameters : p_cc_grp(2) default 'MH' .
parameters : p_leave as checkbox default 'X'.
parameters : p_rp    as checkbox default 'X'.
parameters : p_reft  as checkbox default 'X'.   "reload temporary MH
* Costing Type
selection-screen begin of block bl2 with frame title text-002.
parameters : p_bpl             radiobutton group ra01
             user-command  cty.
parameters : p_std  radiobutton group ra01.
parameters : p_act default 'X' radiobutton group ra01.
parameters : p_plnver like cosl-versn obligatory default '311'.
parameters : p_klvar like cki64a-klvar memory id krt no-display.
selection-screen end of block bl2.

parameters : p_test as checkbox default 'X'.
selection-screen end of block bl1.

*selection-screen begin of block bl4 with frame title text-005.
**            p_plscn like plaf-plscn default '901' obligatory.
*selection-screen end of block bl4.


select-options: s_KALST for ztco_shop_sum-KALST.
select-options: s_kostl for ztco_shop_sum-kostl.
select-options: s_artnr for ztco_shop_sum-artnr.
select-options: s_llvmt for ztco_shop_sum-llv_matnr.


initialization.
*  s_kstar-option = 'EQ'.
*  s_kstar-sign   = 'I'.
*  s_kstar-low    = '0000836001'.
*  append s_kstar.
*  s_kstar-low    = '0000540300'.
*  append s_kstar.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.
  perform get_global_variables.

*Actual
  if p_act = 'X'.
    perform read_alt_material.
    perform read_ztco_shop_sum.
    if p_test = 'X'.
      perform display_result_act.
    else.
      perform update_ztco_mhv.
    endif.
  elseif p_std eq 'X'.
    write:/ '***Obsoleted function'.
    exit.
    p_klvar = 'PPC1'.
    perform read_ztco_shop_pln_standard.
    perform update_ztco_mhv_pln.

*Annual Business
  else.
*Read :ZTCO_SHOPCOST
    p_klvar = 'ZPCP'.
    perform read_ztco_shopcost_business.
    if p_test = 'X'.
      perform display_result_pln.
    else.
      perform update_ztco_mhv_pln.
    endif.
  endif.

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  read_ztco_shopcost_business
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztco_shopcost_business.

  perform progress_indicator using  '10' ' '
                   'Read Business data'.

  perform gathering_data_shop_pln using p_klvar.

  perform progress_indicator using  '40' ' '
                   'Read Business data'.
  perform gathering_data_cosl.

  perform progress_indicator using  '60' ' '
                   'Calculate Business data'.

  perform calculate_mh_pln.
  perform append_ztco_mhv.

endform.                    " read_ztco_shopcost_business
*&---------------------------------------------------------------------*
*&      Form  read_ztco_SHOP_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztco_shop_sum.

  perform progress_indicator using  '10' ' '
                   'Read Actual data'.

*Gathering data from ztco_SHOP_SUM

  perform read_production.
  perform read_wip_data.
  perform read_shop_cost.
  perform read_master_info.
  perform adjust_costing_level.
  perform collect_shop_cc.

  perform progress_indicator using  '40' ' '
                   'Read Actual data'.

  perform gathering_data_hma.

  perform progress_indicator using  '60' ' '
                   'Calculate data'.

  perform calculate_mh.

  perform append_ztco_mhv_actual.

endform.                    " read_ztco_SHOP_SUM
*&---------------------------------------------------------------------*
*&      Form  gathering_data
*&---------------------------------------------------------------------*
form read_shop_cost.

  clear : w_int.
  refresh it_shop_sum.

  select kokrs bdatj aufnr artnr shop llv_matnr kstar kalst
         kostl lstar typps bwkey par_werks meeht
         mbgbtr mbgbtr2 add_mbgbtr wip_qty wip_pqty
         manu_qty gr_qty poper
         into corresponding fields of table it_shop_act
      from ztco_shop_sum
           for all entries in IT_PRODQTY
           where kokrs eq p_kokrs
             and bdatj eq p_bdatj
             and poper eq p_poper
             and ( ( typps = 'E' and lstar = 'MAN_HR'
                                 and kostl in s_kostl )
                  or typps = 'M' and fevor <> space
                                 and llv_matnr in s_llvmt )
*                     typps = 'M' and kstar eq p_ksmip )

             and kalst in s_kalst
             and artnr = IT_PRODQTY-matnr
             and artnr in s_artnr.

  loop at it_shop_act.
*---exclude self-consumption
    if  it_shop_act-typps = 'M'
    and it_shop_act-artnr = it_shop_act-llv_matnr .
      continue.
    endif.

* by ig.moon 2/19/2010 {
    if it_shop_act-lstar = 'MAN_HR'.
      if it_shop_act-shop is initial.
        it_shop_act-shop = it_shop_act-kostl(4).
      endif.

    else.
      if it_shop_act-typps eq 'M'.
        if it_shop_act-shop is initial.
          it_shop_act-shop = 'MXTX'.
        endif.
      endif.
    endif.
* }

    move-corresponding it_shop_act to it_shop_sum.
    collect it_shop_sum. clear it_shop_sum.
  endloop.


endform.                    " gathering_data

*&---------------------------------------------------------------------*
*&      Form  progress_indicator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0079   text
*----------------------------------------------------------------------*
form progress_indicator  using   p_%  p_wint p_text.

  data : l_text(40).
  concatenate p_text  p_% '%' into l_text.

  call function 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = p_%
            text       = l_text.

endform.                    " progress_indicator
*&---------------------------------------------------------------------*
*&      Form  gathering_data_hma
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form gathering_data_hma.
* Cost center Group.
  perform get_all_cc_group.

  perform get_it_mha.
  sort it_mha_hr by kostl.

  perform get_shop_from_csks.

* Select data from cosr : i) for exclude rate : CS011
*                         2) Out source data  : CS012
*                         3) PC, QC, PM

  perform select_outsource using '04'
                                  '000'.

  perform select_cosr_exclud_rate using '04'
                                        '000'.

  perform select_pc_qc_pm  using '04'.

* calculate excluding rate
  perform calculate_excluding.

* collect shop MH
  perform collect_hr_mh.

  perform make_it_shop_value using 'A'.


  perform calculate_others_value.


  perform get_value_rate using 'A'.


endform.                    " gathering_data_hma
*&---------------------------------------------------------------------*
*&      Form  GET_CC_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_cc_group  tables p_cc structure r_semi
                   using p_type.
  data: t_setlist like setlist occurs 0 with header line.
  data: t_sethier like sethier occurs 0 with header line.
  data: t_setvalues like setvalues occurs 0 with header line.

  data : l_ksgru(30).
  clear : t_setlist.

  concatenate p_cc_grp '_'  p_type into l_ksgru.
  check not l_ksgru is initial.

  call function 'G_SET_LIST_SELECT'
       EXPORTING
            setclass      = '0101'
            shortname     = l_ksgru
            kokrs         = p_kokrs
            ktopl         = g_ktopl
       TABLES
            matching_sets = t_setlist.
  if t_setlist[] is initial.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  else.
    read table t_setlist index 1.
  endif.

  call function 'G_SET_TREE_IMPORT'
       EXPORTING
            setid                     = t_setlist-setname
       TABLES
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       EXCEPTIONS
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            others                    = 4.

  if sy-subrc <> 0.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  endif.
* TRANSFER THE VALUE TO CC RANGE.
  p_cc-sign = 'I'.
  p_cc-option = 'BT'.
  loop at t_setvalues.
    p_cc-low  = t_setvalues-from.
    p_cc-high = t_setvalues-to.
    append p_cc.
  endloop.
  clear p_cc.


endform.                    " GET_CC_GROUP
*&---------------------------------------------------------------------*
*&      Form  calculate_by_level
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_mh.

  perform calculate_from_shop.

  perform calculate_by_level.



endform.                    " calculate_by_level
*&---------------------------------------------------------------------*
*&      Form  calculate_from_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_from_shop.

*DELETE IT_SHOP_SUM WHERE KOSTL <> 'MXPX10'.
*DELETE IT_GR_WIP WHERE WORKCT <> 'MXPX10'.
*DELETE IT_GR_WIP WHERE YYMM <> '200601'.
*delete it_gr_wip where matnr <> '4B28AAEMFGT6B 0060'.

* 1. Component = '-'.  (No exist)
  loop at it_shop_sum.
    check it_shop_sum-typps = 'E' .

    move-corresponding it_shop_sum to it_mhv.

    perform get_gr_qty    using it_shop_sum-artnr
                                ''
                                it_shop_sum-par_werks
                                it_shop_sum-kostl
                       changing it_mhv-gr_qty
                                it_mhv-pp_gr.

    perform move_shop_to_mhv tables it_mhv
                             using 'P'.

    perform move_mha_to_mhv  tables it_mhv
                             using  it_shop_sum-kostl.

    append it_mhv. clear it_mhv.
  endloop.


endform.                    " calculate_from_shop
*&---------------------------------------------------------------------*
*&      Form  calculate_by_level
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_by_level.
**ANDY - exclude self-consumption
*    check it_shop_sum-artnr <> it_shop_sum-llv_matnr .
**ANDY - exclude same/lower level
*      check it_mhv_base-kalst < it_shop_sum-kalst.

  data: begin of lt_gr occurs 0,
          artnr     like it_shop_sum-artnr,
          kostl     like it_shop_sum-kostl,
          llv_matnr like it_shop_sum-llv_matnr,
          gr_qty    like it_mhv-gr_qty,
          pp_gr   like it_mhv-pp_gr,
        end of lt_gr.


  data: l_kalst type CK_KALST.
  data: lt_mhv_tmp like it_mhv occurs 0 with header line.
  data: l_matnr like it_shop_sum-llv_matnr.

  perform make_mhv_base tables it_mhv it_mhv_base.


  sort it_shop_sum by  typps descending
                       kalst artnr llv_matnr.

* 2. Step 2 - level by level... logic missing.. - fix it~~~

  loop at it_shop_sum where typps = 'M' .

*---check substitue product for roll-up
    l_matnr = it_shop_sum-llv_matnr.
    read table it_mhv_base with key artnr = l_matnr binary search .
    if sy-subrc <> 0.
      read table it_alt with key artnr = l_matnr .
      if sy-subrc = 0.
        l_matnr = it_alt-altmt.
      endif.
    endif.

    loop at it_mhv_base where artnr = l_matnr .

      move-corresponding it_shop_sum to it_mhv.
*      if l_matnr = 'AU52'. break-point. endif.
      perform get_gr_qty    using it_shop_sum-artnr
                                  it_mhv_base-artnr
                                  it_shop_sum-par_werks
                                  it_mhv_base-kostl
                         changing it_mhv-gr_qty
                                  it_mhv-pp_gr.
      perform move_shop_to_mhv tables it_mhv
                               using 'C'.

      perform move_mha_to_mhv tables it_mhv
                              using  it_mhv_base-kostl.


*     it_mhv-sub_matnr  = it_mhv_base-artnr.
      it_mhv-shop       = it_mhv_base-shop .
      it_mhv-kostl      = it_mhv_base-kostl.

      move-corresponding it_mhv to lt_gr. append lt_gr.
      collect it_mhv.

      it_mhv_base = it_mhv.
      clear it_mhv_base-llv_matnr.
      collect it_mhv_base.

      clear: it_mhv, it_mhv_base.
    endloop.

*-- if level is complete, then start next level
*    perform make_mhv_base tables it_mhv it_mhv_base.

  endloop.

* GR qty - no collect... read again and put GR qty back
  sort lt_gr by artnr kostl llv_matnr.
  DELETE ADJACENT DUPLICATES FROM lt_gr comparing artnr kostl llv_matnr.

  sort it_mhv by artnr kostl llv_matnr.
  loop at lt_gr.
    it_mhv-gr_qty  = lt_gr-gr_qty.
    it_mhv-pp_gr = lt_gr-pp_gr.

    modify it_mhv from it_mhv transporting gr_qty pp_gr
       where artnr     = lt_gr-artnr
         and kostl     = lt_gr-kostl
         and llv_matnr = lt_gr-llv_matnr.
  endloop.

* 3. Step 3 --> incorrect rollup logic... (3 level only...)
*  clear : it_mhv_base, it_mhv_base[].
*  it_mhv_base[] = it_mhv[].
*  sort it_mhv by kalst.
*  sort it_mhv_base by artnr.
*  loop at it_mhv where llv_matnr <> ''.
*
*    loop at it_mhv_base where artnr     = it_mhv-llv_matnr
*                          and llv_matnr <> '' .
*
*
*      move-corresponding it_mhv to it_mhv2.
*
*      perform get_gr_qty    using it_mhv-artnr
*                                  it_mhv_base-artnr
*                                  it_shop_sum-par_werks
*                                  it_mhv_base-kostl
*                         changing it_mhv2-gr_qty
*                                  it_mhv2-pp_gr.
*
*
*
*      perform move_mhv_to_mhv2 tables it_mhv2
*                                using 'C'.
*
*      perform move_mha_to_mhv tables it_mhv2
*                              using  it_mhv2-kostl.
*
*
*      clear it_shop_sum.
*      read table it_shop_sum with key artnr = it_mhv-artnr.
*
*      it_mhv2-sub_matnr = it_mhv_base-artnr.
*
**FIXME - ANDY
**      it_mhv2-llv_matnr = it_mhv_base-llv_matnr.
*
*      it_mhv2-shop      = it_mhv_base-shop .
*      it_mhv2-kostl     = it_mhv_base-kostl.
*      collect it_mhv2.
*      move-corresponding it_mhv2 to it_mhv_base.
*      collect it_mhv_base.
*      clear : it_mhv2, it_mhv_base.
*    endloop.
*  endloop.


endform.                    " calculate_by_level
*&---------------------------------------------------------------------*
*&      Form  update_Ztco_mhv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ztco_mhv.
  perform progress_indicator using  '90' ' '
                   'Insert data'.


  delete from ztco_mhv where kokrs = p_kokrs
                         and bdatj = p_bdatj
                         and poper = p_poper.


  insert ztco_mhv from table it_ztco_mhv.
  if sy-subrc = 0 .
    commit work.
    message s009.
  endif.

endform.                    " update_Ztco_mhv
*&---------------------------------------------------------------------*
*&      Form  append_ztco_mhv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_ztco_mhv.

  delete it_mhv_pln where artnr = ''.

  sort it_mhv_pln by kalst artnr .
  loop at it_mhv_pln .
    move-corresponding it_mhv_pln to it_ztco_mhv_pln.
* LOG
    it_ztco_mhv_pln-erdat = sy-datum.
    it_ztco_mhv_pln-erzet = sy-uzeit.
    it_ztco_mhv_pln-ernam = sy-uname.
    it_ztco_mhv_pln-kokrs = p_kokrs.
    it_ztco_mhv_pln-bdatj = p_bdatj.
    it_ztco_mhv_pln-poper = p_poper.
    if it_ztco_mhv_pln-gr_qty = 0. it_ztco_mhv_pln-gr_qty = 1. endif.
    append it_ztco_mhv_pln. clear it_ztco_mhv_pln.
  endloop.

  sort it_mhv_pln2 by kalst artnr .
  loop at it_mhv_pln2 .
    move-corresponding it_mhv_pln2 to it_ztco_mhv_pln.
* LOG
    it_ztco_mhv_pln-erdat = sy-datum.
    it_ztco_mhv_pln-erzet = sy-uzeit.
    it_ztco_mhv_pln-ernam = sy-uname.
    it_ztco_mhv_pln-kokrs = p_kokrs.
    it_ztco_mhv_pln-bdatj = p_bdatj.
    it_ztco_mhv_pln-poper = p_poper.
    if it_ztco_mhv_pln-gr_qty = 0. it_ztco_mhv_pln-gr_qty = 1. endif.
    append it_ztco_mhv_pln. clear it_ztco_mhv_pln.
  endloop.


  sort it_ztco_mhv_pln.

endform.                    " append_ztco_mhv
*&---------------------------------------------------------------------*
*&      Form  move_shop_to_mhv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3247   text
*----------------------------------------------------------------------*
form move_shop_to_mhv tables p_mhv  structure it_mhv
                      using  p_type.


  p_mhv-curr_qty   = it_shop_sum-mbgbtr.
  p_mhv-add_qty    = it_shop_sum-add_mbgbtr + it_shop_sum-mbgbtr2.
  p_mhv-wip_chg    = it_shop_sum-wip_qty - it_shop_sum-wip_pqty.

  p_mhv-manu_qty   = it_shop_sum-manu_qty.

  if p_type = 'P' .
*    if p_grqty = 'X'.
    p_mhv-curr_mh_t   = p_mhv-curr_qty - p_mhv-wip_chg.
*    else.
*      p_mhv-curr_mh_t   = p_mhv-curr_qty.
*    endif.
    p_mhv-add_mh_t    = p_mhv-add_qty.

  else.
*  caution!! - lower level
    p_mhv-curr_mh_t   = it_mhv_base-curr_mh * p_mhv-manu_qty.
    p_mhv-add_mh_t    = it_mhv_base-add_mh  * p_mhv-manu_qty.
*  caution!!
  endif.

  if p_mhv-gr_qty <> 0 .
    p_mhv-curr_mh  = p_mhv-curr_mh_t     / abs( p_mhv-gr_qty ).
    p_mhv-add_mh   = p_mhv-add_mh_t      / abs( p_mhv-gr_qty ).
  endif.

  p_mhv-gross_mh   = p_mhv-curr_mh + p_mhv-add_mh.

endform.                    " move_shop_to_mhv
*&---------------------------------------------------------------------*
*&      Form  move_mha_to_mhv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MHV2  text
*----------------------------------------------------------------------*
form move_mha_to_mhv tables  p_mhv structure it_mhv
                     using   p_kostl.

  clear it_mha.
  read table it_mha with key kostl = p_kostl.
  p_mhv-del_mh       = p_mhv-gross_mh * it_mha-del_rat.
  p_mhv-del_rat      = it_mha-del_rat.
*  p_mhv-final_mh     = p_mhv-gross_mh + it_mhv-del_mh.
  p_mhv-final_mh   = p_mhv-gross_mh * ( 1 + ( it_mha-del_rat / 100 ) ) .

  if p_mhv-pp_gr <> 0 .
    p_mhv-dir_mh      = ( p_mhv-curr_mh_t + p_mhv-add_mh_t ) /
                        p_mhv-pp_gr.
  endif.

  p_mhv-dir_mh2     = p_mhv-dir_mh * ( 1 + ( it_mha-del_rat / 100 ) ).

  p_mhv-net_mh       = p_mhv-final_mh * it_mha-net_rat.

  p_mhv-semi_mh      = p_mhv-dir_mh2 * it_mha-semi_rat.
  p_mhv-pc_mh        = p_mhv-dir_mh2 * it_mha-pc_rat.
  p_mhv-qc_mh        = p_mhv-dir_mh2 * it_mha-qc_rat.
  p_mhv-pm_mh        = p_mhv-dir_mh2 * it_mha-pm_rat.

  p_mhv-indir_mh     = p_mhv-dir_mh2 * it_mha-in_rat.
  p_mhv-admin_mh     = p_mhv-dir_mh2 * it_mha-admin_rat.
  p_mhv-os_dir       = p_mhv-dir_mh2 * it_mha-os_dir_rat.
  p_mhv-os_semi      = p_mhv-dir_mh2 * it_mha-os_semi_rat.
  p_mhv-os_pc        = p_mhv-dir_mh2 * it_mha-os_pc_rat.
  p_mhv-os_qc        = p_mhv-dir_mh2 * it_mha-os_qc_rat.
  p_mhv-os_pm        = p_mhv-dir_mh2 * it_mha-os_pm_rat.
  p_mhv-os_indir     = p_mhv-dir_mh2 * it_mha-os_in_rat.
  p_mhv-os_admin     = p_mhv-dir_mh2 * it_mha-os_admin_rat.

endform.                    " move_mha_to_mhv
*&---------------------------------------------------------------------*
*&      Form  move_mhv_to_mhv2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MHV2  text
*      -->P_3351   text
*----------------------------------------------------------------------*
form move_mhv_to_mhv2 tables   p_mhv structure it_mhv
                      using    p_type.

  p_mhv-curr_qty    = it_mhv-curr_qty.
  p_mhv-add_qty     = it_mhv-add_qty.
  p_mhv-wip_chg     = it_mhv-wip_chg.

  p_mhv-manu_qty    = it_mhv-manu_qty.
*  p_mhv-gr_qty      = it_mhv-gr_qty.

  if p_type = 'P' .
    p_mhv-curr_mh_t    = p_mhv-curr_qty - p_mhv-wip_chg.
    p_mhv-add_mh_t     = p_mhv-add_qty.
  else.
*  caution!! - lower level
    p_mhv-curr_mh_t    = it_mhv_base-curr_mh * p_mhv-manu_qty.
    p_mhv-add_mh_t     = it_mhv_base-add_mh  * p_mhv-manu_qty.
*  caution!!
  endif.

  if p_mhv-gr_qty <> 0 .
    p_mhv-curr_mh  = p_mhv-curr_mh_t     / abs( p_mhv-gr_qty ).
    p_mhv-add_mh   = p_mhv-add_mh_t      / abs( p_mhv-gr_qty ).
  endif.

  p_mhv-gross_mh   = p_mhv-curr_mh + p_mhv-add_mh.

endform.                    " move_mhv_to_mhv2
*&---------------------------------------------------------------------*
*&      Form  read_ztco_shop_pln_standard
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_ztco_shop_pln_standard.
  perform progress_indicator using  '10' ' '
                   'Read Standard data'.


  perform gathering_data_shop_pln using p_klvar.

  perform progress_indicator using  '40' ' '
                    'Read Standard data'.

  perform gathering_data_cosl.

  perform progress_indicator using  '60' ' '
                   'Calculate Standard data'.


  perform calculate_mh_pln.
  perform append_ztco_mhv.


endform.                    " read_ztco_shop_pln_standard
*&---------------------------------------------------------------------*
*&      Form  gathering_data_shop_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3847   text
*----------------------------------------------------------------------*
form gathering_data_shop_pln using   p_klvar.
  refresh :it_shop_pln.
  case p_klvar.
    when 'PPC1'.
      select kokrs bdatj poper klvar artnr shop llv_matnr meeht
             kstar kostl lstar meeht menge ck_kalst lot_size typps
           into corresponding fields of table it_shop_pln
            from ztco_shop_pln
             where kokrs eq p_kokrs
               and klvar eq p_klvar
               and bdatj eq p_bdatj
               and poper eq p_poper
*             and ( ( typps = 'E' and lstar eq 'MAN_HR' ) or
*                     typps = 'M' and kstar = p_ksmip ) .
             and ( ( typps = 'E' and lstar = 'MAN_HR'
                                 and kostl in s_kostl )
                  or typps = 'M' and fevor <> space
                                 and llv_matnr in s_llvmt )
             and ck_kalst in s_kalst
             and artnr in s_artnr.

    when 'ZPCP'.
      select kokrs bdatj poper klvar artnr shop llv_matnr
             kstar kostl lstar meeht menge ck_kalst lot_size typps
              into corresponding fields of table it_shop_pln
                from ztco_shop_pln
                where kokrs eq p_kokrs
                  and klvar eq p_klvar
                  and poper eq p_poper
                  and bdatj eq p_bdatj
*             and ( ( typps = 'E' and lstar eq 'MAN_HR' ) or
*                     typps = 'M' and kstar = p_ksmip ) .
             and ( ( typps = 'E' and lstar = 'MAN_HR'
                                 and kostl in s_kostl )
                  or typps = 'M' and fevor <> space
                                 and llv_matnr in s_llvmt )
             and ck_kalst in s_kalst
             and artnr in s_artnr.

  endcase.


*  loop at it_shop_pln.
*    if it_shop_pln-kstar eq p_ksmh or
*       it_shop_pln-kstar eq p_ksmip.
*      continue.
*    else.
*      delete table it_shop_pln from it_shop_pln.
*    endif.
*  endloop.
*  if p_bpl eq 'X'.
*    loop at it_shop_pln where kstar eq p_ksmip and
*                              llv_matnr eq space.
*      delete table it_shop_pln from it_shop_pln.
*    endloop.
*  endif.

  sort it_shop_pln by  kokrs bdatj klvar artnr
                       shop llv_matnr kstar kostl lstar.

  describe table it_shop_pln lines w_int.
  if w_int = 0.
    message e026.
  endif.

  loop at it_shop_pln.
    perform make_it_shop_pp_pln .
  endloop.

endform.                    " gathering_data_shop_pln
*&---------------------------------------------------------------------*
*&      Form  calculate_mh_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_mh_pln.

  perform calculate_from_shop_pln.
  perform calculate_by_level_pln.


endform.                    " calculate_mh_pln
*&---------------------------------------------------------------------*
*&      Form  move_shop_to_mhv_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MHV_PLN  text
*      -->P_4057   text
*----------------------------------------------------------------------*
form move_shop_to_mhv_pln tables   p_mhv structure it_mhv_pln
                          using    p_type .

  move-corresponding it_shop_pln to p_mhv.
  p_mhv-artnr = it_shop_pln-artnr.

  if p_type = 'P' .
    p_mhv-menge     = it_shop_pln-menge.
    if it_shop_pln-lot_size = 0 .
      it_shop_pln-lot_size = 1.
    endif.
    p_mhv-curr_mh   = p_mhv-menge / it_shop_pln-lot_size.
  else.
*  caution!! - lower level
    if it_shop_pln-lot_size = 0 .
      it_shop_pln-lot_size = 1.
    endif.
    p_mhv-menge     = it_shop_pln-menge .
    p_mhv-curr_mh   = p_mhv-menge * it_mhv_pln_base-curr_mh /
                      it_shop_pln-lot_size.
*  caution!!
  endif.


  clear it_plaf.
  read table it_plaf with key artnr = p_mhv-artnr binary search.

  if sy-subrc = 0.
    p_mhv-gr_qty  = it_plaf-gsmng .

*-default quantity = 1 by andy
  else.
    p_mhv-gr_qty  = 1.

  endif.

endform.                    " move_shop_to_mhv_pln
*&---------------------------------------------------------------------*
*&      Form  calculate_from_shop_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_from_shop_pln.
* 1. Component = '-'.  (No exist)
  loop at it_shop_pln where typps = 'E' .

    perform move_shop_to_mhv_pln tables it_mhv_pln
                                 using  'P'.

    perform move_mha_to_mhv_pln tables it_mhv_pln
                                using it_shop_pln-kostl.

    append it_mhv_pln. clear it_mhv_pln.
  endloop.

endform.                    " calculate_from_shop_pln
*&---------------------------------------------------------------------*
*&      Form  calculate_by_level_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_by_level_pln.
  PERFORM make_mhv_base_pln tables it_mhv_pln it_mhv_pln_base.

  sort it_shop_pln     by typps descending
                          ck_kalst artnr llv_matnr.
  sort it_mhv_pln_base by artnr .

* 2. Step 2
  loop at it_shop_pln where typps = 'M' .

    loop at it_mhv_pln_base where artnr = it_shop_pln-llv_matnr .

      perform move_shop_to_mhv_pln tables it_mhv_pln
                                   using 'C'.

      perform move_mha_to_mhv_pln tables it_mhv_pln
                                  using it_mhv_pln_base-kostl.

*     it_mhv_pln-sub_matnr  = it_mhv_pln_base-artnr.
      it_mhv_pln-shop       = it_mhv_pln_base-shop .
      it_mhv_pln-kostl      = it_mhv_pln_base-kostl.
      collect it_mhv_pln.

      it_mhv_pln_base = it_mhv_pln.
      clear it_mhv_pln_base-llv_matnr.
      collect it_mhv_pln_base.
      clear: it_mhv_pln, it_mhv_pln_base.
    endloop.

*-- if level is complete, then start next level
*    PERFORM make_mhv_base_pln tables it_mhv_pln it_mhv_pln_base.

  endloop.

** 3. Step 3
*  clear : it_mhv_pln_base, it_mhv_pln_base[].
*  it_mhv_pln_base[] = it_mhv_pln[].
*  sort it_mhv_pln by kalst.
*  sort it_mhv_pln_base by artnr.
*  loop at it_mhv_pln where llv_matnr <> ''.
*    move-corresponding it_mhv_pln to it_mhv_pln2.
*
*    loop at it_mhv_pln_base where artnr  = it_mhv_pln-llv_matnr
*                          and llv_matnr <> '' .
*
*      perform move_mhv_to_mhv2_pln tables it_mhv_pln2.
*
*      perform move_mha_to_mhv_pln tables it_mhv_pln2
*                                  using  it_mhv_pln2-kostl.
*
**      it_mhv_pln2-sub_matnr = it_mhv_pln_base-artnr.
*      it_mhv_pln2-llv_matnr = it_mhv_pln_base-llv_matnr.
*      it_mhv_pln2-shop      = it_mhv_pln_base-shop .
*      it_mhv_pln2-kostl     = it_mhv_pln_base-kostl.
*
*      collect it_mhv_pln2.
*      move-corresponding it_mhv_pln2 to it_mhv_pln_base.
*
*      collect it_mhv_pln_base.
*      clear : it_mhv_pln2, it_mhv_pln_base.
*    endloop.
*  endloop.

endform.                    " calculate_by_level_pln
*&---------------------------------------------------------------------*
*&      Form  move_mhv_to_mhv2_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MHV_PLN2  text
*      -->P_4204   text
*----------------------------------------------------------------------*
form move_mhv_to_mhv2_pln tables  p_mhv  structure it_mhv_pln.


  move-corresponding it_mhv_pln to p_mhv.

  p_mhv-menge     = it_mhv_pln-menge .
  p_mhv-curr_mh   = p_mhv-menge * it_mhv_pln_base-curr_mh /
                    it_shop_pln-lot_size.

endform.                    " move_mhv_to_mhv2_pln
*&---------------------------------------------------------------------*
*&      Form  update_ztco_mhv_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ztco_mhv_pln.

  perform progress_indicator using  '90' ' '
                   'Insert data...'.

  delete from ztco_mhv_pln where kokrs = p_kokrs
                             and bdatj = p_bdatj
                             and poper = p_poper.


  insert ztco_mhv_pln from table it_ztco_mhv_pln.

  if sy-subrc = 0 .
    commit work.
    if p_bpl = 'X'.
      message s009.
    endif.
  endif.

endform.                    " update_ztco_mhv_pln
*&---------------------------------------------------------------------*
*&      Form  append_ztco_mhv_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_ztco_mhv_actual.

  sort it_mhv by kalst artnr .
  loop at it_mhv .
    move-corresponding it_mhv to it_ztco_mhv.
* LOG
    it_ztco_mhv-erdat = sy-datum.
    it_ztco_mhv-erzet = sy-uzeit.
    it_ztco_mhv-ernam = sy-uname.
    it_ztco_mhv-kokrs = p_kokrs.
    it_ztco_mhv-bdatj = p_bdatj.
    it_ztco_mhv-poper = p_poper.
    append it_ztco_mhv. clear it_ztco_mhv.
  endloop.

  sort it_mhv2 by kalst artnr .
  loop at it_mhv2 .
    move-corresponding it_mhv2 to it_ztco_mhv.
* LOG
    it_ztco_mhv-erdat = sy-datum.
    it_ztco_mhv-erzet = sy-uzeit.
    it_ztco_mhv-ernam = sy-uname.
    it_ztco_mhv-kokrs = p_kokrs.
    it_ztco_mhv-bdatj = p_bdatj.
    it_ztco_mhv-poper = p_poper.
    append it_ztco_mhv. clear it_ztco_mhv.
  endloop.


  sort it_ztco_mhv_pln.


endform.                    " append_ztco_mhv_actual
*&---------------------------------------------------------------------*
*&      Form  get_gr_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SHOP_SUM_LLV_MATNR  text
*      <--P_IT_MHV_GR_QTY  text
*----------------------------------------------------------------------*
form get_gr_qty using    p_artnr
                         p_matnr
                         p_werks
                         p_kostl
                changing p_gr_qty      "MH
                         p_pp_gr.    "HPV


  data : l_gr_qty type p decimals 4.

  clear : l_gr_qty, p_gr_qty, p_pp_gr.
  clear it_mara.
  read table it_mara with key matnr = p_artnr binary search.

  clear: IT_PRODQTY.
  read table IT_PRODQTY with key matnr = p_artnr
                        binary search.
  p_gr_qty = p_pp_gr = IT_PRODQTY-OUT_MENGE.

  check p_rp = 'X' and it_mara-mtart = 'FERT'.
  clear: it_gr_wip.

* FSC Product => MIP component
  if not p_matnr is initial.
*     GR QTy
    clear it_marc.
    read table it_marc with key matnr = p_matnr
                                werks = p_werks  binary search.

    clear it_plpo.
    read table it_plpo with key usr00 = it_marc-vspvb binary search.
    if it_plpo-usr01 is initial.
      it_plpo-usr01 = '17'.      "if blank, then default 17 RP
    endif.

    read table  it_gr_wip with key matnr   = p_artnr
                                   rppoint = it_plpo-usr01.
* FSC Produc => CC
  else.
    read table it_gr_wip with key matnr  = p_artnr
                                  workct = p_kostl
                         binary search.

  endif.

  if sy-subrc = 0.
    p_gr_qty  =  it_gr_wip-outqty.
*         p_gr_qty  =  it_gr_wip-outqty +
*                      ( it_gr_wip-ewqty - it_gr_wip-bwqty ) / 2.
  endif.

endform.                    " get_gr_qty
*&---------------------------------------------------------------------*
*&      Form  make_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SEMI  text
*----------------------------------------------------------------------*
form make_itab tables  p_itab  structure it_semi.

*  Department CHECK
  clear it_csks.
  read table it_csks with key kostl = it_mha_hr-kostl.
  p_itab-shop  = it_csks-abtei.
  p_itab-anzhl = it_mha_hr-anzhl.
  p_itab-menge = it_mha_hr-menge.  "out sourcing

  collect p_itab. clear : p_itab.

endform.                    " make_itab
*&---------------------------------------------------------------------*
*&      Form  make_itab2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IN  text
*----------------------------------------------------------------------*
form make_itab2 tables   p_itab structure it_in .

  p_itab-anzhl = it_mha_hr-anzhl.
  p_itab-menge = it_mha_hr-menge.  "out sourcing
  collect p_itab. clear : p_itab.

endform.                    " make_itab2
*&---------------------------------------------------------------------*
*&      Form  select_ztco_wip
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_pp_gr  text
*----------------------------------------------------------------------*
*form select_ztco_wip  using    p_artnr
*                      changing p_pp_gr.
*
**  DATA : l_rp LIKE ztco_wip-rppoint.
**
**  CLEAR l_rp.
**  SELECT MAX( rppoint ) INTO l_rp  FROM ztco_wip
**      WHERE  matnr   = p_artnr.
*
*  clear p_pp_gr.
*
*  read table it_gr_wip with key gubun  = 'F'
*                                matnr  = p_artnr
*                       binary search.
*
*  p_pp_gr = it_gr_wip-outqty +   p_pp_gr .
*
*endform.                    " select_ztco_wip
*&---------------------------------------------------------------------*
*&      Form  cal_semi_pc_qc_pm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SEMI  text
*      <--P_IT_SHOP_SEMI_DIR1  text
*      <--P_IT_SHOP_OS_SEMI1  text
*      <--P_IT_SHOP_SEMI_DIR2  text
*      <--P_IT_SHOP_OS_SEMI2  text
*----------------------------------------------------------------------*
form cal_semi_pc_qc_pm tables   p_it structure it_semi
                       changing p_val1
                                p_val_os1
                                p_val2
                                p_val_os2
                                p_val
                                p_val_os.

  clear p_it.
  read table p_it with key shop = it_shop_hr-shop.
* Non Shop exist CC
  if p_it-shop <> ''.
* Shop exist CC
    p_val1     = it_shop_hr-shop_rate *  p_it-anzhl.
    p_val_os1  = it_shop_hr-shop_rate *  p_it-menge.
  endif.

  clear p_it.
  read table p_it with key shop = ''.
  p_val2       = it_shop_hr-total_rate      *  p_it-anzhl.
  p_val_os2    = it_shop_hr-total_rate      *  p_it-menge.

  p_val    = p_val1 + p_val2.
  p_val_os = p_val_os1 + p_val_os2.
endform.                    " cal_semi_pc_qc_pm
*&---------------------------------------------------------------------*
*&      Form  cal_indir_admin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IN  text
*      <--P_IT_SHOP_IN_DIR  text
*      <--P_IT_SHOP_OS_IN  text
*----------------------------------------------------------------------*
form cal_indir_admin tables   p_it structure  it_in
                     changing p_val
                              p_val_os.

  clear p_it.
  read table p_it index 1.
  p_val     = it_shop_hr-total_rate    *  p_it-anzhl.
  p_val_os  = it_shop_hr-total_rate    *  p_it-menge.

endform.                    " cal_indir_admin
*&---------------------------------------------------------------------*
*&      Form  gathering_data_cosl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form gathering_data_cosl.

* Cost center Group.
  perform get_all_cc_group.

* GR Qty
  perform get_pln_gr_qty_plaf.

* Get plan mh : direct : COSL , others : COSR
  perform get_plan_mh.
  perform select_cosr_exclud_rate using '01' p_plnver.
  perform calculate_excluding.

* Get Shop code
  perform get_shop_from_csks.

* Select data from cosr : 1) Out source data  : CS012
*                         2) PC, QC, PM
* 1)
  perform select_outsource using '01'
                                 p_plnver.

* 2)
  perform select_pc_qc_pm using '01'.

  loop at it_mha_hr.
    move-corresponding it_mha_hr to it_mha.
    it_mha-hr_mh = it_mha_hr-anzhl.
    collect it_mha. clear it_mha.
  endloop.

  sort it_shop_pln  by kostl.

  sort it_mha_hr by kostl.

  perform make_it_shop_value using 'P'.

  perform calculate_others_value.

  perform get_value_rate using 'P'.




endform.                    " gathering_data_cosl
*&---------------------------------------------------------------------*
*&      Form  get_all_cc_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_all_cc_group.


  perform get_cc_group tables r_semi
                       using  'SEM'.
  perform get_cc_group tables r_in
                       using  'IND'.
  perform get_cc_group tables r_admin
                       using  'ADM'.
  perform get_cc_group tables r_pc_cc
                       using  'PC'.
  perform get_cc_group tables r_qc_cc
                       using  'QC'.
  perform get_cc_group tables r_pm_cc
                       using  'PM'.


* For allocation by Shop.
  perform get_cc_group2.


endform.                    " get_all_cc_group
*&---------------------------------------------------------------------*
*&      Form  get_shop_from_csks
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_shop_from_csks.
* Get Shop Code
  select  * into corresponding fields of table  it_csks
      from csks
    where  datab <= sy-datum
      and  datbi >= sy-datum
      and  abtei <> '' .

  loop at it_csks.
    clear it_shop_sum.
    read table it_shop_sum with key shop = it_csks-abtei.
    if sy-subrc <> 0 .
      delete it_csks.
    endif.
  endloop.

endform.                    " get_shop_from_csks
*&---------------------------------------------------------------------*
*&      Form  get_mh_from_cosl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_plan_mh.
  data : l_field(30).
  data : it_cosr_pln like cosr occurs 0 with header line.
* 'CS010' include all man hour
*  Comment 'select COSL logic'


** Direct MH
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_cosl_temp
*     FROM cosl
*     WHERE gjahr = p_bdatj
*       AND ( wrttp = '01' AND versn = p_plnver )
*       AND objnr LIKE '%MAN_HR%'.
*
*  LOOP AT it_cosl_temp.
*    MOVE-CORRESPONDING it_cosl_temp TO it_mha_hr.
*    it_mha_hr-kostl = it_cosl_temp-objnr+6(6).
**-----capacity
*    CONCATENATE 'IT_COSL_TEMP-KAP' p_poper INTO l_field.
*    ASSIGN  (l_field)    TO   <f_field> .
*    IF <f_field> =  0 .
**-------plan qty
*      CONCATENATE 'IT_COSL_TEMP-LST' p_poper INTO l_field.
*      ASSIGN  (l_field)    TO   <f_field> .
*    ENDIF.
*    it_mha_hr-anzhl = <f_field> .
*    COLLECT  it_mha_hr.
*    CLEAR it_mha_hr.
*  ENDLOOP.

* Others

* 'CS010' include all man hour
  select  * into corresponding fields of table  it_cosr_pln
      from cosr
    where lednr = '00'
      and objnr like 'KS%'
      and gjahr = p_bdatj
      and wrttp = '01'
      and stagr = c_cs010
      and versn = p_plnver.

  loop at it_cosr_pln.
    move-corresponding it_cosr_pln to it_mha_hr.
    it_mha_hr-kostl = it_cosr_pln-objnr+6(10).
    concatenate 'IT_COSR_PLN-SME' p_poper into l_field.
    assign (l_field) to <f_field>.
    it_mha_hr-anzhl = <f_field>.
    collect it_mha_hr. clear it_mha_hr.
  endloop.


endform.                    " get_plan_mh
*&---------------------------------------------------------------------*
*&      Form  calculate_others_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_others_value.

  loop at it_shop_hr.
    clear it_shop2.
    read table it_shop2 with key shop = it_shop_hr-shop.
    if sy-subrc = 0 and it_shop2-anzhl <> 0 .
      it_shop_hr-shop_rate      = it_shop_hr-anzhl / it_shop2-anzhl.
    endif.
    it_shop_hr-total_rate       = it_shop_hr-anzhl / g_total_anzhl.

* SEMI
    perform cal_semi_pc_qc_pm tables it_semi
                              changing it_shop_hr-semi_dir1
                                       it_shop_hr-os_semi1
                                       it_shop_hr-semi_dir2
                                       it_shop_hr-os_semi2
                                       it_shop_hr-semi_dir
                                       it_shop_hr-os_semi.
* PC
    perform cal_semi_pc_qc_pm tables it_pc
                              changing it_shop_hr-pc1
                                       it_shop_hr-os_pc
                                       it_shop_hr-pc2
                                       it_shop_hr-os_pc2
                                       it_shop_hr-pc
                                       it_shop_hr-os_pc.
* QC
    perform cal_semi_pc_qc_pm tables it_qc
                              changing it_shop_hr-qc1
                                       it_shop_hr-os_qc
                                       it_shop_hr-qc2
                                       it_shop_hr-os_qc2
                                       it_shop_hr-qc
                                       it_shop_hr-os_qc.
* PM
    perform cal_semi_pc_qc_pm tables it_pm
                              changing it_shop_hr-pm1
                                       it_shop_hr-os_pm
                                       it_shop_hr-pm2
                                       it_shop_hr-os_pm2
                                       it_shop_hr-pm
                                       it_shop_hr-os_pm.
* INDIRECT
    perform cal_indir_admin   tables it_in
                              changing it_shop_hr-in_dir
                                       it_shop_hr-os_in.

* ADMIN
    perform cal_indir_admin   tables it_admin
                              changing it_shop_hr-admin
                                       it_shop_hr-os_admin.

    modify it_shop_hr. clear it_shop_hr.

  endloop.

endform.                    " calculate_others_value
*&---------------------------------------------------------------------*
*&      Form  make_it_shop_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_it_shop_value using p_type.

  data :l_shop like it_shop_sum-shop.

* Semi
  clear g_total_anzhl.
  loop at it_mha_hr.
    clear l_shop.
    perform get_shop_code using    it_mha_hr-kostl
                          changing l_shop.
*    IF p_type = 'A'.
*      CLEAR it_shop_sum.
*      READ TABLE it_shop_sum WITH KEY kostl = it_mha_hr-kostl
*                             BINARY SEARCH.
*      l_shop = it_shop_sum-shop.
*    ELSE.
*      CLEAR it_shop_pln.
*      READ TABLE it_shop_pln WITH KEY kostl = it_mha_hr-kostl
*                             BINARY SEARCH.
*      l_shop = it_shop_pln-shop.
*    ENDIF.
    if l_shop <> '' .
      it_shop_hr-shop  = l_shop.
      it_shop_hr-kostl = it_mha_hr-kostl.
      it_shop_hr-anzhl = it_mha_hr-anzhl.
      it_shop_hr-menge = it_mha_hr-menge.
      collect it_shop_hr.

      it_shop2-shop  = l_shop.
      it_shop2-anzhl = it_mha_hr-anzhl .
      it_shop2-menge = it_mha_hr-menge .
      collect it_shop2.

      g_total_anzhl = g_total_anzhl +  it_shop2-anzhl .
      clear : it_shop_hr, it_shop2.
    endif.

**  Group FOR SEMI
    if it_mha_hr-kostl in r_semi and not r_semi[] is initial.
      perform make_itab tables it_semi.
    endif.

**  Group FOR PC
    if it_mha_hr-kostl in r_pc and not r_pc[] is initial.
      perform make_itab tables it_pc.
    endif.

**  Group FOR QC
    if it_mha_hr-kostl in r_qc and not r_qc[] is initial.
      perform make_itab tables it_qc.
    endif.

**  Group FOR PM
    if it_mha_hr-kostl in r_pm and not r_pm[] is initial.
      perform make_itab tables it_pm.
    endif.

*  Group FOR INDIRECT
    if it_mha_hr-kostl in r_in and not r_in[] is initial.
      perform make_itab2 tables it_in.

    endif.
*  Group FOR ADMIN
    if it_mha_hr-kostl in r_admin and not r_admin[] is initial.
      perform make_itab2 tables it_admin.
    endif.
    clear : it_mha_hr.
  endloop.


endform.                    " make_it_shop_value
*&---------------------------------------------------------------------*
*&      Form  make_it_shop_pp_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_it_shop_pp_pln.
  move-corresponding it_shop_pln to it_shop_pp_pln .
  collect it_shop_pp_pln . clear it_shop_pp_pln .
endform.                    " make_it_shop_pp_pln
*&---------------------------------------------------------------------*
*&      Form  get_value_rate_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_value_rate using p_type .
  data : l_wip_chg    like it_shop_sum-wip_qty,
         l_curr_mh_t  like it_shop_sum-mbgbtr,
         l_add_mh_t   like it_shop_sum-mbgbtr,
         l_dir_mh2    type p decimals 6.

  loop at it_mha.
    clear it_shop_hr.
    read table it_shop_hr with key kostl = it_mha-kostl.
    check sy-subrc = 0 .

    clear it_shop2.
    read table it_shop2 with key shop = it_shop_hr-shop.

    clear it_shop_pp.
    read table it_shop_pp with key kostl = it_mha-kostl.

    it_mha-semi_dir = it_shop_hr-semi_dir.
    it_mha-pc       = it_shop_hr-pc.
    it_mha-qc       = it_shop_hr-qc.
    it_mha-pm       = it_shop_hr-pm.

    it_mha-in_dir   = it_shop_hr-in_dir.
    it_mha-admin    = it_shop_hr-admin.

    it_mha-os_semi  = it_shop_hr-os_semi.
    it_mha-os_pc    = it_shop_hr-os_pc.
    it_mha-os_qc    = it_shop_hr-os_qc.
    it_mha-os_pm    = it_shop_hr-os_pm.

    it_mha-os_in    = it_shop_hr-os_in.
    it_mha-os_admin = it_shop_hr-os_admin.

    if p_type = 'A'.

      if it_mha-hr_mh <>  0 .
        it_mha-del_rat  = it_mha-del_mh  / it_mha-hr_mh.
      endif.

      clear : l_wip_chg , l_curr_mh_t, l_add_mh_t, l_dir_mh2.

      l_wip_chg     = it_shop_pp-wip_qty - it_shop_pp-wip_pqty.
      l_curr_mh_t   = it_shop_pp-mbgbtr  - l_wip_chg.
      l_add_mh_t    = it_shop_pp-add_mbgbtr + it_shop_pp-mbgbtr2.
      l_dir_mh2     = ( l_curr_mh_t + l_add_mh_t ) *
                      ( 1 + ( it_mha-del_rat / 100 ) ).
    else.
      clear l_dir_mh2.
      l_dir_mh2 = it_shop_pp-menge.
    endif.

    if l_dir_mh2 <> 0 .
      it_mha-net_rat  = abs( it_mha-netmh    / l_dir_mh2 ) .
      it_mha-semi_rat = abs( it_mha-semi_dir / l_dir_mh2 ) .
      it_mha-pc_rat   = abs( it_mha-pc       / l_dir_mh2 ) .
      it_mha-qc_rat   = abs( it_mha-qc       / l_dir_mh2 ) .
      it_mha-pm_rat   = abs( it_mha-pm       / l_dir_mh2 ) .
      it_mha-in_rat   = abs( it_mha-in_dir   / l_dir_mh2 ) .
      it_mha-admin_rat = abs( it_mha-admin   / l_dir_mh2 ) .

* Out source
      it_mha-os_dir_rat    = it_mha-menge / l_dir_mh2 .
      it_mha-os_semi_rat   = it_mha-os_semi  / l_dir_mh2 .
      it_mha-os_pc_rat     = it_mha-os_pc    / l_dir_mh2 .
      it_mha-os_qc_rat     = it_mha-os_qc    / l_dir_mh2 .
      it_mha-os_pm_rat     = it_mha-os_pm    / l_dir_mh2 .
      it_mha-os_in_rat     = it_mha-os_in    / l_dir_mh2 .
      it_mha-os_admin_rat  = it_mha-os_admin / l_dir_mh2 .
    endif.

    modify it_mha. clear it_mha.
  endloop.

endform.                    " get_value_rate
*&---------------------------------------------------------------------*
*&      Form  select_outsource
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3903   text
*      -->P_P_VERSN  text
*      -->P_3905   text
*----------------------------------------------------------------------*
form select_outsource using  p_wrttp
                             p_ver.
  data : l_field(30).

* MH out source
  if p_reft = space.
    select  * into corresponding fields of table  it_cosr_os
        from cosr
      where gjahr = p_bdatj
        and wrttp = p_wrttp
        and versn = p_ver
        and stagr = c_cs012.

    loop at it_cosr_os.
******FIXME
      it_mha_os-kostl = it_cosr_os-objnr+6(10).

      concatenate 'IT_COSR_OS-SME' p_poper into l_field.
      assign (l_field) to <f_field>.
      it_mha_os-menge = <f_field>.

      collect it_mha_os. clear it_mha_os.
    endloop.

  else.
    data: l_yyyymm type JAHRPER.
    concatenate p_bdatj p_poper into l_yyyymm.
    select KOSTL sum( MENGE )
       into table it_mha_os
        from ztco_mhos
       where kokrs = p_kokrs
         and RETYM = l_yyyymm
       group by kostl.
  endif.

  loop at it_mha_os.
    read table it_mha_hr with key kostl = it_mha_os-kostl
                         binary search.
    if sy-subrc = 0.
      it_mha_hr-menge = it_mha_hr-menge + it_mha_os-menge.
      modify it_mha_hr transporting menge
             where kostl = it_mha_os-kostl.
    endif.
  endloop.
endform.                    " select_outsource
*&---------------------------------------------------------------------*
*&      Form  select_PC_QC_PM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3707   text
*      -->P_P_VERSN  text
*      -->P_3709   text
*----------------------------------------------------------------------*
form select_pc_qc_pm using p_wrttp.

  data : l_temp1(30),
         l_temp2(10),
         l_field(30).


* PC, QC, PM CC

*-Statistical key figures
  select  * into corresponding fields of table  it_tka03_temp
      from tka03 as a
    inner join tkt03 as b
      on  a~kokrs  = b~kokrs
     and  a~stagr  = b~stagr
    where a~kokrs = p_kokrs
      and b~spras = sy-langu.

  loop at it_tka03_temp.
    if it_tka03_temp-bezei cs ';'.
      clear : l_temp1, l_temp2.
      split it_tka03_temp-bezei at ';' into l_temp1 l_temp2.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                input  = l_temp2
           IMPORTING
                output = l_temp2.

      if l_temp2 in r_pc_cc.
        r_pc-sign = 'I'.    r_pc-option = 'EQ'.
        r_pc-low = l_temp2.
        append r_pc. clear r_pc.
      elseif l_temp2 in r_qc_cc.
        r_qc-sign = 'I'.    r_qc-option = 'EQ'.
        r_qc-low = l_temp2.
        append r_qc. clear r_qc.

      elseif l_temp2 in r_pm_cc.
        r_pm-sign = 'I'.    r_pm-option = 'EQ'.
        r_pm-low = l_temp2.
        append r_pm. clear r_pm.
      endif.
      it_tka03-kostl = l_temp2.
      it_tka03-stagr = it_tka03_temp-stagr.
      collect it_tka03. clear it_tka03.
    endif.
  endloop.

*SKF value
  select  * into corresponding fields of table  it_cosr_skf
      from cosr
      for all entries in it_tka03
    where gjahr = p_bdatj
      and wrttp = p_wrttp
      and stagr = it_tka03-stagr.

  loop at it_cosr_skf.
    move-corresponding it_cosr_skf to it_skf.
    it_skf-kostl = it_cosr_skf-objnr+6(10).
    concatenate 'IT_COSR_SKF-SME' p_poper into l_field.
    assign (l_field) to <f_field>.
    it_skf-sme000 = <f_field>.
    clear it_tka03.
    read table it_tka03 with key stagr = it_cosr_skf-stagr.
    it_skf-type = it_tka03-type.
    collect it_skf. clear it_skf.
  endloop.

* PC, QC, PM : distribution %
  data : begin of it_skf_sum occurs 0,
         stagr      like it_skf-stagr,
         sme000     like it_skf-sme000,
         end of it_skf_sum.

  loop at it_skf.
    it_skf_sum-stagr  = it_skf-stagr.
    it_skf_sum-sme000 = it_skf-sme000.
    collect it_skf_sum. clear it_skf_sum.
  endloop.

  loop at it_mha_hr.
    read table it_tka03 with key kostl = it_mha_hr-kostl.

    if sy-subrc =  0.
      loop at it_skf where stagr = it_tka03-stagr.
        clear it_skf_sum.
        read table it_skf_sum with key stagr = it_skf-stagr.

        if it_skf_sum-sme000 <>  0 .
          it_dist-kostl = it_skf-kostl.
          it_dist-persn = it_skf-sme000 / it_skf_sum-sme000.
          append it_dist.
        endif.
      endloop.
    endif.
  endloop.


endform.                    " select_PC_QC_PM
*&---------------------------------------------------------------------*
*&      Form  select_cosr_EXCLUD_RATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_cosr_exclud_rate  using  p_wrttp
                                     p_ver.

* Selection for Exclude rate
  loop at it_mha_hr.
    concatenate 'KS' p_kokrs it_mha_hr-kostl into it_mha_hr-objnr.
    modify it_mha_hr. clear it_mha_hr.
  endloop.

  if not it_mha_hr[] is initial.
    select  * into corresponding fields of table  it_cosr_exc
        from cosr
        for all entries in it_mha_hr
      where lednr = '00'
        and objnr = it_mha_hr-objnr
        and gjahr = p_bdatj
        and wrttp = p_wrttp
        and versn = p_ver
        and stagr = c_cs011.   "MH exclusion
  endif.

endform.                    " select_cosr_EXCLUD_RATE
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_EXCLUDING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calculate_excluding.
  data : l_field(20),
         l_rate type p decimals 4.

  loop at it_mha_hr.
    clear : l_field, l_rate.
*   Exclude ratio from cosr
    clear it_cosr_exc.
    read table it_cosr_exc with key objnr = it_mha_hr-objnr.
    if sy-subrc = 0 .
      concatenate 'IT_COSR_EXC-SME' p_poper into l_field.
      assign (l_field) to <f_field>.
      l_rate = 100 - <f_field> .
      it_mha_hr-anzhl  = it_mha_hr-anzhl * l_rate / 100.
      it_mha_hr-netmh  = it_mha_hr-netmh * l_rate / 100.
      modify it_mha_hr. clear it_mha_hr.
    endif.
  endloop.

endform.                    " CALCULATE_EXCLUDING
*&---------------------------------------------------------------------*
*&      Form  move_MHA_to_mhv_pln
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MHV_PLN  text
*      -->P_2586   text
*----------------------------------------------------------------------*
form move_mha_to_mhv_pln tables p_mhv structure it_mhv_pln
                         using p_kostl.

  clear it_mha.
  read table it_mha with key kostl = p_kostl.

  check p_mhv-gr_qty <> 0.

  p_mhv-semi_mh      = p_mhv-menge * it_mha-semi_rat ."  / p_mhv-gr_Qty.
  p_mhv-pc_mh        = p_mhv-menge * it_mha-pc_rat  ."   / p_mhv-gr_Qty.
  p_mhv-qc_mh        = p_mhv-menge * it_mha-qc_rat ."    / p_mhv-gr_Qty.
  p_mhv-pm_mh        = p_mhv-menge * it_mha-pm_rat."    / p_mhv-gr_Qty.
  p_mhv-indir_mh     = p_mhv-menge * it_mha-in_rat  ."   / p_mhv-gr_Qty.
  p_mhv-admin_mh     = p_mhv-menge * it_mha-admin_rat ." / p_mhv-gr_Qty.
  p_mhv-os_dir       = p_mhv-menge * it_mha-os_dir_rat ."/ p_mhv-gr_Qty.
  p_mhv-os_semi      = p_mhv-menge * it_mha-os_semi_rat."/ p_mhv-gr_Qty.
  p_mhv-os_pc        = p_mhv-menge * it_mha-os_pc_rat ." / p_mhv-gr_Qty.
  p_mhv-os_qc        = p_mhv-menge * it_mha-os_qc_rat  ."/ p_mhv-gr_Qty.
  p_mhv-os_pm        = p_mhv-menge * it_mha-os_pm_rat  ."/ p_mhv-gr_Qty.
  p_mhv-os_indir     = p_mhv-menge * it_mha-os_in_rat  ."/ p_mhv-gr_Qty.
  p_mhv-os_admin     = p_mhv-menge * it_mha-os_admin_rat." p_mhv-gr_Qty.


endform.                    " move_MHA_to_mhv_pln
*&---------------------------------------------------------------------*
*&      Form  get_pln_gr_qty_plaf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_pln_gr_qty_plaf.
*  select * into corresponding fields of table it_plaf_temp
*   from plaf
*   where plscn = p_plscn              "Scenario
*     and sobes = 'E'                  "Special procurement
*     and stlan = '6' .                 "BOM usage
*
*
*  loop at it_plaf_temp.
*    move-corresponding it_plaf_temp to it_plaf.
*    it_plaf-perio(6) = it_plaf_temp-pedtr.
*    check it_plaf-perio(4)   = p_bdatj.
*    check it_plaf-perio+4(2) = p_poper+1(2).
*    collect it_plaf. clear it_plaf.
*  endloop.

  select * into corresponding fields of table it_plaf
     from ztco_shop_plh
     where kokrs = p_kokrs
       and bdatj = p_bdatj
       and poper = p_poper
       and klvar = p_klvar.
  sort it_plaf by artnr.

  loop at it_shop_pp_pln.
    clear it_plaf.
    read table it_plaf with key artnr = it_shop_pp_pln-artnr
                       binary search.
    move-corresponding it_shop_pp_pln to it_shop_pp.
    it_shop_pp-menge = it_shop_pp_pln-menge * it_plaf-gsmng.
    collect it_shop_pp. clear it_shop_pp.
  endloop.

endform.                    " get_pln_gr_qty_plaf
*&---------------------------------------------------------------------*
*&      Form  GET_SHOP_CODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_SHOP  text
*----------------------------------------------------------------------*
form get_shop_code using    p_kostl
                   changing p_shop.

  loop at it_ccgrp.
    if p_kostl between it_ccgrp-low and it_ccgrp-high.
      p_shop = it_ccgrp-group.
      exit.
    endif.
  endloop.
endform.                    " GET_SHOP_CODE
*&---------------------------------------------------------------------*
*&      Form  get_cc_group2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_DIR_CC  text
*----------------------------------------------------------------------*
form get_cc_group2 .

  data: t_setlist like setlist occurs 0 with header line.
  data: t_sethier like sethier occurs 0 with header line.
  data: t_setvalues like setvalues occurs 0 with header line.

  data : begin of it_dir_grp occurs 0,
           group like t_sethier-shortname,
         end of it_dir_grp.


  clear : t_setlist.
  call function 'G_SET_LIST_SELECT'
       EXPORTING
            setclass      = '0101'
            shortname     = 'DIRECT'
            kokrs         = p_kokrs
            ktopl         = g_ktopl
       TABLES
            matching_sets = t_setlist.
  if t_setlist[] is initial.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  else.
    read table t_setlist index 1.
  endif.

  call function 'G_SET_TREE_IMPORT'
       EXPORTING
            setid                     = t_setlist-setname
       TABLES
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       EXCEPTIONS
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            others                    = 4.

  if sy-subrc <> 0.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  endif.
* TRANSFER THE VALUE TO CC RANGE.
  loop at t_sethier where level = 1.
    it_dir_grp-group = t_sethier-shortname.
    collect it_dir_grp. clear it_dir_grp.
  endloop.

  loop at it_dir_grp.
    perform get_cc_group3 using it_dir_grp-group.
  endloop.

endform.                    " get_cc_group2
*&---------------------------------------------------------------------*
*&      Form  get_cc_group3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CCGRP_SHORTNAME  text
*----------------------------------------------------------------------*
form get_cc_group3 using    p_shortname.

  data: t_setlist like setlist occurs 0 with header line.
  data: t_sethier like sethier occurs 0 with header line.
  data: t_setvalues like setvalues occurs 0 with header line.


  clear : t_setlist.
  call function 'G_SET_LIST_SELECT'
       EXPORTING
            setclass      = '0101'
            shortname     = p_shortname
            kokrs         = p_kokrs
            ktopl         = g_ktopl
       TABLES
            matching_sets = t_setlist.
  if t_setlist[] is initial.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  else.
    read table t_setlist index 1.
  endif.

  call function 'G_SET_TREE_IMPORT'
       EXPORTING
            setid                     = t_setlist-setname
       TABLES
            set_hierarchy             = t_sethier
            set_values                = t_setvalues
       EXCEPTIONS
            set_not_found             = 1
            illegal_field_replacement = 2
            illegal_table_replacement = 3
            others                    = 4.

  if sy-subrc <> 0.
    message e002(sy) with 'Cost Center group does not exist'.
    exit.
  endif.


  loop at t_setvalues.
    it_ccgrp-group = p_shortname.
    it_ccgrp-low   = t_setvalues-from.
    it_ccgrp-high  = t_setvalues-to.
    append it_ccgrp. clear it_ccgrp.
  endloop.

endform.                    " get_cc_group3
*&---------------------------------------------------------------------*
*&      Form  read_wip_data
*&---------------------------------------------------------------------*
FORM read_wip_data.
  data : l_yymm like ztco_wip-yymm.
  check p_rp = 'X'.

  concatenate p_bdatj p_poper+1(2) into l_yymm.

* Select wip (gr Qty & gr Qty2)
  select plant matnr rppoint workct
         inqty outqty bwqty ewqty
     into  corresponding fields of it_gr_wip
     from ztco_wip
        for all entries in IT_PRODQTY
        where gubun = 'W'
          and yymm  = l_yymm
          and matnr = IT_PRODQTY-matnr
          and matnr in s_artnr.

    collect it_gr_wip.
  endselect.

  sort it_gr_wip by matnr workct.

  if sy-subrc <> 0.
    message w000 with 'Run WIP program first. (t-code:ZCOA70_W)'.
  endif.

*  delete it_gr_wip where yymm <> g_yymm.

ENDFORM.                    " read_wip_data
*&---------------------------------------------------------------------*
*&      Form  read_master_info
*&---------------------------------------------------------------------*
FORM read_master_info.
  data: l_idx like sy-tabix.

* Select material type
  select matnr mtart into  corresponding fields of table it_mara
     from mara
      for all entries in it_shop_sum
        where matnr = it_shop_sum-llv_matnr .

* check if shop cost is read
  sort it_shop_sum by artnr.
  loop at it_mara.
    read table it_shop_sum with key artnr = it_mara-matnr binary search.
    if sy-subrc <> 0.
      message s000 with 'Incomplete reading of shop cost;'
                        it_mara-matnr.
    endif.
  endloop.

  select matnr mtart
     appending corresponding fields of  table it_mara
     from mara
      for all entries in it_shop_sum
        where matnr = it_shop_sum-artnr.
  sort it_mara by matnr.

* Select material type
  select matnr werks vspvb into  corresponding fields of table it_marc
     from marc
      for all entries in it_shop_sum
        where matnr = it_shop_sum-llv_matnr
          and werks = it_shop_sum-par_werks.
  sort it_marc by werks matnr.

* Select routing for SHOP+WC determine
  select usr00 usr01  into  corresponding fields of table it_plpo
     from plpo
      where plnty = 'M'
*        AND plnty = 'RP'
        and datuv <= sy-datum.
  sort it_plpo by usr00.


ENDFORM.                    " read_master_info
*&---------------------------------------------------------------------*
*&      Form  collect_shop_cc
*&---------------------------------------------------------------------*
FORM collect_shop_cc.

* sum by shop+cc
  loop at it_shop_sum where typps = 'E'.
    move-corresponding it_shop_sum to it_shop_pp.
    collect it_shop_pp. clear it_shop_pp.
  endloop.

  describe table it_shop_sum lines w_int.
  if w_int = 0.
    message i000 with 'No exist actual data in ZTCO_SHOP_SUM'.
    stop.
  endif.

  sort it_shop_sum by  artnr
                       shop
                       llv_matnr
                       kostl
                       lstar.

ENDFORM.                    " collect_shop_cc
*&---------------------------------------------------------------------*
*&      Form  adjust_costing_level
*&---------------------------------------------------------------------*
FORM adjust_costing_level.
  data: l_idx like sy-tabix.

  sort it_shop_sum by artnr typps.

  loop at it_mara.
    l_idx = sy-tabix.
    read table it_shop_sum with key artnr = it_mara-matnr
                                    typps = 'M'
                           binary search.
    it_mara-kalst = it_shop_sum-kalst.
    modify it_mara index l_idx transporting kalst.
  endloop.

  sort it_mara     by matnr.
  sort it_shop_sum by typps kalst artnr llv_matnr.

  loop at it_shop_sum where typps = 'M'.
    l_idx = sy-tabix.
    read table it_mara with key matnr = it_shop_sum-llv_matnr
                       binary search.
    it_shop_sum-kalst_c = it_mara-kalst.
    modify it_shop_sum index l_idx transporting kalst_c.
    if it_shop_sum-kalst < it_mara-kalst.
      it_shop_sum-kalst = it_mara-kalst + 1.
      modify it_shop_sum  transporting kalst
                          where artnr = it_shop_sum-artnr.
      it_mara-kalst = it_shop_sum-kalst.
      modify it_mara transporting kalst
                     where matnr =  it_shop_sum-artnr.
    endif.
  endloop.

*adjust again???
  sort it_shop_sum by kalst artnr llv_matnr.

  loop at it_shop_sum where typps = 'M'.
    l_idx = sy-tabix.
    read table it_mara with key matnr = it_shop_sum-llv_matnr
                       binary search.
    it_shop_sum-kalst_c = it_mara-kalst.
    modify it_shop_sum index l_idx transporting kalst_c.
    if it_shop_sum-kalst < it_mara-kalst.
      it_shop_sum-kalst = it_mara-kalst + 1.
      modify it_shop_sum  transporting kalst
                          where artnr = it_shop_sum-artnr.
      it_mara-kalst = it_shop_sum-kalst.
      modify it_mara transporting kalst
                     where matnr =  it_shop_sum-artnr.

    endif.
  endloop.

ENDFORM.                    " adjust_costing_level
*&---------------------------------------------------------------------*
*&      Form  read_alt_material
*&---------------------------------------------------------------------*
FORM read_alt_material.

  select * into table it_alt
    from ztco_mhv_alt
    where kokrs = p_kokrs.

ENDFORM.                    " read_alt_material
*&---------------------------------------------------------------------*
*&      Form  make_mhv_base
*&---------------------------------------------------------------------*
FORM make_mhv_base TABLES   f_IT_MHV      STRUCTURE it_mhv
                            f_IT_MHV_BASE STRUCTURE it_mhv.

*  f_IT_MHV_BASE[] = F_IT_MHV[].

  refresh f_it_mhv_base.
  clear f_it_mhv_base.

* sort f_it_mhv by artnr kostl.
  loop at f_it_mhv.
    f_it_mhv_base = f_it_mhv.
    clear f_it_mhv_base-llv_matnr.
    collect f_it_mhv_base.
  endloop.

  sort f_it_mhv_base by artnr .

ENDFORM.                    " make_mhv_base
*&---------------------------------------------------------------------*
*&      Form  make_mhv_base
*&---------------------------------------------------------------------*
FORM make_mhv_base_pln TABLES   f_IT_MHV      STRUCTURE it_mhv_pln
                                f_IT_MHV_BASE STRUCTURE it_mhv_pln.

*  f_IT_MHV_BASE[] = F_IT_MHV[].
  refresh f_it_mhv_base.
  clear f_it_mhv_base.

* sort f_it_mhv by artnr kostl.
  loop at f_it_mhv.
    f_it_mhv_base = f_it_mhv.
    clear f_it_mhv_base-llv_matnr.
    collect f_it_mhv_base.
  endloop.

* sort f_it_mhv_base by artnr .

ENDFORM.                    " make_mhv_base
*&---------------------------------------------------------------------*
*&      Form  get_it_mha
*&---------------------------------------------------------------------*
FORM get_it_mha.

* Cost Center
  select kostl chnge sum( anzhl ) sum( netmh )
     into table it_mha_hr
     from ztco_mha
     where kokrs       =  p_kokrs
       and gjahr       =  p_bdatj
       and perid       =  p_poper
       and lgart       in ('1', '2', '3')
     group by kostl chnge.

  if p_leave = 'X'.
    select kostl chnge sum( anzhl ) sum( netmh )
       into table it_mha_abs
       from ztco_mha
       where kokrs       =  p_kokrs
         and gjahr       =  p_bdatj
         and perid       =  p_poper
         and lgart       =  'A'
    group by kostl chnge.

    loop at it_mha_abs.
      it_mha_hr-kostl = it_mha_abs-kostl.
      it_mha_hr-chnge = it_mha_abs-chnge.
      it_mha_hr-anzhl = - it_mha_abs-anzhl.
      it_mha_hr-netmh = - it_mha_abs-netmh.
      collect it_mha_hr.
    endloop.
  endif.

ENDFORM.                    " get_it_mha
*&---------------------------------------------------------------------*
*&      Form  collect_hr_mh
*&---------------------------------------------------------------------*
FORM collect_hr_mh.

  loop at it_mha_hr.
*  delta MH
*    clear it_shop_sum.
*    read table it_shop_sum with key kostl = it_mha_hr-kostl
*                           binary search.
*    check sy-subrc = 0 .
    if it_mha_hr-chnge = '' .
      it_mha-hr_mh = it_mha_hr-anzhl.
    else.
      it_mha-del_mh = it_mha_hr-anzhl.
    endif.
    it_mha-netmh  = it_mha_hr-netmh.

    it_mha-menge  = it_mha_hr-menge.  "OS

    it_mha-kostl  = it_mha_hr-kostl.

    collect it_mha.
    clear it_mha.
  endloop.

ENDFORM.                    " collect_hr_mh
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
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV
*&---------------------------------------------------------------------*
*&      Form  display_result
*&---------------------------------------------------------------------*
FORM display_result_act.

  PERFORM field_setting TABLES gt_fieldcat USING :
'ARTNR    '   'Product'       '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SHOP     '   'Shop'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KOSTL    '   'CC'            '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LLV_MATNR'   'MIP'           '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KALST    '   'Lvl'           '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MEEHT    '   'UoM'           '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'CURR_QTY '   'CURR_QTY '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'ADD_QTY  '   'ADD_QTY  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'WIP_CHG  '   'WIP_CHG  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MANU_QTY '   'MANU_QTY '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'GR_QTY   '   'GR_QTY   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'CURR_MH_T'   'CURR_MH_T'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'ADD_MH_T '   'ADD_MH_T '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'CURR_MH  '   'CURR_MH  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'ADD_MH   '   'ADD_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'GROSS_MH '   'GROSS_MH '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'DEL_MH   '   'DEL_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'DEL_RAT  '   'DEL_RAT  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'FINAL_MH '   'FINAL_MH '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'NET_MH   '   'NET_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PP_GR  '     'PP_GR  '       '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'DIR_MH   '   'DIR_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'DIR_MH2  '   'DIR_MH2  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'SEMI_MH  '   'SEMI_MH  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PC_MH    '   'PC_MH    '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'QC_MH    '   'QC_MH    '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PM_MH    '   'PM_MH    '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'INDIR_MH '   'INDIR_MH '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'ADMIN_MH '   'ADMIN_MH '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_DIR   '   'OS_DIR   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_SEMI  '   'OS_SEMI  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_PC    '   'OS_PC    '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_QC    '   'OS_QC    '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_PM    '   'OS_PM    '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_INDIR '   'OS_INDIR '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = it_ztco_mhv
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.


ENDFORM.                    " display_result
*&---------------------------------------------------------------------*
*&      Form  display_result_pln
*&---------------------------------------------------------------------*
FORM display_result_pln.

  PERFORM field_setting TABLES gt_fieldcat USING :
'ARTNR    '   'Product'      '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'SHOP     '   'Shop'         '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'LLV_MATNR'   'MIP'          '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KOSTL    '   'CC'           '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'KALST    '   'Lvl'          '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'MENGE    '   'Qty'          '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'CURR_MH  '   'CURR_MH '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'LOT_SIZE '   'LOT_SIZE'     '06' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MEEHT    '   'UoM'          '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'GR_QTY   '   'GR_QTY  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'SEMI_MH  '   'SEMI_MH '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PC_MH    '   'PC_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'QC_MH    '   'QC_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'PM_MH    '   'PM_MH   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'INDIR_MH '   'INDIR_MH'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'ADMIN_MH '   'ADMIN_MH'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_DIR   '   'OS_DIR  '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_SEMI  '   'OS_SEMI '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_PC    '   'OS_PC   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_QC    '   'OS_QC   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_PM    '   'OS_PM   '     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_INDIR '   'OS_INDIR'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'OS_ADMIN '   'OS_ADMIN'     '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = it_ztco_mhv_pln
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

ENDFORM.                    " display_result_pln

*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  read_production
*&---------------------------------------------------------------------*
FORM read_production.
  DATA : IT_PRODQTY_TEMP LIKE IT_PRODQTY OCCURS 0 WITH HEADER LINE.

  SELECT  B~BWKEY B~MATNR A~PERIO
          A~OUT_MENGE
          A~MEINH
    INTO CORRESPONDING FIELDS OF TABLE IT_PRODQTY_TEMP
    FROM
    ( ( CKMLMV003 AS A
    INNER JOIN CKMLMV001 AS B
       ON B~KALNR = A~KALNR_BAL )
    INNER JOIN CKMLMV013 AS C
       ON C~KALNR_PROC = A~KALNR_IN )
   WHERE A~MGTYP EQ '00001'
     AND A~GJAHR EQ P_BDATJ
     AND A~PERIO = P_POPER
     AND A~WERKS IN R_BWKEY
     AND B~BTYP  =  'BF'
     AND C~FLG_WBWG = 'X'
     AND C~AUTYP = '05'
     and B~matnr in s_artnr.


  LOOP AT IT_PRODQTY_TEMP.
    MOVE-CORRESPONDING IT_PRODQTY_TEMP TO IT_PRODQTY.
    COLLECT IT_PRODQTY. CLEAR IT_PRODQTY.
  ENDLOOP.

  SORT IT_PRODQTY BY MATNR.

ENDFORM.                    " read_production
*&---------------------------------------------------------------------*
*&      Form  get_global_variables
*&---------------------------------------------------------------------*
FORM get_global_variables.

  clear g_ktopl.
  select single ktopl into g_ktopl from t001
    where bukrs = p_kokrs.

  TYPES: BEGIN OF TY_BWKEY,
           BWKEY TYPE BWKEY,
         END OF TY_BWKEY.

  DATA   LT_BWKEY TYPE TABLE OF TY_BWKEY WITH HEADER LINE.

  REFRESH: LT_BWKEY, R_BWKEY.

  SELECT BWKEY INTO TABLE LT_BWKEY
    FROM T001K
   WHERE BUKRS = P_KOKRS.

  R_BWKEY-SIGN   = 'I'.
  R_BWKEY-OPTION = 'EQ'.

  LOOP AT LT_BWKEY.
    R_BWKEY-LOW = LT_BWKEY-BWKEY.
    APPEND R_BWKEY.
  ENDLOOP.

  CLEAR R_BWKEY.

ENDFORM.                    " get_global_variables
