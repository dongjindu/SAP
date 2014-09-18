*
* Spec: Andy Choi
*

report zacou120 .

tables: ztco_shop_sum, ztcou111, ztcou120.

parameters     : p_kokrs like ztco_shop_sum-kokrs memory id cac,
                 p_bdatj like ztco_shop_sum-bdatj memory id bdtj,
                 p_poper like ztco_shop_sum-poper memory id popr.
select-options : s_kzust for ztcou111-kzust.

parameters: p_save as checkbox.

ranges: r_poper for ztco_shop_sum-poper.
data: it_111  like ztcou111      occurs 0 with header line.

data: begin of it_111_sum occurs 0,
        prodh     type prodh_d,
        model(5)  type c,
        kzust     type kzust,
        chnge     type zchnge,  "AMT
        menge     type menge,   "Qty
        unita     type zchnge,  "Unit$
      end of it_111_sum.

data: begin of it_shop_sum occurs 0,
        prodh     type prodh_d,
        model(5)  type c,
        matnr     like ztco_shop_sum-llv_matnr,
        manu_qty  like ztco_shop_sum-manu_qty,
        ratio     like ztco_shop_sum-manu_qty,
      end of it_shop_sum.
data: begin of it_compn occurs 0,
        matnr     like ztco_shop_sum-llv_matnr,
        manu_qty  like ztco_shop_sum-manu_qty,
      end of it_compn.

data : begin of it_gr_sum    occurs 0,
         prodh     type prodh_d,
         model(5)   type c,
         vehicle(1) type c,
         out_menge  like ckmlmv003-out_menge,
         meins      like ckmlmv003-meins_pc,
         ratio      like ztco_shop_sum-manu_qty,
       end of  it_gr_sum.

start-of-selection.
  perform init_variable.
  perform read_data_from_db.

  perform collect_sum.

  perform calc_unit_cost_reduction.

  if p_save = 'X'.
    perform save_data.
  endif.

  perform display.

*&---------------------------------------------------------------------*
*&      Form  read_data_from_db
*&---------------------------------------------------------------------*
form read_data_from_db.
  data: begin of lt_shop occurs 0,
          par_werks like ztco_shop_sum-par_werks,
          artnr     like ztco_shop_sum-artnr,
*         verid     like ztco_shop_sum-verid,
          matnr     like ztco_shop_sum-llv_matnr,
          fevor     type fevor,
          sfepr     type sfepr,
          prodh     type prodh_d,
          manu_qty  like ztco_shop_sum-manu_qty,
        end of lt_shop.
  data : begin of it_ckmlmv003 occurs 0,
           bwkey      like ckmlmv001-bwkey,
           matnr      like ckmlmv001-matnr,
           meins      like ckmlmv003-meinh,
           out_menge  like ckmlmv003-out_menge,
           fevor     type fevor,
           sfepr     type sfepr,
           prodh     type prodh_d,
         end of  it_ckmlmv003.
  data : begin of lt_proc_gr occurs 0,
          par_werks type werks_d,
          artnr     type artnr,
          verid     type verid,
        end of lt_proc_gr.

  data : lt_ckmlmv003 like it_ckmlmv003 occurs 0 with header line.


  select * into table it_111 from ztcou111
    where kokrs = p_kokrs
      and bdatj = p_bdatj
      and poper = p_poper.
  sort it_111 by matnr.

  select par_werks artnr llv_matnr
         a~fevor sfepr
         c~prdha as prodh
         sum( manu_qty )
    into table lt_shop
    from ztco_shop_sum as a
      inner join marc as b
         on  a~par_werks = b~werks
         and a~artnr     = b~matnr
      inner join mara as c
        on b~matnr = c~matnr
    where kokrs = p_kokrs
      and bdatj = p_bdatj
      and poper in r_poper
      and typps = 'M'
    group by  par_werks artnr llv_matnr
              a~fevor sfepr
              c~prdha.

  loop at lt_shop.
    read table it_111 with key matnr = lt_shop-matnr binary search.
    check sy-subrc = 0.

    if lt_shop-sfepr = 'VEHI'.
      it_shop_sum-model    = lt_shop-artnr+6(2).
    else.
      it_shop_sum-model    = lt_shop-fevor.
    endif.

    it_shop_sum-prodh    = lt_shop-prodh.
    it_shop_sum-matnr    = lt_shop-matnr.
    it_shop_sum-manu_qty = lt_shop-manu_qty.
    collect it_shop_sum.

    move-corresponding lt_shop to lt_proc_gr.
    append lt_proc_gr. clear lt_proc_gr.
  endloop.


  sort lt_proc_gr.
  delete adjacent duplicates from lt_proc_gr.

* read GR data
  select  a~bwkey a~matnr
          b~meins_pc b~out_menge
          c~fevor c~sfepr
          d~prdha as prodh
    into table lt_ckmlmv003
    from ckmlmv001 as a
   inner join ckmlmv003 as b
      on a~kalnr    =  b~kalnr_bal
   inner join marc as c
      on a~bwkey    = c~werks
     and a~matnr    = c~matnr
   join mara as d
      on d~matnr = c~matnr

     for all entries in lt_proc_gr
   where a~bwkey    =  lt_proc_gr-par_werks
     and a~matnr    =  lt_proc_gr-artnr
*    and a~verid_nd =  lt_proc_gr-verid
     and a~btyp     =  'BF'
     and b~gjahr    =  p_bdatj
     and b~perio    in r_poper.


  loop at lt_ckmlmv003.
    move-corresponding lt_ckmlmv003 to it_ckmlmv003.

    collect it_ckmlmv003. clear it_ckmlmv003.

    it_gr_sum-prodh = lt_ckmlmv003-prodh.
    if lt_ckmlmv003-sfepr = 'VEHI'.
      it_gr_sum-model   = lt_ckmlmv003-matnr+6(2).
      it_gr_sum-vehicle = 'V'.
    else.
      it_gr_sum-model  = lt_ckmlmv003-fevor.
    endif.
    it_gr_sum-out_menge = lt_ckmlmv003-out_menge.
    collect it_gr_sum.

  endloop.


endform.                    " read_data_from_db
*&---------------------------------------------------------------------*
*&      Form  collect_sum
*&---------------------------------------------------------------------*
form collect_sum.
  data: l_idx like sy-tabix.


  loop at it_shop_sum.
    it_compn-matnr    = it_shop_sum-matnr.
    it_compn-manu_qty = it_shop_sum-manu_qty.
    collect it_compn.
  endloop.
  sort it_compn by matnr.

  loop at it_shop_sum.
    l_idx = sy-tabix.
    read table it_compn with key matnr = it_shop_sum-matnr
                        binary search.
    if it_compn-manu_qty > 0.
      it_shop_sum-ratio = it_shop_sum-manu_qty / it_compn-manu_qty.
    else.
      it_shop_sum-ratio = 1.
    endif.

    modify it_shop_sum index l_idx transporting ratio.
  endloop.

  sort it_shop_sum by matnr.
  loop at it_111.
    loop at it_shop_sum where matnr = it_111-matnr.
      it_111_sum-prodh = it_shop_sum-prodh.
      it_111_sum-model = it_shop_sum-model.
      it_111_sum-kzust = it_111-kzust.
      it_111_sum-chnge = it_111-chnge.
      collect it_111_sum.
    endloop.
    if sy-subrc <> 0.
      it_111_sum-model = space.
      it_111_sum-kzust = it_111-kzust.
      it_111_sum-chnge = it_111-chnge.
      collect it_111_sum.
    endif.
  endloop.

endform.                    " collect_sum
*&---------------------------------------------------------------------*
*&      Form  calc_unit_cost_reduction
*&---------------------------------------------------------------------*
form calc_unit_cost_reduction.
  data: l_idx like sy-tabix,
        l_qty like it_gr_sum-out_menge.

*-not assigned to "MODEL"
  clear l_qty.
  loop at it_gr_sum where vehicle = 'V'.
    l_qty = it_gr_sum-out_menge + l_qty.
  endloop.
  loop at it_gr_sum where vehicle = 'V'.
    l_idx = sy-tabix.
    it_gr_sum-ratio = it_gr_sum-out_menge / l_qty.
    modify it_gr_sum index l_idx transporting ratio.
  endloop.

  loop at it_111_sum where model = space.
    loop at it_gr_sum where vehicle = 'V'.
      it_111_sum-prodh = it_gr_sum-prodh.
      it_111_sum-model = it_gr_sum-model.
      it_111_sum-kzust = it_111_sum-kzust.
      it_111_sum-chnge = it_111_sum-chnge * it_gr_sum-ratio.
      collect it_111_sum.
    endloop.
  endloop.
  delete it_111_sum where model = space.

  loop at it_111_sum.
    l_idx = sy-tabix.

    read table it_gr_sum with key model = it_111_sum-model.
    if sy-subrc <> 0 or it_gr_sum-out_menge = 0.
      it_111_sum-menge = 1.
    else.
      it_111_sum-menge = it_gr_sum-out_menge.
    endif.

    it_111_sum-unita =  it_111_sum-chnge / it_111_sum-menge.
    modify it_111_sum index l_idx transporting unita menge.

  endloop.

endform.                    " calc_unit_cost_reduction
*&---------------------------------------------------------------------*
*&      Form  display
*&---------------------------------------------------------------------*
form display.
*--- ALV
  type-pools: slis.
  data : w_fieldcat type slis_t_fieldcat_alv with header line,
         w_eventcat type slis_t_event with header line,
         w_selfield type slis_selfield,
         w_sortcat  type slis_t_sortinfo_alv with header line,
         w_col_pos  type i,
         w_program  like sy-repid,
         w_top_of_page type slis_t_listheader,
         w_line1 type slis_listheader.

  data: gt_fieldcat type slis_t_fieldcat_alv,
        gs_layout   type slis_layout_alv,
        gt_sp_group type slis_t_sp_group_alv,
        gt_events   type slis_t_event,
        gt_sorts    type slis_t_sortinfo_alv with header line,
        gs_prnt     type slis_print_alv,
        g_repid     like sy-repid.
*---- ALV
  g_repid = sy-repid.

  perform field_setting(zcogsrev) tables gt_fieldcat using :
  'PRODH'     'ProdH'         '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
  'MODEL'     'Model'         '05' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
  'KZUST'     'KZUST'         '03' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
  'CHNGE'     'Amount'        '18' ' ' 'R'  ' '  ' '  '  ' '  ' ' ',
  'MENGE'     'Qty'           '14' ' ' 'R'  ' '  ' '  '  ' '  ' ' ',
  'UNITA'     'Amt/Unit'      '16' ' ' 'R'  ' '  ' '  '  ' '  ' ' '.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
            it_events          = w_eventcat[]
       tables
            t_outtab           = it_111_sum
       exceptions
            program_error      = 1
            others             = 2.

endform.                    " display
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
form save_data.
  delete from ztcou120
     where kokrs = p_kokrs
       and bdatj = p_bdatj
       and poper = p_poper.

  loop at it_111_sum.
    move-corresponding it_111_sum to ztcou120.
    ztcou120-mandt = sy-mandt.
    ztcou120-kokrs = p_kokrs.
    ztcou120-bdatj = p_bdatj.
    ztcou120-poper = p_poper.
    ztcou120-meins = 'EA'.
    insert ztcou120 from ztcou120.
  endloop.
endform.                    " save_data
*&---------------------------------------------------------------------*
*&      Form  init_variable
*&---------------------------------------------------------------------*
form init_variable.

  refresh r_poper.
  r_poper-option = 'BT'.
  r_poper-sign   = 'I'.
  r_poper-low = '001'.
  r_poper-high = p_poper.
  append r_poper.

endform.                    " init_variable
