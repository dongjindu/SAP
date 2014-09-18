*&---------------------------------------------------------------------*
*& Recalculation program for Hyundai for the period 2007006
*&---------------------------------------------------------------------*
*& This works according to following assumptions:
*&    - lotsize = 1 and no GR reversals. Thus, an order which has one
*&      GR confirmation will be considered 'closed' for WIP.
*&    - no scrap, no variances - these quantities won't be computed
*&      for the new WIP
*&    - no special components are used
*&    - no activities will be considered
*&    - for the prev per 2007005 WIP calculation is correct
*&    - Updates the GMSUM and ISTMN for the period 2007006
*&    - GMPER for the period 2007006 is correct
*&---------------------------------------------------------------------*

report  Z_CPZP_CORRETION_581661 line-size 126.
type-pools icon.
tables: PPC_MAT.

selection-screen begin of block b1 with frame title text-sl1.
parameters:
  p_matnr  type matnr,
  p_werks  type werks_d.
*  p_gjper  type co_gjper.

selection-screen end of block b1.
selection-screen begin of block b2 with frame title text-sl2.
parameters:
  p_debug  type c as checkbox,
  p_atrep  type c as checkbox.
selection-screen end of block b2.
select-options: s_compn for PPC_MAT-matnr.
parameters:
  p_gjper  type co_gjper default '2007006'.

*DATA: p_gjper  type co_gjper.
*p_gjper = '2007006'.
define u_break.
  if not p_debug is initial.
    break-point.
  endif.
end-of-definition.
* --
data:
  gf_accassobj type ppc_accassobj_int,
  gf_objnr     type j_objnr,
  gf_datum     type budat,
  gf_frdatum  type budat,
  gf_icon      like icon_red_light,

  begin of gs_stats,
      kkey     type c,
      text(30) type c,
      val1     type i,
      val2     type i,
  end of gs_stats,
  gt_stats like table of gs_stats,

  begin of gs_order,
      orderid  type ppc_orderid,
      kdauf    type kdauf,
      ordernr  type ppc_ordernr,
  end of gs_order,
*  gt_openorder like table of gs_order,
  gt_closedorder like table of gs_order,
  gt_varorder  like table of gs_order,

  begin of gs_comp,
      matnr    type matnr,
      werks    type werks_d,
      sobkz    type sobkz,
      kzbws    type kzbws,
      kdauf    type kdauf,
      kdpos    type kdpos,
      confunit type PPC_CONFUNIT,
      quant    type menge_pos,
  end of gs_comp,
  gt_comp like hashed table of gs_comp
        with unique key matnr werks sobkz kzbws kdauf kdpos confunit,

  begin of gs_finaltb,
      matnr    type matnr,
      kdauf    type kdauf,
      kdpos    type kdpos,
      objnr    type objnr,
      f_objnr  type f_objnr,
      gjper    type co_gjper,
      quant    type menge_pos,
      istmn    type ist_menge,
      gmper    type g_men_per,
      gmsum    type g_men_sum,
      status   type c,
      delta    type menge_pos,
  end of gs_finaltb,
  gs_hidtab  like gs_finaltb,
  gt_finaltb like table of gs_finaltb.


*---------------
load-of-program.
*---------------
  set run time clock resolution low.


*-------------------
at selection-screen.
*-------------------
  p_gjper  = '2007006'.
  perform check_input_val  using p_matnr
                                 p_werks
                                 p_gjper
                        changing gf_datum
                                 gf_frdatum
                                 gf_accassobj.

*------------------
start-of-selection.
*------------------
*CCC
  perform enqueue.
*CCC
  perform find_closed_orders using p_matnr
                                 p_werks
                                 gf_datum
                                 gf_frdatum
                        changing gt_closedorder
                                 gt_stats.

  perform compute_bfl_qty  using gf_datum
                                 gt_closedorder
                        changing gt_comp
                                 gt_stats.

  perform update_bfl_cpzp using gf_accassobj
                                 p_gjper
                                 gt_comp
                        changing gf_objnr
                                 gt_finaltb
                                 gt_stats.
  perform write_log using gt_stats '1'.


*  perform write_output     using p_matnr
*                                 p_werks
*                                 gf_accassobj
*                                 gf_objnr
*                                 p_eonly
*                                 gt_openorder
*                                 gt_finaltb
*                                 gt_stats.
*

*-----------------
at line-selection.
*-----------------

  case sy-lsind.

    when 1.
      if not gs_hidtab is initial.
*        perform popup_detail changing gs_hidtab.
      elseif not gs_stats is initial.
        perform write_log using gt_stats '2'.
      endif.

    when 2.
      "do nothing

  endcase.



*&---------------------------------------------------------------------*
*&      Form  check_input_val
*&---------------------------------------------------------------------*
form check_input_val using    pf_matnr
                              pf_werks
                              pf_gjper
                     changing pf_datum
                              pf_frdatum
                              pf_accassobj.

  data:
    lf_year(4) type n,
    lf_peri(3) type n,
    lf_datum   type budat,
    ls_head    type ppc_head,
    lp_kokrs   like aufk-kokrs,   "Kostenrechungskreis
    lp_variant like tka01-lmona.  "GJahr-Variante


  u_break.

* get accounting object
  select single accassobj
      from ppc_ord_inf
      into pf_accassobj
      where materialnr = pf_matnr
        and plant      = pf_werks.

* get limit date
  lf_year = pf_gjper div 1000.
  lf_peri = pf_gjper - ( lf_year * 1000 ).


* Zuerst Kostenrechnungskreis aus Werk bestimmen
  call function 'RM_KOKRS_TO_PLANT_FIND'
       exporting
            werks   = pf_werks
       importing
            e_kokrs = lp_kokrs
       exceptions
            others  = 1.
  if sy-subrc ne 0.
    message id sy-msgid type sy-msgty number sy-msgno       "P45K072474
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4        "P45K072474
            raising wrong_input.                            "P45K072474
  endif.

* GJahr-Variante bestimmen
  select single lmona from tka01 into lp_variant
                                 where kokrs = lp_kokrs.

* To get the posting period date
  call function 'G_POSTING_DATE_OF_PERIOD_GET'
       exporting
            period              = lf_peri
            variant             = lp_variant
            year                = lf_year
       importing
            from_date           = pf_frdatum
            to_date             = pf_datum
       exceptions
            period_not_defined  = 1
            variant_not_defined = 2
            others              = 3.





* check to see if there are any open backflushes
  select single * from ppc_head into ls_head
      where postdate le gf_datum
        and accassobj eq pf_accassobj
        and ( ( flg_synch ne 'X' and flg_synch ne 'V' )
              or ( flg_asynch ne 'X' and flg_asynch ne 'V' ) ) .
  if sy-dbcnt gt 0.
    "ups! unprocessed PPCGO
    message e999(pp) with 'Before running the report, run PPCGO'(m01)
                          pf_matnr 'period'(m02) pf_gjper.
  endif.

endform.                    " check_input_val


*&---------------------------------------------------------------------*
*&      Form  find_open_orders
*&---------------------------------------------------------------------*
form find_closed_orders  using    pf_matnr     type matnr
                                pf_werks     type werks_d
                                pf_datum     type budat
                                pf_frdatum     type budat
                       changing pt_closedorder like gt_closedorder
                                pt_stats     like gt_stats.

  data:
    ls_stats   like gs_stats.
  ranges:
    lt_selopt  for ppc_ord_inf-orderid.

  u_break.
  get run time field ls_stats-val1.

* get all open orders by a single select statement
  select orderid ordernr
      from ppc_ord_inf as o
      into corresponding fields of table pt_closedorder
      where materialnr = pf_matnr
        and plant      = pf_werks
        and exists ( select * from ppc_head
                     where orderid eq o~orderid
                       and postdate le pf_datum
                       and postdate ge pf_frdatum
                       and flg_gr_head eq 'X'
                    ).

* stats and log
  get run time field ls_stats-val2.
  ls_stats-kkey = 'R'.
  ls_stats-text = 'Order selection took:'.
  append ls_stats to pt_stats.
  clear ls_stats.
  ls_stats-kkey = 'A'.
  describe table pt_closedorder lines ls_stats-val1.
  append ls_stats to pt_stats.
  clear ls_stats.

endform.                    " find_open_orders



*&---------------------------------------------------------------------*
*&      Form  compute_bfl_qty
*&---------------------------------------------------------------------*
form compute_bfl_qty  using    pf_datum     type budat
                               pt_closedorder like gt_closedorder
                      changing pt_comp      like gt_comp
                               pt_stats     like gt_stats.

  data:
    ls_order   like gs_order,
    ls_comp    like gs_comp,
    ls_stats   like gs_stats,
    ls_mats    type ppc_material_components,
    lt_repts   type ppc_t_reppoint,
    lt_mats    type ppc_t_material_components,
    lt_matsvar type ppc_t_material_components,
    lt_matsrev type ppc_t_material_components,
    lt_matreva type ppc_t_material_components,
    lf_year(4) type n,
    lf_peri(3) type n,
    lf_datum   type budat.

  u_break.
  get run time field ls_stats-val1.

* Process orders one by one
  loop at pt_closedorder into ls_order.

    " get backflushed components
    perform get_comp_per_order using ls_order-orderid
                                     pf_datum
                            changing lt_mats
                                     lt_matsvar
                                     lt_matsrev
                                     lt_matreva.

    " collect the issued components
    loop at lt_mats into ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ls_mats-quantity.
      ls_comp-confunit = ls_mats-UNIT_OF_MEASURE.
      collect ls_comp into pt_comp.
    endloop.
    " subtract the variance values,
    loop at lt_matsvar into ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ( -1 ) * ls_mats-quantity.
      ls_comp-confunit = ls_mats-UNIT_OF_MEASURE.
      collect ls_comp into pt_comp.
    endloop.
    " subtract the reversed GI's
    loop at lt_matsrev into ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ( -1 ) * ls_mats-quantity.
      ls_comp-confunit = ls_mats-UNIT_OF_MEASURE.
      collect ls_comp into pt_comp.
    endloop.
    " add the variance reversed values,
    loop at lt_matreva into ls_mats.
      ls_comp-matnr = ls_mats-mat_number.
      ls_comp-werks = ls_mats-plant.
      ls_comp-sobkz = ls_mats-special_stock.
      ls_comp-kzbws = ls_mats-special_stock_val.
      ls_comp-kdauf = ls_mats-sales_doc.
      ls_comp-kdpos = ls_mats-sales_doc_item.
      ls_comp-quant = ls_mats-quantity.
      ls_comp-confunit = ls_mats-UNIT_OF_MEASURE.
      collect ls_comp into pt_comp.
    endloop.

  endloop.

  sort pt_comp by matnr ascending.

* stats and log
  get run time field ls_stats-val2.
  ls_stats-kkey = 'R'.
  ls_stats-text = 'Component selection took:'.
  append ls_stats to pt_stats.
  clear ls_stats.
  ls_stats-kkey = 'B'.
  describe table pt_comp lines ls_stats-val1.
  append ls_stats to pt_stats.
  clear ls_stats.


endform.                    " compute_bfl_qty


*&---------------------------------------------------------------------*
*&      Form  compare_bfl_cpzp
*&---------------------------------------------------------------------*
form update_bfl_cpzp  using    pf_accassobj type ppc_accassobj_int
                                pf_gjper     type co_gjper
                                pt_comp      like gt_comp
                       changing pf_objnr     type j_objnr
                                pt_finaltb   like gt_finaltb
                                pt_stats     like gt_stats.

  data:
    lf_aufnr   type aufnr,
    lf_kaln1   type ck_kalnr1,
    lf_fobjnr  type f_objnr,
    lf_quant   type ist_menge,
    ls_comp    like gs_comp,
    ls_finaltb like gs_finaltb,
    ls_stats   like gs_stats,
    ls_cpzp    type cpzp,
    lt_cpzp    type table of cpzp,
    ls_cpzp_prv    type cpzp,
    lt_cpzp_prv type table of cpzp.
DATA :
    nistmn    type ist_menge,
    ngmsum    type g_men_sum,
    iv_err    type sy-subrc,
    iv_upd    type sy-subrc,
    iv_prvper type co_gjper.

* Assuming the prev period as 2007005
    if pf_gjper = '2007006'.
      iv_prvper = '2007005'.
    endif.

  u_break.
  get run time field ls_stats-val1.
  iv_err  = 0.
* Get accounting order number anf cpzp object number
  call function 'QRP_QRP002_READ'
       exporting
            if_cc_guid = pf_accassobj
       importing
            ef_aufnr   = lf_aufnr
       exceptions
            others     = 1.
  if sy-subrc is initial.
    call function 'QRP_APO_PKOSA_AUFNR_TO_OBJNR'
         exporting
              if_pkosa_aufnr = lf_aufnr
         importing
              ef_pkosa_objnr = pf_objnr.
  endif.
* ... and buffer cpzp table
  select * from cpzp into table lt_cpzp
      where objnr = pf_objnr
        and gjper = pf_gjper.
*.....get prev period cpzp value for the calculation of istmn
* ... and buffer cpzp table
  select * from cpzp into table lt_cpzp_prv
      where objnr = pf_objnr
        and gjper = iv_prvper.


* Build final table
  loop at pt_comp into ls_comp.

*   ... write already determined values
    ls_finaltb-matnr = ls_comp-matnr.
    ls_finaltb-quant = ls_comp-quant.

*   ... find objnr from comat
    call function 'QRP_APO_COMP_OBJNR_ENCODE'
         exporting
              if_matnr   = ls_comp-matnr
              if_werks   = ls_comp-werks
              if_vbeln   = ls_comp-kdauf
              if_posnr   = ls_comp-kdpos
              if_kzbws   = ls_comp-kzbws
              if_sobkz   = ls_comp-sobkz
         importing
              ef_f_objnr = lf_fobjnr
              ef_kaln1   = lf_kaln1
         exceptions
              others     = 1.

    if sy-subrc ne 0.

      ls_finaltb-kdauf   = ls_comp-kdauf.
      ls_finaltb-kdpos   = ls_comp-kdpos.
      ls_finaltb-f_objnr = 'Not found!'.
      ls_finaltb-status  = 'P'.

    else.

*     ... find cpzp data
      read table lt_cpzp into ls_cpzp
              with key objnr   = pf_objnr
                       f_objnr = lf_fobjnr.
      if sy-subrc eq 0.
        IF LS_CPZP-MEINH NE ls_comp-CONFUNIT.
          MESSAGE E001(00) WITH 'Uom problem'
                       ls_comp-matnr.
        ENDIF.
        ls_finaltb-objnr   = ls_cpzp-objnr.
        ls_finaltb-f_objnr = ls_cpzp-f_objnr.
        ls_finaltb-gjper   = ls_cpzp-gjper.
        ls_finaltb-gmper   = ls_cpzp-gmper.
        if ls_cpzp-gmsum <> ls_comp-quant.
          ngmsum   = ls_comp-quant.
* For the current problem
*          nistmn =   ls_cpzp-istmn - ( ls_cpzp-gmsum - ls_comp-quant ).
* generic istmn
          READ table lt_cpzp_prv into ls_cpzp_prv
              with key objnr   = pf_objnr
                       f_objnr = lf_fobjnr.
          If sy-subrc = 0.
            nistmn = ( ls_cpzp_prv-istmn - ls_cpzp_prv-gmsum ) +
                       ls_cpzp-gmper.
          endif.

          write: / 'Assembly:', p_matnr color 5 inverse intensified,
           / 'Plant:   ', p_werks color 5 inverse intensified,
           / 'Acc. obj:', pf_accassobj color 5 inverse intensified,
           / 'Objnr:   ', pf_objnr color 5 inverse intensified,
           / 'F_Objnr:   ', lf_fobjnr color 5 inverse intensified,
           / 'GMSUM:   ', ngmsum color 5 inverse intensified,
           / 'ISTMN:   ', nistmn color 5 inverse intensified.
          if p_atrep eq 'X'.

             update cpzp
              set gmsum   = ngmsum
                  istmn = nistmn
              where objnr = pf_objnr
                and f_objnr = lf_fobjnr
               and gjper = pf_gjper.
               If sy-subrc <> 0.
                 iv_err = 1.
               endif.
          endif.
           ls_finaltb-gmsum   = ngmsum.
           ls_finaltb-istmn   = nistmn.
           ls_finaltb-status   = 'X'.
           iv_upd = 1.
        ELSE.

*     This line is ok...
          ls_finaltb-gmsum   = ls_cpzp-gmsum.
          ls_finaltb-istmn   = ls_cpzp-istmn.
          ls_finaltb-status   = 'Y'.
          WRITE:/ 'The component is consistent:', ls_comp-matnr color 6
                 inverse intensified.

        ENDIF.
        IF NOT ls_finaltb IS INITIAL.
          append ls_finaltb to pt_finaltb.
          clear ls_finaltb.
        ENDIF.
      ENDIF.
    ENDIF.
*   append line to the final table
    clear: nistmn, ngmsum.
  endloop.
* Do commit if everything is right.
  IF p_atrep eq 'X'.
* if no error happened during updation
    IF iv_err = 0.
* Atleast one record modified.
      IF iv_upd = 1.
        commit work.
      ENDIF.
    ELSE.
        rollback work.
    ENDIF.
  ENDIF.

* stats and log
  get run time field ls_stats-val2.
  ls_stats-kkey = 'R'.
  ls_stats-text = 'CPZP comparison took:'.
  append ls_stats to pt_stats.
  clear ls_stats.

endform.                    " compare_bfl_cpzp


*---------------------------------------------------------------------*
*       FORM get_comp_per_order                                       *
*---------------------------------------------------------------------*
form get_comp_per_order
         using  if_orderid  type ppc_orderid
                if_postdate type budat
       changing et_material_components type ppc_t_material_components
                et_rev_mat_components  type ppc_t_material_components
                et_mat_comp_var        type ppc_t_material_components
                et_rev_mat_comp_var    type ppc_t_material_components.


  constants c_gmove_ind_0 type ppc_gmove_ind value '0'.
  data:
    begin of ls_ext_mat_comp,
      mat_comp like ppc_material_components,
      head_sales_doc type kdauf,
      head_sales_doc_item type kdpos,
      head_wbs_elem type ps_psp_pnr,
      flg_reversal type xflag,
    end of ls_ext_mat_comp.


* ... Initialize output
  refresh et_material_components.
  refresh et_rev_mat_components.
  refresh et_mat_comp_var.
  refresh et_rev_mat_comp_var.


* ... Lock the orderid
  call function 'ENQUEUE_E_PPC_ORDERID'
       exporting
            mode_ppc_ord_inf = 'E'
            mandt            = sy-mandt
            orderid          = if_orderid
            _scope           = '1'
            _wait            = 'X'
       exceptions
            foreign_lock     = 1
            system_failure   = 2
            others           = 3.

  if sy-subrc eq 1.
    message e550(ppc1dm) raising lock_error.
  elseif sy-subrc gt 2.
    message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          raising lock_error.
  endif.


* ... Select components
  select
    t1~reppoint
    t2~matnr
    t2~werks
    t2~lgort
    t2s~charg
    t2~prvbe
    t2~gmove_ind
    t2~sobkz
    t2~kzbws
    t2~kzvbr
    t2s~kdauf
    t2s~kdpos
    t2s~pspnr
    t2s~calcnr
    sum( t4~confquant ) as confquant
    sum( t5~delta_confquant )  as delta_confquant
    t4~confunit
    t1~kdauf
    t1~kdpos
    t1~pspnr
    t1~flg_reversal
  into
    (ls_ext_mat_comp-mat_comp-reppoint,
     ls_ext_mat_comp-mat_comp-mat_number,
     ls_ext_mat_comp-mat_comp-plant,
     ls_ext_mat_comp-mat_comp-storage_loc,
     ls_ext_mat_comp-mat_comp-batch,
     ls_ext_mat_comp-mat_comp-supply_area,
     ls_ext_mat_comp-mat_comp-gmove_ind,
     ls_ext_mat_comp-mat_comp-special_stock,
     ls_ext_mat_comp-mat_comp-special_stock_val,
     ls_ext_mat_comp-mat_comp-consumpt_posting,
     ls_ext_mat_comp-mat_comp-sales_doc,
     ls_ext_mat_comp-mat_comp-sales_doc_item,
     ls_ext_mat_comp-mat_comp-wbs_elem,
     ls_ext_mat_comp-mat_comp-costing_num,
     ls_ext_mat_comp-mat_comp-quantity,
     ls_ext_mat_comp-mat_comp-delta_quantity,
     ls_ext_mat_comp-mat_comp-unit_of_measure,
     ls_ext_mat_comp-head_sales_doc,
     ls_ext_mat_comp-head_sales_doc_item,
     ls_ext_mat_comp-head_wbs_elem,
     ls_ext_mat_comp-flg_reversal)
  from
      ( ( ( ppc_head as t1 inner join ppc_conf_mat as t4
          on t1~headid = t4~headid )
        left outer join ppc_conf_mat_var as t5
          on t4~headid = t5~headid and
             t4~accid = t5~accid )
        inner join ppc_mat_det as t2s
          on t4~accid = t2s~accid )
        inner join ppc_mat as t2
          on t2s~matid = t2~matid
  where
        t1~orderid eq if_orderid and
        t2~gmove_ind eq c_gmove_ind_0 and
        t1~postdate le if_postdate
        and t2~matnr in s_compn
  group by
      t1~reppoint
      t2~matnr
      t2~werks
      t2~lgort
      t2s~charg
      t2~prvbe
      t2~gmove_ind
      t2~sobkz
      t2~kzbws
      t2~kzvbr
      t2s~kdauf
      t2s~kdpos
      t2s~pspnr
      t2s~calcnr
      t4~confunit
      t1~kdauf
      t1~kdpos
      t1~pspnr
      t1~flg_reversal
      %_hints oracle '&SUBSTITUTE LITERALS&'
              mssqlnt '&SUBSTITUTE LITERALS&'
              db2     '&SUBSTITUTE LITERALS&'.


*   AMF_CONVERT
    if not ls_ext_mat_comp-mat_comp-special_stock is initial and
       not ls_ext_mat_comp-head_sales_doc is initial and
           ls_ext_mat_comp-mat_comp-sales_doc is initial.
      move ls_ext_mat_comp-head_sales_doc to
           ls_ext_mat_comp-mat_comp-sales_doc.
    endif.
    if not ls_ext_mat_comp-mat_comp-special_stock is initial and
       not ls_ext_mat_comp-head_sales_doc_item is initial and
           ls_ext_mat_comp-mat_comp-sales_doc_item is initial.
      move ls_ext_mat_comp-head_sales_doc_item to
           ls_ext_mat_comp-mat_comp-sales_doc_item.
    endif.
    if not ls_ext_mat_comp-mat_comp-special_stock is initial and
       not ls_ext_mat_comp-head_wbs_elem is initial and
           ls_ext_mat_comp-mat_comp-wbs_elem is initial.
      move ls_ext_mat_comp-head_wbs_elem to
             ls_ext_mat_comp-mat_comp-wbs_elem.
    endif.


*   Build output tables: first check reversal flag
    if ls_ext_mat_comp-flg_reversal is initial.

*     Direct posting
      append ls_ext_mat_comp-mat_comp to et_material_components.
*     When variances found, fill the export table accordingly
      if not ls_ext_mat_comp-mat_comp-delta_quantity is initial.
        move ls_ext_mat_comp-mat_comp-delta_quantity to
             ls_ext_mat_comp-mat_comp-quantity.
        clear ls_ext_mat_comp-mat_comp-delta_quantity.
        append ls_ext_mat_comp-mat_comp to et_mat_comp_var.
      endif.

    else.

*     Reversal
      append ls_ext_mat_comp-mat_comp to et_rev_mat_components.
*     When variances found, fill the export table accordingly
      if not ls_ext_mat_comp-mat_comp-delta_quantity is initial.
        move ls_ext_mat_comp-mat_comp-delta_quantity to
             ls_ext_mat_comp-mat_comp-quantity.
        clear ls_ext_mat_comp-mat_comp-delta_quantity.
        append ls_ext_mat_comp-mat_comp to et_rev_mat_comp_var.
      endif.

    endif.

    clear ls_ext_mat_comp.

  endselect.

* ... Unlock the order
  call function 'DEQUEUE_E_PPC_ORDERID'
       exporting
            mode_ppc_ord_inf = 'E'
            mandt            = sy-mandt
            orderid          = if_orderid
            _scope           = '1'.

endform.


*&---------------------------------------------------------------------*
*&      Form  write_output
*&---------------------------------------------------------------------*
form write_output  using    pf_matnr     type matnr
                            pf_werks     type werks_d
                            pf_accassobj type ppc_accassobj_int
                            pf_objnr     type j_objnr
                            pf_eonly     type c
                            pt_openorder like gt_closedorder
                            pt_finaltb   like gt_finaltb
                            pt_stats     like gt_stats.

  data:
    ls_finaltb like gs_finaltb.

  skip.
  write: / 'Assembly:', pf_matnr color 5 inverse intensified,
         / 'Plant:   ', pf_werks color 5 inverse intensified,
         / 'Acc. obj:', pf_accassobj color 5 inverse intensified,
         / 'Objnr:   ', pf_objnr color 5 inverse intensified.
  skip.

  perform write_log using pt_stats '1'.

  skip.
  uline at 1(126). format intensified color 4.
  write: /1 '|', 3 'matnr', 20 'f_objnr',
         50 'quant', 68 'istmn', 88 'gmper',
         107 'gmsum', 113 '|', 120 'delta', 126 '|'.
  new-line.
  uline at 1(126). format intensified color off.

  format intensified off.
  loop at pt_finaltb into ls_finaltb.
    if not pf_eonly is initial and ls_finaltb-status is initial.
      continue.
    endif.
    case ls_finaltb-status.
      when 'P' or 'Y'.
        format color 3.
      when 'C'.
        format color 7.
      when 'X' or 'F'.
        format color 6.
      when 'S'.
        format color 5.
      when others.
        "nothing
    endcase.
    write: /3 ls_finaltb-matnr, 20 ls_finaltb-f_objnr,
           36 ls_finaltb-quant, 56 ls_finaltb-istmn,
           76 ls_finaltb-gmper, 95 ls_finaltb-gmsum.
    format color off.
    case ls_finaltb-status.
      when 'F'.
        gf_icon = icon_red_light.
        format hotspot on.
        write: 115 gf_icon as icon.
        format hotspot off.
      when 'X' or 'C'.
        gf_icon = icon_yellow_light.
        format hotspot on.
        write: 115 gf_icon as icon.
        format hotspot off.
      when 'S'.
        gf_icon = icon_green_light.
        write: 115 gf_icon as icon.
    endcase.
    write: 120 ls_finaltb-delta left-justified.
    move-corresponding ls_finaltb to gs_hidtab.
    hide gs_hidtab.
  endloop.

  clear gs_hidtab.

  skip.
  uline at 1(70). new-line.
  format intensified color 4.
  write:  '|', 'Legend', 70 '|'.
  format intensified color off.
  new-line. uline at 1(70).
  format intensified off.
  write: / '|', 5 'F_OBJNR error  ' color 3,
      'Could not find OBJNR for the component'(l01), 70 '|'.
  write: / '|', 5 'CPZP error     ' color 7,
      'A WIP entry without reference to an open order'(l02), 70 '|'.
  write: / '|', 5 'WIP value error' color 6,
      'The WIP is not consistent with backflushes'(l03), 70 '|'.
  write: / '|', 5 'Error corrected' color 5,
      'WIP inconsistency was corrected - CPZP adjusted', 70 '|'.
  new-line. uline at 1(70).

endform.                    " write_output


*&---------------------------------------------------------------------*
*&      Form  write_log
*&---------------------------------------------------------------------*
form write_log  using  pt_stats like gt_stats
                       pf_level type c.


  if pf_level ne '1'.
    skip 1.
    uline (25).
    write: / '| DETAILED INFO:', 25 '|'.
    uline /(25).
    skip 1.
  endif.

* write orders
  read table pt_stats with key kkey = 'A' into gs_stats.
  write: / 'Open orders found: ',
           gs_stats-val1 left-justified.
  hide gs_stats.
* write orders
  read table pt_stats with key kkey = 'B' into gs_stats.
  write:  'Number of aggregated components checked: ',
           gs_stats-val1 left-justified.
  hide gs_stats.

* write detailed timestamps
  check pf_level ne '1'.
  skip 2.
  loop at pt_stats into gs_stats where kkey = 'R'.
    subtract gs_stats-val1 from gs_stats-val2.
    write: / gs_stats-text, gs_stats-val2, 'milisecs.'.
  endloop.

endform.                    " write_log



*CCC
*&---------------------------------------------------------------------*
*&      Form  enqueue
*&---------------------------------------------------------------------*
form enqueue .

* LOCK SYNC. PPC PROCESS
  CALL FUNCTION 'ENQUEUE_E_PPC_ORDERID'
       EXPORTING
            mode_ppc_ord_inf = 'E'
            mandt            = sy-mandt
            _scope           = '2'
            _wait            = 'X'
       EXCEPTIONS
            foreign_lock     = 1
            system_failure   = 2
            OTHERS           = 3.
  If sy-subrc ne 0.
    MESSAGE a507(PPC1DM) WITH sy-msgv1 .
  endif.
* LOCK PPCGO RUN
  CALL FUNCTION 'ENQUEUE_E_PPC_CONF_MAT'
       EXPORTING
            MODE_PPC_CONF_MAT = 'E'
            MANDT             = SY-MANDT
            _SCOPE            = '2'
            _WAIT             = 'X'
       EXCEPTIONS
            FOREIGN_LOCK      = 1
            SYSTEM_FAILURE    = 2
            OTHERS            = 3.
  If sy-subrc ne 0.
    MESSAGE A020(PPC1PR) WITH SY-MSGV1 .
  endif.

endform.                    " enqueue
*CCC
