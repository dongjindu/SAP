report z_wip_detail .

tables: ztco_shop_sum.
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

parameters: p_kokrs like ztco_shop_sum-kokrs.
parameters: p_bdatj like ztco_shop_sum-bdatj.
parameters: p_poper like ztco_shop_sum-poper.
parameters: p_pwip as checkbox.

select-options: s_artnr for ztco_shop_sum-artnr.

data: begin of itab occurs 0,
       artnr like ztco_shop_sum-artnr,
       verid like ztco_shop_sum-verid,
       bdatj like ztco_shop_sum-bdatj,
       poper like ztco_shop_sum-poper,
      end of itab.
data: begin of it_wip occurs 0,
       artnr like ztco_shop_sum-artnr,
       verid like ztco_shop_sum-verid,

       aufnr     like ztco_shop_sum-aufnr,
       typps     like ztco_shop_sum-typps,
       kstar     like ztco_shop_sum-kstar,
       resou     like ztco_shop_sum-resou,
       llv_matnr like ztco_shop_sum-llv_matnr,

       wip_amt   like ztco_shop_sum-wip_amt,
       wip_qty   like ztco_shop_sum-wip_qty,
       wip_pamt  like ztco_shop_sum-wip_amt,
       wip_pqty  like ztco_shop_sum-wip_qty,

       bdatj like ztco_shop_sum-bdatj,
       poper like ztco_shop_sum-poper,
      end of it_wip.

start-of-selection.

* by ig.moon 4/9/2008 {
* -
*perform get_recent_wip_period.
*perform get_shop_wip.

* +
  perform get_shop_wip_new.

* }

  perform display_out.


*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
form display_out.


  perform field_setting(zcogsrev) tables gt_fieldcat using :
 'ARTNR'     'Product'        '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'VERID'     'Verid'          '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',

 'AUFNR'     'PCC'            '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'TYPPS'     'Type'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'KSTAR'     'CostEl'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'RESOU'     'Resource'       '22' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'LLV_MATNR' 'Material'       '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'WIP_AMT'   'WIP amt'        '18' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'WIP_QTY'   'WIP qty'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

 'WIP_PAMT'  'Prev WIP$'      '18' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'WIP_PQTY'  'Prev WIP'       '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',

 'BDATJ'     'Year'           '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'POPER'     'Period'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       tables
            t_outtab           = it_wip
       exceptions
            program_error      = 1
            others             = 2.

endform.                    " display_out
*&---------------------------------------------------------------------*
*&      Form  get_recent_wip_period
*&---------------------------------------------------------------------*
form get_recent_wip_period.

  select artnr verid max( bdatj )
    into table itab
    from ztco_shop_sum
    where kokrs = p_kokrs
      and bdatj <= p_bdatj
      and artnr in s_artnr
    group by artnr verid.


  loop at itab.
    select max( poper ) into itab-poper
      from ztco_shop_sum
      where kokrs = p_kokrs
        and bdatj = itab-bdatj
        and artnr = itab-artnr
        and verid = itab-verid.
    modify itab.
  endloop.

endform.                    " get_recent_wip_period
*&---------------------------------------------------------------------*
*&      Form  get_shop_wip
*&---------------------------------------------------------------------*
form get_shop_wip.
  loop at itab.
    if p_pwip = 'X'.
      select
         artnr
         verid
         aufnr
         typps
         kstar
         resou
         llv_matnr
         wip_amt
         wip_qty
         wip_pamt
         wip_pqty
         bdatj
         poper

       appending table it_wip
       from ztco_shop_sum
       where kokrs = p_kokrs
         and artnr = itab-artnr
         and bdatj = itab-bdatj
         and poper = itab-poper
         and verid = itab-verid
         and ( wip_amt <> 0 or wip_pamt <> 0 ).

    else.
      select
         artnr
         verid
         aufnr
         typps
         kstar
         resou
         llv_matnr
         wip_amt
         wip_qty
         wip_pamt
         wip_pqty
         bdatj
         poper

       appending table it_wip
       from ztco_shop_sum
       where kokrs = p_kokrs
         and artnr = itab-artnr
         and bdatj = itab-bdatj
         and poper = itab-poper
         and verid = itab-verid
         and wip_amt <> 0.

    endif.
  endloop.

endform.                    " get_shop_wip
*&---------------------------------------------------------------------*
*&      Form  get_shop_wip_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_shop_wip_new.

  if p_pwip = 'X'.
    select
       artnr
       verid
       aufnr
       typps
       kstar
       resou
       llv_matnr
       wip_amt
       wip_qty
       wip_pamt
       wip_pqty
       bdatj
       poper

     appending table it_wip
     from ztco_shop_sum
     where kokrs = p_kokrs
       and artnr in s_artnr
       and bdatj = p_bdatj
       and poper = p_poper
       and ( wip_amt <> 0 or wip_pamt <> 0 ).

  else.
    select
       artnr
       verid
       aufnr
       typps
       kstar
       resou
       llv_matnr
       wip_amt
       wip_qty
       wip_pamt
       wip_pqty
       bdatj
       poper

     appending table it_wip
     from ztco_shop_sum
     where kokrs = p_kokrs
       and artnr in s_artnr
       and bdatj = p_bdatj
       and poper = p_poper
       and wip_amt <> 0.

  endif.

endform.                    " get_shop_wip_new
