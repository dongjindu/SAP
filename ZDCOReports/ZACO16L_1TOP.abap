*----------------------------------------------------------------------*
*   INCLUDE ZACO16L_1TOP                                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
tables : tka01, t134, aufk, cska, cobk, coep, mara, mseg, ztco_abispost,
sscrfields.

** Internal Table
* For I/O COEP
data : begin of it_io_coep occurs 1000.
data :  matnr  like coep-matnr,
        stype  like ztco_abispost-stype,
        werks  like coep-werks,
        wkgbtr like coep-wkgbtr,
        waers  like tka01-waers,
        fevor  like marc-fevor,
        objnr  like coep-objnr,
        kstar  like coep-kstar,
        refbn  like covp-refbn,
        gjahr  like covp-gjahr,
        buzei  like covp-buzei,
* Qty
        mefbtr like coep-mefbtr,
        mbgbtr like coep-mbgbtr,
        mbfbtr like coep-mbfbtr,
        meinb  like coep-meinb.
data : end of   it_io_coep.
* For PCC order and material Information
data : begin of it_pcc_mat occurs 500,
        matnr  like coep-matnr,
        werks  like coep-werks,
        fevor  like marc-fevor,
        aufnr  like aufk-aufnr,
        objnr  like coep-objnr,
        verid  type verid,
        categ(3) type c,          "DI, REM(MTS), MTO
       end of   it_pcc_mat.
* For PCC B/F data
data : begin of it_pcc_coep occurs 1000.
        include structure it_io_coep.
*        INCLUDE STRUCTURE COEP.
data :  rate_child    like ztco_abispost-rate_child.
data :  mbg_int       like ztco_abispost-mbgbtr.
data : end of   it_pcc_coep.
data : begin of it_tot_rate occurs 0.
data :
*       KSTAR  LIKE COEP-KSTAR,
        werks  like coep-werks,
        objnr  like coep-objnr,
        fevor  like marc-fevor,
        kstar_rate    like ztco_abispost-kstar_rate.
data : end of  it_tot_rate.
* For POSTING
data : begin of it_post occurs 500.
        include structure ztco_abispost.
data : end of it_post.

data : begin of it_saved occurs 500.
        include structure ztco_abispost.
data : end of it_saved.

*DATA:  it_post_tmp LIKE it_post OCCURS 0 WITH HEADER LINE.
data : begin of it_post_fin occurs 500,
        kstar      like ztco_abispost-kstar,
        matnr      like ztco_abispost-matnr,
        stype      like ztco_abispost-stype,
        io_aufnr   like ztco_abispost-io_aufnr,
        pcc_aufnr  like ztco_abispost-pcc_aufnr,
        chg_wkgbtr like ztco_abispost-chg_wkgbtr,
        waers      like ztco_abispost-waers,
        belnr      like ztco_abispost-belnr,
        mbgbtr     like ztco_abispost-mbgbtr,
        meinb      like ztco_abispost-meinb,
        line_no    like sy-tabix,
        remarks(40),
        kokrs      like ztco_abispost-kokrs,
        gjahr      like ztco_abispost-gjahr,
        period     like ztco_abispost-period,
        versn      like ztco_abispost-versn,
       end of it_post_fin.
data : it_l_aufk like standard table of aufk
                 with header line .

* Type for ALV
types: begin of ty_out.
include  structure ztco_abispost.
types icon type icon_d.
types chk(1).
types line_no type sytabix.
types remarks(80).
TYPES   CELLTAB  TYPE LVC_T_STYL.
TYPES   TABCOLOR TYPE SLIS_T_SPECIALCOL_ALV.
types: end of ty_out.
data   gt_out     type table of ty_out     with header line.
data  $gt_out     type table of ty_out     with header line.

data : gt_return	      like	standard table of 	bapiret2
                              with  header line.

** Global Variables
ranges : r_io_objnr for coep-objnr.
ranges : r_fk_mtart for mara-mtart.
ranges:  r_matnr for coep-matnr.

** Constants
constants: c_blank  like marc-fevor value 'SPB',
           c_press  like marc-fevor value 'SPP',
           c_coil   like marc-fevor value 'AM',
           c_3c     like marc-fevor value 'SEC',
           c_engine like marc-fevor value 'SEA'.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
class lcl_event_receiver definition.
  public section.
    types: begin of ztco_abispost_k,
              kokrs type kokrs,
              period type period,
              versn type versn,
              kstar type kstar,
              werks type werks_d,
              matnr type matnr,
              stype type zstype,
              io_aufnr type aufnr,
              pcc_aufnr type aufnr,
           end of ztco_abispost_k.

    types: ztco_abispost_key   type standard table of ztco_abispost_k,
           ztco_abispost_table type standard table of ztco_abispost.

    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed,
                       get_deleted_rows
             exporting
                       deleted_rows type ztco_abispost_table,

      refresh_delta_tables.

  private section.
    data deleted_rows type standard table of ztco_abispost.

* This flag is set if any error occured in one of the
* following methods:
    data: error_in_data type c.
    methods:
      update_delta_tables
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

endclass.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

* Setting for Change data
  method handle_data_changed.

* remember deleted lines for saving
    call method update_delta_tables( er_data_changed ).

    perform data_changed using er_data_changed.
  endmethod.                    " handle_data_changed

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.

  method refresh_delta_tables.
    clear me->deleted_rows[].
  endmethod.

  method update_delta_tables.
    data: l_del_row type lvc_s_moce,
          ls_ztco_abispost type ztco_abispost,
          ls_outtab like line of gt_out.

    loop at pr_data_changed->mt_deleted_rows into l_del_row.
      read table gt_out into ls_outtab index l_del_row-row_id.
      if sy-subrc ne 0.
        message i000(0k) with text-e01. "Internal error
      else.
        move-corresponding ls_outtab to ls_ztco_abispost.
        append ls_ztco_abispost to deleted_rows.
      endif.
    endloop.
  endmethod.

endclass.                   " LCL_EVENT_RECEIVER Implementation

data g_event_receiver  type ref to lcl_event_receiver.

************************************************************************
data  : flag_data_changed,
        info(80).
data: begin of ftab occurs 10,
        fcode(6),
      end of ftab.

*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
define __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
end-of-definition.

define cal_by_qty.
*  Distributed Qty / Unit
  if it_io_coep-mbgbtr < 0.
    it_post-mbgbtr
                = floor( it_io_coep-mbgbtr * it_post-&1 ).
  else.
    it_post-mbgbtr
                = ceil( it_io_coep-mbgbtr * it_post-&1 ) .
  endif.
*  SUM
  clear lv_res_qty.
  lv_res_qty = lv_sum_qty.
  lv_sum_qty = lv_sum_qty +  it_post-mbgbtr.
*
  if      it_io_coep-mbgbtr >= 0
     and  lv_sum_qty > it_io_coep-mbgbtr.
    it_post-mbgbtr = it_io_coep-mbgbtr - lv_res_qty.
    if it_post-mbgbtr < 0.
      it_post-mbgbtr = space.
    endif.
  endif.

  if      it_io_coep-mbgbtr < 0
     and  lv_sum_qty < it_io_coep-mbgbtr.
    it_post-mbgbtr = it_io_coep-mbgbtr - lv_res_qty.
    if it_post-mbgbtr > 0.
      it_post-mbgbtr = space.
    endif.
  endif.

*  Cal. Cost
  it_post-chg_wkgbtr  =
   ( it_io_coep-wkgbtr / it_io_coep-mbgbtr ) * it_post-mbgbtr .

end-of-definition.

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

define __process.
  perform show_progress using &1 &2.
end-of-definition.

define __message.
  call function 'POPUP_TO_INFORM'
       exporting
            titel = &1
            txt1  = &2
            txt2  = sy-subrc.
end-of-definition.

****************************** constants *******************************
constants:  false value ' ',
            true  value 'X'.

data: g_error(1),
      g_repid  like sy-repid.
