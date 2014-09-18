*----------------------------------------------------------------------*
*   INCLUDE ZACOU123_TOP                                               *
*----------------------------------------------------------------------*
tables: ekbz, ekpo, ekbe ,sscrfields,
        *ztcou123, ztcou123.

*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*

type-pools: truxs.
* internal table for uploaded file
data: begin of i_articles occurs 0,
         matnr(18),
         menge(13),
         iv_amt(13),   " I/V Amount
         dt_amt(13),   " Duty Amount
      end of i_articles.

ranges gr_matnr for ekpo-matnr.

data : begin of it_row_tab occurs 0,
         bukrs           like   ekpo-bukrs,
         kschl           like   ekbz-kschl,
         matnr           like   ekpo-matnr,
         ebeln           like   ekpo-ebeln,
         ebelp           like   ekpo-ebelp,
         pmenge          like   ekpo-menge, "P/O Qty
         gmenge          like   ekpo-menge, "G/R Qty
         bmenge          like   ekbz-menge, "Balance Qty
         tmenge          like   ekbz-menge, "Bal.Qty Total by Mat
         dmbtr           like   ekbz-dmbtr, "Amount
         uprice          type   p decimals 5,
         shkzg           like   ekbz-shkzg,
         bewtp           like   ekbz-bewtp,
         meins           like   ekpo-meins,
         waers           like   ekbz-waers,
         lifnr           like   ekbz-lifnr,
         werks           like   ekpo-werks,
         lichn           like   lips-lichn,
         mblnr           type   mblnr,
         xblnr           type   xblnr,
         budat           like   mkpf-budat,
       end   of  it_row_tab.

types: begin of ty_calc,
         bukrs           like   ekpo-bukrs,
         kschl           like   ekbz-kschl,
         matnr           like   ekpo-matnr,
         ebeln           like   ekpo-ebeln,
         ebelp           like   ekpo-ebelp,
         xmenge          like   ekbz-menge, " Qty from Excel
         xmenges(13), " Qty from Excel with string format
         pmenge          like   ekbz-menge, " Qty from PO
         gmenge          like   ekpo-menge, " G/R Qty
         bmenge          like   ekbz-menge, " Qty Balance
         rmenge          like   ekbz-menge, " Qty reconciling
         amenge          like   ekbz-menge, " Qty after reconciling
         uprice          type   p decimals 5,
         dmbtr           like   ekbz-dmbtr, " Amount
         meins           like   ekpo-meins,
         waers           like   ekbz-waers,
         lifnr           like   ekbz-lifnr,
         werks           like   ekpo-werks,
         post_type(1),   " Post type '' = credit memo, 'X' = I/V
         lichn           like   lips-lichn,
         mblnr           type   mblnr,
         xblnr           type   xblnr,
         budat           like   mkpf-budat,
         belum like mseg-belum,
         icon  type icon_d,
         msg   like zfi_auto_invoice-msg,
       end of ty_calc.

types: begin of ty_plant,
         bwkey type bwkey,
       end of ty_plant.

types: begin of ty_out.
include  structure ztcou123.
types : xmenges(13),
        status type icon_d,
        mblnr_r type mblnr,
        balamt  type dmbtr,
        lichn           like   lips-lichn,
        budat like mkpf-budat,
*        mblnr           type   mblnr,
        xblnr           type   xblnr,
        tabcolor type slis_t_specialcol_alv,
        chk(1),
        desc(30),
        handle_style type lvc_t_styl. "FOR DISABLE
types: colinfo   type lvc_t_scol.
types: end of ty_out.

data: gt_calc       type table of ty_calc      with header line,
      gt_out        type table of ty_out      with header line,
      gt_plant      type table of ty_plant    with header line.

* ITAB for SUM infor. by balance QTY
data  itab_for_matnr_sum like it_row_tab occurs 0 with header line.

* I/V Item
data: begin of it_inv occurs 0,
      group(10),
      matnr like ekpo-matnr,
      ebeln like ekbz-ebeln,
      ebelp like ekbz-ebelp,
      lifnr like ekbz-lifnr,
      lfbnr like ekbe-lfbnr,
      lfpos like ekbe-lfpos,
      budat like ekbz-budat,
      maktx like makt-maktx,
      menge like ekpo-menge,
      meins like ekpo-meins,
      waers like ekbz-waers,
      dmbtr like ekbz-dmbtr,
      amount like ekbz-dmbtr,
      werks like ekpo-werks,
      index like sy-index,
      $group(10),
      $ind(1),
      end of it_inv.
data  it_cre like it_inv occurs 0 with header line.

* BAPI
data : headerdata        like    bapi_incinv_create_header,
       i_invoice         like    rbkp-xrech,
       i_creditmemo      like    rbkp-xrech,
       invoicedocnumber  like    bapi_incinv_fld-inv_doc_no,
       fiscalyear        like    bapi_incinv_fld-fisc_year.
data : i_ztcou123        like    ztcou123 occurs 0.

data:   begin of itemdata occurs 0.
        include structure   bapi_incinv_create_item.
data:   end   of itemdata.

data:   begin of taxdata occurs 0.
        include structure   bapi_incinv_create_tax.
data:   end   of taxdata.

data:   begin of return occurs 0.
        include structure   bapiret2.
data:   end   of return.

data:   begin of xreturn occurs 0.
        include structure   bapiret2.
data:   end   of xreturn.

* Total Doc. Count to be created.
data  : total_doc_cnt type i,
        current_doc_cnt type i.

* Value List for Condition Type
data: begin of con_list occurs 0,
          kschl like t685t-kschl,
          vtext like t685t-vtext,
      end of con_list.

data: begin of help_field occurs 0.
        include structure help_value.
data: end of help_field.

data: begin of help_vtab occurs 0.
        include structure help_vtab.
data: end of help_vtab.

data: begin of help_value occurs 0,
      value like help_vtab-value,
      end of help_value.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
class lcl_event_receiver definition.

  public section.
    methods:
      handle_data_changed for event data_changed of cl_gui_alv_grid
              importing er_data_changed,

      handle_double_click for event double_click of cl_gui_alv_grid
              importing e_row
                        e_column
                        es_row_no.

endclass.                    "LCL_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

* Setting for Change data
  method handle_data_changed.
    perform data_changed using er_data_changed.
  endmethod.                    " handle_data_changed

* Double Click
  method handle_double_click.
    perform double_click using e_row
                               e_column
                               es_row_no.
  endmethod.                    " handle_double_click

endclass.                   " LCL_EVENT_RECEIVER Implementation

data g_event_receiver  type ref to lcl_event_receiver.

*----------------------------------------------------------------------*
* Others
*----------------------------------------------------------------------*
data:
      gv_index      type i,
      g_error(1),
      num(12) value ' 0123456789.'.

ranges : gr_bwkey for t001w-bwkey.

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

define __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
end-of-definition.

define __process.
  perform show_progress using text-s01 &1.
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
