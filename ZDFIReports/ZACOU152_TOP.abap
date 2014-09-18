*----------------------------------------------------------------------*
*   INCLUDE ZACOU123_NEW_TOP                                               *
*----------------------------------------------------------------------*
TABLES: ekbz, ekpo, ekbe, sscrfields, ekko, lfa1,
        *ztcou124_new, ztcou124_new,ztmm_duty_it,*rbkp, ztmm_duty_hd.
TYPE-POOLS: truxs.
*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*\

* Structures for calling the BAPI
DATA : headerdata        LIKE    bapi_incinv_create_header,
       i_invoice         LIKE    rbkp-xrech,
       i_creditmemo      LIKE    rbkp-xrech,
       invoicedocnumber  LIKE    bapi_incinv_fld-inv_doc_no,
       fiscalyear        LIKE    bapi_incinv_fld-fisc_year.
DATA : i_ztcou124_new        LIKE    ztcou124_new OCCURS 0.

DATA:   BEGIN OF gl_data OCCURS 0.
        INCLUDE STRUCTURE   bapi_incinv_create_gl_account.
DATA:   END   OF gl_data.

DATA:   BEGIN OF itemdata OCCURS 0.
        INCLUDE STRUCTURE   bapi_incinv_create_item.
DATA:   END   OF itemdata.

DATA:   BEGIN OF taxdata OCCURS 0.
        INCLUDE STRUCTURE   bapi_incinv_create_tax.
DATA:   END   OF taxdata.

DATA:   BEGIN OF return OCCURS 0.
        INCLUDE STRUCTURE   bapiret2.
DATA:   END   OF return.

DATA:   BEGIN OF xreturn OCCURS 0.
        INCLUDE STRUCTURE   bapiret2.
DATA:   END   OF xreturn.

DATA : BEGIN OF it_index OCCURS 0, " For Status Modify
         index     LIKE sy-tabix,
       END   OF  it_index.

* Total Doc. Count to be created.
DATA  : total_doc_cnt TYPE i,
        current_doc_cnt TYPE i.
DATA : percentage TYPE p,$mod TYPE i,
       $prog_text(50),$current_cnt(10),$total_cnt(10),$text(30) .


* Value List for Condition Type
DATA: BEGIN OF con_list OCCURS 0,
          kschl LIKE t685t-kschl,
          vtext LIKE t685t-vtext,
      END OF con_list.

* For creating the help list on selection screen
DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.

* Internal table for holding the data from BSIS
DATA: BEGIN OF i_bsis OCCURS 0,
          matnr  LIKE ztmm_duty_it-matnr,
          entno  LIKE ztmm_duty_it-entno,
         $key(60),
          bukrs  LIKE bsis-bukrs,
          hkont  LIKE bsis-hkont,
          fentp(3),
          gjahr  LIKE bsis-gjahr,
          belnr  LIKE bsis-belnr,
          dmbtr  LIKE bsis-dmbtr,
          menge  LIKE ztmm_duty_it-menge,
          pstqty LIKE ztcou124_new-xmenge2,          " Posted Quantity
          pstamt LIKE ztcou124_new-dmbtr,            " Posted Amount
          pdstamt LIKE ztcou124_new-dmbtr,           " Posted Amount
          entamount  LIKE ztmm_duty_it-entamount,
          duty_amt  LIKE ztmm_duty_it-duty_amt,
          mpf_amt  LIKE ztmm_duty_it-mpf_amt,
          entcurr   LIKE ztmm_duty_it-entcurr,
          uom    LIKE ztmm_duty_it-uom,
         $menge  LIKE ztmm_duty_it-menge,
          cl_doc_no LIKE ztmm_duty_it-cl_doc_no,
         $dst_amt  LIKE bsis-dmbtr,
         shkzg     TYPE shkzg,
         zuonr TYPE dzuonr,
* 29.02.2012(1)
          ebeln  LIKE ekko-ebeln,
      END OF i_bsis.
* Internal table for holding the data from BSIS
** 29.02.2012(insert start)
*DATA: BEGIN OF i_bsis_detail OCCURS 0,
*          matnr  LIKE ztmm_duty_it-matnr,
*          entno  LIKE ztmm_duty_it-entno,
*          ebeln  like ekko-ebeln,
*         $key(60),
*          bukrs  LIKE bsis-bukrs,
*          hkont  LIKE bsis-hkont,
*          fentp(3),
*          gjahr  LIKE bsis-gjahr,
*          belnr  LIKE bsis-belnr,
*          dmbtr  LIKE bsis-dmbtr,
*          menge  LIKE ztmm_duty_it-menge,
*          pstqty LIKE ZTCOU124_NEW-xmenge2,          " Posted Quantity
*          pstamt LIKE ZTCOU124_NEW-dmbtr,            " Posted Amount
*          pdstamt LIKE ZTCOU124_NEW-dmbtr,           " Posted Amount
*          entamount  LIKE ztmm_duty_it-entamount,
*          duty_amt  LIKE ztmm_duty_it-duty_amt,
*          mpf_amt  LIKE ztmm_duty_it-mpf_amt,
*          entcurr   LIKE ztmm_duty_it-entcurr,
*          uom    LIKE ztmm_duty_it-uom,
*         $menge  LIKE ztmm_duty_it-menge,
*          cl_doc_no LIKE ztmm_duty_it-cl_doc_no,
*         $dst_amt  LIKE bsis-dmbtr,
*         shkzg     TYPE shkzg,
*         zuonr TYPE dzuonr,
*      END OF i_bsis_detail.
** 29.02.2012(insert end)
*
** 28.02.2012(insert start)
*DATA: BEGIN OF i_bsis_ebeln OCCURS 0,
*          ebeln  like ekko-ebeln,
*      END OF i_bsis_ebeln.
** 29.02.2012(insert end)

DATA : sum_i_bsis    LIKE i_bsis OCCURS 0 WITH HEADER LINE.

* Internal table for holding the data from PO
DATA : BEGIN OF it_row_tab OCCURS 0,
         $key(60),
         bukrs           LIKE   ekpo-bukrs,
         kschl           LIKE   ekbz-kschl,
         matnr           LIKE   ekpo-matnr,
         frbnr           LIKE   ekbz-frbnr,   "BOL#
         ebeln           LIKE   ekpo-ebeln,
         ebelp(5),
         pmenge          LIKE   ekpo-menge,   " P/O Qty
         gmenge          LIKE   ekpo-menge,   " G/R Qty
         bmenge          LIKE   ekbz-menge,   " Balance Qty
         rmenge          LIKE   ekbz-menge,   " Bal.Qty Total by Mat
         dtyup           TYPE   p DECIMALS 5, " Unit Price for Duty
         dmbtr           LIKE   ekbz-dmbtr,   " Amount
         accup           TYPE   p DECIMALS 5,
         shkzg           LIKE   ekbz-shkzg,
         bewtp           LIKE   ekbz-bewtp,
         meins           LIKE   ekpo-meins,
         waers           LIKE   ekbz-waers,
         werks           LIKE   ekpo-werks,
* 02.29.2012(insert start), mjc
         lifnr           LIKE   ekbz-lifnr,
         land1           LIKE   lfa1-land1,
* 02.29.2012(insert end), mjc
         bal_tr(1),     " Balance is tansfered
         bal_rv         LIKE   ekpo-ebeln,    " Balance reveiced
       END   OF  it_row_tab.
DATA  it_row_tab_bal LIKE it_row_tab OCCURS 0 WITH HEADER LINE.

* I/V Item to POST
DATA: BEGIN OF it_inv OCCURS 0,
      group(4),
      entno LIKE ztmm_duty_it-entno,
      hkont LIKE bsis-hkont,
      matnr LIKE ekpo-matnr,
      ebeln LIKE ekbz-ebeln,
      ebelp LIKE ekbz-ebelp,
      lfbnr LIKE ekbe-lfbnr,
      lfpos LIKE ekbe-lfpos,
      budat LIKE ekbz-budat,
      maktx LIKE makt-maktx,
      menge LIKE ekpo-menge,
      pstqty LIKE ztcou124_new-rmenge,
      pstamt LIKE ztcou124_new-dmbtr,
      pdstamt LIKE ztcou124_new-dmbtr,    " Posted Distribute-Amount
      meins LIKE ekpo-meins,
      waers LIKE ekbz-waers,
      dmbtr LIKE ekbz-dmbtr,
      dstamt LIKE ekbz-dmbtr, " Distributed Amount
      amount LIKE ekbz-dmbtr,
      werks LIKE ekpo-werks,
      bal_tr(1),     " Balance is tansfered
      bal_rv         LIKE   ekpo-ebeln,    " Balance reveiced
      $group(4),
      line_no LIKE sy-tabix,
      END OF it_inv.

* ITAB for SUM infor. by balance QTY
DATA  itab_for_matnr_sum LIKE it_row_tab OCCURS 0 WITH HEADER LINE.

* Temporary Itab to contain the calculated data
TYPES: BEGIN OF ty_calc.
TYPES: $key(60).
INCLUDE  STRUCTURE ztcou124_new.
TYPES: xmenges(13),
       xmenge2s(13),                  " Dist. Duty Qty
       ivamts(13),
       dtamts(13),
       dtyups(10),
* UD1K941354 - by IG.MOON 7/17/2007 {
       percent TYPE p DECIMALS 2,
       percents(6),
* }
*       ACCAMTS(13),                    " Acc. Amount string - 6/26/2007
*       VARAMTS(13),                    " Var. Amount string - 6/26/2007
       pstqty LIKE ztcou124_new-rmenge,   " Posted Quantity
       pstamt LIKE ztcou124_new-dmbtr,    " Posted Amount
       pdstamt LIKE ztcou124_new-dmbtr,

       accamt LIKE ztcou124_new-dmbtr,   " Acc. Amount - 6/26/2007
       varamt LIKE ztcou124_new-dmbtr,   " Var. Amount - 6/26/2007

       rev(1),
       END OF ty_calc.

* Internal Table for Plant
TYPES: BEGIN OF ty_plant,
         bwkey TYPE bwkey,
       END OF ty_plant.

* Type for ALV
TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_calc.
TYPES: tabcolor TYPE slis_t_specialcol_alv,
        chk(1),
       END OF ty_out.

* Internal table for Write off posting
DATA : BEGIN OF i_write_off OCCURS 0,
         entno     LIKE ztcou124_new-entno,
         dmbtr     LIKE ztcou124_new-dmbtr, "Amount
       END   OF  i_write_off.

DATA $ix LIKE sy-tabix.

* Instance
DATA: gt_calc       TYPE TABLE OF ty_calc     WITH HEADER LINE,
      gt_out        TYPE TABLE OF ty_out      WITH HEADER LINE,
      gt_plant     TYPE TABLE OF ty_plant    WITH HEADER LINE.

* Mat'l ranges for gathering the data from PO
RANGES : gr_matnr FOR ekpo-matnr,
* Reference doc.# ranges when type '01'
         gr_xblnr FOR bsis-xblnr,
         gr_bwkey FOR t001w-bwkey.
* 02.28.2012(1)
RANGES : gr_ebeln FOR ekpo-ebeln.
*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed,

      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
              IMPORTING e_row
                        e_column
                        es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
* Others
*----------------------------------------------------------------------*
DATA:
      gv_index      TYPE i,
      g_error(1).

*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*

DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.

DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

DEFINE __message.
  call function 'POPUP_TO_INFORM'
    exporting
      titel = &1
      txt1  = &2
      txt2  = sy-subrc.
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.
