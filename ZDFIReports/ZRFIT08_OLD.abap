*&--------------------------------------------------------------------*
*& Author                 : .....
*& Creation Date          : 11/21/2003
*& Specification By       : ANDY
*& Pattern                : Report 1-8
*& Development Request No :
*& Description  : Collect FM actuals
*&                                                                    *
*& Modification Log                                                   *
*&--------------------------------------------------------------------*
*& Date        Developer      Request ID        Description           *
*&--------------------------------------------------------------------*
*& 09/13/2011  Yn.kim         UP1K920015        ECC6.Upgrade          *
*& 01/31/2013  Valerian       UD1K956416        Remove Dummy and      *
*&                                              unassigned items from *
*&                                              table ZTFI_FMAL       *
*& 01/31/2013  Valerian       UD1K956477        Remove Doc with prefix*
*&                                              9* and G/L 125000 from*
*&                                              table ZTFI_FMAL       *
*&--------------------------------------------------------------------*
report zrfit08 message-id zmfi no standard page heading
                               line-size 70.

*Note 400924 - Recommendations and rules for the payment transfer in FM
*
*14. No partial payments but only residual items are to be posted when
*you clear between the invoices on the debit side and on the credit side
* or between invoices and credit memos. If you also want to display the
*residual items in FM on the corresponding invoice, only 1:1 assignments
* are to be maintained for the clearing.

***********************************************************************
*     DATA DECLARATION
***********************************************************************
tables: ztfi_cmal, fmifiit, ztfi_fmal, bsak, bkpf, bseg, bset,
        t001, aufk, sscrfields, *fmfpo.
data:  g_fikrs        like t001-fikrs.

data : begin of it_cmal occurs 0,
        bukrs like ztfi_cmal-bukrs,
        gjahr like ztfi_cmal-gjahr,
        belnr like ztfi_cmal-belnr,

        budat like ztfi_cmal-budat,

        dispw like ztfi_cmal-dispw,
        wrshb like ztfi_cmal-wrshb,
        dmshb like ztfi_cmal-dmshb,

        saknr like ztfi_cmal-saknr, "bank acct.
        hkont like ztfi_cmal-hkont,
        gsber like ztfi_cmal-gsber, "BA

        koart like ztfi_cmal-koart, "Acct.Type
        umskz like ztfi_cmal-umskz, "S/P GL
        lifnr like ztfi_cmal-lifnr,
        kunnr like ztfi_cmal-kunnr,
*       fmflag,
       end of it_cmal.

types: begin of it_fmifiit,
*       INCLUDE STRUCTURE fmifiit.
        vogjahr like fmifiit-vogjahr,
        vobelnr like fmifiit-vobelnr, "payment doc.
        vobukrs like fmifiit-vobukrs,
        zhldt   like fmifiit-zhldt,   "PAY DATE
        kngjahr like fmifiit-kngjahr,
        knbelnr like fmifiit-knbelnr, "invoice doc.
        knbuzei like fmifiit-knbuzei, "d/p line item#

        trbtr   like fmifiit-trbtr,   "Transaction
        fkbtr   like fmifiit-fkbtr,   "Local
        invamt  like fmifiit-trbtr,

        fistl   like fmifiit-fistl,   "FC
        fonds   like fmifiit-fonds,   "FUND
        fipex   like fmifiit-fipex,   "CI
        hkont   like fmifiit-hkont,   "G/L account
        wrttp   like fmifiit-wrttp,   "Amt Type
        twaer   like fmifiit-twaer,   "CURR

        objnrz  like fmifiit-objnrz,  "cost obj
        sgtxt   like fmifiit-sgtxt,
        payflg  like fmifiit-payflg,

        vrefbt  like fmifiit-vrefbt,
        vrefbn  like fmifiit-vrefbn,
        vrfpos  like fmifiit-vrfpos,

        lifnr   like ztfi_cmal-lifnr, "VENDOR
        kunnr   like ztfi_cmal-kunnr, "CUST

        saknr   like ztfi_cmal-saknr, "BANK GL
        aufnr   like ztfi_fmal-aufnr, "Order
        mwskz   like bseg-mwskz,      "tax code (bset-KBETR)

        gsber   like ztfi_cmal-gsber,
        augdt   like ztfi_cmal-augdt,

        fmdummy(1) type c,
       end of it_fmifiit.

data : wa_fm_pkg    type it_fmifiit,
       it_fm        type it_fmifiit occurs 0 with header line,
       fm_itab      type it_fmifiit occurs 0 with header line.

data: begin of it_objnr occurs 0,
        objnrz  like fmifiit-objnrz,  "cost obj
        aufnr   like aufk-aufnr,
      end of it_objnr.

data : begin of it_invoices occurs 0,
         gjahr like bkpf-gjahr,
         belnr like bkpf-belnr,
       end of it_invoices.

data: g_pay_amt   like bseg-dmbtr,  "local
      g_pay_damt  like bseg-wrbtr.  "document

data : begin of tot_cmal occurs 0,
        bukrs    like ztfi_cmal-bukrs,
        gjahr    like ztfi_cmal-gjahr,
        belnr    like ztfi_cmal-belnr,
        budat    like ztfi_cmal-budat,  "posting date
        saknr    like bseg-saknr,
        wrshb    like ztfi_cmal-wrshb,  "document
        dmshb    like ztfi_cmal-dmshb,  "local
        match(1) type c,
       end of tot_cmal.

data : begin of tot_fmal occurs 0,
          vogjahr  like fmifiit-vogjahr,
          vobelnr  like fmifiit-vobelnr,
          trbtr    like fmifiit-trbtr,
          fkbtr    like fmifiit-fkbtr,
          match(1) type c,
       end of tot_fmal.

data : begin of it_tmp01 occurs 0,
       bukrs like ztfi_cmal-bukrs,
       gjahr like ztfi_cmal-gjahr,
       belnr like ztfi_cmal-belnr,
       wrshb like ztfi_cmal-wrshb,
       grupp like ztfi_cmal-grupp,
       ebene like ztfi_cmal-ebene,
       saknr like ztfi_cmal-saknr, "bank acct.
       gsber like ztfi_cmal-gsber,
*      nrgnr LIKE ztfi_cmal-nrgnr,
       budat like ztfi_cmal-budat,
       end of it_tmp01.
data : begin of it_log occurs 0,
       bukrs like ztfi_cmal-bukrs,
       gjahr like ztfi_cmal-gjahr,
       belnr like ztfi_cmal-belnr,
       budat like ztfi_cmal-budat,
       subrc(1),
       end of it_log.

data : it_fmal like ztfi_fmal occurs 0 with header line.

data : i_len type i value 70,
       $subrc(1).
data : fs_wtp(20), fs_wlp(20),
       l_sum like fmifiit-trbtr,
       l_absamt like fmifiit-fkbtr,
       l_fonds_amt like fmifiit-fonds,
       l_bseg_amt like fmifiit-fkbtr,
       l_first.

field-symbols : <fs_wtp> type any,
                <fs_wlp> type any.

ranges: r_bank for ztfi_cmal-hkont.

constants: c_wrttp57(2) value '57', "payment
           c_wrttp61(2) value '61', "down payment
           c_wrttp58(2) value '58', "down payment req.
           c_ciurv(7) value 'CASHURV',
           c_ciuex(7) value 'CASHUEX'.
* USING ALV REPORTING..
type-pools : slis.

include rvreuse_global_data.
include rvreuse_local_data.
include rvreuse_forms.

data : g_save(1)    type c,
       g_exit(1)    type c,
       gx_variant   like disvariant,
       g_variant    like disvariant,
       g_repid      like sy-repid,
       g_cnt(2)     type n,
       c_waers      like t001-waers.

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
      gs_prnt     type slis_print_alv.
*---- ALV

* BEGIN OF UD1K956477
DATA: g_tabix TYPE sy-tabix.

DATA: BEGIN OF gt_block OCCURS 0,
        belnr TYPE ztfi_fmal-belnr,
      END OF gt_block.
* END OF UD1K956477

* by IG.MOON
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

constants : c_status_set   type slis_formname
                           value 'PF_STATUS_SET',
            c_user_command type slis_formname
                           value 'USER_COMMAND',
            c_top_of_page  type slis_formname value 'TOP_OF_PAGE',
            c_top_of_list  type slis_formname value 'TOP_OF_LIST',
            c_end_of_list  type slis_formname value 'END_OF_LIST'.
***********************************************************************
* SELECTION-SCREEN
***********************************************************************
selection-screen begin of block b1 with frame title text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-h01.
PARAMETERS: p_bukrs  LIKE t001-bukrs   DEFAULT 'H201' OBLIGATORY.
SELECTION-SCREEN COMMENT 52(40) p_butxt.
SELECTION-SCREEN END OF LINE.

parameters : p_gjahr like ztfi_fmal-gjahr memory id gja
                                          default sy-datum(4).
select-options: s_datum for sy-datum    memory id date obligatory,
                s_belnr for bkpf-belnr  memory id bln.

*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/12/03, by WSKIM
*---Start
*                s_belnr FOR ztfi_fmal-belnr.
*---End
parameters : p_test   as checkbox,
             p_bloc   AS CHECKBOX.                          "UD1K956477
parameters : p_delf   as checkbox.
*             p_miss  AS CHECKBOX.
selection-screen function key 1.
selection-screen end of block b1.
parameters : p_bp like bkpf-belnr.

selection-screen begin of block b2 with frame title text-002.
parameters : p_dbread as checkbox.  "db read (cbo read)
select-options :
                 s_fonds for ztfi_fmal-fincode,
                 s_fipos for ztfi_fmal-fipos,
                 s_aufnr for ztfi_fmal-aufnr,
                 s_lifnr for ztfi_fmal-lifnr,
                 s_kunnr for ztfi_fmal-kunnr.
selection-screen end of block b2.

parameters p_coll as checkbox.

***********************************************************************
* INITIALIZATION.
***********************************************************************
initialization.

  move 'Payment Transfer' to sscrfields-functxt_01.
  perform  init_screen.

***********************************************************************
* START-OF-SELECTION
***********************************************************************
start-of-selection.

  perform initialization.

  if p_dbread eq 'X'.
    perform get_ztfi_fmal.
  else.
    if p_delf = 'X'.
       perform delete_data_on_date.
    endif.
    perform get_data_proc.
  endif.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
end-of-selection.

  perform display_list_main.

*----------------------------------------------------------------------*
***INCLUDE ZRFIT08_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INIT_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_SCREEN .


  loop at screen.
    if screen-name = 'P_BUTXT'.
      screen-input  = 0.
      screen-intensified = '0'.
      screen-display_3d  = '0'.
      modify screen.
    endif.
    if screen-name = 'P_BUKRS'.
      screen-input = ' '.
      modify screen.
    endif.
  endloop.


* & c????
  perform fi_wt_read_t001 using    p_bukrs
                          changing p_butxt.

ENDFORM.                    " INIT_SCREEN
*&---------------------------------------------------------------------*
*&      Form  FI_WT_READ_T001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      <--P_P_BUTXT  text
*----------------------------------------------------------------------*
FORM FI_WT_READ_T001  using    pa_bukrs
                      changing pa_butxt.

  data : it_t001 like t001.

  call function 'FI_WT_READ_T001'
    exporting
      i_bukrs   = pa_bukrs
    importing
      t_t001    = it_t001
    exceptions
      not_found = 1.

  case sy-subrc.
    when 0.
      pa_butxt = it_t001-butxt.
    when 1.
      message s101(f5).
    when others.
  endcase.


ENDFORM.                    " FI_WT_READ_T001
*&---------------------------------------------------------------------
*&      Form  WRITE_TITLE
*&---------------------------------------------------------------------
form write_title using p_new p_len p_txt.

  case p_new.
    when 'S'.
      write: at /(i_len) sy-uline,
             at / sy-vline no-gap.
    when 'M'.
      write: at / sy-vline no-gap.
  endcase.

  write: at (p_len) p_txt no-gap color col_heading centered,
         at (1) sy-vline no-gap.

  if p_new = 'E'.
    write: at /(i_len) sy-uline.
  endif.

endform.                                        "WRITE_TITLE
************************************************************************
* Form  PF_STATUS_SET
************************************************************************
form  pf_status_set using p_rt_extab type slis_t_extab.

  set pf-status 'MENU'.

endform.
*&---------------------------------------------------------------------
*&      Form  WRITE_ITEM
*&---------------------------------------------------------------------
form write_item using p_new p_len  p_item.

  if p_new = 'S'.
    write: / sy-vline no-gap.
  endif.

  write: at (p_len) p_item no-gap, "COLOR COL_NORMAL,
         at (1) sy-vline no-gap.

endform.                               " WRITE_ITEM
*&---------------------------------------------------------------------*
*&      Form  save_fm_actuals
*&---------------------------------------------------------------------*
form save_fm_actuals using l_subrc.

  data: l_idx(2) type n,
        l_rcnt(5) type n.

* Delete Existing Line items
  delete from ztfi_fmal
         where bukrs eq p_bukrs
         and   gjahr eq p_gjahr
         and   datum in s_datum
         and   belnr in s_belnr.
  commit work.

* BEGIN OF UD1K956416
  DELETE it_fmal WHERE fipos   = 'DUMMY' OR
                       fipos   = 'CASHUEX' OR
                       fipos   = 'CASHURV'.

  loop at it_fmal where hkont = '0000125000' AND belnr CP '9*'.
    gt_block-belnr = it_fmal-belnr.
    append gt_block.
  endloop.

  sort gt_block by belnr.
  delete adjacent duplicates from gt_block.

  loop at it_fmal.
    g_tabix = sy-tabix.

    read table gt_block with key belnr = it_fmal-belnr
                             binary search.
    if sy-subrc = 0.
      delete it_fmal index g_tabix.
    endif.
  endloop.

* END OF UD1K956416

*....FM line-item
  loop at it_fmal.
    it_fmal-stunr = l_rcnt.
    move-corresponding it_fmal to ztfi_fmal.

    insert ztfi_fmal.  "MODIFY

    if sy-subrc <> 0.
      l_subrc = 'E'.
      message w000 with 'Data save error' ztfi_fmal-belnr.
    endif.

    l_rcnt = l_rcnt + 1.
    if l_rcnt >= 99999.
      commit work.
      clear l_rcnt.
    endif.

  endloop.

  check l_subrc eq space.
  clear l_rcnt.

endform.                    " save_fm_actuals
*&---------------------------------------------------------------------*
*&      Form  get_bank_acct
*&---------------------------------------------------------------------*
form get_bank_acct.

*//
  r_bank-sign = 'I'.  r_bank-option = 'EQ'.

  select saknr into (r_bank-low)
         from skb1
         where bukrs eq p_bukrs
         and   xgkon eq 'X'
         and ( fdlev like 'B%' or fdlev like 'C%' ).
    append r_bank.
  endselect.

endform.                    " get_bank_acct
*&---------------------------------------------------------------------*
*&      Form  get_ztfi_fmal
*&---------------------------------------------------------------------*
form get_ztfi_fmal.

  clear: it_fmal[], it_fmal.

  select * from ztfi_fmal
           into table it_fmal
           where bukrs   eq p_bukrs
           and   datum   in s_datum
           and   fincode in s_fonds
           and   fipos   in s_fipos
           and   aufnr   in s_aufnr
           and   lifnr   in s_lifnr
           and   kunnr   in s_kunnr.

endform.                    " get_ztfi_fmal
*&---------------------------------------------------------------------*
*&      Form  fill_blank_invoice_no
*&---------------------------------------------------------------------*
form fill_blank_invoice_no.

  data: l_bsak like bsak.
  data: i_bsak like bsak occurs 0 with header line.

  loop at fm_itab.
* if pay = invoice
    check fm_itab-vobelnr = fm_itab-knbelnr.

** pay doc <> clearing doc
    select single * into l_bsak from bsak
       where belnr =  fm_itab-knbelnr
         and gjahr =  fm_itab-kngjahr
         and augbl <> fm_itab-knbelnr.

    check sy-subrc = 0.

** check other invoice
    select * from bsak into table i_bsak
       where augbl =  l_bsak-augbl
         and augdt =  l_bsak-augdt
         and belnr <> fm_itab-knbelnr.

** only one invoice...
    check sy-dbcnt = 1.
    read table i_bsak index 1.

    fm_itab-knbelnr = i_bsak-belnr.
    fm_itab-kngjahr = i_bsak-gjahr.

    modify fm_itab.   clear fm_itab.

  endloop.

endform.                    " fill_blank_invoice_no
*&---------------------------------------------------------------------*
*&      Form  get_fm_data
*&---------------------------------------------------------------------*
form get_fm_data.

  data: lt_objnr_tmp like it_objnr occurs 0 with header line.

  check not it_cmal[] is initial.

*...VIEW: FMIFIHD + FMIFIIT
  select * into  corresponding fields of it_fm
          from v_fmifi
          for all entries in it_cmal    " Manju
          where fikrs   =  p_bukrs
           and vogjahr  eq p_gjahr
           and vobelnr  eq it_cmal-belnr
           and wrttp   in (c_wrttp57, c_wrttp61)
          and vrgng ne 'KAVR'.        "Exclude TAX ITEM!!!

    collect it_fm.

    lt_objnr_tmp-objnrz = it_fm-objnrz.
    append lt_objnr_tmp.
  endselect.

  sort lt_objnr_tmp by objnrz.
  delete adjacent duplicates from lt_objnr_tmp comparing objnrz.

  select objnr aufnr into table it_objnr
    from aufk
    for all entries in lt_objnr_tmp
    where objnr = lt_objnr_tmp-objnrz.

  sort it_objnr by objnrz.

**FIND invoice/credit clearing!!!
*    SELECT * INTO  CORRESPONDING FIELDS OF it_fm
*          FROM v_fmifi
*          FOR ALL ENTRIES IN it_invoices
*          WHERE fikrs   = p_bukrs
*           AND vogjahr  = it_invoices-gjahr
*           AND vobelnr  = it_invoices-belnr
*           AND wrttp   IN (c_wrttp57, c_wrttp61)
*           AND vrgng NE 'KAVR'.        "Exclude TAX ITEM!!!
*        collect it_fm.
*    ENDSELECT.

endform.                    " get_fm_data
*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
form initialization.

  clear: it_cmal[], it_cmal, fm_itab[], fm_itab,
         fm_itab[], fm_itab, it_log[], it_log, it_tmp01[], it_tmp01.

  select waers into (c_waers)
         from t001
         where bukrs eq p_bukrs.
    exit.
  endselect.

  call function 'FMFK_GET_FIKRS_FROM_BUKRS'
       exporting
            i_bukrs = p_bukrs
       importing
            e_fikrs = g_fikrs.

  loop at s_datum.
    if s_datum-high is initial.
      s_datum-high = s_datum-low.
      modify s_datum.
    endif.
  endloop.

*...get bank account
  perform get_bank_acct.

endform.                    " initialization
*&---------------------------------------------------------------------*
*&      Form  get_cm_actuals
*&---------------------------------------------------------------------*
form get_cm_actuals.

*...cm actuals
  select * from ztfi_cmal
           into corresponding fields of it_cmal
           where bukrs  eq p_bukrs
           and   gjahr  eq  p_gjahr
           and   datum  in s_datum
           and   grupp  not like 'W%'
           and   belnr  in s_belnr.
    collect it_cmal.
  endselect.

endform.                    " get_cm_actuals
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
form display_list.

  perform field_setting tables gt_fieldcat using :
   'DATUM'     'PAYday'         '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'PAYFLG'    'PF'             '01' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'WRTTP'     'VT'             '02' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'GJAHR'     'PayYr'          '04' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BELNR'     'Payment'        '10' 'X' 'R'  ' '  ' '  '  ' ' '  ' ',
   'DMSHB'     'PayAmt(Loc)'    '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'KNGJAHR'   'InvYr'          '04' 'X' 'R'  ' '  ' '  '  ' ' '  ' ',
   'KNBELNR'   'Invoice'        '10' 'X' 'R'  ' '  ' '  '  ' ' '  ' ',
   'INVAMT'    'InvAmt'         '15' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'FIPOS'     'CommItm'        '07' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'FINCODE'   'Fund'           '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'FISTL'     'FCtr'           '06' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'HKONT'     'G/L Acct'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'AUFNR'     'Order'          '12' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'LIFNR'     'Vendor'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'KUNNR'     'Customer'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'SAKNR'     'BankGL'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'GSBER'     'BA'             '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'SGTXT'     'Text'           '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.
  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program      = g_repid
            i_callback_user_command = 'USER_COMMAND'
            it_fieldcat             = gt_fieldcat
            i_save                  = 'A'
       tables
            t_outtab                = it_fmal
       exceptions
            program_error           = 1
            others                  = 2.

endform.                    " display_list
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
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

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
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

  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  process_fm_data
*&---------------------------------------------------------------------*
form process_fm_data.
  data : l_idx     like sy-tabix,
         lw_fm     like it_fm,
         lw_pkg(1) type c,
         lt_pay_pkg like wa_fm_pkg occurs 0 with header line.


  sort it_cmal by belnr gjahr koart.
  sort it_fm   by vobelnr vogjahr.

  data flag(1).
  data $ix like sy-tabix.

  loop at it_fm.
    $ix = sy-tabix.
*---exclude down payment document
    if it_fm-wrttp = '61' and it_fm-vobelnr = it_fm-knbelnr.
      delete it_fm index $ix.
    endif.
  endloop.

  loop at it_fm.
*---- fill to pay pkg
    move-corresponding it_fm to lt_pay_pkg .
    clear it_cmal.
    read table it_cmal with key belnr = it_fm-vobelnr
                                gjahr = it_fm-vogjahr
                                binary search.

    lt_pay_pkg-saknr = it_cmal-saknr.

    if not it_fm-objnrz is initial.
      read table it_objnr with key objnrz = it_fm-objnrz binary search.
      if sy-subrc = 0.
        lt_pay_pkg-aufnr = it_objnr-aufnr.
      endif.
    endif.
    append lt_pay_pkg.
    clear lt_pay_pkg.

    at end of vobelnr.
      flag = 'X'.
    endat.

    check flag eq 'X'.
    clear flag.

    if it_fm-vobelnr = p_bp. break-point. endif.

*-----process payament package
    perform process_payment_pkg tables lt_pay_pkg.


    refresh lt_pay_pkg.
    clear lt_pay_pkg.

  endloop.

* -------------------------------------- *
* end of UD1K940661
* -------------------------------------- *

endform.                    " process_fm_data
*&---------------------------------------------------------------------*
*&      Form  fill_fmitab
*&---------------------------------------------------------------------*
form fill_fmitab.

  loop at fm_itab.
    clear it_fmal.

    move : sy-mandt              to it_fmal-mandt,    "Client
           fm_itab-vobukrs    to it_fmal-bukrs,    "company
*          fm_itab-vogjahr    TO it_fmal-gjahr,    "year
           fm_itab-zhldt(4)   to it_fmal-gjahr,    "year
           fm_itab-vobelnr    to it_fmal-belnr,    "payment doc.no
           fm_itab-twaer      to it_fmal-dispw,    "currency
           fm_itab-zhldt      to it_fmal-datum,    "date
           fm_itab-gsber      to it_fmal-gsber,    "b/a
           fm_itab-fipex      to it_fmal-fipos,    "commitment item
           fm_itab-fonds      to it_fmal-fincode,  "fund
           fm_itab-fistl      to it_fmal-fistl,    "fund center

           fm_itab-trbtr      to it_fmal-wrshb,    "doc. amount
           fm_itab-fkbtr      to it_fmal-dmshb,    "local amount "?
           fm_itab-wrttp      to it_fmal-wrttp,    "val.type
           fm_itab-kngjahr    to it_fmal-kngjahr,  "fi doc. year
           fm_itab-knbelnr    to it_fmal-knbelnr,  "fi doc. no

           fm_itab-hkont      to it_fmal-hkont,    "g/l acct.
           fm_itab-saknr      to it_fmal-saknr,    "
           fm_itab-payflg     to it_fmal-payflg,   "pmnt stts fm doc
           fm_itab-aufnr      to it_fmal-aufnr,    "order no.
           fm_itab-augdt      to it_fmal-augdt,    "clearing date
           fm_itab-lifnr      to it_fmal-lifnr,    "vendor
           fm_itab-kunnr      to it_fmal-kunnr,    "customer
           fm_itab-mwskz      to it_fmal-mwskz,    "tax code.

           fm_itab-sgtxt      to it_fmal-sgtxt,    "text(gl line)
           fm_itab-vrefbt     to it_fmal-vrefbt,   "Predecessor
           fm_itab-vrefbn     to it_fmal-vrefbn,   "Predecessor
           fm_itab-vrfpos     to it_fmal-vrfpos,   "Predecessor

           fm_itab-fmdummy    to it_fmal-fmdummy,
           fm_itab-invamt     to it_fmal-invamt.   " Inv amt(inc.tax)

*    APPEND it_fmal.  CLEAR it_fmal.
    it_fmal-stunr = ''.
    collect it_fmal.

  endloop.

endform.                    " fill_fmitab
*&---------------------------------------------------------------------*
*&      Form  progress_ind
*&---------------------------------------------------------------------*
form progress_ind using    p_%
                           p_text.
  call function 'FI_PROGRESS_INDICATOR'
       exporting
            percentage = p_%
            text       = p_text.
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' ' .
endform.                    " progress_ind
*&---------------------------------------------------------------------*
*&      Form  check_totals
*&---------------------------------------------------------------------*
form check_totals.

  types : begin of ty_miss,
           vobukrs like  fmifiit-vobukrs,
           vogjahr like  fmifiit-vogjahr,
           vobelnr like  fmifiit-vobelnr,
           lifnr   like  ztfi_cmal-lifnr,
           kunnr   like  ztfi_cmal-kunnr,
          end of   ty_miss.
  data : it_not_match type ty_miss occurs 0 with header line.

  types : begin of wa_bsak,
           bukrs   like bsak-bukrs,
           lifnr   like bsak-lifnr,
           augbl   like bsak-augbl,
           gjahr   like bsak-gjahr,
           belnr   like bsak-belnr,
           buzei   like bsak-buzei,
           augdt   like bsak-augdt,  "clearing date
           dmbtr   like bsak-dmbtr,
           wrbtr   like bsak-wrbtr,
           bschl   like bsak-bschl,  "posting key
           shkzg   like bsak-shkzg,  "dr/cr

           bstat   like bsak-bstat,  "noted item

           umsks   like bsak-umsks,
           waers   type waers,

           hkont   like bsak-hkont,
           fipos   like bsak-fipos,
           fistl   like bsak-fistl,
           geber   like bsak-geber,
           aufnr   like bsak-aufnr,
           ebeln   like bsak-ebeln,
           ebelp   like bsak-ebelp,

           vogjahr like bsak-gjahr,

         end of wa_bsak.

  data : it_bsak      type wa_bsak occurs 0 with header line,
         it_bsak_temp type wa_bsak occurs 0 with header line.


* Collect payment for which Totals are not matching

* UD1K940867 by IG.MOON 6/21/2007
  sort : it_fm by vobelnr vogjahr wrttp,
         tot_fmal by vobelnr vogjahr fkbtr.
* end of UD1K940867

  data: l_idx like sy-tabix.
  data  no_match(1).

  loop at tot_cmal.
    l_idx = sy-tabix.
*-- check if down-payment exists
    clear : it_fm, no_match.
    read table it_fm with key vobelnr = tot_cmal-belnr
                              vogjahr = tot_cmal-gjahr
                              wrttp   = '61'
                     binary search.

    if sy-subrc eq 0.
      no_match = 'X'.
    else.
      clear tot_fmal.
      read table tot_fmal with key vobelnr = tot_cmal-belnr
                                   vogjahr = tot_cmal-gjahr
                                   fkbtr   = tot_cmal-dmshb
                                   binary search.
      if sy-subrc eq 0.
        tot_cmal-match = 'X'.
        modify tot_cmal index l_idx transporting match.
      else.
        no_match = 'X'.
      endif.
    endif.

* Searching Missing item => The diff exist Both of them or
* Same total and wrttp = '61'. WHY????
    if no_match eq space.
      tot_cmal-match = 'X'.
      modify tot_cmal index l_idx transporting match.
    else.  " or ( sy-subrc =  0 and it_fm-wrttp = '61' ) .
      it_not_match-vobukrs = tot_cmal-bukrs.
      it_not_match-vogjahr = tot_cmal-gjahr.
      it_not_match-vobelnr = tot_cmal-belnr.

      read table it_cmal with key gjahr = tot_cmal-gjahr
                                  belnr = tot_cmal-belnr
                                  koart = 'K'.

      it_not_match-lifnr   = it_cmal-lifnr.
      read table it_cmal with key gjahr = tot_cmal-gjahr
                                  belnr = tot_cmal-belnr
                                  koart = 'D'.

      it_not_match-kunnr   = it_cmal-kunnr.
      append it_not_match.

    endif.
  endloop.

* -------------------------------------- *
* END of UD1K940661 by IG.MOON 5/25/2007
* -------------------------------------- *

* Select all the Invoices from BSAK for  given Payment Document to
* find missing invoice.
* FIXME - rederive FM object again!!!
  sort it_fm by vobelnr vogjahr knbelnr kngjahr.

  loop at it_not_match.
    select bukrs lifnr augbl gjahr belnr buzei augdt dmbtr wrbtr
           bschl shkzg bstat umsks waers
           hkont fipos fistl geber aufnr ebeln ebelp
     into table it_bsak_temp
     from bsak
        where bukrs =  p_bukrs
          and lifnr =  it_not_match-lifnr
*          AND UMSKS <> SPACE
          and augbl =  it_not_match-vobelnr
          and belnr <> it_not_match-vobelnr.

    select bukrs kunnr augbl gjahr belnr buzei augdt dmbtr wrbtr
           bschl shkzg bstat umsks waers
           hkont fipos fistl geber aufnr"ebeln ebelp
     appending table it_bsak_temp
     from bsad
        where bukrs =  p_bukrs
          and kunnr =  it_not_match-kunnr
*          AND UMSKS <> SPACE
          and augbl =  it_not_match-vobelnr
          and belnr <> it_not_match-vobelnr.

    loop at it_bsak_temp.
      check it_bsak_temp-augdt(4) = it_not_match-vogjahr.

      read  table it_fm with key  vobelnr = it_bsak_temp-augbl
                                  vogjahr = it_bsak_temp-augdt(4)
                                  knbelnr = it_bsak_temp-belnr
                                  kngjahr = it_bsak_temp-gjahr
                             binary search.
      if sy-subrc ne 0 .
        move-corresponding it_bsak_temp to it_bsak.
        append it_bsak.  clear it_bsak.

*------ WARNING MESSAGE...
      endif.
    endloop.

  endloop.

  sort it_bsak by gjahr belnr buzei.


* Select Missing Invoice data from FUND TABLE again
*  data: lt_fmit like it_fm occurs 0 with header line.
*  refresh lt_fmit.

  data: it_fm_tmp like it_fm occurs 0 with header line.
  data $it_fm like  it_fm occurs 0 with header line.

  loop at it_bsak.

    if it_bsak-bstat = 'S'.  "noted item
      it_fm_tmp-wrttp = c_wrttp58.  "DP request
    elseif it_bsak-umsks <> space.
      it_fm_tmp-wrttp = c_wrttp61.  "DP

    else.  "invoice payment

      clear : $it_fm[],$it_fm.
      select * into  corresponding fields of $it_fm
              from v_fmifi
              where fikrs   =  p_bukrs
               and kngjahr  eq it_bsak-gjahr
               and knbelnr  eq it_bsak-belnr
               and wrttp   in (c_wrttp57, c_wrttp61)
              and vrgng ne 'KAVR'.        "Exclude TAX ITEM!!!

        $it_fm-vobukrs = it_bsak-bukrs.
        $it_fm-vobelnr = it_bsak-augbl.
        $it_fm-vogjahr = it_bsak-augdt(4).
        $it_fm-zhldt   = it_bsak-augdt.

        collect $it_fm.
      endselect.

      read table $it_fm index 1.
      if sy-subrc eq 0.
        append lines of $it_fm to it_fm.
        continue.
      endif.

      it_fm_tmp-wrttp = c_wrttp57.
    endif.

    if it_bsak-shkzg = 'H'.
      it_fm_tmp-trbtr  = - it_bsak-wrbtr.
      it_fm_tmp-fkbtr  = - it_bsak-dmbtr.
      it_fm_tmp-invamt = - it_bsak-dmbtr.
    else.
      it_fm_tmp-trbtr  = it_bsak-wrbtr.
      it_fm_tmp-fkbtr  = it_bsak-dmbtr.
      it_fm_tmp-invamt = it_bsak-dmbtr.
    endif.

    it_fm_tmp-fistl = it_bsak-fistl.
    it_fm_tmp-fonds = it_bsak-geber.

    if it_fm_tmp-wrttp = c_wrttp57.
      if it_bsak-shkzg = 'H'.
        it_fm_tmp-fipex = c_ciuex.
      else.
        it_fm_tmp-fipex = c_ciurv.
      endif.
    else.
*---- DOWN PAYMENT ONLY
      it_fm_tmp-fipex = it_bsak-fipos.
      it_fm_tmp-hkont = it_bsak-hkont.
    endif.

* {
    it_fm_tmp-twaer = it_bsak-waers.
    if it_bsak-bschl < '20'.
      it_fm_tmp-kunnr = it_bsak-lifnr.
    else.
      it_fm_tmp-lifnr = it_bsak-lifnr.
    endif.

    if not it_bsak-ebeln is initial.
      it_fm_tmp-vrefbt = '020'.
      it_fm_tmp-vrefbn = it_bsak-ebeln.
      it_fm_tmp-vrfpos = it_bsak-ebelp.
    endif.
* }
    it_fm_tmp-aufnr = it_bsak-aufnr.

    it_fm_tmp-kngjahr = it_bsak-gjahr.
    it_fm_tmp-knbelnr = it_bsak-belnr.
    it_fm_tmp-knbuzei = it_bsak-buzei.

    it_fm_tmp-vobukrs = it_bsak-bukrs.
    it_fm_tmp-vobelnr = it_bsak-augbl.
    it_fm_tmp-vogjahr = it_bsak-augdt(4).
    it_fm_tmp-zhldt   = it_bsak-augdt.
    append it_fm_tmp.
  endloop.

  append lines of it_fm_tmp to it_fm.

  sort it_fm by vobelnr vogjahr wrttp.

endform.                    " check_totals
*&---------------------------------------------------------------------*
*&      Form  process_fm_missing
*&---------------------------------------------------------------------*
form process_fm_missing.

  data : l_cnt like fmifiit-stunr.

* IF CM exist, but FM not exist
* UNASSIGNED IN/OUTGOING PAYMENT
  loop at it_cmal.
    read table it_fm with key  vobelnr = it_cmal-belnr
                               vogjahr = it_cmal-gjahr
                          binary search.
    if sy-subrc ne 0.
      move-corresponding it_cmal to fm_itab.
      move : it_cmal-belnr       to fm_itab-vobelnr,
             it_cmal-gjahr       to fm_itab-vogjahr,
             it_cmal-bukrs       to fm_itab-vobukrs,
             it_cmal-dispw       to fm_itab-twaer,  "currency
             it_cmal-wrshb       to fm_itab-trbtr,
             it_cmal-dmshb       to fm_itab-fkbtr,

             it_cmal-saknr       to fm_itab-saknr,
             it_cmal-hkont       to fm_itab-hkont,
             it_cmal-budat       to fm_itab-zhldt,
             'X'                 to fm_itab-fmdummy.
*
      if it_cmal-wrshb > 0.
        fm_itab-payflg = '+'.
        fm_itab-fipex  = c_ciurv.
      else.
        fm_itab-payflg = '-'.
        fm_itab-fipex  = c_ciuex.
      endif.

* UD1K941018 by IG.MOON 7/16/2007 {
      if not fm_itab-hkont is initial.
        perform check_skbi_fmfpo  using fm_itab-hkont
                                  changing fm_itab-fipex .
      endif.
* }

      perform determine_fund_again
        using    fm_itab-aufnr fm_itab-hkont
        changing fm_itab-fonds.

      append fm_itab.   clear fm_itab.

    endif.
  endloop.

endform.                    " process_fm_missing
*&---------------------------------------------------------------------*
*&      Form  process_payment_pkg
*&---------------------------------------------------------------------*
form process_payment_pkg tables   lt_pay_pkg structure wa_fm_pkg.

  data: l_idx      like sy-tabix.
  data: lw_fm      like it_fm,
        lw_pkg(1)  type c,
        lt_inv_pkg like wa_fm_pkg occurs 0 with header line.

  clear: g_pay_amt, g_pay_damt.
  read table lt_pay_pkg index 1.

  clear tot_cmal.
  read table tot_cmal with key belnr = lt_pay_pkg-vobelnr
                               gjahr = lt_pay_pkg-vogjahr
                           binary search.

*PROCESS PAYMENT PKG
  sort lt_pay_pkg by kngjahr knbelnr.

  data  $flag(1).

  loop at lt_pay_pkg.
    move-corresponding lt_pay_pkg to lt_inv_pkg.

    append lt_inv_pkg.
    clear lt_inv_pkg.

    at end of knbelnr.
      $flag = 'X'.
    endat.
    check $flag eq 'X'.
    clear $flag.

    if lt_pay_pkg-knbelnr = p_bp. break-point. endif.
    perform process_invoice_pkg tables lt_inv_pkg.
    __cls lt_inv_pkg.

  endloop.

*check payment = total of invoice paid
  if g_pay_amt <> tot_cmal-dmshb.
    clear fm_itab.
    fm_itab-vobelnr  = it_cmal-belnr.
    fm_itab-vogjahr  = it_cmal-gjahr.
    fm_itab-vobukrs  = it_cmal-bukrs.
    fm_itab-twaer    = it_cmal-dispw.
    fm_itab-saknr    = it_cmal-saknr.
    fm_itab-hkont    = it_cmal-hkont.
    fm_itab-zhldt    = it_cmal-budat.
    fm_itab-fmdummy  = 'X'          .
*    fm_itab-fkbtr    = it_cmal-dmshb - g_pay_amt.
*    fm_itab-trbtr    = it_cmal-dmshb - g_pay_amt.
    fm_itab-fkbtr = tot_cmal-dmshb - g_pay_amt.  "local
    fm_itab-trbtr = tot_cmal-wrshb - g_pay_damt. "document
    fm_itab-fipex    = c_ciuex.

*---use vendor from FM
    fm_itab-lifnr    = lt_pay_pkg-lifnr.
    fm_itab-kunnr    = lt_pay_pkg-kunnr.

    if g_pay_amt < 0.
      fm_itab-payflg = '-'.
    else.
      fm_itab-payflg = '+'.
    endif.

    append fm_itab.

    write:/ 'MISMATCH...', lt_pay_pkg-knbelnr, lt_pay_pkg-kngjahr.
  endif.

endform.                    " process_payment_pkg
*&---------------------------------------------------------------------*
*&      Form  process_invoice_pkg
*&---------------------------------------------------------------------*
form process_invoice_pkg tables   lt_inv_pkg structure wa_fm_pkg.

  data: begin of lt_bset occurs 0,
             mwskz like bseg-mwskz, "tax code
             ktosl like bset-ktosl,
             kbetr like bset-kbetr, "tax rate
             hwste like bset-hwste, "Tax Amount in Local Currency
             fwste like bset-fwste, "Tax Amount in Document Currency
         end of lt_bset.

  data: begin of lt_bseg occurs 0,
         mwskz  like bseg-mwskz,
         geber  like bseg-geber,
         fistl  like bseg-fistl,
         fipos  like bseg-fipos,
         hkont  like bseg-hkont,
         shkzg  like bseg-shkzg,
         aufnr  like bseg-aufnr,

         dmbtr  like bseg-dmbtr,
         wrbtr  like bseg-wrbtr,
        end of lt_bseg.

  data: l_idx     like sy-tabix,
        l_tax(1)  type c,
        l_fund(1) type c,
        l_hwste   like bseg-dmbtr,
        l_mwskz   like bseg-mwskz,
        lw_pkg    like wa_fm_pkg.

  data : l_trbtr   like fm_itab-trbtr,
         l_fkbtr   like fm_itab-fkbtr,
         l_last(1),
         l_downpay(1),
         lw_fmbtr    like bseg-dmbtr,  "INV PAY AMT (FM)
         lw_glbtr    like bseg-dmbtr,  "INV PAY AMT (BSEG)
         lw_ap_dmbtr   like bseg-dmbtr,
         lw_ap_wrbtr   like bseg-wrbtr,
         lwp_shkzg   like bseg-shkzg,
         lwp_lifnr   like bseg-lifnr,
         lwp_kunnr   like bseg-kunnr.

  data : lt_fmtab    type it_fmifiit occurs 0 with header line.
  ranges: lr_umskz for bseg-umskz.

  clear:   lw_fmbtr, lw_glbtr.

* PAYMENT AMT of INVOICE
  loop at lt_inv_pkg.
    at last.
      sum.
      lw_fmbtr = lt_inv_pkg-fkbtr.
    endat.
  endloop.

* Get Vendor/Customer/Amount
  read table lt_inv_pkg index 1.
  lw_pkg = lt_inv_pkg.

  clear: lw_ap_dmbtr, lw_ap_wrbtr.

  if lw_pkg-wrttp = '58'.  "DP req
    select single * from bseg
       where bukrs = p_bukrs
         and gjahr = lw_pkg-kngjahr
         and belnr = lw_pkg-knbelnr
         and buzei = lw_pkg-knbuzei.

    lwp_shkzg   = bseg-shkzg.
    lwp_lifnr   = bseg-lifnr.
    lwp_kunnr   = bseg-kunnr.

    if lwp_shkzg = 'S'.  "SELF pay
      lw_ap_dmbtr = lw_ap_dmbtr - bseg-dmbtr.
      lw_ap_wrbtr = lw_ap_wrbtr - bseg-wrbtr.
    else.
      lw_ap_dmbtr = lw_ap_dmbtr + bseg-dmbtr.
      lw_ap_wrbtr = lw_ap_wrbtr + bseg-wrbtr.
    endif.
  elseif lw_pkg-wrttp = '61'.  "DP

    select * from bseg
       where bukrs = p_bukrs
         and gjahr = lw_pkg-kngjahr
         and belnr = lw_pkg-knbelnr
         and umsks = 'A'.

      lwp_shkzg   = bseg-shkzg.
      lwp_lifnr   = bseg-lifnr.
      lwp_kunnr   = bseg-kunnr.

      if lwp_shkzg = 'S'.  "SELF pay
        lw_ap_dmbtr = lw_ap_dmbtr - bseg-dmbtr.
        lw_ap_wrbtr = lw_ap_wrbtr - bseg-wrbtr.
      else.
        lw_ap_dmbtr = lw_ap_dmbtr + bseg-dmbtr.
        lw_ap_wrbtr = lw_ap_wrbtr + bseg-wrbtr.
      endif.
    endselect.

  else.      "Invoice
    select * from bseg
       where bukrs = p_bukrs
         and gjahr = lw_pkg-kngjahr
         and belnr = lw_pkg-knbelnr
         and koart = it_cmal-koart
         and umskz = space.

      lwp_shkzg   = bseg-shkzg.
      lwp_lifnr   = bseg-lifnr.
      lwp_kunnr   = bseg-kunnr.

      if lwp_shkzg = 'H'.  "INVOICE
        lw_ap_dmbtr = lw_ap_dmbtr - bseg-dmbtr.
        lw_ap_wrbtr = lw_ap_wrbtr - bseg-wrbtr.
      else.
        lw_ap_dmbtr = lw_ap_dmbtr + bseg-dmbtr.
        lw_ap_wrbtr = lw_ap_wrbtr + bseg-wrbtr.
      endif.
    endselect.
  endif.


* Find if invoice has tax amount
  refresh lt_bset.
  clear l_tax.
  select * from bset
        where bukrs = p_bukrs
          and gjahr = lw_pkg-kngjahr
          and belnr = lw_pkg-knbelnr
          and ktosl = 'NVV'
          and kbetr <> 0.
    move-corresponding bset to lt_bset.
    lt_bset-kbetr = lt_bset-kbetr / 10 .
    collect lt_bset.
    l_tax = 'X'.
  endselect.

* Find if Fund exist
*  clear l_fund.
*  loop at lt_inv_pkg where WRTTP = '61' and fonds <> space.
*    l_fund = 'X'.
*  endloop.



*-2. tax invoice
  if l_tax = 'X'.
    refresh lt_bseg.
    select * from bseg for all entries in lt_inv_pkg
          where bukrs = p_bukrs
            and gjahr = lt_inv_pkg-kngjahr
            and belnr = lt_inv_pkg-knbelnr
            and buzid <> 'T'      "exclude tax account
            and hkont = lt_inv_pkg-hkont.
      move-corresponding bseg to lt_bseg.
      if lt_bseg-shkzg = 'S'.
        lt_bseg-dmbtr  = lt_bseg-dmbtr * -1.
        lt_bseg-wrbtr  = lt_bseg-wrbtr * -1.
      endif.
      collect lt_bseg.
    endselect.

    clear : l_trbtr ,   l_fkbtr, l_last, l_downpay.

    loop at lt_bseg.

      move-corresponding  lw_pkg to lt_fmtab.
      if tot_cmal-dmshb < 0.
        lt_fmtab-payflg = '-'.
      else.
        lt_fmtab-payflg = '+'.
      endif.

      lt_fmtab-zhldt   = tot_cmal-budat.   "POSTING DATE of pay.doc

      lt_fmtab-mwskz   = lt_bseg-mwskz.
      lt_fmtab-fonds   = lt_bseg-geber.
      lt_fmtab-fistl   = lt_bseg-fistl.
      lt_fmtab-fipex   = lt_bseg-fipos.
      lt_fmtab-hkont   = lt_bseg-hkont.
      lt_fmtab-aufnr   = lt_bseg-aufnr.
      lt_fmtab-mwskz   = lt_bseg-mwskz.
      lt_fmtab-saknr   = lt_inv_pkg-saknr.
      .
*-----Fund/Order
      if lt_bseg-geber <> space.
        perform determine_fund_again using    lt_bseg-aufnr
                                              lt_bseg-hkont
                                     changing lt_fmtab-fonds.
      endif.
      if lt_fmtab-lifnr = '*' or lt_fmtab-lifnr = ''.
        lt_fmtab-lifnr = lwp_lifnr.
      endif.
      if lt_fmtab-kunnr = '*' or lt_fmtab-kunnr = ''.
        lt_fmtab-kunnr = lwp_kunnr.
      endif.

      if lt_fmtab-vobelnr = lt_fmtab-knbelnr.
        l_downpay = 'X'.
      endif.

      at last.
        if l_downpay <> 'X'.
          lt_fmtab-trbtr =  lw_ap_wrbtr - l_trbtr . "Trn amt
          lt_fmtab-fkbtr =  lw_ap_dmbtr - l_fkbtr . "Local amt
          l_last = 'X'.
        endif.
      endat.

      if l_downpay = 'X'.
        lt_fmtab-trbtr =  lt_bseg-dmbtr .
        lt_fmtab-fkbtr =  lt_bseg-wrbtr .
      else.
        if l_last <> 'X'.
          clear lt_bset.
          read table lt_bset with key mwskz = lt_bseg-mwskz.
          lt_fmtab-trbtr =  lt_bseg-dmbtr / ( 1 + lt_bset-kbetr / 100 ).
          lt_fmtab-fkbtr =  lt_bseg-wrbtr / ( 1 + lt_bset-kbetr / 100 ).
          l_trbtr = l_trbtr + lt_fmtab-trbtr .
          l_fkbtr = l_fkbtr + lt_fmtab-trbtr .
        endif.
      endif.
      lt_fmtab-invamt = lt_bseg-dmbtr. "Net + Tax

      collect lt_fmtab.
      lw_glbtr  = lw_glbtr + lt_fmtab-fkbtr.
    endloop.

*---partial/residual payment -> scale to FM basis
    loop at lt_fmtab.
      if ( l_tax = space and lw_fmbtr <> lw_glbtr ).
        if lw_glbtr = 0.
          message s000 with 'Zero divide: '
                            lt_fmtab-kngjahr lt_fmtab-knbelnr.
        else.
          lt_fmtab-fkbtr  = lt_fmtab-fkbtr  * lw_fmbtr / lw_glbtr.
          lt_fmtab-trbtr  = lt_fmtab-trbtr  * lw_fmbtr / lw_glbtr.
          lt_fmtab-invamt = lt_fmtab-invamt * lw_fmbtr / lw_glbtr.
          modify lt_fmtab.
        endif.
      elseif lw_ap_dmbtr <> lw_glbtr.
        if lw_glbtr = 0.
          message s000 with 'Zero divide: '
                            lt_fmtab-kngjahr lt_fmtab-knbelnr.
        else.
          lt_fmtab-fkbtr  = lt_fmtab-fkbtr  * lw_ap_dmbtr / lw_glbtr.
          lt_fmtab-trbtr  = lt_fmtab-trbtr  * lw_ap_dmbtr / lw_glbtr.
          lt_fmtab-invamt = lt_fmtab-invamt * lw_ap_dmbtr / lw_glbtr.
          modify lt_fmtab.
        endif.
      endif.
    endloop.

  else.
*-1. Total match CM = FM  and not tax invoice, "No Fund
*  if tot_cmal-match = 'X' and l_tax = space.
** or ( l_tax = space and l_fund = space ).
    loop at lt_inv_pkg.
      l_idx = sy-tabix.

*-----Move to main FM table
      move-corresponding  lt_inv_pkg  to lt_fmtab.
      if tot_cmal-dmshb < 0.
        lt_fmtab-payflg = '-'.
      else.
        lt_fmtab-payflg = '+'.
      endif.

      lt_fmtab-zhldt   = tot_cmal-budat.   "POSTING DATE of pay.doc

      if lt_fmtab-lifnr = '*' or lt_fmtab-lifnr = ''.
        lt_fmtab-lifnr = lwp_lifnr.
      endif.
      if lt_fmtab-kunnr = '*' or lt_fmtab-kunnr = ''.
        lt_fmtab-kunnr = lwp_kunnr.
      endif.

      lt_fmtab-invamt = lt_fmtab-fkbtr.

      clear lt_fmtab-knbuzei.

      collect lt_fmtab.

*     g_pay_amt = g_pay_amt + lt_fmtab-fkbtr.
    endloop.

  endif.

* calc. accumulated amt.
  loop at lt_fmtab.
    at last.
      sum.
      g_pay_amt  = g_pay_amt  + lt_fmtab-fkbtr.   "local
      g_pay_damt = g_pay_damt + lt_fmtab-trbtr.   "document
    endat.
  endloop.

  append lines of lt_fmtab to fm_itab.

endform.                    " process_invoice_pkg
*&---------------------------------------------------------------------*
*&      Form  determine_fund_again
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->P_LT_BSEG_AUFNR  text
*      <--P_FM_ITAB_FONDS  text
*----------------------------------------------------------------------*
form determine_fund_again using    f_aufnr f_kstar
                          changing f_fonds.

  data : l_objnr like aufk-objnr.

  select single objnr into l_objnr from aufk
                where aufnr = f_aufnr.

  call function 'FM_CO_ASSIGNMENT_READ_OBJECT'
       exporting
            i_kokrs            = p_bukrs
            i_fikrs            = p_bukrs
            i_objnr            = l_objnr
            i_kstar            = f_kstar
       importing
            e_fonds            = f_fonds
*                 E_FICTR            = c_fistl
*                 E_fipex            = c_fipex
*                 E_FLG_FOUND_OBJECT = l_xfeld
       exceptions
            others             = 1.

endform.                    " determine_fund_again
*&---------------------------------------------------------------------*
*&      Form  calc_cm_fm_total
*&---------------------------------------------------------------------*
form calc_cm_fm_total.

*  Get the Totals of FMIFIIT Line items
  loop at  it_fm.
    tot_fmal-vogjahr = it_fm-vogjahr.
    tot_fmal-vobelnr = it_fm-vobelnr.
    tot_fmal-trbtr   = it_fm-trbtr.
    tot_fmal-fkbtr   = it_fm-fkbtr.
    collect tot_fmal.
  endloop.


* GET Totals of ZTFI_CMAL Table
  loop at it_cmal.
    tot_cmal-budat = it_cmal-budat.
    tot_cmal-bukrs = it_cmal-bukrs.
    tot_cmal-gjahr = it_cmal-gjahr.
    tot_cmal-belnr = it_cmal-belnr.
    tot_cmal-saknr = it_cmal-saknr.
    tot_cmal-wrshb = it_cmal-wrshb.
    tot_cmal-dmshb = it_cmal-dmshb.
    collect tot_cmal.
  endloop.

  sort tot_cmal by belnr   gjahr.
  sort tot_fmal by vobelnr vogjahr fkbtr.

endform.                    " calc_cm_fm_total
*&---------------------------------------------------------------------*
*&      Form  CHECK_SKBI_FMFPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_FM_ITAB_FIPEX  text
*----------------------------------------------------------------------*
form check_skbi_fmfpo   using    p_hkont
                        changing p_fm_itab_fipex.
  select single
         b~fipos into *fmfpo-fipos
         from skb1 as a
         inner join fmfpo as b
         on  b~fikrs eq a~bukrs
         and b~fipos eq a~fipos
         where a~bukrs eq p_bukrs
           and a~saknr eq p_hkont
           and b~datbis gt s_datum-low
           and b~fivor eq '30'.

  if sy-subrc eq 0.
    p_fm_itab_fipex = *fmfpo-fipos.
  endif.

endform.                    " CHECK_SKBI_FMFPO

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
form user_command using p_ucomm    like sy-ucomm
                        p_selfield type slis_selfield.
  case p_ucomm.
    when '&IC1' or '&ETA'.  "PICK.."
      read table it_fmal index p_selfield-tabindex.
      if sy-subrc = 0.
        case p_selfield-fieldname.
          when 'KNBELNR'.
            set parameter id:'BLN' field it_fmal-knbelnr,
                             'BUK' field p_bukrs,
                             'GJR' field it_fmal-kngjahr.
            call transaction 'FB03' and skip first screen.

          when 'BELNR'.
            set parameter id:'BLN' field it_fmal-belnr,
                             'BUK' field p_bukrs,
                             'GJR' field it_fmal-gjahr.
            call transaction 'FB03' and skip first screen.
        endcase.
      endif.
  endcase.

endform.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST_MAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form DISPLAY_LIST_MAIN .

  if p_dbread eq 'X' or p_test = 'X'.
* BEGIN OF UD1K956477
    IF p_bloc EQ 'X'.
      PERFORM delete_non_block_doc.
    ENDIF.
* END OF UD1K956477
    perform display_list.
  else.
*--collecting error list
    sort it_log by bukrs gjahr belnr.
    delete adjacent duplicates from it_log.

    if it_log[] is initial.
      message s000 with 'No errors.'.
    else.
      loop at it_log.
        if it_log-subrc eq '1'.
          perform write_item using :
                  'S'  '30' 'FM payment document not updated',
                  ' '  '10'  it_log-bukrs,
                  ' '  '04'  it_log-gjahr,
                  ' '  '10'  it_log-belnr,
                  'E'  '10'  it_log-budat.
        else.
          perform write_item using :
                 'S'  '30' 'Plan group not assigned',
                 ' '  '10'  it_log-bukrs,
                 ' '  '04'  it_log-gjahr,
                 ' '  '10'  it_log-belnr,
                 'E'  '10'  it_log-budat.
        endif.
      endloop.
      uline.
    endif.
  endif.

endform.                    " DISPLAY_LIST_MAIN
*&---------------------------------------------------------------------*
*&      Form  CHECK_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form CHECK_ITAB .

  describe table fm_itab lines sy-index.

  if sy-index = 0.
    message s001. exit.
  else.
    perform progress_ind using '70' text-206.
    perform fill_fmitab.
    free fm_itab.

*////////////////////////////////////////////////////////////////////
*by ig.moon 11/21/2007 {
*// === "Reduce record count by collect === //*
    if p_coll eq 'X'.
      data $it_fmal like it_fmal occurs 0 with header line.

      loop at it_fmal.
        $it_fmal = it_fmal.
        clear : $it_fmal-sgtxt,
                $it_fmal-vrefbt,
                $it_fmal-vrefbn,
                $it_fmal-vrfpos.
        collect $it_fmal.
      endloop.

      __cls  it_fmal.
      it_fmal[] = $it_fmal[].
    endif.
*}
*////////////////////////////////////////////////////////////////////
    if p_test = space.
      clear $subrc.
      perform progress_ind using '80' text-207.
      perform save_fm_actuals using $subrc.
      if $subrc eq 'E'.
        rollback work.
        message s000 with 'Failed to collecting'.
      else.
        message s007.
      endif.
    endif.

  endif.

endform.                    " CHECK_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_PROC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form GET_DATA_PROC .

*1.Get CM Data
  perform progress_ind using '00' text-200. "Reading from ztfi_cmal
  perform get_cm_actuals.

*2.Get FM data
  perform progress_ind using '10' text-201.  "Reading Fm Data
  perform get_fm_data.

*   IF  p_miss EQ 'X'.
  perform calc_cm_fm_total.
  perform check_totals.
*   ENDIF.

*3. Process FM package
  perform progress_ind using '20' text-202.
  perform process_fm_data.

* IF CM exist, but FM not exist
* UNASSIGNED IN/OUTGOING PAYMENT
  perform progress_ind using '30' text-208.
  perform process_fm_missing.

*  perform progress_ind using '30' text-203.
*  PERFORM get_internal_order.

*  perform progress_ind using '40' text-204.

*...get group/level from CM actuals
  perform progress_ind using '50' text-205.

*...if pay = invoice
  perform fill_blank_invoice_no.
* PERFORM assign_grupp_level.

*// Populating it_fmal internal table
  perform  check_itab.

endform.                    " GET_DATA_PROC
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA_ON_DATE
*&---------------------------------------------------------------------*
form DELETE_DATA_ON_DATE .

  delete from ztfi_fmal where DATUM in s_datum.

endform.                    " DELETE_DATA_ON_DATE
* BEGIN OF UD1K956477
*&---------------------------------------------------------------------*
*&      Form  DELETE_NON_BLOCK_DOC
*&---------------------------------------------------------------------*
*       Get only blocked document for display purpose
*----------------------------------------------------------------------*
FORM delete_non_block_doc.
  LOOP AT it_fmal WHERE hkont = '0000125000' AND belnr CP '9*'.
    gt_block-belnr = it_fmal-belnr.
    APPEND gt_block.
  ENDLOOP.

  SORT gt_block BY belnr.
  DELETE ADJACENT DUPLICATES FROM gt_block.

  LOOP AT it_fmal.
    g_tabix = sy-tabix.

    IF it_fmal-fipos = 'DUMMY' OR
       it_fmal-fipos = 'CASHUEX' OR
       it_fmal-fipos = 'CASHURV'.

      CONTINUE.
    ENDIF.

    READ TABLE gt_block WITH KEY belnr = it_fmal-belnr
                             BINARY SEARCH.
    IF sy-subrc <> 0.
      DELETE it_fmal INDEX g_tabix.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DELETE_NON_BLOCK_DOC
* END OF UD1K956477
