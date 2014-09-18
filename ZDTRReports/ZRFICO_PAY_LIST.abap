report zrfico_pay_list message-id e0.
*&--------------------------------------------------------------------&*
*&  Program id   : ZRMM_IDOC_LIST.
*&  Developer    : Furong
*&  Spec         : Andy Choi
*&--------------------------------------------------------------------&*
*& Date        Transport        Description
*& 09/2005                      initial program.
*
*&--------------------------------------------------------------------&*
ranges:r_hkont   for skb1-saknr occurs 0.
tables: bkpf.
data: begin of i_skb1 occurs 0,
        bukrs like skb1-bukrs,
        saknr like skb1-saknr,
        fdlev like skb1-fdlev,
        xgkon like skb1-xgkon,
      end of i_skb1.
data: begin of it_bkpf occurs 0,
      bukrs like bkpf-bukrs,
      belnr like bkpf-belnr,
      gjahr like bkpf-gjahr,
      budat like bkpf-budat,
      bldat like bkpf-bldat,
      lifnr like bseg-lifnr,
      xblnr like bkpf-xblnr,
      end of it_bkpf.
*DATA: it_bkpf_inv LIKE TABLE OF bkpf WITH HEADER LINE.
data: it_bkpf_inv like table of it_bkpf with header line.
data: it_bseg like table of bseg with header line.
data: it_bseg_inv like table of bseg with header line.
data: it_bsak like table of bsak with header line.
data: it_skat like table of skat with header line.
data: begin of it_lfa1 occurs 0,
        lifnr  like lfa1-lifnr,
        name1  like lfa1-name1,
        xcpdk  like lfa1-xcpdk,
      end of it_lfa1.

data: wa_bkpf like it_bkpf.
data: w_cash(1),
      w_ktopl like skat-ktopl.
data: p_f_date like sy-datum,
      p_e_date like sy-datum.

data: begin of it_output occurs 0,
      pjahr like bkpf-gjahr,
      belnr like bkpf-belnr,
      lifnr like bseg-lifnr,
      gjahr like bkpf-gjahr,
      invoice like bsak-belnr,
      buzei like bseg-buzei,
      chect like bkpf-xblnr,
      pamt  like bseg-dmbtr,
      xblnr like bkpf-xblnr,
      pudat like bkpf-budat, "payment date
      bldat like bkpf-bldat, "invoice doc. date
      budat like bkpf-budat, "invoce posting date
      hkont like bseg-hkont,
      txt20 like skat-txt20,
      shkzg like bseg-shkzg,
      iamt like bseg-dmbtr,
      kostl like bseg-kostl,
      mwskz like bseg-mwskz,
      name1 like lfa1-name1,
      end of it_output.


selection-screen begin of block blk1
                          with frame title text-001.
parameters : p_bukrs like bkpf-bukrs default 'H201' obligatory.
parameters : p_gjahr like bkpf-gjahr.
select-options: s_budat for it_bkpf-budat.
select-options: s_belnr for it_bkpf-belnr.

selection-screen end of block blk1.

selection-screen begin of block block2 with frame title text-007.
parameters : p_ztable type c as checkbox default 'X'.
parameters : p_show   type c as checkbox default 'X'.
selection-screen end of block block2.

select-options: s_blart for bkpf-blart.

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
initialization.
 s_blart-option = 'EQ'.
 s_blart-sign   = 'I'.
 s_blart-low    = 'ZP'. append s_blart.
 s_blart-low    = 'KZ'. append s_blart.
 s_blart-low    = 'ZV'. append s_blart.


start-of-selection.
  if p_ztable = 'X'.
    perform get_data_from_ztable.
    perform display_data.
  else.
    perform get_data.
    perform process_data.
    perform save_to_table.

    if p_show = 'X'.
      perform display_data.
    else.
      data: w_lines like sy-index.
      describe table it_output lines w_lines.
      write: 'COMPLETED', w_lines.
    endif.
  endif.


************************************
** FROM                           **
************************************
form get_data.

  select bukrs saknr fdlev xgkon
         from skb1   "G/L account master (company code)
         into corresponding fields of table i_skb1
         where bukrs = p_bukrs
           and xgkon = 'X'  "Cash receipt(disbursement) account
           and fdlev <> space.

  clear: r_hkont[],r_hkont.
  r_hkont-sign   = 'I'.     r_hkont-option   = 'EQ'.
  loop at i_skb1 where bukrs eq p_bukrs
                   and   xgkon eq 'X'
                   and ( fdlev cp 'B*' or  "bank account
                         fdlev cp 'C*' ).  "cash account
    move : i_skb1-saknr to r_hkont-low.
*           i_skb1-fdlev TO l_ebene.
    append r_hkont.
  endloop.

  select single ktopl into w_ktopl from t001 where bukrs = p_bukrs.

  select * from skat into table it_skat
                    where spras = sy-langu
                    and ktopl = w_ktopl.

  select * into corresponding fields of table it_lfa1
     from lfa1.

  select * from bkpf into corresponding fields of table it_bkpf
      where bukrs = p_bukrs
       and gjahr = p_gjahr
       and ( blart = 'ZP' or  blart = 'KZ' or blart = 'ZV' )
       and budat in s_budat
       and belnr in s_belnr
       and stblg = space.

  select * from bseg into table it_bseg
       for all entries in it_bkpf
      where bukrs = it_bkpf-bukrs
        and belnr = it_bkpf-belnr
        and gjahr = it_bkpf-gjahr.
  sort it_bseg by belnr koart.
*       AND HKONT IN R_HKONT.

  data: i_idx like sy-index.
  loop at it_bkpf.
*    i_idx = sy-index.
    loop at it_bseg where belnr = it_bkpf-belnr
                      and gjahr = it_bkpf-gjahr.
      if it_bseg-lifnr <> space.
        it_bkpf-lifnr = it_bseg-lifnr. modify it_bkpf.
        exit.
      endif.
    endloop.
  endloop.

  concatenate p_gjahr '0101' into p_f_date.
  concatenate p_gjahr '1231' into p_e_date.

  select * from bsak into table it_bsak
       for all entries in it_bkpf
      where bukrs = it_bkpf-bukrs
        and lifnr = it_bkpf-lifnr
        and augbl = it_bkpf-belnr
        and augdt between p_f_date and p_e_date.

  select * from bkpf into corresponding fields of
       table  it_bkpf_inv
       for all entries in it_bsak
       where bukrs = it_bsak-bukrs
        and belnr = it_bsak-belnr
        and gjahr = it_bsak-gjahr.

  select * from bseg into table it_bseg_inv
       for all entries in it_bsak
       where bukrs = it_bsak-bukrs
        and belnr = it_bsak-belnr
        and gjahr = it_bsak-gjahr.

endform.
*&---------------------------------------------------------------------*
*&      Form  append_data
*&---------------------------------------------------------------------*
form append_data.
  it_output-pjahr = it_bkpf-gjahr.
  it_output-pudat = it_bkpf-budat.

  it_output-buzei = it_bseg_inv-buzei.

  if it_output-shkzg = 'C'.
    it_output-iamt = - it_bseg_inv-dmbtr.
  else.
    it_output-iamt = it_bseg_inv-dmbtr.
  endif.

  it_output-kostl = it_bseg_inv-kostl.
  if it_output-kostl = space.
    it_output-kostl = it_bseg_inv-fistl.
  endif.

  it_output-mwskz = it_bseg_inv-mwskz.

  it_output-hkont = it_bseg_inv-hkont.
  read table it_skat with key saknr = it_bseg_inv-hkont.
  it_output-txt20 = it_skat-txt20.

  append it_output.

endform.                    " append_data
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
form process_data.

  loop at it_bkpf.
    clear: w_cash.
    it_output-belnr = it_bkpf-belnr.
    loop at it_bseg where belnr = it_bkpf-belnr
                      and gjahr = it_bkpf-gjahr
                      and hkont in r_hkont.
      w_cash = 'X'.
    endloop.
    if w_cash is initial.
      continue.
    endif.
* get pay ref.
    select single chect into it_output-chect
           from payr
           where zbukr = p_bukrs
             and gjahr = p_gjahr
             and vblnr = it_bkpf-belnr.
    if sy-subrc <> 0.
      it_output-chect = it_bkpf-xblnr.
    endif.

* get pay amt, vendor
    loop at it_bseg where belnr = it_bkpf-belnr
                      and gjahr = it_bkpf-gjahr.
      if it_bseg-hkont in r_hkont.
        it_output-pamt = it_bseg-dmbtr.
      else.
        if it_bseg-koart = 'K'.
          it_output-lifnr = it_bseg-lifnr.
* get vendor name
          read table it_lfa1 with key lifnr = it_output-lifnr.
          if it_lfa1-xcpdk = 'X'. "onetime
            select single name1 into it_output-name1
               from bsec
               where belnr = it_bkpf-belnr
                 and gjahr = it_bkpf-gjahr
                 and buzei = it_bseg-buzei.
          else.
            it_output-name1 = it_lfa1-name1.
          endif.

        else.
* discount....
          perform append_discount_misc.
        endif.
      endif.
    endloop.

*--------------------------------------------------
    perform process_invoice_doc.

    clear it_output.
  endloop.

endform.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  save_to_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_to_table.
  tables: ztfitaxap.
  delete from ztfitaxap where pjahr = p_gjahr.
  modify ztfitaxap  from table it_output.
  if sy-subrc eq 0.
    commit work.
  else.
    rollback work.
  endif.
endform.                    " save_to_table
*&---------------------------------------------------------------------*
*&      Form  get_data_from_ztable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data_from_ztable.
  select * into table it_output
                 from ztfitaxap
       where pjahr = p_gjahr
*       AND bukrs = p_bukrs
       and pudat in s_budat
       and belnr in s_belnr.
*       AND stblg = space.

endform.                    " get_data_from_ztable
*&---------------------------------------------------------------------*
*&      Form  append_discount_misc
*&---------------------------------------------------------------------*
form append_discount_misc.
* append from payment doc.
  it_output-gjahr   = it_bkpf-gjahr.
  it_output-xblnr   = it_bkpf-xblnr.
  it_output-invoice = it_bkpf-belnr.
  it_output-bldat   = it_bkpf-bldat.
  it_output-budat   = it_bkpf-budat.
  if it_bseg-shkzg = 'H'.
    it_output-shkzg = 'C'.
    it_output-iamt = - it_bseg-dmbtr.
  else.
    it_output-shkzg = 'D'.
    it_output-iamt = it_bseg-dmbtr.
  endif.
  it_output-kostl = it_bseg-kostl.
  if it_output-kostl = space.
    it_output-kostl = it_bseg-fistl.
  endif.
  it_output-mwskz = it_bseg-mwskz.
  it_output-hkont = it_bseg-hkont.
  read table it_skat with key saknr = it_bseg-hkont.
  it_output-txt20 = it_skat-txt20.
  it_output-pjahr = it_bkpf-gjahr.
  it_output-gjahr = it_bkpf-gjahr.
  it_output-buzei = it_bseg-buzei.
  it_output-pudat = it_bkpf-budat.
  append it_output.

endform.                    " append_discount_misc
*&---------------------------------------------------------------------*
*&      Form  process_invoice_doc
*&---------------------------------------------------------------------*
form process_invoice_doc.
  loop at it_bsak where augbl = it_bkpf-belnr.
    if it_bsak-augbl = it_bsak-belnr.
      continue.
    endif.

    it_output-gjahr   = it_bsak-gjahr.
    it_output-invoice = it_bsak-belnr.

    read table it_bkpf_inv into wa_bkpf
      with key belnr = it_bsak-belnr
               gjahr = it_bsak-gjahr.
    it_output-xblnr = wa_bkpf-xblnr.
    it_output-bldat = wa_bkpf-bldat.
    it_output-budat = wa_bkpf-budat.

* down payment
    if it_bsak-umsks = 'A'.  "UMSKZ = 'F'.
      read table it_bseg_inv with key belnr = it_bsak-belnr
                                      gjahr = it_bsak-gjahr
                                      buzei = it_bsak-buzei.

      if it_bsak-shkzg = 'H'.
        it_output-shkzg = 'D'.
      else.
        it_output-shkzg = 'C'.
      endif.
      perform append_data.
    else.

      loop at it_bseg_inv where belnr = it_bsak-belnr.
        if it_bseg_inv-buzei = it_bsak-buzei.
          continue.
        endif.
        if it_bseg_inv-shkzg = 'H'.
          it_output-shkzg = 'C'.
        else.
          it_output-shkzg = 'D'.
        endif.
        perform append_data.
      endloop.
    endif.

  endloop.
endform.                    " process_invoice_doc
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
form display_data.
  perform field_setting tables gt_fieldcat using :
   'PJAHR'     'PayYr'          '04' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BELNR'     'PayDoc#'        '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'LIFNR'     'Vendor'         '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'CHECT'     'Check/Ref'      '16' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'PUDAT'     'PayDate'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'PAMT '     'PayAmt'         '16' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
   'XBLNR'     'Invoice#'       '16' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'SHKZG'     'D/C'            '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'IAMT'      'Inv Amt'        '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'GJAHR'     'InvYr'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'INVOICE'   'InvDoc#'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BUZEI'     'InvLine'        '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BLDAT'     'InvDate'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'BUDAT'     'InvPsDt'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'HKONT'     'G/L Acct'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'TXT20'     'G/L Desc'       '20' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'KOSTL'     'Cost Center'    '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'MWSKZ'     'TAX'            '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'NAME1'     'Vendor Name'    '35' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       tables
            t_outtab           = it_output
       exceptions
            program_error      = 1
            others             = 2.

endform.                    " display_data
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
