*
* Provided by Andy
*
report zcorimprt
       no standard page heading line-size 255.

* Date      Developer    Request      Desc
* 05/04/06  Manju        UD1K920511   Prorgam bug to remove duplicates
*----------------------------------------------------------------------*
*   TABLE Declare                                                      *
*----------------------------------------------------------------------*
tables : ztbkpf,             " Import Cost Header.
         ztbdiv,             " Import Cost Distribution of P/O
         ztreqhd,            " Import Request Header Table
         ztreqit,            " Import Request Item Table
         ztbl,               " B/L Header Table
         ztblit,             " B/L Item Table
         ztsetac,            " Settled Account Management Table
         ekpo,               " P/O Item Table
         ekbe,               " G/R History Table
         ekbz,               " G/R History Item Table.
         t685t,              " Conditions: Types: Texts.
         mbew,               " Material Valuation
         usr01,              " User master record (runtime data)
         t001w,              " Plants/Branches
         t001k,              " Valuation area
         marv,               " Material Control Record
         t030,               " Standard Accounts Table
         ztimimg11,          " Account No. Management Table
         ztimimg00.          " Basic Config.

tables: t001,bsis.

*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.
parameters:     p_bukrs  like bkpf-bukrs  obligatory memory id buk,
                p_year   like bkpf-gjahr  obligatory ,
                p_month  like bkpf-monat  obligatory .
selection-screen skip 1.
select-options:  s_zuonr for bsis-zuonr.
*SELECT-OPTIONS: s_ebeln  FOR ekpo-ebeln,
*                s_ebelp  FOR ekpo-ebelp.
selection-screen skip 1.
parameters: p_all     as checkbox default 'X'.
selection-screen end of block b1.

parameters: p_budat  type budat.   "  DEFAULT sy-datum
selection-screen skip 1.
parameters: p_run     as checkbox,
            p_acc     like bsis-hkont  default '0000138400' no-display,
            p_act     like bsis-hkont  default '0000138490' no-display.

parameters: p_disp     as checkbox default ' '.

selection-screen skip 2.
include zcoi_bdc_inc.


*---------------------------------------------------------------------*
* program data
*---------------------------------------------------------------------*
tables: t100.
* for function
data: g_func(1) type c.

data:begin of t_line occurs 0,
        zuonr  like bsis-zuonr,
        blart  like bkpf-blart,
        gjahr  like bkpf-gjahr,
        belnr  like bkpf-belnr,
        buzei  like bsis-buzei,
        dmbtr  like bsis-dmbtr,
        menge  like bseg-menge,
        ebeln  like bseg-ebeln,
        ebelp  like bseg-ebelp,
        hkont  like bsis-hkont,
        meins  like bseg-meins,
        monat  like bkpf-monat,
        awkey  like bkpf-awkey,
        shkzg  like bsis-shkzg.
data end of t_line.


data:   g_bseg          like bseg,
        g_bkpf          like bkpf.

data : w_text(18)      type  c,
       g_menge(17)     type  c,
       g_meins(5)      type  c,
       w_lcno(20)      type  c,
       w_blno(20)      type  c,
       w_act_acc       like  ztbdiv-akont,
       w_pld_acc       like  ztbdiv-akont,
       w_co_acc        like  ztbdiv-akont.



*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
data: begin of it_po occurs 0,
   ebeln    like     ekpo-ebeln,
   ebelp    like     ekpo-ebelp,
   matnr    like     ekpo-matnr,
   knttp    like     ekpo-knttp,
   elikz    like     ekpo-elikz,
   meins    like     ekpo-meins,
end of it_po.

data : begin of it_blno occurs 0,
       zfblno  like ztblit-zfblno,
       ebeln   like ztblit-ebeln,
       ebelp   like ztblit-ebelp,
       end of it_blno.



data: begin of it_bsis occurs 0,
   zuonr    like       bsis-zuonr,
end of it_bsis.

data : begin of it_inv occurs 0,
       ebeln           like   ztbdiv-ebeln,      " PO Header.
       ebelp           like   ztbdiv-ebelp,      " PO Item.
       kschl           like   ztbdiv-cond_type,  " Condition
       dmbtr           like   ztbdiv-dmbtr,      " Amount(Local)
       menge           like   ztbdiv-menge,      " Quantity
       avgamt          like   ztbdiv-dmbtr,      " AVG
       cnt             like   sy-index,

       dmbtr2          like   ztbdiv-dmbtr,      " amt
       menge2          like   ztbdiv-menge,      " qty

       gramt           like   ztbdiv-dmbtr,      " amt
       grqty           like   ztbdiv-menge,      " qty

       hwaer           like   ztbdiv-hwaer,      " Currency
       meins           like   ztbdiv-meins,      " Unit of measure

       hkont           like   bsis-hkont,
       matnr           like   ztbdiv-matnr,      " Material
       zfimdno         like   ztbdiv-zfimdno,    " Import Doc. No.
       bukrs           like   ztbdiv-bukrs,      " Company Code
       zfacdo          like   ztbkpf-zfacdo,     " Account No
       zffiyr          like   ztbkpf-zffiyr,     " Account Year
       cstgrp          like   ztbdiv-zfcstgrp,   " Cost Group.
       wrbtr           like   ztbdiv-wrbtr.      " Amount
data : end   of it_inv.

data : begin of it_ekbz occurs 0,
       ebeln           like   ekbz-ebeln,        " PO Header
       ebelp           like   ekbz-ebelp,        " PO Item
       kschl           like   ekbz-kschl,        " Condition
       dmbtr           like   ekbz-dmbtr,        " Amount(Local)
       menge           like   ekbz-menge,        " Quanitity
       hswae           like   ekbz-hswae,        " Currency
       meins           like   ekpo-meins,        " Unit of Measure
       gjahr           like   ekbz-gjahr,        " Account Year
       belnr           like   ekbz-belnr.        " Account No
*      shkzg           like   ekbz-shkzg,        " Debit/Credit Type
data : end   of it_ekbz.

data : begin of it_tab occurs 0,
       ebeln           like   ekbz-ebeln,
       ebelp           like   ekbz-ebelp,
       matnr           like   ekpo-matnr,
       knttp           like   ekpo-knttp,
       meins           like   ekpo-meins,
       elikz           like   ekpo-elikz,
       iv_qty          like   ekpo-menge,
       gr_qty          like   ekpo-menge,
       set_qty         like   ekpo-menge,
       iv_amt          like   ekbz-dmbtr,
       gr_amt          like   ekbz-dmbtr,
       set_pln         like   ekbz-dmbtr,
       set_act         like   ekbz-dmbtr,
       var_amt         like   ekbz-dmbtr,
       bal_amt         like   ekbz-dmbtr,  "actual
       err_amt         like   ekbz-dmbtr.
data : end   of  it_tab.

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
      gv_user_command type slis_formname value 'USER_COMMAND'.

*---- ALV
data: g_blart like bkpf-blart value 'SV',
      w_budat like  sy-datum.

*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
initialization.
  group = 'IMPORT SETTLE'.


*-----------------------------------------------------------------------
* START OF SELECTION .
*-----------------------------------------------------------------------
start-of-selection.
  perform determine_posting_date.

  " P/O SELECT.
  perform  select_po_data.
  perform  select_open_item.
  perform  calculate_settlement.

*-----------------------------------------------------------------------
end-of-selection.
  if p_disp = space.
    delete it_tab where set_act eq 0 and set_pln eq 0.
  endif.

  sort it_tab by matnr ebeln ebelp.

  if p_run ne  'X'.
    perform display_out.   "ANDY
  endif.

  if p_run = 'X'.
    perform  get_bolno.
    perform  run_settlement.
  endif.

*&---------------------------------------------------------------------*
*&      Form  determine_posting_date
*&---------------------------------------------------------------------*
form determine_posting_date.
  data: l_budat like sy-datum.

  select single * from t001 where bukrs = p_bukrs.

  if p_budat is initial.
    concatenate  p_year  p_month  '01'  into  l_budat.

    call function 'MM_ARRANG_GET_END_OF_MONTH'
         exporting
              i_datum = l_budat
         importing
              e_datum = p_budat.
  endif.

  select single * from usr01
  where  bname    eq   sy-uname.

  case usr01-datfm.
    when  '1'.
      concatenate p_budat+6(2)  p_budat+4(2)  p_budat(4) into w_budat.
    when  '2'.
      concatenate p_budat+4(2)  p_budat+6(2)  p_budat(4) into w_budat.
    when  '3'.
      concatenate p_budat+4(2)  p_budat+6(2)  p_budat(4) into w_budat.
    when  others.
      w_budat =  p_budat.
  endcase.

*read account setting.
  select single * from ztimimg11 where bukrs eq p_bukrs.

endform.                    " determine_posting_date
*&---------------------------------------------------------------------*
*&      Form  check_im_invoice
*&---------------------------------------------------------------------*
form check_im_invoice using    p_rc.
  select  single b~cond_type as kschl into it_inv-kschl
    from  ztbkpf as a  inner  join  ztbdiv as b
      on  a~bukrs      eq  b~bukrs
      and a~gjahr      eq  b~gjahr
      and a~belnr      eq  b~belnr
    where b~ebeln      eq  it_po-ebeln
      and b~ebelp      eq  it_po-ebelp
      and a~zfacdo     =   t_line-belnr
      and a~zffiyr     =   t_line-gjahr.

  p_rc = sy-subrc.
endform.                    " check_im_invoice
*&---------------------------------------------------------------------*
*&      Form  check_qty_sign
*&---------------------------------------------------------------------*
form check_qty_sign.
  select single * into g_bseg from bseg
     where bukrs = p_bukrs
       and gjahr = t_line-gjahr
       and belnr = t_line-belnr
       and buzei = t_line-buzei.

  if t_line-hkont = p_acc.
    if t_line-shkzg = 'S'.
      t_line-dmbtr = - t_line-dmbtr.
      t_line-menge = - g_bseg-menge.
    else.
      t_line-menge =   g_bseg-menge.
    endif.
  else.
    if t_line-shkzg = 'H'.
      t_line-dmbtr = - t_line-dmbtr.
      t_line-menge = - g_bseg-menge.
    else.
      t_line-menge = g_bseg-menge.
    endif.
  endif.
endform.                    " check_qty_sign
*&---------------------------------------------------------------------*
*&      Form  collect_ekbz
*&---------------------------------------------------------------------*
form collect_ekbz.
  data: l_cnt like sy-index.
* accrual
*       PERFORM get_accrual USING l_budt1 l_budt2.
  it_ekbz-ebeln = t_line-ebeln.
  it_ekbz-ebelp = t_line-ebelp.
  it_ekbz-dmbtr = t_line-dmbtr.
*QTY doubled..FIXME
  select  count( * )  into l_cnt from    ekbz
      where ebeln = t_line-ebeln
        and ebelp = t_line-ebelp
        and gjahr = t_line-awkey+10(4)
        and belnr = t_line-awkey(10)
        and vgabe = '1'
        and bewtp = 'C'.

  if l_cnt eq 0.
    it_ekbz-menge = 0.
    it_ekbz-dmbtr = 0.
  else.
    it_ekbz-menge = t_line-menge / l_cnt.
  endif.

  collect it_ekbz.
endform.                    " collect_ekbz
*&---------------------------------------------------------------------*
*&      Form  collect_inv
*&---------------------------------------------------------------------*
form collect_inv.
* actual
*        PERFORM check_im_invoice USING sy-subrc.
*        CHECK sy-subrc = 0.
  it_inv-ebeln  = t_line-ebeln.
  it_inv-ebelp  = t_line-ebelp.

  if t_line-hkont = p_act.
    it_inv-dmbtr  = t_line-dmbtr.
    it_inv-menge  = t_line-menge.

    if t_line-dmbtr > 0.
      it_inv-cnt = 1.
    else.
      it_inv-cnt = -1.
    endif.

  else.
* planned, change sign ?
    if t_line-blart <> 'WE'.
      it_inv-dmbtr2  =  t_line-dmbtr.
      it_inv-menge2  =  t_line-menge.
    endif.
  endif.

  if t_line-blart = 'WE'.
    it_inv-gramt = t_line-dmbtr.
    it_inv-grqty = t_line-menge.
  endif.

  collect it_inv.  clear it_inv.

endform.                    " collect_inv
*&---------------------------------------------------------------------*
*&      Form  post_mm_document
*&---------------------------------------------------------------------*
form post_mm_document.
  data: l_wrbtr(16)  type  c.

  check it_tab-var_amt <> 0.

* post FI & ML
  refresh : bdcdata.

  write it_tab-var_amt to  l_wrbtr currency  t001-waers.
  perform    p2000_write_no_mask     changing  l_wrbtr.

  perform bdc_field using :
          'X' 'SAPRCKM_MR22'           '0201',
          ' ' 'MR21HEAD-BUDAT'         w_budat,
          ' ' 'MR21HEAD-BUKRS'         p_bukrs,
          ' ' 'MR21HEAD-WERKS'         ekpo-werks,
          ' ' 'MR21HEAD-XBLNR'         w_text,
          ' ' 'MR21HEAD-BKTXT'         'IMPORT SETTLEMENT',
          ' ' 'BDC_OKCODE'             '=ENTR'.

  perform bdc_field using :
       'X' 'SAPRCKM_MR22'          '0201',
       ' ' 'MR21HEAD-SCREEN_VARIANT'  'LAGERMATERIAL - OHNE BWKEY_025',
       ' ' 'CKI_MR22_0250-MATNR(01)'  it_tab-matnr,
       ' ' 'CKI_MR22_0250-ZUUMB(01)'  l_wrbtr,
       ' ' 'BDC_OKCODE'               '=ENTR'.

  perform bdc_field using :
          'X' 'SAPRCKM_MR22'           '0201',
       ' ' 'BDC_OKCODE'                '=SAVE'.

  set parameter id 'MLN' field ''.        " Document No
  set parameter id 'MLJ' field ''.        " Document Year

  perform bdc_transaction using 'MR22'.

endform.                    " post_mm_document
*&---------------------------------------------------------------------*
*&      Form  select_po_data
*&---------------------------------------------------------------------*
form select_po_data.

  refresh : it_po, it_bsis.

  if s_zuonr[] is initial.
    select  zuonr                                           "UD1K919990
      into corresponding fields of table it_bsis
      from bsis
    where bukrs eq p_bukrs                                  "UD1K919990
          and gjahr <= p_year                               "UD1K919990
          and budat <= p_budat                              "UD1K919990
           and  hkont  in   (p_acc, p_act).
  else.
*    RANGES: r_zuonr FOR bsis-zuonr.
*    refresh r_zuonr.
*    r_zuonr-sign = 'I'. r_zuonr-option = 'EQ'.
*    SELECT * FROM ekpo
*      WHERE ebeln IN s_ebeln
*        AND ebelp IN s_ebelp.
*      CONCATENATE ekpo-ebeln ekpo-ebelp INTO r_zuonr-low.
*      APPEND r_zuonr.
*    ENDSELECT.

*  select distinct zuonr    "UD1K919990
    select  zuonr                                           "UD1K919990
      into corresponding fields of table it_bsis
      from bsis
    where bukrs eq p_bukrs                                  "UD1K919990
          and gjahr <= p_year                               "UD1K919990
          and budat <= p_budat                              "UD1K919990
           and  hkont  in   (p_acc, p_act)
           and zuonr in s_zuonr.
  endif.

  delete adjacent duplicates from it_bsis comparing zuonr. ""UD1K919990

  loop at it_bsis.
    move : it_bsis-zuonr(10)     to  it_po-ebeln,
           it_bsis-zuonr+10(05)  to  it_po-ebelp.

    select single * from ekpo
      where ebeln = it_po-ebeln  ""UD1K919990
        and ebelp = it_po-ebelp.   ""UD1K919990
*      where ebeln = it_bsis-zuonr(10)  ""UD1K919990
*        and ebelp = it_bsis-zuonr+10(05). ""UD1K919990

    if not ekpo-knttp is initial.
      continue.
    endif.

    it_po-matnr = ekpo-matnr.
    it_po-elikz = ekpo-elikz.
    it_po-knttp = ekpo-knttp.
    it_po-meins = ekpo-meins.

    append it_po.
  endloop.

*   DELETE ADJACENT DUPLICATES FROM IT_BSIS COMPARING ALL FIELDS.

endform.                    " select_po_data
*&---------------------------------------------------------------------*
*&      Form  select_open_item
*&---------------------------------------------------------------------*
form select_open_item.

  data: l_budt1         like sy-datum,   "to
        l_budt2         like sy-datum,   "from
        l_budt3         like sy-datum,   "prev.end date
        l_cnt           like sy-index.

* select table...
  perform get_dates  using l_budt1  l_budt2  l_budt3.
  perform get_openitem  using l_budt1  l_budt2.

  sort t_line by ebeln ebelp.

  loop at t_line.
    clear: it_ekbz, it_inv.

    perform check_qty_sign.
*----------------------------------------------------------
    if p_all eq 'X'.
      perform collect_ekbz.
    else.
      if t_line-blart = 'WE'.
        perform collect_ekbz.
      endif.
    endif.

    perform collect_inv.
  endloop.

  sort : it_inv  by  ebeln ebelp,
         it_ekbz by  ebeln ebelp.

  loop at it_inv.
    if it_inv-cnt = 0.
      it_inv-menge = 0.
    else.
      it_inv-menge = it_inv-menge / abs( it_inv-cnt ).
    endif.
    modify it_inv index sy-tabix.
  endloop.

endform.                    " select_open_item
*&---------------------------------------------------------------------*
*&      Form  get_dates
*&---------------------------------------------------------------------*
form get_dates using    p_l_budt1
                        p_l_budt2
                        p_l_budt3.

  concatenate p_year p_month(2) '01' into p_l_budt2.

  call function 'MM_ARRANG_GET_END_OF_MONTH'
       exporting
            i_datum = p_l_budt2
       importing
            e_datum = p_l_budt1.

  call function 'CCM_GO_BACK_MONTHS'
       exporting
            currdate   = p_l_budt1
            backmonths = 1
       importing
            newdate    = p_l_budt3.
endform.                    " get_dates
*&---------------------------------------------------------------------*
*&      Form  get_openitem
*&---------------------------------------------------------------------*
form get_openitem  using  p_to_date  p_fr_date.

  select *
*    INTO CORRESPONDING FIELDS OF TABLE t_line
     into corresponding fields of table t_line
     from bsis as a  inner join bkpf as b
       on a~bukrs      eq  b~bukrs
      and a~gjahr      eq  b~gjahr
      and a~belnr      eq  b~belnr
     where a~bukrs = p_bukrs
       and a~budat <=  p_to_date
       and a~hkont in  (p_acc, p_act)
       and a~zuonr in s_zuonr..


*    t_line-ebeln = t_line-zuonr(10).
*    t_line-ebelp = t_line-zuonr+10(5).
*    check t_line-ebeln in s_ebeln and t_line-ebelp in s_ebelp.
*
*    append t_line.
*
*  endselect.


  loop at t_line.
    t_line-ebeln = t_line-zuonr(10).
    t_line-ebelp = t_line-zuonr+10(5).
    modify t_line transporting ebeln ebelp.
  endloop.
endform.                    " get_openitem
*&---------------------------------------------------------------------*
*&      Form  run_settlement
*&---------------------------------------------------------------------*
form run_settlement.
  data : l_cnt type i.
  loop at it_tab.
*    add 1 to l_cnt.
    at first.
      perform open_group.
    endat.

*    if it_tab-set_act eq 0 and it_tab-set_pln eq 0.
*      continue.
*    endif.
    add 1 to l_cnt.
    " Settlement Account,
    perform  p6000_settle_account_get.
    " L/C No OR B/L No Get.
    perform  p6000_refrence_no_get.
    concatenate it_tab-ebeln '-' it_tab-ebelp into w_text.

    perform post_fi_document.
    perform post_mm_document.

    if l_cnt = p_scount.
      perform close_group.
      perform open_group.
      clear l_cnt.
    endif.
*    at last.
*      perform close_group.
*    endat.
  endloop.

*  if l_cnt <= p_cnt .
  perform close_group.
*  endif.


endform.                    " run_settlement
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
form display_out.
  perform field_setting tables gt_fieldcat using :
   'EBELN'      'PO'            '10' 'X' 'L'  ' '  ' '  '  ' ' '  ' ',
   'EBELP'      'ITM'           '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'ELIKZ'      'CHK'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'IV_QTY'     'IV_QTY'        '09' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',
   'GR_QTY'     'GR_QTY'        '09' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',
   'SET_QTY'    'SETQTY'        '09' ' ' 'L'  ' '  '0'  '  ' ' '  ' ',

   'IV_AMT'     'IV_AMT'        '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'GR_AMT'     'GR_AMT'        '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'ERR_AMT'    'RECON'         '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'SET_PLN'    '138400'        '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'SET_ACT'    '138490'        '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'VAR_AMT'    'VARAMT'        '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'BAL_AMT'    'BALANCE'       '13' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
   'MATNR'      'Material'      '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
   'KNTTP'      'AAC'           '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  w_program = sy-repid.

*  call function 'REUSE_ALV_LIST_DISPLAY'
*       exporting
*            i_callback_program = w_program
*            it_fieldcat        = gt_fieldcat
*            i_save             = 'A'
*       tables
*            t_outtab           = it_tab
*       exceptions
*            program_error      = 1
*            others             = 2.
*

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = w_program
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       tables
            t_outtab           = it_tab
       exceptions
            program_error      = 1
            others             = 2.

endform.                    " display_out
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
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
form p2000_write_no_mask changing p_text_amount.

  select single * from usr01 where bname eq sy-uname.

  case usr01-dcpfm.
    when 'X'.    " Decimal point is period: N,NNN.NN
      perform    p2000_change_symbol    using p_text_amount ',' ' '.
      condense         p_text_amount    no-gaps.
    when 'Y'.    " Decimal point is N NNN NNN,NN
      perform    p2000_change_symbol    using p_text_amount  ',' '.'.
      condense         p_text_amount    no-gaps.
    when others. " Decimal point is comma: N.NNN,NN
      perform    p2000_change_symbol    using p_text_amount  '.' ' '.
      perform    p2000_change_symbol    using p_text_amount  ',' '.'.
      condense         p_text_amount    no-gaps.
  endcase.

endform.                    " P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
form p2000_change_symbol using    p_amount  p_from  p_to.

  do.
    replace  p_from   with   p_to  into    p_amount.
    if  sy-subrc  <>    0.
      exit.
    endif.
  enddo.

endform.                    " P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*&      Form  P6000_REFRENCE_NO_GET
*&---------------------------------------------------------------------*
form p6000_refrence_no_get.

  clear : w_lcno, w_blno.

  " L/C No Get.
  select single * from ztreqhd where ebeln eq it_tab-ebeln.
  move  ztreqhd-zfopnno  to  w_lcno.
  if w_lcno is initial.
    move  'N/A'       to  w_lcno.
  endif.

  " B/L No Get.
  read table  it_blno with key ebeln = it_tab-ebeln
                               ebelp = it_tab-ebelp.
  if sy-subrc eq 0.
*  select   * from ztblit
*  where    ebeln       eq    it_tab-ebeln
*  and      ebelp       eq    it_tab-ebelp
*  order by ZFSHNO descending.
*    exit.
*  endselect.
*  order by zfblno descending.

    select single * from ztbl
    where  zfblno   eq   it_blno-zfblno.
    move  ztbl-zfhblno   to  w_blno.
  endif.
*  endselect.

endform.                    " P6000_REFRENCE_NO_GET
*&---------------------------------------------------------------------*
*&      Form  P6000_SETTLE_ACCOUNT_GET
*&---------------------------------------------------------------------*
form p6000_settle_account_get.

  clear : ekpo, w_act_acc, w_pld_acc, w_co_acc.
  select single * from ekpo
   where ebeln    eq   it_tab-ebeln
     and ebelp    eq   it_tab-ebelp.

  " Actual Account
  perform   p7000_get_account  using ekpo-matnr
                                     ekpo-werks
                                     'ZR3'
                            changing w_act_acc.

  " Planned Account
  perform   p7000_get_account  using ekpo-matnr
                                     ekpo-werks
                                     'FR3'
                            changing w_pld_acc.

  " CO Account
  perform   p7000_get_account  using ekpo-matnr
                                     ekpo-werks
                                     'UMB'
                            changing w_co_acc.

endform.                    " P6000_SETTLE_ACCOUNT_GET
*&---------------------------------------------------------------------*
*&      Form  P7000_GET_ACCOUNT
*&---------------------------------------------------------------------*
form p7000_get_account using    p_matnr
                                p_werks
                                p_acc_key
                       changing p_account.

  clear : mbew, t001w, t001k, t030.

  " Valuation Class
  select single * from mbew
  where  matnr    eq   p_matnr
  and    bwkey    eq   p_werks.

  " Valuation Modification Key
  select single * from t001w
  where  werks    eq   p_werks.

  " Valuation Modification Group
  select single * from t001k
  where  bukrs    eq   p_bukrs
  and    bwkey    eq   t001w-bwkey.

  if ztimimg11-zfvcyn eq 'X'.
    if ztimimg11-zfvmyn eq 'X'.
      select single *  from  t030
      where  ktopl     eq    ztimimg11-ktopl
      and    ktosl     eq    p_acc_key
      and    bwmod     eq    t001k-bwmod
      and    komok     eq    ztimimg11-komok
      and    bklas     eq    mbew-bklas.
    else.
      select single *  from  t030
      where  ktopl     eq     ztimimg11-ktopl
      and    ktosl     eq     p_acc_key
      and    komok     eq     ztimimg11-komok
      and    bklas     eq     mbew-bklas.
    endif.
  else.
    if ztimimg11-zfvmyn eq 'X'.
      select single * from t030
      where  ktopl    eq   ztimimg11-ktopl
      and    ktosl    eq   p_acc_key
      and    bwmod    eq   t001k-bwmod
      and    komok    eq   ztimimg11-komok.
    else.
      select single * from t030
      where  ktopl    eq   ztimimg11-ktopl
      and    ktosl    eq   p_acc_key
      and    komok    eq   ztimimg11-komok.
    endif.
  endif.

  p_account  = t030-konts.

endform.                    " P7000_GET_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  add_new_line
*&---------------------------------------------------------------------*
form add_new_line using    f_pk  f_acc.
  perform bdc_field using :
          ' ' 'RF05A-NEWBS' f_pk,
          ' ' 'RF05A-NEWKO' f_acc.

endform.                    " add_new_line
*&---------------------------------------------------------------------*
*&      Form  add_line_detail
*&---------------------------------------------------------------------*
form add_line_detail using    f_amt.
  data: l_wrbtr(16)  type  c,
        abs_amt  like bsis-dmbtr.

  abs_amt = abs( f_amt ).
  write abs_amt to  l_wrbtr currency  t001-waers.
  perform p2000_write_no_mask  changing  l_wrbtr.

  perform bdc_field using :
          ' ' 'BSEG-WRBTR'  l_wrbtr,
          ' ' 'BSEG-EBELN'  it_tab-ebeln,
          ' ' 'BSEG-EBELP'  it_tab-ebelp,
          ' ' 'BSEG-MENGE'  g_menge,
          ' ' 'BSEG-MEINS'  g_meins,
          ' ' 'BSEG-SGTXT'  it_tab-matnr.

endform.                    " add_line_detail
*&---------------------------------------------------------------------*
*&      Form  post_fi_document
*&---------------------------------------------------------------------*
form post_fi_document.
  data: abs_amt  like bsis-dmbtr,
        abs_qty  like bseg-menge.

  refresh : bdcdata.
  abs_qty = abs( it_tab-set_qty ).
  write abs_qty        to  g_menge   unit  it_tab-meins.
  write it_tab-meins   to  g_meins.


  perform bdc_field      using 'X' 'SAPMF05A'    '0100'.
  perform bdc_field       using:
          ' ' 'BDC_CURSOR'  'RF05A-NEWKO',
          ' ' 'BDC_OKCODE'  '/00'.

  perform bdc_field       using:
          ' ' 'BKPF-BLDAT'  w_budat,
          ' ' 'BKPF-BUDAT'  w_budat,
          ' ' 'BKPF-BLART'  g_blart,
          ' ' 'BKPF-BUKRS'  p_bukrs,
*         ' ' 'BKPF-MONAT'  p_budat+4(2),
          ' ' 'BKPF-WAERS'  t001-waers,
          ' ' 'BKPF-XBLNR'  w_text,
          ' ' 'BKPF-BKTXT' 'IMPORT SETTLEMENT'.



* plan
  if it_tab-set_pln <> 0.
    if it_tab-set_pln > 0.
      perform add_new_line  using '40' w_pld_acc.
    elseif it_tab-set_pln < 0.
      perform add_new_line  using '50' w_pld_acc.
    endif.

    perform bdc_field       using 'X' 'SAPMF05A' '0300'.
    perform bdc_field       using:
            ' ' 'BDC_CURSOR'    'RF05A-NEWKO',
            ' ' 'BDC_OKCODE'    '/00'.

    perform add_line_detail    using it_tab-set_pln.
  endif.

*actual
  if it_tab-set_act <> 0.
    if it_tab-set_act > 0.
      perform add_new_line  using '40' w_act_acc.
    elseif it_tab-set_act < 0.
      perform add_new_line  using '50' w_act_acc.
    endif.

    if it_tab-set_pln <> 0.
      perform add_fmore  using ' '.   "for plan
    endif.

    perform bdc_field       using:
            'X' 'SAPMF05A'      '0300',
            ' ' 'BDC_CURSOR'    'RF05A-NEWKO',
            ' ' 'BDC_OKCODE'    '=ZK'.

    perform add_line_detail    using it_tab-set_act.
  endif.

* variance
  if it_tab-var_amt = 0.
    perform add_fmore  using ' '.   "for actual
    perform bdc_field       using:
            'X'  'SAPMF05A'    '0330',
            ' '  'BDC_OKCODE'  '=ZK'.

  else.
    if it_tab-var_amt > 0.
      perform add_new_line  using '40' w_co_acc.
    else.
      perform add_new_line  using '50' w_co_acc.
    endif.

    if it_tab-set_act <> 0. "d it_tab-set_pln <> 0.
      perform add_fmore  using ' '.   "for actual

      perform bdc_field       using:
              'X'  'SAPMF05A'    '0330',
              ' '  'BSEG-XREF3'  w_blno,
*              ' '  'BDC_OKCODE'  '=ZK'.
              ' '  'BDC_OKCODE'  '/00'.
*      if it_tab-var_amt > 0.
*        perform add_new_line  using '40' w_co_acc.
*      else.
*        perform add_new_line  using '50' w_co_acc.
*      endif.

    else.
      perform add_fmore  using ' '.   "for plan
*      perform bdc_field       using:
*              'X' 'SAPMF05A'      '0300',
*              ' ' 'BDC_CURSOR'    'RF05A-NEWKO',
*              ' ' 'BDC_OKCODE'    '/=ZK'.
    endif.


    perform bdc_field       using:
            'X' 'SAPMF05A'      '0300',
            ' ' 'BDC_CURSOR'    'RF05A-NEWKO',
            ' ' 'BDC_OKCODE'    '/00'.

    perform add_line_detail    using it_tab-var_amt.
    perform bdc_field       using:
          ' ' 'BDC_OKCODE'    '=AB'.

    perform add_fmore  using ' '.   "for plan


  endif.

*  PERFORM add_fmore  USING ' '.   "for plan  - Manju
*  PERFORM bdc_field       USING:            "Manju
*              'X'  'SAPMF05A'    '0330',
*              ' '  'BSEG-XREF3'  w_blno,
*              ' '  'BDC_OKCODE'  '=ZK'.
*
*
*
*    PERFORM bdc_field       USING:
*          ' ' 'BDC_OKCODE'    '=AB'.
*
*  PERFORM bdc_field       USING:             " Manju
*            'X' 'SAPMF05A'      '0300',
*            ' ' 'BDC_CURSOR'    'RF05A-NEWKO',
*            ' ' 'BDC_OKCODE'    '/BU'.
*
*   PERFORM add_line_detail    USING it_tab-var_amt. "manju
*
*    PERFORM bdc_field       USING:
*          ' ' 'BDC_OKCODE'    '=00'.   "Manju
*
*    PERFORM add_fmore  USING ' '.   "for plan - Manju


  perform bdc_field       using:
          'X'  'SAPMF05A'     '0700',
          ' '  'BDC_CURSOR'   'RF05A-NEWBS',
          ' '  'BDC_OKCODE'   '=BU'.


  perform bdc_transaction using 'FB01'.

endform.                    " post_fi_document
*&---------------------------------------------------------------------*
*&      Form  add_fmore
*&---------------------------------------------------------------------*
form add_fmore  using fmore.

  if fmore = 'X'.
    perform bdc_field       using:
            ' ' 'DKACB-FMORE'   'X'.
  endif.

  perform bdc_field       using:
          'X' 'SAPLKACB'      '0002',
          ' ' 'BDC_OKCODE'    '=ENTE',
          ' ' 'COBL-MATNR'    it_tab-matnr.
endform.                    " add_fmore
*&---------------------------------------------------------------------*
*&      Form  calculate_settlement
*&---------------------------------------------------------------------*
form calculate_settlement.
  data: l_iv   like sy-subrc,
        l_gr   like sy-subrc,
        w_inv  like it_inv,
        w_ekbz like it_ekbz.

  refresh : it_tab.
  sort it_po   by  ebeln ebelp.

  loop  at  it_po.
    clear: it_tab, it_inv, it_ekbz.

    move : it_po-ebeln  to  it_tab-ebeln,
           it_po-ebelp  to  it_tab-ebelp,
           it_po-matnr  to  it_tab-matnr,
           it_po-elikz  to  it_tab-elikz,
           it_po-meins  to  it_tab-meins,
           it_po-knttp  to  it_tab-knttp.

* IV exist...
    read table it_inv with key ebeln = it_po-ebeln
                               ebelp = it_po-ebelp.
    l_iv = sy-subrc.
    if l_iv = 0.
      it_tab-iv_amt = it_inv-dmbtr.
      it_tab-iv_qty = it_inv-menge.
    endif.

    read table it_ekbz with key ebeln = it_po-ebeln
                                ebelp = it_po-ebelp.
    l_gr = sy-subrc.

* skip if no data exist in PO.
    if l_iv <> 0 and l_gr <> 0. continue. endif.

    it_tab-gr_amt  = it_ekbz-dmbtr.
    it_tab-gr_qty  = it_ekbz-menge.
    it_tab-set_pln = it_ekbz-dmbtr.

*elseif GR was completed.
    if it_po-elikz = 'X'.
      it_tab-set_act = - it_tab-iv_amt.
      it_tab-set_qty = it_tab-iv_qty.

* GR exist...
    else.
      it_tab-set_qty = it_tab-gr_qty.
      if it_tab-iv_qty < it_tab-gr_qty.
        it_tab-set_qty = it_tab-iv_qty.
      endif.

* settle all invoice...
      if p_all = 'X' and l_gr = 0.
        it_tab-set_act = - it_tab-iv_amt.
        it_tab-set_qty = it_tab-iv_qty.

      else.
        if it_tab-iv_qty = 0.
          it_tab-set_act = - it_tab-iv_amt.
        else.

          it_tab-set_act = - it_tab-iv_amt *
                           abs( it_tab-set_qty / it_tab-iv_qty ).
        endif.
      endif.

    endif.


    it_tab-err_amt = it_inv-dmbtr2.
    it_tab-set_pln = it_tab-set_pln + it_tab-err_amt.
    it_tab-var_amt = - it_tab-set_pln - it_tab-set_act.

    it_tab-bal_amt = it_tab-iv_amt + it_tab-set_act.

    append it_tab.

  endloop.
  delete adjacent duplicates from it_tab.                   "UD1K920511
endform.                    " calculate_settlement
*&---------------------------------------------------------------------*
*&      Form  get_bolno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_bolno.
  if not it_po[] is initial.
    select zfblno ebeln ebelp into table it_blno
       from ztblit for all entries in it_po
         where ebeln = it_po-ebeln
            and ebelp = it_po-ebelp.

    sort it_blno  by ebeln ebelp zfblno descending.
  endif.

endform.                    " get_bolno
