*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 11/24/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-9
*& Development Request No : UD1K904434
*& Addl documentation     :
*& Description  : Cash flow report(FM) - Actuals
*&
*& Modification Log
*& Date      Developer      Request ID      Description
*& 06/16/06  Manju          UD1K921082      Program bug Fixing
*& 07/14/06  Manju          UD1K921357      Program bug Fixing -
*&                                          Was not returning end of
*&                                          month sometimes properly
*&--------------------------------------------------------------------
report  zrfit09  message-id zmfi
        no standard page heading line-size 118 .
*                                 line-count 87.

***********************************************************************
*     DATA DECLARATION
***********************************************************************
tables: ztfi_fmal, ztfi_cmal, ztfi_fmap,
        bsis, bsas, skb1, t035t, t036t.

data : begin of ifmal occurs 0.
        include structure ztfi_fmal.
data : balgu(1) type c,
       gubun(2) type c,                                     "category 1
       gubun2(10) type c,                                   "category 2
       gubun3(10) type c,                                   "category 3
       textv(10)  type c,  "summarization term
       end of ifmal.
data : ifmap like ztfi_fmap occurs 0 with header line,
       icmap like ztfi_cmap occurs 0 with header line.

data : begin of it_fmal occurs 0, "drill-down list
       balgu(1) type c,
       gubun(2) type c,                                     "category 1
       gubun2(10) type c,                                   "category 2
       gubun3(10) type c,                                   "category 3
       acctt    like ztfi_fmap-titem,
       amount   like glt0-hslvt,  "current amount
       prvamt   like glt0-hslvt,  "previous amountsumamt
       dispw    like ztfi_cmal-dispw,
       acctxt(30),                     "to drill-down
       textv    like ztfi_fmap-textv,
       end of it_fmal.
data : begin of it_fm_list occurs 0, "summary list
       balgu(1) type c,
       gubun(2) type c,                                     "category 1
       gubun2(10) type c,                                   "category 2
       gubun3(10) type c,                                   "category 3
       acctxt(30),
       amount   like glt0-hslvt,  "amount
       sumamt   like glt0-hslvt,  "sum
       dispw    like ztfi_cmal-dispw,
*      acctt    LIKE ztfi_fmap-titem,  "to drill-down
       end of it_fm_list.
data : iglt0  like glt0  occurs 0 with header line.
data : icmal like ztfi_cmal occurs 0 with header line.
data : it_cmal like it_fmal occurs 0 with header line,
       it_cmal02 like it_fmal occurs 0 with header line. "balance
data : begin of isum01,    "account transfer sum.
       amt01 like glt0-hslvt,
       amt02 like glt0-hslvt,
       end of isum01.
data : isum02 like isum01. "bank reconciliation sum.
data : isum03 like isum01. "open transaction sum.
data : isum04 like isum01. "bank ending(to compute funture balance)
data : isum05 like isum01. "open balance(to compute funture balance)
data : iholiday type standard table of iscal_day with header line.

data : begin of isum,  "each balance value..
       asum1 like glt0-hslvt,
       bsum1 like glt0-hslvt,
       asum2 like glt0-hslvt,
       bsum2 like glt0-hslvt,
       asum3 like glt0-hslvt,
       bsum3 like glt0-hslvt,
       end of isum.
data : begin of tab1 occurs 0,
       acctt like it_fmal-acctt,
       datum like sy-datum,
       amount like it_fmal-amount,
       end of tab1.

*-------drill-down data
data: begin of it_ddwn occurs 0,
        budat   like sy-datum,          "(posting)date
        dispw   like ztfi_cmal-dispw,   "currency
        wrshb   like ztfi_cmal-wrshb,   "amt
        dmshb   like ztfi_cmal-dmshb,   "local amt
        saknr   like ztfi_cmal-saknr,   "bank acct
        grupp   like ztfi_cmal-grupp,   "group
        grptt(30),                      "group desc.
        koart   like ztfi_cmal-koart,   "account type
        kacct   like ztfi_cmal-hkont,   "vnd/cust/account
        fipos   like ztfi_fmal-fipos,   "cmltm
        rftext(25),                     "cmltm desc.
        fincode like ztfi_fmal-fincode, "fund
        fistl   like ztfi_fmal-fistl,   "fctr
        sbewart like ztfi_cmal-sbewart, "flow type
        belnr   like ztfi_cmal-belnr,   "document
        finacct(1)  type c,             "fund acct
        findesc(40) type c,             "fund desc.
        aufnr   like ztfi_fmal-aufnr,   "order
        bukrs   like ztfi_cmal-bukrs,
        gjahr   like ztfi_cmal-gjahr,
        knbelnr like ztfi_cmal-belnr,   "invoice doc.
        kngjahr like ztfi_cmal-gjahr,   "invoice year
      end of it_ddwn.

*variable..
data : sv_spras like t001-spras,
       sv_ktopl like t001-ktopl,
       sv_waers like t001-waers,
       sv_trunctxt(15),
       sv_fr_datum like ztfi_cmal-datum,                    "
       sv_to_datum like ztfi_cmal-datum,
       sv_gubun(20) type c,
       sv_gubun2(20) type c,
       sv_textl   like t035t-textl,
       sv_rdacct(10),
       sv_hslvt     like glt0-hslvt,
       sv_hslvt1    like glt0-hslvt,
       sv_hslvt2    like glt0-hslvt,
       sv_hslvt3    like glt0-hslvt,
       sv_fmal_sum1 like glt0-hslvt,
       sv_fmal_sum2 like glt0-hslvt,
       sv_glt0_amt1 like glt0-hslvt,
       sv_glt0_amt2 like glt0-hslvt,
       dispw_lin    type i,
       l_bsis_dmbtr like bsis-dmbtr,
       l_bsas_dmbtr like bsas-dmbtr,
       l_bsis_wrbtr like bsis-wrbtr,
       l_bsas_wrbtr like bsas-wrbtr,
       sv_holday type i,
       sv_wrkday type i.
data : l_mon type i,
       l_mon_pv type i,
       l_idx(2) type n,
       l_amt like glt0-hslvt,
       l_date like sy-datum.

ranges: r_bank   for skb1-saknr occurs 0,          "bank account
        r_cler   for skb1-saknr occurs 0,
        r_mstr   for skb1-saknr occurs 0,
        r_acct   for skb1-saknr occurs 0,
        r_sdatb  for sy-datum,
        pv_datb  for sy-datum.

constants : h_gubun(20)   value  '                    ',
            h_gubun2(20)  value  '                    ',
*           h_group(10)   VALUE  'Account',
            h_textl(30)   value  'Account',
            h_head(118)   value   'Cash Flow Report(actual)',
            h_headline(118) value '================================'.
data:       h_dmshb(21)   type c,
            h_dmshb1(21)  type c.

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.


*.. field name : available cash bal.
constants : c_cate0(4) value 'A',  "Available cash bal.
            c_cate1(4) value 'A100', "Beg. Bal.
            c_catea(4) value 'A110', "surplus/deficit
            c_cate2(4) value 'A200', "open transaction
            c_cate3(4) value 'A300', "Bank reconciliation
            c_cate4(4) value 'A400', "End. book bal.
            c_cate5(4) value 'A500', "bank ending(g/l)
            c_cate6(4) value 'A600', "open balance
            c_cate7(4) value 'A700', "future balance
            c_lvlb1(2) value 'B1',
            c_lvlb9(2) value 'B9',
            c_wgrp(2)  value 'W%'. "Wx group
constants : c_gubun_exp(2)   value '2',   "expenditure
            c_gubun2_exp(10) value '215', "others
            c_gubun3_exp(10) value '999', "non-account
            c_gubun_rev(2)   value '1',  "revenue
            c_gubun2_rev(10) value '105', "others
            c_gubun3_rev(10) value '999', "non-account
            c_ci6011(06) value '6011++'.


* by ig.moon 2/11/2008 {
data : begin of it_plan occurs 0, "SUMMARY
       sumflg(1) type c,
       fstflg(2) type c,                                    "category 1
       scdflg(10) type c,                                   "category 2
       grupp    like fdes-grupp,
       sign(1),
       amount   like glt0-hslvt, "amount
       sumamt   like glt0-hslvt, "sum
       dispw    like ztfi_cmal-dispw,
       end of it_plan.
data : it_cmal_o like ztfi_cmal occurs 0 with header line.
data : iztfi_cmap like ztfi_cmap occurs 0 with header line.
* }

* field symbol
field-symbols : <fs_hsl> type any.
data fs_hsl(15).
***********************************************************************
* SELECTION-SCREEN
***********************************************************************
selection-screen begin of block b1 with frame title text-001.
select-options : s_bukrs for ztfi_cmal-bukrs obligatory default 'H201'
                         no-extension no intervals,
                 s_datum for ztfi_cmal-datum  no-extension
                         obligatory default sy-datum.
selection-screen  begin of line.
selection-screen comment 1(31) text-010.
parameter : p_lcurr as checkbox default 'X'. "local currency
selection-screen  end of line.
select-options : s_saknr for ztfi_cmal-saknr
                         no-extension no intervals.
selection-screen end of block b1.

selection-screen begin of block b4 with frame title text-006.
select-options : s_compa for ztfi_cmal-datum  no-extension
                         obligatory default sy-datum.
selection-screen end of block b4.

selection-screen begin of block b2 with frame title text-002.
select-options : s_dispw for ztfi_cmal-dispw.
selection-screen end of block b2.

selection-screen begin of block b3 with frame title text-003.
selection-screen begin of line.
selection-screen comment (15) text-004.
selection-screen position 33.
*--scale
parameters: p_trunc like rfpdo1-ts70skal  default '0'.
selection-screen comment 35(1) text-005.
*--decimals
parameters: p_decl like rfpdo1-ts70skal  default '0'.
selection-screen end of line.
selection-screen  begin of line.
selection-screen comment 1(31) text-009.
parameter : p_avrge as checkbox. "daily average
selection-screen  end of line.
selection-screen end of block b3.
***********************************************************************
* TOP-OF-PAGE
***********************************************************************
top-of-page.
*  SET   MARGIN 5 5.
  write :/ h_head centered.
  write :/ h_headline centered.
  write :/.
*-----Start wskim
  if r_acct[] is initial.
    write : 'Cash Account : all'.
  else.
    read table r_acct index 1.
    write : 'Cash Account : ', r_acct-low.
  endif.
  write : 101 'Date : ', sy-datum.
*-----END
  if s_dispw-low is initial.
    write :/  'Currency : ', sv_waers, sv_trunctxt.
  else.
    write :/  'Currency : ', s_dispw-low, sv_trunctxt.
  endif.
  write : 101 'Page : ', sy-pagno.
  if p_avrge eq 'X'.
    write :/  'Working day : ', sv_wrkday.
  endif.
*-----Start wskim
*  IF r_acct[] IS INITIAL.
*    WRITE :/ 'Cash Account : all'.
*  ELSE.
*    READ TABLE r_acct INDEX 1.
*    WRITE :/ 'Cash Account : ', r_acct-low.
*  ENDIF.
*-----END
  uline.
  format color col_heading.

  write: s_datum-low  mm/dd/yy to h_dmshb,
         ' - '                 to h_dmshb+8(3),
         s_datum-high mm/dd/yy to h_dmshb+11.
  write: s_compa-low  mm/dd/yy to h_dmshb1,
         ' - '                 to h_dmshb1+8(3),
         s_compa-high mm/dd/yy to h_dmshb1+11.

  write :/ sy-vline no-gap, h_gubun  no-gap,
           sy-vline no-gap, h_gubun2 no-gap,
*          sy-vline NO-GAP, h_group  NO-GAP,
           sy-vline no-gap, h_textl  no-gap,
           sy-vline no-gap, h_dmshb  no-gap,
           sy-vline no-gap, h_dmshb1 no-gap,
           sy-vline no-gap.
  format color off.
  uline.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/11/24, by WSKIM
*---Start
at selection-screen on value-request for s_saknr-low.
  perform search_help_hkont.
*---End
***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
at selection-screen.
  select spras ktopl waers into (sv_spras, sv_ktopl, sv_waers)
         from t001
         where bukrs in s_bukrs.
    exit.
  endselect.

*  IF NOT s_datum-high IS INITIAL AND
*         s_datum-low+0(6) <> s_datum-high+0(6).
*    MESSAGE e004.
* Scale
  concatenate '(' p_trunc '/' p_decl ')' into sv_trunctxt.

  if p_lcurr eq space.
    describe table s_dispw lines dispw_lin.
    if dispw_lin > 1.
      message e005.
    elseif dispw_lin = 1.
      if not s_dispw-low  is initial and
         not s_dispw-high is initial.
        message e005.
      endif.
      sv_waers = s_dispw-low.
    endif.
  endif.

* working day (FIXME)
  if p_avrge eq 'X'.
    sv_wrkday = ( s_datum-high - s_datum-low ) + 1.
    call function 'HOLIDAY_GET'
         exporting
              factory_calendar = 'U1'
              date_from        = s_datum-low
              date_to          = s_datum-high
         tables
              holidays         = iholiday.
    describe table iholiday lines sv_holday.
    sv_wrkday = sv_wrkday - sv_holday.
  endif.
**Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/11/24, by WSKIM
*---Start
  perform check_hkont.
*---End
***********************************************************************
* START-OF-SELECTION
***********************************************************************
start-of-selection.
  if s_datum-high is initial.
    loop at s_datum.
      s_datum-high = s_datum-low.  "add by JIPARK
      modify s_datum.
    endloop.
  endif.
  if s_compa-high is initial.
    loop at s_compa.
      s_compa-high = s_compa-low.
      modify s_compa.
    endloop.
  endif.

  perform get_account. "get cash account

* FM actuals
  perform get_ztfi_fmal.
  perform make_it_fmal. "fm cash flow category

* CM actuals

* by ig.moon 2/11/2008 {
*  perform get_ztfi_cmal.
*  perform make_it_cmal. "balance category
*  perform get_glt0.
*  perform get_bank_clearing.

  perform get_cm_new.

* }

***********************************************************************
* END-OF-SELECTION
***********************************************************************
end-of-selection.
  perform summary_fm.
  perform display_fm.

* by ig.moon 2/11/2008 {
*  perform display_cm.
  perform display_cm_new.
* }
***********************************************************************
* AT LINE-SELECTION
***********************************************************************
at line-selection.
  perform call_drill_down.

*&---------------------------------------------------------------------*
*&      Form  get_ztfi_fmal
*&---------------------------------------------------------------------*
form get_ztfi_fmal.
  clear : ifmal[], ifmal, ifmap[], ifmap.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/11/22, by WSKIM
*---Start
*delete start
*  SELECT * FROM ztfi_fmal INTO CORRESPONDING FIELDS OF TABLE ifmal
*           WHERE bukrs IN s_bukrs
*             AND dispw IN s_dispw
*             AND ( datum IN s_datum OR datum IN s_compa )
*             and HKONT in R_BANK.
**            AND datum BETWEEN sv_fr_datum and sv_to_datum.
*delete end
*insert start
  select *  from ztfi_fmal into corresponding fields of table ifmal
           where bukrs in s_bukrs
             and dispw in s_dispw
             and datum in s_datum
             and saknr in r_bank.

  select * from ztfi_fmal appending corresponding fields of table ifmal
             where bukrs in s_bukrs
               and dispw in s_dispw
               and datum in s_compa
               and saknr in r_bank.

  sort ifmal by datum stunr belnr.
  delete adjacent duplicates from ifmal comparing datum stunr belnr .
*insert end
*---End
* fm cash flow report layout
  select * from ztfi_fmap client specified
           into corresponding fields of table ifmap
           where mandt eq sy-mandt.

endform.                    " get_ztfi_fmal
*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_FMAL
*&---------------------------------------------------------------------*
form make_it_fmal.
  data: $subrc.

  clear :  ifmal, it_fmal[], it_fmal, sv_fmal_sum1, sv_fmal_sum2.
  loop at ifmal.

    check ifmal-dmshb <> 0.

    if ifmal-fipos  ne space. "commitment item
      perform gubun_rtn using ifmal-fipos  ifmal-dmshb ifmal-payflg
                        changing it_fmal-balgu  it_fmal-gubun
                                 it_fmal-gubun2 it_fmal-gubun3
                                 it_fmal-acctt  it_fmal-textv.
*    ELSEIF ifmal-fincode NE space.  "fund
*      PERFORM gubun_rtn USING ifmal-fincode ifmal-dmshb
*                        CHANGING it_fmal-balgu  it_fmal-gubun
*                                 it_fmal-gubun2 it_fmal-gubun3
*                                 it_fmal-acctt  it_fmal-textv.
    endif.

*    CASE it_fmal-gubun.
**---- investment
*      WHEN '3'.
*        PERFORM move_gubun_3.
**---- asset sales
*      WHEN '4'.
*        PERFORM move_gubun_4.
**---- scheduled repayment
*      WHEN '6'.
*        PERFORM move_gubun_6.
**---- financial instrument
*      WHEN '7'.
*        PERFORM move_gubun_7.
**---- others
*      WHEN others. " '1' OR '2' OR '5' OR '8'.
*        PERFORM move_others USING ifmal-datum ifmal-dmshb ifmal-wrshb
*                            CHANGING it_fmal-amount it_fmal-sumamt.
*    ENDCASE.

* by Andy
    perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                        changing it_fmal-amount it_fmal-prvamt.

    sv_fmal_sum1 = sv_fmal_sum1 + it_fmal-amount.
    sv_fmal_sum2 = sv_fmal_sum2 + it_fmal-prvamt.

    ifmal-balgu  = it_fmal-balgu.
    ifmal-gubun  = it_fmal-gubun.
    ifmal-gubun2 = it_fmal-gubun2.
    ifmal-gubun3 = it_fmal-gubun3.
*==> confirm hardcording?
*   IF ifmal-rfipex CP c_ci6011.                            "'6011++'.
*     ifmal-textv = '1'.
*   ELSE.
    ifmal-textv  = it_fmal-textv.
*   ENDIF.
    modify ifmal.

*   if it_fmal-acctt eq space. break-point. endif.

    collect it_fmal. clear : it_fmal, ifmal.
  endloop.

*-----> list include initial amount..2004/03/25
* DELETE it_fmal WHERE amount = 0 AND sumamt = 0.
  perform append_initial_ci.
endform.                    " make_it_fmal
*&---------------------------------------------------------------------*
*&      Form  GUBUN_RTN
*&---------------------------------------------------------------------*
form gubun_rtn using p_code p_dmshb p_payflag
               changing i_balgu  i_gubun i_gubun2
                        i_gubun3 i_acctt i_textv.
  data : l_sign,
         l_code(24) type c,
         l_exist(1).

*  IF p_dmshb > 0.
*    l_sign = '+'.
*  ELSE.
*    l_sign = '-'.
*  ENDIF.

  l_sign = p_payflag.
  clear l_exist.
  if p_code+0(3) = '+'.
    concatenate p_code+0(2) '*' into l_code.
  else.
    concatenate p_code+0(3) '*' into l_code.
    loop at ifmap where titem cp l_code
                    and ( sign eq space or sign eq l_sign ) .
      l_exist = 'X'.
    endloop.

    if l_exist <> 'X'.
      concatenate p_code+0(2) '*' into l_code.
    endif.
  endif.

  loop at ifmap where titem cp l_code  .
    perform setting_form using p_code l_sign
                         changing i_balgu  i_gubun i_gubun2
                                  i_gubun3 i_acctt i_textv.
  endloop.

endform.                    " GUBUN_RTN
*&---------------------------------------------------------------------*
*&      Form  setting_form
*&---------------------------------------------------------------------*
form setting_form using    p_code  p_sign
                  changing p_balgu p_gubun  p_gubun2
                           p_gubun3 p_acctt p_textv.
  if p_code cp ifmap-titem.
    if ifmap-sign eq space.
      p_balgu  = ifmap-tmflg.
      p_gubun  = ifmap-flevl(1).
      p_gubun2 = ifmap-flevl.
      p_gubun3 = ifmap-slevl.
      p_acctt  = ifmap-titem.
      p_textv  = ifmap-textv.
      exit.    "only one
* if sign exit...
    else.
      if ifmap-sign eq p_sign.
        p_balgu  = ifmap-tmflg.
        p_gubun  = ifmap-flevl(1).
        p_gubun2 = ifmap-flevl.
        p_gubun3 = ifmap-slevl.
        p_acctt  = ifmap-titem.
        p_textv  = ifmap-textv.
        exit.    "only one
      endif.
    endif.
  endif.

* revenue (-)amount -> expenditure-others
* expenditure (+)amount -> revenue-others
*-- 2004/03/23 Changed by JIPARK.. revenue & expenditure => same balgu??
  if p_gubun eq '1' and p_sign eq '-'.
*   p_balgu  = c_gubun_rev.  "=>
    p_gubun  = c_gubun_exp.
    p_gubun2 = c_gubun2_exp.
    p_gubun3 = c_gubun3_exp.
    p_acctt  = c_gubun3_exp.
    clear p_textv.
  elseif p_gubun eq '2' and ( p_sign eq '+' ).
*   p_balgu  = c_gubun_rev.
    p_gubun  = c_gubun_rev.
    p_gubun2 = c_gubun2_rev.
    p_gubun3 = c_gubun3_rev.
    p_acctt  = c_gubun3_rev.
    clear p_textv.
  endif.
*  IF p_balgu EQ '1' AND p_sign EQ '-'.
*    p_balgu  = c_gubun_exp.
*    p_gubun  = c_gubun_exp.
*    p_gubun2 = c_gubun2_exp.
*    p_gubun3 = c_gubun3_exp.
*    p_acctt  = c_gubun3_exp.
*    CLEAR p_textv.
*  ELSEIF p_balgu EQ '2' AND p_sign EQ '+'.
*    p_balgu  = c_gubun_rev.
*    p_gubun  = c_gubun_rev.
*    p_gubun2 = c_gubun2_rev.
*    p_gubun3 = c_gubun3_rev.
*    p_acctt  = c_gubun3_rev.
*    CLEAR p_textv.
*  ENDIF.
endform.                    " setting_form
*&---------------------------------------------------------------------*
*&      Form  move_gubun_9
*&---------------------------------------------------------------------*
form move_gubun_9 using p_subrc.
  clear p_subrc.
* ..bank transfer : different master acct.
  if icmal-saknr+4(5) ne icmal-hkont+4(5).
    if icmal-dmshb > 0.
      it_cmal-gubun2 = '90'.  "incoming
      perform move_others using icmal-budat icmal-dmshb icmal-wrshb
                          changing it_cmal-amount it_cmal-prvamt.
    elseif icmal-dmshb < 0.
      it_cmal-gubun2 = '91'.  "outcoming
      perform move_others using icmal-budat icmal-dmshb icmal-wrshb
                          changing it_cmal-amount it_cmal-prvamt.
    endif.

    isum01-amt01 = isum01-amt01 + it_cmal-amount.
    isum01-amt02 = isum01-amt02 + it_cmal-prvamt.

    clear it_cmal-acctt.

* ..bank reconciliation : same master acct.
  else.
    p_subrc = 1.
*    IF it_cmal-acctt EQ 'W0'.
*      it_cmal02-gubun  = c_cate0.   "available cash bal.
*      it_cmal02-gubun2 = c_cate3.   ".. bank reconciliation
*      CLEAR it_cmal-acctt.
**     IF icmal-datum IN s_datum.
*      IF icmal-budat IN s_datum.
*        PERFORM assign_amount USING icmal-dmshb icmal-wrshb
*                              CHANGING it_cmal02-amount.
*      ENDIF.
**     IF icmal-datum IN s_compa.
*      IF icmal-budat IN s_compa.
*        PERFORM assign_amount USING icmal-dmshb icmal-wrshb
*                              CHANGING it_cmal02-sumamt.
*      ENDIF.
*      isum02-amt01 = isum02-amt01 + it_cmal02-amount.
*      isum02-amt02 = isum02-amt02 + it_cmal02-sumamt.
*      COLLECT it_cmal02.  CLEAR it_cmal02.
**     CLEAR: it_cmal-gubun, it_cmal-gubun2.
*      p_subrc = 1.
*    ELSE.
*      CLEAR: it_cmal-balgu,  it_cmal-gubun,
*             it_cmal-gubun2, it_cmal-gubun3, it_cmal-acctt.
*    ENDIF.
  endif.
endform.                    " move_gubun_9
*&---------------------------------------------------------------------*
*&      Form  assign_flag
*&---------------------------------------------------------------------*
form assign_flag using i_grupp i_dmshb
                 changing p_sumflg p_fstflg p_scdflg.
  data : l_sign.

  if i_dmshb > 0.
    l_sign = '+'.
  else.
    l_sign = '-'.
  endif.
  loop at icmap.
    if i_grupp between icmap-frgrp and icmap-togrp.
      if icmap-sign eq space.
        p_sumflg = icmap-tmflg.
        p_fstflg = icmap-grptm(1).
        p_scdflg = icmap-grptm.
        exit.    "only one
      else.
        if icmap-sign eq l_sign.
          p_sumflg = icmap-tmflg.
          p_fstflg = icmap-grptm(1).
          p_scdflg = icmap-grptm.
          exit.    "only one
        endif.
      endif.
    endif.
  endloop.
endform.                    " assign_flag
*&---------------------------------------------------------------------*
*&      Form  move_gubun_3
*&---------------------------------------------------------------------*
form move_gubun_3.
  if it_fmal-gubun2 eq '30'.
    if ifmal-dmshb < 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  elseif it_fmal-gubun2 eq '31'.
    if ifmal-dmshb > 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  endif.
endform.                    " move_gubun_3
*&---------------------------------------------------------------------*
*&      Form  move_gubun_4
*&---------------------------------------------------------------------*
form move_gubun_4.
  if it_fmal-gubun2 eq '40'.
    if ifmal-dmshb < 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  elseif it_fmal-gubun2 eq '41'.
    if ifmal-dmshb > 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  endif.
endform.                    " move_gubun_4
*&---------------------------------------------------------------------*
*&      Form  move_gubun_6
*&---------------------------------------------------------------------*
form move_gubun_6.
  if it_fmal-gubun2 eq '60'.
    if ifmal-dmshb < 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  elseif it_fmal-gubun2 eq '61'.
    if ifmal-dmshb > 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  endif.
endform.                    " move_gubun_6
*&---------------------------------------------------------------------*
*&      Form  move_gubun_7
*&---------------------------------------------------------------------*
form move_gubun_7.
  if it_fmal-gubun2 eq '70'.
    if ifmal-dmshb > 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  elseif it_fmal-gubun2 eq '71'.
    if ifmal-dmshb < 0.
      perform move_others using ifmal-datum ifmal-dmshb ifmal-wrshb
                          changing it_fmal-amount it_fmal-prvamt.
    endif.
  endif.
endform.                    " move_gubun_7
*&---------------------------------------------------------------------*
*&      Form  move_others
*&---------------------------------------------------------------------*
form move_others using p_datum p_dmshb p_wrshb
                 changing i_amount i_sumamt.
  if p_datum in  s_datum.
    perform assign_amount using p_dmshb p_wrshb
                          changing i_amount.
  endif.
  if p_datum in s_compa.
    perform assign_amount using p_dmshb p_wrshb
                          changing i_sumamt.
  endif.
endform.                    " move_others.
*&---------------------------------------------------------------------*
*&      Form  assign_amount
*&---------------------------------------------------------------------*
form assign_amount using    p_dmshb
                            p_wrshb
                   changing p_hslvt.
  clear p_hslvt.
  if p_lcurr eq 'X'.
    p_hslvt = p_dmshb.
  else.
    p_hslvt = p_wrshb.
  endif.
endform.                    " assign_amount
*&---------------------------------------------------------------------*
*&      Form  get_ztfi_cmal
*&---------------------------------------------------------------------*
*  account transfer & available cash bal.<= select ztfi_cmal..
*&---------------------------------------------------------------------*
form get_ztfi_cmal.
  clear : icmal[], icmal.
  select * from ztfi_cmal
           into corresponding fields of table icmal
           where bukrs in s_bukrs
           and   dispw in s_dispw
           and   saknr in r_bank
           and ( datum in s_datum or datum in s_compa ).

endform.                    " get_ztfi_cmal
*&--------------------------------------------------------------------
*&      Form  get_glt0
*&---------------------------------------------------------------------
*  input: 2003/11/10~2003/11/29
*    1. beg.bal: {C/F + SUM(2003/1 ~ 2003/9)} + (2003/11/01~2003/11/09)
*               => r_bdate: 2003/11/01~2003/11/09)
*----------------------------------------------------------------------
form get_glt0.
*  DATA : l_mon TYPE i,
*         l_mon_pv TYPE i,
*         l_idx(2) TYPE n,
*         l_amt LIKE glt0-hslvt,
*         l_date LIKE sy-datum.
  ranges: r_bdate for sy-datum,
          r_bdate_pv for sy-datum.

  data : l_bsis_bsas1 like bsis-dmbtr,
         l_bsis_bsas2 like bsis-dmbtr.

* <beginning book balance>
  clear: iglt0[], iglt0, it_cmal02.
  clear : l_bsis_bsas1,l_bsis_bsas2.
  clear: l_bsis_dmbtr, l_bsis_wrbtr, l_bsas_wrbtr, l_bsas_dmbtr,
         sv_glt0_amt1, sv_glt0_amt2.

  it_cmal02-gubun  = c_cate0.   "available cash bal.
  it_cmal02-gubun2 = c_cate1.  "begin bal.
* get glt0 : C/F + SUM{JAN. ~ (current month - 1)}
  read table s_datum index 1.

  call function 'RP_CALC_DATE_IN_INTERVAL'
       exporting
            date      = s_datum-low
            days      = 0
            months    = 1
            signum    = '-'
            years     = 0
       importing
            calc_date = l_date.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/11/24, by WSKIM
*---Start
*  l_mon = l_date+4(2).
* PERFORM get_data_glt0 USING l_date
  perform get_data_glt0 using    l_date
                        changing it_cmal02-amount.

* bank ending(g/l) "Current
  perform get_ending_gl tables   s_datum
                        changing l_bsis_bsas1.


  clear l_date.
  read table s_compa index 1.
  call function 'RP_CALC_DATE_IN_INTERVAL'
       exporting
            date      = s_compa-low
            days      = 0
            months    = 1
            signum    = '-'
            years     = 0
       importing
            calc_date = l_date.

  perform get_data_glt0 using    l_date
                        changing it_cmal02-prvamt.

* bank ending(g/l) "Previous
  perform get_ending_gl tables   s_compa
                        changing l_bsis_bsas2.

* bank ending(g/l) = bsis + bsas + beginging
  sv_glt0_amt1 = it_cmal02-amount + l_bsis_bsas1.  "bank ending(g/l)
  sv_glt0_amt2 = it_cmal02-prvamt + l_bsis_bsas2.


* glt0 -> previous data
*  l_mon_pv = l_mon - 1.
*  LOOP AT iglt0 WHERE racct IN r_mstr.
*    CLEAR l_idx.
*    DO.
*      l_idx = l_idx + 1.
*      IF l_idx > l_mon_pv.
*        EXIT.
*      ELSE.
*        IF p_lcurr EQ 'X'.
*          CONCATENATE 'IGLT0-HSL' l_idx INTO fs_hsl.
*        ELSE.
*          CONCATENATE 'IGLT0-TSL' l_idx INTO fs_hsl.
*        ENDIF.
*        ASSIGN (fs_hsl) TO <fs_hsl>.
*        it_cmal02-sumamt = it_cmal02-sumamt + <fs_hsl>.
*
*      ENDIF.
*    ENDDO.
*    PERFORM assign_amount USING iglt0-hslvt iglt0-tslvt
*                          CHANGING l_amt. "C/F
*    it_cmal02-sumamt = it_cmal02-sumamt + l_amt.
*  ENDLOOP.
*  sv_glt0_amt2 = it_cmal02-sumamt.
*----End
* bsis: basic amount
  if s_datum-low+6(2) ne '01'.
    r_bdate-sign = 'I'.
    r_bdate-option = 'BT'.
    r_bdate-high = s_datum-low - 1.
    concatenate r_bdate-high(6) '01' into r_bdate-low.
    append r_bdate.


    clear : l_bsis_dmbtr,  l_bsis_wrbtr.
    select dmbtr wrbtr shkzg
           into (bsis-dmbtr, bsis-wrbtr, bsis-shkzg)
           from bsis
           where bukrs in s_bukrs
           and   hkont in r_mstr
           and   gjahr eq s_datum-low+0(4)
           and   budat in r_bdate
*           AND   budat IN s_datum
           and   waers in s_dispw.
      if bsis-shkzg eq 'H'.
        bsis-wrbtr = bsis-wrbtr * -1.
        bsis-dmbtr = bsis-dmbtr * -1.
      endif.
      l_bsis_dmbtr = l_bsis_dmbtr + bsis-dmbtr.
      l_bsis_wrbtr = l_bsis_wrbtr + bsis-wrbtr.
    endselect.


    perform assign_amount using l_bsis_dmbtr l_bsis_wrbtr
                          changing l_amt.
    it_cmal02-amount = it_cmal02-amount + l_amt.  "begin bal.
  endif.
  if s_compa-low+6(2) ne '01'.
    r_bdate_pv-sign = 'I'.
    r_bdate_pv-option = 'BT'.
    r_bdate_pv-high = s_compa-low - 1.
    concatenate r_bdate_pv-high(6) '01' into r_bdate_pv-low.
    append r_bdate_pv.

    clear : l_bsis_dmbtr,  l_bsis_wrbtr.
    select dmbtr wrbtr shkzg
           into (bsis-dmbtr, bsis-wrbtr, bsis-shkzg)
           from bsis
           where bukrs in s_bukrs
           and   hkont in r_mstr
           and   gjahr eq s_datum-low+0(4)
           and   budat in r_bdate_pv
*            AND   budat IN s_compa
           and   waers in s_dispw.
      if bsis-shkzg eq 'H'.
        bsis-wrbtr = bsis-wrbtr * -1.
        bsis-dmbtr = bsis-dmbtr * -1.
      endif.
      l_bsis_dmbtr = l_bsis_dmbtr + bsis-dmbtr.
      l_bsis_wrbtr = l_bsis_wrbtr + bsis-wrbtr.
    endselect.

    perform assign_amount using l_bsis_dmbtr l_bsis_wrbtr
                          changing l_amt.
    it_cmal02-prvamt = it_cmal02-prvamt + l_amt.
  endif.
  collect it_cmal02.


* ..bank ending(fm total+transfer+begin bal.+open trans+bank recon)
  it_cmal02-gubun   = c_cate0.
  it_cmal02-gubun2  = c_cate4.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/11/22, by WSKIM
*---Start
*  it_cmal02-amount  = sv_fmal_sum1 + isum01-amt01 + it_cmal02-amount
*                    + ( isum03-amt01 * -1 ).
*   .bank ending(fm total+begin bal.+open trans+bank recon)

  it_cmal02-amount  = sv_fmal_sum1 + it_cmal02-amount
                  + isum03-amt01 + isum02-amt01.
*it_cmal02-sumamt  = sv_fmal_sum2 + isum01-amt02 + it_cmal02-sumamt
*                   + ( isum03-amt02 * -1 ).
  it_cmal02-prvamt  = sv_fmal_sum2  + it_cmal02-prvamt
                  + isum03-amt02 + isum02-amt02.
*---End
  isum04-amt01 = it_cmal02-amount.  "to compute future balance.
  isum04-amt02 = it_cmal02-prvamt.
  collect it_cmal02.  "CLEAR it_cmal02.

*--2004/04/30 surplus/deficit = surplus/deficit after financial instru..
  it_cmal02-gubun   = c_cate0.
  it_cmal02-gubun2  = c_catea.
  it_cmal02-amount  = sv_fmal_sum1.
  it_cmal02-prvamt  = sv_fmal_sum2.
  collect it_cmal02.  clear it_cmal02.
endform.                                                    " get_glt0
*&---------------------------------------------------------------------
*&      Form  get_bank_clearing
*&---------------------------------------------------------------------
*  input: 2003/11/10~2003/11/29
*    2. Bnk.clg: {C/F + SUM(2003/1 ~ 2003/9)} + (2003/11/01~2003/11/29)
*                ==> sv_fr_datum: 2003/11/01
*                    sv_to_datum: 2003/11/29
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
form get_bank_clearing.
  data : l_idx(2) type n,
         l_mon type i,
         l_mon_pv type i,
         l_amt like glt0-hslvt.

  clear: l_bsis_dmbtr, l_bsas_dmbtr, isum05.

  read table s_datum index 1.
  l_mon = s_datum-low+4(2) - 1.
  read table s_compa index 1.
  l_mon_pv = s_compa-low+4(2) - 1.

* <open balance> => glt0 : c/f + sum(1~9)
  loop at iglt0.
    if not r_cler[] is initial and iglt0-racct in r_cler[].
      perform set_clg_level using iglt0-racct.
      it_cmal02-gubun  = c_cate0.  "available cash bal.
      it_cmal02-gubun2 = c_cate6.  "open balance

* -----> current open balance
      if iglt0-ryear eq s_datum-low(4).
        clear l_idx.
        do.
          l_idx = l_idx + 1.
          if l_idx > l_mon.
            exit.
          else.
            if p_lcurr eq 'X'.
              concatenate 'IGLT0-HSL' l_idx into fs_hsl.
            else.
              concatenate 'IGLT0-TSL' l_idx into fs_hsl.
            endif.
            assign (fs_hsl) to <fs_hsl>.
            it_cmal02-amount  = it_cmal02-amount  + <fs_hsl>.
          endif.
        enddo.
        perform assign_amount using iglt0-hslvt iglt0-tslvt
                              changing l_amt.
        it_cmal02-amount = it_cmal02-amount + l_amt.
      endif.
* -----> previous month open balance
      if iglt0-ryear eq s_compa-low(4).
        clear l_idx.
*-----start wskim
        if l_mon_pv <> 0.
*-----end
          do.
            l_idx = l_idx + 1.
            if l_idx > l_mon_pv.
              exit.
            else.
              if p_lcurr eq 'X'.
                concatenate 'IGLT0-HSL' l_idx into fs_hsl.
              else.
                concatenate 'IGLT0-TSL' l_idx into fs_hsl.
              endif.
              assign (fs_hsl) to <fs_hsl>.
              it_cmal02-prvamt = it_cmal02-prvamt + <fs_hsl>.
            endif.
          enddo.
          perform assign_amount using iglt0-hslvt iglt0-tslvt
                                changing l_amt.
*-----start
        else.
          perform assign_amount using iglt0-hslvt iglt0-tslvt
                                changing l_amt.
        endif.
*----end
        it_cmal02-prvamt = it_cmal02-prvamt + l_amt.

      endif.

      isum05-amt01 = it_cmal02-amount + isum05-amt01. "open balance.
      isum05-amt02 = it_cmal02-prvamt + isum05-amt02.
      collect it_cmal02.  clear it_cmal02.
    endif.
  endloop.

*-
  perform coversion_date.

*   ..bsis: ..open item
  select hkont budat dmbtr wrbtr shkzg
         into (bsis-hkont, bsis-budat,
               bsis-dmbtr, bsis-wrbtr, bsis-shkzg)
         from bsis
         where bukrs in s_bukrs
         and   hkont in r_cler      "r_bank
         and ( gjahr eq s_datum-low or
               gjahr eq s_compa-low )
         and  ( budat in r_sdatb or budat in pv_datb ) .

    if bsis-shkzg eq 'H'.
      bsis-wrbtr = bsis-wrbtr * -1.
      bsis-dmbtr = bsis-dmbtr * -1.
    endif.

    if not r_cler[] is initial and bsis-hkont in r_cler.
      perform set_clg_level using bsis-hkont.
      it_cmal02-gubun  = c_cate0.   "available cash bal.
      it_cmal02-gubun2 = c_cate6.   "bank clearing
      if bsis-budat in r_sdatb.
        perform assign_amount using bsis-dmbtr bsis-wrbtr
                              changing l_amt.
        it_cmal02-amount = it_cmal02-amount + l_amt.
      endif.
* -----> previous month open bal.
      if bsis-budat in pv_datb.
        perform assign_amount using bsis-dmbtr bsis-wrbtr
                              changing l_amt.
        it_cmal02-prvamt = it_cmal02-prvamt + l_amt.
      endif.

      isum05-amt01 = it_cmal02-amount + isum05-amt01. "open balance.
      isum05-amt02 = it_cmal02-prvamt + isum05-amt02.
      collect it_cmal02.  clear it_cmal02.

    elseif bsis-hkont in r_mstr.
      if bsis-budat in r_sdatb.
        perform assign_amount using bsis-dmbtr bsis-wrbtr
                              changing l_amt.
        sv_glt0_amt1 = l_amt + sv_glt0_amt1.
      endif.
      if bsis-budat in pv_datb.
        perform assign_amount using bsis-dmbtr bsis-wrbtr
                              changing l_amt.
        sv_glt0_amt2 = l_amt + sv_glt0_amt2.
      endif.
    endif.
  endselect.

*   ..bsas: ..clearing item
  select hkont budat dmbtr wrbtr shkzg
         into (bsas-hkont, bsas-budat,
               bsas-dmbtr, bsas-wrbtr, bsas-shkzg)
         from bsas
         where bukrs in s_bukrs
         and   hkont in r_cler      "r_bank
         and ( gjahr eq s_datum-low or
               gjahr eq s_compa-low )
         and  ( budat in r_sdatb or budat in pv_datb ) .

    if bsas-shkzg eq 'H'.
      bsas-wrbtr = bsas-wrbtr * -1.
      bsas-dmbtr = bsas-dmbtr * -1.
    endif.

    if not r_cler[] is initial and bsas-hkont in r_cler.
      perform set_clg_level using bsas-hkont.
      it_cmal02-gubun = c_cate0.   "available cash bal.
      it_cmal02-gubun2 = c_cate6.  "bank clearing
      if bsas-budat in r_sdatb.
        perform assign_amount using bsas-dmbtr bsas-wrbtr
                              changing l_amt.
        it_cmal02-amount = it_cmal02-amount + l_amt.
      endif.
      if bsas-budat in pv_datb.
        perform assign_amount using bsas-dmbtr bsas-wrbtr
                              changing l_amt.
        it_cmal02-prvamt = it_cmal02-prvamt + l_amt.
      endif.
      isum05-amt01 = it_cmal02-amount + isum05-amt01. "open balance.
      isum05-amt02 = it_cmal02-prvamt + isum05-amt02.
      collect it_cmal02.  clear it_cmal02.
    elseif bsas-hkont in r_mstr.
      if bsas-budat in r_sdatb.
        perform assign_amount using bsas-dmbtr bsas-wrbtr
                              changing l_amt.
        sv_glt0_amt1 = l_amt + sv_glt0_amt1.
      endif.
      if bsas-budat in pv_datb.
        perform assign_amount using bsas-dmbtr bsas-wrbtr
                              changing l_amt.
        sv_glt0_amt2 = l_amt + sv_glt0_amt2.
      endif.
    endif.
  endselect.

* future balance.
  it_cmal02-gubun  = c_cate0.   "available cash bal.
  it_cmal02-gubun2 = c_cate7.   "future balance
  it_cmal02-amount = isum04-amt01 + isum05-amt01.
  it_cmal02-prvamt = isum04-amt02 + isum05-amt02.
  collect it_cmal02.
* ending bank(g/l)
  it_cmal02-gubun  = c_cate0.   "available cash bal.
  it_cmal02-gubun2 = c_cate5.   "end. bank(g/l)
  it_cmal02-amount = sv_glt0_amt1.
  it_cmal02-prvamt = sv_glt0_amt2.
  collect it_cmal02.

* DELETE it_cmal02 WHERE amount = 0 AND sumamt = 0.
  loop at it_cmal02.
    move-corresponding it_cmal02 to it_cmal.
    append it_cmal.  clear it_cmal.
  endloop.

endform.                    " get_bank_clearing
*&---------------------------------------------------------------------
*&      Form  set_clg_level
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
form set_clg_level using p_hkont.
  if p_hkont cp '*1'.
    it_cmal02-acctt = 'B1'.
  elseif p_hkont cp '*2'.
    it_cmal02-acctt = 'B2'.
  elseif p_hkont cp '*3'.
    it_cmal02-acctt = 'B3'.
  elseif p_hkont cp '*4'.
    it_cmal02-acctt = 'B4'.
  elseif p_hkont cp '*5'.
    it_cmal02-acctt = 'B5'.
  endif.
endform.                    " set_clg_level
*&---------------------------------------------------------------------*
*&      Form  make_it_cmal
*&---------------------------------------------------------------------*
form make_it_cmal.
  data: vsubrc.

* by ig.moon {
*  sort icmal by wrshb   .
* }

  clear: isum01, isum02, isum03.
  loop at icmal where dmshb <> 0.
    clear: it_cmal.
    clear: vsubrc.

*---- account transfer : master acct. / bank reconciliation
    if icmal-grupp(1) eq c_wgrp(1).
      move : icmal-grupp to it_cmal-acctt.
      perform assign_flag using icmal-grupp icmal-dmshb
                          changing it_cmal-balgu it_cmal-gubun
                                   it_cmal-gubun2.
      perform move_gubun_9 using vsubrc.
      if vsubrc eq space.   collect it_cmal.  endif.
    endif.

*--- available cash balance....bank reconciliation
*   IF icmal-saknr+9(1) EQ '0'  "AND "master account
    if ( icmal-ebene = 'B0' or icmal-ebene = 'C0'
* by ig.moon 1/7/08 {
         or icmal-ebene = 'TH'
* }
        ) and
         icmal-grupp(1) eq 'W' .
      it_cmal02-gubun  = c_cate0.   "available cash bal.
      it_cmal02-gubun2 = c_cate3.   ".. bank reconciliation
      clear it_cmal-acctt.
      if icmal-budat in s_datum.
        perform assign_amount using icmal-dmshb icmal-wrshb
                              changing it_cmal02-amount.
      endif.

      if icmal-budat in s_compa.
        perform assign_amount using icmal-dmshb icmal-wrshb
                              changing it_cmal02-prvamt.
      endif.
      isum02-amt01 = isum02-amt01 + it_cmal02-amount.
      isum02-amt02 = isum02-amt02 + it_cmal02-prvamt.
      collect it_cmal02.  clear it_cmal02.
*     CLEAR: it_cmal-gubun, it_cmal-gubun2.

*--- available cash balance....open transaction(except bank reconcl.)
*    IF icmal-ebene BETWEEN c_lvlb1 AND c_lvlb9
    elseif icmal-saknr in r_cler
       and ( icmal-grupp(1) ne 'W' ). "level B1~B9
      it_cmal02-gubun  = c_cate0.   "available cash bal.
      it_cmal02-gubun2 = c_cate2.   ".. open transaction
      clear it_cmal-acctt.
*     IF icmal-datum IN  s_datum.
      if icmal-budat in  s_datum.
        perform assign_amount using icmal-dmshb
                                    icmal-wrshb
                              changing it_cmal02-amount.
      endif.
*     IF icmal-datum IN s_compa.
      if icmal-budat in s_compa.
        perform assign_amount using icmal-dmshb
                                    icmal-wrshb
                              changing it_cmal02-prvamt.
      endif.
      it_cmal02-amount = it_cmal02-amount * -1.
      it_cmal02-prvamt = it_cmal02-prvamt * -1.

      isum03-amt01 = isum03-amt01 + it_cmal02-amount.
      isum03-amt02 = isum03-amt02 + it_cmal02-prvamt.

      collect it_cmal02.  clear it_cmal02.
    endif.

  endloop.

* DELETE it_cmal WHERE amount = 0 AND sumamt = 0.
**--> Include initial amount
*  PERFORM append_initial_group.
endform.                    " make_it_cmal
*&---------------------------------------------------------------------
*&      Form  get_account
*&---------------------------------------------------------------------
form get_account.
**  clear: r_bank[],r_bank, r_cler[], r_cler, r_mstr[], r_mstr.
**
**  r_bank-sign = 'I'.     r_bank-option = 'EQ'.
**  r_cler-sign = 'I'.     r_cler-option = 'EQ'.
**  r_mstr-sign = 'I'.     r_mstr-option = 'EQ'.
**
**  r_acct[] = s_saknr[].  "header display
**  loop at s_saknr.
**    s_saknr-option = 'CP'.
**    if s_saknr-low ne space.
**      concatenate s_saknr-low(9) '+' into s_saknr-low.
**    endif.
**    if s_saknr-high ne space.
**      concatenate s_saknr-high(9) '+' into s_saknr-high.
**    endif.
**    modify s_saknr. clear s_saknr.
**  endloop.
**
**  select saknr fdlev into (r_bank-low, skb1-fdlev)
**         from skb1
**         where bukrs in s_bukrs
**           and waers in s_dispw
**           and xgkon eq 'X'
**           and saknr in s_saknr
**           and ( fdlev like 'B%' or fdlev like 'C%' or fdlev = 'TH' ).
**    if skb1-fdlev between 'B1' and 'B9'.  "bank clearing acct.
**      r_cler-low = r_bank-low.
**      append r_cler.
**    elseif skb1-fdlev eq 'B0' or skb1-fdlev eq 'C0'.  "master acct.
**      r_mstr-low = r_bank-low.
**      append r_mstr.
**    endif.
**
**    if s_saknr-low <> space.
**      append r_bank.
**    endif.
**  endselect.
**
*** cm layout
**  select * from ztfi_cmap client specified
**           into corresponding fields of table icmap
**           where mandt eq sy-mandt
**           and   grptm >= 90.


  clear: r_bank[],r_bank,    "cash account
         r_cler[], r_cler,   "open balance account
         r_mstr[], r_mstr,   "master account
         icmap[], icmap.     "cm layout

  r_bank-sign = 'I'.     r_bank-option = 'EQ'.  "bank account
  r_cler-sign = 'I'.     r_cler-option = 'EQ'.  "open balance acct.
  r_mstr-sign = 'I'.     r_mstr-option = 'EQ'.  "master acct.

* master + sub account
  loop at s_saknr.
    s_saknr-option = 'CP'.
    if s_saknr-low ne space.
      concatenate s_saknr-low(9) '+' into s_saknr-low.
    endif.
    if s_saknr-high ne space.
      concatenate s_saknr-high(9) '+' into s_saknr-high.
    endif.
    modify s_saknr. clear s_saknr.
  endloop.

  select saknr fdlev into (r_bank-low, skb1-fdlev)
         from skb1
         where bukrs in s_bukrs
           and waers in s_dispw
           and xgkon eq 'X'
           and saknr in s_saknr
           and ( fdlev like 'B%'
              or fdlev like 'C%'
              or fdlev eq   'TH' ).
    if skb1-fdlev between 'B1' and 'B9'.  "bank clearing acct.
      r_cler-low = r_bank-low.
      append r_cler.
    elseif skb1-fdlev eq 'B0' or skb1-fdlev eq 'C0'.  "master acct.
      r_mstr-low = r_bank-low.
      append r_mstr.
    endif.
    append r_bank.
  endselect.

* cm layout
  select * from ztfi_cmap client specified
           into corresponding fields of table icmap
           where mandt eq sy-mandt.

endform.                    " get_account
*&---------------------------------------------------------------------*
*&      Form  display_fm
*&---------------------------------------------------------------------*
form display_fm.
  data : mode like sy-tabix.

* FORMAT COLOR 2.

  clear : it_fm_list, isum.
  loop at it_fm_list.
*   FORMAT COLOR 2.
*   mode = sy-tabix MOD 2.

*   PERFORM set_mode USING mode.
    format intensified off.
    new-line.
    clear: sv_gubun, sv_gubun2.

    at new gubun.
      perform gubun_text using it_fm_list-gubun.
    endat.
    at new gubun2.
      perform gubun2_text using it_fm_list-gubun2.
    endat.

    write : sy-vline no-gap, sv_gubun  under h_gubun no-gap,
            sy-vline no-gap, sv_gubun2 under h_gubun2 no-gap.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/12/02,changed by WSKIM
*---Start
*            sy-vline NO-GAP, it_fm_list-acctxt UNDER h_textl NO-GAP.
    read table ifmap with key  slevl  = it_fm_list-gubun3.
    if sy-subrc = 0 and ifmap-titem eq space.
      clear it_fm_list-acctxt.
      write : sy-vline no-gap, it_fm_list-acctxt under h_textl no-gap.
    else.
      write : sy-vline no-gap, it_fm_list-acctxt under h_textl no-gap.
    endif.
*---End
    hide it_fm_list-acctxt.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/12/02,changed by WSKIM
*---Start
*    WRITE : sy-vline NO-GAP, it_fm_list-amount UNDER h_dmshb
*                 ROUND p_trunc DECIMALS p_decl
*                 CURRENCY sv_waers NO-GAP,
*            sy-vline NO-GAP, it_fm_list-sumamt UNDER h_dmshb1
*                 ROUND p_trunc DECIMALS p_decl
*                 CURRENCY sv_waers NO-GAP,
*            sy-vline NO-GAP.
    read table ifmap with key  slevl  = it_fm_list-gubun3.
    if sy-subrc = 0 and ifmap-titem eq space.
      data  : w_blank(20).
      write : sy-vline no-gap, w_blank,sy-vline no-gap, w_blank,
             sy-vline no-gap.
    else.
      write : sy-vline no-gap, it_fm_list-amount under h_dmshb
               round p_trunc decimals p_decl
               currency sv_waers no-gap,
          sy-vline no-gap, it_fm_list-sumamt under h_dmshb1
               round p_trunc decimals p_decl
               currency sv_waers no-gap,
          sy-vline no-gap.

    endif.

    add  : it_fm_list-amount to  sv_hslvt,
           it_fm_list-sumamt to  sv_hslvt1.

    at end of gubun.
      new-line.
      format color off.
      write: at 1(1) sy-vline, 22(1) sy-vline.
      write at 22(108) sy-uline.

      format color 3.
      format intensified off.
      write :/ sy-vline no-gap,
               sv_hslvt  under h_dmshb  round p_trunc decimals p_decl
                                        currency sv_waers no-gap,
               sv_hslvt1 under h_dmshb1 round p_trunc decimals p_decl
                                        currency sv_waers no-gap,
               sy-vline no-gap.
      format color off.
      uline.

      clear : sv_hslvt, sv_hslvt1.
    endat.

    at end of balgu.
      if it_fm_list-balgu <> space.
        sum. sv_hslvt2 = it_fm_list-amount.
        sv_hslvt3 = it_fm_list-sumamt.

        case it_fm_list-balgu.
          when '1'.  "operating bal.= revenue - expenditure
            isum-asum1 = sv_hslvt2.
            isum-bsum1 = sv_hslvt3.
            perform write_sub_total using sv_hslvt2 sv_hslvt3
                                          h_dmshb   h_dmshb1
                                          text-030.
          when '2'.  "bal.= op.bal.- investment - tax
            isum-asum2 = sv_hslvt2.
            isum-bsum2 = sv_hslvt3.
            sv_hslvt2 = sv_hslvt2 + isum-asum1.
            sv_hslvt3 = sv_hslvt3 + isum-bsum1.
*            isum-asum2 = sv_hslvt2.
*            isum-bsum2 = sv_hslvt3.
            perform write_sub_total using sv_hslvt2 sv_hslvt3
                                          h_dmshb   h_dmshb1
                                          text-031.
          when '3'.  "surplus/deficit before = bal.+ shdld.repay.
            isum-asum3 = sv_hslvt2.
            isum-bsum3 = sv_hslvt3.
            sv_hslvt2 = sv_hslvt2 + isum-asum2 + isum-asum1.
            sv_hslvt3 = sv_hslvt3 + isum-bsum2 + isum-bsum1.
*            isum-asum3 = sv_hslvt2.
*            isum-bsum3 = sv_hslvt3.
            perform write_sub_total using sv_hslvt2 sv_hslvt3
                                          h_dmshb   h_dmshb1
                                          text-032.
          when '4'.  "sur/def after = ..before + instrumnt + exp/rev
            sv_hslvt2 = sv_hslvt2 + isum-asum3
                        + isum-asum2 + isum-asum1.
            sv_hslvt3 = sv_hslvt3 + isum-bsum3
                        + isum-bsum2 + isum-bsum1.

            perform write_sub_total using sv_hslvt2 sv_hslvt3
                                          h_dmshb   h_dmshb1
                                          text-033.
        endcase.
      endif.
      clear: sv_hslvt2, sv_hslvt3.
    endat.
  endloop.
* WRITE:(118) sy-uline.
endform.                    " display_fm
*&---------------------------------------------------------------------*
*&      Form  set_mode
*&---------------------------------------------------------------------*
form set_mode using    p_mode.
  if p_mode = 0.
    format intensified on.
  else.
    format intensified off.
  endif.
endform.                    " set_mode
*&---------------------------------------------------------------------*
*&      Form  display_cm
*&---------------------------------------------------------------------*
form display_cm.
  data : mode like sy-tabix.

* FORMAT COLOR 2.

  sort it_cmal by gubun gubun2 gubun3 acctt.
  clear : it_cmal.
  loop at it_cmal.
*   FORMAT COLOR 2.
*   mode = sy-tabix MOD 2.

*   PERFORM set_mode USING mode.
    new-line.
    clear: sv_gubun, sv_gubun2.

    at new gubun.
      perform gubun_text using it_cmal-gubun.
    endat.
    at new gubun2.
      perform gubun2_text_cm using it_cmal-gubun2.
    endat.

    perform get_t035t using it_cmal-acctt it_cmal-gubun it_cmal-gubun2.

    write : sy-vline no-gap, sv_gubun  under h_gubun no-gap.
*           sy-vline NO-GAP, sv_gubun2 UNDER h_gubun2 NO-GAP,
    if it_cmal-gubun = '9'.   "account transfer
      clear sv_gubun2.
      write : sv_gubun2 under h_gubun2 no-gap,
      sy-vline no-gap, sv_textl  under h_textl no-gap.
    else.
      write : sy-vline no-gap, sv_gubun2 under h_gubun2 no-gap,
      sy-vline no-gap, sv_textl  under h_textl no-gap.
    endif.

    write : sy-vline no-gap, it_cmal-amount under h_dmshb
                 round p_trunc decimals p_decl
                 currency sv_waers no-gap,
            sy-vline no-gap, it_cmal-prvamt under h_dmshb1
                 round p_trunc decimals p_decl
                 currency sv_waers no-gap,
            sy-vline no-gap.

*--> draw ending balance uline..
    if it_cmal-gubun2 eq c_cate4.  "ending balance
      new-line.
      write: at 1(1) sy-vline, 22(1) sy-vline.
      write at 22(146) sy-uline.
    endif.

    at end of gubun.
      new-line.
      format color off.
      if it_cmal-gubun eq '9'.  "account transfer
        write at 1(140) sy-uline.
        continue.
      elseif it_cmal-gubun eq 'A'.
        uline.
        continue.
      endif.
    endat.
  endloop.
endform.                    " display_cm
*&---------------------------------------------------------------------*
*&      Form  write_sub_total
*&---------------------------------------------------------------------*
form write_sub_total using    p_sum1 p_sum2 u_sum1 u_sum2 c_text.
  format color 5.
  format intensified off.
  write :  '|' no-gap, c_text,
           p_sum1 under u_sum1 round p_trunc decimals p_decl
                               currency sv_waers no-gap,
           p_sum2 under u_sum2 round p_trunc decimals p_decl
                               currency sv_waers no-gap,
           '|' no-gap.
  uline.
  format color off.
endform.                    " write_sub_total
*&---------------------------------------------------------------------*
*&      Form  get_t035t
*&---------------------------------------------------------------------*
form get_t035t using p_acctt p_gubun p_gubun2.
  clear : sv_textl.
  case p_gubun.
*   available cash bal.->bank clg. acct.
    when 'A'.
      select single ltext from t036t into sv_textl "level name
       where spras = sv_spras
         and ebene = p_acctt.
      if   sy-subrc <> 0.
        move space  to sv_textl.
      endif.
*   account transfer
    when '9'.
*     PERFORM set_detail_itemtext USING text-020 text-021 ''
*                                       p_gubun2.
      read table icmap with key grptm = p_gubun2.
      if sy-subrc eq 0.
        move icmap-grptt to sv_textl.
      endif.
  endcase.
endform.                                                    " GET_T035T
*&---------------------------------------------------------------------*
*&      Form  gubun2_text
*&---------------------------------------------------------------------*
form gubun2_text using    p_gubun2.
  read table ifmap with key flevl = p_gubun2.
  if sy-subrc eq 0.
    sv_gubun2 = ifmap-text01.
  endif.
endform.                    " gubun2_text
*&---------------------------------------------------------------------*
*&      Form  gubun2_text_cm
*&---------------------------------------------------------------------*
form gubun2_text_cm using    p_gubun2.
  read table icmap with key grptm = p_gubun2.
  if sy-subrc eq 0.
    sv_gubun2 = icmap-grptt.
  endif.

*+ by ig.moon {
  if it_cmal-gubun2 eq 'A900'.
    move 'Other Balance' to sv_gubun.
    uline.
  endif.
* }

endform.                    " gubun2_text
*&---------------------------------------------------------------------*
*&      Form  gubun_text
*&---------------------------------------------------------------------*
form gubun_text using gubun.
  case gubun.
    when '1'.  "Revenue
      move text-011  to sv_gubun.
    when '2'.  "Expenditure
      move text-012  to sv_gubun.
    when '3'.
*ISSUE # 20041111-007 cash flow report
*Requested by GHLEE, changed by wskim,on 20041122
*----Start
*      MOVE text-013  TO sv_gubun.
      move text-014  to sv_gubun."Investment expenditure
    when '4'.
*      MOVE text-014  TO sv_gubun.
      move text-015  to sv_gubun. "Tax

    when '5'.
*      MOVE text-015  TO sv_gubun.
      move text-016  to sv_gubun."Scheduled repayment

    when '6'.
*      MOVE text-016  TO sv_gubun.
      move text-017  to sv_gubun. "Financial Instrumnt
    when '7'.
*      MOVE text-017  TO sv_gubun.
      move text-018  to sv_gubun."Financial Rev./Exp
*-----End
    when '8'. "Financial Rev./Exp
      move text-018  to sv_gubun.
    when '9'.  "Account Transfer
      move text-019  to sv_gubun.
    when 'A' or 'Z'.  "Availiable cash bal.
      move text-01a  to sv_gubun.
  endcase.
endform.                    " GUBUN_TEXT
*&---------------------------------------------------------------------*
*&      Form  set_detail_itemtext
*&---------------------------------------------------------------------*
form set_detail_itemtext using p_value1 p_value2 p_value3 p_value4.
  if p_value4+1(1) eq '0'.
    sv_textl = p_value1.
  elseif p_value4+1(1) eq '1'.
    sv_textl = p_value2.
  elseif p_value4+1(1) eq '2'.
    sv_textl = p_value3.
  endif.
endform.                    " set_detail_itemtext
*&---------------------------------------------------------------------*
*&      Form  summary_fm
*&---------------------------------------------------------------------*
form summary_fm.
  sort it_fmal by gubun gubun2 gubun3.
  clear : it_fmal.
  loop at it_fmal.
    move-corresponding it_fmal to it_fm_list.
    it_fm_list-sumamt = it_fmal-prvamt.

    read table ifmap with key flevl  = it_fmal-gubun2
                              slevl  = it_fmal-gubun3
                              titem  = it_fmal-acctt
                              textv  = it_fmal-textv.
    if sy-subrc eq 0.
**==> confirm hardcording?
*      IF ifmap-titem EQ '6011++'.
*        ifmap-textv = '1'.
*        it_fmal-textv = '1'.
*      ENDIF.
* check if summarize term exist.
      if not ifmap-textv is initial.
        it_fm_list-gubun3 = ifmap-textv.  "second level
        read table ifmap with key flevl  = it_fmal-gubun2
                                  slevl  = ifmap-textv.
      endif.

      it_fm_list-acctxt = ifmap-text01.
    endif.

    it_fmal-gubun3 = it_fm_list-gubun3.
    it_fmal-acctxt = it_fm_list-acctxt.
    modify it_fmal.  clear it_fmal.

    collect it_fm_list.   clear it_fm_list.
  endloop.

*.....calculate daily average
  if p_avrge eq 'X'.
    loop at it_fm_list.
      perform check_wrkday using it_fm_list-amount it_fm_list-sumamt
                                 sv_wrkday.  "working day
      modify it_fm_list.  clear it_fm_list.
    endloop.
    loop at it_cmal.
      perform check_wrkday using it_cmal-amount it_cmal-prvamt
                                 sv_wrkday.  "CM working day
      modify it_cmal.  clear it_cmal.
    endloop.
  endif.

* DELETE it_fm_list WHERE amount = 0 AND sumamt = 0.
  delete it_fm_list where amount = 0 and sumamt = 0 and acctxt = space.
endform.                    " summary_fm
*&---------------------------------------------------------------------*
*&      Form  check_wrkday
*&---------------------------------------------------------------------*
form check_wrkday using p_amount p_sumamt p_wrkday.
  p_amount = p_amount / p_wrkday.
  p_sumamt = p_sumamt / p_wrkday.
endform.                    " check_wrkday
*&---------------------------------------------------------------------*
*&      Form  call_drill_down
*&---------------------------------------------------------------------*
form call_drill_down.
  data: l_field(30),
        l_textv like ifmap-textv.

  get cursor field l_field.
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2004/12/01, by WSKIM
*---Start
*  CHECK l_field EQ 'IT_FM_LIST-AMOUNT'.
  refresh it_ddwn.
  if  l_field eq 'IT_FM_LIST-AMOUNT'.
*---End
    clear: it_ddwn[], it_ddwn, l_textv.

    loop at it_fmal where acctxt eq it_fm_list-acctxt
                    and   textv  ne space.
      l_textv = it_fmal-textv.
      exit.
    endloop.

    if l_textv eq space.
      read table it_fmal with key  acctxt = it_fm_list-acctxt.
      loop at ifmal where balgu  eq it_fmal-balgu
                    and   gubun  eq it_fmal-gubun
                    and   gubun2 eq it_fmal-gubun2
                    and   gubun3 eq it_fmal-gubun3
                    and   datum  in s_datum.
* {
*        PERFORM append_it_ddwn.
        perform append_it_ddwn_new.
*}
      endloop.
    else.
      loop at ifmal where balgu  eq it_fmal-balgu
                    and   gubun  eq it_fmal-gubun
                    and   gubun2 eq it_fmal-gubun2
                    and ( gubun3 eq l_textv or textv eq l_textv )
                    and   datum  in s_datum.
* {
*        PERFORM append_it_ddwn.
        perform append_it_ddwn_new.
*}
      endloop.
    endif.
*---Start
  endif.
  if  l_field eq 'IT_FM_LIST-SUMAMT'.
    clear: it_ddwn[], it_ddwn, l_textv.

    loop at it_fmal where acctxt eq it_fm_list-acctxt
                    and   textv  ne space.
      l_textv = it_fmal-textv.
      exit.
    endloop.
    if l_textv eq space.
      read table it_fmal with key acctxt = it_fm_list-acctxt.
      loop at ifmal where balgu  eq it_fmal-balgu
                    and   gubun  eq it_fmal-gubun
                    and   gubun2 eq it_fmal-gubun2
                    and   gubun3 eq it_fmal-gubun3
                    and   datum  in s_compa.
        perform append_it_ddwn.
      endloop.
    else.
      loop at ifmal where balgu  eq it_fmal-balgu
                    and   gubun  eq it_fmal-gubun
                    and   gubun2 eq it_fmal-gubun2
                    and ( gubun3 eq l_textv or textv eq l_textv )
                    and   datum  in s_compa.
        perform append_it_ddwn.
      endloop.
    endif.
  endif.
*---End
*  READ TABLE it_fmal WITH KEY acctxt = it_fm_list-acctxt.
*  IF it_fmal-textv IS INITIAL.
*    LOOP AT ifmal WHERE balgu  EQ it_fmal-balgu
*                  AND   gubun  EQ it_fmal-gubun
*                  AND   gubun2 EQ it_fmal-gubun2
*                  AND   gubun3 EQ it_fmal-gubun3
*                  AND   datum  IN s_datum.
*      PERFORM append_it_ddwn.
*    ENDLOOP.
*  ELSE.
*    LOOP AT ifmal WHERE balgu  EQ it_fmal-balgu
*                  AND   gubun  EQ it_fmal-gubun
*                  AND   gubun2 EQ it_fmal-gubun2
*                  AND   textv  EQ it_fmal-gubun3
*                  AND   datum  IN s_datum.
*      PERFORM append_it_ddwn.
*    ENDLOOP.
*  ENDIF.

  check not it_ddwn[] is initial.

  sort it_ddwn by budat dispw .
  export it_ddwn sv_waers to memory id 'Z10'.
  submit zrfit10 and return.
endform.                    " call_drill_down
*&---------------------------------------------------------------------*
*&      Form  append_it_ddwn
*&---------------------------------------------------------------------*
form append_it_ddwn.
*...CM data
  select single * from ztfi_cmal
                  where bukrs eq ifmal-bukrs
                  and   gjahr eq ifmal-gjahr
                  and   belnr eq ifmal-belnr
*                  AND   grupp EQ ifmal-grupp
*                  AND   ebene EQ ifmal-ebene
                  and   dispw eq ifmal-dispw
                  and   budat eq ifmal-datum
                  and   gsber eq ifmal-gsber.

  check sy-subrc eq 0.

  move-corresponding ztfi_cmal to it_ddwn.
  case ztfi_cmal-koart.
    when 'D'.  "customer
      it_ddwn-kacct = ztfi_cmal-kunnr.
    when 'K'.  "vendors
      it_ddwn-kacct = ztfi_cmal-lifnr.
    when 'S'.  "g/l accounts
      it_ddwn-kacct = ztfi_cmal-hkont.
  endcase.

*...FM data
  move-corresponding ifmal to it_ddwn.

*...group desc.
*  SELECT SINGLE textl INTO it_ddwn-grptt
*                FROM t035t
*                WHERE spras = sv_spras
*                AND   grupp = ifmal-grupp.

  append it_ddwn.  clear it_ddwn.
endform.                    " append_it_ddwn
*&---------------------------------------------------------------------*
*&      Form  append_initial_ci
*&---------------------------------------------------------------------*
form append_initial_ci.
  loop at ifmap.
    read table it_fmal with key balgu  = ifmap-tmflg
                                gubun  = ifmap-flevl(1)
                                gubun2 = ifmap-flevl
                                gubun3 = ifmap-slevl.

    if sy-subrc eq 0.  clear it_fmal. continue. endif.

    move : ifmap-tmflg    to it_fmal-balgu,
           ifmap-flevl(1) to it_fmal-gubun,
           ifmap-flevl    to it_fmal-gubun2,
           ifmap-slevl    to it_fmal-gubun3,
           ifmap-titem    to it_fmal-acctt.
    append it_fmal.  clear it_fmal.
  endloop.
endform.                    " append_initial_ci
*&---------------------------------------------------------------------*
*&      Form  append_initial_group
*&---------------------------------------------------------------------*
form append_initial_group.
  loop at icmap.
    read table it_cmal with key balgu  = icmap-tmflg
                                gubun  = icmap-grptm(1)
                                gubun2 = icmap-grptm.
    if sy-subrc <> 0.
      check icmap-grptm(1) ne 'A'.
      move : icmap-tmflg    to it_cmal-balgu,
             icmap-grptm(1) to it_cmal-gubun,
             icmap-grptm    to it_cmal-gubun2,
             icmap-frgrp    to it_cmal-acctt.
      append it_cmal.  clear it_cmal.
    endif.
  endloop.
endform.                    " append_initial_group
*&---------------------------------------------------------------------*
*&      Form  search_help_hkont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_help_hkont.
  data : begin of value_hkont occurs 0,
          saknr like ska1-saknr,
          txt50 like skat-txt50,
        end of value_hkont.
  data: scr_fields like dynpread occurs 0 with header line.
  data : selectfield   like  help_info-fieldname,
         it_fields     like  help_value occurs 0 with header line,
         it_fields1    like  help_value occurs 0 with header line,
         select_value  like  help_info-fldvalue,
         ld_tabix      like  sy-tabix.

  clear: selectfield, it_fields, select_value, ld_tabix.
* Help List.
  select a~saknr                                            "c~txt50
       into corresponding fields of table value_hkont
        from  ( ska1 as a
                    inner join skb1 as b
                      on a~saknr = b~saknr )
          where a~ktopl = 'HNA1'
            and b~bukrs = 'H201'
            and ( b~fdlev eq 'B0' or b~fdlev eq 'C0' ) .
  loop at  value_hkont.
    select single txt50 into value_hkont-txt50
         from skat
           where saknr eq  value_hkont-saknr
             and ktopl eq 'HNA1'
             and spras = 'E'.
    modify value_hkont.
  endloop.

  if sy-dbcnt eq 0.
    message s001 with 'ERROR.'.
    exit.
  endif.
* Hit List
  it_fields-tabname = 'SKA1'.
  it_fields-fieldname = 'SAKNR'.
  it_fields-selectflag = 'X'.
  append it_fields.

  it_fields-tabname = 'SKAT'.
  it_fields-fieldname = 'TXT50'.
  it_fields-selectflag = ' '.
  append it_fields.


  call function 'HELP_VALUES_GET_NO_DD_NAME'
       exporting
            selectfield                  = selectfield
       importing
            ind                          = ld_tabix
            select_value                 = select_value
       tables
            fields                       = it_fields
            full_table                   = value_hkont
       exceptions
            full_table_empty             = 1
            no_tablestructure_given      = 2
            no_tablefields_in_dictionary = 3
            more_then_one_selectfield    = 4
            no_selectfield               = 5
            others                       = 6.

  check not ld_tabix is initial.
  read table value_hkont index ld_tabix.
  value_hkont-saknr = select_value.
*
  scr_fields-fieldname  = 'S_HKONT-LOW'.
  scr_fields-fieldvalue = value_hkont-saknr.
  append scr_fields.
  move value_hkont-saknr to s_saknr-low.

  clear :  value_hkont[],it_fields[].
endform.                    " search_help_hkont
*&---------------------------------------------------------------------*
*&      Form  check_hkont
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_hkont.
  data : begin of value_hkont occurs 0,
         saknr like ska1-saknr,
         txt50 like skat-txt50,
       end of value_hkont.

  refresh value_hkont.
  check not s_saknr-low is initial.
  select a~saknr                                            "c~txt50
       into corresponding fields of table value_hkont
        from  ( ska1 as a
                    inner join skb1 as b
                      on a~saknr = b~saknr )
          where a~ktopl = 'HNA1'
            and b~bukrs = 'H201'
            and ( b~fdlev eq 'B0' or b~fdlev eq 'C0' ) .
  loop at  value_hkont.
    select single txt50 into value_hkont-txt50
         from skat
           where saknr eq  value_hkont-saknr
             and spras = 'E'.
    modify value_hkont.
  endloop.

  read table value_hkont with key saknr = s_saknr-low.
  if sy-subrc <> 0.
    message e002 with s_saknr-low.
    exit.
  endif.
endform.                    " check_hkont
*&---------------------------------------------------------------------*
*&      Form  get_data_glt0
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
form get_data_glt0 using    l_date
                   changing p_amount.
  data : y_mon type i.
  clear : l_mon,y_mon.

  if l_date(4) = s_datum-low(4).
    select * from glt0
             into corresponding fields of table iglt0
             where rldnr = '00'
               and rrcty = '0'
               and bukrs in s_bukrs
               and racct in r_bank
               and rtcur in s_dispw
               and ryear eq l_date(4).

    l_mon = l_date+4(2).
* glt0 -> current data
    loop at iglt0 where racct in r_mstr.
      clear : l_idx,l_amt.
      do.
        l_idx = l_idx + 1.
        if l_idx > l_mon.
          exit.
        else.
          if p_lcurr eq 'X'.
            concatenate 'IGLT0-HSL' l_idx into fs_hsl.
          else.
            concatenate 'IGLT0-TSL' l_idx into fs_hsl.
          endif.
          assign (fs_hsl) to <fs_hsl>.
          p_amount = p_amount + <fs_hsl>.

        endif.
      enddo.
      perform assign_amount using iglt0-hslvt iglt0-tslvt
                            changing l_amt. "C/F
      p_amount = p_amount + l_amt.
    endloop.

*    PERFORM get_data_glt0_pre USING l_mon.


  else.
    select * from glt0
           into corresponding fields of table iglt0
           where rldnr = '00'
             and rrcty = '0'
             and bukrs in s_bukrs
             and racct in r_bank
             and rtcur in s_dispw
             and ryear eq s_datum-low(4) .

    l_mon = s_datum-low+4(2).

* glt0 -> current data
    loop at iglt0 where racct in r_mstr.
      clear : l_idx,l_amt.
      if l_mon <> 01.
        l_mon = l_mon - 1.
        do.
          l_idx = l_idx + 1.
          if l_idx > l_mon.
            exit.
          else.
            if p_lcurr eq 'X'.
              concatenate 'IGLT0-HSL' l_idx into fs_hsl.
            else.
              concatenate 'IGLT0-TSL' l_idx into fs_hsl.
            endif.
            assign (fs_hsl) to <fs_hsl>.
            p_amount = p_amount + <fs_hsl>.
          endif.
        enddo.
      endif.
      perform assign_amount using iglt0-hslvt iglt0-tslvt
                            changing l_amt. "C/F
      p_amount = p_amount + l_amt.
    endloop.

*    PERFORM get_data_glt0_pre_d USING l_date.

  endif.

endform.                    " get_data_glt0
*&---------------------------------------------------------------------*
*&      Form  get_data_glt0_pre
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
form get_data_glt0_pre using  l_mon.

  l_mon_pv = l_mon - 1.
  loop at iglt0 where racct in r_mstr.
    clear l_idx.
    do.
      l_idx = l_idx + 1.
      if l_idx > l_mon_pv.
        exit.
      else.
        if p_lcurr eq 'X'.
          concatenate 'IGLT0-HSL' l_idx into fs_hsl.
        else.
          concatenate 'IGLT0-TSL' l_idx into fs_hsl.
        endif.
        assign (fs_hsl) to <fs_hsl>.
        it_cmal02-prvamt = it_cmal02-prvamt + <fs_hsl>.

      endif.
    enddo.
    perform assign_amount using iglt0-hslvt iglt0-tslvt
                          changing l_amt. "C/F
    it_cmal02-prvamt = it_cmal02-prvamt + l_amt.
  endloop.

endform.                    " get_data_glt0_pre
*&---------------------------------------------------------------------*
*&      Form  get_data_glt0_pre_d
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*----------------------------------------------------------------------*
form get_data_glt0_pre_d using    pl_date.

  l_mon = s_datum-low+4(2).
  l_mon_pv = l_mon - 1.
  if l_mon_pv = 0.
    loop at iglt0 where racct in r_mstr.
      perform assign_amount using iglt0-hslvt iglt0-tslvt
                           changing l_amt. "C/F
      it_cmal02-prvamt = it_cmal02-prvamt + l_amt.
    endloop.
  else.
    loop at iglt0 where racct in r_mstr.
      clear l_idx.
      if l_mon_pv <> 01.
        l_mon_pv  = l_mon_pv - 1.
        do.
          l_idx = l_idx + 1.
          if l_idx > l_mon_pv.
            exit.
          else.
            if p_lcurr eq 'X'.
              concatenate 'IGLT0-HSL' l_idx into fs_hsl.
            else.
              concatenate 'IGLT0-TSL' l_idx into fs_hsl.
            endif.
            assign (fs_hsl) to <fs_hsl>.
            it_cmal02-prvamt = it_cmal02-prvamt + <fs_hsl>.

          endif.
        enddo.
      endif.
      perform assign_amount using iglt0-hslvt iglt0-tslvt
                            changing l_amt. "C/F
      it_cmal02-prvamt = it_cmal02-prvamt + l_amt.
    endloop.
  endif.
endform.                    " get_data_glt0_pre_d
*&---------------------------------------------------------------------*
*&      Form  coversion_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form coversion_date.
  refresh r_sdatb.
  read table  s_datum index 1.
  move : s_datum-sign   to r_sdatb-sign,
         'BT'           to r_sdatb-option,
         s_datum-low(6) to r_sdatb-low,
         '01'           to r_sdatb-low+6(2),
         s_datum-high   to r_sdatb-high.
  append r_sdatb.

  refresh pv_datb  .
  read table s_compa  index 1.
  move : s_compa-sign   to pv_datb-sign,
*        s_compa-option TO pv_datb-option,
         'BT'           to pv_datb-option,
         s_compa-low(6) to pv_datb-low,
         '01'           to pv_datb-low+6(2),
         s_compa-high   to pv_datb-high.
  append pv_datb.

endform.                    " coversion_date
*&---------------------------------------------------------------------*
*&      Form  GET_ENDING_GL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      <--P_V_GLT0_AMT1  text
*----------------------------------------------------------------------*
form get_ending_gl tables p_date structure s_datum
                   changing p_amt.

  data : l_amt like bsis-dmbtr.
  clear : l_amt.

  data : begin of lt_bsis occurs 0 ,
         hkont like bsis-hkont,
         budat like bsis-budat,
         dmbtr like bsis-dmbtr,
         wrbtr like bsis-wrbtr,
         shkzg like bsis-shkzg,
         end of lt_bsis.

  data : lt_bsas like lt_bsis occurs 0 with header line.
  ranges: s_dat01 for sy-datum.

  read table p_date index 1.
  loop at p_date.
    s_dat01-sign = 'I'.   s_dat01-option = 'BT'.
    concatenate p_date-low+0(6) '01' into s_dat01-low.
    if p_date-high is initial.
      move p_date-low  to s_dat01-high.
    else.
      move p_date-high to s_dat01-high.
    endif.
    append s_dat01. clear s_dat01.
  endloop.

*   ..bsis: ..open item
  select hkont budat dmbtr wrbtr shkzg
*       INTO (lt_bsis-hkont, lt_bsis-budat,
*             lt_bsis-dmbtr, lt_bsis-wrbtr, lt_bsis-shkzg)
       into corresponding fields of table lt_bsis
       from bsis
       where bukrs in s_bukrs
       and   hkont in r_mstr
       and   ( gjahr = p_date-low(4)  or gjahr = p_date-high(4) )
       and    budat in s_dat01 .


  loop at lt_bsis.
    if lt_bsis-shkzg eq 'H'.
      lt_bsis-wrbtr = lt_bsis-wrbtr * -1.
      lt_bsis-dmbtr = lt_bsis-dmbtr * -1.
    endif.

*---> Ending balance (GL)
    perform assign_amount using lt_bsis-dmbtr lt_bsis-wrbtr
                          changing l_amt.
    p_amt = l_amt + p_amt.

  endloop.


*   ..bsas: ..clearing item
  select hkont budat dmbtr wrbtr shkzg
*         INTO (lt_bsas-hkont, lt_bsas-budat,
*               lt_bsas-dmbtr, lt_bsas-wrbtr, lt_bsas-shkzg)
       into corresponding fields of table lt_bsas
       from bsas
       where bukrs in s_bukrs
       and   hkont in r_mstr
       and   ( gjahr = p_date-low(4)  or gjahr = p_date-high(4) )
       and    budat in s_dat01 .

  loop at lt_bsas.
    if lt_bsas-shkzg eq 'H'.
      lt_bsas-wrbtr = lt_bsas-wrbtr * -1.
      lt_bsas-dmbtr = lt_bsas-dmbtr * -1.
    endif.

    perform assign_amount using lt_bsas-dmbtr lt_bsas-wrbtr
                          changing l_amt.
    p_amt = l_amt + p_amt.

  endloop.

endform.                    " GET_ENDING_GL
*&---------------------------------------------------------------------*
*&      Form  append_it_ddwn_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form append_it_ddwn_new.

  select single * from ztfi_cmal
                  where bukrs eq ifmal-bukrs
                  and   gjahr eq ifmal-gjahr
                  and   belnr eq ifmal-belnr
                  and   dispw eq ifmal-dispw
                  and   budat eq ifmal-datum
                  and   gsber eq ifmal-gsber.

  if sy-subrc eq 0.
    move-corresponding ztfi_cmal to it_ddwn.
    case ztfi_cmal-koart.
      when 'D'.  "customer
        it_ddwn-kacct = ztfi_cmal-kunnr.
      when 'K'.  "vendors
        it_ddwn-kacct = ztfi_cmal-lifnr.
      when 'S'.  "g/l accounts
        it_ddwn-kacct = ztfi_cmal-hkont.
    endcase.
  endif.

  move-corresponding ifmal to it_ddwn.

  select single textl into it_ddwn-grptt
                from t035t
                where spras = sv_spras
                and   grupp = it_ddwn-grupp.

  append it_ddwn.  clear it_ddwn.

endform.                    " append_it_ddwn_new
*&---------------------------------------------------------------------*
*&      Form  display_cm_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_cm_new.
  data : mode like sy-tabix.

* FORMAT COLOR 2.

* by ig.moon 4/3/2008 {
  data $ix like sy-tabix.
  sort iztfi_cmap by grptm.

  loop at it_cmal.
    $ix = sy-tabix.
    if it_cmal-gubun2 cp 'A7*' or
       it_cmal-gubun2 cp 'A9*'.
      it_cmal-balgu = '9'.
      it_cmal-gubun = 'Z'.
      modify it_cmal index $ix transporting gubun balgu.
    endif.
  endloop.
* }

  sort it_cmal by balgu gubun gubun2 gubun3 acctt.
  clear : it_cmal.

  loop at it_cmal.
    $ix = sy-tabix.
*   FORMAT COLOR 2.
*   mode = sy-tabix MOD 2.

*   PERFORM set_mode USING mode.
    new-line.
    clear: sv_gubun, sv_gubun2.

    at new gubun.
      perform gubun_text using it_cmal-gubun.
    endat.
    at new gubun2.
      perform gubun2_text_cm using it_cmal-gubun2.
    endat.

    perform get_t035t using it_cmal-acctt it_cmal-gubun it_cmal-gubun2.

    write : sy-vline no-gap, sv_gubun  under h_gubun no-gap.
*           sy-vline NO-GAP, sv_gubun2 UNDER h_gubun2 NO-GAP,
    if it_cmal-gubun = '9'.   "account transfer
      clear sv_gubun2.
      write : sv_gubun2 under h_gubun2 no-gap,
      sy-vline no-gap, sv_textl  under h_textl no-gap.
    else.

*+ by ig.moon {
      if it_cmal-gubun2 cp 'A9*'.
        clear : sv_gubun, sv_textl.
      endif.
* }

      write : sy-vline no-gap, sv_gubun2 under h_gubun2 no-gap,
      sy-vline no-gap, sv_textl  under h_textl no-gap.
    endif.

    write : sy-vline no-gap, it_cmal-amount under h_dmshb
                 round p_trunc decimals p_decl
                 currency sv_waers no-gap,
            sy-vline no-gap, it_cmal-prvamt under h_dmshb1
                 round p_trunc decimals p_decl
                 currency sv_waers no-gap,
            sy-vline no-gap.

*--> draw ending balance uline..
    if it_cmal-gubun2 eq c_cate4.  "ending balance
      new-line.
      write: at 1(1) sy-vline, 22(1) sy-vline.
      write at 22(146) sy-uline.
    endif.

    at end of gubun.
      new-line.
      format color off.
      if it_cmal-gubun eq '9'.  "account transfer
        write at 1(140) sy-uline.
        continue.
      elseif it_cmal-gubun eq 'A'.
        uline.
        continue.
      elseif it_cmal-gubun eq 'Z'.
        uline.
      endif.
    endat.

    if it_cmal-balgu eq '9'.
      read table iztfi_cmap with key grptm = it_cmal-gubun2
      binary search.
      if sy-subrc eq 0.
        if iztfi_cmap-sign ne '='.
          it_cmal-amount = it_cmal-prvamt = 0.
          modify it_cmal index $ix transporting amount prvamt.
        endif.
      endif.
    endif.

    at end of balgu.
      if it_cmal-balgu <> space.
        sum.
        sv_hslvt2 = it_cmal-amount.
        sv_hslvt3 = it_cmal-prvamt.

        case it_cmal-balgu.
          when '9'.  "Total Free Cash Balance

            perform write_sub_total using sv_hslvt2 sv_hslvt3
                                          h_dmshb   h_dmshb1
                                          text-044.
        endcase.
      endif.
      clear: sv_hslvt2, sv_hslvt3.
    endat.

  endloop.

endform.                    " display_cm_new
*&---------------------------------------------------------------------*
*&      Form  get_cm_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_cm_new.
  data wrkday type i.
  ranges s_hkont for ztfi_cmal-hkont.

  __cls :  it_plan, iztfi_cmap, it_cmal_o, it_cmal .

  call function 'Z_FFI_GET_CASH_FLOW'
       exporting
            ioption = '02'
            ilcurr  = 'X'
            igroup  = 'P1'
            iwaers  = sv_waers
            iavrge  = ' '
            ialsel  = '1'
            ipread  = 'X'
            ifunc   = '0'
       importing
            ewrkday = wrkday
       tables
            tbukrs  = s_bukrs
            tdatum  = s_datum
            tdispw  = s_dispw
            thkont  = s_saknr  "-LOWs_hkont
            ftab01  = it_plan  "input <->output
            ftab02  = iztfi_cmap
            ftab03  = it_cmal_o.


  delete it_plan where sumflg ne space.
  sort it_plan by fstflg scdflg.

  loop at it_plan.
    it_cmal-balgu = it_plan-sumflg.
    it_cmal-gubun = it_plan-fstflg.
    it_cmal-gubun2 = it_plan-scdflg.
    it_cmal-acctt = it_plan-grupp.
    it_cmal-amount = it_plan-sumamt.
    it_cmal-dispw = it_plan-dispw.
    append it_cmal.
    clear it_cmal.
  endloop.


  __cls :  it_plan, iztfi_cmap, it_cmal_o.

  call function 'Z_FFI_GET_CASH_FLOW'
       exporting
            ioption = '02'
            ilcurr  = 'X'
            igroup  = 'P1'
            iwaers  = sv_waers
            iavrge  = ' '
            ialsel  = '1'
            ipread  = 'X'
            ifunc   = '0'
       importing
            ewrkday = wrkday
       tables
            tbukrs  = s_bukrs
            tdatum  = s_compa
            tdispw  = s_dispw
            thkont  = s_hkont
            ftab01  = it_plan  "input <->output
            ftab02  = iztfi_cmap
            ftab03  = it_cmal_o.

  delete it_plan where sumflg ne space.
  sort it_plan by fstflg scdflg.

  data $ix like sy-tabix.

  sort it_plan by sumflg fstflg scdflg grupp.

  loop at it_cmal.
    $ix = sy-tabix.
    read table it_plan with key sumflg = it_cmal-balgu
                                fstflg = it_cmal-gubun
                                scdflg = it_cmal-gubun2
                                grupp = it_cmal-acctt
                                binary search.
    if sy-subrc eq 0.
      it_cmal-prvamt = it_plan-sumamt.
      modify it_cmal index $ix transporting prvamt.
    endif.

  endloop.

  sort it_cmal by balgu gubun gubun2 gubun3 acctt.

endform.                    " get_cm_new
