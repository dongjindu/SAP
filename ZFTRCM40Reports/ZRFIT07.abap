*&--------------------------------------------------------------------
*& Author                 : JIPARK
*& Creation Date          : 11/10/2003
*& Specification By       : JIPARK
*& Pattern                : Report 1-7
*& Development Request No : UD1K903862
*& Addl documentation     :
*& Description  : Cash flow report(CM)
*&
*& Modification Log
*& Date         Developer      Request ID      Description
*& 08/26/2005   Shiva          UD1K917378      Display the detail
*&                   UD1K916748, UD1K917499   line for the total amount.
*&--------------------------------------------------------------------
report zrfit06 message-id zmfi
               no standard page heading line-size 130 .
*                                        line-count 67."73

**********************************************************************
* Data Declaration
**********************************************************************
tables : fdsr, fdsb, fdes, fdt1, t036, t038, t001,
         ztfi_cmap, ztfi_cmaf, ztfi_altm, ztfi_pltm.
tables : ztfi_cmal,t035, skb1, glt0, bsis, bsas.
tables : tzpa, t036v, t036r, t037, t036p, pyordh, knb1.

data: begin of it_fdes occurs 0.
        include structure fdes.
data: orign like t036-orign,         "..Original Source
      o_ebe like fdes-ebene.         "..Original plan level
data: end of it_fdes.

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

data : it_cmal like ztfi_cmal occurs 0 with header line.
data : iztfi_cmap like ztfi_cmap occurs 0 with header line.
data : icmap like ztfi_cmap occurs 0 with header line.

data : it_pltm like ztfi_pltm occurs 0 with header line,
       it_altm like ztfi_altm occurs 0 with header line,
       it_cmtm like ztfi_cmtm occurs 0 with header line.

data : begin of isum,
         asum1 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
         bsum1 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
         asum2 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
         bsum2 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
         asum3 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
         bsum3 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
       end of isum.
data : begin of isum2,
         hslvt_r  type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
         hslvt_rr type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
       end of isum2.
data : isum3 like isum2.
data : begin of pftab  occurs 0,
         fcode like  rsmpe-func,
       end   of pftab.

data : sv_fstflg(17) type c,
       sv_scdflg(18) type c,
       sv_textl  like t035t-textl,
       sv_hslvt  type bekobj,   "output length 16
       sv_hslvt1 type bekobj,
       sv_hslvt2 type bekobj,
       sv_hslvt3 type bekobj,
*      sv_hslvt  LIKE ztfi_cmal-wrshb, "glt0-hslvt,
*      sv_hslvt1 LIKE ztfi_cmal-wrshb, "glt0-hslvt,
*      sv_hslvt2 LIKE ztfi_cmal-wrshb, "glt0-hslvt,
*      sv_hslvt3 LIKE ztfi_cmal-wrshb, "glt0-hslvt,
       sv_spras  like t001-spras,
       sv_%hslvt(8),  " TYPE p DECIMALS 2,
       sv_diff1 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
       sv_diff2 type bekobj, "LIKE ztfi_cmal-wrshb, "glt0-hslvt,
       sv_trunctxt(15).
data : l_plan_amount   type bekobj, "=> Output length 16
       l_actual_amount type bekobj.

data : gv_fdsbsum like glt0-hslvt.
data : sv_ktopl like t001-ktopl, "??????
       sv_waers like t001-waers,
       sv_to_date like sy-datum,
       sv_open_sum like glt0-hslvt,
       sv_%value(8),
       sv_psel,        "actual selection date
       sv_func,        "if plan data, distribution rule, payment cycle
                       "if actual data, "current actual" option
       wrkday type i.  "calculate working day

*-- List amount header
data : h_dmshb(16),
       h_dmshb1(16).

*..Working Info.
ranges: r_wrk_fdsb  for  t036-ebene,
        r_wrk_fdsr  for  t036-ebene,

        r_ftype     for  vtbfhapo-sfhazba.

ranges: $grupp  for ztfi_cmal-grupp,
        $group  for ztfi_cmal-ebene,            "..Grouping.Level
        $group2 for ztfi_cmal-grupp,            "..Grouping.Group
        pv_date for sy-datum,
        r_datum for sy-datum,
        s_gsber for fdes-gsber.

constants: c_levl1 type fdes-ebene value 'BT',
*          c_bxgrupp(2) VALUE 'BX',
*          c_bxtext(7)  VALUE 'Others',
           c_nvalue(16) value '-',
           c_flg2(4)    value 'A200',
           c_flg3(4)    value 'A300',
           c_flg5(4)    value 'A500',
           c_flg6(4)    value 'A600',
           c_flg7(4)    value 'A700',
           c_flga(4)    value 'A110'.  "surplus/deficit

constants: h_fstflg(17)   value  '                 ',
           h_scdflg(18)  value  '                  ',
           h_group(5)   value  'P/G',
           h_textl(25)   value  'Description',
*          h_dmshb(16)   VALUE  'Plan Amount',
*          h_dmshb1(16)  VALUE  'Actual Amount',
           h_diff(16)    value  'Diff. Amount',
           h_%dmshb(8)   value  'Diff(%)',
           h_head(130)   value   'Cash Flow Report',
           h_headline(130) value '=========================',
           h_date(10)    value 'Date',
           h_plan1(5)     value 'Plan',
           h_plan2(5)     value 'Plan',
           h_grp(5)      value 'Group',
           h_lvl(5)      value 'Level',
           h_bus(4)      value 'Bus.',
           h_area(4)     value 'Area',
           h_cur(4)      value 'CURR',
           h_amt(15)     value 'Amount'.

constants: c_cate0(4) value 'A',  "Available cash bal.
           c_cate1(4) value 'A100', "Beg. Bal.
           c_cate4(4) value 'A400', "bank ending
           c_cate6(4) value 'A600', "open balance
           c_cate7(4) value 'A700', "future balance
           c_catea(4) value 'A110'. "surplus/deficit.

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

data: begin of i036 occurs 100.
        include structure t036.
data: end of i036.
data: begin of i036q occurs 10.
        include structure t036q.
data: end of i036q.
data: begin of i037 occurs 100.
        include structure t037.
data: end of i037.
data: begin of i039 occurs 10.
        include structure t039.
data: end of i039.
data: wabnkko     like fdsb-bnkko,
      wabukrs     like fdsb-bukrs.
data: lines       type i.
data: begin of gkoarttab occurs 10,
        koart like bseg-koart,
        grupp like t035-grupp,
      end   of gkoarttab.
data: begin of zus_fdtab_felder,
        diskb  like fdsb-bnkko,        "dispositive Kontobezeichnung
        azshb  like fdsb-wrshb,
      end   of zus_fdtab_felder.
data: begin of fdtab occurs 0.

*     hier darf nichts eingefügt werden, sonst Fehler nach Rückkehr vom
*     FB cash_forecast_select_and_compr, da aus Performancegründen
*     statt move-corresponding nur move verwendet wird
        include structure rf40v.
        include structure zus_fdtab_felder.
data: end   of fdtab.
data: begin of it_fdes_d occurs 0.
        include structure fdes.
data: orign like t036-orign,         "..Original Source
      o_ebe like fdes-ebene,         "..Original plan level
      dweek like ztfi_des-dweek.
data: end of it_fdes_d.
data: begin of wa_fdm1,
        rftyp like fdm1-rftyp,
        ebeln like fdm1-ebeln,
        ebelp like fdm1-ebelp,
        bukrs like fdm1-bukrs,
        fdgrp like fdm1-fdgrp,
        fdlev like fdm1-fdlev,
        currd like fdm1-currd,
        fdtag like fdm1-fdtag,
        fdwbt like fdm1-fdwbt,
      end of wa_fdm1.
data: begin of wa_fdm2,
        rftyp like fdm2-rftyp,
        banfn like fdm2-banfn,
        bukrs like fdm2-bukrs,
        fdgrp like fdm2-fdgrp,
        fdlev like fdm2-fdlev,
        currd like fdm2-currd,
        fdtag like fdm2-fdtag,
        fddbt like fdm2-fddbt,
        lifnr like fdm2-lifnr,
      end of wa_fdm2.
data: begin of wa_fds2,
        awtyp like fds2-awtyp,
        awref like fds2-awref,
        awpos like fds2-awpos,
        bukrs like fds2-bukrs,
        kunrg like fds2-kunrg,
        fdgrp like fds2-fdgrp,
        fdtag like fds2-fdtag,
        twaer like fds2-twaer,
        fdlev like fds2-fdlev,
        oaume like fds2-oaume,
        basme like fds2-basme,
        dmshb like fds2-dmshb,
      end of wa_fds2.
data: it_fdm1 like table of wa_fdm1,
      it_fdm2 like table of wa_fdm2,
      it_fds2 like table of wa_fds2.

*variable..
data : sv_fr_datum like ztfi_cmal-datum,
       sv_to_datum like ztfi_cmal-datum,
       sv_rdacct(10),
       sv_glt0_amt like glt0-hslvt,
       dispw_lin   type i,
       l_bsis_dmbtr like bsis-dmbtr,
       l_bsas_dmbtr like bsas-dmbtr,
       l_bsis_wrbtr like bsis-wrbtr,
       l_bsas_wrbtr like bsas-wrbtr.

data: cucol(20)   type c,
      vondt       like fdsb-datum,
      bisdt       like fdsb-datum,
      bisd1       like fdsb-datum,
      biswo       like scal-week,
      textu(47)   type c,
      betrg       like fdsb-wrshb.
data: mindat     like sy-datum value '00000000',
      maxdat     like sy-datum value '20981231'.
data: w_grupp like fdes-grupp,
      w_ebene like fdes-ebene.
*Ranges..
ranges:r_bank   for skb1-saknr occurs 0,          "bank account
       r_cler   for skb1-saknr occurs 0,
       r_mstr   for skb1-saknr occurs 0.

* field symbol
field-symbols : <fs_hsl> type any.
data fs_hsl(15).

**********************************************************************
*  SELECTION-SCREEN
**********************************************************************
*...executing condition
selection-screen begin of block b1 with frame title text-001.
select-options : s_bukrs for glt0-bukrs obligatory default 'H201'
                         no-extension no intervals,
                 s_datum for ztfi_cmal-datum
                         obligatory default sy-datum no-extension.
select-options : s_hkont for ztfi_cmal-hkont
                         no-extension no intervals.
selection-screen end of block b1.
*...plan data
selection-screen begin of block b3 with frame title text-003.
selection-screen begin of line.
parameters : p_cmtm radiobutton group rd1 default 'X'. "CBO Read
selection-screen comment 5(21) text-040.
parameters : p_coll radiobutton group rd1.             "Std.Collect
selection-screen comment 30(23) text-041.

parameters : p_alflg radiobutton group rd1.
selection-screen comment 57(22) text-044.

selection-screen end of line.
parameters : p_glied like t038-glied default 'P1' obligatory.
*PARAMETERS : p_orign(3) TYPE c. " LIKE t036-orign DEFAULT 'BNK'.
selection-screen end of block b3.
*...actual data
selection-screen begin of block b4 with frame title text-042.
selection-screen begin of line.
parameters : p_dnow  radiobutton group rd2 default 'X'.
selection-screen comment 5(21) text-006.
parameters : p_mprv  radiobutton group rd2.
selection-screen comment 30(23) text-007.
parameters : p_yprv  radiobutton group rd2.
selection-screen comment 57(22) text-008.
selection-screen end of line.
selection-screen end of block b4.
*...currency option
selection-screen begin of block b5 with frame title text-043.
selection-screen  begin of line.
selection-screen comment 1(31) text-010.
parameter : p_lcurr as checkbox default 'X'. "local currency
selection-screen  end of line.
select-options : s_dispw for fdsr-dispw.
selection-screen end of block b5.
*...display option
selection-screen begin of block b2 with frame title text-002.
selection-screen begin of line.
selection-screen comment (15) text-004.
selection-screen position 33.
*--(scale)
parameters: p_trunc like rfpdo1-ts70skal  default '0'.
selection-screen comment 35(1) text-005.
*--(decimals)
parameters: p_decl like rfpdo1-ts70skal  default '0'.
selection-screen end of line.
selection-screen  begin of line.
selection-screen comment 1(31) text-009.
parameter : p_avrge as checkbox. "daily average
selection-screen  end of line.
parameters : p_dst as checkbox,   "distribution rule
             p_pay as checkbox.   "payment cycle
selection-screen end of block b2.
***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
*Issue Number : FI-20041111-007, Requested by GHLEE
*Changed on 2005/01/12, by WSKIM
*---Start
at selection-screen on value-request for s_hkont-low.
  perform search_help_hkont.
*---End

***********************************************************************
* AT SELECTION-SCREEN
***********************************************************************
at selection-screen.
  data : dispw_lin type i.

  select spras ktopl waers into (sv_spras, sv_ktopl, sv_waers)
         from t001
         where bukrs in s_bukrs.
    exit.
  endselect.

* check cash account.
  if not s_hkont[] is initial.
    select single saknr into skb1-saknr
                  from skb1
                  where bukrs in s_bukrs
                  and   saknr in s_hkont.
    if sy-subrc <> 0.
      message e002 with 'Cash account' s_hkont-low.
    endif.
  endif.

*  IF NOT s_datum-high IS INITIAL AND
*         s_datum-low+0(6) <> s_datum-high+0(6).
*    MESSAGE e004.
*  ELSEIF s_datum-high IS INITIAL.
*    LOOP AT s_datum.
*      s_datum-high = s_datum-low.  "add by JIPARK
*      MODIFY s_datum.
*    ENDLOOP.
*  ENDIF.
* Scale
  concatenate '(' p_trunc '/' p_decl ')' into sv_trunctxt.
* display currency
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
* actual selection date
  if p_dnow eq 'X'.
    sv_psel = 1.  "current date
  elseif p_mprv eq 'X'.
    sv_psel = 2.  "prev. month
  elseif p_yprv eq 'X'.
    sv_psel = 3. "prev. year
  endif.

* if plan vs. actual report..
* plan data => distribution rule, payment cycle
  if p_dst eq space and p_pay eq space.
    sv_func = 0.   "don't adjust
  elseif p_dst eq 'X' and p_pay eq space.
    sv_func = 1.   "only distribution rule
  elseif p_dst eq space and p_pay eq 'X'.
    sv_func = 2.   "only payment cycle
  elseif p_dst eq 'X' and p_pay eq 'X'.
    sv_func = 3.   "all adjust
  endif.
* if previous year actual vs. current actual report..
* actual data flag
  if p_alflg eq 'X'.
    sv_func = 'X'.
  endif.
***********************************************************************
* INITIALIZATION
***********************************************************************
  case sv_func.
    when 'X'.
      h_dmshb  = 'Prev. Actual'.
      h_dmshb1 = 'Curr. Actual'.
    when others.
      h_dmshb  = 'Plan Amount'.
      h_dmshb1 = 'Actual Amount'.
  endcase.

***********************************************************************
* TOP-OF-PAGE
***********************************************************************
top-of-page.
* SET   MARGIN 5 5.
  skip 2."4
  format color col_background.
  read table r_datum index 1.
  write :/ h_head centered.
  write :/ h_headline centered.

  skip 2. "3

  write :/  'Period : ', r_datum-low.
  if  not r_datum-high is initial.
    write : ' ~ ', r_datum-high.
  endif.
  write : 112 'Date : ', sy-datum.
  if s_dispw-low is initial.
    write :/  'Currency : ', sv_waers, sv_trunctxt.
  else.
    write :/  'Currency : ', s_dispw-low, sv_trunctxt.
  endif.
  write : 112 'Page : ', sy-pagno.
  if p_avrge eq 'X'.
    write :/  'Working day : ', wrkday.
  endif.

  if s_hkont[] is initial.
    write :/ 'Cash Account : all'.
  else.
    write :/ 'Cash Account : ', s_hkont-low.
  endif.
  format color off.
*  skip 1.
  uline.
  format color col_heading.
  write :/ sy-vline no-gap, h_fstflg no-gap,
           sy-vline no-gap, h_scdflg no-gap,
           sy-vline no-gap, h_group  no-gap,
           sy-vline no-gap, h_textl centered  no-gap,
           sy-vline no-gap, h_dmshb  no-gap,  "plan amt
           sy-vline no-gap, h_dmshb1 no-gap,  "actual amt
           sy-vline no-gap, h_diff   no-gap,
           sy-vline no-gap, h_%dmshb no-gap.
  format color off.
  write: sy-vline no-gap.

  uline .

top-of-page during line-selection.
  format color col_heading.
  if sy-lsind = 1.
    write: /(10) h_date, (5) h_plan1, (5) h_plan2, (4) h_bus,
            (3) h_cur, h_amt.
    write: / h_grp under h_plan1, h_lvl under h_plan2,
             h_area under h_bus.
  endif.
  uline .
***********************************************************************
* START-OF-SELECTION
***********************************************************************
start-of-selection.
  perform setting_date.

  if p_cmtm eq 'X'.
    perform get_existing_cm .
    call function 'Z_FFI_GET_CASH_FLOW'
         exporting
              ioption = '02'
              ilcurr  = p_lcurr
              igroup  = p_glied
*             iorign  = p_orign
              iwaers  = sv_waers
              iavrge  = p_avrge
              ialsel  = sv_psel
              ipread  = 'X'
              ifunc   = sv_func
         importing
              ewrkday = wrkday
         tables
              tbukrs  = s_bukrs
              tdatum  = s_datum
              tdispw  = s_dispw
              thkont  = s_hkont
              ftab01  = it_plan  "input <->output
              ftab02  = iztfi_cmap
              ftab03  = it_cmal.
  else.
    call function 'Z_FFI_GET_CASH_FLOW'
         exporting
              ioption = '02'
              ilcurr  = p_lcurr
              igroup  = p_glied
*             iorign  = p_orign
              iwaers  = sv_waers
              iavrge  = p_avrge
              ialsel  = sv_psel
              ifunc   = sv_func
         importing
              ewrkday = wrkday
         tables
              tbukrs  = s_bukrs
              tdatum  = s_datum
              tdispw  = s_dispw
              thkont  = s_hkont
              ftab01  = it_plan
              ftab02  = iztfi_cmap
              ftab03  = it_cmal
              ftab04  = it_fdes
              ftab05  = it_fdes_d
              ftab06  = it_fdm1
              ftab07  = it_fdm2
              ftab08  = it_fds2.
  endif.

  if it_plan[] is initial.
    message s001.  stop.
  endif.

  if sv_func eq 'X'.
    pftab-fcode = 'PLAN'.  append pftab.
    pftab-fcode = 'ACTL'.  append pftab.
    delete pftab where fcode eq 'PREV' or
                       fcode eq 'CURR'.
  else.
    pftab-fcode = 'PREV'.  append pftab.
    pftab-fcode = 'CURR'.  append pftab.
    delete pftab where fcode eq 'PLAN' or
                       fcode eq 'ACTL'.
  endif.
  set pf-status 'MAIN' excluding pftab.

***********************************************************************
* END-OF-SELECTION
***********************************************************************
end-of-selection.
  perform display_data.
***********************************************************************
* AT LINE-SELECTION
***********************************************************************
*Issue Number : FI-20041111-009, Requested by GHLEE
*Changed on 2004/12/01,changed by WSKIM
*double click
*---Start
  data: wa_fdes like it_fdes,
        w_cline type i,
        w_datum(10) type c,
        w_datum1 like sy-datum.

  sort: it_fdes by grupp ebene datum,
        it_fdes_d by grupp ebene datum.

at line-selection.
*Modification by 100565, 100471 Issue log 20050616-002
*Helpdesk 55BB392843
  format hotspot on.
  if sy-lsind = 1.
    perform call_drill_down.
  endif.

  if sy-lsind = 2.
    format hotspot off.
    w_cline = sy-lilli.
    read line w_cline field value it_fdes-grupp into w_grupp
                                  it_fdes-ebene into w_ebene
                                  it_fdes-datum into w_datum.
    call function 'CONVERT_DATE_TO_INTERNAL'
         exporting
              date_external = w_datum
         importing
              date_internal = w_datum1.

    loop at it_fdes_d where grupp = w_grupp
                      and   ebene = w_ebene.
      write: / it_fdes_d-datum,it_fdes_d-gsber,it_fdes_d-grupp,
               it_fdes_d-ebene,it_fdes_d-merkm,it_fdes_d-zuonr,
               it_fdes_d-refer,it_fdes_d-dispw,it_fdes_d-wrshb.
    endloop.
    if sy-subrc ne 0.
      loop at it_fdm1 into wa_fdm1
                      where fdgrp = w_grupp
                      and   fdlev = w_ebene
                      and   fdtag = w_datum1.
        write: / wa_fdm1-fdgrp, wa_fdm1-fdlev, wa_fdm1-fdtag,
                  wa_fdm1-ebeln, wa_fdm1-ebelp,
                 wa_fdm1-currd, wa_fdm1-fdwbt.
      endloop.
      if sy-subrc ne 0.
        loop at it_fdm2 into wa_fdm2
                        where fdgrp = w_grupp
                        and   fdlev = w_ebene
                        and   fdtag = w_datum1.
          write: / wa_fdm2-fdgrp, wa_fdm2-fdlev, wa_fdm2-currd,
                   wa_fdm2-fdtag, wa_fdm2-rftyp, wa_fdm2-banfn,
                   wa_fdm2-lifnr, wa_fdm2-fddbt.
        endloop.
        if sy-subrc ne 0.
          loop at it_fds2 into wa_fds2
                          where fdgrp = w_grupp
                          and   fdlev = w_ebene
                          and   fdtag = w_datum1.
            write: / wa_fds2-twaer, wa_fds2-fdgrp, wa_fds2-fdlev,
                       wa_fds2-fdtag, wa_fds2-awtyp, wa_fds2-awref,
                       wa_fds2-awpos, wa_fds2-kunrg, wa_fds2-oaume,
                       wa_fds2-basme, wa_fds2-dmshb.
          endloop.
        endif.
      endif.
    endif.
  endif.
*---End
***********************************************************************
* AT USER-COMMAND
***********************************************************************
at user-command.
  case sy-ucomm.

    when 'SAVE'.
      perform save_ztfi_cmtm.
*Issue Number : FI-20041111-009, Requested by GHLEE
*Changed on 2004/12/01,changed by WSKIM
*double click
*---Start
*    WHEN 'PLAN'.
*      PERFORM call_drill_down_plan.

*    WHEN 'ACTL' OR 'CURR'.
*      CLEAR: it_ddwn[], it_ddwn.
*      LOOP AT it_cmal WHERE grupp EQ it_plan-grupp
*                      AND   budat IN s_datum.
*        PERFORM call_drill_down_actual.
*      ENDLOOP.
*
*      CHECK NOT it_ddwn[] IS INITIAL.
*      SORT it_ddwn BY budat dispw.
*      EXPORT it_ddwn sv_waers TO MEMORY ID 'Z10'.
*      SUBMIT zrfit10 AND RETURN.
*---End
    when 'PREV'.
      clear: it_ddwn[], it_ddwn.
      loop at it_cmal where grupp eq it_plan-grupp
                      and   not budat in s_datum.
        perform call_drill_down_actual.
      endloop.

      check not it_ddwn[] is initial.
      sort it_ddwn by budat dispw.
      export it_ddwn sv_waers to memory id 'Z10'.
      submit zrfit10 and return.
  endcase.

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
form display_data.
  data : mode like sy-tabix.

* by ig.moon 4/3/2008 {
  sort iztfi_cmap by grptm.
  data $ix like sy-tabix.
  loop at it_plan.
    $ix = sy-tabix.
    if it_plan-scdflg cp 'A7*' or
       it_plan-scdflg cp 'A9*'.
      it_plan-sumflg = '9'.
      it_plan-fstflg = 'Z'.
      modify it_plan index $ix transporting fstflg sumflg.
    endif.
  endloop.
* }

  sort it_plan by fstflg scdflg grupp.

  clear : it_plan, isum.
  loop at it_plan.
    $ix = sy-tabix.
    format intensified off.
*   mode = sy-tabix MOD 2.

*   PERFORM set_mode USING mode.
    new-line.
    clear: sv_fstflg, sv_scdflg.

    at new fstflg.
      perform fstflg_text using it_plan-fstflg it_plan-scdflg.
    endat.
    at new scdflg.  "add by JIPARK
      perform scdflg_text using it_plan-scdflg.
    endat.

    perform get_t035t using it_plan-fstflg it_plan-grupp it_plan-scdflg.

    write : sy-vline no-gap, (17) sv_fstflg  no-gap under h_fstflg.

    if it_plan-fstflg = '3' or "divestiture
       it_plan-fstflg = '5' or "tax
       it_plan-fstflg = '9'.   "account transfer
*      it_plan-fstflg = 'A'.   "available cash bal.
      clear sv_scdflg.
      write : sv_scdflg under h_scdflg no-gap.
    else.
      write : sy-vline no-gap, (18) sv_scdflg under h_scdflg no-gap.
    endif.

    if it_plan-fstflg = '9'.  "account transfer
      clear it_plan-grupp.
      write :  it_plan-grupp under h_group no-gap,
               sy-vline no-gap, sv_textl under h_textl no-gap.
    else.

*+ by ig.moon {
      if it_plan-scdflg cp 'A9*'.
        clear : it_plan-grupp, sv_textl.
      endif.
* }
      write : sy-vline no-gap, (5) it_plan-grupp under h_group no-gap,
              sy-vline no-gap, (25) sv_textl under h_textl no-gap.
    endif.

    l_plan_amount   = it_plan-amount.
    l_actual_amount = it_plan-sumamt.

    case sv_func.
      when 'X'.
* modification by 100565
        format hotspot on.
*end modif
        write : sy-vline no-gap, l_plan_amount under h_dmshb
                     round p_trunc decimals p_decl
                     currency sv_waers no-gap,
                sy-vline no-gap, l_actual_amount under h_dmshb1
                     round p_trunc decimals p_decl
                     currency sv_waers no-gap,
                sy-vline no-gap.

      when others.
*---> plan => cash balance => sur/def,open,bank recon,ending bal(gl),
*                             accumulated open items,free cash bal : '-'
        if it_plan-scdflg eq c_flg2 or
           it_plan-scdflg eq c_flg3 or
           it_plan-scdflg eq c_flg5 or
           it_plan-scdflg eq c_flg6 or
           it_plan-scdflg eq c_flg7 or

* by ig.moon {
           it_plan-scdflg cp 'A9*' or
* }

           it_plan-scdflg eq c_flga.
* modification by 100565
          format hotspot on.
*end modif

          write : sy-vline no-gap, c_nvalue under h_dmshb
                       round p_trunc decimals p_decl right-justified
                       currency sv_waers no-gap,
                  sy-vline no-gap, l_actual_amount under h_dmshb1
                       round p_trunc decimals p_decl
                       currency sv_waers no-gap,
                  sy-vline no-gap.
        else.
* modification by 100565
          format hotspot on.
*end modif

*         WRITE : sy-vline NO-GAP, it_plan-amount UNDER h_dmshb
          write : sy-vline no-gap, l_plan_amount under h_dmshb
                       round p_trunc decimals p_decl
                       currency sv_waers no-gap,
*             sy-vline NO-GAP, it_plan-sumamt UNDER h_dmshb1
                  sy-vline no-gap, l_actual_amount under h_dmshb1
                       round p_trunc decimals p_decl
                       currency sv_waers no-gap,
                  sy-vline no-gap.
        endif.
    endcase.

    perform compute_%value using it_plan-amount it_plan-sumamt
                           changing sv_%value.
    sv_diff1 = it_plan-sumamt - it_plan-amount.

    write : sv_diff1 under h_diff round p_trunc decimals p_decl
            currency sv_waers no-gap, sy-vline no-gap.
    write : sv_%value right-justified under h_%dmshb no-gap,
            sy-vline no-gap.

    hide : it_plan-grupp.
    add  : it_plan-amount   to  sv_hslvt,
           it_plan-sumamt   to  sv_hslvt1.

*--> draw ending balance uline..
    if it_plan-scdflg eq c_cate4. "ending balance
      new-line.
      write: at 1(1) sy-vline, 19(1) sy-vline.
      write at 19(112) sy-uline.
    endif.

    at end of fstflg.

      new-line.

      format color off.
      if it_plan-fstflg eq '5' or "tax
         it_plan-fstflg eq '3'.   "Divestiture (+03/29)
        write: at 1(1) sy-vline, 38(1) sy-vline.
        write at 38(93) sy-uline.
      elseif it_plan-fstflg eq '9'.  "account transfer
        write at 1(130) sy-uline.
        continue.
      elseif it_plan-fstflg eq 'A'.
        uline.
        continue.
*+ by ig.moon 4/3/2008 {
      elseif it_plan-fstflg eq 'Z'.
        uline.
* }
      else.
        write: at 1(1) sy-vline, 19(1) sy-vline.
        write at 20(111) sy-uline.
      endif.

*+ by ig.moon 4/3/2008 {
      if it_plan-fstflg ne 'Z'.
* }
        format color 3.
        format intensified off.

        perform compute_%value using sv_hslvt sv_hslvt1
                               changing sv_%hslvt.
        sv_diff2 = sv_hslvt1 - sv_hslvt. "actual - plan

        write :/ sy-vline no-gap,
                 sv_hslvt  under h_dmshb  round p_trunc decimals p_decl
                                          currency sv_waers no-gap,
                 sv_hslvt1 under h_dmshb1 round p_trunc decimals p_decl
                                          currency sv_waers no-gap.
        write : sv_diff2 under h_diff round p_trunc decimals p_decl
                currency sv_waers no-gap. "sy-vline NO-GAP.
        write : sv_%hslvt right-justified under h_%dmshb no-gap.
        format color off.
        write : sy-vline no-gap.

        uline.

        clear : sv_hslvt, sv_hslvt1.
      endif.

    endat.

    if it_plan-sumflg eq '9'.
      read table iztfi_cmap with key grptm = it_plan-scdflg
      binary search.
      if sy-subrc eq 0.
        if iztfi_cmap-sign ne '='.
          it_plan-sumamt = it_plan-amount = 0.
          modify it_plan index $ix transporting sumamt amount.
        endif.
      endif.
    endif.

    at end of sumflg.
      if it_plan-sumflg <> space.
        sum.

        sv_hslvt2 = it_plan-amount.
        sv_hslvt3 = it_plan-sumamt.

        perform compute_%value using sv_hslvt2 sv_hslvt3
                               changing sv_%hslvt.
        sv_diff2 = sv_hslvt3 - sv_hslvt2.
        case it_plan-sumflg.

          when '9'.  "Total Free Cash Balance

            isum-asum1 = sv_hslvt2.
            isum-bsum1 = sv_hslvt3.

            perform write_sub_total using sv_hslvt2 sv_hslvt3 sv_%hslvt
                                          sv_diff2
                                          h_dmshb   h_dmshb1  h_%dmshb
                                          h_diff    text-055.

          when '1'.  "Operating balance
            isum-asum1 = sv_hslvt2.
            isum-bsum1 = sv_hslvt3.
            perform write_sub_total using sv_hslvt2 sv_hslvt3 sv_%hslvt
                                            sv_diff2
                                           h_dmshb   h_dmshb1  h_%dmshb
                                            h_diff    text-030.
          when '2'.  "Balance Before Financing
            isum-asum2 = sv_hslvt2.
            isum-bsum2 = sv_hslvt3.
            sv_hslvt2 = sv_hslvt2 + isum-asum1.
            sv_hslvt3 = sv_hslvt3 + isum-bsum1.
*            isum-asum2 = sv_hslvt2.
*            isum-bsum2 = sv_hslvt3.
            perform write_sub_total using sv_hslvt2 sv_hslvt3 sv_%hslvt
                                            sv_diff2
                                           h_dmshb   h_dmshb1  h_%dmshb
                                            h_diff    text-031.
          when '3'.  "Surplus/Deficit Before Financial Instrument
            isum-asum3 = sv_hslvt2.
            isum-bsum3 = sv_hslvt3.
            sv_hslvt2 = sv_hslvt2 + isum-asum2 + isum-asum1.
            sv_hslvt3 = sv_hslvt3 + isum-bsum2 + isum-bsum1.
*            isum-asum3 = sv_hslvt2.
*            isum-bsum3 = sv_hslvt3.
            perform write_sub_total using sv_hslvt2 sv_hslvt3 sv_%hslvt
                                            sv_diff2
                                           h_dmshb   h_dmshb1  h_%dmshb
                                            h_diff    text-032.
          when '4'.  "Surplus/Deficit After Financial Instrument
            sv_hslvt2 = sv_hslvt2 + isum-asum3
                        + isum-asum2 + isum-asum1.
            sv_hslvt3 = sv_hslvt3 + isum-bsum3
                        + isum-bsum2 + isum-bsum1.

            perform write_sub_total using sv_hslvt2 sv_hslvt3 sv_%hslvt
                                            sv_diff2
                                           h_dmshb   h_dmshb1  h_%dmshb
                                            h_diff    text-033.
        endcase.
      endif.
      clear: sv_hslvt2, sv_hslvt3.

    endat.

  endloop.
endform.                    " display_data
*&---------------------------------------------------------------------
*&      Form  SET_MODE
*&---------------------------------------------------------------------
form set_mode using    p_mode.
  if p_mode = 0.
    format intensified on.
  else.
    format intensified off.
  endif.
endform.                    " SET_MODE
*&---------------------------------------------------------------------
*&      Form  fstflg_TEXT
*&---------------------------------------------------------------------
form fstflg_text using fstflg scdflg.
  case fstflg.
    when '1'.
      move text-011  to sv_fstflg.
    when '2'.
      move text-012  to sv_fstflg.
    when '3'.
      move text-013  to sv_fstflg.
    when '4'.
      move text-014  to sv_fstflg.
    when '5'.
      move text-015  to sv_fstflg.
    when '6'.
      move text-016  to sv_fstflg.
    when '7'.
      move text-017  to sv_fstflg.
    when '8'.
      move text-018  to sv_fstflg.
    when '9'.
      move text-019  to sv_fstflg.
    when 'A'.
      move text-01a  to sv_fstflg.
  endcase.
endform.                    " fstflg_TEXT
*&---------------------------------------------------------------------
*&      Form  GET_T035T
*&---------------------------------------------------------------------
form get_t035t using p_fstflg p_grupp p_scdflg.
  clear : sv_textl.

  case p_fstflg.
*   available cash bal.->bank clg. acct.
    when 'A'.
*      IF p_grupp EQ c_bxgrupp. "BX
*        sv_textl = c_bxtext.  "Others
*      ELSE.
      select single ltext from t036t into sv_textl "level name
       where spras = sv_spras
         and ebene = p_grupp.
      if   sy-subrc <> 0.
        move space  to sv_textl.
      endif.
*      ENDIF.

*   account transfer
    when '9'.
*      PERFORM set_detail_itemtext USING text-020 text-021 ''
*                                        p_scdflg.
      read table iztfi_cmap with key grptm = p_scdflg binary search.
      if sy-subrc eq 0.
        move iztfi_cmap-grptt to sv_textl.
      endif.

    when others.
      select single textl from t035t into sv_textl "group name
       where spras = sv_spras
         and grupp = p_grupp.
      if   sy-subrc <> 0.
        move space  to sv_textl.
      endif.
  endcase.

endform.                                                    " GET_T035T
*&---------------------------------------------------------------------
*&      Form  scdflg_text
*&---------------------------------------------------------------------
form scdflg_text using p_fstflg.
  read table iztfi_cmap with key grptm = p_fstflg binary search.
  if sy-subrc eq 0.
    sv_scdflg = iztfi_cmap-grptt.
  endif.

*+ by ig.moon {
  if it_plan-scdflg eq 'A900'.
    move 'Other Balance' to sv_fstflg.
    uline.
  endif.
* }
endform.                    " scdflg_text
*&---------------------------------------------------------------------
*&      Form  write_sub_total
*&---------------------------------------------------------------------
form write_sub_total using p_sum1 p_sum2 p_% p_diff
                           u_sum1 u_sum2 u_% u_diff c_text.
  format color 5.
  format intensified off.
  write :  sy-vline no-gap, c_text,
           p_sum1 under u_sum1 round p_trunc decimals p_decl
                               currency sv_waers no-gap,
           p_sum2 under u_sum2 round p_trunc decimals p_decl
                               currency sv_waers no-gap.

  write : p_diff under u_diff round p_trunc decimals p_decl
          currency sv_waers no-gap. "sy-vline NO-GAP.
  write : p_%    right-justified under u_%    no-gap.
  format color off.
  write: sy-vline no-gap.
  uline.

endform.                    " write_sub_total
*&---------------------------------------------------------------------
*&      Form  set_detail_itemtext
*&---------------------------------------------------------------------
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
*&      Form  compute_%value
*&---------------------------------------------------------------------*
*       ( Actual - Plan ) / abs(Plan)
*&---------------------------------------------------------------------*
form compute_%value using value1 value2 %value.
  data : l_%ceil(8) type p decimals 2,
         l_absvalue like it_plan-amount.

  l_absvalue = abs( value1 ).
  clear: %value.
  if value1 = 0.
    %value = '-'.
  else.

*   l_%ceil = ( value1 - value2 ) / value1.
    l_%ceil = ( value2 - value1 ) / l_absvalue.
    if l_%ceil > 999.
      l_%ceil = 999.
    endif.
    %value = l_%ceil.
  endif.
endform.                    " compute_%value
*&---------------------------------------------------------------------*
*&      Form  call_drill_down_plan
*&---------------------------------------------------------------------*
form call_drill_down_plan.
  case p_cmtm.
    when 'X'.  "if DB Read..
      clear: it_pltm[], it_pltm.
      select * from ztfi_pltm
               into corresponding fields of table it_pltm
               where frdat eq s_datum-low
               and   todat eq s_datum-high
               and   bukrs eq s_bukrs-low
               and   grupp eq it_plan-grupp.

      check not it_pltm[] is initial.
      export it_pltm sv_waers sv_spras to memory id 'Z15'.
      submit zrfit15 and return.
* Modification by 100565,100471issue log 20050616-002
* Helpdesk 55BB392843
    when space.
      read table r_datum index 1.
      sort it_fdes by bukrs bnkko grupp ebene.
      loop at it_fdes where grupp eq it_plan-grupp.
       write: /(10) it_fdes-datum, (5) it_fdes-grupp, (5) it_fdes-ebene,
                    (4) it_fdes-gsber, (3) it_fdes-dispw, it_fdes-wrshb.
      endloop.
  endcase.


*          PERFORM drill_down_plan USING it_fdes-grupp
*                                        it_fdes-ebene
*                                        it_fdes-bnkko
*                                        it_fdes-bukrs.

* End Modification by 100565 issue log 20050616-002
* End Helpdesk 55BB392843


*    WHEN space.
*      READ TABLE r_datum INDEX 1.
*      SORT it_fdes BY bukrs bnkko grupp ebene.
*      LOOP AT it_fdes WHERE grupp EQ it_plan-grupp.
*        AT NEW ebene.
*          PERFORM drill_down_plan USING it_fdes-grupp
*                                        it_fdes-ebene
*                                        it_fdes-bnkko
*                                        it_fdes-bukrs.
*        ENDAT.
*      ENDLOOP.

endform.                    " call_drill_down_plan
*&---------------------------------------------------------------------*
*&      Form  call_drill_down_actual
*&---------------------------------------------------------------------*
form call_drill_down_actual.
  move-corresponding it_cmal  to  it_ddwn.
  case it_cmal-koart.
    when 'D'.  "customer
      it_ddwn-kacct = it_cmal-kunnr.
    when 'K'.  "vendors
      it_ddwn-kacct = it_cmal-lifnr.
    when 'S'.  "g/l accounts
      it_ddwn-kacct = it_cmal-hkont.
  endcase.
*...FM data
  select single fipos  fincode fistl knbelnr kngjahr
            into (it_ddwn-fipos,  it_ddwn-fincode, it_ddwn-fistl,
                  it_ddwn-knbelnr, it_ddwn-kngjahr)
                from ztfi_fmal
                where bukrs eq it_cmal-bukrs
                and   gjahr eq it_cmal-gjahr
                and   belnr eq it_cmal-belnr
*                 and   STUNR ..
*                AND   grupp EQ it_cmal-grupp
*                AND   ebene EQ it_cmal-ebene
                and   dispw eq it_cmal-dispw
                and   datum eq it_cmal-budat
                and   gsber eq it_cmal-gsber.
*...group desc.
  select single textl into it_ddwn-grptt
                from t035t
                where spras = sv_spras
                and   grupp = it_cmal-grupp.

  append it_ddwn.  clear it_ddwn.
endform.                    " call_drill_down_actual
*&---------------------------------------------------------------------*
*&      Form  call_drill_down
*&---------------------------------------------------------------------*
form call_drill_down.
  data l_field(20).

  get cursor field l_field.
*  CHECK it_plan-grupp NE space.
  case l_field.
    when 'L_PLAN_AMOUNT'.
      perform call_drill_down_plan.

    when 'L_ACTUAL_AMOUNT'.
      clear: it_ddwn[], it_ddwn.
      loop at it_cmal where grupp eq it_plan-grupp
                      and   budat in s_datum.
        perform call_drill_down_actual.
      endloop.

      check not it_ddwn[] is initial.
      sort it_ddwn by budat dispw.
      export it_ddwn sv_waers to memory id 'Z10'.
      submit zrfit10 and return.
  endcase.

endform.                    " call_drill_down
*&---------------------------------------------------------------------*
*&      Form  save_ztfi_cmtm
*&---------------------------------------------------------------------*
form save_ztfi_cmtm.
  clear: it_pltm[], it_pltm, it_altm[], it_altm, it_cmtm[], it_cmtm.

*Issue Number : FI-20041111-009, Requested by GHLEE
*Changed on 2004/12/10,changed by WSKIM
*---Start
  data : w_int type i.
  refresh it_pltm.clear w_int.
  select * into corresponding fields of table it_pltm
   from ztfi_pltm
    where  bukrs = s_bukrs-low
      and  frdat = s_datum-low
      and  todat = s_datum-high.
  describe table it_pltm lines w_int.
  if w_int <> 0.
    delete ztfi_pltm from table it_pltm.
  endif.
  refresh it_pltm.

  refresh it_cmtm.clear w_int.
  select * into corresponding fields of table it_cmtm
   from ztfi_cmtm
    where  bukrs = s_bukrs-low
      and  frdat = s_datum-low
      and  todat = s_datum-high.
  describe table it_cmtm lines w_int.
  if w_int <> 0.
    delete ztfi_cmtm from table it_cmtm.
  endif.
  refresh it_cmtm.
*---End

*.. Save plan orign data
  loop at it_fdes.
    move-corresponding it_fdes to it_pltm.
    it_pltm-bukrs = s_bukrs-low.
    it_pltm-frdat = s_datum-low.
    it_pltm-todat = s_datum-high.
    append it_pltm.  clear it_pltm.
  endloop.

*  DELETE ztfi_pltm FROM TABLE it_pltm.
*
* INSERT ztfi_pltm FROM TABLE it_pltm.
  modify ztfi_pltm from table it_pltm.
  if sy-subrc <> 0.
    message s000 with 'Failed to save'.
    rollback work.  exit.
  endif.
**.. Save actual orign data
*  LOOP AT it_cmal.
*    MOVE-CORRESPONDING it_cmal TO it_altm.
*    it_altm-bukrs = s_bukrs-low.
*    it_altm-frdat = s_datum-low.
*    it_altm-todat = s_datum-high.
*    APPEND it_altm.  CLEAR it_altm.
*  ENDLOOP.
*
*  DELETE ztfi_altm FROM TABLE it_altm.
*
*  INSERT ztfi_altm FROM TABLE it_altm.
*  IF sy-subrc <> 0.
*    MESSAGE s000 WITH 'Failed to save'.
*    ROLLBACK WORK.  EXIT.
*  ENDIF.
*
*.. Save plan-actual orign data
  loop at it_plan.
    move-corresponding it_plan to it_cmtm.
    it_cmtm-bukrs = s_bukrs-low.
    it_cmtm-frdat = s_datum-low.
    it_cmtm-todat = s_datum-high.
    it_cmtm-sumamt = ''.
    append it_cmtm.  clear it_cmtm.
  endloop.

*  DELETE ztfi_cmtm FROM TABLE it_cmtm.

  insert ztfi_cmtm from table it_cmtm.
  if sy-subrc eq 0.
    commit work.
    message s007.
  else.
    message s000 with 'Failed to save'.
    rollback work.  exit.
  endif.
endform.                    " save_ztfi_cmtm
*&---------------------------------------------------------------------*
*&      Form  get_existing_cm
*&---------------------------------------------------------------------*
form get_existing_cm .
  data: l_year like t5a4a-dlyyr,
        w_int type i.
  clear : it_cmal[], it_cmal, it_plan[], it_plan,
          iztfi_cmap[], iztfi_cmap.

  select * from ztfi_cmtm
           into corresponding fields of table it_plan
           where bukrs in s_bukrs
           and   frdat eq s_datum-low
           and   todat eq s_datum-high.
*Issue Number : FI-20041111-009, Requested by GHLEE
*Changed on 2004/12/10,changed by WSKIM
*---Start
  clear : w_int.
  describe table it_plan lines  w_int.
  if w_int = 0.
    message s011 with 'Not found data'.
    stop.
  endif.
*---End
*  SELECT * FROM ztfi_altm
*           INTO CORRESPONDING FIELDS OF TABLE it_cmal
*           WHERE bukrs IN s_bukrs
*           AND   frdat EQ s_datum-low
*           AND   todat EQ s_datum-high.

  select * from ztfi_pltm
           into corresponding fields of table it_fdes
           where bukrs in s_bukrs
           and   frdat eq s_datum-low
           and   todat eq s_datum-high.

*  PERFORM get_account(saplzgfi_cash_flow) TABLES r_bank iztfi_cmap.

  pftab-fcode = 'SAVE'.
  append pftab.
*  pv_date-sign = 'I'.    pv_date-option = 'BT'.
*  IF p_prv EQ 'X'.  "last year
*    l_year = 1.
*  ENDIF.
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*       EXPORTING
*            date      = s_datum-low
*            days      = 0
*            months    = 1
*            signum    = '-'
*            years     = l_year
*       IMPORTING
*            calc_date = pv_date-low.
*
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*       EXPORTING
*            date      = s_datum-high
*            days      = 0
*            months    = 1
*            signum    = '-'
*            years     = l_year
*       IMPORTING
*            calc_date = pv_date-high.
*  APPEND pv_date.
*
endform.                    " get_existing_cm
*&---------------------------------------------------------------------*
*&      Form  setting_date
*&---------------------------------------------------------------------*
form setting_date.
  data l_last_day like sy-datum.

  r_datum[] = s_datum[].
  loop at r_datum where high is initial.
    r_datum-high = r_datum-low.
    modify r_datum.
  endloop.

  read table s_datum index 1.
  if s_datum-high is initial.
    s_datum-option = 'BT'.
    s_datum-high = s_datum-low.
    modify s_datum index sy-tabix.
  endif.

  check s_datum-low(6) ne s_datum-high(6).
  l_last_day = s_datum-high.
  call function 'RP_LAST_DAY_OF_MONTHS'
       exporting
            day_in            = s_datum-low
       importing
            last_day_of_month = s_datum-high.
  modify s_datum index sy-tabix.

  do.
    call function 'MONTH_PLUS_DETERMINE'
         exporting
              months  = 1
              olddate = s_datum-low
         importing
              newdate = s_datum-low.
    concatenate s_datum-low(6) '01' into s_datum-low.

    call function 'RP_LAST_DAY_OF_MONTHS'
         exporting
              day_in            = s_datum-low
         importing
              last_day_of_month = s_datum-high.

    if s_datum-high(6) eq l_last_day(6).
      s_datum-high = l_last_day.
      append s_datum.
      exit.
    else.
      append s_datum.
    endif.
  enddo.
endform.                    " setting_date
*&---------------------------------------------------------------------*
*&      Form  drill_down_plan
*&---------------------------------------------------------------------*
form drill_down_plan using grupp ebene bnkko bukrs.
  data: l_fdgrp like fdsr-grupp.
  statics l_st_immo_gsart like range of t036v-gsart.
  data:   l_immo_gsart like line of l_st_immo_gsart.

*----------------------------Verprobung Ebene------------------------*
  perform t036_lesen.                  "Dispositionsebenen-Tabelle
  loop at i036 where ebene = ebene.
    exit.
  endloop.
  if sy-subrc <> 0.
    message e105(rq) with ebene.
  endif.

*------ Selektionszeitraum bestimmen --------------------------------*
*  PERFORM ermittlung_von_bis_datum.
*------ bei Liq.vorschau ggf. von/bis-Daten ändern, falls Zahlungs-
*------ termine bei FF70-Anzeige berücksichtigt wurden
*  if  p_zahll    = 'X'      "X-Zahlungstermine berücksichtigen
** 1-es gibt Verschiebungen aufgrund von Zahlungsterminen
*  and zahll-flag = '1'
*  and i039-xtfst = space.   "Space - Liquiditätsvorschau
*    call function 'CASH_FORECAST_PAYMENT_RUN_2'
*         exporting
*              i_group     = grupp
*              i_level     = ebene
*              i_date_from = vondt
*              i_date_to   = bisdt
*         importing
*              e_date_from = vondt
*              e_date_to   = bisdt
*         tables
*              s_bukrs     = s_bukrs.
*  endif.

*----------------------------Verprobung Herkunftssymbol---------------*
*  PERFORM t039_lesen.                  "Herkunftssymbole
*  LOOP AT i039 WHERE orign = i036-orign.
*    EXIT.
*  ENDLOOP.
*  IF sy-subrc <> 0.
*    MESSAGE e106(rq) WITH i036-orign.
*  ENDIF.
*
**--------Range aufbauen mit Produkt
**arten, die zu Immobilien gehören ---*
*  IF l_st_immo_gsart[] IS INITIAL.
*    l_immo_gsart-sign   = 'I'.
*    l_immo_gsart-option = 'EQ'.
*    SELECT gsart FROM tzpa INTO tzpa-gsart
*           WHERE rantyp = '3'    "Produktarten von Immo
*           OR    rantyp = '8'
*           OR    rantyp = '9'.
*      l_immo_gsart-low = tzpa-gsart.
*      APPEND l_immo_gsart TO l_st_immo_gsart.
*    ENDSELECT.
*  ENDIF.
*
*  IF i039-xtfst <> space.              "X - Tagesfinanzstatus
*    l_fdgrp = space.   "in GRUPP steht dann die dispositve Kontobez.
*    WRITE: bnkko TO wabnkko,
*           bukrs TO wabukrs.
*
**----------------------------Ebene gehört zum Treasury ? ------------*
**   Ebene gehört zum Tagesfinanzstatus -> in T036V wird in den Feldern
**   FDLEVSK und FDLEVSK2 nachgesehen, ob die Ebene für das große
**   Treasury verwendet wird.
*    IF l_st_immo_gsart[] IS INITIAL.
*      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
*             WHERE ( fdlevsk = ebene OR fdlevsk2 = ebene ).
*      ENDSELECT.
*    ELSE.
*      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
*             WHERE ( fdlevsk = ebene OR fdlevsk2 = ebene )
*             AND   NOT gsart IN l_st_immo_gsart.   "aber nicht Immo
*      ENDSELECT.
*    ENDIF.
*  ELSE.                                "X - Liquiditätsvorschau
*    l_fdgrp = grupp.                   "Dispogruppe aus T035
**   Ebene gehört nicht zum Tagesfinanzstatus -> FDLEVPK und FDLEVPK2
**   in T036V relevant.
*    CLEAR: wabukrs.
*    IF l_st_immo_gsart[] IS INITIAL.
*      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
*             WHERE ( fdlevpk = ebene OR fdlevpk2 = ebene ).
*      ENDSELECT.
*    ELSE.
*      SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
*             WHERE ( fdlevpk = ebene OR fdlevpk2 = ebene )
*             AND   NOT gsart IN l_st_immo_gsart.   "aber nicht Immo
*      ENDSELECT.
*    ENDIF.
*  ENDIF.
*
*  IF sy-subrc = 0.                     "-> Ebene gehört zum Treasury
*    CALL FUNCTION 'CASH_FORECAST_TR_SELECT_ITEM'
*         EXPORTING
*              ebene            = ebene
*              grupp            = l_fdgrp
*              date_low         = r_datum-low
*              date_high        = r_datum-high
*              bankk            = wabnkko
*              bukrs            = wabukrs
*         TABLES
*              s_bukrs          = s_bukrs
*              s_dispw          = s_dispw
*         EXCEPTIONS
*              e_no_items_found = 1
*              e_no_authority   = 2
*              OTHERS           = 3.
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE e147(rq) WITH ebene.       "Nothing found
*      WHEN 2.
*        MESSAGE e188(rq) WITH wabukrs.     "No authority
*      WHEN 3.
*        MESSAGE e380(rq).                  "No display possible
*    ENDCASE.
*    CLEAR wabnkko.
*
*    EXIT.
*  ENDIF.                               "if sy-subrc = 0
*
**----------------------------Ebene gehört zum Immobilienmanagement ?
*  IF  i039-xtfst = space                "d.h. Liq.vorschau
*  AND NOT l_st_immo_gsart[] IS INITIAL.
**   Derzeit schreibt Immo nur auf Debitor oder Kreditor fort.
**   Ebene gehört nicht zum Tagesfinanzstatus -> FDLEVPK und FDLEVPK2
**   in T036V relevant.
*    CLEAR: wabukrs.
*    SELECT bukrs FROM t036v INTO t036v-bukrs UP TO 1 ROWS
*           WHERE ( fdlevpk = ebene OR fdlevpk2 = ebene )
*           AND   gsart IN l_st_immo_gsart.   "Immo
*    ENDSELECT.
*
*    IF sy-subrc = 0.                   "-> Ebene gehört zu Immobilien
*      CALL FUNCTION 'CASH_FORECAST_RE_SELECT_ITEM'
*           EXPORTING
*                ebene            = ebene
*                fdgrp            = l_fdgrp
*                date_low         = r_datum-low
*                date_high        = r_datum-high
*                bukrs            = wabukrs
*           TABLES
*                s_dispw          = s_dispw
*                s_bukrs          = s_bukrs
*           EXCEPTIONS
*                e_no_items_found = 1
*                e_no_authority   = 2
*                OTHERS           = 3.
*
*      CASE sy-subrc.
*        WHEN 1.
*          MESSAGE e217(rq) WITH ebene.     "Nothing found
*        WHEN 2.
*          MESSAGE e188(rq) WITH wabukrs.   "No authority
*        WHEN 3.
*          MESSAGE e380(rq).                "No display possible
*      ENDCASE.
*
*      EXIT.
*    ENDIF.                             "if sy-subrc = 0
*  ENDIF.                               "if i039-xtfst = space ...
*
**----------------------------Ebene gehört zu den Zahlungsanordnungen ?
*  SELECT ebene FROM t036r INTO t036r-ebene UP TO 1 ROWS
*          WHERE levpr = ebene.
*  ENDSELECT.
*
*  IF sy-subrc = 0.   "-> Ebene gehört zu den Zahlungsanordnungen
*    CALL FUNCTION 'CASH_FORECAST_TR_SELECT_ITEM'
*         EXPORTING
*              ebene            = ebene
*              grupp            = l_fdgrp
*              date_low         = r_datum-low
*              date_high        = r_datum-high
*              bankk            = wabnkko
*              bukrs            = wabukrs
*         TABLES
*              s_dispw          = s_dispw
*              s_bukrs          = s_bukrs
*         EXCEPTIONS
*              e_no_items_found = 1
*              e_no_authority   = 2
*              OTHERS           = 3.
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE e148(rq) WITH ebene.       "Nothing found
*      WHEN 2.
*        MESSAGE e188(rq) WITH wabukrs.     "No authority
*      WHEN 3.
*        MESSAGE e380(rq).                  "No display possible
*    ENDCASE.
*    CLEAR wabnkko.
*
*    EXIT.
*  ENDIF.
*
**----------------------------Einzelsatz (FF63) ? --------------------*
*  PERFORM t037_lesen.                  "Dispositionsarten-Tabelle
*  LOOP AT i037 WHERE ebene = ebene.
*    EXIT.
*  ENDLOOP.
**----------------------------Eintrag gefunden -> Aufruf RFTS6500-----*
**----------------------------(Sachkonto oder Debitor/Kreditor)-------*
*  IF sy-subrc = 0.
*    PERFORM submit_ff65 USING grupp bukrs ebene.
*    EXIT.
*  ENDIF.
*
**----------------------------Ebene gehört zu Agenturgeschäften ?-----*
*  PERFORM t036p_t036q_lesen.           "Ebenenzuordnung fuer Logistik
*  LOOP AT i036q
*       WHERE inteb = '201'             "201-Agenturgeschäft
*       AND   ebene = ebene.            "vom User ausgewählte Ebene
*    EXIT.
*  ENDLOOP.
*
*  IF sy-subrc = 0.   "-> Ebene gehört zu den Agenturgeschäften
*
*    CALL FUNCTION 'CASH_FORECAST_LO_SELECT_ITEM'
*         EXPORTING
*              ebene            = ebene
*              fdgrp            = l_fdgrp
*              date_low         = r_datum-low
*              date_high        = r_datum-high
*              bukrs            = wabukrs
*         TABLES
*              s_dispw          = s_dispw
*              s_bukrs          = s_bukrs
*              s_gsber          = s_gsber
*         EXCEPTIONS
*              e_no_items_found = 1
*              e_no_authority   = 2
*              OTHERS           = 3.
*
*    CASE sy-subrc.
*      WHEN 1.
*        MESSAGE e152 WITH ebene.       "Nothing found
*      WHEN 2.
*        MESSAGE e188 WITH wabukrs.     "No authority
*      WHEN 3.
*        MESSAGE e380.                  "No display possible
*    ENDCASE.
*
*    EXIT.
*  ENDIF.
*
**----------------------------Oben nichts gefunden -> Einzelposten an-
**----------------------------zeigen bei Sachkonten, MM/SD-Belegen; bei
**----------------------------Debitoren/Kreditoren in die FI-Infosysteme
**----------------------------springen--------------------------------*
*
**----------------------------Tagesfinanzstatus: Sachkonten Einzelposten
*  IF i039-xtfst <> space.              "X - Tagesfinanzstatus
**   Meldung falls es Zahlungsaufträge gibt, da diese die gleiche
**   Ebene haben wie die Buchungen auf das Sachkonto und damit keine
**   Möglichkeit besteht, diese separat anzuspringen
*    SELECT COUNT(*) FROM pyordh INTO lines
*           WHERE waers IN s_dispw
*           AND   zbukr =  wabukrs
**          einen Geschäftsbereich gibt es in pyordh nicht
*           AND   valut >= r_datum-low
*           AND   valut <= r_datum-high
*           AND   fdlev =  ebene
*           AND   bnkko =  wabnkko.
*
*    IF lines <> 0.
**     Neben den Einzelposten gibt es auch & passende Zahlungsaufträge
*      MESSAGE i159 WITH lines.
*    ENDIF.
*
*    PERFORM show_gl_account_fi_items
*            TABLES s_dispw s_gsber
*            USING wabnkko wabukrs ebene r_datum-low r_datum-high.
*
*  ELSE.                                "d.h. Liquiditätsvorschau
**----------------------------Debitor oder Kreditor: prüfen, ob in die
**----------------------------Einzelposten von MM/SD-Einzelposten zu--*
**----------------------------springen ist oder in die FI-Infosysteme-*
*
**----------------------------MM/SD-Einzelposten?---------------------*
*    PERFORM t036p_t036q_lesen.         "Ebenenzuordnung fuer MM/SD
*    LOOP AT i036q WHERE ebene = ebene.
*      EXIT.
*    ENDLOOP.
**----------------------------Eintrag gefunden -> MM/SD-Einzelposten
**----------------------------anzeigen--------------------------------*
*    IF sy-subrc = 0.
*      CALL FUNCTION 'CASH_FORECAST_LO_SELECT_ITEM'
*           EXPORTING
*                ebene            = ebene
*                fdgrp            = l_fdgrp
*                date_low         = r_datum-low
*                date_high        = r_datum-high
*                bukrs            = wabukrs
*           TABLES
*                s_dispw          = s_dispw
*                s_bukrs          = s_bukrs
*                s_gsber          = s_gsber
*           EXCEPTIONS
*                e_no_items_found = 1
*                e_no_authority   = 2
*                OTHERS           = 3.
*
*      CASE sy-subrc.
*        WHEN 1.
*          MESSAGE e235 WITH ebene.     "Nothing found
*        WHEN 2.
*          MESSAGE e188 WITH wabukrs.   "No authority
*        WHEN 3.
*          MESSAGE e380.                "No display possible
*      ENDCASE.
*      EXIT.
*    ELSE.
**----------------------------Eintrag nicht gefunden -> Aufruf FIS für
**----------------------------die Debitor- bzw. Kreditorposten--------*
**     PERFORM infosystem_fi USING grupp.
**     PERFORM list3.
*      PERFORM check_zdrcm USING s_bukrs-low. "fdtab-bukrs.
*
**     READ TABLE fdtab WITH KEY diskb = grupp.
*      SUBMIT zrfit21 WITH s_bukrs  IN s_bukrs
*                     WITH s_gsber  IN s_gsber
*                     WITH p_ebene  EQ ebene
*                     WITH p_grupp  EQ grupp
*                     WITH s_datum  IN r_datum
**                    WITH vondt     = r_datum-low
**                    WITH bisdt     = r_datum-high
**                    WITH azart     = azart
**                    WITH p_skalv   = p_skalv
**                    WITH p_skaln   = p_skaln
**                    WITH kwaer     = p_zwaer
**                    WITH awaer     = awaer
*                     AND RETURN.
*    ENDIF.
*  ENDIF.
endform.                    " drill_down_plan
**&---------------------------------------------------------------------
*
**&      Form  LIST3
**&---------------------------------------------------------------------
*
*FORM list3.
*  PERFORM check_zdrcm USING fdtab-bukrs.
*
*  READ TABLE fdtab WITH KEY diskb = grupp.
*  SUBMIT zrfit21 WITH s_bukrs   IN s_bukrs
*                  WITH s_gsber   IN s_gsber
*                  WITH p_ebene   = ebene
*                  WITH p_grupp   = grupp
*                  WITH vondt     = vondt
*                  WITH bisdt     = bisdt
*                  WITH azart     = azart
*                  WITH p_skalv   = p_skalv
*                  WITH p_skaln   = p_skaln
*                  WITH kwaer     = p_zwaer
*                  WITH awaer     = awaer
*                  AND RETURN.
*
*ENDFORM.                                                    " LIST3
*&---------------------------------------------------------------------*
*&      Form  CHECK_ZDRCM
*&---------------------------------------------------------------------*
form check_zdrcm using    p_fdtab_bukrs.

  tables: ztfi_drcm.

  select * from ztfi_drcm where bukrs = p_fdtab_bukrs
                     order by tstlo descending .
    if ztfi_drcm-okcod = space.
      message e437(ds) with text-010.
    endif.
    exit.
  endselect.


endform.                    " CHECK_ZDRCM
*---------------------------------------------------------------------*
*       FORM T036_LESEN                                               *
*---------------------------------------------------------------------*
form t036_lesen.
  describe table i036 lines lines.
  if lines = 0.
    select * from t036 into table i036.
  endif.
endform.
*---------------------------------------------------------------------*
*       FORM T039_LESEN                                               *
*---------------------------------------------------------------------*
form t039_lesen.
  describe table i039 lines lines.
  if lines = 0.
    select * from t039 into table i039.
  endif.
endform.
*---------------------------------------------------------------------*
*       FORM T037_LESEN                                               *
*---------------------------------------------------------------------*
form t037_lesen.
  describe table i037 lines lines.
  if lines = 0.
    select * from t037 into table i037.
  endif.
endform.
*--------------------------------------------------------------------*
* FORM SUBMIT_FF65                                                   *
*--------------------------------------------------------------------*
form submit_ff65 using grupp bukrs ebene.
  data: l_diskb like t035d-diskb.

  ranges s_valut for fdes-datum.

*--- Selektionszeitraum eingrenzen -----------------------------------
  refresh s_valut. clear s_valut.
  move: r_datum-low  to s_valut-low,
        r_datum-high to s_valut-high,
        'I'          to s_valut-sign,
        'BT'         to s_valut-option.
  append s_valut.

*----------------------------alle, die von RFTS6500 gelesen werden---*
  set parameter id 'BUK' field space.
  set parameter id 'GSB' field space.
  set parameter id 'FFX' field space.
  set parameter id 'FFC' field space.
  set parameter id 'FFY' field space.
  set parameter id 'FFG' field space.
  set parameter id 'FFE' field space.
  set parameter id 'FFA' field space.
  set parameter id 'FDW' field space.
  set parameter id 'FDT' field space.
  set parameter id 'FFI' field space.
  set parameter id 'FFK' field space.

  if i039-xtfst <> space.              "Tagesfinanzstatus
    l_diskb = grupp.                   "Defaultwert setzen

    submit rfts6500 with s_avdat ge sy-datum
*----------------------------beim Tagesfinanzstatus BK immer eindeutig
                    with s_bukrs eq bukrs
                    with s_gsber in s_gsber
                    with p_tfrec eq 'X'
                    with s_ebene eq ebene
                    with s_datum in s_valut
                    with s_diskb eq l_diskb and return.
  else.
    submit rfts6500 with s_avdat ge sy-datum
                    with s_bukrs in s_bukrs
                    with s_gsber in s_gsber
                    with p_fdrec eq 'X'
                    with s_ebene eq ebene
                    with s_datum in s_valut
                    with s_grupp eq grupp and return.
  endif.
endform.
*---------------------------------------------------------------------*
*       FORM T036P_T036Q_LESEN                                        *
*---------------------------------------------------------------------*
form t036p_t036q_lesen.
  describe table i036q lines lines.
  if lines = 0.
    select * from t036q into table i036q.
*   i036p wird nicht benötigt
*   select * from t036p into table i036p where spras = sy-langu.
  endif.
endform.
*----------------------------------------------------------------------*
*  Form SHOW_GL_ACCOUNT_FI_ITEMS
*----------------------------------------------------------------------*
*  Entspricht Aufruf von FBL3N - Anzeige der FI-Sachkonteneinzelposten
*----------------------------------------------------------------------*
form show_gl_account_fi_items
     tables u_t_dispw
            u_t_gsber
     using  u_saknr like bseg-hkont
            u_bukrs like bseg-bukrs
            u_fdlev like bseg-fdlev
            u_vondt like bsis-valut
            u_bisdt like bsis-valut.

  type-pools: rsds.
  data: l_vari_alv    type slis_vari.
  data: l_trange type rsds_trange   with header line,
        l_frange type rsds_frange_t with header line,
        l_selopt type rsds_selopt_t with header line,
        l_twhere type rsds_twhere,
        l_texpr  type rsds_texpr.

* Ranges
  ranges: r_dispw for bsis-waers,
          r_gsber for bseg-gsber,
          r_saknr for bseg-hkont,
          r_bukrs for bseg-bukrs.

  r_dispw[] = u_t_dispw[].
  r_gsber[] = u_t_gsber[].

* Report RFITEMGL verwendet Datenbank SDF. In SDF gibt es für die
* Knoten SKA1, SKB1 und BSIS freie Abgrenzungen. Zusätzlich werden vom
* Report auch die angegebenen freien Abgrenzungen auf VBSEGS und VBKPF
* angewendet, d.h. es gilt hier auch für VBSEGS-GSBER, VBSEGS-VALUT
* und VBKPF-WAERS.
* Nicht möglich für BSEG-FDLEV: es gibt zwar bei den freien Abgrenzungen
* die Möglichkeit, auf Ebene einzuschränken, aber das bezieht sich auf
* SKB1-FDLEV und das nützt hier nichts. Da aber alle FI-Buchungen auf
* einem Sachkonto die gleiche Ebene haben (nämlich die Ebene gemäß
* Sachkontostamm), kann man die Abfrage aber auch weglassen.
* Beim Füllen der nachfolgenden Tabellen ist zu beachten, daß jeder
* Knoten (z.B. BSIS) nur einen Eintrag haben darf.

* 1) freie Abgrenzungen für BSIS
* a) Währung (BSIS-WAERS)
  clear: l_selopt.  refresh: l_selopt.
  clear: l_frange.  refresh: l_frange.
  clear: l_trange.  refresh: l_trange.
  loop at r_dispw.
*   nicht l_selopt[] = r_dispw[] wegen Länge von LOW und HIGH
    l_selopt-low    = r_dispw-low.
    l_selopt-high   = r_dispw-high.
    l_selopt-sign   = r_dispw-sign.
    l_selopt-option = r_dispw-option.
    append l_selopt.
  endloop.

  l_frange-fieldname = 'WAERS'.
  l_frange-selopt_t[] = l_selopt[].
  append l_frange.
  append lines of l_frange to l_trange-frange_t[].

* b) Geschäftsbereich (BSIS-GSBER)
  clear: l_selopt.  refresh: l_selopt.
  clear: l_frange.  refresh: l_frange.
  loop at r_gsber.
*   nicht l_selopt[] = r_gsber[] wegen Länge von LOW und HIGH
    l_selopt-low    = r_gsber-low.
    l_selopt-high   = r_gsber-high.
    l_selopt-sign   = r_gsber-sign.
    l_selopt-option = r_gsber-option.
    append l_selopt.
  endloop.

  l_frange-fieldname = 'GSBER'.
  l_frange-selopt_t[] = l_selopt[].
  append l_frange.
  append lines of l_frange to l_trange-frange_t[].

* c) Valutadatum (BSIS-VALUT)
  clear: l_selopt.  refresh: l_selopt.
  clear: l_frange.  refresh: l_frange.
  l_selopt-low    = u_vondt.
  l_selopt-high   = u_bisdt.
  l_selopt-sign   = 'I'.
  l_selopt-option = 'BT'.
  append l_selopt.

  l_frange-fieldname = 'VALUT'.
  l_frange-selopt_t[] = l_selopt[].
  append l_frange.
  append lines of l_frange to l_trange-frange_t[].

  l_trange-tablename = 'BSIS'.
  append l_trange.

************************************

* (Vorlage: FB FI_PRQ_PAYMENT_RUN_FREE_SEL)
* Freie Abgrenzungen: Konvertierung Format RSDS_TRANGE ==> RSDS_TWHERE
  call function 'FREE_SELECTIONS_RANGE_2_WHERE'
       exporting
            field_ranges  = l_trange[]
       importing
            where_clauses = l_twhere[].

* Freie Abgrenzungen: Konvertierung Format RSDS_TRANGE ==> RSDS_TEXPR
  call function 'FREE_SELECTIONS_RANGE_2_EX'
       exporting
            field_ranges = l_trange[]
       importing
            expressions  = l_texpr[].

* Ranges erstellen für Submit
  r_saknr-low    = u_saknr.
  r_saknr-sign   = 'I'.
  r_saknr-option = 'EQ'.
  append r_saknr.
  r_bukrs-low    = u_bukrs.
  r_bukrs-sign   = 'I'.
  r_bukrs-option = 'EQ'.
  append r_bukrs.

* ALV Listvariante gemäß Benutzervorgaben, sofern bei den Parametern
* eine Variante gepflegt ist
  get parameter id 'FIT_ALV_GL' field l_vari_alv.
* if l_vari_alv is initial.
*   Standardvariante 1SAP nicht mitgeben, da sonst ggf. eine definierte
*   ALV-Einstiegsvariante verloren geht. Gibt es keine, wird vom
*   Report RFITEMGL die 1SAP automatisch gezogen
*   l_vari_alv = '1SAP'.   "Standardvariante
* endif.

  submit rfitemgl
    with free selections l_texpr
    with sd_saknr in r_saknr
    with sd_bukrs in r_bukrs
    with x_opsel  = 'X'
    with x_clsel  = space              "ausgeglichene Posten
    with x_aisel  = space              "alle Posten
    with pa_stida = '99991231'
    with x_norm   = 'X'                "normale Posten
    with x_merk   = space              "Merkposten
    with x_park   = 'X'                "vorerfasste Posten
    with pa_vari  = l_vari_alv
    and return.
endform.                               "show_gl_account_fi_items
*--------------------------------------------------------------------*
* FORM INFOSYSTEM_FI                                                 *
*--------------------------------------------------------------------*
* (FIS) Aufruf RFDRRANZ (für Debitoren) bzw. RFKRRANZ (für Kred.)    *
* (Beide Reports sind stichtagsbezogen. Stichtag wurde beim letzten  *
*  Lauf, als die Datenbestände aufgebaut wurden, angegeben. Beide    *
*  Reports haben keine Abgrenzungen, sondern nach dem Aufruf kann    *
*  man mit F2 jeweils weiter aufreißen)                              *
*--------------------------------------------------------------------*
form infosystem_fi using grupp.
* ermitteln, ob es die Gruppe zu Debitoren oder Kreditoren gehört
  loop at gkoarttab
       where grupp = grupp.            "ausgewählte Gruppe
    exit.
  endloop.
  if sy-subrc <> 0.
*   Zuordnung Gruppe zu Kontoart noch nicht ermittelt für diese Gruppe
    select kunnr from knb1 into knb1-kunnr up to 1 rows
                      where fdgrv = grupp.
    endselect.
    if sy-dbcnt = 0.
*     da kein Debitor mit dieser Gruppe gefunden -> Kreditorgruppe
      gkoarttab-koart = 'K'.
    else.
      gkoarttab-koart = 'D'.
    endif.
    gkoarttab-grupp = grupp.
    append gkoarttab.
  endif.

  if gkoarttab-koart = 'D'.
    submit rfdrranz and return.
  else.                                "kann nur 'K' sein
    submit rfkrranz and return.
  endif.
endform.                               "infosystem_fi

*--------------------------------------------------------------------*
* FORM ermittlung_von_bis_datum                                      *
*--------------------------------------------------------------------*
form ermittlung_von_bis_datum.
*  CLEAR: bisd1,
*         biswo,
*         vondt,
*         bisdt.
*
*  GET CURSOR FIELD cucol.
** gilt bei Anzeigeart K immer,bei D nur für erste Spalte,bei A gar
*nicht
*  vondt = mindat.
*
*  IF     cucol = 'AZTAB-DMSH1'         "Vaterbild
*  OR     cucol = 'GETAB-DMSH1'         "Sohnbild
*  OR     cucol = 'WRTAB-DMSH1'.        "Enkelbild
*    IF azart+0(1) = 'A'.
*      vondt = p_perdt.
*    ENDIF.
*    bisdt = zb-d1.
*  ELSEIF cucol = 'AZTAB-DMSH2'
*  OR     cucol = 'GETAB-DMSH2'
*  OR     cucol = 'WRTAB-DMSH2'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
**----------------------------Uebergabeparameter: vorherige Spalte----*
*      vondt = zb-d1 + 1.
*    ENDIF.
*    bisdt = zb-d2.
*  ELSEIF cucol = 'AZTAB-DMSH3'
*  OR     cucol = 'GETAB-DMSH3'
*  OR     cucol = 'WRTAB-DMSH3'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d2 + 1.
*    ENDIF.
*    bisdt = zb-d3.
*  ELSEIF cucol = 'AZTAB-DMSH4'
*  OR     cucol = 'GETAB-DMSH4'
*  OR     cucol = 'WRTAB-DMSH4'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d3 + 1.
*    ENDIF.
*    bisdt = zb-d4.
*  ELSEIF cucol = 'AZTAB-DMSH5'
*  OR     cucol = 'GETAB-DMSH5'
*  OR     cucol = 'WRTAB-DMSH5'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d4 + 1.
*    ENDIF.
*    bisdt = zb-d5.
*  ELSEIF cucol = 'AZTAB-DMSH6'
*  OR     cucol = 'GETAB-DMSH6'
*  OR     cucol = 'WRTAB-DMSH6'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d5 + 1.
*    ENDIF.
*    bisdt = zb-d6.
*  ELSEIF cucol = 'AZTAB-DMSH7'
*  OR     cucol = 'GETAB-DMSH7'
*  OR     cucol = 'WRTAB-DMSH7'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d6 + 1.
*    ENDIF.
*    bisdt = zb-d7.
*  ELSEIF cucol = 'AZTAB-DMSH8'
*  OR     cucol = 'GETAB-DMSH8'
*  OR     cucol = 'WRTAB-DMSH8'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d7 + 1.
*    ENDIF.
*    bisdt = zb-d8.
*  ELSEIF cucol = 'AZTAB-DMSH9'
*  OR     cucol = 'GETAB-DMSH9'
*  OR     cucol = 'WRTAB-DMSH9'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d8 + 1.
*    ENDIF.
*    bisdt = zb-d9.
*  ELSEIF cucol = 'AZTAB-DMSH10'
*  OR     cucol = 'GETAB-DMSH10'
*  OR     cucol = 'WRTAB-DMSH10'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d9 + 1.
*    ENDIF.
*    bisdt = zb-d10.
*  ELSEIF cucol = 'AZTAB-DMSH11'
*  OR     cucol = 'GETAB-DMSH11'
*  OR     cucol = 'WRTAB-DMSH11'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d10 + 1.
*    ENDIF.
*    bisdt = zb-d11.
*  ELSEIF cucol = 'AZTAB-DMSH12'
*  OR     cucol = 'GETAB-DMSH12'
*  OR     cucol = 'WRTAB-DMSH12'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d11 + 1.
*    ENDIF.
*    bisdt = zb-d12.
*  ELSEIF cucol = 'AZTAB-DMSH13'
*  OR     cucol = 'GETAB-DMSH13'
*  OR     cucol = 'WRTAB-DMSH13'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d12 + 1.
*    ENDIF.
*    bisdt = zb-d13.
*  ELSEIF cucol = 'AZTAB-DMSH14'
*  OR     cucol = 'GETAB-DMSH14'
*  OR     cucol = 'WRTAB-DMSH14'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d13 + 1.
*    ENDIF.
*    bisdt = zb-d14.
*  ELSEIF cucol = 'AZTAB-DMSH15'
*  OR     cucol = 'GETAB-DMSH15'
*  OR     cucol = 'WRTAB-DMSH15'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d14 + 1.
*    ENDIF.
*    bisdt = zb-d15.
*  ELSEIF cucol = 'AZTAB-DMSH16'
*  OR     cucol = 'GETAB-DMSH16'
*  OR     cucol = 'WRTAB-DMSH16'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d15 + 1.
*    ENDIF.
*    bisdt = zb-d16.
*  ELSEIF cucol = 'AZTAB-DMSH17'
*  OR     cucol = 'GETAB-DMSH17'
*  OR     cucol = 'WRTAB-DMSH17'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d16 + 1.
*    ENDIF.
*    bisdt = zb-d17.
*  ELSEIF cucol = 'AZTAB-DMSH18'
*  OR     cucol = 'GETAB-DMSH18'
*  OR     cucol = 'WRTAB-DMSH18'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d17 + 1.
*    ENDIF.
*    bisdt = zb-d18.
*  ELSEIF cucol = 'AZTAB-DMSH19'
*  OR     cucol = 'GETAB-DMSH19'
*  OR     cucol = 'WRTAB-DMSH19'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d18 + 1.
*    ENDIF.
*    bisdt = zb-d19.
*  ELSEIF cucol = 'AZTAB-DMSH20'
*  OR     cucol = 'GETAB-DMSH20'
*  OR     cucol = 'WRTAB-DMSH20'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d19 + 1.
*    ENDIF.
*    bisdt = zb-d20.
*  ELSEIF cucol = 'AZTAB-DMSH21'
*  OR     cucol = 'GETAB-DMSH21'
*  OR     cucol = 'WRTAB-DMSH21'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d20 + 1.
*    ENDIF.
*    bisdt = zb-d21.
*  ELSEIF cucol = 'AZTAB-DMSH22'
*  OR     cucol = 'GETAB-DMSH22'
*  OR     cucol = 'WRTAB-DMSH22'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d21 + 1.
*    ENDIF.
*    bisdt = zb-d22.
*  ELSEIF cucol = 'AZTAB-DMSH23'
*  OR     cucol = 'GETAB-DMSH23'
*  OR     cucol = 'WRTAB-DMSH23'.
*    IF azart+0(1) = 'D' OR azart+0(1) = 'A'.
*      vondt = zb-d22 + 1.
*    ENDIF.
*    bisdt = maxdat.
*  ELSE.
*    MESSAGE e031.  "bitte Cursor auf gültiges Betragsfeld positionieren
*  ENDIF.
endform.                               "ermittlung_von_bis_datum
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
  move value_hkont-saknr to s_hkont-low.

  clear :  value_hkont[],it_fields[].
endform.                    " search_help_hkont
