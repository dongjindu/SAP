*
* FM Budget/Actual Report
*
*  made by Andy Choi (2003.1)
*
* FM actual total : FMIT
*   RVERS, RYEAR, FIKRS, RFISTL, RFONDS, RFIPEX, TSL01
*
report yfmep line-size 170
             line-count 58
             no standard page heading.

include <icon>.

*copy program : WISP_LOESCHE_LAYOUT

tables: fmci, fmcit,    "commitment
        fmfctr, fmfctrt,  "fund center
        fmhictr,          "fund center hier
        bppe.
tables: fmep,fmsu, fmit.
tables: fkrs, fpos, tbpfm.
data: ibppe like bppe occurs 0 with header line.

*actual
data: ifmit like fmit occurs 0 with header line.


data: begin of itab occurs 0,
         key(15) type c,
         fictr   like fmfctr-fictr,  "fundcenter
         geber   like bppe-geber,    "fund
         potyp like fmci-potyp, "category
         fipos   like fmci-fipos,   "commitment
         oamt    like bppe-wtp01,   "original
         samt    like bppe-wtp01,   "supplement
         tamt    like bppe-wtp01,   "transfer
         ramt    like bppe-wtp01,   "return
         camt    like bppe-wtp01,   "current
         lamt    like bppe-wtp01,   "released
         camtc    like bppe-wtp01,   "cumulative budget
         mamt    like bppe-wtp02,   "commitment
         aamt    like bppe-wtp02,   "actual
         aamts   like bppe-wtp02,   "actual(statistical)
         pamt    like bppe-wtp02,   "payment
         damt    like bppe-wtp02,   "downpay
         aamtc   like bppe-wtp02,   "cumulative actual (prev)
         zamt    like bppe-wtp02,   "residual
         profil  like tbpfm-profil,
         mark(1) type c,
      end of itab.


data : begin of ifmfctr occurs 0,
         ctr_objnr  like fmfctr-ctr_objnr,
         fictr      like fmfctr-fictr,
         parent_obj like fmhictr-parent_obj,
       end of ifmfctr.

data : begin of ifmci occurs 0,
         fipos    like fmci-fipos,
         bezei  like fmcit-bezei,
         posit    like fmep-posit,
         potyp  like fmci-potyp,
       end of ifmci.

* Sam file Layout
data : begin of rec occurs 10,
             geber like fmps-geber,   "fund
             fistl like bpfmps-fistl, "fund center
             fipos like bpfmps-fipos, "commitment
             wert(15) type c,         "amt
        end of rec.


* Active availability control on commitment budget
data: begin of fmctl occurs 0,
         fictr     like fmfctr-fictr,
         fipos     like fmci-fipos,
         geber     like bppe-geber,
         profil    like tbpfm-profil,
      end of fmctl.

* for combobox
type-pools: vrm.
data: it_val type vrm_values,
      w_line like line of it_val.


selection-screen begin of block sb with frame title c010.
*p_buk  like fmit-rbukrs memory id BUK obligatory,
parameters :
             p_fik  like fmps-fikrs  memory id fik obligatory,
             p_gjr  like bpdy-jahr   memory id gjr obligatory,
             p_per  like bpdy-perio  memory id per obligatory.
selection-screen end of block sb.
* WRTTP: 43 - current, 46 - release
* VORGA: kbud - origin, kbn0 - supp, kbr0 - return, kbus - transfer
*        kbfr - release


selection-screen begin of block sl with frame title c020.
select-options: p_fictr for fmfctr-fictr memory id fis,
                p_fipos for fmci-fipos,   "default '600000' option GE,
                p_geber for bppe-geber  memory id fic,
                p_prof  for tbpfm-profil,  "default 'B' option NE
                p_knz   for fmci-potyp default '3'.  " item category

parameters: p_ver  like bppe-versn default '000' no-display.
* ' ' - available.    'X' - released

selection-screen end of block sl.


selection-screen begin of block s3 with frame title c030.
selection-screen begin of line.
selection-screen comment (15) c_scale.
selection-screen position 33.
parameters: p_r like rfpdo1-ts70skal  default '0'.
selection-screen comment 35(1) c_slash.
selection-screen position 37.
parameters: p_d like rfpdo1-ts70skal  default '0'.
selection-screen comment 39(1) c_slash2.
selection-screen position 41.
parameters: p_amtype(1) type c default ' '.
selection-screen end of line.
parameters: p_act(1) type c as listbox visible length 25 obligatory.

parameters: p_cumm as checkbox default 'X'.
selection-screen end of block s3.


selection-screen begin of block s4 with frame title c040.
selection-screen begin of line.
selection-screen comment (15) c_thred.
selection-screen position 33.
parameters: p_thred like raip1-prozu default 90.
selection-screen position 41.
parameters: p_thonly as checkbox default ' '.
selection-screen end of line.
selection-screen end of block s4.



parameters : p_file like rlgrap-filename default
   'c:\temp\fmrpt.xls'.

data: g_per(2)  type n.
data: g_bldat   like bpdy-bldat,
      g_subrc   like sy-subrc.

tables: t100.

* for function
data: g_func(1) type c.

* Variables
data: v_field(20) type c,
      g_history   type c,
      g_pfr       like fmto-perio,
      g_pto       like fmto-perio.
ranges: r_perio   for  fmfi-perio.

constants: gv(1) type c value '|'.
*---------------------------------------------------------------------*
* Initialization.
*---------------------------------------------------------------------*
initialization.
* for combo box
  w_line-key  = '1'.
  w_line-text = 'Sort by Fund Center'.
  append w_line to it_val.
  w_line-key  = '2'.
  w_line-text = 'Sort by Commitment Item'.
  append w_line to it_val.
  w_line-key  = '3'.
  w_line-text = 'Sort by Fund'.
  append w_line to it_val.
  p_act = '1'.

* initial screen text
  c_scale  = 'Scale/Decimal/Scr'.
  c_slash  = '/'.
  c_slash2 = '/'.
  c_thred  = 'Threshold'.
  c010 = 'Run Parameter'.
  c020 = 'Select option'.
  c030 = 'Display'.
  c040 = 'Threshold'.



*----------------------------------------------------------------------*
*   at selection screen                                                *
*----------------------------------------------------------------------*
at selection-screen output.
  call function 'VRM_SET_VALUES'
       exporting
            id     = 'P_ACT'
            values = it_val.

at selection-screen.
*---------------------------------------------------------------------
*  S T A R T - O F - S E L E C T I O N
*---------------------------------------------------------------------
start-of-selection.

  perform get_master_info.
* read database
  refresh itab.

  perform get_bbpe.
  perform get_fmit.
  perform sort_itab.

*---------------------------------------------------------------------
*  END-OF-SELECTION.
*---------------------------------------------------------------------
end-of-selection.
  perform display_data.

*----------------------------------------------------------------------*
* AT LINE SELECTION
*----------------------------------------------------------------------*
at line-selection.

  get cursor field v_field.
  case v_field.
    when 'ITAB-GEBER'.
* Drill down to master data
      set parameter id 'FIK' field p_fik.
      set parameter id 'FIC' field itab-geber.
      call transaction 'FM5S' and skip first screen.
    when 'ITAB-FICTR'.
* Drill down to master data
      set parameter id 'FIK' field p_fik.
      set parameter id 'FIS' field itab-fictr.
      call transaction 'FM2S' and skip first screen.

    when 'ITAB-FIPOS'.
* Drill down to actual summary
      if p_cumm = 'X'.
        g_pfr = '01'.
      else.
        g_pfr = p_per.
      endif.
      g_pto = p_per.
      check itab-fipos <> space.
      r_perio-sign = 'I'.
      r_perio-low = g_pfr.
      r_perio-high = g_pto.
      r_perio-option = 'BT'.
      append r_perio.

      submit rffmep1a
        with s_fictr = itab-fictr
        with s_fikrs = p_fik
        with s_fincd = itab-geber
        with s_fipos = itab-fipos
        with s_gjahr = p_gjr
        with s_perio in r_perio
      and return.

*         OAMT    like BPPE-WTP01,   "original
*         SAMT    like BPPE-WTP01,   "supplement
*         TAMT    like BPPE-WTP01,   "transfer
*         RAMT    like BPPE-WTP01,   "return

    when 'ITAB-WTP01'.
      perform display_fm_doc.


    when others.
* Drill down to actual summary
      if p_cumm = 'X'.
        g_pfr = '01'.
      else.
        g_pfr = p_per.
      endif.
      g_pto = p_per.
      check itab-fictr <> space.

*  SET PARAMETER ID 'VPE' FIELD g_pfr.
*  SET PARAMETER ID 'BPE' FIELD g_pto.
*  SET PARAMETER ID 'VPF' FIELD g_pfr.
*  SET PARAMETER ID 'BPF' FIELD g_pto.
      submit rffmto10
              with p_fyr_fr = p_gjr
              with p_fyr_to = p_gjr
              with p_per_fr = g_pfr
              with p_per_to = g_pto
*             WITH P_FYF_FR = p_gjr
*             WITH P_FYF_TO = p_gjr
*             WITH P_PEF_FR = g_pfr
*             WITH P_PEF_TO = g_pto
              with s_fictr = itab-fictr
              with s_fikrs = p_fik
              with s_fincd = itab-geber
              with s_fipos = itab-fipos
            and return.

  endcase.
************************************************************************
top-of-page.
  perform top_of_page.

************************************************************************
* TOP-OF-PAGE DURING LINE-SELECTION
************************************************************************
top-of-page during line-selection.
  perform top_of_page.

************************************************************************
* Line selection                                                       *
************************************************************************
at line-selection.
  perform pick.

************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
at pf13.
  perform data_download.

************************************************************************
***  (SHIFT+PF4) Release simulation
************************************************************************
at pf16.

************************************************************************
***  (SHIFT+PF5) Release
************************************************************************
at pf17.

  include lfmauequ.
*&---------------------------------------------------------------------*
*&      Form  get_master_info
*&---------------------------------------------------------------------*
form get_master_info.
  data: l_berec.

* commitment Item
  select * into corresponding fields of table ifmci
     from fmci
     where fikrs   =  p_fik
       and fipos   in p_fipos
       and potyp in p_knz.

* commitment item text
  loop at ifmci.
    select single bezei into ifmci-bezei
       from fmcit
       where spras = sy-langu
         and fikrs = p_fik
         and fipex = ifmci-fipos.
    modify ifmci.
  endloop.

* Fund Center
  select * into corresponding fields of table ifmfctr
     from fmfctr
     where fikrs =  p_fik
       and fictr in p_fictr.

* Fund Center Hiearchy (select end node only)
  loop at ifmfctr.
    select single * from fmhictr
       where ctr_objnr = ifmfctr-ctr_objnr.
    if fmhictr-parent_obj = space.
      delete ifmfctr.
    else.
*-- end node...
*--- check FundCenter Auth.
      select single * from fmfctr
          where fikrs = p_fik
            and fictr = ifmfctr-fictr
            and datbis >= sy-datum.

      authority-check object 'Z_FICTR'
               id 'FM_FIKRS'   field p_fik
               id 'FM_FICTR'   field fmfctr-fictr.
      if sy-subrc <> 0.
        delete ifmfctr.
      endif.
    endif.
  endloop.



* Budget Profile
  select * into corresponding fields of table fmctl
    from tbpfm
    where fikrs = p_fik
      and gjahr = p_gjr.

endform.                    " get_master_info
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
form data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  call function 'DOWNLOAD'
       exporting
            filename = p_file
            filetype = 'WK1'
       tables
            data_tab = itab.

  write:/ p_file, ' is created...'.
endform.                    " data_download
*&---------------------------------------------------------------------*
*&      Form  get_bbpe
*&---------------------------------------------------------------------*
form get_bbpe.
  data: l_amt like bppe-wtp01,
        lcamt like bppe-wtp01.

  select * from bppe
    into table ibppe
    for all entries in ifmfctr
    where objnr =  ifmfctr-ctr_objnr
      and gjahr =  p_gjr
      and versn =  p_ver
      and geber in p_geber.

  loop at ibppe.
    clear itab.

    read table ifmci  with key posit = ibppe-posit.
    check sy-subrc = 0.
    check ifmci-fipos in p_fipos.

    read table ifmfctr with key ctr_objnr = ibppe-objnr.
    check sy-subrc = 0.
    check ifmfctr-fictr in p_fictr.

    move-corresponding ibppe to itab.
    itab-fipos   = ifmci-fipos.
    itab-potyp = ifmci-potyp.
    itab-fictr   = ifmfctr-fictr.

    perform get_budget_period using itab-fictr itab-fipos itab-geber
                              changing itab-profil.
* filter profile
    check itab-profil in p_prof.

* Cumulative Sum until previous period
    perform cal_cum_budget using    'X'
                           changing lcamt.

* available budget = orgin - released
    if p_cumm = ' '.
      case p_per.  " Period
        when 1.     l_amt = ibppe-wtp01.
        when 2.     l_amt = ibppe-wtp02.
        when 3.     l_amt = ibppe-wtp03.
        when 4.     l_amt = ibppe-wtp04.
        when 5.     l_amt = ibppe-wtp05.
        when 6.     l_amt = ibppe-wtp06.
        when 7.     l_amt = ibppe-wtp07.
        when 8.     l_amt = ibppe-wtp08.
        when 9.     l_amt = ibppe-wtp09.
        when 10.    l_amt = ibppe-wtp10.
        when 11.    l_amt = ibppe-wtp11.
        when 12.    l_amt = ibppe-wtp12.
      endcase.
    else.
* Cumulative Sum by Period Control
      case itab-profil.
        when 'Y'.
          l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09
                + ibppe-wtp10 + ibppe-wtp11 + ibppe-wtp12.

        when 'H'.
          if p_per <= 6.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06.
          else.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                  + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09
                  + ibppe-wtp10 + ibppe-wtp11 + ibppe-wtp12.
          endif.
        when 'Q'.
          if p_per <= 3.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03.
          elseif p_per <= 6.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06.
          elseif p_per <= 9.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                  + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09.
          else. " p_per >= 12.
            l_amt = ibppe-wtp01 + ibppe-wtp02 + ibppe-wtp03
                  + ibppe-wtp04 + ibppe-wtp05 + ibppe-wtp06
                  + ibppe-wtp07 + ibppe-wtp08 + ibppe-wtp09
                  + ibppe-wtp10 + ibppe-wtp11 + ibppe-wtp12.
          endif.
        when others.
          l_amt = lcamt.   " use lcamt
      endcase.
    endif.
* if not controlling budget, do not use released budget
    if itab-profil na 'MQHY'.
      check ibppe-wrttp <> '46'.

      itab-camtc = lcamt.
      itab-lamt = l_amt.
    else.
* Released
      if ibppe-wrttp = '46'.
        itab-lamt  = l_amt.    "current month release
        itab-camtc  = lcamt.
      else.
        case ibppe-vorga.
          when 'KBUD'.  itab-oamt = l_amt. "origin
          when 'KBR0'.  itab-ramt = l_amt. "return
          when 'KBN0'.  itab-samt = l_amt. "supplement
          when 'KBUE'.  itab-tamt = l_amt. "receiver
          when 'KBUS'.  itab-tamt = l_amt. "sender
        endcase.
        itab-camt  = l_amt.  " current
      endif.
    endif.

    collect itab.

  endloop.
endform.                    " get_bbpe
*&---------------------------------------------------------------------*
*&      Form  get_fmit
*&---------------------------------------------------------------------*
form get_fmit.
  data: l_amt like bppe-wtp01.

  select * from fmit
    into table ifmit
    where rfistl in p_fictr
      and rfonds in p_geber
      and ryear  =  p_gjr.

  loop at ifmit.
* check auth
    read table ifmfctr with key fictr = ifmit-rfistl.
    check sy-subrc = 0.

* filter for commitment item
    check ifmit-rfipex in p_fipos.
    read table ifmci  with key fipos = ifmit-rfipex.
    check sy-subrc = 0.

    clear itab.

    itab-fipos   = ifmit-rfipex.
    itab-potyp = ifmci-potyp.
    itab-fictr   = ifmit-rfistl.
    itab-geber   = ifmit-rfonds.

    perform get_budget_period using itab-fictr itab-fipos itab-geber
                              changing itab-profil.
* filter profile
    check itab-profil in p_prof.

*   perform switch_sign.

    clear: l_amt.
    if p_cumm = ' '.
      case p_per.  " Period
        when 1.     l_amt = ifmit-tsl01.
        when 2.     l_amt = ifmit-tsl02.
        when 3.     l_amt = ifmit-tsl03.
        when 4.     l_amt = ifmit-tsl04.
        when 5.     l_amt = ifmit-tsl05.
        when 6.     l_amt = ifmit-tsl06.
        when 7.     l_amt = ifmit-tsl07.
        when 8.     l_amt = ifmit-tsl08.
        when 9.     l_amt = ifmit-tsl09.
        when 10.    l_amt = ifmit-tsl10.
        when 11.    l_amt = ifmit-tsl11.
        when 12.    l_amt = ifmit-tsl12.
      endcase.
    else.
*     if p_amtype = '9'.
      if p_per >=  1.  l_amt = l_amt + ifmit-tsl01.  endif.
      if p_per >=  2.  l_amt = l_amt + ifmit-tsl02.  endif.
      if p_per >=  3.  l_amt = l_amt + ifmit-tsl03.  endif.
      if p_per >=  4.  l_amt = l_amt + ifmit-tsl04.  endif.
      if p_per >=  5.  l_amt = l_amt + ifmit-tsl05.  endif.
      if p_per >=  6.  l_amt = l_amt + ifmit-tsl06.  endif.
      if p_per >=  7.  l_amt = l_amt + ifmit-tsl07.  endif.
      if p_per >=  8.  l_amt = l_amt + ifmit-tsl08.  endif.
      if p_per >=  9.  l_amt = l_amt + ifmit-tsl09.  endif.
      if p_per >= 10.  l_amt = l_amt + ifmit-tsl10.  endif.
      if p_per >= 11.  l_amt = l_amt + ifmit-tsl11.  endif.
      if p_per >= 12.  l_amt = l_amt + ifmit-tsl12.  endif.
*     endif.
    endif.

* 50 - p/r, 51 - p/o, 54 - invoice, 57 - payment, 58 - d/p req
* 60 -parked, 61 - downpayment
* 95 - co posting (secondary cost posting)
    if ifmit-rwrttp = '50' or ifmit-rwrttp = '51'
    or ifmit-rwrttp = '60' or ifmit-rwrttp = '61'.

      perform cumm_commitment.
      perform cumm_used.
      if ifmit-rwrttp = '61'.
        itab-damt = l_amt.   " downpayment
      endif.
      collect itab.

* Original Only for invoice
    elseif (  ifmit-rwrttp = '54'
           or ifmit-rwrttp = '66'
           or ifmit-rwrttp = '95' )
        and ifmit-rbtart = '0100'.
      perform cumm_used.

*statistical update (just information)
*      if ifmit-rwrttp = '66' and ifmit-RSTATS = 'X'.
*        itab-aamtc = l_amt.
*      else.
*      if p_cumm = 'X'.
*        itab-aamt = l_amt + itab-aamtc.  "Cumulative
*      else.
      itab-aamt = l_amt.
*      endif.

      collect itab.

* Payment
    elseif ifmit-rbtart = '0250'.  " Paid
      itab-pamt = l_amt.
      collect itab.
    elseif ifmit-rwrttp = '61'.  " down payment
      itab-damt = l_amt.
      collect itab.
    endif.

  endloop.
endform.                    " get_fmit
*&---------------------------------------------------------------------*
*&      Form  cumm_used
*&---------------------------------------------------------------------*
form cumm_used.
  clear itab-aamtc.
* for calc auto c/f budget amount
  if p_per >  1.  itab-aamtc = itab-aamtc + ifmit-tsl01.  endif.
  if p_per >  2.  itab-aamtc = itab-aamtc + ifmit-tsl02.  endif.
  if p_per >  3.  itab-aamtc = itab-aamtc + ifmit-tsl03.  endif.
  if p_per >  4.  itab-aamtc = itab-aamtc + ifmit-tsl04.  endif.
  if p_per >  5.  itab-aamtc = itab-aamtc + ifmit-tsl05.  endif.
  if p_per >  6.  itab-aamtc = itab-aamtc + ifmit-tsl06.  endif.
  if p_per >  7.  itab-aamtc = itab-aamtc + ifmit-tsl07.  endif.
  if p_per >  8.  itab-aamtc = itab-aamtc + ifmit-tsl08.  endif.
  if p_per >  9.  itab-aamtc = itab-aamtc + ifmit-tsl09.  endif.
  if p_per > 10.  itab-aamtc = itab-aamtc + ifmit-tsl10.  endif.
  if p_per > 11.  itab-aamtc = itab-aamtc + ifmit-tsl11.  endif.
  if p_per > 12.  itab-aamtc = itab-aamtc + ifmit-tsl12.  endif.
endform.                    " cumm_used
*&---------------------------------------------------------------------*
*&      Form  cumm_commitment
*&---------------------------------------------------------------------*
form cumm_commitment.
* commitment is Cumulative (include year c/f)
* C/F - Amt Type : 0350
  itab-mamt = itab-mamt + ifmit-tslvt.
  if p_per >=  1.  itab-mamt = itab-mamt + ifmit-tsl01.  endif.
  if p_per >=  2.  itab-mamt = itab-mamt + ifmit-tsl02.  endif.
  if p_per >=  3.  itab-mamt = itab-mamt + ifmit-tsl03.  endif.
  if p_per >=  4.  itab-mamt = itab-mamt + ifmit-tsl04.  endif.
  if p_per >=  5.  itab-mamt = itab-mamt + ifmit-tsl05.  endif.
  if p_per >=  6.  itab-mamt = itab-mamt + ifmit-tsl06.  endif.
  if p_per >=  7.  itab-mamt = itab-mamt + ifmit-tsl07.  endif.
  if p_per >=  8.  itab-mamt = itab-mamt + ifmit-tsl08.  endif.
  if p_per >=  9.  itab-mamt = itab-mamt + ifmit-tsl09.  endif.
  if p_per >= 10.  itab-mamt = itab-mamt + ifmit-tsl10.  endif.
  if p_per >= 11.  itab-mamt = itab-mamt + ifmit-tsl11.  endif.
  if p_per >= 12.  itab-mamt = itab-mamt + ifmit-tsl12.  endif.
endform.                    " cumm_commitment

*&---------------------------------------------------------------------*
*&      Form  fill_budget
*&---------------------------------------------------------------------*
form fill_budget.
  refresh rec.
  loop at itab.

* release - actual
    rec-wert = itab-lamt - itab-aamt.

    rec-fistl = itab-fictr. "fundcenter
    rec-fipos = itab-fipos. "commitment item
    rec-geber = itab-geber. "fund

    if rec-wert <> 0.
      append rec.
    endif.
  endloop.
endform.                    " fill_budget
*&---------------------------------------------------------------------*
*&      Form  sort_itab
*&---------------------------------------------------------------------*
form sort_itab..
* delete zero amount
  loop at itab.
    if  itab-oamt  = 0   "original
    and itab-samt  = 0   "supplement
    and itab-tamt  = 0   "transfer
    and itab-ramt  = 0   "return
    and itab-camt  = 0   "current
    and itab-lamt  = 0   "released
    and itab-mamt  = 0   "commitment
    and itab-aamt  = 0   "actual
    and itab-pamt  = 0   "payment
    and itab-damt  = 0   "downpay
    and itab-camtc = 0   "release sum
    and itab-aamtc = 0.  "actual sum
      delete itab.
    endif.
  endloop.

* fill key field
  loop at itab.
    case p_act.
      when '1'.
        itab-key(10) = itab-fictr.  itab-key+10(1) = itab-potyp.
      when '2'.
        itab-key(1) = itab-potyp. itab-key+1(10) = itab-fipos.
      when '3'.
        itab-key(10) = itab-geber.  itab-key+10(1) = itab-potyp.
    endcase.

* residual amount = cum budget - current budget - actual - commitment
    itab-zamt = itab-camtc - itab-lamt + itab-aamtc.
    modify itab.
  endloop.
  case p_act.
    when '1'. sort itab by key fipos.
    when '2'. sort itab by key fictr geber.
    when '3'. sort itab by key fipos fictr.
  endcase.


endform.                    " sort_itab
*&---------------------------------------------------------------------*
*&      Form  get_budget_period
*&---------------------------------------------------------------------*
form get_budget_period using    f_fictr f_fipos f_geber
                       changing f_profil.
  read table ifmci  with key fipos = f_fipos.
  if sy-subrc = 0.
    perform determine_profile_fs using    p_fik
                                          f_fictr
                                          ifmci-posit
                                          f_geber
                                          p_gjr
                                 changing f_profil.
  else.
    f_profil = ' '.
  endif.
endform.                    " get_budget_period
*---------------------------------------------------------------------*
*       FORM DETERMINE_PROFILE_FS                                     *
*---------------------------------------------------------------------*
form determine_profile_fs using    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          changing l_bprof.
  data: l_objnr like fmfctr-ctr_objnr.

  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  call function 'KBPA_FIFM_GET_PROFIL'
       exporting
            i_objnr         = l_objnr
            i_posit         = l_posit
            i_geber         = l_geber
            i_gjahr         = l_gjahr
       importing
            e_profil        = l_bprof
       exceptions
            no_profil_found = 01.

  if not sy-subrc is initial.
*   Profile from FundMgt Area
    call function 'FM5B_GET_PROFILE'
         exporting
              i_fikrs           = l_fikrs
              i_fincode         = l_geber
         importing
              e_profil          = l_bprof
         exceptions
              fm_area_not_found = 1
              others            = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  pick
*&---------------------------------------------------------------------*
form pick.
  check g_func = 'X'.

  if sy-cucol < 15.     " download
    perform data_download.
  elseif sy-cucol < 30. " test

  else.
    read line 1 field value p_r.
    read line 1 field value p_d.
    sy-lsind = sy-lsind - 1.
    perform display_data.

  endif.

* icon_refresh AS ICON HOTSPOT
* READ CURRENT LINE
*           FIELD VALUE report_lst-optres INTO report_lst-optres.
endform.                    " pick
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
form top_of_page.
  write:/ 'Year/Period:',
          p_gjr no-gap, '/' no-gap, p_per.
  if p_cumm = 'X'.
    write: '- Cumulative'.
  else.
    write: '- Current Month'.
  endif.
  write: ' (Round:',   p_r no-gap ,
         ', Decimal:', p_d no-gap , ') '.
*  uline.
*  g_func = 'X'.
*  write: icon_execute_object AS ICON HOTSPOT, 'Download(S_F1) ',
*         icon_execute_object AS ICON HOTSPOT, 'Test(S_F4)     ',
*         icon_execute_object AS ICON HOTSPOT, 'Run(S_F5)      ',
*         icon_refresh        AS ICON HOTSPOT, 'Refresh        '.
*
*  hide: g_func.
*  clear: g_func.
*  uline.

  format color col_heading.
  perform write_uline.
  write:/ gv no-gap, ' ' no-gap, gv no-gap, 'FndCtr' no-gap,
          gv no-gap, 'Commitment Item      ' no-gap,
          gv no-gap, 'Fund      '   no-gap,
          gv no-gap, 'P'            no-gap.

  if p_cumm = 'X'.
    write: gv no-gap, ' Cumulative ' no-gap.
  else.
    write: gv no-gap, '   Residual ' no-gap.
  endif.

  write:  gv no-gap, 'Release/Plan' no-gap,
          gv no-gap, '     Actual ' no-gap,
          gv no-gap, ' Commitment ' no-gap,
          gv no-gap, ' Var(%)'      no-gap,
          gv no-gap.

  if p_amtype = '1'.
    write:             '    Current ' no-gap,
            gv no-gap.
  elseif p_amtype = '2'.
    write:             '   Original ' no-gap,
            gv no-gap, ' Supplement ' no-gap,
            gv no-gap, '   Transfer ' no-gap,
            gv no-gap, '     Return ' no-gap,
            gv no-gap.
  elseif p_amtype = '9'.
    write: '    Payment ' no-gap, gv no-gap.
    write: 'DownPayment ' no-gap, gv no-gap.
  endif.
  perform write_uline.
  format color col_normal.

endform.                    " top_of_page
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
form display_data.

  loop at itab.
    perform display_line using 'L'.

    at end of key.
      if p_thonly = space.
        sum.
        perform display_line using 'S'.
        perform write_uline.
      endif.
    endat.

    at last.
      if p_thonly = space.
        sum.
        perform write_uline.
        perform display_line  using 'S'.
      endif.
      perform write_uline.
    endat.
  endloop.

  clear itab.
endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  display_line
*&---------------------------------------------------------------------*
form display_line using l_type.
  data: c_amt  like itab-lamt,
        ccamt  like itab-lamt,
        bbamt  like itab-lamt,
        l_pct  type i,
        l_pcts like raip1-prozu.

* current or c/f budget amount (just sum)
  if p_cumm = 'X'.
    ccamt = itab-camtc.  "cumulative
    bbamt = itab-lamt.
  else.
    if itab-profil ca 'MQHY'.
      ccamt = itab-zamt.  "residual
    else.
      clear ccamt.
    endif.
    bbamt = itab-lamt + ccamt.
  endif.

* variance %
  if bbamt <= 0.
    l_pct = 0.
  else.
    l_pct = -100 * ( itab-aamt + itab-mamt ) / bbamt.

* for protect dump...
    if l_pct  > 999.
      l_pcts = 999.
    elseif l_pct < -999.
      l_pcts = -999.
    else.
      l_pcts = l_pct.
    endif.
  endif.

  if p_thonly = 'X' and l_pct < p_thred.
    exit.
  endif.

* commitment item text
  read table ifmci  with key fipos = itab-fipos.
  if sy-subrc <> 0. clear ifmci-bezei. endif.

* switch amount sign (actual, commitment)
*    itab-aamt   = itab-aamt * -1.
*    itab-mamt   = itab-mamt * -1.

  if l_type = 'S'.
    write:/ gv no-gap, ' ' no-gap.
    format intensified on.
  else.
    write:/ gv no-gap, itab-mark as checkbox no-gap.
    format intensified off.
    hide: itab-fictr, itab-fipos, itab-geber, itab-profil.
  endif.

  format color col_key.
  write:  gv no-gap, itab-fictr(6)       no-gap,
          gv no-gap, itab-fipos(6)       no-gap,
          gv no-gap, ifmci-bezei(14)  no-gap,
          gv no-gap, itab-geber(10)      no-gap,
          gv no-gap, itab-profil(1)      no-gap.


  format color col_normal.
  write:
    gv no-gap, (12) ccamt       round p_r decimals p_d no-gap,
    gv no-gap, (12) itab-lamt   round p_r decimals p_d no-gap.

* actual
  format color col_group.
  write:
    gv no-gap, (12) itab-aamt   round p_r decimals p_d no-gap.

* commitment
  if itab-mamt = 0.
    format color col_background.
  else.
    format color col_positive.
  endif.
  write:
    gv no-gap, (12) itab-mamt   round p_r decimals p_d no-gap.

* variance %
*  bbamt = itab-lamt + ccamt.
*  if bbamt <= 0.
*    write: gv no-gap, (7) '-' right-justified no-gap, gv no-gap.
*  else.
*    l_pct = -100 * ( itab-aamt + itab-mamt ) / bbamt.

  if l_pct > p_thred.
    format color col_negative.
  else.
    format color col_background.
  endif.
  if bbamt <= 0.
    write: gv no-gap, (7) '-' right-justified no-gap, gv no-gap.
  else.
    write: gv no-gap, l_pcts decimals 0 no-gap, '%' no-gap,
           gv no-gap.
  endif.


  if p_amtype = '1'.
    format color col_background.
    write:       (12) itab-camt   round p_r decimals p_d no-gap,
      gv no-gap.

  elseif p_amtype = '2'.
    format color col_background.
    write:
                 (12) itab-oamt   round p_r decimals p_d no-gap,
      gv no-gap, (12) itab-samt   round p_r decimals p_d no-gap,
      gv no-gap, (12) itab-tamt   round p_r decimals p_d no-gap,
      gv no-gap, (12) itab-ramt   round p_r decimals p_d no-gap,
      gv no-gap.

* payment
  elseif p_amtype = '9'.
    format color col_background.
    write: (12) itab-pamt   round p_r decimals p_d no-gap,
           gv no-gap, (12) itab-damt   round p_r decimals p_d no-gap,
           gv no-gap.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
form read_data.
*  DATA: $IX    LIKE SY-TABIX VALUE 3,             "..Data Display Line
*        $INDEX LIKE SY-INDEX.                     "..Table Index
*
*  CLEAR:WK_SUTAK[],WK_SUTAK.
*  DESCRIBE TABLE itab LINES SY-TFILL.
*
*  DO SY-TFILL TIMES.
*    ADD 1 TO: $IX, $INDEX.
*
*    CLEAR:itab.
*    READ LINE $IX  FIELD VALUE itab-MARK.
*    CHECK SY-SUBRC EQ 0.
*    CHECK itab-MARK EQ 'X'.
*
*    READ TABLE itab INDEX $INDEX.
*    CHECK SY-SUBRC EQ 0 AND itab-Z_CHKFLAG EQ SPACE. "..????
*
*    MOVE-CORRESPONDING itab TO WK_SUTAK.
*    APPEND WK_SUTAK. CLEAR:WK_SUTAK.
*  ENDDO.
*
*  IF WK_SUTAK[] IS INITIAL.
*    MESSAGE E006.
*  ENDIF.
endform.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
form popup_to_confirm changing fp_answer.
  data: l_defaultoption, l_textline1(70),  l_textline2(70).

  clear fp_answer.
  l_defaultoption = 'N'.
  case sy-ucomm.
    when 'EXEC'.
      l_textline1     = text-002.
      l_textline2     = text-003.
  endcase.

  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
            defaultoption = l_defaultoption
            textline1     = l_textline1
            textline2     = l_textline2
            titel         = sy-title
       importing
            answer        = fp_answer.

endform.                    " POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      Form  EXEC_DATA
*&---------------------------------------------------------------------*
form exec_data.
  data: answer.

  perform read_data.
  perform popup_to_confirm changing answer.
  check answer eq 'J'.

*  PERFORM EXEC_BDC.

*  PERFORM REFRESH_DATA.
endform.                    " EXEC_DATA

*    WHEN 'EXEC'. PERFORM EXEC_DATA.          "..??
*    WHEN 'SALL'. PERFORM CHECK_MARK_FIELD USING 'X'.
*    WHEN 'DALL'. PERFORM CHECK_MARK_FIELD USING ' '.
*&---------------------------------------------------------------------*
*&      Form  check_mark_field
*&---------------------------------------------------------------------*
form check_mark_field using    p_mark.
  data: $ix like sy-tabix.

  loop at itab.
    $ix = sy-tabix.
    itab-mark = p_mark.
    modify itab index $ix.
  endloop.

  sy-lsind = sy-lsind - 1.
  perform display_data.
endform.                    " check_mark_field
*&---------------------------------------------------------------------*
*&      Form  write_uline
*&---------------------------------------------------------------------*
form write_uline.
  case p_amtype.
    when '1'.
      write:/(118) sy-uline.
    when '2'.
      write:/(157) sy-uline.
    when '9'.
      write:/(131) sy-uline.
    when others.
      write:/(105) sy-uline.
  endcase.
endform.                    " write_uline
*&---------------------------------------------------------------------*
*&      Form  switch_sign
*&---------------------------------------------------------------------*
form switch_sign.
  ifmit-tslvt = -1 * ifmit-tslvt.
  ifmit-tsl01 = -1 * ifmit-tsl01.
  ifmit-tsl02 = -1 * ifmit-tsl02.
  ifmit-tsl03 = -1 * ifmit-tsl03.
  ifmit-tsl04 = -1 * ifmit-tsl04.
  ifmit-tsl05 = -1 * ifmit-tsl05.
  ifmit-tsl06 = -1 * ifmit-tsl06.
  ifmit-tsl07 = -1 * ifmit-tsl07.
  ifmit-tsl08 = -1 * ifmit-tsl08.
  ifmit-tsl09 = -1 * ifmit-tsl09.
  ifmit-tsl10 = -1 * ifmit-tsl10.
  ifmit-tsl11 = -1 * ifmit-tsl11.
  ifmit-tsl12 = -1 * ifmit-tsl12.
endform.                    " switch_sign
*&---------------------------------------------------------------------*
*&      Form  cal_cum_budget
*&---------------------------------------------------------------------*
form cal_cum_budget using    pl_current
                    changing pl_amt.

  data: lcamt like bppe-wtp01.

  clear lcamt.
  if pl_current = 'X'.
    if p_per >=  1. lcamt = lcamt + ibppe-wtp01.  endif.
    if p_per >=  2. lcamt = lcamt + ibppe-wtp02.  endif.
    if p_per >=  3. lcamt = lcamt + ibppe-wtp03.  endif.
    if p_per >=  4. lcamt = lcamt + ibppe-wtp04.  endif.
    if p_per >=  5. lcamt = lcamt + ibppe-wtp05.  endif.
    if p_per >=  6. lcamt = lcamt + ibppe-wtp06.  endif.
    if p_per >=  7. lcamt = lcamt + ibppe-wtp07.  endif.
    if p_per >=  8. lcamt = lcamt + ibppe-wtp08.  endif.
    if p_per >=  9. lcamt = lcamt + ibppe-wtp09.  endif.
    if p_per >= 10. lcamt = lcamt + ibppe-wtp10.  endif.
    if p_per >= 11. lcamt = lcamt + ibppe-wtp11.  endif.
    if p_per >= 12. lcamt = lcamt + ibppe-wtp12.  endif.
  else.
    if p_per >  1. lcamt = lcamt + ibppe-wtp01.  endif.
    if p_per >  2. lcamt = lcamt + ibppe-wtp02.  endif.
    if p_per >  3. lcamt = lcamt + ibppe-wtp03.  endif.
    if p_per >  4. lcamt = lcamt + ibppe-wtp04.  endif.
    if p_per >  5. lcamt = lcamt + ibppe-wtp05.  endif.
    if p_per >  6. lcamt = lcamt + ibppe-wtp06.  endif.
    if p_per >  7. lcamt = lcamt + ibppe-wtp07.  endif.
    if p_per >  8. lcamt = lcamt + ibppe-wtp08.  endif.
    if p_per >  9. lcamt = lcamt + ibppe-wtp09.  endif.
    if p_per > 10. lcamt = lcamt + ibppe-wtp10.  endif.
    if p_per > 11. lcamt = lcamt + ibppe-wtp11.  endif.
  endif.

  pl_amt = lcamt.
endform.                    " cal_cum_budget
*&---------------------------------------------------------------------*
*&      Form  display_fm_doc
*&---------------------------------------------------------------------*
form display_fm_doc.

*  SELECT l~belnr l~buzei l~wtjhr l~objnr l~posit l~geber
*         h~sgtext l~vorga
*                     INTO TABLE it_bpej
*                     FROM bpej as l inner join bpbk as h
*                       ON l~belnr  = h~belnr
*                     WHERE l~gjahr = p_gjahr
*                       and h~bldat in p_datum
*                       and l~WRTTP = '43'      "no budget release
*                       AND l~posit > 'FP000001'
*                       AND l~vorga IN ('KBUD', 'KBN0', 'KBR0',
*                                       'KBUE', 'KBUS').

endform.                    " display_fm_doc
