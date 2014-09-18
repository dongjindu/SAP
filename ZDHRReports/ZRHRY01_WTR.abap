*&---------------------------------------------------------------------*
* Program Name      : ZRHRY01_WTR                                      *
* Author            : Ho-Joong, Hwang                                  *
* Creation Date     : 2003.11.21                                       *
* Specifications By : Ho-Joong, Hwang                                  *
* Pattern           : Report 1-1                                       *
* Development Request No : 4.6C UD1K902999                             *
* Addl Documentation:                                                  *
* Description       : Wage Type Report                                 *
*                                                                      *
* Modification Logs                                                    *
* Date       Developer    RequestNo    Description                     *
* 5/10012        t-code is deleted by APM Monitoring                   *
*&---------------------------------------------------------------------*
REPORT ZRHRY01_WTR MESSAGE-ID ZMHR
*                  LINE-COUNT 65(1)
*                  LINE-SIZE 132
                   LINE-COUNT 80
                   LINE-SIZE 400
                   NO STANDARD PAGE HEADING.
************************************************************************
*                          DATA SOURCES                                *
************************************************************************

tables: zthr_wtmng,           " Wage Type Report format management
        pa0001,
        t512w,                " Wage Type Valuation
        t512t,                " Wage Type Texts
        pernr,                " Standard Selections for HR Master Data
        pcl2.                 " HR Cluster 2

infotypes: 0001, 0002.

field-symbols: <fs>    like pc207-betrg,
               <ca>    like pc207-betrg,
               <pa>    like pc207-betrg.
************************************************************************
*                         DATA INCLUDES                                *
************************************************************************

include zthr_wtmng_forms.     " Include subroutines for parameter check
include ole2incl.             " Include OLE for excel download
include <CTLDEF>.
include OFFICEINTEGRATIONINCLUDE.
************************************************************************
*                           VARIABLES                                  *
************************************************************************

*... internal tables
data: begin of it_form1 occurs 0,           " format 1 - data collect
      zcoln    like zthr_wtmng-zcoln,       " column number
      lgart    like t512w-lgart,            " wage type
      lgtxt    like zthr_wtmng-lgtxt.       " wage type text
data: end of it_form1.

data: it_form2 like it_form1 occurs 0 with header line,
      it_form3 like it_form1 occurs 0 with header line,
      it_form4 like it_form1 occurs 0 with header line,
      it_formt like it_form1 occurs 0 with header line.

data: begin of it_writ1 occurs 0,
      pernr    like pernr-pernr,            " personnel number
      perid    like pa0002-perid,           " ID number
      nchmc    like pa0002-nchmc,           " Last name
      vnamc    like pa0002-vnamc,           " first name

      cur01    like pc207-betrg,
      cur02    like pc207-betrg,
      cur03    like pc207-betrg,
      cur04    like pc207-betrg,
      cur05    like pc207-betrg,
      cur06    like pc207-betrg,
      cur07    like pc207-betrg,
      cur08    like pc207-betrg,
      cur09    like pc207-betrg,
      cur10    like pc207-betrg,
      cur11    like pc207-betrg,
      cur12    like pc207-betrg,
      cur13    like pc207-betrg,
      cur14    like pc207-betrg,
      cur15    like pc207-betrg,

      pre01    like pc207-betrg,
      pre02    like pc207-betrg,
      pre03    like pc207-betrg,
      pre04    like pc207-betrg,
      pre05    like pc207-betrg,
      pre06    like pc207-betrg,
      pre07    like pc207-betrg,
      pre08    like pc207-betrg,
      pre09    like pc207-betrg,
      pre10    like pc207-betrg,
      pre11    like pc207-betrg,
      pre12    like pc207-betrg,
      pre13    like pc207-betrg,
      pre14    like pc207-betrg,
      pre15    like pc207-betrg,

      waerk    like pc207-amt_curr,
      diffa    like pc207-betrg.
data: end of it_writ1.

data: it_writ2 like it_writ1 occurs 0 with header line,
      it_writ3 like it_writ1 occurs 0 with header line,
      it_writ4 like it_writ1 occurs 0 with header line.

data: begin of cd-key,
      pernr like p0001-pernr,               "key to cluster directory
      end of cd-key.

data: begin of rgdir occurs 100.            " Cluster Directory
      include structure pc261.
data: end of rgdir.

data: begin of rx-key.
      include structure pc200.              " Payroll Results Key
data: end of rx-key.

data: begin of rt occurs 150.               " Payroll Results
      include structure pc207.
data: end of rt.
*... test
data: begin of tax occurs 0.
      include structure pc22t.
data: end of tax.

data: begin of versc .
      include structure pc202 .
data: end of versc.

data: begin of perm.
      include structure pc22r.
data: end of perm.
*... test end
DATA: begin of tmp occurs 10,
      num like rgdir-seqnr,
      flag.
data: end of tmp.

data: begin of cd-version.
      include structure pc201.
DATA: molga  like t001p-molga,           "country identifier
      end of cd-version.

data: begin of okr-version.
      include structure pc201.
data: end of okr-version.

*... variants
data: w_permo       like t549a-permo,   " Period parameters
      w_abkrt       like t549t-atext,   " Payroll Area Text
      w_index(2)    type n,             " table index
      w_fname(20),                      " field name
      w_abkrc(4),                       " payroll area - current
      w_abkrp(4),                       " payroll area - previous
      w_width       type i,             " line size
      w_cwdth       type i,             " column size
      w_tabno       type i,             " internal table number
      w_count(2)    type n.             " field count

data: w_title(100),                     " header title
      w_titl1(20),                      " current column title 1
      w_titl2(20),                      " current column title 2
      w_titl3(20),                      " current column title 3
      w_titl4(20),                      " current column title 4

      w_tipl1(20),                      " previous column title 1
      w_tipl2(20),                      " preivous column title 2
      w_tipl3(20),                      " previous column title 3
      w_tipl4(20).                      " current  column title 4

data: cd-next_seq   type i,             "Next available seq number
      cd-last_pay   type d,             "Last payroll run date
      w_fpper       like rgdir-fpper,
      w_emcnt       type i,
      w_tfill       like sy-tfill,
      w_pagno       like sy-pagno,
      w_newfl.

data excel_template(256) type c.
data driver like sy-repid.

*... excel download
data: file_name like rlgrap-filename,
      shet_name like rlgrap-filename.

data: begin of it_title occurs 0,
      htext(30).
data: end of it_title.

data: excel   type ole2_object,
      SHEET   type ole2_object,
      book    type ole2_object,
      cell    type ole2_object.

data: row     type i,
      column  type i,
      cols    type i,
      perc    type i.

*... read file from web repository
data: doc_table like w3mime occurs 0,
      doc_size type i,
      doc_type(80),         " value SOI_DOCTYPE_WORD97_DOCUMENT,
      doc_format(80),
      object_id(20).

data: gv_fname(8)  type  c.

DATA: FACTORY TYPE REF TO I_OI_DOCUMENT_FACTORY,
      DOCUMENT TYPE REF TO I_OI_DOCUMENT_PROXY,
      RETCODE TYPE T_OI_RET_STRING.

DATA: LINK_SERVER TYPE REF TO I_OI_LINK_SERVER.
DATA: IS_CLOSED TYPE I.

************************************************************************
*                             MACRO                                    *
************************************************************************

define read_tab.
  clear &1. refresh &1.
  clear zthr_wtmng.
  select lgart lgtxt zcoln
    into (zthr_wtmng-lgart, zthr_wtmng-lgtxt, zthr_wtmng-zcoln)
    from zthr_wtmng where zform = &2
                      and lgart in s_lgart
                      and zmark = space.
    &1-lgart = zthr_wtmng-lgart.
    &1-lgtxt = zthr_wtmng-lgtxt.
    &1-zcoln = zthr_wtmng-zcoln.
    append &1. clear &1.
  endselect.
end-of-definition.

define collect_current.
  w_index = 0.
  sort &1 by zcoln.
  loop at &1.
    &2-pernr = pernr-pernr.
    &2-perid = p0002-perid.
    &2-nchmc = p0002-nchmc.
    &2-vnamc = p0002-vnamc.
    &2-waerk = 'USD'.

    at new zcoln.
      w_index = w_index + 1.
    endat.

*   w_index = sy-tabix.
    clear rt.
    read table rt with key lgart = &1-lgart.
    concatenate '&2' '-' '&3' w_index into w_fname.
    assign (w_fname) to <fs>.
    <fs> = rt-betrg.
    collect &2. clear &2.
  endloop.
end-of-definition.

define get_diffa.
  describe table &1 lines sy-tfill.
  w_index = sy-tfill.

  loop at &2.
    concatenate '&2' '-cur' w_index into w_fname.
    assign (w_fname) to <ca>.
    concatenate '&2' '-pre' w_index into w_fname.
    assign (w_fname) to <pa>.
    &2-diffa = <ca> - <pa>.
    modify &2. clear &2.
  endloop.
end-of-definition.

define write_heading.
  clear w_tfill.
* sort &1 by zcoln.
  loop at &1.
    at new zcoln.
      w_tfill = w_tfill + 1.
    endat.
  endloop.
* describe table &1 lines sy-tfill.
  if p_compp = 'X'.
    w_width = 44 + ( ( w_tfill * 2 ) + 1 ) * 10.
    if &4 = 4.
      w_width = w_width - 10.
    endif.
  else.
    w_width = 44 + ( w_tfill * 10 ).
  endif.
  w_cwdth = ( w_tfill * 10 ) - 1.

* if w_width > 80 and w_width < 130.
*   new-page line-size 132.
* elseif w_width > 130 and w_width < 200.
*   new-page line-size 200.
* elseif w_width > 200.
*   new-page line-size 275.
* endif.

  write: /2 w_title left-justified.
  new-line.  uline at (w_width).
  write: / sy-vline no-gap.
  write: (6) ' ' no-gap, sy-vline no-gap.
  write: (9) ' ' no-gap, sy-vline no-gap.
  write: (12) ' ' no-gap, sy-vline no-gap.
  write: (12) ' ' no-gap, sy-vline no-gap.
  write: at (w_cwdth) &2 centered no-gap, sy-vline no-gap.
  if p_compp = 'X'.
    write: at (w_cwdth) &3 centered no-gap, sy-vline no-gap.
    if &4 < 4.
      write: (9) ' ' no-gap, sy-vline no-gap.
    endif.
  endif.

  write: / sy-vline no-gap.
  write: (6) ' ' no-gap, sy-vline no-gap.
  write: (9) ' ' no-gap, sy-vline no-gap.
  write: (12) ' ' no-gap, sy-vline no-gap.
  write: (12) ' ' no-gap, sy-vline no-gap.
  uline at 45(w_cwdth) no-gap.
  write: sy-vline no-gap.
  if p_compp = 'X'.
    uline at (w_cwdth) no-gap.
    write: sy-vline no-gap.
    if &4 < 4.
      write: (9) ' ' no-gap, sy-vline.
    endif.
  endif.

  write: / sy-vline no-gap.
  write: (6) 'SAP NO' centered no-gap, sy-vline no-gap.
  write: (9) 'SSNO' centered no-gap, sy-vline no-gap.
  write: (12) 'Last' centered no-gap, sy-vline no-gap.
  write: (12) 'First' centered no-gap, sy-vline no-gap.
  clear w_newfl.
  loop at &1.
    at new zcoln.
      w_newfl = 'X'.
    endat.
    if w_newfl = 'X'.
      write: (9) &1-lgtxt+(9) no-gap, sy-vline no-gap.
    endif.
    clear w_newfl.
  endloop.
*
  if p_compp = 'X'.
    clear w_newfl.
    loop at &1.
      at new zcoln.
        w_newfl = 'X'.
      endat.
      if w_newfl = 'X'.
        write: (9) &1-lgtxt+(9) no-gap, sy-vline no-gap.
      endif.
      clear w_newfl.
    endloop.
    if &4 < 4.
      write: (9) 'Diff.' centered no-gap, sy-vline no-gap.
    endif.
  endif.
*
  write: / sy-vline no-gap.
  write: (6) ' ' no-gap, sy-vline no-gap.
  write: (9) ' ' no-gap, sy-vline no-gap.
  write: (12) 'Name' centered no-gap, sy-vline no-gap.
  write: (12) 'Name' centered no-gap, sy-vline no-gap.
  clear w_newfl.
  loop at &1.
    at new zcoln.
      w_newfl = 'X'.
    endat.
    if w_newfl = 'X'.
      write: (9) &1-lgtxt+9(9) no-gap, sy-vline no-gap.
    endif.
    clear w_newfl.
  endloop.
  if p_compp = 'X'.
    clear w_newfl.
    loop at &1.
      at new zcoln.
        w_newfl = 'X'.
      endat.
      if w_newfl = 'X'.
        write: (9) &1-lgtxt+9(9) no-gap, sy-vline no-gap.
      endif.
      clear w_newfl.
    endloop.
    if &4 < 4.
      write: (9) ' ' no-gap, sy-vline no-gap.
    endif.
  endif.

  new-line. uline at (w_width).
end-of-definition.

define write_body.
  clear w_index.
  loop at &1.
    at new zcoln.
      w_index = w_index + 1.
    endat.
  endloop.
* describe table &1 lines sy-tfill.
* w_index = sy-tfill.

  w_pagno = 1.
  loop at &2.
    if sy-linno = 79.
      new-line.
      uline at (w_width).
*   elseif sy-linno = 80.
*     if &3 = 2.
*       new-line. uline at (w_width).
*     else.
*       uline at (w_width).
*     endif.
      case w_tabno.
        when 1. write_heading it_form1 w_titl1 w_tipl1 w_tabno.
        when 2. write_heading it_form2 w_titl2 w_tipl2 w_tabno.
        when 3. write_heading it_form3 w_titl3 w_tipl3 w_tabno.
        when 4. write_heading it_form4 w_titl4 w_tipl4 w_tabno.
      endcase.
    endif.
    write: / sy-vline no-gap.
    write: (6) &2-pernr using edit mask '==ALPHA' no-gap,
               sy-vline no-gap.
    write: (9) &2-perid no-gap, sy-vline no-gap.
    write: (12) &2-nchmc no-gap, sy-vline no-gap.
    write: (12) &2-vnamc no-gap, sy-vline no-gap.
*   write: (12) sy-LINNO no-gap, sy-vline no-gap.

    w_count = 1.
    do w_index times.
      concatenate '&2' '-cur' w_count into w_fname.
      assign (w_fname) to <fs>.
      write: (9) <fs> currency &2-waerk no-gap, sy-vline no-gap.
      w_count = w_count + 1.
    enddo.

    if p_compp = 'X'.
      w_count = 1.
      do w_index times.
        concatenate '&2' '-pre' w_count into w_fname.
        assign (w_fname) to <fs>.
        write: (9) <fs> currency &2-waerk no-gap, sy-vline no-gap.
        w_count = w_count + 1.
      enddo.
      if &3 < 4.
        write: (9) &2-diffa currency &2-waerk no-gap, sy-vline no-gap.
      endif.
    endif.

    w_pagno = sy-pagno.
  endloop.
end-of-definition.

define make_title.
  loop at &1.
    it_title-htext = &1-lgtxt.
    append it_title. clear it_title.
  endloop.
end-of-definition.

************************************************************************
*                           PARAMETERS                                 *
************************************************************************

*... frame : payroll period (comparison, payroll area, payroll period)
selection-screen begin of block frm1 with frame title text-f01.
selection-screen begin of line.                  " comparison
selection-screen comment 33(20) text-002.
selection-screen position 58.
parameters: p_compp   type h99cwtr-compper as checkbox default space.
selection-screen comment 60(20) text-003 for field p_compp.
selection-screen end of line.

selection-screen begin of line.                  " payroll area
selection-screen comment 1(31) text-004 for field p_abkrc.
parameters: p_abkrc  like t569v-abkrs obligatory.
selection-screen position 58.
parameters: p_abkrr  like t569v-abkrs.
selection-screen end of line.

selection-screen begin of line.                  " payroll period
selection-screen comment 1(31) text-001 for field p_abrpc.
parameters: p_abrpc  like t549q-pabrp,
            p_abrjc  like t549q-pabrj.
selection-screen position 58.
parameters: p_abrpr  like t549q-pabrp,
            p_abrjr  like t549q-pabrj.
selection-screen end of line.
selection-screen begin of line.
selection-screen position 33.
parameters: p_begca  like t549q-begda modif id pe4.
selection-screen comment (1) text-005 for field p_endca  modif id pe4.
parameters: p_endca  like t549q-endda modif id pe4.
selection-screen position 58.
parameters: p_begre  like t549q-begda modif id pe5.
selection-screen comment (1) text-005 for field p_endre  modif id pe5.
parameters: p_endre  like t549q-endda modif id pe5.
selection-screen end of line.
selection-screen end of block frm1.

*... frame : other selections - wage type
selection-screen begin of block field with frame title text-f02.
select-options: s_lgart for t512w-lgart.
selection-screen end of block field.

************************************************************************
*                        AT SELECTION-SCREEN                           *
************************************************************************

at selection-screen.
*...  initialization payroll period
  perform get_payroll_period using p_abkrc
                          changing w_permo p_begca p_endca
                                   w_abkrt p_abrpc p_abrjc.
  if p_compp = 'X'.
    perform check_comparison_parm using p_abkrc p_abrpc p_abrjc
                               changing p_abkrr p_abrpr p_abrjr.
    perform get_payroll_period using p_abkrr
                            changing w_permo p_begre p_endre
                                     w_abkrt p_abrpr p_abrjr.
  endif.

*... check payroll period
  if p_abkrr ne space.
    if p_abkrc <> p_abkrr.
      message e002.
    endif.
  endif.

  if pnpabkrs-low ne space.
    if p_abkrc <> pnpabkrs-low.
      message e002.
    endif.
  endif.

*... screen modify
at selection-screen output.
  loop at screen.
    if screen-group1 = 'PE4' or screen-group1 = 'PE5'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.

*... end page
*nd-of-page.
* uline at (w_width).

************************************************************************
*                         START-OF-SELECTION                           *
************************************************************************

start-of-selection.
  set pf-status 'PS1000'.
  perform get_assigned_wage_type.
  perform make_header_title.

************************************************************************
*                             GET LOOP                                 *
************************************************************************

get pernr.
  perform get_seqno.
  loop at tmp.
    at first.
      rp_provide_from_last p0002 space p_begca p_endca.
    endat.
    perform get_results_table.
    collect_current it_form1 it_writ1 cur.
    collect_current it_form2 it_writ2 cur.
    collect_current it_form3 it_writ3 cur.
    collect_current it_form4 it_writ4 cur.
  endloop.
*
  if p_compp = 'X'.
    perform get_previous_data.
    loop at tmp.
      perform get_results_table.
      collect_current it_form1 it_writ1 pre.
      collect_current it_form2 it_writ2 pre.
      collect_current it_form3 it_writ3 pre.
      collect_current it_form4 it_writ4 pre.
    endloop.
  endif.

************************************************************************
*                         END-OF-SELECTION                             *
************************************************************************

end-of-selection.
  if p_compp = 'X'.
    perform calc_difference_amount.
  endif.
*
  perform write_body.

************************************************************************
*                          USER COMMNAD                                *
************************************************************************

at user-command.
  case sy-ucomm.
    when 'EXCL'.
      perform excel_download.
*      perform get_filename.
*      check file_name ne space.
*      w_tabno = 1.
*      do 4 times.
*        perform make_title using w_tabno.
*        perform download_to_excel using w_tabno.
*        w_tabno = w_tabno + 1.
*      enddo.
    when 'ASOT'.
      clear: gv_fname.
      get cursor field w_fname.
      gv_fname = w_fname(8).
*      case w_fname+(8).
      case gv_fname.
        when 'IT_WRIT1'. perform ascending_sort_itab1.
        when 'IT_WRIT2'. perform ascending_sort_itab2.
        when 'IT_WRIT3'. perform ascending_sort_itab3.
        when 'IT_WRIT4'. perform ascending_sort_itab4.
      endcase.
      perform write_body.
    when 'DSOT'.
      clear: gv_fname.
      get cursor field w_fname.
      gv_fname = w_fname(8).
*      case w_fname+(8).
      case gv_fname.
        when 'IT_WRIT1'. perform descending_sort_itab1.
        when 'IT_WRIT2'. perform descending_sort_itab2.
        when 'IT_WRIT3'. perform descending_sort_itab3.
        when 'IT_WRIT4'. perform descending_sort_itab4.
      endcase.
      perform write_body.
  endcase.

************************************************************************
*                            SUBROUTINES                               *
************************************************************************
*&---------------------------------------------------------------------*
*&      Form  get_assigned_wage_type
*&---------------------------------------------------------------------*
FORM get_assigned_wage_type.
* if p_abkrc = '13'.
*   it_form1-lgart = '0007'.
*   select single lgtxt into t512t-lgtxt
*     from t512t where sprsl = sy-langu
*                  and molga = '10'
*                  and lgart = '0007'.
*   it_form1-lgtxt = t512t-lgtxt.
*   modify it_form1 transporting lgart lgtxt where lgart = '0003'.
* endif.
*
* if p_abkrc = '11' and pnppersk-low = 'U1'.
*   it_form1-lgart = '0001'.
*   select single lgtxt into t512t-lgtxt
*     from t512t where sprsl = sy-langu
*                  and molga = '10'
*                  and lgart = '0001'.
*   it_form1-lgtxt = t512t-lgtxt.
*   modify it_form1 transporting lgart lgtxt where lgart = '0003'.
* endif.
*... modified on 2004.02.03
  clear pa0001.
  select count( distinct pernr ) into w_emcnt
    from pa0001 where endda = '99991231'
                  and persg in ('1', '9').
*
  if w_emcnt >= 500.
*... health insurance
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = ' '
                  WHERE ZFORM = '3'
                    AND ZCOLN = '2'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = 'X'
                  WHERE ZFORM = '3'
                    AND ZCOLN = '1'.
*... life insurance
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = ' '
                  WHERE ZFORM = '3'
                    AND ZCOLN = '4'
                    AND ZPRNT = SPACE.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = 'X'
                  WHERE ZFORM = '3'
                    AND ZCOLN = '3'.
  else.
*... health insurance
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = 'X'
                  WHERE ZFORM = '3'
                    AND ZCOLN = '2'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = ' '
                  WHERE ZFORM = '3'
                    AND ZCOLN = '1'.
*... life insurance
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = 'X'
                  WHERE ZFORM = '3'
                    AND ZCOLN = '4'.
      CLEAR ZTHR_WTMNG.
      UPDATE ZTHR_WTMNG SET ZMARK = ' '
                  WHERE ZFORM = '3'
                    AND ZCOLN = '3'
                    AND ZPRNT = SPACE.
  endif.
*
  read_tab it_form1 1.
  read_tab it_form2 2.
  read_tab it_form3 3.
  read_tab it_form4 4.
*
  clear it_formt. refresh it_formt.
  it_formt[] = it_form3[].
  loop at it_formt.
    clear zthr_wtmng.
    select lgart lgtxt zcoln
      into (zthr_wtmng-lgart, zthr_wtmng-lgtxt, zthr_wtmng-zcoln)
      from zthr_wtmng where zform = 3
                        and zcoln = it_formt-zcoln
                        and zmark = 'X'
                        and zprnt = it_formt-lgart.
      it_form3-lgart = zthr_wtmng-lgart.
      it_form3-lgtxt = zthr_wtmng-lgtxt.
      it_form3-zcoln = zthr_wtmng-zcoln.
      append it_form3. clear it_form3.
    endselect.
*   if sy-subrc = 0.
*     delete it_form3 where lgart = it_formt-lgart
*                       and lgtxt = it_formt-lgtxt
*                       and zcoln = it_formt-zcoln.
*   endif.
  endloop.
*
  clear: it_writ1, it_writ2, it_writ3, it_writ4.
  refresh: it_writ1, it_writ2, it_writ3, it_writ4.
ENDFORM.                    " get_assigned_wage_type
*&---------------------------------------------------------------------*
*&      Form  get_seqno
*&---------------------------------------------------------------------*
FORM get_seqno.
  concatenate p_abrjc p_abrpc into w_fpper.
*
  clear: rgdir, tmp.
  refresh: rgdir, tmp.
*
  cd-key = pernr-pernr.
  import cd-version cd-next_seq cd-last_pay rgdir
    from database pcl2(CU) client sy-mandt id cd-key.
*
  if sy-subrc = 0.
    loop at rgdir where abkrs = p_abkrc
                    and fpper = w_fpper
                    and fpbeg = p_begca
                    and fpend = p_endca
                    and srtza = 'A'.
      tmp-num = rgdir-seqnr.
      append tmp. clear tmp.
    endloop.
  else.
    reject.
  endif.
ENDFORM.                    " get_seqno
*&---------------------------------------------------------------------*
*&      Form  get_results_table
*&---------------------------------------------------------------------*
FORM get_results_table.
  clear rt. refresh rt.
  clear: tax, tax[], versc, perm.
*
  rx-key-pernr = pernr-pernr.
  rx-key-seqno = tmp-num.
*
  import kr-version to okr-version rt tax versc perm
    from database pcl2(RU) id rx-key.
ENDFORM.                    " get_results_table
*&---------------------------------------------------------------------*
*&      Form  get_previous_data
*&---------------------------------------------------------------------*
FORM get_previous_data.
  concatenate p_abrjr p_abrpr into w_fpper.
*
  clear tmp. refresh tmp.
  loop at rgdir where abkrs = p_abkrr
                  and fpper = w_fpper
                  and fpbeg = p_begre
                  and fpend = p_endre
                  and srtza = 'A'.
    tmp-num = rgdir-seqnr.
    append tmp. clear tmp.
  endloop.
ENDFORM.                    " get_previous_data
*&---------------------------------------------------------------------*
*&      Form  calc_difference_amount
*&---------------------------------------------------------------------*
FORM calc_difference_amount.

  field-symbols: <ca>    like pc207-betrg,
                 <pa>    like pc207-betrg.

  get_diffa it_form1 it_writ1.
  get_diffa it_form2 it_writ2.
  get_diffa it_form3 it_writ3.
ENDFORM.                    " calc_difference_amount
*&---------------------------------------------------------------------*
*&      Form  write_body
*&---------------------------------------------------------------------*
FORM write_body.
  perform write_format_1. reserve 80 lines.
  perform write_format_2. reserve 80 lines.
  perform write_format_3. reserve 80 lines.
  perform write_format_4. reserve 80 lines.
* reserve 65 lines.
ENDFORM.                    " write_body
*&---------------------------------------------------------------------*
*&      Form  write_format_1
*&---------------------------------------------------------------------*
FORM write_format_1.
  w_tabno = 1. sort it_form1 by zcoln.
  write_heading it_form1 w_titl1 w_tipl1 w_tabno.
  write_body    it_form1 it_writ1 w_tabno.
  new-line. uline at (w_width).
ENDFORM.                    " write_format_1
*&---------------------------------------------------------------------*
*&      Form  make_header_title
*&---------------------------------------------------------------------*
FORM make_header_title.
  data: l_parea(20),
        l_perid(40),
        l_begda(10),
        l_endda(10),
        l_abkrc(2)       type n.
*
  case p_abkrc.
    when '11'. l_parea = 'Bi-Weekly'.
    when '13'. l_parea = 'Monthly'.
    when '14'. l_parea = 'Non-Payroll'.
  endcase.
*
  write p_begca to l_begda using edit mask '__/__/____'.
  write p_endca to l_endda using edit mask '__/__/____'.
*
  if p_abrpc >= 10.
    concatenate p_abrpc 'th' into w_abkrc.
  else.
    concatenate p_abrpc+1(1) 'th' into w_abkrc.
  endif.
  concatenate '(Term:' l_begda '~' l_endda ')' into l_perid.
*
  concatenate '<<<' l_parea w_abkrc 'Salary Pay' l_perid '>>>'
         into w_title separated by space.
*
  concatenate 'Earnings(' w_abkrc ')' into w_titl1.
  concatenate 'Tax(' w_abkrc ')' into w_titl2.
  concatenate 'Deduction(' w_abkrc ')' into w_titl3.
  concatenate 'Check Amount(' w_abkrc ')' into w_titl4.
*
  if p_compp = 'X'.
    if p_abrpr >= 10.
      concatenate p_abrpr 'th' into w_abkrp.
    else.
      concatenate p_abrpr+1(1) 'th' into w_abkrp.
    endif.
    concatenate 'Earnings(' w_abkrp ')' into w_tipl1.
    concatenate 'Tax(' w_abkrp ')' into w_tipl2.
    concatenate 'Deduction(' w_abkrp ')' into w_tipl3.
    concatenate 'Check Amount(' w_abkrp ')' into w_tipl4.
  endif.
ENDFORM.                    " make_header_title
*&---------------------------------------------------------------------*
*&      Form  write_format_2
*&---------------------------------------------------------------------*
FORM write_format_2.
  w_tabno = 2. sort it_form1 by zcoln.
  write_heading it_form2 w_titl2 w_tipl2 w_tabno.
  write_body    it_form2 it_writ2 w_tabno.
  new-line. uline at (w_width).
ENDFORM.                    " write_format_2
*&---------------------------------------------------------------------*
*&      Form  write_format_3
*&---------------------------------------------------------------------*
FORM write_format_3.
  w_tabno = 3.
  sort it_form3 by zcoln lgart descending.
  write_heading it_form3 w_titl3 w_tipl3 w_tabno.
  write_body    it_form3 it_writ3 w_tabno.
  new-line. uline at (w_width).
ENDFORM.                    " write_format_3
*&---------------------------------------------------------------------*
*&      Form  write_format_4
*&---------------------------------------------------------------------*
FORM write_format_4.
  w_tabno = 4. sort it_form1 by zcoln.
  write_heading it_form4 w_titl4 w_tipl4 w_tabno.
  write_body    it_form4 it_writ4 w_tabno.
  new-line.     uline at (w_width).
ENDFORM.                    " write_format_4
*&---------------------------------------------------------------------*
*&      Form  make_title
*&---------------------------------------------------------------------*
FORM make_title using p_tabno.
  clear it_title. refresh it_title.
*
  it_title-htext = 'SAP NO'.         append it_title.
  it_title-htext = 'SSNO'.           append it_title.
  it_title-htext = 'Last Name'.      append it_title.
  it_title-htext = 'First Name'.     append it_title.
*
  case p_tabno.
    when 1. make_title it_form1.
    when 2. make_title it_form2.
    when 3. make_title it_form3.
    when 4. make_title it_form4.
  endcase.
ENDFORM.                    " make_title
*&---------------------------------------------------------------------*
*&      Form  download_to_excel
*&---------------------------------------------------------------------*
FORM download_to_excel using p_tabno.
  data tp_write like it_writ1 occurs 0 with header line.
  clear tp_write. refresh tp_write.
*
  case p_tabno.
    when 1. shet_name = 'Earings'.       tp_write[] = it_writ1[].
    when 2. shet_name = 'Tax'.           tp_write[] = it_writ2[].
    when 3. shet_name = 'Deduction'.     tp_write[] = it_writ3[].
    when 4. shet_name = 'Check Amount'.  tp_write[] = it_writ4[].
  endcase.
*
  call function 'MS_EXCEL_OLE_STANDARD_DAT'
       exporting
            file_name       = file_name
            data_sheet_name = shet_name
       tables
            data_tab        = tp_write
            fieldnames      = it_title.
ENDFORM.                    " download_to_excel
*&---------------------------------------------------------------------*
*&      Form  get_filename
*&---------------------------------------------------------------------*
FORM get_filename.
  data: p_docid      type  dsvasdocid,
        p_direc      type  dsvasdocid,
        p_filen      type  dsvasdocid,
        p_exten      type  dsvasdocid.
  data: l_filen(50),
        l_exten(50).
  clear: p_direc, p_filen, p_exten.
*
  call function 'WS_FILENAME_GET'
       exporting
            mask             = ',*.xls,*.xls.'
            mode             = 'O'
            title            = 'PC File'
       importing
            filename         = file_name
       exceptions
            inv_winsys       = 1
            no_batch         = 2
            selection_cancel = 3
            selection_error  = 4
            others           = 5.
  check file_name ne space.
*
  p_docid = file_name.
  call function 'DSVAS_DOC_FILENAME_SPLIT'
       exporting
            pf_docid         = p_docid
       importing
            pf_directory     = p_direc
            pf_filename      = p_filen
            pf_extension     = p_exten.
*
  if p_filen <> p_exten.
    split p_filen at '.' into l_filen l_exten.
    clear file_name.
    concatenate p_direc l_filen into file_name.
  endif.
ENDFORM.                    " get_filename
*&---------------------------------------------------------------------*
*&      Form  ascending_sort_itab1
*&---------------------------------------------------------------------*
FORM ascending_sort_itab1.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ1 by pernr.
    when 'PERID'. sort it_writ1 by perid.
    when 'NCHMC'. sort it_writ1 by nchmc.
    when 'VNAMC'. sort it_writ1 by vnamc.
    when 'CUR01'. sort it_writ1 by cur01.
    when 'CUR02'. sort it_writ1 by cur02.
    when 'CUR03'. sort it_writ1 by cur03.
    when 'CUR04'. sort it_writ1 by cur04.
    when 'CUR05'. sort it_writ1 by cur05.
    when 'CUR06'. sort it_writ1 by cur06.
    when 'CUR07'. sort it_writ1 by cur07.
    when 'CUR08'. sort it_writ1 by cur08.
    when 'CUR09'. sort it_writ1 by cur09.
    when 'CUR10'. sort it_writ1 by cur10.
    when 'CUR11'. sort it_writ1 by cur11.
    when 'CUR12'. sort it_writ1 by cur12.
    when 'CUR13'. sort it_writ1 by cur13.
    when 'CUR14'. sort it_writ1 by cur14.
    when 'CUR15'. sort it_writ1 by cur15.
    when 'PRE01'. sort it_writ1 by pre01.
    when 'PRE02'. sort it_writ1 by pre02.
    when 'PRE03'. sort it_writ1 by pre03.
    when 'PRE04'. sort it_writ1 by pre04.
    when 'PRE05'. sort it_writ1 by pre05.
    when 'PRE06'. sort it_writ1 by pre06.
    when 'PRE07'. sort it_writ1 by pre07.
    when 'PRE08'. sort it_writ1 by pre08.
    when 'PRE09'. sort it_writ1 by pre09.
    when 'PRE10'. sort it_writ1 by pre10.
    when 'PRE11'. sort it_writ1 by pre11.
    when 'PRE12'. sort it_writ1 by pre12.
    when 'PRE13'. sort it_writ1 by pre13.
    when 'PRE14'. sort it_writ1 by pre14.
    when 'PRE15'. sort it_writ1 by pre15.
  endcase.
ENDFORM.                    " ascending_sort_itab1
*&---------------------------------------------------------------------*
*&      Form  ascending_sort_itab2
*&---------------------------------------------------------------------*
FORM ascending_sort_itab2.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ2 by pernr.
    when 'PERID'. sort it_writ2 by perid.
    when 'NCHMC'. sort it_writ2 by nchmc.
    when 'VNAMC'. sort it_writ2 by vnamc.
    when 'CUR01'. sort it_writ2 by cur01.
    when 'CUR02'. sort it_writ2 by cur02.
    when 'CUR03'. sort it_writ2 by cur03.
    when 'CUR04'. sort it_writ2 by cur04.
    when 'CUR05'. sort it_writ2 by cur05.
    when 'CUR06'. sort it_writ2 by cur06.
    when 'CUR07'. sort it_writ2 by cur07.
    when 'CUR08'. sort it_writ2 by cur08.
    when 'CUR09'. sort it_writ2 by cur09.
    when 'CUR10'. sort it_writ2 by cur10.
    when 'CUR11'. sort it_writ2 by cur11.
    when 'CUR12'. sort it_writ2 by cur12.
    when 'CUR13'. sort it_writ2 by cur13.
    when 'CUR14'. sort it_writ2 by cur14.
    when 'CUR15'. sort it_writ2 by cur15.
    when 'PRE01'. sort it_writ2 by pre01.
    when 'PRE02'. sort it_writ2 by pre02.
    when 'PRE03'. sort it_writ2 by pre03.
    when 'PRE04'. sort it_writ2 by pre04.
    when 'PRE05'. sort it_writ2 by pre05.
    when 'PRE06'. sort it_writ2 by pre06.
    when 'PRE07'. sort it_writ2 by pre07.
    when 'PRE08'. sort it_writ2 by pre08.
    when 'PRE09'. sort it_writ2 by pre09.
    when 'PRE10'. sort it_writ2 by pre10.
    when 'PRE11'. sort it_writ2 by pre11.
    when 'PRE12'. sort it_writ2 by pre12.
    when 'PRE13'. sort it_writ2 by pre13.
    when 'PRE14'. sort it_writ2 by pre14.
    when 'PRE15'. sort it_writ2 by pre15.
  endcase.
ENDFORM.                    " ascending_sort_itab2
*&---------------------------------------------------------------------*
*&      Form  ascending_sort_itab3
*&---------------------------------------------------------------------*
FORM ascending_sort_itab3.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ3 by pernr.
    when 'PERID'. sort it_writ3 by perid.
    when 'NCHMC'. sort it_writ3 by nchmc.
    when 'VNAMC'. sort it_writ3 by vnamc.
    when 'CUR01'. sort it_writ3 by cur01.
    when 'CUR02'. sort it_writ3 by cur02.
    when 'CUR03'. sort it_writ3 by cur03.
    when 'CUR04'. sort it_writ3 by cur04.
    when 'CUR05'. sort it_writ3 by cur05.
    when 'CUR06'. sort it_writ3 by cur06.
    when 'CUR07'. sort it_writ3 by cur07.
    when 'CUR08'. sort it_writ3 by cur08.
    when 'CUR09'. sort it_writ3 by cur09.
    when 'CUR10'. sort it_writ3 by cur10.
    when 'CUR11'. sort it_writ3 by cur11.
    when 'CUR12'. sort it_writ3 by cur12.
    when 'CUR13'. sort it_writ3 by cur13.
    when 'CUR14'. sort it_writ3 by cur14.
    when 'CUR15'. sort it_writ3 by cur15.
    when 'PRE01'. sort it_writ3 by pre01.
    when 'PRE02'. sort it_writ3 by pre02.
    when 'PRE03'. sort it_writ3 by pre03.
    when 'PRE04'. sort it_writ3 by pre04.
    when 'PRE05'. sort it_writ3 by pre05.
    when 'PRE06'. sort it_writ3 by pre06.
    when 'PRE07'. sort it_writ3 by pre07.
    when 'PRE08'. sort it_writ3 by pre08.
    when 'PRE09'. sort it_writ3 by pre09.
    when 'PRE10'. sort it_writ3 by pre10.
    when 'PRE11'. sort it_writ3 by pre11.
    when 'PRE12'. sort it_writ3 by pre12.
    when 'PRE13'. sort it_writ3 by pre13.
    when 'PRE14'. sort it_writ3 by pre14.
    when 'PRE15'. sort it_writ3 by pre15.
  endcase.
ENDFORM.                    " ascending_sort_itab3
*&---------------------------------------------------------------------*
*&      Form  ascending_sort_itab4
*&---------------------------------------------------------------------*
FORM ascending_sort_itab4.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ4 by pernr.
    when 'PERID'. sort it_writ4 by perid.
    when 'NCHMC'. sort it_writ4 by nchmc.
    when 'VNAMC'. sort it_writ4 by vnamc.
    when 'CUR01'. sort it_writ4 by cur01.
    when 'CUR02'. sort it_writ4 by cur02.
    when 'CUR03'. sort it_writ4 by cur03.
    when 'CUR04'. sort it_writ4 by cur04.
    when 'CUR05'. sort it_writ4 by cur05.
    when 'CUR06'. sort it_writ4 by cur06.
    when 'CUR07'. sort it_writ4 by cur07.
    when 'CUR08'. sort it_writ4 by cur08.
    when 'CUR09'. sort it_writ4 by cur09.
    when 'CUR10'. sort it_writ4 by cur10.
    when 'CUR11'. sort it_writ4 by cur11.
    when 'CUR12'. sort it_writ4 by cur12.
    when 'CUR13'. sort it_writ4 by cur13.
    when 'CUR14'. sort it_writ4 by cur14.
    when 'CUR15'. sort it_writ4 by cur15.
    when 'PRE01'. sort it_writ4 by pre01.
    when 'PRE02'. sort it_writ4 by pre02.
    when 'PRE03'. sort it_writ4 by pre03.
    when 'PRE04'. sort it_writ4 by pre04.
    when 'PRE05'. sort it_writ4 by pre05.
    when 'PRE06'. sort it_writ4 by pre06.
    when 'PRE07'. sort it_writ4 by pre07.
    when 'PRE08'. sort it_writ4 by pre08.
    when 'PRE09'. sort it_writ4 by pre09.
    when 'PRE10'. sort it_writ4 by pre10.
    when 'PRE11'. sort it_writ4 by pre11.
    when 'PRE12'. sort it_writ4 by pre12.
    when 'PRE13'. sort it_writ4 by pre13.
    when 'PRE14'. sort it_writ4 by pre14.
    when 'PRE15'. sort it_writ4 by pre15.
  endcase.
ENDFORM.                    " ascending_sort_itab4
*&---------------------------------------------------------------------*
*&      Form  descending_sort_itab1
*&---------------------------------------------------------------------*
FORM descending_sort_itab1.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ1 by pernr descending.
    when 'PERID'. sort it_writ1 by perid descending.
    when 'NCHMC'. sort it_writ1 by nchmc descending.
    when 'VNAMC'. sort it_writ1 by vnamc descending.
    when 'CUR01'. sort it_writ1 by cur01 descending.
    when 'CUR02'. sort it_writ1 by cur02 descending.
    when 'CUR03'. sort it_writ1 by cur03 descending.
    when 'CUR04'. sort it_writ1 by cur04 descending.
    when 'CUR05'. sort it_writ1 by cur05 descending.
    when 'CUR06'. sort it_writ1 by cur06 descending.
    when 'CUR07'. sort it_writ1 by cur07 descending.
    when 'CUR08'. sort it_writ1 by cur08 descending.
    when 'CUR09'. sort it_writ1 by cur09 descending.
    when 'CUR10'. sort it_writ1 by cur10 descending.
    when 'CUR11'. sort it_writ1 by cur11 descending.
    when 'CUR12'. sort it_writ1 by cur12 descending.
    when 'CUR13'. sort it_writ1 by cur13 descending.
    when 'CUR14'. sort it_writ1 by cur14 descending.
    when 'CUR15'. sort it_writ1 by cur15 descending.
    when 'PRE01'. sort it_writ1 by pre01 descending.
    when 'PRE02'. sort it_writ1 by pre02 descending.
    when 'PRE03'. sort it_writ1 by pre03 descending.
    when 'PRE04'. sort it_writ1 by pre04 descending.
    when 'PRE05'. sort it_writ1 by pre05 descending.
    when 'PRE06'. sort it_writ1 by pre06 descending.
    when 'PRE07'. sort it_writ1 by pre07 descending.
    when 'PRE08'. sort it_writ1 by pre08 descending.
    when 'PRE09'. sort it_writ1 by pre09 descending.
    when 'PRE10'. sort it_writ1 by pre10 descending.
    when 'PRE11'. sort it_writ1 by pre11 descending.
    when 'PRE12'. sort it_writ1 by pre12 descending.
    when 'PRE13'. sort it_writ1 by pre13 descending.
    when 'PRE14'. sort it_writ1 by pre14 descending.
    when 'PRE15'. sort it_writ1 by pre15 descending.
  endcase.
ENDFORM.                    " descending_sort_itab1
*&---------------------------------------------------------------------*
*&      Form  descending_sort_itab2
*&---------------------------------------------------------------------*
FORM descending_sort_itab2.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ2 by pernr descending.
    when 'PERID'. sort it_writ2 by perid descending.
    when 'NCHMC'. sort it_writ2 by nchmc descending.
    when 'VNAMC'. sort it_writ2 by vnamc descending.
    when 'CUR01'. sort it_writ2 by cur01 descending.
    when 'CUR02'. sort it_writ2 by cur02 descending.
    when 'CUR03'. sort it_writ2 by cur03 descending.
    when 'CUR04'. sort it_writ2 by cur04 descending.
    when 'CUR05'. sort it_writ2 by cur05 descending.
    when 'CUR06'. sort it_writ2 by cur06 descending.
    when 'CUR07'. sort it_writ2 by cur07 descending.
    when 'CUR08'. sort it_writ2 by cur08 descending.
    when 'CUR09'. sort it_writ2 by cur09 descending.
    when 'CUR10'. sort it_writ2 by cur10 descending.
    when 'CUR11'. sort it_writ2 by cur11 descending.
    when 'CUR12'. sort it_writ2 by cur12 descending.
    when 'CUR13'. sort it_writ2 by cur13 descending.
    when 'CUR14'. sort it_writ2 by cur14 descending.
    when 'CUR15'. sort it_writ2 by cur15 descending.
    when 'PRE01'. sort it_writ2 by pre01 descending.
    when 'PRE02'. sort it_writ2 by pre02 descending.
    when 'PRE03'. sort it_writ2 by pre03 descending.
    when 'PRE04'. sort it_writ2 by pre04 descending.
    when 'PRE05'. sort it_writ2 by pre05 descending.
    when 'PRE06'. sort it_writ2 by pre06 descending.
    when 'PRE07'. sort it_writ2 by pre07 descending.
    when 'PRE08'. sort it_writ2 by pre08 descending.
    when 'PRE09'. sort it_writ2 by pre09 descending.
    when 'PRE10'. sort it_writ2 by pre10 descending.
    when 'PRE11'. sort it_writ2 by pre11 descending.
    when 'PRE12'. sort it_writ2 by pre12 descending.
    when 'PRE13'. sort it_writ2 by pre13 descending.
    when 'PRE14'. sort it_writ2 by pre14 descending.
    when 'PRE15'. sort it_writ2 by pre15 descending.
  endcase.
ENDFORM.                    " descending_sort_itab2
*&---------------------------------------------------------------------*
*&      Form  descending_sort_itab3
*&---------------------------------------------------------------------*
FORM descending_sort_itab3.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ3 by pernr descending.
    when 'PERID'. sort it_writ3 by perid descending.
    when 'NCHMC'. sort it_writ3 by nchmc descending.
    when 'VNAMC'. sort it_writ3 by vnamc descending.
    when 'CUR01'. sort it_writ3 by cur01 descending.
    when 'CUR02'. sort it_writ3 by cur02 descending.
    when 'CUR03'. sort it_writ3 by cur03 descending.
    when 'CUR04'. sort it_writ3 by cur04 descending.
    when 'CUR05'. sort it_writ3 by cur05 descending.
    when 'CUR06'. sort it_writ3 by cur06 descending.
    when 'CUR07'. sort it_writ3 by cur07 descending.
    when 'CUR08'. sort it_writ3 by cur08 descending.
    when 'CUR09'. sort it_writ3 by cur09 descending.
    when 'CUR10'. sort it_writ3 by cur10 descending.
    when 'CUR11'. sort it_writ3 by cur11 descending.
    when 'CUR12'. sort it_writ3 by cur12 descending.
    when 'CUR13'. sort it_writ3 by cur13 descending.
    when 'CUR14'. sort it_writ3 by cur14 descending.
    when 'CUR15'. sort it_writ3 by cur15 descending.
    when 'PRE01'. sort it_writ3 by pre01 descending.
    when 'PRE02'. sort it_writ3 by pre02 descending.
    when 'PRE03'. sort it_writ3 by pre03 descending.
    when 'PRE04'. sort it_writ3 by pre04 descending.
    when 'PRE05'. sort it_writ3 by pre05 descending.
    when 'PRE06'. sort it_writ3 by pre06 descending.
    when 'PRE07'. sort it_writ3 by pre07 descending.
    when 'PRE08'. sort it_writ3 by pre08 descending.
    when 'PRE09'. sort it_writ3 by pre09 descending.
    when 'PRE10'. sort it_writ3 by pre10 descending.
    when 'PRE11'. sort it_writ3 by pre11 descending.
    when 'PRE12'. sort it_writ3 by pre12 descending.
    when 'PRE13'. sort it_writ3 by pre13 descending.
    when 'PRE14'. sort it_writ3 by pre14 descending.
    when 'PRE15'. sort it_writ3 by pre15 descending.
  endcase.
ENDFORM.                    " descending_sort_itab3
*&---------------------------------------------------------------------*
*&      Form  descending_sort_itab4
*&---------------------------------------------------------------------*
FORM descending_sort_itab4.
  case w_fname+9(5).
    when 'PERNR'. sort it_writ4 by pernr descending.
    when 'PERID'. sort it_writ4 by perid descending.
    when 'NCHMC'. sort it_writ4 by nchmc descending.
    when 'VNAMC'. sort it_writ4 by vnamc descending.
    when 'CUR01'. sort it_writ4 by cur01 descending.
    when 'CUR02'. sort it_writ4 by cur02 descending.
    when 'CUR03'. sort it_writ4 by cur03 descending.
    when 'CUR04'. sort it_writ4 by cur04 descending.
    when 'CUR05'. sort it_writ4 by cur05 descending.
    when 'CUR06'. sort it_writ4 by cur06 descending.
    when 'CUR07'. sort it_writ4 by cur07 descending.
    when 'CUR08'. sort it_writ4 by cur08 descending.
    when 'CUR09'. sort it_writ4 by cur09 descending.
    when 'CUR10'. sort it_writ4 by cur10 descending.
    when 'CUR11'. sort it_writ4 by cur11 descending.
    when 'CUR12'. sort it_writ4 by cur12 descending.
    when 'CUR13'. sort it_writ4 by cur13 descending.
    when 'CUR14'. sort it_writ4 by cur14 descending.
    when 'CUR15'. sort it_writ4 by cur15 descending.
    when 'PRE01'. sort it_writ4 by pre01 descending.
    when 'PRE02'. sort it_writ4 by pre02 descending.
    when 'PRE03'. sort it_writ4 by pre03 descending.
    when 'PRE04'. sort it_writ4 by pre04 descending.
    when 'PRE05'. sort it_writ4 by pre05 descending.
    when 'PRE06'. sort it_writ4 by pre06 descending.
    when 'PRE07'. sort it_writ4 by pre07 descending.
    when 'PRE08'. sort it_writ4 by pre08 descending.
    when 'PRE09'. sort it_writ4 by pre09 descending.
    when 'PRE10'. sort it_writ4 by pre10 descending.
    when 'PRE11'. sort it_writ4 by pre11 descending.
    when 'PRE12'. sort it_writ4 by pre12 descending.
    when 'PRE13'. sort it_writ4 by pre13 descending.
    when 'PRE14'. sort it_writ4 by pre14 descending.
    when 'PRE15'. sort it_writ4 by pre15 descending.
  endcase.
ENDFORM.                    " descending_sort_itab4
*&---------------------------------------------------------------------*
*&      Form  excel_download
*&---------------------------------------------------------------------*
FORM excel_download.
  perform init_factory.
  perform excel_file_open.
  perform set_sheet_form1.
  perform set_sheet_form2.
  perform set_sheet_form3.
  perform set_sheet_form4.
  SET PROPERTY OF EXCEL 'Visible' = 1.
  perform close_doc.
  perform close_factory.
ENDFORM.                    " excel_download
*&---------------------------------------------------------------------*
*&      Form  fill_cell
*&---------------------------------------------------------------------*
FORM fill_cell USING  I J VAL.
  CALL METHOD OF EXCEL 'CELLS' = CELL EXPORTING #1 = I #2 = J.
  SET PROPERTY OF CELL 'VALUE' = VAL.
ENDFORM.                    " fill_cell
*&---------------------------------------------------------------------*
*&      Form  excel_file_open
*&---------------------------------------------------------------------*
FORM excel_file_open.
  CREATE OBJECT EXCEL 'EXCEL.APPLICATION'.
* SET PROPERTY OF EXCEL 'Visible' = 0.
  CALL METHOD OF EXCEL 'Workbooks' = SHEET.
*... file open (import from PC)
* CALL METHOD OF SHEET 'OPEN'
*                EXPORTING #1 = 'D:\WageType.xls'.
*
  if p_compp = 'X'.
    object_id = 'ZWAGETYPED'.
  else.
    object_id = 'ZWAGETYPE'.
  endif.
*... field open (import from WEB repository)
  CALL FUNCTION 'SAP_OI_LOAD_MIME_DATA'
       EXPORTING
            OBJECT_ID        = object_id
       IMPORTING
            DATA_SIZE        = DOC_SIZE
            DOCUMENT_FORMAT  = DOC_FORMAT
            DOCUMENT_TYPE    = DOC_TYPE
       TABLES
            DATA_TABLE       = DOC_TABLE
       EXCEPTIONS
            OBJECT_NOT_FOUND = 1
            INTERNAL_ERROR   = 2
            OTHERS           = 3.
*
  IF DOC_SIZE NE 0.
    CALL METHOD FACTORY->GET_DOCUMENT_PROXY
                      EXPORTING DOCUMENT_TYPE = DOC_TYPE
                      IMPORTING DOCUMENT_PROXY = DOCUMENT
                                RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD DOCUMENT->OPEN_DOCUMENT_FROM_TABLE
                      EXPORTING DOCUMENT_TABLE = DOC_TABLE[]
                                DOCUMENT_SIZE  = DOC_SIZE
                      IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    SET PROPERTY OF EXCEL 'Visible' = 0.
  endif.
ENDFORM.                    " excel_file_open
*&---------------------------------------------------------------------*
*&      Form  set_sheet_form1
*&---------------------------------------------------------------------*
FORM set_sheet_form1.
*... assign sheet
  CALL METHOD OF EXCEL 'Worksheets' = BOOK
               EXPORTING #1 = 1.

  CALL METHOD OF BOOK 'Activate'.
*
  perc = 1.
  do 5 times.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = perc
              TEXT       = TEXT-006
         EXCEPTIONS
              OTHERS     = 1.
    perc = perc + 1.
  enddo.
*
  row = 1.   column = 1.
  perform fill_cell using row column w_title.
  row = 2.   column = 5.
  perform fill_cell using row column w_titl1.

  if p_compp = 'X'.
    column = 20. perform fill_cell using row column w_tipl1.
  endif.
*
  row = 3. column = 5.
  loop at it_form1.
    perform fill_cell using row column it_form1-lgtxt.
    column = column + 1.
  endloop.
  cols = column - 5.

  if p_compp = 'X'.
    column = 20.
    loop at it_form1.
      perform fill_cell using row column it_form1-lgtxt.
      column = column + 1.
    endloop.
  endif.
*
  row = 5.
  loop at it_writ1.
    column = 1.  perform fill_cell using row column it_writ1-pernr.
    column = 2.  perform fill_cell using row column it_writ1-perid.
    column = 3.  perform fill_cell using row column it_writ1-nchmc.
    column = 4.  perform fill_cell using row column it_writ1-vnamc.

    w_count = '01'.
    do cols times.
      column = column + 1.
      concatenate 'it_writ1-cur' w_count into w_fname.
      assign (w_fname) to <fs>.
      perform fill_cell using row column <fs>.
      w_count = w_count + 1.
    enddo.

    if p_compp = 'X'.
      w_count = '01'.
      column = 19.
      do cols times.
        column = column + 1.
        concatenate 'it_writ1-pre' w_count into w_fname.
        assign (w_fname) to <fs>.
        perform fill_cell using row column <fs>.
        w_count = w_count + 1.
      enddo.
      column = 35. perform fill_cell using row column it_writ1-diffa.
    endif.

    row = row + 1.
  endloop.
*
  do 10 times.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = perc
              TEXT       = TEXT-006
         EXCEPTIONS
              OTHERS     = 1.
    perc = perc + 2.
  enddo.
ENDFORM.                    " set_sheet_form1
*&---------------------------------------------------------------------*
*&      Form  set_sheet_form2
*&---------------------------------------------------------------------*
FORM set_sheet_form2.
*... assign sheet
  CALL METHOD OF EXCEL 'Worksheets' = BOOK
               EXPORTING #1 = 2.

  CALL METHOD OF BOOK 'Activate'.
* SET PROPERTY OF EXCEL 'VISIBLE' = 1.
*
  row = 1.   column = 1.
  perform fill_cell using row column w_title.
  row = 2.   column = 5.
  perform fill_cell using row column w_titl2.

  if p_compp = 'X'.
    column = 10. perform fill_cell using row column w_tipl2.
  endif.
*
  row = 3. column = 5.
  loop at it_form2.
    perform fill_cell using row column it_form2-lgtxt.
    column = column + 1.
  endloop.
  cols = column - 5.

  if p_compp = 'X'.
    loop at it_form2.
      column = column + 1.
      perform fill_cell using row column it_form2-lgtxt.
    endloop.
  endif.
*
  row = 5.
  loop at it_writ2.
    column = 1.  perform fill_cell using row column it_writ2-pernr.
    column = 2.  perform fill_cell using row column it_writ2-perid.
    column = 3.  perform fill_cell using row column it_writ2-nchmc.
    column = 4.  perform fill_cell using row column it_writ2-vnamc.

    w_count = '01'.
    do cols times.
      column = column + 1.
      concatenate 'it_writ2-cur' w_count into w_fname.
      assign (w_fname) to <fs>.
      perform fill_cell using row column <fs>.
      w_count = w_count + 1.
    enddo.

    if p_compp = 'X'.
      w_count = '01'.
      column = 9.
      do cols times.
        column = column + 1.
        concatenate 'it_writ2-pre' w_count into w_fname.
        assign (w_fname) to <fs>.
        perform fill_cell using row column <fs>.
        w_count = w_count + 1.
      enddo.
      column = 15. perform fill_cell using row column it_writ2-diffa.
    endif.

    row = row + 1.
  endloop.
*
  do 10 times.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = perc
              TEXT       = TEXT-006
         EXCEPTIONS
              OTHERS     = 1.
    perc = perc + 2.
  enddo.
ENDFORM.                    " set_sheet_form2
*&---------------------------------------------------------------------*
*&      Form  set_sheet_form3
*&---------------------------------------------------------------------*
FORM set_sheet_form3.
*... assign sheet
  CALL METHOD OF EXCEL 'Worksheets' = BOOK
               EXPORTING #1 = 3.

  CALL METHOD OF BOOK 'Activate'.
* SET PROPERTY OF EXCEL 'VISIBLE' = 1.
*
  row = 1.   column = 1.
  perform fill_cell using row column w_title.
  row = 2.   column = 5.
  perform fill_cell using row column w_titl3.

  if p_compp = 'X'.
    column = 20. perform fill_cell using row column w_tipl3.
  endif.
*
  row = 3. column = 5.
  loop at it_form3.
    at new zcoln.
      w_newfl = 'X'.
    endat.

    if w_newfl = 'X'.
      perform fill_cell using row column it_form3-lgtxt.
      column = column + 1.
    endif.
    w_newfl = space.
  endloop.
  cols = column - 5.

  if p_compp = 'X'.
    column = 20.
    loop at it_form3.
      perform fill_cell using row column it_form3-lgtxt.
      column = column + 1.
    endloop.
  endif.
*
  row = 5.
  loop at it_writ3.
    column = 1.  perform fill_cell using row column it_writ3-pernr.
    column = 2.  perform fill_cell using row column it_writ3-perid.
    column = 3.  perform fill_cell using row column it_writ3-nchmc.
    column = 4.  perform fill_cell using row column it_writ3-vnamc.

    w_count = '01'.
    do cols times.
      column = column + 1.
      concatenate 'it_writ3-cur' w_count into w_fname.
      assign (w_fname) to <fs>.
      perform fill_cell using row column <fs>.
      w_count = w_count + 1.
    enddo.

    if p_compp = 'X'.
      w_count = '01'.
      column = 19.
      do cols times.
        column = column + 1.
        concatenate 'it_writ3-pre' w_count into w_fname.
        assign (w_fname) to <fs>.
        perform fill_cell using row column <fs>.
        w_count = w_count + 1.
      enddo.
      column = 35. perform fill_cell using row column it_writ3-diffa.
    endif.

    row = row + 1.
  endloop.
*
  do 10 times.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = perc
              TEXT       = TEXT-006
         EXCEPTIONS
              OTHERS     = 1.
    perc = perc + 2.
  enddo.
ENDFORM.                    " set_sheet_form3
*&---------------------------------------------------------------------*
*&      Form  set_sheet_form4
*&---------------------------------------------------------------------*
FORM set_sheet_form4.
*... assign sheet
  CALL METHOD OF EXCEL 'Worksheets' = BOOK
               EXPORTING #1 = 4.

  CALL METHOD OF BOOK 'Activate'.
* SET PROPERTY OF EXCEL 'VISIBLE' = 1.
*
  row = 1.   column = 1.
  perform fill_cell using row column w_title.
  row = 2.   column = 5.
  perform fill_cell using row column w_titl4.

  if p_compp = 'X'.
    column = 9. perform fill_cell using row column w_tipl4.
  endif.
*
  row = 3. column = 5.
  loop at it_form4.
    perform fill_cell using row column it_form4-lgtxt.
    column = column + 1.
  endloop.
  cols = column - 5.

  if p_compp = 'X'.
    column = 8.
    loop at it_form4.
      column = column + 1.
      perform fill_cell using row column it_form4-lgtxt.
    endloop.
  endif.
*
  row = 5.
  loop at it_writ4.
    column = 1.  perform fill_cell using row column it_writ4-pernr.
    column = 2.  perform fill_cell using row column it_writ4-perid.
    column = 3.  perform fill_cell using row column it_writ4-nchmc.
    column = 4.  perform fill_cell using row column it_writ4-vnamc.

    w_count = '01'.
    do cols times.
      column = column + 1.
      concatenate 'it_writ4-cur' w_count into w_fname.
      assign (w_fname) to <fs>.
      perform fill_cell using row column <fs>.
      w_count = w_count + 1.
    enddo.

    if p_compp = 'X'.
      w_count = '01'.
      column = 8.
      do cols times.
        column = column + 1.
        concatenate 'it_writ4-pre' w_count into w_fname.
        assign (w_fname) to <fs>.
        perform fill_cell using row column <fs>.
        w_count = w_count + 1.
      enddo.
    endif.

    row = row + 1.
  endloop.
*
  do 10 times.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = perc
              TEXT       = TEXT-006
         EXCEPTIONS
              OTHERS     = 1.
    perc = perc + 2.
  enddo.
ENDFORM.                    " set_sheet_form4
*&---------------------------------------------------------------------*
*&      Form  init_factory
*&---------------------------------------------------------------------*
FORM init_factory.
  IF FACTORY IS INITIAL.
    CALL METHOD C_OI_FACTORY_CREATOR=>GET_DOCUMENT_FACTORY
                      IMPORTING FACTORY = FACTORY
                                RETCODE = RETCODE.
    IF RETCODE NE C_OI_ERRORS=>RET_OK. EXIT. ENDIF.

    CALL METHOD FACTORY->START_FACTORY
         EXPORTING
              R3_APPLICATION_NAME = 'Wage Type Report'
         IMPORTING
              RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD FACTORY->GET_LINK_SERVER
                       IMPORTING LINK_SERVER = LINK_SERVER
                                 RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    CALL METHOD LINK_SERVER->START_LINK_SERVER
                      IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
  ENDIF.
ENDFORM.                    " init_factory
*&---------------------------------------------------------------------*
*&      Form  close_factory
*&---------------------------------------------------------------------*
FORM close_factory.
  IF NOT LINK_SERVER IS INITIAL.
    CALL METHOD LINK_SERVER->STOP_LINK_SERVER
                                   IMPORTING RETCODE = RETCODE.
    FREE LINK_SERVER.
  ENDIF.
  IF NOT FACTORY IS INITIAL.
    CALL METHOD FACTORY->STOP_FACTORY IMPORTING RETCODE = RETCODE.
    FREE FACTORY.
  ENDIF.
ENDFORM.                    " close_factory
*&---------------------------------------------------------------------*
*&      Form  close_doc
*&---------------------------------------------------------------------*
FORM close_doc.
  IF NOT DOCUMENT IS INITIAL.
    CALL METHOD DOCUMENT->IS_DESTROYED
         IMPORTING RET_VALUE = IS_CLOSED.

    IF IS_CLOSED IS INITIAL.
      CALL METHOD DOCUMENT->CLOSE_DOCUMENT
                     EXPORTING DO_SAVE = 'X'
                     IMPORTING RETCODE = RETCODE.
      CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.
    ENDIF.

    CALL METHOD DOCUMENT->RELEASE_DOCUMENT
         IMPORTING RETCODE = RETCODE.
    CALL METHOD C_OI_ERRORS=>SHOW_MESSAGE EXPORTING TYPE = 'E'.

    FREE DOCUMENT.
  ENDIF.
ENDFORM.                    " close_doc
