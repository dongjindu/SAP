*----------------------------------------------------------------------*
*   INCLUDE RPCPRRUS - Report Specific - Selection Screen Actions
*----------------------------------------------------------------------*
tables: sscrfields.

selection-screen begin of block frm1 with frame title text-150.
select-options: p_txcmp for t5u0p-txcmp.
select-options: p_taxau for t5utw-taxau.
selection-screen begin of line.
selection-screen comment 1(31) text-260.
parameters: p_emptax  like rpcctxu0-emp_tax  default 'C'.
parameters: d_emptax(25).
selection-screen end of line.
selection-screen end of block frm1.

selection-screen begin of block frm4 with frame title text-210.
selection-screen begin of line.
parameters: wtype1 radiobutton group lgrt.
selection-screen comment 4(25) text-wt1.
select-options: p_forms for rpt_form-form no-extension
                                          no intervals.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment 4(28) text-wt3.
parameters: rpt_typ(80) modif id 015.
selection-screen end of line.
parameters: p_form(4) type c no-display .
parameters: p_srtopt(100) type c  no-display.
parameters: p_frmtyp default 'X' no-display.
selection-screen begin of line.
parameters: wtype2 radiobutton group lgrt.
selection-screen comment 4(25) text-wt2.
select-options s_lgart for swgtyp-lgart.
selection-screen end of line.
data: frm_btn.
selection-screen end of block frm4.

selection-screen begin of block frm6 with frame title text-180.
parameters: p_ctd as checkbox default 'X'.
selection-screen begin of block frm7 with frame title text-170.
parameters: p_ytd  as checkbox default space.
parameters: p_qtd  as checkbox default space.
parameters: p_mtd  as checkbox default space.
selection-screen end of block frm7.
parameters: p_tls as checkbox default 'X'.
parameters: p_inf  as checkbox default space.
parameters: option(100) default 'K/L' no-display.
parameters: p_log     as checkbox default 'X'.
selection-screen skip.
selection-screen pushbutton /1(24) p_sort  user-command selo.
select-options p_molga for rpt_molga-molga no-extension
                                           no intervals
                                           default '10'
                                           no-display.
selection-screen end of block frm6.

selection-screen begin of block frm5 with frame title text-250.
selection-screen begin of line.
parameters: p_w2  radiobutton group tr.
selection-screen comment 4(35) text-r01.
selection-screen end of line.
selection-screen begin of line.
parameters: p_941 radiobutton group tr.
selection-screen comment 4(35) text-r02.
selection-screen end of line.
selection-screen begin of line.
parameters: p_sui radiobutton group tr.
selection-screen comment 4(35) text-r03.
selection-screen end of line.
selection-screen begin of line.
parameters: p_fi  radiobutton group tr default 'X'.
selection-screen comment 4(35) text-r04.
selection-screen end of line.
selection-screen skip.
parameters: p_tfrcl like t5ut7-tfrcl.
selection-screen skip.
selection-screen begin of line.
parameters: p_trprd radiobutton group tr1.
selection-screen comment 4(35) text-r05.
selection-screen end of line.
selection-screen begin of line.
parameters: p_trsim radiobutton group tr1.
selection-screen comment 4(35) text-r06.
selection-screen end of line.
parameters: p_lasfd like t5uxy-asofd default space.
parameters: p_asofd like t5uxy-asofd default space.
parameters: p_genda like t5uxy-genda.
parameters: p_gentm like t5uxy-gentm default space.
selection-screen end of block frm5.

selection-screen begin of block frm2 with frame title text-101.
selection-screen begin of line.
parameters: p_payty   like pc261-payty no-display.
selection-screen end of line.
selection-screen begin of line.
parameters: p_payid   like pc261-payid default '*' no-display.
selection-screen end of line.
parameters: p_off      default 'X' no-display.
selection-screen end of block frm2.

selection-screen begin of block frm3 with frame title text-100.
parameter: p_arch as checkbox default space.
parameter: p_last as checkbox default space.
parameter: p_voids  as checkbox default ' '.
parameter: p_offcyc as checkbox default ' '.
select-options: p_paytyr for rgdir-inpty.
selection-screen begin of line.
parameters: r_beg like pc261-fpbeg modif id 001 no-display.
parameters: r_tim like pc261-runtm modif id 001 no-display.
selection-screen end of line.
selection-screen begin of line.
parameters: r_end like pc261-fpend modif id 001 no-display.
parameters: p_void(1) type c default 'X' no-display.
selection-screen end of line.
data: 3pr_btn.
selection-screen end of block frm3.

at selection-screen on value-request for p_forms-low.
  clear rpt_form.  refresh rpt_form.
    perform re5963 tables rpt_form
                    using p_molga+3(2).

    clear f4_fields.  refresh f4_fields.
    move 'T596B' to f4_fields-tabname.
    move 'APPL' to f4_fields-fieldname.
    move 'X' to f4_fields-selectflag.
    append f4_fields.
    move 'T596B' to f4_fields-tabname.
    move 'TEXT' to f4_fields-fieldname.
    move ' ' to f4_fields-selectflag.
    append f4_fields.

call function 'HELP_VALUES_GET_NO_DD_NAME'
     exporting
          selectfield                   = 'APPL'
    importing
          ind                           = index
     tables
          fields                        = f4_fields
          full_table                    = rpt_form.

      read table rpt_form index index.
      form_value = rpt_form-form.
      text_value = rpt_form-text.
move  form_value to p_forms-low.
move  text_value to rpt_typ.
loop at screen. endloop.


at selection-screen on value-request for s_lgart-low.
  perform s_lgart_val tables swgtyp
                             s_lgart
                       using p_molga-low
                             p_frmtyp
                             p_end.


    clear f4_fields.  refresh f4_fields.
    move 'T512W' to f4_fields-tabname.
    move 'LGART' to f4_fields-fieldname.
    move 'X' to f4_fields-selectflag.
    append f4_fields.
    move 'T512T' to f4_fields-tabname.
    move 'LGTXT' to f4_fields-fieldname.
    move ' ' to f4_fields-selectflag.
    append f4_fields.

call function 'HELP_VALUES_GET_NO_DD_NAME'
     exporting
          selectfield                   = 'LGART'
    importing
          ind                           = index
*       select_value                       =
     tables
          fields                        = f4_fields
          full_table                    = swgtyp.
  if index > 0.
    clear s_lgart.  refresh s_lgart.
  endif.

  read table swgtyp index index.
  wgtyp_value = swgtyp-lgart.
  move wgtyp_value to s_lgart-low.

at selection-screen on value-request for s_lgart-high.

  perform s_lgart_val tables swgtyp
                             s_lgart
                       using p_molga-low
                             p_frmtyp
                             p_end.


    clear f4_fields.  refresh f4_fields.
    move 'T512W' to f4_fields-tabname.
    move 'LGART' to f4_fields-fieldname.
    move 'X' to f4_fields-selectflag.
    append f4_fields.
    move 'T512T' to f4_fields-tabname.
    move 'LGTXT' to f4_fields-fieldname.
    move ' ' to f4_fields-selectflag.
    append f4_fields.

call function 'HELP_VALUES_GET_NO_DD_NAME'
     exporting
          selectfield                   = 'LGART'
    importing
          ind                           = index
*     select_value                       =
     tables
          fields                        = f4_fields
          full_table                    = swgtyp.

      read table swgtyp index index.
      wgtyp_value = swgtyp-lgart.
move wgtyp_value to s_lgart-high.


at selection-screen output.
  if wtype2 is initial.
    if not s_lgart[] is initial.
      message i016(rp) with text-wt5.
      wtype1 = ' '.
      wtype2 = 'X'.
      clear p_forms.
      refresh p_forms.
    endif.
  endif.
  if wtype1 is initial.
    if not p_forms is initial.
      wtype1 = 'X'.
      wtype2 = ' '.
      clear s_lgart.
      refresh s_lgart.
      message i016(rp) with text-wt6.
    endif.
  endif.

  p_form = p_forms-low.
  loop at screen.
    if screen-group1 = '015'.
      screen-input = '0'.
      modify screen.
    endif.
  endloop.

  case p_emptax.
    when 'A'.
       d_emptax = 'Employee taxes'(312).
    when 'B'.
       d_emptax = 'Employer taxes'(313).
    when 'C'.
       d_emptax = 'Employee and Employer taxes'(314).
    when space.
       d_emptax = space.
  endcase.

  if wtype1 = 'X'.                         " NT. 0363102
*    frm_btn = 'Ad hoc WT selection'(308).  " NT. 0363102
    if p_forms-low is initial.
      read table p_forms index 1.
    endif.
    loop at rpt_form where form = p_forms-low. endloop.
    if sy-subrc = 0.
      rpt_typ = rpt_form-text.
    else.
      perform re5963 tables rpt_form
                      using p_molga+3(2).
      loop at rpt_form where form = p_forms-low. endloop.
      if sy-subrc = 0.
        rpt_typ = rpt_form-text.
      else.
        rpt_typ = space.
      endif.
    endif.
    p_forms-low = p_form.
    loop at screen.
      if screen-group1 = '011'.
        screen-input = '0'.
        modify screen.
      endif.
      if screen-group1 = '010'.
        screen-input = '1'.
        modify screen.
      endif.
    endloop.
  else.
    p_form      = space.
    p_forms-low = space.
*    frm_btn = 'Pre-defined WT selection'(309). " NT. 0363102
    rpt_typ = space.
    loop at screen.
      if screen-group1 = '010'.
        screen-input = '0'.
        modify screen.
      endif.
      if screen-group1 = '011'.
        screen-input = '1'.
        modify screen.
      endif.
    endloop.
  endif.

  if p_void = 'X'.
    3pr_btn = 'No 3PR'(105).
    loop at screen.
      if screen-group1 = '001'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  else.
    3pr_btn = 'Using 3PR'(104).
    loop at screen.
      if screen-group1 = '001'.
        screen-input = '1'.
        modify screen.
      endif.
    endloop.
  endif.

*----------------------------------------------------------------------*
* initialize SEL_TAB for selection-popup                               *
*----------------------------------------------------------------------*
form screen_initialization.
  repid   = sy-repid.
  rp_curr = 'USD'.
  rp_langu = sy-langu.
  cnt_x = 0.
  pagno = 1.
* constants for fieldnames according to each sort-ID
  p_sort = 'Reportable fields'(B01).
  data: t_01(30),
        t_02(30),
        t_03(30),
        t_04(30),
        t_05(30),
        t_06(30),
        t_07(30),
        t_08(30),
        t_09(30),
        t_10(30),
        t_11(30),
        t_12(30).

  t_01 = 'Company code'(P01).
  t_02 = 'Personnel area'(P02).
  t_03 = 'Personnel subarea'(P03).
  t_04 = 'Cost center'(P04).
  t_05 = 'Payroll area'(P05).
  t_06 = 'Employee group'(P06).
  t_07 = 'Employee subgroup'(P07).
  t_08 = 'Organizational key'(P08).
  t_09 = 'Employment status '(P09).
  t_10 = 'Personnel number'(P10).
  t_11 = 'Tax company'(P11).
  t_12 = 'Tax authority'(P12).

  refresh sel_tab.
  perform fill_sel_tab using 'A' 'P0001' 'BUKRS'  t_01.
  perform fill_sel_tab using 'B' 'P0001' 'WERKS'  t_02.
  perform fill_sel_tab using 'C' 'P0001' 'BTRTL'  t_03.
  perform fill_sel_tab using 'D' 'P0001' 'KOSTL'  t_04.
  perform fill_sel_tab using 'E' 'P0001' 'iabkrs'  t_05.
  perform fill_sel_tab using 'F' 'P0001' 'PERSG'  t_06.
  perform fill_sel_tab using 'G' 'P0001' 'PERSK'  t_07.
  perform fill_sel_tab using 'H' 'P0001' 'VDSK1'  t_08.
  perform fill_sel_tab using 'I' 'P0000' 'STAT2'  t_09.
  perform fill_sel_tab using 'J' 'P0001' 'PERNR'  t_10.
  perform fill_sel_tab using 'K' ''      'TXCMP'  t_11.
  perform fill_sel_tab using 'L' ''      'TAXAU'  t_12.
  if p_srtopt is initial.
    option = 'K/L'.
  else.
    option = p_srtopt.
  endif.
endform.                               " SCREEN_INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  FILL_SEL_TAB
*&---------------------------------------------------------------------*
form fill_sel_tab using value(v1) value(v2) value(v3) value(v4).

  sel_tab-shortstrg  = v1.
  sel_tab-optiontext = v4.
  sel_tab-tabname    = v2.
  sel_tab-fieldname  = v3.
  append sel_tab.

endform.                               " FILL_SEL_TAB
*&---------------------------------------------------------------------*
*&      Form  RE596B
*&---------------------------------------------------------------------*
form re5963 tables p_forms
             using p_molga.

  select * from t5963 where molga = p_molga
                        and sagrp = 'PRRU'.

    select * from t596b where appl = t5963-appl
                        and sprsl = sy-langu
                        and molga = p_molga.
       move t596b-appl to rpt_form-form.
       move t596b-text to rpt_form-text.
       collect rpt_form.
    endselect.
  endselect.
endform.                    " RE596B

*&---------------------------------------------------------------------*
*&      Form  Build_Tax_Forms_Table
*&---------------------------------------------------------------------*
form build_tax_forms_table tables   p_forms structure rpt_form
                           using    p_trfrm
                                    value(p_text).
  move p_trfrm to p_forms-form.
  move p_text to p_forms-text.
  append p_forms.
endform.                    " Build_Tax_Forms_Table
