*&---------------------------------------------------------------------*
*& Report  ZDI_BF_WIP
*&
*&---------------------------------------------------------------------*
*&
*& RP from material-default supply area is not accurate for common parts
*& RP is in BOM -
*&---------------------------------------------------------------------*
*
* spec by Andy Choi
*

report  zdi_bf_wip message-id zmpp.

tables: cpzp, ckmlmv013, mbew, marv, *cpzp.

define __popup.
  perform pop_up using
  &1 &2 &3
  changing l_answer.
end-of-definition.

data: g_kokrs like tka02-kokrs,
      g_bukrs like t001k-bukrs.

data: begin of it_itab occurs 0,
        objnr   like cpzp-objnr ,
        f_objnr like cpzp-f_objnr ,
        gjper   like cpzp-gjper ,   "yyyymmm
        istmn   like cpzp-istmn ,   "PrevWIP + Current Input
        gmper   like cpzp-gmper ,   "Current Input
        gmsum   like cpzp-gmsum ,   "Current Output
        varmn   like cpzp-varmn ,   "Variance
        xmper   like cpzp-xmper ,   "current scrap
        xmsum   like cpzp-xmsum ,   "total scrap
        meinh   like cpzp-meinh ,   "Unit of measure for operation
        aufnr   like aufk-aufnr ,
        pmatn   like ckmlmv013-pmatn ,
        verid   like ckmlmv013-verid,
        wip     like cpzp-gmsum ,   "WIP=ISTMN-GMSUM
        wipamt  type salk3,
        peinh   type peinh,
        stprs   type stprs,
        typps(2) type c,            "VS-material, KL-activity
        compn(18) type c,
        vspvb(10) type c,
        rp(4)     type c,
        istmn0  like cpzp-istmn ,   "PrevWIP
        istmnr  like cpzp-istmn ,   "PrevWIP from Table
        err,
      end of it_itab.

data: begin of it_prvbe occurs 0,
        prvbe    type prvbe,
        key2     like ztpp0001-key1,
        rp(4)    type c,
      end of it_prvbe.

data: begin of it_wc occurs 0,
        arbpl  type arbpl,     "crhd-ARBPL work center
        kostl  type kostl,
        sortb  type sortb,
        matyp  type matyp,
      end of it_wc.

data: begin of it_kal occurs 0,
        kaln1   like mbew-kaln1,
        f_objnr like cpzp-f_objnr,
      end of it_kal.

data: begin of it_objid occurs 0,
        objid   like ppc_comat-objid,
      end of it_objid.

data: begin of it_comat occurs 0,
        objid   like ppc_comat-objid,
        matnr   like ppc_comat-matnr,
        vspvb   type vspvb,
      end of it_comat.

data: begin of it_mbew occurs 0,
        kaln1   like mbew-kaln1,
        matnr   like mbew-matnr,
        vspvb   type vspvb,
        f_objnr like cpzp-f_objnr,
      end of it_mbew.


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

*&---------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-sl1.
parameters: p_bwkey  like t001k-bwkey memory id bwk obligatory.

select-options:
       s_objnr   for cpzp-objnr    ,
       s_fobjn   for cpzp-f_objnr  ,
       s_gjper   for cpzp-gjper    .
selection-screen end of block b1.

selection-screen begin of block b3 with frame title text-sl2.
select-options:
       s_aufnr   for ckmlmv013-aufnr memory id anr,
       s_pmatn   for ckmlmv013-pmatn ,
       s_verid   for ckmlmv013-verid ,
       s_matnr   for mbew-matnr.
selection-screen end of block b3.

parameters: p_wip as checkbox.
parameters: p_err as checkbox.
*default supply area from RP routing
parameters: p_src as checkbox default 'X'.

*not implemented...
parameters: p_upd type char1 no-display.

*&---------------------------------------------------------------------*
initialization.
*&---------------------------------------------------------------------*
  concatenate sy-datum(4) '0' sy-datum+4(2) into s_gjper-low.
  s_gjper-sign = 'I'.
  s_gjper-option = 'EQ'.
  append s_gjper.

*&---------------------------------------------------------------------*
start-of-selection.
*&---------------------------------------------------------------------*
  perform get_org_info.

  if p_upd eq 'X'.
    perform chk_run_date.
  endif.

  perform get_basic_info.
  perform select_wip.
  perform fill_info_itab.

  if p_err = 'X'.
    delete it_itab where err = space.
  endif.

  perform display_out.

*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
form display_out.

  perform field_setting tables gt_fieldcat using :
'TYPPS'   'Type'           '02' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'RP'      'RP'             '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'VSPVB'   'Def.Suppl'      '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'COMPN'   'Component'      '18' ' ' 'L'  ' '  ' '  'MARA' 'MATNR'  'R',
'ISTMNR'  'PrWIP-Tbl'      '14' ' ' 'R'  ' '  ' '  '  ' 'MEINH '  ' ',
'ISTMN0'  'PrWIP'          '14' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'ISTMN'   'PrW+In(ISTMN)'  '14' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'GMPER'   'Input(GMPER)'   '14' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'GMSUM'   'Output(GMSUM)'  '14' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'WIP'     'WIP'            '14' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'PEINH'   'Unit'           '14' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'STPRS'   'STPRS'          '14' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'WIPAMT'  'WIPAmt'         '14' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
'MEINH'   'MEINH'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'VARMN'   'Variance'       '10' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'XMPER'   'Scrap'          '10' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'XMSUM'   'ScrapSum'       '10' ' ' 'R'  ' '  ' '  '  ' 'MEINH'  ' ',
'ERR'     'Error'          '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'GJPER'   'YYYYMM'         '07' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'PMATN'   'Product'        '21' ' ' 'L'  ' '  ' '  'MARA' 'MATNR'  'R',
'VERID'   'VERID'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'AUFNR'   'Order'          '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'OBJNR'   'OBJNR'          '14' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
'F_OBJNR' 'F_OBJNR'        '17' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = g_repid
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
    tables
      t_outtab           = it_itab
    exceptions
      program_error      = 1
      others             = 2.

endform.                    " display_out
*&---------------------------------------------------------------------*
*&      Form  select_wip
*&---------------------------------------------------------------------*
form select_wip.

  tables: ppc_comat.
  ranges: r_objnr for cpzp-f_objnr.

  describe table s_matnr lines sy-tabix.
  if sy-tabix > 0.
    r_objnr-option = 'EQ'.
    r_objnr-sign   = 'I'.
    select * from ppc_comat where matnr in s_matnr
                              and werks = p_bwkey .
      r_objnr-low(4)    = 'MK00'.
      r_objnr-low+4(10) = ppc_comat-objid.
      append r_objnr.
    endselect.
  endif.

  select cpzp~objnr
         cpzp~f_objnr cpzp~gjper
         cpzp~istmn cpzp~gmper cpzp~gmsum
         cpzp~varmn
         cpzp~xmper cpzp~xmsum
         cpzp~meinh
         aufk~aufnr
         ckmlmv013~pmatn ckmlmv013~verid
  into it_itab
  from ( cpzp
         inner join aufk
            on aufk~objnr = cpzp~objnr
         inner join afpo
            on afpo~aufnr = aufk~aufnr
         inner join ckmlmv013
            on ckmlmv013~aufnr = aufk~aufnr )
         where afpo~rtp04      = 'X'           "APO order only
           and afpo~dwerk      = p_bwkey
           and cpzp~objnr      in s_objnr
           and cpzp~f_objnr    in s_fobjn
           and cpzp~f_objnr    in r_objnr
           and cpzp~gjper      in s_gjper
           and ckmlmv013~aufnr in s_aufnr
           and ckmlmv013~pmatn in s_pmatn
           and ckmlmv013~verid in s_verid.

    it_itab-wip    = it_itab-istmn - it_itab-gmsum.
    it_itab-istmn0 = it_itab-istmn - it_itab-gmper.
    if p_wip = ' ' or it_itab-wip <> 0.
      append it_itab.
    endif.

  endselect.

endform.                    " select_wip

*&---------------------------------------------------------------------*
*&      Form  check_run_date
*&---------------------------------------------------------------------*
form chk_run_date.
  data: h_dontpanic like sy-datlo.

  get parameter id 'DONTPANIC' field h_dontpanic.
  if h_dontpanic <> sy-datlo.
    message e000 with 'stop!'.
    stop.
  endif.

  data : $year(4) type n,
         $mon(3) type n,
         yvper type co_gjper.

  select single * from marv where bukrs = g_bukrs.
  if sy-subrc eq 0.
    $year = marv-lfgja.
    $mon = marv-lfmon.
    concatenate $year $mon into yvper.

    if s_gjper-low <> yvper.

      $year = marv-vmgja.
      $mon = marv-vmmon.
      concatenate $year $mon into yvper.
      if s_gjper-low <> yvper.
        message e000 with 'The date must be greater or equal to current Date!'.
        stop.
      endif.
    else.
      exit.
    endif.

  else.
    message e000 with 'Invalid Company!'.
    stop.
  endif.
endform.                    " check_run_date

*---------------------------------------------------------------------*
*       FORM pop_up                                                   *
*---------------------------------------------------------------------*
form pop_up using    p_text p_text2 p_canc
            changing p_answer.
  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
      textline1      = p_text
      textline2      = p_text2
      titel          = 'Check!'
      cancel_display = p_canc
    importing
      answer         = p_answer.

endform.                    " POP_UP
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables         p_fieldcat_t like gt_fieldcat
                   using          p_fieldname       " FIELD name
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

  if p_dosum = 'R'. "reference field / table
    ls_fieldcat-ref_tabname   = p_cfield.
    ls_fieldcat-ref_fieldname = p_qfield.
  else.
    ls_fieldcat-currency   = p_cfield.
    ls_fieldcat-qfieldname = p_qfield.

  endif.
  ls_fieldcat-do_sum     = p_dosum.

  append ls_fieldcat to p_fieldcat_t.

endform.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  FILL_INFO_ITAB
*&---------------------------------------------------------------------*
form fill_info_itab .
  data: l_idx like sy-tabix.
  data: l_matnr like mara-matnr,
        l_kostl like crco-kostl.
  data: l_objid like ppc_comat-objid.

  data: l_bdatj type bdatj,
        l_poper type poper,
        l_stprs type stprs,
        l_peinh type peinh.
*  LOOP AT it_itab.
*    IF it_itab-f_objnr(2) = 'VS'.
*      it_kal-kaln1   = it_itab-f_objnr+4(12).
*      it_kal-f_objnr = it_itab-f_objnr.
*      APPEND it_kal.
*    ENDIF.
*  ENDLOOP.
*  SORT it_kal BY kaln1.
*  DELETE ADJACENT DUPLICATES FROM it_kal.
*
*  SELECT a~kaln1 a~matnr c~vspvb
*    INTO TABLE it_mbew
*    FROM mbew AS a
*    INNER JOIN marc AS c
*       ON c~matnr = a~matnr
*      AND c~werks = a~bwkey
*    FOR ALL ENTRIES IN it_kal
*    WHERE a~kaln1 = it_kal-kaln1.
*
*  LOOP AT it_mbew.
*    l_idx = sy-tabix.
*    READ TABLE it_kal WITH KEY kaln1 = it_mbew-kaln1 BINARY SEARCH.
*    it_mbew-f_objnr = it_kal-f_objnr.
*    MODIFY it_mbew INDEX l_idx TRANSPORTING f_objnr.
*  ENDLOOP.
*  SORT it_mbew BY f_objnr.

  loop at it_itab.
    if it_itab-f_objnr(2) = 'MK'.
      it_objid-objid = it_itab-f_objnr+4(10).
      append it_objid.
    endif.
  endloop.
  sort it_objid by objid.
  delete adjacent duplicates from it_objid.
  select a~objid a~matnr c~vspvb into table it_comat
      from ppc_comat as a
      inner join marc as c
       on c~matnr = a~matnr
      and c~werks = a~werks
      for all entries in it_objid
      where a~objid = it_objid-objid.
  sort it_comat by objid.

  loop at it_itab.
    l_idx = sy-tabix.
    it_itab-typps = it_itab-f_objnr(2).
    l_bdatj = it_itab-gjper(4).
    l_poper = it_itab-gjper+4(3).

    if it_itab-typps = 'MK'.
      l_objid = it_itab-f_objnr+4(10).
      clear it_comat.
      read table it_comat with key objid = l_objid binary search.
      if sy-subrc <> 0.
*----  critical error
        it_itab-err = 'M'.
        modify it_itab index l_idx transporting err.
      else.
        it_itab-compn = it_comat-matnr.
        it_itab-vspvb = it_comat-vspvb.

        read table it_prvbe with key prvbe = it_itab-vspvb binary search.
        if sy-subrc = 0.
          it_itab-rp    = it_prvbe-rp.
        endif.

        clear: l_stprs, l_peinh, it_itab-wipamt.
        select single stprs peinh
           into (l_stprs, l_peinh) from ckmlcr
          inner join ckmlhd
             on ckmlhd~kalnr = ckmlcr~kalnr
          where matnr = it_itab-compn
            and bwkey = p_bwkey
            and bdatj = l_bdatj
            and poper = l_poper.

        if l_peinh > 0.
          it_itab-wipamt = it_itab-wip * l_stprs / l_peinh.
          it_itab-stprs = l_stprs.
          it_itab-peinh = l_peinh.
        endif.

        modify it_itab index l_idx transporting typps compn vspvb rp wipamt stprs peinh.

      endif.

    else. "cost center + activity type
      it_itab-compn = it_itab-f_objnr+6(16).

      l_kostl = it_itab-f_objnr+6(10).
      read table it_wc with key kostl = l_kostl binary search.
      if sy-subrc = 0.
        it_itab-rp    = it_wc-sortb.
      endif.
      modify it_itab index l_idx transporting typps compn rp.
    endif.

  endloop.


*    CALL FUNCTION 'QRP_APO_COMP_OBJNR_DECODE'
*      EXPORTING
*        if_f_objnr            = it_itab-f_objnr
*        IF_COMPLETE_KEY       = 'X'
*      IMPORTING
*       EF_MATNR              = L_MATNR
**     EXCEPTIONS
**       NOT_FOUND             = 1
**       OTHERS                = 2

endform.                    " FILL_INFO_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_BASIC_INFO
*&---------------------------------------------------------------------*
form get_basic_info .
  tables: ztpp0001.
  data: l_idx like sy-tabix.
  data: lt_05 like ztpp0001 occurs 0 with header line.

  refresh: it_prvbe.

*KMMG logic
  if p_src = space.
*--read supply area
    select key1 key2  into table it_prvbe
        from ztpp0001 where code = '07'.
*--read MES reporting point
    select * into table lt_05
        from ztpp0001 where code = '05'.
    sort lt_05 by key1.
    loop at it_prvbe.
      l_idx = sy-tabix.
      read table lt_05 with key key1 = it_prvbe-key2 binary search.
      if sy-subrc = 0.
        it_prvbe-rp = lt_05-key2+2(2).
        modify it_prvbe index l_idx transporting rp.
      endif.
    endloop.

  else.
    select usr00 usr02 usr01  into table it_prvbe
            from plpo
             where  plnty eq 'M'
                and plnnr eq 'RP'      "HMMA
                and werks eq p_bwkey.
*                AND nvadd NE 'X'.
  endif.
  sort it_prvbe by prvbe.


* work center - cost center
  select arbpl kostl sortb matyp into table it_wc
  from crco as a
    join crhd as b
      on a~objty = b~objty
     and a~objid = b~objid
   where b~verwe = '0007'            "production work center
     and a~kokrs = g_kokrs.
  sort it_wc by kostl.

endform.                    " GET_BASIC_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_ORG_INFO
*&---------------------------------------------------------------------*
form get_org_info .
  tables: t001k.

  select single bukrs into g_bukrs from t001k where bwkey = p_bwkey.

  select single kokrs into g_kokrs from tka02
         where bukrs = g_bukrs.

endform.                    " GET_ORG_INFO
