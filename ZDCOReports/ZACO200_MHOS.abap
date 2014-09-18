*&--------------------------------------------------------------------
*& REPORT                 : ZACO200_MHOS
*& Author                 : HSJung
*& Creation Date          : 08/22/2006
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description            :
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------
report zaco200_mhos message-id  zmco.
*
* NOTICE from Andy
* - Must select service master which is relevant for HPV calculation

* Service Entry Assumption  (effective 8/13/2007)
*  Price ref    08/07/2007 -> working week ending date
*  Document date           -> invoice date of invoice
*  Posting date            -> current date
*
* For TOP include
*nclude zaco200_mhos_top .
*----------------------------------------------------------------------*
*   INCLUDE ZACO200_MHOS_TOP                                           *
*----------------------------------------------------------------------*
tables : ztco_mhos, asmd.
*MANDT	Client
*KOKRS	Controlling Area
*BDATJ	Posting date YYYY
*POPER	Posting period
*KOSTL	Cost Center
*SRVPOS	Service number
*EBELN	Purchasing Document Number
*EBELP	Item Number of Purchasing Document
*ACCRL	Accrual?
*RETRO	Field relevant for retrocalculation
*RETYM	Period/year (for retro)
*LIFNR	Account number of vendor or creditor
*SAKTO	G/L account number
*AUFNR	Order Number
*TBTWR	Amount in local currency
*MENGE	Working Hour
*MEINS	Base unit of measure
*HEAD_CNT	Head Count
*.INCLUDE	Time Stamp Structure


data : begin of it_dates occurs 0.
        include structure  casdayattr.
data :  week like scal-week,
        monday type datum,
        sunday type datum,
       end of it_dates.


data : begin of it_essr occurs 0,
        lblni   like essr-lblni,
        ebeln   like essr-ebeln,
        ebelp   like essr-ebelp,
        packno  like essr-packno,
        lblni_packno like eskn-packno,
        budat   type datum,  "posting date
        bldat   type datum,  "invoice date
        xblnr   like essr-xblnr,  "ref: old='INV: date' format
        LZVON   like essr-lzvon,  "start of period
        LZBIS   like essr-LZBIS,  "end of period
        LBLDT   like essr-LBLDT,  "price ref. date
       end of it_essr .


data : begin of it_eskn occurs 0,
        packno  like eskn-packno,
        kostl   like eskn-kostl,
        sakto   like eskn-sakto,
        aufnr   like eskn-aufnr,
        end of it_eskn .

data : begin of it_eskn2 occurs 0,
        aufnr   like eskn-aufnr,
        end of it_eskn2 .

data : begin of it_coas occurs 0,
        aufnr   like eskn-aufnr,
        cycle   like coas-cycle,
        akstl   like coas-akstl,
        kostv   like coas-kostv,
        end of it_coas .

data : begin of it_lfa1 occurs 0,
        ebeln   like ekko-ebeln,
        lifnr   like ekko-lifnr,
        name1   like lfa1-name1,
        end of it_lfa1 .


data : begin of it_esll_temp occurs 0,
        packno      like esll-packno,
        sub_packno  like esll-sub_packno,
        end of it_esll_temp .

data : begin of it_esll occurs 0,
        packno  like esll-packno,
        srvpos  like esll-srvpos,
        menge   like esll-menge,
        meins   like esll-meins,
        tbtwr   like esll-tbtwr,
        end of it_esll .

*data : itab like ztco_mhos occurs 0 with header line.
data : begin of itab occurs 0.
        include structure  ztco_mhos.
*data : LZBIS type datum,  "end period
data:  end of itab.

data : it_mhos like ztco_mhos occurs 0 with header line.


data : begin of it_weeks occurs 0,
        week        like scal-week,
        yymm(6)     type c,
        monday      type datum,
        sunday      type datum,
*        date        TYPE datum,
        tot_wk_day  type i, "p decimals 0,
        this_wk_day type i, "p decimals 0,
        prev_wk_day type i, "p decimals 0,
       end of it_weeks .

data : begin of it_head_temp occurs 0,
        kostl       like ztco_mhos-kostl,
        srvpos      like ztco_mhos-srvpos,
        ebeln       like ztco_mhos-ebeln,
        ebelp       like ztco_mhos-ebelp,
        LBLNI       like ztco_mhos-LBLNI,
        week        like scal-week,
        menge       like ztco_mhos-menge,
        end of it_head_temp .


data : begin of it_head occurs 0,
        BDATJ       like ztco_mhos-BDATJ,
        poper       like ztco_mhos-poper,
        kostl       like ztco_mhos-kostl,
        srvpos      like ztco_mhos-srvpos,
        ebeln       like ztco_mhos-ebeln,
        ebelp       like ztco_mhos-ebelp,
        LBLNI       like ztco_mhos-LBLNI,
        week        like scal-week,
        menge       like ztco_mhos-menge,

        tot_wk_day  type p decimals 0,
        this_wk_day type p decimals 0,
        week_rat    type p decimals 4,
        this_menge  like ztco_mhos-menge,
*        pre_menge   LIKE ztco_mhos-menge,
*        next_menge  LIKE ztco_mhos-menge,
        head_cnt    type p decimals 4,
        end of it_head .


data : w_fdate type datum,
       w_tdate type datum.

*posting start/end date
data : gw_fdate type datum,
       gw_tdate type datum,
       gw_mindt type datum,
       gw_maxdt type datum.

ranges: r_tempsr for asmd-asnum.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
* General Info.
parameters : p_kokrs like csks-kokrs   memory id cac  obligatory
               default 'H201'.
*SELECTION-SCREEN SKIP 1.
* Posted Yr.
parameters : p_bdatj like keko-bdatj memory id bdtj obligatory.
parameters : p_poper like covja-perab memory id vpe
             modif id per obligatory ."DEFAULT '10'.

parameters : p_approv as checkbox default 'X',
             p_wkend  as checkbox default 'X'.

selection-screen begin of block bl2 with frame title text-002.
select-options : s_lblni       for  ztco_mhos-lblni,
                 s_ebeln       for  ztco_mhos-ebeln,
                 s_ebelp       for  ztco_mhos-ebelp,
                 s_kostl       for  ztco_mhos-kostl,
                 s_srvpos      for  ztco_mhos-srvpos.


selection-screen end of block bl2.
selection-screen end of block bl1.

select-options : s_tempsr      for  ztco_mhos-srvpos.
parameters: p_test as checkbox default ' '.

initialization.
  s_tempsr-option = 'EQ'.
  s_tempsr-sign   = 'I'.
  s_tempsr-low    = 'HR*'.
  append s_tempsr.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

  perform determine_select_date_range.

  perform select_data.

  perform read_calendar.

  perform create_itab.

  if p_test = ' '.
    perform update_ztco_mhos.
  else.
    PERFORM display_out.
  endif.

*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data.

* po no service pack PACKNO
  if p_approv = 'X'.
   select lblni ebeln ebelp packno  budat bldat xblnr LZVON LZBIS LBLDT
                             into corresponding fields of table it_essr
                                      from essr
                              where budat between gw_fdate and gw_tdate
                                         and loekz = ''
                                       and kzabn = 'X'   " Accepted Only
                                         and lblni in s_lblni
                                         and ebeln in s_ebeln
                                         and ebelp in s_ebelp.
  else.
    select lblni ebeln ebelp packno budat bldat xblnr LZVON LZBIS LBLDT
           into corresponding fields of table it_essr
            from essr
             where budat between gw_fdate and gw_tdate
               and loekz = ''
               and lblni in s_lblni
               and ebeln in s_ebeln
               and ebelp in s_ebelp.
  endif.

  check not it_essr[] is initial.


***  data: l_day(2) type n, l_mon(2) type n, l_year(4) type n, l_yrln.
***  loop at it_essr.
***    it_essr-lblni_packno = it_essr-lblni.
***
****old data conversion: FIXME
***    if it_essr-budat < '20070812' and it_essr-xblnr(3) = 'INV'.
***      split it_essr-xblnr+4(12) at '/' into l_mon l_day l_year.
***      l_yrln = strlen( l_year ).
***      if l_yrln = 4.
***        concatenate l_year l_mon l_day into it_essr-bldat.
***      elseif l_yrln = 2.
***          concatenate '20' l_year l_mon l_day into it_essr-bldat.
***      endif.
***    endif.
****----------end: FIXME
***
***    modify it_essr. clear it_essr.
***  endloop.


  loop at it_essr.
    it_essr-lblni_packno = it_essr-lblni.


* by IG.MOON 8/9/2007 {
* must be deleted after 2019.
    if it_essr-budat < '20070812' and
        ( it_essr-xblnr(3) cp 'INV*' or it_essr-xblnr(3) cp '*INV*' ).

      perform make_date_with_string using it_essr-xblnr
                                 changing it_essr-bldat .
* }
    endif.

    if it_essr-LZBIS is initial.
      it_essr-LZBIS = it_essr-bldat.
      perform get_service_period changing it_essr-LZVON it_essr-LZBIS .
    endif.
    if it_essr-LZVON is initial.
      perform get_service_period changing it_essr-LZVON it_essr-LZBIS .
    endif.

    modify it_essr. clear it_essr.
  endloop.

* For Cost center
  select packno kostl aufnr sakto
         into corresponding fields of table it_eskn
          from eskn
          for all entries in it_essr
          where packno = it_essr-lblni_packno
            and loekz = '' .

* If Cost center not exist in ESKN => serach in COAS using AUFNR
  loop at it_eskn where kostl = ''.
    move-corresponding it_eskn to it_eskn2.
    collect it_eskn2. clear it_eskn2.
  endloop.

  if not it_eskn2[] is initial.
    select aufnr cycle  akstl  kostv
         into corresponding fields of table it_coas
          from coas
          for all entries in it_eskn2
          where aufnr = it_eskn2-aufnr.
  endif.

* Cost center : CYCLE => AKSTL => KOSTV
  loop at it_eskn where kostl = ''.
    clear it_coas.
    read table it_coas with key aufnr = it_eskn-aufnr.
    if it_coas-cycle <> ''.
      it_eskn-kostl = it_coas-cycle.
    elseif it_coas-akstl <> ''.
      it_eskn-kostl = it_coas-akstl.
    else .
      it_eskn-kostl = it_coas-kostv.
    endif.
    modify it_eskn. clear it_eskn.
  endloop.


* Vendor

  select ebeln lifnr                                        "b~name1
         into corresponding fields of table it_lfa1
          from ekko "AS a
*         INNER JOIN lfa1 AS b
*            ON a~lifnr = b~lifnr
          for all entries in it_essr
          where ebeln  = it_essr-ebeln.

* 1. Select sub_packno using ESSR-PACKNO
  select packno sub_packno
         into corresponding fields of table it_esll_temp
          from esll
          for all entries in it_essr
          where packno = it_essr-packno
            and del = '' .


* 2. Select packno & other data using esll-sub_packno
*    Selecting JUST meins = STD(HR) !!
*    only select MEINS is TIME Type
*    'MON'= Month , 'WCH' = Week, '10' = Day , 'STD' = HR
  check not it_esll_temp[] is initial.
  select packno srvpos menge meins tbtwr
     into corresponding fields of table it_esll
          from esll
          for all entries in it_esll_temp
           where packno = it_esll_temp-sub_packno
             and del = ''
             and meins in ('MON' , 'WCH', '10', 'STD')
             and menge <> 0.

* Meins change to STD
  data: l_idx like sy-tabix.
  loop at it_esll.
    l_idx = sy-tabix.

    it_esll-tbtwr = it_esll-tbtwr * it_esll-menge.
    if it_esll-meins = 'MON'.
      it_esll-menge = it_esll-menge * 160.
      it_esll-meins = 'STD'.
    elseif it_esll-meins = 'WCH'.
      it_esll-menge = it_esll-menge * 40.
      it_esll-meins = 'STD'.
    elseif it_esll-meins = '10'.
      it_esll-menge = it_esll-menge * 8.
      it_esll-meins = 'STD'.
    endif.

    modify it_esll index l_idx  transporting menge meins tbtwr.
    clear it_esll.
  endloop.

endform.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  create_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_itab.

* collect time
  perform collect_itab.

* calc headcount
  r_tempsr-low = 'I'.
  r_tempsr-option = 'EQ'.
  select * from asmd
    where asnum in s_tempsr.
    r_tempsr-low = asmd-asnum.
    append r_tempsr.
  endselect.
  perform get_head_count.

* prepare output
  perform create_it_mhos.

endform.                    " create_itab
*&---------------------------------------------------------------------*
*&      Form  determine_select_date_range
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form determine_select_date_range.
  concatenate p_bdatj p_poper+1(2) '01' into gw_fdate.

* Get From day & Get To day
  call function 'RP_LAST_DAY_OF_MONTHS'
       EXPORTING
            day_in            = gw_fdate
       IMPORTING
            last_day_of_month = gw_tdate.

  gw_mindt = gw_fdate.
  gw_maxdt = gw_tdate.

endform.                    " determine_select_date_range
*&---------------------------------------------------------------------*
*&      Form  get_first
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FWEEK  text
*      <--P_W_FDATE  text
*----------------------------------------------------------------------*
form get_first using    p_week
               changing p_date.

  call function 'WEEK_GET_FIRST_DAY'
       EXPORTING
            week         = p_week
       IMPORTING
            date         = p_date
       EXCEPTIONS
            week_invalid = 1
            others       = 2.

endform.                    " get_first
*&---------------------------------------------------------------------*
*&      Form  collect_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form collect_itab.
  data: l_menge like ztco_mhos-MENGE,
        l_value type ACT_WERT.

  loop at it_esll.
    clear it_esll_temp.
    read table it_esll_temp with key sub_packno = it_esll-packno .
    check sy-subrc = 0 .
    clear it_essr.
    read table it_essr with key packno = it_esll_temp-packno.
    check sy-subrc = 0 .

    move-corresponding it_esll to itab.
    move-corresponding it_essr to itab.

*   Cost center
    clear it_eskn .
    read table it_eskn with key packno = it_essr-lblni.
    itab-kostl = it_eskn-kostl.
    itab-sakto = it_eskn-sakto.
    itab-aufnr = it_eskn-aufnr.
*   Vendor
    clear it_lfa1 .
    read table it_lfa1 with key ebeln = it_essr-ebeln.
    itab-lifnr = it_lfa1-lifnr.

    l_menge = itab-menge.
    l_value = itab-TBTWR.

* split month!!! - FIXME
    read table it_dates with key date = itab-LZBIS.
    read table it_weeks with key week = it_dates-week.

    if it_essr-LZVON(6) <> it_essr-LZBIS(6)
    and it_weeks-tot_wk_day <> 0.

*---portion of previous period
      itab-MENGE = l_menge *
                   it_weeks-prev_wk_day / it_weeks-tot_wk_day.
      itab-TBTWR = l_value *
                   it_weeks-prev_wk_day / it_weeks-tot_wk_day.

      concatenate it_essr-LZVON(4) '0' it_essr-LZVON+4(2)
             into itab-retym.   "format = YYYYMMM

      if it_weeks-monday(4) <> p_bdatj
      or it_weeks-monday+4(2) <> p_poper+1(2).
        itab-retro = 'X'.
      else.
        clear itab-retro.
      endif.

      if itab-TBTWR > 0. collect itab.  endif.

*---portion of current period
      itab-MENGE = l_menge *
                   it_weeks-this_wk_day / it_weeks-tot_wk_day.
      itab-TBTWR = l_value *
                   it_weeks-this_wk_day / it_weeks-tot_wk_day.

    else.
*"all holiday week--> collect to current period???
*      itab-MENGE = l_menge .
*      itab-TBTWR = l_value .
    endif.

    concatenate it_essr-LZBIS(4) '0' it_essr-LZBIS+4(2)
           into itab-retym.   "format = YYYYMMM


    if it_weeks-sunday(4) <> p_bdatj
    or it_weeks-sunday+4(2) <> p_poper+1(2).
      itab-retro = 'X'.
    else.
      clear itab-retro.
    endif.

    if itab-TBTWR > 0. collect itab.  endif.

    clear itab.
  endloop.

endform.                    " collect_itab
*&---------------------------------------------------------------------*
*&      Form  get_head_count
*&---------------------------------------------------------------------*
*  FIXME; limit to collect current period headcount...
*----------------------------------------------------------------------*
form get_head_count.

  data : l_head_cnt type p decimals 4.



  loop at itab .
    check itab-srvpos in r_tempsr.
    check itab-srvpos+7(1) = 'R'. "regular working.
    move-corresponding itab to it_head_temp.
    clear it_dates.
    read table it_dates with key date = itab-LZBIS.

    it_head_temp-week      = it_dates-week.
    it_head_temp-menge     = itab-menge.
    collect it_head_temp. clear it_head_temp.
  endloop.

  sort it_head_temp.
  loop at it_head_temp.

    move-corresponding it_head_temp to it_head.
    clear it_weeks.
    read table it_weeks with key week = it_head_temp-week.

    it_head-tot_wk_day   = it_weeks-tot_wk_day.
    it_head-this_wk_day  = it_weeks-this_wk_day.

    it_head-week_rat     = it_head-this_wk_day / it_head-tot_wk_day.
    it_head-this_menge   = it_head-menge * it_head-week_rat.
*    it_head-pre_menge    = it_head-menge - it_head-this_menge.
*    it_head-next_menge   = it_head-menge - it_head-this_menge.
    it_head-head_cnt     = it_head-this_menge /
                                 ( it_head-this_wk_day * 8 )  .

*---- scaling to monthly average headcount???
    it_head-head_cnt     = it_head-head_cnt / 4.

*    l_head_cnt = l_head_cnt + it_head-head_cnt.

    collect it_head. clear it_head.

*    at end of ebelp.
*      clear it_head-week.
*      it_head-head_cnt = l_head_cnt / 4.
*      collect it_head.
*      clear : it_head, l_head_cnt.
*    endat.
  endloop.


endform.                    " get_head_count
*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_it_mhos.
  loop at itab.
    move-corresponding itab to it_mhos.

    read table it_head with key  kostl   = itab-kostl
                                 srvpos  = itab-srvpos
                                 ebeln   = itab-ebeln
                                 ebelp   = itab-ebelp
                                 LBLNI   = itab-LBLNI.
*???                             week    = ''.      "=> Total value
    if sy-subrc = 0.
      it_mhos-head_cnt = it_head-head_cnt.
    endif.

    it_mhos-BRTWR = it_mhos-TBTWR / it_mhos-menge.

    it_mhos-kokrs    = p_kokrs.
    it_mhos-bdatj    = p_bdatj.
    it_mhos-poper    = p_poper.
    it_mhos-erdat    = sy-datum.
    it_mhos-erzet    = sy-uzeit.
    it_mhos-ernam    = sy-uname.
    append it_mhos. clear it_mhos.
  endloop.

  sort it_mhos.

endform.                    " modify_itab
*&---------------------------------------------------------------------*
*&      Form  update_ztco_mhos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ztco_mhos.
  delete from ztco_mhos where kokrs  = p_kokrs
                          and bdatj  = p_bdatj
                          and poper  = p_poper
                          and kostl  in s_kostl
                          and srvpos in s_srvpos
                          and ebeln  in s_ebeln
                          and ebelp  in s_ebelp.

  commit work.
  insert ztco_mhos from table it_mhos.


  if sy-subrc = 0 .
    commit work.
    message s009.
  endif.

endform.                    " update_ztco_mhos
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_$STR  text
*----------------------------------------------------------------------*
form check_num_with_slash changing n_value.
  data num(12) value ' 0123456789/'.
  data $char(1).
  data $strlen type i.
  data $strlen2 type i.
  data $offset type i.

  condense n_value no-gaps.
  $strlen = strlen( n_value ).

  do $strlen times.
    $offset = sy-index - 1.
    $strlen2 =  strlen( n_value ).
    if $strlen2 <= $offset.
      exit.
    endif.
    $char = n_value+$offset(1).
    if $char cn num.
      replace $char with '' into n_value.
    endif.
  enddo.

  condense n_value no-gaps.

endform.                    " CHECK_NUM

*---------------------------------------------------------------------*
*       FORM check_pure_num                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  N_VALUE                                                       *
*---------------------------------------------------------------------*
form check_pure_num changing n_value.
  data num(12) value ' 0123456789'.
  data $char(1).
  data $strlen type i.
  data $strlen2 type i.
  data $offset type i.

  condense n_value no-gaps.
  $strlen = strlen( n_value ).

  do $strlen times.
    $offset = sy-index - 1.
    $strlen2 =  strlen( n_value ).
    if $strlen2 <= $offset.
      exit.
    endif.
    $char = n_value+$offset(1).
    if $char cn num.
      replace $char with '' into n_value.
    endif.
  enddo.

  condense n_value no-gaps.

endform.                    " CHECK_NUM

*&---------------------------------------------------------------------*
*&      Form  get_date_from_str
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$STR  text
*      <--P_L_MON  text
*      <--P_L_DAY  text
*      <--P_L_YEAR  text
*----------------------------------------------------------------------*
form get_date_from_str using    p_$str
                       changing p_l_mon
                                p_l_day
                                p_l_year.

  data : $year(4), $strlen type i , $offset type i,
         $md(4), $str(20).
  data : c_year(2).

  $str = p_$str.
  check $str ne space.

  $strlen = strlen( $str ).

* year
  if $strlen > 4.
    $offset = $strlen - 4.
  else.
    if $strlen > 3.
      $offset = $strlen - 3.
    else.
      if $strlen eq '3'.
        concatenate '0' $str(1) into p_l_mon.
        p_l_day = $str+1(1).
        concatenate '200' $str+2(1) into p_l_year.
      endif.
      exit.
    endif.
  endif.

  $year = $str+$offset(4).

  if $year(3) <> '200' and $year(3) <> '201'. " will work until 2019
    $offset = $strlen - 2.
    $year = $str+$offset(2).
    condense $year.

    c_year = sy-datum+2(2).
    perform check_pure_num changing $year.

    $strlen = strlen( $year ).

    if $year > c_year and $strlen > '1'.
      shift $year left.
      condense $year.
    endif.

    $strlen = strlen( $year ).
    if  $strlen = '1'.
      concatenate '200' $year into $year.
      $offset = $offset + 1.
    else.
      concatenate '20' $year into $year.
    endif.
    $str = $str($offset).
  else.
    $str = $str($offset).
  endif.

  p_l_year = $year.

* month & day
  $md = $str.

  split $md at '/' into p_l_mon p_l_day.

  perform check_pure_num changing : p_l_mon,
                                    p_l_day.

  if p_l_mon eq space or p_l_day eq space.

    perform check_pure_num changing $md.

    $strlen = strlen( $md ).
    if $strlen < 4.
      concatenate '0' $md into $md.
      if $md(2) eq '00'.
        shift $md left.
      endif.
    endif.
    p_l_mon = $md(2).
    p_l_day = $md+2.
  endif.

* make month
  condense p_l_mon.
  $strlen = strlen( p_l_mon ).
  if $strlen < 2.
    concatenate '0' p_l_mon into p_l_mon.
  endif.

* just in case.
  condense p_l_day.
  $strlen = strlen( p_l_day ).
  if $strlen < 2.
    concatenate '0' p_l_day into p_l_day.
  endif.

endform.                    " get_date_from_str
*&---------------------------------------------------------------------*
*&      Form  make_date_with_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ESSR_XBLNR  text
*      <--P_IT_ESSR_BLDAT  text
*----------------------------------------------------------------------*
form make_date_with_string using    p_source_str
                           changing p_target_str.

  data: l_day(2) type n, l_mon(2) type n, l_year(4) type n, l_yrln.
  data: $str(20).

  $str = p_source_str.

  perform : check_num_with_slash changing $str,
            get_date_from_str using $str
                           changing l_mon l_day l_year.

  concatenate l_year l_mon l_day into p_target_str.

endform.                    " make_date_with_string
*&---------------------------------------------------------------------*
*&      Form  get_service_period
*&---------------------------------------------------------------------*
FORM get_service_period CHANGING  f_LZVON
                                  f_LZBIS.

*FIXME
* Saturday? / Friday?
* Monday - Saturday

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
       EXPORTING
            DATE     = f_LZBIS
       IMPORTING
            MONDAY   = F_LZVON
            SATURDAY = f_LZBIS.

*CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*     EXPORTING
*          DATE      = f_LZBIS
*          DAYS      = '05'
*          MONTHS    = '00'
*          SIGNUM    = '-'
*          YEARS     = '00'
*     IMPORTING
*          CALC_DATE = f_LZVON.


  if gw_mindt is initial or f_lzvon < gw_mindt.
    gw_mindt = f_lzvon.
  endif.
  if gw_maxdt is initial or f_lzbis > gw_maxdt.
    gw_maxdt = f_lzbis.
  endif.

ENDFORM.                    " get_service_period
*&---------------------------------------------------------------------*
*&      Form  read_calendar
*&---------------------------------------------------------------------*
FORM read_calendar.


  data : l_fweek like scal-week,
         l_tweek like scal-week.

  data : lt_date like standard table of casdayattr
                                  with header line.

** Get From Week & Get To Week
*  perform get_week using     gw_fdate
*                   changing  l_fweek.
*
*  perform get_week using     gw_tdate
*                   changing  l_tweek.
*
** Get first day of 'From Week'
*  perform get_first using    l_fweek
*                    changing w_fdate .
** Get first day of 'To Week'
*  perform get_first using    l_tweek
*                    changing w_tdate .
*
** Get last day of 'To Week'
*  w_tdate = w_tdate + 6  .

*FIXME
  w_fdate = gw_mindt - 6.
  w_tdate = gw_maxdt + 6.

*holiday calendar - exit in T001P
  call function 'DAY_ATTRIBUTES_GET'
       EXPORTING
            factory_calendar = 'HM'
            holiday_calendar = 'U1'
            date_from        = w_fdate
            date_to          = w_tdate
       TABLES
            day_attributes   = lt_date.

* it_dates => LT_date + week
  loop at lt_date .
    move-corresponding lt_date to it_dates.

    CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
         EXPORTING
              DATE   = it_dates-date
         IMPORTING
              WEEK   = it_dates-week
              MONDAY = it_dates-monday
              SUNDAY = it_dates-sunday.

    append it_dates . clear it_dates.
  endloop.

* it_weeks => WEEK : Total work day & This week work day
* get first date of week, end date of week.
* count of period
  loop at it_dates.

    it_weeks-week   = it_dates-week.
    it_weeks-yymm   = it_dates-sunday(6).
    it_weeks-monday = it_dates-monday.
    it_weeks-sunday = it_dates-sunday.


    if it_dates-freeday = ''.
      it_weeks-tot_wk_day = 1.

      if it_dates-monday(6) <> it_dates-sunday(6)
      and it_dates-date(6) = it_dates-monday(6).
        it_weeks-prev_wk_day = 1.
      else.
        it_weeks-this_wk_day = 1.
      endif.
    endif.

    collect it_weeks. clear it_weeks.
  endloop.

ENDFORM.                    " read_calendar
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.


  PERFORM field_setting(ZCOGSREV) TABLES gt_fieldcat USING :
 'BDATJ'     'Year'           '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'POPER'     'Period'         '03' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'LIFNR'     'Vendor'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'LBLNI'     'Entry#'         '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'RETYM'     'RetYM'          '07' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'LZVON'     'From'           '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'LZBIS'     'To'             '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'KOSTL'     'CostCtr'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'SRVPOS'    'Service'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'RETRO'     'Retro'          '01' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MENGE'     'Hours'          '12' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'TBTWR'     'Value$'         '14' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'BRTWR'     'Rate$'          '11' ' ' 'R'  ' '  ' '  '  ' ' '  ' ',
 'HEAD_CNT'  'Headcnt'        '10' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'EBELN'     'Pur.Ord'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'EBELP'     'Itm'            '05' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

  g_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       TABLES
            t_outtab           = it_mhos
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.

ENDFORM.                    " display_out
