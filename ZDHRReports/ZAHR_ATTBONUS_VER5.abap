* Program Name      : ZAHR_ATTBONUS_VER3
* Author            : ig.moon ( copied from ZAHR_ATTBONUS_NEW )
* Creation Date     : 2008.04.22.
* Specifications By : Imtiaz Ahmad
* Pattern           : BDC program
* Addl Documentation:
* Description       : check the attendance bonus eligibility and
*                     upload the bonus payment by using PA30
*&--------------------------------------------------------------------&*
* Modification Logs
* Date       Developer    De scription
* 04/22/2008  ig          copied from ZAHR_ATTBONUS and fully revised
* 02/28/2010 VALERIAN     Remove A/A type: 1019, 1027, 1058 from
*            HIS20094     occurrences list
*                         For Action type: ZC and Reason for action:
*                         02 or 05 or 06, count the days as occurrences
*
* 04/26/2010 VALERIAN     UD1K951488 Various fixes.
* 11/30/2011 VALERIAN     UD1K953480 Add new absence type in the check
*                                    logic.
* 05/23/2012 VALERIAN     UD1K954831 Add A/A type 3300 and 3303 as
*                                    unexcused absence
*&--------------------------------------------------------------------&*
report zahr_attbonus_ver5 message-id zmhr.

*****************************************************************
*GLOBAL DATA
*****************************************************************
tables: pa0001, pa0002, t554s,  t554t, pa0000, p0001, pc261.
include zacoui00.

constants: c_active(1) type   c value 1.
controls: tc1 type tableview using screen 200.

data: save_okcode  like sy-ucomm.
data: high(8),
      low(8),
      pdate(10).
data: total(5),
      etotal(5),
      suc_tot(5).

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.

define __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
end-of-definition.

constants:  false value ' ',
            true  value 'X'.

data  : flag_data_changed,
        info(80).
data: begin of ftab occurs 10,
        fcode(6),
      end of ftab.

define __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
end-of-definition.

****************************************************************
*INTERNAL TABLES
****************************************************************

data: begin of $it_dis occurs 0,
  pernr       like pernr-pernr,
  massn       like pa0000-massn,
  massg       like pa0000-massg,
  nachn       like pa0002-nachn,
  vorna       like pa0002-vorna,
  begda       like pa0002-begda,
  persg       like pa0001-persg,
  stell       like pa0001-stell,
  stltx       like t513s-stltx,
  amunt       like pa0008-bet08,
  eligi(3)    type c,
  updat(10),
  messg(80) type c,
  chkda      like pa0002-begda,
  mark(1),
      end of $it_dis.

types: begin of ty_out.
        include structure zshr_attbonus_alv.
types   abcnt1   type zabcnt.                               "HIS20094
types   celltab  type lvc_t_styl.
types   tabcolor type slis_t_specialcol_alv.
types: end of ty_out.

data  it_dis   type table of ty_out  with header line.
data  $gt_out like it_dis occurs 0 with header line.

data: begin of it_file occurs 0,
       pernr(10),
       nachn(15),
       vorna(15),
       begda(10),
       persg(03),
       stltx(20),
       text1(10),
       pamunt(12),
       amunt(10),
       eligi(03) ,
       updat(10),
       abcnt(10),
       abcnt1(10),                                          "UD1K951488
       messg(80),
      end of it_file.
data: begin of it_colnames occurs 10,
            name(20),
           end of it_colnames.
*FOR BDC
data: begin of bdcdata occurs 0.
        include structure bdcdata.
data: end of bdcdata.

data: begin of it_message occurs 0.
        include structure bdcmsgcoll.
data: end of it_message.
data: begin of it_mess occurs 0,
        msgty like sy-msgty,
        msgtx(120) type c,
      end of it_mess.
data  g_des(20).
data: l_period(02).
data: l_incentive_beg  like sy-datum.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
class lcl_event_receiver definition.
  public section.

    types: begin of ztcou131_k,
              co_area   type kokrs,
              fisc_year type gjahr,
              version   type versn,
              kostl     type kostl,
              kstar     type kstar,
           end of ztcou131_k.

    types: ztcou131_key   type standard table of ztcou131_k,
           ztcou131_table type standard table of ztcou131.

    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed,
                       get_deleted_rows
             exporting
                       deleted_rows type ztcou131_table,

      refresh_delta_tables.

  private section.
    data deleted_rows type standard table of ztcou131.

* This flag is set if any error occured in one of the
* following methods:
    data: error_in_data type c.
    methods:
      update_delta_tables
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

endclass.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

* Setting for Change data
  method handle_data_changed.

** remember deleted lines for saving
*    CALL METHOD update_delta_tables( er_data_changed ).
*
*    PERFORM data_changed USING er_data_changed.
*
*    __set_refresh_mode true.
*    CALL METHOD g_grid->refresh_table_display
*         EXPORTING is_stable = stable.

  endmethod.                    " handle_data_changed

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.                    "get_deleted_rows

  method refresh_delta_tables.
    clear me->deleted_rows[].
  endmethod.                    "refresh_delta_tables

  method update_delta_tables.
*    DATA: l_del_row TYPE lvc_s_moce,
*          ls_ztcou131 TYPE ztcou131,
*          ls_outtab LIKE LINE OF it_dis.
*
*    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
*      READ TABLE it_dis INTO ls_outtab INDEX l_del_row-row_id.
*      IF sy-subrc NE 0.
*        MESSAGE i000(0k) WITH text-e01. "Internal error
*      ELSE.
*        MOVE-CORRESPONDING ls_outtab TO ls_ztcou131.
*        APPEND ls_ztcou131 TO deleted_rows.
*      ENDIF.
*    ENDLOOP.
  endmethod.                    "update_delta_tables

endclass.                   " LCL_EVENT_RECEIVER Implementation

data g_event_receiver  type ref to lcl_event_receiver.

*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
selection-screen begin of block b2 with frame title text-001.
select-options:  r_jobkey for pa0001-stell no-display  ,
                 r_abstyp for t554s-subty  no-display  ,
                 r_subty1 for t554s-subty  no-display  ,
                 r_mastyp for pa0000-massn no-display.
selection-screen end of block b2.
selection-screen begin of block b1 with frame title text-001.
select-options:  p_pernr     for  pa0001-pernr.
selection-screen skip.
select-options:  p_bperd     for  sy-datum     obligatory.
selection-screen skip.
parameters:      p_pdate     like sy-datum     obligatory.
selection-screen skip.
parameters:      p_amunt     like pa0008-bet08 no-display.

selection-screen begin of block b4 with frame title text-004.
parameters :   p_abkrs like  pa0001-abkrs obligatory default '11'.
selection-screen end of block b4.

selection-screen end of block b1.

* Layout
*selection-screen begin of block b4 with frame title text-01s.
parameter p_vari type slis_vari no-display .
*selection-screen end of block b4.

*****************************************************************
*END OF SELECTION SCREEN
*****************************************************************
at selection-screen output.
  perform initial.

at selection-screen.
  perform check_input.

start-of-selection.

  perform show_progress     using 'Initializing...' '5'.
  perform get_employee.
  perform check_eligible.

  info = text-015.

  perform show_progress     using 'Preparing screen...' '95'.

*  PERFORM display_list.

end-of-selection.
  call screen 200.

*****************************************************************
*               FORMS
*****************************************************************
*&---------------------------------------------------------------------*
*&      Form  check_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_input.
  data: l_days type i.
  l_days = p_bperd-high - p_bperd-low + 1.
  if p_bperd-high is initial.
    message e001 with 'Please enter the period end date!'.
  elseif p_bperd-high gt sy-datum.
    message e001 with 'The period can not be future date!'.
*  ELSEIF l_days NE 28.
*    MESSAGE e001 WITH 'The period is not four weeks.'.
  endif.


  high = p_bperd-high.
  low  = p_bperd-low.

* CONVERT THE DATE FOR EXTERNAL DATE FORMAT
  call function 'CONVERT_DATE_TO_EXTERNAL'
    exporting
      date_internal            = p_pdate
    importing
      date_external            = pdate
    exceptions
      date_internal_is_invalid = 1.

* MAKE THE ASSIGN DESCRIPTION
  g_des(8)   = p_bperd-low.
  g_des+8(1) = '-'.
  g_des+9(8) = p_bperd-high.

  suc_tot = 0.



endform.                    " check_input
*&---------------------------------------------------------------------*
*&      Form  check_eligible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_eligible.

  data: begin of lt_p0001 occurs 0,
          pernr  like pernr-pernr,
          persg  like pa0001-persg,
          stell  like pa0001-stell,
          begda  like pa0001-begda,
          endda  like pa0001-endda,
          messg(80) type c,
          persk  like pa0001-persk,
          categ  type zhrcatg,
         end of lt_p0001.

  data: begin of lt_p0000 occurs 0,
        pernr like pernr-pernr,
        massn like pa0000-massn,
        massg like pa0000-massg,
        begda like pa0000-begda,
        endda like pa0000-endda,
        stat2 like pa0000-stat2,
       end of lt_p0000.

* Begin of HIS20094
  data: loa_date type i,
        loa_date_tot type i,
        loa_tot(5) type c.

  data: begin of lt_loa occurs 0,
        begda like pa0000-begda,
        endda like pa0000-endda,
        pernr like pernr-pernr,
       end of lt_loa.

  data: lt_loa_pernr like lt_loa occurs 0 with header line.
* End of HIS20094

  data: l_days type i.
  data: lt_513s like t513s occurs 0 with header line.
  data: begin of lt_p2001 occurs 0,
         pernr   like pa2001-pernr,
         awart   like pa2001-awart,
         atext   like t554t-atext,
         begda   like pa2001-begda,
         endda   like pa2001-endda,
        end of lt_p2001.

  data: lt_abstx  like t554t occurs 0 with header line.

  select pernr persg stell begda endda persk
               into corresponding fields of table lt_p0001
               from pa0001
             for all entries in it_dis
      where pernr  = it_dis-pernr  and
           begda le p_bperd-high   and
           endda ge p_bperd-high   and
            sprps ne 'X'           and
               persg = c_active       .

  sort lt_p0001 by pernr.
  data $ix like sy-tabix.

  perform show_progress     using 'Gather data...' '15'.

  if not it_dis[]  is initial.
    __cls lt_p0000.
    select pernr massn stat2
  into corresponding fields of table lt_p0000
      from pa0000
      for all entries in it_dis
       where pernr   = it_dis-pernr
         and endda >= sy-datum
         and begda <= sy-datum
         and not massn in r_mastyp.

    sort lt_p0000 by pernr.
  endif.

  loop at it_dis.
    $ix = sy-tabix.
    clear lt_p0001.
    read table lt_p0001 with key pernr = it_dis-pernr
                      binary search.
    if sy-subrc ne 0 or lt_p0001-persg eq '5' .
      delete it_dis index $ix .
      continue.
    endif.

*    IF lt_p0001-persk EQ 'U0'.
*      READ TABLE lt_p0000 WITH KEY pernr = it_dis-pernr BINARY SEARCH.
*      IF sy-subrc EQ 0 AND lt_p0000-stat2 NE '3'.
*        DELETE it_dis INDEX $ix.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    read table lt_p0000 with key pernr = it_dis-pernr binary search.
    if sy-subrc eq 0.
      it_dis-stat2 = lt_p0000-stat2.
      case it_dis-stat2.
        when '0'.
          it_dis-text1 = 'Withdrawn'.
        when '1'.
          it_dis-text1 = 'Inactive'.
        when '2'.
          it_dis-text1 = 'Retire'.
        when '3'.
          it_dis-text1 = 'Active'.
        when others.
      endcase.
      modify it_dis index $ix transporting stat2 text1.
    endif.

    perform get_emp_categ  using    lt_p0001-persg lt_p0001-persk
                           changing lt_p0001-categ.

    if lt_p0001-categ ne 'B'.
      delete it_dis index $ix .
    endif.

  endloop.


**********************************************
  check not it_dis[] is initial.

*& Do not select emp with action type zw,zx,zy from PA0000.

  loop at it_dis.
    $ix = sy-tabix.
    clear lt_p0000.
    read table lt_p0000 with key pernr = it_dis-pernr binary search.
    if sy-subrc ne 0
      or lt_p0000-massn eq 'zx'
      or lt_p0000-massn eq 'zy'
      or lt_p0000-massn eq 'zw'
      or lt_p0000-stat2 eq '0'.                             "UD1K951488
      delete it_dis index $ix.
    endif.
  endloop.

***************************

  perform show_progress     using 'Gather data...' '25'.

  __cls lt_p0000.
*& Do not select emp with action type zc and sub action type:
*('02','03','05','06','07','08','09','10','11') from PA0000.

  select: pernr massg massn into table lt_p0000
     from pa0000
     for all entries in it_dis
      where pernr   = it_dis-pernr
        and endda >= p_pdate
        and begda <= p_pdate
        and massn = 'ZC'
*        AND massg IN ('02','03','05','06','07','08','09','10','11') .
        and massg in ('08','09','10','13') .

  sort lt_p0000 by pernr.

  loop at it_dis.
    $ix = sy-tabix.
    clear lt_p0000.
    read table lt_p0000 with key pernr = it_dis-pernr binary search.
    if sy-subrc eq 0.
      delete it_dis index $ix .
    endif.
  endloop.

* Begin of HIS20094
* Include this Action type 'ZC' and reason codes
  select: begda endda pernr into table lt_loa
     from pa0000
     for all entries in it_dis
      where pernr = it_dis-pernr
        and massn = 'ZC'
** Chnaged on 01/15/14
*        and massg in ('02','05','06','08').
        and massg in ('02','03','05','06','07','08',
                      '09','12','13','15').
** End

  sort lt_loa by pernr.
* End of HIS20094

  check not it_dis[] is initial.

  perform show_progress     using 'Gather data...' '35'.

****************************************
  __cls lt_p0001.

* CHECK EMPLOYEE EFFECTIVE
  select pernr persg stell begda endda persk
               into  corresponding fields of table lt_p0001
               from pa0001
             for all entries in it_dis
      where pernr  = it_dis-pernr
        and begda le p_bperd-low
        and endda ge p_bperd-low
        and sprps ne 'X'
        and persg = c_active
        and not stell in r_jobkey.

  sort lt_p0001 by pernr.

* DELETE THE NON ELIGIBLE EE
  loop at it_dis.

    $ix = sy-tabix.
    clear lt_p0001.
    read table lt_p0001 with key pernr = it_dis-pernr binary search.

    if sy-subrc ne 0 or lt_p0001-persg eq '5'.
      delete it_dis index $ix.
      continue.
    endif.

    it_dis-persg = lt_p0001-persg.
    it_dis-stell = lt_p0001-stell.
    modify it_dis index $ix transporting persg stell.

  endloop.

  perform show_progress     using 'Gather data...' '45'.

  check not it_dis[] is initial.

* GET THE JOB TITLE
  select * into table lt_513s
    from t513s
    for all entries in it_dis
    where stell = it_dis-stell.

  sort lt_513s by stell.

  loop at it_dis.
    $ix = sy-tabix.
    clear lt_513s.
    read table lt_513s with key stell = it_dis-stell binary search.
    if sy-subrc = 0.
      it_dis-stltx = lt_513s-stltx.
      modify it_dis index $ix transporting stltx.
    endif.
  endloop.

*CHECK THE EE HIRE DAYS >90 DAYS OR INACTIVE
  loop at it_dis.
    $ix = sy-tabix.
*    l_days = p_bperd-low - it_dis-begda.
    l_days = p_bperd-high - it_dis-begda.
    if l_days lt 90.
      it_dis-eligi = 'NO'.
      it_dis-messg  = 'Hire less than 90 days'.
      modify it_dis  index $ix transporting eligi messg.
    elseif l_days ge 90 and l_days lt 118.
      "it's the first period
*           "requested by Naveen, changed by chris
      it_dis-chkda = p_bperd-low .   "it_dis-begda.
      modify it_dis  index $ix transporting chkda.
    elseif l_days ge 118. "it's not the first period
      it_dis-chkda = p_bperd-low.
      modify it_dis  index $ix transporting chkda.
    endif.
* BEGIN OF UD1K951488
    if it_dis-stat2 = '1'.
      it_dis-eligi = 'NO'.
      modify it_dis  index $ix transporting eligi.
    endif.
* END OF UD1K951488
  endloop.

  perform show_progress     using 'Gather data...' '55'.

* CHECK THE EE ABSENCE STATUS.
  __cls lt_p2001.
  select pernr awart begda endda
    into corresponding fields of table lt_p2001
    from pa2001
    for all entries in it_dis
        where  pernr   = it_dis-pernr
*          AND  ( ( begda GE it_dis-chkda AND begda LE p_bperd-high )
*              OR ( endda GE _dis-chkda AND endda LE p_bperd-high ) )
          and ( begda in p_bperd or endda in p_bperd )
          and awart  in ( select awart from zthr_disqualify ).

*&----select from PA2002 Attendance also.

  select pernr awart  begda endda
    appending corresponding fields of table lt_p2001
    from pa2002
    for all entries in it_dis
            where  pernr   = it_dis-pernr
*           AND ( ( begda GE it_dis-chkda  AND begda LE p_bperd-high )
*              OR ( endda GE it_dis-chkda  AND endda LE p_bperd-high ) )
           and ( begda in p_bperd or endda in p_bperd )
           and awart  in ( select awart from zthr_disqualify ).

  perform show_progress     using 'Gather data...' '65'.

  read table lt_p2001 index 1.
  if sy-subrc eq 0.
* get the absence text
    select * into table lt_abstx
      from t554t
      for all entries in lt_p2001
      where awart = lt_p2001-awart
        and sprsl = 'EN'.
  endif.

  sort lt_abstx by awart.
  loop at lt_p2001.
    $ix = sy-tabix.
    clear lt_abstx.
    read table lt_abstx with key awart = lt_p2001-awart binary search.
    if sy-subrc eq 0.
      lt_p2001-atext = lt_abstx-atext.
      modify lt_p2001 index $ix transporting atext.
    endif.
  endloop.

  perform show_progress     using 'Gather data...' '75'.

* CHECK THE NON-ELIGIBLE EE WHO HAS ABSENCE

* by ig.moon 10/23 {
*  SORT lt_p2001 BY pernr.

  sort lt_p2001 by pernr begda.
  delete adjacent duplicates from lt_p2001 comparing pernr begda.

* }

  data : $cnt type i,
         $atext like lt_p2001-atext,
         $tcnt(5).

  __define_not_important.
  describe table it_dis lines total_doc_cnt.
  $total_cnt = total_doc_cnt.

  loop at it_dis.

    add 1 to current_doc_cnt.
    $current_cnt = current_doc_cnt.
*    CONCATENATE it_dis-pernr ':' $current_cnt '/' $total_cnt
*    INTO $text.
    concatenate 'Calculating...' $current_cnt '/' $total_cnt
    into $text.
    condense $text.
*    percentage = current_doc_cnt / total_doc_cnt * 100.
    percentage = current_doc_cnt mod 10.
*    IF percentage EQ 0.
    perform show_progress using $text 0.
*    ENDIF.
    $ix = sy-tabix.
    clear lt_p2001.
    clear : $cnt, $atext.

    read table lt_p2001 with key pernr = it_dis-pernr binary search.
    if sy-subrc eq 0.
      loop at lt_p2001 from sy-tabix.
        if lt_p2001-pernr ne it_dis-pernr.
          exit.
        endif.
        $atext = lt_p2001-atext.
        add 1 to $cnt.
      endloop.
    endif.

* Begin of HIS20094 - Calculate LOA Date
    __cls lt_loa_pernr.
    loop at lt_loa where pernr = it_dis-pernr.
      lt_loa_pernr = lt_loa.
      append lt_loa_pernr.
    endloop.

    clear loa_date_tot.
    provide * from lt_loa_pernr between p_bperd-low and p_bperd-high.
      loa_date = lt_loa_pernr-endda - lt_loa_pernr-begda + 1.
      loa_date_tot = loa_date_tot + loa_date.
    endprovide.

    it_dis-abcnt1 = loa_date_tot.
    loa_tot       = loa_date_tot.

* End of HIS20094

    clear p_amunt.

    if $cnt > 2.
*     IF it_dis-eligi NE 'YES'.                             "UD1K951488
*     IF it_dis-eligi NE 'NO'.                              "UD1K951488
      it_dis-eligi = 'NO'.
      $tcnt = $cnt.

* Begin of HIS20094 - Set Absence Descriptions
      if not loa_date_tot is initial.
        concatenate $tcnt ' Absence(s)(' $atext '..)+' loa_tot
                          'Occurrence(s) LOA'
        into it_dis-messg separated by space.
      else.

        concatenate $tcnt ' Absence(s)(' $atext '..)' into it_dis-messg
                                      separated by space.

      endif.

      condense it_dis-messg.
* End of HIS20094

      it_dis-abcnt = $cnt.

      perform read_payroll_data using it_dis-pernr
                                changing p_amunt.

      it_dis-pamunt = p_amunt.
      modify it_dis index $ix transporting eligi pamunt messg abcnt
                                           abcnt1.          "HIS20094
*     ENDIF.                                                "UD1K951488
    else.

      perform read_payroll_data using it_dis-pernr          "UD1K951488
                                changing p_amunt.           "UD1K951488

      if it_dis-eligi ne 'NO'.
        it_dis-eligi   = 'YES'.
*       PERFORM read_payroll_data USING it_dis-pernr        "UD1K951488
*                                 CHANGING p_amunt.         "UD1K951488
        if $cnt eq 0.
          it_dis-amunt  = p_amunt * '0.025'.
        endif.
        if $cnt eq 1.
          it_dis-amunt  = p_amunt * '0.0125'.
        endif.
        if $cnt eq 2.
          it_dis-amunt  = p_amunt * '0.006'.
        endif.

* BEGIN OF UD1K951488
*        etotal = etotal + 1.
*        it_dis-abcnt = $cnt.
*        it_dis-pamunt = p_amunt.
*       MODIFY it_dis INDEX $ix TRANSPORTING eligi pamunt amunt abcnt
*                                            abcnt1.        "HIS20094
* END OF UD1K951488

      endif.

* BEGIN OF UD1K951488
      etotal = etotal + 1.
      it_dis-abcnt = $cnt.
      it_dis-pamunt = p_amunt.

      modify it_dis index $ix transporting eligi pamunt amunt abcnt
                                             abcnt1.
* END OF UD1K951488
    endif.

  endloop.

  describe table it_dis lines total.
**S> 08/04/11 Paul
  perform show_progress     using 'Gather data...' '85'.
**E<
endform.                    " check_eligible
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_list.
  call screen 100.
endform.                    " display_list
*&---------------------------------------------------------------------*
*&      Form  GET_EMPLOYEE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_employee.
  data: lt_name like $it_dis occurs 0 with header line.

  __cls : $it_dis, it_dis.

* select the EE and hire date
  select pernr dat01 as begda
into corresponding fields of table $it_dis
   from pa0041
   where sprps = space and
         pernr in p_pernr.

  sort $it_dis by pernr begda.
  delete adjacent duplicates from $it_dis
                   comparing pernr begda.
* get the name of ee
  select pernr nachn vorna
into corresponding fields of table lt_name
   from pa0002
   for all entries in $it_dis
 where pernr = $it_dis-pernr
         and begda <= p_pdate
         and endda >= p_pdate
         and sprps = space.

  loop at $it_dis.
    if $it_dis-begda is initial.
      message e001 with 'No hire date for :' $it_dis-pernr.
    endif.
    read table lt_name with key pernr = $it_dis-pernr.
    $it_dis-nachn = lt_name-nachn.
    $it_dis-vorna = lt_name-vorna.
    modify $it_dis.
  endloop.

  loop at $it_dis.
    move-corresponding $it_dis to it_dis.
    append it_dis.
  endloop.

endform.                    " GET_EMPLOYEE
*&---------------------------------------------------------------------*
*&      Form  INITIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initial.
* MAKE THE RANGES
  r_jobkey-sign     = 'I'.
  r_jobkey-option   = 'EQ'.

  r_jobkey-low      = '10001144'.
  append r_jobkey.
  r_jobkey-low      = '10001145'.
  append r_jobkey.
  r_jobkey-low      = '90000040'.
  append r_jobkey.
  r_jobkey-low      = '90000269'.
  append r_jobkey.
  r_jobkey-low      = '90000657'.
  append r_jobkey.
  r_jobkey-low      = '90000658'.
  append r_jobkey.
  r_jobkey-low      = '90000659'.
  append r_jobkey.
  r_jobkey-low      = '90000661'.
  append r_jobkey.
  r_jobkey-low      = '90002147'.
  append r_jobkey.

*************************************
**Do notpay these Abs types*********
*  r_abstyp-sign     = 'I'.
*  r_abstyp-option   = 'EQ'.
*  r_abstyp-low      = '1017'.
*  append r_abstyp.
*  r_abstyp-low      = '3300'.                               "UD1K954831
*  append r_abstyp.                                          "UD1K954831
*
** by ig.moon 2/3/2009 {
**  r_abstyp-low      = '1018'.
**  APPEND r_abstyp.
** }
*
**  r_abstyp-low      = '1019'. "HIS20094
**  APPEND r_abstyp.            "HIS20094
*  r_abstyp-low      = '1025'.
*  append r_abstyp.
*  r_abstyp-low      = '1034'.
*  append r_abstyp.
*  r_abstyp-low      = '1035'.
*  append r_abstyp.
*  r_abstyp-low      = '1044'.
*  append r_abstyp.
*  r_abstyp-low      = '1045'.
*  append r_abstyp.
*  r_abstyp-low      = '1046'.
*  append r_abstyp.
*  r_abstyp-low      = '1050'.
*  append r_abstyp.
*  r_abstyp-low      = '1053'.
*  append r_abstyp.
*  r_abstyp-low      = '1057'.
*  append r_abstyp.
**  r_abstyp-low      = '1058'. "HIS20094
**  APPEND r_abstyp.            "HIS20094
*  r_abstyp-low      = '2000'.
*  append r_abstyp.
**  r_abstyp-low      = '1027'. "HIS20094
**  APPEND r_abstyp.            "HIS20094
*  r_abstyp-low      = '1074'.                               "UD1K953480
*  append r_abstyp.                                          "UD1K953480
*
*
********************************
**Do not pay these Attendance types
*** pulling from pa2002
*  r_subty1-sign   = 'I'.
*  r_subty1-option = 'EQ'.
*
**  r_subty1-low    = '1019'.   "HIS20094
**  APPEND r_subty1.            "HIS20094
*  r_subty1-low    = '2000'.
*  append r_subty1.
*  r_subty1-low    = '3303'.                                 "UD1K954831
*  append r_subty1.                                          "UD1K954831
*  r_subty1-low    = '1046'.
*  append r_subty1.
**  r_subty1-low    = '1058'.   "HIS20094
**  APPEND r_subty1.            "HIS20094
*** end of change
*******************************
*******************************
*******************************
*Do not pay these action types
  r_mastyp-sign   = 'I'.
  r_mastyp-option = 'EQ'.
*  r_mastyp-low    = 'ZC'.
*  append r_mastyp.
  r_mastyp-low    = 'ZW'.
  append r_mastyp.
  r_mastyp-low    = 'ZX'.
  append r_mastyp.
  r_mastyp-low    = 'ZY'.
  append r_mastyp.
*****************************

endform.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set titlebar 'TITLE_100'.
  perform exclude_functions.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  save_okcode = ok_code.
  clear ok_code.
  case save_okcode.
    when 'BACK' or 'EXIT'.
      leave to screen 0.
    when 'CANCEL'.
      leave program.
    when 'UPDAT'.
      perform save_pay_100.
      __focus g_grid.
    when 'DISE'.
      perform dis_eligible_100.
      __focus g_grid.
    when 'ELIG'.
      perform eligible_100.
      __focus g_grid.
    when 'DSEL'.
      perform dis_select_all.
    when 'SELA'.
      perform select_all.
    when 'DNLD'.
      perform download_file.
    when 'SWITCH'.
      if sy-dynnr eq '0100'.
        perform switch_edit_mode.
      endif.
    when others.
  endcase.

endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_pay_100.

  data: i_total type i.
  data: i_percent type i.
  data: i_processed type i.
  data: answer.

  if total = 0.
    exit.
  endif.

* CHECK DUPLICATE UPLOAD
  perform check_duplicate_pay.

* CHECK IF THE DATA HAS BEEN SAVED
*  OR SOME DATA HAVE BEEN SAVED.
  read table it_dis with key updat = 'SUCCESS'
                             mark = true.
  if sy-subrc eq 0.
    call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      exporting
        titel         = 'make selection ;;'(b01)
        diagnosetext1 = 'ALL OR SOME DATA HAS BEEN SAVED. ;;->B03'(b02)
        diagnosetext2 = 'ONLY UNSAVED DATA CAN BE SAVED AGAIN. '(b03)
*       DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
        textline1     = 'ARE YOU SURE YOU WANT TO SAVE ?? ;;->B06'(b05)
*       TEXTLINE2     = 'new planning alternative? ;;'(B06)
      importing
        answer        = answer.

    if answer ne 'J'.
      exit.
    endif.
  else.
    clear answer.

*   CONFIRM UPDATE
    call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
                                        exporting
                 titel = 'make selection ;;'(b01)
**S> 08/04/11 Paul
diagnosetext1 = 'THE DATA WILL BE SAVED TO DATABASE.;;->b03'(b02)
**E<
*      DIAGNOSETEXT2 = 'alternative for the specified ;;->B04'(B03)
*      DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
textline1 = 'ARE YOU SURE YOU WANT TO SAVE?? ;;->B06'(b05)
*      TEXTLINE2 = 'new planning alternative? ;;'(B06)
                        importing answer = answer.

    if answer ne 'J'.
      exit.
    endif.
  endif.
*
* UPDATE THE DATABASE
*  DESCRIBE TABLE it_dis LINES i_total.

  loop at it_dis where mark eq true.
    add 1 to i_total.
  endloop.

  if i_total ne 0.
    i_processed = 0.
    loop at it_dis where mark eq true.
      __cls bdcdata.
*     CHECK IF THIS RECORD HAS BEEN SAVED.
*     OR NON-ELIGIBLE OR ALREADY UPLOAD
      if it_dis-updat = 'SUCCESS' or
         it_dis-updat = 'EXIST'   or
              it_dis-eligi ne 'YES'.
        continue.
      endif.

      perform do_bdc using it_dis.

      i_processed = i_processed + 1.
      i_percent = ( i_processed * 100 ) / i_total .
      perform progress_indicator using i_percent.
    endloop.
  endif.

  suc_tot = '0'.
  loop at it_dis where updat = 'SUCCESS' and mark eq true.
    suc_tot = suc_tot + 1.
  endloop.

endform.                    " SAVE_PAY

*&      Form  DO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form do_bdc using p_person structure it_dis.
  data: l_update type c.

* FILL BDC DATA
  perform fill_bdcdata using p_person .
* CALL TRANSACTION
  perform call_transaction using 'PA30' l_update.
  if l_update = 'S'.
    p_person-updat = 'SUCCESS'.
    modify it_dis from p_person transporting updat messg.
  else.
    p_person-updat = 'FAIL'.
    modify it_dis from p_person transporting updat messg.
  endif.

endform.                    " DO_BDC
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0194   text
*----------------------------------------------------------------------*
form call_transaction using p_tcode p_update.
  data: l_msgstr(100) type c.

  call transaction p_tcode
           using bdcdata
           mode 'N'
           update 'S'
       messages into it_message.
* ckeck the message
  if sy-subrc = 0.
    p_update = 'S'.
    clear it_dis-messg.
  else.
    p_update = 'F'.

    perform rkc_msg_string using l_msgstr.

    append it_mess.
    it_dis-messg = l_msgstr.
  endif.

endform.                    " CALL_TRANSACTION

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
*       SERACH THE MESSAGE OF BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

form rkc_msg_string changing p_msg.
  data: lw_msg like cfgnl-msglin.

  call function 'RKC_MSG_STRING'
    exporting
      id      = sy-msgid
      mtype   = sy-msgty
      number  = sy-msgno
      par1    = sy-msgv1
      par2    = sy-msgv2
      par3    = sy-msgv3
      par4    = sy-msgv4
    importing
      msg_lin = lw_msg
    exceptions
      others  = 1.
*  CONCATENATE 'Update failed for' it_person-pernr
*    INTO lw_msg SEPARATED BY space.
  concatenate it_dis-pernr lw_msg into lw_msg
                           separated by space.
  move: lw_msg to p_msg.
endform.                    " RKC_MSG_STRING

*&---------------------------------------------------------------------*
*&      Form  FILL_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_bdcdata using p_person structure it_dis.
  data: l_amount(11) type c.

  write it_dis-amunt to l_amount .

  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_OKCODE'
                                       '/00'.
  perform bdc_field       using 'RP50G-PERNR'
                               p_person-pernr.
  perform bdc_field       using 'BDC_CURSOR'
                               'RP50G-CHOIC'.
  perform bdc_field       using 'RP50G-CHOIC'
                                       '0015'.
  perform bdc_dynpro      using 'SAPMP50A' '1000'.
  perform bdc_field       using 'BDC_OKCODE'
                                      '=INS'.
  perform bdc_field       using 'RP50G-PERNR'
                               p_person-pernr.
  perform bdc_field       using 'BDC_CURSOR'
                               'RP50G-ENDDA'.
  perform bdc_field       using 'RP50G-TIMR6'
                                          'X'.
  perform bdc_field       using 'RP50G-BEGDA'
                                        pdate.
  perform bdc_field       using 'RP50G-ENDDA'
                                        pdate.
  perform bdc_field       using 'RP50G-CHOIC'
                                       '0015'.
  perform bdc_field       using 'RP50G-SUBTY'
                                       '0318'.
  perform bdc_dynpro      using 'MP001500' '2000'.
  perform bdc_field       using 'BDC_CURSOR'
                               'P0015-ZUORD'.
  perform bdc_field       using 'BDC_OKCODE'
                                      '=UPD'.
  perform bdc_field       using 'P0015-LGART'
                                       '0318'.
  perform bdc_field       using 'Q0015-BETRG'
                                     l_amount.
  perform bdc_field       using 'P0015-WAERS'
                                        'USD'.
  perform bdc_field       using 'P0015-BEGDA'
                                        pdate.
  perform bdc_field       using 'P0015-ZUORD'
                                        g_des.
  perform bdc_dynpro      using 'MP001500' '2000'.
  perform bdc_field       using 'BDC_OKCODE'
                                       '/00'.

endform.                    " FILL_BDCDATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
form bdc_dynpro using program dynpro.
  clear bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  append bdcdata.
endform.                    "bdc_dynpro
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
form bdc_field using fnam fval.
  clear bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  append bdcdata.
endform.                    "bdc_field
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_PERCENT  text
*----------------------------------------------------------------------*
form progress_indicator using    p_percent.
  data: l_text(40).
  data: i_mod type i.

  l_text = p_percent.
  condense l_text.
  i_mod = p_percent mod 5.
  if i_mod = 0.
    concatenate l_text '% PROCESSED' into l_text.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
        text = l_text.
  endif.
endform.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  DIS_ELIGIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dis_eligible_100.

  loop at it_dis where mark = 'X'.
    it_dis-eligi = 'NO'.
    modify it_dis.
  endloop.

  it_dis-mark = false .
  modify it_dis transporting mark where mark eq true.

endform.                    " DIS_ELIGIBLE
*&---------------------------------------------------------------------*
*&      Form  ELIGIBLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form eligible_100.
  loop at it_dis where mark = 'X'.
    it_dis-eligi = 'YES'.
    modify it_dis.
  endloop.

  it_dis-mark = false .
  modify it_dis transporting mark where mark eq true.

endform.                    " ELIGIBLE
*&---------------------------------------------------------------------*
*&      Form  DIS_SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dis_select_all.
  it_dis-mark = space.
  modify it_dis transporting mark where mark ne space.

endform.                    " DIS_SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  SELECT_ALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_all.
  it_dis-mark = 'X'.
  modify it_dis transporting mark where mark ne 'X'.
endform.                    " SELECT_ALL
*&---------------------------------------------------------------------*
*&      Form  check_duplicate_pay
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_duplicate_pay.

  data: begin of lt_p0015 occurs 0,
          pernr  like pa0015-pernr,
          begda  like pa0015-begda,
          endda  like pa0015-endda,
          zuord  like pa0015-zuord,
          lgart  like pa0015-lgart,
          betrg  like pa0015-betrg,
           end of lt_p0015.

  data: l_begin like sy-datum.
  data: l_end   like sy-datum.
  data: l_period(20).
  concatenate p_pdate(4) '0101' into l_begin.
  concatenate p_pdate(4) '1231' into l_end.

  select * into corresponding fields of table lt_p0015
                                           from pa0015
                             for all entries in it_dis
                       where pernr = it_dis-pernr  and
                             lgart = '0318'        and
                             betrg ne 0            and
                             endda ge l_begin      and
                                        endda le l_end.
  check sy-subrc eq 0.
  loop at it_dis where eligi = 'YES' and mark eq true.

    clear lt_p0015.
    read table lt_p0015 with key pernr = it_dis-pernr
                                      begda = p_pdate.
    if sy-subrc eq 0.
      it_dis-updat = 'EXIST'.
      it_dis-messg = 'Payment already exist for this period'.
      modify it_dis.
      continue.
    endif.

    clear lt_p0015.
    read table lt_p0015 with key pernr = it_dis-pernr
                                        zuord = g_des.
    if sy-subrc eq 0.
      it_dis-updat = 'EXIST'.
      it_dis-messg = 'Payment already exist for this period'.
      modify it_dis.
    endif.


  endloop.


endform.                    " check_duplicate_pay
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form download_file.
  data : filename like  rlgrap-filename.
  __cls it_file.
  loop at it_dis.
    clear it_file.
    move-corresponding it_dis to it_file.
    append it_file.
  endloop.
  perform make_colname.
  perform get_filename using filename.
  perform download     using filename.

endform.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  GET_FILENAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*----------------------------------------------------------------------*
form get_filename using    p_file  like rlgrap-filename.
  data: tmp_filename like rlgrap-filename.
  data: fname     like rlgrap-filename.
  data: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.

  tmp_mask = '*,*.*.'.
  call function 'WS_FILENAME_GET'
    exporting
      def_filename     = fname
      def_path         = fname
      mask             = tmp_mask
      mode             = 'O'
**           TITLE            = ' '
    importing
      filename         = tmp_filename
*     RC               =
    exceptions
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

  if sy-subrc = 0.
    p_file = tmp_filename.
  endif.

endform.                    " GET_FILENAME
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FILENAME  text
*----------------------------------------------------------------------*
form download using    p_filename.
  data:l_text(30).

  if p_filename is initial.
    exit.
  endif.
  call function 'WS_DOWNLOAD'
    exporting
      filename                = p_filename
      filetype                = 'DAT'
*     col_select              = 'X'
    tables
      data_tab                = it_file
      fieldnames              = it_colnames
    exceptions
      file_open_error         = 1
      file_write_error        = 2
      invalid_filesize        = 3
      invalid_table_width     = 4
      invalid_type            = 5
      no_batch                = 6
      unknown_error           = 7
*     GUI_REFUSE_FILETRANSFER = 8
      others                  = 9.
  if sy-subrc <> 0.
    l_text = 'File Download Not Success'.
  else.
    l_text = 'File Download Success '.
  endif.
  message s001 with l_text.
endform.                    " DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  make_colname
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_colname.
* MAKE COLIMN HEADER
  __cls it_colnames.
  it_colnames-name = 'PERNR'.
  append it_colnames.
  it_colnames-name = 'LAST NAME'.
  append it_colnames.
  it_colnames-name = 'FIRST NAME'.
  append it_colnames.
  it_colnames-name = 'HIRE DATE'.
  append it_colnames.
  it_colnames-name = 'GROUP'.
  append it_colnames.
  it_colnames-name = 'JOB TITLE'.
  append it_colnames.
  it_colnames-name = 'STATUS'.
  append it_colnames.
  it_colnames-name = '/101'.
  append it_colnames.
  it_colnames-name = 'AMOUNT'.
  append it_colnames.
  it_colnames-name = 'ELEIGIBLE'.
  append it_colnames.
  it_colnames-name = 'UPDATE'.
  append it_colnames.
  it_colnames-name = 'ABCNT'.
  append it_colnames.
  it_colnames-name = 'LOA.CNT'.                             "UD1K951488
  append it_colnames.                                       "UD1K951488
  it_colnames-name = 'MESSAGE'.
  append it_colnames.
*  it_colnames-name = 'PAYID'.
*  APPEND it_colnames.

endform.                    " make_colname
*&---------------------------------------------------------------------*
*&      Form  exclude_functions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exclude_functions.

  perform append_exclude_functions
           tables gt_exclude[]
           using: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

endform.                    " exclude_functions
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module display_alv_100 output.

  if g_custom_container is initial.
    perform create_and_init_alv.
*   Display alv grid
    call method g_grid->set_table_for_first_display
      exporting
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      changing
        it_outtab            = it_dis[]
        it_fieldcatalog      = gt_fcat[]
        it_sort              = gt_sort[].
  else.
    call method g_grid->refresh_table_display.
  endif.
  __focus g_grid.
  perform user_status.

endmodule.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_and_init_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_and_init_alv.

*   Create object
  perform create_object.

*  Create Object to verify input values.
  create object g_event_receiver.
  set handler : g_event_receiver->handle_data_changed for g_grid.

*   Create field category
  perform create_field_category using false.

  call method g_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  call method g_grid->set_ready_for_input
    exporting
      i_ready_for_input = 0.

  perform sort_build using gt_sort[].

*   Setting for layout
  perform set_lvc_layout.

*   Set colors
  perform set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
  gs_variant-variant = p_vari.

*   Define cell attribute
  perform build_cell_attr.
endform.                    " create_and_init_alv
*&---------------------------------------------------------------------*
*&      Form  create_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
form create_field_category using mode_edit.
  data: l_pos       type i.
  define __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  end-of-definition.

  __catalog :
    'X'  'PERNR'             'Pernr'             8 'CHAR' '',
    'X'  'NACHN'             'Last Name'        15 'CHAR' '',
    'X'  'VORNA'             'First Name'       15 'CHAR' '',
    ' '  'BEGDA'             'Hire Date'        10 'DATS' '',
    ' '  'PERSG'             'GR'                5 'CHAR' '',
    ' '  'STLTX'             'Job Title'        20 'CHAR' '',
    ' '  'AMUNT'             'Amount'           15 'CURR' '',
    ' '  'ELIGI'             'Elig.'
                                                 4 'CHAR' '',
    ' '  'UPDAT'             'Update'           10 'CHAR' '',
    ' '  'MESSG'             'Message'          80
'CHAR' ''.

  loop at gt_fcat into gs_fcat.
    gs_fcat-ref_table = 'ZSHR_ATTBONUS_ALV'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.
    modify gt_fcat from gs_fcat.
  endloop.

endform.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  sort_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
form sort_build using ft_sort type lvc_t_sort.
  define sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  end-of-definition.

  sort_tab :
      'PERNR'             ' ' 'X' '' 'X' '',
      'NACHN'             ' ' 'X' '' 'X' '',
      'VORNA'             ' ' 'X' '' 'X' ''.
endform.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  set_lvc_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_lvc_layout.
  clear gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.

endform.                    " set_lvc_layout
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_color.
  clear: gs_specialcol, gt_specialcol[], it_dis-tabcolor[].

  define __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  end-of-definition.

  __color :
           'PERNR'  '1' 0,
           'NACHN'  '1' 0,
           'VORNA'  '1' 0,
           'BEGDA'  '2' 0,
           'PERSG'  '2' 0,
           'STLTX'  '2' 0,
           'AMUNT'  '2' 0,
           'ELIGI'  '2' 0,
           'UPDAT'  '2' 0,
           'MESSG'  '2' 0.

  it_dis-tabcolor[] = gt_specialcol[].
  modify it_dis transporting tabcolor where tabcolor is initial.

endform.                    " set_color
*&---------------------------------------------------------------------*
*&      Form  build_cell_attr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_cell_attr.

  data: lt_celltab type lvc_t_styl,
        ls_celltab type lvc_s_styl.

  clear lt_celltab.
  refresh lt_celltab.

  clear gs_fcat.

  loop at gt_fcat into gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    if ls_celltab-fieldname eq 'BEGDA'
      or ls_celltab-fieldname eq 'PERSG'
      or ls_celltab-fieldname eq 'STLTX'
      or ls_celltab-fieldname eq 'BET08'
      or ls_celltab-fieldname eq 'ELIGI'
      or ls_celltab-fieldname eq 'UPDAT'
      or ls_celltab-fieldname eq 'MESSG'.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    else.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    endif.

    insert ls_celltab into table lt_celltab.
  endloop.

  clear it_dis-celltab.
  insert lines of lt_celltab into table it_dis-celltab.
  modify it_dis transporting celltab where celltab is initial.

endform.                    " build_cell_attr
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_status.

  __cls ftab.

  if g_grid->is_ready_for_input( ) eq 1.
    ftab-fcode = 'SAVE'.
    append ftab.
  endif.

  set pf-status '100' excluding ftab.

endform.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  data_changed
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_data_changed  text
*----------------------------------------------------------------------*
form data_changed using    p_er_data_changed.

endform.                    " data_changed
*&---------------------------------------------------------------------*
*&      Form  switch_edit_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form switch_edit_mode.
  data answer.
  if g_grid->is_ready_for_input( ) eq 0.
    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 1.

    perform info_text_set using true.
  else.
    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 0.
    perform info_text_set using false.
  endif.

  perform build_cell_attr.

endform.                    " switch_edit_mode
*&---------------------------------------------------------------------*
*&      Form  info_text_set
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
form info_text_set using    p_true.
  if p_true eq true.
    info = text-015.
  else.
    info = text-015.
  endif.

endform.                    " INFO_TEXT_SET
*&---------------------------------------------------------------------*
*&      Form  show_progress
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1894   text
*      -->P_1895   text
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = pf_val
      text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  set pf-status '100' excluding 'SWITCH'.
  set titlebar 'TITLE_100'.
  describe table it_dis lines tc1-lines.
endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_DIS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module modify_dis input.
  modify it_dis index tc1-current_line
         transporting amunt eligi mark.

endmodule.                 " MODIFY_DIS  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  save_okcode = ok_code.
  clear ok_code.
  case save_okcode.
    when 'BACK' or 'EXIT'.
      leave to screen 0.
    when 'CANCEL'.
      leave program.
    when 'UPDAT'.
      perform save_pay.
    when 'DISE'.
      perform dis_eligible.
    when 'ELIG'.
      perform eligible.
    when 'DSEL'.
      perform dis_select_all.
    when 'SELA'.
      perform select_all.
    when 'DNLD'.
      perform download_file.
    when others.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_pay
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_pay.

  data: i_total type i.
  data: i_percent type i.
  data: i_processed type i.
  data: answer.

  if total = 0.
    exit.
  endif.

  read table it_dis with key mark = true.
  if sy-subrc ne 0.
    it_dis-mark = true.
    modify it_dis transporting mark where mark eq false.
  endif.

* CHECK DUPLICATE UPLOAD
  perform check_duplicate_pay.

* CHECK IF THE DATA HAS BEEN SAVED
*  OR SOME DATA HAVE BEEN SAVED.
  read table it_dis with key updat = 'SUCCESS'.
  if sy-subrc eq 0.
    call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      exporting
        titel         = 'make selection ;;'(b01)
        diagnosetext1 = 'ALL OR SOME DATA HAS BEEN SAVED. ;;->B03'(b02)
        diagnosetext2 = 'ONLY UNSAVED DATA CAN BE SAVED AGAIN. '(b03)
*       DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
        textline1     = 'ARE YOU SURE YOU WANT TO SAVE ?? ;;->B06'(b05)
*       TEXTLINE2     = 'new planning alternative? ;;'(B06)
      importing
        answer        = answer.

    if answer ne 'J'.
      exit.
    endif.
  else.
    clear answer.

*   CONFIRM UPDATE
    call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      exporting
        titel         = 'make selection ;;'(b01)
        diagnosetext1 = 'THE DATA WILL BE SAVED TO DATABASE. ;;->B03'(b02)
*       DIAGNOSETEXT2 = 'alternative for the specified ;;->B04'(B03)
*       DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
        textline1     = 'ARE YOU SURE YOU WANT TO SAVE?? ;;->B06'(b05)
*       TEXTLINE2     = 'new planning alternative? ;;'(B06)
      importing
        answer        = answer.

    if answer ne 'J'.
      exit.
    endif.
  endif.
*
* UPDATE THE DATABASE
  describe table it_dis lines i_total.
  if i_total ne 0.
    i_processed = 0.
    loop at it_dis.
      clear: bdcdata, bdcdata[].
*     CHECK IF THIS RECORD HAS BEEN SAVED.
*     OR NON-ELIGIBLE OR ALREADY UPLOAD
      if it_dis-updat = 'SUCCESS' or
         it_dis-updat = 'EXIST'   or
              it_dis-eligi ne 'YES'.
        continue.
      endif.

      perform do_bdc using it_dis.

      i_processed = i_processed + 1.
      i_percent = ( i_processed * 100 ) / i_total .
      perform progress_indicator using i_percent.
    endloop.
  endif.

  suc_tot = '0'.
  loop at it_dis where updat = 'SUCCESS'.
    suc_tot = suc_tot + 1.
  endloop.

endform.                    " save_pay
*&---------------------------------------------------------------------*
*&      Form  dis_eligible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form dis_eligible.
  loop at it_dis where mark = 'X'.
    it_dis-eligi = 'NO'.
    modify it_dis.
  endloop.

endform.                    " dis_eligible
*&---------------------------------------------------------------------*
*&      Form  eligible
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form eligible.
  loop at it_dis where mark = 'X'.
    it_dis-eligi = 'YES'.
    modify it_dis.
  endloop.

endform.                    " eligible
*&---------------------------------------------------------------------*
*&      Form  read_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DIS_PERNR  text
*----------------------------------------------------------------------*
form read_payroll_data using p_pernr changing p_p_amount.
  clear p_p_amount.

  data: in_rgdir like pc261 occurs 0 with header line,
        wa_rt like pc207 occurs 0 with header line,
        seqnr like pc261-seqnr,
        ws_401k type   pc207-betrg,
        ws_gsal type   pc207-betrg,
        ws_8387 type   pc207-betrg,
        ws_8337 type  pc207-betrg,
        ws_bc31 type  pc207-betrg,
        l_limit  type pc207-betrg,
        l_ee% type pa0169-eepct,
        ws_pernr type p0000-pernr,
        result type pay99_result.

  data : ws_rgdir like line of in_rgdir,
         l_relid like  pcl2-relid,
         l_pernr like pc200-pernr,
         l_code like p0041-dar01,
         l_code1 like p0041-dar01,
         t_date like p0041-dat01,
         hire_date like  p0041-dat01,
         l_edate  like pa0169-begda,
         l_edt1  like pa0169-begda,
         l_edate1(10) type c,
         l_edate2(10) type c .

  data : flag(1) type c,
         prev_fpper type  pc261-fpper,
         lv_molga type molga,
         lw_month  like  t5a4a-dlydy,
         $l_edate like l_edate.


  data: w_permo  like t549a-permo,   " Period parameters
        w_abkrt  like t549t-atext,   " Payroll Area Text
        w_begda  type begda,
        w_endda  type endda,
        w_abrjr  like t549q-pabrj,
        w_abrpr  like t549q-pabrp,
        $ix like sy-tabix.

* Read Payroll Control Records
  clear lv_molga.
  call function 'CU_READ_RGDIR'
    exporting
      persnr          = p_pernr
    importing
      molga           = lv_molga
    tables
      in_rgdir        = in_rgdir
    exceptions
      no_record_found = 1
      others          = 2.

  ws_pernr = p_pernr.

* Delete payroll control records based on selection input
  if not p_bperd[] is initial.
*    DELETE in_rgdir WHERE NOT paydt IN p_bperd.

    delete in_rgdir where not fpbeg in p_bperd.

*    LOOP AT in_rgdir.
*      $ix = sy-tabix.
*      w_abrpr = in_rgdir-fpper+4(2).
*      w_abrjr = in_rgdir-fpper(4).
*
*      PERFORM get_payroll_period USING p_abkrs
*                              CHANGING w_permo w_begda w_endda
*                                       w_abkrt w_abrpr w_abrjr.
*
*      IF w_begda IN p_bperd AND w_endda IN p_bperd.
*      ELSE.
*        DELETE in_rgdir INDEX $ix.
*      ENDIF.
*
*    ENDLOOP.


  endif.


** Delete payroll control records based on selection input
*  IF  NOT  s_fpper[]  IS INITIAL.
*    DELETE in_rgdir WHERE NOT fpper IN s_fpper. "Payroll Period
*  ENDIF.
* Delete payroll control records where payroll period is 000000
*  DELETE in_rgdir WHERE fpper EQ '000000'.

** Delete voided payroll data.
  delete in_rgdir where voidr ne space.

  delete in_rgdir where srtza ne 'A'. "Active

* Cluster id for US
* Personnel Country Grouping
  clear l_relid.

  select single relid into l_relid
                from t500l
                where molga = lv_molga.
  if   l_relid is initial.
    l_relid = 'RU'.
  endif.

  loop at in_rgdir into ws_rgdir.
    seqnr = ws_rgdir-seqnr.
    l_pernr = ws_pernr.

* Read Payroll cluster Data for each payroll control record
    call function 'PYXX_READ_PAYROLL_RESULT'
      exporting
        clusterid                    = l_relid
        employeenumber               = l_pernr
        sequencenumber               = seqnr
        read_only_international      = 'X'
      changing
        payroll_result               = result
      exceptions
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        error_reading_archive        = 8
        error_reading_relid          = 9
        others                       = 10.

    clear : ws_8387, ws_401k,ws_8337, ws_bc31, ws_gsal .

    loop at result-inter-rt into wa_rt
    where ( lgart = '/101' ).
      p_p_amount = p_p_amount + wa_rt-betrg.
    endloop.


  endloop.

endform.                    " read_payroll_data
*&---------------------------------------------------------------------*
*&      Form  get_emp_categ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_P0001_PERSG  text
*      -->P_LT_P0001_PERSK  text
*      <--P_LT_P0001_CATEG  text
*----------------------------------------------------------------------*
form get_emp_categ using    f_persg
                            f_persk
                   changing f_categ.

  constants:
   c_eg1(1)   type c value   'A',"US-Salary
   c_eg2(1)   type c value   'B',"US-Wage
   c_eg3(1)   type c value   'K'."KR-Salary

  if f_persg = '9' and f_persk = 'U2'.
    f_categ = c_eg3.
  elseif ( ( f_persg = '1' and f_persk = 'U2' ) or
           ( f_persg = '1' and f_persk = 'U3' ) ).
    f_categ = c_eg1.
  else.
    f_categ = c_eg2.
  endif.

endform.                    " get_emp_categ
*&---------------------------------------------------------------------*
*&      Form  get_payroll_period
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ABKRS  text
*      <--P_W_PERMO  text
*      <--P_W_BEGDA  text
*      <--P_W_ENDDA  text
*      <--P_W_ABKRT  text
*      <--P_P_ABRPR  text
*      <--P_W_ABRJR  text
*----------------------------------------------------------------------*
form get_payroll_period using v_abkrs
                     changing v_permo v_begda v_endda
                              v_abkrt v_pabrp v_pabrj.

  call function 'PA03_PERIODDATES_GET'
    exporting
      f_abkrs               = v_abkrs
    importing
      f_permo               = v_permo
      f_current_begda       = v_begda
      f_current_endda       = v_endda
      f_abkrs_text          = v_abkrt
    changing
      f_current_period      = v_pabrp
      f_current_year        = v_pabrj
    exceptions
      pcr_does_not_exist    = 1
      abkrs_does_not_exist  = 2
      period_does_not_exist = 3
      others                = 4.
  if sy-subrc <> 0.
*      message e003 with v_pabrp v_pabrj.
  endif.


endform.                    " get_payroll_period
