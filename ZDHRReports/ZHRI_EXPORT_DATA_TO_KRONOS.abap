*&----------------------------------------------------------------------
*& Program ID        : ZHRI_EXPORT_DATA_TO_KRONOS
*& Title             : [HR] - Team member data Export to Kronos
*& Created by        : Valerian Utama
*& Created on        : 02/08/2011
*& Specifications By : Euna Lee
*& Reference Pgm     : N/A
*& Description       : Extract Team Member master data to coma delimited
*&                     file (*.csv File).
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 02/08/11    Valerian  UD1K950832  Initial Program Development
*& 05/09/11    Valerian  UD1K951581  Fix logic to get Wage Type
*&                                   Replace Seniority with Rehire date
*& 08/02/2012  Valerian  UD1K955318  Overwrite Pay rule change date/
*&                                   effective date with Sch. eff. date
*&----------------------------------------------------------------------

report  zhri_export_data_to_kronos message-id zmfi.

constants: c_dlmtd(1)  type c value ',',
           c_dfile(30) type c value 'SAP_Employees.csv'.

type-pools: slis.

tables: pa0050, ps0041, pa0008.

field-symbols: <fs> type any.

data: v_file      type string,
      v_ans       type i.

data : gt_fieldcat type slis_t_fieldcat_alv.

data : begin of it_file_dlmtd occurs 0,
         record(500) type c,
       end of it_file_dlmtd.

data : begin of it_file occurs 0,
         pernr(8)              type c,
         nachn(40)             type c,
         vorna(40)             type c,
         midnm(40)             type c,
         rufnm(40)             type c,
         hiredate(10)          type c,
         stat2(1)              type c,
         statd(10)             type c,
         persk(2)              type c,
         efftd(10)             type c,
         usrid1(30)            type c,
         persg(1)              type c,
         stell(8)              type c,
         schkz(8)              type c,
         ccpa(8)               type c,
         btrtl(4)              type c,
         dept_code(8)          type c,
         kostl(10)             type c,
         orgeh(8)              type c,
         sachz(3)              type c,
         ll7(1)                type c,
         laeffd(10)            type c,
         prchgd(10)            type c,
         worktyp(1)            type c,
         wostd(7)              type c,
         arbst(7)              type c,
         zausw(8)              type c,
         beffd(10)             type c,
         sdurd(10)             type c,
         shift(1)              type c,
         mgrlf(1)              type c,
         mgred(10)             type c,
         wgtyp(15)             type c,
         sched(10)             type c,
         wgdat(10)             type c,
         usrid(8)              type c,

* by IG.MOON 7/27/2012 {
         zzpayrule(4)          type c,
         pay_rule_ef_dt(10)    type c,
* }

       end of it_file.

data : begin of it_data occurs 0,
         pernr                 type pa0000-pernr,
         nachn                 type pa0002-nachn,
         vorna                 type pa0002-vorna,
         midnm                 type pa0002-midnm,
         rufnm                 type pa0002-rufnm,
         hiredate              type sy-datum,
         stat2                 type pa0000-stat2,
         statd                 type pa0000-begda,
         persk                 type pa0001-persk,
         efftd                 type sy-datum,
         usrid1                type pa0105-usrid,
         persg                 type pa0001-persg,
         stell                 type pa0001-stell,
         schkz                 type pa0007-schkz,
         ccpa(8)               type c,
         btrtl                 type pa0001-btrtl,
         dept_code(8)          type n,
         kostl                 type pa0001-kostl,
         orgeh                 type pa0001-orgeh,
         sachz                 type pa0001-sachz,
         ll7(1)                type c,
         laeffd                type pa0001-begda,
         prchgd                type pa0001-begda,
         worktyp(1)            type c,
         wostd                 type pa0007-wostd,
         arbst                 type pa0007-arbst,
         zausw                 type pa0050-zausw,
         beffd                 type pa0050-begda,
         sdurd                 type sy-datum,
         shift(1)              type c,
         mgrlf(1)              type c,
         mgred                 type pa0050-begda,
         wgtyp                 type pa0008-bet01,
         sched                 type pa0007-begda,
         wgdat                 type pa0008-begda,
         werks                 type pa0001-werks,
         bukrs                 type pa0001-bukrs,
         dept_name(100)        type c,
         team_code(20)         type c,
         team_name(100)        type c,
         begda                 type pa0000-begda,
         usrid                 type t526-usrid,
         aedtm                 type aedtm,
* by IG.MOON 7/27/2012 {
         zzpayrule             type pa0007-zzpayrule,
         pay_rule_ef_dt        type pa0007-begda,
* }

       end of it_data.

selection-screen begin of block blk1 with frame title text-t01.
select-options: s_pernr for it_data-pernr,
                s_persg for it_data-persg,
**Furong on 05/08/14 (
                s_sachz for it_data-SACHZ.
** ) End
selection-screen skip.
parameters: p_test as checkbox default 'X' user-command chck.
selection-screen end of block blk1.

selection-screen begin of block blk2 with frame title text-t02.
parameters: p_pres radiobutton group serv user-command rad
                                        modif id chk default 'X',
            p_appl radiobutton group serv modif id chk.
selection-screen skip.
parameters: p_file(1024) type c lower case modif id chk
                                visible length 45.
selection-screen end of block blk2.

at selection-screen output.
  loop at screen.
    if screen-group1 = 'CHK'.
      if not p_test is initial.
        screen-active = 0.
        modify screen.
      endif.
    endif.
  endloop.

at selection-screen.
  check sy-ucomm ne 'CHCK'.
  if p_test is initial and
     p_file is initial.
    message e000 with 'Please Enter Filename'.
  endif.

  v_file = p_file.

at selection-screen on value-request for p_file.
  if not p_pres is initial.
    perform browser changing p_file v_ans.
  else.
    perform display_unix changing p_file v_ans.
  endif.

start-of-selection.
  perform select_data.
  perform modify_data.
  perform output_data.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
form select_data.

  data: l_vdate type dats.

  select  a~ename a~ename as ename1 a~stell a~plans a~werks a~orgeh
          b~vorna b~nachn c~pernr a~persg c~stat2 c~massn c~begda
          b~midnm b~rufnm a~btrtl a~kostl a~sachz a~bukrs a~persk
          a~begda as laeffd c~aedtm

  into corresponding fields of table it_data
  from pa0001 as a
      inner join pa0002 as b
          on a~pernr  =  b~pernr
      inner join pa0000 as c
          on a~pernr  =  c~pernr

  where  a~endda = '99991231'
    and  b~endda = '99991231'
    and  c~endda = '99991231'
* 08/16/2013 - T00306 Start
    and  c~stat2 in (0,1,3)
* 08/16/2013 - T00306 End
    and  a~pernr in s_pernr
    and  a~persg in s_persg
** Furong on 05/08/14 (
    and  sachz in s_sachz.
** ) End
* 08/16/2013 - T00306 Start
  l_vdate = sy-datum - 14.
  delete it_data where stat2 eq 0
                   and aedtm < l_vdate.
* 08/16/2013 - T00306 End

  sort it_data by pernr.

endform.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
form modify_data .
  data: l_rec   type i,
        l_tprog type tprog,
        l_col   type sy-index.

* by IG.MOON 7/27/2012 {
  data $mosid(2) type n.
* }

  describe table it_data lines l_rec.

  loop at it_data.
* Show progress indicator.
    perform show_progress using 'Processing Data...' l_rec.

* Get E-Mail Address
    select usrid_long into it_data-usrid1
      from pa0105
     up to 1 rows
     where pernr = it_data-pernr
       and endda = '99991231'
       and usrty = '0010'.
    endselect.

* Get Departement Info
    perform get_dept_info.

* Get Hire Date and Rehire Date
    l_col = 1.

    select *
      into corresponding fields of ps0041
      from pa0041
     up to 1 rows
     where pernr = it_data-pernr
       and endda = '99991231'.

      while it_data-hiredate is initial or
            it_data-sdurd    is initial.

        assign component l_col of structure ps0041 to <fs>.
        if sy-subrc <> 0.
          exit.
        endif.

        case <fs>.
** Furong on 06/13/14 (
*          when 'Z1'.
           when 'ZC'.
** )
            l_col = l_col + 1.
            assign component l_col of structure ps0041 to <fs>.
            it_data-hiredate = <fs>.
            l_col = l_col - 1.
          when 'Z2'.                                        "UD1K951581
          when 'ZB'.
            l_col = l_col + 1.
            assign component l_col of structure ps0041 to <fs>.
            it_data-sdurd = <fs>.
            l_col = l_col - 1.
        endcase.

        l_col = l_col + 2.
      endwhile.
    endselect.

* Get Employment Status Effective Date
    it_data-statd = it_data-begda.

* Get Accrual Effective Date
    it_data-efftd = it_data-hiredate.

* Get Company Code/Personal Area
    concatenate it_data-bukrs it_data-werks into it_data-ccpa.

* Get Pay Rule Change Date
    it_data-prchgd = it_data-laeffd.

* Get schedule code, weekly hours, daily hours, sch eff.date
    select schkz arbst wostd begda as sched
* by IG.MOON 7/27/2012 {
        zzpayrule
* }
      into corresponding fields of it_data
      from pa0007
     up to 1 rows
     where pernr = it_data-pernr
       and endda = '99991231'.
    endselect.

* Get Shift code
    if it_data-schkz = '8000_A' or
       it_data-schkz = '8000_B' or
       it_data-schkz = '8000_C'.

      call function 'Z_CO_GET_DWS_IG'
        exporting
          schkz                          = it_data-schkz
          datum                          = '99991231'
        importing
          tprog                          = l_tprog
        exceptions
          not_found_work_schedule_rules  = 1
          invalid_date                   = 2
          not_found_period_work_schedule = 3
          others                         = 4.

      case l_tprog.

        when '0002' or '1003' or '1002' or '1009' or '2006'.

          it_data-shift = '2'.

        when others.

          it_data-shift = '1'.

      endcase.

    else.

* by IG.MOON 7/27/2012 {
*      IF it_data-schkz = '1000' OR
*         it_data-schkz = '1001' OR
*         it_data-schkz = '2001' OR
*         it_data-schkz = '4001' OR
*         it_data-schkz = '9001'.
*        it_data-shift = '1'.
*      ELSEIF it_data-schkz = '1002' OR
*             it_data-schkz = '1003' OR
*             it_data-schkz = '2002' OR
*             it_data-schkz = '9002'.
*        it_data-shift = '2'.
*      ENDIF.
* }

      case it_data-btrtl.
        when '0004' or '0005' or '0001' or '0002'.
          $mosid = '10'.
        when '0003'.
          $mosid = '9'.
        when '0006'.
          $mosid = '8'.
        when others.
      endcase.

      clear it_data-shift.
      select single anzsh into it_data-shift
         from ztco_mh_ws where kokrs eq 'H201'
                         and   mosid eq $mosid
                         and   schkz eq it_data-schkz.

*}

    endif.

* Get badge number, badge eff.date, mgr. license flag, mgr. eff.date
    select *
      from pa0050
     up to 1 rows
     where pernr = it_data-pernr
       and endda = '99991231'.

      it_data-zausw = pa0050-zausw.
      it_data-beffd = pa0050-begda.
      if pa0050-zanbe = '01'.
        it_data-mgrlf = 'Y'.
        it_data-mgred = pa0050-begda.
      else.
        it_data-mgrlf = 'N'.
        clear it_data-mgred.
      endif.
    endselect.

* Get Wage Type
    select *
      into pa0008
      from pa0008
     up to 1 rows
     where pernr = it_data-pernr
       and endda = '99991231'.

      if it_data-persk = 'U2' or
         it_data-persk = 'U3' or
         it_data-persk = 'UD'.

        if ( pa0008-lga01 = '0003' or pa0008-lga01 = '0007' )
             and not pa0008-divgv is initial.

          it_data-wgtyp = pa0008-bet01 / pa0008-divgv.
          it_data-wgdat = pa0008-begda.
        endif.

      elseif it_data-persk = 'U0'.
        if pa0008-lga01 = '0001'.
          it_data-wgtyp = pa0008-bet01.
          it_data-wgdat = pa0008-begda.
* BEGIN OF UD1K951581
          check it_data-wgtyp is initial.
          select betrg into it_data-wgtyp
          from t510
          up to 1 rows
          where molga = '10'
            and trfar = pa0008-trfar
            and trfgb = pa0008-trfgb
            and trfgr = pa0008-trfgr
            and trfst = pa0008-trfst
            and begda <= pa0008-begda
            and endda >= pa0008-endda.
          endselect.
* END OF UD1K951581
        endif.
      endif.
    endselect.

* Get Report to user id
    select single usrid into it_data-usrid
      from t526
     where werks = 'HMMA'
       and sachx = it_data-sachz.

* BEGIN OF UD1K955318
* Overwrite Pay rule change date and Pay rule effective date with
* Schedule effective date
    it_data-prchgd         = it_data-sched.
    it_data-pay_rule_ef_dt = it_data-sched.
* END OF UD1K955318

    modify it_data.
  endloop.

* Move data to output structure
  set locale language sy-langu.

  loop at it_data.
    move-corresponding it_data to it_file.

    write: it_data-hiredate to it_file-hiredate,
           it_data-statd    to it_file-statd,
           it_data-efftd    to it_file-efftd,
           it_data-laeffd   to it_file-laeffd,
           it_data-prchgd   to it_file-prchgd,
           it_data-beffd    to it_file-beffd,
           it_data-sdurd    to it_file-sdurd,
           it_data-mgred    to it_file-mgred,
           it_data-sched    to it_file-sched,
           it_data-wgdat    to it_file-wgdat,

           it_data-pay_rule_ef_dt to                        "UD1K955318
           it_file-pay_rule_ef_dt.                          "UD1K955318

    shift: it_file-wostd left deleting leading space,
           it_file-arbst left deleting leading space,
           it_file-wgtyp left deleting leading space.

    unpack it_data-usrid to it_file-usrid.

* by IG.MOON 7/27/2012 {
*   it_file-pay_rule_ef_dt = it_file-laeffd.                "UD1K955318
* }

    append it_file. clear it_file.
  endloop.

  free it_data.

  message s000 with 'Total record(s) extracted:' l_rec.

endform.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display Data
*----------------------------------------------------------------------*
form display_data.
  perform fieldcat_init     using gt_fieldcat[].
  perform alv_grid_display  tables it_file.
endform.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       Transfer Data to Desktop/PC or UNIX/Presentation Server
*----------------------------------------------------------------------*
form transfer_data.
  data: l_totrec type i.

  loop at it_file.
    do.
      assign component sy-index of structure it_file to <fs>.
      if sy-subrc <> 0.
        exit.
      endif.

      if not it_file_dlmtd is initial.
        concatenate it_file_dlmtd-record c_dlmtd <fs>
               into it_file_dlmtd-record.
      else.
        it_file_dlmtd-record = <fs>.
      endif.
    enddo.

    append it_file_dlmtd. clear it_file_dlmtd.
  endloop.

  describe table it_file_dlmtd lines l_totrec.

  case 'X'.
    when p_appl.
      open dataset v_file for output in text mode.
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      loop at it_file_dlmtd.
        transfer it_file_dlmtd to v_file.
      endloop.

      close dataset v_file.

    when p_pres.
      if not sy-batch is initial.
        message s000
           with 'Writing File to Desktop Not Possible'
                'in Background Mode' ' ' ' '.
        exit.
      endif.

      call function 'GUI_DOWNLOAD'
        exporting
          filename                = v_file
        tables
          data_tab                = it_file_dlmtd
        exceptions
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          others                  = 22.

      if sy-subrc <> 0.
        if sy-subrc = 15.
          message s011 with 'Access Denied'.
        elseif not sy-msgid is initial.
          message id sy-msgid type sy-msgty number sy-msgno
          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        else.
          message s011 with 'Error when creating the file'.
        endif.
      endif.
  endcase.

  if sy-subrc eq 0.
    message s000 with 'File is written to:' v_file l_totrec
                      'Record(s)'.
  endif.

endform.                    " TRANSFER_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_DEPT_INFO
*&---------------------------------------------------------------------*
*       Get Department Information
*----------------------------------------------------------------------*
form get_dept_info.

  data: it_objec type objec occurs 0 with header line,
        it_struc type struc occurs 0 with header line,
        l_level  type struc-level.

  data: ls_pa0001 like pa0001.

  data: l_begda type dats,
        l_endda type dats.

  data: l_varyf type varyf,
        l_sobid type sobid,
        l_dept_code(8) type n.

  data: l_otjid like hrp1000-otjid.

  data: begin of lt_info occurs 0,
          sobid type sobid,
          begda type dats,
          endda type dats,
        end of lt_info.

  check not it_data-orgeh is initial.

*** 07/15/2013 - T00306 Start
*  l_begda = '99991231'.
  l_endda = '99991231'.

  case it_data-stat2.
    when 0 or 2.
      select * into ls_pa0001
        from pa0001
       where pernr = it_data-pernr
       order by endda descending.
        if ls_pa0001-endda ne '99991231'.
*          l_begda = ls_pa0001-begda.
          l_endda = ls_pa0001-endda.
*          it_data-orgeh = ls_pa0001-orgeh.
          exit.
        endif.
      endselect.

*      select single orgeh into it_data-orgeh
*        from pa0001
*       where pernr = it_data-pernr
*         and begda = l_begda
*         and endda = l_endda.
  endcase.
*** 07/15/2013 - T00306 End

  call function 'RH_STRUC_GET'
    exporting
      act_otype              = 'O'
      act_objid              = it_data-orgeh
      act_wegid              = 'O-O'
*     ACT_INT_FLAG           =
*     act_plvar              = '01'
      act_begda              = l_endda
      act_endda              = l_endda
*     ACT_TDEPTH             = 0
*     ACT_TFLAG              = 'X'
*     ACT_VFLAG              = 'X'
*     AUTHORITY_CHECK        = 'X'
*     TEXT_BUFFER_FILL       =
*     BUFFER_MODE            =
*   IMPORTING
*     ACT_PLVAR              =
    tables
*     RESULT_TAB             =
      result_objec           = it_objec
      result_struc           = it_struc
    exceptions
      no_plvar_found         = 1
      no_entry_found         = 2
      others                 = 3
            .
  if sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* Get Team code or Department code
  loop at it_struc.
    clear: l_dept_code, l_varyf.

    select sobid begda endda
      into corresponding fields of table lt_info
      from hrp1001
     where objid = it_struc-objid
       and subty = 'AZ03'
       and otype = 'O'
       and sclas = 'OL'
     order by endda descending.

    check sy-subrc eq 0.

    loop at lt_info where begda <= l_endda
                      and endda >= l_endda.
      exit.
    endloop.

    if sy-subrc ne 0.
      read table lt_info index 1.
    endif.

    if sy-subrc eq 0 and lt_info-sobid <= 40.
      it_data-dept_code = it_struc-objid.
      exit.
    endif.

  endloop.


endform.                    "get_dept_info

*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       Show Progress Indicator
*----------------------------------------------------------------------*
*      -->pf_text  Message Text
*      -->pf_val   Total Records
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            pf_val.

  statics: l_text(50) type c,
           l_baseval  type i,
           l_percent  type i,
           l_counter  type i.

  if l_text ne pf_text.
    l_text = pf_text.
    clear: l_baseval,
           l_percent,
           l_counter.
  endif.

  if not l_baseval is initial.
    l_counter = l_counter - 1.
    check l_counter le 0.
    l_percent = l_percent + 10.
    check l_percent le 100.
    l_counter = l_baseval.
  else.
    l_baseval = pf_val div 10.
    l_counter = l_baseval.
  endif.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = l_percent
      text       = pf_text.

endform.                    " SHOW_PROGRESS

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->ft_fieldcat  Field Catalog Value
*----------------------------------------------------------------------*
form fieldcat_init using ft_fieldcat type slis_t_fieldcat_alv .

  data: l_pos type i,
        gs_fieldcat type slis_fieldcat_alv.

  define __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  end-of-definition.

  __catalog :
    'X'  'PERNR'      'Emp.No.'          8   'CHAR' '' '',
    ' '  'NACHN'      'Last Name'        40  'CHAR' '' '',
    ' '  'VORNA'      'First Name'       40  'CHAR' '' '',
    ' '  'MIDNM'      'Middle Initial'   40  'CHAR' '' '',
    ' '  'RUFNM'      'Short Name'       40  'CHAR' '' '',
    ' '  'HIREDATE'   'Hire Date'        10  'CHAR' '' '',
    ' '  'STAT2'      'Emp.Stat.'        1   'CHAR' '' '',
    ' '  'STATD'      'Emp.Eff.Date'     10  'CHAR' '' '',
    ' '  'PERSK'      'Emp.Type'         2   'CHAR' '' '',
    ' '  'EFFTD'      'Acc.Eff.Date'     10  'CHAR' '' '',
    ' '  'USRID1'     'Email'            30  'CHAR' '' '',
    ' '  'PERSG'      'Emp.Group'        1   'CHAR' '' '',
    ' '  'STELL'      'Job Code'         8   'CHAR' '' '',
    ' '  'SCHKZ'      'Sch.Code'         8   'CHAR' '' '',
    ' '  'CCPA'       'CoCd/P.Area'      8   'CHAR' '' '',
    ' '  'BTRTL'      'Sub area'         4   'CHAR' '' '',
    ' '  'DEPT_CODE'  'Department'       8   'CHAR' '' '',
    ' '  'KOSTL'      'Cost Center'      10  'CHAR' '' '',
    ' '  'ORGEH'      'Org.Unit'         8   'CHAR' '' '',
    ' '  'SACHZ'      'Time Admin Grp.'  3   'CHAR' '' '',
    ' '  'LL7'        'LL7'              1   'CHAR' '' '',
    ' '  'LAEFFD'     'Labor Eff.Date'   10  'CHAR' '' '',
    ' '  'PRCHGD'     'Pay Rule Chg.dat' 10  'CHAR' '' '',
    ' '  'WORKTYP'    'Worker Type'      1   'CHAR' '' '',
    ' '  'WOSTD'      'Weekly Hr.'       7   'CHAR' '' '',
    ' '  'ARBST'      'Daily Hr.'        7   'CHAR' '' '',
    ' '  'ZAUSW'      'Badge No.'        8   'CHAR' '' '',
    ' '  'BEFFD'      'Badge Eff.Date'   10  'CHAR' '' '',
    ' '  'SDURD'      'Rehire Date'      10  'CHAR' '' '',
    ' '  'SHIFT'      'Shift'            1   'CHAR' '' '',
    ' '  'MGRLF'      'Mgr.License'      1   'CHAR' '' '',
    ' '  'MGRED'      'Mgr.Eff.Date'     10  'CHAR' '' '',
    ' '  'WGTYP'      'Base Wage'        15   'CHAR' '' '',
    ' '  'SCHED'      'Sch. Date'        10  'CHAR' '' '',
    ' '  'WGDAT'      'B.Wage Eff.Date'  10  'CHAR' '' '',
    ' '  'USRID'      'Reports to'       12  'CHAR' '' '',

* by IG.MOON 7/27/2012 {
    ' '  'ZZPAYRULE'      'Pay rule'      4  'CHAR' '' '',
    ' '  'PAY_RULE_EF_DT' 'Pay Rule Eff.dat' 10  'CHAR' '' ''.
* }


endform.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
form alv_grid_display tables ft_outtab.

  data: l_repid type sy-repid.
  data : gs_layout type slis_layout_alv.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  l_repid = sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = l_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    tables
      t_outtab           = ft_outtab
    exceptions
      program_error      = 1
      others             = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       Browse Desktop/PC Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
form browser changing filename answer.
  data  $filename type string.
  data: l_path  type string,
        l_fpath type string,
        l_dfile type string.

  l_dfile = c_dfile.

  call method cl_gui_frontend_services=>file_save_dialog
    exporting
      window_title      = 'Select File Name'
      default_extension = 'csv'
      default_file_name = l_dfile
      file_filter       = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
      initial_directory = '\\10.121.233.22\Data'
    changing
      filename          = $filename
      path              = l_path
      fullpath          = l_fpath
      user_action       = answer
    exceptions
      cntl_error        = 1
      error_no_gui      = 2
      others            = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  elseif answer = 0.
    filename = l_fpath.
  else.
    message s118(ba).
  endif.

endform.                    " BROWSER

*&---------------------------------------------------------------------*
*&      Form  output_data
*&---------------------------------------------------------------------*
*       Display data or write data to file
*----------------------------------------------------------------------*
form output_data.
  check not it_file[] is initial.

  if p_test is initial.
    perform transfer_data.
  else.
    perform display_data.
  endif.
endform.                    " output_data

*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
form display_unix changing filename answer.
  data: begin of it_filename occurs 0,
          path(1024) type c,
        end of it_filename.

  select dirname
    from user_dir
    into table it_filename
   where aliass = 'DIR_KRONOS'.

  call 'C_SAPGPARAM' id 'NAME'  field 'DIR_TEMP'
                     id 'VALUE' field it_filename-path.
  append it_filename.

  call function 'POPUP_WITH_TABLE'
    exporting
      endpos_col   = '100'
      endpos_row   = '10'
      startpos_col = '1'
      startpos_row = '1'
      titletext    = 'Select UNIX Directory'
    importing
      choice       = filename
    tables
      valuetab     = it_filename
    exceptions
      break_off    = 1
      others       = 2.

  answer = sy-subrc.

  if sy-subrc = 0.
    concatenate filename '/' c_dfile into filename.
  else.
    message s549(fibl).
  endif.
endform.                    " display_unix
