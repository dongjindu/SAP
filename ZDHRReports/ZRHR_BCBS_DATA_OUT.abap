*&----------------------------------------------------------------------
*& Program ID        : ZRHR_BCBS_DATA_OUT
*& Title             : [HR] - BCBS Outbound Data Transfer
*& Created by        : Sunho Jeong
*& Created on        : 10/09/2013
*& Specifications By : Grace Li
*& Reference Pgm     : N/A
*& Description       : Extract outbound data for BCBS
*&                     (*.csv File).
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*&
*&----------------------------------------------------------------------

report  zrhr_bcbs_data_out message-id zmfi.

tables: pa0006,
        zthr_dummy_ssn.

constants:
  c_dlmtd(1)     type c value cl_abap_char_utilities=>horizontal_tab,
  c_dpath        type string value 'C:\TEMP\BCBS\',
  c_dextn        type string value '.TXT',
  c_unix_pre(30) type c value '/sapmnt/',
  c_unix_suf(30) type c value '/kronos/kronosftp'.

data: l_dfile    type string value 'HMMA_MONTHLY_PAYMENT_'.

constants: c_active(1) type   c value 1.

type-pools: slis.

field-symbols: <fs> type any.


data: v_file      type string,
      v_ans       type i.

data : gt_fieldcat type slis_t_fieldcat_alv,
       gs_fieldcat type slis_fieldcat_alv.

data : begin of it_file_dlmtd occurs 0,
         record(500) type c,
       end of it_file_dlmtd.

data : it_file  like table of zshr_bcbs_out  with header line,
       l_cdate type dats.

data : begin of it_data occurs 0.
data :  perid type prdni,
        pardt type ben_partdt,
        begda type begda,
        endda type endda,
        vorna type pad_vorna,
        midnm type pad_midnm,
        nachn type pad_nachn,
        namzu type namzu,
        depcv type ben_depcov,
        stras type stras,
        locat type locat,
        ort01 type ort01,
        state type regio,
        pstlz type pstlz,
        gesch type pa0002-gesch,
        gbdat type dats,
        pernr type persno,

        dty01 type ben_deptyp,
        did01 type ben_depid,
        dty02 type ben_deptyp,
        did02 type ben_depid,
        dty03 type ben_deptyp,
        did03 type ben_depid,
        dty04 type ben_deptyp,
        did04 type ben_depid,
        dty05 type ben_deptyp,
        did05 type ben_depid,
        dty06 type ben_deptyp,
        did06 type ben_depid,
        dty07 type ben_deptyp,
        did07 type ben_depid,
        dty08 type ben_deptyp,
        did08 type ben_depid,
        dty09 type ben_deptyp,
        did09 type ben_depid,
        dty10 type ben_deptyp,
        did10 type ben_depid,
        dty11 type ben_deptyp,
        did11 type ben_depid,
        dty12 type ben_deptyp,
        did12 type ben_depid,
        dty13 type ben_deptyp,
        did13 type ben_depid,
        dty14 type ben_deptyp,
        did14 type ben_depid,
        dty15 type ben_deptyp,
        did15 type ben_depid,
        dty16 type ben_deptyp,
        did16 type ben_depid,
        dty17 type ben_deptyp,
        did17 type ben_depid,
        dty18 type ben_deptyp,
        did18 type ben_depid,
        dty19 type ben_deptyp,
        did19 type ben_depid,
        dty20 type ben_deptyp,
        did20 type ben_depid,

        aedtm type dats,
        needchkdep,
       end of it_data.

* BEGIN OF UD1K955233
data: begin of it_phone occurs 0,
       pernr type pernr-pernr,
       com01 type pa0006-com01,
       num01 type pa0006-num01,
       com02 type pa0006-com02,
       num02 type pa0006-num02,
       com03 type pa0006-com03,
       num03 type pa0006-num03,
       com04 type pa0006-com04,
       num04 type pa0006-num04,
       com05 type pa0006-com05,
       num05 type pa0006-num05,
       com06 type pa0006-com06,
       num06 type pa0006-num06,
      end of it_phone.
* END OF UD1K955233


data : begin of it_pdata occurs 0,
      pernr	type p_pernr,
      nachn type nachn,
      vorna type vorna,
      stat2 type stat2,
      massn type massn,
      betrg type pa0008-bet08,
end of it_pdata.

data: gt_temp like table of it_data with header line.

selection-screen begin of block blk2 with frame title text-t02.
parameters: p_pres radiobutton group serv user-command rad
                                        modif id chk default 'X',
            p_appl radiobutton group serv modif id chk.
selection-screen skip.
parameters: p_file(1024) type c lower case modif id chk
                                visible length 45.
* BEGIN OF UD1K955233
selection-screen skip.
parameters: p_eai  radiobutton group serv modif id chk,
            p_dest type rfcdest modif id chk.

selection-screen skip.
parameters: p_tdate like sy-datum obligatory.
* END OF UD1K955233
selection-screen end of block blk2.

selection-screen begin of block blk4 with frame title text-t02.
select-options: s_bperd for sy-datum modif id bpe no-extension obligatory.
selection-screen end of block blk4.

initialization.
  p_dest = 'WMHR01'.
  perform set_date_ranges_by_radio.
  p_tdate = sy-datum - 14.


at selection-screen output.
  loop at screen.
    if screen-group1 = 'CHK'.
      if sy-tcode = 'ZHR001' and
          ( screen-name = 'P_APPL' or screen-name = 'P_EAI'
                                   or screen-name = 'P_DEST').
        screen-input = 0.
        modify screen.
      endif.
    endif.

    if screen-name = 'P_GDAT'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.


at selection-screen.

  if sy-ucomm eq 'RAD2'.
    perform set_date_ranges_by_radio.
  endif.

  if p_file is initial.
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
* Employee informaton
  perform select_employee_data.

  perform modify_employee_data.
* Dependant information
  perform process_dependant_data.
* Sort
  sort it_file by sussn.
* Send out
  perform output_data.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
form select_employee_data.

  data: begin of lt_pernr occurs 0,
          pernr type persno,
        end of lt_pernr,
        lt_pernr_temp like table of lt_pernr with header line.

  data: l_begda type pa0001-begda,
        l_endda type pa0001-endda.

  data: l_cnt type i.

  data: l_tabix type i.
  data: l_numc(2) type n,
        l_fname(20) type c,
        l_index type i.

  data: l_dtyxx type ben_deptyp,
        l_didxx type ben_depid.
  data: l_valid.
  field-symbols: <fs_fcnum> type any.


  select pernr
    into corresponding fields of table lt_pernr
    from pa0006
   where anssa = '5'
     and ( ( begda in s_bperd or endda in s_bperd )
         or ( begda < s_bperd-low and aedtm > p_tdate )
         or ( endda < s_bperd-low and aedtm > p_tdate ) ).

  select pernr
    appending corresponding fields of table lt_pernr
    from pa0002
   where ( ( begda in s_bperd or endda in s_bperd )
         or ( begda < s_bperd-low and aedtm > p_tdate )
         or ( endda < s_bperd-low and aedtm > p_tdate ) ).

  select pernr
    appending corresponding fields of table lt_pernr
    from pa0167
   where subty = 'MEDI'
     and ( ( begda in s_bperd or endda in s_bperd )
         or ( begda < s_bperd-low and aedtm > p_tdate )
         or ( endda < s_bperd-low and aedtm > p_tdate ) ).

  select pernr
    into corresponding fields of table lt_pernr_temp
    from pa0021
   where  ( ( begda in s_bperd or endda in s_bperd )
         or ( begda < s_bperd-low and aedtm > p_tdate )
         or ( endda < s_bperd-low and aedtm > p_tdate ) ).


  loop at lt_pernr_temp.

    l_tabix = sy-tabix.

    clear l_valid.
    do 20 times.
      l_numc = sy-index.
      concatenate 'IT_DATA-DTY' l_numc into l_fname.
      assign (l_fname) to <fs_fcnum>.
      l_dtyxx = <fs_fcnum>.

      concatenate 'IT_DATA-DID' l_numc into l_fname.
      assign (l_fname) to <fs_fcnum>.
      l_didxx = <fs_fcnum>.

      check not ( l_dtyxx is initial
             and l_didxx is initial ).

      select single count(*) into l_cnt
        from pa0021
       where pernr = lt_pernr_temp-pernr
         and subty = l_dtyxx
         and objps = l_didxx.
      if sy-subrc eq 0.
        l_valid = 'X'.
        exit.
      endif.
    enddo.

    if l_valid ne 'X'.
      delete lt_pernr_temp index l_tabix.
    endif.

  endloop.

  append lines of lt_pernr_temp to lt_pernr.
  sort lt_pernr by pernr.
  delete adjacent duplicates from lt_pernr.

  sort lt_pernr_temp by pernr.

  if not lt_pernr[] is initial.

    select b~perid c~pardt c~endda b~vorna
           b~midnm b~nachn b~namzu c~depcv
           a~stras a~locat a~ort01 a~state
           a~pstlz b~gesch b~gbdat b~pernr
           c~dty01 c~did01 c~dty02 c~did02
           c~dty03 c~did03 c~dty04 c~did04
           c~dty05 c~did05 c~dty06 c~did06
           c~dty07 c~did07 c~dty08 c~did08
           c~dty09 c~did09 c~dty10 c~did10
           c~dty11 c~did11 c~dty12 c~did12
           c~dty13 c~did13 c~dty14 c~did14
           c~dty15 c~did15 c~dty16 c~did16
           c~dty17 c~did17 c~dty18 c~did18
           c~dty19 c~did19 c~dty20 c~did20
           c~begda c~aedtm
      into corresponding fields of table it_data
      from pa0006 as a join pa0002 as b
                         on a~pernr = b~pernr
                       join pa0167 as c
                         on a~pernr = c~pernr
       for all entries in lt_pernr
     where a~pernr = lt_pernr-pernr
       and a~endda = '99991231'
       and b~endda = '99991231'
       and c~subty = 'MEDI'
       and a~subty = '5'
       and c~depcv <> 'WAIV'.

    sort it_data by perid.

    loop at it_data.
      l_tabix = sy-tabix.
      read table lt_pernr_temp with key it_data-pernr binary search.

      select single *
        from zthr_dummy_ssn
       where pernr = it_data-pernr.

      if sy-subrc eq 0.
        delete it_data index l_tabix.
      endif.
    endloop.
  endif.

endform.                    "select_employee_data
*&---------------------------------------------------------------------*
*&      Form  modify_employee_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form modify_employee_data.

  data: l_rec     type i,
        l_darxx   type ps0041-dar01,
        l_datxx   type ps0041-dat01,
        l_endda type dats,
        l_begda type dats,
        l_index type i,
        l_temp_date type dats.

  data: l_numc(2) type n,
        l_fname(20) type c,
        l_tabix like sy-tabix,
        l_valid,
        l_dtyxx type ben_deptyp,
        l_didxx type ben_depid,
        l_cnt type i.

  data: l_first.
  data: l_stat2 like pa0000-stat2.

  field-symbols: <fs_fcnum> type any.


  data: begin of lt_termination occurs 0,
          begda type pa0000-begda,
          massn type pa0000-massn,
        end of lt_termination.
  data: l_cddate type dats.


  gt_temp[] = it_data[].
  sort gt_temp by pernr begda descending.

  l_cddate = s_bperd-low - 1.
* Move data to output structure
  delete it_data where endda < l_cddate and aedtm <= p_tdate.
  delete it_data where begda > s_bperd-high and aedtm <= p_tdate.

  sort it_data by begda descending.
*  describe table it_data lines l_rec.


  loop at it_data.

    clear: l_first.
    at new perid.
      l_first = 'X'.
    endat.

    check l_first eq 'X'.
    move-corresponding it_data to it_file.

* Sub SSN
    it_file-sussn = it_data-perid.
* Group Number
    it_file-grnum = '48548'.
* Division
    it_file-divsn = '001'.

* Effective Date
    it_file-edate = it_data-pardt.
    clear: l_begda, l_endda, l_index.
    read table gt_temp with key pernr = it_data-pernr binary search.
    if sy-subrc eq 0.
      loop at gt_temp from sy-tabix where pernr = it_data-pernr.
        " and depcv <> 'WAIV'.
        add 1 to l_index.
        if l_index eq 1.
          l_begda = gt_temp-begda.
          l_endda = gt_temp-endda.
        else.
          l_temp_date = gt_temp-endda + 1.
          if l_begda = l_temp_date.
            l_begda = gt_temp-begda.
            l_endda = gt_temp-endda.
          else.
            exit.
          endif.
        endif.
      endloop.
      it_file-edate = l_begda.
    endif.

* New or Update
    if it_file-edate in s_bperd.
      it_file-zflag = 'N'.
    elseif gt_temp-aedtm > p_tdate and gt_temp-endda eq '99991231'.
      it_file-zflag = 'N'.
    else.
      it_file-zflag = 'U'.
    endif.

    read table gt_temp with key perid = it_data-perid.
* Cancel Date
    clear l_cdate.
    if gt_temp-endda <> '99991231'.
      l_cdate = gt_temp-endda + 1.
      it_file-cdate = l_cdate.
    endif.

*** Additional Logic
    clear l_stat2.
    select single stat2
     into l_stat2
     from pa0000
    where pernr = it_data-pernr
      and endda = '99991231'.

    if l_stat2 eq '0'.
      it_file-cdate = it_file-cdate - 1.
      if it_file-cdate+4(2) = 12.
        it_file-cdate+0(4) = it_file-cdate+0(4) + 1.
        it_file-cdate+4(4) = '0101'.
      else.
        it_file-cdate+4(2) = it_file-cdate+4(2) + 1.
        it_file-cdate+6(2) = '01'.
      endif.
    endif.

* Middle Name
    it_file-midnm = it_data-midnm.
* Type Contract
    case it_data-depcv.
      when 'EE+F'.
        it_file-ctype = 'FAMILY'.
      when 'EE'.
        it_file-ctype = 'ONE PERSON'.
    endcase.

* Relation
    it_file-relat = 'E'.

* Gender
    if it_data-gesch = '1'.
      it_file-gendr = 'm'.
    elseif it_data-gesch = '2'.
      it_file-gendr = 'f'.
    else.
      clear it_file-gendr.
    endif.

* Dep SSN
    it_file-dessn = it_data-perid.

* Get Date of Birth
    it_file-gbdat = it_data-gbdat.
* EMP #
    it_file-empnr = it_data-pernr.
* Dental
    it_file-dentl = 'Y'.

    translate it_file to upper case.
    append it_file. clear it_file.
  endloop.

*  free it_data.

  describe table it_file lines l_rec.
  message s000 with 'Total record(s) extracted:' l_rec.

endform.                    "modify_employee_data
*&---------------------------------------------------------------------*
*&      Form  process_dependant_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form process_dependant_data.

  data: begin of lt_dependant occurs 0,
          pernr type persno,
          perid type prdni,
          dtyxx type ben_deptyp,
          didxx type ben_depid,
        end of lt_dependant.

  data: l_numc(2) type n,
        l_fname(20) type c,
        l_tabix like sy-tabix,
        l_index type i.

  data: lt_0021 type table of pa0021 with header line,
        lt_0106 type table of pa0106 with header line.
  field-symbols: <fs_fcnum> type any.

  data: l_endda type dats,
        l_begda type dats,
        l_temp_date type dats,
        l_cdate type dats.

  data: l_exist,
        l_cnt type i.

  data: ls_femp like line of it_file.
  data: l_stat2 like pa0000-stat2.

  sort gt_temp  by pernr begda descending.

  loop at it_data.
    clear lt_dependant.
    move: it_data-pernr to lt_dependant-pernr,
          it_data-perid to lt_dependant-perid.

    do 20 times.
      l_numc = sy-index.
      concatenate 'IT_DATA-DTY' l_numc into l_fname.
      assign (l_fname) to <fs_fcnum>.
      lt_dependant-dtyxx = <fs_fcnum>.

      concatenate 'IT_DATA-DID' l_numc into l_fname.
      assign (l_fname) to <fs_fcnum>.
      lt_dependant-didxx = <fs_fcnum>.

      check not ( lt_dependant-dtyxx is initial
             and lt_dependant-didxx is initial ).
      append lt_dependant.
    enddo.

  endloop.


  sort lt_dependant by pernr.

  if not lt_dependant[] is initial.

    select *
      into table lt_0021
      from pa0021
       for all entries in lt_dependant
     where pernr = lt_dependant-pernr
       and subty = lt_dependant-dtyxx
       and objps = lt_dependant-didxx.
    sort lt_0021 by pernr subty objps seqnr endda descending.
    delete adjacent duplicates from lt_0021 comparing
                                    pernr subty objps seqnr.

    select *
      into table lt_0106
      from pa0106
       for all entries in lt_dependant
     where pernr = lt_dependant-pernr
       and subty = lt_dependant-dtyxx
       and objps = lt_dependant-didxx.
    sort lt_0106 by pernr subty objps seqnr endda descending.
    delete adjacent duplicates from lt_0106 comparing
                                    pernr subty objps seqnr.

  endif.


  loop at lt_0021.

* Effective Date
    clear: l_begda, l_endda.
    read table gt_temp with key pernr = lt_0021-pernr binary search.
    if sy-subrc eq 0.
      loop at gt_temp from sy-tabix where pernr = lt_0021-pernr.

        perform check_dependant_line using gt_temp
                                           lt_0021
                                  changing l_exist.
        check l_exist eq 'X'.
        add 1 to l_index.

        if l_begda is initial and l_endda is initial.
          l_begda = gt_temp-begda.
          l_endda = gt_temp-endda.
        else.
          l_temp_date = gt_temp-endda + 1.
          if l_begda = l_temp_date.
            l_begda = gt_temp-begda.
            l_endda = gt_temp-endda.
          else.
            exit.
          endif.
        endif.
      endloop.
      it_file-edate = l_begda.
    endif.


* New or Update
    if it_file-edate in s_bperd.
      it_file-zflag = 'N'.
    elseif gt_temp-aedtm > p_tdate and gt_temp-endda eq '99991231'.
      it_file-zflag = 'N'.
    else.
      it_file-zflag = 'U'.
    endif.

* Cancel Date
    clear l_cdate.
    read table gt_temp with key pernr = lt_0021-pernr binary search.
    if sy-subrc eq 0.
      loop at gt_temp from sy-tabix where pernr = lt_0021-pernr.

        perform check_dependant_line using gt_temp
                                           lt_0021
                                  changing l_exist.
        check l_exist eq 'X'.
        if gt_temp-endda <> '99991231'.
          l_cdate = gt_temp-endda + 1.
        endif.
        exit.
      endloop.
    endif.

    if not l_cdate is initial.
      it_file-cdate = l_cdate.
    endif.


*** Additional Logic
    clear l_stat2.
    select single stat2
     into l_stat2
     from pa0000
    where pernr = lt_0021-pernr
      and endda = '99991231'.

    if l_stat2 eq '0'.
      it_file-cdate = it_file-cdate - 1.
      if it_file-cdate+4(2) = 12.
        it_file-cdate+0(4) = it_file-cdate+0(4) + 1.
        it_file-cdate+4(4) = '0101'.
      else.
        it_file-cdate+4(2) = it_file-cdate+4(2) + 1.
        it_file-cdate+6(2) = '01'.
      endif.
    endif.

* Sub SSN
    read table lt_dependant with key pernr = lt_0021-pernr binary search.
    if sy-subrc eq 0.
      it_file-sussn = lt_dependant-perid.
    endif.

* Group Number
    it_file-grnum = '48548'.
* Division
    it_file-divsn = '001'.
* First Name
    it_file-vorna = lt_0021-favor.
* Mi
    it_file-midnm = lt_0021-finit.
* Last Name
    it_file-nachn = lt_0021-fanam.

* Dep SSN
    read table lt_0106 with key pernr = lt_0021-pernr
                                subty = lt_0021-subty
                                objps = lt_0021-objps binary search.
    if sy-subrc eq 0.
      it_file-dessn = lt_0106-perid.
    endif.

* DOB
    it_file-gbdat = lt_0021-fgbdt.
* Address
    read table it_file into ls_femp with key sussn = lt_dependant-perid.
    if sy-subrc eq 0.
      it_file-ctype = ls_femp-ctype.
      it_file-empnr = ls_femp-empnr.
    endif.

* Gender
    if lt_0021-fasex = '1'.
      it_file-gendr = 'm'.
    elseif lt_0021-fasex = '2'.
      it_file-gendr = 'f'.
    else.
      clear it_file-gendr.
    endif.

    case lt_0021-famsa.
      when '1' or '13' or '15'.
        it_file-relat = 'S'.
      when '2' or '6' or '14'.
        it_file-relat = 'C'.
    endcase.

    it_file-dentl = 'Y'.

    translate it_file to upper case.
    append it_file. clear it_file.

  endloop.

endform.                    "process_dependant_data
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
  data: l_totrec type i,
        l_filter(50) type c value 'des -e -k abc123forme',
        l_return type zmms0053.

  data: l_message type char50.

* BEGIN OF UD1K955233
  if not p_eai is initial.
    describe table it_file lines l_totrec.
    call function 'ZFHR_BCBS_OUT'
      destination p_dest
      tables
        it_data                        = it_file
      exceptions
        call_function_destination_no_t = 1
        call_function_no_dest          = 2
        call_function_remote_error     = 3
        rfc_no_authority               = 4
        others                         = 5.
    if sy-subrc <> 0.
      message e000 with 'Error when calling RFC destination:'
                         p_dest.
    else.
      concatenate 'Data has been sent to' p_dest
             into l_message separated by space.
      message s000 with l_message l_totrec 'Record(s)' .
    endif.

  else.

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
*       BEGIN OF VALTEST
        open dataset v_file for output in text mode encoding default
             filter l_filter.
*       OPEN DATASET v_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*       END OF VALTEST
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
  endif.                                                    "UD1K955233
endform.                    " TRANSFER_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->ft_fieldcat  Field Catalog Value
*----------------------------------------------------------------------*
form fieldcat_init using ft_fieldcat type slis_t_fieldcat_alv.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_internal_tabname     = 'IT_FILE'
      i_inclname             = sy-repid
    changing
      ct_fieldcat            = ft_fieldcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

* Set Key field.
  loop at ft_fieldcat into gs_fieldcat
                     where fieldname = 'PERNR'
                        or fieldname = 'NACHN'
                        or fieldname = 'VORNA'.
    gs_fieldcat-key = 'X'.
    modify ft_fieldcat from gs_fieldcat transporting key.
  endloop.

* Change Description
  perform change_desc using 'NACHN'  'Last Name'.
  perform change_desc using 'VORNA'  'First Name'.
  perform change_desc using 'STRAS'  'Home Addr1'.
  perform change_desc using 'LOCAT'  'Home Addr2'.
  perform change_desc using 'ORT01'  'City'.
  perform change_desc using 'HOMPH'  'Home Phone'.
  perform change_desc using 'WORPH'  'Work Phone'.
  perform change_desc using 'CELPH'  'Work Cell'.
  perform change_desc using 'EMAIL'  'Work e-Mail'.
  perform change_desc using 'BIRDT'  'Date of Birth'.
  perform change_desc using 'HIRDT'  'Date of Hire'.
  perform change_desc using 'TERDT'  'Date of Termination'.
  perform change_desc using 'WSTAT'  'Work Status'.
  perform change_desc using 'PERNRC' 'PersNo.'.             "UD1K955849

endform.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
form alv_grid_display tables ft_outtab type table.

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
form browser changing filename type c answer type i.
  data  $filename type string.
  data: l_path  type string,
        l_fpath type string,
        l_dfile type string.

  concatenate l_dfile sy-datum sy-uzeit c_dextn into l_dfile.

  call method cl_gui_frontend_services=>file_save_dialog
    exporting
      window_title      = 'Select File Name'
      default_extension = 'txt'
      default_file_name = l_dfile
      file_filter       = 'TXT (*.txt)|*.txt| All (*.*)|*.*'
      initial_directory = c_dpath
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
  if it_file[] is initial.
    message s020.
    stop.
  endif.
  perform transfer_data.

endform.                    " output_data

*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
form display_unix changing filename type c answer type i.
  data: begin of it_filename occurs 0,
          path(1024) type c,
        end of it_filename.

  concatenate c_unix_pre sy-sysid c_unix_suf into it_filename-path.
  append it_filename.

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
    concatenate filename '/' l_dfile sy-datum sy-uzeit c_dextn
           into filename.
  else.
    message s549(fibl).
  endif.
endform.                    " display_unix
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
*      -->P_FIELD   Field Name
*      -->P_DESC    Field Description
*----------------------------------------------------------------------*
form change_desc  using    p_field type c
                           p_desc  type c.

  data: gs_fieldcat type slis_fieldcat_alv.

  read table gt_fieldcat into gs_fieldcat
                         with key fieldname = p_field.
  if sy-subrc = 0.
    gs_fieldcat-seltext_l    = p_desc.
    gs_fieldcat-seltext_m    = p_desc.
    gs_fieldcat-seltext_s    = p_desc.
    gs_fieldcat-reptext_ddic = p_desc.
    modify gt_fieldcat from gs_fieldcat index sy-tabix.
  endif.
endform.                    " CHANGE_DESC
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*       Show progress indicator
*----------------------------------------------------------------------*
*      -->PF_TEXT  Display Text
*      -->PF_VAL   Percentage calculated base
*----------------------------------------------------------------------*
form show_progress2 using   pf_text type c
                            pf_val  type i.

  data:    l_prctx(3) type c,
           l_tex1(50) type c.

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
    l_counter = l_baseval - 1.
  endif.

  if not pf_val is initial.
    if not l_percent is initial.
      l_prctx = l_percent.
    else.
      l_prctx = 1.
      l_percent = 1.
    endif.

    concatenate pf_text l_prctx '%' into l_tex1 separated by space.
  else.
    l_tex1 = pf_text.
  endif.

  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = l_percent
      text       = l_tex1.

  if l_percent = 1.
    clear l_percent.
  endif.
endform.                    " SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*&      Form  INSERT_QUOTE
*&---------------------------------------------------------------------*
*       Insert quote to string
*----------------------------------------------------------------------*
*      <--P_SFIELD  String Field
*----------------------------------------------------------------------*
form insert_quote  changing p_sfield type c.
  check not p_sfield is initial.
  concatenate '"' p_sfield '"' into p_sfield.
endform.                    " INSERT_QUOTE
*&---------------------------------------------------------------------*
*&      Form  set_date_ranges_by_radio
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form set_date_ranges_by_radio.

  clear s_bperd[].
  call function 'PA03_PERIODDATES_GET'
    exporting
      f_abkrs         = '11'
    importing
      f_current_begda = s_bperd-low
      f_current_endda = s_bperd-high.

  if s_bperd-high > sy-datum.
    subtract 14 from s_bperd-low.
    subtract 14 from s_bperd-high.
  endif.

  l_dfile = 'HMMA_CENSUS_FILE_'.
  append s_bperd.

  concatenate c_dpath l_dfile sy-datum sy-uzeit c_dextn into p_file.

endform.                    "set_date_ranges_by_radio
*&---------------------------------------------------------------------*
*&      Form  CHECK_DEPENDANT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_gt_temp  text
*      <--P_L_EXIST  text
*----------------------------------------------------------------------*
form check_dependant_line  using    p_data like it_data
                                    p_0021 structure pa0021
                           changing c_exist.

  data: l_dtyxx type ben_deptyp,
        l_didxx type ben_depid.

  clear c_exist.

  do 20 times varying l_dtyxx from p_data-dty01
                              next p_data-dty02

              varying l_didxx from p_data-did01
                              next p_data-did02.
    if l_dtyxx eq p_0021-subty and
       l_didxx eq p_0021-objps.
      c_exist = 'X'.
      exit.
    endif.
  enddo.

endform.                    " CHECK_DEPENDANT_LINE
