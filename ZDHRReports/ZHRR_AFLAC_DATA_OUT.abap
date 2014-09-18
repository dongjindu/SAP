*&----------------------------------------------------------------------
*& Program ID        : ZHR_AFLAC_DATA_OUT
*& Title             : [HR] - AFLAC Outbound Data Transfer
*& Created by        : Sunho Jeong
*& Created on        : 06/26/2013
*& Specifications By : Grace Li
*& Reference Pgm     : N/A
*& Description       : Extract outbound data for AFLAC
*&                     (*.csv File).
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*&
*&----------------------------------------------------------------------

report  zhrr_aflac_data_out message-id zmfi.


constants:
  c_dlmtd(1)     type c value cl_abap_char_utilities=>horizontal_tab,
  c_dpath        type string value 'C:\TEMP\AFLAC\',
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

data : it_file  like table of zshr_aflac_out  with header line,
       it_file2 like table of zshr_aflac_out2 with header line.

data : begin of it_data occurs 0.
data :  stat2 type stat2,
        vorna type vorna,
        nachn type nachn,
        pernr type p_pernr,
        stras type stras,
        locat type locat,
        ort01 type ort01,
        state type regio,
        pstlz type pstlz,
        gbdat type dats,
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
*        gbdat type pa0002-gbdat,
        werks type pa0001-werks,
        btrtl type pa0001-btrtl,
        persg type pa0001-persg,
        gesch type pa0002-gesch,
        kostl type pa0001-kostl,
        anred type pa0002-anred,
        abkrs type abkrs,
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
* END OF UD1K955233
selection-screen end of block blk2.

selection-screen begin of block blk4 with frame title text-t02.
selection-screen: begin of line.

selection-screen position 1.
parameters p_new radiobutton group typ user-command rad2.
selection-screen comment 3(10) text-001 for field p_new.

selection-screen position 16.
parameters p_chg radiobutton group typ modif id chk.
selection-screen comment 18(16) text-002 for field p_chg.

selection-screen position 36.
parameters p_pay radiobutton group typ modif id chk default 'X'.
selection-screen comment 38(16) text-003 for field p_pay.
selection-screen:   end of line.

selection-screen skip.
select-options: s_bperd for sy-datum modif id bpe.
selection-screen end of block blk4.

initialization.
  p_dest = 'WMHR01'.
  concatenate c_dpath l_dfile sy-datum sy-uzeit c_dextn into p_file.

  concatenate sy-datum+0(6) '01' into s_bperd-low.
  call function 'LAST_DAY_OF_MONTHS'
    exporting
      day_in            = s_bperd-low
    importing
      last_day_of_month = s_bperd-high.
  append s_bperd.


at selection-screen output.
  loop at screen.
    if screen-group1 = 'CHK'.
      if sy-tcode = 'ZHRR00007' and
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

  if sy-batch eq 'X' and not sy-slset is initial.
    perform set_date_ranges_by_radio.
  endif.


at selection-screen.

  if sy-ucomm eq 'RAD2'.
    perform set_date_ranges_by_radio.
  endif.


  if p_file is initial.
    message e000 with 'Please Enter Filename'.
  endif.

* BEGIN OF UD1K955233
  if sy-tcode = 'ZHR_AFLAC_DATA' and
   ( not p_appl is initial or not p_eai is initial ).
    message e000 with text-m01 text-m02.
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

  case p_pay.
    when 'X'.
      perform modify_payment_data.
    when others.
      perform modify_census_data.
  endcase.

  perform output_data.

end-of-selection.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
form select_data.

  data: l_begda type pa0001-begda,
        l_endda type pa0001-endda.


  case 'X'.
    when p_new.

      select a~pernr c~nachn c~vorna d~stras d~locat d~ort01 d~state
             d~pstlz c~perid c~gesch c~gbdat a~stat2 c~famst b~kostl
             b~orgeh b~stell b~persk b~werks b~btrtl b~persg
             d~com01 d~num01 d~com02 d~num02 d~com03 d~num03 d~com04
             d~num04 d~com05 d~num05 d~com06 d~num06 c~anred b~abkrs
        into corresponding fields of table it_data
        from pa0000 as a join pa0001 as b
                           on b~pernr = a~pernr
                         join pa0002 as c
                           on c~pernr = a~pernr
                         left join pa0006 as d
                           on d~pernr = a~pernr
                          and d~endda = '99991231'
                          and d~subty = '5'

       where a~begda in s_bperd
         and a~endda = '99991231'
         and b~endda = '99991231'
         and c~endda = '99991231'
         and a~massn in ('Z0','Z1','Z6','Z9','ZF').

    when p_chg.

      select a~pernr c~nachn c~vorna a~stras a~locat a~ort01 a~state
             a~pstlz c~perid c~gesch c~gbdat d~stat2 c~famst b~kostl
             b~orgeh b~stell b~persk b~werks b~btrtl b~persg
             a~com01 a~num01 a~com02 a~num02 a~com03 a~num03 a~com04
             a~num04 a~com05 a~num05 a~com06 a~num06 c~anred d~stat2
             b~abkrs
        into corresponding fields of table it_data
        from pa0006 as a join pa0001 as b
                           on b~pernr = a~pernr

                         join pa0002 as c
                           on c~pernr = a~pernr

                         join pa0000 as d
                           on a~pernr = d~pernr

       where a~begda in s_bperd
         and a~endda = '99991231'
         and b~endda = '99991231'
         and c~endda = '99991231'
         and d~endda = '99991231'
         and a~anssa = '5'
         and a~subty = '5'.

    when p_pay.

      select t1~pernr t1~stat2 t2~nachn t2~vorna t1~massn
        into corresponding fields of table it_pdata
        from pa0000 as t1 inner join pa0002 as t2
                             on t1~pernr = t2~pernr
       where t1~endda = '99991231'
         and t2~endda = '99991231'.
*         and t1~stat2 in (1,3).
      "and not stell in r_jobkey.

      sort it_pdata by pernr.

  endcase.



* BEGIN OF UD1K955233
* Get phone information from Mailing Address
  if not it_data[] is initial.
    select pernr com01 num01 com02 num02 com03 num03
           com04 num04 com05 num05 com06 num06
      into table it_phone
      from pa0006
      for all entries in it_data
     where pernr = it_data-pernr
       and subty = '5'
       and endda = '99991231'.

    sort it_phone by pernr.

  endif.
* END OF UD1K955233
endform.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  read_payroll_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR    text
*      -->P_P_AMOUNT text
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
  if not s_bperd[] is initial.
    delete in_rgdir where not paydt in s_bperd.
  endif.

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
    where ( lgart = '8480' ).
      p_p_amount = p_p_amount + wa_rt-betrg.
    endloop.

  endloop.

endform.                    "read_payroll_data
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
form modify_census_data .
  data: l_rec     type i,
        l_comxx   type p0006-com01,
        l_numxx   type p0006-num01,
        l_darxx   type ps0041-dar01,
        l_datxx   type ps0041-dat01,
        l_mofid   type t001p-mofid,
        l_mosid   type t001p-mosid,
        l_zeity   type t503-zeity,
        lw_ps0041 type ps0041,
        l_usrid   type pa0105-pernr.                        "UD1K955849

  data: begin of lt_termination occurs 0,
          begda type pa0000-begda,
          massn type pa0000-massn,
        end of lt_termination.

* Move data to output structure
  describe table it_data lines l_rec.
  loop at it_data.
    perform show_progress2 using 'Processing Data...' l_rec.

    move-corresponding it_data to it_file.

    read table it_phone with key pernr = it_data-pernr
                               binary search.
    if sy-subrc = 0.
      do 6 times varying l_comxx from it_phone-com01
                                 next it_phone-com02

                 varying l_numxx from it_phone-num01
                                 next it_phone-num02.
        case l_comxx.
          when 'HOME'.
            it_file-home_phone = l_numxx.
        endcase.
      enddo.
    endif.
* END OF UD1K955233

* Get e-Mail
*    select usrid_long into it_file-email
*      from pa0105
*     up to 1 rows
*     where pernr = it_data-pernr
*       and subty = '0010'
*       and endda = '99991231'.
*    endselect.

* Get Gender
    if it_data-gesch = '1'.
      it_file-gender = 'm'.
    elseif it_data-gesch = '2'.
      it_file-gender = 'f'.
    else.
      clear it_file-gender.
    endif.

    it_file-location = 'Montgomery'.

* Get Cost Center Description
    select single ltext into it_file-ltext
      from cskt
     where spras = 'E'
       and kokrs = 'H201'
       and kostl = it_data-kostl
       and datbi = '99991231'.

* Get Title
    select single atext into it_file-title
      from t522t
     where sprsl = 'E'
       and anred = it_data-anred.

    case it_data-abkrs.
      when '11'.
        it_file-pay_cycle = 'b'.
      when '13'.
        it_file-pay_cycle = 'm'.
      when others.
        clear it_file-pay_cycle.
    endcase.

* Get Date of Birth
    write it_data-gbdat to it_file-gbdat.

* Get Date of Hire
    select * into corresponding fields of lw_ps0041
      from pa0041
     up to 1 rows
     where pernr = it_data-pernr
       and endda = '99991231'.
    endselect.

    if sy-subrc = 0.
      do 12 times varying l_darxx from lw_ps0041-dar01
                                  next lw_ps0041-dar02

                  varying l_datxx from lw_ps0041-dat01
                                  next lw_ps0041-dat02.
        case l_darxx.
          when 'ZC'.
            write l_datxx to it_file-hire_date.
        endcase.
      enddo.
    endif.

    append it_file. clear it_file.
  endloop.

  free it_data.

  describe table it_file lines l_rec.
  message s000 with 'Total record(s) extracted:' l_rec.

endform.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  modify_payment_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form modify_payment_data .
  data: l_rec     type i,
        l_comxx   type p0006-com01,
        l_numxx   type p0006-num01,
        l_darxx   type ps0041-dar01,
        l_datxx   type ps0041-dat01,
        l_mofid   type t001p-mofid,
        l_mosid   type t001p-mosid,
        l_zeity   type t503-zeity,
        lw_ps0041 type ps0041,
        l_usrid   type pa0105-pernr.                        "UD1K955849

  data: begin of lt_termination occurs 0,
          begda type pa0000-begda,
          massn type pa0000-massn,
        end of lt_termination.

* Move data to output structure
  describe table it_pdata lines l_rec.
  loop at it_pdata.
    perform show_progress2 using 'Processing Data...' l_rec.
    move-corresponding it_pdata to it_file2.
    perform read_payroll_data using it_pdata-pernr changing it_file2-betrg.
*    write it_pdata-betrg currency it_file2-betrg.

    case it_pdata-stat2.
      when 0.
        if it_pdata-massn eq 'ZY'.
          it_file2-event = 'D'.
        else.
          it_file2-event = 'T'.
        endif.
      when 1.
        it_file2-event = 'L'.
      when 2.
        it_file2-event = 'R'.
      when 3.
        it_file2-event = 'A'.
    endcase.

    if it_file2-betrg < 0.
      it_file2-betrg = it_file2-betrg * -1.
    endif.

    check not it_file2-betrg is initial.
    append it_file2. clear it_file2.
  endloop.

  free it_pdata.

  describe table it_file2 lines l_rec.
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
  data: l_totrec type i,
        l_filter(50) type c value 'des -e -k abc123forme',
        l_return type zmms0053.

  data: l_message type char50.

* BEGIN OF UD1K955233
  if not p_eai is initial.

    case p_pay.
      when 'X'.
        describe table it_file2 lines l_totrec.
        call function 'ZFHR_AFLAC_OUT'
          destination p_dest
          tables
            it_data2                       = it_file2
          exceptions
            call_function_destination_no_t = 1
            call_function_no_dest          = 2
            call_function_remote_error     = 3
            rfc_no_authority               = 4
            others                         = 5.

      when others.
        if p_new eq 'X'.
          describe table it_file lines l_totrec.
          call function 'ZFHR_AFLAC_OUT'
            destination p_dest
            tables
              it_data1a                      = it_file
            exceptions
              call_function_destination_no_t = 1
              call_function_no_dest          = 2
              call_function_remote_error     = 3
              rfc_no_authority               = 4
              others                         = 5.
        else.
          describe table it_file lines l_totrec.
          call function 'ZFHR_AFLAC_OUT'
            destination p_dest
            tables
              it_data1b                      = it_file
            exceptions
              call_function_destination_no_t = 1
              call_function_no_dest          = 2
              call_function_remote_error     = 3
              rfc_no_authority               = 4
              others                         = 5.
        endif.
    endcase.

    if sy-subrc <> 0.
      message e000 with 'Error when calling RFC destination:'
                         p_dest.
    else.
      concatenate 'Data has been sent to' p_dest
             into l_message separated by space.
      message s000 with l_message l_totrec 'Record(s)' .
    endif.

  else.

    case p_pay.
      when 'X'.
        loop at it_file2.
          do.
            assign component sy-index of structure it_file2 to <fs>.
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

      when others.
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
    endcase.




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

  case 'X'.
    when p_pay.
      if it_file2[] is initial.
        message s020.
        stop.
      endif.
    when others.
      if it_file[] is initial.
        message s020.
        stop.
      endif.
  endcase.

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

  case 'X'.
    when p_new.
      l_dfile = 'HMMA_CENSUS_NEWHIRE_'.
    when p_chg.
      l_dfile = 'HMMA_CENSUS_ADDRESS_'.
    when p_pay.
      concatenate sy-datum+0(6) '01' into s_bperd-low.
      call function 'LAST_DAY_OF_MONTHS'
        exporting
          day_in            = s_bperd-low
        importing
          last_day_of_month = s_bperd-high.

      l_dfile = 'HMMA_MONTHLY_PAYMENT_'.
  endcase.
  append s_bperd.

  concatenate c_dpath l_dfile sy-datum sy-uzeit c_dextn into p_file.

endform.                    "set_date_ranges_by_radio
