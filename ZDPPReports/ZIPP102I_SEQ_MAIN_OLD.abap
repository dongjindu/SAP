************************************************************************
* Program Name      : ZIPP102I_SEQ_MAIN
* Author            : Bobby
* Creation Date     : 2004.02.08.
* Specifications By : Bobby
* Pattern           : 5.2.2
* Development Request No :
* Addl Documentation:
* Description       : Vehicle Order(Planned Order) Creation
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
report  zipp102i_seq_main   message-id zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
tables: ztpp_common_vals,
        equi ,
        ausp .

************* DO NOT USE!!!!! *****************************************
data: wa_filename             like  rlgrap-filename,
      wa_filetype             like  rlgrap-filetype value 'DAT',
      wa_bdcgroup             like  sy-uname,
      p_tcode                 like  tstc-tcode                ,
      p_cmode                 type  c                         ,
      it_rec                  like table of mara       with header line.
********************************************************************

*----------------------------------------------------------------------*
* WORKING-AREA VARIABLES DECLARATION
*----------------------------------------------------------------------*
data: wa_material             like mara-matnr                 ,
      wa_number               like ztpp_pp_log_head-logkey    ,
      wa_7jb                  like ztpp_pmt07jb_b             ,
      wa_7jb_log              like ztpp_pmt07jb_b             ,
      wa_maxday               like sy-datum                   ,
      wa_minday               like sy-datum                   ,
      wa_vin                  like mara-matnr                 ,
      wa_lines                type i                          ,
      wa_msg(70)              type c                          ,
      wa_mng                  type i                          ,
      wa_error                type c                          ,
      wa_flag                 type c                          ,
      wa_date                 type d                          ,
      wa_err_hd               type c                          ,
      wa_mode                 type c   value   'N'            ,
      sv_log_color            like mara-matnr                 ,
      jobc                    like tbtcjob-jobcount           ,
      jobn                    like  tbtcjob-jobname           ,
      immediate               like btch0000-char1  value  'X' ,
      wa_count(4)             type n                          ,
      c_prog                  like sy-repid                   .

ranges: s_jobnam for tbtcp-jobname,
        s_pronam for tbtcp-progname,
        s_date for tbtcp-sdldate,
        s_time for tbtcp-sdltime.

*----------------------------------------------------------------------*
* Field-Symbols VARIABLES DECLARATION
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNAL TABLES DECLARATION
*----------------------------------------------------------------------*
data: begin of it_vin         occurs 0                        .
        include structure     ztpp_pmt07jb_b .
data:   vin                   like mara-matnr,
      end of it_vin                          ,
      it_7jb              like table of ztpp_pmt07jb_b with header line,
      it_msg              like table of bdcmsgcoll     with header line,
      it_bdcdata          like table of bdcdata        with header line,
      it_vmaster          like table of zspp_vin_value with header line.

data: global_job          like table of tbtcjob        with header line,
      it_joblist          like table of tbtcjob        with header line,
      global_start_date   like table of tbtcstrt       with header line,
      global_step_tbl     like table of tbtcstep       with header line.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
selection-screen begin of block b1 with frame.
selection-screen skip 1.
selection-screen begin of line.
parameters   p_run      radiobutton group ra  default 'X'.
selection-screen comment  (55) text-102 for field p_run  .
selection-screen end of line.
selection-screen begin of line.
parameters   p_rerun    radiobutton group ra.
selection-screen comment  (55) text-101 for field p_rerun.
selection-screen end of line.
selection-screen end of block b1.

*----------------------------------------------------------------------
top-of-page.
*----------------------------------------------------------------------
  if wa_err_hd = 'E'     .
    write at: /001(50)  text-001.
    skip 1 .
    uline at: /(50)             .
    write at: /001(09)  text-201,     " l_ordr ,
               011(05)  text-202,     " l_dist ,
               017(03)  text-203,     " l_extc ,
               021(03)  text-204.     " l_intc .
    uline at: /(50)             .
  endif.

*----------------------------------------------------------------------
initialization.
*----------------------------------------------------------------------
  get time.
  wa_date = sy-datum.

*----------------------------------------------------------------------
start-of-selection.
*----------------------------------------------------------------------
  if p_run = 'X'.
    " Normal Processing...
    perform get_data.
    check wa_flag is initial.
    perform check_data .
    check wa_flag is initial.
    perform bdc_wocl_summary .           " Step 1: Work Order Color
    if wa_error = 'X'        .
      rollback work          .
      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      perform create_log using '1' wa_7jb_log. " Log Create in STEP 1 .
      exit .
    else.
      commit work            .
    endif.

    perform bdc_wohd_summary .
    if wa_error = 'X'        .
      rollback work          .
      " WA_7JB_LOG is Null Values..   ( For the Structure Parameter..)
      perform create_log using '2' wa_7jb_log. " Log Create in STEP 2 .
      exit .
    else.
      commit work            .
    endif.

    perform clear_vinn       .
    perform record_processing.           " Log Create in Step 3 & 4 .
  else.
    " Reprocessing..
  endif.

  check wa_error = space   .
  perform check_result     .
  perform write_result     .
  perform write_timestamp  using  text-011 .

  include zcpp103_common_routine .

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  data: com_date             type d,
        chk_date             type d.

  select * into corresponding fields of table it_7jb
    from ztpp_pmt07jb_b .

  describe table it_7jb lines wa_lines .
  read table it_7jb index 1.
  chk_date = it_7jb-sqdt   .

  " Data Check for Re-Running..
  c_prog = 'ZIPP101U_PMT07JB_A'.
  select single *
    from ztpp_common_vals
   where jobs  = c_prog  .

  com_date = ztpp_common_vals-item1.   " Previous Max Sequenced Date..
  if chk_date <= com_date .
    wa_flag  =  'X'      .            " Already Sequenced Data..
  endif.
endform.                    " GET_DATA

*&---------------------------------------------------------------------*
*&      Form  RECORD_PROCESSING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form record_processing.
  " Check the process about the followings:
  " 1. Sequence date is diffrent. and same record is exist each days.
  " 2. early day's record is sequencing. and late day's record is MITU
  "    the record's updating time is delay. data will be a unmatch!!

  sort it_7jb by sqdt ssr1.
  loop at it_7jb where mtgu ne 'M'.
    " Error Log Step 3 : VIN Code Generation Error...
    perform check_vin         .
    perform vin_code_gen      .
    if wa_error = 'X'.  exit.  endif.
    modify it_7jb.
  endloop.

  if wa_error = 'X'.  exit.  endif.

* COMMIT WORK AND WAIT.
  call function 'BAPI_TRANSACTION_COMMIT'
       exporting
           wait    = 'X' .
* WAIT UP TO 3  SECONDS.

  loop at it_7jb .
    wa_7jb = it_7jb .
    " Error Log Step 4 : Call the Job-Processor.....
    case it_7jb-mtgu .
      when 'M'      .
        perform process_mitu_value .
      when others.
        perform select_vin        .
        c_prog = 'ZIPP102I_SEQ_JOB1'.
        perform job_create  using  '_CREATE_VM'  .
*       WAIT UP TO 1  SECONDS.
    endcase.
    modify it_7jb.
  endloop.

  commit work.
  do.
    " Check the Background-Job is finished!!!
    clear it_joblist.
    refresh it_joblist.
    call function 'BP_FIND_JOBS_WITH_PROGRAM'
         exporting
              abap_program_name             = c_prog  "'YIPP_TEST16'
              dialog                        = 'N'
         tables
              joblist                       = it_joblist
         exceptions
              no_jobs_found                 = 1
              program_specification_missing = 2
              invalid_dialog_type           = 3
              job_find_canceled             = 4
              others                        = 5.

    if sy-subrc <> 0.
      exit.
    endif.
    read table it_joblist with key jobname(3) = sy-mandt
                                   status = 'S'.
    if sy-subrc = 0.
      continue.
    else.
      read table it_joblist with key jobname(3) = sy-mandt
                                     status = 'R'.
      if sy-subrc = 0.
        continue.
      else.
        exit.
      endif.
    endif.
  enddo.

  loop at it_7jb where mtgu ne 'M' .
    " Error Log Step 5 : Call the Job-Processor.....
    perform select_vin        .
    c_prog = 'ZIPP102I_SEQ_JOB2'.
    perform job_create using '_PLANORDER'.
  endloop.

  do.
    " Check the Background-Job is finished!!!
    clear it_joblist.
    refresh it_joblist.
    call function 'BP_FIND_JOBS_WITH_PROGRAM'
         exporting
              abap_program_name             = c_prog  "'YIPP_TEST16'
              dialog                        = 'N'
         tables
              joblist                       = it_joblist
         exceptions
              no_jobs_found                 = 1
              program_specification_missing = 2
              invalid_dialog_type           = 3
              job_find_canceled             = 4
              others                        = 5.

    if sy-subrc <> 0.
      exit.
    endif.
    read table it_joblist with key jobname(3) = sy-mandt
                                   status = 'S'.
    if sy-subrc = 0.
      continue.
    else.
      read table it_joblist with key jobname(3) = sy-mandt
                                     status = 'R'.
      if sy-subrc = 0.
        continue.
      else.
        exit.
      endif.
    endif.
  enddo.

  perform update_commonvlas    .
endform.                    " RECORD_PROCESSING

*&---------------------------------------------------------------------*
*&      Form  process_mitu_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form process_mitu_value.
  data: l_equnr              like equi-equnr ,
        l_atinn              like ausp-atinn ,
        l_porder             like plaf-plnum ,
        l_vin                like mara-matnr ,
        l_mituqty            like ztpp_wosum-mituqty.

  clear: wa_material.
  concatenate it_7jb-ordr it_7jb-dist into wa_material .

  " Work Order Summary Table Update.....
  select single mituqty into l_mituqty
    from ztpp_wosum
   where wo_ser = it_7jb-ordr
     and nation = it_7jb-dist(3)
     and dealer = it_7jb-dist+3(2)
     and extc   = it_7jb-extc
     and intc   = it_7jb-intc     .

  l_mituqty = l_mituqty - it_7jb-pqty  .
  update ztpp_wosum   set mituqty = l_mituqty
                    where wo_ser = it_7jb-ordr
                      and nation = it_7jb-dist(3)
                      and dealer = it_7jb-dist+3(2)
                      and extc   = it_7jb-extc
                      and intc   = it_7jb-intc     .

  " Vehicla Master Update..
  concatenate  it_7jb-bmdl(3)  it_7jb-vhno  into  l_equnr .
  clear: it_vmaster, it_vmaster[] .
  it_vmaster-atnam = 'P_SEQUENCE_DATE' .
  it_vmaster-atwrt = it_7jb-sqdt       .
  append it_vmaster.
  it_vmaster-atnam = 'P_MITU'          .
  it_vmaster-atwrt = ' '               .
  append it_vmaster.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object     = l_equnr
            mode       = 'W'
       tables
            val_table  = it_vmaster
       exceptions
            no_data    = 1
            error_mode = 2
            others     = 3.

  " Plan Order Number Search and Saving...
  select single atinn into l_atinn
    from cabn
   where atnam = 'P_PLAN_ORDER'.

  select single atwrt into l_porder
    from ausp
   where objek  = l_equnr
     and atinn  = l_atinn .

  select single atinn into l_atinn
    from cabn
   where atnam = 'P_VIN'       .

  select single atwrt into l_vin
    from ausp
   where objek  = l_equnr
     and atinn  = l_atinn .

  if sy-subrc = 0.
    update ztpp_pmt07jb_b    set: plnum = l_porder
                                  vinn  = l_vin
                                  aedat = sy-datum
                                  aezet = sy-uzeit
                                  aenam = sy-uname
                           where sqdt  = it_7jb-sqdt
                             and plnt  = it_7jb-plnt
                             and line  = it_7jb-line
                             and modl  = it_7jb-modl
                             and mtgu  = it_7jb-mtgu
                             and ssr1  = it_7jb-ssr1
                             and ssr2  = it_7jb-ssr2  .
  endif.
endform.                    " process_mitu_value

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_WORKORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATERIAL  text
*      -->P_L_PLNMG  text
*----------------------------------------------------------------------*
form call_bdc_workorder using    pa_material  pa_plnmg  pa_char  pa_z .
  data: l_vars                like table of zspp_vin_value
                                                  with header line,
        l_dec                 type i,
        l_dec1(6)             type c,
        l_dec2(6)             type c.

  l_dec = pa_plnmg.
  write l_dec    to l_dec1.

  l_vars-atnam = pa_char .  l_vars-atwrt = l_dec1.  append l_vars.
  if pa_z = 'Z'.
    l_dec = wa_mng  .
    write l_dec   to l_dec2.
    l_vars-atnam = 'P_MITU_QTY' .  l_vars-atwrt =  l_dec2.
    append l_vars.
  endif.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object     = pa_material
            mode       = 'W'
            ctype      = '001'
       tables
            val_table  = l_vars
       exceptions
            no_data    = 1
            error_mode = 2
            others     = 3.

  if sy-subrc ne 0.
    wa_error = 'X'.
  endif.
endform.                    " CALL_BDC_WORKORDER

*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_SALES_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_bdc_sales_order   using pa_sorder pa_val2  pa_val10  pa_val20.
  data: l_val10(6)          type n           ,
        l_val20(6)          type n           ,
        l_flag              type c           ,   " Roll-Back Flag..
        l_kwmeng            like vbap-kwmeng .

  " Change the Sales Order
  l_val10 =  pa_val10 .
  l_val20 =  pa_val20 .
  l_val20 = l_val20 - l_val10.

  " Change the Coding for the Re-Processing (BDC --> BAPI)
  perform call_bapi_salesorder using pa_sorder l_val10 l_val20 .
*
*  PERFORM bdc_dynpro_processing USING :
*                         'X'  'SAPMV45A'             '0102',
*                         ' '  'BDC_OKCODE'           '=UER2' ,
*                         ' '  'VBAK-VBELN'            pa_sorder,
*
*                         'X'  'SAPMV45A'             '4001',
*                         ' '  'BDC_OKCODE'           '=SICH' ,
*                         ' '  'RV45A-KWMENG(01)'      l_val10,
*                         ' '  'RV45A-KWMENG(02)'      l_val20.
*
*  CALL TRANSACTION 'VA02'  USING it_bdcdata MODE wa_mode
*                           MESSAGES INTO    it_msg    .
*
*  LOOP AT it_msg WHERE msgid = 'E' .
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*         EXPORTING
*              msgid               = it_msg-msgid
*              msgnr               = it_msg-msgnr
*              msgv1               = it_msg-msgv1
*              msgv2               = it_msg-msgv2
*              msgv3               = it_msg-msgv3
*              msgv4               = it_msg-msgv4
*         IMPORTING
*              message_text_output = wa_msg.
*    l_flag = 'X'.
*    WRITE: /'Sales Order -- ' , wa_msg .
*  ENDLOOP.
*
*  IF l_flag = 'X'.
*    ROLLBACK WORK.
*    " Log-Creation for the Re-Working...
*  ELSE.
*    " Saving the Success Material for the Re-Working Point....
*    sv_log_color = wa_material.
*    COMMIT WORK.
*  ENDIF.
*  CLEAR: it_bdcdata, it_bdcdata[], it_msg, it_msg[].
endform.                    " CALL_BDC_SALES_ORDER

*&---------------------------------------------------------------------*
*&      Form  BDC_WOHD_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bdc_wohd_summary.
  " Sum the MITU Qty and SEQ Qty ...  (Each Others..)
  data: l_plnmg(4)           type n          ,
        l_ordr               like it_7jb-ordr,
        l_dist               like it_7jb-dist,
        l_seqqty             like ztpp_wosum-seqqty ,
        l_mituqty            like ztpp_wosum-mituqty,
        l_data               like table of zspp_vin_value
                                                       with header line.

  sort it_7jb by ordr dist extc intc ssr1.
  clear: l_seqqty, l_mituqty.
  read table it_7jb index 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  move-corresponding it_7jb  to   it_vin     .

  loop at it_7jb .
    if it_7jb-ordr = l_ordr  and  it_7jb-dist = l_dist .
      if it_7jb-mtgu = space .
        l_seqqty    = l_seqqty + 1.
      else.
        l_mituqty   = l_mituqty + 1 .
      endif.
    else.
      " Process.
      clear: wa_material,  l_data, l_data[], l_plnmg, wa_mng, ausp.
      concatenate l_ordr      l_dist      into wa_material .

      " Work Order Header's SEQ  Qty Change......
      l_data-atnam = 'P_SEQ_QTY'.         append l_data.
      l_data-atnam = 'P_MITU_QTY'.        append l_data.
      l_data-atnam = 'P_VIN_SPEC'.        append l_data.
      call function 'Z_FPP_HANDLING_MASTER'
           exporting
                object       = wa_material
                ctype        = '001'
           tables
                val_table    = l_data
           exceptions
                no_data      = 1
                error_mode   = 2
                error_object = 3
                others       = 4.

      read table l_data index 1.
      l_plnmg = l_data-atwrt   .         clear: l_data.
      l_plnmg = l_plnmg + l_seqqty.
      read table l_data index 2.
      wa_mng  = l_data-atwrt   .         clear: l_data.
      wa_mng  = wa_mng  - l_mituqty.
      read table l_data index 3.
      it_vin-vin = l_data-atwrt.         clear: l_data.

*     PERFORM call_bdc_workorder
*                         USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

      if wa_error = 'X'.  exit.  endif.
      append it_vin .     clear: it_vin.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      it_vin = it_7jb     .
      if it_7jb-mtgu = space .
        l_seqqty    = 1.
      else.
        l_mituqty   = 1 .
      endif.
    endif.
  endloop.

  if wa_error = 'X'.  exit.  endif.

  check wa_error = space.

  if wa_lines > 0 .
    clear: wa_material,  l_data, l_data[], l_plnmg, wa_mng, ausp.
    concatenate l_ordr      l_dist      into wa_material .
    " Work Order Header's SEQ  Qty Change......
    l_data-atnam = 'P_SEQ_QTY'.         append l_data.
    l_data-atnam = 'P_MITU_QTY'.        append l_data.
    l_data-atnam = 'P_VIN_SPEC'.        append l_data.

    " Work Order Header's SEQ  Qty Change......
    call function 'Z_FPP_HANDLING_MASTER'
         exporting
              object       = wa_material
              ctype        = '001'
         tables
              val_table    = l_data
         exceptions
              no_data      = 1
              error_mode   = 2
              error_object = 3
              others       = 4.


    read table l_data index 1.
    l_plnmg = l_data-atwrt   .         clear: l_data.
    l_plnmg = l_plnmg + l_seqqty.
    read table l_data index 2.
    wa_mng  = l_data-atwrt   .         clear: l_data.
    wa_mng  = wa_mng  - l_mituqty.
    read table l_data index 3.
    it_vin-vin = l_data-atwrt.         clear: l_data.

*   PERFORM call_bdc_workorder
*                       USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.
    if wa_error = 'X'.  exit.  endif.
    append it_vin .
  endif.
endform.                    " BDC_WOHD_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  bdc_wocl_summary
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bdc_wocl_summary.
  data: l_plnmg              like ztpp_wosum-seqqty ,
        l_modqty             like ztpp_wosum-modqty ,
        l_salesorder         like ztpp_wosum-sales,
        l_ordr               like it_7jb-ordr,
        l_dist               like it_7jb-dist,
        l_extc               like it_7jb-extc,
        l_intc               like it_7jb-intc,
        l_seq                like ztpp_wosum-seqqty ,
        l_seqqty             like ztpp_wosum-seqqty ,
        l_tseq               like ztpp_wosum-seqqty ,
        l_mituqty            like ztpp_wosum-mituqty,
        l_data               like table of zspp_vin_value
                                                       with header line.

  data: l_count              type i.

  perform write_timestamp  using  text-010 .

  sort it_7jb by ordr dist extc intc ssr1.
  clear: l_seqqty, l_mituqty, l_seq .
  read table it_7jb index 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  l_extc = it_7jb-extc.  l_intc = it_7jb-intc.

  loop at it_7jb .
    if it_7jb-ordr = l_ordr  and  it_7jb-dist = l_dist and
       it_7jb-extc = l_extc  and  it_7jb-intc = l_intc .
      if it_7jb-mtgu = space .
        l_seq       = l_seq    + 1.
      else.
        l_mituqty   = l_mituqty + 1 .
      endif.
    else.
      " Work Order Summary Table Update.....
      select single modqty seqqty sales
               into (l_modqty, l_tseq, l_salesorder)
        from ztpp_wosum
       where wo_ser = l_ordr
         and nation = l_dist(3)
         and dealer = l_dist+3(2)
         and extc   = l_extc
         and intc   = l_intc     .

      l_seqqty  = l_seq     + l_tseq  .      " Total SEQ-Qty..
*      UPDATE ztpp_wosum   SET seqqty  = l_seqqty
*                        WHERE wo_ser = l_ordr
*                          AND nation = l_dist(3)
*                          AND dealer = l_dist+3(2)
*                          AND extc   = l_extc
*                          AND intc   = l_intc     .

      " Process.
      if sy-subrc ne 0.
        wa_error = 'X'.
        exit.
      endif.
      clear: wa_material,  l_data, l_data[].
      concatenate l_ordr       l_dist         into wa_material .
      concatenate wa_material  l_extc  l_intc into wa_material.

      clear: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
      l_data-atnam = 'P_SEQ_QTY'.     append l_data.
      l_data-atnam = 'P_MITU_QTY'.    append l_data.
      call function 'Z_FPP_HANDLING_MASTER'
           exporting
                object       = wa_material
                ctype        = '001'
           tables
                val_table    = l_data
           exceptions
                no_data      = 1
                error_mode   = 2
                error_object = 3
                others       = 4.

      read table l_data index 1.
      l_plnmg = l_data-atwrt   .
      l_plnmg = l_plnmg + l_seq.        clear: l_data.
      " MITU Qauntity is MUNIS.. => MITU Sequence reduced the MITU-Qty..
      read table l_data index 2.
      wa_mng  = l_data-atwrt   .
      wa_mng  = wa_mng  - l_mituqty.    clear: l_data.

*     PERFORM call_bdc_workorder
*                          USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.
      if wa_error = 'X'.   exit.  endif.

      " Sales Order Master Change...
*     PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
*                                        l_seqqty     l_modqty  .
      if wa_error = 'X'.   exit.  endif.

      clear: l_seq, l_mituqty.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      l_extc = it_7jb-extc.  l_intc = it_7jb-intc.
      if it_7jb-mtgu = space .
        l_seq       = 1.
      else.
        l_mituqty   = 1 .
      endif.
    endif.
  endloop.

  if wa_error = 'X'.   exit.  endif.

  if wa_lines > 0 .
    " Work Order Summary Table Update.....
    select single modqty seqqty sales
            into (l_modqty, l_tseq, l_salesorder)
      from ztpp_wosum
     where wo_ser = it_7jb-ordr
       and nation = it_7jb-dist(3)
       and dealer = it_7jb-dist+3(2)
       and extc   = it_7jb-extc
       and intc   = it_7jb-intc     .

    l_seqqty  = l_seq     + l_tseq  .
*    UPDATE ztpp_wosum   SET seqqty  = l_seqqty
*                      WHERE wo_ser = it_7jb-ordr
*                        AND nation = it_7jb-dist(3)
*                        AND dealer = it_7jb-dist+3(2)
*                        AND extc   = it_7jb-extc
*                        AND intc   = it_7jb-intc     .

    if sy-subrc ne 0.
      " Step 1: Update Fail - Reason: Data not found!!!
      "         Check the Working data.
      wa_error = 'X'.
      exit.
    endif.

    clear: wa_material,  l_data, l_data[].
    concatenate l_ordr       l_dist         into wa_material .
    concatenate wa_material  l_extc  l_intc into wa_material.

    clear: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
    l_data-atnam = 'P_SEQ_QTY'.     append l_data.
    l_data-atnam = 'P_MITU_QTY'.    append l_data.
    call function 'Z_FPP_HANDLING_MASTER'
         exporting
              object       = wa_material
              ctype        = '001'
         tables
              val_table    = l_data
         exceptions
              no_data      = 1
              error_mode   = 2
              error_object = 3
              others       = 4.

    read table l_data index 1.
    l_plnmg = l_data-atwrt   .
    l_plnmg = l_plnmg + l_seq.        clear: l_data.
    read table l_data index 2.
    wa_mng  = l_data-atwrt   .
    wa_mng  = wa_mng  - l_mituqty.    clear: l_data.

*   PERFORM call_bdc_workorder
*                        USING wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

    if wa_error = 'X'.   exit.  endif.

    " Sales Order Master Change...
*   PERFORM call_bdc_sales_order USING l_salesorder l_mituqty
*                                      l_seqqty     l_modqty  .
  endif.
endform.                    " bdc_wocl_summary

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_data.
  data: l_plnmg(4)           type n          ,
        l_salesorder         like ztpp_wosum-sales,
        l_ordr               like it_7jb-ordr,
        l_dist               like it_7jb-dist,
        l_extc               like it_7jb-extc,
        l_intc               like it_7jb-intc,
        l_seqqty             like ztpp_wosum-seqqty ,
        l_modqty             like ztpp_wosum-modqty ,
        l_chkqty             type i                 ,
        l_count              type i                 ,
        l_data               like table of conf_out    with header line.

  sort it_7jb by ordr dist extc intc ssr1.
  clear: l_seqqty.
  read table it_7jb index 1.
  l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
  l_extc = it_7jb-extc.  l_intc = it_7jb-intc.

  loop at it_7jb .
    if it_7jb-ordr = l_ordr  and  it_7jb-dist = l_dist and
       it_7jb-extc = l_extc  and  it_7jb-intc = l_intc .
      if it_7jb-mtgu = space .
        l_count      = l_count  + 1.
      endif.
    else.
      " Processing Data Check with ZTPP_WOSUM Table...
      select single modqty seqqty into (l_modqty, l_seqqty)
        from ztpp_wosum
       where wo_ser = l_ordr
         and nation = l_dist(3)
         and dealer = l_dist+3(2)
         and extc   = l_extc
         and intc   = l_intc     .

      l_chkqty  = l_modqty - l_seqqty .
      if l_chkqty < l_count .
        wa_err_hd = wa_flag  = 'E'     .
        write at: /001(09)  l_ordr ,
                   011(05)  l_dist ,
                   017(03)  l_extc ,
                   021(03)  l_intc .
        uline at: /(60)            .
      endif.
      if it_7jb-mtgu = space .
        l_count = 1 .
      else.
        clear: l_count.
      endif.
      l_ordr = it_7jb-ordr.  l_dist = it_7jb-dist.
      l_extc = it_7jb-extc.  l_intc = it_7jb-intc.
    endif.
  endloop.
endform.                    " CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  update_commonvlas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_commonvlas.
  c_prog = 'ZIPP101U_PMT07JB_A'.
  select max( sqdt ) min( sqdt ) into (wa_maxday, wa_minday)
    from ztpp_pmt07jb_b .
  update ztpp_common_vals set: dates = wa_minday
                               item1 = wa_maxday
                        where jobs  = c_prog   .
endform.                    " update_commonvlas

*&---------------------------------------------------------------------*
*&      Form  job_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form job_create   using pa_type.
  refresh: global_job, global_step_tbl.
  clear:   global_job, global_step_tbl.

  wa_count = wa_count + 1.
  concatenate sy-mandt  pa_type '_' wa_count into jobn.

  global_job-jobname = jobn.             "S_PROG.
  global_job-jobclass = 'A'.             "
  global_job-newflag = 'O'.
  global_step_tbl-program = 'RSBTCPT3'.
*  global_step_tbl-program = c_prog.      "dummy step
  global_step_tbl-typ = 'A'.             "
  global_step_tbl-status = 'P'.          "scheduled
  global_step_tbl-authcknam = sy-uname.
  append global_step_tbl.
  append global_job.

  call function 'BP_JOB_CREATE'
       exporting
            job_cr_dialog       = 'N'
            job_cr_head_inp     = global_job
       importing
            job_cr_head_out     = global_job
       tables
            job_cr_steplist     = global_step_tbl
       exceptions
            cant_create_job     = 1
            invalid_dialog_type = 2
            invalid_job_data    = 3
            job_create_canceled = 4
            others              = 5.

  jobc = global_job-jobcount.
  jobn = global_job-jobname.
  wa_number = wa_count.
  wa_date   = sy-datum.

  submit (c_prog)     and return
         with p_7jb    eq wa_7jb
         with p_log    eq wa_number
         with p_date   eq wa_date
         via job jobn number jobc .

* CONCATENATE 'IEQ' jobn     INTO s_jobnam.  APPEND s_jobnam.
  call function 'JOB_CLOSE'
       exporting
            jobcount  = jobc
            jobname   = jobn
            strtimmed = immediate
       exceptions
            others    = 4.
endform.                    " job_create

*&---------------------------------------------------------------------*
*&      Form  check_vin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_vin.
  clear: it_vin.
  wa_7jb = it_7jb.
  read table it_vin with key ordr = wa_7jb-ordr
                             dist = wa_7jb-dist .
endform.                    " check_vin

*&---------------------------------------------------------------------*
*&      Form  WRITE_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_011  text
*----------------------------------------------------------------------*
form write_timestamp using    pa_text.
  get time.
  write at: /001(030)  pa_text,
             031(015)  sy-datum,
             047(015)  sy-uzeit.
endform.                    " WRITE_TIMESTAMP

*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_result.
  data: l_lines             type i.

  describe table it_7jb     lines l_lines.
  write at:/001(020) text-015 ,
            022(010) l_lines  .
endform.                    " WRITE_RESULT

*&---------------------------------------------------------------------*
*&      Form  VIN_CODE_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form vin_code_gen.
  data: l_vin                like mara-matnr ,
        l_mode               like ztpp_common_vals-key2.

  " Call the function for the Last Data...
  l_mode = 'EMF'.      " wa_7jb-modl .
  l_vin = it_vin-vin.

  call function 'Z_FPP_VIN_GENERATION'
       exporting
            w_order  = l_vin
            mode     = l_mode
       importing
            p_lastid = l_vin.

  it_7jb-vinn = wa_7jb-vinn = l_vin     .

  " Update the ZTPP_PMT07JB_B..
  update ztpp_pmt07jb_b   set  vinn  = l_vin
                        where sqdt  = wa_7jb-sqdt
                          and modl  = wa_7jb-modl
                          and mtgu  = wa_7jb-mtgu
                          and ssr1  = wa_7jb-ssr1 .

  if sy-subrc ne 0  or l_vin = space .
    "Error Data
    wa_error = 'X' .
    perform create_log using '3' it_7jb.   " Log Create in STEP 3    .
  endif.
endform.                    " VIN_CODE_GEN

*&---------------------------------------------------------------------*
*&      Form  select_vin
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_vin.
  select single * into wa_7jb
    from ztpp_pmt07jb_b
   where sqdt  = it_7jb-sqdt
     and modl  = it_7jb-modl
     and mtgu  = it_7jb-mtgu
     and ssr1  = it_7jb-ssr1 .
endform.                    " select_vin

*&---------------------------------------------------------------------*
*&      Form  clear_vinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form clear_vinn.
  data: l_porder            like ztpp_pmt07jb_b-plnum,
        l_vinn              like ztpp_pmt07jb_b-vinn.

  clear: l_vinn, l_porder.
  update ztpp_pmt07jb_b   set vinn  = l_vinn
                        where vinn ne l_vinn .
  commit work.

  update ztpp_pmt07jb_b   set plnum = l_porder
                        where plnum ne l_porder.
  commit work.
endform.                    " clear_vinn

*&---------------------------------------------------------------------*
*&      Form  check_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_result.
  " Check the result of the Plan Order & Vehicle Master.
endform.                    " check_result

*&---------------------------------------------------------------------*
*&      Form  call_bapi_salesorder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_SORDER  text
*      -->P_L_VAL10  text
*      -->P_L_VAL20  text
*----------------------------------------------------------------------*
form call_bapi_salesorder using    pa_order  pa_val10  pa_val20.
  data : l_it_ord_header_inx like table of bapisdh1x  with header line,
         l_it_return         like table of bapiret2   with header line,
         l_it_itm            like table of bapisditm  with header line,
         l_it_itmx           like table of bapisditmx with header line,
         l_it_lines          like table of bapischdl  with header line,
         l_it_linesx         like table of bapischdlx with header line.

  data : p_item10_org         like vbap-kwmeng,
         p_item20_org         like vbap-kwmeng,
         p_item20_qty         like ztpp_wosum-modqty,
         p_item10_qty         like ztpp_wosum-seqqty,
         l_item10_flg(01),
         l_item20_flg(01),
         l_item10_qty_flg(01),
         l_item20_qty_flg(01).

  p_item10_qty = pa_val10 .
  p_item20_qty = pa_val20 .

  select single kwmeng into p_item10_org
    from vbap
   where vbeln = pa_order
     and posnr = '000010' .

  select single kwmeng into p_item20_org
    from vbap
   where vbeln = pa_order
     and posnr = '000020' .

  if p_item10_qty = 0 and p_item10_org > 0.
    l_item10_flg = 'U'.             " l_item10_flg = 'U'.
    l_item10_qty_flg = 'D'.
  elseif p_item10_qty > 0 and p_item10_org = 0.
    l_item10_flg = 'U'.
    l_item10_qty_flg = 'I'.
  else.
    l_item10_flg = 'U'.
    l_item10_qty_flg = 'U'.
  endif.

  if p_item20_qty = 0 and p_item20_org > 0.
    l_item20_flg = 'U'.
    l_item20_qty_flg = 'D'.
  elseif p_item20_qty > 0 and p_item20_org = 0.
    l_item20_flg = 'U'.
    l_item20_qty_flg = 'I'.
  else.
    l_item20_flg = 'U'.
    l_item20_qty_flg = 'U'.
  endif.

  l_it_ord_header_inx-updateflag = 'U'.
  append l_it_ord_header_inx.

  l_it_itm-itm_number = '000010'.
  append l_it_itm.
  l_it_itm-itm_number = '000020'.
  append l_it_itm.

  l_it_itmx-updateflag = l_item10_flg.
  l_it_itmx-itm_number = '000010'.
  append l_it_itmx.
  l_it_itmx-updateflag = l_item20_flg.
  l_it_itmx-itm_number = '000020'.
  append l_it_itmx.

  p_item10_org = pa_val10 .
  p_item20_org = pa_val20 .

  l_it_lines-itm_number = '000010'.
  l_it_lines-sched_line = '0001'.
  l_it_lines-req_qty = p_item10_org.
  append l_it_lines.
  l_it_lines-itm_number = '000020'.
  l_it_lines-sched_line = '0001'.
  l_it_lines-req_qty = p_item20_org.
  append l_it_lines.

  l_it_linesx-updateflag = l_item10_qty_flg.
  l_it_linesx-itm_number = '000010'.
  l_it_linesx-sched_line = '0001'.
  l_it_linesx-req_qty = 'X'.
  append l_it_linesx.
  l_it_linesx-updateflag = l_item20_qty_flg.
  l_it_linesx-itm_number = '000020'.
  l_it_linesx-sched_line = '0001'.
  l_it_linesx-req_qty = 'X'.
  append l_it_linesx.

  call function 'BAPI_SALESORDER_CHANGE'
       exporting
            salesdocument    = pa_order
            order_header_inx = l_it_ord_header_inx
       tables
            return           = l_it_return
            order_item_in    = l_it_itm
            order_item_inx   = l_it_itmx
            schedule_lines   = l_it_lines
            schedule_linesx  = l_it_linesx.

  loop at l_it_return.
    if l_it_return-type = 'E' or
       l_it_return-type = 'A'   .
      wa_error = 'X'           .
*    ELSE.
    endif.
  endloop.
endform.                    " call_bapi_salesorder

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0346   text
*----------------------------------------------------------------------*
form create_log using    pa_step  pa_data like it_7jb .
  data: l_log                like ztpp_rep_seq .

  clear: l_log.
  select max( sequence ) into l_log-sequence
    from ztpp_rep_seq
   where wk_date  = wa_date .

  l_log-wk_date   = wa_date            .
  l_log-sequence  = l_log-sequence + 1 .
  l_log-step      = pa_step            .
  l_log-status    = 'E'                .
  l_log-logtype   = 'E'                .

  case pa_step.
    when '1'  .
    when '2'  .
    when '3'  .
      move-corresponding pa_data  to l_log .
      l_log-msg = it_7jb-vinn              .
    when '4'  .
      move-corresponding pa_data  to l_log .
    when '5'  .
      move-corresponding pa_data  to l_log .
  endcase.
  insert into ztpp_rep_seq values l_log    .
endform.                    " create_log
