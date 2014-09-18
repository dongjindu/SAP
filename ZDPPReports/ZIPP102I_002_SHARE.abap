************************************************************************
* Program Name      : ZIPP102I_002_SHARE
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
report  zipp102i_002_share  message-id zmpp  .

*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
tables: equi ,
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
      wa_instance             like mara-cuobf                 ,
      wa_number               like ztpp_pp_log_head-logkey    ,
      wa_7jb                  like ztpp_pmt07jb_b             ,
      wa_maxday               like sy-datum                   ,
      wa_vin                  like mara-matnr                 ,
      wa_equnr                like equi-equnr                 ,
      wa_lines                type i                          ,
      wa_msg(70)              type c                          ,
      wa_mng                  type i                          ,
      wa_flag                 type c                          ,
      wa_err_hd               type c                          ,
      wa_mode                 type c   value   'N'            ,
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
        write at: /001(09)  text-101,     " l_ordr ,
                   011(05)  text-102,     " l_dist ,
                   017(03)  text-103,     " l_extc ,
                   021(03)  text-104.     " l_intc .
     uline at: /(50)             .
  endif.

*----------------------------------------------------------------------
start-of-selection.
*----------------------------------------------------------------------
  if p_run = 'X'.
    " Normal Processing...
    perform get_data.
    perform check_data .
    check wa_flag is initial.
    perform bdc_wocl_summary .           " Step 1: Work Order Color
    perform bdc_wohd_summary .
    perform record_processing.
  else.
    " Reprocessing..
  endif.
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
  select * into corresponding fields of table it_7jb
    from ztpp_pmt07jb_b .

  describe table it_7jb lines wa_lines .
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

  c_prog = 'ZIPP102I_JOBPROGRAM'.
  sort it_7jb by sqdt ssr1.
  loop at it_7jb .
    case it_7jb-mtgu .
      when 'M'      .
        perform process_mitu_value .
      when others.
*       PERFORM CREATE_HEAD        .
        perform job_create.
    endcase.
  endloop.
  perform update_commonvlas    .
  perform delete_joblist                .
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
    update ztpp_pmt07jb_a    set: plnum = l_porder
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
*
*  WRITE AT: /001(50) 'Update the Work Order...........' ,
*             045(20)  pa_material                       .

  l_vars-atnam = pa_char .  l_vars-atwrt = l_dec1.  append l_vars.
  if pa_z = 'Z'.
    l_dec = wa_mng  .
    write l_dec   to l_dec2.
    l_vars-atnam = 'P_MITU_QTY' .  l_vars-atwrt =  l_dec2.
    append l_vars.
  endif.

  call function 'Z_FPP_HANDLING_MASTER'
       exporting
            object    = pa_material
            mode      = 'W'
            ctype     = '001'
       tables
            val_table = l_vars.

  loop at l_vars.
    check l_vars-zflag = 'E' .
    write at: /001(025) 'Function - Error Result : ' ,
               026(020)  pa_material  ,
               046(020) 'Characteristics is '        ,
               066(020)  l_vars-atwrt                .
  endloop.
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
        l_kwmeng            like vbap-kwmeng .

  " Change the Sales Order
  l_val10 =  pa_val10 .
  l_val20 =  pa_val20 .
  l_val20 = l_val20 - l_val10.
*
*  WRITE AT: /001(50) 'Update the Sales Order..........' ,
*             045(20)  pa_sorder                         .

  perform bdc_dynpro_processing using :
                         'X'  'SAPMV45A'             '0102',
                         ' '  'BDC_OKCODE'           '=UER2' ,
                         ' '  'VBAK-VBELN'            pa_sorder,

                         'X'  'SAPMV45A'             '4001',
                         ' '  'BDC_OKCODE'           '=SICH' ,
                         ' '  'RV45A-KWMENG(01)'      l_val10,
                         ' '  'RV45A-KWMENG(02)'      l_val20.

  call transaction 'VA02'  using it_bdcdata mode wa_mode
                           messages into    it_msg    .

  loop at it_msg.
    check it_msg-msgid = 'E' .
    call function 'MESSAGE_TEXT_BUILD'
         exporting
              msgid               = it_msg-msgid
              msgnr               = it_msg-msgnr
              msgv1               = it_msg-msgv1
              msgv2               = it_msg-msgv2
              msgv3               = it_msg-msgv3
              msgv4               = it_msg-msgv4
         importing
              message_text_output = wa_msg.
    write: /'Sales Order -- ' , wa_msg .
  endloop.

  clear: it_bdcdata, it_bdcdata[], it_msg, it_msg[].
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
      clear: wa_material,  l_data, l_data[].
      concatenate l_ordr      l_dist      into wa_material .

      " Work Order Header's SEQ  Qty Change......
      select single cuobf into wa_instance
        from mara
       where matnr = wa_material.

      call function 'Z_FPP_HANDLING_MASTER'
        exporting
          object             = wa_material
          ctype              = '001'
        tables
          val_table          = l_data .

      clear: l_plnmg, wa_mng, ausp.

      call function 'CONVERSION_EXIT_ATINN_INPUT'
           exporting
                input  = 'P_SEQ_QTY'
           importing
                output = ausp-atinn.

      select single *
        from ausp
       where objek = wa_material
         and atinn = ausp-atinn    .

      if sy-subrc = 0 .
        l_plnmg = ausp-atflv.
      endif.
      l_plnmg = l_plnmg + l_seqqty     .
      clear: l_data-atwrt, ausp        .
      call function 'CONVERSION_EXIT_ATINN_INPUT'
           exporting
                input  = 'P_MITU_QTY'
           importing
                output = ausp-atinn.

      select single *
        from ausp
       where objek = wa_material
         and atinn = ausp-atinn    .

      if sy-subrc = 0 .
        wa_mng  = ausp-atflv.
      endif.
      wa_mng  = wa_mng  - l_mituqty    .

      clear: l_data-atwrt .
      read table l_data     with key atnam = 'P_VIN_SPEC'    .
      if sy-subrc = 0.
        it_vin-vin  = l_data-atwrt .
      endif.

      perform call_bdc_workorder
                          using wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

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

  if wa_lines > 0 .
    clear: wa_material,  l_data, l_data[].
    concatenate l_ordr      l_dist      into wa_material .

    " Work Order Header's SEQ  Qty Change......
    select single cuobf into wa_instance
      from mara
     where matnr = wa_material.

    call function 'Z_FPP_HANDLING_MASTER'
         exporting
              object    = wa_material
              ctype     = '001'
         tables
              val_table = l_data.

    clear: l_plnmg, wa_mng, l_data-atwrt, ausp.

    call function 'CONVERSION_EXIT_ATINN_INPUT'
         exporting
              input  = 'P_SEQ_QTY'
         importing
              output = ausp-atinn.

    select single *
      from ausp
     where objek = wa_material
       and atinn = ausp-atinn    .

    if sy-subrc = 0 .
      l_plnmg = ausp-atflv.
    endif.
    l_plnmg = l_plnmg + l_seqqty     .
    clear: l_data-atwrt, ausp        .
    call function 'CONVERSION_EXIT_ATINN_INPUT'
         exporting
              input  = 'P_MITU_QTY'
         importing
              output = ausp-atinn.

    select single *
      from ausp
     where objek = wa_material
       and atinn = ausp-atinn    .

    if sy-subrc = 0 .
      wa_mng  = ausp-atflv.
    endif.
    wa_mng  = wa_mng  - l_mituqty    .

    clear: l_data-atwrt .
    read table l_data     with key atnam = 'P_VIN_SPEC'    .
    if sy-subrc = 0.
      it_vin-vin  = l_data-atwrt .
    endif.

    perform call_bdc_workorder
                        using wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

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

      l_seqqty  = l_seq     + l_tseq  .
      update ztpp_wosum   set seqqty  = l_seqqty
                        where wo_ser = l_ordr
                          and nation = l_dist(3)
                          and dealer = l_dist+3(2)
                          and extc   = l_extc
                          and intc   = l_intc     .

      if sy-subrc ne 0.
        " Step 1: Update Fail - Reason: Data not found!!!
        " Save the Working data.
        wa_flag = 'X'.
        exit.
      endif.
*
*      WRITE AT: /001(50) 'Update WorkOrder Summary...' ,
*                 050(05)  sy-subrc,
*                /001(09)  l_ordr ,
*                 011(05)  l_dist ,
*                 017(03)  l_extc ,
*                 021(03)  l_intc .
      " Process.
      clear: wa_material,  l_data, l_data[].
      concatenate l_ordr       l_dist         into wa_material .
      concatenate wa_material  l_extc  l_intc into wa_material.

      select single cuobf into wa_instance
        from mara
       where matnr = wa_material.

      clear: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
      call function 'CONVERSION_EXIT_ATINN_INPUT'
           exporting
                input  = 'P_SEQ_QTY'
           importing
                output = ausp-atinn.

      select single *
        from ausp
       where objek = wa_material
         and atinn = ausp-atinn    .

      if sy-subrc = 0 .
        l_plnmg = ausp-atflv.
      endif.
      l_plnmg = l_plnmg + l_seq        .
      clear: l_data-atwrt, ausp        .
      call function 'CONVERSION_EXIT_ATINN_INPUT'
           exporting
                input  = 'P_MITU_QTY'
           importing
                output = ausp-atinn.

      select single *
        from ausp
       where objek = wa_material
         and atinn = ausp-atinn    .

      if sy-subrc = 0 .
        wa_mng  = ausp-atflv.
      endif.
      wa_mng  = wa_mng  - l_mituqty    .
      perform call_bdc_workorder
                           using wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

      " Sales Order Master Change...
     perform call_bdc_sales_order using l_salesorder l_mituqty
                                        l_seqqty     l_modqty  .
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
    update ztpp_wosum   set seqqty  = l_seqqty
                      where wo_ser = it_7jb-ordr
                        and nation = it_7jb-dist(3)
                        and dealer = it_7jb-dist+3(2)
                        and extc   = it_7jb-extc
                        and intc   = it_7jb-intc     .
*
*    WRITE AT: /001(50) 'Update WorkOrder Summary...' ,
*               050(05)  sy-subrc,
*              /001(09)  l_ordr ,
*               011(05)  l_dist ,
*               017(03)  l_extc ,
*               021(03)  l_intc .

    clear: wa_material,  l_data, l_data[].
    concatenate l_ordr       l_dist         into wa_material .
    concatenate wa_material  l_extc  l_intc into wa_material.

    select single cuobf into wa_instance
      from mara
     where matnr = wa_material.

    clear: l_plnmg, wa_mng, l_tseq, l_data-atwrt, ausp.
    call function 'CONVERSION_EXIT_ATINN_INPUT'
         exporting
              input  = 'P_SEQ_QTY'
         importing
              output = ausp-atinn.

    select single *
      from ausp
     where objek = wa_material
       and atinn = ausp-atinn    .

*     READ TABLE l_data WITH KEY atnam = 'P_SEQ_QTY'  .
*     l_plnmg = l_data-atwrt .
*     l_plnmg = AUSP-atwrt + AUSP-ATFLV.
    if sy-subrc = 0 .
      l_plnmg = ausp-atflv.
    endif.
    l_plnmg = l_plnmg + l_seq        .
    clear: l_data-atwrt, ausp        .
    call function 'CONVERSION_EXIT_ATINN_INPUT'
         exporting
              input  = 'P_MITU_QTY'
         importing
              output = ausp-atinn.

    select single *
      from ausp
     where objek = wa_material
       and atinn = ausp-atinn    .

*     READ TABLE l_data WITH KEY atnam = 'P_MITU_QTY' .
*     wa_mng  = l_data-atwrt .
*     wa_mng  = AUSP-atwrt + AUSP-ATFLV.
    if sy-subrc = 0 .
      wa_mng  = ausp-atflv.
    endif.
    wa_mng  = wa_mng  - l_mituqty    .
    perform call_bdc_workorder
                         using wa_material l_plnmg 'P_SEQ_QTY' 'Z'.

    " Sales Order Master Change...
    perform call_bdc_sales_order using l_salesorder l_mituqty
                                       l_seqqty     l_modqty  .
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
  data: c_prog                like ztpp_common_vals-jobs.

  c_prog = 'ZIPP101U_PMT07JB_A'.
  select max( sqdt ) into wa_maxday
    from ztpp_pmt07jb_b .
  update ztpp_common_vals set dates = wa_maxday
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
form job_create.
  refresh: global_job, global_step_tbl.
  clear:   global_job, global_step_tbl.

  wa_count = wa_count + 1.
  concatenate sy-mandt '_' wa_count into jobn.

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
       job_cr_dialog             = 'N'
       job_cr_head_inp           = global_job
    importing
      job_cr_head_out           = global_job
    tables
      job_cr_steplist           = global_step_tbl
    exceptions
      cant_create_job           = 1
      invalid_dialog_type       = 2
      invalid_job_data          = 3
      job_create_canceled       = 4
      others                    = 5.

  jobc = global_job-jobcount.
  jobn = global_job-jobname.

  submit zipp102i_jobprogram   and return
         with p_7jb    eq wa_7jb
*        with p_vin    eq it_vin-vin
*        WITH P_EQUNR  EQ WA_EQUNR
         with p_log    eq wa_number .
*        VIA JOB jobn NUMBER jobc .

  concatenate 'IEQ' jobn     into s_jobnam.  append s_jobnam.
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
*&      Form  DELETE_JOBLIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_joblist.

endform.                    " DELETE_JOBLIST

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
*&      Form  CREATE_HEAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_head.
  data: l_salesorder         like ztpp_wosum-sales,
        l_fsc                like mara-matnr,
        l_workorder          like mara-matnr,
        l_general            like bapi_itob ,
        l_specific           like bapi_itob_eq_only,
        l_return             like bapiret2.

  perform check_vin         .
  perform vin_code_gen      .

  select single fsc sales into (l_fsc, l_salesorder)
    from ztpp_wosum
   where wo_ser = it_7jb-ordr
     and nation = it_7jb-dist(3)
     and dealer = it_7jb-dist+3(2)
     and extc   = it_7jb-extc
     and intc   = it_7jb-intc     .

  select single *
    from equi
   where equnr = wa_equnr.

  if sy-subrc = 0.
    " Create Error Log for the COMMON_VALS incorrect!!!
    exit.
  endif.

  " Create the Vehicle Master
  concatenate  wa_7jb-ordr  wa_7jb-dist  wa_7jb-extc  wa_7jb-intc
         into l_workorder.
  concatenate l_fsc wa_7jb-vers+1(2) l_workorder
         into l_general-descript separated by space     .
  perform get_workcenter      using l_general-pp_wkctr  .
* perform get_rsnum           using pa_plnum  l_rsnum   .

  l_general-objecttype       = '1000' .
  l_general-manfacture       = 'HMMA' .
  l_general-mancountry       = 'US'   .
  l_general-countr_iso       = 'US'   .
  l_general-manserno         =  wa_vin+11(7)            .
  l_general-manmodel         =  wa_7jb-bmdl(3)          .
  l_general-constyear        =  sy-datum(4)             .
  l_general-constmonth       =  sy-datum+4(2)           .
  l_general-start_from       =  sy-datum                .
  l_general-planplant        = 'P001'                   .
  l_general-manparno         =  wa_vin                  .
* l_general-descript         =  l_equnr                 .
* l_general-sortfield        =  l_rsnum                 .
  l_general-maintplant       = 'P001'                   .
* l_general-pp_wkctr         =  l_workcenter            .
  l_general-read_crdat       = sy-datum.
  l_general-read_crnam       = sy-uname.

  l_specific-equicatgry      = 'V' .

  call function 'BAPI_EQUI_CREATE'
       exporting
            external_number = wa_equnr
            data_general    = l_general
            data_specific   = l_specific
            valid_date      = sy-datum
       importing
            return          = l_return.

  call function 'BAPI_TRANSACTION_COMMIT' .
  wait up to 3 seconds.
endform.                    " CREATE_HEAD

*&---------------------------------------------------------------------*
*&      Form  GET_WORKCENTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORKCENTER  text
*----------------------------------------------------------------------*
form get_workcenter using    pa_workcenter.
  select single objid
               into pa_workcenter
               from crhd
               where arbpl eq 'T'.
endform.                    " GET_WORKCENTER

*&---------------------------------------------------------------------*
*&      Form  get_rsnum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PA_PLNUM  text
*      -->P_L_RSNUM  text
*----------------------------------------------------------------------*
form get_rsnum using    pa_plnum  pa_rsnum.
  select single rsnum into pa_rsnum
    from plaf
   where plnum = pa_plnum.
endform.                    " get_rsnum

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
            p_lastid = l_vin .

  wa_vin = l_vin     .
  concatenate l_mode  l_vin+11(7)  into wa_equnr .
endform.                    " VIN_CODE_GEN
