*&----------------------------------------------------------------------
*& Program ID        : ZHR_AFLAC_DATA_IN
*& Title             : [HR] - AFLAC Inbound Data Transfer
*& Created by        : Sunho Jeong
*& Created on        : 06/26/2013
*& Specifications By : Grace Li
*& Reference Pgm     : N/A
*& Description       : This program is to receive inbound data from AFLAC
*&                     via EAI. After receiving data this program check the
*&                     validation and execute BDC for processing.
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*&
*&----------------------------------------------------------------------

report  zhrr_aflac_data_in message-id zmhr.

data: gt_bdcdata like bdcdata     occurs 10 with header line,
      gt_bdcmsg  like bdcmsgcoll  occurs 10 with header line.

data: it_msg_list       type tcdat_msg_list,
      st_msg_list       type tcdat_msg.

data: gv_total_cnt      type string,
      gv_error_cnt      type string.

data: gs_text           type line of bcsy_text,
      gt_e_text         type bcsy_text.

data: gs_result type bapiret2.

data: g_return type zmms0053.
data: it_data like table of zshr_aflac_in with header line.

data: gs_option like ctu_params.

selection-screen begin of block blk with frame title text-t02.
parameters: p_pernr(8),
            p_nachn(40),
            p_vorna(40),
            p_event(1),
            p_begda type dats,
            p_amont type char8,
            p_ddate type dats.
* 07/17/2013 - T00306 Start
selection-screen skip.
parameters: p_dest like rzllitab-classname obligatory
                   default 'WMHR01'.
* 07/17/2013 - T00306 End
selection-screen end of block blk.

parameters: p_batch as checkbox.

initialization.


start-of-selection.

  case 'X'.
    when p_batch.

      call function 'ZFHR_AFLAC_IN' destination p_dest
        importing
          e_return                       = g_return
        tables
          it_data                        = it_data
        exceptions
          call_function_destination_no_t = 1
          call_function_no_dest          = 2
          call_function_remote_error     = 3
          rfc_no_authority               = 4
          others                         = 5.

      if sy-subrc <> 0.
        message i001 with g_return-message.
        exit.
*        message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.

      describe table it_data lines gv_total_cnt.

      loop at it_data.
        clear gs_result.
        perform check_validation using it_data changing gs_result.

        if gs_result-type eq 'S'.
          perform process_call_transaction using 'PA30' it_data.
        else.
          add 1 to gv_error_cnt.
          concatenate '[' it_data-pernr ']' into gs_text.
          append gs_text to gt_e_text. clear gs_text.
          concatenate gs_text gs_result-message into gs_text.
          append gs_text to gt_e_text. clear gs_text.
          append gs_text to gt_e_text. clear gs_text.
        endif.

      endloop.

      perform send_email.

    when others.
      it_data-pernr = p_pernr.
      it_data-nachn = p_nachn.
      it_data-vorna = p_vorna.
      it_data-event = p_event.
*      it_data-begda = p_begda.
*      it_data-ddate = p_ddate.
      if not p_begda is initial.
        write p_begda using edit mask '__/__/____' to it_data-begda.
      endif.
      if not p_ddate is initial.
        write p_ddate using edit mask '__/__/____' to it_data-ddate.
      endif.
      it_data-amont = p_amont.
      clear gs_result.

      gv_total_cnt = 1.

      perform check_validation using it_data changing gs_result.
      if gs_result-type eq 'S'.
        perform process_call_transaction using 'PA30' it_data.
      else.
        add 1 to gv_error_cnt.
        concatenate '[' it_data-pernr ']' into gs_text.
        append gs_text to gt_e_text. clear gs_text.
        concatenate gs_text gs_result-message into gs_text.
        append gs_text to gt_e_text. clear gs_text.
        append gs_text to gt_e_text. clear gs_text.
      endif.

      perform send_email.

  endcase.

*&---------------------------------------------------------------------*
*&      Form  check_validation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATA     text
*----------------------------------------------------------------------*
form check_validation using p_data structure zshr_aflac_in
                   changing c_result structure bapiret2.

  data: lv_abkrs type abkrs,
        lv_revised type sy-datum,
        lv_nextmon type sy-datum.

  data: l_date type dats.

  ranges: r_curr for sy-datum,
          r_foll for sy-datum.

  " next month
  if sy-datum+4(2) eq 12.
    lv_nextmon+0(4) = sy-datum+0(4) + 1.
    lv_nextmon+4(4) = '0101'.
  else.
    lv_nextmon+0(4) = sy-datum+0(4).
    lv_nextmon+4(2) = sy-datum+4(2) + 1.
    lv_nextmon+6(2) = '01'.
  endif.

  select single abkrs into lv_abkrs
    from pa0001
   where pernr = p_data-pernr
     and endda = '99991231'.

  r_curr = 'IBT'.
  r_foll = 'IBT'.
*  call function 'PA03_PERIODDATES_GET'
*    exporting
*      f_abkrs         = lv_abkrs
*    importing
*      f_current_begda = r_curr-low
*      f_current_endda = r_curr-high.
  call function 'PA03_PCR_READ'
    exporting
      f_abkrs               = lv_abkrs
      error_no_accounting   = 'X'
    importing
      f_current_begda       = r_curr-low
      f_current_endda       = r_curr-high
      f_following_begda     = r_foll-low
      f_following_endda     = r_foll-high
    exceptions
      abkrs_no_accounting   = 1
      pcr_does_not_exist    = 2
      abkrs_does_not_exist  = 3
      period_does_not_exist = 4
      others                = 5.

  append r_curr.
  append r_foll.

  if sy-datum in r_curr.
  elseif sy-datum in r_foll.
    r_curr[] = r_foll[].
  else.
    c_result-type = 'E'.
    c_result-message = 'Date doesn''t fall in the upcoming Pay Period'.
    exit.
  endif.

  read table r_curr index 1.
*append r_date_revised.

  case lv_abkrs.
    when '11'.
      lv_revised = r_curr-low.
    when '13'.
      if p_data-begda+6(2) > 15.
        lv_revised = lv_nextmon.
      else.
        lv_revised = sy-datum.
        lv_revised+6(2) = '01'.
      endif.
  endcase.

* Validation

  if not p_data-begda is initial.
*    l_date = p_data-begda.
    l_date+0(4) = p_data-begda+6(4).
    l_date+4(2) = p_data-begda+0(2).
    l_date+6(2) = p_data-begda+3(2).
*    clear p_data-begda+8(2).
  endif.

  if not p_data-ddate is initial.
*    l_date = p_data-ddate.
    l_date+0(4) = p_data-ddate+6(4).
    l_date+4(2) = p_data-ddate+0(2).
    l_date+6(2) = p_data-ddate+3(2).
*    clear p_data-begda+8(2).
  endif.

  case p_data-event.
* Start a new policy or Change the deduction amount
    when 'N' or 'X'.
      if l_date not in r_curr.
        c_result-type = 'E'.
        c_result-message =
        'BeginDate doesn''t fall in the upcoming Pay Period'.
        exit.
      else.
        l_date = lv_revised.
        write l_date to p_data-begda using edit mask '__/__/____'.
      endif.
* Stop an existing policy
    when 'C'.
      if l_date not in r_curr.
        c_result-type = 'E'.
        c_result-message =
        'DelimitationDate doesn''t fall in the upcoming Pay Period'.
        exit.
      else.
        l_date = lv_revised.
        write l_date to p_data-ddate using edit mask '__/__/____'.
      endif.
  endcase.

  c_result-type ='S'.

endform.                    "check_validation

*&---------------------------------------------------------------------*
*&      Form  PROCESS_CALL_TRANSACTION
*&---------------------------------------------------------------------*
form process_call_transaction using p_tcode
                                    p_data structure zshr_aflac_in.

  data: l_begda(10),
        l_ddate(10).

  l_begda = p_data-begda.
  l_ddate = p_data-ddate.
*  concatenate p_data-begda+4(2) p_data-begda+6(2) p_data-begda+0(4)
*         into l_begda separated by '/'.
*  concatenate p_data-ddate+4(2) p_data-ddate+6(2) p_data-ddate+0(4)
*         into l_ddate separated by '/'.

  case p_data-event.
* Start a new policy
    when 'N'.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using:
              'BDC_OKCODE '       '=INS',
              'RP50G-PERNR'       p_data-pernr,
              'BDC_SUBSCR'        'SAPMP50A                                0800SUBSCR_HEADER',
              'BDC_SUBSCR'        'SAPMP50A                                0320SUBSCR_ITMENU',
              'BDC_SUBSCR'        'SAPMP50A                                0330SUBSCR_TIME',
              'RP50G-TIMR6'       'X',
              'BDC_SUBSCR'        'SAPMP50A                                0350SUBSCR_ITKEYS',
              'BDC_CURSOR'        'RP50G-SUBTY',
              'RP50G-CHOIC'       '0014',
              'RP50G-SUBTY'       '8480',
              '           '       '     '.
      perform bdc_dynpro      using 'MP001400' '2010'.
      perform bdc_field       using:
              'BDC_CURSOR'        'Q0014-BETRG',
              'BDC_OKCODE'        'UPD',
              'P0014-BEGDA'       l_begda,
              'P0014-ENDDA'       '12/31/9999',
              'P0014-LGART'       '8480',
              'Q0014-BETRG'       p_data-amont,
              'P0014-WAERS'       'USD',
              'BDC_SUBSCR'        'MP001400                                0100SUB0014',
              'BDC_SUBSCR'        'MP081500                                2020MULTIPLE_CHECKS',
              'BDC_SUBSCR'        'SAPMP50A                                0090SUBSCREEN_T582C'.
* Stop an existing policy
    when 'C'.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using:
             'BDC_OKCODE'       '=TIMERANGE'  ,
             'RP50G-PERNR'      p_data-pernr,
             'BDC_SUBSCR'       '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER',
             'BDC_SUBSCR'       'SAPMP50A                                0320SUBSCR_ITMENU',
             'BDC_SUBSCR'       'SAPMP50A                                0330SUBSCR_TIME',
             'BDC_CURSOR'       'RP50G-TIMR1',
             'RP50G-TIMR6'      ' ',
             'RP50G-TIMR1'      'X',
             'BDC_SUBSCR'       'SAPMP50A                                0350SUBSCR_ITKEYS',
             'RP50G-CHOIC'      '0014',
             'RP50G-SUBTY'      '8480'.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using:
             'BDC_OKCODE'     '=LIS9',
             'RP50G-PERNR'    p_data-pernr,
             'BDC_SUBSCR'     '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER',
             'BDC_SUBSCR'     'SAPMP50A                                0320SUBSCR_ITMENU',
             'BDC_SUBSCR'     'SAPMP50A                                0330SUBSCR_TIME',
             'BDC_CURSOR'     'RP50G-TIMR1',
*             'RP50G-BEGDA'    sy-datum,
*             'RP50G-ENDDA'    sy-datum,
             'RP50G-TIMR1'    'X',
             'BDC_SUBSCR'     'SAPMP50A                                0350SUBSCR_ITKEYS',
             'RP50G-CHOIC'    'Recurring Payments/Deductions',
             'RP50G-SUBTY'    '8480'.
      perform bdc_dynpro      using 'SAPMP50A' '0500'.
      perform bdc_field       using:
             'BDC_CURSOR'     'RP50M-ABGRD',
             'BDC_OKCODE'     '=TAKE',
             'RP50M-ABGRD'    l_ddate.
      perform bdc_dynpro      using 'MP001400' '3000'.
      perform bdc_field       using:
             'BDC_CURSOR'   'P0014-LGART(01)',
             'BDC_OKCODE'   '=DLIM',
             'RP50M-BEGDA'    l_ddate,
             'RP50M-ENDDA'    l_ddate,
             'RP50M-SUBTY'    '8480',
             'RP50M-ABGRD'    l_ddate,
             'RP50M-PAGEA'    ' 1',
             'RP50M-SELE2(01)'  'X',
             'BDC_SUBSCR'   'SAPMP50A_CE                             0100SUBSCREEN_EMPL'.

* Change the deduction amount
    when 'X'.
      perform bdc_dynpro      using 'SAPMP50A' '1000'.
      perform bdc_field       using:
              'BDC_OKCODE'         '=COP'              ,
              'RP50G-PERNR'        p_data-pernr        ,
              'BDC_SUBSCR'         '/1PAPAXX/HDR_30060A                     0100SUBSCR_HEADER' ,
              'BDC_SUBSCR'         'SAPMP50A                                0320SUBSCR_ITMENU' ,
              'BDC_SUBSCR'         'SAPMP50A                                0330SUBSCR_TIME'   ,
              'RP50G-TIMR6'        'X'      ,
              'BDC_SUBSCR'         'SAPMP50A                                0350SUBSCR_ITKEYS' ,
              'BDC_CURSOR'         'RP50G-SUBTY'      ,
              'RP50G-CHOIC'        '0014'      ,
              'RP50G-SUBTY'        '8480'      .

      perform bdc_dynpro      using 'MP001400' '2010'.
      perform bdc_field       using:
              'BDC_CURSOR'         'Q0014-BETRG',
              'BDC_OKCODE'         'UPD      ',
              'P0014-BEGDA'	       l_begda,
              'P0014-ENDDA'	       '12/31/9999',
              'P0014-LGART'	       '8480',
              'Q0014-BETRG'	       p_data-amont,
              'P0014-WAERS'	       'USD',
              'BDC_SUBSCR'         'MP001400                               0100SUB0014',
              'BDC_SUBSCR'         'MP081500                               2020MULTIPLE_CHECKS',
              'BDC_SUBSCR'         'SAPMP50A                               0090SUBSCREEN_T582C'.
  endcase.


  perform call_transaction tables gt_bdcmsg
                           using  p_tcode
                                  'E'.

  read table gt_bdcmsg with key msgtyp = 'E'.
  if sy-subrc eq 0.
    concatenate '[' p_tcode ']  :' gt_bdcmsg-msgv1 into gt_bdcmsg-msgv1.
    perform append_msg_list using gt_bdcmsg-msgtyp  gt_bdcmsg-msgv1.
    add 1 to gv_error_cnt.
    concatenate '[' p_data-pernr ']' into gs_text.
    append gs_text to gt_e_text. clear gs_text.
    concatenate gs_text gt_bdcmsg-msgv1 into gs_text separated by space.
    append gs_text to gt_e_text. clear gs_text.
    append gs_text to gt_e_text. clear gs_text.
  endif.

endform.                    "PROCESS_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PROGRAM    text
*      -->DYNPRO     text
*----------------------------------------------------------------------*
form bdc_dynpro  using  program dynpro.
  clear gt_bdcdata.
  gt_bdcdata-program  = program.
  gt_bdcdata-dynpro   = dynpro.
  gt_bdcdata-dynbegin = 'X'.
  append gt_bdcdata.
endform.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
form bdc_field  using   fnam fval.
  clear gt_bdcdata.
  gt_bdcdata-fnam = fnam.
  gt_bdcdata-fval = fval.
  append gt_bdcdata.
endform.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
form call_transaction  tables  pt_bdcmsg structure bdcmsgcoll
                       using   p_tcode   type sy-tcode
                               p_dismode type ctu_mode.
  clear: pt_bdcmsg,
         gs_option.

  refresh: pt_bdcmsg.

  gs_option-dismode  = p_dismode.
  gs_option-dismode  = 'N'.
  gs_option-updmode  = 'S'.
  gs_option-defsize  = 'X'.
*  GS_OPTION-CATTMODE = ' '.
*  GS_OPTION-RACOMMIT = ' '.
*  GS_OPTION-NOBINPT  = 'X'.
*  GS_OPTION-NOBIEND  = 'X'.

  call transaction p_tcode using gt_bdcdata
                         options from gs_option
                        messages into pt_bdcmsg.
  refresh: gt_bdcdata.
  clear  : gt_bdcdata.
endform.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  APPEND_MSG_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TYPE     text
*      -->P_MSG      text
*----------------------------------------------------------------------*
form append_msg_list using p_type p_msg.

  clear st_msg_list.

  perform divide_message  using p_msg
                                     st_msg_list-msgv1
                                     st_msg_list-msgv2 .

  st_msg_list-msgid = 'ZKUSSD'.
  st_msg_list-msgty = p_type.
  st_msg_list-msgno = '001'.
  append st_msg_list to it_msg_list.

endform.                    "APPEND_MSG_LIST
*&---------------------------------------------------------------------*
*&      Form  DIVIDE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MESSAGE  text
*      -->P_MSG1     text
*      -->P_MSG2     text
*----------------------------------------------------------------------*
form divide_message using p_message p_msg1 p_msg2.

  data: l_segment  type i,
        l_offset   type i,
        l_cnt      type i.

  l_cnt = strlen( p_message ).

  if l_cnt > 50.
    call function 'TRUNCATE_MULTIPLE_BYTE_STRING'
      exporting
        string        = p_message
        target_length = 50
      importing
        use_length    = l_segment.

    l_offset = l_cnt - l_segment + 1.
    p_msg1   = p_message+0(l_segment).
    p_msg2   = p_message+l_segment(l_offset).
  else.
    p_msg1   = p_message.
  endif.

endform.                    "DIVIDE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR    text
*      -->P_EMAIL    text
*----------------------------------------------------------------------*
form send_email.

  data: send_request       type ref to cl_bcs.
  data: text               type bcsy_text.
  data: document           type ref to cl_document_bcs.
  data: sender             type ref to cl_sapuser_bcs.
  data: recipient          type ref to if_recipient_bcs.
  data: bcs_exception      type ref to cx_bcs.
  data: sent_to_all        type os_boolean.

  data: l_date(10),
        l_time(8).

  data: l_subject          type string,
        l_subject_char     type char50,
        l_text             type line of bcsy_text,
        l_nachn            type p0002-nachn,
        l_vorna            type p0002-vorna,
        l_fname(100)       type c,
        l_pernr(10)        type c.

  data: l_desc(100).

* Set Subject
  l_subject_char = '[STATUS]:AFLAC Inbound file import log - '.
  write: sy-datum using edit mask '__/__/____' to l_subject_char+41.
  l_subject = l_subject_char.

  try.
*     -------- create persistent send request ------------------------
      send_request = cl_bcs=>create_persistent( ).

*     -------- create and set document -------------------------------
*     create document from internal table with text

      write sy-datum mm/dd/yyyy to l_date.
      write sy-uzeit to l_time using edit mask '__:__:__'.
      append l_text to text. clear l_text.

      concatenate l_date l_time '- AFLAC changes upload finished.'
                                    into l_text separated by space.
      append l_text to text. clear l_text.
      append l_text to text. clear l_text.

      concatenate 'Total records:' gv_total_cnt
                                   into l_text separated by space.
      append l_text to text. clear l_text.
      append l_text to text. clear l_text.

      gv_total_cnt = gv_total_cnt - gv_error_cnt.
      concatenate 'No. of Records uploaded successfully:' gv_total_cnt
                                                    into l_text separated by space.
      append l_text to text. clear l_text.
      append l_text to text. clear l_text.

      concatenate 'No. of Records with Errors:' gv_error_cnt
                                          into l_text separated by space.
      append l_text to text. clear l_text.
      append l_text to text. clear l_text.

      loop at gt_e_text into l_text.
        append l_text to text.
      endloop.

      document = cl_document_bcs=>create_document(
                      i_type    = 'RAW'
                      i_text    = text
                      i_length  = '12'
                      i_subject = 'Subject' ).

*     add document to send request
      call method send_request->set_document( document ).

*     change subject line using more than 50 characters
      call method send_request->set_message_subject( l_subject ).

*     No delivery status
      call method send_request->set_status_attributes( 'N' ).

*     --------- set sender -------------------------------------------
*     note: this is necessary only if you want to set the sender
*           different from actual user (SY-UNAME). Otherwise sender is
*           set automatically with actual user.

      sender = cl_sapuser_bcs=>create( sy-uname ).
      call method send_request->set_sender
        exporting
          i_sender = sender.

*     --------- add recipient (e-mail address) -----------------------

      recipient = cl_distributionlist_bcs=>getu_persistent( i_dliname = 'HR_BN_AFLAC'
                                                            i_private = space ).
      send_request->set_send_immediately('X').


*     add recipient with its respective attributes to send request
      call method send_request->add_recipient
        exporting
          i_recipient = recipient
          i_express   = 'X'.

*     ---------- send document ---------------------------------------
      call method send_request->send(
        exporting
          i_with_error_screen = 'X'
        receiving
          result              = sent_to_all ).

      commit work.

* -----------------------------------------------------------
* *                     exception handling
* -----------------------------------------------------------
* * replace this very rudimentary exception handling
* * with your own one !!!
* -----------------------------------------------------------
    catch cx_bcs into bcs_exception.
*      write: / text-001, p_email.
      write:   text-002, bcs_exception->error_type.
      exit.

  endtry.

endform.                    "send_email
