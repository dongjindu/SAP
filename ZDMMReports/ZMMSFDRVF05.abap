*&---------------------------------------------------------------------*
*&  Include           ZMMSFDRVF05
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*    Subroutines .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_ADDRESS
*&---------------------------------------------------------------------*
form get_vendor_address using    p_emlif like lfa1-lifnr
                        changing p_adrnr.
* parameter P_ADRNR without type since there are several address
* fields with different domains

  data: l_lfa1 like lfa1.

  check not p_emlif is initial.
  call function 'VENDOR_MASTER_DATA_SELECT_00'
       exporting
            i_lfa1_lifnr     = p_emlif
            i_data           = 'X'
            i_partner        = ' '
       importing
            a_lfa1           = l_lfa1
       exceptions
            vendor_not_found = 1.
  if sy-subrc eq 0.
    p_adrnr = l_lfa1-adrnr.
  else.
    clear p_adrnr.
  endif.

endform.                               " GET_VENDOR_ADDRESS
*&---------------------------------------------------------------------*
*&      Form  get_addr_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CS_BIL_INVOICE_HD_ADR  text
*      <--P_CS_ADDR_KEY  text
*----------------------------------------------------------------------*
form get_addr_key
                  changing l_addr_key like addr_key.

  data: l_lfa1 like lfa1.
  select single * from lfa1 into l_lfa1
  where lifnr = nast-parnr.
  if sy-subrc = 0.
    move l_lfa1-adrnr to l_addr_key.
  endif.

endform.                               " get_addr_key
*&---------------------------------------------------------------------*
*&      Form  protocol_update_I
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form protocol_update_i.
  check xscreen = space.
  call function 'NAST_PROTOCOL_UPDATE'
       exporting
            msg_arbgb = syst-msgid
            msg_nr    = syst-msgno
            msg_ty    = syst-msgty
            msg_v1    = syst-msgv1
            msg_v2    = syst-msgv2
            msg_v3    = syst-msgv3
            msg_v4    = syst-msgv4
       exceptions
            others    = 1.
endform.                               " protocol_update_I
*&---------------------------------------------------------------------*
*&      Form  add_smfrm_prot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_smfrm_prot.
  data: lt_errortab             type tsferror.
  data: lf_msgnr                type sy-msgno.
  data:  l_s_log          type bal_s_log,
         p_loghandle      type balloghndl,
       l_s_msg          type bal_s_msg.
  field-symbols: <fs_errortab>  type line of tsferror.
* get smart form protocoll
  call function 'SSF_READ_ERRORS'
       importing
            errortab = lt_errortab.
  sort lt_errortab.
* delete adjacent duplicates from lt_errortab comparing errnumber.
* add smartform protocoll to nast protocoll
  loop at lt_errortab assigning <fs_errortab>.
    clear lf_msgnr.
    lf_msgnr = <fs_errortab>-errnumber.
    call function 'NAST_PROTOCOL_UPDATE'
         exporting
              msg_arbgb = <fs_errortab>-msgid
              msg_nr    = lf_msgnr
              msg_ty    = <fs_errortab>-msgty
              msg_v1    = <fs_errortab>-msgv1
              msg_v2    = <fs_errortab>-msgv2
              msg_v3    = <fs_errortab>-msgv3
              msg_v4    = <fs_errortab>-msgv4
         exceptions
              others    = 1.
  endloop.


* open the application log
  l_s_log-extnumber    = sy-uname.

  call function 'BAL_LOG_CREATE'
       exporting
            i_s_log      = l_s_log
       importing
            e_log_handle = p_loghandle
       exceptions
            others       = 1.
  if sy-subrc <> 0.
  endif.

  loop at lt_errortab assigning <fs_errortab>.
    move-corresponding <fs_errortab> to l_s_msg.
    call function 'BAL_LOG_MSG_ADD'
         exporting
              i_log_handle = p_loghandle
              i_s_msg      = l_s_msg
         exceptions
              others       = 1.
    if sy-subrc <> 0.
    endif.
  endloop.
  call function 'BAL_DSP_LOG_DISPLAY'.


endform.                               " add_smfrm_prot
