function z_dicom_msg_populate.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ERROR_MSG) LIKE  SY-MSGV1
*"  EXCEPTIONS
*"      GENERAL_ERROR
*"----------------------------------------------------------------------
  data:
        msg        like  sy-msgv1.

  if sy-msgid ne space.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if error_msg ne space.
      msg = error_msg.
    else.
      msg = 'Unexpected Error'.
    endif.

    call function 'Z_DICOM_GEN_BAPIRETURN'
      exporting
        p_msgty = 'E'
        p_msgid = '00'
        p_msgno = '208'
        p_msgv1 = msg
        p_msgv2 = ''
        p_msgv3 = ''
        p_msgv4 = ''.
  endif.

endfunction.
