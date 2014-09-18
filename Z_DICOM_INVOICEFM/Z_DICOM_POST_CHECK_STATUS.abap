function z_dicom_post_check_status.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2
*"  CHANGING
*"     REFERENCE(P_RETURN) TYPE  BAPIRETURN1
*"--------------------------------------------------------------------
*{   INSERT         TS2K900130                                        1

  data:
      err_msgid like sy-msgid,
      err_msgno like sy-msgno,
      err_msg   like sy-msgv1.

  if not return[] is initial.
    loop at return.
      err_msgid = return-id.
      err_msgno = return-number.
      err_msg = return-message.

      perform zd_gen_bapireturn1
              using 'E'
              err_msgid
              err_msgno
              err_msg
              ''
              ''
              ''
      changing p_return.
*     We only return the first error message
      exit.
    endloop.
  endif.

*}   INSERT





endfunction.
