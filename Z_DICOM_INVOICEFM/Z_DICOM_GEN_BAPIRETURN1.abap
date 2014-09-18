function z_dicom_gen_bapireturn1.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(P_MSGTY) LIKE  SY-MSGTY
*"     VALUE(P_MSGID) LIKE  SY-MSGID
*"     VALUE(P_MSGNO) LIKE  SY-MSGNO
*"     VALUE(P_MSGV1) LIKE  SY-MSGV1
*"     VALUE(P_MSGV2) LIKE  SY-MSGV2
*"     VALUE(P_MSGV3) LIKE  SY-MSGV3
*"     VALUE(P_MSGV4) LIKE  SY-MSGV4
*"  EXPORTING
*"     VALUE(P_RETURN) TYPE  BAPIRET1
*"--------------------------------------------------------------------
  perform zd_gen_bapireturn1
                using
                   p_msgty
                   p_msgid
                   p_msgno
                   p_msgv1
                   p_msgv2
                   p_msgv3
                   p_msgv4
                changing
                   p_return.





endfunction.
