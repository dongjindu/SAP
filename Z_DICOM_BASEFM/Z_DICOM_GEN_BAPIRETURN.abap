function z_dicom_gen_bapireturn.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(P_MSGTY) LIKE  BAPIRETURN-TYPE
*"     REFERENCE(P_MSGID) LIKE  SY-MSGID
*"     REFERENCE(P_MSGNO) LIKE  SY-MSGNO
*"     REFERENCE(P_MSGV1) LIKE  SY-MSGV1
*"     REFERENCE(P_MSGV2) LIKE  SY-MSGV2
*"     REFERENCE(P_MSGV3) LIKE  SY-MSGV3
*"     REFERENCE(P_MSGV4) LIKE  SY-MSGV4
*"  EXPORTING
*"     REFERENCE(P_RETURN) TYPE  BAPIRET1
*"--------------------------------------------------------------------
  data:
        lv_msgty like sy-msgty.

  clear p_return.

  sy-msgty = p_msgty.
  sy-msgid = p_msgid.
  sy-msgno = p_msgno.
  sy-msgv1 = p_msgv1.
  sy-msgv2 = p_msgv2.
  sy-msgv3 = p_msgv3.
  sy-msgv4 = p_msgv4.

* This is the temporary solution over the 'I' being overriden.
  lv_msgty = sy-msgty.
  call function 'BALW_BAPIRETURN_GET1'
    exporting
      type       = sy-msgty
      cl         = sy-msgid
      number     = sy-msgno
      par1       = sy-msgv1
      par2       = sy-msgv2
      par3       = sy-msgv3
      par4       = sy-msgv4
    importing
      bapireturn = p_return.

  sy-msgty = lv_msgty.





endfunction.
