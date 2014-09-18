FUNCTION z_mm_if_ob_02_006_re.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0048
*"----------------------------------------------------------------------

  CLEAR : it_m048, it_m048[].

  g_dest = 'WMPM01'.

  LOOP AT it_body.
    MOVE-CORRESPONDING it_body TO it_m048.
    it_m048-type = ' '.
    APPEND it_m048. CLEAR it_m048.
  ENDLOOP.

  CHECK NOT it_m048[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_006'
    DESTINATION g_dest
    IMPORTING
      e_return              = e_return
    TABLES
      it_body               = it_m048
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      resource_failure      = 3
      OTHERS                = 4.

  CASE sy-subrc.
    WHEN '1'.
      e_return-message = 'communication_failure'.
      e_return-type    = 'E'.
    WHEN '2'.
      e_return-message = 'system_failure'.
      e_return-type    = 'E'.
    WHEN '3'.
      e_return-message = 'resource_failure'.
      e_return-type    = 'E'.
    WHEN '4'.
      e_return-message = 'Others'.
      e_return-type    = 'E'.
  ENDCASE.

  it_m048-type     = e_return-type.
  it_m048-etdat    = sy-datum.
  it_m048-ettim    = sy-uzeit.
  it_m048-etnam    = sy-uname.
  it_m048-message  = e_return-message.
  MODIFY it_m048 TRANSPORTING type etdat
                              ettim etnam message
                        WHERE type = ' '.

  MODIFY zmmt0048 FROM TABLE it_m048.

*  COMMIT WORK.

*-- Error Log -----------------------------------

  LOOP AT it_m048.
    gv_return =  it_m048-type.

    CALL METHOD zmmc_cl_if=>if_set_key(  ifkey = 'MMIF206_ECC_OB'
                             modle = 'GCS'       " 'MM', 'SD', 'FI'
                             centy = 'US'       "
                             dirct = 'O'
                             logox = ' '
                             ttype = 'S'
                             cparm = '7'
                           ).

    CALL METHOD zmmc_cl_if=>if_set_messg( type    = it_m048-type
                              id      = ' '    "gt_retmsg-id
                              message = it_m048-message
                            ).

    CALL METHOD zmmc_cl_if=>if_set_param( istat = gv_return
                              ifp01 = it_m048-mblnr
                              ifp02 = it_m048-vbeln
                              ifp03 = it_m048-lifnr
                              ifp04 = it_m048-exidv
                              ifp05 = it_m048-lgort
                              ifp06 = it_m048-matnr
                              ifp07 = it_m048-zflag
                            ).
    CALL METHOD zmmc_cl_if=>if_save_data( ).
  ENDLOOP.


ENDFUNCTION.
