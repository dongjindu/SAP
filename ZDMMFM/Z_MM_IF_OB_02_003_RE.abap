FUNCTION z_mm_if_ob_02_003_re.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0038
*"----------------------------------------------------------------------


  CLEAR : it_m038, it_m038[].

*  CONCATENATE 'KMMG_EAI_CLNT' sy-mandt
*              INTO g_dest.
  G_DEST = 'WMPM01'.

  LOOP AT it_body.
    MOVE-CORRESPONDING it_body TO it_m038.
    it_m038-type = ' '.
    APPEND it_m038. CLEAR : it_m038.
  ENDLOOP.

  CHECK NOT it_m038[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_02_003'
    DESTINATION g_dest
    IMPORTING
      e_return              = e_return
    TABLES
      it_body               = it_m038
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

  it_m038-type     = e_return-type.
**S__PAUL 06/07/11
  it_m038-Atdat    = sy-datum.
  it_m038-Attim    = sy-uzeit.
  it_m038-Atnam    = sy-uname.
**  E
  it_m038-message  = e_return-message.
  MODIFY it_m038 TRANSPORTING type Atdat Attim Atnam message
                        WHERE type = ' '.

  MODIFY zmmt0038 FROM TABLE it_m038.

*  COMMIT WORK.

*-- Error Log -----------------------------------

  LOOP AT it_m038.

    gv_return =  it_m038-type.

    CALL METHOD zmmc_cl_if=>if_set_key(  ifkey = 'MMIF203_ECC_OB'
                             modle = 'GCS'       " 'MM', 'SD', 'FI'
                             centy = 'US'       "
                             dirct = 'O'        " 'O' : Outbound,
                                                " 'I' : Inbound
                             logox = ' '
                             ttype = 'S'
                             cparm = '8'
                           ).

    CALL METHOD zmmc_cl_if=>if_set_messg( type    = it_m038-type
                              id      = ' '    "gt_retmsg-id
                              message = it_m038-message
                            ).

    CALL METHOD zmmc_cl_if=>if_set_param( istat = gv_return
                              ifp01 = it_m038-pkkey
                              ifp02 = it_m038-rsnum
                              ifp03 = it_m038-rspos
                              ifp04 = it_m038-reversed
                              ifp05 = it_m038-saedt
                              ifp06 = it_m038-saeuz
                              ifp07 = it_m038-prvbe
                              ifp08 = it_m038-matnr
                            ).
    CALL METHOD zmmc_cl_if=>if_save_data( ).
  ENDLOOP.


ENDFUNCTION.
