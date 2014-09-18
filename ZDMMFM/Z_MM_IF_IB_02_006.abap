FUNCTION z_mm_if_ib_02_006.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0037 OPTIONAL
*"----------------------------------------------------------------------

  DATA : l_mtyp    TYPE bwart,
         l_body_37 LIKE zmmt0037,
         l_return  LIKE zmms0053.

  CLEAR : gt_body_37,
          gt_body_37[].

  IF it_body[] IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = text-m01.
    EXIT.
  ENDIF.

  gt_body_37[] = it_body[].

  PERFORM bapi_clear.

  LOOP AT gt_body_37.
    g_tabix = sy-tabix.
    MOVE-CORRESPONDING  gt_body_37 TO l_body_37.

    PERFORM update_mm02_data  USING l_body_37
                           CHANGING l_return.

    gt_body_37-type    = l_return-type.
    gt_body_37-message = l_return-message.
    MODIFY gt_body_37 INDEX g_tabix
                      TRANSPORTING type
                                   message.
  ENDLOOP.

**** LOGIC
  LOOP AT gt_body_37.
    g_tabix = sy-tabix.

    g_return =  e_return-type = gt_body_37-type.
    gt_body_37-etnam = sy-uname.
    gt_body_37-etdat = sy-datum.
    gt_body_37-ettim = sy-uzeit.
    e_return-message = gt_body_37-message.

    call method zmmc_cl_if=>if_set_key(   ifkey = 'MMIF206_ECC_IB'
                              modle = 'GCS'       " 'MM', 'SD', 'FI'
                              centy = 'US'
                              dirct = 'I'  "'O' - Outbound, 'I'- Inbound
                              logox = ' '
                              ttype = 'S'
                              cparm = '3'
                           ).

    call method zmmc_cl_if=>if_set_messg( type    = e_return-type
                              id      = ' '    "gt_retmsg-id
                              message = e_return-message
                            ).

    call method zmmc_cl_if=>if_set_param( istat = g_return
                              ifp01 = gt_body_37-matnr
                              ifp02 = gt_body_37-werks
                              ifp03 = gt_body_37-lgort
                              ifp04 = gt_body_37-lgpbe
                              ifp05 = gt_body_37-lminb
                              ifp06 = gt_body_37-meins
                            ).
    call method zmmc_cl_if=>if_save_data( ).

    MODIFY gt_body_37 INDEX g_tabix.
  ENDLOOP.

  MODIFY zmmt0037 FROM TABLE gt_body_37.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CLEAR : it_body, it_body[].

  it_body[] = gt_body_37[].


ENDFUNCTION.
