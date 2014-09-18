FUNCTION Z_MM_IF_IB_02_002.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0031 OPTIONAL
*"----------------------------------------------------------------------


 DATA : l_mtyp  TYPE bwart,
         l_return LIKE zmms0053,
         ls_body  LIKE zmmt0031,
         l_vbeln LIKE likp-vbeln.

  CLEAR : gt_body_31,
          gt_body_31[].

  IF it_body[] IS INITIAL.
    e_return-type    = 'E'.
    e_return-message = text-m01.
    EXIT.
  ENDIF.

  gt_body_31[] = it_body[].

  LOOP AT gt_body_31.
    g_tabix = sy-tabix.
    CLEAR : l_vbeln, l_return.
    PERFORM read_likp_data  USING ' '
                                  gt_body_31-transp_id
                         CHANGING l_vbeln.

    MOVE-CORRESPONDING gt_body_31 TO ls_body.

    PERFORM post_vl32n_data_tr USING l_vbeln
                                     ls_body
                           CHANGING l_return.

    gt_body_31-vbeln   = l_vbeln.
    gt_body_31-type    = l_return-type.
    gt_body_31-message = l_return-message.
    MODIFY gt_body_31 INDEX g_tabix.
  ENDLOOP.


**** LOGIC
  LOOP AT gt_body_31.
    g_tabix = sy-tabix.

    g_return =  e_return-type = gt_body_31-type.
    gt_body_31-etnam = sy-uname.
    gt_body_31-etdat = sy-datum.
    gt_body_31-ettim = sy-uzeit.
    e_return-message = gt_body_31-message.

    CALL METHOD zmmc_cl_if=>if_set_key(   ifkey = 'MMIF202_ECC_IB'
                              modle = 'GCS'       " 'MM', 'SD', 'FI'
                              centy = 'US'
                              dirct = 'I'  "'O': Outbound, 'I':nbound
                              logox = ' '
                              ttype = 'S'
                              cparm = '3'
                           ).

    CALL METHOD zmmc_cl_if=>if_set_messg( type    = e_return-type
                              id      = ' '    "gt_retmsg-id
                              message = e_return-message
                            ).

     CALL METHOD zmmc_cl_if=>if_set_param( istat = g_return
                              ifp01 = gt_body_31-transp_id
                              ifp02 = gt_body_31-route
                              ifp03 = gt_body_31-vendor
                              ifp04 = gt_body_31-vbeln
                            ).
     CALL METHOD zmmc_cl_if=>if_save_data( ).

    MODIFY gt_body_31 INDEX g_tabix.
  ENDLOOP.

  MODIFY zmmt0031 FROM TABLE gt_body_31.

  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  CLEAR : it_body, it_body[].

  it_body[] = gt_body_31[].



ENDFUNCTION.
