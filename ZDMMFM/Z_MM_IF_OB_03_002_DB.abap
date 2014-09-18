FUNCTION z_mm_if_ob_03_002_db.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      T_IDOC STRUCTURE  ZMMS0024
*"----------------------------------------------------------------------

*-< added by Victor 03/30/2010
  DATA : int_edids LIKE TABLE OF edids WITH HEADER LINE,
         int_edidd LIKE TABLE OF edidd WITH HEADER LINE.
  DATA : l_e1edt13 TYPE e1edt13.
*->

  g_dest = 'WMPM01'.

  CHECK NOT t_idoc[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_03_002'
    DESTINATION g_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_idoc                = t_idoc
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

*-- Error Log -----------------------------------

  LOOP AT t_idoc.

    CLEAR : int_edids[], int_edidd[] , l_e1edt13.

    gv_return =  t_idoc-if_return.

    call method zmmc_cl_if=>if_set_key(  ifkey = 'MMIF302_ECC_OB'
                             modle = 'IDOC'       " 'MM', 'SD', 'FI'
                             centy = 'US'
                             dirct = 'O' "'O':Outbound, 'I':Inbound
                             logox = ' '
                             ttype = 'S'
                             cparm = '11'
                           ).

    call method  zmmc_cl_if=>if_set_messg( type    = t_idoc-if_return
                              id      = ' '    "gt_retmsg-id
                              message = t_idoc-message
                            ).

*-< added by Victor 03/30/2010
    CALL FUNCTION 'IDOC_READ_COMPLETELY'
      EXPORTING
        document_number         = t_idoc-docnum
      TABLES
        int_edids               = int_edids
        int_edidd               = int_edidd
      EXCEPTIONS
        document_not_exist      = 1
        document_number_invalid = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      LOOP AT int_edidd WHERE segnam    = 'E1EDT13'
                          AND sdata(3)  = '007'.
        CLEAR : l_e1edt13.
        MOVE int_edidd-sdata TO l_e1edt13.
        "ETA date : l_e1edt13-NTANF
      ENDLOOP.
    ENDIF.

*->

    call method  zmmc_cl_if=>if_set_param( istat = gv_return
                              ifp01 = t_idoc-docnum
                              ifp02 = t_idoc-arckey
                              ifp03 = t_idoc-refmes
                              ifp04 = t_idoc-partner_idlf
                              ifp05 = t_idoc-partner_idwe
                              ifp06 = t_idoc-name1_we
                              ifp07 = t_idoc-lifex
                              ifp08 = t_idoc-traid
                              ifp09 = t_idoc-rcode
                              ifp10 = t_idoc-message
                              ifp11 = t_idoc-if_return
                              ifp12 = l_e1edt13-ntanf  "added by Victor
** 03/30/2010
                            ).
    call method  zmmc_cl_if=>if_save_data( ).
  ENDLOOP.


ENDFUNCTION.
