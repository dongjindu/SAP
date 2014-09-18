FUNCTION Z_MM_IF_OB_03_001_DB .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      T_HEAD STRUCTURE  ZMMS0025
*"      T_ITEM STRUCTURE  ZMMS0026
*"----------------------------------------------------------------------

*  CONCATENATE 'KMMG_EDI_CLNT' sy-mandt
*              INTO g_dest.
** furong on 09/14/12
  data: L_MSGTXT(127).
  g_dest = 'WEBMETHODS'.
** End on 09/14/12.

  CHECK NOT t_item[] IS INITIAL.

  CALL FUNCTION 'Z_MM_IF_OB_03_001'
    DESTINATION g_dest
    IMPORTING
      e_return              = e_return
    TABLES
      t_head                = t_head
      t_item                = t_item
    EXCEPTIONS

      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT
      resource_failure      = 3
      OTHERS                = 4 .

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

  LOOP AT t_item.

    READ TABLE t_head WITH KEY pos = t_item-pos.

    gv_return =  t_head-if_return.

    CALL METHOD zmmc_cl_if=>if_set_key(  ifkey = 'MMIF301_ECC_OB'
                             modle = 'IDOC'       " 'MM', 'SD', 'FI'
                             centy = 'US'
                             dirct = 'O'        " 'O' : Outbound,
                                                " 'I' : Inbound
                             logox = ' '
                             ttype = 'S'
                             cparm = '9'
                           ).

    CALL METHOD zmmc_cl_if=>if_set_messg( type    = t_head-if_return
                              id      = ' '    "gt_retmsg-id
                              message = t_item-message
                            ).


    CALL METHOD zmmc_cl_if=>if_set_param( istat = gv_return
                              ifp01 = t_item-pos
                              ifp02 = t_item-docnum
                              ifp03 = t_item-lifex
                              ifp04 = t_item-rcode
                              ifp05 = t_item-msg_seq
                              ifp06 = t_item-message
                              ifp07 = t_item-message+1(10)
                              ifp08 = t_head-824
                              ifp09 = t_item-lifex+1(4)
                            ).
    CALL METHOD zmmc_cl_if=>if_save_data( ).
  ENDLOOP.


ENDFUNCTION.
