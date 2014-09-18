FUNCTION z_ppc1tp_read_rp.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_RP16) TYPE  PPC_REPPOINT_INT OPTIONAL
*"     REFERENCE(I_RP32) TYPE  PPC_REPPOINT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_SORTP) TYPE  SORTP
*"     REFERENCE(E_RPEXT) TYPE  PPC_REPPOINT_EXT
*"----------------------------------------------------------------------

* read RP
  DATA: lv_ppc_rp   TYPE ppc_rp,
        lv_reppoint TYPE ppc_reppoint_int.

  IF i_rp32 NE space.
    lv_reppoint = i_rp32.
  ELSE.
    lv_reppoint = i_rp16.
  ENDIF.

  CALL FUNCTION 'PPC1DC_RP_READ'
    EXPORTING
      if_reppoint = lv_reppoint
    IMPORTING
      ef_ppc_rp   = lv_ppc_rp
    EXCEPTIONS
      not_found   = 1.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* return RP external
  e_sortp  = lv_ppc_rp-reppoint_ext. "ls_sortb.
  e_rpext  = lv_ppc_rp-reppoint_ext.

ENDFUNCTION.
