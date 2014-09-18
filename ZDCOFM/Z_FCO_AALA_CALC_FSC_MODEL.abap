FUNCTION Z_FCO_AALA_CALC_FSC_MODEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_MAKRT) LIKE  ZTCO_AALA_MODEL-MAKRT
*"     REFERENCE(I_BDATJ) LIKE  ZTCO_AALA_MODEL-BDATJ
*"     REFERENCE(I_POPER) LIKE  ZTCO_AALA_MODEL-POPER
*"     REFERENCE(I_ZVER_DES) LIKE  ZTCO_AALA_MODEL-ZVER_DES
*"  TABLES
*"      T_FSC_QTY STRUCTURE  ZTCO_AALA_CREATION_9000_TAB
*"      T_MIP STRUCTURE  ZTCO_AALA_MIP
*"      T_FSC STRUCTURE  ZTCO_AALA_FSC
*"      T_MODEL STRUCTURE  ZTCO_AALA_MODEL
*"  EXCEPTIONS
*"      QUANTITY_ERROR
*"      PROGRAM_ERROR
*"----------------------------------------------------------------------
  clear: w_makrt, w_bdatj, w_poper, w_zver_des.
  refresh: it_fsc_qty, it_mip, it_fsc, it_model,
           it_origin_fsc, it_origin_model,
           it_aala.

  move: i_makrt    to w_makrt,
        i_bdatj    to w_bdatj,
        i_poper    to w_poper,
        i_zver_des to w_zver_des.

  it_fsc_qty[] = t_fsc_qty[].
  it_mip[]     = t_mip[].

  perform set_et_fsc.

  perform set_et_model.

  t_fsc[]   = it_fsc[].
  t_model[] = it_model[].
ENDFUNCTION.
