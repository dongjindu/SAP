FUNCTION z_fmm_dms.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LGBZO) LIKE  EKPO-LGBZO
*"     VALUE(T_FR_UZEIT) LIKE  ZTMM_DMS-UZEIT OPTIONAL
*"     VALUE(T_TO_UZEIT) LIKE  ZTMM_DMS-UZEIT OPTIONAL
*"  EXPORTING
*"     VALUE(O_FLAG) TYPE  ZRESULT
*"     VALUE(O_MSG) TYPE  ZMSG
*"  TABLES
*"      T_DMS STRUCTURE  ZTMM_DMS
*"----------------------------------------------------------------------

  SELECT * INTO TABLE t_dms
  FROM ztmm_dms
  WHERE lgbzo = i_lgbzo
   and jit_indicator = 'G'
   AND uzeit BETWEEN t_fr_uzeit AND t_to_uzeit .
  IF sy-subrc = 0.
    o_flag = 'S'.
    o_msg = 'Successful'.
  ELSE.
    o_flag = 'E'.
    o_msg = 'No Data'.
  ENDIF.
ENDFUNCTION.
