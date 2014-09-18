FUNCTION z_fmm_6003_04.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EX_DZ707SEQ) LIKE  ZSMM_6003_01-DZ707SEQ
*"----------------------------------------------------------------------

  SELECT SINGLE MAX( dz707seq ) INTO ex_dz707seq
    FROM ztmm_6003_01.

  IF sy-subrc <> 0.
    ex_dz707seq = 0.
  ENDIF.

ENDFUNCTION.
