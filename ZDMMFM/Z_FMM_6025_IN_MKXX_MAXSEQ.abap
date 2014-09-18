FUNCTION Z_FMM_6025_IN_MKXX_MAXSEQ.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(EX_DZ707SEQ) LIKE  ZTMM_6025_01-DZ707SEQ
*"----------------------------------------------------------------------

  SELECT MAX( dz707seq )
    INTO ex_dz707seq
    FROM ztmm_6003_01.

  IF sy-subrc <> 0.
    ex_dz707seq = 0.
  ENDIF.

ENDFUNCTION.
