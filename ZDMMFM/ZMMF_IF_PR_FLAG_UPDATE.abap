FUNCTION ZMMF_IF_PR_FLAG_UPDATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_BANFN) TYPE  BANFN
*"     VALUE(I_BNFPO) TYPE  BNFPO
*"----------------------------------------------------------------------


  UPDATE EBAN SET ZZTYPE = SPACE
            WHERE BANFN  = I_banfn
              AND BNFPO  = I_bnfpo.
  commit work and wait.
  if sy-subrc = 0.

  endif.
ENDFUNCTION.
