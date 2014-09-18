FUNCTION Z_EAI_DELAY_BY_SEC .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(WAIT_SEC) TYPE  NUMC10 DEFAULT 10
*"----------------------------------------------------------------------

  WAIT UP TO wait_sec SECONDS.

ENDFUNCTION.
