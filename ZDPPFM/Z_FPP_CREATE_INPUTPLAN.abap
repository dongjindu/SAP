FUNCTION Z_FPP_CREATE_INPUTPLAN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_DEL) TYPE  CHAR01 DEFAULT 'X'
*"     VALUE(I_UPH) TYPE  NUM03 DEFAULT '063'
*"     VALUE(I_MITU) TYPE  CHAR01 OPTIONAL
*"     VALUE(I_FIFO) TYPE  CHAR01
*"     VALUE(I_MOD1) TYPE  CHAR04 OPTIONAL
*"     VALUE(I_MOD2) TYPE  CHAR04 OPTIONAL
*"     VALUE(I_MOD3) TYPE  CHAR04 OPTIONAL
*"     VALUE(I_RAT1) TYPE  ZNUM04 OPTIONAL
*"     VALUE(I_RAT2) TYPE  ZNUM04 OPTIONAL
*"     VALUE(I_RAT3) TYPE  ZNUM04 OPTIONAL
*"     VALUE(I_RP06TM) TYPE  NUM06 OPTIONAL
*"----------------------------------------------------------------------

  WA_UPH  = I_UPH.


  PERFORM AUTHORITY_CHECK .
  PERFORM CREATE_INPUTPLAN  using  i_del i_fifo
                             i_mod1 i_mod2 i_mod3
                             i_rat1 i_rat2 i_rat3
                             i_rp06tm.
ENDFUNCTION.
