FUNCTION ZFFI_DETERMINE_DOCTYPE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(TCODE) TYPE  BKPF-TCODE
*"  EXPORTING
*"     REFERENCE(AWTYP) TYPE  BKPF-AWTYP
*"----------------------------------------------------------------------

  RANGES:r_tcodel FOR bkpf-tcode OCCURS 0.           "..Loan????
  RANGES:r_tcodet FOR bkpf-tcode OCCURS 0.           "..??????

  r_tcodet-sign   = 'I'.
  r_tcodet-option = 'EQ'.
  r_tcodet-low    = 'TBB1'. APPEND r_tcodet. "????
  r_tcodet-low    = 'TBB2'. APPEND r_tcodet. "???
  r_tcodet-low    = 'TBB3'. APPEND r_tcodet. "?????
* R_TCODET-LOW    = 'FB05'. APPEND R_TCODET.
  r_tcodet-low    = 'FWSO'. APPEND r_tcodet. "???? ????


* Loan Transaction...
  r_tcodel-sign   = 'I'.
  r_tcodel-option = 'EQ'.
  r_tcodel-low    = 'FNM1'. APPEND r_tcodel. "????
* R_TCODEL-LOW    = 'FNM2'. APPEND R_TCODEL."BS??
  r_tcodel-low    = 'FNM3'. APPEND r_tcodel. "???
  r_tcodel-low    = 'FN5V'. APPEND r_tcodel. "????
  r_tcodel-low    = 'FN8A'. APPEND r_tcodel. "?????
  r_tcodel-low    = 'FN8B'. APPEND r_tcodel. "?????
  r_tcodel-low    = 'FN8C'. APPEND r_tcodel. "????

  CLEAR awtyp.
  IF tcode IN r_tcodel.
    awtyp = 'LOANS'.
  ENDIF.
  IF tcode IN r_tcodet.
    awtyp = 'TR-TM'.
  ENDIF.

ENDFUNCTION.
