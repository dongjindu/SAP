FUNCTION ZCATS_LOAD_CATSCELL_TO_CATSDB.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(I_STAGR) TYPE  STAGR
*"             REFERENCE(I_MSEHI) TYPE  MSEHI
*"             REFERENCE(I_WAERS) TYPE  WAERS
*"             REFERENCE(I_CATSCELL) TYPE  CATSCELL
*"       EXPORTING
*"             REFERENCE(E_CATSHOURS) TYPE  CATSHOURS
*"             REFERENCE(E_CATSAMOUNT) TYPE  CATSAMOUNT
*"             REFERENCE(E_CATSQUANTITY) TYPE  CATSNUMBER
*"----------------------------------------------------------------------

PERFORM load_catscell_to_catsdb
            USING
               i_stagr
               i_msehi
               i_waers
               i_catscell
            CHANGING
               e_catshours
               e_catsamount
               e_catsquantity.


ENDFUNCTION.
