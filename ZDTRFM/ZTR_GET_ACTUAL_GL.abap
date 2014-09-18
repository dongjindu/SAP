FUNCTION ZTR_GET_ACTUAL_GL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS DEFAULT 'H201'
*"     REFERENCE(GJAHR) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      DATE
*"      ACTUAL STRUCTURE  FDSR
*"      BELNR STRUCTURE  FCO_BELNR_S OPTIONAL
*"----------------------------------------------------------------------

  REFRESH : ACTUAL, R_BELNR.
  CLEAR   : ACTUAL, R_BELNR.


  IF BELNR[] IS NOT INITIAL.
    LOOP AT BELNR.
      R_BELNR-SIGN = 'I'.
      R_BELNR-OPTION = 'EQ'.
      R_BELNR-LOW    = BELNR.
      APPEND R_BELNR.

    ENDLOOP.

  ENDIF.


  REFRESH R_BUDAT.
  R_BUDAT[] = DATE[].

  PERFORM SELECT_FAGLFLEXA_GL_DATA TABLES ACTUAL
                                    USING BUKRS.


ENDFUNCTION.
