FUNCTION ZTR_GET_ACTUAL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS DEFAULT 'H201'
*"     REFERENCE(GJAHR) TYPE  GJAHR DEFAULT SY-DATUM(4)
*"     REFERENCE(MONTH) TYPE  MONTH DEFAULT SY-DATUM+4(2)
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

  CONCATENATE GJAHR MONTH '01' INTO R_BUDAT-LOW.

*// LAST_DAY_OF_MONTHS.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = R_BUDAT-LOW
    IMPORTING
      LAST_DAY_OF_MONTH = R_BUDAT-HIGH.

  R_BUDAT(3) = 'IBT'.
  APPEND R_BUDAT.
  CLEAR  R_BUDAT.

  PERFORM SELECT_FAGLFLEXA_DATA TABLES ACTUAL
                                       DATE
                                 USING BUKRS GJAHR.


ENDFUNCTION.
