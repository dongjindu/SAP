FUNCTION ZIM_GET_ZTIMIMG08_RECORD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(W_ZFCDTY) LIKE  ZTIMIMG08-ZFCDTY
*"     VALUE(W_ZFCD) LIKE  ZSIMIMG08-ZFCD
*"  EXPORTING
*"     VALUE(W_ZTIMIMG08) LIKE  ZTIMIMG08 STRUCTURE  ZTIMIMG08
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_INPUT
*"----------------------------------------------------------------------
  CLEAR : W_ZTIMIMG08.

  IF W_ZFCDTY IS INITIAL.
     RAISE NOT_INPUT.
  ENDIF.

  IF W_ZFCD IS INITIAL.
     RAISE NOT_INPUT.
  ENDIF.

  SELECT SINGLE * INTO   W_ZTIMIMG08 FROM ZTIMIMG08
                  WHERE  ZFCDTY    EQ   W_ZFCDTY
                  AND    ZFCD      EQ   W_ZFCD.

  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

ENDFUNCTION.
