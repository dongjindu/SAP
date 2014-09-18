FUNCTION ZIM_GET_TERM_OF_PAYMENT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(ZTERM) LIKE  ZTIMIMG01-ZTERM
*"       EXPORTING
*"             VALUE(TEXT1) LIKE  V_T052-TEXT1
*"       EXCEPTIONS
*"              KEY_INCOMPLETE
*"              NOT_FOUND
*"              NOT_FOUND_TEXT
*"----------------------------------------------------------------------
  CLEAR : V_T052, T052, T052U, TVZBT.

  IF ZTERM IS INITIAL OR ZTERM LE SPACE.
     RAISE KEY_INCOMPLETE.
  ENDIF.

  SELECT * FROM T052 WHERE ZTERM EQ ZTERM.
     EXIT.
  ENDSELECT.

  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

* SELECT SINGLE *  FROM TVZBT WHERE ZTERM EQ ZTERM
*                             AND   SPRAS EQ SY-LANGU.

* IF SY-SUBRC NE 0.
*    RAISE NOT_FOUND_TEXT.
* ENDIF.

* TEXT1 = TVZBT-VTEXT.

ENDFUNCTION.
