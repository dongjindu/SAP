FUNCTION ZIM_GET_ZTIMIMG01.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BSART) LIKE  EKKO-BSART DEFAULT 'NB'
*"     VALUE(BSTYP) LIKE  EKKO-BSTYP DEFAULT 'F'
*"     VALUE(ZTERM) LIKE  EKKO-ZTERM
*"  EXPORTING
*"     VALUE(W_ZFREQTY) LIKE  ZTREQHD-ZFREQTY
*"     VALUE(W_ZTIMIMG01) LIKE  ZTIMIMG01 STRUCTURE  ZTIMIMG01
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_OK
*"----------------------------------------------------------------------
  W_FOUND_CHECK = 'N'.
  CLEAR : W_ZFREQTY, ZTIMIMG01.
  REFRESH : IT_ZTIMIMG01.

  SELECT  * INTO TABLE IT_ZTIMIMG01 FROM   ZTIMIMG01
*                                    WHERE  LOEKZ    NE 'X'
                                    WHERE  BSART    EQ BSART
                                    AND    BSTYP    EQ BSTYP
                                    AND    ZTERM    EQ ZTERM
                                    AND    ZFAPLDT  <= SY-DATUM
                               ORDER BY ZFAPLDT DESCENDING.
  IF SY-SUBRC NE 0.
     RAISE NOT_FOUND.
  ENDIF.

* LOOP AT IT_ZTIMIMG01 WHERE KTOKK EQ KTOKK.
  LOOP AT IT_ZTIMIMG01.

     IF ZTERM EQ IT_ZTIMIMG01-ZTERM     " PAYMENT TERMS이 일치
        OR IT_ZTIMIMG01-ZTERM IS INITIAL.    " or 전체일 경우.
        W_FOUND_CHECK = 'Y'.
        W_ZFREQTY = IT_ZTIMIMG01-ZFREQTY.
        MOVE-CORRESPONDING IT_ZTIMIMG01  TO  W_ZTIMIMG01.
        EXIT.
     ENDIF.
  ENDLOOP.

  IF SY-SUBRC NE 0 OR W_FOUND_CHECK = 'N'.
     RAISE NOT_OK.
  ENDIF.
ENDFUNCTION.
