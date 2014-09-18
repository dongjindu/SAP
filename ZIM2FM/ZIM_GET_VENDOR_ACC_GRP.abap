FUNCTION ZIM_GET_VENDOR_ACC_GRP.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(KTOKK) LIKE  LFA1-KTOKK
*"  EXPORTING
*"     VALUE(TXT30) LIKE  V_T077K-TXT30
*"  EXCEPTIONS
*"      KEY_INCOMPLETE
*"      NOT_FOUND
*"      NOT_FOUND_TEXT
*"----------------------------------------------------------------------
   CLEAR : TXT30, T077Y.

   IF KTOKK IS INITIAL
        OR KTOKK LE SPACE.
     RAISE KEY_INCOMPLETE.
   ENDIF.

   SELECT SINGLE * FROM T077K WHERE KTOKK EQ KTOKK.
   IF SY-SUBRC NE 0.
      RAISE NOT_FOUND.
   ENDIF.

   SELECT SINGLE * FROM T077Y WHERE KTOKK EQ KTOKK
                              AND   SPRAS EQ SY-LANGU.

   IF SY-SUBRC NE 0.
      RAISE NOT_FOUND_TEXT.
   ENDIF.

   TXT30 = T077Y-TXT30.

ENDFUNCTION.
