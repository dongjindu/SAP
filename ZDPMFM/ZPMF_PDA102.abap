FUNCTION ZPMF_PDA102.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SY-SUBRC
*"  TABLES
*"      T102 STRUCTURE  ZSPM_PDA102
*"      T102R STRUCTURE  ZSPM_PDA102R
*"----------------------------------------------------------------------

  DATA : IT_EKPO LIKE TABLE OF EKPO WITH HEADER LINE.
  DATA : L_WEMNG LIKE EKET-WEMNG.

  READ TABLE T102 INDEX 1.

  PERFORM CONVERSION_EXIT_ALPHA_INPUT USING T102-EBELN T102-EBELN.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_EKPO
           FROM EKPO
          WHERE EBELN EQ T102-EBELN
            AND ELIKZ = ' '
            and LOEKZ = ' '.

  LOOP AT IT_EKPO.
    MOVE-CORRESPONDING IT_EKPO TO T102R.

    SELECT SINGLE
           WEMNG INTO L_WEMNG
           FROM EKET
          WHERE EBELN EQ IT_EKPO-EBELN
          AND   EBELP EQ IT_EKPO-EBELP.

    T102R-MENGE = IT_EKPO-MENGE - L_WEMNG.
    IF T102R-MENGE < 0.
      T102R-MENGE = 0.
    ENDIF.

    PERFORM CONVERSION_EXIT_ALPHA_OUTPUT USING T102R-MATNR T102R-MATNR.
    PERFORM CONVERSION_EXIT_CUNIT_OUTPUT USING T102R-MEINS T102R-MEINS.

    APPEND T102R. CLEAR T102R.
  ENDLOOP.

  DESCRIBE TABLE T102R LINES SY-INDEX.
  IF SY-INDEX = 0.
    SUBRC = 4.
  ELSE.
    SUBRC = 0.
  ENDIF.

  SORT T102R.

ENDFUNCTION.