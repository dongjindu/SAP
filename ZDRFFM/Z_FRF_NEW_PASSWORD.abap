FUNCTION Z_FRF_NEW_PASSWORD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_PERNR) LIKE  ZTRF_ID_PASS-PERNR
*"     VALUE(I_PASSW) LIKE  ZTRF_ID_PASS-PASSW OPTIONAL
*"     VALUE(N_PASSW) LIKE  ZTRF_ID_PASS-PASSW
*"  EXPORTING
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"----------------------------------------------------------------------


  SELECT SINGLE *
         FROM ZTRF_RECIPIENT
         WHERE PERNR EQ I_PERNR
         AND   PASSW EQ I_PASSW.
  IF SY-SUBRC EQ 0.
    UPDATE ZTRF_RECIPIENT SET:  PASSW = N_PASSW
                          WHERE PERNR = I_PERNR.
    IF SY-SUBRC EQ 0.
      ZRESULT = TEXT-M03.
      E_MESS  = TEXT-M19.
      COMMIT WORK.
    ELSE.
      ZRESULT = TEXT-M04.
      E_MESS  = TEXT-M20.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFUNCTION.
