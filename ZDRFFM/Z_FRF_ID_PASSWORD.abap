FUNCTION Z_FRF_ID_PASSWORD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_PERNR) LIKE  ZTRF_ID_PASS-PERNR OPTIONAL
*"     VALUE(I_PASSW) LIKE  ZTRF_ID_PASS-PASSW OPTIONAL
*"     VALUE(N_PASSW) LIKE  ZTRF_ID_PASS-PASSW OPTIONAL
*"  EXPORTING
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(E_MODULE) TYPE  ZRF_MODU
*"     VALUE(E_GROUP) TYPE  LVS_REFNR
*"     VALUE(E_DATUM) TYPE  CHAR8
*"     VALUE(E_UZEIT) TYPE  CHAR6
*"----------------------------------------------------------------------
  DATA: BEGIN OF LA_MM ,
          PERNR TYPE ZTRF_ID_PASS-PERNR,
          PASSW TYPE ZTRF_ID_PASS-PASSW,
          REFNR TYPE ZTRF_ID_PASS-REFNR,
          MODULE(2),
        END OF LA_MM.
  DATA: BEGIN OF LA_PM,
          PERNR TYPE ZTRF_RECIPIENT-PERNR,
          PASSW TYPE ZTRF_RECIPIENT-PASSW,
          WEMPF TYPE ZTRF_RECIPIENT-WEMPF,
          MODULE(2),
        END OF LA_PM.


  IF NOT I_PERNR IS INITIAL.

    SELECT SINGLE *
                  FROM ZTRF_ID_PASS
                  WHERE PERNR EQ I_PERNR
**INSERTED BY FURONG *******88
                  AND   PASSW EQ I_PASSW.
** END OF INSERT **********
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING ZTRF_ID_PASS TO LA_MM.
      LA_MM-MODULE = 'MM'.
      E_MODULE = 'MM'.
      E_GROUP = ZTRF_ID_PASS-REFNR.
      E_DATUM = SY-DATUM.
      E_UZEIT = SY-UZEIT.
      E_MESS  = TEXT-M14.  "'Login OK'.
      ZRESULT = TEXT-M03.
      IF NOT N_PASSW IS INITIAL.
        UPDATE ZTRF_ID_PASS SET:  PASSW = N_PASSW
                            WHERE PERNR = ZTRF_ID_PASS-PERNR.
        IF SY-SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.

    ELSE.
      SELECT SINGLE *
             FROM ZTRF_RECIPIENT
             WHERE PERNR EQ I_PERNR
             AND   PASSW EQ I_PASSW.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING ZTRF_RECIPIENT TO LA_PM.
        LA_PM-MODULE = 'PM'.

        E_MODULE = 'PM'.
        E_DATUM = SY-DATUM.
        E_UZEIT = SY-UZEIT.

        E_MESS  = TEXT-M14.  "'Login OK'.
        ZRESULT = TEXT-M03.
      ELSE.
        ZRESULT = TEXT-M04.
        E_MESS  = TEXT-M15.
        "'Name or password is incorrect. Please re-enter'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
