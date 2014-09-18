FUNCTION Z_FPM_COMPLETE_ORDER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE OPTIONAL
*"     VALUE(AUFNR) TYPE  AUFNR
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '0101'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM BDC_FIELD       USING 'CAUFVD-AUFNR'
                                AUFNR.
  PERFORM BDC_DYNPRO      USING 'SAPLCOIH' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ARCH'.
  PERFORM BDC_DYNPRO      USING 'SAPLIWO1' '0200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=WTER'.
  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'IW32'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
