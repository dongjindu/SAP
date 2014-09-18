FUNCTION Z_FPM_TIME_CONFIRMATION2.
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
*"     VALUE(IDAUR) TYPE  IDAUR
*"     VALUE(IDAUE) TYPE  IDAUE
*"     VALUE(VORNR) TYPE  VORNR
*"     VALUE(ZSPM_TIME_CONF) LIKE  ZSPM_TIME_CONF STRUCTURE
*"        ZSPM_TIME_CONF
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  DATA: WA_BUDAT(10),
        WA_ISDD(10),
        WA_IEDD(10),
        WA_IDAUR(7).

  WRITE : IDAUR TO WA_IDAUR.
  WRITE : ZSPM_TIME_CONF-IEDD  TO WA_BUDAT MM/DD/YYYY,
          ZSPM_TIME_CONF-ISDD  TO WA_ISDD MM/DD/YYYY,
          ZSPM_TIME_CONF-IEDD  TO WA_IEDD MM/DD/YYYY.


  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPLCORU' '3000'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=ENTR'.
  PERFORM BDC_FIELD       USING 'CORUF-AUFNR'
                                AUFNR.
  PERFORM BDC_FIELD       USING 'CORUF-VORNR'
                                VORNR.

  PERFORM BDC_DYNPRO      USING 'SAPLCORU' '3200'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.
*  PERFORM BDC_FIELD       USING 'AFRUD-ARBPL'
*                                ZSPM_TIME_CONF-ARBPL.
*  PERFORM BDC_FIELD       USING 'AFRUD-WERKS'
*                                ZSPM_TIME_CONF-WERKS.
*  PERFORM BDC_FIELD       USING 'AFRUD-ISMNU'
*                                ZSPM_TIME_CONF-ISMNU.

  PERFORM BDC_FIELD       USING 'AFRUD-BUDAT'
                                WA_IEDD.

*  PERFORM BDC_FIELD       USING 'AFRUD-AUERU'
*                                ZSPM_TIME_CONF-AUERU.
*  PERFORM BDC_FIELD       USING 'AFRUD-LEKNW'
*                                ZSPM_TIME_CONF-LEKNW.
*  PERFORM BDC_FIELD       USING 'AFRUD-OFMNU'
*                                ZSPM_TIME_CONF-OFMNU.

  PERFORM BDC_FIELD       USING 'AFRUD-ISDD'
                                WA_ISDD.

  PERFORM BDC_FIELD       USING 'AFRUD-ISDZ'
                                ZSPM_TIME_CONF-ISDZ.

  PERFORM BDC_FIELD       USING 'AFRUD-IDAUR'
                                WA_IDAUR.

  PERFORM BDC_FIELD       USING 'AFRUD-IDAUE'
                                IDAUE.

  PERFORM BDC_FIELD       USING 'AFRUD-IEDD'
                                WA_IEDD.

  PERFORM BDC_FIELD       USING 'AFRUD-IEDZ'
                                ZSPM_TIME_CONF-IEDZ.

*  PERFORM BDC_FIELD       USING 'AFRUD-PEDZ'
*                                ZSPM_TIME_CONF-PEDZ.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'IW41'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.





ENDFUNCTION.
