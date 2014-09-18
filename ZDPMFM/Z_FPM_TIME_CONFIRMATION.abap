FUNCTION Z_FPM_TIME_CONFIRMATION.
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
*"     VALUE(BUDAT) TYPE  BUDAT
*"     VALUE(IDAUR) TYPE  IDAUR
*"     VALUE(IDAUE) TYPE  IDAUE
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      ZSPM_TIME_CONF STRUCTURE  ZSPM_TIME_CONF OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  DATA: WA_BUDAT(10),
        WA_ISDD(10),
        WA_IEDD(10),
        WA_IDAUR(7).


  WRITE :
          IDAUR TO WA_IDAUR.

**** Enter Order #.....
  PERFORM BDC_DYNPRO      USING 'SAPLCMFU' '0001'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=01_MKAL'.
  PERFORM BDC_FIELD       USING 'CMFUD-AUFNR'
                                AUFNR.
**** Select All
  PERFORM BDC_DYNPRO      USING 'SAPLCMFU' '0001'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '01_RLDT'.
**** Confirm ....
*  PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0300'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=YES'.
BREAK-POINT.
**** Change Item data....
  LOOP AT ZSPM_TIME_CONF.
    CLEAR: WA_ISDD, WA_ISDD.
    WRITE : ZSPM_TIME_CONF-IEDD  TO WA_BUDAT MM/DD/YYYY,
            ZSPM_TIME_CONF-ISDD  TO WA_ISDD MM/DD/YYYY,
            ZSPM_TIME_CONF-IEDD  TO WA_IEDD MM/DD/YYYY.

    PERFORM BDC_DYNPRO      USING 'SAPLCORU' '3390'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
*perform bdc_field       using 'AFRUD-ARBPL'
*                              WERKS_016.
*perform bdc_field       using 'AFRUD-WERKS'
*                              ISMNW_2_017.

*    PERFORM BDC_FIELD       USING 'AFRUD-ISMNW_2'
*                                  '100'.

*perform bdc_field       using 'AFRUD-ISMNU'
*                              BUDAT_019.

**** Posting date -> Working end date (2003.11.27)
    PERFORM BDC_FIELD       USING 'AFRUD-BUDAT'
                                   WA_IEDD.    "WA_BUDAT.

*perform bdc_field       using 'AFRUD-AUERU'
*                              LEKNW_021.
*perform bdc_field       using 'AFRUD-LEKNW'
*                              OFMNU_022.
*perform bdc_field       using 'AFRUD-OFMNU'
*                              ISDD_023.

***** Confirmed date for start of execution
    PERFORM BDC_FIELD       USING 'AFRUD-ISDD'
                                  WA_ISDD.
**** Confirmed time for 'Execution start'
    PERFORM BDC_FIELD       USING 'AFRUD-ISDZ'
                                  ZSPM_TIME_CONF-ISDZ.

*perform bdc_field       using 'AFRUD-IDAUE'
*                              IEDD_026.

***** Confirmed date for execution finish
    PERFORM BDC_FIELD       USING 'AFRUD-IEDD'
                                  WA_IEDD.
**** Confirmed time for 'Execution finish'
    PERFORM BDC_FIELD       USING 'AFRUD-IEDZ'
                                  ZSPM_TIME_CONF-IEDZ.
**** Actual duration
    PERFORM BDC_FIELD       USING 'AFRUD-IDAUR'
                                  WA_IDAUR.
**** Unit for actual duration
    PERFORM BDC_FIELD       USING 'AFRUD-IDAUE'
                                  IDAUE.

*perform bdc_field       using 'AFRUD-PEDZ'
*                              PEDZ_028.
  ENDLOOP.

  PERFORM BDC_DYNPRO      USING 'SAPLCORU' '3390'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                'BACK'.
*** Save...
  PERFORM BDC_DYNPRO      USING 'SAPLCMFU' '0001'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'IW42'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.

ENDFUNCTION.
