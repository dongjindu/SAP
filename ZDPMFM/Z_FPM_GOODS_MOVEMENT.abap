FUNCTION Z_FPM_GOODS_MOVEMENT.
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
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(BLDAT) TYPE  BLDAT
*"     VALUE(BUDAT) TYPE  BUDAT
*"     VALUE(XFULL) LIKE  BDCDATA-FVAL DEFAULT ' '
*"     VALUE(AUFNR) TYPE  AUFNR
*"     VALUE(WEMPF) TYPE  WEMPF OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  DATA: WA_BLDAT(10),
        WA_BUDAT(10).

  WRITE : BLDAT TO WA_BLDAT MM/DD/YYYY.
  WRITE : BUDAT TO WA_BUDAT MM/DD/YYYY.

  SUBRC = 0.

  PERFORM BDC_NODATA      USING NODATA.

  PERFORM OPEN_GROUP      USING GROUP USER KEEP HOLDDATE CTU.

  PERFORM BDC_DYNPRO      USING 'SAPMM07M' '0400'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=NFAL'.
  PERFORM BDC_FIELD       USING 'MKPF-BLDAT'
                                WA_BLDAT.
  PERFORM BDC_FIELD       USING 'MKPF-BUDAT'
                                WA_BUDAT.
*  PERFORM BDC_FIELD       USING 'RM07M-BWARTWA'
*                                BWARTWA.
*  PERFORM BDC_FIELD       USING 'RM07M-LGORT'
*                                LGORT.
  PERFORM BDC_FIELD       USING 'XFULL'
                                XFULL.
*  PERFORM BDC_FIELD       USING 'RM07M-WVERS2'
*                                WVERS2.

  PERFORM BDC_DYNPRO      USING 'SAPMM07M' '1405'.
  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                'RM07M-AUFNR(01)'.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=OK'.
  PERFORM BDC_FIELD       USING 'RM07M-AUFNR(01)'
                                AUFNR.
*  PERFORM BDC_FIELD       USING 'RM07M-LGORT(01)'
*                                LGORT.
*  PERFORM BDC_FIELD       USING 'RM07M-WERKS(01)'
*                                WERKS.
  PERFORM BDC_DYNPRO      USING 'SAPMM07M' '0421'.
  PERFORM BDC_FIELD       USING 'MSEGK-WEMPF'
                                 WEMPF.
  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                '=BU'.

  PERFORM BDC_TRANSACTION TABLES MESSTAB
  USING                         'MB11'
                                CTU
                                MODE
                                UPDATE.
  IF SY-SUBRC <> 0.
    SUBRC = SY-SUBRC.
    EXIT.
  ENDIF.

  PERFORM CLOSE_GROUP USING     CTU.

ENDFUNCTION.
