FUNCTION z_fmm_6025_in_mk05 .
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"             VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'N'
*"             VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"             VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"             VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"             VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"             VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"             VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"             VALUE(LIFNR_001) LIKE  BDCDATA-FVAL DEFAULT 'N001'
*"             VALUE(EKORG_002) LIKE  BDCDATA-FVAL DEFAULT 'PU01'
*"             VALUE(SPERM_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"             VALUE(SPERM_004) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"             VALUE(SPERQ_005) LIKE  BDCDATA-FVAL DEFAULT '99'
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0507'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02K-LIFNR'
                                lifnr_001.
  PERFORM bdc_field       USING 'RF02K-EKORG'
                                ekorg_002.
  PERFORM bdc_dynpro      USING 'SAPMF02K' '0510'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPDA'.
  PERFORM bdc_field       USING 'LFA1-SPERM'
                                sperm_003.
  PERFORM bdc_field       USING 'LFM1-SPERM'
                                sperm_004.
  PERFORM bdc_field       USING 'LFA1-SPERQ'
                                sperq_005.
  PERFORM bdc_transaction TABLES messtab
  USING                         'MK05'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.





ENDFUNCTION.
