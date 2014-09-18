FUNCTION z_fmm_60xx_vl32n.
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
*"     VALUE(VBELN_001) LIKE  BDCDATA-FVAL
*"     VALUE(BLDAT_002) LIKE  BDCDATA-FVAL
*"     VALUE(WADAT_IST_LA_003) LIKE  BDCDATA-FVAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '4104'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'LIKP-VBELN'
                                vbeln_001.

  PERFORM bdc_dynpro      USING 'SAPMV50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                'T\01'.  "Item overview

  PERFORM bdc_dynpro      USING 'SAPMV50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=WABU_T'.
  PERFORM bdc_field       USING 'LIKP-BLDAT'
                                bldat_002.
  PERFORM bdc_field       USING 'RV50A-WADAT_IST_LA'
                                wadat_ist_la_003.
  PERFORM bdc_transaction TABLES messtab
  USING                         'VL32N'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.

ENDFUNCTION.
