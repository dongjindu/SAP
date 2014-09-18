FUNCTION z_fmm_6010_01.
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
*"     VALUE(LGNUM_001) LIKE  BDCDATA-FVAL DEFAULT 'p01'
*"     VALUE(VBELN_002) LIKE  BDCDATA-FVAL DEFAULT '180000335'
*"     VALUE(POSNR_003) LIKE  BDCDATA-FVAL DEFAULT '000001'
*"     VALUE(ALAKT_004) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(WDATU_005) LIKE  BDCDATA-FVAL DEFAULT '10/08/2003'
*"     VALUE(ANZL2_006) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(LMEN2_007) LIKE  BDCDATA-FVAL DEFAULT '1,000'
*"     VALUE(WDATU_008) LIKE  BDCDATA-FVAL DEFAULT '10/08/2003'
*"     VALUE(LMEN2_009) LIKE  BDCDATA-FVAL DEFAULT '1,000'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

  subrc = 0.

  PERFORM bdc_nodata      USING nodata.

  PERFORM open_group      USING group user keep holddate ctu.

  PERFORM bdc_dynpro      USING 'SAPML03T' '0151'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                '*VBLKP-POSNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'LTAK-LGNUM'
                                lgnum_001.
  PERFORM bdc_field       USING 'VBLKK-VBELN'
                                vbeln_002.
  PERFORM bdc_field       USING '*VBLKP-POSNR'
                                posnr_003.
  PERFORM bdc_field       USING 'RL03T-ALAKT'
                                alakt_004.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0104'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RL03T-ANZL1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=TAD1'.
  PERFORM bdc_field       USING 'LTAP-WDATU'
                                wdatu_005.
  PERFORM bdc_field       USING 'RL03T-ANZL2'
                                anzl2_006.
  PERFORM bdc_field       USING 'RL03T-LMEN2'
                                lmen2_007.
  PERFORM bdc_dynpro      USING 'SAPML03T' '0104'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RL03T-ANZL1'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=BU'.
  PERFORM bdc_field       USING 'LTAP-WDATU'
                                wdatu_008.
  PERFORM bdc_field       USING 'RL03T-LMEN2'
                                lmen2_009.
  PERFORM bdc_transaction TABLES messtab
  USING                         'LT03'
                                ctu
                                mode
                                update.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
*    EXIT.
  ENDIF.

  PERFORM close_group USING     ctu.

ENDFUNCTION.
INCLUDE bdcrecxy .
