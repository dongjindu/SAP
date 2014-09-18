FUNCTION ZQM_SCRAP_MATERIAL_OBS.
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
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL DEFAULT '2006/12/04'
*"     VALUE(BUDAT_002) LIKE  BDCDATA-FVAL DEFAULT '2006/12/04'
*"     VALUE(BWARTWA_003) LIKE  BDCDATA-FVAL DEFAULT '551'
*"     VALUE(WERKS_004) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(LGORT_005) LIKE  BDCDATA-FVAL DEFAULT 'P400'
*"     VALUE(XNUVR_006) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(XFULL_007) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(WVERS2_008) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_01_009) LIKE  BDCDATA-FVAL DEFAULT '811803K000QS'
*"     VALUE(ERFMG_01_010) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(AUFNR_011) LIKE  BDCDATA-FVAL DEFAULT 'CP001'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPMM07M' '0400'.
perform bdc_field       using 'BDC_CURSOR'
                              'XFULL'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'MKPF-BLDAT'
                              BLDAT_001.
perform bdc_field       using 'MKPF-BUDAT'
                              BUDAT_002.
perform bdc_field       using 'RM07M-BWARTWA'
                              BWARTWA_003.
perform bdc_field       using 'RM07M-WERKS'
                              WERKS_004.
perform bdc_field       using 'RM07M-LGORT'
                              LGORT_005.
perform bdc_field       using 'RM07M-XNUVR'
                              XNUVR_006.
perform bdc_field       using 'XFULL'
                              XFULL_007.
perform bdc_field       using 'RM07M-WVERS2'
                              WVERS2_008.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'MSEG-ERFMG(05)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'MSEG-MATNR(01)'
                              MATNR_01_009.
perform bdc_field       using 'MSEG-ERFMG(01)'
                              ERFMG_01_010.
perform bdc_field       using 'COBL-AUFNR'
                              AUFNR_011.
perform bdc_transaction tables messtab
using                         'MB1A'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
