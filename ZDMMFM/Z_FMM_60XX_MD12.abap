FUNCTION Z_FMM_60XX_MD12.
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
*"             VALUE(PLNUM_001) LIKE  BDCDATA-FVAL DEFAULT '141'
*"             VALUE(GSMNG_002) LIKE  BDCDATA-FVAL DEFAULT '1,000'
*"             VALUE(PEDTR_003) LIKE  BDCDATA-FVAL
*"         DEFAULT '01/05/2004'
*"             VALUE(PSTTR_004) LIKE  BDCDATA-FVAL
*"         DEFAULT '01/05/2004'
*"             VALUE(PERTR_005) LIKE  BDCDATA-FVAL
*"         DEFAULT '01/05/2004'
*"             VALUE(PWWRK_006) LIKE  BDCDATA-FVAL DEFAULT 'E001'
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPMM61P' '0101'.
perform bdc_field       using 'BDC_CURSOR'
                              'RM61P-PLNUM'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RM61P-PLNUM'
                              PLNUM_001.
perform bdc_dynpro      using 'SAPLM61O' '0110'.
perform bdc_field       using 'BDC_CURSOR'
                              'PLAF-MATNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '=DLPL'.
perform bdc_field       using 'PLAF-GSMNG'
                              GSMNG_002.
perform bdc_field       using 'PLAF-PEDTR'
                              PEDTR_003.
perform bdc_field       using 'PLAF-PSTTR'
                              PSTTR_004.
perform bdc_field       using 'PLAF-PERTR'
                              PERTR_005.
perform bdc_field       using 'PLAF-PWWRK'
                              PWWRK_006.
perform bdc_transaction tables messtab
using                         'MD12'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
