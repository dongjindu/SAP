FUNCTION ZQM_SCRAP_MATERIAL_555.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CTU) LIKE  APQI-PUTACTIVE DEFAULT 'X'
*"     VALUE(MODE) LIKE  APQI-PUTACTIVE DEFAULT 'E'
*"     VALUE(UPDATE) LIKE  APQI-PUTACTIVE DEFAULT 'L'
*"     VALUE(GROUP) LIKE  APQI-GROUPID OPTIONAL
*"     VALUE(USER) LIKE  APQI-USERID OPTIONAL
*"     VALUE(KEEP) LIKE  APQI-QERASE OPTIONAL
*"     VALUE(HOLDDATE) LIKE  APQI-STARTDATE OPTIONAL
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT '/'
*"     VALUE(BLDAT_001) LIKE  BDCDATA-FVAL DEFAULT '10/31/2006'
*"     VALUE(BUDAT_002) LIKE  BDCDATA-FVAL DEFAULT '10/31/2006'
*"     VALUE(BWARTWA_003) LIKE  BDCDATA-FVAL DEFAULT '555'
*"     VALUE(WERKS_004) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(LGORT_005) LIKE  BDCDATA-FVAL DEFAULT 'P400'
*"     VALUE(XFULL_006) LIKE  BDCDATA-FVAL DEFAULT ''
*"     VALUE(WVERS2_007) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(MATNR_01_008) LIKE  BDCDATA-FVAL DEFAULT '312103K600'
*"     VALUE(ERFMG_01_009) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(AUFNR_010) LIKE  BDCDATA-FVAL DEFAULT 'CP001'
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL
*"     VALUE(I_CUSTOMIZING) LIKE  V_TQ85 STRUCTURE  V_TQ85
*"     VALUE(I_MANUM) LIKE  QMSM-MANUM
*"     REFERENCE(I_FBCALL)
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"     VALUE(E_QNQMASM0) LIKE  QNQMASM0 STRUCTURE  QNQMASM0
*"     VALUE(E_QNQMAQMEL0) LIKE  QNQMAQMEL0 STRUCTURE  QNQMAQMEL0
*"     VALUE(E_BUCH) TYPE  QKZ
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"      TI_IVIQMFE STRUCTURE  WQMFE
*"      TI_IVIQMUR STRUCTURE  WQMUR
*"      TI_IVIQMSM STRUCTURE  WQMSM
*"      TI_IVIQMMA STRUCTURE  WQMMA
*"      TI_IHPA STRUCTURE  IHPA
*"      TE_CONTAINER STRUCTURE  SWCONT OPTIONAL
*"      TE_LINES STRUCTURE  TLINE OPTIONAL
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
*                              BLDAT_001.
                               sy-datum.
perform bdc_field       using 'MKPF-BUDAT'
*                              BUDAT_002.
                               sy-datum.
perform bdc_field       using 'RM07M-BWARTWA'
                              BWARTWA_003.
perform bdc_field       using 'RM07M-WERKS'
*                              WERKS_004.
                              i_viqmel-mawerk.
perform bdc_field       using 'RM07M-LGORT'
                              LGORT_005.
perform bdc_field       using 'XFULL'
                              XFULL_006.
perform bdc_field       using 'RM07M-WVERS2'
                              WVERS2_007.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'MSEG-ERFMG(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'MSEG-MATNR(01)'
*                              MATNR_01_008.
                               i_viqmel-matnr.
perform bdc_field       using 'MSEG-ERFMG(01)'
                              ERFMG_01_009.
perform bdc_field       using 'COBL-AUFNR'
                              AUFNR_010.
perform bdc_dynpro      using 'SAPMM07M' '0421'.
perform bdc_field       using 'BDC_CURSOR'
                              'MSEG-ERFMG(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.

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
*INCLUDE BDCRECXY .
