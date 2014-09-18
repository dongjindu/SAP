FUNCTION ZQM_SCRAP_RTV.
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
*"             VALUE(ACTION_001) LIKE  BDCDATA-FVAL DEFAULT 'A02'
*"             VALUE(REFDOC_002) LIKE  BDCDATA-FVAL DEFAULT 'R02'
*"             VALUE(MAT_DOC_003) LIKE  BDCDATA-FVAL
*"         DEFAULT '5100423302'
*"             VALUE(BUDAT_004) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/11/17'
*"             VALUE(BLDAT_005) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/03/13'
*"             VALUE(LFSNR_006) LIKE  BDCDATA-FVAL
*"         DEFAULT '0180421745'
*"             VALUE(BUDAT_007) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/11/17'
*"             VALUE(FRBNR_008) LIKE  BDCDATA-FVAL DEFAULT '79222468'
*"             VALUE(WEVER_009) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(DETAIL_ZEILE_010) LIKE  BDCDATA-FVAL
*"         DEFAULT '   1'
*"             VALUE(ERFME_011) LIKE  BDCDATA-FVAL DEFAULT 'EA'
*"             VALUE(ERFMG_012) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(BWART_013) LIKE  BDCDATA-FVAL DEFAULT '122'
*"             VALUE(LGOBE_014) LIKE  BDCDATA-FVAL
*"         DEFAULT 'W/M Total'
*"             VALUE(BLDAT_015) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/03/13'
*"             VALUE(LFSNR_016) LIKE  BDCDATA-FVAL
*"         DEFAULT '0180421745'
*"             VALUE(BUDAT_017) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/11/17'
*"             VALUE(FRBNR_018) LIKE  BDCDATA-FVAL DEFAULT '79222468'
*"             VALUE(WEVER_019) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(DETAIL_ZEILE_020) LIKE  BDCDATA-FVAL
*"         DEFAULT '   1'
*"             VALUE(ERFME_021) LIKE  BDCDATA-FVAL DEFAULT 'EA'
*"             VALUE(ERFMG_022) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(BWART_023) LIKE  BDCDATA-FVAL DEFAULT '122'
*"             VALUE(LGOBE_024) LIKE  BDCDATA-FVAL
*"         DEFAULT 'W/M Total'
*"             VALUE(GRUND_025) LIKE  BDCDATA-FVAL DEFAULT '0010'
*"             VALUE(SGTXT_026) LIKE  BDCDATA-FVAL DEFAULT 'scrap'
*"             VALUE(BLDAT_027) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/03/13'
*"             VALUE(LFSNR_028) LIKE  BDCDATA-FVAL
*"         DEFAULT '0180421745'
*"             VALUE(BUDAT_029) LIKE  BDCDATA-FVAL
*"         DEFAULT '2006/11/17'
*"             VALUE(FRBNR_030) LIKE  BDCDATA-FVAL DEFAULT '79222468'
*"             VALUE(WEVER_031) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(DETAIL_ZEILE_032) LIKE  BDCDATA-FVAL
*"         DEFAULT '   1'
*"             VALUE(ERFME_033) LIKE  BDCDATA-FVAL DEFAULT 'EA'
*"             VALUE(ERFMG_034) LIKE  BDCDATA-FVAL DEFAULT '1'
*"             VALUE(BWART_035) LIKE  BDCDATA-FVAL DEFAULT '122'
*"             VALUE(LGOBE_036) LIKE  BDCDATA-FVAL
*"         DEFAULT 'W/M Total'
*"             VALUE(GRUND_037) LIKE  BDCDATA-FVAL DEFAULT '10'
*"             VALUE(SGTXT_038) LIKE  BDCDATA-FVAL DEFAULT 'scrap'
*"             VALUE(DETAIL_TAKE_039) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"       EXPORTING
*"             VALUE(SUBRC) LIKE  SYST-SUBRC
*"       TABLES
*"              MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLMIGO' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=OK_GO'.
perform bdc_field       using 'BDC_CURSOR'
                              'GODYNPRO-REFDOC'.
perform bdc_field       using 'GODYNPRO-ACTION'
                              ACTION_001.
perform bdc_field       using 'GODYNPRO-REFDOC'
                              REFDOC_002.
perform bdc_field       using 'GODYNPRO-MAT_DOC'
                              MAT_DOC_003.
perform bdc_field       using 'GOHEAD-BUDAT'
                              BUDAT_004.
perform bdc_dynpro      using 'SAPLMIGO' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=OK_GO'.
perform bdc_field       using 'GOHEAD-BLDAT'
                              BLDAT_005.
perform bdc_field       using 'GOHEAD-LFSNR'
                              LFSNR_006.
perform bdc_field       using 'GOHEAD-BUDAT'
                              BUDAT_007.
perform bdc_field       using 'GOHEAD-FRBNR'
                              FRBNR_008.
perform bdc_field       using 'GOHEAD-WEVER'
                              WEVER_009.
perform bdc_field       using 'GODYNPRO-DETAIL_ZEILE'
                              DETAIL_ZEILE_010.
perform bdc_field       using 'BDC_CURSOR'
                              'GOITEM-ERFMG'.
perform bdc_field       using 'GOITEM-ERFME'
                              ERFME_011.
perform bdc_field       using 'GOITEM-ERFMG'
                              ERFMG_012.
perform bdc_field       using 'GOITEM-BWART'
                              BWART_013.
perform bdc_field       using 'GOITEM-LGOBE'
                              LGOBE_014.
perform bdc_dynpro      using 'SAPLMIGO' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=OK_GO'.
perform bdc_field       using 'GOHEAD-BLDAT'
                              BLDAT_015.
perform bdc_field       using 'GOHEAD-LFSNR'
                              LFSNR_016.
perform bdc_field       using 'GOHEAD-BUDAT'
                              BUDAT_017.
perform bdc_field       using 'GOHEAD-FRBNR'
                              FRBNR_018.
perform bdc_field       using 'GOHEAD-WEVER'
                              WEVER_019.
perform bdc_field       using 'GODYNPRO-DETAIL_ZEILE'
                              DETAIL_ZEILE_020.
perform bdc_field       using 'GOITEM-ERFME'
                              ERFME_021.
perform bdc_field       using 'GOITEM-ERFMG'
                              ERFMG_022.
perform bdc_field       using 'BDC_CURSOR'
                              'GOITEM-SGTXT'.
perform bdc_field       using 'GOITEM-BWART'
                              BWART_023.
perform bdc_field       using 'GOITEM-LGOBE'
                              LGOBE_024.
perform bdc_field       using 'GOITEM-GRUND'
                              GRUND_025.
perform bdc_field       using 'GOITEM-SGTXT'
                              SGTXT_026.
perform bdc_dynpro      using 'SAPLMIGO' '0001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=OK_POST'.
perform bdc_field       using 'GOHEAD-BLDAT'
                              BLDAT_027.
perform bdc_field       using 'GOHEAD-LFSNR'
                              LFSNR_028.
perform bdc_field       using 'GOHEAD-BUDAT'
                              BUDAT_029.
perform bdc_field       using 'GOHEAD-FRBNR'
                              FRBNR_030.
perform bdc_field       using 'GOHEAD-WEVER'
                              WEVER_031.
perform bdc_field       using 'GODYNPRO-DETAIL_ZEILE'
                              DETAIL_ZEILE_032.
perform bdc_field       using 'GOITEM-ERFME'
                              ERFME_033.
perform bdc_field       using 'GOITEM-ERFMG'
                              ERFMG_034.
perform bdc_field       using 'GOITEM-BWART'
                              BWART_035.
perform bdc_field       using 'GOITEM-LGOBE'
                              LGOBE_036.
perform bdc_field       using 'GOITEM-GRUND'
                              GRUND_037.
perform bdc_field       using 'GOITEM-SGTXT'
                              SGTXT_038.
perform bdc_field       using 'BDC_CURSOR'
                              'GODYNPRO-DETAIL_TAKE'.
perform bdc_field       using 'GODYNPRO-DETAIL_TAKE'
                              DETAIL_TAKE_039.
perform bdc_transaction tables messtab
using                         'MIGO'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
