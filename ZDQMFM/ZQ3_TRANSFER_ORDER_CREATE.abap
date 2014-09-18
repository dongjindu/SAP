FUNCTION ZQ3_TRANSFER_ORDER_CREATE.
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
*"     VALUE(LGNUM_001) LIKE  MSEG-LGNUM
*"     VALUE(TBNUM_002) LIKE  MSEG-TBNUM
*"     VALUE(ALAKT_003) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(DUNKL_004) LIKE  BDCDATA-FVAL DEFAULT 'H'
*"     VALUE(ANFME_005) LIKE  VIQMEL-RKMNG DEFAULT '1'
*"     VALUE(ALTME_006) LIKE  VIQMEL-MGEIN DEFAULT 'EA'
*"     VALUE(SQUIT_007) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(VLTYP_008) LIKE  ZSRF_PICKER_LIST-NLTYP
*"     VALUE(VLPLA_009) LIKE  ZSRF_PICKER_LIST-NLPLA
*"     VALUE(LGTY0_010) LIKE  BDCDATA-FVAL DEFAULT '431'
*"     VALUE(LGTY1_011) LIKE  BDCDATA-FVAL DEFAULT '432'
*"     VALUE(LGTY2_012) LIKE  BDCDATA-FVAL DEFAULT '433'
*"     VALUE(LGTY3_013) LIKE  BDCDATA-FVAL DEFAULT '434'
*"     VALUE(LGTY4_014) LIKE  BDCDATA-FVAL DEFAULT '435'
*"     VALUE(LGTY5_015) LIKE  BDCDATA-FVAL DEFAULT '436'
*"     VALUE(LGTY6_016) LIKE  BDCDATA-FVAL DEFAULT '437'
*"     VALUE(LGTY7_017) LIKE  BDCDATA-FVAL DEFAULT '441'
*"     VALUE(LGTY8_018) LIKE  BDCDATA-FVAL DEFAULT '442'
*"     VALUE(LGTY9_019) LIKE  BDCDATA-FVAL DEFAULT '443'
*"     VALUE(LGT10_020) LIKE  BDCDATA-FVAL DEFAULT '444'
*"     VALUE(LGT11_021) LIKE  BDCDATA-FVAL DEFAULT '445'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
DATA: ZQTY(13) TYPE C.
subrc = 0.
*
perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPML03T' '0131'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTBK-TBNUM'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'LTAK-LGNUM'
                              LGNUM_001.
perform bdc_field       using 'LTBK-TBNUM'
                              TBNUM_002.
perform bdc_field       using 'RL03T-ALAKT'
                              ALAKT_003.
perform bdc_field       using 'RL03T-DUNKL'
                              DUNKL_004.
perform bdc_dynpro      using 'SAPML03T' '0132'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BEST'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTBP1-OFMEA(01)'.
perform bdc_dynpro      using 'SAPML03T' '0106'.
perform bdc_field       using 'BDC_OKCODE'
                              '=TAH2'.
perform bdc_field       using 'BDC_CURSOR'
                              'RL03T-SELMG(01)'.
perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTAP-VLPLA'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
MOVE ANFME_005 TO ZQTY.
perform bdc_field       using 'RL03T-ANFME'
*                              ANFME_005.
                               ZQTY.
perform bdc_field       using 'LTAP-ALTME'
                              ALTME_006.
perform bdc_field       using 'RL03T-SQUIT'
                              SQUIT_007.
perform bdc_field       using 'LTAP-VLTYP'
                              VLTYP_008.
perform bdc_field       using 'LTAP-VLPLA'
                              VLPLA_009.
perform bdc_dynpro      using 'SAPML03T' '0105'.
perform bdc_field       using 'BDC_CURSOR'
                              'T334T-LGTY0'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
*perform bdc_field       using 'T334T-LGTY0'
*                              LGTY0_010.
*perform bdc_field       using 'T334T-LGTY1'
*                              LGTY1_011.
*perform bdc_field       using 'T334T-LGTY2'
*                              LGTY2_012.
*perform bdc_field       using 'T334T-LGTY3'
*                              LGTY3_013.
*perform bdc_field       using 'T334T-LGTY4'
*                              LGTY4_014.
*perform bdc_field       using 'T334T-LGTY5'
*                              LGTY5_015.
*perform bdc_field       using 'T334T-LGTY6'
*                              LGTY6_016.
*perform bdc_field       using 'T334T-LGTY7'
*                              LGTY7_017.
*perform bdc_field       using 'T334T-LGTY8'
*                              LGTY8_018.
*perform bdc_field       using 'T334T-LGTY9'
*                              LGTY9_019.
*perform bdc_field       using 'T334T-LGT10'
*                              LGT10_020.
*perform bdc_field       using 'T334T-LGT11'
*                              LGT11_021.
perform bdc_transaction tables messtab
using                         'LT04'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
