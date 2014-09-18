FUNCTION ZQ3_TRANSFER_ORDER.
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
*"     VALUE(MBLNR_001) LIKE  BDCDATA-FVAL DEFAULT '5100481010'
*"     VALUE(MJAHR_002) LIKE  BDCDATA-FVAL DEFAULT '2006'
*"     VALUE(DUNKL_003) LIKE  BDCDATA-FVAL DEFAULT 'H'
*"     VALUE(SELMG_01_004) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(ANFME_005) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(ALTME_006) LIKE  BDCDATA-FVAL DEFAULT 'EA'
*"     VALUE(SQUIT_007) LIKE  BDCDATA-FVAL DEFAULT 'X'
*"     VALUE(VLTYP_008) LIKE  BDCDATA-FVAL DEFAULT '435'
*"     VALUE(VLPLA_009) LIKE  BDCDATA-FVAL DEFAULT 'C1-01-01'
*"     VALUE(VLQNR_010) LIKE  BDCDATA-FVAL DEFAULT '587081'
*"     VALUE(LGTY0_011) LIKE  BDCDATA-FVAL DEFAULT '431'
*"     VALUE(LGTY1_012) LIKE  BDCDATA-FVAL DEFAULT '432'
*"     VALUE(LGTY2_013) LIKE  BDCDATA-FVAL DEFAULT '433'
*"     VALUE(LGTY3_014) LIKE  BDCDATA-FVAL DEFAULT '434'
*"     VALUE(LGTY4_015) LIKE  BDCDATA-FVAL DEFAULT '435'
*"     VALUE(LGTY5_016) LIKE  BDCDATA-FVAL DEFAULT '436'
*"     VALUE(LGTY6_017) LIKE  BDCDATA-FVAL DEFAULT '437'
*"     REFERENCE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL
*"     REFERENCE(I_CUSTOMIZING) LIKE  V_TQ85 STRUCTURE  V_TQ85
*"     REFERENCE(I_MANUM) LIKE  QMSM-MANUM
*"     REFERENCE(I_FBCALL)
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"     REFERENCE(E_QNQMASM0) LIKE  QNQMASM0 STRUCTURE  QNQMASM0
*"     REFERENCE(E_QNQMAQMEL0) LIKE  QNQMAQMEL0 STRUCTURE  QNQMAQMEL0
*"     REFERENCE(E_BUCH) TYPE  QKZ
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

perform bdc_dynpro      using 'SAPML02B' '0203'.
perform bdc_field       using 'BDC_CURSOR'
                              'RL02B-MBLNR'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RL02B-MBLNR'
                              MBLNR_001.
perform bdc_field       using 'RL02B-MJAHR'
                              MJAHR_002.
perform bdc_field       using 'RL02B-DUNKL'
                              DUNKL_003.
perform bdc_dynpro      using 'SAPML03T' '0132'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTBP1-OFMEA(01)'.
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
perform bdc_field       using 'RL03T-SELMG(01)'
                              SELMG_01_004.
perform bdc_dynpro      using 'SAPML03T' '0102'.
perform bdc_field       using 'BDC_CURSOR'
                              'LTAP-LETYP'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RL03T-ANFME'
                              ANFME_005.
perform bdc_field       using 'LTAP-ALTME'
                              ALTME_006.
perform bdc_field       using 'RL03T-SQUIT'
                              SQUIT_007.
perform bdc_field       using 'LTAP-VLTYP'
                              VLTYP_008.
perform bdc_field       using 'LTAP-VLPLA'
                              VLPLA_009.
perform bdc_field       using 'LTAP-VLQNR'
                              VLQNR_010.
perform bdc_dynpro      using 'SAPML03T' '0105'.
perform bdc_field       using 'BDC_CURSOR'
                              'T334T-LGTY0'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'T334T-LGTY0'
                              LGTY0_011.
perform bdc_field       using 'T334T-LGTY1'
                              LGTY1_012.
perform bdc_field       using 'T334T-LGTY2'
                              LGTY2_013.
perform bdc_field       using 'T334T-LGTY3'
                              LGTY3_014.
perform bdc_field       using 'T334T-LGTY4'
                              LGTY4_015.
perform bdc_field       using 'T334T-LGTY5'
                              LGTY5_016.
perform bdc_field       using 'T334T-LGTY6'
                              LGTY6_017.
perform bdc_transaction tables messtab
using                         'LT06'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
