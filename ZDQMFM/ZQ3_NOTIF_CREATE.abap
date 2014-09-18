FUNCTION ZQ3_NOTIF_CREATE.
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
*"     VALUE(QMART_001) LIKE  BDCDATA-FVAL DEFAULT 'q3'
*"     VALUE(QMGRP_002) LIKE  BDCDATA-FVAL DEFAULT 'MXBXC1'
*"     VALUE(QMCOD_003) LIKE  BDCDATA-FVAL DEFAULT 'BXC1'
*"     VALUE(HEADKTXT_004) LIKE  BDCDATA-FVAL DEFAULT
*"       'test notification'
*"     VALUE(MATNR_005) LIKE  BDCDATA-FVAL DEFAULT '1140306121'
*"     VALUE(MAWERK_006) LIKE  BDCDATA-FVAL DEFAULT 'p001'
*"     VALUE(RKMNG_007) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(OTGRP_008) LIKE  BDCDATA-FVAL DEFAULT 'MXBXC1'
*"     VALUE(OTEIL_009) LIKE  BDCDATA-FVAL DEFAULT 'BXC1'
*"     VALUE(FEGRP_010) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(FECOD_011) LIKE  BDCDATA-FVAL DEFAULT '1002'
*"     VALUE(URCOD_012) LIKE  BDCDATA-FVAL DEFAULT '01'
*"     VALUE(URGRP_013) LIKE  BDCDATA-FVAL DEFAULT 'CAUS'
*"     VALUE(PRIOK_014) LIKE  BDCDATA-FVAL DEFAULT '2'
*"     VALUE(STRMN_015) LIKE  BDCDATA-FVAL DEFAULT '2006/11/14'
*"     VALUE(STRUR_016) LIKE  BDCDATA-FVAL DEFAULT '09:09:02'
*"     VALUE(LTRUR_017) LIKE  BDCDATA-FVAL DEFAULT '00:00:00'
*"     VALUE(AUSVN_018) LIKE  BDCDATA-FVAL DEFAULT '2006/11/14'
*"     VALUE(AUZTV_019) LIKE  BDCDATA-FVAL DEFAULT '09:09:46'
*"     VALUE(BUNAME_020) LIKE  BDCDATA-FVAL DEFAULT '100565'
*"     VALUE(PRIOK_021) LIKE  BDCDATA-FVAL DEFAULT '2'
*"     VALUE(STRMN_022) LIKE  BDCDATA-FVAL DEFAULT '2006/11/14'
*"     VALUE(STRUR_023) LIKE  BDCDATA-FVAL DEFAULT '09:09:02'
*"     VALUE(LTRMN_024) LIKE  BDCDATA-FVAL DEFAULT '2006/11/16'
*"     VALUE(LTRUR_025) LIKE  BDCDATA-FVAL DEFAULT '09:09:02'
*"     VALUE(AUSVN_026) LIKE  BDCDATA-FVAL DEFAULT '2006/11/14'
*"     VALUE(AUZTV_027) LIKE  BDCDATA-FVAL DEFAULT '09:09:46'
*"     VALUE(BUNAME_028) LIKE  BDCDATA-FVAL DEFAULT '100565'
*"     VALUE(CODE_VH_029) LIKE  BDCDATA-FVAL DEFAULT 'CM'
*"     VALUE(CODE_VH_030) LIKE  BDCDATA-FVAL DEFAULT 'QQVE'
*"     VALUE(PARNR_02_031) LIKE  BDCDATA-FVAL DEFAULT 'sef9'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLIQS0' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'RIWO00-QMNUM'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RIWO00-QMART'
                              QMART_001.
perform bdc_dynpro      using 'SAPLIQS0' '7200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=10\TAB02'.
perform bdc_field       using 'VIQMEL-QMGRP'
                              QMGRP_002.
perform bdc_field       using 'VIQMEL-QMCOD'
                              QMCOD_003.
perform bdc_field       using 'RIWO00-HEADKTXT'
                              HEADKTXT_004.
perform bdc_field       using 'BDC_CURSOR'
                              'RQM00-SERIALNR'.
perform bdc_field       using 'RQM00-MATNR'
                              MATNR_005.
perform bdc_field       using 'RQM00-MAWERK'
                              MAWERK_006.
perform bdc_field       using 'VIQMEL-RKMNG'
                              RKMNG_007.
perform bdc_field       using 'VIQMFE-OTGRP'
                              OTGRP_008.
perform bdc_field       using 'VIQMFE-OTEIL'
                              OTEIL_009.
perform bdc_field       using 'VIQMFE-FEGRP'
                              FEGRP_010.
perform bdc_field       using 'VIQMFE-FECOD'
                              FECOD_011.
perform bdc_field       using 'VIQMUR-URCOD'
                              URCOD_012.
perform bdc_field       using 'VIQMUR-URGRP'
                              URGRP_013.
perform bdc_dynpro      using 'SAPLIQS0' '7200'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'BDC_CURSOR'
                              'VIQMEL-PRIOK'.
perform bdc_field       using 'VIQMEL-PRIOK'
                              PRIOK_014.
perform bdc_field       using 'VIQMEL-STRMN'
                              STRMN_015.
perform bdc_field       using 'VIQMEL-STRUR'
                              STRUR_016.
perform bdc_field       using 'VIQMEL-LTRUR'
                              LTRUR_017.
perform bdc_field       using 'VIQMEL-AUSVN'
                              AUSVN_018.
perform bdc_field       using 'VIQMEL-AUZTV'
                              AUZTV_019.
perform bdc_field       using 'VIQMEL-BUNAME'
                              BUNAME_020.
perform bdc_dynpro      using 'SAPLIQS0' '7200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=10\TAB17'.
perform bdc_field       using 'BDC_CURSOR'
                              'VIQMEL-PRIOK'.
perform bdc_field       using 'VIQMEL-PRIOK'
                              PRIOK_021.
perform bdc_field       using 'VIQMEL-STRMN'
                              STRMN_022.
perform bdc_field       using 'VIQMEL-STRUR'
                              STRUR_023.
perform bdc_field       using 'VIQMEL-LTRMN'
                              LTRMN_024.
perform bdc_field       using 'VIQMEL-LTRUR'
                              LTRUR_025.
perform bdc_field       using 'VIQMEL-AUSVN'
                              AUSVN_026.
perform bdc_field       using 'VIQMEL-AUZTV'
                              AUZTV_027.
perform bdc_field       using 'VIQMEL-BUNAME'
                              BUNAME_028.
perform bdc_dynpro      using 'SAPLIQS0' '7204'.
perform bdc_field       using 'BDC_OKCODE'
                              '=COWO'.
perform bdc_field       using 'BDC_CURSOR'
                              'ZSQM_CI_QMEL-CODE_VH'.
perform bdc_field       using 'ZSQM_CI_QMEL-CODE_VH'
                              CODE_VH_029.
perform bdc_dynpro      using 'SAPLIQS0' '7204'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BUCH'.
perform bdc_field       using 'BDC_CURSOR'
                              'ZSQM_CI_QMEL-CODE_VH'.
perform bdc_field       using 'ZSQM_CI_QMEL-CODE_VH'
                              CODE_VH_030.
perform bdc_dynpro      using 'SAPLIPAR' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'IHPA-PARNR(02)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'IHPA-PARNR(02)'
                              PARNR_02_031.
perform bdc_dynpro      using 'SAPLIPAR' '0200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BACK'.
perform bdc_transaction tables messtab
using                         'QM01'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
INCLUDE BDCRECXY .
