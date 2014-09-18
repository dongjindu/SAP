FUNCTION ZQ3_NOTIFICATION_CREATE.
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
*"     VALUE(QMART_001) LIKE  BDCDATA-FVAL DEFAULT 'Q3'
*"     VALUE(QMGRP_002) LIKE  BDCDATA-FVAL DEFAULT 'MXBXB1'
*"     VALUE(QMCOD_003) LIKE  BDCDATA-FVAL DEFAULT 'BXB1'
*"     VALUE(MATNR_004) LIKE  BDCDATA-FVAL DEFAULT '1140306121'
*"     VALUE(MAWERK_005) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(RKMNG_006) LIKE  BDCDATA-FVAL DEFAULT '2'
*"     VALUE(OTGRP_007) LIKE  BDCDATA-FVAL DEFAULT 'MXBXB1'
*"     VALUE(OTEIL_008) LIKE  BDCDATA-FVAL DEFAULT 'BXB1'
*"     VALUE(FEGRP_009) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(FECOD_010) LIKE  BDCDATA-FVAL DEFAULT '1001'
*"     VALUE(URCOD_011) LIKE  BDCDATA-FVAL DEFAULT '01'
*"     VALUE(URGRP_012) LIKE  BDCDATA-FVAL DEFAULT 'CAUS'
*"     VALUE(QMGRP_013) LIKE  BDCDATA-FVAL DEFAULT 'MXBXB1'
*"     VALUE(QMCOD_014) LIKE  BDCDATA-FVAL DEFAULT 'BXB1'
*"     VALUE(MATNR_015) LIKE  BDCDATA-FVAL DEFAULT '1140306121'
*"     VALUE(MAWERK_016) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(RKMNG_017) LIKE  BDCDATA-FVAL DEFAULT '2'
*"     VALUE(BZMNG_018) LIKE  BDCDATA-FVAL DEFAULT '2'
*"     VALUE(OTGRP_019) LIKE  BDCDATA-FVAL DEFAULT 'MXBXB1'
*"     VALUE(OTEIL_020) LIKE  BDCDATA-FVAL DEFAULT 'BXB1'
*"     VALUE(FEGRP_021) LIKE  BDCDATA-FVAL DEFAULT '1'
*"     VALUE(FECOD_022) LIKE  BDCDATA-FVAL DEFAULT '1001'
*"     VALUE(URCOD_023) LIKE  BDCDATA-FVAL DEFAULT '01'
*"     VALUE(URGRP_024) LIKE  BDCDATA-FVAL DEFAULT 'CAUS'
*"     VALUE(PARNR_02_025) LIKE  BDCDATA-FVAL DEFAULT 'AG59'
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
                              '=COWO'.
perform bdc_field       using 'VIQMEL-QMGRP'
                              QMGRP_002.
perform bdc_field       using 'VIQMEL-QMCOD'
                              QMCOD_003.
perform bdc_field       using 'RQM00-MATNR'
                              MATNR_004.
perform bdc_field       using 'RQM00-MAWERK'
                              MAWERK_005.
perform bdc_field       using 'VIQMEL-RKMNG'
                              RKMNG_006.
perform bdc_field       using 'BDC_CURSOR'
                              'VIQMUR-URCOD'.
perform bdc_field       using 'VIQMFE-OTGRP'
                              OTGRP_007.
perform bdc_field       using 'VIQMFE-OTEIL'
                              OTEIL_008.
perform bdc_field       using 'VIQMFE-FEGRP'
                              FEGRP_009.
perform bdc_field       using 'VIQMFE-FECOD'
                              FECOD_010.
perform bdc_field       using 'VIQMUR-URCOD'
                              URCOD_011.
perform bdc_field       using 'VIQMUR-URGRP'
                              URGRP_012.
perform bdc_dynpro      using 'SAPLIQS0' '7200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BUCH'.
perform bdc_field       using 'VIQMEL-QMGRP'
                              QMGRP_013.
perform bdc_field       using 'VIQMEL-QMCOD'
                              QMCOD_014.
perform bdc_field       using 'RQM00-MATNR'
                              MATNR_015.
perform bdc_field       using 'RQM00-MAWERK'
                              MAWERK_016.
perform bdc_field       using 'VIQMEL-RKMNG'
                              RKMNG_017.
perform bdc_field       using 'VIQMEL-BZMNG'
                              BZMNG_018.
perform bdc_field       using 'BDC_CURSOR'
                              'VIQMUR-URCOD'.
perform bdc_field       using 'VIQMFE-OTGRP'
                              OTGRP_019.
perform bdc_field       using 'VIQMFE-OTEIL'
                              OTEIL_020.
perform bdc_field       using 'VIQMFE-FEGRP'
                              FEGRP_021.
perform bdc_field       using 'VIQMFE-FECOD'
                              FECOD_022.
perform bdc_field       using 'VIQMUR-URCOD'
                              URCOD_023.
perform bdc_field       using 'VIQMUR-URGRP'
                              URGRP_024.
perform bdc_dynpro      using 'SAPLIPAR' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'IHPA-PARNR(02)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'IHPA-PARNR(02)'
                              PARNR_02_025.
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
