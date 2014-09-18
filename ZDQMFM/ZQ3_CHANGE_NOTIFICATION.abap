FUNCTION ZQ3_CHANGE_NOTIFICATION.
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
*"     VALUE(QMNUM_001) LIKE  BDCDATA-FVAL DEFAULT '4000000297'
*"     VALUE(QMGRP_002) LIKE  BDCDATA-FVAL DEFAULT 'MXTX53'
*"     VALUE(QMCOD_003) LIKE  BDCDATA-FVAL DEFAULT 'TX53'
*"     VALUE(HEADKTXT_004) LIKE  BDCDATA-FVAL OPTIONAL
*"     VALUE(MATNR_005) LIKE  BDCDATA-FVAL DEFAULT '312103K600'
*"     VALUE(MAWERK_006) LIKE  BDCDATA-FVAL DEFAULT 'P001'
*"     VALUE(RKMNG_007) LIKE  BDCDATA-FVAL DEFAULT '13'
*"     VALUE(BZMNG_008) LIKE  BDCDATA-FVAL DEFAULT '13'
*"     VALUE(OTGRP_009) LIKE  BDCDATA-FVAL DEFAULT 'MXBXB1'
*"     VALUE(OTEIL_010) LIKE  BDCDATA-FVAL DEFAULT 'BXB1'
*"     VALUE(FEGRP_011) LIKE  BDCDATA-FVAL DEFAULT '4'
*"     VALUE(FECOD_012) LIKE  BDCDATA-FVAL DEFAULT '4002'
*"     VALUE(URCOD_013) LIKE  BDCDATA-FVAL DEFAULT '01'
*"     VALUE(URGRP_014) LIKE  BDCDATA-FVAL DEFAULT 'CAUS'
*"     VALUE(I_VIQMEL) LIKE  VIQMEL STRUCTURE  VIQMEL OPTIONAL
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

data:zshort_text(132) type c.
data:zNotification(132) type c.

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLIQS0' '0200'.
perform bdc_field       using 'BDC_CURSOR'
                              'RIWO00-QMNUM'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RIWO00-QMNUM'
*                              QMNUM_001.
                               i_viqmel-qmnum.
*                               znotification.
perform bdc_dynpro      using 'SAPLIQS0' '7200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BUCH'.
perform bdc_field       using 'BDC_CURSOR'
                              'RIWO00-HEADKTXT'.
*perform bdc_field       using 'VIQMEL-QMGRP'
*                              QMGRP_002.
*perform bdc_field       using 'VIQMEL-QMCOD'
*                              QMCOD_003.
perform bdc_field       using 'RIWO00-HEADKTXT'
*                              HEADKTXT_004.
*                               zshort_text.
                             i_viqmel-qmtxt.
*perform bdc_field       using 'RQM00-MATNR'
*                              MATNR_005.
*perform bdc_field       using 'RQM00-MAWERK'
*                              MAWERK_006.
*perform bdc_field       using 'VIQMEL-RKMNG'
*                              RKMNG_007.
*perform bdc_field       using 'VIQMEL-BZMNG'
*                              BZMNG_008.
*perform bdc_field       using 'VIQMFE-OTGRP'
*                              OTGRP_009.
*perform bdc_field       using 'VIQMFE-OTEIL'
*                              OTEIL_010.
*perform bdc_field       using 'VIQMFE-FEGRP'
*                              FEGRP_011.
*perform bdc_field       using 'VIQMFE-FECOD'
*                              FECOD_012.
*perform bdc_field       using 'VIQMUR-URCOD'
*                              URCOD_013.
*perform bdc_field       using 'VIQMUR-URGRP'
*                              URGRP_014.
perform bdc_transaction tables messtab
using                         'QM02'
                              CTU
                              MODE
                              UPDATE.


if sy-subrc = 0.
delete messtab where tcode = 'QM02'.
endif.

if sy-subrc <> 0.
loop at messtab where tcode = 'QM02'.
concatenate 'Error updating Notification:' messtab-msgv1
'Call Team Leader' into messtab-msgv1 separated by space.
modify messtab.
endloop.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
