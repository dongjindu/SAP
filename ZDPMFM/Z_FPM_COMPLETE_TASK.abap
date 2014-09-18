FUNCTION Z_FPM_COMPLETE_TASK.
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
*"     VALUE(NODATA) LIKE  APQI-PUTACTIVE DEFAULT ' '
*"     VALUE(QMNUM) LIKE  ZSPM_COMP-QMNUM
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLIQS0' '0100'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RIWO00-QMNUM'
                              QMNUM.
perform bdc_dynpro      using 'SAPLIQS0' '7200'.
perform bdc_field       using 'BDC_OKCODE'
                              '=10\TAB10'.
perform bdc_dynpro      using 'SAPLIQS0' '7204'.
perform bdc_field       using 'BDC_OKCODE'
                              '=20\TAB03'.
perform bdc_dynpro      using 'SAPLIQS0' '7204'.
perform bdc_field       using 'BDC_OKCODE'
                              '=MALL'.
perform bdc_dynpro      using 'SAPLIQS0' '7204'.
perform bdc_field       using 'BDC_OKCODE'
                              '=MADF'.
perform bdc_dynpro      using 'SAPLIQS0' '7204'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BUCH'.
perform bdc_transaction tables messtab
using                         'IW22'
                              CTU
                              MODE
                              UPDATE.
if sy-subrc <> 0.
  subrc = sy-subrc.
  exit.
endif.

perform close_group using     CTU.





ENDFUNCTION.
