FUNCTION ZPM_CREATE_MEASUREMENT_READ.
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
*"     VALUE(DFTIM_001) LIKE  ZPM_DIE_MAINT-ZRUN_TIME DEFAULT
*"       '14:03:49'
*"     VALUE(DFDAT_002) LIKE  ZPM_DIE_MAINT-ZRUN_DATE DEFAULT
*"       '2007/03/01'
*"     VALUE(DFRDR_003) LIKE  APQI-USERID DEFAULT '100565'
*"     VALUE(EQUNR_004) LIKE  EQUI-EQUNR DEFAULT '60000002'
*"     VALUE(DFTIM_005) LIKE  BDCDATA-FVAL DEFAULT '14:03:49'
*"     VALUE(DFDAT_006) LIKE  BDCDATA-FVAL DEFAULT '2007/03/01'
*"     VALUE(DFRDR_007) LIKE  APQI-USERID DEFAULT '100565'
*"     VALUE(DFTIM_008) LIKE  BDCDATA-FVAL DEFAULT '14:03:49'
*"     VALUE(DFDAT_009) LIKE  BDCDATA-FVAL DEFAULT '2007/03/01'
*"     VALUE(DFRDR_010) LIKE  APQI-USERID DEFAULT '100565'
*"     VALUE(RDCNT_01_011) LIKE  ZPM_DIE_MAINT-ZSTROKE_COUNT DEFAULT
*"       '305'
*"     VALUE(DFTIM_012) LIKE  BDCDATA-FVAL DEFAULT '14:03:49'
*"     VALUE(DFDAT_013) LIKE  BDCDATA-FVAL DEFAULT '2007/03/01'
*"     VALUE(DFRDR_014) LIKE  APQI-USERID DEFAULT '100565'
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
DATA: ZEQUNR LIKE EQUI-EQUNR,
      l_date(10) type c,
      l_date1(10) type c..

DATA: Z_ZSTROKE_COUNT LIKE ZPM_DIE_MAINT-ZSTROKE_COUNT.

write DFDAT_002 to l_date.
write sy-datum to l_date1.

subrc = 0.

perform bdc_nodata      using NODATA.

perform open_group      using GROUP USER KEEP HOLDDATE CTU.

perform bdc_dynpro      using 'SAPLIMR0' '1220'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RIMR0-DFTIM'
                              DFTIM_001.
perform bdc_field       using 'RIMR0-DFDAT'
                              l_date.
perform bdc_field       using 'RIMR0-DFRDR'
                              DFRDR_003.
perform bdc_field       using 'BDC_CURSOR'
                              'EQUI-EQUNR'.
perform bdc_field       using 'EQUI-EQUNR'
                              EQUNR_004.
*                               ZEQUNR.
perform bdc_dynpro      using 'SAPLIMR0' '4210'.
perform bdc_field       using 'BDC_CURSOR'
                              'IMRG-POINT(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ADAL'.
perform bdc_field       using 'RIMR0-DFTIM'
                              DFTIM_001.
*                               SY-UZEIT.
perform bdc_field       using 'RIMR0-DFDAT'
*                              DFDAT_006.
                               l_date.
perform bdc_field       using 'RIMR0-DFRDR'
*                              DFRDR_007.
                               SY-UNAME.
perform bdc_dynpro      using 'SAPLIMR0' '4210'.
perform bdc_field       using 'BDC_CURSOR'
                              'RIMR0-RDCNT(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'RIMR0-DFTIM'
                               DFTIM_001.
*                               SY-UZEIT.
perform bdc_field       using 'RIMR0-DFDAT'
*                              DFDAT_009.
                               l_date.
perform bdc_field       using 'RIMR0-DFRDR'
*                              DFRDR_010.
                                SY-UNAME.
*perform bdc_field       using 'RIMR0-RDCNT(01)'
**                              RDCNT_01_011.
*                               Z_ZSTROKE_COUNT.
perform bdc_field       using 'RIMR0-CDIFC(01)'
                              RDCNT_01_011.
*                               Z_ZSTROKE_COUNT.

perform bdc_dynpro      using 'SAPLIMR0' '4210'.
perform bdc_field       using 'BDC_CURSOR'
                              'RIMR0-FLGSL(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
*perform bdc_field       using 'RIMR0-DFTIM'
*                              DFTIM_012.
*perform bdc_field       using 'RIMR0-DFDAT'
*                              DFDAT_013.
*perform bdc_field       using 'RIMR0-DFRDR'
*                              DFRDR_014.
perform bdc_transaction tables messtab
using                         'IK22'
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
