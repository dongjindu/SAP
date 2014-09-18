FUNCTION z_fmm_6001_01_time_stamp.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  CHANGING
*"     REFERENCE(CH_ERDAT) LIKE  ZSCA_TIME_STAMP-ERDAT
*"     REFERENCE(CH_ERZET) LIKE  ZSCA_TIME_STAMP-ERZET
*"     REFERENCE(CH_ERNAM) LIKE  ZSCA_TIME_STAMP-ERNAM
*"     REFERENCE(CH_AEDAT) LIKE  ZSCA_TIME_STAMP-AEDAT
*"     REFERENCE(CH_AEZET) LIKE  ZSCA_TIME_STAMP-AEZET
*"     REFERENCE(CH_AENAM) LIKE  ZSCA_TIME_STAMP-AENAM
*"----------------------------------------------------------------------
*  IF ch_erdat IS INITIAL AND
*     ch_erzet IS INITIAL AND
*     ch_ernam IS INITIAL.
  IF ch_erdat IS INITIAL.
    ch_erdat = sy-datum.
    ch_erzet = sy-uzeit.
    ch_ernam = sy-uname.
  ELSE.
    ch_aedat = sy-datum.
    ch_aezet = sy-uzeit.
    ch_aenam = sy-uname.
  ENDIF.

ENDFUNCTION.
