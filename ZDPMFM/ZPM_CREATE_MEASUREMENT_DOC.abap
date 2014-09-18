FUNCTION ZPM_CREATE_MEASUREMENT_DOC.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(ZDATE) LIKE  SY-DATUM
*"----------------------------------------------------------------------

Tables: ZPM_DIE_MAINT, EQUI.
DATA: MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.
DATA: Z_ZSTROKE_COUNT(132) TYPE C.

DATA: IT_ZPM_DIE_MAINT LIKE ZPM_DIE_MAINT OCCURS 0 WITH HEADER LINE.
data: zequnr like equi-equnr.

SELECT * FROM ZPM_DIE_MAINT INTO TABLE IT_ZPM_DIE_MAINT
WHERE ZRUN_DATE = ZDATE.

loop at it_zpm_die_maint.
   select single equnr into zequnr from equi where matnr =
it_zpm_die_maint-zdie_number.

MOVE it_zpm_die_maint-ZSTROKE_COUNT TO Z_ZSTROKE_COUNT.

CALL FUNCTION 'ZPM_CREATE_MEASUREMENT_READ'
 EXPORTING
   CTU                = 'X'
   MODE               = 'N'
   UPDATE             = 'L'
*   GROUP              =
*   USER               =
*   KEEP               =
*   HOLDDATE           =
*   NODATA             = '/'
   DFTIM_001          = '14:03:49'
   DFDAT_002          = '2007/03/01'
   DFRDR_003          = '100565'
   EQUNR_004          = ZEQUNR
   DFTIM_005          = '14:03:49'
   DFDAT_006          = '2007/03/01'
   DFRDR_007          = '100565'
   DFTIM_008          = '14:03:49'
   DFDAT_009          = '2007/03/01'
   DFRDR_010          = '100565'
   RDCNT_01_011       = Z_ZSTROKE_COUNT
   DFTIM_012          = '14:03:49'
   DFDAT_013          = '2007/03/01'
   DFRDR_014          = '100565'
* IMPORTING
*   SUBRC              =
 TABLES
   MESSTAB            = MESSTAB
          .

endloop.

ENDFUNCTION.
