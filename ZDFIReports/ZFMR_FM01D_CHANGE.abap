*&---------------------------------------------------------------------*
*& Report  ZFMR_FM01D_CHANGE
*&
*&---------------------------------------------------------------------*
*&   Table [FM01D] field value change. imsi.
*&   2011.09.15  by yn.kim
*&---------------------------------------------------------------------*

REPORT  ZFMR_FM01D_CHANGE.

tables: fm01d.

select single * from fm01d
 where fikrs = 'H201'
   and applc = 'A'.

if sy-subrc = 0.
***  fm01d-FM_PAYM_FISTL = 'HMMA'.
***  fm01d-FM_PAYM_FIPEX = 'DUMMY'.

  update fm01d set FM_PAYM_FISTL = 'HMMA'   "'HMMA'
                   FM_PAYM_FIPEX = 'DUMMY'   "'DUMMY'
                where fikrs = 'H201'
                  and applc = 'A'.
  if sy-subrc = 0.
    message i000(zmfi) with 'Value Change Success...!!!'.
  endif.
endif.
