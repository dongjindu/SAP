REPORT ZMMR_GET_PREV_MONTH_LASTDAY .

*******************************************************************
*  Date         Developer   Request    Description
* 06/05/2007    Manju       UD1K940752 Program to populate last
*                                      date of 2nd previous month
*******************************************************************

TABLES: TVARV.

DATA:   DATUM LIKE SY-DATUM,
        l_date like sy-datum ,
        p_mdate like sy-datum.

parameters : p_date like sy-datum obligatory default sy-datum.

TVARV-TYPE = 'P'.                      "Parameter
TVARV-NUMB = 0.                                             "1 Eintrag.


* Get 2nd months Date

CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    DATE            = p_date
    DAYS            = '00'
    MONTHS          = '02'
    SIGNUM          = '-'
    YEARS           = '00'
 IMPORTING
  CALC_DATE       =  l_date.

* Get Last Day of 2nd Month

CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
  EXPORTING
    DAY_IN                  =  l_date
 IMPORTING
   LAST_DAY_OF_MONTH       =   p_mdate
 EXCEPTIONS
   DAY_IN_NO_DATE          = 1
   OTHERS                  = 2
          .

* Populate  TVARV Table with last day of 2nd previous
* month from the given date

TVARV-NAME = 'Z_PERV_2ND_MONTH_LDAY'.
TVARV-LOW  = p_mdate.
modify TVARV.
