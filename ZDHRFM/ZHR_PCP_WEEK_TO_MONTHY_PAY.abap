FUNCTION ZHR_PCP_WEEK_TO_MONTHY_PAY.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(BDATE) TYPE  D
*"     REFERENCE(EDATE) TYPE  D
*"     REFERENCE(PAYMENT) TYPE  MAXBT
*"  EXPORTING
*"     REFERENCE(C_PAYMENT) TYPE  MAXBT
*"     REFERENCE(N_PAYMENT) TYPE  MAXBT
*"----------------------------------------------------------------------
  DATA : WA_TMP_DATE TYPE D.
  DATA : IT_HOLIDAYS	LIKE	ISCAL_DAY OCCURS 0 WITH HEADER LINE.
  data : wa_cday type i,
         wa_nday type i,
         wa_count_hday type i,
         WA_TOTAL_HDAY TYPE I,
         WA_ERROR TYPE I.


WA_TMP_DATE = BDATE.
  DO .
    IF WA_ERROR > 90.
     EXIT.
    ENDIF.
    WA_ERROR = WA_ERROR + 1.
   clear : it_holidays , it_holidays[], WA_COUNT_HDAY.

    CALL FUNCTION 'HOLIDAY_GET'
         EXPORTING
              HOLIDAY_CALENDAR = 'HM'
              FACTORY_CALENDAR = 'HM'
              DATE_FROM        = WA_TMP_DATE
              DATE_TO          = WA_TMP_DATE
         TABLES
              HOLIDAYS         = IT_HOLIDAYS.

    describe table it_holidays lines wa_count_hday .

    if  wa_count_hday EQ 0.
       if wa_tmp_date(6) = bdate(6).
          wa_cday = wa_cday + 1.
       else.
          wa_nday = wa_nday + 1.
       endif.

    endif.

    IF EDATE = WA_TMP_DATE .
      EXIT.
    ENDIF.

    WA_TMP_DATE = WA_TMP_DATE +  1.

  ENDDO.


WA_TOTAL_HDAY =  WA_CDAY + WA_NDAY .

 C_PAYMENT =  ( PAYMENT / WA_TOTAL_HDAY ) * WA_CDAY .
 N_PAYMENT =  ( PAYMENT / WA_TOTAL_HDAY ) * WA_NDAY .


ENDFUNCTION.
