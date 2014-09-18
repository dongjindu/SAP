FUNCTION ZIM_DATE_CONVERT_INTERNAL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  C
*"  EXPORTING
*"     VALUE(E_DATE) TYPE  SY-DATUM
*"  EXCEPTIONS
*"      NO_DATE
*"      NO_MATCH
*"----------------------------------------------------------------------
DATA : TEMP_YEAR(002)  TYPE C,
       TEMP_MONTH(002) TYPE C,
       TEMP_DAY(002)   TYPE C.


  CLEAR : E_DATE.

  TEMP_YEAR  = I_DATE+7(2).
  TEMP_DAY   = I_DATE(2).

  CASE I_DATE+3(3).
     WHEN  'JAN'.
        TEMP_MONTH = '01'.
     WHEN  'FEB'.
        TEMP_MONTH = '02'.
     WHEN  'MAR'.
        TEMP_MONTH = '03'.
     WHEN  'APR'.
        TEMP_MONTH = '04'.
     WHEN  'MAY'.
        TEMP_MONTH = '05'.
     WHEN  'JUN'.
        TEMP_MONTH = '06'.
     WHEN  'JUL'.
        TEMP_MONTH = '07'.
     WHEN  'AUG'.
        TEMP_MONTH = '08'.
     WHEN  'SEP'.
        TEMP_MONTH = '09'.
     WHEN  'OCT'.
        TEMP_MONTH = '10'.
     WHEN  'NOV'.
        TEMP_MONTH = '11'.
     WHEN  'DEC'.
        TEMP_MONTH = '12'.
     WHEN  OTHERS.
        RAISE   NO_MATCH.
  ENDCASE.


  CONCATENATE  TEMP_YEAR TEMP_MONTH TEMP_DAY  INTO E_DATE.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
       EXPORTING
          DATE_EXTERNAL = E_DATE
       IMPORTING
          DATE_INTERNAL = E_DATE.


*-----------------------------------------------------------------------
* CHECK DATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
       EXPORTING
          DATE                      = E_DATE
       EXCEPTIONS
          PLAUSIBILITY_CHECK_FAILED = 4.

  IF SY-SUBRC NE 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                RAISING NO_DATE.
  ENDIF.


ENDFUNCTION.
