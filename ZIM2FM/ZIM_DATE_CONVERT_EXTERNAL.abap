FUNCTION ZIM_DATE_CONVERT_EXTERNAL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  SY-DATUM
*"  EXPORTING
*"     VALUE(E_DATE) TYPE  C
*"  EXCEPTIONS
*"      NO_DATE
*"      NO_MATCH
*"----------------------------------------------------------------------
DATA : TEMP_YEAR(002)  TYPE C,
       TEMP_MONTH(003) TYPE C,
       TEMP_DAY(002)   TYPE C.


  CLEAR : E_DATE.


*-----------------------------------------------------------------------
* CHECK DATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
       EXPORTING
          DATE                      = I_DATE
       EXCEPTIONS
          PLAUSIBILITY_CHECK_FAILED = 4.

  IF SY-SUBRC NE 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                RAISING NO_DATE.
  ENDIF.

  TEMP_YEAR  = I_DATE+2(2).
  TEMP_DAY   = I_DATE+6(2).


  CASE I_DATE+4(2).
     WHEN  '01'.
        TEMP_MONTH = 'Jan'.
     WHEN  '02'.
        TEMP_MONTH = 'Feb'.
     WHEN  '03'.
        TEMP_MONTH = 'Mar'.
     WHEN  '04'.
        TEMP_MONTH = 'Apr'.
     WHEN  '05'.
        TEMP_MONTH = 'May'.
     WHEN  '06'.
        TEMP_MONTH = 'Jun'.
     WHEN  '07'.
        TEMP_MONTH = 'Jul'.
     WHEN  '08'.
        TEMP_MONTH = 'Aug'.
     WHEN  '09'.
        TEMP_MONTH = 'Sep'.
     WHEN  '10'.
        TEMP_MONTH = 'Oct'.
     WHEN  '11'.
        TEMP_MONTH = 'Nov'.
     WHEN  '12'.
        TEMP_MONTH = 'Dec'.
     WHEN  OTHERS.
        RAISE   NO_MATCH.
  ENDCASE.

  CONCATENATE   TEMP_DAY '-' TEMP_MONTH '-' TEMP_YEAR INTO E_DATE.


ENDFUNCTION.
