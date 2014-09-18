FUNCTION ZIM_BDC_DATE_CONVERT_EXTERNAL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DATE) TYPE  SY-DATUM
*"  EXPORTING
*"     VALUE(E_DATE) TYPE  SY-DATUM
*"  EXCEPTIONS
*"      NO_DATE
*"      NO_MATCH
*"----------------------------------------------------------------------

  CLEAR : E_DATE.

  CLEAR : USR01.
  SELECT SINGLE * FROM USR01
  WHERE  BNAME    EQ   SY-UNAME.

  CASE USR01-DATFM.
     WHEN  '1'.
        CONCATENATE I_DATE+6(2)  I_DATE+4(2)  I_DATE(4) INTO E_DATE.
     WHEN  '2'.
        CONCATENATE I_DATE+4(2)  I_DATE+6(2)  I_DATE(4) INTO E_DATE.
     WHEN  '3'.
        CONCATENATE I_DATE+4(2)  I_DATE+6(2)  I_DATE(4) INTO E_DATE.
     WHEN  OTHERS.
        E_DATE  =  I_DATE.
  ENDCASE.

ENDFUNCTION.
