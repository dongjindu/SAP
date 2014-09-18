FUNCTION ZIM_BEFORE_N_MONTH_DATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(DATE) LIKE  SY-DATUM
*"     VALUE(W_MONTH) TYPE  I
*"  EXPORTING
*"     VALUE(W_OUT_DATE)
*"  EXCEPTIONS
*"      PLAUSIBILITY_CHECK_FAILED
*"----------------------------------------------------------------------
  DATA : W_YEAR    TYPE   I.
  DATA : W_MOD     TYPE   I.
  DATA : W_TMP     TYPE   I.
  DATA : W_DAY     TYPE   I.

*-----------------------------------------------------------------------
* CHECK DATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
       EXPORTING
          DATE                      = DATE
       EXCEPTIONS
          PLAUSIBILITY_CHECK_FAILED = 4.

  IF SY-SUBRC NE 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4
                RAISING PLAUSIBILITY_CHECK_FAILED.
  ENDIF.

  W_DAY = DATE+6(2).
  W_MOD = W_MONTH  MOD   12.
  W_YEAR = ( W_MONTH - W_MOD )  /  12.

  DATE(4)      = DATE(4)   + W_YEAR.
  W_TMP = DATE+4(2) + W_MOD.
  IF W_TMP > 12.
     DATE(4)   = DATE(4)   +    1.
     W_TMP     = W_TMP     -    12.
  ENDIF.

  DATE+4(2)    =  W_TMP.

  IF DATE+6(2) EQ 1.
     IF DATE+4(2) EQ 1.
        DATE(4)      =  DATE(4) - 1.
        DATE+4(4)    =  '1231'.
     ELSE.
        DATE+4(2)    =  DATE+4(2) - 1.
        DATE+6(2)    =  '31'.
     ENDIF.
  ELSE.
     DATE+6(2)    =  DATE+6(2) - 1.
  ENDIF.

  DO.
     CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
          EXPORTING
               DATE                      = DATE
          EXCEPTIONS
               PLAUSIBILITY_CHECK_FAILED = 4.

     IF SY-SUBRC EQ 0.
        EXIT.
     ELSE.
        IF DATE+6(2) EQ 1.
           IF DATE+4(2) EQ 1.
               DATE(4)      =  DATE(4) - 1.
               DATE+4(4)    =  '1231'.
           ELSE.
               DATE+4(2)    =  DATE+4(2) - 1.
               DATE+6(2)    =  '31'.
           ENDIF.
        ELSE.
           DATE+6(2)    =  DATE+6(2) - 1.
        ENDIF.
     ENDIF.

  ENDDO.

  W_OUT_DATE = DATE.

ENDFUNCTION.
