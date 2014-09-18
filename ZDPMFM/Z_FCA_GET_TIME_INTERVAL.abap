FUNCTION Z_FCA_GET_TIME_INTERVAL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(S_DATE) LIKE  SY-DATUM
*"     REFERENCE(S_TIME) LIKE  SY-UZEIT
*"     REFERENCE(E_DATE) LIKE  SY-DATUM
*"     REFERENCE(E_TIME) LIKE  SY-UZEIT
*"  EXPORTING
*"     REFERENCE(INTERVAL)
*"----------------------------------------------------------------------
  DATA: S_POINT TYPE  I,
        E_POINT TYPE  I.

  CALL FUNCTION 'DATE_TIME_CONVERT'
       EXPORTING
            DATE          = S_DATE
            TIME          = S_TIME
       IMPORTING
            POINT_IN_TIME = S_POINT.


  CALL FUNCTION 'DATE_TIME_CONVERT'
       EXPORTING
            DATE          = E_DATE
            TIME          = E_TIME
       IMPORTING
            POINT_IN_TIME = E_POINT.

  INTERVAL  = E_POINT - S_POINT.
  INTERVAL = INTERVAL / 60.

ENDFUNCTION.
