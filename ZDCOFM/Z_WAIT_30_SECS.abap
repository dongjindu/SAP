FUNCTION Z_WAIT_30_SECS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"----------------------------------------------------------------------

DATA: ZTIME LIKE SY-UZEIT..

GET TIME.

ZTIME = SY-UZEIT + 30.

DO.
  GET TIME.
  IF SY-UZEIT >= ZTIME.
     EXIT.
   ENDIF.
ENDDO.


ENDFUNCTION.
