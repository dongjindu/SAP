FUNCTION Z_CO_GET_QUATER_START_END.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(TODAY) TYPE  SY-DATUM DEFAULT SY-DATUM
*"  EXPORTING
*"     REFERENCE(START_DATE) TYPE  SY-DATUM
*"     REFERENCE(END_DATE) TYPE  SY-DATUM
*"----------------------------------------------------------------------

  DATA : $MONTH(2) TYPE N,
         $S_M_Q(2) TYPE N,
         $MOD TYPE P,
         MON TYPE I,
         $MON(2) TYPE N,
         $TODAY LIKE TODAY,
         $IX LIKE SY-INDEX.

  IF TODAY IS INITIAL.
    $TODAY = SY-DATUM.
  ELSE.
    $TODAY = TODAY.
  ENDIF.

  $MONTH = TODAY+4(2).

  MON = $MONTH.
  DO 4 TIMES.
    MON = MON - 3.
    $IX = SY-INDEX.
    IF MON <= 0.
      EXIT.
    ENDIF.
  ENDDO.

  SUBTRACT 1 FROM $IX.

  MON = $IX * 3 + 1.$MON = MON.
  CONCATENATE TODAY(4) $MON '01' INTO START_DATE.

  ADD 3 TO MON.$MON = MON.
  IF MON > 12.
    CONCATENATE TODAY(4) '1231' INTO END_DATE.
  ELSE.
    CONCATENATE TODAY(4) $MON '01' INTO END_DATE.
    END_DATE = END_DATE - 1.
  ENDIF.

ENDFUNCTION.
