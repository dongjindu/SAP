FUNCTION z_chk_lock_status_fsc.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(KOKRS) TYPE  KOKRS
*"     VALUE(BDATJ) TYPE  BDATJ
*"     VALUE(KALKA) TYPE  CK_KALKA
*"     VALUE(ID) TYPE  ZID1
*"     VALUE(POPER) TYPE  POPER OPTIONAL
*"  EXPORTING
*"     VALUE(LOCKED) TYPE  CHAR1
*"  EXCEPTIONS
*"      INVALID_POPER
*"----------------------------------------------------------------------

  DATA $ix(2) TYPE n.
  FIELD-SYMBOLS : <to> .
  DATA fname(30).
  DATA w_lock TYPE ztcou104lock.
  $ix = poper+1(2).

  IF $ix > 0 AND $ix < 13.

    SELECT SINGLE  * INTO w_lock
                     FROM ztcou104lock
                    WHERE kokrs EQ kokrs
                      AND bdatj EQ bdatj
                      AND kalka EQ kalka
                      AND id    EQ id .
    IF sy-subrc EQ 0.
      CONCATENATE 'W_LOCK-LOCK' $ix INTO fname.
      ASSIGN (fname) TO <to>.
      IF <to> EQ 'X'.
        locked = 'X'.
      ELSE.
        locked = ' '.
      ENDIF.
    ELSE.
      locked = ' '.
    ENDIF.
  ELSE.
    RAISE invalid_poper.
  ENDIF.

ENDFUNCTION.
