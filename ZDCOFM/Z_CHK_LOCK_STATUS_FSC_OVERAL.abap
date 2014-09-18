FUNCTION z_chk_lock_status_fsc_overal.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(KOKRS) TYPE  KOKRS
*"     VALUE(BDATJ) TYPE  BDATJ
*"     VALUE(KALKA) TYPE  CK_KALKA
*"     VALUE(ID) TYPE  ZID1
*"  EXPORTING
*"     VALUE(LOCKED) TYPE  CHAR1
*"----------------------------------------------------------------------
  DATA w_lock TYPE ztcou104lock.

  SELECT SINGLE  * INTO w_lock
                   FROM ztcou104lock
                  WHERE kokrs EQ kokrs
                    AND bdatj EQ bdatj
                    AND kalka EQ kalka
                    AND id    EQ id
                    AND (
                          lock00 EQ 'X'
                       OR lock01 EQ 'X'
                       OR lock02 EQ 'X'
                       OR lock03 EQ 'X'
                       OR lock04 EQ 'X'
                       OR lock05 EQ 'X'
                       OR lock06 EQ 'X'
                       OR lock07 EQ 'X'
                       OR lock08 EQ 'X'
                       OR lock09 EQ 'X'
                       OR lock10 EQ 'X'
                       OR lock11 EQ 'X'
                       OR lock12 EQ 'X' ).

  IF sy-subrc EQ 0.
    locked = 'X'.
  ELSE.
    locked = ' '.
  ENDIF.

ENDFUNCTION.
