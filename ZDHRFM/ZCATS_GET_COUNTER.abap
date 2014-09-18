FUNCTION ZCATS_GET_COUNTER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(EXTERNAL_CALL) DEFAULT SPACE
*"       TABLES
*"              I_CATSDBCOMM STRUCTURE  CATSDBCOMM
*"              I_CATSCOUNT STRUCTURE  CATS_COUNT
*"----------------------------------------------------------------------
  REFRESH I_CATSCOUNT.
* get the new counters in the correct order
  SORT I_CATSDBCOMM BY COUNTER.
  LOOP AT I_CATSDBCOMM WHERE ACTION EQ ACTION-INSERT.
    I_CATSCOUNT-COUNTER = I_CATSDBCOMM-COUNTER.
    PERFORM GET_NEXT_COUNTER USING I_CATSDBCOMM-COUNTER YX
                                   EXTERNAL_CALL.
    IF I_CATSDBCOMM-LONGTEXT = YX.
      I_CATSCOUNT-NEWCOUNTER = I_CATSDBCOMM-COUNTER.
      APPEND I_CATSCOUNT.
    ENDIF.
    MODIFY I_CATSDBCOMM.
  ENDLOOP.
* sort again to restore the original status
  SORT I_CATSDBCOMM BY MANDT WORKDATE COUNTER.

ENDFUNCTION.
*eject
