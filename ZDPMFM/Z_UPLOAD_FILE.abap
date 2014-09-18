FUNCTION Z_UPLOAD_FILE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(FILENAME) LIKE  RLGRAP-FILENAME DEFAULT SPACE
*"  TABLES
*"      DATA_TAB
*"----------------------------------------------------------------------

  DATA: GV_ERRMSG(100).

* perform write_to_server
  OPEN DATASET FILENAME FOR OUTPUT IN TEXT MODE "IN BINARY MODE
                                   MESSAGE GV_ERRMSG.
*  IF SY-SUBRC <> 0.
*    MESSAGE I000(ZZ) WITH GV_ERRMSG.
*    EXIT.
*  ENDIF.
  LOOP AT DATA_TAB.
    TRANSFER DATA_TAB TO FILENAME.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
  ENDLOOP.

  CLOSE DATASET FILENAME.




ENDFUNCTION.
