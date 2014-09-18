FUNCTION ZCATS_CREATE_NEW_RECORD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(N_CATSDB) LIKE  CATSDBCOMM STRUCTURE  CATSDBCOMM
*"             VALUE(O_CATSDB) LIKE  CATSDBCOMM STRUCTURE  CATSDBCOMM
*"       EXPORTING
*"             VALUE(N_CATSDB) LIKE  CATSDBCOMM STRUCTURE  CATSDBCOMM
*"       TABLES
*"              I_CATSDBCOMM STRUCTURE  CATSDBCOMM
*"----------------------------------------------------------------------
  DATA: HELP_COUNTER LIKE CATSDB-COUNTER.

  PERFORM GET_NEXT_COUNTER USING HELP_COUNTER SPACE SPACE.
  CLEAR I_CATSDBCOMM.
  MOVE-CORRESPONDING N_CATSDB TO I_CATSDBCOMM.
  I_CATSDBCOMM-COUNTER = HELP_COUNTER.
  I_CATSDBCOMM-MANDT = SY-MANDT.
  I_CATSDBCOMM-ACTION = ACTION-INSERT.
  I_CATSDBCOMM-STATUS = STATUS-LOCK.
  IF O_CATSDB-STATUS = STATUS-APPR.
* move reference number to old record into new one
    I_CATSDBCOMM-REFCOUNTER = O_CATSDB-COUNTER.
  ENDIF.
* put the longtext properly
  IF I_CATSDBCOMM-LONGTEXT CA '0123456789'.
    I_CATSDBCOMM-LONGTEXT = YX.
  ENDIF.

  READ TABLE I_CATSDBCOMM TRANSPORTING NO FIELDS
                     WITH KEY MANDT    = SY-MANDT
                              WORKDATE = N_CATSDB-WORKDATE
                              COUNTER  = HELP_COUNTER
                              BINARY SEARCH.
  INSERT I_CATSDBCOMM INDEX SY-TABIX.
  IF SY-SUBRC NE 0.
    MESSAGE X030.
  ENDIF.
* move new record to new_i_catsdbcomm:
*  these values are needed e.g. after
* the deletion of records
  MOVE I_CATSDBCOMM TO N_CATSDB.

ENDFUNCTION.
*eject
