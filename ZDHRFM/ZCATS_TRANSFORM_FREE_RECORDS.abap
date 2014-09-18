* Hot package
*YCY note374290 170101 CATSDBCOMM is not sorted propperly
*                      and order is destroyed by append cmd
*4.5B
*YIKP45K076266 030299 deleted records are approved


FUNCTION ZCATS_TRANSFORM_FREE_RECORDS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(EXTERNAL_CALL) DEFAULT SPACE
*"       TABLES
*"              I_CATSDBCOMM STRUCTURE  CATSDBCOMM
*"              I_CATSDB_OUT STRUCTURE  CATSDB
*"       EXCEPTIONS
*"              INTERNAL_ERROR
*"----------------------------------------------------------------------

  DATA: HELP_ICATSDBCOMM LIKE CATSDBCOMM.
  DATA: HELP_INDEX LIKE SY-TABIX.
  DATA: CATSINITIAL TYPE CATSH_INITIAL.                     "YIK
  DATA: CATSTIMES TYPE CATSH_TIMES.                         "YIK


  REFRESH I_CATSDB_OUT. CLEAR I_CATSDB_OUT.

  LOOP AT I_CATSDBCOMM WHERE NOT ACTION IS INITIAL
                      AND ACTION NE 'D'"YIKP45K076266
                      AND   STATUS = STATUS-FREE.
* set status to status approved
    HELP_INDEX = SY-TABIX.
    I_CATSDBCOMM-STATUS = STATUS-APPR.
    HELP_ICATSDBCOMM = I_CATSDBCOMM.
* set status void to ancestors
    IF NOT ( I_CATSDBCOMM-REFCOUNTER IS INITIAL ).
      READ TABLE I_CATSDBCOMM
                        WITH KEY MANDT = SY-MANDT
                        WORKDATE = I_CATSDBCOMM-WORKDATE
                        COUNTER = I_CATSDBCOMM-REFCOUNTER. "note 374290
*                        BINARY SEARCH.                    "note 374290
      IF SY-SUBRC NE 0.
* it could be the case that personel number has been changed
        SELECT SINGLE * FROM CATSDB INTO I_CATSDBCOMM
                                          WHERE COUNTER =
                                          HELP_ICATSDBCOMM-REFCOUNTER.
        IF SY-SUBRC NE 0 OR I_CATSDBCOMM-STATUS NE STATUS-CHAN.
          MESSAGE X030 RAISING INTERNAL_ERROR.
        ELSE.
          I_CATSDBCOMM-STATUS = STATUS-VOID.
          I_CATSDBCOMM-ACTION = ACTION-UPDATE.
          APPEND I_CATSDBCOMM.
          MOVE-CORRESPONDING I_CATSDBCOMM TO I_CATSDB_OUT.
          APPEND I_CATSDB_OUT.
        ENDIF.
      ELSE.
        IF I_CATSDBCOMM-STATUS NE STATUS-CHAN.
          MESSAGE X030.
        ENDIF.
        I_CATSDBCOMM-STATUS = STATUS-VOID.
        I_CATSDBCOMM-ACTION = ACTION-UPDATE.
        MODIFY I_CATSDBCOMM INDEX SY-TABIX.
        MOVE-CORRESPONDING I_CATSDBCOMM TO I_CATSDB_OUT.
        APPEND I_CATSDB_OUT.
      ENDIF.
    ENDIF.
* set statistical info
    HELP_ICATSDBCOMM-APNAM = SY-UNAME.
    HELP_ICATSDBCOMM-APDAT = SY-DATUM.
* get the document number
    PERFORM GET_DOCUMENT_NUMBER USING HELP_ICATSDBCOMM-BELNR
                                      EXTERNAL_CALL
                                      HELP_ICATSDBCOMM-ROW. "LUX
    MODIFY I_CATSDBCOMM FROM HELP_ICATSDBCOMM INDEX HELP_INDEX.
* if the record is a successor of an approved record and if the record
* contains only zeros, don't feed the interface
    MOVE-CORRESPONDING  HELP_ICATSDBCOMM TO CATSINITIAL.
    MOVE-CORRESPONDING  HELP_ICATSDBCOMM TO CATSTIMES.
    IF NOT CATSINITIAL IS INITIAL OR
       NOT CATSTIMES CO ' 0'.                               "YIK
      MOVE-CORRESPONDING HELP_ICATSDBCOMM TO I_CATSDB_OUT.
      APPEND I_CATSDB_OUT.
    ELSE.
      IF HELP_ICATSDBCOMM-REFCOUNTER IS INITIAL.
* this would be a bug in the program
        MESSAGE X030.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
*eject
