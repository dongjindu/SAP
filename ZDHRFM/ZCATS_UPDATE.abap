FUNCTION ZCATS_UPDATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(COMMIT_WORK) TYPE  CATSFIELDS-YESORNO
*"         DEFAULT 'X'
*"             REFERENCE(CATS_TEXT_FORMAT) TYPE  CATS_TEXT_FORMAT
*"         OPTIONAL
*"             REFERENCE(GET_COUNTER) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT 'X'
*"             REFERENCE(UPDATE_DATABASE) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT 'X'
*"       TABLES
*"              CATSDB_IN STRUCTURE  CATSDBCOMM
*"              LONGTEXT STRUCTURE  LONGTEXT_EXT
*"----------------------------------------------------------------------
  DATA: UEXT_CALL VALUE YX.
  REFRESH ICATSDB.
  CLEAR ICATSDB.

  ICATSDB[] = CATSDB_IN[].


  PERFORM LAST_STEPS_AND_SAVE TABLES
                                  LONGTEXT
                                USING UEXT_CALL
                                      COMMIT_WORK
                                      CATS_TEXT_FORMAT
                                      get_counter
                                      update_database.





ENDFUNCTION.
