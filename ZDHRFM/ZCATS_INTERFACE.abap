* 4.6b
* 15071999 YIKP99K042874 set feeatsave for targethours check

FUNCTION ZCATS_INTERFACE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(PROFILE) LIKE  TCATS-VARIANT
*"             VALUE(TESTRUN) LIKE  CATSFIELDS-YESORNO DEFAULT 'X'
*"             VALUE(TESTRUN_COUNTER) LIKE  CATSFIELDS-YESORNO
*"         OPTIONAL
*"             VALUE(REL_DATA) LIKE  CATSFIELDS-YESORNO DEFAULT SPACE
*"             VALUE(AGENT) LIKE  SWHACTOR STRUCTURE  SWHACTOR
*"         OPTIONAL
*"             VALUE(COMMIT_WORK) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT 'X'
*"             VALUE(WORKFLOW_POPUP) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT SPACE
*"             VALUE(CATS_TEXT_FORMAT) TYPE  CATS_TEXT_FORMAT
*"         OPTIONAL
*"       TABLES
*"              I_CATSDB_EXT STRUCTURE  CATSDB_EXT
*"              CHECK_MESSAGES STRUCTURE  MESG
*"              I_CATSDB_EXT_OUT STRUCTURE  CATSDB_EXT OPTIONAL
*"              WORKFLOW_TEXT STRUCTURE  SOLISTI1 OPTIONAL
*"              LONGTEXT STRUCTURE  LONGTEXT_EXT OPTIONAL
*"----------------------------------------------------------------------
  DATA: USUBRC LIKE SY-SUBRC.
  DATA: UEXT_CALL VALUE YX.
  DATA: UCHECK_CALL_MODE(1) TYPE C VALUE 'I'.
  DATA: NOT_RELEASED TYPE C.           "YIKALRK137235
  DATA: UPDATE_DATABASE TYPE C.
  DATA: GET_COUNTER TYPE C.
* allowed pernr has to be cleared from the last run
  refresh allowed_pernr.
* set the mode
  MODE = FMODE-MAINTAIN.

* activate message handler
* perform activate_messages tables check_messages.   new ess

* general settings
  PERFORM EXT_GENERAL_SETTINGS USING PROFILE.
  IF REL_DATA = YX.                    "YIKP99K042874
    TCATS-FREEATSAVE = YX.             "YIKP99K042874
  ENDIF.                               "YIKP99K042874

* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* Check Textformat for Longtext
  PERFORM CHECK_TEXT_FORMAT TABLES LONGTEXT
                            USING CATS_TEXT_FORMAT.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* Analyze data and find all personel numbers and the whole date range
  PERFORM EXT_GET_PERNR_AND_DATES TABLES I_CATSDB_EXT.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.


* perform check_authority
  PERFORM EXT_CHECK_AUTHORITY.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* fill table allowed_pernr
  PERFORM FILL_TABLE_ALLOWED_PERNR.
* reduce input information to allowed records
  PERFORM EXT_REDUCE_INPUT TABLES I_CATSDB_EXT.
* fill internal table icatsdb with relevant records
  PERFORM EXT_FILL_ICATSDB TABLES I_CATSDB_EXT.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* enqueue
  PERFORM EXT_ENQUEUE_PERNR.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* enrich external input
  PERFORM EXT_ENRICH_INPUT TABLES I_CATSDB_EXT CHECK_MESSAGES
                           USING  UEXT_CALL.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* check external input
  PERFORM EXT_CHECK_INPUT TABLES I_CATSDB_EXT CHECK_MESSAGES.

* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* compare new record with old record
  PERFORM EXT_TREAT_RECORDS TABLES I_CATSDB_EXT
                                   LONGTEXT.
* last steps to save data finally
  PERFORM GLOBAL_CHECKS TABLES ICATSDB USING UCHECK_CALL_MODE.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* free data, if defined in profile
* perform free_at_save using uext_call.                 "YIKALRK137235
* DWI Note 780025
  IF REL_DATA NE YX.                                    "DWI Note 780025
    PERFORM FREE_AT_SAVE USING UEXT_CALL NOT_RELEASED.    "YIKALRK137235
  ENDIF.                                                "DWI Note 780025
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* free data, if explicitely asked by user
  IF REL_DATA = YX.
    PERFORM EXT_FREE_AT_SAVE TABLES I_CATSDB_EXT USING UEXT_CALL.

    PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
    CHECK USUBRC = 0.
  ENDIF.
* workflow
  IF NOT TCATS-WF_APPR IS INITIAL AND TESTRUN IS INITIAL AND
     TESTRUN_COUNTER IS INITIAL AND
      ( FREE_DATA = YX OR TCATS-FREEATSAVE = YX )
      AND TCATS-APPROVAL EQ YX AND
      ( NOT_RELEASED IS INITIAL or FREE_DATA = YX ).          "YIK
    PERFORM WORKFLOW_APPROVAL TABLES WORKFLOW_TEXT
                              USING USUBRC AGENT UEXT_CALL
                                    WORKFLOW_POPUP.
    PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
    CHECK USUBRC = 0.
  ENDIF.
* save data
  IF TESTRUN IS INITIAL.

    GET_COUNTER = YX.

    IF NOT TESTRUN_COUNTER IS INITIAL.
      UPDATE_DATABASE = SPACE.
    ELSE.
      UPDATE_DATABASE = YX.
    ENDIF.

    PERFORM LAST_STEPS_AND_SAVE TABLES
                                  LONGTEXT
                                USING UEXT_CALL COMMIT_WORK
                                      CATS_TEXT_FORMAT
                                      GET_COUNTER
                                      UPDATE_DATABASE.

  ENDIF.
* get return values
  PERFORM GET_CATSDB_EXT TABLES I_CATSDB_EXT
                                I_CATSDB_EXT_OUT            "YIK
                                ICATSDB
                          USING TESTRUN.
* dequeue
  PERFORM EXT_DEQUEUE_PERNR.           "new ess
* check messages finally
  PERFORM EXT_CHECK_MESSAGES_FINALLY TABLES CHECK_MESSAGES.

ENDFUNCTION.
*eject
