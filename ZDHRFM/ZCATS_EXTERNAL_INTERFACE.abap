FUNCTION ZCATS_EXTERNAL_INTERFACE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(PROFILE) LIKE  TCATS-VARIANT
*"             VALUE(TESTRUN) LIKE  CATSFIELDS-YESORNO DEFAULT 'X'
*"             VALUE(TESTRUN_COUNTER) LIKE  CATSFIELDS-YESORNO
*"         OPTIONAL
*"             VALUE(RELEASE_DATA) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT SPACE
*"             VALUE(AGENT) LIKE  SWHACTOR STRUCTURE  SWHACTOR
*"         OPTIONAL
*"             VALUE(COMMIT_WORK) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT 'X'
*"             VALUE(WORKFLOW_POPUP) LIKE  CATSFIELDS-YESORNO
*"         DEFAULT SPACE
*"             VALUE(CATS_TEXT_FORMAT) TYPE  CATS_TEXT_FORMAT
*"         OPTIONAL
*"       EXPORTING
*"             VALUE(ERROR_OCCURRED) LIKE  CATSFIELDS-YESORNO
*"       TABLES
*"              EXT_INTERFACE STRUCTURE  CATS_EXT
*"              CHECK_MESSAGES STRUCTURE  MESG
*"              EXT_INTERFACE_OUT STRUCTURE  CATSDB_EXT OPTIONAL
*"              WORKFLOW_TEXT STRUCTURE  SOLISTI1 OPTIONAL
*"              EXT_LONGTEXT STRUCTURE  LONGTEXT_EXT OPTIONAL
*"       EXCEPTIONS
*"              ERROR_OCCURRED
*"----------------------------------------------------------------------

  DATA: usubrc LIKE sy-subrc.
  DATA: utxtnr LIKE mesg-txtnr.
* structure to work with
  DATA: i_catsdb_ext LIKE catsdb_ext OCCURS 0 WITH HEADER LINE.
  CLEAR free_data.           "XZQ
* activate message handler
  PERFORM activate_messages TABLES check_messages.      "new ess
* fill structure i_catsdb_ext.
  PERFORM ext_fill_icatsdb_ext TABLES i_catsdb_ext ext_interface.
* call the internal interface
  CALL FUNCTION 'CATS_INTERFACE'
       EXPORTING
            profile          = profile
            testrun          = testrun
            rel_data         = release_data
            agent            = agent  "YIK
            commit_work      = commit_work  "YIK
            workflow_popup   = workflow_popup  "YIK
            cats_text_format = cats_text_format  "QWK
       TABLES
            i_catsdb_ext     = i_catsdb_ext
            check_messages   = check_messages
            i_catsdb_ext_out = ext_interface_out  "YIK
            workflow_text    = workflow_text  "YIK
            longtext         = ext_longtext  "QWK
       EXCEPTIONS
            error_message    = 1
            OTHERS           = 2.

  IF sy-subrc = 1.
* save unexpected error in internal table
    utxtnr = sy-msgno.
    PERFORM add_message USING sy-msgid sy-msgty utxtnr sy-msgv1
                                                       sy-msgv2
                                                       sy-msgv3
                                                       sy-msgv4.
* check messages
    PERFORM ext_check_messages TABLES check_messages USING usubrc.
  ENDIF.
  LOOP AT check_messages WHERE msgty = msgty-a OR msgty = msgty-e.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    error_occurred = yx.
    REFRESH ext_interface_out.
*    LOOP AT I_CATSDB_EXT.
*      CLEAR EXT_INTERFACE_OUT.
*      MOVE-CORRESPONDING I_CATSDB_EXT TO EXT_INTERFACE_OUT.
*      APPEND EXT_INTERFACE_OUT.
*    ENDLOOP.
  ELSE.
    CLEAR error_occurred.
  ENDIF.

ENDFUNCTION.
*eject
