FUNCTION ZCATS_CONVERT_CATSDB_TO_CATSD.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_PERNR) LIKE  CATSDB-PERNR
*"             VALUE(I_KEY_DATE) LIKE  CATSDB-WORKDATE
*"             VALUE(I_PROFILE) LIKE  TCATS-VARIANT
*"       EXPORTING
*"             VALUE(E_DATETO) LIKE  CATSFIELDS-DATETO
*"             VALUE(E_DATEFROM) LIKE  CATSFIELDS-DATEFROM
*"             VALUE(ERROR_OCCURRED) LIKE  CATSFIELDS-YESORNO
*"       TABLES
*"              EXT_INTERFACE_IN STRUCTURE  CATSDB_EXT
*"              E_CATSD_OUT STRUCTURE  CATSD
*"              CHECK_MESSAGES STRUCTURE  MESG OPTIONAL
*"----------------------------------------------------------------------
  DATA: UTXTNR LIKE MESG-TXTNR.
  DATA: USUBRC LIKE SY-SUBRC.
* Assumed to be called always externally
  data: i_ext_call type c value yx.                           "lux

* set the mode
  MODE = FMODE-DISPLAY.

* activate message handler
  PERFORM ACTIVATE_MESSAGES TABLES CHECK_MESSAGES.

* general settings
  PERFORM EXT_GENERAL_SETTINGS USING I_PROFILE.
* check messages
  PERFORM EXT_CHECK_MESSAGES TABLES CHECK_MESSAGES USING USUBRC.
  CHECK USUBRC = 0.

* set the start date
  START_DATE = I_KEY_DATE.
* get ddic-information
  PERFORM GET_DDIC_INFORMATION using i_ext_call.        "lux

* build catsd
  CALL FUNCTION 'CATS_BUILD_CATSD'
       EXPORTING
            I_PERNR          = I_PERNR
            I_SIMULATE       = Yx
       IMPORTING
            E_DATEFROM       = E_DATEFROM
            E_DATETO         = E_DATETO
       TABLES
            ext_interface_IN = ext_interface_in
            E_CATSD          = E_CATSD_out
*            E_CATSDB      = E_CATSDB
       EXCEPTIONS
            ERROR_MESSAGE = 1
            OTHERS        = 2.

  IF SY-SUBRC = 1.
* save unexpected error in internal table
    UTXTNR = SY-MSGNO.
    PERFORM ADD_MESSAGE USING SY-MSGID SY-MSGTY UTXTNR SY-MSGV1
                                                       SY-MSGV2
                                                       SY-MSGV3
                                                       SY-MSGV4.

  ENDIF.
* did a severe error occur?
  LOOP AT CHECK_MESSAGES WHERE MSGTY = MSGTY-A OR MSGTY = MSGTY-E.
    EXIT.
  ENDLOOP.
  IF SY-SUBRC = 0.
    ERROR_OCCURRED = YX.
  ELSE.
    CLEAR ERROR_OCCURRED.
  ENDIF.

ENDFUNCTION.
*eject
