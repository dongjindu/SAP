* 4.6C
* QWKP9CK110895 050101 note 371245
FUNCTION ZCATS_READ_TIMESHEET_DATA.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_PERNR) LIKE  CATSDB-PERNR
*"             VALUE(I_KEY_DATE) LIKE  CATSDB-WORKDATE
*"             VALUE(I_PROFILE) LIKE  TCATS-VARIANT
*"             VALUE(I_SIMULATE) LIKE  CATSFIELDS-YESORNO OPTIONAL
*"       EXPORTING
*"             VALUE(E_DATETO) LIKE  CATSFIELDS-DATETO
*"             VALUE(E_DATEFROM) LIKE  CATSFIELDS-DATEFROM
*"             VALUE(ERROR_OCCURRED) LIKE  CATSFIELDS-YESORNO
*"       TABLES
*"              E_CATSDB STRUCTURE  CATSDB
*"              E_CATSD STRUCTURE  CATSD
*"              CHECK_MESSAGES STRUCTURE  MESG
*"----------------------------------------------------------------------
  DATA: utxtnr LIKE mesg-txtnr.
  DATA: usubrc LIKE sy-subrc.
* Assumed to be called externally always
  DATA: i_ext_call TYPE c VALUE yx.                         "lux
* get system type
  PERFORM get_appl_sys_type CHANGING appl_sys_type.         "YIK
* set the mode
  mode = fmode-display.

* activate message handler
  PERFORM activate_messages TABLES check_messages.

* general settings
  PERFORM ext_general_settings USING i_profile.
* check messages
  PERFORM ext_check_messages TABLES check_messages USING usubrc.
  CHECK usubrc = 0.

* set external personnel number
  PERFORM ext_set_personnel_number USING i_pernr i_key_date.
* check messages
  PERFORM ext_check_messages TABLES check_messages USING usubrc.
  CHECK usubrc = 0.
* set period
  catsfields-inputdate = i_key_date.                       "note 371245
  PERFORM set_week USING i_key_date catsfields-catsweek.
  PERFORM get_boundaries USING catsfields-inputdate
                               catsfields-datefrom
                               catsfields-dateto
                               catsfields-catsweek
                               days_on_screen.

* perform check_authority: use key_date for the check
  PERFORM ext_check_authority.
* check messages
  PERFORM ext_check_messages TABLES check_messages USING usubrc.
  CHECK usubrc = 0.

* set the start date
  start_date = i_key_date.
* get ddic-information
  PERFORM get_ddic_information USING i_ext_call.            "lux

* build catsd
  CALL FUNCTION 'CATS_BUILD_CATSD'
       EXPORTING
            i_pernr       = i_pernr
            i_simulate    = i_simulate
       IMPORTING
            e_datefrom    = e_datefrom
            e_dateto      = e_dateto
       TABLES
            e_catsd       = e_catsd
            e_catsdb      = e_catsdb
       EXCEPTIONS
            error_message = 1
            OTHERS        = 2.

  IF sy-subrc = 1.
* save unexpected error in internal table
    utxtnr = sy-msgno.
    PERFORM add_message USING sy-msgid sy-msgty utxtnr sy-msgv1
                                                       sy-msgv2
                                                       sy-msgv3
                                                       sy-msgv4.

  ENDIF.
* did a severe error occur?
  LOOP AT check_messages WHERE msgty = msgty-a OR msgty = msgty-e.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    error_occurred = yx.
  ELSE.
    CLEAR error_occurred.
  ENDIF.

ENDFUNCTION.
*eject
