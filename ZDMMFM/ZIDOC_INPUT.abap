FUNCTION ZIDOC_INPUT.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(NO_OF_RETRIES) LIKE  BDWF_PARAM-RETRIES DEFAULT 0
*"     VALUE(MASS_PROCESSING) LIKE  BDWF_PARAM-MASS_PROC
*"     VALUE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD DEFAULT
*"       C_IN_BACKGROUND
*"     VALUE(DIRECT_CALL) LIKE  BDWF_PARAM-DIRECTCALL DEFAULT C_FALSE
*"     VALUE(IDOC_START_EVENT_ENABLED) LIKE  BDWF_PARAM-MANUALINPT
*"       DEFAULT C_TRUE
*"     VALUE(END_EVENT_ENABLED) LIKE  BDWF_PARAM-MANUALINPT DEFAULT
*"       C_FALSE
*"     VALUE(END_EVENT_ALWAYS) LIKE  BDWF_PARAM-MANUALINPT DEFAULT
*"       C_FALSE
*"     VALUE(DO_COMMIT) LIKE  BDFIELDS-PARALLEL DEFAULT C_TRUE
*"     VALUE(PROCESS_CODE) LIKE  EDP21-EVCODE OPTIONAL
*"  EXPORTING
*"     VALUE(EXCEPTION_VALUE) LIKE  BDWF_PARAM-EXCEPTION
*"     VALUE(INPUT_SUCCEEDED_FOR_ALL) LIKE  BDWF_PARAM-INPUT_OK
*"  TABLES
*"      UNPROCESSED_IDOCS STRUCTURE  BDIDOCS
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_CONTROL STRUCTURE  EDIDC
*"      T_IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"  EXCEPTIONS
*"      IDOC_OPEN_LOCK
*"      IDOC_OPEN_NOT_EXIST
*"      IDOC_OPEN_INVALID
*"      IDOC_OPEN_ALREADY
*"      IDOC_WRITE_NUMBER_INVALID
*"      IDOC_WRITE_STATUS_INVALID
*"      IDOC_WRITE_NO_STATUS
*"      IDOC_WRITE_LOCK
*"      IDOC_WRITE_DB_ERROR
*"      IDOC_CLOSE_NOT_OPEN
*"      IDOC_CLOSE_DB_ERROR
*"      IDOC_CLOSE_PARAMETER_ERROR
*"      IDOC_CLOSE_NO_STATUS_WRITTEN
*"      IDOCS_DO_NOT_EXIST
*"      IDOCS_HAVE_NO_DATA_RECORDS
*"      UNPROCESSED_IDOCS_EMPTY
*"----------------------------------------------------------------------
  DATA: BEGIN OF T_ONE_IDOC_CONTROL OCCURS 0.
          INCLUDE STRUCTURE EDIDC.
  DATA: END OF T_ONE_IDOC_CONTROL.

  DATA: BEGIN OF T_ONE_IDOC_DATA OCCURS 0.
          INCLUDE STRUCTURE EDIDD.
  DATA: END OF T_ONE_IDOC_DATA.

  DATA: BEGIN OF T_ONE_IDOC_STATUS OCCURS 0.
          INCLUDE STRUCTURE BDIDOCSTAT.
  DATA: END OF T_ONE_IDOC_STATUS.

  DATA: BEGIN OF F_IDOC_CONTROL.
          INCLUDE STRUCTURE EDIDC.
  DATA: END OF F_IDOC_CONTROL.

  DATA: BEGIN OF F_EVENT_INFO.
          INCLUDE STRUCTURE TBD52.
  DATA: END OF F_EVENT_INFO.

  DATA: BEGIN OF F_FUNCTION_INFO.
          INCLUDE STRUCTURE TBD51.
  DATA: END OF F_FUNCTION_INFO.

  DATA: NEED_TO_READ_IDOC LIKE C_TRUE,
        PACKET_OK LIKE C_TRUE,
        ERROR_WITH_STATUS LIKE C_TRUE,
        PROCESS_AS_PACKET LIKE C_TRUE,
        INPUT_SUCCEEDED LIKE C_TRUE,
        ORIGINAL_NO_OF_RETRIES LIKE BDWF_PARAM-RETRIES,
        LAST_IDOC LIKE C_TRUE,
        LINES_IN_TABLE TYPE I,
        TOTAL_IDOCS TYPE I,
        RET.

* encryption
  data: begin of i_edids occurs 0.
          include structure edids.
  data  end of i_edids.

  data: begin of i_crypt_edids occurs 0,
          docnum like edidd-docnum,
          segnum like edidd-segnum,
        end of i_crypt_edids.

*  data: my_badi TYPE REF TO idoc_data_cryption,
   DATA: c_crypt(10) value '**********',
         idx like sy-tabix,
         l_change(1).

* Set the update task to run in the same dialog task - this improves
* performance by removing the overhead of passing the data to the
* update task etc.
  SET UPDATE TASK LOCAL.

* Initialize variables
  EXCEPTION_VALUE = C_WF_EXCEPTION_OK.
  ORIGINAL_NO_OF_RETRIES = NO_OF_RETRIES.

* Read IDOCs from database if necessary................................
.
* If PIT_IDOC_DATA contains the IDOC's segments, don't read the IDOC
* from the database.
  DESCRIBE TABLE IDOC_DATA LINES LINES_IN_TABLE.
  IF LINES_IN_TABLE > 0.
    NEED_TO_READ_IDOC = C_FALSE.
  ELSE.
    NEED_TO_READ_IDOC = C_TRUE.
    CLEAR IDOC_CONTROL.
    REFRESH IDOC_CONTROL.
  ENDIF.

  IF NEED_TO_READ_IDOC = C_TRUE.
*    PERFORM IDOCS_READ TABLES   UNPROCESSED_IDOCS
*                                IDOC_CONTROL
*                                IDOC_DATA
*                       CHANGING EXCEPTION_VALUE.
  ELSE.
* check for encryption
  SELECT * FROM  edids INTO CORRESPONDING FIELDS OF TABLE i_edids
                        FOR ALL ENTRIES IN idoc_control
                        WHERE docnum = idoc_control-docnum
                        AND STATUS = '69'.

  if sy-subrc = 0.
* status 69 with message E0 052 and STAPA = c_crypt
    loop at i_edids where STAMID eq 'E0' and
            stamno eq '052' and
            stapa3 eq c_crypt.
         i_crypt_edids-docnum = i_edids-docnum.
         i_crypt_edids-segnum = i_edids-stapa1.
         append i_crypt_edids.
    endloop.
    sort i_crypt_edids by docnum segnum.
    delete adjacent duplicates from i_crypt_edids.
      loop at i_crypt_edids.
        read table idoc_data with key docnum = i_crypt_edids-docnum
                                      segnum = i_crypt_edids-segnum.
        idx = sy-tabix.
        read table IDOC_CONTROL
                   with key docnum = i_crypt_edids-docnum.
*        GET BADI my_badi FILTERS segment = idoc_data-segnam.
            clear l_change.
*            CALL BADI my_badi->read_crypt
*                 exporting segnam = idoc_data-segnam
*                           control = idoc_control
*                 changing  have_to_change = l_change
*                           data_record = idoc_data-sdata.
          "if have_to_change = 'X' ,
          "change sdata
          if l_change = 'X'.
            modify idoc_data index idx.
          endif.
      endloop.
    endif.
  ENDIF.
* IDOCs read from database if necessary................................
.


  IF EXCEPTION_VALUE = C_WF_EXCEPTION_OK.
*   Check that IDOCs in packet belong together.........................
.
    READ TABLE IDOC_CONTROL INDEX 1.
    F_IDOC_CONTROL = IDOC_CONTROL.
    PACKET_OK = C_TRUE.

    LOOP AT IDOC_CONTROL.
      IF   IDOC_CONTROL-SNDPRN <> F_IDOC_CONTROL-SNDPRN
        OR IDOC_CONTROL-SNDPRT <> F_IDOC_CONTROL-SNDPRT
        OR IDOC_CONTROL-SNDPFC <> F_IDOC_CONTROL-SNDPFC
        OR IDOC_CONTROL-MESTYP <> F_IDOC_CONTROL-MESTYP
        OR IDOC_CONTROL-MESCOD <> F_IDOC_CONTROL-MESCOD
        OR IDOC_CONTROL-MESFCT <> F_IDOC_CONTROL-MESFCT
        OR IDOC_CONTROL-TEST   <> F_IDOC_CONTROL-TEST.

        PACKET_OK = C_FALSE.
      ENDIF.
    ENDLOOP.                           "End of  loop at idoc_control.
*   Packet check completed.............................................
.

*   If IDOC-packet not OK, update the status table T_IDOC_STATUS
    IF PACKET_OK = C_FALSE.
      LOOP AT IDOC_CONTROL.
        IDOC_CONTROL-STATUS = C_IDOC_STATUS_ERROR.
        MODIFY IDOC_CONTROL.
        T_IDOC_STATUS-DOCNUM = IDOC_CONTROL-DOCNUM.
        T_IDOC_STATUS-STATUS = C_IDOC_STATUS_ERROR.
        T_IDOC_STATUS-MSGID = C_MESSAGE_ID.
        T_IDOC_STATUS-MSGTY = C_ERROR.
        T_IDOC_STATUS-MSGNO = C_ERROR_IDOCS_INCOMPATIBLE.
        T_IDOC_STATUS-REPID = SY-REPID.
        APPEND T_IDOC_STATUS.
      ENDLOOP.
      ERROR_WITH_STATUS = C_TRUE.
      EXCEPTION_VALUE = C_WF_EXCEPTION_TOO_MANY_IDOCS.
    ELSE.                              "i.e. packet OK
*     Get input information (input function module etc.)
*      PERFORM INPUT_INFO_READ TABLES   T_IDOC_STATUS
*                                       IDOC_CONTROL
*                              USING    PROCESS_CODE
*                              CHANGING F_EVENT_INFO
*                                       F_FUNCTION_INFO
*                                       ERROR_WITH_STATUS
*                                       EXCEPTION_VALUE.
    ENDIF.                             "End of  if packet_ok = c_false.

*   Process packet if application function can do so.
    PROCESS_AS_PACKET = C_TRUE.

    IF F_FUNCTION_INFO-INPUT_TYPE <> C_IN_TYPE_MASS_INPUT.
      PROCESS_AS_PACKET = C_FALSE.
    ELSE.
* Dont process as packet in case of objettype serialisation
      LOOP AT IDOC_CONTROL.
        CALL FUNCTION 'ALE_SERIAL_CHANNEL_ACTIVE_CHK'
             EXPORTING
                  I_EDIDC  = IDOC_CONTROL
             IMPORTING
                  E_RETURN = RET.
        IF RET = 'X'.
          PROCESS_AS_PACKET = C_FALSE.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDIF.           "End of  if exception_value = c_wf_exception_ok.

* If the packet only contains one IDoc, process as packet to avoid
* needlessly copying the IDoc.
  DESCRIBE TABLE IDOC_CONTROL LINES TOTAL_IDOCS.

* Process IDOCs........................................................
.
  IF   PROCESS_AS_PACKET = C_TRUE
    OR EXCEPTION_VALUE <> C_WF_EXCEPTION_OK
    OR TOTAL_IDOCS = 1.

    LAST_IDOC = C_TRUE.

*    PERFORM IDOCS_PROCESS
*               TABLES  IDOC_CONTROL
*                       IDOC_DATA
*                       T_IDOC_STATUS
*               USING   MASS_PROCESSING
*                       INPUT_METHOD
*                       DIRECT_CALL
*                       LAST_IDOC
*                       IDOC_START_EVENT_ENABLED
*                       END_EVENT_ENABLED
*                       END_EVENT_ALWAYS
*                       DO_COMMIT
*                       F_EVENT_INFO
*                       F_FUNCTION_INFO
*              CHANGING NO_OF_RETRIES
*                       EXCEPTION_VALUE
*                       ERROR_WITH_STATUS
*                       INPUT_SUCCEEDED.

    INPUT_SUCCEEDED_FOR_ALL = INPUT_SUCCEEDED.

  ELSE.         "i.e. application cannot process packets,
*                and there is more than one IDoc in the packet.
    INPUT_SUCCEEDED_FOR_ALL = C_TRUE.
    LAST_IDOC = C_FALSE.
*   Pass IDOCs to the application one at a time.
    LOOP AT IDOC_CONTROL.

      AT LAST.
        LAST_IDOC = C_TRUE.
      ENDAT.

*     Copy the IDOC to temporary tables.
      APPEND IDOC_CONTROL TO T_ONE_IDOC_CONTROL.
      LOOP AT IDOC_DATA WHERE DOCNUM = IDOC_CONTROL-DOCNUM.
        APPEND IDOC_DATA TO T_ONE_IDOC_DATA.
      ENDLOOP.
      LOOP AT T_IDOC_STATUS WHERE DOCNUM = IDOC_CONTROL-DOCNUM.
        APPEND T_IDOC_STATUS TO T_ONE_IDOC_STATUS.
      ENDLOOP.

*     Process the IDOC in the temporary tables.
*      PERFORM IDOCS_PROCESS
*                 TABLES  T_ONE_IDOC_CONTROL
*                         T_ONE_IDOC_DATA
*                         T_ONE_IDOC_STATUS
*                 USING   MASS_PROCESSING
*                         INPUT_METHOD
*                         DIRECT_CALL
*                         LAST_IDOC
*                         IDOC_START_EVENT_ENABLED
*                         END_EVENT_ENABLED
*                         END_EVENT_ALWAYS
*                         DO_COMMIT
*                         F_EVENT_INFO
*                         F_FUNCTION_INFO
*                CHANGING NO_OF_RETRIES
*                         EXCEPTION_VALUE
*                         ERROR_WITH_STATUS
*                         INPUT_SUCCEEDED.

      IF INPUT_SUCCEEDED = C_FALSE.
        INPUT_SUCCEEDED_FOR_ALL = C_FALSE.
      ENDIF.

*     Leave loop if an exception occurred.
      IF EXCEPTION_VALUE <> C_WF_EXCEPTION_OK.
        EXIT.                          "Leave loop
      ENDIF.

*     Refresh the temporary tables and other fields.
      REFRESH T_ONE_IDOC_CONTROL.
      REFRESH T_ONE_IDOC_DATA.
      CLEAR T_ONE_IDOC_STATUS.
      REFRESH T_ONE_IDOC_STATUS.
      NO_OF_RETRIES = ORIGINAL_NO_OF_RETRIES.
      ERROR_WITH_STATUS = C_FALSE.

    ENDLOOP.

 ENDIF.    "End of  if f_function_info-input_type =
"c_in_type_mass_input
* IDOCs processed......................................................
.

* If an exception occurred, raise an error with message
  IF EXCEPTION_VALUE <> C_WF_EXCEPTION_OK.
*    PERFORM MESSAGE_RAISING USING EXCEPTION_VALUE.
  ENDIF.

ENDFUNCTION.
