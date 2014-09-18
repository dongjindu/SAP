FUNCTION ZIDOC_WRITE_AND_START_INBOUND.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_EDIDC) LIKE  EDIDC STRUCTURE  EDIDC
*"     VALUE(DO_COMMIT) LIKE  EDI_HELP-UPDATEMODE DEFAULT 'X'
*"  EXPORTING
*"     VALUE(DOCNUM) LIKE  EDIDC-DOCNUM
*"     VALUE(ERROR_BEFORE_CALL_APPLICATION) LIKE  EDI_HELP-ERROR_FLAG
*"  TABLES
*"      I_EDIDD STRUCTURE  EDIDD
*"      T_IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"  EXCEPTIONS
*"      IDOC_NOT_SAVED
*"----------------------------------------------------------------------
DATA: BEGIN OF INT_EDIDC OCCURS 1.
        INCLUDE STRUCTURE EDIDC.
DATA: END OF INT_EDIDC.
DATA: STAT_OF_PROCESSING LIKE SY-SUBRC.

  MOVE I_EDIDC TO EDIDC.
  CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
       IMPORTING
            PE_IDOC_NUMBER          = DOCNUM
            PE_STATE_OF_PROCESSING  = STAT_OF_PROCESSING
            PE_INBOUND_PROCESS_DATA = TEDE2
       TABLES
            T_DATA_RECORDS          = I_EDIDD
*           T_LINKED_OBJECTS        =
       CHANGING
            PC_CONTROL_RECORD       = EDIDC
       EXCEPTIONS
            IDOC_NOT_SAVED          = 1
            OTHERS                  = 2.

  IF SY-SUBRC EQ 0.
    IF STAT_OF_PROCESSING EQ 0.  " IDoc ist weiter verarbeitbar
      REFRESH INT_EDIDC.
      APPEND EDIDC TO INT_EDIDC.
* zu 3.1 wird versucht für alle Eingangs-Verarbeitungen über den FBStein
* IDOC_START_INBOUND zu gehen, um nur noch einmal das Coding zu haben
* Workflow starten usw.
      IF DO_COMMIT = 'X'.                 " nicht in der Verbuchung
        CALL FUNCTION 'ZIDOC_START_INBOUND'
           EXPORTING
                PI_INBOUND_PROCESS_DATA       = TEDE2
           TABLES
                T_CONTROL_RECORDS             = INT_EDIDC
                T_DATA_RECORDS                = I_EDIDD
                T_IDOC_STATUS                 = T_IDOC_STATUS
           EXCEPTIONS
                OTHERS                        = 8.
        IF SY-SUBRC NE 0.
          MOVE 'X' TO ERROR_BEFORE_CALL_APPLICATION.
          EXIT.
        ENDIF.
      ELSE.                                   " in der Verbuchung
        CALL FUNCTION 'ZIDOC_START_INBOUND'
           IN BACKGROUND TASK
           EXPORTING
              PI_INBOUND_PROCESS_DATA       = TEDE2
           TABLES
              T_CONTROL_RECORDS             = INT_EDIDC
              T_DATA_RECORDS                = I_EDIDD
              T_IDOC_STATUS                 = T_IDOC_STATUS.
      ENDIF.
    ELSE.  " stat_of_processing # 0 , Fehlerhandling bereits gestartet
      ERROR_BEFORE_CALL_APPLICATION = 'X'.
    ENDIF.
  ELSE.      " kein IDoc konnte abgespeichert werden
    RAISE IDOC_NOT_SAVED.
  ENDIF.




ENDFUNCTION.
