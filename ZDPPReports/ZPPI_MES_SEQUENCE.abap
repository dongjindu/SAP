*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI_MES_SEQUENCE
*& Program Name   : MES sequenced data interface Program
*& Created by     : Victor Park
*& Created on     : 02.04.2014
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*&======================================================================
*& Desc.
*&
*&----------------------------------------------------------------------

REPORT  zppi_mes_sequence MESSAGE-ID zmpp.

TABLES : ztpp_mes_seq.

DATA : it_mes_seq LIKE zspp_mes_seq OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
* SELECTION-SCREEN.
*----------------------------------------------------------------------*
PARAMETERS : p_mes AS CHECKBOX.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
INITIALIZATION.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_mes IS INITIAL.
    MESSAGE s000 WITH 'Please check Receive data from MES'.
    STOP.
  ENDIF.

  PERFORM process_data.


*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
FORM process_data .
  DATA : v_dest(30) VALUE 'WMPP01'.   "Interface Destination.
  DATA : e_result TYPE  zresult,
         e_msg    TYPE  zmsg2,
         l_msgtxt(200),
         l_success_chk(1).

  CALL FUNCTION 'ZFPP_RCV_MES_SEQ' DESTINATION v_dest
    IMPORTING
      e_result              = e_result
      e_msg                 = e_msg
    TABLES
      t_data                = it_mes_seq
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt
      resource_failure      = 3
      OTHERS                = 4.

  IF it_mes_seq[] IS NOT INITIAL AND sy-subrc = 0
                                 AND e_result = 'S'.
    CALL FUNCTION 'ZFPP_RCV_MES_SEQ'
      IMPORTING
        e_result              = e_result
        e_msg                 = e_msg
      TABLES
        t_data                = it_mes_seq
      EXCEPTIONS
        communication_failure = 1
        system_failure        = 2
        resource_failure      = 3
        OTHERS                = 4.
    IF e_result = 'S' and sy-subrc = 0.
      l_success_chk = 'X'.
    ENDIF.
  ENDIF.

  IF l_success_chk = 'X'.
    MESSAGE s000 WITH 'I/F result : Success'.
  ELSE.
    MESSAGE i000 WITH 'I/F result : Error' e_msg l_msgtxt.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
