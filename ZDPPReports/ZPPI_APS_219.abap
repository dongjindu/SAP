************************************************************************
* Program Name      : ZPPI_APS_219
* Creation Date     : 06/13/2011
* Development Request No :
* Addl Documentation:
* Description       : Send 219 to APS
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZPPI_APS_219 NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS, VRM.
TABLES: ZTBM_ABXOPVDT.
DATA: IT_DATA LIKE TABLE OF ZSBM_ABXOPVDT WITH HEADER LINE.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

*SELECT-OPTIONS: S_DATE FOR ZTBM_ABXOPVDT-ZEDAT.
PARAMETERS: P_EAI AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN OUTPUT.

START-OF-SELECTION.

  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
    MESSAGE I001 WITH 'No data'.
  ELSE.
    PERFORM SAVE_SEND_DATA.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.

  SELECT CARX CLNO VALU VANM CLNM
    INTO CORRESPONDING FIELDS OF TABLE IT_DATA
    FROM ZTBM_ABXOPVDT.
*    WHERE ZEDAT IN S_DATE.

ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_SEND_DATA.
  DATA: L_RESULT(1),
        L_MSGTXT(100),
        L_INDEX LIKE SY-TABIX.
  DATA: LT_SEND LIKE TABLE OF ZTPP_APS_219 WITH HEADER LINE.

  DATA: L_DATE LIKE SY-DATUM,
        L_TIME LIKE SY-UZEIT.

  L_DATE = SY-DATUM.
  L_TIME = SY-UZEIT.

  IF  NOT P_EAI IS INITIAL.
    CALL FUNCTION 'Z_FPP_SET_APS_219'
        DESTINATION C_DEST
        IMPORTING
          FLAG          = L_RESULT
        TABLES
          T_DATA        = IT_DATA
        EXCEPTIONS
               COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
               SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.


    IF L_RESULT = 'S' OR L_RESULT = 's'.
      WRITE: / 'Successfully sent'.
      LOOP AT IT_DATA.
        MOVE-CORRESPONDING IT_DATA TO LT_SEND.
        LT_SEND-ZEDAT = L_DATE.
        LT_SEND-ZETIM = L_TIME.
        LT_SEND-ZRESULT  = 'S'.
        APPEND LT_SEND.
      ENDLOOP.
    ELSE.
      WRITE: / 'EAI Failed'.
      LOOP AT IT_DATA.
        MOVE-CORRESPONDING IT_DATA TO LT_SEND.
        LT_SEND-ZEDAT = L_DATE.
        LT_SEND-ZETIM = L_TIME.
        LT_SEND-ZRESULT  = 'E'.
        APPEND LT_SEND.
      ENDLOOP.
    ENDIF.
    MODIFY ZTPP_APS_219 FROM TABLE LT_SEND.
    IF SY-SUBRC = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

  ENDIF.
ENDFORM.                    " write_data
