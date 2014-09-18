REPORT  ZIPP115I_APS_3EE1_2 NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMPP.
************************************************************************
* Program Name      : ZIPP115I_APS_3EE1_2
* Author            :
* Creation Date     : 09/26/08
* Specifications By :
* Addl Documentation: APS II - Spec / Color
* Description       :
*
************************************************************************
* Modification Logs
* Date        Developer    RequestNo    Description
*
************************************************************************
TABLES:ZTPP_WOSUM.

DATA: BEGIN OF IT_WOSUM OCCURS 0,
      FSC LIKE ZTPP_WOSUM-FSC,
      NATION LIKE ZTPP_WOSUM-NATION,
      DEALER LIKE ZTPP_WOSUM-DEALER,
      VERSION LIKE ZTPP_WOSUM-VERSION,
      EXTC LIKE ZTPP_WOSUM-EXTC,
      INTC LIKE ZTPP_WOSUM-INTC,
      END OF IT_WOSUM.

DATA: IT_PSS03EE LIKE TABLE OF ZTPP_PSS03EE WITH HEADER LINE.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

************************************************************************
*              SELECTION SCREEN LAYOUT                                 *
************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME.
SELECT-OPTIONS: S_DATE FOR ZTPP_WOSUM-WOCREDATE,
                S_DEALER FOR ZTPP_WOSUM-DEALER.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_EAI          TYPE C AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) TEXT-100 FOR FIELD P_EAI.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK B1.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM DATA_SELECT.
  IF IT_WOSUM[] IS INITIAL.
    MESSAGE I001 WITH TEXT-001.
  ELSE.
    IF P_EAI = 'X'.
      PERFORM SEND_DATA.
    ENDIF.
    PERFORM DATA_UPDATE.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  SELECT FSC NATION DEALER VERSION EXTC INTC
    INTO TABLE IT_WOSUM
    FROM ZTPP_WOSUM
     WHERE DEALER IN S_DEALER
       AND WOCREDATE IN S_DATE.

  SORT IT_WOSUM BY FSC VERSION EXTC INTC.
  DELETE ADJACENT DUPLICATES FROM IT_WOSUM
         COMPARING FSC VERSION EXTC INTC.

  LOOP AT IT_WOSUM.
    IT_PSS03EE-PLNT = '1'.
    IT_PSS03EE-MODL = IT_WOSUM-FSC+5(2).
    IT_PSS03EE-BMDL = IT_WOSUM-FSC+5(9).
    IT_PSS03EE-OCN =  IT_WOSUM-FSC+14(4).
    IT_PSS03EE-VERSION = IT_WOSUM-VERSION.
    CONCATENATE IT_WOSUM-NATION IT_WOSUM-DEALER INTO IT_PSS03EE-NATION.
    IT_PSS03EE-EXTC = IT_WOSUM-EXTC.
    IT_PSS03EE-INTC =  IT_WOSUM-INTC.
    IT_PSS03EE-ERDAT = SY-DATUM.
    IT_PSS03EE-ERZET = SY-UZEIT.
    APPEND IT_PSS03EE.
    CLEAR: IT_PSS03EE, IT_WOSUM.
  ENDLOOP.
ENDFORM.                    " DATA_SELECT

*&---------------------------------------------------------------------*
*&      Form  DATA_UPDATE
*&---------------------------------------------------------------------*
FORM DATA_UPDATE.
  DATA: L_TEXT(60) TYPE C,
        L_INT TYPE I.

*  DELETE FROM ZTPP_PSS03EE CLIENT SPECIFIED WHERE MANDT = SY-MANDT.

  MODIFY ZTPP_PSS03EE FROM TABLE IT_PSS03EE.

  IF SY-SUBRC = 0.
    DESCRIBE TABLE IT_PSS03EE LINES L_INT.
    WRITE L_INT TO L_TEXT LEFT-JUSTIFIED.
    COMMIT WORK.
    CONCATENATE 'Created Record count :' L_TEXT
      INTO L_TEXT.
    MESSAGE  S001 WITH L_TEXT.
    MESSAGE  S001 WITH TEXT-002.
  ELSE.
    ROLLBACK WORK.
    MESSAGE  W001 WITH TEXT-003.
  ENDIF.
ENDFORM.                    " DATA_UPDATE
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_DATA.
  DATA : L_MSGTXT(100),
         L_RESULT(1).

  CALL FUNCTION 'Z_FPP_SET_PSS03EE_2'
    DESTINATION C_DEST
    IMPORTING
      FLAG          = L_RESULT
    TABLES
      I_PSS03EE     = IT_PSS03EE
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

  IF L_RESULT = 'S'.
    MESSAGE I001 WITH 'Successfully sent out'.
  ELSE.
    IF l_RESULT IS INITIAL.
      l_RESULT = 'E'.
    ENDIF.
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.
  LOOP AT IT_PSS03EE.
    IT_PSS03EE-INT_FLAG = L_RESULT.
    MODIFY IT_PSS03EE TRANSPORTING INT_FLAG.
  ENDLOOP.
ENDFORM.                    " SEND_DATA
