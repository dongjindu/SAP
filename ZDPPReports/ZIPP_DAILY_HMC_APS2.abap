************************************************************************
* Program Name      : ZIPP_DAILY_HMC_APS2
* Author            : Furong Wang
* Creation Date     : 08/22/2007
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : HMC Interface
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZIPP_DAILY_HMC_APS2  MESSAGE-ID ZMPP.

DATA: IT_DATA     LIKE TABLE OF ZTPP_PMT01TB WITH HEADER LINE,
      IT_OUT LIKE TABLE OF ZSPP_PM02C_APS2 WITH HEADER LINE.

CONSTANTS: C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_DATUM LIKE SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  PERFORM GET_INIT_DATA.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM GET_DATA.
  PERFORM SEND_TO_EAI.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  get_init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_INIT_DATA.
  P_DATUM = SY-DATUM - 1.
ENDFORM.                    " get_date_range
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: L_NATION(3).

*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*       EXPORTING
**            percentage = p_percentage
*            text       = 'Reading data ...'
*       EXCEPTIONS
*            OTHERS     = 1.
*
  SELECT * INTO TABLE IT_DATA
    FROM ZTPP_PMT01TB
    WHERE ZDATE = P_DATUM.

  IF SY-SUBRC = 0.
    LOOP AT IT_DATA.
      SELECT SINGLE NATION INTO L_NATION
        FROM ZTPP_WOSUM2
        WHERE CR_DATE = IT_DATA-ZDATE
          AND DEALER = IT_DATA-ZREGI
          AND MI = IT_DATA-ZBMDL
          AND OCN = IT_DATA-ZOCN.
      IF SY-SUBRC = 0.
        CONCATENATE L_NATION IT_DATA-ZREGI INTO IT_OUT-NATN.
      ENDIF.
      IT_OUT-STDT = IT_DATA-ZDATE.
      IT_OUT-MODL = IT_DATA-ZMODL.
      IT_OUT-USEE = IT_DATA-ZUSEE.
      IT_OUT-BMDL = IT_DATA-ZBMDL.
      IT_OUT-OCNN = IT_DATA-ZOCN.
      IT_OUT-PROD = IT_DATA-ZRESU.
      APPEND IT_OUT.
      CLEAR: IT_OUT, IT_DATA, L_NATION.
    ENDLOOP.
  ELSE.
    MESSAGE I000 WITH TEXT-M01.
    EXIT.
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  send_to_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_TO_EAI.
  DATA : L_MSGTXT(100),
         L_RESULT(1).

  CALL FUNCTION 'Z_FPP_DAILY_APS2'
    DESTINATION C_DEST
    IMPORTING
      FLAG          = L_RESULT
    TABLES
      I_PM02C       = IT_OUT
    EXCEPTIONS
           COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
           SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.

*  IF L_RESULT = 'S'.
  IF SY-SUBRC = 0.
    MESSAGE I001 WITH 'Successfully sent out'.
  ELSE.
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.
ENDFORM.                    " send_to_alc
