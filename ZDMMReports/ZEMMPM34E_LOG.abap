************************************************************************
* Program name : ZEMMPM34E_LOG
* Created by   : Min-su Park
* Created on   : 2003.11.27.
* Pattern      :
* Description  : LOG Display about Manage Standard Price for Purchase
*                Material
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.27.     Min-su Park      UD1K901873     Initial Coding       *
* 08.13.2014      Victor     T-code has been deleted for APM                                                                      *
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZEMMPM34E_LOG                                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZEMMPM34E_LOG                 .

*ALV
TYPE-POOLS: SLIS.

DATA:   WA_EVENTS      TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID           ,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*LOG Internal Table
DATA:   BEGIN OF IT_LOG OCCURS 0.
          INCLUDE STRUCTURE ZTMM_LOG.
DATA:   END OF IT_LOG.
DATA:   TCODE1 LIKE SY-TCODE VALUE 'ZMME21',
        TCODE2 LIKE SY-TCODE VALUE 'ZMME22'.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       R1 RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN POSITION 7.
SELECTION-SCREEN COMMENT 8(30) TEXT-002 FOR FIELD R1.
SELECTION-SCREEN POSITION 39.
PARAMETERS       R2 RADIOBUTTON GROUP RADI.
SELECTION-SCREEN POSITION 40.
SELECTION-SCREEN COMMENT 41(30) TEXT-003 FOR FIELD R1.
SELECTION-SCREEN END OF LINE.

AT SELECTION-SCREEN.
IF R1 = 'X'.
 SELECT * FROM ZTMM_LOG
          INTO CORRESPONDING FIELDS OF TABLE IT_LOG
         WHERE TCODE = TCODE1.
ELSE.
 SELECT * FROM ZTMM_LOG
          INTO CORRESPONDING FIELDS OF TABLE IT_LOG
         WHERE TCODE = TCODE2.
ENDIF.

START-OF-SELECTION.
 PERFORM DISPLAY_LOG.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LOG.
   W_REPID = SY-REPID.
   CLEAR : IT_FIELDCAT[], WA_EVENTS[], WA_LIST_TOP_OF_PAGE[].
   PERFORM FIELDCAT_INIT  USING IT_FIELDCAT[].
   PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
   PERFORM COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
   PERFORM ALV_DISPLAY.
ENDFORM.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.
  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I.

*Tcode.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TCODE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'TCODE'.
  LS_FIELDCAT-SELTEXT_M     = 'TCODE'.
  LS_FIELDCAT-SELTEXT_S     = 'TCODE'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Material
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MATNR'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Material'.
  LS_FIELDCAT-SELTEXT_M     = 'Material'.
  LS_FIELDCAT-SELTEXT_S     = 'Material'.
  LS_FIELDCAT-OUTPUTLEN     = '18'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Message type.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TYPE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Message Type'.
  LS_FIELDCAT-SELTEXT_M     = 'Message Type'.
  LS_FIELDCAT-SELTEXT_S     = 'Message Type'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Message class.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'ID'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Message Class'.
  LS_FIELDCAT-SELTEXT_M     = 'Message Class'.
  LS_FIELDCAT-SELTEXT_S     = 'Message Class'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Message number
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'NUMBER'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Message Number'.
  LS_FIELDCAT-SELTEXT_M     = 'Message Number'.
  LS_FIELDCAT-SELTEXT_S     = 'Message Number'.
  LS_FIELDCAT-OUTPUTLEN     = '3'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Message txt
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MESSAGE'.
  LS_FIELDCAT-REF_FIELDNAME = ''.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Message txt'.
  LS_FIELDCAT-SELTEXT_M     = 'Message txt'.
  LS_FIELDCAT-SELTEXT_S     = 'Message txt'.
  LS_FIELDCAT-OUTPUTLEN     = '100'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM EVENTTAB_BUILD USING LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = LT_EVENTS.

ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: ls_line TYPE slis_listheader.
  DATA: info_txt(50).

ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_DISPLAY.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID
*     I_STRUCTURE_NAME             =
*     IT_SORT                      = WA_SORT[]
      IT_EVENTS                    = WA_EVENTS[]
      IT_FIELDCAT                  = IT_FIELDCAT[]
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_LOG.
ENDFORM.                    " ALV_DISPLAY
