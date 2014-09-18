************************************************************************
* Program name : ZEMMPM23E_CONTAINER
* Created by   : Min-su Park
* Created on   : 2003.11.05.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.05.     Min-su Park      UD1K901873     Initial Coding       *
* 08.13.2014      Victor     T-code has been deleted for APM
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZEMMPM23E_CONTAINER                                         *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZEMMPM23E_CONTAINER          .

TABLES : ZTMM_CONTAINER.
*ALV Definition.
TYPE-POOLS: SLIS.
DATA:   WA_EVENTS   TYPE SLIS_T_EVENT       ,
        W_REPID LIKE SY-REPID               ,
        WA_SORT     TYPE SLIS_T_SORTINFO_ALV,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        W_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*Internal Table
DATA : BEGIN OF IT_CONTAINER OCCURS 0.
 INCLUDE STRUCTURE ZTMM_CONTAINER.
DATA : END OF IT_CONTAINER.

SELECT-OPTIONS : S_NUMB1 FOR ZTMM_CONTAINER-CONT_REG_NUMB1.
*                 S_KZLER FOR ZTMM_CONTAINER-KZLER         .

AT SELECTION-SCREEN.
  PERFORM MAKE_BASIC_DATA.
  PERFORM ALV_FIELD_BUILD.
START-OF-SELECTION.
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
      T_OUTTAB                     = IT_CONTAINER
            .
*&---------------------------------------------------------------------*
*&      Form  MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_BASIC_DATA.
 SELECT * FROM ZTMM_CONTAINER
          INTO CORRESPONDING FIELDS OF TABLE IT_CONTAINER
         WHERE CONT_REG_NUMB1 IN S_NUMB1.
*           AND KZLER          IN S_KZLER.
ENDFORM.                    " MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ALV_FIELD_BUILD.
   W_REPID = SY-REPID.
   CLEAR : IT_FIELDCAT[], WA_EVENTS[], WA_LIST_TOP_OF_PAGE[].
   PERFORM FIELDCAT_INIT  USING IT_FIELDCAT[].
   PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
   PERFORM COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
ENDFORM.                    " ALV_FIELD_BUILD
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

*Container No.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'CONT_REG_NUMB1'.
  LS_FIELDCAT-REF_FIELDNAME = 'CONT_REG_NUMB1'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Container'.
  LS_FIELDCAT-SELTEXT_M     = 'Container'.
  LS_FIELDCAT-SELTEXT_S     = 'Container'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Date of arrival
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'PASS_DATE'.
  LS_FIELDCAT-REF_FIELDNAME = 'PASS_DATE'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Date of Arrival'.
  LS_FIELDCAT-SELTEXT_M     = 'Date of Arrival'.
  LS_FIELDCAT-SELTEXT_S     = 'Date of Arrival'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

**Date emptied
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'BDATU'.
*  LS_FIELDCAT-REF_FIELDNAME = 'BDATU'.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = 'Date emptied'.
*  LS_FIELDCAT-SELTEXT_M     = 'Date emptied'.
*  LS_FIELDCAT-SELTEXT_S     = 'Date emptied'.
*  LS_FIELDCAT-OUTPUTLEN     = '10'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.
*
*Date of return
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LEAVE_DATE'.
  LS_FIELDCAT-REF_FIELDNAME = 'LEAVE_DATE'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Date of return'.
  LS_FIELDCAT-SELTEXT_M     = 'Date of return'.
  LS_FIELDCAT-SELTEXT_S     = 'Date of return'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Previous Location
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LGPLA'.
  LS_FIELDCAT-REF_FIELDNAME = 'LGPLA'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Previous Location'.
  LS_FIELDCAT-SELTEXT_M     = 'Previous Location'.
  LS_FIELDCAT-SELTEXT_S     = 'Previous Location'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

**Empty
*  clear ls_fieldcat.
*  POS = POS + 1.
*  LS_FIELDCAT-COL_POS       = POS.
*  LS_FIELDCAT-FIELDNAME     = 'KZLER'.
*  LS_FIELDCAT-REF_FIELDNAME = 'KZLER'.
*  LS_FIELDCAT-KEY           = ''.
*  LS_FIELDCAT-QFIELDNAME    = ''.
*  LS_FIELDCAT-CFIELDNAME    = ''.
*  LS_FIELDCAT-SELTEXT_L     = 'Empty'.
*  LS_FIELDCAT-SELTEXT_M     = 'Empty'.
*  LS_FIELDCAT-SELTEXT_S     = 'Empty'.
*  LS_FIELDCAT-OUTPUTLEN     = '1'.
*  LS_FIELDCAT-NO_OUT        = ''.
*  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Returned
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'CONT_REG_NUMB2'.
  LS_FIELDCAT-REF_FIELDNAME = 'CONT_REG_NUMB2'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Returned'.
  LS_FIELDCAT-SELTEXT_M     = 'Returned'.
  LS_FIELDCAT-SELTEXT_S     = 'Returned'.
  LS_FIELDCAT-OUTPUTLEN     = '20'.
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
  READ TABLE LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE W_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO LT_EVENTS.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: LS_LINE TYPE SLIS_LISTHEADER.
DATA: INFO_TXT(50).

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-100.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Container Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'    .
  INFO_TXT+5(20)  = S_NUMB1-LOW .
  INFO_TXT+26(2) = 'To'      .
  INFO_TXT+29(20) = S_NUMB1-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Container:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
*
 CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
           IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE.
ENDFORM.
