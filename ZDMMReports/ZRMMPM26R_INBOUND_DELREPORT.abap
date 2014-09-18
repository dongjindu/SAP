************************************************************************
* Program Name : ZRMMPM26R_INBOUND_DELREPORT
* Created by   : Min-su Park
* Created on   : 2003.09.15.
* Pattern      :
* Description  : Inbound Delivery Report for ENG. T/M Assy
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.09.16.     Min-su Park      UD1K901873     Initial Coding       *
************************************************************************

*&---------------------------------------------------------------------*
*& Report  ZRMMPM26R_INBOUND_DELREPORT                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  ZRMMPM26R_INBOUND_DELREPORT   .
TYPE-POOLS: SLIS.
*ALV Definition.
DATA:   WA_EVENTS      TYPE SLIS_T_EVENT,
        W_REPID LIKE SY-REPID                           ,
        IT_FIELDCAT TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        WA_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME VALUE 'TOP_OF_PAGE',
        WA_LIST_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

TABLES : LIKP, LIPS.
DATA : BEGIN OF IT_INBOUND OCCURS 0,
              MATNR LIKE LIPS-MATNR,
              MAKTX LIKE MAKT-MAKTX,
              LFIMG LIKE LIPS-LFIMG,
              MEINS LIKE LIPS-MEINS,
              TRAID LIKE LIKP-TRAID,
              VBELN LIKE LIKP-VBELN,
              POSNR LIKE LIPS-POSNR,
              VGBEL LIKE LIPS-VGBEL,
       END OF IT_INBOUND.


SELECT-OPTIONS : S_MATNR FOR  LIPS-MATNR,
                 S_TRAID FOR  LIKP-TRAID,
                 S_WERKS FOR  LIKP-WERKS.
PARAMETERS     : P_ERDAT LIKE LIKP-ERDAT NO-DISPLAY.


AT SELECTION-SCREEN.
  PERFORM MAKE_BASIC_DATA.
  PERFORM ALV_FIELD_BUILD.

START-OF-SELECTION.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM           = W_REPID
      IT_EVENTS                    = WA_EVENTS[]
      IT_FIELDCAT                  = IT_FIELDCAT[]
      I_CALLBACK_USER_COMMAND      = 'USER_COMMAND'
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                     = IT_INBOUND
* EXCEPTIONS
*   PROGRAM_ERROR                  = 1
*   OTHERS                         = 2
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_BASIC_DATA.
  TABLES : VBFA.
  DATA : CONT_REG_NUMB1 LIKE LECI_EVENT_DATA-CONT_REG_NUMB1.
  P_ERDAT = SY-DATUM.
  SELECT * FROM ZVMM_INBOUND
           INTO CORRESPONDING FIELDS OF TABLE IT_INBOUND
          WHERE MATNR IN S_MATNR
            AND TRAID IN S_TRAID
            AND WERKS IN S_WERKS
            AND ERDAT = P_ERDAT.
  LOOP AT IT_INBOUND.
    SELECT SINGLE CONT_REG_NUMB1 FROM LECI_EVENT_DATA
                    INTO CONT_REG_NUMB1
                   WHERE CONT_REG_NUMB1 = IT_INBOUND-TRAID.
    IF SY-SUBRC = 0.
      SELECT SINGLE * FROM VBFA
                     WHERE VBELV   = IT_INBOUND-VBELN
                       AND POSNV   = IT_INBOUND-POSNR
                       AND VBTYP_N = 'I'.
      IF SY-SUBRC = 0.
        DELETE IT_INBOUND. CONTINUE.
      ELSE.
      ENDIF.
    ELSE.
      DELETE IT_INBOUND. CONTINUE.
    ENDIF.
  ENDLOOP.
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
  CLEAR : IT_FIELDCAT[], WA_EVENTS[],
          WA_LIST_TOP_OF_PAGE[]     .
  PERFORM FIELDCAT_INIT  USING IT_FIELDCAT[].
  PERFORM EVENTTAB_BUILD USING WA_EVENTS[].
  PERFORM COMMENT_BUILD  USING WA_LIST_TOP_OF_PAGE[].
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM FIELDCAT_INIT
              USING RT_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV.

  DATA: LS_FIELDCAT TYPE SLIS_FIELDCAT_ALV.
  DATA: POS TYPE I VALUE 1.

*Material
  clear ls_fieldcat.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MATNR'.
  LS_FIELDCAT-REF_FIELDNAME = 'MATNR'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Material'.
  LS_FIELDCAT-SELTEXT_M     = 'Material'.
  LS_FIELDCAT-SELTEXT_S     = 'Material'.
  LS_FIELDCAT-OUTPUTLEN     = '18'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Description
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MAKTX'.
  LS_FIELDCAT-REF_FIELDNAME = 'MAKTX'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Description'.
  LS_FIELDCAT-SELTEXT_M     = 'Description'.
  LS_FIELDCAT-SELTEXT_S     = 'Description'.
  LS_FIELDCAT-OUTPUTLEN     = '25'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Quantity
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'LIFMG'.
  LS_FIELDCAT-REF_FIELDNAME = 'MEINS'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Quantity'.
  LS_FIELDCAT-SELTEXT_M     = 'Quantity'.
  LS_FIELDCAT-SELTEXT_S     = 'Quantity'.
  LS_FIELDCAT-OUTPUTLEN     = '13'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Uom
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'MEINS'.
  LS_FIELDCAT-REF_FIELDNAME = 'MEINS'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'UOM'.
  LS_FIELDCAT-SELTEXT_M     = 'UOM'.
  LS_FIELDCAT-SELTEXT_S     = 'UOM'.
  LS_FIELDCAT-OUTPUTLEN     = '4'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Container Number
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'TRAID'.
  LS_FIELDCAT-REF_FIELDNAME = 'TRAID'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Container Number'.
  LS_FIELDCAT-SELTEXT_M     = 'Container Number'.
  LS_FIELDCAT-SELTEXT_S     = 'Container Number'.
  LS_FIELDCAT-OUTPUTLEN     = '25'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Inbound Delivery No.
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'VBELN'.
  LS_FIELDCAT-REF_FIELDNAME = 'VBELN'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Inbound Delivery No'.
  LS_FIELDCAT-SELTEXT_M     = 'Inbound Delivery No'.
  LS_FIELDCAT-SELTEXT_S     = 'Inbound Delivery No'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Delivery Item
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'POSNR'.
  LS_FIELDCAT-REF_FIELDNAME = 'POSNR'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Delivery Item'.
  LS_FIELDCAT-SELTEXT_M     = 'Delivery Item'.
  LS_FIELDCAT-SELTEXT_S     = 'Delivery Item'.
  LS_FIELDCAT-OUTPUTLEN     = '6'.
  LS_FIELDCAT-NO_OUT        = ''.
  APPEND LS_FIELDCAT TO  RT_FIELDCAT.

*Order Number
  clear ls_fieldcat.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS       = POS.
  LS_FIELDCAT-FIELDNAME     = 'VGBEL'.
  LS_FIELDCAT-REF_FIELDNAME = 'VGBEL'.
  LS_FIELDCAT-KEY           = ''.
  LS_FIELDCAT-QFIELDNAME    = ''.
  LS_FIELDCAT-CFIELDNAME    = ''.
  LS_FIELDCAT-SELTEXT_L     = 'Order Number'.
  LS_FIELDCAT-SELTEXT_M     = 'Order Number'.
  LS_FIELDCAT-SELTEXT_S     = 'Order Number'.
  LS_FIELDCAT-OUTPUTLEN     = '10'.
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
FORM EVENTTAB_BUILD
              USING E03_LT_EVENTS TYPE SLIS_T_EVENT.
  DATA: LS_EVENT TYPE SLIS_ALV_EVENT.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = E03_LT_EVENTS.
  READ TABLE E03_LT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                           INTO LS_EVENT.
  IF SY-SUBRC = 0.
    MOVE WA_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO E03_LT_EVENTS.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM COMMENT_BUILD
             USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
DATA: LS_LINE TYPE SLIS_LISTHEADER.
DATA: INFO_TXT(50).

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
* LS_LINE-KEY:  not used for this type
  LS_LINE-INFO = TEXT-100.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Material Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'      .
  INFO_TXT+5(18) = S_MATNR-LOW .
  INFO_TXT+24(2) = 'To'        .
  INFO_TXT+27(18) = S_MATNR-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Material:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Material Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)   = 'From'      .
  INFO_TXT+5(20)  = S_TRAID-LOW .
  INFO_TXT+26(2)  = 'To'        .
  INFO_TXT+29(20) = S_TRAID-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Container:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*Plant Selection Range Display
  CLEAR INFO_TXT.
  INFO_TXT+0(4)  = 'From'    .
  INFO_TXT+5(4)  = S_WERKS-LOW .
  INFO_TXT+10(2) = 'To'      .
  INFO_TXT+13(4) = S_WERKS-HIGH.
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Plant:'.
  LS_LINE-INFO = INFO_TXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
 CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
           IT_LIST_COMMENTARY = WA_LIST_TOP_OF_PAGE.
ENDFORM.
