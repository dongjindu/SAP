*&---------------------------------------------------------------------*
*& Report  ZRIMPAYMNG                                                  *
*&---------------------------------------------------------------------*
*&  Program Name : B/L NOTICE Registration Modification                *
*&  Created By   : Lee Chae-Kyung INFOLINK Ltd.                        *
*&  Created Date : 2001.10.23                                          *
*&---------------------------------------------------------------------*
*&   DESC.       : Registrates and modificates B/L notice.
*&---------------------------------------------------------------------*
*& [∫Ø∞Ê≥ªøÎ]
*&---------------------------------------------------------------------*
REPORT  ZRIMPAYMNG  MESSAGE-ID ZIM
*                    LINE-SIZE 161
                    NO STANDARD PAGE HEADING.

TABLES : ZTREQHD,
         ZTREQST,
         ZTREQIT,
         EKKO,
         LFA1,
         ZTBL,
         *ZTBL,
         ZTBLIT,
         SPOP,
         ZSBLNOTICE,
         ZTPMTHD,
         ZTPMTEDI.

*> TABLE CONTROL.
CONTROLS  TC_1100 TYPE TABLEVIEW USING SCREEN 1100.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " º±≈√ LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " ∆‰¿Ã¡ˆ¥Á LINE COUNT
       W_COUNT           TYPE I,             " ¿¸√º COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " « µÂ?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_ZFPYDT(10),
       W_UPDATE_CNT      TYPE I,
       OPTION(1)       TYPE C,             " ∞¯≈Î popup Screenø°º≠ ªÁ?
       ANTWORT(1)      TYPE C,             " ∞¯≈Î popup Screenø°º≠ ªÁ?
       CANCEL_OPTION   TYPE C,             " ∞¯≈Î popup Screenø°º≠ ªÁ?
       W_GUBUN           TYPE C,
       W_BUTTON_ANSWER   TYPE C,
       TEXTLEN           TYPE C,
       F(20)             TYPE C,             " Field Name Alias
       LINE              TYPE I,
       W_ROWMARK         TYPE C,
       G_PARM_LINE       LIKE SY-TABIX,
       W_SY_SUBRC        LIKE SY-SUBRC,
*> 2001.06.18 KSB INSERT START
       W_SUBRC           LIKE SY-SUBRC,
*> 2001.06.18 KSB INSERT END.
       W_LOOPLINES       LIKE SY-LOOPC,
       W_COUNTER1        LIKE SY-LOOPC,
       W_COUNTER         LIKE SY-LOOPC,
       W_ZFREQNO         LIKE ZTREQHD-ZFREQNO,
       OK-CODE           LIKE SY-UCOMM,
       W_OK_CODE         LIKE SY-UCOMM.

*> BL µ•¿Ã≈∏.
DATA : BEGIN OF IT_BL  OCCURS 0.
       INCLUDE STRUCTURE ZTBL.
DATA : END   OF IT_BL.

*> BL µ•¿Ã≈∏.
DATA : BEGIN OF IT_TAB  OCCURS 0.
       INCLUDE STRUCTURE ZSBLNOTICE.
       DATA : ZFMARK.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_TAB_ORG  OCCURS 0.
       INCLUDE STRUCTURE ZSBLNOTICE.
       DATA : ZFMARK.
DATA : END   OF IT_TAB_ORG.

DATA: BEGIN OF  IT_LOCK OCCURS 0,
      ZFBLNO   LIKE ZTBL-ZFBLNO,
      ZFBLAMT  LIKE ZTBL-ZFBLAMT,
      ZFPNADT  LIKE ZTBL-ZFPNADT,
      ZFPNPDT  LIKE ZTBL-ZFPNPDT,
      ZFPNRDT  LIKE ZTBL-ZFPNRDT,
      ZFPNRNM  LIKE ZTBL-ZFPNRNM,
      ZFPNCDT  LIKE ZTBL-ZFPNCDT,
      ZFPNCNM  LIKE ZTBL-ZFPNCNM,
 END OF IT_LOCK.

*-----------------------------------------------------------------------
* Selection Screen ¿˝.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_OPBN   FOR ZTREQHD-ZFOPBN,  "∞≥º≥¿∫«‡.
                   S_WERKS  FOR ZTBL-ZFWERKS,   " ¥Î«•«√∑£∆Æ.
                   S_EBELN  FOR ZTREQHD-EBELN,   "P/O No(¥Î«•)
                   S_ZFSHNO FOR ZTBL-ZFSHNO,     "º±¿˚¬˜ºˆ
                   S_EKORG  FOR ZTBL-EKORG,      "±∏∏≈¡∂¡˜.
                   S_EKGRP  FOR ZTBL-EKGRP,      "±∏∏≈±◊∑Ï.
                   S_OPNNO  FOR ZTREQST-ZFOPNNO, "L/C No
                   S_HBLNO  FOR ZTBL-ZFHBLNO,    "HOUSE B/L.
                   S_PNPDT  FOR ZTBL-ZFPNPDT,    "∞·¡¶¿œ.
                   S_PNADT  FOR ZTBL-ZFPNADT,    "ø¯∫ªµµ¬¯¿œ.
                   S_PNRDT  FOR ZTBL-ZFPNRDT,    "∞·¡¶ø‰√ª¿œ
                   S_PNRNM  FOR ZTBL-ZFPNRNM,    "∞·¡¶ø‰√ª¿⁄.
                   S_LCKN   FOR ZTPMTHD-ZFLCKN.  "L/C Type.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
     PARAMETERS : P_BLNO      RADIOBUTTON GROUP RDG." B/L µµ¬¯¿œ.
     PARAMETERS : P_PRQNO     RADIOBUTTON GROUP RDG." ∞·¡¶ø‰√ª¿œ.
     PARAMETERS : P_PRNO      RADIOBUTTON GROUP RDG." ∞·¡¶¿œ.
     PARAMETERS : P_ALL       RADIOBUTTON GROUP RDG." ¿¸√º.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " √ ±‚∞™ SETTING
   PERFORM   P2000_SET_INITIAL.

* Title Text Write
TOP-OF-PAGE.
*  PERFORM   P3000_TITLE_WRITE.                  " «ÿ¥ı √‚∑¬...

*-----------------------------------------------------------------------
* START OF SELECTION ¿˝.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* ±««— ∞À¡ı «‘?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

   PERFORM   P1000_GET_BASIC_DATA      USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.  MESSAGE S738.  EXIT.    ENDIF.

   CALL SCREEN 1100.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
*     WHEN 'STUP' OR 'STDN'.         " SORT º±≈√?
*        W_FIELD_NM = 'ZFREQDT'.
*        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*        PERFORM HANDLE_SORT TABLES  IT_TAB
*                            USING   SY-UCOMM.
*      WHEN 'MKAL' OR 'MKLO'.         " ¿¸√º º±≈√ π◊ º±≈√«ÿ?
*            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
*      WHEN 'DOWN'.          " FILE DOWNLOAD....
*            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN OTHERS.
    ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INITIAL
*&---------------------------------------------------------------------*
FORM P2000_SET_INITIAL.

*  P_NO = 'X'.                      ">πÃµÓ∑œ «ˆ»≤.
  SET  TITLEBAR 'ZIMP1'.           " GUI TITLE SETTING..

ENDFORM.                    " P2000_SET_INITIAL
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  «ÿ¥Á »≠∏È AUTHORITY CHECK
*-----------------------------------------------------------------------
*  IF P_NO EQ 'X'.
*      AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*                ID 'ACTVT' FIELD '*'.
*      IF SY-SUBRC NE 0.
*         MESSAGE S960 WITH SY-UNAME 'B/L µÓ∑œ'.
*         W_ERR_CHK = 'Y'.   EXIT.
*      ENDIF.
*   ELSE.
*      AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*                ID 'ACTVT' FIELD '*'.
*      IF SY-SUBRC NE 0.
*         MESSAGE S960 WITH SY-UNAME 'B/L µÓ∑œ'.
*         W_ERR_CHK = 'Y'.   EXIT.
*      ENDIF.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_BASIC_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_BASIC_DATA USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.

   SELECT * INTO TABLE IT_BL
            FROM ZTBL
            WHERE ZFREBELN IN  S_EBELN
            AND   EKORG    IN  S_EKORG
            AND   EKGRP    IN  S_EKGRP
            AND   ZFPOYN   NE 'N'
            AND   ZFWERKS  IN  S_WERKS
            AND   ZFOPNNO  IN  S_OPNNO
            AND   ZFSHNO   IN  S_ZFSHNO
            AND   ZFHBLNO  IN  S_HBLNO
            AND   ZFPNPDT  IN  S_PNPDT      "∞·¡¶¿œ.
            AND   ZFPNADT  IN  S_PNADT      "ø¯∫ªµµ¬¯¿œ.
            AND   ZFPNRDT  IN  S_PNRDT      "∞·¡¶ø‰√ª¿œ
            AND   ZFPNRNM  IN  S_PNRNM.     "∞·¡¶ø‰√ª¿⁄.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.
   REFRESH: IT_TAB, IT_TAB_ORG.
   LOOP AT IT_BL.
*>> ø¯∫ª πÃµµ¬¯∞«.
      IF P_BLNO EQ 'X'.
         IF NOT IT_BL-ZFPNADT IS INITIAL.
            CONTINUE.
         ENDIF.
      ENDIF.
*>> ∞·¡¶ πÃø‰√ª∞«.
      IF P_PRQNO = 'X'.
         IF NOT IT_BL-ZFPNRDT IS INITIAL.
            CONTINUE.
         ENDIF.
      ENDIF.
*>> πÃ∞·¡¶∞«.
      IF P_PRNO = 'X'.
         IF NOT IT_BL-ZFPNPDT IS INITIAL.
             CONTINUE.
         ENDIF.
      ENDIF.

      MOVE-CORRESPONDING IT_BL TO IT_TAB.

*      MOVE : IT_BL-BUKRS     TO   IT_TAB-BUKRS,
      MOVE : 'B'             TO   IT_TAB-ZFJOBGB.

      IF NOT IT_TAB-ZFOPNNO IS INITIAL.
         SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
                FROM ZTREQST
               WHERE ZFOPNNO EQ IT_TAB-ZFOPNNO.

      ELSE.
         SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
               FROM ZTBLIT
              WHERE ZFBLNO = IT_BL-ZFBLNO.
      ENDIF.
      IF NOT W_ZFREQNO IS  INITIAL.
            SELECT SINGLE * FROM ZTREQHD
                   WHERE ZFREQNO EQ W_ZFREQNO
                     AND ZFLCKN  IN S_LCKN
                     AND ZFOPBN  IN S_OPBN.
            MOVE : ZTREQHD-ZFLCKN TO IT_TAB-ZFLCKN.

            IF SY-SUBRC NE 0.
               CONTINUE.
            ENDIF.
            SELECT SINGLE * FROM LFA1
                   WHERE LIFNR EQ ZTREQHD-ZFOPBN.
            MOVE : LFA1-LIFNR   TO   IT_TAB-ZFOPBN,
                   LFA1-NAME1   TO   IT_TAB-NAME1.
      ELSE.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING IT_TAB TO IT_TAB_ORG.
      APPEND IT_TAB.
      APPEND IT_TAB_ORG.
   ENDLOOP.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE EQ 0.
      W_ERR_CHK = 'Y'.
   ENDIF.

ENDFORM.                    " P1000_GET_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.

   SET PF-STATUS 'ZIMP1'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMP1'.           " GUI TITLE SETTING..

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1100 OUTPUT.

   DESCRIBE TABLE IT_TAB  LINES G_PARM_LINE.   " LINE ºˆ GET
   TC_1100-LINES = G_PARM_LINE.                     " LINE ºˆ ¡§?

ENDMODULE.                 " TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE¿« ¿Ø»øº∫ ø©∫Œ ∞À¡ı.
  IF TC_1100-CURRENT_LINE GT TC_1100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
  IF SY-UCOMM  EQ  'MKAL'.
     W_ROWMARK = 'X'.
  ENDIF.
  IF SY-UCOMM EQ  'MKLO'.
       W_ROWMARK = SPACE.
  ENDIF.
* Internal Table Read ( Line∫∞ )
  READ TABLE IT_TAB INDEX TC_1100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_TAB  TO  ZSBLNOTICE.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

   MOVE OK-CODE TO W_OK_CODE.
   CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

   SET SCREEN 0. LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1100-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR1100 INPUT.

   CASE SY-UCOMM.
      WHEN 'CANC'.
           PERFORM P2000_CANCEL_MESSAGE.
           IF ANTWORT EQ  'Y'.
               LEAVE TO SCREEN 0.
           ENDIF.
      WHEN 'EXIT' OR 'BACK'.
           CLEAR  W_GUBUN.
           PERFORM P2000_SET_MODIFY_CHECK.
           IF W_LOOPLINES >  0 .
              PERFORM P2000_EXIT_MESSAGE.
              IF ANTWORT EQ  'Y'.
                 PERFORM P2000_SAVE_PROCESS.
              ELSE.
                 LEAVE TO SCREEN 0.                " ¡æ?
              ENDIF.
           ELSE.
              LEAVE TO SCREEN 0.                " ¡æ?
           ENDIF.
      WHEN 'EXCU'.
           PERFORM P2000_SAVE_MESSAGE.
           IF ANTWORT EQ  'Y'.
              PERFORM P2000_SET_MODIFY_CHECK.
              IF W_LOOPLINES = 0.
                 MESSAGE E977 WITH '∫Ø∞Êµ» ≥ªøÎ¿Ã æ¯Ω¿¥œ¥Ÿ.'.
              ENDIF.
              PERFORM P2000_SAVE_PROCESS.
              LEAVE TO SCREEN 0.
           ELSEIF ANTWORT EQ 'N'.
              MESSAGE S957.
              LEAVE TO SCREEN 0.
           ENDIF.
      WHEN 'EXCU1'.
           CALL TRANSACTION 'ZIM35'.
      WHEN 'SHLC'.
           CLEAR W_COUNT.
           LOOP AT IT_TAB WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_TAB WITH KEY ZFMARK = 'X'.
                 PERFORM  P2000_LC_DOC_DISPLAY
                                       USING  IT_TAB-ZFOPNNO.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_TAB INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_LC_DOC_DISPLAY
                                       USING  IT_TAB-ZFOPNNO.
                    ELSE.
                       MESSAGE S962.
                    ENDIF.
                 ELSE.
                    MESSAGE S962.
                 ENDIF.
              WHEN OTHERS.
                 MESSAGE S965.
           ENDCASE.

      WHEN 'SHPO'.
           CLEAR W_COUNT.
           LOOP AT IT_TAB WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_TAB WITH KEY ZFMARK = 'X'.
                 PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_TAB-ZFREBELN.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_TAB INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_TAB-ZFREBELN.
                    ELSE.
                       MESSAGE S962.
                    ENDIF.
                 ELSE.
                    MESSAGE S962.
                 ENDIF.
              WHEN OTHERS.
                 MESSAGE S965.
           ENDCASE.
      WHEN 'SHBL'.
           CLEAR W_COUNT.
           LOOP AT IT_TAB WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_TAB WITH KEY ZFMARK = 'X'.
                 PERFORM  P2000_BL_DOC_DISPLAY
                                       USING  IT_TAB-ZFBLNO.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_TAB INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_BL_DOC_DISPLAY
                                       USING  IT_TAB-ZFBLNO.
                    ELSE.
                       MESSAGE S962.
                    ENDIF.
                 ELSE.
                    MESSAGE S962.
                 ENDIF.
              WHEN OTHERS.
                 MESSAGE S965.
           ENDCASE.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1100_UPDATE_SCR1100 INPUT.
*-----------------------------------------------------------------------
* ¡∂»∏ MODEΩ√ MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_TAB  INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  IF ZSBLNOTICE-ZFLCKN EQ '4' OR
     ZSBLNOTICE-ZFLCKN EQ '5'.
     IF NOT ZSBLNOTICE-ZFPNRDT IS INITIAL AND
            ZSBLNOTICE-ZFPNPDT IS INITIAL.
        MOVE : ZSBLNOTICE-ZFPNRDT TO  ZSBLNOTICE-ZFPNPDT.
     ENDIF.
  ENDIF.
  MOVE-CORRESPONDING  ZSBLNOTICE TO IT_TAB.
  MOVE      W_ROWMARK       TO IT_TAB-ZFMARK.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_TAB   INDEX TC_1100-CURRENT_LINE.
  ENDIF.

ENDMODULE.                 " TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE OPERATION_MODE_SET_SCRCOM OUTPUT.

  LOOP AT SCREEN.
*    IF P_NO EQ 'X'.       "> NOTICE µÓ∑œ.
        IF SCREEN-GROUP1 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
*    ELSE.
        IF SCREEN-GROUP2 = 'IO'.
          SCREEN-INPUT   = '1'.
        ELSE.
          SCREEN-INPUT   = '0'.
        ENDIF.
*    ENDIF.
    IF SCREEN-NAME EQ 'W_ROWMARK'.
       SCREEN-INPUT   = '1'.
    ENDIF.
    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.                 " OPERATION_MODE_SET_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING '√Îº“ »Æ¿Œ'             " ≈∏¿Ã∆≤...
                                    '∫Ø∞Êµ» ≥ªøÎ¿ª ¿˙¿Âæ¯¿Ã ¡æ∑·µÀ¥œ¥Ÿ.'
                                    '¡æ∑·«œΩ√∞⁄Ω¿¥œ±Ó?' " Message #2
                                    'N'                 " √Îº“ πˆ?
                                    '2'.                      " default
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING
                          'Confirm Cancel'     " Title..
                          'Changed item will be exit without Exit.'
                          'Do you want to exitÓ?'   " Message #2
                          'N'                      " Cancel Button..
                          '2'.                     " default
  ENDIF.

ENDFORM.                    " P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

  SPOP-TITEL = TITLE.
  SPOP-TEXTLINE1 = TEXT1.
  SPOP-TEXTLINE2 = TEXT2.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 40.

  CALL SCREEN 0001 STARTING AT 30 6
                      ENDING   AT 78 10.

  IF ANTWORT = 'C'.                                         " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU'.
   SET PF-STATUS 'POPU'.

   IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
   ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
       IF CANCEL_OPTION = SPACE.
          SCREEN-ACTIVE = 0.
       ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_SET_MODIFY_CHECK.

  DESCRIBE TABLE IT_TAB        LINES W_COUNTER.
  DESCRIBE TABLE IT_TAB_ORG    LINES W_COUNTER1.
  W_LOOPLINES = 0.
*>> ¿Ã¿¸∞™∞˙ √÷±Ÿ∞™¿ª ∫Ò±≥.
  LOOP AT IT_TAB_ORG.
     READ TABLE IT_TAB WITH KEY ZFBLNO  = IT_TAB_ORG-ZFBLNO .
       IF SY-SUBRC EQ 0.
          IF IT_TAB_ORG-ZFPNADT NE IT_TAB-ZFPNADT
             OR IT_TAB_ORG-ZFBLAMT NE IT_TAB-ZFBLAMT
             OR IT_TAB_ORG-ZFPNPDT NE IT_TAB-ZFPNPDT
             OR IT_TAB_ORG-ZFPNADT NE IT_TAB-ZFPNADT
             OR IT_TAB_ORG-ZFPNRDT NE IT_TAB-ZFPNRDT.
             W_LOOPLINES = 1. EXIT.
          ENDIF.
       ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING '¡æ∑· »Æ¿Œ'             " ≈∏¿Ã∆≤...
                            '«ˆ¿Á ¿‘∑¬≥ªø™¿ª ¿˙¿Â«œ¡ˆ æ Ω¿¥œ¥Ÿ.'   "
                            '¿˙¿Â »ƒ ¡æ∑·«œΩ√∞⁄Ω¿¥œ±Ó?'       " MSG2
                            'Y'                         " √Îº“ πˆ∆∞ ?
                            '1'.                        " default button
  ELSE.
    PERFORM P2000_MESSAGE_BOX USING 'Confirm Exit'      " Title..
                            'Do not save current item'   "
                            'Do you exit after saving?'       " MSG2
                            'Y'                         " √Îº“ πˆ∆∞ ?
                            '1'.                        " default button
  ENDIF.


ENDFORM.                    " P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SAVE_PROCESS.

* µ•¿Ã≈∏∏¶ Insert
  REFRESH: IT_LOCK.
  LOOP AT IT_TAB.
     READ TABLE IT_TAB_ORG WITH KEY ZFBLNO = IT_TAB-ZFBLNO.
     IF SY-SUBRC EQ 0.
        IF IT_TAB-ZFPNPDT NE IT_TAB_ORG-ZFPNPDT
           OR IT_TAB-ZFPNRDT NE IT_TAB_ORG-ZFPNRDT
           OR IT_TAB-ZFBLAMT NE IT_TAB_ORG-ZFBLAMT
           OR IT_TAB-ZFPNADT NE IT_TAB_ORG-ZFPNADT.
           PERFORM P3000_LOCK_PROCESS USING IT_TAB-ZFBLNO.
*>> ¿ÃπÃ LOCK ∞…∑¡¿÷¥¬ ∞Õ ¡¶ø‹.
           READ TABLE IT_LOCK WITH KEY ZFBLNO = IT_TAB-ZFBLNO.
           IF SY-SUBRC NE 0.
              MOVE: IT_TAB-ZFBLNO   TO  IT_LOCK-ZFBLNO,
                    IT_TAB-ZFBLAMT  TO  IT_LOCK-ZFBLAMT,
                    IT_TAB-ZFPNADT  TO  IT_LOCK-ZFPNADT,  " ø¯∫ªµµ¬¯¿œ.
                    IT_TAB-ZFPNPDT  TO  IT_LOCK-ZFPNPDT,  " ∞·¡¶¿œ.
                    IT_TAB-ZFPNRDT  TO  IT_LOCK-ZFPNRDT,  " ∞·¡¶ø‰√ª¿œ.
                    SY-UNAME        TO  IT_LOCK-ZFPNRNM,  " ø‰√ª¿⁄.
                    SY-DATUM        TO  IT_LOCK-ZFPNCDT,  " »Æ¿Œ.
                    SY-UNAME        TO  IT_LOCK-ZFPNCNM.  " »Æ¿Œ¿⁄.
              APPEND IT_LOCK.
           ENDIF.
        ENDIF.
     ENDIF.
  ENDLOOP.
  DESCRIBE TABLE IT_LOCK LINES W_LINE.
  IF W_LINE EQ 0.
      MESSAGE E977 WITH '∫Ø∞Êµ» ≥ªøÎ¿Ã æ¯Ω¿¥œ¥Ÿ!!!'.
  ENDIF.
* LOCK INTERNAL TABLE READ «œø©º≠ Ω«¡¶ DBø° UPDATE.
  CLEAR W_COUNT.
  LOOP AT IT_LOCK.

        SELECT SINGLE *
               FROM ZTBL
               WHERE ZFBLNO = IT_LOCK-ZFBLNO.

            MOVE: IT_LOCK-ZFPNADT  TO  ZTBL-ZFPNADT,  " ø¯∫ªµµ¬¯¿œ.
                  IT_LOCK-ZFBLAMT  TO  ZTBL-ZFBLAMT,  " ∞·¡¶±›æ◊.
                  IT_LOCK-ZFPNPDT  TO  ZTBL-ZFPNPDT,  " ∞·¡¶¿œ.
                  IT_LOCK-ZFPNRDT  TO  ZTBL-ZFPNRDT,  " ∞·¡¶ø‰√ª¿œ.
                  IT_LOCK-ZFPNRNM  TO  ZTBL-ZFPNRNM,  " ø‰√ª¿⁄.
                  SY-UNAME         TO  ZTBL-UNAM,
                  SY-DATUM         TO  ZTBL-UDAT.

        UPDATE ZTBL.
        IF SY-SUBRC EQ 0.
          PERFORM P3000_UNLOCK_PROCESS  USING IT_LOCK-ZFBLNO.
          W_COUNT = W_COUNT + 1.
        ELSE.
          MESSAGE E041.
        ENDIF.
  ENDLOOP.
  MESSAGE  S102(ZIM1) WITH W_COUNT.

ENDFORM.                    " P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

  IF SY-LANGU EQ '3'.
    PERFORM P2000_MESSAGE_BOX USING 'ø‰√ª »Æ¿Œ'             " ≈∏¿Ã∆≤...
                                    '¿‘∑¬µ» ≥ªø™¿ª π›øµ«’¥œ¥Ÿ.'
                                    'π›øµ«œΩ√∞⁄Ω¿¥œ±Ó?' " Message #2
                                    'Y'                 " √Îº“ πˆ?
                                    '1'.                      " default
  ELSE.
    PERFORM P2000_MESSAGE_BOX
      USING 'Confirm Request'           " Title...
            'It will apply entered item'
            'Do you want to apply?'     " Message #2
            'Y'                         " Cancel Button..
            '1'.                        " default..
  ENDIF.

ENDFORM.                    " P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_LC_DOC_DISPLAY USING  P_ZFOPNNO.

   DATA: W_MAX_AMD LIKE ZTREQST-ZFAMDNO.
   SELECT SINGLE *
     FROM ZTREQST
    WHERE ZFOPNNO = P_ZFOPNNO.
   IF SY-SUBRC NE 0.
      MESSAGE E009.
   ENDIF.
   SELECT MAX( ZFAMDNO ) INTO W_MAX_AMD
       FROM ZTREQST
       WHERE ZFOPNNO = P_ZFOPNNO.

   SET PARAMETER ID 'ZPREQNO' FIELD ''.
   SET PARAMETER ID 'ZPAMDNO' FIELD ''.
   SET PARAMETER ID 'ZPOPNNO' FIELD  P_ZFOPNNO.
   SET PARAMETER ID 'BES'     FIELD ''.

   IF W_MAX_AMD EQ '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_PO_DOC_DISPLAY USING    P_EBELN.

  SELECT SINGLE *
     FROM EKKO
     WHERE EBELN = P_EBELN.
  IF SY-SUBRC NE 0.
      MESSAGE E977 WITH '±∏∏≈ø¿¥ı∞° æ¯Ω¿¥œ¥Ÿ.'.
  ENDIF.
  SET PARAMETER ID 'BES' FIELD P_EBELN.
  SET PARAMETER ID 'BSP' FIELD ' '.

  CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P3000_LOCK_PROCESS
*&---------------------------------------------------------------------*
FORM P3000_LOCK_PROCESS USING    P_ZFBLNO.

  CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
       EXPORTING
                ZFBLNO  =  P_ZFBLNO
       EXCEPTIONS
                OTHERS   =  1.

  IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import Document'
                                 ZTBL-ZFBLNO P_ZFBLNO
                                 RAISING DOCUMENT_LOCKED.

  ENDIF.

ENDFORM.                    " P3000_LOCK_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P3000_UNLOCK_PROCESS
*&---------------------------------------------------------------------*
FORM P3000_UNLOCK_PROCESS USING    P_ZFBLNO.

  CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
       EXPORTING
                ZFBLNO = P_ZFBLNO.

ENDFORM.                    " P3000_UNLOCK_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_DISPLAY USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD '' .
   CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_BL_DOC_DISPLAY
