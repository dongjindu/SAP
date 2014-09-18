*&---------------------------------------------------------------------*
*& Report  ZRIMPAYLST                                                  *
*&---------------------------------------------------------------------*
*& Program Name : Import Payment Amout List                            *
*& Created By   : Lee chae-kyung INFOLINK Ltd.                         *
*& Created Date : 2001.10.24                                           *
*&---------------------------------------------------------------------*
*&   DESC.      : Import Payment Amout List
*&---------------------------------------------------------------------*
*& [Change History]
*&---------------------------------------------------------------------*
REPORT  ZRIMPAYLST  MESSAGE-ID ZIM
                    LINE-SIZE 120
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

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_ZFPYDT(10),
       W_UPDATE_CNT      TYPE I,
       OPTION(1)       TYPE C,             " 공통 popup Screen에서 사?
       ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
       CANCEL_OPTION   TYPE C,             " 공통 popup Screen에서 사?
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

*> BL 데이타.
DATA : BEGIN OF IT_BL  OCCURS 0.
       INCLUDE STRUCTURE ZTBL.
DATA : END   OF IT_BL.

*> BL 데이타.
DATA : BEGIN OF IT_TAB  OCCURS 0.
       INCLUDE STRUCTURE ZSBLNOTICE.
       DATA : ZFMARK.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_TAB1  OCCURS 0,
       ZFOPBN   LIKE ZTREQHD-ZFOPBN,
       ZFBLAMT  LIKE ZTBL-ZFBLAMT,
       ZFBLAMC  LIKE ZTBL-ZFBLAMC.
DATA : END   OF IT_TAB1.

DATA : BEGIN OF IT_TAB2  OCCURS 0,
      ZFBLAMT  LIKE ZTBL-ZFBLAMT,
      ZFBLAMC  LIKE ZTBL-ZFBLAMC.
DATA : END   OF IT_TAB2.
DATA  W_CHECK(1).
DATA  W_TITLE(50).

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_OPBN   FOR ZTREQHD-ZFOPBN,  "개설은행.
                   S_PNPDT  FOR ZTBL-ZFPNPDT  OBLIGATORY,   "결제일.
                   S_PNRDT  FOR ZTBL-ZFPNRDT,     "결제요청일
                   S_PNRNM  FOR ZTBL-ZFPNRNM,    "결제요청자.
                   S_PNADT  FOR ZTBL-ZFPNADT,    "원본도착일.
                   S_EBELN  FOR ZTREQHD-EBELN,   "P/O No(대표)
                   S_ZFSHNO FOR ZTBL-ZFSHNO,     "선적차수
                   S_EKORG  FOR ZTBL-EKORG,      "구매조직.
                   S_EKGRP  FOR ZTBL-EKGRP,      "구매그룹.
                   S_OPNNO  FOR ZTREQST-ZFOPNNO, "L/C No
                   S_HBLNO  FOR ZTBL-ZFHBLNO,    "HOUSE B/L.
                   S_LCKN   FOR ZTPMTHD-ZFLCKN.  "L/C Type.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_INITIAL.

* Title Text Write
TOP-OF-PAGE.
*  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.

   PERFORM   P1000_GET_BASIC_DATA      USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.  MESSAGE S738.  EXIT.    ENDIF.

   PERFORM   P3000_DATA_WRITE.

* 레포트 관련 TEXT TABLE SELECT
*  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*
** 레포트 Write
*   PERFORM   P3000_DATA_WRITE       USING W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
        WHEN 'SHLC'.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           PERFORM  P2000_LC_DOC_DISPLAY
                                  USING  IT_TAB-ZFOPNNO.
        WHEN 'SHPO'.
           IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.

           PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_TAB-ZFREBELN.
        WHEN 'SHBL'.
          IF W_TABIX EQ 0.
              MESSAGE S962.EXIT.
           ENDIF.
           PERFORM  P2000_BL_DOC_DISPLAY
                                       USING  IT_TAB-ZFBLNO.
        WHEN OTHERS.
    ENDCASE.
    CLEAR: IT_TAB,W_TABIX.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INITIAL
*&---------------------------------------------------------------------*
FORM P2000_SET_INITIAL.

DATA : L_FR_DATE LIKE SY-DATUM.
DATA : L_TO_DATE LIKE SY-DATUM.

  L_FR_DATE = SY-DATUM + 1.
  L_TO_DATE = SY-DATUM + 2.

  MOVE : 'I'       TO     S_PNPDT-SIGN,
         'BT'      TO     S_PNPDT-OPTION,
         L_FR_DATE TO     S_PNPDT-LOW,
         L_TO_DATE TO     S_PNPDT-HIGH.
  APPEND S_PNPDT.

  SET  TITLEBAR 'ZIMP1'.           " GUI TITLE SETTING..

ENDFORM.                    " P2000_SET_INITIAL
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
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
            AND   ZFOPNNO  IN  S_OPNNO
            AND   ZFSHNO   IN  S_ZFSHNO
            AND   ZFHBLNO  IN  S_HBLNO
            AND   ZFPNPDT  IN  S_PNPDT      "결제일.
            AND   ZFPNADT  IN  S_PNADT      "원본도착일.
            AND   ZFPNRDT  IN  S_PNRDT      "결제요청일
            AND   ZFPNRNM  IN  S_PNRNM.     "결제요청자.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.
   REFRESH: IT_TAB.
   LOOP AT IT_BL.
      MOVE-CORRESPONDING IT_BL TO IT_TAB.
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
      APPEND IT_TAB.
   ENDLOOP.
   LOOP AT IT_TAB.
        MOVE-CORRESPONDING IT_TAB TO IT_TAB1.
        MOVE-CORRESPONDING IT_TAB TO IT_TAB2.
        COLLECT IT_TAB1.
        COLLECT IT_TAB2.
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
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

DATA : TMP_ZFOPBN   LIKE   IT_TAB-ZFOPBN.

  SET  TITLEBAR   'ZIMP1'.           " GUI TITLE SETTING..
  SET PF-STATUS   'ZIMP1'.           " GUI STATUS SETTING

  SORT IT_TAB BY ZFOPBN ZFREBELN ZFOPNNO ZFSHNO.

  CLEAR : TMP_ZFOPBN, W_COUNT, W_CHECK.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
     IF W_TABIX EQ 1.
        PERFORM   P3000_MAIN_TITLE_WRITE.
     ENDIF.

*> 은행이 변경될 경우...
     IF TMP_ZFOPBN NE IT_TAB-ZFOPBN.

        LOOP AT IT_TAB1 WHERE ZFOPBN = TMP_ZFOPBN.
             FORMAT COLOR COL_GROUP INTENSIFIED OFF.
             IF W_CHECK = SPACE.
                WRITE:/ SY-ULINE.
                IF SY-LANGU EQ '3'.
                  MOVE '은 행 계:' TO  W_TITLE.
                ELSE.
                  MOVE 'Bank Sum:' TO  W_TITLE.
                ENDIF.
             ENDIF.
             WRITE : / SY-VLINE,
                   (60) W_TITLE,
                   (03) IT_TAB1-ZFBLAMC,
                   (21) IT_TAB1-ZFBLAMT CURRENCY IT_TAB1-ZFBLAMC,
                   120 SY-VLINE.
            W_CHECK = 'Y'.
            CLEAR W_TITLE.
       ENDLOOP.

       IF W_TABIX NE 1.
          WRITE:/ SY-ULINE.
       ENDIF.
       PERFORM   P3000_TITLE_WRITE.
       TMP_ZFOPBN = IT_TAB-ZFOPBN.
     ENDIF.

     PERFORM   P3000_LINE_WRITE.

     AT LAST.
        CLEAR W_CHECK.
        LOOP AT IT_TAB1 WHERE ZFOPBN = TMP_ZFOPBN.
             FORMAT COLOR COL_GROUP INTENSIFIED OFF.
             IF W_CHECK = SPACE.
                WRITE:/ SY-ULINE.
                IF SY-LANGU EQ '3'.
                  MOVE '은 행 계:' TO  W_TITLE.
                ELSE.
                  MOVE 'Bank Sum:' TO  W_TITLE.
                ENDIF.
             ENDIF.
             WRITE : / SY-VLINE,
                   (60) W_TITLE,
                   (03) IT_TAB1-ZFBLAMC,
                   (21) IT_TAB1-ZFBLAMT CURRENCY IT_TAB1-ZFBLAMC,
                                      120 SY-VLINE.
            W_CHECK = 'Y'.
            CLEAR W_TITLE.
       ENDLOOP.
       WRITE:/ SY-ULINE.
     ENDAT.
  ENDLOOP.

  CLEAR:IT_TAB,W_TABIX, W_CHECK.

  LOOP AT IT_TAB2.
     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
     IF W_CHECK = SPACE.
        IF SY-LANGU EQ '3'.
          MOVE '총    계:' TO  W_TITLE.
        ELSE.
          MOVE ' Total  :' TO  W_TITLE.
        ENDIF.
     ENDIF.
     WRITE : / SY-VLINE,
             (60) W_TITLE,
             (03) IT_TAB2-ZFBLAMC,
             (21) IT_TAB2-ZFBLAMT CURRENCY IT_TAB2-ZFBLAMC,
                                      120 SY-VLINE.
     W_CHECK = 'Y'.
     CLEAR W_TITLE.
     AT LAST.
        WRITE:/ SY-ULINE.
     ENDAT.
  ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

     WRITE : / SY-VLINE,
             (11) IT_TAB-ZFREBELN NO-GAP,
             (01) IT_TAB-ZFSHNO,      SY-VLINE,
             (20) IT_TAB-ZFOPNNO,     SY-VLINE,
             (20) IT_TAB-ZFHBLNO,     SY-VLINE,
             (03) IT_TAB-ZFBLAMC,
             (21) IT_TAB-ZFBLAMT CURRENCY IT_TAB-ZFBLAMC,
                                      SY-VLINE,
             (12) IT_TAB-ZFPNPDT,     SY-VLINE,
             (12) IT_TAB-ZFPNRDT,     SY-VLINE.

   HIDE : IT_TAB,W_TABIX.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  FORMAT RESET.
  IF SY-LANGU EQ '3'.
    WRITE : / '개설은행 :', IT_TAB-ZFOPBN, IT_TAB-NAME1.
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE,
             (12) 'P/O No',         SY-VLINE,
             (20) 'L/C No',         SY-VLINE,
             (20) 'B/L No',         SY-VLINE,
             (25) '결제금액',       SY-VLINE,
             (12) '결제일',         SY-VLINE,
             (12) '결제요청일',     SY-VLINE.
    WRITE : / SY-ULINE.
  ELSE.
    WRITE : / 'Open Bank:', IT_TAB-ZFOPBN, IT_TAB-NAME1.
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : / SY-VLINE,
             (12) 'P/O No',         SY-VLINE,
             (20) 'L/C No',         SY-VLINE,
             (20) 'B/L No',         SY-VLINE,
             (25) 'Payment Amount', SY-VLINE,
             (12) 'Payment Date',   SY-VLINE,
             (12) 'Pymt.Req.Dat',   SY-VLINE.
    WRITE : / SY-ULINE.
  ENDIF.
  FORMAT RESET.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_MAIN_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_MAIN_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  IF SY-LANGU EQ '3'.
    WRITE : /50  ' [  수입결제대금 현황  ] '
                 COLOR COL_HEADING INTENSIFIED OFF.
  ELSE.
    WRITE : /40  ' [  Import settlement money list display  ] '
                 COLOR COL_HEADING INTENSIFIED OFF.
  ENDIF.

ENDFORM.                    " P3000_MAIN_TITLE_WRITE
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
      MESSAGE E977 WITH '구매오더가 없습니다.'.
  ENDIF.
  SET PARAMETER ID 'BES' FIELD P_EBELN.
  SET PARAMETER ID 'BSP' FIELD ' '.

  CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_DISPLAY USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPBLNO'  FIELD  P_ZFBLNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD '' .
   CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_BL_DOC_DISPLAY
