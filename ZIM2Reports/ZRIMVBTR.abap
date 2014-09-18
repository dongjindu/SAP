*&---------------------------------------------------------------------*
*& Report  ZRIMVBTR                                                    *
*&---------------------------------------------------------------------*
*&   프로그램명: 긴급 보세운송 의뢰 내역                               *
*&       작성자: 이석철 INFOLINK.Ltd                                   *
*&       작성일: 2000.06.30                                            *
*&---------------------------------------------------------------------*
*& DESC:                                                               *
*&---------------------------------------------------------------------*
*& [변경내용]                                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
REPORT  ZRIMVBTR  MESSAGE-ID ZIM
                  LINE-SIZE 116
                  NO STANDARD PAGE HEADING.

DATA: W_SEQ(2)      TYPE    P,
      W_HIDE        LIKE   ZTBLUG-ZFHBLNO.

DATA: BEGIN OF IT_TAB OCCURS 0.
        INCLUDE STRUCTURE ZTBLUG.
DATA: END OF IT_TAB.
*&----------------------------------------------------------------------
*&    Tables 및 변수 Define
*&----------------------------------------------------------------------
INCLUDE   ZRIMVBTRTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?


SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BNARCD  FOR   ZTBLUG-ZFBNARCD, " 반입장?
                S_TRCK    FOR   ZTBLUG-ZFTRCK,   " Trucker.
                S_HBLNO   FOR   ZTBLUG-ZFHBLNO,  " House B/L NO.
                S_MBLNO   FOR   ZTBLUG-ZFMBLNO,  " Master B/L NO.
                S_FCT     FOR   ZTBLUG-ZFCARNM,  " FCT/V.Name.
                S_ETD     FOR   ZTBLUG-ZFETD,    "ETD.
                S_ETA     FOR   ZTBLUG-ZFETA,    "ETA.
                S_RQNME   FOR   ZTBLUG-ZFRQNME,  "의뢰자.
                S_AREDT   FOR   ZTBLUG-ZFAREDT.  "도착 요망일.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.


TOP-OF-PAGE.
  PERFORM P3000_TITLE_WRITE.           "헤더 출?
*&----------------------------------------------------------------------
*&    START-OF-SELECTION 절.
*&----------------------------------------------------------------------

START-OF-SELECTION.

* 파라메타 설?
    PERFORM P2000_SET_SELETE_OPTION  USING W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
PERFORM   P1000_READ_DATA        USING   W_ERR_CHK.
     IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM P3000_DATA_WRITE    USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*&----------------------------------------------------------------------
*&    User Command
*&----------------------------------------------------------------------
AT USER-COMMAND.

    CASE SY-UCOMM.
      WHEN 'DISP'.                       " 상세 내역 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_VBTR
                       USING IT_SELECTED-ZFHBLNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.

       WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
             LEAVE TO SCREEN 0.
   ENDCASE.
   CLEAR W_HIDE.
*&----------------------------------------------------------------------
*&    Form P2000_SET_SELETE_OPTION.
*&----------------------------------------------------------------------
FORM P2000_SET_SELETE_OPTION    USING W_ERR_CHK.

    W_ERR_CHK = 'N'.
* Import Config Select
     SELECT SINGLE * FROM ZTBLUG.
* Not Found
    IF SY-SUBRC NE 0.
       W_ERR_CHK = 'Y'.
    ENDIF.

ENDFORM.

*&----------------------------------------------------------------------
*&    Form P3000_DATA_WRITE.        " 운송 내역 출?
*&----------------------------------------------------------------------
FORM P3000_DATA_WRITE   USING W_ERR_CHK.

  SET PF-STATUS 'ZIRM09'.               " GUI STATUS SETTING
  SET  TITLEBAR 'ZIRM09'.               " GUI TITLE SETTING..

  W_SEQ = 0.    W_LINE = 0.       W_COUNT = 0.
   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.
ENDFORM.

*&----------------------------------------------------------------------
*&    Form P2000_PAGE_CHECK.
*&----------------------------------------------------------------------
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
     WRITE : / SY-ULINE.
     W_PAGE = W_PAGE + 1.  W_LINE = 0.
     NEW-PAGE.
   ENDIF.

ENDFORM.

*&----------------------------------------------------------------------
*&    Form P3000_LAST_WRITE.
*&----------------------------------------------------------------------
FORM P3000_LAST_WRITE.

  IF  W_COUNT GT 0.
      FORMAT RESET.
      WRITE : /98 '총', W_COUNT,'건'.
      WRITE : /95 SY-ULINE.
  ENDIF.
ENDFORM.

*&----------------------------------------------------------------------
*&    Form P1000_READ_DATA           " 의뢰 내역 조?
*&----------------------------------------------------------------------

FORM P1000_READ_DATA             USING W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

  CLEAR IT_TAB.
  REFRESH IT_TAB.

  SELECT * FROM ZTBLUG INTO IT_TAB
       WHERE      ZFBNARCD IN   S_BNARCD
           AND    ZFTRCK   IN   S_TRCK
           AND    ZFHBLNO  IN   S_HBLNO
           AND    ZFMBLNO  IN   S_MBLNO
           AND    ZFCARNM  IN   S_FCT
           AND    ZFETD    IN   S_ETD
           AND    ZFETA    IN   S_ETA
           AND    ZFRQNME  IN   S_RQNME
           AND    ZFAREDT  IN   S_AREDT.
           APPEND IT_TAB.
  ENDSELECT.
ENDFORM.

*&----------------------------------------------------------------------
*&    Form P3000_TITLE_WRITE         "헤더 출력.
*&----------------------------------------------------------------------
FORM P3000_TITLE_WRITE.
  W_PAGE = 1.
  SKIP 2.
  WRITE: /40 '[ 긴급 보세운송 의뢰 내역 ]'  COLOR 1 INTENSIFIED OFF.
  WRITE: /98 W_PAGE, 'Page', / SY-ULINE.
  FORMAT COLOR 1 INTENSIFIED.
  WRITE: / SY-VLINE, 5 SY-VLINE,
           'Seq',                      SY-VLINE,
           '반입장소        ',         SY-VLINE,
           'Trucker            ',   48 SY-VLINE,
           'B/L No',                75 SY-VLINE,
           'Master No',            116 SY-VLINE.
   FORMAT COLOR 1 INTENSIFIED OFF.
  WRITE: / SY-VLINE, 5 SY-VLINE, 11 SY-VLINE,
           'FCT/V.Name',            48 SY-VLINE,
           'EDT',                   61 SY-VLINE,
           'ETA',                   75 SY-VLINE,
           '의뢰자',               103 SY-VLINE,
           '도착요망일',           116 SY-VLINE.
  WRITE: / SY-ULINE.
ENDFORM.
*&----------------------------------------------------------------------
*&    Form RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

   MOVE 0 TO SY-LSIND.

      W_PAGE  = 1.
      W_LINE  = 1.
      W_COUNT = 0.

ENDFORM.                                   "RESET_LIST.
*&----------------------------------------------------------------------
*&    Form P2000_MULTI_SELECTION.
*&----------------------------------------------------------------------
FORM P2000_MULTI_SELECTION.
  DATA: INDEX   TYPE   P,
        ZFHBLNO LIKE   ZTBLUG-ZFHBLNO.

    REFRESH  IT_SELECTED.
    CLEAR    IT_SELECTED.
    CLEAR    W_SELECTED_LINES.

  MOVE : W_LIST_INDEX     TO   INDEX,
         IT_TAB-ZFHBLNO   TO   ZFHBLNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX     TO INDEX,
                IT_TAB-ZFHBLNO   TO IT_SELECTED-ZFHBLNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.
ENDFORM.                    " P2000_MULTI_SELECTION
*&----------------------------------------------------------------------
*&    Form  P2000_SHOW_VBTR
*&----------------------------------------------------------------------
FORM  P2000_SHOW_VBTR USING P_ZFHBLNO.

   SET PARAMETER ID 'ZPHBLNO'  FIELD P_ZFHBLNO.
   CALL TRANSACTION 'ZIM73' AND SKIP FIRST SCREEN.
ENDFORM.                    " P2000_SHOW_VBTR

*&---------------------------------------------------------------------*
*&    Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  W_SEQ = W_SEQ + 1.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX.      "체크 박?
    WRITE:  SY-VLINE, W_SEQ,       11 SY-VLINE.    "출력순?
    WRITE:      IT_TAB-ZFBNARCD,   30 SY-VLINE,    "반입장?
                IT_TAB-ZFTRCK,     48 SY-VLINE,    "Trucker
                IT_TAB-ZFHBLNO,       SY-VLINE,    "House B/L No.
                IT_TAB-ZFMBLNO,                    "Master B/L No.
            116 SY-VLINE.

       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     WRITE: / SY-VLINE, 5 SY-VLINE, 11 SY-VLINE,
              IT_TAB-ZFCARNM,       48 SY-VLINE,   "FCT/V.Name
              IT_TAB-ZFETD,            SY-VLINE,   "ETD
              IT_TAB-ZFETA,         75 SY-VLINE,   "ETA
              IT_TAB-ZFRQNME,          SY-VLINE,   "의뢰?
              IT_TAB-ZFAREDT,          SY-VLINE.   "도착요망?
* Stored value...
*  MOVE SY-TABIX  TO W_LIST_INDEX.
*  HIDE: W_LIST_INDEX, IT_TAB.
*  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
