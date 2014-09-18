*&---------------------------------------------------------------------*
*& Report  ZRIMLLCPN                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : Local L/C 어음도착통보?
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.06.21                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 어음도착통보서를 조회한다.                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLLCPN    MESSAGE-ID ZIM
                     LINE-SIZE 117
                     NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFBENI          LIKE   ZTPMTHD-ZFBENI,     " Benificiary(Vendor)
       ZFBENI_NM(20)   TYPE   C,                  " Vendor Name
       ZFOPNNO(30)     TYPE   C,                  " L/C No
       ZFREQNO         TYPE   ZTPMTHD-ZFREQNO,    " 수입의뢰 관리번?
       ZFPYDT          LIKE   ZTPMTHD-ZFPYDT,     " 결제완료일(접수일)
       ZFDSDT          LIKE   ZTPMTHD-ZFDSDT,     " 할인일(매입일)
       ZFISNO          LIKE   ZTPMTHD-ZFISNO,     " 인수증 발급번?
       ZFREDNO         LIKE   ZTRED-ZFREDNO,      " 인수증 관리번?
       ZFREAMK         LIKE   ZTRED-ZFREAMK,      " 인수금액 원?
       ZFREAMF         LIKE   ZTRED-ZFREAMF,      " 인수금액 외?
       ZFREAMFC        LIKE   ZTRED-ZFREAMFC,     " 인수금액 외화 통?
       ZFTIVAMK        LIKE   ZTPMTHD-ZFTIVAMK,   " Notice Amount-원?
       ZFPNAM          LIKE   ZTPMTHD-ZFPNAM,     " Notice Amount-외?
       ZFPNAMC         LIKE   ZTPMTHD-ZFPNAMC,  " Notice Amount-외화통?
       ZFEXRT          LIKE   ZTPMTHD-ZFEXRT,   " Notice 환?
       ZFDFAMT         LIKE   ZTPMTHD-ZFTIVAMK. " 환차?
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMLLCPNTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BENI    FOR ZTPMTHD-ZFBENI,    " Vendor
                   S_OPNNO   FOR ZTPMTHD-ZFOPNNO,   " L/C No
                   S_PYDT    FOR ZTPMTHD-ZFPYDT,    " 접수?
                   S_DSDT    FOR ZTPMTHD-ZFDSDT.    " 매입?
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_INIT.
  SET  TITLEBAR  'ZIMX1'.               " GUI TITLE  SETTING
* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
*  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
            W_FIELD_NM = 'ZFREQDT'.
            ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
            PERFORM HANDLE_SORT TABLES  IT_TAB
                                USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DSRQ'.                   " 수입의뢰 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'DISPR'.                   " 인수증 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_ZTRED USING IT_SELECTED-ZFREDNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /35  '[ Local L/C 어음도착통보서 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM,  99 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'Vendor                        ' NO-GAP, SY-VLINE NO-GAP,
            '접수일    '                     NO-GAP, SY-VLINE NO-GAP,
            '    인수 금액-원화      '       NO-GAP, SY-VLINE NO-GAP,
            '  Notice 금액-원화      '       NO-GAP, SY-VLINE NO-GAP,
            '           외환차손'            NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            'L/C No                        ' NO-GAP, SY-VLINE NO-GAP,
            '매입일    '                     NO-GAP, SY-VLINE NO-GAP,
            '    인수 금액-외화      '       NO-GAP, SY-VLINE NO-GAP,
            '  Notice 금액-외화      '       NO-GAP, SY-VLINE NO-GAP,
            '        Notice 환율' NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.

   SET PF-STATUS 'ZIMX1'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMX1'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.

   ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI            NO-GAP, " Vendor
       IT_TAB-ZFBENI_NM         NO-GAP, " Vendor ?
       SY-VLINE NO-GAP,
       IT_TAB-ZFPYDT            NO-GAP, " 접수?
       SY-VLINE NO-GAP,
       IT_TAB-ZFREAMK CURRENCY 'KRW'  NO-GAP, " 인수금액-원?
       '     '                  NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFTIVAMK CURRENCY 'KRW'  NO-GAP, " Notice금액-원?
       '     '                  NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFDFAMT CURRENCY 'KRW'  NO-GAP, " 환차?
       SY-VLINE NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFOPNNO           NO-GAP, " L/C No
       SY-VLINE NO-GAP,
       IT_TAB-ZFDSDT            NO-GAP, " 매입?
       SY-VLINE NO-GAP,
       IT_TAB-ZFREAMF CURRENCY IT_TAB-ZFREAMFC   NO-GAP, " 인수금액-외?
       IT_TAB-ZFREAMFC        NO-GAP, " 인수금액-외화 통?
       SY-VLINE NO-GAP,
       IT_TAB-ZFPNAM CURRENCY IT_TAB-ZFPNAMC  NO-GAP, " Notice금액-외?
       IT_TAB-ZFPNAMC        NO-GAP, " Notice금액-외화 통?
       SY-VLINE NO-GAP,
       '       '                NO-GAP,
       IT_TAB-ZFEXRT            NO-GAP, " Notice 환?
       SY-VLINE NO-GAP.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / SY-ULINE.    WRITE : / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.
  SELECT *
    FROM ZTPMTHD
   WHERE ZFBENI  IN S_BENI
     AND ZFOPNNO IN S_OPNNO
     AND ZFPYDT  IN S_PYDT
     AND ZFLCKN = '8'.
         CLEAR    IT_TAB.
         MOVE-CORRESPONDING ZTPMTHD TO IT_TAB.
         SELECT SINGLE NAME1
           INTO IT_TAB-ZFBENI_NM
           FROM LFA1
          WHERE LIFNR = IT_TAB-ZFBENI.
         SELECT MAX( ZFREDNO )
           INTO IT_TAB-ZFREDNO
           FROM ZTRED
          WHERE ZFISNO = IT_TAB-ZFISNO.
         SELECT SINGLE *
           FROM ZTRED
          WHERE ZFREDNO = IT_TAB-ZFREDNO.
         MOVE-CORRESPONDING ZTRED TO IT_TAB.
         MOVE ZTPMTHD-ZFEXRT TO IT_TAB-ZFEXRT.
         IT_TAB-ZFDFAMT = IT_TAB-ZFREAMK - IT_TAB-ZFTIVAMK.
         APPEND  IT_TAB.
  ENDSELECT.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     MESSAGE S738.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX    TYPE P,
        ZFREDNO  LIKE ZTRED-ZFREDNO,
        ZFREQNO  LIKE ZTPMTHD-ZFREQNO.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX     TO INDEX,
         IT_TAB-ZFREDNO   TO ZFREDNO,
         IT_TAB-ZFREQNO   TO ZFREQNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFREDNO   TO IT_SELECTED-ZFREDNO,
                IT_TAB-ZFREQNO   TO IT_SELECTED-ZFREQNO.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_ZTRED
*&---------------------------------------------------------------------*
FORM P2000_SHOW_ZTRED USING    P_ZFREDNO.

   SET PARAMETER ID 'ZPREDNO'  FIELD P_ZFREDNO.
   EXPORT 'ZPREDNO'  TO MEMORY ID 'ZPREDNO'.
   CALL TRANSACTION 'ZIMA7' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_ZTRED

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC
