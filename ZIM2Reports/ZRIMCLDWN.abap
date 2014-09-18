*&---------------------------------------------------------------------*
*& Report  ZRIMCLDWN                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입신고 Download                                     *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.22                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 수입신고자료를 조회하여 P/C로 Download한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMCLDWN   MESSAGE-ID ZIM
                    LINE-SIZE 108
                    NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFINRC          LIKE   ZTIDR-ZFINRC,        " 신고지 세?
       ZFINRCD         LIKE   ZTIDR-ZFINRCD,       " 세관의 담당?
       ZFBNARCD        LIKE   ZTIDR-ZFBNARCD,      " 보세구역 내부코?
       ZFIDRNO         LIKE   ZTIDR-ZFIDRNO,       " 수입신고번?
       ZFIDWDT         LIKE   ZTIDR-ZFIDWDT,       " 신고희망?
       ZFPONC          LIKE   ZTIDR-ZFPONC,        " 수입거래구?
       ZFITKD          LIKE   ZTIDR-ZFITKD,        " 수입종?
       ZFCOCD          LIKE   ZTIDR-ZFCOCD,        " 관세징수형?
       ZFHBLNO         LIKE   ZTIDR-ZFHBLNO,       " House B/L No
       ZFGOMNO         LIKE   ZTIDR-ZFGOMNO,       " 화물관리번?
       ZFCUT           LIKE   ZTIDR-ZFCUT,         " 관세?
       ZFCUST          LIKE   ZTCUCL-ZFCUST,       " 통관상?
       ZFBLNO          LIKE   ZTIDR-ZFBLNO,        " B/L 관리번?
       ZFCLSEQ         LIKE   ZTIDR-ZFCLSEQ,       " 통관순?
       ZFDNCD          LIKE   ZTIDR-ZFDNCD.        " Download 상?

DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMCLDWNTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?


*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_INRC   FOR ZTIDR-ZFINRC,        " 신고지 세?
                   S_INRCD  FOR ZTIDR-ZFINRCD,       " 세관의 담당?
                   S_IDRNO  FOR ZTIDR-ZFIDRNO,       " 수입신고번?
                   S_IDWDT  FOR ZTIDR-ZFIDWDT,       " 신고희망?
                   S_PONC   FOR ZTIDR-ZFPONC,        " 수입거래구?
                   S_ITKD   FOR ZTIDR-ZFITKD,        " 수입종?
                   S_BNARCD FOR ZTIDR-ZFBNARCD,      " 보세구역 내부코?
                   S_COCD   FOR ZTIDR-ZFCOCD,        " 관세징수형?
                   S_HBLNO  FOR ZTIDR-ZFHBLNO,       " House B/L No
                   S_GOMNO  FOR ZTIDR-ZFGOMNO,       " 화물관리번?
                   S_CUT    FOR ZTIDR-ZFCUT.         " 관세?
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* 통관상?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-002, POSITION 1.
*    SELECTION-SCREEN : COMMENT 32(8) TEXT-021, POSITION 41.
*    PARAMETERS : P_CU1    AS CHECKBOX.              " 생성대?
     SELECTION-SCREEN : COMMENT 31(9) TEXT-022, POSITION 41.
     PARAMETERS : P_CU2    AS CHECKBOX.              " 의뢰대?
     SELECTION-SCREEN : COMMENT 51(13) TEXT-023, POSITION 65.
     PARAMETERS : P_CU3    AS CHECKBOX.              " Download 대?
*    SELECTION-SCREEN : COMMENT 68(8) TEXT-024, POSITION 77.
*    PARAMETERS : P_CUY    AS CHECKBOX.              " 통관완?
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
* Download 여?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-003, POSITION 1.
     SELECTION-SCREEN : COMMENT 38(2) TEXT-031, POSITION 41.
     PARAMETERS : P_DNN    AS CHECKBOX.              " No
     SELECTION-SCREEN : COMMENT 61(3) TEXT-032, POSITION 65.
     PARAMETERS : P_DNY    AS CHECKBOX.              " Yes
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING W_ERR_CHK.
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
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P4000_TO_PC_DOWNLOAD.
            PERFORM P1000_READ_TEXT        USING W_ERR_CHK.
           PERFORM RESET_LIST.
      WHEN 'CHDC'.                    " 수입신고 변?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_IDR USING IT_SELECTED-ZFBLNO
                                            IT_SELECTED-ZFCLSEQ.
               CALL TRANSACTION 'ZIM62' AND SKIP FIRST SCREEN.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
               IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
   WHEN 'DISP'.                    " 수입신고 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_IDR USING IT_SELECTED-ZFBLNO
                                            IT_SELECTED-ZFCLSEQ.
               CALL TRANSACTION 'ZIM63' AND SKIP FIRST SCREEN.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'DSBL'.                    " Bill of Lading 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_BL USING IT_SELECTED-ZFBLNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  P_CU3 = 'X'.
  P_DNN = 'X'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  '[ 수입신고자료 Download ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 90 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ', SY-VLINE     NO-GAP,
            '신고지 세관  '             NO-GAP, SY-VLINE NO-GAP,
            '수입신고번호  '            NO-GAP, SY-VLINE NO-GAP,
            '수입거래구분'              NO-GAP, SY-VLINE NO-GAP,
            '보세구역     '             NO-GAP, SY-VLINE NO-GAP,
            'House B/L No            '  NO-GAP, SY-VLINE NO-GAP,
            '통관/Dwn'                  NO-GAP, SY-VLINE NO-GAP,
            'B/L 관리번호'              NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE     NO-GAP,
            '세관의 담당과'             NO-GAP, SY-VLINE NO-GAP,
            '신고희망일    '            NO-GAP, SY-VLINE NO-GAP,
            '수입종류    '              NO-GAP, SY-VLINE NO-GAP,
            '관세 징수형태'             NO-GAP, SY-VLINE NO-GAP,
            '화물관리번호            '  NO-GAP, SY-VLINE NO-GAP,
            '관세사  '                  NO-GAP, SY-VLINE NO-GAP,
            '통관순번    '              NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIM6D'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM6D'.           " GUI TITLE SETTING..

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
       IT_TAB-ZFINRC   NO-GAP,         " 신고지세?
       '          '    NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFIDRNO  NO-GAP,
       SY-VLINE NO-GAP,                " 수입신고번?
       IT_TAB-ZFPONC   NO-GAP,         " 수입거래구?
       '         '     NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBNARCD NO-GAP,         " 보세구?
       '           '   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFHBLNO  NO-GAP,
       SY-VLINE NO-GAP,                " House B/L No
       IT_TAB-ZFCUST   NO-GAP,         " 통관상?
       '      '        NO-GAP,
       IT_TAB-ZFDNCD   NO-GAP,         " Download 상?
       SY-VLINE NO-GAP,
       IT_TAB-ZFBLNO   NO-GAP,         " B/L 관리번?
       '  '            NO-GAP,
       SY-VLINE NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFINRCD  NO-GAP,         " 세관의 담당?
       '           '   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFIDWDT  NO-GAP,         " 신고희망?
       '    '          NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFITKD   NO-GAP,         " 수입종?
       '           '   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFCOCD   NO-GAP,         " 관세징수형?
       '           '   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFGOMNO  NO-GAP,         " B/L 화물관리번?
       '      '        NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFCUT    NO-GAP,         " 관세?
       '   '           NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFCLSEQ  NO-GAP,         " 통관순?
       '       '       NO-GAP,
       SY-VLINE NO-GAP.

* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / '총', W_COUNT, '건'.
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
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.
  IF P_CU2 = ' ' AND P_CU3 = ' '.
     MESSAGE S738.
     EXIT.
  ENDIF.
  IF P_DNN = ' ' AND P_DNY = ' '.
     MESSAGE S738.
     EXIT.
  ENDIF.

  SELECT *
    FROM ZTIDR
   WHERE ZFINRC     IN S_INRC
     AND ZFINRCD    IN S_INRCD
     AND ZFIDRNO    IN S_IDRNO
     AND ZFIDWDT    IN S_IDWDT
     AND ZFPONC     IN S_PONC
     AND ZFITKD     IN S_ITKD
     AND ZFBNARCD   IN S_BNARCD
     AND ZFCOCD     IN S_COCD
     AND ZFHBLNO    IN S_HBLNO
     AND ZFGOMNO    IN S_GOMNO
     AND ZFCUT      IN S_CUT.
         CLEAR   IT_TAB.
         MOVE-CORRESPONDING ZTIDR TO IT_TAB.

         SELECT SINGLE ZFCUST INTO IT_TAB-ZFCUST
           FROM ZTCUCL
          WHERE ZFBLNO = IT_TAB-ZFBLNO
            AND ZFCLSEQ = IT_TAB-ZFCLSEQ.

         CASE IT_TAB-ZFCUST.                       " 통관상?
              WHEN '2'.                            " 의뢰대?
                   IF P_CU2 = ' '.
                      CONTINUE.
                   ENDIF.
              WHEN '3'.                           " Download 대?
                   IF P_CU3 = ' '.
                      CONTINUE.
                   ENDIF.
              WHEN OTHERS.
                   CONTINUE.
         ENDCASE.
         CASE IT_TAB-ZFDNCD.                       " Download 여?
              WHEN 'Y'.                            " Yes
                   IF P_DNY = ' '.
                      CONTINUE.
                   ENDIF.
              WHEN 'N'.                           " No
                   IF P_DNN = ' '.
                      CONTINUE.
                   ENDIF.
              WHEN OTHERS.
                   CONTINUE.
         ENDCASE.

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

  DATA: INDEX   TYPE P,
        ZFBLNO  LIKE ZTIDR-ZFBLNO,
        ZFCLSEQ LIKE ZTIDR-ZFCLSEQ.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFCLSEQ  TO ZFCLSEQ.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
                IT_TAB-ZFCLSEQ  TO IT_SELECTED-ZFCLSEQ.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDR
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IDR USING    P_ZFBLNO
                             P_ZFCLSEQ.

   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ'  FIELD P_ZFCLSEQ.
   EXPORT 'ZPBLNO'        TO   MEMORY ID 'ZPBLNO'.
   EXPORT 'ZPCLSEQ'       TO   MEMORY ID 'ZPCLSEQ'.

ENDFORM.                    " P2000_SHOW_IDR
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.

   CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P4000_TO_PC_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P4000_TO_PC_DOWNLOAD.

  REFRESH IT_ZTIDR.
  LOOP AT IT_SELECTED.
       CLEAR ZTIDR.
       SELECT SINGLE *                                     " 공통사?
         FROM ZTIDR
        WHERE ZFBLNO = IT_SELECTED-ZFBLNO
          AND ZFCLSEQ = IT_SELECTED-ZFCLSEQ.
       SELECT SINGLE *
         FROM ZTCUCL
        WHERE ZFBLNO = IT_SELECTED-ZFBLNO
          AND ZFCLSEQ = IT_SELECTED-ZFCLSEQ.
       IF ZTCUCL-ZFCUST NE '3'.
          CONTINUE.
       ENDIF.
       CLEAR IT_ZTIDR.
       PERFORM P2000_MOVE_ZTIDR.
       APPEND IT_ZTIDR.
       UPDATE ZTIDR SET ZFDNCD = 'Y'
                        UNAM = SY-UNAME UDAT = SY-DATUM
        WHERE ZFBLNO = IT_SELECTED-ZFBLNO
          AND ZFCLSEQ = IT_SELECTED-ZFCLSEQ.
*
       CLEAR ZTIDRHS.
       SELECT *                                   " 란사?
         FROM ZTIDRHS
        WHERE ZFBLNO = IT_SELECTED-ZFBLNO
          AND ZFCLSEQ = IT_SELECTED-ZFCLSEQ.
              CLEAR IT_ZTIDRHS.
              PERFORM P2000_MOVE_ZTIDRHS.
              APPEND IT_ZTIDRHS.
       ENDSELECT.
*
       CLEAR ZTIDRHSD.
       SELECT *                                   " 규격(행)사?
         FROM ZTIDRHSD
        WHERE ZFBLNO = IT_SELECTED-ZFBLNO
          AND ZFCLSEQ = IT_SELECTED-ZFCLSEQ.
              CLEAR IT_ZTIDRHSD.
              PERFORM P2000_MOVE_ZTIDRHSD.
              APPEND IT_ZTIDRHSD.
       ENDSELECT.
*
       CLEAR ZTIDRHSL.
       SELECT *                                   " 요건확?
         FROM ZTIDRHSL
        WHERE ZFBLNO = IT_SELECTED-ZFBLNO
          AND ZFCLSEQ = IT_SELECTED-ZFCLSEQ.
              CLEAR IT_ZTIDRHSL.
              MOVE-CORRESPONDING ZTIDRHSD   TO IT_ZTIDRHSL.
              MOVE ZTIDR-ZFIDRNO            TO IT_ZTIDRHSL-ZFIDRNO.
*              MOVE '0A'                     TO IT_ZTIDRHSL-ZFCR_LF.
              APPEND IT_ZTIDRHSL.
       ENDSELECT.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'                             " 공통사?
       EXPORTING
       FILENAME = 'C:\TEMP\IMPORT.TXT'
       FILETYPE = 'ASC'
  TABLES
       DATA_TAB = IT_ZTIDR.

  CALL FUNCTION 'DOWNLOAD'                             " 란사?
       EXPORTING
       FILENAME = 'C:\TEMP\IMPORTPD.TXT'
       FILETYPE = 'ASC'
  TABLES
       DATA_TAB = IT_ZTIDRHS.

  CALL FUNCTION 'DOWNLOAD'                             " 규격(행)사?
       EXPORTING
       FILENAME = 'C:\TEMP\IMPORTPS.TXT'
       FILETYPE = 'ASC'
  TABLES
       DATA_TAB = IT_ZTIDRHSD.

  CALL FUNCTION 'DOWNLOAD'                             " 요건확?
       EXPORTING
       FILENAME = 'C:\TEMP\IMPORTPV.TXT'
       FILETYPE = 'ASC'
  TABLES
       DATA_TAB = IT_ZTIDRHSL.

ENDFORM.                    " P4000_TO_PC_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
FORM P2000_WRITE_NO_MASK CHANGING P_TEXT_AMOUNT.

  SELECT SINGLE * FROM USR01 WHERE BNAME EQ SY-UNAME.

  CASE USR01-DCPFM.
     WHEN 'X'.    " Decimal point is period: N,NNN.NN
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT ',' ' '.
        CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
     WHEN 'Y'.    " Decimal point is N NNN NNN,NN
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
        CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
     WHEN OTHERS. " Decimal point is comma: N.NNN,NN
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  '.' ' '.
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
        CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
  ENDCASE.

ENDFORM.                    " P2000_WRITE_NO_MASK

*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
FORM P2000_CHANGE_SYMBOL USING    P_AMOUNT  P_FROM  P_TO.

  DO.
     REPLACE  P_FROM   WITH   P_TO  INTO    P_AMOUNT.
        IF  SY-SUBRC  <>    0.
            EXIT.
        ENDIF.
  ENDDO.

ENDFORM.                    " P2000_CHANGE_SYMBOL

*&---------------------------------------------------------------------*
*&      Form  P2000_MOVE_ZTIDR
*&---------------------------------------------------------------------*
FORM P2000_MOVE_ZTIDR.

  MOVE-CORRESPONDING ZTIDR   TO IT_ZTIDR.

  WRITE   ZTIDR-ZFINAMT TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFINAMTC.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFINAMT. " 보험?

  WRITE   ZTIDR-ZFINAMTS TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFINAMTC.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFINAMTS. " 총보험?

  WRITE   ZTIDR-ZFTFA TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFTFAC.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTFA. " 운임A

  WRITE   ZTIDR-ZFTFB TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFTFBC.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTFB. " 운임B

  WRITE   ZTIDR-ZFTRT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTRT. " 총운?

  WRITE   ZTIDR-ZFADAM TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFADAMCU.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFADAM. " 가산금?

  WRITE   ZTIDR-ZFADAMK TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFADAMK. " 가산금액(원화)

  WRITE   ZTIDR-ZFDUAM TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFDUAMCU.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFDUAM. " 공제금?

  WRITE   ZTIDR-ZFDUAMK TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFDUAMK. " 공제금액(원화)

  WRITE   ZTIDR-ZFSTAMT TO W_TEXT_AMOUNT  CURRENCY ZTIDR-ZFSTAMC.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFSTAMT. " 결제금?

  WRITE   ZTIDR-ZFCUAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFCUAMTS. " 총관?

  WRITE   ZTIDR-ZFSCAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFSCAMTS. " 총특소?

  WRITE   ZTIDR-ZFDRAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFDRAMTS. " 총주?

  WRITE   ZTIDR-ZFTRAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTRAMTS. " 총교통?

  WRITE   ZTIDR-ZFEDAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFEDAMTS. " 총교육?

  WRITE   ZTIDR-ZFAGAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFAGAMTS. " 총농특?

  WRITE   ZTIDR-ZFVAAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFVAAMTS. " 총부가?

  WRITE   ZTIDR-ZFIDAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFIDAMTS. " 총신고지연 가산?

  WRITE   ZTIDR-ZFTXAMTS TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTXAMTS. " 총세?

  WRITE   ZTIDR-ZFTBAK TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTBAK. " 과세가격-원?

  WRITE   ZTIDR-ZFTBAU TO W_TEXT_AMOUNT  CURRENCY 'USD'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDR-ZFTBAU. " 과세가격-미?

*  MOVE    '0A'         TO IT_ZTIDR-ZFCR_LF.

ENDFORM.                    " P2000_MOVE_ZTIDR

*&---------------------------------------------------------------------*
*&      Form  P2000_MOVE_ZTIDRHSD
*&---------------------------------------------------------------------*
FORM P2000_MOVE_ZTIDRHSD.

  MOVE-CORRESPONDING ZTIDRHSD   TO IT_ZTIDRHSD.

  MOVE ZTIDR-ZFIDRNO            TO IT_ZTIDRHSD-ZFIDRNO.

  WRITE   ZTIDRHSD-NETPR TO W_TEXT_AMOUNT  CURRENCY ZTIDRHSD-ZFCUR.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHSD-NETPR. " 단?

  WRITE   ZTIDRHSD-ZFAMT TO W_TEXT_AMOUNT  CURRENCY ZTIDRHSD-ZFCUR.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHSD-ZFAMT. " 금?

*  MOVE    '0A'         TO IT_ZTIDRHSD-ZFCR_LF.

ENDFORM.                    " P2000_MOVE_ZTIDRHSD

*&---------------------------------------------------------------------*
*&      Form  P2000_MOVE_ZTIDRHS
*&---------------------------------------------------------------------*
FORM P2000_MOVE_ZTIDRHS.

  MOVE-CORRESPONDING ZTIDRHS    TO IT_ZTIDRHS.

  MOVE ZTIDR-ZFIDRNO            TO IT_ZTIDRHS-ZFIDRNO.

  WRITE   ZTIDRHS-ZFTXPER TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFTXPER. " 단위당세?

  WRITE   ZTIDRHS-ZFHSAM TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFHSAM. " 란신고금?

  WRITE   ZTIDRHS-ZFCUAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFCUAMT. " 관?

  WRITE   ZTIDRHS-ZFCCAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFCCAMT. " 관세감면?

  WRITE   ZTIDRHS-ZFHMAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFHMAMT. " 내국?

  WRITE   ZTIDRHS-ZFHCAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFHCAMT. " 내국세감면?

  WRITE   ZTIDRHS-ZFEDAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFEDAMT. " 교육?

  WRITE   ZTIDRHS-ZFECAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFECAMT. " 교육세감면?

  WRITE   ZTIDRHS-ZFAGAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFAGAMT. " 농특?

  WRITE   ZTIDRHS-ZFVAAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFVAAMT. " 부가?

  WRITE   ZTIDRHS-ZFVCAMT TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFVCAMT. " 부가세감면?

  WRITE   ZTIDRHS-ZFTBAK TO W_TEXT_AMOUNT  CURRENCY 'KRW'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFTBAK. " 과세가격-원?

  WRITE   ZTIDRHS-ZFTBAU TO W_TEXT_AMOUNT  CURRENCY 'USD'.
  PERFORM P2000_WRITE_NO_MASK CHANGING W_TEXT_AMOUNT.
  MOVE    W_TEXT_AMOUNT TO IT_ZTIDRHS-ZFTBAU. " 과세가격-미?

*  MOVE    '0A'         TO IT_ZTIDRHS-ZFCR_LF.

ENDFORM.                    " P2000_MOVE_ZTIDRHS
