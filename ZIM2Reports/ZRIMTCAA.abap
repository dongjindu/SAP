*&---------------------------------------------------------------------
*& Report  ZRIMTCAA
*&---------------------------------------------------------------------
*
*&  프로그램명 : 세금계산서 접수/편성취소/EDI Flat File 형?
*&      작성자 : 김연중 INFOLINK Ltd.
*&      작성일 : 2000.03.07
*&---------------------------------------------------------------------
*&   DESC.     : 세금계산서를 조회하여 접수처리하거나 편성을 취소한다.
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMTCAA    MESSAGE-ID ZIM
                    LINE-SIZE 156
                    NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       LIFNR           LIKE   ZTVT-LIFNR,          " Vendor
       LIFNR_NM(21)    TYPE   C,                   " Vendor Name
       J_1KFREPRE(11)  TYPE   C,                   " 대표자?
       STCD2           LIKE   LFA1-STCD2,          " 사업자등록번?
       ZFVCDT          LIKE   ZTVT-ZFVCDT,         " 작성?
       ZFDOCNO         LIKE   ZTVT-ZFDOCNO,        " 전자문서번?
       ZFVTSEQ         LIKE   ZTVT-ZFVTSEQ,        " 세금계산서일련번?
       ZFVTNO          LIKE   ZTVT-ZFVTNO,         " 세금계산서관리번?
       ZFVTRYN         LIKE   ZTVT-ZFVTRYN,        " 접수구분.
       ZFVTRYNTX(08),
       ZFTSAMK         LIKE   ZTVT-ZFTSAMK,        " 공급가액 합?
       ZFTTXAM         LIKE   ZTVT-ZFTTXAM,        " 세액 합?
       ZFEDIST         LIKE   ZTVT-ZFEDIST,        " EDI Status
       ZFEDISTTX(10),
       ZFEDICK         LIKE   ZTVT-ZFEDICK,        " EDI Check
       ZFEDICKTX(10),
       ZFDHENO         LIKE   ZTDHF1-ZFDHENO,      " 문서관리번호.
       BAHNS(08)       TYPE   C.                   " EDI 업체.
DATA : END OF IT_TAB.
*----------------------------------------------------------------------
* Tables 및 변수 Define
*----------------------------------------------------------------------
INCLUDE   ZRIMTCAATOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

INCLUDE   <ICON>.
*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_LIFNR   FOR ZTVT-LIFNR,          " Vendor
                   S_VCDT    FOR ZTVT-ZFVCDT,         " 작성일(발행일)
                   S_DOCNO   FOR ZTVT-ZFDOCNO,        " 전자문서번?
                   S_VTSEQ   FOR ZTVT-ZFVTSEQ,        " 세금계산서 일?
                   S_VTNO    FOR ZTVT-ZFVTNO.         " 세금계산서 관?

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

* 접수구?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-002, POSITION 1.
     SELECTION-SCREEN : COMMENT 32(4) TEXT-021, POSITION 37.
     PARAMETERS : P_N      RADIOBUTTON GROUP RDG.     " None
     SELECTION-SCREEN : COMMENT 40(8) TEXT-022, POSITION 49.
     PARAMETERS : P_M      RADIOBUTTON GROUP RDG.     " 수기접?
     SELECTION-SCREEN : COMMENT 52(8) TEXT-023, POSITION 61.
     PARAMETERS : P_E      RADIOBUTTON GROUP RDG.     " EDI 접?
     SELECTION-SCREEN : COMMENT 64(3) TEXT-024, POSITION 68.
     PARAMETERS : P_A      RADIOBUTTON GROUP RDG.     " ALL
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU' AND INCLUDE  NE 'POER'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
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

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
            W_FIELD_NM = 'ZFREQDT'.
            ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
            PERFORM HANDLE_SORT TABLES  IT_TAB
                                USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISPT' OR 'DISCR'.            " 세금계산서 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_ZTVT USING IT_SELECTED-ZFVTNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
*>> 인수증 편성 SIMULATION.
      WHEN 'SIRD'.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P2000_WRITE_IT.
*>> ERROR 사항 CHECK하여서 ERROR LIST DISPLAY.
            DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
            IF W_LINE GT 0.
               INCLUDE = 'POER'.
               CALL SCREEN 0200 STARTING AT  05   3
                                ENDING   AT  100 12.
               CLEAR : INCLUDE.
               EXIT.
            ENDIF.

*>> SIMULATION 화면 CALL.
            MOVE  'POPU'   TO  INCLUDE.
            CALL  SCREEN   0100  STARTING  AT 002  03
                                 ENDING    AT 122  12.
            CLEAR  INCLUDE.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            PERFORM RESET_LIST.
*>>인수증 편성 자료 DB 반영.
      WHEN 'CRRD'.
            DESCRIBE  TABLE IT_ZTRED LINES W_LINE.
            IF W_LINE LE  0.
               MESSAGE  S766.
               EXIT.
            ENDIF.
            PERFORM P3000_CREATE_ZTRED USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.
               MESSAGE I849.
               PERFORM  P3000_DB_ROLLBACK.
               EXIT.
            ENDIF.
            W_PROC_CNT = W_PROC_CNT - 1 .
            MESSAGE  S850   WITH W_PROC_CNT.
            SET  SCREEN  0. LEAVE SCREEN.
      WHEN 'FLAT'.                                " FLAT 테이블 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_FLAT USING IT_SELECTED-ZFDOCNO.
                 ELSEIF W_SELECTED_LINES GT 1.
                        MESSAGE E965.
                 ELSEIF W_SELECTED_LINES EQ 0.
                        MESSAGE S766.
                        EXIT.
            ENDIF.
      WHEN 'CKEK'.                  " EDI 문서 검?
            PERFORM P2000_MULTI_SELECTION.
              IF W_SELECTED_LINES EQ 1.
                 READ TABLE IT_SELECTED INDEX 1.
                 PERFORM P2000_EDI_DOC_CHECK.
              ELSEIF W_SELECTED_LINES GT 1.
                     MESSAGE E965.
              ELSEIF W_SELECTED_LINES EQ 0.
                     MESSAGE S766.
                     EXIT.
               ENDIF.
      WHEN 'TCAA'.                   " 접?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_ACC_ZTVT USING W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
               PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
               PERFORM RESET_LIST.
               MESSAGE S838.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'TCAC'.                   " 접수취?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_ACC_CAN_ZTVT USING W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
               PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
               PERFORM RESET_LIST.
               MESSAGE S839.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'CAVT'.                   " 편성취?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P3000_ZTVT_DELETE USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
            MESSAGE S836 WITH W_PROC_CNT.
      WHEN 'EDIS'.                   " EDI Flat File 형?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P3000_ZTVT_FLAT_CREATE USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
            MESSAGE S868 WITH W_PROC_CNT.
      WHEN 'REVK'.                   " EDI Flat File 형?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P3000_ZTVT_FLAT_CANCLE USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
            MESSAGE S869 WITH W_PROC_CNT.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
*------- Abbrechen (CNCL) ----------------------------------------------
      WHEN 'CNCL'.
         SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
      WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
      WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
      WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
      WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
      WHEN 'WESU'.
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.
  SET  TITLEBAR 'ZIMA5'.           " GUI TITLE SETTING..
  P_N = 'X'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /38  '[ 세금계산서 접수/편성취소/FLAT File 형성 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM,  96 'Page : ', W_PAGE.
  WRITE : / SY-ULINE(117).
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'Vendor    '                     NO-GAP, SY-VLINE NO-GAP,
            '발행일    '                     NO-GAP, SY-VLINE NO-GAP,
            '대표자명   '                    NO-GAP, SY-VLINE NO-GAP,
            '세금계산서 일련번호'            NO-GAP,
            '                    '           NO-GAP, SY-VLINE NO-GAP,
            '접수상태'                       NO-GAP, SY-VLINE NO-GAP,
            '전자문서번호     '              NO-GAP, SY-VLINE NO-GAP,
            'EDI Status'                     NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            '                     '          NO-GAP, SY-VLINE NO-GAP,
            '사업자번호 '                    NO-GAP, SY-VLINE NO-GAP,
            '           공급가액'            NO-GAP, SY-VLINE NO-GAP,
            '               세액'            NO-GAP, SY-VLINE NO-GAP,
            'EDI 업체'                       NO-GAP, SY-VLINE NO-GAP,
            '관리번호         '              NO-GAP, SY-VLINE NO-GAP,
            'EDI Check '                     NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE(117).

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.

   SET PF-STATUS 'ZIMA5'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMA5'.           " GUI TITLE SETTING..

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

*&---------------------------------------------------------------------
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------
FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-LIFNR           NO-GAP, " Vendor
       SY-VLINE NO-GAP,
       IT_TAB-ZFVCDT          NO-GAP, " 작성?
       SY-VLINE NO-GAP,
       IT_TAB-J_1KFREPRE      NO-GAP, " 대표자?
       SY-VLINE NO-GAP,
       IT_TAB-ZFVTSEQ         NO-GAP, " 세금계산서 일련번?
       '    '                 NO-GAP,
       SY-VLINE NO-GAP.
*>> 접수상태.
  IF IT_TAB-ZFVTRYN EQ 'N'.
     WRITE : IT_TAB-ZFVTRYNTX NO-GAP COLOR COL_GROUP INTENSIFIED OFF.
  ELSE.
     WRITE : IT_TAB-ZFVTRYNTX NO-GAP COLOR COL_POSITIVE INTENSIFIED OFF.
  ENDIF.

  WRITE:SY-VLINE NO-GAP,
       IT_TAB-ZFDOCNO         NO-GAP, " 전자문서번?
       SY-VLINE NO-GAP.
*>> EDI STATUS.
  CASE IT_TAB-ZFEDIST.
     WHEN 'N'.
        WRITE : IT_TAB-ZFEDISTTX NO-GAP
                                    COLOR COL_NORMAL INTENSIFIED ON,
                SY-VLINE NO-GAP.
     WHEN 'S'.
        WRITE : IT_TAB-ZFEDISTTX NO-GAP
                                    COLOR COL_GROUP INTENSIFIED OFF,
                SY-VLINE NO-GAP.
     WHEN 'R'.
        WRITE : IT_TAB-ZFEDISTTX NO-GAP
                                    COLOR COL_POSITIVE INTENSIFIED OFF,
                SY-VLINE NO-GAP.
     WHEN 'T'.
        WRITE : IT_TAB-ZFEDISTTX NO-GAP
                                    COLOR COL_NEGATIVE INTENSIFIED OFF,
                SY-VLINE NO-GAP.
     WHEN 'E'.
        WRITE : IT_TAB-ZFEDISTTX NO-GAP
                                    COLOR COL_NEGATIVE INTENSIFIED OFF,
                SY-VLINE NO-GAP.
  ENDCASE.

* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-LIFNR_NM        NO-GAP, " Vendor Name
       SY-VLINE NO-GAP,
       IT_TAB-STCD2           NO-GAP, " 사업자등록번?
       SY-VLINE NO-GAP,
       IT_TAB-ZFTSAMK CURRENCY 'KRW'  NO-GAP, " 공급가액-원?
       SY-VLINE NO-GAP,
       IT_TAB-ZFTTXAM CURRENCY 'KRW'  NO-GAP, " 세?
       SY-VLINE NO-GAP,
       IT_TAB-BAHNS           NO-GAP, " EDI 업?
       SY-VLINE NO-GAP,
       IT_TAB-ZFVTNO          NO-GAP, " 세금계산서 관리번?
       '       '              NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFEDICKTX       NO-GAP, " EDI Check
       SY-VLINE NO-GAP.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.
  WRITE : / SY-ULINE(117).

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

*>> 조건 값 SETTING
  IF P_N EQ 'X'.
     MOVE   'N%'  TO  W_YN.
  ENDIF.
  IF P_M EQ 'X'.
     MOVE   'M%'  TO  W_YN.
  ENDIF.
  IF P_E EQ 'X'.
     MOVE   'E%'  TO  W_YN.
  ENDIF.
  IF P_A EQ 'X'.
     MOVE   '%'   TO  W_YN.
  ENDIF.

  REFRESH IT_TAB.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM     ZTVT
  WHERE    LIFNR     IN    S_LIFNR
  AND      ZFVCDT    IN    S_VCDT
  AND      ZFDOCNO   IN    S_DOCNO
  AND      ZFVTNO    IN    S_VTNO
  AND      ZFVTRYN   LIKE  W_YN.

*>> TEXT 값 SETTING!
  LOOP  AT  IT_TAB.

     MOVE  SY-TABIX  TO  W_TABIX.
     CLEAR  LFA1.
     SELECT SINGLE * FROM LFA1 WHERE  LIFNR  EQ  IT_TAB-LIFNR.

     MOVE  : LFA1-NAME1          TO  IT_TAB-LIFNR_NM,
             LFA1-J_1KFREPRE     TO  IT_TAB-J_1KFREPRE,
             LFA1-STCD2          TO  IT_TAB-STCD2.
     IF  LFA1-BAHNS IS INITIAL.
         MOVE ' 미등록 '      TO IT_TAB-BAHNS.
     ELSE.
         MOVE ' 등  록 '      TO IT_TAB-BAHNS.
     ENDIF.
*>> 접수상태.
     CASE IT_TAB-ZFVTRYN.
        WHEN 'N'.
             IT_TAB-ZFVTRYNTX = ' 미접수 '.
        WHEN 'M'.
             IT_TAB-ZFVTRYNTX = '수기접수'.
        WHEN 'E'.
             IT_TAB-ZFVTRYNTX = 'EDI 접수'.
     ENDCASE.
*>> EDI STATUS.
     CASE IT_TAB-ZFEDIST.
        WHEN 'N'.
             IT_TAB-ZFEDISTTX  = ' None     '.
        WHEN 'S'.
             IT_TAB-ZFEDISTTX  = ' 의뢰     '.
        WHEN 'R'.
             IT_TAB-ZFEDISTTX  = ' 완료     '.
        WHEN 'T'.
             IT_TAB-ZFEDISTTX  = ' 전송오류 '.
        WHEN 'E'.
             IT_TAB-ZFEDISTTX  = ' 변환오류 '.
     ENDCASE.

     CASE IT_TAB-ZFEDICK.
        WHEN 'O'.
             IT_TAB-ZFEDICKTX  = ' Okay     '.
        WHEN 'X'.
             IT_TAB-ZFEDICKTX  = ' Not Okay '.
     ENDCASE.

     MODIFY  IT_TAB  INDEX  W_TABIX.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     MESSAGE S738.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : IT_TAB-ZFVTNO   TO IT_SELECTED-ZFVTNO,
                IT_TAB-LIFNR    TO IT_SELECTED-LIFNR,
                IT_TAB-ZFDOCNO  TO IT_SELECTED-ZFDOCNO.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------
*&      Form  P2000_SHOW_ZTVT
*&---------------------------------------------------------------------
FORM P2000_SHOW_ZTVT USING    P_ZFVTNO.

   SET PARAMETER ID 'ZPVTNO'  FIELD P_ZFVTNO.
   EXPORT 'ZPVTNO'  TO MEMORY ID 'ZPVTNO'.

   IF SY-UCOMM EQ 'DISCR'.
     CALL TRANSACTION 'ZIMA9' AND SKIP FIRST SCREEN.
   ELSE.
     CALL TRANSACTION 'ZIMA4' AND SKIP FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_ZTVT
*&---------------------------------------------------------------------
*&      Form  P2000_SHOW_FLAT
*&---------------------------------------------------------------------
FORM P2000_SHOW_FLAT USING    P_ZFDOCNO.

    SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ P_ZFDOCNO
            AND RETURN.

ENDFORM.                    " P2000_SHOW_FLAT

*&---------------------------------------------------------------------
*&      Form  P3000_ZTVT_DELETE
*&---------------------------------------------------------------------
FORM P3000_ZTVT_DELETE USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.

  PERFORM P2000_DELETE_MESSAGE.

  IF ANTWORT NE 'Y'.
     EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
       SELECT SINGLE *
         FROM ZTVT
        WHERE ZFVTNO = IT_SELECTED-ZFVTNO.      " 세금계산?
       IF ZTVT-ZFVTRYN NE 'N' OR ZTVT-ZFEDIST NE 'N'.
          CONTINUE.
       ENDIF.
       SELECT COUNT( DISTINCT ZFREDNO ) INTO W_COUNT
         FROM ZTRED
        WHERE ZFVTNO = ZTVT-ZFVTNO.
       IF W_COUNT > 0.
          CONTINUE.
       ENDIF.

       DELETE FROM ZTVT
        WHERE ZFVTNO = IT_SELECTED-ZFVTNO.      " 세금계산?
       DELETE FROM ZTVTSG1
        WHERE ZFVTNO = IT_SELECTED-ZFVTNO.  " 세금계산서 Seg1
       DELETE FROM ZTVTSG3
        WHERE ZFVTNO = IT_SELECTED-ZFVTNO. " 세금계산서 Seg3
       UPDATE ZTVTIV SET ZFVTNO = '          '
                    UNAM = SY-UNAME
                    UDAT = SY-DATUM
        WHERE ZFVTNO = IT_SELECTED-ZFVTNO.
       IF SY-SUBRC NE 0.
          MESSAGE E833.
          MOVE 'N' TO W_ERR_CHK.
          EXIT.
       ENDIF.
       ADD 1 TO W_PROC_CNT.
  ENDLOOP.

ENDFORM.                    " P3000_ZTVT_DELETE

*&---------------------------------------------------------------------
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------
FORM P2000_DELETE_MESSAGE.

   PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                           '현재 Document를 삭제합니다.'
                           '삭제하시겠습니까?' " MSG2
                           'N'                 " 취소 버튼 유/?
                           '1'.                " default button

ENDFORM.                    " P2000_DELETE_MESSAGE

*&---------------------------------------------------------------------
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------
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

   IF ANTWORT = 'C'.       " Cancel
       SET SCREEN SY-DYNNR.
   ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX

*&---------------------------------------------------------------------
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

   IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
   ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT

*&---------------------------------------------------------------------
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------
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

*&---------------------------------------------------------------------
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT

*&---------------------------------------------------------------------
*&      Form  P2000_ACC_ZTVT
*&---------------------------------------------------------------------
FORM P2000_ACC_ZTVT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  SELECT SINGLE *
    FROM ZTVT
  WHERE ZFVTNO = IT_SELECTED-ZFVTNO.      " 세금계산?
  IF ZTVT-ZFVTRYN NE 'N' OR ZTVT-ZFEDIST NE 'N'.
     MESSAGE E837.
     MOVE 'Y' TO W_ERR_CHK.
     EXIT.
  ENDIF.

  UPDATE ZTVT SET ZFVTRYN = 'M'
                  UNAM = SY-UNAME
                  UDAT = SY-DATUM
   WHERE ZFVTNO = IT_SELECTED-ZFVTNO.

ENDFORM.                    " P2000_ACC_ZTVT

*&---------------------------------------------------------------------
*&      Form  P2000_ACC_CAN_ZTVT
*&---------------------------------------------------------------------
FORM P2000_ACC_CAN_ZTVT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  SELECT SINGLE *
    FROM ZTVT
  WHERE ZFVTNO = IT_SELECTED-ZFVTNO.      " 세금계산?
  IF ZTVT-ZFVTRYN NE 'M' OR ZTVT-ZFEDIST NE 'N'.
     MESSAGE E840.
     MOVE 'Y' TO W_ERR_CHK.
     EXIT.
  ENDIF.

  UPDATE ZTVT SET ZFVTRYN = 'N'
                  UNAM = SY-UNAME
                  UDAT = SY-DATUM
   WHERE ZFVTNO = IT_SELECTED-ZFVTNO.

ENDFORM.                    " P2000_ACC_CAN_ZTVT

*&---------------------------------------------------------------------
*&      Form  P3000_ZTVT_FLAT_CREATE
*&---------------------------------------------------------------------
FORM P3000_ZTVT_FLAT_CREATE USING    W_ERR_CHK.

  CLEAR W_PROC_CNT.
  SELECT SINGLE *
    FROM ZTVT
   WHERE ZFVTNO = IT_SELECTED-ZFVTNO.
  IF ZTVT-ZFEDIST NE 'N' OR ZTVT-ZFEDICK NE 'O'.
     EXIT.
  ENDIF.
  W_ZFCDDOC = 'VATBIL'.
  SELECT SINGLE BAHNS INTO W_ZFDHSRO
    FROM LFA1
   WHERE LIFNR = IT_SELECTED-LIFNR.
  W_ZFDHSRO = 'TESTID'.        " 임?
*  W_ZFDHSRO = W_LFA1-BAHNS.        " 식별?
*  CLEAR : W_ZFDHREF, W_ZFDHDDB, W_ZFDHENO.
  CLEAR : W_ZFDHREF, W_ZFDHENO.
*  W_ZFDHREF = ZTREQHD-ZFREQNO.     " 참조번?
*  W_ZFDHDDB = ZTREQST-EKORG.       " 부?
*  W_ZFDHENO = ZTREQST-ZFDOCNO.     " 문서번?

  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
             W_ZFCDDOC    =   W_ZFCDDOC
             W_ZFDHSRO    =   W_ZFDHSRO
             W_ZFDHREF    =   W_ZFDHREF
*             W_ZFDHDDB    =   W_ZFDHDDB
             W_BUKRS        =   ZTVT-BUKRS
       CHANGING
             W_ZFDHENO    =   W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4
             NO_TYPE      =   8.

  CASE SY-SUBRC.
     WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO. EXIT.
     WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC. EXIT.
  ENDCASE.

*----------------------------------------------------------------------
* ITEM DATA CREATE
*----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_VATBIL_EDI_SEND'
       EXPORTING
             W_ZFVTNO     =    IT_SELECTED-ZFVTNO
             W_ZFDHENO    =    W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4.

  CASE SY-SUBRC.
     WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO. EXIT.
     WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC. EXIT.
  ENDCASE.

  ADD 1 TO W_PROC_CNT.
  UPDATE ZTVT SET ZFEDIST = 'S'
                  ZFDOCNO =  W_ZFDHENO
                  ZFVTRNO =  W_ZFDHENO
                  UNAM = SY-UNAME
                  UDAT = SY-DATUM
   WHERE ZFVTNO = IT_SELECTED-ZFVTNO.

ENDFORM.                    " P3000_ZTVT_FLAT_CREATE

*&---------------------------------------------------------------------
*&      Form  P3000_ZTVT_FLAT_CANCLE
*&---------------------------------------------------------------------
FORM P3000_ZTVT_FLAT_CANCLE USING    W_ERR_CHK.

  CLEAR W_PROC_CNT.
  SELECT SINGLE *
    FROM ZTVT
   WHERE ZFVTNO = IT_SELECTED-ZFVTNO.
  IF ZTVT-ZFEDIST NE 'S'.
     EXIT.
  ENDIF.

* FLAT ITEM TABLE
  DELETE FROM ZTDHF1 WHERE ZFDHENO EQ ZTVT-ZFDOCNO.
  DELETE FROM ZTDDF1 WHERE ZFDDENO EQ ZTVT-ZFDOCNO.

  ADD 1 TO W_PROC_CNT.
  UPDATE ZTVT SET ZFEDIST = 'N'
                  UNAM = SY-UNAME
                  UDAT = SY-DATUM
   WHERE ZFVTNO = IT_SELECTED-ZFVTNO.

ENDFORM.                    " P3000_ZTVT_FLAT_CANCLE
*&---------------------------------------------------------------------
*&      Form  P2000_EDI_DOC_CHECK
*&---------------------------------------------------------------------
FORM P2000_EDI_DOC_CHECK.

  SELECT SINGLE * FROM ZTVTSG1
         WHERE ZFVTNO = IT_SELECTED-ZFVTNO.
  SELECT SINGLE * FROM ZTVT
         WHERE ZFVTNO = IT_SELECTED-ZFVTNO.
  SELECT SINGLE * FROM LFA1
         WHERE LIFNR = IT_SELECTED-LIFNR.


  IF ZTVTSG1-ZFTXN1 IS INITIAL. " 공급자 사업자등록번?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급자 사업자등록번호'.
  ENDIF.
  IF ZTVTSG1-ZFCONM1 IS INITIAL. " 공급자 상?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급자상호'.
  ENDIF.
  IF ZTVTSG1-ZFCHNM1 IS INITIAL. " 공급자 대표자?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급자 대표자명'.
  ENDIF.
  IF ZTVTSG1-ZFADD11 IS INITIAL. " 공급자 주소 1
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급자 주소 1'.
  ENDIF.
  IF ZTVTSG1-ZFTXN2 IS INITIAL. " 공급받는자 사업자등록번?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급받는자 사업자등록번호'.
  ENDIF.
  IF ZTVTSG1-ZFCONM2 IS INITIAL. " 공급받는자 상?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급자받는자 상호'.
  ENDIF.
  IF ZTVTSG1-ZFCHNM2 IS INITIAL. " 공급받는자 대표자?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급받는자 대표자명'.
  ENDIF.
  IF ZTVTSG1-ZFADD12 IS INITIAL. " 공급받는자 주소 1
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급받는자 주소 1'.
  ENDIF.
  IF ZTVT-ZFVCDT IS INITIAL.     " 세금계산서 작성?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '세금계산서 작성일'.
  ENDIF.
  IF ZTVT-ZFVERC IS INITIAL.      " 공란?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공란수'.
  ENDIF.
  IF ZTVT-ZFVTAMT IS INITIAL.     " 공급가?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH '공급가액'.
  ENDIF.
  IF LFA1-BAHNS IS INITIAL.       " EDI 식별?
     MOVE 'X' TO ZTVT-ZFEDICK.
     MESSAGE E167 WITH 'EDI 식별자'.
  ENDIF.
     MESSAGE I260 WITH ZTVT-ZFVTNO.

ENDFORM.                    " P2000_EDI_DOC_CHECK
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '인수증 Simulation'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POPU'.
         PERFORM  P1000_CREATE_ZTRED_SCR0100.
         PERFORM  P2000_WRITE_TITLE_SCR0100.
         PERFORM  P3000_WRITE_ZTRED_SCR0100.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_CREATE_ZTRED_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_CREATE_ZTRED_SCR0100.

  REFRESH : IT_SIM.
  CLEAR :   W_PROC_CNT, SV_EBELN.

  SORT IT_ZTRED BY EBELN EBELP.

  LOOP AT IT_ZTRED.

*>> 접수된 세금계산서에 한해서 인수증 발급.
       SELECT SINGLE * FROM  ZTVT
       WHERE  ZFVTNO   EQ    IT_ZTRED-ZFVTNO.
       IF ZTVT-ZFVTRYN EQ    'N'.  CONTINUE.  ENDIF.

*>> PO 별로 인수증 발급.
       IF SV_EBELN NE IT_ZTRED-EBELN.
          PERFORM P3000_INSERT_ZTRED_SCR0100.
          MOVE : IT_ZTRED-EBELN  TO  SV_EBELN,
                 IT_ZTRED-LIFNR  TO  SV_LIFNR.
       ENDIF.

       CLEAR : IT_SIM, ZVVTIV_IT.

*>> 인수증 ITEM은 PO ITEM 별로.
       SELECT SINGLE * FROM MAKT
       WHERE  MATNR  EQ  IT_ZTRED-MATNR
       AND    SPRAS  EQ  SY-LANGU.

       MOVE : IT_ZTRED-MATNR     TO  IT_SIM-MATNR,
              IT_ZTRED-EBELN     TO  IT_SIM-EBELN,
              IT_ZTRED-EBELP     TO  IT_SIM-EBELP,
              IT_ZTRED-ZFREAM    TO  IT_SIM-ZFREAM,
              IT_ZTRED-ZFREAMC   TO  IT_SIM-ZFREAMC,
              IT_ZTRED-ZFREAMK   TO  IT_SIM-ZFREAMK,
              '2'                TO  IT_SIM-SORT,
              MAKT-MAKTX         TO  IT_SIM-MAKTX.
       APPEND IT_SIM.

  ENDLOOP.

  PERFORM P3000_INSERT_ZTRED_SCR0100.

  SORT  IT_SIM   BY  EBELN  SORT  EBELP.
ENDFORM.                    " P1000_CREATE_ZTRED_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P3000_INSERT_ZTRED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_INSERT_ZTRED USING    W_ERR_CHK.

  ADD   1  TO W_PROC_CNT.
  IF W_PROC_CNT = 1. EXIT. ENDIF.

  CLEAR  ZTRED.
  MOVE : SY-MANDT           TO   ZTRED-MANDT,
         SV_BUKRS           TO   ZTRED-BUKRS,
         SV_ZFVTNO          TO   ZTRED-ZFVTNO,
         W_ZFREDNO          TO   ZTRED-ZFREDNO,
         SV_LIFNR           TO   ZTRED-LIFNR,
         SV_EBELN           TO   ZTRED-EBELN.

*>> 인수일, 발급일 GET.
  SELECT SINGLE ZFVCDT INTO ZTRED-ZFREVDT
  FROM   ZTVT
  WHERE ZFVTNO   EQ   SV_ZFVTNO.
  ZTRED-ZFISUDT = ZTRED-ZFREVDT + 9.

*>> 발급번호 GET.
  SELECT MAX( ZFOPNNO ) MAX( ZFREQNO )
  INTO   (W_ZFOPNNO, W_ZFREQNO)
  FROM   ZTREQHD
  WHERE  EBELN     EQ  SV_EBELN.
*>> 공급자 상호 GET.
  SELECT SINGLE NAME1 INTO  ZTRED-ZFSCONM
  FROM   LFA1
  WHERE  LIFNR = SV_LIFNR.
*>> 수령인 자료 GET.
  SELECT SINGLE * FROM ZTIMIMGTX  WHERE  BUKRS  EQ  SV_BUKRS.
*>> 인수금액 GET
  SELECT SUM( ZFIVAMT )  MAX( ZFIVAMC )  SUM( ZFKAMT )
         SUM( ZFQUN )    MAX( ZFQUNM )
  INTO   (ZTRED-ZFREAMF, ZTRED-ZFREAMFC, ZTRED-ZFREAMK,
          ZTRED-ZFTQUN,  ZTRED-ZFTQUNM)
  FROM   ZVVTIV_IT
  WHERE  EBELN  EQ  SV_EBELN.
*>> 수입의뢰 HEAD GET.
  SELECT SINGLE * FROM ZTREQHD  WHERE  ZFREQNO  EQ  W_ZFREQNO.
*>> OPEN BANK GET.
  SELECT SINGLE * FROM LFA1     WHERE  LIFNR    EQ  ZTREQHD-ZFOPBN.
*>> 은행 지점.
  SELECT SINGLE * FROM ZTLLCHD  WHERE  ZFREQNO  EQ  W_ZFREQNO.
*>>인수발급번호.
  CONCATENATE 'S' SY-DATUM 'LOCRCT' W_ZFOPNNO INTO ZTRED-ZFISNO.
*>> LAST 개설내역 SELECT.
  SELECT MAX( ZFAMDNO ) INTO    W_ZFAMDNO
  FROM   ZTREQST        WHERE  ZFREQNO = W_ZFREQNO.
  IF W_ZFAMDNO > 0.
     SELECT SINGLE * FROM  ZTLLCAMHD
     WHERE  ZFREQNO = W_ZFREQNO  AND ZFAMDNO = W_ZFAMDNO.
     MOVE : ZTLLCAMHD-ZFNGDDT   TO   ZTRED-ZFGDDT,
            ZTLLCAMHD-ZFNEXDT   TO   ZTRED-ZFEXDT,
            ZTLLCAMHD-ZFEXRT    TO   ZTRED-ZFLEXRT.
  ELSE.
     SELECT SINGLE *  FROM ZTLLCHD WHERE ZFREQNO = W_ZFREQNO.
     MOVE : ZTLLCHD-ZFGDDT     TO    ZTRED-ZFGDDT,
            ZTLLCHD-ZFEXDT     TO    ZTRED-ZFEXDT,
            ZTLLCHD-ZFEXRT     TO    ZTRED-ZFLEXRT.
  ENDIF.

  MOVE    : W_ZFOPNNO           TO  ZTRED-ZFLLCON,
            ZTIMIMGTX-ZFELENML  TO  ZTRED-ZFRCONM,
            ZTIMIMGTX-ZFREPREL  TO  ZTRED-ZFRCHNM,
            ZTRED-ZFREAMF       TO  ZTRED-ZFTOTAM,
            ZTRED-ZFREAMFC      TO  ZTRED-ZFTOTAMC,
            ZTREQHD-ZFLASTAM    TO  ZTRED-ZFOPAMF,
            ZTREQHD-WAERS       TO  ZTRED-ZFOPAMFC,
            LFA1-BAHNS          TO  ZTRED-ZFOBNEID,
            ZTLLCHD-ZFOBNM      TO  ZTRED-ZFOBNM,
            ZTLLCHD-ZFOBBR      TO  ZTRED-ZFOBBR,
            'N'                 TO  ZTRED-ZFEDIST,
            'O'                 TO  ZTRED-ZFEDICK,
            SY-UNAME            TO  ZTRED-UNAM,
            SY-DATUM            TO  ZTRED-UDAT,
            SY-UNAME            TO  ZTRED-ERNAM,
            SY-DATUM            TO  ZTRED-CDAT.

  INSERT ZTRED.
  IF SY-SUBRC <> 0.
     W_ERR_CHK = 'Y'.  EXIT.
  ENDIF.

ENDFORM.                    " P3000_INSERT_ZTRED
*&---------------------------------------------------------------------*
*&      Form  P3000_INSERT_ZTRED_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_INSERT_ZTRED_SCR0100.

  ADD   1  TO W_PROC_CNT.
  IF W_PROC_CNT = 1. EXIT. ENDIF.

  MOVE IT_ZTRED-LIFNR     TO IT_SIM-LIFNR.
*>> 발급번호.
  SELECT MAX( ZFOPNNO ) MAX( ZFREQNO )
  INTO   (W_ZFOPNNO, W_ZFREQNO)
  FROM   ZTREQHD
  WHERE  EBELN     EQ  SV_EBELN.
*>> 공급자 상호 GET.
  SELECT SINGLE NAME1 INTO IT_SIM-ZFSCONM " 공급자 상?
  FROM   LFA1
  WHERE  LIFNR = IT_ZTRED-LIFNR.
*>> 인수금액 GET
  SELECT SUM( ZFIVAMT ) MAX( ZFIVAMC ) SUM( ZFKAMT )
         SUM( ZFQUN )   MAX( ZFQUNM )
  INTO   (IT_SIM-ZFTREAMF, IT_SIM-ZFTREAMFC, IT_SIM-ZFTREAMK,
          IT_SIM-ZFTQUN,   IT_SIM-ZFTQUNM)
  FROM   ZVVTIV_IT
  WHERE  EBELN  EQ  SV_EBELN.

  CONCATENATE 'S' SY-DATUM 'LOCRCT' W_ZFOPNNO INTO IT_SIM-ZFISNO.
  MOVE W_ZFOPNNO   TO IT_SIM-ZFLLCON.
  MOVE '1'         TO IT_SIM-SORT.
  APPEND  IT_SIM.

ENDFORM.                    " P3000_INSERT_ZTRED_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_TITLE_SCR0100.

  SKIP 1.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE ,
            '인수증 발급 번호              ' NO-GAP,
            '     '                          NO-GAP, SY-VLINE NO-GAP,
            '공급자              '           NO-GAP, SY-VLINE NO-GAP,
            '              수량  '           NO-GAP, SY-VLINE NO-GAP,
            '     인수금액(원화)'            NO-GAP, SY-VLINE NO-GAP,
            '     인수금액(외화)     '       NO-GAP, SY-VLINE NO-GAP,
            '신용장 번호              '      NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE ,
            '        '                       NO-GAP,
            '품목                          ' NO-GAP,
            '                  '             NO-GAP, SY-VLINE NO-GAP,
            '              수량  '           NO-GAP, SY-VLINE NO-GAP,
            '         금액(원화)'            NO-GAP, SY-VLINE NO-GAP,
            '         금액(외화)     '       NO-GAP, SY-VLINE NO-GAP,
            'PO Document No.          '      NO-GAP, SY-VLINE NO-GAP.

ENDFORM.                    " P2000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ZTRED_SCR0100.

   LOOP  AT  IT_SIM.

      IF IT_SIM-LIFNR NE SPACE.
         WRITE / SY-ULINE.
         FORMAT COLOR COL_NORMAL INTENSIFIED ON.
         WRITE :                                       SY-VLINE ,
                IT_SIM-ZFISNO                  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFSCONM                 NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFTQUN   UNIT IT_SIM-ZFQUNM              NO-GAP,
                IT_SIM-ZFTQUNM                 NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFTREAMK CURRENCY 'KRW' NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFTREAMF CURRENCY IT_SIM-ZFTREAMFC       NO-GAP,
                IT_SIM-ZFTREAMFC               NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFLLCON                 NO-GAP, SY-VLINE NO-GAP.
      ELSE.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         WRITE :                                       SY-VLINE ,
                '        '                     NO-GAP,
                IT_SIM-MATNR                   NO-GAP,
                IT_SIM-MAKTX                   NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFQUN  UNIT IT_SIM-ZFQUNM  NO-GAP,
                IT_SIM-ZFQUNM                  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFREAMK CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFREAM  CURRENCY IT_SIM-ZFREAMC NO-GAP,
                IT_SIM-ZFREAMC                 NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-EBELN                   NO-GAP,
                '-'                            NO-GAP,
                IT_SIM-EBELP                   NO-GAP,
                '         '                    NO-GAP, SY-VLINE NO-GAP.
      ENDIF.

   ENDLOOP.
   WRITE / SY-ULINE.

ENDFORM.                    " P3000_WRITE_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_IT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_IT.

  REFRESH : IT_ZTRED, IT_ERR_LIST.
  CLEAR :   IT_ZTRED, W_ERR_CNT.

  LOOP AT IT_SELECTED.

       SELECT  B~EBELN          B~EBELP     MAX( A~BUKRS )   AS BUKRS
               MAX( A~LIFNR )   AS  LIFNR   MAX( A~ZFVTNO )  AS ZFVTNO
               SUM( B~ZFQUN )   AS  ZFQUN   MAX( B~ZFQUNM )  AS ZFQUNM
               SUM( B~ZFIVAMT ) AS  ZFREAM  MAX( B~ZFIVAMC ) AS ZFREAMC
               SUM( B~ZFKAMT )  AS  ZFREAMK MAX( B~NETPR )   AS NETPR
               MAX( B~BPRME )   AS  BPRME   MAX( B~PEINH )   AS PEINH
               MAX( B~MATNR )   AS  MATNR
       APPENDING  CORRESPONDING FIELDS OF TABLE IT_ZTRED
       FROM    ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
       ON      A~ZFGFDYR    EQ   B~ZFGFDYR
       AND     A~ZFGFDNO    EQ   B~ZFGFDNO
       AND     A~BUKRS      EQ   B~BUKRS
       WHERE   A~ZFVTNO     EQ   IT_SELECTED-ZFVTNO
       AND     A~ZFREDNO    EQ   SPACE
       AND     B~ZFREQTY    EQ   'LO'
       GROUP BY
               B~EBELN   B~EBELP.

*>> ERROR 사항 CHECK!
       SELECT  B~ZFREQTY   A~ZFREDNO
        INTO    (W_ZFREQTY, W_ZFREDNO)
        FROM   ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
        ON     A~ZFGFDYR    EQ   B~ZFGFDYR
        AND    A~ZFGFDNO    EQ   B~ZFGFDNO
        AND    A~BUKRS      EQ   B~BUKRS
        WHERE  A~ZFVTNO     EQ   IT_SELECTED-ZFVTNO.
       ENDSELECT.
*>> ERROR MESSAGE 생성.
       IF  W_ZFREQTY  NE 'LO'.
           MESSAGE S625 WITH IT_SELECTED-ZFVTNO.
           PERFORM  P2000_SINGLE_MESS_MAKE.
       ENDIF.
       IF  W_ZFREDNO NE  SPACE.
           MESSAGE S626 WITH IT_SELECTED-ZFVTNO.
           PERFORM  P2000_SINGLE_MESS_MAKE.
       ENDIF.

  ENDLOOP.

  DESCRIBE TABLE IT_ZTRED LINES W_COUNT.

ENDFORM.                    " P2000_WRITE_IT
*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_ZTRED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_CREATE_ZTRED USING    W_ERR_CHK.

  CLEAR  : W_ZFREDNO, SV_EBELN, SV_LIFNR, SV_ZFVTNO, SV_BUKRS,
           W_PROC_CNT.
  SORT IT_ZTRED BY EBELN EBELP.

  LOOP AT IT_ZTRED.

*>> 접수된 세금계산서에 한해서 인수증 발급.
       SELECT SINGLE * FROM  ZTVT
       WHERE  ZFVTNO   EQ    IT_ZTRED-ZFVTNO.
       IF ZTVT-ZFVTRYN EQ    'N'.  CONTINUE.  ENDIF.

*>> PO 별로 인수증 발급.
       IF SV_EBELN NE IT_ZTRED-EBELN.
          PERFORM P3000_INSERT_ZTRED USING W_ERR_CHK.
          PERFORM P2000_GET_NUMBER_NEXT USING 'RE' W_ZFREDNO.
          MOVE : IT_ZTRED-EBELN  TO  SV_EBELN,
                 IT_ZTRED-LIFNR  TO  SV_LIFNR,
                 IT_ZTRED-ZFVTNO TO  SV_ZFVTNO,
                 IT_ZTRED-BUKRS  TO  SV_BUKRS.
       ENDIF.

       CLEAR : ZTREDSG1, ZVVTIV_IT.

*>> 반복수 GET.
       SELECT MAX( ZFLSG1 )  INTO  W_ZFLSG1
       FROM   ZTREDSG1
       WHERE  ZFREDNO        EQ    W_ZFREDNO.
       W_ZFLSG1  =  W_ZFLSG1 +  1.

*>> HS CODE GET!
       SELECT SINGLE * FROM ZTREQIT
       WHERE  EBELN    EQ   IT_ZTRED-EBELN
       AND    EBELP    EQ   IT_ZTRED-EBELP.
*>> 자재내역 GET.
       SELECT SINGLE * FROM MAKT
       WHERE  MATNR  EQ  ZTREDSG1-MATNR
       AND    SPRAS  EQ  SY-LANGU.

       MOVE : SY-MANDT           TO  ZTREDSG1-MANDT,
              W_ZFREDNO          TO  ZTREDSG1-ZFREDNO,
              W_ZFLSG1           TO  ZTREDSG1-ZFLSG1,
              ZTREQIT-ZFITMNO    TO  ZTREDSG1-ZFITMNO,
              IT_ZTRED-EBELP     TO  ZTREDSG1-EBELP,
              IT_ZTRED-ZFREAM    TO  ZTREDSG1-ZFREAM,
              IT_ZTRED-ZFREAMC   TO  ZTREDSG1-ZFREAMC,
              IT_ZTRED-ZFQUN     TO  ZTREDSG1-ZFQUN,
              IT_ZTRED-ZFQUNM    TO  ZTREDSG1-ZFQUNM,
              IT_ZTRED-NETPR     TO  ZTREDSG1-NETPR,
              IT_ZTRED-PEINH     TO  ZTREDSG1-PEINH,
              IT_ZTRED-BPRME     TO  ZTREDSG1-BPRME,
              IT_ZTRED-MATNR     TO  ZTREDSG1-MATNR,
              ZTREQIT-STAWN      TO  ZTREDSG1-ZFGDNO,
              MAKT-MAKTX         TO  ZTREDSG1-MAKTX.
       INSERT ZTREDSG1.
       IF SY-SUBRC <> 0.
          W_ERR_CHK = 'Y'. EXIT.
       ENDIF.
*>> 세금계산서 송장 DB UPDATE.
       SELECT  BUKRS      ZFGFDYR     ZFGFDNO
       INTO    (W_BUKRS,  W_ZFGFDYR,  W_ZFGFDNO)
       FROM    ZTVTIVIT
       WHERE   EBELN    EQ  IT_ZTRED-EBELN
       AND     EBELP    EQ  IT_ZTRED-EBELP
       GROUP BY
               BUKRS    ZFGFDYR   ZFGFDNO.

           UPDATE  ZTVTIV
           SET     ZFREDNO  =  W_ZFREDNO
                   UNAM     =  SY-UNAME
                   UDAT     =  SY-DATUM
           WHERE   BUKRS    =  W_BUKRS
           AND     ZFGFDYR  =  W_ZFGFDYR
           AND     ZFGFDNO  =  W_ZFGFDNO.
       ENDSELECT.

  ENDLOOP.

  PERFORM P3000_INSERT_ZTRED USING W_ERR_CHK.

ENDFORM.                    " P3000_CREATE_ZTRED
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_ROLLBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_DB_ROLLBACK.

   LOOP  AT  IT_ZTRED.

      CLEAR  ZTRED.
      SELECT SINGLE * FROM ZTRED  WHERE  EBELN EQ IT_ZTRED-EBELN.
*>> 인수증 DB DELETE.
      DELETE FROM ZTRED     WHERE ZFREDNO  EQ  ZTRED-ZFREDNO.
      DELETE FROM ZTREDSG1  WHERE ZFREDNO  EQ  ZTRED-ZFREDNO.
*>> 송장 DB UPDATE.
      SELECT  BUKRS     ZFGFDYR    ZFGFDNO
      INTO    (W_BUKRS, W_ZFGFDYR, W_ZFGFDNO)
      FROM    ZTVTIVIT
      WHERE   EBELN    EQ   IT_ZTRED-EBELN
      AND     EBELP    EQ   IT_ZTRED-EBELP
      GROUP BY
              BUKRS  ZFGFDYR  ZFGFDNO.

         UPDATE  ZTVTIV
         SET     ZFREDNO  =  SPACE
                 UNAM     =  SY-UNAME
                 UDAT     =  SY-DATUM
         WHERE   ZFGFDYR  =  W_ZFGFDYR
         AND     ZFGFDNO  =  W_ZFGFDNO
         AND     BUKRS    =  W_BUKRS.

     ENDSELECT.

   ENDLOOP.

ENDFORM.                    " P3000_DB_ROLLBACK
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0200 OUTPUT.

  SET PF-STATUS 'STDLIST'.

  CASE INCLUDE.
     WHEN 'POER'.
        SET TITLEBAR 'POPU' WITH '상태 LIST'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0200 INPUT.

   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POER'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(96),    /   SY-VLINE NO-GAP,
                   '유형'   NO-GAP,     SY-VLINE NO-GAP,
                   '메세지 텍스트',  94 SY-VLINE NO-GAP,
                   'T'      NO-GAP,     SY-VLINE,
                 / SY-ULINE(96).
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E'.
                  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
               WHEN 'W'.
                  FORMAT COLOR COL_KEY      INTENSIFIED OFF.
               WHEN 'I'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
               WHEN 'S'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
            ENDCASE.

            WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
            HIDE:IT_ERR_LIST.
         ENDLOOP.
         WRITE : / SY-ULINE(96).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SINGLE_MESS_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SINGLE_MESS_MAKE.

   REFRESH : MESSTAB.
   MOVE : 'E'                 TO     MESSTAB-MSGTYP,
          SY-MSGID            TO     MESSTAB-MSGID,
          SY-MSGNO            TO     MESSTAB-MSGNR,
          SY-MSGV1            TO     MESSTAB-MSGV1,
          SY-MSGV2            TO     MESSTAB-MSGV2,
          SY-MSGV3            TO     MESSTAB-MSGV3,
          SY-MSGV4            TO     MESSTAB-MSGV4.
   APPEND MESSTAB.

   PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.

ENDFORM.                    " P2000_SINGLE_MESS_MAKE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

   LOOP AT MESSTAB.

      MOVE : MESSTAB-MSGTYP           TO     IT_ERR_LIST-MSGTYP,
             MESSTAB-MSGID            TO     IT_ERR_LIST-MSGID,
             MESSTAB-MSGNR            TO     IT_ERR_LIST-MSGNR,
             MESSTAB-MSGV1            TO     IT_ERR_LIST-MSGV1,
             MESSTAB-MSGV2            TO     IT_ERR_LIST-MSGV2,
             MESSTAB-MSGV3            TO     IT_ERR_LIST-MSGV3,
             MESSTAB-MSGV4            TO     IT_ERR_LIST-MSGV4.


       CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
                   MSGID               = IT_ERR_LIST-MSGID
                   MSGNR               = IT_ERR_LIST-MSGNR
                   MSGV1               = IT_ERR_LIST-MSGV1
                   MSGV2               = IT_ERR_LIST-MSGV2
                   MSGV3               = IT_ERR_LIST-MSGV3
                   MSGV4               = IT_ERR_LIST-MSGV4
            IMPORTING
                   MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
         WHEN OTHERS.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      ENDCASE.

      APPEND  IT_ERR_LIST.

   ENDLOOP.

ENDFORM.                    " P2000_MESSAGE_MAKE
