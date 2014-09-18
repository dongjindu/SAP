*&---------------------------------------------------------------------*
*& Report  ZRIMCLTC                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입신고(과세통관) 자료 생성                          *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.25                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Invoice 자료를 조회하여 수입신고자료(과세통관)?
*&               생성한다.
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMCLTC    MESSAGE-ID ZIM
                    LINE-SIZE 101
                    NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFHBLNO         LIKE   ZTBL-ZFHBLNO,        " House B/L No
       ZFMBLNO         LIKE   ZTBL-ZFMBLNO,
       ZFCLSEQ         LIKE   ZTCUCLIV-ZFCLSEQ,    " 통관순?
       ZFETA           LIKE   ZTBL-ZFETA,          " 도착일(ETA)
       ZFCCDT          LIKE   ZTCUCLIV-ZFCCDT,     " Invoice 입력?
       ZFBLNO          LIKE   ZTCUCLIV-ZFBLNO,     " B/L 관리번?
       ZFIVNO          LIKE   ZTCUCLIV-ZFIVNO,     " Invoice 관리번?
       MJAHR           LIKE   ZTCUCLIV-MJAHR,      " Material doc. year
       MBLNR           LIKE   ZTCUCLIV-MBLNR,      " Material doc. No
       ZFCUST          LIKE   ZTCUCLIV-ZFCUST.     " 통관상?
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMCLTCTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_HBLNO  FOR ZTBL-ZFHBLNO,      " House B/L No
                   S_CLSEQ  FOR ZTCUCLIV-ZFCLSEQ,  " 통관순?
                   S_ETA    FOR ZTBL-ZFETA,        " 도착일(ETA)
                   S_BLNO   FOR ZTCUCLIV-ZFBLNO,   " B/L 관리번?
                   S_IVNO   FOR ZTCUCLIV-ZFIVNO,   " Invoice  관리번?
                   S_CDAT   FOR ZTCUCLIV-CDAT.     " Invoice 입력?

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

* 통관상?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-002, POSITION 1.
     SELECTION-SCREEN : COMMENT 32(8) TEXT-021, POSITION 41.
     PARAMETERS : P_CU1    AS CHECKBOX.              " 생성대?
     SELECTION-SCREEN : COMMENT 44(8) TEXT-022, POSITION 53.
     PARAMETERS : P_CU2    AS CHECKBOX.              " 의뢰대?
     SELECTION-SCREEN : COMMENT 58(6) TEXT-023, POSITION 65.
     PARAMETERS : P_CU3    AS CHECKBOX.              " 의뢰?
     SELECTION-SCREEN : COMMENT 68(8) TEXT-024, POSITION 77.
     PARAMETERS : P_CUY    AS CHECKBOX.              " 통관완?
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.
   SET  TITLEBAR 'ZIM61'.           " GUI TITLE SETTING..
   SELECT  SINGLE *  FROM  ZTIMIMG00.

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

* 권한 검증 함?
*  PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 파라메타 설?
*  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 구매의뢰 테이블 SELECT
*  PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* PROGRAM 사용여부 판단.
  IF ZTIMIMG00-ZFIMPATH NE '1'.
     MESSAGE  S564.  EXIT.
  ENDIF.

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
*     WHEN 'STUP' OR 'STDN'.         " SORT 선택?
*        W_FIELD_NM = 'ZFREQDT'.
*        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*        PERFORM HANDLE_SORT TABLES  IT_TAB
*                            USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'CDRC'.                   " 수입신고 자료 생?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P4000_CREATE_CUDATA      USING W_ERR_CHK.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
            MESSAGE S750 WITH W_SELECTED_LINES W_PROC_CNT .
      WHEN 'DISP'.          " 과세통관 Invoice 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_IV USING IT_SELECTED-ZFIVNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
               IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
   WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
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

  P_CU1 = 'X'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  '[ 과세통관 자료 생성 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / ' Date : ', SY-DATUM,  83 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'House B/L No            '       NO-GAP, SY-VLINE NO-GAP,
            '통관순번'                       NO-GAP, SY-VLINE NO-GAP,
            '도착일(ETA)'                    NO-GAP,
            '   '                            NO-GAP, SY-VLINE NO-GAP,
            'B/L 관리번호'                   NO-GAP, SY-VLINE NO-GAP,
            '통관상태'                       NO-GAP, SY-VLINE NO-GAP,
            'Material Document No    '       NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            'Invoice 관리번호        '       NO-GAP, SY-VLINE NO-GAP,
            '        '                       NO-GAP, SY-VLINE NO-GAP,
            'Invoice 입력일'                 NO-GAP, SY-VLINE NO-GAP,
            '                                              '
                                             NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIM61'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM61'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      IF IT_TAB-ZFHBLNO EQ  SPACE.
         MOVE  IT_TAB-ZFMBLNO  TO  IT_TAB-ZFHBLNO.
      ENDIF.
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
  FORMAT COLOR COL_BACKGROUND.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE        NO-GAP,
       IT_TAB-ZFHBLNO  NO-GAP,
       SY-VLINE        NO-GAP,         " House B/L No
       IT_TAB-ZFCLSEQ  NO-GAP,
       '   '           NO-GAP,
       SY-VLINE        NO-GAP,         " 통관순?
       IT_TAB-ZFETA    NO-GAP,
       '    '          NO-GAP,
       SY-VLINE        NO-GAP,         " 도착일(ETA)
       IT_TAB-ZFBLNO   NO-GAP,
       '  '            NO-GAP,
       SY-VLINE        NO-GAP,         " B/L 관리번?
       IT_TAB-ZFCUST   NO-GAP,
       '       '       NO-GAP,
       SY-VLINE        NO-GAP,         " 통관상?
       IT_TAB-MJAHR    NO-GAP,         " Material doc. year
       '-'             NO-GAP,
       IT_TAB-MBLNR    NO-GAP,         " Material doc. No
       '         '     NO-GAP,
       SY-VLINE        NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFIVNO    NO-GAP,
       '              ' NO-GAP,
       SY-VLINE         NO-GAP,        " Invoice 관리번?
       '        '       NO-GAP,
       SY-VLINE         NO-GAP,
       IT_TAB-ZFCCDT    NO-GAP,
       '    '           NO-GAP,
       SY-VLINE         NO-GAP,        " Invoice 입력?
       '                                              '
                        NO-GAP,
       SY-VLINE         NO-GAP.

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
     WRITE : / ' 총', W_COUNT, '건'.
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
  IF P_CU1 = ' ' AND P_CU2 = ' ' AND P_CU3 = ' ' AND P_CUY = ' '.
     MESSAGE S738.
     EXIT.
  ENDIF.

* 통관 상태 SETTING!
  REFRESH R_TERM.
  IF P_CU1 = 'X'.
    MOVE : 'I'       TO       R_TERM-SIGN,
           'EQ'      TO       R_TERM-OPTION,
           '1'       TO       R_TERM-LOW,
           SPACE     TO       R_TERM-HIGH.
    APPEND    R_TERM.
  ENDIF.
  IF P_CU2 = 'X'.
    MOVE : 'I'       TO       R_TERM-SIGN,
           'EQ'      TO       R_TERM-OPTION,
           '2'       TO       R_TERM-LOW,
           SPACE     TO       R_TERM-HIGH.
    APPEND    R_TERM.
  ENDIF.
  IF P_CU3 = 'X'.
    MOVE : 'I'       TO       R_TERM-SIGN,
           'EQ'      TO       R_TERM-OPTION,
           '3'       TO       R_TERM-LOW,
           SPACE     TO       R_TERM-HIGH.
    APPEND    R_TERM.
  ENDIF.
  IF P_CUY = 'X'.
    MOVE : 'I'       TO       R_TERM-SIGN,
           'EQ'      TO       R_TERM-OPTION,
           'Y'       TO       R_TERM-LOW,
           SPACE     TO       R_TERM-HIGH.
    APPEND    R_TERM.
  ENDIF.

  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM   ZTCUCLIV AS H INNER JOIN ZTBL AS I
  ON     H~ZFBLNO  EQ  I~ZFBLNO
  WHERE  I~ZFHBLNO IN  S_HBLNO
  AND    I~ZFETA   IN  S_ETA
  AND    H~ZFCLSEQ IN  S_CLSEQ
  AND    H~ZFBLNO  IN  S_BLNO
  AND    H~ZFCCDT  IN  S_CDAT
  AND    H~ZFIVNO  IN  S_IVNO
  AND    H~ZFCUST  IN  R_TERM.

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
        ZFIVNO  LIKE ZTCUCLIV-ZFIVNO,
        ZFBLNO  LIKE ZTCUCLIV-ZFBLNO,
        ZFCLSEQ LIKE ZTCUCLIV-ZFCLSEQ,
        ZFCUST  LIKE ZTCUCLIV-ZFCUST.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFIVNO   TO ZFIVNO,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFCLSEQ  TO ZFCLSEQ,
         IT_TAB-ZFCUST   TO ZFCUST.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFIVNO   TO IT_SELECTED-ZFIVNO,
                IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
                IT_TAB-ZFCLSEQ  TO IT_SELECTED-ZFCLSEQ,
                IT_TAB-ZFCUST   TO IT_SELECTED-ZFCUST.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IV
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IV USING    P_ZFIVNO..

   SET PARAMETER ID 'ZPIVNO'  FIELD P_ZFIVNO.
   EXPORT 'ZPIVNO'        TO MEMORY ID 'ZPIVNO'.

   CALL TRANSACTION 'ZIM69' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IV

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_CUCL
*&---------------------------------------------------------------------*
FORM P4000_CREATE_CUCL USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_CUCL.
  CLEAR   IT_CUCL.
  LOOP AT IT_SELECTED.
       IF IT_SELECTED-ZFCUST NE '1'.
          CONTINUE.
       ENDIF.

       CLEAR : ZTCUCLIV, ZTCUCL.
       SELECT SINGLE *
         FROM ZTCUCLIV
        WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
       IF SY-SUBRC NE 0.
          CONTINUE.
       ENDIF.

       MOVE SY-UNAME TO ZTCUCLIV-UNAM.
       MOVE SY-DATUM TO ZTCUCLIV-UDAT.
       MOVE '2'      TO ZTCUCLIV-ZFCUST.
       UPDATE ZTCUCLIV.
       IF SY-SUBRC NE 0.
          MESSAGE E739 WITH ZTCUCLIV-ZFIVNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.

       SELECT SINGLE *
         FROM ZTCUCL
        WHERE ZFBLNO = ZTCUCLIV-ZFBLNO
          AND ZFCLSEQ = ZTCUCLIV-ZFCLSEQ.
       IF SY-SUBRC = 0.
                 " 신고금액등 add..
       ELSE.
          MOVE ZTCUCLIV-ZFBLNO  TO ZTCUCL-ZFBLNO.
          MOVE ZTCUCLIV-ZFCLSEQ TO ZTCUCL-ZFCLSEQ.
          MOVE 'USD'            TO ZTCUCL-ZFUSD.
          MOVE 'KRW'            TO ZTCUCL-ZFKRW.
          MOVE ZTCUCLIV-ZFCLCD TO ZTCUCL-ZFCLCD.   " 통관구분 = 수입통?
          MOVE '2'              TO ZTCUCL-ZFCUST.  " 통관상태 = 의뢰대?
          MOVE SY-UNAME         TO ZTCUCL-ERNAM.
          MOVE SY-DATUM         TO ZTCUCL-CDAT.
          MOVE SY-UNAME         TO ZTCUCL-UNAM.
          MOVE SY-DATUM         TO ZTCUCL-UDAT.
          INSERT ZTCUCL.
          IF SY-SUBRC NE 0.
             MESSAGE E739 WITH ZTCUCLIV-ZFIVNO.
             MOVE 'Y' TO W_ERR_CHK.
             EXIT.
          ENDIF.
          MOVE ZTCUCLIV-ZFBLNO  TO IT_CUCL-ZFBLNO.
          MOVE ZTCUCLIV-ZFCLSEQ TO IT_CUCL-ZFCLSEQ.
          APPEND IT_CUCL.
       ENDIF.

  ENDLOOP.

ENDFORM.                    " P4000_CREATE_CUCL

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_CUIDR
*&---------------------------------------------------------------------*
FORM P4000_CREATE_CUIDR USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_CRET_CNT.
  SELECT SINGLE *
    FROM ZTIMIMGTX
    WHERE BUKRS EQ ZTIDR-BUKRS.
*
  LOOP AT IT_CUCL.                              " 수입신고 자료 형?
       CLEAR ZTIDR.
       MOVE IT_CUCL-ZFBLNO      TO ZTIDR-ZFBLNO.         " B/L 관리번?
       MOVE IT_CUCL-ZFCLSEQ     TO ZTIDR-ZFCLSEQ.        " 통관순?
       MOVE 'B'                 TO ZTIDR-ZFITKD.         " 수입신고종?
       MOVE 'ETC'               TO ZTIDR-ZFTRCN.         " 운송용?
*       MOVE ZTIMIMG00-ZFTDNO    TO ZTIDR-ZFTDNO.         " 납세자 통관?
       MOVE ZTIMIMGTX-ZFTDNM1   TO ZTIDR-ZFTDNM1.        " 납세자 상?
       MOVE ZTIMIMGTX-ZFTDNM2   TO ZTIDR-ZFTDNM2.        " 납세자 성?
*       MOVE ZTIMIMG00-ZFTDAD1   TO ZTIDR-ZFTDAD1.        " 납세자 주소1
*       MOVE ZTIMIMG00-ZFTDAD2   TO ZTIDR-ZFTDAD2.        " 납세자 주소2
*       MOVE ZTIMIMG00-ZFTDTC    TO ZTIDR-ZFTDTC.         " 납세자 사업?
       MOVE 'KRW'               TO ZTIDR-ZFKRW.          " 원화통?
       MOVE 'USD'               TO ZTIDR-ZFUSD.          " 미화통?
       MOVE 'N'                 TO ZTIDR-ZFDNCD.         " Download 여?
       MOVE SY-UNAME            TO ZTIDR-ERNAM.
       MOVE SY-DATUM            TO ZTIDR-CDAT.
       MOVE SY-UNAME            TO ZTIDR-UNAM.
       MOVE SY-DATUM            TO ZTIDR-UDAT.
*
       CLEAR ZTBL.
       SELECT SINGLE *
         FROM ZTBL
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.
       MOVE ZTBL-ZFPONC         TO ZTIDR-ZFPONC.          "수입거래구?
       MOVE ZTBL-ZFAPRTC        TO ZTIDR-ZFAPRTC.         "도착?
       MOVE ZTBL-ZFCARC         TO ZTIDR-ZFSCON.          "적출?
       MOVE ZTBL-ZFETA          TO ZTIDR-ZFENDT.          "입항?

       IF ZTBL-ZFPOYN = 'Y'.
          MOVE 'B'              TO ZTIDR-ZFIMCD.          "수입자구?
          MOVE ZTIMIMGTX-ZFAPNO2 TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
          MOVE ZTIMIMGTX-ZFIAPNM2 TO ZTIDR-ZFIAPNM.  "수입자 상?
       ENDIF.

       IF ZTBL-ZFPOYN = 'N'.
          MOVE 'A'              TO ZTIDR-ZFIMCD.
          MOVE ZTIMIMGTX-ZFAPNO2 TO ZTIDR-ZFAPNO.   "수입자 무역업등록?
          MOVE ZTIMIMGTX-ZFIAPNM2 TO ZTIDR-ZFIAPNM.  "수입자 상?
       ENDIF.

       MOVE ZTBL-ZFHBLNO        TO ZTIDR-ZFHBLNO.         "House B/L No
       IF ZTBL-ZFVIA = 'AIR'.
          MOVE '40'             TO ZTIDR-ZFTRMET.         "운송수?
       ENDIF.
       IF ZTBL-ZFVIA = 'VSL'.
          MOVE '10'             TO ZTIDR-ZFTRMET.
       ENDIF.
       MOVE ZTBL-ZFCARNM        TO ZTIDR-ZFCARNM.         "선기?
       MOVE ZTBL-ZFREBELN       TO ZTIDR-ZFREBELN.        "대표P/O번?
       MOVE ZTBL-ZFOPNNO        TO ZTIDR-ZFOPNNO.         "대표L/C번?
       MOVE ZTBL-ZFPRNAM        TO ZTIDR-ZFPRNAM.         "P/R담당?
       MOVE ZTBL-ZFMATGB        TO ZTIDR-ZFMATGB.         "자재구?
*
*       MOVE ZTBL-ZFPKCN    TO ZTIDR-ZFPKCNT.             "총포장갯?
       MOVE ZTBL-ZFPKCNM   TO ZTIDR-ZFPKNM.              "포장종?
*       MOVE ZTBL-ZFTOWT    TO ZTIDR-ZFTOWT.              "총중?
       MOVE ZTBL-ZFTOWTM   TO ZTIDR-ZFTOWTM.             "총중량단?

       SELECT MAX( WAERS ) INTO ZTIDR-ZFTFAC               " 운임 A 통?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND ZFCSCD IN ( SELECT ZFCD
                            FROM ZTIMIMG08
                           WHERE ZFCDTY = '004').
       SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFA               " 운임 A 금?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS = ZTIDR-ZFTFAC
          AND ZFCSCD IN ( SELECT ZFCD
                            FROM ZTIMIMG08
                           WHERE ZFCDTY = '004').
       SELECT MAX( WAERS ) INTO ZTIDR-ZFTFBC               " 운임 B 통?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS NE ZTIDR-ZFTFAC
          AND ZFCSCD IN ( SELECT ZFCD
                            FROM ZTIMIMG08
                           WHERE ZFCDTY = '004').
       SELECT SUM( ZFCAMT ) INTO ZTIDR-ZFTFB               " 운임 B 금?
         FROM ZTBLCST
        WHERE ZFBLNO = ZTIDR-ZFBLNO
          AND WAERS = ZTIDR-ZFTFBC
          AND ZFCSCD IN ( SELECT ZFCD
                            FROM ZTIMIMG08
                           WHERE ZFCDTY = '004').
       INSERT ZTIDR.
       IF SY-SUBRC NE 0.
          MESSAGE E749 WITH ZTIDR-ZFBLNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.
       ADD 1       TO W_CRET_CNT.
*
       REFRESH IT_IDRIT.
       SELECT *
         FROM ZTCUCLIV
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
              SELECT *
                FROM ZTCUCLIVIT
               WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
                     CLEAR IT_IDRIT.
                     MOVE-CORRESPONDING ZTCUCLIVIT  TO IT_IDRIT.
                     APPEND IT_IDRIT.
              ENDSELECT.
       ENDSELECT.

       LOOP AT IT_IDRIT.
            CLEAR ZTIDRHS.
            SELECT SINGLE *
              FROM ZTIDRHS
             WHERE ZFBLNO = IT_CUCL-ZFBLNO
               AND ZFCLSEQ = IT_CUCL-ZFCLSEQ
               AND STAWN = IT_IDRIT-STAWN.

            IF SY-SUBRC NE 0.
               SELECT MAX( ZFCONO ) INTO ZTIDRHS-ZFCONO
                 FROM ZTIDRHS
                WHERE ZFBLNO = IT_CUCL-ZFBLNO
                  AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
               MOVE IT_CUCL-ZFBLNO      TO ZTIDRHS-ZFBLNO.
               MOVE IT_CUCL-ZFCLSEQ     TO ZTIDRHS-ZFCLSEQ.
               ADD  1                   TO ZTIDRHS-ZFCONO.    "?
               MOVE IT_IDRIT-STAWN      TO ZTIDRHS-STAWN.     "HS Code
               SELECT MAX( TEXT1 ) INTO ZTIDRHS-ZFTGDNM       "거래품?
                 FROM T604T
                WHERE SPRAS = SY-LANGU
                  AND STAWN = IT_IDRIT-STAWN.
               MOVE 'Hyundai Electronics' TO ZTIDRHS-ZFGCNM. "상표품?
               MOVE 'KRW'               TO ZTIDRHS-ZFKRW.
               MOVE 'USD'               TO ZTIDRHS-ZFUSD.
               MOVE ZTBL-ZFTOWTM   TO ZTIDR-ZFTOWTM.        "중량단?

               INSERT ZTIDRHS.
               IF SY-SUBRC NE 0.
                  MESSAGE E749 WITH IT_CUCL-ZFBLNO.
                  MOVE 'Y' TO W_ERR_CHK.
                  EXIT.
               ENDIF.
            ENDIF.

            CLEAR ZTIDRHSD.
            SELECT MAX( ZFRONO ) INTO ZTIDRHSD-ZFRONO
              FROM ZTIDRHSD
             WHERE ZFBLNO   EQ  ZTIDRHS-ZFBLNO
               AND ZFCLSEQ  EQ  ZTIDRHS-ZFCLSEQ
               AND ZFCONO   EQ  ZTIDRHS-ZFCONO.
            MOVE ZTIDRHS-ZFBLNO      TO ZTIDRHSD-ZFBLNO.
            MOVE ZTIDRHS-ZFCLSEQ     TO ZTIDRHSD-ZFCLSEQ.
            MOVE ZTIDRHS-ZFCONO      TO ZTIDRHSD-ZFCONO.     "?
            ADD  1                   TO ZTIDRHSD-ZFRONO.     "?
            MOVE IT_IDRIT-ZFIVNO     TO ZTIDRHSD-ZFIVNO.     "관리번?
            MOVE IT_IDRIT-ZFIVDNO    TO ZTIDRHSD-ZFIVDNO.    "일련번?
            MOVE IT_IDRIT-MATNR      TO ZTIDRHSD-ZFSTCD.     "규격코?
            MOVE IT_IDRIT-TXZ01      TO ZTIDRHSD-ZFGDDS1.    "규격1
            MOVE IT_IDRIT-MENGE      TO ZTIDRHSD-ZFQNT.      "수?
            MOVE IT_IDRIT-MEINS      TO ZTIDRHSD-ZFQNTM.     "수량단?
            MOVE IT_IDRIT-NETPR      TO ZTIDRHSD-NETPR.      "단?
            MOVE IT_IDRIT-PEINH      TO ZTIDRHSD-PEINH.      "Price uni
            MOVE IT_IDRIT-BPRME      TO ZTIDRHSD-BPRME.      "Order pri
            IF ZTIDRHSD-PEINH > 0.
               ZTIDRHSD-ZFAMT = ZTIDRHSD-NETPR * ZTIDRHSD-ZFQNT
                                               / ZTIDRHSD-PEINH. "금?
            ELSE.
               ZTIDRHSD-ZFAMT = 0.
            ENDIF.
            MOVE IT_IDRIT-ZFIVAMC    TO ZTIDRHSD-ZFCUR.       "통?
            MOVE IT_IDRIT-STAWN      TO ZTIDRHSD-STAWN.       "HS Code
*            IF NOT ( IT_IDRIT-ZFREQNO IS INITIAL ).       " 수입의뢰수?
*               SELECT SINGLE MENGE INTO ZTIDRHSD-ZFMENGE
*                 FROM ZTREQIT
*                WHERE ZFREQNO = IT_IDRIT-ZFREQNO
*                  AND ZFITMNO = IT_IDRIT-ZFIVDNO.
*            ENDIF.
            INSERT ZTIDRHSD.
            IF SY-SUBRC NE 0.
               MESSAGE E749 WITH IT_CUCL-ZFBLNO.
               MOVE 'Y' TO W_ERR_CHK.
               EXIT.
            ENDIF.
       ENDLOOP.                      " IT_IDRIT

  ENDLOOP.                           " IT_CUCL

  LOOP AT IT_CUCL.

       SELECT SINGLE *
         FROM ZTIDR
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.

       CLEAR W_MENGE.
       SELECT *
         FROM ZTCUCLIV
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.
              SELECT *
                FROM ZTCUCLIVIT
               WHERE ZFIVNO = ZTCUCLIV-ZFIVNO.
                     ADD ZTCUCLIVIT-MENGE  TO W_MENGE.
              ENDSELECT.
       ENDSELECT.
       CLEAR ZTBL.
       SELECT SINGLE *
         FROM ZTBL
        WHERE ZFBLNO = IT_CUCL-ZFBLNO.
*
       CLEAR ZTIDRHS.
       SELECT *
         FROM ZTIDRHS
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.
              SELECT SUM( ZFQNT ) MAX( ZFQNTM )            " 란사항 수?
                INTO (ZTIDRHS-ZFQNT, ZTIDRHS-ZFQNTM)
                FROM ZTIDRHSD
               WHERE ZFBLNO  = ZTIDRHS-ZFBLNO
                 AND ZFCLSEQ = ZTIDRHS-ZFCLSEQ
                 AND ZFCONO  = ZTIDRHS-ZFCONO.
              IF ( ZTIDRHS-ZFQNTM NE 'L'  ) AND
                 ( ZTIDRHS-ZFQNTM NE 'KG' ) AND
                 ( ZTIDRHS-ZFQNTM NE 'G'  ).
                 MOVE 'U'    TO ZTIDRHS-ZFQNTM.
              ENDIF.
              IF W_MENGE > 0.                              " 란사항 중?
                 ZTIDRHS-ZFWET =
                         ZTBL-ZFTOWT * ZTIDRHS-ZFQNT / W_MENGE.
              ELSE.
                 ZTIDRHS-ZFWET = 0.
              ENDIF.
              UPDATE ZTIDRHS.
              IF SY-SUBRC NE 0.
                 MESSAGE E749 WITH IT_CUCL-ZFBLNO.
                 MOVE 'Y' TO W_ERR_CHK.
                 EXIT.
              ENDIF.
       ENDSELECT.
*

       SELECT SUM( ZFWET )  SUM( ZFQNT )         "공통사항 총중?
         INTO (ZTIDR-ZFTOWT, W_ZFQNT)
         FROM ZTIDRHS
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.

       ZTIDR-ZFPKCNT = ZTBL-ZFPKCN * W_ZFQNT / W_MENGE. "포장수?

       SELECT SUM( ZFAMT ) MAX( ZFCUR )  "공통사항 결제금액/통?
         INTO (ZTIDR-ZFSTAMT, ZTIDR-ZFSTAMC)
         FROM ZTIDRHSD
        WHERE ZFBLNO = IT_CUCL-ZFBLNO
          AND ZFCLSEQ = IT_CUCL-ZFCLSEQ.

       UPDATE ZTIDR.
       IF SY-SUBRC NE 0.
          MESSAGE E749 WITH IT_CUCL-ZFBLNO.
          MOVE 'Y' TO W_ERR_CHK.
          EXIT.
       ENDIF.

  ENDLOOP.

ENDFORM.                    " P4000_CREATE_CUIDR
*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_CUDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P4000_CREATE_CUDATA USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR : W_PROC_CNT, W_CRET_CNT.

  LOOP AT IT_SELECTED.
    IF IT_SELECTED-ZFCUST NE '1'. CONTINUE. ENDIF.
    W_PROC_CNT = W_PROC_CNT + 1.
    CALL FUNCTION 'ZIM_CUDATA_PAY_CREATE'
       EXPORTING
             W_ZFIVNO            =   IT_SELECTED-ZFIVNO
       IMPORTING
             W_ZFBLNO            =   W_ZFBLNO
             W_ZFCLSEQ           =   W_ZFCLSEQ
       EXCEPTIONS
             ERROR_INSERT.

    IF SY-SUBRC NE 0.
       MOVE 'Y' TO W_ERR_CHK.
       CONTINUE.
    ENDIF.
    W_CRET_CNT = W_CRET_CNT + 1.
  ENDLOOP.

ENDFORM.                    " P4000_CREATE_CUDATA
