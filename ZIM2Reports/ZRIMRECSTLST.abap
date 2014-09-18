*&---------------------------------------------------------------------*
*& Report  ZRIMRECSTLST                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 비용 회계처리 현황                           *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.09.26                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 수입의뢰 비용을 조회한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMRECSTLST    MESSAGE-ID ZIM
                        LINE-SIZE 125
                        NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMRECSTLSTTOP.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFOPNNO(16)     TYPE   C,                " L/C No
       ZFREQNO         LIKE   ZTRECST-ZFREQNO,  " 수입의뢰 관리번호
       ZFCSQ           LIKE   ZTRECST-ZFCSQ,    " 비용순번
       ZFCSCD          LIKE   ZTRECST-ZFCSCD,   " 비용구분
       ZFCSCD_NM(10)   TYPE   C,                " 비용명칭
       ZFCD3           LIKE   ZTIMIMG08-ZFCD3,  " 집계구분
       ZFCD3_NM(10)    TYPE   C,                " 집계구분명
       ZFCAMT          LIKE   ZTRECST-ZFCAMT,   " 비용금액
       WAERS           LIKE   ZTRECST-WAERS,    " 통화
       ZFCKAMT         LIKE   ZTRECST-ZFCKAMT,  " 비용원화금액
       ZFKRW           LIKE   ZTRECST-ZFKRW,    " 원화통화
       ZFOCDT          LIKE   ZTRECST-ZFOCDT,   " 발생일
       ZFPSDT          LIKE   ZTRECST-ZFPSDT,   " 회계처리일
       ZFFIYR          LIKE   ZTRECST-ZFFIYR,   " 회계전표 연도
       ZFACDO          LIKE   ZTRECST-ZFACDO,   " 회계전표 번호
       ZFVEN           LIKE   ZTRECST-ZFVEN,    " Vendor
       BUKRS           LIKE   ZTRECST-BUKRS,    " Company code.
       ZFVEN_NM(20)    TYPE   C,                " Vendor 명
       ZFPAY           LIKE   ZTRECST-ZFPAY,    " 지불처
       ZFPAY_NM(20)    TYPE   C,                " 지불처명
       EBELN           LIKE   ZTREQHD-EBELN,    " P/O No.
       ZTERM           LIKE   ZTRECST-ZTERM,    " Terms of Payment
       MWSKZ           LIKE   ZTRECST-MWSKZ,    " Tax Code
       ZFWERKS         LIKE   ZTRECST-ZFWERKS,  " Plant
       UNAM            LIKE   ZTRECST-UNAM,     " Last changed by
       UDAT            LIKE   ZTRECST-UDAT,     " Last changed on
       SUM_MARK        TYPE   C.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_TMP OCCURS 0,
       ZFOPNNO(16)     TYPE   C,                " L/C No
       ZFREQNO         LIKE   ZTRECST-ZFREQNO,  " 수입의뢰 관리번호
       ZFCSQ           LIKE   ZTRECST-ZFCSQ,    " 비용순번
       ZFCSCD          LIKE   ZTRECST-ZFCSCD,   " 비용구분
       ZFCSCD_NM(10)   TYPE   C,                " 비용명칭
       ZFCD3           LIKE   ZTIMIMG08-ZFCD3,  " 집계구분
       ZFCD3_NM(10)    TYPE   C,                " 집계구분명
       ZFCAMT          LIKE   ZTRECST-ZFCAMT,   " 비용금액
       WAERS           LIKE   ZTRECST-WAERS,    " 통화
       ZFCKAMT         LIKE   ZTRECST-ZFCKAMT,  " 비용원화금액
       ZFKRW           LIKE   ZTRECST-ZFKRW,    " 원화통화
       ZFOCDT          LIKE   ZTRECST-ZFOCDT,   " 발생일
       ZFPSDT          LIKE   ZTRECST-ZFPSDT,   " 회계처리일
       ZFFIYR          LIKE   ZTRECST-ZFFIYR,   " 회계전표 연도
       ZFACDO          LIKE   ZTRECST-ZFACDO,   " 회계전표 번호
       ZFVEN           LIKE   ZTRECST-ZFVEN,    " Vendor
       BUKRS           LIKE   ZTRECST-BUKRS,    " Vendor
       ZFVEN_NM(20)    TYPE   C,                " Vendor 명
       ZFPAY           LIKE   ZTRECST-ZFPAY,    " 지불처
       ZFPAY_NM(20)    TYPE   C,                " 지불처명
       EBELN           LIKE   ZTREQHD-EBELN,    " P/O No.
       ZTERM           LIKE   ZTRECST-ZTERM,    " Terms of Payment
       MWSKZ           LIKE   ZTRECST-MWSKZ,    " Tax Code
       ZFWERKS         LIKE   ZTRECST-ZFWERKS,  " Plant
       UNAM            LIKE   ZTRECST-UNAM,     " Last changed by
       UDAT            LIKE   ZTRECST-UDAT.     " Last changed on
DATA : END OF IT_TAB_TMP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모음

*-----------------------------------------------------------------------
* Selection Screen 절
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS  FOR ZTRECST-BUKRS,     " 회사코드.
                   S_VEN    FOR ZTRECST-ZFVEN,     " Vendor
                   S_PAY    FOR ZTRECST-ZFPAY,     " 지불처
                   S_WERKS  FOR ZTRECST-ZFWERKS,   " Plant
                   S_OCDT   FOR ZTRECST-ZFOCDT,    " 발생일
                   S_PSDT   FOR ZTRECST-ZFPSDT,    " 회계처리일
                   S_OPNNO  FOR ZTREQHD-ZFOPNNO,   " L/C No
                   S_EBELN  FOR ZTREQHD-EBELN,     " P/O
                   S_ZTERM  FOR ZTRECST-ZTERM,     " Terms Of Payment
                   S_MWSKZ  FOR ZTRECST-MWSKZ,     " Tax Code
*                  S_CD3    FOR ZTIMIMG08-ZFCD3,   " 집계구분
                   S_CSCD   FOR ZTRECST-ZFCSCD     " 비용구분.
                            MATCHCODE OBJECT ZIC5L.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* 회계처리 여부
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-002, POSITION 1.
     SELECTION-SCREEN : COMMENT 36(3) TEXT-021, POSITION 40.
     PARAMETERS : P_Y   RADIOBUTTON GROUP RDG.  " Yes
     SELECTION-SCREEN : COMMENT 51(2) TEXT-022, POSITION 54.
     PARAMETERS : P_N   RADIOBUTTON GROUP RDG.  " No
     SELECTION-SCREEN : COMMENT 64(3) TEXT-023, POSITION 68.
     PARAMETERS : P_A   RADIOBUTTON GROUP RDG.  " ALL
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
* Sort By
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-003, POSITION 1.
     SELECTION-SCREEN : COMMENT 33(6) TEXT-031, POSITION 40.
     PARAMETERS : P_V   RADIOBUTTON GROUP RDG1. " Vendor별
     SELECTION-SCREEN : COMMENT 45(8) TEXT-032, POSITION 54.
     PARAMETERS : P_S   RADIOBUTTON GROUP RDG1. " 전표별
     SELECTION-SCREEN : COMMENT 59(8) TEXT-033, POSITION 68.
     PARAMETERS : P_C   RADIOBUTTON GROUP RDG1. " 집계구분별
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.
   SET  TITLEBAR 'ZIMR04'.           " GUI TITLE SETTING..

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 절
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Import System Configuration Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE      USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택시
            W_FIELD_NM = 'ZFREQDT'.
            ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
            PERFORM HANDLE_SORT TABLES  IT_TAB
                                USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'DSRQ'.                    " L/C 조회
            IF IT_TAB-ZFREQNO IS INITIAL.
               MESSAGE S319. EXIT.
            ENDIF.
            PERFORM P2000_SHOW_LC USING IT_TAB-ZFREQNO.
            CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
      WHEN 'FB03'.                    " 회계전표 조회
            IF IT_TAB-ZFACDO IS INITIAL.
               MESSAGE S252. EXIT.
            ENDIF.
            PERFORM P2000_SHOW_SL USING IT_TAB-ZFFIYR
                                        IT_TAB-ZFACDO.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종료
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  P_N = 'X'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 수입의뢰 비용 회계처리 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / ' Date : ', SY-DATUM, 107 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /                                       SY-VLINE NO-GAP,
            '집계구분      '              NO-GAP, SY-VLINE NO-GAP,
            '          발생금액       '   NO-GAP, SY-VLINE NO-GAP,
            '               '             NO-GAP, SY-VLINE NO-GAP,
            '발생일    '                  NO-GAP, SY-VLINE NO-GAP,
            'Vendor                         ' NO-GAP, SY-VLINE NO-GAP,
            'P/O No    '                  NO-GAP, SY-VLINE NO-GAP,
            'Plnt/Tx/Term'                NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /                                       SY-VLINE NO-GAP,
            '비용구분      '              NO-GAP, SY-VLINE NO-GAP,
            '          원화금액       '   NO-GAP, SY-VLINE NO-GAP,
            '전표번호       '             NO-GAP, SY-VLINE NO-GAP,
            '회계처리일'                  NO-GAP, SY-VLINE NO-GAP,
            'L/C No.         '            NO-GAP, SY-VLINE NO-GAP,
            '수입관리No    '              NO-GAP, SY-VLINE NO-GAP,
            'Last Changed           '     NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMR04'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR04'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      IF IT_TAB-ZFCKAMT <= 0. CONTINUE. ENDIF.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      IF IT_TAB-SUM_MARK = 'Y'.
         PERFORM P3000_SUM_LINE_WRITE.
      ELSE.
         PERFORM P3000_LINE_WRITE.
      ENDIF.

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
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE             NO-GAP,
       IT_TAB-ZFCD3            NO-GAP, " 집계구분
       ' '                     NO-GAP,
       IT_TAB-ZFCD3_NM         NO-GAP, " 집계구분명
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCAMT CURRENCY IT_TAB-ZFKRW NO-GAP, " 발생금액
       ' '                     NO-GAP,
       IT_TAB-WAERS            NO-GAP, " 발생통화
       SY-VLINE                NO-GAP,
       '               '       NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFOCDT           NO-GAP, " 발생일
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVEN            NO-GAP, " Vendor
       ' '                     NO-GAP,
       IT_TAB-ZFVEN_NM         NO-GAP, " Vendor 명
       SY-VLINE                NO-GAP,
       IT_TAB-EBELN            NO-GAP, " P/O No
       SY-VLINE                NO-GAP,
       IT_TAB-ZFWERKS          NO-GAP, " Plant
       '/'                     NO-GAP,
       IT_TAB-MWSKZ            NO-GAP, " Tax Code
       '/'                     NO-GAP,
       IT_TAB-ZTERM            NO-GAP, " Terms of Payment
       SY-VLINE                NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_BACKGROUND.
       WRITE : / SY-VLINE      NO-GAP,
       IT_TAB-ZFCSCD           NO-GAP, " 비용코드
       ' '                     NO-GAP,
       IT_TAB-ZFCSCD_NM        NO-GAP, " 비용명칭
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT CURRENCY IT_TAB-ZFKRW NO-GAP, " 원화금액
       ' '                     NO-GAP,
       IT_TAB-ZFKRW            NO-GAP, " 원화통화
       SY-VLINE                NO-GAP,
       IT_TAB-ZFFIYR           NO-GAP, " 회계전표 연도
       '-'                     NO-GAP,
       IT_TAB-ZFACDO           NO-GAP, " 회계전표 번호
       SY-VLINE                NO-GAP,
       IT_TAB-ZFPSDT           NO-GAP, " 회계처리일
       SY-VLINE                NO-GAP,
       IT_TAB-ZFOPNNO          NO-GAP, " L/C No
       SY-VLINE                NO-GAP,
       IT_TAB-ZFREQNO          NO-GAP, " 수입의뢰 관리번호
       '    '                  NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-UNAM             NO-GAP, " Last changed by
       ' '                     NO-GAP,
       IT_TAB-UDAT             NO-GAP, " Last changed on
       SY-VLINE                NO-GAP.

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
     FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
     WRITE:/ SY-VLINE          NO-GAP,
       '총계          '        NO-GAP,
       SY-VLINE                NO-GAP,
       TOT_ZFCKAMT CURRENCY 'KRW' NO-GAP, " 비용금액
       ' '                     NO-GAP,
       'KRW  '                 NO-GAP,
       SY-VLINE                NO-GAP,
       '                          ' NO-GAP,
       SY-VLINE                NO-GAP,
       '                            ' NO-GAP,
       '                           '  NO-GAP,
       SY-VLINE                NO-GAP.
     WRITE : / SY-ULINE.
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

  REFRESH IT_TAB_TMP.

  SELECT *
    FROM ZTRECST
   WHERE ZFVEN     IN S_VEN
     AND ZFPAY     IN S_PAY
     AND ZFWERKS   IN S_WERKS
     AND ZFOCDT    IN S_OCDT
     AND ZFOCDT    IN S_OCDT
     AND ZFPSDT    IN S_PSDT
     AND ZTERM     IN S_ZTERM
     AND MWSKZ     IN S_MWSKZ
     AND ZFCSCD    IN S_CSCD.
         IF P_Y = 'X'.  "회계처리 Yes
            IF ZTRECST-ZFFIYR IS INITIAL.
               CONTINUE.
            ENDIF.
         ENDIF.
         IF P_N = 'X'.  "회계처리 No
            IF NOT ( ZTRECST-ZFFIYR IS INITIAL ).
               CONTINUE.
            ENDIF.
         ENDIF.
         CLEAR ZTREQHD.
         SELECT SINGLE *
           FROM ZTREQHD
          WHERE ZFREQNO = ZTRECST-ZFREQNO
            AND EBELN IN S_EBELN
            AND ZFOPNNO IN S_OPNNO.
         IF SY-SUBRC NE 0.
            CONTINUE.
         ENDIF.

         CLEAR ZTIMIMG08.
         SELECT SINGLE *
           FROM ZTIMIMG08
          WHERE ZFCDTY = '003'
            AND ZFCD = ZTRECST-ZFCSCD.
*           AND ZFCD3 IN S_CD3.             ">비용집계구분 막음.
         IF SY-SUBRC NE 0.
            CONTINUE.
         ENDIF.
         CLEAR IT_TAB_TMP.
         MOVE-CORRESPONDING ZTRECST TO IT_TAB_TMP.
         MOVE ZTREQHD-BUKRS TO IT_TAB_TMP-BUKRS.
         MOVE ZTREQHD-EBELN TO IT_TAB_TMP-EBELN.
         MOVE ZTREQHD-ZFOPNNO  TO IT_TAB_TMP-ZFOPNNO.
         MOVE ZTIMIMG08-ZFCDNM TO IT_TAB_TMP-ZFCSCD_NM.
         MOVE ZTIMIMG08-ZFCD3 TO IT_TAB_TMP-ZFCD3.
         MOVE ZTIMIMG08-ZFCD4 TO IT_TAB_TMP-ZFCD3_NM.
         SELECT SINGLE NAME1 INTO IT_TAB_TMP-ZFVEN_NM
           FROM LFA1
          WHERE LIFNR = IT_TAB_TMP-ZFVEN.
         SELECT SINGLE NAME1 INTO IT_TAB_TMP-ZFPAY_NM
           FROM LFA1
          WHERE LIFNR = IT_TAB_TMP-ZFPAY.
         APPEND  IT_TAB_TMP.
  ENDSELECT.

  DESCRIBE TABLE IT_TAB_TMP LINES W_COUNT.
  IF W_COUNT = 0.
     MESSAGE S738.
  ENDIF.

  IF P_V = 'X'.  "Sort By Vendor
     SORT IT_TAB_TMP BY ZFVEN ZFFIYR ZFACDO ASCENDING.
  ENDIF.

  IF P_S = 'X'.  "Sort By 전표번호
     SORT IT_TAB_TMP BY ZFFIYR ZFACDO ZFCSCD ASCENDING.
  ENDIF.

  IF P_C = 'X'.  "Sort By 집계구분
     SORT IT_TAB_TMP BY ZFCD3 ZFVEN ZFFIYR ZFACDO ASCENDING.
  ENDIF.

  CLEAR : SV_ZFVEN, SV_ZFFIYR, SV_ZFACDO, SV_ZFCD3,
          SUM_ZFCKAMT, TOT_ZFCKAMT.
  REFRESH IT_TAB.
  LOOP AT IT_TAB_TMP.
       ADD 1   TO W_LOOP_CNT.
       IF SV_ZFVEN NE IT_TAB_TMP-ZFVEN.
          IF P_V = 'X'.
             CLEAR  IT_TAB.
             MOVE   SUM_ZFCKAMT TO IT_TAB-ZFCKAMT.
             MOVE   'KRW'       TO IT_TAB-ZFKRW.
             MOVE   SV_ZFVEN    TO IT_TAB-ZFVEN.
             SELECT SINGLE NAME1 INTO IT_TAB-ZFVEN_NM
               FROM LFA1
             WHERE LIFNR = IT_TAB-ZFVEN.
             MOVE   'Y'        TO IT_TAB-SUM_MARK.
             IF W_LOOP_CNT NE 1. APPEND IT_TAB. ENDIF.
             CLEAR  SUM_ZFCKAMT.
          ENDIF.
       ENDIF.
       IF ( SV_ZFFIYR  NE IT_TAB_TMP-ZFFIYR ) OR
          ( SV_ZFACDO  NE IT_TAB_TMP-ZFACDO ).
          IF P_S = 'X'.
             CLEAR  IT_TAB.
             MOVE   SUM_ZFCKAMT  TO IT_TAB-ZFCKAMT.
             MOVE   'KRW'        TO IT_TAB-ZFKRW.
             MOVE   SV_ZFFIYR    TO IT_TAB-ZFFIYR.
             MOVE   SV_ZFACDO    TO IT_TAB-ZFACDO.
             MOVE   'Y'          TO IT_TAB-SUM_MARK.
             IF W_LOOP_CNT NE 1. APPEND IT_TAB. ENDIF.
             CLEAR  SUM_ZFCKAMT.
          ENDIF.
       ENDIF.
       IF SV_ZFCD3  NE IT_TAB_TMP-ZFCD3.
          IF P_C = 'X'.
             CLEAR  IT_TAB.
             MOVE   SUM_ZFCKAMT  TO IT_TAB-ZFCKAMT.
             MOVE   'KRW'        TO IT_TAB-ZFKRW.
             MOVE   SV_ZFCD3     TO IT_TAB-ZFCD3.
             SELECT MAX( ZFCD4 ) INTO IT_TAB-ZFCD3_NM
               FROM ZTIMIMG08
              WHERE ZFCDTY = '003'
                AND ZFCD3 = SV_ZFCD3.
             MOVE   'Y'         TO IT_TAB-SUM_MARK.
             IF W_LOOP_CNT NE 1. APPEND IT_TAB. ENDIF.
             CLEAR  SUM_ZFCKAMT.
          ENDIF.
       ENDIF.
       CLEAR   IT_TAB.
       MOVE-CORRESPONDING IT_TAB_TMP TO IT_TAB.
       APPEND  IT_TAB.
       ADD  IT_TAB-ZFCKAMT TO SUM_ZFCKAMT.
       ADD  IT_TAB-ZFCKAMT TO TOT_ZFCKAMT.
       MOVE IT_TAB-ZFVEN   TO SV_ZFVEN.
       MOVE IT_TAB-ZFFIYR  TO SV_ZFFIYR.
       MOVE IT_TAB-ZFACDO  TO SV_ZFACDO.
       MOVE IT_TAB-ZFCD3  TO SV_ZFCD3.
   ENDLOOP.

   IF P_V = 'X'.
      CLEAR  IT_TAB.
      MOVE   SUM_ZFCKAMT TO IT_TAB-ZFCKAMT.
      MOVE   'KRW'       TO IT_TAB-ZFKRW.
      MOVE   SV_ZFVEN    TO IT_TAB-ZFVEN.
      SELECT SINGLE NAME1 INTO IT_TAB-ZFVEN_NM
        FROM LFA1
       WHERE LIFNR = IT_TAB-ZFVEN.
      MOVE   'Y'        TO IT_TAB-SUM_MARK.
      APPEND IT_TAB.
   ENDIF.
   IF P_S = 'X'.
      CLEAR  IT_TAB.
      MOVE   SUM_ZFCKAMT  TO IT_TAB-ZFCKAMT.
      MOVE   'KRW'        TO IT_TAB-ZFKRW.
      MOVE   SV_ZFFIYR    TO IT_TAB-ZFFIYR.
      MOVE   SV_ZFACDO    TO IT_TAB-ZFACDO.
      MOVE   'Y'          TO IT_TAB-SUM_MARK.
      APPEND IT_TAB.
   ENDIF.
   IF P_C = 'X'.
      CLEAR  IT_TAB.
      MOVE   SUM_ZFCKAMT  TO IT_TAB-ZFCKAMT.
      MOVE   'KRW'        TO IT_TAB-ZFKRW.
      MOVE   SV_ZFCD3     TO IT_TAB-ZFCD3.
      SELECT MAX( ZFCD4 ) INTO IT_TAB-ZFCD3_NM
        FROM ZTIMIMG08
       WHERE ZFCDTY = '003'
         AND ZFCD3 = SV_ZFCD3.
      MOVE   'Y'         TO IT_TAB-SUM_MARK.
      APPEND IT_TAB.
   ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX       TYPE P,
        ZFREQNO     LIKE ZTRECST-ZFREQNO,
        ZFFIYR      LIKE ZTRECST-ZFFIYR,
        ZFACDO      LIKE ZTRECST-ZFACDO.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX       TO INDEX,
         IT_TAB-ZFREQNO     TO ZFREQNO,
         IT_TAB-ZFFIYR      TO ZFFIYR,
         IT_TAB-ZFACDO      TO ZFACDO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX       TO INDEX,
                IT_TAB-ZFREQNO     TO IT_SELECTED-ZFREQNO,
                IT_TAB-ZFFIYR      TO IT_SELECTED-ZFFIYR,
                IT_TAB-ZFACDO      TO IT_SELECTED-ZFACDO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  EXPORT 'ZPBLNO'            TO MEMORY ID 'ZPBLNO'.
  CALL TRANSACTION 'ZIM23'   AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_SL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_SL USING    P_ZFFIYR P_ZFACDO.

  SET PARAMETER ID 'BUK'     FIELD IT_TAB-BUKRS.
  SET PARAMETER ID 'GJR'     FIELD P_ZFFIYR.
  SET PARAMETER ID 'BLN'     FIELD P_ZFACDO.

  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_SL

*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUM_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

  IF P_V = 'X'.  "Sort By Vendor
     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT CURRENCY IT_TAB-ZFKRW NO-GAP, " 비용금액
       ' '                     NO-GAP,
       IT_TAB-ZFKRW            NO-GAP, " 원화통화
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVEN            NO-GAP, " Vendor
       ' '                     NO-GAP,
       IT_TAB-ZFVEN_NM(15)     NO-GAP, " Vendor 명
       SY-VLINE                NO-GAP,
       '                            ' NO-GAP,
       '                           '  NO-GAP,
       SY-VLINE                NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.

  IF P_S = 'X'.  "Sort By 전표번호
     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT CURRENCY IT_TAB-ZFKRW NO-GAP, " 비용금액
       ' '                     NO-GAP,
       IT_TAB-ZFKRW            NO-GAP, " 원화통화
       SY-VLINE                NO-GAP,
       IT_TAB-ZFFIYR           NO-GAP, " 회계전표 연도
       '-'                     NO-GAP,
       IT_TAB-ZFACDO           NO-GAP, " 회계전표 번호
       '           '           NO-GAP,
       SY-VLINE                NO-GAP,
       '                            ' NO-GAP,
       '                           '  NO-GAP,
       SY-VLINE                NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.

  IF P_C = 'X'.  "Sort By 집계구분
     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT CURRENCY IT_TAB-ZFKRW NO-GAP, " 비용금액
       ' '                     NO-GAP,
       IT_TAB-ZFKRW            NO-GAP, " 원화통화
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCD3            NO-GAP,
       ' '                     NO-GAP,
       IT_TAB-ZFCD3_NM         NO-GAP,
       '            '          NO-GAP,
       SY-VLINE                NO-GAP,
       '                            ' NO-GAP,
       '                           '  NO-GAP,
       SY-VLINE                NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.

ENDFORM.                    " P3000_SUM_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  SET PARAMETER ID 'ZPREQNO' FIELD IT_TAB-ZFREQNO.
  SET PARAMETER ID 'ZPOPNNO' FIELD ''.
  SET PARAMETER ID 'BES'     FIELD ''.

  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZTERM_HIGH  text
*----------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP USING    P_ZTERM.

   TABLES : T052.

   CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = P_ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.


  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
     P_ZTERM = T052-ZTERM.
  ENDIF.


ENDFORM.                    " P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFPSMS NE 1.
     W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
