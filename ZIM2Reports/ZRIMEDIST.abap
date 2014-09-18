*&---------------------------------------------------------------------*
*& Report  ZRIMEDIST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI 송수신 현황                                       *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.18                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMEDIST     MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Menu Statsu Function을 Inactive하기 위한 Internal Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.

*-----------------------------------------------------------------------
* INTERNAL TABLE이상
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MANDT      LIKE   ZTDHF1-MANDT,        "클라이언트
       ZFDHENO    LIKE   ZTDHF1-ZFDHENO,      "문서관리번호
       BUKRS      LIKE   ZTDHF1-BUKRS,        "회사코드
       ZFDHDOC    LIKE   ZTDHF1-ZFDHDOC,      "전자문서명
       ZFDHSRG    LIKE   ZTDHF1-ZFDHSRG,      "송수신구분(S:송신,R:수신)
       ZFDHSRO    LIKE   ZTDHF1-ZFDHSRO,      "거래상대방 ID
       ZFDHREF    LIKE   ZTDHF1-ZFDHREF,      "발신인이 부여한 참조번호
       ZFDHJSD    LIKE   ZTDHF1-ZFDHJSD,      "전송일자
       ZFDHJSH    LIKE   ZTDHF1-ZFDHJSH,      "전송시간
       ZFDHJSC    LIKE   ZTDHF1-ZFDHJSC,      "전송 응답코드(ACK)
       ZFDHTRA    LIKE   ZTDHF1-ZFDHTRA,      "변환결과 응답코드(ACK)
       ZFDOCNOR   LIKE   ZTDHF1-ZFDOCNOR,     "Receive 전자문서번호
       ZFDHSSD    LIKE   ZTDHF1-ZFDHSSD,      "수신일자(YYMMDD)
       ZFDHSST    LIKE   ZTDHF1-ZFDHSST,      "수신시간(HHMMSS)
       ZFDHAPP    LIKE   ZTDHF1-ZFDHAPP,
                        "수신된 문서의 Application DB Update 여부("Y")
       ZFDHPRT    LIKE   ZTDHF1-ZFDHPRT,      "문서출력 회수(원본 : 0)
       FILENAME   LIKE   ZTDHF1-FILENAME.     "송신화일명
DATA : END OF IT_TAB.

DATA : W_LINE(4),
       W_CNT(4),
       W_ERR_CHK,
       W_MOD       TYPE   I,
       W_BUTXT(60).

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.           " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.            " Utility function 모듈.

TABLES: ZTDHF1, T001.                   " 표준 EDI FLAT HEADER

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS:  S_BUKRS   FOR  ZTDHF1-BUKRS  "회사코드.
                              OBLIGATORY MEMORY ID BUK,
                    S_DHDOC   FOR  ZTDHF1-ZFDHDOC,  "전자문서명.
                    S_DHENO   FOR  ZTDHF1-ZFDHENO,  "문서번호.
                    S_DHJSD   FOR  ZTDHF1-ZFDHJSD,  "전송일자.
                    S_DHJSH   FOR  ZTDHF1-ZFDHJSH,  "전송시간.
                    S_DHREF   FOR  ZTDHF1-ZFDHREF,  "참조번호.
                    S_DHAPP   FOR  ZTDHF1-ZFDHAPP,  "응답여부.
                    S_DHSSD   FOR  ZTDHF1-ZFDHSSD,  "수신일자(YYMMDD).
                    S_DHSST   FOR  ZTDHF1-ZFDHSST.  "수신시간(HHMMSS).
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.          " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 표준 EDI FLAT HEAD 테이블 SELECT
   PERFORM   P1000_GET_EDI_DATA      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.     ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE        USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.     ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'DISP'.
         IF IT_TAB IS INITIAL.
            MESSAGE S962.
         ELSE.
            CASE IT_TAB-ZFDHDOC.
               WHEN 'APP700' OR 'LOCAPP' OR 'PAYORD' OR 'APPPUR' OR
                    'APP707' OR 'LOCAMR'.
                  SET PARAMETER ID 'BES'       FIELD ''.  "P/O.
                  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.  "L/C.
                  SET PARAMETER ID 'ZPREQNO'   FIELD IT_TAB-ZFDHREF(10).
                                               "수입의뢰관리번호.
                  SET PARAMETER ID 'ZPAMDNO'
                                FIELD IT_TAB-ZFDHREF+11(5). "AMEND 회차

                  IF IT_TAB-ZFDHREF+11(5) EQ '00000'.
                     CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
                  ELSE.
                     CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
                  ENDIF.

               WHEN 'IMPREQ'.
                  SET PARAMETER ID 'ZPHBLNO'    FIELD ''.  "HOUSE B/L.
                  SET PARAMETER ID 'ZPBLNO'
                                FIELD IT_TAB-ZFDHREF(10).  "L/C.
                  SET PARAMETER ID 'ZPIDRNO'
                                FIELD ''.          "수입의뢰관리번호.
                  SET PARAMETER ID 'ZPCLSEQ'
                                FIELD IT_TAB-ZFDHREF+11(5). "AMEND 회차
                  IF IT_TAB-ZFDHAPP EQ 'Y'.
                     CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.
                  ELSE.
                     CALL TRANSACTION 'ZIM63' AND SKIP  FIRST SCREEN.
                  ENDIF.

               WHEN OTHERS.
            ENDCASE.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
* *     WHEN 'REFR'.
*           PERFORM  P1000_RESET_LIST.
*      WHEN OTHERS.
      WHEN 'FVIEW'.          "EDI FLAT FILE 조회
         IF NOT IT_TAB IS INITIAL.
            PERFORM P3000_VIEW_FLATFILE.
         ENDIF.
   ENDCASE.
   CLEAR : IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET TITLEBAR  'ZIM40'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /51  '[ EDI 송수신 현황]'
               COLOR COL_POSITIVE INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_EDI_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_EDI_DATA   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH IT_TAB.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB FROM ZTDHF1
                              WHERE BUKRS IN S_BUKRS
                              AND ZFDHDOC IN S_DHDOC
                              AND ZFDHENO IN S_DHENO
                              AND ZFDHJSD IN S_DHJSD
                              AND ZFDHJSH IN S_DHJSH
                              AND ZFDHREF IN S_DHREF
                              AND ZFDHAPP IN S_DHAPP
                              AND ZFDHSSD IN S_DHSSD
                              AND ZFDHSST IN S_DHSST.

  IF SY-SUBRC NE 0.            " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.
ENDFORM.            " P1000_GET_EDI_DATA
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.                  "헤더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM06'.           " GUI STATUS SETTING.
   SET TITLEBAR  'ZIM40'.           " GUI TITLE SETTING..

   W_LINE = 0.
   W_CNT  = 0.
   SORT IT_TAB BY BUKRS ZFDHDOC ZFDHENO ZFDHJSD.

   LOOP AT IT_TAB.
*> 2001.09.03 KSB MODIFY
     IF SY-TABIX EQ 1 AND IT_TAB-BUKRS IS INITIAL.
       PERFORM  P3000_HEADER_WRITE.
     ENDIF.

     ON CHANGE OF IT_TAB-BUKRS.

       IF W_LINE NE 0.
           FORMAT RESET.
           WRITE:/ SY-ULINE,
                 / SY-VLINE, 98 '회사별 건수:', W_CNT, 116 SY-VLINE,
                  SY-ULINE.
       ENDIF.
*> 2001.09.03 KSB MODIFY
       PERFORM  P3000_HEADER_WRITE.

     ENDON.

     W_MOD = ( W_CNT MOD 2 ).
     IF W_MOD EQ 0.
         FORMAT RESET.
         FORMAT COLOR COL_NORMAL INTENSIFIED.
     ELSE.
         FORMAT RESET.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     ENDIF.

     IF IT_TAB-ZFDHAPP EQ 'Y'.
        WRITE: / SY-VLINE, IT_TAB-ZFDHDOC,
              15 SY-VLINE, IT_TAB-ZFDHENO,
              35 SY-VLINE, IT_TAB-ZFDHREF,
              55 SY-VLINE, IT_TAB-ZFDHJSD,
              68 SY-VLINE, IT_TAB-ZFDHJSH,
              80 SY-VLINE, 85 IT_TAB-ZFDHAPP,
              91 SY-VLINE, IT_TAB-ZFDHSSD,
             104 SY-VLINE, IT_TAB-ZFDHSST, 116 SY-VLINE.
     ELSE.
        WRITE: / SY-VLINE, IT_TAB-ZFDHDOC,
              15 SY-VLINE, IT_TAB-ZFDHENO,
              35 SY-VLINE, IT_TAB-ZFDHREF,
              55 SY-VLINE, IT_TAB-ZFDHJSD,
              68 SY-VLINE, IT_TAB-ZFDHJSH,
              80 SY-VLINE, 85 IT_TAB-ZFDHAPP,
              91 SY-VLINE,
             104 SY-VLINE, 116 SY-VLINE.
     ENDIF.

     HIDE IT_TAB.

     W_LINE = W_LINE + 1.
     W_CNT  = W_CNT + 1.

     AT LAST.
*        W_CNT = '9999'.
        FORMAT RESET.
        WRITE:/ SY-ULINE,
              / SY-VLINE, 98 '회사별 건수:', W_CNT, 116 SY-VLINE,
                SY-ULINE.
     ENDAT.
   ENDLOOP.
   CLEAR : IT_TAB.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_VIEW_FLATFILE
*&---------------------------------------------------------------------*
FORM P3000_VIEW_FLATFILE.

   DATA:W_EDI_RECORD(500),
        W_LCNT(4),
        W_FNAME2(80),
        W_LMOD(1)    VALUE  '0'.

   REFRESH : IT_EXCL.              " Inactive Function용 Internal Table

   MOVE 'REFR'  TO IT_EXCL-FCODE.     APPEND IT_EXCL.
   MOVE 'DISP'  TO IT_EXCL-FCODE.     APPEND IT_EXCL.
   MOVE 'DOWN'  TO IT_EXCL-FCODE.     APPEND IT_EXCL.
   MOVE 'FVIEW' TO IT_EXCL-FCODE.     APPEND IT_EXCL.
   MOVE '%EX'   TO IT_EXCL-FCODE.     APPEND IT_EXCL.
   MOVE 'RW'    TO IT_EXCL-FCODE.     APPEND IT_EXCL.
   MOVE 'ENTR'  TO IT_EXCL-FCODE.     APPEND IT_EXCL.

   SET PF-STATUS 'ZIM06' EXCLUDING IT_EXCL.
   SET TITLEBAR  'ZIM41'.           " GUI TITLE SETTING..

   SKIP 2.
   FORMAT RESET.
   WRITE:/50 '[ FLAT FILE 내역 ]' COLOR COL_POSITIVE INTENSIFIED OFF.
   SKIP 1.
   FORMAT RESET.
   FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
   WRITE: IT_TAB-FILENAME TO W_FNAME2 LEFT-JUSTIFIED.
   WRITE:/ SY-ULINE,
         / SY-VLINE, 'FLAT File 명: ', W_FNAME2, 116 SY-VLINE,
         / SY-ULINE.

*   SKIP 1.
*   WRITE:/ SY-ULINE.
*>> 서버의 파일을 읽기위해 Data Set을 열고
   OPEN    DATASET   IT_TAB-FILENAME    FOR     INPUT   IN  TEXT  MODE.
   IF SY-SUBRC NE 0.
      EXIT.
   ENDIF.
*>> 읽은 파일의 내용을 루프를 돌며 찍는다.
   DO.
      READ    DATASET   IT_TAB-FILENAME INTO    W_EDI_RECORD.
      IF SY-SUBRC    EQ    4.
         EXIT.
      ENDIF.

      W_LMOD = W_LCNT MOD 2.

      IF W_LMOD EQ '0'.
        FORMAT RESET.
        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
        WRITE :/ SY-VLINE, W_EDI_RECORD(114), 116 SY-VLINE.
      ELSE.
        FORMAT RESET.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
        WRITE : / SY-VLINE, W_EDI_RECORD(114), 116 SY-VLINE.
      ENDIF.
      W_LCNT = W_LCNT + 1.
   ENDDO.
      WRITE: SY-ULINE.

ENDFORM.                    " P3000_VIEW_FLATFILE
*&---------------------------------------------------------------------*
*&      Form  P3000_HEADER_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_HEADER_WRITE.
       SELECT SINGLE * FROM T001 WHERE BUKRS EQ IT_TAB-BUKRS.

       CONCATENATE '(' T001-BUTXT INTO W_BUTXT.
       WRITE: W_BUTXT TO W_BUTXT LEFT-JUSTIFIED.
       CONCATENATE W_BUTXT ')' INTO W_BUTXT.
       WRITE: W_BUTXT TO W_BUTXT LEFT-JUSTIFIED.

       FORMAT RESET.
       WRITE:/3 '회사코드:', IT_TAB-BUKRS, W_BUTXT,
             96 'Date : ', SY-DATUM.

       FORMAT COLOR COL_HEADING INTENSIFIED OFF.
       WRITE:/ SY-ULINE,
             / SY-VLINE, '전자문서종류',
            15 SY-VLINE, '전자문서번호',
            35 SY-VLINE, '참조문서번호',
            55 SY-VLINE, '송수신일자',
            68 SY-VLINE, '송수신시간',
            80 SY-VLINE, '반영여부',
            91 SY-VLINE, '반영일자',
           104 SY-VLINE, '반영시간', 116 SY-VLINE, SY-ULINE.
       W_CNT = 0.

ENDFORM.                    " P3000_HEADER_WRITE
