*&---------------------------------------------------------------------
*& Report  ZRIMCUST
*&---------------------------------------------------------------------
*& 프로그램명 : ZRIMCUST
*&     작성자 : 맹성호 INFOLINK.Ltd
*&     작성일 : 01/07/2002
*&---------------------------------------------------------------------
*& Desc.      : 자재별/ 플랜트별/ 국가별 수입실적을 보여준다.
*&---------------------------------------------------------------------

REPORT  ZRIMCUST       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 300
                       LINE-COUNT 65.

TABLES : ZTIDS,                                    " 수입면허.
         ZTCUCLCST,                                " 통관비용.
         ZTBL,                                     " B/L 헤더.
         LFA1,
         T001W.

DATA : MAX_LINSZ TYPE I.
DATA : BEGIN OF IT_TAB OCCURS  0,
       CODE(10)         TYPE    C,
       ZFSUPC           LIKE    ZTIDS-ZFSUPC,      " 공급자 국적부호.
       ZFTBAU           LIKE    ZTIDS-ZFTBAU,      " 과세가격-미화.
       ZFUSD            LIKE    ZTIDS-ZFUSD,       " 미화통화.
       ZFMATGB          LIKE    ZTIDS-ZFMATGB,     " 자재구분.
       ZFMATGBNM(15)    TYPE    C,
       ZFIDSDT          LIKE    ZTIDS-ZFIDSDT,     " 신고수리일.
       ZFBLNO           LIKE    ZTIDS-ZFBLNO,      " B/L 관리번호.
       BUKRS            LIKE    ZTIDS-BUKRS,       " 회사코드.
       ZFWERKS          LIKE    ZTBL-ZFWERKS,      " 플랜트.
       NAME1(20)        TYPE    C,
       LIFNR            LIKE    ZTBL-LIFNR.        " 구매처 코드.
DATA : END  OF IT_TAB.

DATA : BEGIN OF IT_LIFNR OCCURS 0,
       LIFNR            LIKE  LFA1-LIFNR,
       NAME1            LIKE  LFA1-NAME1.
DATA : END   OF IT_LIFNR.

DATA : BEGIN OF IT_LAND1 OCCURS 0,
       LAND1            LIKE  T005T-LAND1,
       LANDX            LIKE  T005T-LANDX.
DATA : END   OF IT_LAND1.

*----------------------------------------------------------------------
* 변수 선언.
*----------------------------------------------------------------------
DATA : W_ERR_CHK         TYPE   C,
       W_SUBRC           LIKE   SY-SUBRC,
       W_PAGE            TYPE   I,               " Page Counter
       W_LINE            TYPE   I,               " 페이지당 Line Count
       W_FLAG            TYPE   C,
       W_COUNT           TYPE   I,               " 전체 COUNT
       W_COLOR           TYPE   I,
       W_CODE(15)        TYPE   C,
       W_LIST_INDEX      LIKE   SY-TABIX,
       W_TABIX           LIKE   SY-TABIX,        " Table Index
       W_CNT(1),
       W_TEXT(20)        TYPE   C,
       W_TOTSUM(20)      TYPE   C,
       W_LINES           TYPE   I,
       W_WERKSNM(20)     TYPE   C,
       SV_JUL            TYPE   C,
       SV_CHK            TYPE   C,
       SV_CODE(10)       TYPE   C,
       SV_AMOUNT         LIKE   ZTIDS-ZFTBAU,
       SV_MATGB          LIKE   ZTIDS-ZFMATGB,
       SV_BUKRS          LIKE   ZTIDS-BUKRS,
       SV_WERKS          LIKE   ZTBL-ZFWERKS,
       SV_TBAU1          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU2          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU3          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU4          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU5          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU6          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU7          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU8          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU9          LIKE   ZTIDS-ZFTBAU,
       SV_TBAU10         LIKE   ZTIDS-ZFTBAU,
       SV_TBAU11         LIKE   ZTIDS-ZFTBAU,
       SV_TBAU12         LIKE   ZTIDS-ZFTBAU,
       SV_TBAUS          LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU1      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU2      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU3      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU4      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU5      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU6      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU7      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU8      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU9      LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU10     LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU11     LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAU12     LIKE   ZTIDS-ZFTBAU,
       SV_TOT_TBAUS      LIKE   ZTIDS-ZFTBAU.
*----------------------------------------------------------------------
* Selection Screen 절.
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

SELECT-OPTIONS : S_BUKRS  FOR ZTIDS-BUKRS DEFAULT '  ', "회사코드.
                 S_MATGB  FOR ZTIDS-ZFMATGB,       " 자재구분.
                 S_WERKS  FOR ZTBL-ZFWERKS,        " Plant
                 S_IDSDT  FOR ZTIDS-ZFIDSDT.       " 기간.
SELECTION-SCREEN SKIP.

PARAMETER : R_MATE   RADIOBUTTON GROUP GP1,       " 자재/국가별.
            R_TERM   RADIOBUTTON GROUP GP1,       " 자재/기간별.
            R_COUN   RADIOBUTTON GROUP GP1.       " 국가/기간별.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* Top-Of-Page.
*----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*----------------------------------------------------------------------
* Initialization.
*----------------------------------------------------------------------
INITIALIZATION.                                  " 초기값 SETTING
  SET  TITLEBAR 'ZIMR50'.                       " GUI TITLE SETTING..
  PERFORM P1000_INITIALIZATION.

*----------------------------------------------------------------------
* Start Of Selection 절.
*----------------------------------------------------------------------
START-OF-SELECTION.

  IF S_IDSDT-LOW(04) NE S_IDSDT-HIGH(04).
    MESSAGE S597.  EXIT.
  ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_DATA        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  SET PF-STATUS 'ZIMR50'.                   " GUI STATUS SETTING
  SET TITLEBAR  'ZIMR50'.                   " GUI TITLE SETTING
  IF R_MATE = 'X'  OR
     R_TERM = 'X'.
    PERFORM   P3000_WRITE_DATA1.
  ENDIF.
  IF R_COUN = 'X'.
    PERFORM   P3000_WRITE_DATA2.
  ENDIF.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.

    WHEN 'REFR'.
      PERFORM   P1000_READ_DATA        USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.                " 종료.
    WHEN OTHERS.

  ENDCASE.

*&---------------------------------------------------------------------
*&      Form  P1000_INITIALIZATION
*&---------------------------------------------------------------------
FORM P1000_INITIALIZATION.
  CONCATENATE SY-DATUM(6) '01' INTO S_IDSDT-LOW.
  S_IDSDT-HIGH = SY-DATUM.
  APPEND S_IDSDT.
*  IF NOT S_IDSDT-LOW+2(02) EQ S_IDSDT-HIGH+2(02).
*     MESSAGE E597.
*  ENDIF.
ENDFORM.                          " P1000_INITIALIZATION

*&---------------------------------------------------------------------
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------
FORM P1000_READ_DATA  USING W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

* 수입 실적 SELECT.
  REFRESH : IT_TAB.

  SELECT A~ZFMATGB  B~ZFWERKS  B~LIFNR
         A~ZFTBAU   A~ZFIDSDT  A~BUKRS
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM ( ZTIDS  AS  A  INNER JOIN  ZTBL AS B
  ON     A~ZFBLNO        EQ    B~ZFBLNO )
  WHERE  A~ZFMATGB       IN    S_MATGB
  AND    A~BUKRS         IN    S_BUKRS
  AND    A~ZFIDSDT       IN    S_IDSDT
  AND    B~ZFWERKS       IN    S_WERKS
  AND    A~ZFMATGB       NE    SPACE.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

  SORT  IT_TAB BY  ZFMATGB  ZFWERKS.

ENDFORM.                    " P1000_READ_DATA

*&----------------------------------------------------------------------
*&      Form  P3000_WRITE_DATA1
*&----------------------------------------------------------------------
FORM P3000_WRITE_DATA1.

  CLEAR IT_TAB.

  LOOP AT IT_TAB.

    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_TAB-LIFNR.

    IF SY-TABIX EQ 1.
      MOVE : IT_TAB-ZFMATGB TO  SV_MATGB,
             IT_TAB-ZFWERKS TO  SV_WERKS.
    ENDIF.

* 각각의 구분별, 실적금액별로 SUM.
    IF SV_MATGB NE IT_TAB-ZFMATGB.
      PERFORM  P2000_SUM_LINE_WRITE1.
      PERFORM  P3000_TOTAL_LINE_WRITE.
      MOVE : IT_TAB-ZFMATGB  TO SV_MATGB,
             IT_TAB-ZFWERKS  TO SV_WERKS.
      CLEAR : SV_TBAU1, SV_TBAU2,     SV_TBAU3,  SV_TBAU4,
              SV_TBAU5, SV_TBAU6,     SV_TBAU7,  SV_TBAU8,
              SV_TBAU9, SV_TBAU10,    SV_TBAU11, SV_TBAU12,
              SV_TBAUS, SV_TOT_TBAU1, SV_TOT_TBAU2,
              SV_TOT_TBAU3,  SV_TOT_TBAU4,  SV_TOT_TBAU5,
              SV_TOT_TBAU6,  SV_TOT_TBAU7,  SV_TOT_TBAU8,
              SV_TOT_TBAU9,  SV_TOT_TBAU10, SV_TOT_TBAU11,
              SV_TOT_TBAU12, SV_TOT_TBAUS.
    ENDIF.

    IF SV_MATGB  EQ  IT_TAB-ZFMATGB AND
       SV_WERKS  NE  IT_TAB-ZFWERKS .
      PERFORM  P2000_SUM_LINE_WRITE1.
      MOVE : IT_TAB-ZFWERKS  TO  SV_WERKS.
      CLEAR : SV_TBAU1, SV_TBAU2,   SV_TBAU3,  SV_TBAU4,
              SV_TBAU5, SV_TBAU6,   SV_TBAU7,  SV_TBAU8,
              SV_TBAU9, SV_TBAU10,  SV_TBAU11, SV_TBAU12,
              SV_TBAUS.
    ENDIF.
* 자재 / 국가별 수입실적.
    IF  R_MATE = 'X'.

      IF IT_TAB-LIFNR = 'X0201'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU1,  SV_TOT_TBAU1.
      ENDIF.
      IF IT_TAB-LIFNR = 'X0401'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU2,  SV_TOT_TBAU2.
      ENDIF.
      IF IT_TAB-LIFNR = 'X0301'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU3,  SV_TOT_TBAU3.
      ENDIF.
      IF LFA1-LAND1 = 'JP'  AND
         IT_TAB-LIFNR  NE 'X0201'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU4,  SV_TOT_TBAU4.
      ENDIF.
      IF LFA1-LAND1 = 'US'  AND
         IT_TAB-LIFNR  NE 'X0401'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU5,  SV_TOT_TBAU5.
      ENDIF.
      IF LFA1-LAND1 = 'FR'  AND
         IT_TAB-LIFNR  NE 'X0301'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU6,  SV_TOT_TBAU6.
      ENDIF.
      IF LFA1-LAND1 = 'GB'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU7,  SV_TOT_TBAU7.
      ENDIF.
      IF LFA1-LAND1 = 'DE'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU8,  SV_TOT_TBAU8.
      ENDIF.
      IF LFA1-LAND1 = 'CH'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU9,  SV_TOT_TBAU9.
      ENDIF.
      IF LFA1-LAND1 = 'IT'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU10, SV_TOT_TBAU10.
      ENDIF.
      IF LFA1-LAND1 = 'CN'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU11, SV_TOT_TBAU11.
      ENDIF.
      IF LFA1-LAND1 NE 'JP'  AND
         LFA1-LAND1 NE 'US'  AND
         LFA1-LAND1 NE 'FR'  AND
         LFA1-LAND1 NE 'GB'  AND
         LFA1-LAND1 NE 'DE'  AND
         LFA1-LAND1 NE 'CH'  AND
         LFA1-LAND1 NE 'IT'  AND
         LFA1-LAND1 NE 'CN'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU12, SV_TOT_TBAU12.
      ENDIF.

      ADD   IT_TAB-ZFTBAU  TO : SV_TBAUS,  SV_TOT_TBAUS.
* 자재 / 기간별 수입실적.
    ELSEIF R_TERM = 'X'.

      IF IT_TAB-ZFIDSDT+4(02) = '01'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU1,  SV_TOT_TBAU1.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '02'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU2,  SV_TOT_TBAU2.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '03'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU3,  SV_TOT_TBAU3.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '04'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU4,  SV_TOT_TBAU4.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '05'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU5,  SV_TOT_TBAU5.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '06'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU6,  SV_TOT_TBAU6.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '07'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU7,  SV_TOT_TBAU7.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '08'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU8,  SV_TOT_TBAU8.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '09'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU9,  SV_TOT_TBAU9.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '10'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU10, SV_TOT_TBAU10.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '11'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU11, SV_TOT_TBAU11.
      ENDIF.
      IF IT_TAB-ZFIDSDT+4(02) = '12'.
        ADD  IT_TAB-ZFTBAU   TO : SV_TBAU12, SV_TOT_TBAU12.
      ENDIF.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAUS,  SV_TOT_TBAUS.

    ENDIF.

  ENDLOOP.
  PERFORM  P2000_SUM_LINE_WRITE1.
  PERFORM  P3000_TOTAL_LINE_WRITE.

ENDFORM.                    " P3000_WRITE_DATA1

*&----------------------------------------------------------------------
*&      Form  P3000_WRITE_DATA2
*&----------------------------------------------------------------------
FORM P3000_WRITE_DATA2.

  CLEAR IT_TAB.

  LOOP AT IT_TAB.

    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_TAB-LIFNR.

    IF IT_TAB-LIFNR = 'X0201'.
      MOVE  'A'   TO  IT_TAB-CODE.
    ENDIF.
    IF IT_TAB-LIFNR = 'X0401'.
      MOVE  'B'   TO  IT_TAB-CODE.
    ENDIF.
    IF IT_TAB-LIFNR = 'X0301'.
      MOVE  'C'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'JP'  AND
       IT_TAB-LIFNR  NE 'X0201'.
      MOVE  'D'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'US'  AND
       IT_TAB-LIFNR  NE 'X0401'.
      MOVE  'E'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'FR'  AND
       IT_TAB-LIFNR  NE 'X0301'.
      MOVE  'F'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'GB'.
      MOVE  'G'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'DE'.
      MOVE  'H'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'CH'.
      MOVE  'I'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'IT'.
      MOVE  'J'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 = 'CN'.
      MOVE  'K'   TO  IT_TAB-CODE.
    ENDIF.
    IF LFA1-LAND1 NE 'JP'  AND
       LFA1-LAND1 NE 'US'  AND
       LFA1-LAND1 NE 'FR'  AND
       LFA1-LAND1 NE 'GB'  AND
       LFA1-LAND1 NE 'DE'  AND
       LFA1-LAND1 NE 'CH'  AND
       LFA1-LAND1 NE 'IT'  AND
       LFA1-LAND1 NE 'CN'.
      MOVE  'L'   TO  IT_TAB-CODE.
    ENDIF.
    MODIFY IT_TAB INDEX SY-TABIX.

  ENDLOOP.

  SORT IT_TAB BY CODE ZFMATGB.

  LOOP AT IT_TAB.

    CLEAR LFA1.
    SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ IT_TAB-LIFNR.

    IF SY-TABIX EQ 1.
      MOVE : IT_TAB-CODE    TO  SV_CODE,
             IT_TAB-ZFMATGB TO  SV_MATGB.
    ENDIF.

* 각각의 구분별, 실적금액별로 SUM.
    IF SV_CODE NE IT_TAB-CODE.
      PERFORM  P2000_SUM_LINE_WRITE2.
      PERFORM  P3000_TOTAL_LINE_WRITE.
      MOVE : IT_TAB-CODE     TO SV_CODE,
             IT_TAB-ZFMATGB  TO SV_MATGB.
      CLEAR : SV_TBAU1, SV_TBAU2,     SV_TBAU3,  SV_TBAU4,
              SV_TBAU5, SV_TBAU6,     SV_TBAU7,  SV_TBAU8,
              SV_TBAU9, SV_TBAU10,    SV_TBAU11, SV_TBAU12,
              SV_TBAUS, SV_TOT_TBAU1, SV_TOT_TBAU2,
              SV_TOT_TBAU3,  SV_TOT_TBAU4,  SV_TOT_TBAU5,
              SV_TOT_TBAU6,  SV_TOT_TBAU7,  SV_TOT_TBAU8,
              SV_TOT_TBAU9,  SV_TOT_TBAU10, SV_TOT_TBAU11,
              SV_TOT_TBAU12, SV_TOT_TBAUS.
    ENDIF.

    IF SV_CODE   EQ  IT_TAB-CODE     AND
       SV_MATGB  NE  IT_TAB-ZFMATGB.
      PERFORM  P2000_SUM_LINE_WRITE2.
      MOVE : IT_TAB-ZFMATGB  TO  SV_MATGB.

      CLEAR : SV_TBAU1, SV_TBAU2,   SV_TBAU3,  SV_TBAU4,
              SV_TBAU5, SV_TBAU6,   SV_TBAU7,  SV_TBAU8,
              SV_TBAU9, SV_TBAU10,  SV_TBAU11, SV_TBAU12,
              SV_TBAUS.
    ENDIF.
* 자재 / 기간별 수입실적.
    IF IT_TAB-ZFIDSDT+4(02) = '01'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU1,  SV_TOT_TBAU1.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '02'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU2,  SV_TOT_TBAU2.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '03'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU3,  SV_TOT_TBAU3.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '04'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU4,  SV_TOT_TBAU4.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '05'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU5,  SV_TOT_TBAU5.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '06'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU6,  SV_TOT_TBAU6.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '07'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU7,  SV_TOT_TBAU7.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '08'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU8,  SV_TOT_TBAU8.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '09'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU9,  SV_TOT_TBAU9.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '10'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU10, SV_TOT_TBAU10.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '11'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU11, SV_TOT_TBAU11.
    ENDIF.
    IF IT_TAB-ZFIDSDT+4(02) = '12'.
      ADD  IT_TAB-ZFTBAU   TO : SV_TBAU12, SV_TOT_TBAU12.
    ENDIF.
    ADD  IT_TAB-ZFTBAU   TO : SV_TBAUS,  SV_TOT_TBAUS.

  ENDLOOP.
  PERFORM  P2000_SUM_LINE_WRITE2.
  PERFORM  P3000_TOTAL_LINE_WRITE.

ENDFORM.                    " P3000_WRITE_DATA2
*&----------------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...
* 레포트 Write
  IF R_MATE = 'X'  OR
     R_TERM = 'X'.
    PERFORM   P3000_WRITE_DATA1.
  ENDIF.
  IF R_COUN = 'X'.
    PERFORM   P3000_WRITE_DATA2.
  ENDIF.
ENDFORM.                              " RESET_LIST

*&---------------------------------------------------------------------
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------

FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                              " P2000_Page_Check

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------

FORM P3000_TITLE_WRITE.
  SKIP 1.

* 자재 / 국가별 수입실적.
  IF  R_MATE = 'X'.
    MAX_LINSZ = 240.
    WRITE : /50 '   [ 자재구분 / 국가별 수입실적 ]   ' COLOR 1.
    SKIP 1.
    WRITE : /216 'DATE :', SY-DATUM.
    WRITE : /3 'CIF(USD)',
             15 '개설기간 :', S_IDSDT-LOW, '-', S_IDSDT-HIGH,
            216 'PAGE :', SY-PAGNO.
    NEW-LINE. ULINE AT 1(MAX_LINSZ).

    FORMAT COLOR 1 INTENSIFIED OFF.
    WRITE : / SY-VLINE,
        (15) '자재구분'      CENTERED,        SY-VLINE,
        (15) '플랜트'        CENTERED,        SY-VLINE,
*         (17) '동경지사'      CENTERED,        SY-VLINE,
*         (17) 'ROYAL MAXIM '  CENTERED,        SY-VLINE,
*         (17) 'P B S'         CENTERED,        SY-VLINE,
        (17) '일  본'        CENTERED,        SY-VLINE,
        (17) '미  국'        CENTERED,        SY-VLINE,
        (17) '프랑스'        CENTERED,        SY-VLINE,
        (17) '영  국'        CENTERED,        SY-VLINE,
        (17) '독  일'        CENTERED,        SY-VLINE,
        (17) '스위스'        CENTERED,        SY-VLINE,
        (17) '이태리'        CENTERED,        SY-VLINE,
        (17) '중  국'        CENTERED,        SY-VLINE,
        (17) '기  타'        CENTERED,        SY-VLINE,
        (20) '합  계'        CENTERED,        SY-VLINE.
    NEW-LINE. ULINE AT 1(MAX_LINSZ).

* 자재 / 기간별 수입실적.
  ELSEIF R_TERM = 'X'.
    MAX_LINSZ = 300.
    WRITE : /65 '   [ 자재구분 / 기간별 수입실적 ]   ' COLOR 1.
    SKIP 1.
    WRITE : /270 'DATE :', SY-DATUM.
    WRITE : /3 'CIF(USD)',
             15 '개설기간 :', S_IDSDT-LOW, '-', S_IDSDT-HIGH,
            270 'PAGE :', SY-PAGNO.
    WRITE : / SY-ULINE.

    FORMAT COLOR 1 INTENSIFIED OFF.
    WRITE : / SY-VLINE,
        (15) '자재구분'      CENTERED,        SY-VLINE,
        (15) '플랜트'        CENTERED,        SY-VLINE,
        (17) '1  월'         CENTERED,        SY-VLINE,
        (17) '2  월'         CENTERED,        SY-VLINE,
        (17) '3  월'         CENTERED,        SY-VLINE,
        (17) '4  월'         CENTERED,        SY-VLINE,
        (17) '5  월'         CENTERED,        SY-VLINE,
        (17) '6  월'         CENTERED,        SY-VLINE,
        (17) '7  월'         CENTERED,        SY-VLINE,
        (17) '8  월'         CENTERED,        SY-VLINE,
        (17) '9  월'         CENTERED,        SY-VLINE,
        (17) '10 월'         CENTERED,        SY-VLINE,
        (17) '11 월'         CENTERED,        SY-VLINE,
        (17) '12 월'         CENTERED,        SY-VLINE,
        (20) '합  계'        CENTERED,        SY-VLINE.
    WRITE : / SY-ULINE.

* 국가 / 기간별 수입실적.
  ELSEIF R_COUN = 'X'.
    MAX_LINSZ = 300.
    WRITE : /65 '   [ 국가구분 / 기간별 수입실적 ]   ' COLOR 1.
    SKIP 1.
    WRITE : /270 'DATE :', SY-DATUM.
    WRITE : /3 'CIF(USD)',
             15 '개설기간 :', S_IDSDT-LOW, '-', S_IDSDT-HIGH,
            270 'PAGE :', SY-PAGNO.
    WRITE : / SY-ULINE.

    FORMAT COLOR 1 INTENSIFIED OFF.
    WRITE : / SY-VLINE,
        (15) '지    역'      CENTERED,        SY-VLINE,
        (15) '자재구분'      CENTERED,        SY-VLINE,
        (17) '1  월'         CENTERED,        SY-VLINE,
        (17) '2  월'         CENTERED,        SY-VLINE,
        (17) '3  월'         CENTERED,        SY-VLINE,
        (17) '4  월'         CENTERED,        SY-VLINE,
        (17) '5  월'         CENTERED,        SY-VLINE,
        (17) '6  월'         CENTERED,        SY-VLINE,
        (17) '7  월'         CENTERED,        SY-VLINE,
        (17) '8  월'         CENTERED,        SY-VLINE,
        (17) '9  월'         CENTERED,        SY-VLINE,
        (17) '10 월'         CENTERED,        SY-VLINE,
        (17) '11 월'         CENTERED,        SY-VLINE,
        (17) '12 월'         CENTERED,        SY-VLINE,
        (20) '합  계'        CENTERED,        SY-VLINE.
    WRITE : / SY-ULINE.

  ENDIF.

ENDFORM.                             " FORM P3000_TITLE_WRITE
*&----------------------------------------------------------------------
*&      Form  P2000_SUM_LINE_WRITE1
*&----------------------------------------------------------------------
FORM P2000_SUM_LINE_WRITE1.

  CASE SV_MATGB.
    WHEN '1'.
      MOVE  '수출용 원자재'  TO W_TEXT.
    WHEN '2'.
      MOVE  'Local'          TO W_TEXT.
    WHEN '3'.
      MOVE  '내수용 원자재'  TO W_TEXT.
    WHEN '4'.
      MOVE  '시설재'         TO W_TEXT.
    WHEN '5'.
      MOVE  '상   품'        TO W_TEXT.
    WHEN '6'.
      MOVE  '제   품'        TO W_TEXT.
  ENDCASE.

  SELECT SINGLE NAME1 INTO W_WERKSNM
    FROM T001W
   WHERE WERKS  EQ SV_WERKS.

  WRITE : / SY-VLINE,
         (15) W_TEXT,                            SY-VLINE,
         (15) W_WERKSNM,                         SY-VLINE.
  IF R_MATE NE 'X'.
    WRITE : (17) SV_TBAU1      CURRENCY 'USD',      SY-VLINE,
            (17) SV_TBAU2      CURRENCY 'USD',      SY-VLINE,
            (17) SV_TBAU3      CURRENCY 'USD',      SY-VLINE.
  ENDIF.
  WRITE : (17) SV_TBAU4      CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU5      CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU6      CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU7      CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU8      CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU9      CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU10     CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU11     CURRENCY 'USD',      SY-VLINE,
          (17) SV_TBAU12     CURRENCY 'USD',      SY-VLINE,
          (20) SV_TBAUS      CURRENCY 'USD',      SY-VLINE.
  NEW-LINE. ULINE AT 1(MAX_LINSZ).

ENDFORM.                    " P2000_SUM_LINE_WRITE1
*&----------------------------------------------------------------------
*&      Form  P2000_SUM_LINE_WRITE2
*&----------------------------------------------------------------------
FORM P2000_SUM_LINE_WRITE2.

  CASE SV_CODE.
*      WHEN 'A'.
*         MOVE  '동경지사'       TO W_CODE.
*      WHEN 'B'.
*         MOVE  'ROYAL MAXIM'    TO W_CODE.
*      WHEN 'C'.
*         MOVE  'PBS'            TO W_CODE.
    WHEN 'D'.
      MOVE  '일  본'         TO W_CODE.
    WHEN 'E'.
      MOVE  '미  국'         TO W_CODE.
    WHEN 'F'.
      MOVE  '프랑스'         TO W_CODE.
    WHEN 'G'.
      MOVE  '영  국'         TO W_CODE.
    WHEN 'H'.
      MOVE  '독  일'         TO W_CODE.
    WHEN 'I'.
      MOVE  '스위스'         TO W_CODE.
    WHEN 'J'.
      MOVE  '이태리'         TO W_CODE.
    WHEN 'K'.
      MOVE  '중  국'         TO W_CODE.
    WHEN 'L'.
      MOVE  '기  타'         TO W_CODE.

  ENDCASE.

  CASE SV_MATGB.
    WHEN '1'.
      MOVE '수출용 원자재'   TO W_TEXT.
    WHEN '2'.
      MOVE  'Local'          TO W_TEXT.
    WHEN '3'.
      MOVE  '내수용 원자재'  TO W_TEXT.
    WHEN '4'.
      MOVE  '시설재'         TO W_TEXT.
    WHEN '5'.
      MOVE  '상   품'        TO W_TEXT.
    WHEN '6'.
      MOVE  '제   품'        TO W_TEXT.
  ENDCASE.

  WRITE : / SY-VLINE,
         (15) W_CODE,                            SY-VLINE,
         (15) W_TEXT,                            SY-VLINE,
         (17) SV_TBAU1      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU2      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU3      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU4      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU5      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU6      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU7      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU8      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU9      CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU10     CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU11     CURRENCY 'USD',      SY-VLINE,
         (17) SV_TBAU12     CURRENCY 'USD',      SY-VLINE,
         (20) SV_TBAUS      CURRENCY 'USD',      SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P2000_SUM_LINE_WRITE2

*&----------------------------------------------------------------------
*&      Form  P3000_TOTAL_LINE_WRITE
*&----------------------------------------------------------------------
FORM P3000_TOTAL_LINE_WRITE.
  WRITE : / SY-VLINE,
         (33) '합      계'  CENTERED,            SY-VLINE.
  IF R_MATE NE 'X'.
    WRITE : (17) SV_TOT_TBAU1  CURRENCY 'USD',      SY-VLINE,
            (17) SV_TOT_TBAU2  CURRENCY 'USD',      SY-VLINE,
            (17) SV_TOT_TBAU3  CURRENCY 'USD',      SY-VLINE.
  ENDIF.
  WRITE : (17) SV_TOT_TBAU4  CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU5  CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU6  CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU7  CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU8  CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU9  CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU10 CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU11 CURRENCY 'USD',      SY-VLINE,
          (17) SV_TOT_TBAU12 CURRENCY 'USD',      SY-VLINE,
          (20) SV_TOT_TBAUS  CURRENCY 'USD',      SY-VLINE.
  NEW-LINE. ULINE AT 1(MAX_LINSZ).
ENDFORM.                    " P3000_TOTAL_LINE_WRITE
