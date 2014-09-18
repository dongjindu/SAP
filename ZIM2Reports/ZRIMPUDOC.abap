************************************************************************
*                                                                      *
*  모듈/서브모듈    :        MM / 수입                                 *
*  TYPE             :        REPORT                                    *
*  NAME             :        ZRIMPUDOC                                 *
*  T_CODE           :                                                  *
*                                                                      *
*  Description      :        구매승인서 출력                           *
*                                                                      *
************************************************************************
*                        MODIFICATION LOG                              *
*                                                                      *
*  Date.           Authors.        description.                        *
* -----------    -----------    ------------------------------         *
* 2002/02/18      김 영 광       INITIAL RELEASE                       *
*                                                                      *
************************************************************************
REPORT  ZRIMPUDOC      NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 120
                       LINE-COUNT 90.

TABLES: ZTPUR,ZTPURSG1,ZTPURSG4,ZTREQST.

*----------------------------------------------------------------------
* 구매승인서 INTERNAL TABLE
*----------------------------------------------------------------------

*DATA : BEGIN OF IT_TAB OCCURS 0,
*       ZFREQNO  LIKE    ZTPUR-ZFREQNO,     " 수입의뢰 관리번호.
*       ZFAPPNM1 LIKE    ZTPUR-ZFAPPNM1,    " 상호1.
*       ZFAPPNM2 LIKE    ZTPUR-ZFAPPNM2,    " 상호2.
*       ZFAPPNM3 LIKE    ZTPUR-ZFAPPNM3,    " 상호3.
*       ZFAPPAD1 LIKE    ZTPUR-ZFAPPAD1,    " 주소.
*       ZFAPPAD2 LIKE    ZTPUR-ZFAPPAD2,    " 주소.
*       ZFAPPAD3 LIKE    ZTPUR-ZFAPPAD3,    " 주소.
*       ZFBENI   LIKE    ZTPUR-ZFBENI,      " 수익자.
*       ZFVENNM1 LIKE    ZTPUR-ZFVENNM1,    " 공급자상호.
*       ZFVENNM2 LIKE    ZTPUR-ZFVENNM2,    " 공급자상호.
*       ZFVENID  LIKE    ZTPUR-ZFVENID,     " 공급자식별자.
*       ZFVENAD1 LIKE    ZTPUR-ZFVENAD1,    " 공급자 주소.
*       ZFVENAD2 LIKE    ZTPUR-ZFVENAD2,    " 공급자 주소.
*       ZFVENAD3 LIKE    ZTPUR-ZFVENAD3,    " 공급자 주소.
*       ZFTOCN   LIKE    ZTPUR-ZFTOCN,      " 총수량.
*       ZFTOCNM  LIKE    ZTPUR-ZFTOCNM,     " 총수량 단위.
*       ZFTOAM   LIKE    ZTPUR-ZFTOAM,      " 총금액.
*       ZFTOAMC  LIKE    ZTPUR-ZFTOAMC,     " 총금액 단위.
*       ZFTOAMU  LIKE    ZTPUR-ZFTOAMU,     " USD총금액.
*       ZFUSD    LIKE    ZTPUR-ZFUSD.       " USD총금액 단위.
*DATA : END OF IT_TAB.

*DATA : BEGIN OF IT_TAB1 OCCURS 0,
*       ZFREQNO  LIKE    ZTPURSG4-ZFREQNO,
*       ZFAMDNO  LIKE    ZTPURSG1-ZFAMDNO,  " AMEND 회차.
*       STAWN    LIKE    ZTPURSG1-STAWN,    " HS CODE.
*       ZFHSDESC LIKE    ZTPURSG1-ZFHSDESC, " HS 코드 품명.
*       ZFGODS1  LIKE    ZTPURSG1-ZFGODS1,  " 품목명세.
*       MENGE    LIKE    ZTPURSG1-MENGE,    " 수량.
*       MEINS    LIKE    ZTPURSG1-MEINS,    " 수량단위.
*       ZFGOAMT  LIKE    ZTPURSG1-ZFGOAMT,  " 금액.
*       WAERS    LIKE    ZTPURSG1-WAERS,    " 금액단위.
*       ZFGOAMTU LIKE    ZTPURSG1-ZFGOAMTU, " 미화금액.
*       ZFUSD    LIKE    ZTPURSG1-ZFUSD,    " 통화.
*       ZFNETPRU LIKE    ZTPURSG1-ZFNETPRU, " 단가미화.
*       NETPR    LIKE    ZTPURSG1-NETPR,    " 단가.
*       PEINH    LIKE    ZTPURSG1-PEINH.    " 단가단위.
*DATA : END OF IT_TAB1.

*DATA : BEGIN OF IT_TAB2 OCCURS 0,
*       ZFREQNO  LIKE    ZTPURSG4-ZFREQNO,  " 수입의뢰 관리번호.
*       ZFAMDNO  LIKE    ZTPURSG4-ZFAMDNO,  " AMEND 회차.
*       STAWN    LIKE    ZTPURSG4-STAWN,    " HS CODE.
*       ZFGOAMT  LIKE    ZTPURSG4-ZFGOAMT,  " 금액.
*       WAERS    LIKE    ZTPURSG4-WAERS,    " 금액단위.
*       ZFGODS1  LIKE    ZTPURSG4-ZFGODS1,  " 품목명세.
*       ZFSDOC   LIKE    ZTPURSG4-ZFSDOC,   " 근거서류명.
*       ZFSDNO   LIKE    ZTPURSG4-ZFSDNO,   " 근거서류번호.
*       ZFEXDT   LIKE    ZTPURSG4-ZFEXDT,   " 유효기일.
*       ZFSPDT   LIKE    ZTPURSG4-ZFSPDT,   " 선적기일.
*       ZFDOCST  LIKE    ZTREQST-ZFDOCST.   " 문서상태.
*DATA : END OF IT_TAB2.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,              " 선택 LINE COUNT
       W_PAGE            TYPE I,              " Page Counter
       W_LINE            TYPE I,              " 페이지당 LINE COUNT
       LINE(3)           TYPE N,              " 페이지당 LINE COUNT
       W_COUNT           TYPE I,              " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME," 필드명.
       W_SUBRC           LIKE SY-UCOMM,
       W_TABIX           LIKE SY-TABIX.       " TABLE INDEX

*----------------------------------------------------------------------
* Selection Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
    PARAMETERS : P_REQNO  LIKE ZTPUR-ZFREQNO
                 MEMORY ID  ZPREQNO,
                 P_AMDNO  LIKE ZTPURSG1-ZFAMDNO
                 MEMORY ID ZPAMDNO.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* INITIALIZATION.
*----------------------------------------------------------------------
INITIALIZATION.                      " 초기값 SETTING
    SET  TITLEBAR 'ZIMRPU'.          " TITLE BAR

*----------------------------------------------------------------------
* START OF SELECTION
*----------------------------------------------------------------------
START-OF-SELECTION.
* 테이블 SELECT
    PERFORM   P1000_READ_DATA.

* 레포트 Write
    PERFORM   P3000_DATA_WRITE.

    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA.
*  SELECT SINGLE * FROM ZTREQST
*          WHERE ZFREQNO = P_REQNO
*            AND ZFAMDNO = P_AMDNO.
*  IF ZTREQST-ZFRLST2 NE 'R'.
*      MESSAGE S356 WITH P_REQNO.
*      LEAVE TO SCREEN 0.
*  ENDIF.
*  CASE  ZTREQST-ZFDOCST.
*    WHEN 'R' OR 'A' OR 'O'.
*       MESSAGE S356 WITH P_REQNO.
*       LEAVE TO SCREEN 0.
*    WHEN OTHERS.
*  ENDCASE.
*
***>> ZTPURSG1
*  REFRESH IT_TAB1.
*  SELECT  *  FROM ZTPURSG1
*            WHERE  ZFREQNO = P_REQNO
*              AND  ZFAMDNO = P_AMDNO.
*     CLEAR IT_TAB.
*     MOVE-CORRESPONDING ZTPURSG1 TO IT_TAB1.
*     APPEND IT_TAB1.
*  ENDSELECT.
***>> ZTPURSG4
*  REFRESH IT_TAB2.
*  SELECT *  FROM ZTPURSG4
*          WHERE  ZFREQNO = P_REQNO
*            AND  ZFAMDNO = P_AMDNO.
*     CLEAR IT_TAB2.
*     MOVE-CORRESPONDING ZTPURSG4 TO IT_TAB2.
*     APPEND IT_TAB2.
*  ENDSELECT.
***>> ZTPUR
*  SELECT SINGLE * FROM ZTPUR
*                 WHERE ZFREQNO = P_REQNO
*                   AND ZFAMDNO = P_AMDNO.
*  MOVE-CORRESPONDING ZTPUR TO IT_TAB.
*  APPEND IT_TAB.
*  DESCRIBE TABLE IT_TAB LINES W_LINE.
*  IF W_LINE EQ 0.               " Not Found?
*    MESSAGE S356 WITH P_REQNO.
*    LEAVE TO SCREEN 0.
*  ENDIF.
*
ENDFORM.                     " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .
    SET TITLEBAR   'ZIMRPU'.          " TITLE BAR
    PERFORM P3000_LINE_WRITE.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.
    WRITE : / '준비중......'.

*  SKIP 5.
*  WRITE : /44 '외회획득용원료(물품)구매승인신청서'.
*  ULINE AT /105(15). WRITE: 120 SY-VLINE.
*  WRITE:/105 SY-VLINE, 106 '  처리기간  ',120 SY-VLINE.
*  ULINE AT /105(15). WRITE: 120 SY-VLINE.
*  WRITE:/105  SY-VLINE, 106 '    3 일    ',120 SY-VLINE.
*  WRITE:/     SY-ULINE,120 SY-VLINE.
*  WRITE:/ SY-VLINE, 59 SY-VLINE, 120 SY-VLINE,
*         /  SY-VLINE, (55) '신청인(상호,주소,성명)',
*        59 SY-VLINE, 61 '공급자(상호,주소,성명)',120 SY-VLINE,
*        / SY-VLINE, 59 SY-VLINE, 120 SY-VLINE,
*        /  SY-VLINE, 10 IT_TAB-ZFAPPNM1,
*        59 SY-VLINE, 68 IT_TAB-ZFVENNM1,            120 SY-VLINE,
*        / SY-VLINE, 59 SY-VLINE, 120 SY-VLINE,
*        /  SY-VLINE, 10 IT_TAB-ZFAPPAD1 NO-GAP,(18)IT_TAB-ZFAPPAD2,
*        59 SY-VLINE, 68 IT_TAB-ZFVENAD1 NO-GAP,(18)IT_TAB-ZFVENAD2,
*                                                  120 SY-VLINE,
*        / SY-VLINE, 59 SY-VLINE, 120 SY-VLINE,
*           SY-VLINE, 10 IT_TAB-ZFAPPNM3,43 '서명또는 (인)',
*        59 SY-VLINE, 68 IT_TAB-ZFVENID,104'서명또는 (인)',120 SY-VLINE,
*        / SY-VLINE, 59 SY-VLINE, 120 SY-VLINE,
*        /   SY-ULINE,                  120 SY-VLINE,
*        /   SY-VLINE,                  120 SY-VLINE,
*        /   SY-VLINE,'공급물품명세(국산원자재,기초원자재,수출물품증',
*                     '해당사항에 O 표 할것.)', 120 SY-VLINE,
*        /   SY-VLINE,                  120 SY-VLINE,
*        /   SY-ULINE,                  120 SY-VLINE,
*        /   SY-VLINE, (15) ' ',
*            SY-VLINE, (30) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (20) ' ',120 SY-VLINE,
*        /   SY-VLINE, (15) '  H S   부 호   ',
*            SY-VLINE, (30) '     품 명  및  규 격',
*            SY-VLINE, (20) '  단 위  및 수 량 ',
*            SY-VLINE, (20) '  단           가 ',
*            SY-VLINE, (20) '  금           액',120 SY-VLINE,
*        /   SY-VLINE, (15) ' ',
*            SY-VLINE, (30) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (20) '',
*            SY-VLINE, (20) ' (U S $ 화  부 기)',120 SY-VLINE,
*        /   SY-ULINE,                  120 SY-VLINE.
**>> MULTIDATA WRITE.
*  PERFORM P3000_MULTIDATA.
*
*  WRITE:/   SY-ULINE,                  120 SY-VLINE,
*        /   SY-VLINE,                  120 SY-VLINE,
*        /   SY-VLINE,45 'T O T A L : ',(41) ' ',
*         (3) IT_TAB-ZFTOAMC,
*        (16) IT_TAB-ZFTOAM  CURRENCY IT_TAB-ZFTOAMC,
*                                       120 SY-VLINE,
*        /   SY-VLINE,                  120 SY-VLINE,
*        /   SY-ULINE,                  120 SY-VLINE,
*        /   SY-VLINE,                  120 SY-VLINE,
*        /   SY-VLINE,'수출또는 구매승인서상의 내용',120 SY-VLINE,
*        /   SY-VLINE,                  120 SY-VLINE,
*        /   SY-ULINE,                  120 SY-VLINE,
*
*        /   SY-VLINE, (20) ' ',
*            SY-VLINE, (15) ' ',
*            SY-VLINE, (26) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (10) '  ',
*            SY-VLINE, (10) ' ',        120 SY-VLINE,
*
*        /   SY-VLINE, (20) ' 근거서류명 및 번호',
*            SY-VLINE, (15) '  H S   부 호',
*            SY-VLINE, (26) '   품               명 ',
*            SY-VLINE, (20) '   금          액',
*            SY-VLINE, (10) '유효 기일',
*            SY-VLINE, (10) '선적 기일',120 SY-VLINE,
*        /   SY-VLINE, (20) ' ',
*            SY-VLINE, (15) ' ',
*            SY-VLINE, (26) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (10) ' ',
*            SY-VLINE, (10) ' ',        120 SY-VLINE,
*        /   SY-ULINE,                  120 SY-VLINE.
**>> MULTIDATAWRITE.
*  PERFORM P3000_MULTIDATAWRITE.
*
*  WRITE:  /  SY-ULINE,                  120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-VLINE,'승인요건',120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-ULINE,                  120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-VLINE,'승인번호',120 SY-VLINE,
*          /  SY-VLINE,'    ',120 SY-VLINE,
*          /  SY-ULINE,                  120 SY-VLINE,
*          /  SY-VLINE,'위의 신청사항을 대외무역관리규정',
*             '제4-2-7조의 규정에 의하여 승인합니다.',120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-VLINE, 93 '20001.    .    . ',120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-VLINE,80 '은  행  장', 113 '(인)', 120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-VLINE,' ',120 SY-VLINE,
*          /  SY-ULINE,                 120 SY-VLINE.
*
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------
*&      Form  P3000_MULTIDATA
*&---------------------------------------------------------------------
*FORM P3000_MULTIDATA.
* LOOP AT IT_TAB1.
*  WRITE:/   SY-VLINE, (15) ' ',
*            SY-VLINE, (30) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (20) '',
*            SY-VLINE, (20) ' ',120 SY-VLINE,
*        /   SY-VLINE, (15) IT_TAB1-STAWN,
*            SY-VLINE, (30) IT_TAB1-ZFHSDESC,
*            SY-VLINE, (16) IT_TAB1-MENGE UNIT IT_TAB1-MEINS,
*                       (3) IT_TAB1-MEINS,
*            SY-VLINE,  (3) IT_TAB1-WAERS,
*                      (16) IT_TAB1-NETPR CURRENCY IT_TAB1-WAERS,
*            SY-VLINE,  (3)  IT_TAB1-WAERS,
*                      (16) IT_TAB1-ZFGOAMT
*                           CURRENCY IT_TAB1-WAERS,120 SY-VLINE,
*         /  SY-VLINE, (15) ' ',
*            SY-VLINE, (30) ' ',
*            SY-VLINE, (16) ' ',
*                       (3) ' ',
*            SY-VLINE,  (3)  ' ',
*                      (16) ' ',
*            SY-VLINE,  (3) IT_TAB1-ZFUSD,
*                      (16) IT_TAB1-ZFGOAMTU
*                           CURRENCY IT_TAB1-ZFUSD,120 SY-VLINE.
* ENDLOOP.
*
*ENDFORM.                    " P3000_MULTIDATA

*&---------------------------------------------------------------------
*&      Form  P3000_MULTIDATAWRITE
*&---------------------------------------------------------------------
*FORM P3000_MULTIDATAWRITE.
* LOOP AT IT_TAB2.
*   WRITE:/  SY-VLINE, (20) ' ',
*            SY-VLINE, (15) ' ',
*            SY-VLINE, (26) ' ',
*            SY-VLINE, (20) ' ',
*            SY-VLINE, (10) ' ',
*            SY-VLINE, (10) ' ',        120 SY-VLINE,
*        /   SY-VLINE, (20)  IT_TAB2-ZFSDOC,
*            SY-VLINE, (15)  IT_TAB2-STAWN,   "  HS 부호',
*            SY-VLINE, (26)  IT_TAB2-ZFGODS1 ,"  품명 ',
*            SY-VLINE,  (3)  IT_TAB2-WAERS,
*                      (16) IT_TAB2-ZFGOAMT CURRENCY IT_TAB2-WAERS,
*"금액'
*            SY-VLINE, (10) IT_TAB2-ZFEXDT," 유효기일',
*            SY-VLINE, (10) IT_TAB2-ZFSPDT, 120 SY-VLINE,
*        /   SY-VLINE, (20) IT_TAB2-ZFSDNO,
*            SY-VLINE, (15) ' ',  "  HS 부호',
*            SY-VLINE, (26) ' ',"  품명 ',
*            SY-VLINE, (3)  ' ',
*                      (16) ' ',"  금액'
*            SY-VLINE, (10) ' '," 유효기일',
*            SY-VLINE, (10) ' ', 120 SY-VLINE.
* ENDLOOP.
*
*ENDFORM.                    " P3000_MULTIDATAWRITE
