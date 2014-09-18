*&--------------------------------------------------------------------*
*& Report  ZRIMLOCAMA                                                 *
*&--------------------------------------------------------------------*
*&  프로그램명 : 취소불능내국신용장조건변경통지서                     *
*&      작성자 : 이석철 INFOLINK Ltd.                                 *
*&      작성일 : 2001.07.24                                           *
*&--------------------------------------------------------------------*
*&   DESC.     :  취소불능내국신용장조건변경신청서.                   *
*&--------------------------------------------------------------------*
*& [변경내용]
*&
*&--------------------------------------------------------------------*

REPORT  ZRIMLOCAMA   MESSAGE-ID ZIM
                     LINE-SIZE 115
                     NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------
* Tables 및 변수 Define
*----------------------------------------------------------------------
TABLES : ZTREQST, ZTLLCHD, ZTLLCAMHD, ZTLLCSG23, ZTLLCAMSGOF, DD07T.

*----------------------------------------------------------------------
* 개설신청 내역 TABLE
*----------------------------------------------------------------------
DATA : W_ERR_CHK(1),
       W_DOM_TEX1 LIKE DD07T-DDTEXT,
       W_DOM_TEX2 LIKE DD07T-DDTEXT,
       W_FOPNNO   LIKE ZTREQST-ZFOPNNO,          "신용장승인번호.
       W_FAPPDT   LIKE ZTREQST-ZFAPPDT,          "신청일(개설예정일).
       W_DHJSD    LIKE ZTDHF1-ZFDHJSD,           "전자문서 전송일자.
       W_DHSSD    LIKE ZTDHF1-ZFDHSSD,           "전자문서 수신일자.
       W_DOCNOR   LIKE ZTDHF1-ZFDOCNOR,          "수신한 전자문서번호.
       W_FDOCNO   LIKE ZTREQST-ZFDOCNO,
       W_INX      TYPE I,
       W_TEM      TYPE STRING,
       W_USD(30),                                "개설후외화금액
       W_KRW(30).                                "개설후원화금액.

DATA : BEGIN  OF    IT_ZTMLOCAMR_1 OCCURS 100,    "물품매도확약서 IT.
       ZFREQNO  LIKE ZTLLCAMSGOF-ZFREQNO,         "수입의뢰 관리번호.
       ZFLSGOF  LIKE ZTLLCAMSGOF-ZFLSGOF,         "반복수.
       ZFSGOF   LIKE ZTLLCAMSGOF-ZFSGOF,          "물품매도확약서번호.
END OF IT_ZTMLOCAMR_1.

*---------------------------------------------------------------------
* SELECTION SCREEN 절.
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS    : P_REQNO   LIKE ZTLLCHD-ZFREQNO,
                   "MEMORY ID  ZPREQNO.
                   P_AMDNO   LIKE ZTLLCAMHD-ZFAMDNO.
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                                    " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.

*----------------------------------------------------------------------
* START OF SELECTION 절.
*----------------------------------------------------------------------
START-OF-SELECTION.
*  테이블 SELECT
    PERFORM   P1000_GET_DATA           USING W_ERR_CHK.

    IF W_ERR_CHK EQ 'Y'.
        MESSAGE S966.
        EXIT.
    ENDIF.

* 레포트 Write
    PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

    IF W_ERR_CHK EQ 'Y'.
        EXIT.
    ENDIF.

*&--------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&--------------------------------------------------------------------*
FORM P1000_GET_DATA USING    P_W_ERR_CHK.

     SELECT SINGLE * FROM ZTLLCAMHD          "Local L/C Amend Header.
            WHERE ZFREQNO = P_REQNO
            AND ZFAMDNO = P_AMDNO.

     IF SY-SUBRC NE 0.
         P_W_ERR_CHK = 'Y'.
     ENDIF.

     SELECT SINGLE * FROM ZTREQST             "수입의뢰 상태(Status).
            WHERE ZFREQNO = P_REQNO
            AND ZFAMDNO = P_AMDNO.

     SELECT SINGLE ZFDHJSD ZFDHSSD ZFDOCNOR   "표준 EDI Flat Head.
            INTO (W_DHJSD, W_DHSSD, W_DOCNOR) FROM ZTDHF1
            WHERE ZFDHENO = ZTREQST-ZFREQNO.

*     IF SY-SUBRC NE 0.
*         P_W_ERR_CHK = 'Y'.
*     ENDIF.
*>> 개설은행명 추출.
     SELECT SINGLE * FROM ZTLLCHD             "Local L/C Header.
            WHERE ZFREQNO = P_REQNO.

     SELECT SINGLE * FROM ZTLLCSG23           "Local L/C Seg 2 - 3.
            WHERE ZFREQNO = P_REQNO.

*>> 최종물품매도확약서번호.
     CLEAR IT_ZTMLOCAMR_1.
     REFRESH IT_ZTMLOCAMR_1.

     SELECT * FROM ZTLLCAMSGOF
              WHERE ZFREQNO = P_REQNO
              AND ZFAMDNO = P_AMDNO.

     IT_ZTMLOCAMR_1-ZFREQNO = ZTLLCAMSGOF-ZFREQNO.
     IT_ZTMLOCAMR_1-ZFLSGOF = ZTLLCAMSGOF-ZFLSGOF.  "반복수.
     IT_ZTMLOCAMR_1-ZFSGOF = ZTLLCAMSGOF-ZFSGOF.    "물품매도확약서번호.

     APPEND IT_ZTMLOCAMR_1.
     CLEAR IT_ZTMLOCAMR_1.

     ENDSELECT.

ENDFORM.                    " P1000_GET_DATA

*&--------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&--------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
    SET  TITLEBAR 'LOCAMA'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&--------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&--------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.
    SET PF-STATUS 'LOCAMA'.           " GUI STATUS SETTING
    SET  TITLEBAR 'LOCAMA'.           " GUI TITLE SETTING..
    PERFORM P3000_LINE_WRITE.
ENDFORM.                    " P3000_DATA_WRITE

*&--------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&--------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
    SKIP 2.
    WRITE:/45  '취소불능내국신용장조건변경통지서'.
    SKIP 1.
    WRITE:/4 '전자문서 번호',  26 ':',
             28 W_DOCNOR,
             88 '조건변경통지일자 : ', 107 W_DHSSD.

    ULINE AT /1(47). WRITE: '< 조건변경신청 내역 >'. ULINE AT 71(115).

*>> 개설은행.
    CLEAR W_TEM.
    W_TEM = ZTLLCHD-ZFOPBNCD.
    CONCATENATE W_TEM ZTLLCHD-ZFOBNM INTO W_TEM SEPARATED BY SPACE.
    CONCATENATE W_TEM ZTLLCHD-ZFOBBR INTO W_TEM SEPARATED BY SPACE.

    WRITE:/4 '개설은행',           26 ':', 28 W_TEM.

    WRITE:/4 '조건변경일자',       26 ':', 28 W_DHJSD.
                                              "전자문서의 전송일자.
    WRITE:/4 '개설일자',           26 ':',  28 ZTREQST-ZFOPNDT,
          /4 '내국신용장번호',     26 ':', 28 W_FOPNNO.

*>> 개설의뢰인.
    CLEAR W_TEM.
    W_TEM = ZTLLCSG23-ZFAPPNM1.

    IF NOT ZTLLCSG23-ZFAPPNM2 IS INITIAL.
        CONCATENATE  W_TEM ZTLLCSG23-ZFAPPNM2 INTO W_TEM
                 SEPARATED BY SPACE.
    ENDIF.

    IF NOT ZTLLCSG23-ZFAPPNM3 IS INITIAL.
        CONCATENATE W_TEM ZTLLCSG23-ZFAPPNM3 INTO W_TEM
                 SEPARATED BY SPACE.
    ENDIF.

    WRITE:/4 '개설의뢰인(상호, 주소, 대표자, 전화)',
          40 ':', 42 W_TEM.

*>> 수혜자.
    CLEAR W_TEM.
    W_TEM = ZTLLCAMHD-ZFBENI1.

    IF NOT ZTLLCAMHD-ZFBENI2 IS INITIAL.
        CONCATENATE W_TEM ZTLLCAMHD-ZFBENI2 INTO W_TEM SEPARATED BY
                    SPACE.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFBENI3 IS INITIAL.
        CONCATENATE W_TEM ZTLLCAMHD-ZFBENI3 INTO W_TEM SEPARATED BY
                    SPACE.
    ENDIF.

    WRITE:/4 '수  혜  자(상호, 주소, 대표자, 전화)',
          40 ':', 42 W_TEM.

* DOMAIN(변경후 내국신용장종류).------------------------------------
    CLEAR: W_DOM_TEX1.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00)
                             USING 'ZDLLCTY' ZTLLCAMHD-ZFNLLCTY
                             CHANGING   W_DOM_TEX1.
*--------------------------------------------------------------------

    WRITE:/4 '내국신용장 종류',       26 ':', 28 W_DOM_TEX1.

*>> 변경후 외화금액.
    WRITE: ZTLLCAMHD-ZFNOAMT CURRENCY ZTLLCAMHD-WAERS TO W_USD
                             LEFT-JUSTIFIED.
    CONCATENATE ZTLLCAMHD-WAERS W_USD INTO W_USD
                             SEPARATED BY SPACE.
    WRITE:/4 '변경후 외화금액', 26 ':', 28 W_USD.

*>> 변경후 원화금액.
    IF NOT ZTLLCAMHD-ZFNOPKAM IS INITIAL.
       WRITE: ZTLLCAMHD-ZFNOPKAM CURRENCY ZTLLCAMHD-ZFKRW
                                 TO W_KRW LEFT-JUSTIFIED.
       CONCATENATE ZTLLCAMHD-ZFKRW W_KRW INTO W_KRW
                                   SEPARATED BY SPACE.

       WRITE:/4 '변경후 원화금액', 26 ':', 28 W_KRW.
    ENDIF.

    WRITE:/4 '매매기준율',      26 ':', 28 ZTLLCAMHD-ZFEXRT.

    W_INX = 1.

    LOOP AT IT_ZTMLOCAMR_1.
        AT FIRST.
            WRITE:/4 '최종물품매도확약서번호', 26 ':'.
        ENDAT.

        IF W_INX = 1.
            WRITE: 28 IT_ZTMLOCAMR_1-ZFSGOF.
        ELSE.
            WRITE:/28 IT_ZTMLOCAMR_1-ZFSGOF.
        ENDIF.

        W_INX = W_INX + 1.
    ENDLOOP.

    CLEAR IT_ZTMLOCAMR_1.
    REFRESH IT_ZTMLOCAMR_1.

    WRITE:/4 '변경후 물품인도기일',   26 ':',  28 ZTLLCAMHD-ZFNGDDT,
          /4 '변경후 유효기일',       26 ':',  28 ZTLLCAMHD-ZFNEXDT,
          /4 '조건변경횟수',          26 ':',  28 ZTLLCAMHD-ZFAMDNO.

    IF NOT ZTLLCAMHD-ZFECON1 IS INITIAL
       OR NOT ZTLLCAMHD-ZFECON2 IS INITIAL
       OR NOT ZTLLCAMHD-ZFECON3 IS INITIAL
       OR NOT ZTLLCAMHD-ZFECON4 IS INITIAL
       OR NOT ZTLLCAMHD-ZFECON5 IS INITIAL.
         WRITE:/4 '기타 조건변경사항', 26 ':'.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFECON1 IS INITIAL.
        WRITE: 28 ZTLLCAMHD-ZFECON1.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFECON2 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFECON2.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFECON3 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFECON3.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFECON4 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFECON4.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFECON5 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFECON5.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFETC1 IS INITIAL
       OR NOT ZTLLCAMHD-ZFETC2 IS INITIAL
       OR NOT ZTLLCAMHD-ZFETC3 IS INITIAL
       OR NOT ZTLLCAMHD-ZFETC4 IS INITIAL
       OR NOT ZTLLCAMHD-ZFETC5 IS INITIAL.
         WRITE:/4 '기타정보', 26 ':'.
     ENDIF.

    IF NOT ZTLLCAMHD-ZFETC1 IS INITIAL.
        WRITE: 28 ZTLLCAMHD-ZFETC1.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFETC2 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFETC2.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFETC3 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFETC3.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFETC4 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFETC4.
    ENDIF.

    IF NOT ZTLLCAMHD-ZFETC5 IS INITIAL.
        WRITE:/28 ZTLLCAMHD-ZFETC5.
    ENDIF.

    SKIP 2.
    ULINE AT /1(47). WRITE: '< 발신기관 전자서명 >'.ULINE AT 71(115).
    WRITE:/4 '발신기관 전자서명', 26 ':', 28 ZTLLCSG23-ZFELENM,
                                         /28 ZTLLCSG23-ZFREPRE,
                                         /28 ZTLLCSG23-ZFELEID.

    SKIP 1.
    WRITE:/ SY-ULINE,
          / SY-VLINE,
            '1. 이 전자문서는 「무역업무 자동화촉진에',
            '관한 법률」에 의거 발행된 전자문서교환방식',
            '내국신용장조건변경통지서로서', 115 SY-VLINE,
          / SY-VLINE, '이 문서를 전송받은 개설의뢰인 또는',
            '수혜자는 동 법률 시행규정 제12조의',
             '별표2(서류제출방법에 관한 특례) 제7조에서',
             115 SY-VLINE,
          / SY-VLINE, '정한 바에 따라 신용장',
            '여백에 정당발급문서임을 표시하는 적색 고무인을',
            '날인하여야 하며 사용시 원내국신용장에', 115 SY-VLINE,
          / SY-VLINE, '첨부되어야 합니다.', 115 SY-VLINE,
           / SY-VLINE, '2. 이 신용장에 관한 사항은 다른 특별한 규정이',
             '없는 한 국제상공회의소제정 화환신용장통일규칙',
             '및 관례에 따릅니다.', 115 SY-VLINE, SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
