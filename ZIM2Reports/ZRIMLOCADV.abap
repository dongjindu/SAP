*&--------------------------------------------------------------------*
*& Report  ZRIMLOCADV                                                 *
*&--------------------------------------------------------------------*
*&  프로그램명 : 내국신용장 통지서(Local Credit Advice)               *
*&      작성자 : 이석철 INFOLINK Ltd.                                 *
*&      작성일 : 2001.07.24                                           *
*&--------------------------------------------------------------------*
*&   DESC.     :  내국신용장 통지서.                                  *
*&--------------------------------------------------------------------*
*& [변경내용]
*&
*&--------------------------------------------------------------------*
REPORT  ZRIMLOCADV   MESSAGE-ID ZIM
                     LINE-SIZE 115
                     NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------
* Tables 및 변수 Define
*----------------------------------------------------------------------
TABLES : ZTLLCHD,ZTREQHD, ZTREQST, ZTLLCSG23, ZTLLCOF, DD07T, ZTDHF1.

*----------------------------------------------------------------------
* 개설신청 내역 TABLE
*----------------------------------------------------------------------
DATA : W_ERR_CHK(1),
       W_DOM_TEX1 LIKE DD07T-DDTEXT,
       W_DOM_TEX2 LIKE DD07T-DDTEXT,
       W_FDOCNOR  LIKE ZTREQST-ZFDOCNOR.

DATA : W_INX TYPE I,
       W_USD(30),
       W_KRW(30),
       W_TEM TYPE STRING.

DATA : BEGIN  OF   IT_ZTMLOCAPP_1 OCCURS 100,      "물품매도확약서 IT
          ZFREQNO  LIKE ZTLLCOF-ZFREQNO,           "수입의뢰 관리번호
          ZFLSGOF  LIKE ZTLLCOF-ZFLSGOF,           "반복수
          ZFOFFER  LIKE ZTLLCOF-ZFOFFER,           "물품매도확약서번호
END OF IT_ZTMLOCAPP_1.

DATA: BEGIN OF IT_ZTMLOCAPP_2 OCCURS 200,       "주요구비서류 IT
      ZDOC TYPE STRING,
      ZDOCNO TYPE I,
END OF IT_ZTMLOCAPP_2.

*----------------------------------------------------------------------
* SELECTION SCREEN 절.
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS    : P_REQNO   LIKE ZTLLCHD-ZFREQNO.
                   "MEMORY ID  ZPREQNO.
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

*>>수입의뢰 상태(Status)
     CLEAR ZTREQST.
     SELECT SINGLE * FROM ZTREQST
            WHERE ZFREQNO = P_REQNO
            AND ZFAMDNO = '0'.

     IF SY-SUBRC NE 0.
         P_W_ERR_CHK = 'Y'.
     ENDIF.
     CLEAR ZTREQHD.
     SELECT SINGLE * FROM ZTREQHD
         WHERE ZFREQNO = P_REQNO.
*>>Local L/C Header
     SELECT SINGLE * FROM ZTLLCHD
            WHERE ZFREQNO = P_REQNO.

*>> 표준 EDI FLAT FILE.
     SELECT SINGLE * FROM ZTDHF1
            WHERE ZFDHENO = ZTREQST-ZFDOCNO.

*>>Local L/C Seg 2 - 3
     SELECT SINGLE * FROM ZTLLCSG23
            WHERE ZFREQNO = ZTLLCHD-ZFREQNO.

*>> LOCAL L/C 물품매도확약서.
     CLEAR IT_ZTMLOCAPP_1.
     REFRESH IT_ZTMLOCAPP_1.

     SELECT * FROM ZTLLCOF
              WHERE ZFREQNO = ZTLLCHD-ZFREQNO.

     IT_ZTMLOCAPP_1-ZFREQNO = ZTLLCOF-ZFREQNO.
                 " 수입의뢰 관리번호.
     IT_ZTMLOCAPP_1-ZFLSGOF = ZTLLCOF-ZFLSGOF.
                 " 반복수 물품매도확약서번호.
     IT_ZTMLOCAPP_1-ZFOFFER = ZTLLCOF-ZFOFFER.
                 " 물품매도확약서 번호.

     APPEND IT_ZTMLOCAPP_1.
     CLEAR IT_ZTMLOCAPP_1.
     ENDSELECT.

ENDFORM.                    " P1000_GET_DATA

*&--------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&--------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
    SET  TITLEBAR 'LOCADV'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&--------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&--------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.
    SET PF-STATUS 'LOCADV'.           " GUI STATUS SETTING
    SET  TITLEBAR 'LOCADV'.           " GUI TITLE SETTING..
    PERFORM P3000_LINE_WRITE.
ENDFORM.                    " P3000_DATA_WRITE

*&--------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&--------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
    SKIP 2.
    WRITE:/ '<별지 제8-6호 서식>',
          /45  '취소불능내국신용장'.
    SKIP 1.
    WRITE:/4 '전자문서번호 :',
             20 W_FDOCNOR,
             85 '통지일자 : ', ZTDHF1-ZFDHSSD.
    SKIP 1.
    ULINE AT /1(48). WRITE: '< 개설 내역 >'. ULINE AT 64(115).

* DOMAIN(내국신용장종류).-------------------------------------------
  CLEAR: W_DOM_TEX1,W_DOM_TEX2.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDLLCTY' ZTLLCHD-ZFLLCTY
                                       CHANGING   W_DOM_TEX1.
*--------------------------------------------------------------------
    CLEAR W_TEM.
    W_TEM = ZTLLCHD-ZFOPBNCD.
    CONCATENATE W_TEM ZTLLCHD-ZFOBNM INTO W_TEM SEPARATED BY SPACE.
    CONCATENATE W_TEM ZTLLCHD-ZFOBBR INTO W_TEM SEPARATED BY SPACE.

    WRITE:/4 '개설은행',     25 ':', 27 W_TEM.
    WRITE:/4 '개설일자',     25 ':', 27 ZTREQST-ZFOPNDT,
          /4 '신용장번호',   25 ':', 27 ZTREQST-ZFOPNNO."ZTLLCHD-ZFDCNO.

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
    WRITE:/4 '개설의뢰인 (상호, 주소, 대표자, 전화)',
          43 ':', 45 W_TEM.
*>> 수혜자.
    CLEAR W_TEM.
    W_TEM = ZTLLCSG23-ZFBENI1.

    IF NOT ZTLLCSG23-ZFBENI2 IS INITIAL.
        CONCATENATE W_TEM ZTLLCSG23-ZFBENI2 INTO W_TEM
                    SEPARATED BY SPACE.
    ENDIF.

    IF NOT ZTLLCSG23-ZFBENI3 IS INITIAL.
        CONCATENATE W_TEM ZTLLCSG23-ZFBENI3 INTO W_TEM
                    SEPARATED BY SPACE.
    ENDIF.
*>> 개설외화금액
    CLEAR W_USD.
    WRITE: ZTLLCHD-ZFOPAMT CURRENCY ZTLLCHD-ZFOPAMTC
                             TO W_USD LEFT-JUSTIFIED.
    CONCATENATE ZTLLCHD-ZFOPAMTC W_USD INTO W_USD
                SEPARATED BY SPACE.

    WRITE:/4 '수  혜  자 (상호, 주소, 대표자, 전화)',
          43 ':', 45 W_TEM,
          /4 '내국신용장 종류', 25 ':', 27 W_DOM_TEX1,
          /4 '개설외화금액',    25 ':', 27 W_USD.
*>> 개설원화금액
     CLEAR W_KRW.
     WRITE: ZTLLCHD-ZFOPKAM CURRENCY ZTLLCHD-ZFKRW
                           TO W_KRW LEFT-JUSTIFIED.
     CONCATENATE ZTLLCHD-ZFKRW W_KRW INTO W_KRW
                 SEPARATED BY SPACE.

     WRITE :/4 '개설원화금액', 25 ':', 27 W_KRW,
            /4 '매매기준율',   25 ':', 27 ZTREQHD-KURSF."ZTLLCHD-ZFEXRT.

     W_INX = 1.

     LOOP AT IT_ZTMLOCAPP_1.
        AT FIRST.
            WRITE:/4 '물품매도확약서번호', 25 ':'.
        ENDAT.

        IF W_INX = 1.
            WRITE: 27 IT_ZTMLOCAPP_1-ZFOFFER.
        ELSE.
            WRITE:/27 IT_ZTMLOCAPP_1-ZFOFFER.
        ENDIF.

        W_INX = W_INX + 1.
     ENDLOOP.

     WRITE:/4 '물품인도기일', 25 ':', 27 ZTLLCHD-ZFGDDT,
           /4 '유효기일',     25 ':', 27 ZTLLCHD-ZFEXDT.
*>> 주요구비서류.
     W_INX = 1.
     LOOP AT  IT_ZTMLOCAPP_2.
        AT FIRST.
            WRITE:/4 '주요 구비서류', 25 ':'.
        ENDAT.

        IF W_INX EQ 1.
            WRITE:27 IT_ZTMLOCAPP_2-ZDOC,
                  60 IT_ZTMLOCAPP_2-ZDOCNO,
                  71 '통'.
        ELSE.
            WRITE:/27 IT_ZTMLOCAPP_2-ZDOC,
                   60 IT_ZTMLOCAPP_2-ZDOCNO,
                   71 '통'.
        ENDIF.

        W_INX = W_INX + 1.
     ENDLOOP.

     CLEAR IT_ZTMLOCAPP_2.
     REFRESH IT_ZTMLOCAPP_2.
*>> 기타구비서류.
     IF NOT ( ZTLLCSG23-ZFEDOC1 IS INITIAL
             AND ZTLLCSG23-ZFEDOC2 IS INITIAL
             AND ZTLLCSG23-ZFEDOC3 IS INITIAL
             AND ZTLLCSG23-ZFEDOC4 IS INITIAL
             AND ZTLLCSG23-ZFEDOC5 IS INITIAL ).
      WRITE:/4 '기타 구비서류', 25 ':'.
        IF NOT ZTLLCSG23-ZFEDOC1 IS INITIAL.
               WRITE: 27 ZTLLCSG23-ZFEDOC1.
        ENDIF.
        IF NOT ZTLLCSG23-ZFEDOC2 IS INITIAL.
               WRITE:/27 ZTLLCSG23-ZFEDOC2.
        ENDIF.
        IF NOT ZTLLCSG23-ZFEDOC3 IS INITIAL.
               WRITE:/27 ZTLLCSG23-ZFEDOC3.
        ENDIF.
        IF NOT ZTLLCSG23-ZFEDOC4 IS INITIAL.
               WRITE:/27 ZTLLCSG23-ZFEDOC4.
        ENDIF.
        IF NOT ZTLLCSG23-ZFEDOC5 IS INITIAL.
               WRITE:/27 ZTLLCSG23-ZFEDOC5.
        ENDIF.
     ENDIF.

     WRITE:/ SY-ULINE,
           / SY-VLINE, '당행은 귀하(사)가 위 금액의 범위내에서 상기의',
             ' 서류를 첨부하여 당행을 지급장소로 하고 개설의뢰인을',
             ' 지급인으로 ', 115 SY-VLINE,
           / SY-VLINE, '한 물품대금전액의 일람출급환어음을 발행할',
             ' 수 있는 취소불능내국신용장을 개설합니다.',
             '당행은 이 신용장에 의하여', 115 SY-VLINE,
           / SY-VLINE, '발행되고 또한 이 신용장의 조건에',
             ' 일치하는 환어음이 당행에 제시된 때에는 이를 이의없이',
             '지급할 것을 환어음의', 115 SY-VLINE,
           / SY-VLINE, '발행인, 배서인, 기타',
             ' 정당한 소지인에게 확약합니다.', 115 SY-VLINE, SY-ULINE.

     IF NOT ( ZTLLCHD-ZFGDSC1 IS INITIAL
             AND ZTLLCHD-ZFGDSC2 IS INITIAL
             AND ZTLLCHD-ZFGDSC3 IS INITIAL
             AND ZTLLCHD-ZFGDSC4 IS INITIAL
             AND ZTLLCHD-ZFGDSC5 IS INITIAL ).
        WRITE:/4 '대표공급물품명', 25 ':'.
        IF NOT ZTLLCHD-ZFGDSC1 IS INITIAL.
               WRITE:27 ZTLLCHD-ZFGDSC1.
        ENDIF.
        IF NOT ZTLLCHD-ZFGDSC2 IS INITIAL.
               WRITE:/27 ZTLLCHD-ZFGDSC2.
        ENDIF.
        IF NOT ZTLLCHD-ZFGDSC3 IS INITIAL.
               WRITE:/27 ZTLLCHD-ZFGDSC3.
        ENDIF.
        IF NOT ZTLLCHD-ZFGDSC4 IS INITIAL.
               WRITE:/27 ZTLLCHD-ZFGDSC4.
        ENDIF.
        IF NOT ZTLLCHD-ZFGDSC5 IS INITIAL.
               WRITE:/27 ZTLLCHD-ZFGDSC5.
        ENDIF.
     ENDIF.
* DOMAIN(분할인도허용여부, 개설근거별용도)-----------------------------
    CLEAR: W_DOM_TEX1,W_DOM_TEX2.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDPRAL' ZTLLCHD-ZFPRAL
                                         CHANGING   W_DOM_TEX1.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDUSG' ZTLLCHD-ZFUSG
                                         CHANGING   W_DOM_TEX2.
*----------------------------------------------------------------------
     WRITE: /4 '분할인도 허용여부',   25 ':',  27 W_DOM_TEX1,
            /4 '서류제시기간',        25 ':',
            27 '물품수령증명서 발급일로부터',
                ZTLLCHD-ZFDPRP, '영업일 이내',
            /4 '개설근거별 용도',     25 ':',  27 W_DOM_TEX2,
            /4 '기       타',         25 ':',  27 ZTLLCHD-ZFOPCNT,
               '차 내국신용장'.

* DOMAIN(개설근거서류)---------------------------------------------
  CLEAR: W_DOM_TEX1.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDOPRED' ZTLLCHD-ZFOPRED
                                       CHANGING   W_DOM_TEX1.
*------------------------------------------------------------------
    IF NOT W_DOM_TEX1 IS INITIAL
       OR NOT ZTLLCHD-ZFDCNO IS INITIAL
       OR NOT ZTLLCHD-ZFDCAMT IS INITIAL
       OR NOT ZTLLCHD-ZFDEDT IS INITIAL
       OR NOT ZTLLCHD-ZFDEXDT IS INITIAL
       OR NOT ZTLLCHD-ZFEXPR1 IS INITIAL
       OR NOT ZTLLCHD-ZFEXPR2 IS INITIAL
       OR NOT ZTLLCHD-ZFEXPR3 IS INITIAL
       OR NOT ZTLLCHD-ZFEXAR IS INITIAL
       OR NOT ZTLLCHD-ZFISBN IS INITIAL
       OR NOT ZTLLCHD-ZFISBNB IS INITIAL
       OR NOT ZTLLCHD-ZFTOP IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM1 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM2 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM3 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM4 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM5 IS INITIAL.

        SKIP 1.
        ULINE AT /1(45).
        WRITE: '< 원수출신용장 등 내역 >'.
        ULINE AT 72(115).
        SKIP 1.
    ENDIF.

    IF NOT W_DOM_TEX1 IS INITIAL.
        WRITE:/4 '개설근거서류 종류',    25 ':', 27 W_DOM_TEX1.
    ENDIF.

    IF NOT ZTLLCHD-ZFDCNO IS INITIAL.
        WRITE:/4 '신용장(계약서)번호',   25 ':', 27 ZTLLCHD-ZFDCNO.
    ENDIF.

    SKIP 1.
    ULINE AT /1(45).
    WRITE: '< 발신기관 전자서명 >'.
    ULINE AT 69(115).
    SKIP 1.
    WRITE:/4 '발신기관 전자서명', 25 ':', 27 ZTLLCHD-ZFOBNM,
                                         /27 ZTLLCHD-ZFOBBR,
                                         /27 ZTLLCHD-ZFOPBN.

    SKIP 1.
    WRITE:/ SY-ULINE,
          / SY-VLINE, '1. 이 전자 문서는 「무역업무 자동화촉진에',
            ' 관한 법률」에 의거 발행된 전자문서교환방식',
            ' 내국신용장으로서', 115 SY-VLINE,
          / SY-VLINE, '이 문서를 전송받은 개설의뢰인 또는 수혜자는',
            '동 법률 시행규정 제12조의 별표2(서류제출방법에',
            ' 관한 특례)', 115 SY-VLINE,
          / SY-VLINE, '제7조에서 정한 바에 따라 신용장 여백에',
            ' 정당발급문서임을 표시하는 적색 고무인을 날인하여야',
            '합니다.', 115 SY-VLINE,
          / SY-VLINE, '2. 이 신용장에 관한 사항은 다른 특별한',
            '규정이 없는 한 국제상공회의소제정 화환신용장통일규칙',
            ' 및 관례에 따릅니다.', 115 SY-VLINE NO-GAP, SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
