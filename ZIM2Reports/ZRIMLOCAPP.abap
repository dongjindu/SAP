*&---------------------------------------------------------------------*
*& Report  ZRIMLOCAPP                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 취소불능내국신용장개설신청서(Local Credit Application)*
*&      작성자 : 김영광 LG-EDS                                         *
*&      작성일 : 2001.07.09                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  취소불능내국신용장개설신청서.                        *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLOCAPP   MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES : ZTLLCHD, ZTREQST, ZTLLCSG23, ZTLLCOF, DD07T.

*-----------------------------------------------------------------------
* 개설신청 내역 TABLE
*-----------------------------------------------------------------------
DATA : W_ERR_CHK(1),
       W_DOM_TEX1 LIKE DD07T-DDTEXT,
       W_DOM_TEX2 LIKE DD07T-DDTEXT,
       W_FAPPDT   LIKE ZTREQST-ZFAPPDT,
       W_FDOCNO   LIKE ZTREQST-ZFDOCNO.

DATA : W_INX TYPE I,
       W_TEM TYPE STRING.

DATA : BEGIN  OF    IT_ZTMLOCAPP_1 OCCURS 100,  "물품매도확약서 IT
       ZFREQNO  LIKE ZTLLCOF-ZFREQNO,           "수입의뢰 관리번호
       ZFLSGOF  LIKE ZTLLCOF-ZFLSGOF,           "반복수
       ZFOFFER  LIKE ZTLLCOF-ZFOFFER,           "물품매도확약서번호
END OF IT_ZTMLOCAPP_1.

DATA: BEGIN OF IT_ZTMLOCAPP_2 OCCURS 200,       "주요구비서류 IT
      ZDOC TYPE STRING,
      ZDOCNO TYPE I,
END OF IT_ZTMLOCAPP_2.

*---------------------------------------------------------------------
* SELECTION SCREEN ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS    : P_REQNO   LIKE ZTLLCHD-ZFREQNO.
                   "MEMORY ID  ZPREQNO.
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                                    " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
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

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_GET_DATA USING    P_W_ERR_CHK.
     SELECT SINGLE *
       FROM ZTLLCHD
      WHERE ZFREQNO = P_REQNO.

     IF SY-SUBRC NE 0.
         P_W_ERR_CHK = 'Y'.
     ENDIF.

     SELECT SINGLE ZFAPPDT ZFDOCNO
       INTO (W_FAPPDT, W_FDOCNO)
       FROM ZTREQST
      WHERE ZFREQNO = ZTLLCHD-ZFREQNO
        AND ZFAMDNO = 0.

     SELECT SINGLE *
       FROM ZTLLCSG23
      WHERE ZFREQNO = ZTLLCHD-ZFREQNO.

     CLEAR IT_ZTMLOCAPP_1.
     REFRESH IT_ZTMLOCAPP_1.

     SELECT *
       FROM ZTLLCOF
      WHERE ZFREQNO = ZTLLCHD-ZFREQNO.

     IT_ZTMLOCAPP_1-ZFREQNO = ZTLLCOF-ZFREQNO.
     IT_ZTMLOCAPP_1-ZFLSGOF = ZTLLCOF-ZFLSGOF.
     IT_ZTMLOCAPP_1-ZFOFFER = ZTLLCOF-ZFOFFER.

     APPEND IT_ZTMLOCAPP_1.
     CLEAR IT_ZTMLOCAPP_1.

     ENDSELECT.

ENDFORM.                    " P1000_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
    SET  TITLEBAR 'LOCAPP'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.
    SET PF-STATUS 'LOCAPP'.           " GUI STATUS SETTING
    SET  TITLEBAR 'LOCAPP'.           " GUI TITLE SETTING..
    PERFORM P3000_LINE_WRITE.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
    SKIP 2.
    WRITE:/45  TEXT-002,           "취소불능내국신용장개설신청서
          /46  TEXT-003.           "(Local Credit Application)
    SKIP 1.
    WRITE:/ SY-ULINE,
          / SY-VLINE,4 TEXT-004, 116 SY-VLINE,
          / SY-VLINE,4 TEXT-005, 116 SY-VLINE.
    WRITE:/ SY-ULINE.
    SKIP 2.
    WRITE:/4 '전자문서번호 :',
             20 W_FDOCNO,
             85 '개설신청일자 : ', W_FAPPDT.

    SKIP 2.
    ULINE AT /1(48). WRITE: '< 개설신청 내역 >'. ULINE AT 67(116).
    SKIP 1.

* DOMAIN.-----------------------------------------------------------
    CLEAR: W_DOM_TEX1,W_DOM_TEX2.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDLLCTY' ZTLLCHD-ZFLLCTY
                                    CHANGING   W_DOM_TEX1.

    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDUSG' ZTLLCHD-ZFUSG
                                    CHANGING   W_DOM_TEX2.
*--------------------------------------------------------------------


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

    WRITE:/4 '개설의뢰인',         22 ':', 23 W_TEM.


    CLEAR W_TEM.
    W_TEM = ZTLLCSG23-ZFBENI1.

    IF NOT ZTLLCSG23-ZFBENI2 IS INITIAL.
        CONCATENATE W_TEM ZTLLCSG23-ZFBENI2 INTO W_TEM SEPARATED BY
                    SPACE.
    ENDIF.

    IF NOT ZTLLCSG23-ZFBENI3 IS INITIAL.
        CONCATENATE W_TEM ZTLLCSG23-ZFBENI3 INTO W_TEM SEPARATED BY
                    SPACE.
    ENDIF.

    WRITE:/4 '수혜자',             22 ':', 23 W_TEM.

    CLEAR W_TEM.
    W_TEM = ZTLLCHD-ZFOPBNCD.
    CONCATENATE W_TEM ZTLLCHD-ZFOBNM INTO W_TEM SEPARATED BY SPACE.
    CONCATENATE W_TEM ZTLLCHD-ZFOBBR INTO W_TEM SEPARATED BY SPACE.

    WRITE:/4 '개설은행',           22 ':', 23 W_TEM.

    CLEAR W_TEM.
    W_TEM = ZTLLCHD-ZFOPAMT .
    CONCATENATE ZTLLCHD-ZFOPAMTC W_TEM INTO W_TEM SEPARATED BY SPACE.

    WRITE:/4 '내국신용장 종류',    22 ':', 23 W_DOM_TEX1,
          /4 '개설외화(원화)금액', 22 ':', 23 W_TEM,
          /4 '개설근거별 용도',    22 ':', 23 W_DOM_TEX2.

    W_INX = 1.

    LOOP AT IT_ZTMLOCAPP_1.
        AT FIRST.
            WRITE:/4 '물품매도확약서번호', 22 ':'.
        ENDAT.

        IF W_INX = 1.
            WRITE: 23 IT_ZTMLOCAPP_1-ZFOFFER.
        ELSE.
            WRITE:/23 IT_ZTMLOCAPP_1-ZFOFFER.
        ENDIF.

        W_INX = W_INX + 1.
    ENDLOOP.

    CLEAR IT_ZTMLOCAPP_1.
    REFRESH IT_ZTMLOCAPP_1.

* DOMAIN.-----------------------------------------------------------
    CLEAR: W_DOM_TEX1.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDPRAL' ZTLLCHD-ZFPRAL
                                    CHANGING   W_DOM_TEX1.
*--------------------------------------------------------------------

    WRITE:/4 '개설회차',              22 ':',  23 ZTLLCHD-ZFOPCNT,
                                               25 '차 내국신용장',
          /4 '서류제시기간',          22 ':',
                                      23 '물품수령증명서 발급일로부터',
                                      51 ZTLLCHD-ZFDPRP,
                                      53 '영업일 이내',
          /4 '물품인도기일',          22 ':',  23 ZTLLCHD-ZFGDDT,
          /4 '유효기일',              22 ':',  23 ZTLLCHD-ZFEXDT,
          /4 '분할인도 허용여부',     22 ':',  23 W_DOM_TEX1,
          /4 '대표 공급물품명',       22 ':',  23 ZTLLCHD-ZFGDSC1.

    IF NOT ZTLLCHD-ZFGDSC2 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFGDSC2.
    ENDIF.

    IF NOT ZTLLCHD-ZFGDSC3 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFGDSC3.
    ENDIF.

    IF NOT ZTLLCHD-ZFGDSC4 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFGDSC4.
    ENDIF.

    IF NOT ZTLLCHD-ZFGDSC5 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFGDSC5.
    ENDIF.

    IF ZTLLCSG23-ZFDOMYN EQ 'X'.
        IT_ZTMLOCAPP_2-ZDOC = '물품수령증명서'.
        IT_ZTMLOCAPP_2-ZDOCNO = ZTLLCSG23-ZFNODOM.

        APPEND IT_ZTMLOCAPP_2.
        CLEAR IT_ZTMLOCAPP_2.
    ENDIF.

    IF ZTLLCSG23-ZFBILYN EQ 'X'.
        IT_ZTMLOCAPP_2-ZDOC = '공급자발행 세금계산서 사본'.
        IT_ZTMLOCAPP_2-ZDOCNO = ZTLLCSG23-ZFNOBIL.

        APPEND IT_ZTMLOCAPP_2.
        CLEAR IT_ZTMLOCAPP_2.
    ENDIF.

    IF ZTLLCSG23-ZFINVYN EQ 'X'.
        IT_ZTMLOCAPP_2-ZDOC = '물품명세가 기재된 송장'.
        IT_ZTMLOCAPP_2-ZDOCNO = ZTLLCSG23-ZFNOINV.

        APPEND IT_ZTMLOCAPP_2.
        CLEAR IT_ZTMLOCAPP_2.
    ENDIF.

    IF ZTLLCSG23-ZFOFYN EQ 'X'.
        IT_ZTMLOCAPP_2-ZDOC = '공급자발행 물품매도확약서 사본'.
        IT_ZTMLOCAPP_2-ZDOCNO = ZTLLCSG23-ZFNOOF.

        APPEND IT_ZTMLOCAPP_2.
        CLEAR IT_ZTMLOCAPP_2.
    ENDIF.

    IF ZTLLCSG23-ZFLLCYN EQ 'X'.
        IT_ZTMLOCAPP_2-ZDOC = '본 내국신용장의 사본'.
        IT_ZTMLOCAPP_2-ZDOCNO = ZTLLCSG23-ZFNOLLC.

        APPEND IT_ZTMLOCAPP_2.
        CLEAR IT_ZTMLOCAPP_2.
    ENDIF.

    W_INX = 1.

    LOOP AT  IT_ZTMLOCAPP_2.
        AT FIRST.
            WRITE:/4 '주요 구비서류', 22 ':'.
        ENDAT.

        IF W_INX EQ 1.
            WRITE:23 IT_ZTMLOCAPP_2-ZDOC,
                  60 IT_ZTMLOCAPP_2-ZDOCNO,
                  71 '통'.
        ELSE.
            WRITE:/23 IT_ZTMLOCAPP_2-ZDOC,
                   60 IT_ZTMLOCAPP_2-ZDOCNO,
                   71 '통'.
        ENDIF.

        W_INX = W_INX + 1.
    ENDLOOP.

    CLEAR IT_ZTMLOCAPP_2.
    REFRESH IT_ZTMLOCAPP_2.

    IF NOT ZTLLCSG23-ZFEDOC1 IS INITIAL
       OR NOT ZTLLCSG23-ZFEDOC2 IS INITIAL
       OR NOT ZTLLCSG23-ZFEDOC3 IS INITIAL
       OR NOT ZTLLCSG23-ZFEDOC4 IS INITIAL
       OR NOT ZTLLCSG23-ZFEDOC5 IS INITIAL.
         WRITE:/4 '기타 구비서류', 22 ':'.
    ENDIF.

    IF NOT ZTLLCSG23-ZFEDOC1 IS INITIAL.
        WRITE: 23 ZTLLCSG23-ZFEDOC1.
    ENDIF.

    IF NOT ZTLLCSG23-ZFEDOC2 IS INITIAL.
        WRITE:/23 ZTLLCSG23-ZFEDOC2.
    ENDIF.

    IF NOT ZTLLCSG23-ZFEDOC3 IS INITIAL.
        WRITE:/23 ZTLLCSG23-ZFEDOC3.
    ENDIF.

    IF NOT ZTLLCSG23-ZFEDOC4 IS INITIAL.
        WRITE:/23 ZTLLCSG23-ZFEDOC4.
    ENDIF.

    IF NOT ZTLLCSG23-ZFEDOC5 IS INITIAL.
        WRITE:/23 ZTLLCSG23-ZFEDOC5.
    ENDIF.

    IF NOT ZTLLCHD-ZFETC1 IS INITIAL
       OR NOT ZTLLCHD-ZFETC2 IS INITIAL
       OR NOT ZTLLCHD-ZFETC3 IS INITIAL
       OR NOT ZTLLCHD-ZFETC4 IS INITIAL
       OR NOT ZTLLCHD-ZFETC5 IS INITIAL.
         WRITE:/4 '기타정보', 22 ':'.
     ENDIF.

    IF NOT ZTLLCHD-ZFETC1 IS INITIAL.
        WRITE: 23 ZTLLCHD-ZFETC1.
    ENDIF.

    IF NOT ZTLLCHD-ZFETC2 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFETC2.
    ENDIF.

    IF NOT ZTLLCHD-ZFETC3 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFETC3.
    ENDIF.

    IF NOT ZTLLCHD-ZFETC4 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFETC4.
    ENDIF.

    IF NOT ZTLLCHD-ZFETC5 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFETC5.
    ENDIF.

* DOMAIN.-----------------------------------------------------------
    CLEAR: W_DOM_TEX1.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDOPRED' ZTLLCHD-ZFOPRED
                                    CHANGING   W_DOM_TEX1.
*--------------------------------------------------------------------
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
        ULINE AT /1(45). WRITE: '< 원수출신용장 등 내역 >'.
        ULINE AT 71(116).
        SKIP 1.
    ENDIF.

    IF NOT W_DOM_TEX1 IS INITIAL.
        WRITE:/4 '개설근거서류 종류',    22 ':', 23 W_DOM_TEX1.
    ENDIF.

    IF NOT ZTLLCHD-ZFDCNO IS INITIAL.
        WRITE:/4 '신용장(계약서)번호',   22 ':', 23 ZTLLCHD-ZFDCNO.
    ENDIF.

    IF NOT ZTLLCHD-ZFDCAMT IS INITIAL.
        CLEAR W_TEM.
        W_TEM = ZTLLCHD-ZFDCAMT.
        CONCATENATE ZTLLCHD-WAERS W_TEM INTO W_TEM SEPARATED BY SPACE.
        WRITE:/4 '결제통화 및 금액',     22 ':', 23 W_TEM.
    ENDIF.

    IF NOT ZTLLCHD-ZFDEDT IS INITIAL.
        WRITE:/4 '선적(인도)기일',       22 ':', 23 ZTLLCHD-ZFDEDT.
    ENDIF.

    IF NOT ZTLLCHD-ZFDEXDT IS INITIAL.
        WRITE:/4 '유효기일',             22 ':', 23 ZTLLCHD-ZFDEXDT.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXPR1 IS INITIAL
       OR  NOT ZTLLCHD-ZFEXPR2 IS INITIAL
       OR  NOT ZTLLCHD-ZFEXPR3 IS INITIAL.
        WRITE:/4 '수출(공급)상대방',     22 ':'.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXPR1 IS INITIAL.
        WRITE:23 ZTLLCHD-ZFEXPR1.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXPR2 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFEXPR2.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXPR3 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFEXPR3.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXAR IS INITIAL.
        WRITE:/4 '수출지역',             22 ':', 23 ZTLLCHD-ZFEXAR.
    ENDIF.

    IF NOT ZTLLCHD-ZFISBN IS INITIAL
       OR NOT ZTLLCHD-ZFISBNB IS INITIAL.

        CLEAR W_TEM.
        W_TEM = ZTLLCHD-ZFISBN.
        CONCATENATE W_TEM ZTLLCHD-ZFISBNB INTO W_TEM SEPARATED BY SPACE.

        WRITE:/4 '발행은행',             22 ':', 23 W_TEM.
    ENDIF.

    IF NOT ZTLLCHD-ZFTOP IS INITIAL.
        WRITE:/4 '대금결제조건',         22 ':', 23 ZTLLCHD-ZFTOP.
    ENDIF.


    IF NOT ZTLLCHD-ZFEXGNM1 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM2 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM3 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM4 IS INITIAL
       OR NOT ZTLLCHD-ZFEXGNM5 IS INITIAL.
        WRITE:/4 '대표수출물품명',       22 ':'.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXGNM1 IS INITIAL.
        WRITE:23 ZTLLCHD-ZFEXGNM1.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXGNM2 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFEXGNM2.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXGNM3 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFEXGNM3.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXGNM4 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFEXGNM4.
    ENDIF.

    IF NOT ZTLLCHD-ZFEXGNM5 IS INITIAL.
        WRITE:/23 ZTLLCHD-ZFEXGNM5.
    ENDIF.

    SKIP 1.
    ULINE AT /1(47). WRITE: '< 발신기관 전자서명 >'.ULINE AT 70(116).
    SKIP 1.
    WRITE:/4 '발신기관 전자서명', 22 ':', 23 ZTLLCSG23-ZFELENM,
                                         /23 ZTLLCSG23-ZFREPRE,
                                         /23 ZTLLCSG23-ZFELEID.

    SKIP 1.
    WRITE:/ SY-ULINE,
          / SY-VLINE, 4 TEXT-006, 116 SY-VLINE,
          / SY-VLINE, 4 TEXT-007, 116 SY-VLINE.
    WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
