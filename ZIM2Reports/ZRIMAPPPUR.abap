*&---------------------------------------------------------------------*
*& Report  ZRIMAPPPUR                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI 응답서                                            *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.20                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  EDI 응답서.                                          *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMAPPPUR    MESSAGE-ID ZIM
                     LINE-SIZE 116
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables 및 변수 Define.
*-----------------------------------------------------------------------
TABLES : ZTPUR, ZTPURSG1, ZTPURSG4, ZTREQST, ZTREQHD, LFA1, DD07T.

*-----------------------------------------------------------------------
* INTERAL TABLE 및 변수.
*-----------------------------------------------------------------------

* 공급물품 명세.
DATA : IT_ZTPURSG4   LIKE ZTPURSG4  OCCURS 10 WITH HEADER LINE.
* 수출, 구매승인서상의 내용.
DATA : IT_ZTPURSG1   LIKE ZTPURSG1  OCCURS 10 WITH HEADER LINE.

DATA : W_APPAD(80),
       W_VENAD(80),
       W_TEM(20),
       W_NETPR(15),
       W_PRICE(20),
       W_DOM_TEXT      LIKE    DD07T-DDTEXT,
       W_ERR_CHK(1).

*---------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS :    P_REQNO   LIKE    ZTREQHD-ZFREQNO,
                   P_AMDNO   LIKE   ZTREQST-ZFAMDNO.
SELECTION-SCREEN END OF BLOCK B1.


* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
*    PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*  테이블 SELECT
    PERFORM   P1000_GET_IT_TAB           USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.
      MESSAGE S966.  EXIT.
    ENDIF.
* 레포트 Write
    PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'APPPUR'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER

**&---------------------------------------------------------------------
*
**&      Form  P2000_AUTHORITY_CHECK
**&---------------------------------------------------------------------
*
*FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.
*
**   W_ERR_CHK = 'N'.
**----------------------------------------------------------------------
**  해당 화면 AUTHORITY CHECK
**----------------------------------------------------------------------
**   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
**           ID 'ACTVT' FIELD '*'.
*
**   IF SY-SUBRC NE 0.
**      MESSAGE S960 WITH SY-UNAME 'B/L Doc Transaction'.
**      W_ERR_CHK = 'Y'.   EXIT.
**   ENDIF.
*
*ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET  PF-STATUS 'APPPUR'.           " GUI STATUS SETTING
  SET  TITLEBAR  'APPPUR'.           " GUI TITLE SETTING..
  PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   SKIP 2.
   WRITE: '(별지 제 4-2 호 서식)'.
   WRITE:/40 '외화획득용원료(물품)구매승인신청서'.

   SKIP 2.
*  신청인 주소.
   WRITE ZTPUR-ZFAPPAD1 TO W_APPAD LEFT-JUSTIFIED.
   CONCATENATE W_APPAD ZTPUR-ZFAPPAD2 INTO W_APPAD
              SEPARATED BY SPACE.
*  WRITE W_APPAD TO W_APPAD LEFT-JUSTIFIED.
*  CONCATENATE W_APPAD ZTPUR-ZFAPPAD3 INTO W_APPAD
*             SEPARATED BY SPACE.

*  신청인 주소.
   WRITE ZTPUR-ZFVENAD1 TO W_VENAD LEFT-JUSTIFIED.
   CONCATENATE W_VENAD ZTPUR-ZFVENAD2 INTO W_VENAD
              SEPARATED BY SPACE.
   WRITE W_VENAD TO W_VENAD LEFT-JUSTIFIED.
   CONCATENATE W_VENAD ZTPUR-ZFVENAD3 INTO W_VENAD
              SEPARATED BY SPACE.

   WRITE:/3 '(1)  신청인', 18 ZTPUR-ZFELEAD1,
         /18 W_APPAD,
         /18 ZTPUR-ZFELEAD2,
         /18 ZTPUR-ZFELEID.

   SKIP 1.
   WRITE:/3 '(2)  공급자', 18 ZTPUR-ZFVENNM1,
*        /18 W_VENAD,
         /18 ZTPUR-ZFVENNM2.

   SKIP 1.
   WRITE:/3 '<공급물품 명세>    수출물품'.
   WRITE:/3 '(3)  HS 부호/(4)  품명 및 규격/(5)  단위 및 수량',
            '/(6)  단가(US$부기)/(7)  금액(US$부기)'.
*   WRITE:/  '     ', (15)'(3)  HS 부호', (34)'/(4)  품명 및 규격',
*            (20)'/(5)  단위 및 수량', (15)'/  (6)단가(US$부기)',
*            (20)'/(7)  금액(US$부기)'.

   SKIP 1.
   WRITE:/    '     ', (15)'HS 부호', (34)'품명 및 규격',
          (20)'단위 및 수량', (15)'단가(US$부기)', (20)'금액(US$부기)'.
   LOOP AT IT_ZTPURSG1.

       CLEAR: W_TEM, W_NETPR, W_PRICE.

       WRITE IT_ZTPURSG1-MENGE TO W_TEM UNIT IT_ZTPURSG1-MEINS .
       CONCATENATE W_TEM IT_ZTPURSG1-MEINS INTO W_TEM
                   SEPARATED BY SPACE.
       WRITE W_TEM TO W_TEM RIGHT-JUSTIFIED.

       WRITE IT_ZTPURSG1-ZFNETPRU CURRENCY IT_ZTPURSG1-ZFUSD TO W_NETPR
             LEFT-JUSTIFIED.
       CONCATENATE IT_ZTPURSG1-ZFUSD W_NETPR INTO W_NETPR
                   SEPARATED BY SPACE.
*       CONCATENATE '@' IT_ZTPURSG1-WAERS ' ' W_NETPR INTO W_NETPR.
       CONCATENATE '@' W_NETPR INTO W_NETPR.
*       WRITE W_NETPR TO W_NETPR RIGHT-JUSTIFIED.

       WRITE IT_ZTPURSG1-ZFGOAMTU CURRENCY IT_ZTPURSG1-ZFUSD TO W_PRICE
             LEFT-JUSTIFIED.
       CONCATENATE IT_ZTPURSG1-ZFUSD W_PRICE INTO W_PRICE
                   SEPARATED BY SPACE.
*       WRITE W_PRICE TO W_PRICE RIGHT-JUSTIFIED.

       WRITE:/  '     ', (15)IT_ZTPURSG1-STAWN,
                (30)IT_ZTPURSG1-ZFHSDESC,
                (16)IT_ZTPURSG1-MENGE UNIT IT_ZTPURSG1-MEINS,
                                      (05)IT_ZTPURSG1-MEINS,
*                (20)W_TEM, '   ',
                 (04)'',
*                (20)IT_ZTPURSG1-NETPR, (20)IT_ZTPURSG1-ZFGOAMT.
                (15)W_NETPR, (20)W_PRICE.
   ENDLOOP.

   WRITE:/23 SY-ULINE.
   CLEAR: W_TEM, W_PRICE.

   WRITE ZTPUR-ZFTOCN TO W_TEM  UNIT ZTPUR-ZFTOCNM.
   CONCATENATE W_TEM ZTPUR-ZFTOCNM INTO W_TEM
               SEPARATED BY SPACE.
*  WRITE W_TEM  TO W_TEM RIGHT-JUSTIFIED.

   WRITE ZTPUR-ZFTOAMU CURRENCY ZTPUR-ZFUSD TO W_PRICE
         LEFT-JUSTIFIED.
   CONCATENATE ZTPUR-ZFUSD W_PRICE INTO W_PRICE
               SEPARATED BY SPACE.

   WRITE:/23 'TOTAL', 53 ZTPUR-ZFTOCN UNIT ZTPUR-ZFTOCNM,
                                      ZTPUR-ZFTOCNM,  97 W_PRICE.

   SKIP 1.
   WRITE:/3 '<수출 또는 구매승인서상의 내용>'.
   LOOP AT IT_ZTPURSG4.
      CLEAR W_TEM.
      WRITE IT_ZTPURSG4-ZFGOAMT CURRENCY IT_ZTPURSG4-WAERS
            TO W_TEM LEFT-JUSTIFIED.
      CONCATENATE IT_ZTPURSG4-WAERS W_TEM INTO W_TEM
                  SEPARATED BY SPACE.
* DOMAIN READ.
      PERFORM  GET_DD07T_SELECT USING   'ZDSDOC'
                                        IT_ZTPURSG4-ZFSDOC
                                        CHANGING   W_DOM_TEXT.
        WRITE W_DOM_TEXT TO W_DOM_TEXT LEFT-JUSTIFIED.
        CONCATENATE W_DOM_TEXT IT_ZTPURSG4-ZFSDNO INTO W_DOM_TEXT
                    SEPARATED BY '     '.

        WRITE:/3 '(8)  근거서류명 및 번호 :', W_DOM_TEXT,
              /3 '(9)  HS 부호            :', IT_ZTPURSG4-STAWN,
              /3 '(10) 품명               :', IT_ZTPURSG4-ZFGODS1,
              /3 '(11) 금액               :', W_TEM,
              /3 '(12) 유효기일           :', IT_ZTPURSG4-ZFEXDT,
              /3 '(13) 선적기일           :', IT_ZTPURSG4-ZFSPDT,
              /3 SY-ULINE.
   ENDLOOP.
   CLEAR W_TEM.
   WRITE ZTPUR-ZFACNM TO W_TEM LEFT-JUSTIFIED.
   CONCATENATE W_TEM ZTPUR-ZFACBR INTO W_TEM
               SEPARATED BY SPACE.
   WRITE:/3 '(14) 승인조건           :',
         /3 '(15) 승인번호           :', ZTREQST-ZFOPNNO,
         /,
         /5 '위 신청사항을 대외무역관리규정 제 4-2-7조의 규정에 의하여',
            '승인신청 합니다.',
         /8 '승인 신청일 :', ZTREQST-ZFAPPDT,
         /8 '승 인 은 행 :', W_TEM,
         /8 '             ', ZTPUR-ZFACBNCD.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING W_ERR_CHK.

   W_ERR_CHK = 'N'.

   CLEAR : ZTPUR, ZTREQST, LFA1, IT_ZTPURSG1, IT_ZTPURSG4.

   SELECT SINGLE * FROM ZTREQST
          WHERE ZFREQNO EQ P_REQNO
            AND ZFAMDNO EQ P_AMDNO.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.

   SELECT SINGLE * FROM ZTPUR
          WHERE ZFREQNO EQ P_REQNO
            AND ZFAMDNO EQ P_AMDNO.

   SELECT SINGLE * FROM LFA1 WHERE LIFNR = ZTPUR-ZFACBN.

   SELECT * INTO TABLE IT_ZTPURSG1 FROM ZTPURSG1
          WHERE ZFREQNO    EQ  P_REQNO
            AND ZFAMDNO    EQ   P_AMDNO.

   SELECT * INTO TABLE IT_ZTPURSG4 FROM ZTPURSG4
          WHERE ZFREQNO    EQ   P_REQNO
            AND ZFAMDNO    EQ   P_AMDNO.

ENDFORM.                    " P1160_GET_IT_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T_SELECT USING    P_DOMNAME
                               P_FIELD
                      CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.
  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

*   TRANSLATE DD07T-DDTEXT TO UPPER CASE.
  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
