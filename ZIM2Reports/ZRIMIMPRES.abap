*&---------------------------------------------------------------------*
*& Report  ZRIMIMPRES                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입신고필증                                          *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.04                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMIMPRES   MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입신고 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
TABLES: ZTIDS,ZTIDSHS,ZTIDSHSD,ZTBL,
        ZTIDSHSL,T005T.


DATA:  W_ERR_CHK(1) TYPE C VALUE 'N',
       W_PAGE       TYPE I,
       W_LINE       TYPE I,
       W_CONO       TYPE I,
       W_FONO       TYPE I,
       W_CNNO       TYPE I,
       W_TCONO      TYPE I,
       W_TFONO      TYPE I,
       W_TCNNO      TYPE I,
       W_NEED_LINE  TYPE I,
       W_DOM_TEX1   LIKE DD07T-DDTEXT,
       W_ZFPKCNT(8) TYPE I,
       W_DOM_TEX2   LIKE DD07T-DDTEXT.


DATA:   BEGIN OF IT_IDSHS OCCURS 0.   ">> 란 내역.
        INCLUDE STRUCTURE   ZTIDSHS.
DATA:   END   OF IT_IDSHS.

DATA:   BEGIN OF IT_IDSHSD OCCURS 0.   ">> 행 내역.
        INCLUDE STRUCTURE   ZTIDSHSD.
DATA:   END   OF IT_IDSHSD.

DATA:   BEGIN OF IT_IDSHSL OCCURS 0.   ">> 요건내역.
        INCLUDE STRUCTURE   ZTIDSHSL.
DATA:   END   OF IT_IDSHSL.


*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
         PARAMETERS: P_BLNO  LIKE  ZTIDS-ZFBLNO,
                     P_CLSEQ LIKE  ZTIDS-ZFCLSEQ.

SELECTION-SCREEN END OF BLOCK B1.
* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*  테이블 SELECT
   PERFORM   P1000_GET_ZTIDS      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'IMPRES'.          " TITLE BAR
*  W_HT  = 11.
*  W_HB  = 17.
*  W_IB1 = 22.
*  W_IB2 = 22.
*  W_BT  = 4.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  IF W_PAGE EQ 1.
     WRITE : /45  '수  입  신  고 (신고필증) '.
     WRITE : / 'File No :',ZTIDS-ZFREBELN,100 'Page: ', W_PAGE.
  ELSE.
     SKIP 1.
     WRITE:/100 'Page: ', W_PAGE.
  ENDIF.
  WRITE : / SY-ULINE.

  WRITE : / SY-VLINE NO-GAP,'신고번호',
         40 SY-VLINE NO-GAP,'신고일',
         60 SY-VLINE NO-GAP,'세관.과',
         80 SY-VLINE NO-GAP,'입항일',
        100 SY-VLINE NO-GAP,'*처리기간:3일',
        120 SY-VLINE NO-GAP.
  WRITE : / SY-VLINE NO-GAP,(4)SPACE,ZTIDS-ZFIDRNO,
         40 SY-VLINE NO-GAP,(4)SPACE,ZTIDS-ZFIDSDT,
         60 SY-VLINE,(4)SPACE,
         ZTIDS-ZFINRC NO-GAP,'-' NO-GAP,ZTIDS-ZFINRCD,
         80 SY-VLINE NO-GAP,(4)SPACE,ZTIDS-ZFENDT,
        100 SY-VLINE NO-GAP,
        120 SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOCD' ZTIDS-ZFCOCD
                                   CHANGING   W_DOM_TEX1.

  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCUPR' ZTIDS-ZFCUPR
                                   CHANGING   W_DOM_TEX2.
*--------------------------------------------------------------------

  WRITE : / SY-VLINE NO-GAP,'B/L(AWB)번호',
         40 SY-VLINE NO-GAP,'화물관리번호',
         80 SY-VLINE NO-GAP,'반입일',
         100 SY-VLINE NO-GAP,'징수 형태',ZTIDS-ZFCOCD,
         120 SY-VLINE NO-GAP.
  WRITE : / SY-VLINE NO-GAP,(4)SPACE,ZTIDS-ZFHBLNO,
         40 SY-VLINE NO-GAP,(4)SPACE,ZTIDS-ZFGOMNO,
         80 SY-VLINE NO-GAP,(4)SPACE,ZTIDS-ZFINDT,
        100 SY-VLINE NO-GAP,(20)W_DOM_TEX1,  " DOMAIN.
        120 SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
           ID 'ACTVT' FIELD '*'.

  IF SY-SUBRC NE 0.
      MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
      W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTIDS
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTIDS   USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.
   SELECT SINGLE *
      FROM  ZTBL
      WHERE ZFBLNO = P_BLNO.

   SELECT  SINGLE *
       FROM ZTIDS
       WHERE ZFBLNO   = P_BLNO
         AND ZFCLSEQ  = P_CLSEQ.
   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDSHS
     FROM ZTIDSHS
     WHERE ZFBLNO   = P_BLNO
       AND ZFCLSEQ  = P_CLSEQ.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDSHSD
       FROM ZTIDSHSD
     WHERE ZFBLNO   = P_BLNO
       AND ZFCLSEQ  = P_CLSEQ.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_IDSHSL
      FROM ZTIDSHSL
      WHERE ZFBLNO  = P_BLNO
        AND ZFCLSEQ = P_CLSEQ.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'IMPRES'.           " GUI STATUS SETTING
   SET  TITLEBAR 'IMPRES'.           " GUI TITLE SETTING..

   CLEAR : W_TCONO, W_TFONO, W_TCNNO, W_NEED_LINE, W_LINE.

   W_PAGE = 1.     W_LINE = 0.
   W_CONO = 0. " 총란수.
   DESCRIBE TABLE IT_IDSHS LINES W_CONO.
*   DESCRIBE TABLE IT_IDSHSD LINES W_FONO.
*   DESCRIBE TABLE IT_IDSHSL LINES W_CNNO.
*   W_TCONO = W_CONO * 22.
*   W_TFONO = W_FONO * 2.
*   W_TCNNO = W_CNNO * 3.
*   W_LINE =   W_TCONO +  W_TFONO +  W_TCNNO.

   PERFORM P3000_HEAD_WRITE.

   LOOP AT IT_IDSHS.
*     PERFORM P2000_PAGE_CHECK.
     IF SY-TABIX EQ 1.
        W_LINE = 53.
     ENDIF.
*>> 필요한 LINE 수 구하기.
     SELECT COUNT( * ) INTO  W_TFONO
     FROM   ZTIDSHSD
     WHERE  ZFBLNO  EQ  IT_IDSHS-ZFBLNO
     AND    ZFCLSEQ EQ  IT_IDSHS-ZFCLSEQ
     AND    ZFCONO  EQ  IT_IDSHS-ZFCONO.

     SELECT COUNT( * ) INTO W_TCNNO
     FROM   ZTIDSHSL
     WHERE  ZFBLNO  EQ  IT_IDSHS-ZFBLNO
     AND    ZFCLSEQ EQ  IT_IDSHS-ZFCLSEQ
     AND    ZFCONO  EQ  IT_IDSHS-ZFCONO.

     W_NEED_LINE  =  21 + W_TFONO + ( W_TCNNO * 3 ).
     IF W_LINE >= W_NEED_LINE.
        PERFORM P3000_ITEM_WRITE.
        W_LINE = W_LINE - W_NEED_LINE.
     ELSE.
        SKIP W_LINE.
        PERFORM P3000_END_LINE_WRITE.
        W_LINE = 70.
        W_PAGE = W_PAGE + 1.
        NEW-PAGE.
        PERFORM P3000_ITEM_WRITE.
        W_LINE = W_LINE - W_NEED_LINE.
     ENDIF.
      AT LAST.
        IF W_LINE < 22.
           W_PAGE = W_PAGE + 1.
           W_LINE = 70.
           NEW-PAGE.
         ENDIF.
         PERFORM P3000_LAST_WRITE.
         W_LINE = W_LINE - 22.
         SKIP W_LINE.
         PERFORM P3000_END_LINE_WRITE.
     ENDAT.
   ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  PERFORM P3000_ITEM_WRITE.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_HSD_DATE
*&---------------------------------------------------------------------*
FORM P3000_HSD_DATE.

  WRITE:/   SY-VLINE,(4)SPACE,IT_IDSHSD-ZFGDDS1,
         40 SY-VLINE,(4)SPACE,IT_IDSHSD-ZFGDIN1,
         60 SY-VLINE NO-GAP,IT_IDSHSD-ZFQNT
         UNIT IT_IDSHSD-ZFQNTM NO-GAP,IT_IDSHSD-ZFQNTM,
         80 SY-VLINE,(1)SPACE,IT_IDSHSD-NETPR CURRENCY IT_IDSHSD-ZFCUR,
        100 SY-VLINE NO-GAP,IT_IDSHSD-ZFAMT CURRENCY IT_IDSHSD-ZFCUR,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

ENDFORM.                    " P3000_HSD_DATE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  DATA: W_TEXT1(30) TYPE C, "인도조건-통화종류-금액-결제방법.
        W_ZFSTAMT(16),      "결제금액.
        W_ZFAAMT   LIKE ZTIDS-ZFVAAMTS. " 부가가치세과표.

  WRITE: ZTIDS-ZFSTAMT CURRENCY ZTIDS-ZFSTAMC TO W_ZFSTAMT.
  SHIFT W_ZFSTAMT LEFT  DELETING LEADING  SPACE.
  CONCATENATE ZTIDS-INCO1 '-' ZTIDS-ZFSTAMC '-' W_ZFSTAMT '-'
              ZTIDS-ZFAMCD INTO W_TEXT1.

  W_ZFAAMT = ZTIDS-ZFVAAMTS * 10.
  WRITE:/ SY-VLINE NO-GAP,'결제금액',
                          '(인도조건-통화종류-금액-결제방법)',
       50 SY-VLINE,W_TEXT1,
       90 SY-VLINE NO-GAP,'환율',
      100 SY-VLINE NO-GAP,(5)SPACE,ZTIDS-ZFEXRT,
      120 SY-VLINE.
  WRITE:/ SY-ULINE.
*>>>총과세.
  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,'$'NO-GAP,ZTIDS-ZFTBAU CURRENCY ZTIDS-ZFUSD,
       35 SY-VLINE NO-GAP,'운임',
       42 SY-VLINE NO-GAP,  ZTIDS-ZFTFB CURRENCY ZTIDS-ZFTFBC,
       62 SY-VLINE NO-GAP,'가산금액',
       72 SY-VLINE NO-GAP, ZTIDS-ZFADAM CURRENCY ZTIDS-ZFADAMCU,
       92 SY-VLINE NO-GAP,'납부No' NO-GAP,
      100 SY-VLINE NO-GAP,ZTIDS-ZFRFFNO NO-GAP,
      120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP,'총과세가격',
       14 SY-VLINE NO-GAP,
       14 SY-ULINE,
       35 SY-VLINE NO-GAP,
       42 SY-VLINE NO-GAP,
       62 SY-VLINE NO-GAP,
       72 SY-VLINE NO-GAP,
       92 SY-VLINE NO-GAP,
      100 SY-VLINE NO-GAP,
      120 SY-VLINE.

  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,'￦'NO-GAP,ZTIDS-ZFTBAK CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,'보험료',
       42 SY-VLINE NO-GAP, ZTIDS-ZFINAMT CURRENCY ZTIDS-ZFINAMTC,
       62 SY-VLINE NO-GAP,'공제금액',
       72 SY-VLINE NO-GAP,ZTIDS-ZFDUAM CURRENCY ZTIDS-ZFDUAMCU,
       92 SY-VLINE NO-GAP,'부-과표',
      100 SY-VLINE NO-GAP,W_ZFAAMT CURRENCY ZTIDS-ZFKRW,
      120 SY-VLINE.
  WRITE:/ SY-ULINE.
*>> 세종.
  WRITE:/ SY-VLINE NO-GAP,'세종',
       14 SY-VLINE NO-GAP,'세액',
       35 SY-VLINE NO-GAP,'※관세사기재란',
       75 SY-VLINE NO-GAP,'※세관기재란',
      120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  ULINE AT 1(35).
  WRITE:/ SY-VLINE NO-GAP,'관    세'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFCUAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,(40)ZTIDS-ZFCTW1,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  ULINE AT 1(35).
  WRITE:/ SY-VLINE NO-GAP,'특 소 세'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFSCAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,(40)ZTIDS-ZFCTW2,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.

  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  ULINE AT 1(35).
  WRITE:/ SY-VLINE NO-GAP,'교 통 세'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFTRAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,(40)ZTIDS-ZFCTW3,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.

  WRITE:/ SY-VLINE NO-GAP,'주     세'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFDRAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,(40)ZTIDS-ZFCTW4,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  ULINE AT 1(35).
  WRITE:/ SY-VLINE NO-GAP,'교 육 세' CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFTRAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,(40)ZTIDS-ZFCTW5,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  ULINE AT 1(35).
  WRITE:/ SY-VLINE NO-GAP,'부 가 세'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFVAAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  ULINE AT 1(35).
  WRITE:/ SY-VLINE NO-GAP,'신고지연가산세'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFIDAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,
       75 SY-VLINE NO-GAP,
      120 SY-VLINE.
  WRITE:SY-ULINE.

*  SELECT SINGLE *
*         FROM USR03
*         WHERE BNAME = ZTIDS-UNAM.

  WRITE:/ SY-VLINE NO-GAP,'총세액합계'CENTERED,
       14 SY-VLINE NO-GAP,ZTIDS-ZFTXAMTS CURRENCY ZTIDS-ZFKRW,
       35 SY-VLINE NO-GAP,'담당자'CENTERED,
       50 SY-VLINE NO-GAP,"(10)USR03-NAME1,     " 누구지? 일단변경인.
       65 SY-VLINE NO-GAP,'접수일시'CENTERED,
       75 SY-VLINE NO-GAP,
       95 SY-VLINE NO-GAP,'수리일자'CENTERED,
      105 SY-VLINE NO-GAP, ZTIDS-ZFIDSDT,
      120 SY-VLINE.
  WRITE:SY-ULINE.


ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_HSL_DATE
*&---------------------------------------------------------------------*
FORM P3000_HSL_DATE.

  WRITE:/ SY-VLINE NO-GAP,
       20 SY-VLINE,IT_IDSHSL-ZFCNDC,  " 요건확인 구분.
       45 SY-VLINE NO-GAP,
       70 SY-VLINE,
       95 SY-VLINE NO-GAP,
      120 SY-VLINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCNDC' IT_IDSHSL-ZFCNDC
                                 CHANGING   W_DOM_TEX1.

  WRITE:/ SY-VLINE ,6'(발급서류명)',
       20 SY-VLINE, (20)W_DOM_TEX1,
       45 SY-VLINE NO-GAP,
       70 SY-VLINE,
       95 SY-VLINE NO-GAP,
      120 SY-VLINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:/ SY-VLINE NO-GAP,
       20 SY-VLINE,IT_IDSHSL-ZFCNNO, " 요건번호.
       45 SY-VLINE NO-GAP,
       70 SY-VLINE,
       95 SY-VLINE NO-GAP,
      120 SY-VLINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

ENDFORM.                    " P3000_HSL_DATE
*&---------------------------------------------------------------------*
*&      Form  P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
FORM P3000_HEAD_WRITE.

  WRITE : / SY-VLINE NO-GAP,'신  고  자:',ZTIDS-ZFAPNM,
         60 SY-VLINE NO-GAP,'통관계획',ZTIDS-ZFCUPR,
         80 SY-VLINE NO-GAP,'원산지증명서',
        100 SY-VLINE NO-GAP,'총  중  량',ZTIDS-ZFTOWTM,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,
         60 SY-VLINE NO-GAP,(15)W_DOM_TEX2, " D0MAIN.
         80 SY-VLINE NO-GAP,
        100 SY-VLINE NO-GAP,(1)SPACE,ZTIDS-ZFTOWT
                     UNIT ZTIDS-ZFTOWTM NO-GAP,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,'수  입  자:',ZTIDS-ZFIAPNM,
         60 SY-ULINE,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDIDRCD' ZTIDS-ZFIDRCD
                                   CHANGING   W_DOM_TEX1.
  MOVE ZTIDS-ZFPKCNT TO W_ZFPKCNT.
  WRITE : / SY-VLINE NO-GAP,
         60 SY-VLINE NO-GAP,'신고구분',ZTIDS-ZFIDRCD,
         80 SY-VLINE NO-GAP,'가격신고서',
        100 SY-VLINE NO-GAP,'총포장갯수',ZTIDS-ZFPKNM,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,'납세의무자:',ZTIDS-ZFTDNM1,
         60 SY-VLINE NO-GAP,(20)W_DOM_TEX1,    " DOMAIN.
         80 SY-VLINE NO-GAP,
        100 SY-VLINE,(6)SPACE,
        W_ZFPKCNT UNIT ZTIDS-ZFPKNM,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,6'(주소):',ZTIDS-ZFTDAD1,ZTIDS-ZFTDAD2,
          60 SY-ULINE,
         120 SY-VLINE NO-GAP.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZEAPRTC' ZTIDS-ZFAPRTC
                                   CHANGING   W_DOM_TEX1.

  WRITE : / SY-VLINE NO-GAP,6'(상호):',ZTIDS-ZFTDNM1,
         60 SY-VLINE NO-GAP,'거래구분',
         80 SY-VLINE NO-GAP,'국내도착항',ZTIDS-ZFAPRTC,
        100 SY-VLINE NO-GAP,'운송 형태',ZTIDS-ZFTRMET,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,6'(성명):',ZTIDS-ZFTDNM2,
         60 SY-VLINE NO-GAP, (4)SPACE,ZTIDS-ZFPONC,
         80 SY-VLINE NO-GAP, (20)W_DOM_TEX1,
        100 SY-VLINE NO-GAP," DOMAIN
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,
         60 SY-ULINE,120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  SELECT SINGLE *
         FROM T005T
         WHERE LAND1 = ZTIDS-ZFSCON.

  WRITE : / SY-VLINE NO-GAP,'무역대리점:',ZTIDS-ZFTRDNM,
         60 SY-VLINE NO-GAP,'종류',ZTIDS-ZFITKD,  " 도메인.
         80 SY-VLINE NO-GAP,'적출국',ZTIDS-ZFSCON,T005T-LANDX,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,(4)SPACE,
         60 SY-VLINE NO-GAP,(4)SPACE,  " 도메인.
         80 SY-ULINE,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDITKD' ZTIDS-ZFITKD
                                   CHANGING   W_DOM_TEX1.

  WRITE : / SY-VLINE NO-GAP,'공  급  자:',ZTIDS-ZFSUPNM,
         60 SY-VLINE NO-GAP, W_DOM_TEX1,
         80 SY-VLINE NO-GAP,'선기명',(20)ZTIDS-ZFCARNM,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,
         60 SY-ULINE,
         120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,
         60 SY-VLINE NO-GAP,'MASTER B/L 번호',(20)ZTBL-ZFMBLNO,
        100 SY-VLINE NO-GAP,'운수기관부호',
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-ULINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE : / SY-VLINE NO-GAP,'검사(반입)장소',ZTIDS-ZFISPL,
         60 SY-VLINE NO-GAP,(4)SPACE,ZTBL-ZFMBLNO,
        100 SY-VLINE NO-GAP,(4)SPACE,ZTBL-ZFTRCK,
        120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:   SY-ULINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

ENDFORM.                    " P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_END_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_END_LINE_WRITE.

  WRITE:/ '*수입신고필증의 진위 여부는 수출입통관정보시스템(KCIS)에',
  '조회하여 확인하시기 바랍니다.(http://kcis.ktnet.co.kr)',
  /'*본 수입신고필증은 세관에서 형식적인 요건만을 심사한 것이므로'
    ,'신고내용이 사실과 다른 때에는 신고인 또는 수입화주가'
   ,/'책임을 져야합니다.'.
  SKIP 4.    " 다음 페이지로 넘어갈때 적용되는 것임 절대 지우지 말것.

ENDFORM.                    " P3000_END_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_ITEM_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ITEM_WRITE.

  DATA: W_ZFCONO TYPE I.
  MOVE IT_IDSHS-ZFCONO TO W_ZFCONO.

  WRITE:/ SY-VLINE NO-GAP,'●품명.규격  (란번호/총란수:'NO-GAP,
  W_ZFCONO NO-GAP,'/',(2)W_CONO,')',
  120 SY-VLINE NO-GAP.
*  PERFORM P2000_PAGE_CHECK.
*  ADD 1 TO W_LINE.
*  PERFORM P2000_PAGE_CHECK.

  WRITE:/ SY-ULINE.
*  ADD 1 TO W_LINE.
*  PERFORM P2000_PAGE_CHECK.
*
  WRITE:/ SY-VLINE NO-GAP,'품    명:    ',IT_IDSHS-ZFGDNM,
       70 SY-VLINE NO-GAP,'상표',
      120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.
*  PERFORM P2000_PAGE_CHECK.

  WRITE:/ SY-VLINE NO-GAP,'거래품명:',IT_IDSHS-ZFTGDNM,
       70 SY-VLINE NO-GAP,IT_IDSHS-ZFGCNM,
      120 SY-VLINE.
*  ADD 1 TO W_LINE.
*  PERFORM P2000_PAGE_CHECK.
  WRITE:  SY-ULINE.
*  ADD 1 TO W_LINE.
*  PERFORM P2000_PAGE_CHECK.
*
*>> 행사항이 없을 경우 타이틀 생략.
   WRITE:/ SY-VLINE NO-GAP,'모델.규격',
        40 SY-VLINE NO-GAP,'성분',
        60 SY-VLINE NO-GAP,'수량',
        80 SY-VLINE NO-GAP,'단가',IT_IDSHSD-ZFCUR,
        100 SY-VLINE NO-GAP,'금액',IT_IDSHSD-ZFCUR,
        120 SY-VLINE NO-GAP.
*      ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
      WRITE: SY-ULINE.
*      ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  LOOP AT IT_IDSHSD WHERE ZFBLNO  = IT_IDSHS-ZFBLNO
                      AND ZFCLSEQ = IT_IDSHS-ZFCLSEQ
                      AND ZFCONO  = IT_IDSHS-ZFCONO.

       PERFORM P3000_HSD_DATE.
  ENDLOOP.
  WRITE: SY-ULINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:/ SY-VLINE NO-GAP,'세번부호',
       14 SY-VLINE NO-GAP,IT_IDSHS-STAWN,
       36 SY-VLINE NO-GAP,'순중량',
       45 SY-VLINE NO-GAP,(3)SPACE, IT_IDSHS-ZFWET
        UNIT IT_IDSHS-ZFWETM NO-GAP,IT_IDSHS-ZFWETM,
       70 SY-VLINE NO-GAP,'C/S검사',
       85 SY-VLINE NO-GAP,'S 청CS검사생략',
      105 SY-VLINE NO-GAP,'사후기관',
      120 SY-VLINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  ULINE AT /1(105).
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE: 120 SY-VLINE.
  WRITE:/ SY-VLINE,
       14 SY-VLINE NO-GAP,'$',IT_IDSHS-ZFTBAU CURRENCY IT_IDSHS-ZFUSD
                              RIGHT-JUSTIFIED,
       36 SY-VLINE NO-GAP,'수량',
       45 SY-VLINE NO-GAP,(3)SPACE,IT_IDSHS-ZFQNT UNIT
                   IT_IDSHS-ZFQNTM NO-GAP,IT_IDSHS-ZFQNTM,
       70 SY-VLINE NO-GAP,'검사변경',
       85 SY-VLINE,
      105 SY-VLINE NO-GAP,IT_IDSHS-ZFMOR1,
      120 SY-VLINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:/ SY-VLINE NO-GAP,'과세가격',
       14 SY-ULINE,
       14 SY-VLINE,
       36 SY-VLINE NO-GAP,
       45 SY-VLINE NO-GAP,
       70 SY-VLINE NO-GAP,
       85 SY-VLINE NO-GAP,
      120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:/ SY-VLINE NO-GAP,
       14 SY-VLINE NO-GAP,'￦',IT_IDSHS-ZFTBAK CURRENCY IT_IDSHS-ZFKRW,
       36 SY-VLINE NO-GAP,'환급물량',
       45 SY-VLINE NO-GAP,(3)SPACE,IT_IDSHS-ZFREQN
                   UNIT IT_IDSHS-ZFREQNM NO-GAP,
                   IT_IDSHS-ZFREQNM,
       70 SY-VLINE NO-GAP,'원산지표시',
       85 SY-VLINE NO-GAP,IT_IDSHS-ZFORIG NO-GAP,'-'NO-GAP,
                   IT_IDSHS-ZFORYN NO-GAP,'-'NO-GAP,
                   IT_IDSHS-ZFORME NO-GAP,'-'NO-GAP,(1)IT_IDSHS-ZFORTY,
       95 SY-VLINE NO-GAP,'특수세액',(6)SPACE,IT_IDSHS-ZFSCCS
                                     CURRENCY IT_IDSHS-ZFKRW,
      120 SY-VLINE NO-GAP.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:/ SY-ULINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  WRITE:/ SY-VLINE NO-GAP,'수입요건확인',
       20 SY-VLINE NO-GAP,
       45 SY-VLINE NO-GAP,
       70 SY-VLINE NO-GAP,
       95 SY-VLINE NO-GAP,
      120 SY-VLINE NO-GAP.

*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  LOOP AT IT_IDSHSL WHERE ZFBLNO  = IT_IDSHS-ZFBLNO
                      AND ZFCLSEQ = IT_IDSHS-ZFCLSEQ
                      AND ZFCONO  = IT_IDSHS-ZFCONO.

       PERFORM P3000_HSL_DATE.
  ENDLOOP.
  WRITE:/ SY-ULINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

*>>>> 세종.
*  IF NOT IT_IDSHS-ZFCUAMT IS INITIAL
*   OR NOT IT_IDSHS-ZFVAAMT IS INITIAL
*   OR NOT IT_IDSHS-ZFHMAMT IS INITIAL
*   OR NOT IT_IDSHS-ZFEDAMT IS INITIAL. "무조건관세와부가세는 출력.
      WRITE:/ SY-VLINE NO-GAP,'세종',
           10 SY-VLINE NO-GAP,'세율(구분)',
           30 SY-VLINE NO-GAP,'감면율',
           40 SY-VLINE NO-GAP,'세액',
           60 SY-VLINE NO-GAP,'감면분납부호',
           80 SY-VLINE NO-GAP,'감면액',
          100 SY-VLINE NO-GAP,'*내국세종부호',
          120 SY-VLINE.
*      ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

      WRITE:/ SY-ULINE.
*      ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

*  ENDIF.
*> 관세.
*  IF NOT IT_IDSHS-ZFCUAMT IS INITIAL.
     WRITE:/ SY-VLINE NO-GAP,'관세',
          10 SY-VLINE NO-GAP,(6)SPACE,IT_IDSHS-ZFTXCD,
          30 SY-VLINE NO-GAP,IT_IDSHS-ZFRDRT,
          40 SY-VLINE NO-GAP,IT_IDSHS-ZFCUAMT CURRENCY IT_IDSHS-ZFKRW,
          60 SY-VLINE NO-GAP,IT_IDSHS-ZFCDPNO,
          80 SY-VLINE NO-GAP,IT_IDSHS-ZFCCAMT CURRENCY IT_IDSHS-ZFKRW,
         100 SY-VLINE NO-GAP,
         120 SY-VLINE.
*     ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

*  ENDIF.
*>부가세.
*  IF NOT IT_IDSHS-ZFVAAMT IS INITIAL.
     WRITE:/ SY-VLINE NO-GAP,'부가세',
          10 SY-VLINE NO-GAP,' 10.00', IT_IDSHS-ZFVTXCD, " 부가세구분.
          30 SY-VLINE NO-GAP,                  " 부가세감면율.
          40 SY-VLINE NO-GAP,IT_IDSHS-ZFVAAMT
             CURRENCY IT_IDSHS-ZFKRW,          " 부가세.
          60 SY-VLINE NO-GAP,                  " 분납부호.
          80 SY-VLINE NO-GAP,IT_IDSHS-ZFVCAMT
                    CURRENCY IT_IDSHS-ZFKRW,   " 감면액.
         100 SY-VLINE NO-GAP,
         120 SY-VLINE.
*     ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

*  ENDIF.
*>내국세
  IF NOT IT_IDSHS-ZFHMAMT IS INITIAL.
     WRITE:/ SY-VLINE NO-GAP,'내국세',
       10 SY-VLINE NO-GAP,IT_IDSHS-ZFHMTRT, " 세율.
                          IT_IDSHS-ZFHMTCD, " 세구분.
       30 SY-VLINE NO-GAP,                  " 감면율.
       40 SY-VLINE NO-GAP,IT_IDSHS-ZFHMAMT
          CURRENCY IT_IDSHS-ZFKRW,          " 세.
       60 SY-VLINE NO-GAP,                  " 분납부호.
       80 SY-VLINE NO-GAP,IT_IDSHS-ZFHCAMT
          CURRENCY IT_IDSHS-ZFKRW,          " 감면액.
      100 SY-VLINE NO-GAP,IT_IDSHS-ZFHMTTY, " 세종구분.
      120 SY-VLINE.
*     ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  ENDIF.

*>교육세.
  IF NOT IT_IDSHS-ZFEDAMT IS INITIAL.
     WRITE:/ SY-VLINE NO-GAP,'교육세',
          10 SY-VLINE NO-GAP,(6)SPACE, IT_IDSHS-ZFETXCD, " 세구분.
          30 SY-VLINE NO-GAP,                  " 감면율.
          40 SY-VLINE NO-GAP,IT_IDSHS-ZFEDAMT
             CURRENCY IT_IDSHS-ZFKRW,          " 세.
          60 SY-VLINE NO-GAP,                  " 분납부호.
          80 SY-VLINE NO-GAP,IT_IDSHS-ZFECAMT
             CURRENCY IT_IDSHS-ZFKRW,         " 감면액.
         100 SY-VLINE NO-GAP,
         120 SY-VLINE.
*    ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.
  ENDIF.
*>농특.
  IF NOT IT_IDSHS-ZFAGAMT IS INITIAL.
     WRITE:/ SY-VLINE NO-GAP,'농특세',
          10 SY-VLINE NO-GAP,(6)SPACE,IT_IDSHS-ZFATXCD, " 세구분.
          30 SY-VLINE NO-GAP,                  " 감면율.
          40 SY-VLINE NO-GAP,IT_IDSHS-ZFAGAMT
             CURRENCY IT_IDSHS-ZFKRW,          " 세.
          60 SY-VLINE NO-GAP,                  " 분납부호.
          80 SY-VLINE NO-GAP,                  " 감면액.
         100 SY-VLINE NO-GAP,
         120 SY-VLINE.
*    ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

  ENDIF.

  WRITE:/ SY-ULINE.
*  ADD 1 TO W_LINE.PERFORM P2000_PAGE_CHECK.

ENDFORM.                    " P3000_ITEM_WRITE
