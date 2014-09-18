*&---------------------------------------------------------------------*
*& Report  ZRIMFLCLST                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  프로그램명 : LG 상사  대행실적 LIST.                               *
*&      작성자 : 이승준 INFOLINK Ltd.                                  *
*&      작성일 : 2001.09.17                                            *
*&---------------------------------------------------------------------*
*&  DESC.      :  수입의뢰에서 문서상태가 '확정'이면서 수입자구분이
*&                '상사대행' 인 LIST를 보여줌.
*&---------------------------------------------------------------------*
*& [변경내용]
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRIMFLCLST MESSAGE-ID ZIM
                   LINE-SIZE 123
                   NO STANDARD PAGE HEADING.

INCLUDE ZRIMFLCLSTTOP.

DATA : W_TOTAL_WON   LIKE    ZTREQHD-ZFLASTAM.


*----------------------------------------------------------------------*
*          SELECTION-SCREEN                                            *
*----------------------------------------------------------------------*

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
    S_BUKRS     FOR  ZTREQHD-BUKRS NO-EXTENSION NO INTERVALS,
    S_OPNDT     FOR  ZTREQST-ZFOPNDT OBLIGATORY,  "개설일-외화,원화.
    S_WERKS     FOR  ZTREQHD-ZFWERKS,       "사업장 번호
    S_EBELN	  FOR  ZTREQHD-EBELN,	   "구매문서번호(P/O 주문번호)
    S_LLIEF	  FOR  ZTREQHD-LLIEF,	   "공급업체(OFFER CODE)
    S_INCO1	  FOR  ZTREQHD-INCO1,	   "인도조건 (파트 1)(가격조건)
    S_WAERS	  FOR  ZTREQHD-WAERS.	   "통화키 (화패)

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*          INITIALIZATION.                                             *
*----------------------------------------------------------------------*

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.

*----------------------------------------------------------------------*
*          START-OF-SELECTION.                                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*> 전사금액 계산.
  PERFORM   P1000_READ_TEMP_TOTAL.

*>>> READ TABLE
  PERFORM   P1000_READ_TEMP.
  PERFORM   P1000_READ_ITEM.
  PERFORM   P1000_READ_TAB.
*>>> SORT-GROUP:WERKS EBERN TXZ01 MENGE.
  SORT IT_TAB BY WERKS EBELN MATNR MENGE.

*>>> WRITE LIST
  PERFORM   P3000_WRITE_IT.

*-----------------------------------------------------------------------
* User Command
*         -ZRIMBWLST [Report] 보세창고출고현황 참조.
*-----------------------------------------------------------------------



*----------------------------------------------------------------------*
*           TOP-OF-PAGE                                                *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT RESET.
  WRITE : /50 '[ LG상사 대행실적 (수입발주기준) ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  SKIP.

*----------------------------------------------------------------------*
*           AT USER-COMMAND                                            *
*----------------------------------------------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
      WHEN 'ME23'.                     " 보세창고출고.
         IF W_TABIX EQ 0.
            MESSAGE E962 .
         ELSE.
            PERFORM  P2000_ME23N_ZTREQHD USING IT_TAB-EBELN.
         ENDIF.
         CLEAR : IT_TAB,W_TABIX.

      WHEN 'MK03'.
         IF W_TABIX EQ 0.
            MESSAGE E962 .
         ELSE.
            PERFORM  P2000_MK03_ZTREQHD USING IT_TAB-LIFNR.
         ENDIF.
         CLEAR : IT_TAB,W_TABIX.

      WHEN 'LCNU'.
         IF W_TABIX EQ 0.
            MESSAGE E962 .
         ELSE.
            PERFORM  P2000_LCNO_ZTREQST USING IT_TAB-ZFREQNO.
         ENDIF.
         CLEAR : IT_TAB,W_TABIX.
  ENDCASE.
  CLEAR : IT_TAB,W_TABIX.

*-----------------------FORM 시작--------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET TITLEBAR 'ZIMR07'.  " TITLE BAR
  REFRESH IT_TAB.
  CLEAR IT_TAB.
  REFRESH IT_ITEM.
  CLEAR IT_ITEM.
  REFRESH IT_TEMP.
  CLEAR IT_TEMP.
  CLEAR : IT_TAB,W_TABIX.
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TEMP.
  SELECT H~ZFREQNO S~ZFOPNDT H~KURSF H~FFACT S~ZFDOCST
         H~IMTRD   H~EBELN   H~LLIEF H~INCO1 H~WAERS
         H~MATNR   H~LIFNR   H~MAKTX
         INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM ZTREQHD AS H JOIN ZTREQST AS S
             ON H~ZFREQNO EQ S~ZFREQNO
          WHERE   S~ZFDOCST EQ 'O'
            AND   H~IMTRD   EQ 'F'
            AND   H~BUKRS   IN S_BUKRS
            AND   S~ZFOPNDT IN S_OPNDT  "개설일-외화,원화.
            AND   H~EBELN IN S_EBELN    "구매문서번호(P/O 주문번호)
            AND   H~LLIEF IN S_LLIEF    "공급업체(OFFER CODE)
            AND   H~INCO1 IN S_INCO1    "인도조건 (파트 1)(가격조건)
            AND   H~WAERS IN S_WAERS    "통화키 (화패)
            AND   H~ZFWERKS IN S_WERKS. "사업장.

  IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.

ENDFORM.                    " P1000_READ_TEMP
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_ITEM.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ITEM FROM ZTREQIT
            FOR ALL ENTRIES IN IT_TEMP
            WHERE ZFREQNO EQ IT_TEMP-ZFREQNO.


   IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.
ENDFORM.                    " P1000_READ_ITEM
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_WAERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_WAERS.
  CLEAR W_WERKS.
  SELECT SINGLE * FROM EKPO
         INTO CORRESPONDING FIELDS OF IT_TAB
         WHERE EBELN EQ IT_TEMP-EBELN
           AND EBELP EQ IT_ITEM-EBELP.
ENDFORM.                    " P1000_GET_WAERS
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TAB.
  REFRESH : IT_TAB.
  LOOP AT IT_TEMP.
    LOOP AT IT_ITEM WHERE ZFREQNO EQ IT_TEMP-ZFREQNO.
      PERFORM   P1000_GET_WAERS.
      MOVE-CORRESPONDING IT_ITEM TO IT_TAB.
      MOVE-CORRESPONDING IT_TEMP TO IT_TAB.
      PERFORM   P1000_EX_FIELD.
      APPEND IT_TAB.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " P1000_READ_TAB
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_IT.

DATA : W_RATE(07) TYPE P DECIMALS 2.

  CLEAR W_LINE.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  W_LINE1 = 0.
  W_SUM = 0.
  W_TOTAL = 0.
  SET TITLEBAR  'ZIMR07'.  " TITLE BAR
  SET PF-STATUS 'ZIMR07'.

  CLEAR : IT_TAB,W_TABIX.
  REFRESH IT_TOTAL.
  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
     FORMAT RESET.
     W_LINE1 = W_LINE1 + 1.
     W_SUM = W_SUM + IT_TAB-EX_WON.
     W_TOTAL = W_TOTAL + IT_TAB-EX_WON.
     AT NEW WERKS.
       SKIP.
       PERFORM P1000_GET_NAME.
       WRITE : /3 'Plant : ', IT_TAB-WERKS,
                ' : ', T001W-NAME1.
       WRITE : / SY-ULINE(123).
       FORMAT COLOR COL_HEADING INTENSIFIED ON.
       WRITE : /
                SY-VLINE NO-GAP,(10)  '주문번호'   CENTERED NO-GAP,
                SY-VLINE NO-GAP,(4)   '화폐'       CENTERED NO-GAP,
                SY-VLINE NO-GAP,(4)   '조건'   CENTERED NO-GAP,
                SY-VLINE NO-GAP,(30)  '품명'       CENTERED NO-GAP,
                SY-VLINE NO-GAP, (11)   '발주물량'  CENTERED NO-GAP,
                                 (2)  ''           CENTERED NO-GAP,
                SY-VLINE NO-GAP,(10)  '단가'       CENTERED NO-GAP,
                                 (4)  ''           CENTERED NO-GAP,
                SY-VLINE NO-GAP,(15)  '외화금액'   CENTERED NO-GAP,
                SY-VLINE NO-GAP,(15)  '원화금액'   CENTERED NO-GAP,
                SY-VLINE NO-GAP,(8)  'OFFER CODE' CENTERED NO-GAP,
                SY-VLINE.
       WRITE : / SY-ULINE(123).
       REFRESH : IT_SUMMARY.
     ENDAT.

     W_MOD = SY-TABIX MOD 2.
     IF W_MOD = 0.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     ELSE .
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     ENDIF.

     WRITE : /
          SY-VLINE NO-GAP,(10) IT_TAB-EBELN    NO-GAP,	
          SY-VLINE NO-GAP,(4) IT_TAB-WAERS     NO-GAP,	
          SY-VLINE NO-GAP,(4) IT_TAB-INCO1     NO-GAP,	
          SY-VLINE NO-GAP,(30) IT_TAB-TXZ01    NO-GAP,	
          SY-VLINE NO-GAP,
          (11) IT_TAB-MENGE  UNIT IT_TAB-MEINS NO-GAP,
          (2) IT_TAB-MEINS  NO-GAP,
          SY-VLINE  NO-GAP,
          (10) IT_TAB-EX_PRI CURRENCY IT_TAB-WAERS  NO-GAP,
          (4) IT_TAB-BPRME  NO-GAP,
          SY-VLINE NO-GAP,
          (15) IT_TAB-EX_FOR CURRENCY IT_TAB-WAERS  NO-GAP,
          SY-VLINE NO-GAP,
          (15) IT_TAB-EX_WON CURRENCY 'KRW' NO-GAP,
          SY-VLINE NO-GAP,(8) IT_TAB-LLIEF  NO-GAP,
          SY-VLINE.
*>>>HIDE
     HIDE : IT_TAB, W_TABIX.
*>> 소계용 INTERNAL TABLES
     MOVE-CORRESPONDING   IT_TAB  TO  IT_SUMMARY.
     COLLECT IT_SUMMARY.
*>> 전사용 INTERNAL TABLES
     MOVE-CORRESPONDING   IT_TAB  TO  IT_TOTAL.
     COLLECT IT_TOTAL.


     AT END OF WERKS.
       FORMAT COLOR 3 INTENSIFIED OFF.
       WRITE : / SY-ULINE(123).
*> 비율계산...
       W_RATE = W_SUM / W_TOTAL_WON * 100.

       WRITE : SY-VLINE, 'Plant계',10 W_LINE1,'건',
              35(15) '전사실적 대비 :',
              50(10) W_RATE, '%',
              99(15) W_SUM CURRENCY 'KRW'.

       WRITE : 123 SY-VLINE.
       W_LINE1 = 0.
       W_SUM = 0.
       SORT IT_SUMMARY BY WAERS.
       LOOP AT IT_SUMMARY.
          WRITE : / SY-VLINE,
                    77(4) IT_SUMMARY-WAERS,
                    83(15) IT_SUMMARY-EX_FOR CURRENCY  IT_SUMMARY-WAERS,
                    99(15) IT_SUMMARY-EX_WON CURRENCY  'KRW',
                    123 SY-VLINE.
       ENDLOOP.
       WRITE : / SY-ULINE(123).
     ENDAT.

     AT LAST.
       FORMAT COLOR 3 INTENSIFIED ON.
       WRITE : / SY-ULINE(123).
*> 비율계산...
       W_RATE = W_TOTAL / W_TOTAL_WON * 100.
       WRITE : SY-VLINE, '전사',10 W_LINE,'건',
               35(15) '전사실적 대비 :',
               50(10) W_RATE, '%',
               65(20) W_TOTAL_WON CURRENCY 'KRW',
               99(15) W_TOTAL CURRENCY 'KRW'.


       WRITE : 123 SY-VLINE.
       CLEAR : W_LINE, W_TOTAL .
       SORT IT_SUMMARY BY WAERS.
       LOOP AT IT_TOTAL.
          WRITE : / SY-VLINE,
                    77(4) IT_TOTAL-WAERS NO-GAP,
                    83(15) IT_TOTAL-EX_FOR CURRENCY
                    IT_TOTAL-WAERS NO-GAP,
                    99(15) IT_TOTAL-EX_WON CURRENCY  'KRW' NO-GAP,
                    123 SY-VLINE.
       ENDLOOP.
       WRITE : / SY-ULINE(123).
     ENDAT.

  ENDLOOP.
  CLEAR : IT_TAB,W_TABIX.

ENDFORM.                    " P3000_WRITE_IT
*&---------------------------------------------------------------------*
*&      Form  P1000_EX_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_EX_FIELD.

   IF IT_TAB-PEINH = 0.
      IT_TAB-PEINH = 1.
   ENDIF.
*> 단가의 단위.
*>  DESC : EX) 1BOX를 주문한다.
*>             1BOX에는 20EA의 제품이 포장되고,
*>             단가는 10EA 당 1원이다.
   W_PRI =  IT_TAB-NETPR / IT_TAB-PEINH.
*> 단가를 출력 포맷으로 변환.
   PERFORM    SET_CURR_CONV_TO_EXTERNAL(SAPMZIM00)
                                        USING W_PRI
                                              IT_TAB-WAERS
                                              W_PRI.

*> 오더단위와 오더가격단위의 환산율로 곱한다.
   W_FOR = IT_TAB-MENGE * W_PRI * IT_TAB-BPUMZ / IT_TAB-BPUMN.

   IF IT_TAB-FFACT = 0.
      IT_TAB-FFACT = 1.
   ENDIF.
   IF IT_TAB-KURSF = 0.
      IT_TAB-KURSF = 1.
   ENDIF.

*> 환율의 단위(RATIO)로 환율을 구한 후 원화금액을 산출한다.
*> 외부 포맷.
   W_WON = W_FOR * IT_TAB-KURSF / IT_TAB-FFACT.

*> 단가, 외화, 원화를 내부포맷으로 변환.
   PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM00)
      USING W_PRI IT_TAB-WAERS.

   PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM00)
      USING W_FOR IT_TAB-WAERS.

   PERFORM SET_CURR_CONV_TO_INTERNAL(SAPMZIM00)
      USING W_WON 'KRW'.

   MOVE W_PRI TO IT_TAB-EX_PRI.
   MOVE W_FOR TO IT_TAB-EX_FOR.
   MOVE W_WON TO IT_TAB-EX_WON.

   CLEAR W_PRI.
   CLEAR W_FOR.
   CLEAR W_WON.

ENDFORM.                    " P1000_EX_FIELD
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_NAME.

  SELECT SINGLE * FROM T001W
      WHERE WERKS = IT_TAB-WERKS.

ENDFORM.                    " P1000_GET_NAME
*&---------------------------------------------------------------------*
*&      Form  P2000_ME23N_ZTREQHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_EBELN  text
*----------------------------------------------------------------------*
FORM P2000_ME23N_ZTREQHD USING    P_EBELN.
  IF P_EBELN IS INITIAL.
    MESSAGE E003.
  ENDIF.

  SET PARAMETER ID 'BSP' FIELD ''.
  SET PARAMETER ID 'BES' FIELD P_EBELN.

  CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_ME23N_ZTREQHD
*&---------------------------------------------------------------------*
*&      Form  P2000_MK03_ZTREQHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_LLIEF  text
*----------------------------------------------------------------------*
FORM P2000_MK03_ZTREQHD USING    P_LIFNR.
  IF P_LIFNR IS INITIAL.
    MESSAGE S174.    EXIT.
  ENDIF.
* 화면 PARAMETER ID
*  SET PARAMETER ID 'KDY' FIELD ''.
  SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
*2  SET PARAMETER ID 'KDY' FIELD '/320/310/130/120/110'.
  SET PARAMETER ID 'LIF' FIELD P_LIFNR.
*  SET PARAMETER ID 'EKO' FIELD ZTREQST-EKORG.
  SET PARAMETER ID 'EKO' FIELD ''.

  CALL TRANSACTION 'MK03' AND SKIP  FIRST SCREEN.
ENDFORM.                    " P2000_MK03_ZTREQHD
*&---------------------------------------------------------------------*
*&      Form  P2000_LCNO_ZTREQST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFREQNO  text
*----------------------------------------------------------------------*
FORM P2000_LCNO_ZTREQST USING    P_ZFREQNO.
    DATA: W_MAX LIKE ZTREQST-ZFAMDNO,
          P_ZFAMDNO LIKE ZTREQST-ZFAMDNO.
    SELECT MAX( ZFAMDNO ) FROM ZTREQST
         INTO W_MAX
         WHERE ZFREQNO EQ P_ZFREQNO
         GROUP BY ZFREQNO.
    ENDSELECT.
    MOVE W_MAX TO P_ZFAMDNO.
    CLEAR W_MAX.
    SET PARAMETER ID 'BES'       FIELD ''.
    SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
    SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
    SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
    EXPORT 'BES'           TO MEMORY ID 'BES'.
    EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
    EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
    EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.
    IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
    ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
    ENDIF.
ENDFORM.                    " P2000_LCNO_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEMP_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TEMP_TOTAL.

  SELECT H~ZFREQNO S~ZFOPNDT H~KURSF H~FFACT S~ZFDOCST
         H~IMTRD   H~EBELN   H~LLIEF H~INCO1 H~WAERS
         H~MATNR   H~LIFNR   H~MAKTX
         INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
           FROM ZTREQHD AS H JOIN ZTREQST AS S
             ON H~ZFREQNO EQ S~ZFREQNO
          WHERE   S~ZFDOCST EQ 'O'
            AND   H~ZFREQTY NOT IN ('PU', 'LO')
            AND   S~ZFOPNDT IN S_OPNDT  "개설일-외화,원화.
            AND   H~BUKRS   IN S_BUKRS
            AND   H~EBELN IN S_EBELN    "구매문서번호(P/O 주문번호)
            AND   H~LLIEF IN S_LLIEF    "공급업체(OFFER CODE)
            AND   H~INCO1 IN S_INCO1    "인도조건 (파트 1)(가격조건)
            AND   H~WAERS IN S_WAERS    "통화키 (화패)
            AND   H~ZFWERKS IN S_WERKS. "사업장.

  IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.

  PERFORM   P1000_READ_ITEM.

  CLEAR : W_TOTAL_WON.
  LOOP AT IT_TEMP.
    LOOP AT IT_ITEM WHERE ZFREQNO EQ IT_TEMP-ZFREQNO.
      PERFORM   P1000_GET_WAERS.
      MOVE-CORRESPONDING IT_ITEM TO IT_TAB.
      MOVE-CORRESPONDING IT_TEMP TO IT_TAB.
      PERFORM   P1000_EX_FIELD.
      ADD IT_TAB-EX_WON TO W_TOTAL_WON.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " P1000_READ_TEMP_TOTAL
