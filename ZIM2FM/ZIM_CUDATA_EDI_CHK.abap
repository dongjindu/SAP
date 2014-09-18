FUNCTION ZIM_CUDATA_EDI_CHK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  TABLES
*"      IT_ZTIDRHS STRUCTURE  ZSIDRHS
*"      IT_ZTIDRHSD STRUCTURE  ZSIDRHSD
*"      IT_ZTIDRHSL STRUCTURE  ZSIDRHSL
*"  CHANGING
*"     VALUE(ZTIDR) LIKE  ZTIDR STRUCTURE  ZTIDR
*"----------------------------------------------------------------------

   ZTIDR-ZFEDICK = 'O'.

*>> 수입거래구분.
   IF ZTIDR-ZFPONC IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '수입거래구분'.
      ENDIF.
      MOVE  'X'     TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 수입신고종류.
   IF ZTIDR-ZFITKD IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '수입신고종류'.
      ENDIF.
      MOVE 'X'      TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 신고희망일자.
   IF ZTIDR-ZFIDWDT IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '신고희망일자'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 결제방법.
   IF ZTIDR-ZFAMCD IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '결제방법'.
      ENDIF.
      MOVE 'X'      TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 도착항.
   IF ZTIDR-ZFAPRTC IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '도착항'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 적출국.
   IF ZTIDR-ZFSCON IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '적출국'.
      ENDIF.
      MOVE 'X'      TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 운송용기.
   IF ZTIDR-ZFTRCN IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '운송용기'.
      ENDIF.
      MOVE 'X'      TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> HOUSE B/L NO.
   IF ZTIDR-ZFHBLNO IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH 'HOUSE B/L NO'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 운송수단.
   IF ZTIDR-ZFTRMET IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '운송수단'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 선기명.
   IF ZTIDR-ZFCARNM IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '선기명'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 신고의뢰자.
   IF ZTIDR-ZFAPNM IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '신고의뢰자'.
      ENDIF.
      MOVE 'X'      TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 납세의무자(상호).
   IF ZTIDR-ZFTDNM1 IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '납세의무자(상호)'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 납세의무자(성명).
   IF ZTIDR-ZFTDNM2 IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '납세의무자(성명)'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 납세의무자(주소).
   IF ZTIDR-ZFTDAD1 IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '납세의무자(주소)'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 납세의무자(사업자등록번호).
   IF ZTIDR-ZFTDTC IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '납세의무자(사업자등록번호)'.
      ENDIF.
      MOVE 'X'      TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 공급자(상호)
   IF ZTIDR-ZFSUPNM IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '공급자(상호)'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 인도조건.
   IF ZTIDR-INCO1 IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '인도조건'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 결제금액.
   IF ZTIDR-ZFSTAMT IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '결제금액'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 총중량.
   IF ZTIDR-ZFTOWT IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '총중량'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*>> 총포장갯수.
   IF ZTIDR-ZFPKCNT IS INITIAL.
      IF MODE NE 'N'.
         MESSAGE W167 WITH '총포장갯수'.
      ENDIF.
      MOVE 'X'       TO  ZTIDR-ZFEDICK.   EXIT.
   ENDIF.

*-------------------------< 란 사항 CHECK >----------------------------*
   LOOP  AT  IT_ZTIDRHS.

*>> HS CODE
      IF IT_ZTIDRHS-STAWN IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '란사항의 H/S CODE'.
         ENDIF.
         MOVE 'X'          TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

*>> 품명.
      IF IT_ZTIDRHS-ZFGDNM IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '란사항의 품명'.
         ENDIF.
         MOVE 'X'          TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

*>> 거래품명.
      IF IT_ZTIDRHS-ZFTGDNM IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '란사항의 거래 품명'.
         ENDIF.
         MOVE 'X'          TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

*>> 원산지< 한수원 주석 처리 >.
*      IF IT_ZTIDRHS-ZFORIG IS INITIAL.
*         IF MODE NE 'N'.
*            MESSAGE W167 WITH '란사항의 원산지'.
*         ENDIF.
*         MOVE 'X'          TO  ZTIDR-ZFEDICK.   EXIT.
*      ENDIF.

*>> 순중량.
*      IF IT_ZTIDRHS-ZFWET IS INITIAL.
*         IF MODE NE 'N'.
*            MESSAGE W167 WITH '란사항의 순중량'.
*         ENDIF.
*         MOVE 'X'          TO  ZTIDR-ZFEDICK.   EXIT.
*      ENDIF.

*>> 과세가격.
      IF IT_ZTIDRHS-ZFTBAK IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '란사항의 과세가격'.
         ENDIF.
         MOVE 'X'          TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

   ENDLOOP.

*-------------------< 규격 사항 CHECK >--------------------------------*
   LOOP  AT  IT_ZTIDRHSD.

*>> 단가.
      IF IT_ZTIDRHSD-NETPR IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '행사항의 단가'.
         ENDIF.
         MOVE 'X'           TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

   ENDLOOP.

*-----------------< 요건 사항 CHECK >----------------------------------*
   LOOP  AT  IT_ZSIDRHSL.

*>> 서류번호.
      IF IT_ZTIDRHSL-ZFCNNO IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '요건사항의 서류번호'.
         ENDIF.
         MOVE 'X'           TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

*>> 발급일자.
      IF IT_ZTIDRHSL-ZFISZDT IS INITIAL.
         IF MODE NE 'N'.
            MESSAGE W167 WITH '요건사항의 발급일자'.
         ENDIF.
         MOVE  'X'          TO  ZTIDR-ZFEDICK.   EXIT.
      ENDIF.

   ENDLOOP.

*>> 문서상태, EDI 상태, EDI CHECK 변경.
*   MOVE SY-DATUM    TO   ZTIDR-UDAT.
*   MOVE SY-UNAME    TO   ZTIDR-UNAM.

ENDFUNCTION.
