*&---------------------------------------------------------------------
*& Report  ZRIMORGPCLST
*&---------------------------------------------------------------------
*&  프로그램명 : 구매실적현황표(통화별)
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.10.08
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMORGPCLST  MESSAGE-ID ZIM
                     LINE-SIZE 255
                     NO STANDARD PAGE HEADING.
TABLES: ZTIVHST,ZTIVHSTIT,ZTIVIT,ZTREQHD,ZTREQST,
        EKKO,EKBE,EKPO,LFA1,ZTREQORJ,
        T001W,T005T,T156.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------

DATA : BEGIN OF IT_ZTIVHST OCCURS 0,
       ZFIVNO    LIKE  ZTIVHST-ZFIVNO,	" 통관요청/입고요청 관리번호.
       ZFIVHST   LIKE  ZTIVHST-ZFIVHST,	" 입고순번.
       ZFREQTY   LIKE  ZTREQHD-ZFREQTY,  " 문서상태.
       WERKS     LIKE  ZTIVHSTIT-WERKS,   " 플랜트.
       ZFGRST	   LIKE  ZTIVHST-ZFGRST,    " Good Receipt 상태.
       ZFCIVHST  LIKE  ZTIVHST-ZFCIVHST,  " Verify 순번.
       BLDAT	   LIKE  ZTIVHST-BLDAT,     " 전표내 증빙일.
       SHKZG	   LIKE  ZTIVHST-SHKZG,     " 차변/대변 지시자.
       MBLNR     LIKE  ZTIVHST-MBLNR,     " 자재문서번호.
       MJAHR     LIKE  ZTIVHST-MJAHR,     " 자재문서연도.
       BWART	   LIKE  ZTIVHST-BWART,     " 이동유형 (재고관리).
       ZFREQNO   LIKE  ZTREQHD-ZFREQNO,   " 수입의뢰관리번호.
       EBELN	   LIKE  ZTIVHSTIT-EBELN,   " 구매문서번호
       ZFIVDNO   LIKE   ZTIVHSTIT-ZFIVDNO," 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       USDDMBTR  LIKE  EKBE-DMBTR,        " 미국.
       JPYDMBTR  LIKE  EKBE-DMBTR,        " 일본.
       DMDMBTR   LIKE  EKBE-DMBTR,        " 독일.
       SFRDMBTR  LIKE  EKBE-DMBTR,        " 스위스.
       FFRDMBTR  LIKE  EKBE-DMBTR,        " 프랑스.
       BDPDMBTR  LIKE  EKBE-DMBTR,        " 영국.
       EURDMBTR  LIKE  EKBE-DMBTR,        " 유로외화.
       ETCDMBTR  LIKE  EKBE-DMBTR,        " 기타.
       FUSDDMBTR LIKE  EKBE-DMBTR,         " 미국외화.
       FJPYDMBTR LIKE  EKBE-DMBTR,         " 일본외화.
       FDMDMBTR  LIKE  EKBE-DMBTR,         " 독일외화.
       FSFRDMBTR LIKE  EKBE-DMBTR,         " 스위스외화.
       FFFRDMBTR LIKE  EKBE-DMBTR,         " 프랑스외화.
       FBDPDMBTR LIKE  EKBE-DMBTR,         " 영국외화.
       FEURDMBTR  LIKE  EKBE-DMBTR,        " 유로통화.
       FETCDMBTR LIKE  EKBE-DMBTR,         " 기타외화.
       IMDMBTR   LIKE  EKBE-DMBTR,        " 수입현지통화.
       LOKRW     LIKE  EKBE-DMBTR,        " LOCAL 원화.
       ZFUSDAM   LIKE  ZTREQHD-ZFUSDAM,   " USD 환산금액.
       FLOKRW    LIKE  EKBE-DMBTR,       " LOCAL 원화.
       LOUSD     LIKE  EKBE-DMBTR,        " LOCAL 외화.
       FLOUSD    LIKE  EKBE-DMBTR,        " LOCAL 외화.
       ZFLASTAM  LIKE  ZTREQHD-ZFLASTAM,  " 개설금액.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST.
*>> ITEM 내역.
DATA : BEGIN OF IT_TAB OCCURS 0,
       WERKS       LIKE  ZTIVHSTIT-WERKS,     " 플랜트.
       USDDMBTR    LIKE  EKBE-DMBTR,          " 미국.
       JPYDMBTR    LIKE  EKBE-DMBTR,          " 일본.
       DMDMBTR     LIKE  EKBE-DMBTR,          " 독일.
       SFRDMBTR    LIKE  EKBE-DMBTR,          " 스위스.
       FFRDMBTR    LIKE  EKBE-DMBTR,          " 프랑스.
       BDPDMBTR    LIKE  EKBE-DMBTR,          " 영국.
       EURDMBTR    LIKE  EKBE-DMBTR,         " 유로외화.
       ETCDMBTR    LIKE  EKBE-DMBTR,          " 기타.
       FUSDDMBTR   LIKE  EKBE-DMBTR,         " 미국외화.
       FJPYDMBTR   LIKE  EKBE-DMBTR,         " 일본외화.
       FDMDMBTR    LIKE  EKBE-DMBTR,         " 독일외화.
       FSFRDMBTR   LIKE  EKBE-DMBTR,         " 스위스외화.
       FFFRDMBTR   LIKE  EKBE-DMBTR,         " 프랑스외화.
       FBDPDMBTR   LIKE  EKBE-DMBTR,         " 영국외화.
       FEURDMBTR  LIKE  EKBE-DMBTR,           " 유로통화.
       FETCDMBTR   LIKE  EKBE-DMBTR,         " 기타외화.
       IMDMBTR    LIKE  EKBE-DMBTR,          " 수입현지통화.
       LOKRW     LIKE  EKBE-DMBTR,           " LOCAL 원화.
       FLOKRW     LIKE  EKBE-DMBTR,          " LOCAL 원화.
       LOUSD     LIKE  EKBE-DMBTR,           " LOCAL 외화.
       FLOUSD    LIKE  EKBE-DMBTR,           " LOCAL 외화.
       DMBTR      LIKE  EKBE-DMBTR.          " 현지통화.
DATA : END OF IT_TAB.

*>> 당년전사TOTAL.
DATA : BEGIN OF IT_CTOTAL OCCURS 0,
       USDDMBTR    LIKE  EKBE-DMBTR,          " 미국.
       JPYDMBTR    LIKE  EKBE-DMBTR,          " 일본.
       DMDMBTR     LIKE  EKBE-DMBTR,          " 독일.
       SFRDMBTR    LIKE  EKBE-DMBTR,          " 스위스.
       FFRDMBTR    LIKE  EKBE-DMBTR,          " 프랑스.
       BDPDMBTR    LIKE  EKBE-DMBTR,          " 영국.
       EURDMBTR    LIKE  EKBE-DMBTR,         " 유로외화.
       ETCDMBTR    LIKE  EKBE-DMBTR,          " 기타.
       FUSDDMBTR   LIKE  EKBE-DMBTR,         " 미국외화.
       FJPYDMBTR   LIKE  EKBE-DMBTR,         " 일본외화.
       FDMDMBTR    LIKE  EKBE-DMBTR,         " 독일외화.
       FSFRDMBTR   LIKE  EKBE-DMBTR,         " 스위스외화.
       FFFRDMBTR   LIKE  EKBE-DMBTR,         " 프랑스외화.
       FBDPDMBTR   LIKE  EKBE-DMBTR,         " 영국외화.
       FEURDMBTR    LIKE  EKBE-DMBTR,         " 유로통화.
       FETCDMBTR   LIKE  EKBE-DMBTR,         " 기타외화.
       IMDMBTR     LIKE  EKBE-DMBTR,         " 수입현지통화.
       LOKRW       LIKE  EKBE-DMBTR,         " LOCAL 원화.
       FLOKRW      LIKE  EKBE-DMBTR,         " LOCAL 원화.
       LOUSD       LIKE  EKBE-DMBTR,         " LOCAL 외화.
       FLOUSD      LIKE  EKBE-DMBTR,         " LOCAL 외화.
       DMBTR       LIKE  EKBE-DMBTR.          " 현지통화.
DATA : END OF IT_CTOTAL.


DATA : BEGIN OF IT_ZTIVHST1 OCCURS 0,
       ZFIVNO    LIKE  ZTIVHST-ZFIVNO,	" 통관요청/입고요청 관리번호.
       ZFIVHST   LIKE  ZTIVHST-ZFIVHST,	" 입고순번.
       ZFREQTY   LIKE  ZTREQHD-ZFREQTY,  " 문서상태.
       WERKS     LIKE  ZTIVHSTIT-WERKS,   " 플랜트.
       ZFGRST	   LIKE  ZTIVHST-ZFGRST,    " Good Receipt 상태.
       ZFCIVHST  LIKE  ZTIVHST-ZFCIVHST,  " Verify 순번.
       BLDAT	   LIKE  ZTIVHST-BLDAT,     " 전표내 증빙일.
       SHKZG	   LIKE  ZTIVHST-SHKZG,     " 차변/대변 지시자.
       MBLNR     LIKE  ZTIVHST-MBLNR,     " 자재문서번호.
       MJAHR     LIKE  ZTIVHST-MJAHR,     " 자재문서연도.
       BWART	   LIKE  ZTIVHST-BWART,     " 이동유형 (재고관리).
       ZFREQNO   LIKE  ZTREQHD-ZFREQNO,   " 수입의뢰관리번호.
       EBELN	   LIKE  ZTIVHSTIT-EBELN,   " 구매문서번호
       ZFIVDNO   LIKE   ZTIVHSTIT-ZFIVDNO," 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       USDDMBTR  LIKE  EKBE-DMBTR,        " 미국.
       JPYDMBTR  LIKE  EKBE-DMBTR,        " 일본.
       DMDMBTR   LIKE  EKBE-DMBTR,        " 독일.
       SFRDMBTR  LIKE  EKBE-DMBTR,        " 스위스.
       FFRDMBTR  LIKE  EKBE-DMBTR,        " 프랑스.
       BDPDMBTR  LIKE  EKBE-DMBTR,        " 영국.
       EURDMBTR  LIKE  EKBE-DMBTR,        " 유로외화.
       ETCDMBTR  LIKE  EKBE-DMBTR,        " 기타.
       FUSDDMBTR LIKE  EKBE-DMBTR,         " 미국외화.
       FJPYDMBTR LIKE  EKBE-DMBTR,         " 일본외화.
       FDMDMBTR  LIKE  EKBE-DMBTR,         " 독일외화.
       FSFRDMBTR LIKE  EKBE-DMBTR,         " 스위스외화.
       FFFRDMBTR LIKE  EKBE-DMBTR,         " 프랑스외화.
       FBDPDMBTR LIKE  EKBE-DMBTR,         " 영국외화.
       FEURDMBTR  LIKE  EKBE-DMBTR,        " 유로통화.
       FETCDMBTR LIKE  EKBE-DMBTR,         " 기타외화.
       IMDMBTR   LIKE  EKBE-DMBTR,        " 수입현지통화.
       LOKRW     LIKE  EKBE-DMBTR,        " LOCAL 원화.
       ZFUSDAM   LIKE  ZTREQHD-ZFUSDAM,   " USD 환산금액.
       FLOKRW    LIKE  EKBE-DMBTR,       " LOCAL 원화.
       LOUSD     LIKE  EKBE-DMBTR,        " LOCAL 외화.
       FLOUSD    LIKE  EKBE-DMBTR,        " LOCAL 외화.
       ZFLASTAM  LIKE  ZTREQHD-ZFLASTAM,  " 개설금액.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST1.
*>> 전년도 Plant.
DATA : BEGIN OF IT_TAB1 OCCURS 0,
         WERKS       LIKE  ZTIVHSTIT-WERKS,     " 플랜트.
       USDDMBTR    LIKE  EKBE-DMBTR,          " 미국.
       JPYDMBTR    LIKE  EKBE-DMBTR,          " 일본.
       DMDMBTR     LIKE  EKBE-DMBTR,          " 독일.
       SFRDMBTR    LIKE  EKBE-DMBTR,          " 스위스.
       FFRDMBTR    LIKE  EKBE-DMBTR,          " 프랑스.
       BDPDMBTR    LIKE  EKBE-DMBTR,          " 영국.
       EURDMBTR    LIKE  EKBE-DMBTR,         " 유로외화.
       ETCDMBTR    LIKE  EKBE-DMBTR,          " 기타.
       FUSDDMBTR   LIKE  EKBE-DMBTR,         " 미국외화.
       FJPYDMBTR   LIKE  EKBE-DMBTR,         " 일본외화.
       FDMDMBTR    LIKE  EKBE-DMBTR,         " 독일외화.
       FSFRDMBTR   LIKE  EKBE-DMBTR,         " 스위스외화.
       FFFRDMBTR   LIKE  EKBE-DMBTR,         " 프랑스외화.
       FBDPDMBTR   LIKE  EKBE-DMBTR,         " 영국외화.
       FEURDMBTR  LIKE  EKBE-DMBTR,           " 유로통화.
       FETCDMBTR   LIKE  EKBE-DMBTR,         " 기타외화.
       IMDMBTR    LIKE  EKBE-DMBTR,          " 수입현지통화.
       LOKRW     LIKE  EKBE-DMBTR,           " LOCAL 원화.
       FLOKRW     LIKE  EKBE-DMBTR,          " LOCAL 원화.
       LOUSD     LIKE  EKBE-DMBTR,           " LOCAL 외화.
       FLOUSD    LIKE  EKBE-DMBTR,           " LOCAL 외화.
       DMBTR      LIKE  EKBE-DMBTR.          " 현지통화.
DATA : END OF IT_TAB1.
*>> 전년전사TOTAL.
DATA : BEGIN OF IT_OTOTAL OCCURS 0,
       USDDMBTR    LIKE  EKBE-DMBTR,          " 미국.
       JPYDMBTR    LIKE  EKBE-DMBTR,          " 일본.
       DMDMBTR     LIKE  EKBE-DMBTR,          " 독일.
       SFRDMBTR    LIKE  EKBE-DMBTR,          " 스위스.
       FFRDMBTR    LIKE  EKBE-DMBTR,          " 프랑스.
       BDPDMBTR    LIKE  EKBE-DMBTR,          " 영국.
       EURDMBTR    LIKE  EKBE-DMBTR,         " 유로외화.
       ETCDMBTR    LIKE  EKBE-DMBTR,          " 기타.
       FUSDDMBTR   LIKE  EKBE-DMBTR,         " 미국외화.
       FJPYDMBTR   LIKE  EKBE-DMBTR,         " 일본외화.
       FDMDMBTR    LIKE  EKBE-DMBTR,         " 독일외화.
       FSFRDMBTR   LIKE  EKBE-DMBTR,         " 스위스외화.
       FFFRDMBTR   LIKE  EKBE-DMBTR,         " 프랑스외화.
       FBDPDMBTR   LIKE  EKBE-DMBTR,         " 영국외화.
       FEURDMBTR    LIKE  EKBE-DMBTR,         " 유로통화.
       FETCDMBTR   LIKE  EKBE-DMBTR,         " 기타외화.
       IMDMBTR     LIKE  EKBE-DMBTR,         " 수입현지통화.
       LOKRW       LIKE  EKBE-DMBTR,         " LOCAL 원화.
       FLOKRW      LIKE  EKBE-DMBTR,         " LOCAL 원화.
       LOUSD       LIKE  EKBE-DMBTR,         " LOCAL 외화.
       FLOUSD      LIKE  EKBE-DMBTR,         " LOCAL 외화.
       DMBTR       LIKE  EKBE-DMBTR.          " 현지통화.
DATA : END OF IT_OTOTAL.
*>> 당년 플랜트 원화 비율.
DATA : USDDMBTR    TYPE P  DECIMALS 2,           " 미국.
       JPYDMBTR    TYPE P  DECIMALS 2,           " 일본.
       DMDMBTR     TYPE P  DECIMALS 2,           " 독일.
       SFRDMBTR    TYPE P  DECIMALS 2,           " 스위스.
       FFRDMBTR    TYPE P  DECIMALS 2,           " 프랑스.
       BDPDMBTR    TYPE P  DECIMALS 2,           " 영국.
       EURDMBTR    TYPE P  DECIMALS 2,           " 유로.
       ETCDMBTR    TYPE P  DECIMALS 2,           " 기타.
       IMDMBTR     TYPE P  DECIMALS 2,         " 수입현지통화.
       LOKRW       TYPE P  DECIMALS 2,
       LOUSD       TYPE P  DECIMALS 2.

*>> 전년 플랜트원화 비율.
DATA : USDDMBTR1    TYPE P  DECIMALS 2,           " 미국.
       JPYDMBTR1    TYPE P  DECIMALS 2,           " 일본.
       DMDMBTR1     TYPE P  DECIMALS 2,           " 독일.
       SFRDMBTR1    TYPE P  DECIMALS 2,           " 스위스.
       FFRDMBTR1    TYPE P  DECIMALS 2,           " 프랑스.
       BDPDMBTR1    TYPE P  DECIMALS 2,           " 영국.
       EURDMBTR1    TYPE P  DECIMALS 2,           " 유로.
       ETCDMBTR1    TYPE P  DECIMALS 2,           " 기타.
       IMDMBTR1     TYPE P  DECIMALS 2,           " 수입현지통화.
       LOKRW1       TYPE P  DECIMALS 2,
       LOUSD1       TYPE P  DECIMALS 2.


*>> 전사당년 원화 비율.
DATA : USDDMBTR2    TYPE P  DECIMALS 2,           " 미국.
       JPYDMBTR2    TYPE P  DECIMALS 2,           " 일본.
       DMDMBTR2     TYPE P  DECIMALS 2,           " 독일.
       SFRDMBTR2    TYPE P  DECIMALS 2,           " 스위스.
       FFRDMBTR2    TYPE P  DECIMALS 2,           " 프랑스.
       BDPDMBTR2    TYPE P  DECIMALS 2,           " 영국.
       EURDMBTR2    TYPE P  DECIMALS 2,           " 유로.
       ETCDMBTR2    TYPE P  DECIMALS 2,           " 기타.
       IMDMBTR2     TYPE P  DECIMALS 2,         " 수입현지통화.
       LOKRW2       TYPE P  DECIMALS 2,
       LOUSD2       TYPE P  DECIMALS 2.

*>> 전년 원화 비율.
DATA : USDDMBTR3    TYPE P  DECIMALS 2,           " 미국.
       JPYDMBTR3    TYPE P  DECIMALS 2,           " 일본.
       DMDMBTR3     TYPE P  DECIMALS 2,           " 독일.
       SFRDMBTR3    TYPE P  DECIMALS 2,           " 스위스.
       FFRDMBTR3    TYPE P  DECIMALS 2,           " 프랑스.
       BDPDMBTR3    TYPE P  DECIMALS 2,           " 영국.
       EURDMBTR3    TYPE P  DECIMALS 2,           " 유로.
       ETCDMBTR3    TYPE P  DECIMALS 2,           " 기타.
       IMDMBTR3     TYPE P  DECIMALS 2,         " 수입현지통화.
       LOKRW3       TYPE P  DECIMALS 2,
       LOUSD3       TYPE P  DECIMALS 2.

DATA :  W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_TO(04),
        W_FROM(04),
        ST_DMBTR      LIKE  EKBE-DMBTR,        " 전년도총액.
        ST_BAMNG      LIKE  EKBE-BAMNG,        " 전년도수량.
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_CHEK,
        W_CHECK_BIT   TYPE C,
        W_SINGN(1),
        W_TABIX       LIKE SY-TABIX,
        W_PAGE        TYPE I,
        W_LIST_INDEX  LIKE SY-TABIX.
RANGES: R_BLDAT FOR ZTIVHST-BLDAT OCCURS 05.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

     SELECT-OPTIONS:
     S_BUKRS   FOR  EKKO-BUKRS NO-EXTENSION NO INTERVALS,
     S_WERKS   FOR  EKPO-WERKS,
     S_BLDAT   FOR  EKBE-BUDAT OBLIGATORY,     "전기일(기간)
     S_EKGRP   FOR EKKO-EKGRP,        " 구매그룹..
     S_EKORG	 FOR  EKKO-EKORG,       " 구매조직.
     S_BSTYP   FOR  EKKO-BSTYP,     "1구매문서범주
     S_BSART   FOR  EKKO-BSART.     "4구매문서유형
 SELECTION-SCREEN END OF BLOCK B1.
 INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

*title Text Write
W_PAGE = 1.
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 테이블 SELECT
   PERFORM   P1000_READ_DATA    USING  W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738.  EXIT.
   ENDIF.
   IF W_ERR_CHK = 'S'.
      MESSAGE S977 WITH '입고일자 범위가 적절하지 않습니다.'.
      EXIT.
   ENDIF.
* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFIDRNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
*      WHEN 'REFR'.
*          PERFORM P1000_READ_DATA USING W_ERR_CHK.
*          PERFORM  P2000_TOP_END_IT_TAB.
*          PERFORM RESET_LIST.
*      WHEN 'DISP'.
*          IF W_TABIX IS INITIAL.
*            MESSAGE S962.    EXIT.
*          ENDIF.
*           PERFORM P2000_PO_DOC_DISPLAY(SAPMZIM01)
*                                     USING IT_TAB-ZFREBELN ''.
*      WHEN 'DISP1'.
*        IF W_TABIX IS INITIAL.
*           MESSAGE S962.    EXIT.
*        ENDIF.
*        PERFORM P2000_DISP_ZTIDS USING IT_TAB-ZFIDRNO.
*      WHEN 'DOWN'.          " FILE DOWNLOAD....
*          PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.
*  CLEAR: IT_TAB, W_TABIX.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.


  SKIP 1.
  WRITE:/100'[    구  매  실  적   현  황  표  ( 통 화 별 )     ]'
      COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 2.
  WRITE:/ '당년도기간:',S_BLDAT-LOW,'~',S_BLDAT-HIGH.
  WRITE:/ '전년도기간:',R_BLDAT-LOW,'~',R_BLDAT-HIGH .

  WRITE:/ '단      위:','원화금액(백만원)',',','외화금액(천)'.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(20) 'Plant'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '미국$'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '일본￥'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '독일DM'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '스위스SF'    NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '프랑스FF'    NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '영국LB'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '유로EUR'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '기타'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '수입계'    NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '￦Local DL' NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '$ Local DL' NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) 'Total '  NO-GAP CENTERED,
            SY-VLINE.
   FORMAT RESET.
   FORMAT COLOR COL_HEADING INTENSIFIED OFF.
   WRITE : / SY-VLINE NO-GAP,(20) ''     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'    NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'    NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '원'      NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) ' 원'    NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '￦Local 원'   NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '$ Local 원 '  NO-GAP CENTERED,
            SY-VLINE NO-GAP, (06) '비율'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) ' 원'  NO-GAP CENTERED,
            SY-VLINE.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L 관리 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.

  W_TO   =  S_BLDAT-LOW(04) - S_BLDAT-HIGH(04).
  W_FROM =  S_BLDAT-LOW(04) - 0001.
*>> 당년도 범위 설정 체크.
  IF W_TO NE 0.
      W_ERR_CHK = 'S'.
      EXIT.
  ENDIF.
*>> 전년도 범위설정.
  MOVE : 'I'               TO  R_BLDAT-SIGN,
         'BT'              TO  R_BLDAT-OPTION.
  CONCATENATE  W_FROM  S_BLDAT-LOW+4(04) INTO   R_BLDAT-LOW.
  CONCATENATE  W_FROM  S_BLDAT-HIGH+4(04) INTO   R_BLDAT-HIGH.
  APPEND R_BLDAT.
*>> 당해년도.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVHST
            FROM ZTIVHST AS R INNER JOIN ZTIVHSTIT AS I
             ON R~ZFIVNO  = I~ZFIVNO
            AND R~ZFIVHST = I~ZFIVHST
         WHERE  I~ZFGRST  = 'Y'
           AND  I~WERKS   IN S_WERKS
           AND  R~BLDAT   IN S_BLDAT.    " 증빙일.
*>> 로컬.
 CLEAR IT_ZTIVHST.
 SELECT * APPENDING CORRESPONDING FIELDS OF TABLE  IT_ZTIVHST
            FROM ( ZTREQHD AS R INNER JOIN ZTREQST AS I
             ON   R~ZFREQNO  = I~ZFREQNO )
             INNER JOIN ZTREQIT AS M
             ON   R~ZFREQNO  = M~ZFREQNO
          WHERE I~ZFDOCST  = 'O'
           AND  R~ZFREQTY  = 'LO'.
  PERFORM P1000_CURRENCT_BLDAT.
*>> 전년도.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVHST1
            FROM ZTIVHST AS R INNER JOIN ZTIVHSTIT AS I
             ON R~ZFIVNO  = I~ZFIVNO
            AND R~ZFIVHST = I~ZFIVHST
         WHERE  I~ZFGRST  = 'Y'
           AND  I~WERKS   IN S_WERKS
           AND  R~BLDAT   IN R_BLDAT.          " 증빙일.
*>> LOCAL.
 CLEAR IT_ZTIVHST1.
 SELECT * APPENDING CORRESPONDING FIELDS OF TABLE  IT_ZTIVHST1
            FROM ( ZTREQHD AS R INNER JOIN ZTREQST AS I
             ON   R~ZFREQNO  = I~ZFREQNO )
             INNER JOIN ZTREQIT AS M
             ON   R~ZFREQNO  = M~ZFREQNO
          WHERE I~ZFDOCST  = 'O'
           AND  R~ZFREQTY  = 'LO'.

  PERFORM P1000_OLDDAT_BLDAT.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'ZIMR30'.
   SET PF-STATUS 'ZIMR30'.
   CLEAR W_COUNT.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM  P3000_LINE_WRITE.
   ENDLOOP.
   WRITE:/ SY-ULINE.
   PERFORM P3000_LINE_TOTAL.

*  CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR30'.

  MOVE : 'I'               TO  S_BLDAT-SIGN,
         'BT'              TO  S_BLDAT-OPTION,
         SY-DATUM          TO  S_BLDAT-HIGH.
  CONCATENATE SY-DATUM(4) '01' '01' INTO S_BLDAT-LOW.

  APPEND S_BLDAT.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.
*>> 비율차이.
 DATA : L_USDDMBTR   TYPE P  DECIMALS 2,           " 미국.
        L_JPYDMBTR    TYPE P  DECIMALS 2,           " 일본.
        L_DMDMBTR     TYPE P  DECIMALS 2,           " 독일.
        L_SFRDMBTR    TYPE P  DECIMALS 2,           " 스위스.
        L_FFRDMBTR    TYPE P  DECIMALS 2,           " 프랑스.
        L_BDPDMBTR    TYPE P  DECIMALS 2,           " 영국.
        L_EURDMBTR    TYPE P  DECIMALS 2,           " 영국.
        L_ETCDMBTR    TYPE P  DECIMALS 2,           " 기타.
        L_IMDMBTR     TYPE P  DECIMALS 2,         " 수입현지통화.
        L_LOKRW       TYPE P  DECIMALS 2,
        L_LOUSD       TYPE P  DECIMALS 2.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*>> 당년 외화.
  WRITE : / SY-VLINE NO-GAP,(20) IT_TAB-WERKS NO-GAP,    " Plant'
            SY-VLINE NO-GAP,(12) IT_TAB-FUSDDMBTR
                     CURRENCY 'USD' NO-GAP," 미국
                      SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FJPYDMBTR
                     CURRENCY 'JPY' NO-GAP,"일본
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FDMDMBTR
                     CURRENCY 'DEM' NO-GAP," 독일
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FSFRDMBTR
                     CURRENCY 'CHF' NO-GAP,"'스위스
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FFFRDMBTR
                     CURRENCY 'CFP' NO-GAP,"'프랑스
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FBDPDMBTR
                     CURRENCY 'GBP' NO-GAP,"'영국
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FEURDMBTR
                     CURRENCY 'EUR' NO-GAP,"유로
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FLOKRW
                     CURRENCY 'USD' NO-GAP,
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FLOUSD
                     CURRENCY 'USD' NO-GAP, " $ Local '  ,
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
            SY-VLINE.
  CLEAR T001W.
  SELECT SINGLE *
         FROM T001W
        WHERE WERKS = IT_TAB-WERKS.
*>> 당년도비율.
  CLEAR: USDDMBTR,JPYDMBTR,DMDMBTR,SFRDMBTR,FFRDMBTR,BDPDMBTR,
         ETCDMBTR,IMDMBTR,LOKRW,LOUSD.
  IF NOT IT_TAB-DMBTR IS INITIAL.
    USDDMBTR  =  IT_TAB-USDDMBTR / IT_TAB-DMBTR * 100.  " 미국.
    JPYDMBTR  =  IT_TAB-JPYDMBTR / IT_TAB-DMBTR * 100.  " 일본.
    DMDMBTR   =  IT_TAB-DMDMBTR  / IT_TAB-DMBTR * 100.  " 독일.
    SFRDMBTR  =  IT_TAB-SFRDMBTR / IT_TAB-DMBTR * 100.  " 스위스.
    FFRDMBTR  =  IT_TAB-FFRDMBTR / IT_TAB-DMBTR * 100.  " 프랑스.
    BDPDMBTR  =  IT_TAB-BDPDMBTR / IT_TAB-DMBTR * 100.  " 영국.
    EURDMBTR  =  IT_TAB-EURDMBTR / IT_TAB-DMBTR * 100.  " 영국.
    ETCDMBTR  =  IT_TAB-ETCDMBTR / IT_TAB-DMBTR * 100.   " 기타.
    IMDMBTR   =  IT_TAB-IMDMBTR  / IT_TAB-DMBTR * 100.  " 수입현지통화.
    LOKRW     =  IT_TAB-LOKRW    / IT_TAB-DMBTR * 100.
    LOUSD     =  IT_TAB-LOUSD    / IT_TAB-DMBTR * 100.
  ENDIF.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

*>> 당년 현지통화.
  WRITE : / SY-VLINE NO-GAP,(20) T001W-NAME1 NO-GAP,    " Plant'
            SY-VLINE NO-GAP,(12) IT_TAB-USDDMBTR
                     CURRENCY 'KRW' NO-GAP," 미국'
                     SY-VLINE NO-GAP,(06) USDDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-JPYDMBTR
                     CURRENCY 'KRW' NO-GAP,"일본'
                     SY-VLINE NO-GAP,(06) JPYDMBTR    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-DMDMBTR
                     CURRENCY 'KRW' NO-GAP," 독일'
                     SY-VLINE NO-GAP,(06) DMDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-SFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'스위스'
                     SY-VLINE NO-GAP,(06) SFRDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-FFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'프랑스'
                     SY-VLINE NO-GAP,(06) FFRDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-BDPDMBTR
                     CURRENCY 'KRW' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) BDPDMBTR     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-EURDMBTR
                     CURRENCY 'KRW' NO-GAP,"'유로'
                     SY-VLINE NO-GAP,(06) EURDMBTR     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-ETCDMBTR
                     CURRENCY 'KRW' NO-GAP,"기타'
                     SY-VLINE NO-GAP,(06) ETCDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-IMDMBTR
                     CURRENCY 'KRW' NO-GAP, " 수입계.
                     SY-VLINE NO-GAP,(06) IMDMBTR    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-LOKRW
                     CURRENCY 'KRW' NO-GAP,
                     SY-VLINE NO-GAP,(06) LOKRW    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-LOUSD
                     CURRENCY 'KRW' NO-GAP, " $ Local '
                     SY-VLINE NO-GAP,(06) LOUSD   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB-DMBTR
                     CURRENCY 'KRW' NO-GAP,  " 토탈'
            SY-VLINE.
 CLEAR IT_TAB1.
 READ TABLE IT_TAB1 WITH KEY WERKS = IT_TAB-WERKS.
*>> 전년도비율.
  CLEAR: USDDMBTR1,JPYDMBTR1,DMDMBTR1,SFRDMBTR1,FFRDMBTR1,BDPDMBTR1,
         ETCDMBTR1,IMDMBTR1,LOKRW1,LOUSD1.
  IF NOT IT_TAB1-DMBTR IS INITIAL.
    USDDMBTR1  =  IT_TAB1-USDDMBTR / IT_TAB1-DMBTR * 100.  " 미국.
    JPYDMBTR1  =  IT_TAB1-JPYDMBTR / IT_TAB1-DMBTR * 100.  " 일본.
    DMDMBTR1   =  IT_TAB1-DMDMBTR  / IT_TAB1-DMBTR * 100.  " 독일.
    SFRDMBTR1  =  IT_TAB1-SFRDMBTR / IT_TAB1-DMBTR * 100.  " 스위스.
    FFRDMBTR1  =  IT_TAB1-FFRDMBTR / IT_TAB1-DMBTR * 100.  " 프랑스.
    BDPDMBTR1  =  IT_TAB1-BDPDMBTR / IT_TAB1-DMBTR * 100.  " 영국.
    EURDMBTR1  =  IT_TAB1-EURDMBTR / IT_TAB1-DMBTR * 100.  " 유로.
    ETCDMBTR1  =  IT_TAB1-ETCDMBTR / IT_TAB1-DMBTR * 100.   " 기타.
    IMDMBTR1  =  IT_TAB1-IMDMBTR  / IT_TAB1-DMBTR * 100. " 수입현지통화.
    LOKRW1     =  IT_TAB1-LOKRW    / IT_TAB1-DMBTR * 100.
    LOUSD1     =  IT_TAB1-LOUSD    / IT_TAB1-DMBTR * 100.
  ENDIF.
*>> 전년도 현지통화.
 FORMAT RESET.
 FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

 WRITE : / SY-VLINE NO-GAP,(20) '' NO-GAP,    " Plant'
            SY-VLINE NO-GAP,(12) IT_TAB1-USDDMBTR
                     CURRENCY 'KRW' NO-GAP," 미국'
                     SY-VLINE NO-GAP,(06) USDDMBTR1  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-JPYDMBTR
                     CURRENCY 'KRW' NO-GAP,"일본'
                     SY-VLINE NO-GAP,(06) JPYDMBTR1 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-DMDMBTR
                     CURRENCY 'KRW' NO-GAP," 독일'
                     SY-VLINE NO-GAP,(06) DMDMBTR1 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-SFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'스위스'
                    SY-VLINE NO-GAP,(06)SFRDMBTR1    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-FFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'프랑스'
                     SY-VLINE NO-GAP,(06) FFRDMBTR1     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-BDPDMBTR
                     CURRENCY 'KRW' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) BDPDMBTR1    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-EURDMBTR
                     CURRENCY 'KRW' NO-GAP,"'유로'
                     SY-VLINE NO-GAP,(06) EURDMBTR1    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-ETCDMBTR
                     CURRENCY 'KRW' NO-GAP,"기타'
                     SY-VLINE NO-GAP,(06)ETCDMBTR1   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-IMDMBTR
                     CURRENCY 'KRW' NO-GAP, " 수입계.
                     SY-VLINE NO-GAP,(06) IMDMBTR1      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-LOKRW
                     CURRENCY 'KRW' NO-GAP,
                     SY-VLINE NO-GAP,(06)LOKRW1      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_TAB1-LOUSD
                     CURRENCY 'KRW' NO-GAP, " $ Local '
                     SY-VLINE NO-GAP,(06) LOUSD1  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
            SY-VLINE.
 L_USDDMBTR  =  USDDMBTR -  USDDMBTR1.           " 미국.
 L_JPYDMBTR  =  JPYDMBTR -  JPYDMBTR1.       " 일본.
 L_DMDMBTR   =  DMDMBTR -   DMDMBTR1.        " 독일.
 L_SFRDMBTR  =  SFRDMBTR -  SFRDMBTR1.       "위스.
 L_FFRDMBTR  =  FFRDMBTR -  FFRDMBTR1.    " 프랑스.
 L_BDPDMBTR  =  BDPDMBTR -  BDPDMBTR1.    " 영국
 L_EURDMBTR  =  EURDMBTR -  EURDMBTR1.    " 유로
 L_ETCDMBTR  =  ETCDMBTR -  ETCDMBTR1.   " 기타.
 L_IMDMBTR   =  IMDMBTR  -  IMDMBTR1.  " 수입현지통화.
 L_LOKRW     =  LOKRW -     LOKRW1.
 L_LOUSD     =  LOUSD -     LOUSD1.

 FORMAT RESET.
 FORMAT COLOR COL_GROUP INTENSIFIED OFF.

 WRITE : / SY-VLINE NO-GAP,(20) '' NO-GAP,    " Plant'
            SY-VLINE NO-GAP,(12) '' NO-GAP," 미국'
                     SY-VLINE NO-GAP,(06) L_USDDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) ''NO-GAP,"일본'
                     SY-VLINE NO-GAP,(06) L_JPYDMBTR NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP," 독일'
                     SY-VLINE NO-GAP,(06) L_DMDMBTR NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'스위스'
                    SY-VLINE NO-GAP,(06) L_SFRDMBTR    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'프랑스'
                     SY-VLINE NO-GAP,(06) L_FFRDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) L_BDPDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) L_EURDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"기타'
                     SY-VLINE NO-GAP,(06) L_ETCDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP, " 수입계.
                     SY-VLINE NO-GAP,(06) L_IMDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
                     SY-VLINE NO-GAP,(06) L_LOKRW    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) ''NO-GAP, " $ Local '
                     SY-VLINE NO-GAP,(06) L_LOUSD  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
            SY-VLINE.

* HIDE : IT_TAB,W_TABIX.

ENDFORM.
*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*&      Form  P1000_CURRENCT_BLDAT
*&---------------------------------------------------------------------
FORM P1000_CURRENCT_BLDAT.

  LOOP AT IT_ZTIVHST.

     W_TABIX = SY-TABIX.
*>> 회사코드체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  =  IT_ZTIVHST-EBELN
          AND BUKRS IN S_BUKRS
          AND BSTYP IN S_BSTYP
          AND BSART IN S_BSART
          AND EKORG IN  S_EKORG
          AND EKGRP IN  S_EKGRP.
     IF SY-SUBRC NE 0.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.

     IF IT_ZTIVHST-ZFREQTY = 'LO'.
        CLEAR EKBE.
        SELECT SINGLE *
          FROM EKBE
         WHERE EBELN  = IT_ZTIVHST-EBELN
           AND EBELP  = IT_ZTIVHST-EBELP
           AND WERKS   IN S_WERKS
           AND BLDAT   IN S_BLDAT.    " 증빙일.
        IF SY-SUBRC NE 0.
           DELETE IT_ZTIVHST INDEX W_TABIX.
           CONTINUE.
        ENDIF.
        MOVE:  EKBE-MATNR  TO IT_ZTIVHST-MATNR,    " 자재번호.
               EKBE-BELNR  TO IT_ZTIVHST-MBLNR,    " 자재문서번호.
               EKBE-GJAHR  TO IT_ZTIVHST-MJAHR,    " 자재문서연도.
               EKBE-BWART  TO IT_ZTIVHST-BWART,    " 이동유형.
               EKBE-WERKS  TO IT_ZTIVHST-WERKS.    " PLANT.
     ELSE.
*>> GET ZFREQNO.
        CLEAR ZTIVIT.
        SELECT   SINGLE *
           FROM  ZTIVIT
           WHERE ZFIVNO  = IT_ZTIVHST-ZFIVNO
             AND ZFIVDNO = IT_ZTIVHST-ZFIVDNO.
        IT_ZTIVHST-ZFREQNO = ZTIVIT-ZFREQNO.
     ENDIF.
*>> 수입의로 타입 PU 제외.
     CLEAR ZTREQHD.
     SELECT   SINGLE *
        FROM  ZTREQHD
        WHERE ZFREQNO = IT_ZTIVHST-ZFREQNO.
     IF ZTREQHD-ZFREQTY EQ 'PU' .
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 수량.현지통화금액.
*>> 구매문서별이력.
     CLEAR EKBE.
     SELECT SINGLE *
       FROM EKBE
      WHERE EBELN  = IT_ZTIVHST-EBELN
        AND EBELP  = IT_ZTIVHST-EBELP
        AND MATNR  = IT_ZTIVHST-MATNR    " 자재번호.
        AND BELNR  = IT_ZTIVHST-MBLNR    " 자재문서번호.
        AND GJAHR  = IT_ZTIVHST-MJAHR    " 자재문서연도.
        AND BWART  = IT_ZTIVHST-BWART    " 이동유형.
        AND BEWTP  = 'E'.                " 이력종류.
     IF SY-SUBRC NE 0.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.
     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = IT_ZTIVHST-BWART.
*>> 이동 유형에 따른 차/대변 지시자.
     IT_ZTIVHST-DMBTR    =  EKBE-DMBTR / 1000000.
     IT_ZTIVHST-ZFLASTAM =  ZTREQHD-ZFLASTAM / 1000.
     IT_ZTIVHST-ZFUSDAM  =  ZTREQHD-ZFUSDAM / 1000.

     IF T156-SHKZG = 'H'.
        IT_ZTIVHST-DMBTR    =  ( EKBE-DMBTR  ) * -1.
        IT_ZTIVHST-ZFLASTAM =  ( ZTREQHD-ZFLASTAM ) * -1.
        IT_ZTIVHST-ZFUSDAM  =  ( ZTREQHD-ZFUSDAM ) * -1.
     ENDIF.

*>> 통화구분.
     IF ZTREQHD-ZFREQTY EQ 'LO'.
         CASE ZTREQHD-WAERS.
           WHEN 'KRW'.
              IT_ZTIVHST-LOKRW =  IT_ZTIVHST-DMBTR.   " 현지통화금액
              IT_ZTIVHST-FLOKRW = IT_ZTIVHST-ZFUSDAM. " USD 환산금액.
           WHEN OTHERS.
              IT_ZTIVHST-LOUSD = IT_ZTIVHST-DMBTR .
              IT_ZTIVHST-FLOUSD = IT_ZTIVHST-ZFUSDAM.
        ENDCASE.
     ELSE.
        IT_ZTIVHST-IMDMBTR = IT_ZTIVHST-DMBTR.    " 수입계.
        CASE ZTREQHD-WAERS.
           WHEN 'USD'.
                IT_ZTIVHST-USDDMBTR = IT_ZTIVHST-DMBTR.
                IT_ZTIVHST-FUSDDMBTR = IT_ZTIVHST-ZFLASTAM.  " 개설금액.
           WHEN 'JPY'.
                IT_ZTIVHST-JPYDMBTR = IT_ZTIVHST-DMBTR.
                IT_ZTIVHST-FJPYDMBTR = IT_ZTIVHST-ZFLASTAM.
           WHEN 'DEM'.
                IT_ZTIVHST-DMDMBTR = IT_ZTIVHST-DMBTR.
                IT_ZTIVHST-FJPYDMBTR = IT_ZTIVHST-ZFLASTAM.
           WHEN 'CHF'.
                 IT_ZTIVHST-SFRDMBTR  = IT_ZTIVHST-DMBTR.
                 IT_ZTIVHST-FJPYDMBTR = IT_ZTIVHST-ZFLASTAM.
           WHEN 'CFP'.
                IT_ZTIVHST-FFRDMBTR  = IT_ZTIVHST-DMBTR.
                IT_ZTIVHST-FFFRDMBTR  = IT_ZTIVHST-ZFLASTAM.
           WHEN 'GBP'.
                IT_ZTIVHST-BDPDMBTR  = IT_ZTIVHST-DMBTR.
                IT_ZTIVHST-FBDPDMBTR  = IT_ZTIVHST-ZFLASTAM.
           WHEN 'EUR'.
                IT_ZTIVHST-EURDMBTR   = IT_ZTIVHST-DMBTR.
                IT_ZTIVHST-FEURDMBTR  = IT_ZTIVHST-ZFLASTAM.
           WHEN OTHERS.
                IT_ZTIVHST-ETCDMBTR = IT_ZTIVHST-DMBTR.
       ENDCASE.
    ENDIF.

    MODIFY IT_ZTIVHST INDEX W_TABIX.
    MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB.
    MOVE-CORRESPONDING IT_ZTIVHST TO IT_CTOTAL.
    COLLECT IT_TAB.
    COLLECT IT_CTOTAL.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
      W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_CURRENCT_BLDAT
*&---------------------------------------------------------------------
*
*&      Form  P1000_OLDDAT_BLDAT
*&---------------------------------------------------------------------
*
FORM P1000_OLDDAT_BLDAT.

   LOOP AT IT_ZTIVHST1.

     W_TABIX = SY-TABIX.
*>> 회사코드체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  =  IT_ZTIVHST1-EBELN
          AND BUKRS IN S_BUKRS
          AND BSTYP IN S_BSTYP
          AND BSART IN S_BSART
          AND EKORG IN S_EKORG
          AND EKGRP IN S_EKGRP.

     IF SY-SUBRC NE 0.
        DELETE IT_ZTIVHST1 INDEX W_TABIX.
        CONTINUE.
     ENDIF.

     IF IT_ZTIVHST1-ZFREQTY = 'LO'.
        CLEAR EKBE.
        SELECT SINGLE *
          FROM EKBE
         WHERE EBELN  = IT_ZTIVHST1-EBELN
           AND EBELP  = IT_ZTIVHST1-EBELP
           AND WERKS   IN S_WERKS
           AND BLDAT   IN R_BLDAT.    " 증빙일.
        IF SY-SUBRC NE 0.
           DELETE IT_ZTIVHST1 INDEX W_TABIX.
           CONTINUE.
        ENDIF.
        MOVE:  EKBE-MATNR  TO IT_ZTIVHST1-MATNR,    " 자재번호.
               EKBE-BELNR  TO IT_ZTIVHST1-MBLNR,    " 자재문서번호.
               EKBE-GJAHR  TO IT_ZTIVHST1-MJAHR,    " 자재문서연도.
               EKBE-BWART  TO IT_ZTIVHST1-BWART,    " 이동유형.
               EKBE-WERKS  TO IT_ZTIVHST1-WERKS.    " PLANT.
     ELSE.
*>> GET ZFREQNO.
        CLEAR ZTIVIT.
        SELECT   SINGLE *
           FROM  ZTIVIT
           WHERE ZFIVNO  = IT_ZTIVHST1-ZFIVNO
             AND ZFIVDNO = IT_ZTIVHST1-ZFIVDNO.
        IT_ZTIVHST1-ZFREQNO = ZTIVIT-ZFREQNO.
     ENDIF.
*>> 수입의로 타입 PU 제외.
     CLEAR ZTREQHD.
     SELECT   SINGLE *
        FROM  ZTREQHD
        WHERE ZFREQNO = IT_ZTIVHST1-ZFREQNO.
     IF ZTREQHD-ZFREQTY EQ 'PU' .
        DELETE IT_ZTIVHST1 INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 수량.현지통화금액.
     CLEAR EKBE.
     SELECT SINGLE *
       FROM EKBE
      WHERE EBELN  = IT_ZTIVHST1-EBELN
        AND EBELP  = IT_ZTIVHST1-EBELP
        AND MATNR  = IT_ZTIVHST1-MATNR    " 자재번호.
        AND BELNR  = IT_ZTIVHST1-MBLNR    " 자재문서번호.
        AND GJAHR  = IT_ZTIVHST1-MJAHR    " 자재문서연도.
        AND BWART  = IT_ZTIVHST1-BWART    " 이동유형.
        AND BEWTP  = 'E'.                " 이력종류.
     IF SY-SUBRC NE 0.
        DELETE IT_ZTIVHST1 INDEX W_TABIX.
        CONTINUE.
     ENDIF.
     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = IT_ZTIVHST1-BWART.
*>> 이동 유형에 따른 차/대변 지시자.
     IT_ZTIVHST1-DMBTR    =  EKBE-DMBTR / 1000000.
     IT_ZTIVHST1-ZFLASTAM =  ZTREQHD-ZFLASTAM / 1000.
     IT_ZTIVHST1-ZFUSDAM  =  ZTREQHD-ZFUSDAM / 1000.

     IF T156-SHKZG = 'H'.
        IT_ZTIVHST1-DMBTR    =  ( EKBE-DMBTR  ) * -1.
        IT_ZTIVHST1-ZFLASTAM =  ( ZTREQHD-ZFLASTAM ) * -1.
        IT_ZTIVHST1-ZFUSDAM  =  ( ZTREQHD-ZFUSDAM ) * -1.
     ENDIF.

*>> 통화구분.
     IF ZTREQHD-ZFREQTY EQ 'LO'.
         CASE ZTREQHD-WAERS.
           WHEN 'KRW'.
              IT_ZTIVHST1-LOKRW =  IT_ZTIVHST1-DMBTR.   " 현지통화금액
              IT_ZTIVHST1-FLOKRW = IT_ZTIVHST1-ZFUSDAM. " USD 환산금액.
           WHEN OTHERS.
              IT_ZTIVHST1-LOUSD =  IT_ZTIVHST1-DMBTR .
              IT_ZTIVHST1-FLOUSD = IT_ZTIVHST1-ZFUSDAM.
        ENDCASE.
     ELSE.
        IT_ZTIVHST1-IMDMBTR = IT_ZTIVHST1-DMBTR.    " 수입계.
        CASE ZTREQHD-WAERS.
           WHEN 'USD'.
              IT_ZTIVHST1-USDDMBTR = IT_ZTIVHST1-DMBTR.
              IT_ZTIVHST1-FUSDDMBTR = IT_ZTIVHST1-ZFLASTAM.  " 개설금액.
           WHEN 'JPY'.
              IT_ZTIVHST1-JPYDMBTR = IT_ZTIVHST1-DMBTR.
              IT_ZTIVHST1-FJPYDMBTR = IT_ZTIVHST1-ZFLASTAM.
           WHEN 'DEM'.
                IT_ZTIVHST1-DMDMBTR = IT_ZTIVHST1-DMBTR.
                IT_ZTIVHST1-FJPYDMBTR = IT_ZTIVHST1-ZFLASTAM.
           WHEN 'CHF'.
                 IT_ZTIVHST1-SFRDMBTR  = IT_ZTIVHST1-DMBTR.
                 IT_ZTIVHST1-FJPYDMBTR = IT_ZTIVHST1-ZFLASTAM.
           WHEN 'CFP'.
                IT_ZTIVHST1-FFRDMBTR  = IT_ZTIVHST1-DMBTR.
                IT_ZTIVHST1-FFFRDMBTR  = IT_ZTIVHST1-ZFLASTAM.
           WHEN 'GBP'.
                IT_ZTIVHST1-BDPDMBTR  = IT_ZTIVHST1-DMBTR.
                IT_ZTIVHST1-FBDPDMBTR  = IT_ZTIVHST1-ZFLASTAM.
           WHEN 'EUR'.
                IT_ZTIVHST1-EURDMBTR   = IT_ZTIVHST1-DMBTR.
                IT_ZTIVHST1-FEURDMBTR  = IT_ZTIVHST1-ZFLASTAM.
           WHEN OTHERS.
                IT_ZTIVHST1-ETCDMBTR = IT_ZTIVHST1-DMBTR.
       ENDCASE.
    ENDIF.
    MODIFY IT_ZTIVHST1 INDEX W_TABIX.
    MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_TAB1.
    MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_OTOTAL.
    COLLECT IT_TAB1.
    COLLECT IT_OTOTAL.
  ENDLOOP.

ENDFORM.                    " P1000_OLDDAT_BLDAT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
FORM P3000_LINE_TOTAL.

*>> 비율차이.
 DATA : L_USDDMBTR   TYPE P  DECIMALS 2,           " 미국.
        L_JPYDMBTR    TYPE P  DECIMALS 2,           " 일본.
        L_DMDMBTR     TYPE P  DECIMALS 2,           " 독일.
        L_SFRDMBTR    TYPE P  DECIMALS 2,           " 스위스.
        L_FFRDMBTR    TYPE P  DECIMALS 2,           " 프랑스.
        L_BDPDMBTR    TYPE P  DECIMALS 2,           " 영국.
        L_EURDMBTR    TYPE P  DECIMALS 2,           " 영국.
        L_ETCDMBTR    TYPE P  DECIMALS 2,           " 기타.
        L_IMDMBTR     TYPE P  DECIMALS 2,         " 수입현지통화.
        L_LOKRW       TYPE P  DECIMALS 2,
        L_LOUSD       TYPE P  DECIMALS 2.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.

  READ TABLE IT_CTOTAL INDEX 1.

*>> 당년 외화.
  WRITE : / SY-VLINE NO-GAP,(20) '전사계(통화별)' NO-GAP,    " Plant
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FUSDDMBTR
                     CURRENCY 'USD' NO-GAP," 미국
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FJPYDMBTR
                     CURRENCY 'JPY' NO-GAP,"일본
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FDMDMBTR
                     CURRENCY 'DEM' NO-GAP," 독일
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FSFRDMBTR
                     CURRENCY 'CHF' NO-GAP,"'스위스
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FFFRDMBTR
                     CURRENCY 'CFP' NO-GAP,"'프랑스
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FBDPDMBTR
                     CURRENCY 'GBP' NO-GAP,"'영국
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FEURDMBTR
                     CURRENCY 'EUR' NO-GAP,"'영국
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
                     " CURRENCY 'USD' NO-GAP,"기타
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
                     SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FLOKRW
                       CURRENCY 'USD' NO-GAP,
                       SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FLOUSD
                     CURRENCY 'USD' NO-GAP, " $ Local
                       SY-VLINE NO-GAP,(06) ''      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
            SY-VLINE.
*>> 당년 현지통화.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*>> 전사 당년도비율.
  CLEAR: USDDMBTR2,JPYDMBTR2,DMDMBTR2,SFRDMBTR2,FFRDMBTR2,BDPDMBTR2,
         ETCDMBTR2,IMDMBTR2,LOKRW2,LOUSD2.
  IF NOT IT_CTOTAL-DMBTR IS INITIAL.
    USDDMBTR2  =  IT_CTOTAL-USDDMBTR / IT_CTOTAL-DMBTR * 100.  " 미국.
    JPYDMBTR2  =  IT_CTOTAL-JPYDMBTR / IT_CTOTAL-DMBTR * 100.  " 일본.
    DMDMBTR2   =  IT_CTOTAL-DMDMBTR  / IT_CTOTAL-DMBTR * 100.  " 독일.
    SFRDMBTR2  =  IT_CTOTAL-SFRDMBTR / IT_CTOTAL-DMBTR * 100.  " 스위스.
    FFRDMBTR2  =  IT_CTOTAL-FFRDMBTR / IT_CTOTAL-DMBTR * 100.  " 프랑스.
    BDPDMBTR2  =  IT_CTOTAL-BDPDMBTR / IT_CTOTAL-DMBTR * 100.  " 영국.
    EURDMBTR2  =  IT_CTOTAL-EURDMBTR / IT_CTOTAL-DMBTR * 100.  " 영국.
    ETCDMBTR2  =  IT_CTOTAL-ETCDMBTR / IT_CTOTAL-DMBTR * 100.   " 기타.
    IMDMBTR2   =  IT_CTOTAL-IMDMBTR  / IT_CTOTAL-DMBTR * 100. "현지통화.
    LOKRW2     =  IT_CTOTAL-LOKRW    / IT_CTOTAL-DMBTR * 100.
    LOUSD2     =  IT_CTOTAL-LOUSD    / IT_CTOTAL-DMBTR * 100.
  ENDIF.

  WRITE : / SY-VLINE NO-GAP,(20) '전사당년(원화)' NO-GAP," Plant'
            SY-VLINE NO-GAP,(12) IT_CTOTAL-USDDMBTR
                     CURRENCY 'KRW' NO-GAP," 미국'
                     SY-VLINE NO-GAP,(06) USDDMBTR2  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-JPYDMBTR
                     CURRENCY 'KRW' NO-GAP,"일본'
                     SY-VLINE NO-GAP,(06) JPYDMBTR2   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-DMDMBTR
                     CURRENCY 'KRW' NO-GAP," 독일'
                     SY-VLINE NO-GAP,(06) DMDMBTR2   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-SFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'스위스'
                     SY-VLINE NO-GAP,(06) SFRDMBTR2 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-FFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'프랑스'
                     SY-VLINE NO-GAP,(06) FFRDMBTR2 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-BDPDMBTR
                     CURRENCY 'KRW' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) BDPDMBTR2 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-EURDMBTR
                     CURRENCY 'KRW' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) EURDMBTR2 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-ETCDMBTR
                     CURRENCY 'KRW' NO-GAP,"기타'
                     SY-VLINE NO-GAP,(06) ETCDMBTR2  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-IMDMBTR
                     CURRENCY 'KRW' NO-GAP, " 수입계.
                     SY-VLINE NO-GAP,(06) IMDMBTR2 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-LOKRW
                     CURRENCY 'KRW' NO-GAP,
                     SY-VLINE NO-GAP,(06) LOKRW2 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-LOUSD
                     CURRENCY 'KRW' NO-GAP, " $ Local
                     SY-VLINE NO-GAP,(06)LOUSD2  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_CTOTAL-DMBTR
                     CURRENCY 'KRW' NO-GAP,"토탈'
            SY-VLINE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  READ TABLE IT_OTOTAL INDEX 1.
*>> 전사 전년도비율.
  CLEAR: USDDMBTR3,JPYDMBTR3,DMDMBTR3,SFRDMBTR3,FFRDMBTR3,BDPDMBTR3,
         ETCDMBTR3,IMDMBTR3,LOKRW3,LOUSD3.
  IF NOT IT_OTOTAL-DMBTR IS INITIAL.
    USDDMBTR3  =  IT_OTOTAL-USDDMBTR / IT_OTOTAL-DMBTR * 100.  " 미국.
    JPYDMBTR3  =  IT_OTOTAL-JPYDMBTR / IT_OTOTAL-DMBTR * 100.  " 일본.
    DMDMBTR3   =  IT_OTOTAL-DMDMBTR  / IT_OTOTAL-DMBTR * 100.  " 독일.
    SFRDMBTR3  =  IT_OTOTAL-SFRDMBTR / IT_OTOTAL-DMBTR * 100.  " 스위스.
    FFRDMBTR3  =  IT_OTOTAL-FFRDMBTR / IT_OTOTAL-DMBTR * 100.  " 프랑스.
    BDPDMBTR3  =  IT_OTOTAL-BDPDMBTR / IT_OTOTAL-DMBTR * 100.  " 영국.
    EURDMBTR3  =  IT_OTOTAL-EURDMBTR / IT_OTOTAL-DMBTR * 100.   " 유로.
    ETCDMBTR3  =  IT_OTOTAL-ETCDMBTR / IT_OTOTAL-DMBTR * 100.   " 기타.
    IMDMBTR3   =  IT_OTOTAL-IMDMBTR  / IT_OTOTAL-DMBTR * 100. "현지통화.
    LOKRW3     =  IT_OTOTAL-LOKRW    / IT_OTOTAL-DMBTR * 100.
    LOUSD3     =  IT_OTOTAL-LOUSD    / IT_OTOTAL-DMBTR * 100.
  ENDIF.

  WRITE : / SY-VLINE NO-GAP,(20) '전사전년(원화)' NO-GAP,    " Plant'
            SY-VLINE NO-GAP,(12) IT_OTOTAL-USDDMBTR
                     CURRENCY 'KRW' NO-GAP," 미국'
                     SY-VLINE NO-GAP,(06) USDDMBTR3 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-JPYDMBTR
                     CURRENCY 'KRW' NO-GAP,"일본'
                     SY-VLINE NO-GAP,(06) JPYDMBTR3 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-DMDMBTR
                     CURRENCY 'KRW' NO-GAP," 독일'
                     SY-VLINE NO-GAP,(06) DMDMBTR3 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-SFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'스위스'
                     SY-VLINE NO-GAP,(06) SFRDMBTR3 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-FFRDMBTR
                     CURRENCY 'KRW' NO-GAP,"'프랑스'
                     SY-VLINE NO-GAP,(06) FFRDMBTR3 NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-BDPDMBTR
                     CURRENCY 'KRW' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) BDPDMBTR3  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-EURDMBTR
                     CURRENCY 'KRW' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) EURDMBTR3  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-ETCDMBTR
                     CURRENCY 'KRW' NO-GAP,"기타'
                     SY-VLINE NO-GAP,(06) ETCDMBTR3  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-IMDMBTR
                     CURRENCY 'KRW' NO-GAP, " 수입계.
                     SY-VLINE NO-GAP,(06) IMDMBTR3    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-LOKRW
                     CURRENCY 'KRW' NO-GAP,
                     SY-VLINE NO-GAP,(06) LOKRW3   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-LOUSD
                     CURRENCY 'KRW' NO-GAP, " $ Local '
                     SY-VLINE NO-GAP,(06) LOUSD3  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) IT_OTOTAL-DMBTR
                     CURRENCY 'KRW' NO-GAP,"토탈'
            SY-VLINE.
 L_USDDMBTR  =  USDDMBTR2 -  USDDMBTR3.           " 미국.
 L_JPYDMBTR  =  JPYDMBTR2 -  JPYDMBTR3.       " 일본.
 L_DMDMBTR   =  DMDMBTR2 -   DMDMBTR3.        " 독일.
 L_SFRDMBTR  =  SFRDMBTR2 -  SFRDMBTR3.       "위스.
 L_FFRDMBTR  =  FFRDMBTR2 -  FFRDMBTR3.    " 프랑스.
 L_BDPDMBTR  =  BDPDMBTR2 -  BDPDMBTR3.    " 영국
 L_EURDMBTR  =  EURDMBTR2 -  EURDMBTR3.    " 유로.
 L_ETCDMBTR  =  ETCDMBTR2 -  ETCDMBTR3.   " 기타.
 L_IMDMBTR   =  IMDMBTR2  -  IMDMBTR3.  " 수입현지통화.
 L_LOKRW     =  LOKRW2 -     LOKRW3.
 L_LOUSD     =  LOUSD2 -     LOUSD3.

 FORMAT RESET.
 FORMAT COLOR COL_GROUP INTENSIFIED OFF.
*>> 당년도 /전년도 비율차이.
 WRITE : / SY-VLINE NO-GAP,(20) ' 'NO-GAP, " Plant'
            SY-VLINE NO-GAP,(12) '' NO-GAP," 미국'
                     SY-VLINE NO-GAP,(06) L_USDDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) ''NO-GAP,"일본'
                     SY-VLINE NO-GAP,(06) L_JPYDMBTR NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP," 독일'
                     SY-VLINE NO-GAP,(06) L_DMDMBTR NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'스위스'
                    SY-VLINE NO-GAP,(06) L_SFRDMBTR    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'프랑스'
                     SY-VLINE NO-GAP,(06) L_FFRDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'영국'
                     SY-VLINE NO-GAP,(06) L_BDPDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"'유로'
                     SY-VLINE NO-GAP,(06) L_EURDMBTR  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,"기타'
                     SY-VLINE NO-GAP,(06) L_ETCDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP, " 수입계.
                     SY-VLINE NO-GAP,(06) L_IMDMBTR   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
                     SY-VLINE NO-GAP,(06) L_LOKRW    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) ''NO-GAP, " $ Local '
                     SY-VLINE NO-GAP,(06) L_LOUSD  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(12) '' NO-GAP,
            SY-VLINE.

   WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LINE_TOTAL
