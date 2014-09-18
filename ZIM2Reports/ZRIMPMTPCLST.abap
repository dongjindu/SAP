*&---------------------------------------------------------------------
*& Report  ZRIMPMTCPCLST
*&---------------------------------------------------------------------
*&  프로그램명 : 거래조건별 구매실적표
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.23
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMPMTPCLST  MESSAGE-ID ZIM
                     LINE-SIZE 215
                     NO STANDARD PAGE HEADING.
TABLES: ZTIVHST,ZTIVHSTIT,ZTIVIT,ZTREQHD,EKKO,EKBE,EKPO,LFA1,ZTREQORJ,
        T156, T001W,T005T.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------

DATA : BEGIN OF IT_ZTIVHST OCCURS 0,
       ZFIVNO    LIKE  ZTIVHST-ZFIVNO,	" 통관요청/입고요청 관리번호.
       ZFIVHST   LIKE  ZTIVHST-ZFIVHST,	" 입고순번.
       WERKS     LIKE  ZTIVHSTIT-WERKS,   " 플랜트.
       ZFGRST	   LIKE  ZTIVHST-ZFGRST,    " Good Receipt 상태.
       ZFCIVHST  LIKE  ZTIVHST-ZFCIVHST,  " Verify 순번.
       BLDAT	   LIKE  ZTIVHST-BLDAT,     " 전표내 증빙일.
       SHKZG	   LIKE  ZTIVHST-SHKZG,     " 차변/대변 지시자.
       MBLNR     LIKE  ZTIVHST-MBLNR,     " 자재문서번호.
       MJAHR     LIKE  ZTIVHST-MJAHR,     " 자재문서연도.
       BWART	   LIKE  ZTIVHST-BWART,     " 이동유형 (재고관리).
       ZFREQNO   LIKE  ZTREQHD-ZFREQNO,   " 수입의뢰관리번호.
       INCO1     LIKE  ZTREQHD-INCO1,     " 가격조건
       EBELN	   LIKE  ZTIVHSTIT-EBELN,   " 구매문서번호
       ZFIVDNO   LIKE   ZTIVHSTIT-ZFIVDNO," 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG#    LIKE  EKKO-EKORG,        " 구매조직.
       LCDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR3  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DPDMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DADMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       ETCDMBTR  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST.
*>> 당년도 PLANT TOTAL.
DATA : BEGIN OF IT_TAB OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS, " 플랜트.
       LCDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR3  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DPDMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DADMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       ETCDMBTR  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화.
DATA : END OF IT_TAB.
*>> 전년도 PALNT TOTAL.
DATA : BEGIN OF IT_TEMP OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS, " 플랜트.
       LCDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR3  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DPDMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DADMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       ETCDMBTR  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화.
DATA : END OF IT_TEMP.

*>> 당년전사TOTAL.
DATA : BEGIN OF IT_CTOTAL OCCURS 0,
       LCDMBTR1   LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR2   LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR3   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DPDMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DADMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       ETCDMBTR  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화.
DATA : END OF IT_CTOTAL.

*>> 전년전사TOTAL.
DATA : BEGIN OF IT_OTOTAL OCCURS 0,
       LCDMBTR1   LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR2   LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR3   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DPDMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DADMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       ETCDMBTR  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화.
DATA : END OF IT_OTOTAL.

DATA : BEGIN OF IT_ZTIVHST1 OCCURS 0,
       ZFIVNO    LIKE  ZTIVHST-ZFIVNO,	" 통관요청/입고요청 관리번호.
       ZFIVHST   LIKE  ZTIVHST-ZFIVHST,	" 입고순번.
       WERKS     LIKE  ZTIVHSTIT-WERKS,   " 플랜트.
       ZFGRST	   LIKE  ZTIVHST-ZFGRST,    " Good Receipt 상태.
       ZFCIVHST  LIKE  ZTIVHST-ZFCIVHST,  " Verify 순번.
       BLDAT	   LIKE  ZTIVHST-BLDAT,     " 전표내 증빙일.
       SHKZG	   LIKE  ZTIVHST-SHKZG,     " 차변/대변 지시자.
       MBLNR     LIKE  ZTIVHST-MBLNR,     " 자재문서번호.
       MJAHR     LIKE  ZTIVHST-MJAHR,     " 자재문서연도.
       BWART	   LIKE  ZTIVHST-BWART,     " 이동유형 (재고관리).
       ZFREQNO   LIKE  ZTREQHD-ZFREQNO,    " 수입의뢰관리번호.
       EBELN	   LIKE  ZTIVHSTIT-EBELN,    " 구매문서번호
       ZFIVDNO   LIKE   ZTIVHSTIT-ZFIVDNO,  " 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       LCDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       LCDMBTR3  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DPDMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       DADMBTR   LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR1  LIKE  EKBE-DMBTR,      " 현지통화금액.
       TTDMBTR2  LIKE  EKBE-DMBTR,      " 현지통화금액.
       ETCDMBTR  LIKE  EKBE-DMBTR,      " 현지통화금액.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST1.

DATA :  W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_LCRATE1     TYPE P DECIMALS 2,
        W_LCRATE2     TYPE P DECIMALS 2,
        W_LCRATE3     TYPE P DECIMALS 2,
        W_DPRATE      TYPE P DECIMALS 2,
        W_DARATE      TYPE P DECIMALS 2,
        W_TTRATE1     TYPE P DECIMALS 2, " 사전.
        W_TTRATE2     TYPE P DECIMALS 2, " 사후.
        W_ETCRATE     TYPE P DECIMALS 2,
        W_ALLLCRATE1  TYPE P DECIMALS 2,
        W_ALLLCRATE2  TYPE P DECIMALS 2,
        W_ALLLCRATE3  TYPE P DECIMALS 2,
        W_ALLDPRATE   TYPE P DECIMALS 2,
        W_ALLDARATE   TYPE P DECIMALS 2,
        W_ALLTTRATE1  TYPE P DECIMALS 2,
        W_ALLTTRATE2  TYPE P DECIMALS 2,
        W_ALLETCRATE  TYPE P DECIMALS 2,
        W_TO(04),
        W_FROM(04),
        ST_DMBTR      LIKE  EKBE-DMBTR,        " 전년도총액.
        ST_BAMNG      LIKE  EKBE-BAMNG,        " 전년도수량.
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_CHEK,
        W_TABIX       LIKE SY-TABIX,
        W_PAGE        TYPE I,
        W_LIST_INDEX  LIKE SY-TABIX.
RANGES: R_BLDAT FOR ZTIVHST-BLDAT OCCURS 10.

INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR  EKKO-BUKRS NO-EXTENSION NO INTERVALS,
                   S_BLDAT	 FOR  ZTIVHST-BLDAT OBLIGATORY,
                   S_WERKS    FOR   ZTIVHSTIT-WERKS NO INTERVALS," 플랜
                   S_EKORG	 FOR  EKKO-EKORG,        " 구매조직.
                   S_EKGRP  FOR  EKKO-EKGRP.
 SELECTION-SCREEN END OF BLOCK B1.
 INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.
*title Text Write
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
  WRITE:/95'[거래조건별 구매실적표]' COLOR COL_HEADING INTENSIFIED OFF.

  WRITE:/ '당년:',S_BLDAT-LOW,'-', S_BLDAT-HIGH.

  WRITE:/ '전년:',R_BLDAT-LOW, '-', R_BLDAT-HIGH.

  WRITE:/ '금액: 천원'.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(20) 'Plant' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) 'L/C At Sight'   NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) 'L/C B/Usance' NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) 'L/C S/Usance' NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) 'D/P'      NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) 'D/A'      NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '사전 T/T' NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '사후 T/T' NO-GAP CENTERED,
                            (06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(24) 'Total'    NO-GAP CENTERED,
            215 SY-VLINE.
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
           AND  R~BLDAT   IN S_BLDAT.          " 증빙일.
  PERFORM P1000_CURRENCT_BLDAT.
*>> 전년도.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVHST1
            FROM ZTIVHST AS R INNER JOIN ZTIVHSTIT AS I
             ON R~ZFIVNO  = I~ZFIVNO
            AND R~ZFIVHST = I~ZFIVHST
         WHERE  I~ZFGRST  = 'Y'
           AND  I~WERKS   IN S_WERKS
           AND  R~BLDAT   IN R_BLDAT.          " 증빙일.
  PERFORM P1000_OLDDAT_BLDAT.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SORT IT_TAB BY WERKS.
   SET TITLEBAR  'ZIMR31'.
   SET PF-STATUS 'ZIMR31'.
   CLEAR W_COUNT.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM   P3000_LINE_WRITE.
      WRITE:/ SY-ULINE.
   ENDLOOP.
*>> 전사.
   PERFORM P3000_LINE_TOTAL.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR31'.

  MOVE : 'I'               TO  S_BLDAT-SIGN,
         'BT'              TO  S_BLDAT-OPTION,
         SY-DATUM          TO  S_BLDAT-HIGH.
  CONCATENATE SY-DATUM(4) '01' '01' INTO S_BLDAT-LOW.

  APPEND S_BLDAT.

*   MOVE : 'I'               TO  S_WERKS-SIGN,
*          'EQ'              TO  S_WERKS-OPTION,
*           ' '              TO  S_WERKS-HIGH,
*           ' '              TO  S_WERKS-LOW.
*
*  APPEND S_WERKS.


ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  CLEAR T001W.
  SELECT SINGLE *
         FROM T001W
        WHERE WERKS = IT_TAB-WERKS.

  FORMAT RESET.
*>> 전년도 리더 데이타.
  READ TABLE IT_TEMP WITH KEY WERKS = IT_TAB-WERKS.
  IF NOT IT_TAB-DMBTR IS INITIAL.
     W_LCRATE1 =  IT_TAB-LCDMBTR1 / IT_TAB-DMBTR * 100.
     W_LCRATE2 =  IT_TAB-LCDMBTR2 / IT_TAB-DMBTR * 100.
     W_LCRATE3 =  IT_TAB-LCDMBTR3 / IT_TAB-DMBTR * 100.
     W_DPRATE =   IT_TAB-DPDMBTR / IT_TAB-DMBTR * 100.
     W_DARATE =   IT_TAB-DADMBTR / IT_TAB-DMBTR * 100.
     W_TTRATE1 =  IT_TAB-TTDMBTR1 / IT_TAB-DMBTR * 100.
     W_TTRATE2 =  IT_TAB-TTDMBTR2 / IT_TAB-DMBTR * 100.
     W_ETCRATE =  IT_TAB-ETCDMBTR / IT_TAB-DMBTR * 100.
  ENDIF.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/  SY-VLINE NO-GAP,(20) IT_TAB-WERKS NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-LCDMBTR1 CURRENCY 'KRW' NO-GAP,
                           (06) W_LCRATE1 NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-LCDMBTR2 CURRENCY 'KRW' NO-GAP,
                           (06) W_LCRATE2 NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-LCDMBTR3 CURRENCY 'KRW' NO-GAP,
                           (06) W_LCRATE3 NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-DPDMBTR CURRENCY 'KRW' NO-GAP,
                           (06) W_DPRATE NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-DADMBTR CURRENCY 'KRW' NO-GAP,
                           (06)  W_DARATE NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-TTDMBTR1 CURRENCY 'KRW' NO-GAP,
                           (06) W_TTRATE1 NO-GAP,
           SY-VLINE NO-GAP,(17) IT_TAB-TTDMBTR2 CURRENCY 'KRW' NO-GAP,
                           (06)  W_TTRATE2 NO-GAP,
*           SY-VLINE NO-GAP,(17) IT_TAB-ETCDMBTR CURRENCY 'KRW' NO-GAP,
*                           (06)  W_ETCRATE NO-GAP,
           SY-VLINE NO-GAP,(24) IT_TAB-DMBTR CURRENCY 'KRW' NO-GAP,
           215 SY-VLINE.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,(20) T001W-NAME1 NO-GAP,
          SY-VLINE NO-GAP,(17)IT_TEMP-LCDMBTR1 CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(17)IT_TEMP-LCDMBTR2 CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(17)IT_TEMP-LCDMBTR3 CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_TEMP-DPDMBTR CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_TEMP-DADMBTR CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_TEMP-TTDMBTR1 CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_TEMP-TTDMBTR2 CURRENCY 'KRW' NO-GAP,
                          (06)'' NO-GAP,
*          SY-VLINE NO-GAP,(17) IT_TEMP-ETCDMBTR CURRENCY 'KRW' NO-GAP,
*                          (06)'' NO-GAP,
          SY-VLINE NO-GAP,(24) IT_TEMP-DMBTR CURRENCY 'KRW' NO-GAP,
          215 SY-VLINE.

ENDFORM.

*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL.

  READ TABLE IT_CTOTAL INDEX 1.
  READ TABLE IT_OTOTAL INDEX 1.
  IF SY-SUBRC EQ 0.
     W_CHEK = 'Y'.
  ENDIF.
  IF NOT IT_CTOTAL-DMBTR IS INITIAL.
     W_ALLLCRATE1 =  IT_CTOTAL-LCDMBTR1 / IT_CTOTAL-DMBTR * 100.
     W_ALLLCRATE2 =  IT_CTOTAL-LCDMBTR2 / IT_CTOTAL-DMBTR * 100.
     W_ALLLCRATE3 =  IT_CTOTAL-LCDMBTR3 / IT_CTOTAL-DMBTR * 100.
     W_ALLDPRATE =  IT_CTOTAL-DPDMBTR   / IT_CTOTAL-DMBTR * 100.
     W_ALLDARATE =  IT_CTOTAL-DADMBTR   / IT_CTOTAL-DMBTR * 100.
     W_ALLTTRATE1 =  IT_CTOTAL-TTDMBTR1 / IT_CTOTAL-DMBTR * 100.
     W_ALLTTRATE2 =  IT_CTOTAL-TTDMBTR2 / IT_CTOTAL-DMBTR * 100.
     W_ALLETCRATE =  IT_CTOTAL-ETCDMBTR / IT_CTOTAL-DMBTR * 100.
  ENDIF.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/ SY-VLINE NO-GAP,(20) '전사' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-LCDMBTR1 CURRENCY 'KRW'
          NO-GAP, (06)  W_ALLLCRATE1 NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-LCDMBTR2 CURRENCY 'KRW'
          NO-GAP, (06)  W_ALLLCRATE2 NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-LCDMBTR3 CURRENCY 'KRW'
          NO-GAP, (06)  W_ALLLCRATE3 NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-DPDMBTR CURRENCY 'KRW'
          NO-GAP, (06) W_ALLDPRATE NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-DADMBTR CURRENCY 'KRW'
          NO-GAP, (06) W_ALLDARATE NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-TTDMBTR1 CURRENCY 'KRW'
          NO-GAP, (06) W_ALLTTRATE1 NO-GAP,
          SY-VLINE NO-GAP,(17) IT_CTOTAL-TTDMBTR2 CURRENCY 'KRW'
          NO-GAP, (06)  W_ALLTTRATE2 NO-GAP,
*          SY-VLINE NO-GAP,(17) IT_CTOTAL-ETCDMBTR
*                               CURRENCY 'KRW' NO-GAP,
*          (06)  W_ALLETCRATE NO-GAP,
          SY-VLINE NO-GAP,(24) IT_CTOTAL-DMBTR
                               CURRENCY 'KRW' NO-GAP,
          215 SY-VLINE.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,(20) '' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_OTOTAL-LCDMBTR1 CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
           SY-VLINE NO-GAP,(17) IT_OTOTAL-LCDMBTR2 CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_OTOTAL-LCDMBTR3 CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_OTOTAL-DPDMBTR CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_OTOTAL-DADMBTR CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_OTOTAL-TTDMBTR1 CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
          SY-VLINE NO-GAP,(17) IT_OTOTAL-TTDMBTR2 CURRENCY 'KRW'
          NO-GAP,
          (06)  '' NO-GAP,
          SY-VLINE NO-GAP,(24) IT_OTOTAL-DMBTR CURRENCY 'KRW'
          NO-GAP,
          215 SY-VLINE.
   WRITE:/ SY-ULINE.
ENDFORM.                    " P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*
*&      Form  P2000_DISP_ZTIDS
*&---------------------------------------------------------------------
*
FORM P2000_DISP_ZTIDS USING    P_ZFIDRNO.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD ''.
  SET PARAMETER ID 'ZPCLSEQ' FIELD ''.
  SET PARAMETER ID 'ZPIDRNO' FIELD P_ZFIDRNO.
  CALL TRANSACTION 'ZIM76' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTIDS
*&---------------------------------------------------------------------
*&      Form  P1000_CURRENCT_BLDAT
*&---------------------------------------------------------------------
FORM P1000_CURRENCT_BLDAT.

  LOOP AT IT_ZTIVHST.
     W_TABIX = SY-TABIX.
*>> GET ZFREQNO.
     CLEAR ZTIVIT.
     SELECT   SINGLE *
        FROM  ZTIVIT
        WHERE ZFIVNO  = IT_ZTIVHST-ZFIVNO
          AND ZFIVDNO = IT_ZTIVHST-ZFIVDNO.
*>> 수입의로 타입 PU.LO 제외.
     CLEAR ZTREQHD.
     SELECT   SINGLE *
        FROM  ZTREQHD
        WHERE ZFREQNO = ZTIVIT-ZFREQNO.

     IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO'.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 오퍼 체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  =  IT_ZTIVHST-EBELN
          AND BUKRS IN  S_BUKRS
          AND EKORG	IN S_EKORG        " 구매조직.
          AND EKGRP  IN S_EKGRP.
     IF SY-SUBRC NE 0.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 수량.현지통화금액.
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

*>> 단위.
     CLEAR EKPO.
     SELECT  SINGLE *
        FROM EKPO
       WHERE EBELN  =  IT_ZTIVHST-EBELN
         AND WERKS  =  IT_ZTIVHST-WERKS
         AND EBELP  =  IT_ZTIVHST-EBELP
         AND MATNR  =  IT_ZTIVHST-MATNR.    " 자재번호.
     IF SY-SUBRC NE 0.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.
     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = IT_ZTIVHST-BWART.
*>> 이동 유형에 따른 차/대변 지시자.
     DATA: W_DMBTR   LIKE EKBE-DMBTR.

     W_DMBTR    =  EKBE-DMBTR / 1000.
     IF T156-SHKZG = 'H'.
        W_DMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.
     IT_ZTIVHST-DMBTR =  W_DMBTR.
     CASE ZTREQHD-ZFREQTY.
       WHEN 'LC'.
          IF ZTREQHD-ZFLCKN = '1'.
             MOVE W_DMBTR  TO  IT_ZTIVHST-LCDMBTR1.
          ENDIF.
          IF ZTREQHD-ZFLCKN = '2'.
             MOVE W_DMBTR  TO  IT_ZTIVHST-LCDMBTR2.
          ENDIF.
          IF ZTREQHD-ZFLCKN = '3'.
             MOVE W_DMBTR  TO  IT_ZTIVHST-LCDMBTR3.
          ENDIF.
       WHEN 'DP'.
          MOVE W_DMBTR  TO  IT_ZTIVHST-DPDMBTR.
       WHEN 'DA'.
          MOVE W_DMBTR  TO  IT_ZTIVHST-DADMBTR.
       WHEN 'TT'.
          IF ZTREQHD-ZFLCKN = '6'.
             MOVE W_DMBTR  TO  IT_ZTIVHST-TTDMBTR1.
          ENDIF.
          IF ZTREQHD-ZFLCKN = '7'.
             MOVE W_DMBTR  TO  IT_ZTIVHST-TTDMBTR2.
          ENDIF.
       WHEN OTHERS.
          MOVE W_DMBTR  TO  IT_ZTIVHST-ETCDMBTR.
     ENDCASE.
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
*>> GET ZFREQNO.
     CLEAR ZTIVIT.
     SELECT   SINGLE *
        FROM  ZTIVIT
        WHERE ZFIVNO  = IT_ZTIVHST1-ZFIVNO
          AND ZFIVDNO = IT_ZTIVHST1-ZFIVDNO.

*>> 수입의로 타입 PU.LO 제외.
     CLEAR ZTREQHD.
     SELECT   SINGLE *
        FROM  ZTREQHD
        WHERE ZFREQNO = ZTIVIT-ZFREQNO.
     IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO'.
        DELETE IT_ZTIVHST1 INDEX W_TABIX.
        CONTINUE.
     ENDIF.

*>> 오퍼,구매조직 체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  = IT_ZTIVHST1-EBELN
          AND BUKRS IN  S_BUKRS
          AND EKORG	IN S_EKORG        " 구매조직.
          AND EKGRP  IN S_EKGRP.
     IF SY-SUBRC NE 0.
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
*>> 단위.
     CLEAR EKPO.
     SELECT  SINGLE *
        FROM EKPO
       WHERE EBELN =  IT_ZTIVHST1-EBELN
         AND EBELP  =  IT_ZTIVHST1-EBELP
         AND MATNR  =  IT_ZTIVHST1-MATNR.    " 자재번호.

     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = IT_ZTIVHST1-BWART.
*>> 이동 유형에 따른 차/대변 지시자.
     DATA: W_DMBTR   LIKE EKBE-DMBTR.

     W_DMBTR    =  EKBE-DMBTR / 1000.
     IF T156-SHKZG = 'H'.
        W_DMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.
    IT_ZTIVHST1-DMBTR =  W_DMBTR.
     CASE ZTREQHD-ZFREQTY.
       WHEN 'LC'.
          IF ZTREQHD-ZFLCKN = '1'.
             MOVE W_DMBTR  TO  IT_ZTIVHST1-LCDMBTR1.
          ENDIF.
          IF ZTREQHD-ZFLCKN = '2'.
             MOVE W_DMBTR  TO  IT_ZTIVHST1-LCDMBTR2.
          ENDIF.
          IF ZTREQHD-ZFLCKN = '3'.
             MOVE W_DMBTR  TO  IT_ZTIVHST1-LCDMBTR3.
          ENDIF.
       WHEN 'DP'.
          MOVE W_DMBTR  TO  IT_ZTIVHST1-DPDMBTR.
       WHEN 'DA'.
          MOVE W_DMBTR  TO  IT_ZTIVHST1-DADMBTR.
       WHEN 'TT'.
          IF ZTREQHD-ZFLCKN = '6'.   " 사전.
             MOVE W_DMBTR  TO  IT_ZTIVHST1-TTDMBTR1.
          ENDIF.
          IF ZTREQHD-ZFLCKN = '7'.   " 사후.
             MOVE W_DMBTR  TO  IT_ZTIVHST1-TTDMBTR2.
          ENDIF.
       WHEN OTHERS.
          MOVE W_DMBTR  TO  IT_ZTIVHST-ETCDMBTR.
     ENDCASE.
     MODIFY IT_ZTIVHST1 INDEX W_TABIX.
     MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_TEMP.
     MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_OTOTAL.
     COLLECT IT_TEMP.
     COLLECT IT_OTOTAL.
  ENDLOOP.

ENDFORM.                    " P1000_OLDDAT_BLDAT
