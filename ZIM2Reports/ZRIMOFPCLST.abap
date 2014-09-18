*&---------------------------------------------------------------------
*& Report  ZRIMOFPCLST
*&---------------------------------------------------------------------
*&  프로그램명 : 오퍼별 구매 실적표
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.23
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMOFPCLST  MESSAGE-ID ZIM
                     LINE-SIZE 143
                     NO STANDARD PAGE HEADING.
TABLES: ZTIVHST, ZTIVHSTIT,ZTIVIT,ZTREQHD,EKKO,EKBE,EKPO,LFA1,T156.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
*>> item.
DATA : BEGIN OF IT_ZTIVHST OCCURS 0,
       ZFIVNO    LIKE  ZTIVHST-ZFIVNO,	" 통관요청/입고요청 관리번호.
       ZFIVHST   LIKE  ZTIVHST-ZFIVHST,	" 입고순번.
       ZFGRST	   LIKE  ZTIVHST-ZFGRST,    " Good Receipt 상태.
       ZFCIVHST  LIKE  ZTIVHST-ZFCIVHST,  " Verify 순번.
       BLDAT	   LIKE  ZTIVHST-BLDAT,     " 전표내 증빙일.
       SHKZG	   LIKE  ZTIVHST-SHKZG,     " 차변/대변 지시자.
       MBLNR     LIKE  ZTIVHST-MBLNR,     " 자재문서번호.
       MJAHR     LIKE  ZTIVHST-MJAHR,     " 자재문서연도.
       BWART	   LIKE  ZTIVHST-BWART,     " 이동유형 (재고관리).
       ZFREQNO   LIKE  ZTREQHD-ZFREQNO,   " 수입의뢰관리번호.
       EBELN	   LIKE  ZTIVHSTIT-EBELN,   " 구매문서번호
       ZFIVDNO  LIKE   ZTIVHSTIT-ZFIVDNO,  " 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       ST_DMBTR  LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST.
*>> item.
DATA : BEGIN OF IT_TAB OCCURS 0,
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       MEINS     LIKE  EKPO-MEINS,        " 수량단위.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화.
DATA : END OF IT_TAB.
*>> 헤더.
DATA : BEGIN OF IT_TAB1 OCCURS 0,
       W_SEQ   TYPE I,
       DMBTR   LIKE  EKBE-DMBTR,         " 현지통화.
       LLIEF   LIKE  EKKO-LLIEF,         " 오퍼.
       NAME1   LIKE  LFA1-NAME1.
DATA : END OF IT_TAB1.

DATA : BEGIN OF IT_ZTIVHST1 OCCURS 0,
       ZFIVNO    LIKE  ZTIVHST-ZFIVNO,	" 통관요청/입고요청 관리번호.
       ZFIVHST   LIKE  ZTIVHST-ZFIVHST,	" 입고순번.
       ZFGRST	   LIKE  ZTIVHST-ZFGRST,    " Good Receipt 상태.
       ZFCIVHST  LIKE  ZTIVHST-ZFCIVHST,  " Verify 순번.
       BLDAT	   LIKE  ZTIVHST-BLDAT,     " 전표내 증빙일.
       SHKZG	   LIKE  ZTIVHST-SHKZG,     " 차변/대변 지시자.
       MBLNR     LIKE  ZTIVHST-MBLNR,     " 자재문서번호.
       MJAHR     LIKE  ZTIVHST-MJAHR,     " 자재문서연도.
       BWART	   LIKE  ZTIVHST-BWART,     " 이동유형 (재고관리).
       ZFREQNO   LIKE  ZTREQHD-ZFREQNO,   " 수입의뢰관리번호.
       EBELN	   LIKE  ZTIVHSTIT-EBELN,   " 구매문서번호
       ZFIVDNO  LIKE   ZTIVHSTIT-ZFIVDNO,  " 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       ST_DMBTR  LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST1.

DATA : BEGIN OF IT_TEMP OCCURS 0,
       DMBTR   LIKE  EKBE-DMBTR,          " 현지통화.
       LLIEF   LIKE  EKKO-LLIEF.        " 오퍼.
DATA : END OF IT_TEMP.


DATA :  W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_TO(04),
        W_FROM(04),
        ST_DMBTR      LIKE  EKBE-DMBTR,        " 당년도총액.
        OLDST_DMBTR   LIKE  EKBE-DMBTR,        " 전년도총액.
        W_RATE(06)    TYPE P DECIMALS 2,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_CHEK,
        W_TABIX       LIKE SY-TABIX,
        W_LIST_INDEX  LIKE SY-TABIX.
RANGES: R_BLDAT FOR ZTIVHST-BLDAT OCCURS 05.
INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS FOR  EKKO-BUKRS NO-EXTENSION NO INTERVALS,
                   S_BLDAT	FOR  ZTIVHST-BLDAT OBLIGATORY,
                   S_LLIEF FOR  EKKO-LLIEF,        " 오퍼.
                   S_EKGRP FOR  EKKO-EKGRP,
                   S_EKORG	FOR  EKKO-EKORG.        " 구매조직.
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
*>> 금액별 순위.
   PERFORM  P2000_TOP_END_IT_TAB.
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
  CLEAR: IT_TAB, W_TABIX.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 1.
  WRITE:/64'[오퍼별 구매 실적표]' COLOR COL_HEADING INTENSIFIED OFF.

  WRITE:/ SY-DATUM,'전사',
           132 '금액: 천원'.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(04) '순위'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(30) '거래선'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '금  액'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '비율'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(10) '자재코드' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(24) '품   명'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(06) '단위'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(15) '수량'     NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '금  액'   NO-GAP CENTERED,
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
  IF W_TO NE 0.
      W_ERR_CHK = 'S'.
      EXIT.
  ENDIF.
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
           AND  R~BLDAT   IN S_BLDAT.          " 증빙일.
  PERFORM P1000_CURRENCT_BLDAT.
*>> 전년도.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTIVHST1
            FROM ZTIVHST AS R INNER JOIN ZTIVHSTIT AS I
             ON R~ZFIVNO  = I~ZFIVNO
            AND R~ZFIVHST = I~ZFIVHST
         WHERE  I~ZFGRST  = 'Y'
           AND  R~BLDAT   IN R_BLDAT.          " 증빙일.
  PERFORM P1000_OLDDAT_BLDAT.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SORT IT_TAB1 BY W_SEQ.
   SET TITLEBAR  'ZIMR23'.
   SET PF-STATUS 'ZIMR23'.
   CLEAR W_COUNT.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   LOOP AT IT_TAB1.
      W_TABIX = SY-TABIX.
      PERFORM   P3000_LINE_WRITE.
      LOOP AT IT_TAB WHERE LLIEF = IT_TAB1-LLIEF.
         PERFORM  P3000_SUB_TOTOL_WRITE.
      ENDLOOP.
      AT LAST.
         PERFORM P3000_LINE_TOTAL.
      ENDAT.
   ENDLOOP.
   PERFORM P3000_LINE_TOTAL1.

   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR23'.

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

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  CLEAR  IT_TEMP-DMBTR.
  READ TABLE IT_TEMP WITH KEY LLIEF = IT_TAB1-LLIEF.
  IF SY-SUBRC EQ 0.
     W_CHEK = 'Y'.
  ENDIF.
   READ TABLE IT_TAB WITH KEY LLIEF = IT_TAB1-LLIEF.
  IF NOT ST_DMBTR IS INITIAL.
     W_RATE = IT_TAB1-DMBTR / ST_DMBTR  * 100.
  ENDIF.
  WRITE:/ SY-VLINE NO-GAP,(04) IT_TAB1-W_SEQ NO-GAP,
          SY-VLINE NO-GAP,(10) IT_TAB1-LLIEF NO-GAP,
                          (20) IT_TAB1-NAME1 NO-GAP,
          SY-VLINE NO-GAP,(19) IT_TAB1-DMBTR CURRENCY 'KRW' NO-GAP,
          SY-VLINE NO-GAP,(06) W_RATE NO-GAP,
          SY-VLINE NO-GAP,(10) ' ' NO-GAP,
          SY-VLINE NO-GAP,(24) ''  NO-GAP,
          SY-VLINE NO-GAP,(06) ''NO-GAP,
          SY-VLINE NO-GAP,(15) ''  NO-GAP,
          SY-VLINE NO-GAP,(19) '' NO-GAP,
          SY-VLINE.
  HIDE : IT_TAB1,W_TABIX.

ENDFORM.

*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL.

*  SKIP 1.
  WRITE:/ SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  SUM.
  IF NOT ST_DMBTR IS INITIAL.
     W_RATE = IT_TAB1-DMBTR / ST_DMBTR * 100.
  ENDIF.
  WRITE:/ SY-VLINE,S_BLDAT-LOW,'-', S_BLDAT-HIGH,
          '당년도총액',(19)IT_TAB1-DMBTR CURRENCY 'KRW',
          (06)W_RATE,143 SY-VLINE .

ENDFORM.                    " P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.
  PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*----------------------------------------------------------------------
*&      Form  P2000_TOP_END_IT_TAB
*&---------------------------------------------------------------------
FORM P2000_TOP_END_IT_TAB.

  SORT  IT_TAB1 BY DMBTR DESCENDING.
  CLEAR: ST_DMBTR,OLDST_DMBTR.
  LOOP AT IT_TAB1.
     W_TABIX = SY-TABIX.
     MOVE W_TABIX TO IT_TAB1-W_SEQ.
*>> 당년도 총액.
     ADD IT_TAB1-DMBTR TO ST_DMBTR.
     MODIFY IT_TAB1 INDEX W_TABIX.
  ENDLOOP.
  LOOP AT IT_TEMP.
*>> 당년도 총액.
     ADD IT_TEMP-DMBTR TO OLDST_DMBTR.
  ENDLOOP.

ENDFORM.                    " P2000_TOP_END_IT_TAB
*&---------------------------------------------------------------------
*
*&      Form  P3000_SUB_TOTOL_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_SUB_TOTOL_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE NO-GAP,(04) '' NO-GAP,
          SY-VLINE NO-GAP,(30) '' NO-GAP.
  IF W_CHEK = 'Y'.
     FORMAT RESET.
     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
     WRITE:  SY-VLINE NO-GAP,(19) IT_TEMP-DMBTR CURRENCY 'KRW' NO-GAP.
     CLEAR W_CHEK.
  ELSE.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     WRITE:  SY-VLINE NO-GAP,(19) '' NO-GAP.
  ENDIF.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: SY-VLINE NO-GAP,(06) ' ' NO-GAP,
          SY-VLINE NO-GAP,(10) IT_TAB-MATNR  NO-GAP,
          SY-VLINE NO-GAP,(24) IT_TAB-TXZ01 NO-GAP,     " 품명.
          SY-VLINE NO-GAP,(06) IT_TAB-MEINS  NO-GAP,    " UNIT.
          SY-VLINE NO-GAP,(15) IT_TAB-BAMNG UNIT IT_TAB-MEINS  NO-GAP,
          SY-VLINE NO-GAP,(19) IT_TAB-DMBTR CURRENCY 'KRW'  NO-GAP,
          SY-VLINE.
  HIDE : IT_TAB,W_TABIX.

ENDFORM.                    " P3000_SUB_TOTOL_WRITE
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
        WHERE ZFREQNO = ZTIVIT-ZFREQNO
          AND LLIEF   IN S_LLIEF.
     IF SY-SUBRC NE 0.
         DELETE IT_ZTIVHST INDEX W_TABIX.
         CONTINUE.
     ENDIF.
     IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO'.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.

*>> 오퍼,구매조직 체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  = IT_ZTIVHST-EBELN
          AND BUKRS IN S_BUKRS
          AND EKGRP IN S_EKGRP
          AND EKORG IN S_EKORG .
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
       WHERE EBELN =  IT_ZTIVHST-EBELN
         AND EBELP  =  IT_ZTIVHST-EBELP
         AND MATNR  =  IT_ZTIVHST-MATNR.    " 자재번호.
     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = IT_ZTIVHST-BWART.

     IT_ZTIVHST-DMBTR =  EKBE-DMBTR / 1000.

     IF T156-SHKZG = 'H'.
        IT_ZTIVHST-DMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.

     SELECT SINGLE *
            FROM LFA1
           WHERE LIFNR  = ZTREQHD-LLIEF.

     MOVE: ZTREQHD-LLIEF  TO IT_ZTIVHST-LLIEF,
           LFA1-NAME1     TO IT_ZTIVHST-NAME1,
           EKPO-MEINS     TO IT_ZTIVHST-MEINS,
           EKPO-TXZ01     TO IT_ZTIVHST-TXZ01,
           EKBE-BAMNG     TO IT_ZTIVHST-BAMNG.  " 단위수량.
     MODIFY IT_ZTIVHST INDEX W_TABIX.
     MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB.
     MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB1.
     COLLECT IT_TAB.
     COLLECT IT_TAB1.
  ENDLOOP.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
      W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_CURRENCT_BLDAT
*&---------------------------------------------------------------------
*&      Form  P1000_OLDDAT_BLDAT
*&---------------------------------------------------------------------
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
        WHERE ZFREQNO = ZTIVIT-ZFREQNO
          AND LLIEF   IN S_LLIEF.
     IF SY-SUBRC NE 0.
         DELETE IT_ZTIVHST1 INDEX W_TABIX.
         CONTINUE.
     ENDIF.
     IF ZTREQHD-ZFREQTY EQ 'PU' OR ZTREQHD-ZFREQTY EQ 'LO'.
        DELETE IT_ZTIVHST1 INDEX W_TABIX.
        CONTINUE.
     ENDIF.

*>> 오퍼,구매조직 체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  = IT_ZTIVHST1-EBELN
          AND BUKRS  IN S_BUKRS
          AND EKGRP  IN S_EKGRP
          AND EKORG IN  S_EKORG .
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
*>> 이동유형에 따른 차/대변.
     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = IT_ZTIVHST1-BWART.

     IT_ZTIVHST1-DMBTR =  EKBE-DMBTR / 1000.

     IF T156-SHKZG = 'H'.
        IT_ZTIVHST1-DMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.
     CLEAR LFA1.
     SELECT SINGLE *
            FROM LFA1
           WHERE LIFNR  = ZTREQHD-LLIEF.

     MOVE: ZTREQHD-LLIEF  TO IT_ZTIVHST1-LLIEF,
           LFA1-NAME1     TO IT_ZTIVHST1-NAME1,
           EKPO-MEINS     TO IT_ZTIVHST1-MEINS,
           EKPO-TXZ01     TO IT_ZTIVHST1-TXZ01,
           EKBE-BAMNG     TO  IT_ZTIVHST1-BAMNG.  " 단위수량.
     MODIFY IT_ZTIVHST1 INDEX W_TABIX.
     MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_TEMP.
     COLLECT IT_TEMP.
  ENDLOOP.

ENDFORM.                    " P1000_OLDDAT_BLDAT
*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL1
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL1.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE,  R_BLDAT-LOW,'-', R_BLDAT-HIGH,
         '전년도총액',(19) OLDST_DMBTR CURRENCY 'KRW',
  143 SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                  " P3000_LINE_TOTAL1
