*&---------------------------------------------------------------------
*& Report  ZRIMORGPCLST
*&---------------------------------------------------------------------
*&  프로그램명 : 원산지별 구매실적표
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.23
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT  ZRIMORGPCLST  MESSAGE-ID ZIM
                     LINE-SIZE 138
                     NO STANDARD PAGE HEADING.
TABLES: ZTIVHST,ZTIVHSTIT,ZTIVIT,ZTREQHD,EKKO,EKBE,EKPO,LFA1,ZTREQORJ,
        T001W,T005T,T156.
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
       EBELN	   LIKE  ZTIVHSTIT-EBELN,   " 구매문서번호
       ZFIVDNO   LIKE   ZTIVHSTIT-ZFIVDNO," 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST.

*>> 당년 플랜트/자재별.
DATA : BEGIN OF IT_TAB1 OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS,   " 플랜트.
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       MEINS     LIKE  EKPO-MEINS,        " 수량단위.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화금액.
DATA : END OF IT_TAB1.
*>> 당년/플랜트/자재/구매처별.
DATA : BEGIN OF IT_TAB2 OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS,     " 플랜트.
       MATNR	   LIKE  ZTIVHSTIT-MATNR,     " 자재번호.
       TXZ01     LIKE  EKPO-TXZ01,          " 품명.
       MEINS     LIKE  EKPO-MEINS,          " 수량단위.
       DMBTR     LIKE  EKBE-DMBTR,          " 현지통화.
       BAMNG     LIKE  EKBE-BAMNG,          " 수량.
       LLIEF     LIKE  EKKO-LLIEF,          " 오퍼.
       NAME1     LIKE  LFA1-NAME1.          " 거래선.
DATA : END OF IT_TAB2.
*>> 당년/플랜트.
DATA : BEGIN OF IT_TAB3 OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS,     " 플랜트.
       MEINS     LIKE  EKPO-MEINS,          " 수량단위.
       DMBTR     LIKE  EKBE-DMBTR,          " 현지통화.
       BAMNG     LIKE  EKBE-BAMNG.          " 수량.
DATA : END OF IT_TAB3.

*>> 당년전사TOTAL.
DATA : BEGIN OF IT_TAB4 OCCURS 0,
       MEINS     LIKE  EKPO-MEINS,        " 수량단위.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화금액.
DATA : END OF IT_TAB4.

*>> 전년도 내역.
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
       ZFIVDNO  LIKE   ZTIVHSTIT-ZFIVDNO,  " 구매문서번호
       EBELP	   LIKE  ZTIVHSTIT-EBELP,   " 구매문서 품목번호
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       LLIEF     LIKE  EKKO-LLIEF,        " 오퍼.
       NAME1     LIKE  LFA1-NAME1,        " NAME1.
       EKORG	   LIKE  EKKO-EKORG,        " 구매조직.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR,        " 현지통화.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       MEINS     LIKE  EKPO-MEINS.        " 수량단위.
DATA : END OF IT_ZTIVHST1.
*>> 전년 플랜트/자재별 .
DATA : BEGIN OF IT_TEMP1 OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS,   " 플랜트.
       MATNR	   LIKE  ZTIVHSTIT-MATNR,   " 자재번호.
       TXZ01     LIKE  EKPO-TXZ01,        " 품명.
       MEINS     LIKE  EKPO-MEINS,        " 수량단위.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화금액.
DATA : END OF IT_TEMP1.

*>> 전년 플랜트.
DATA : BEGIN OF IT_TEMP3 OCCURS 0,
       WERKS     LIKE  ZTIVHSTIT-WERKS,     " 플랜트.
       DMBTR     LIKE  EKBE-DMBTR,          " 현지통화.
       BAMNG     LIKE  EKBE-BAMNG,          " 수량.
       MEINS     LIKE  EKPO-MEINS.          " 수량단위.
DATA : END OF IT_TEMP3.


*>> 전년전사TOTAL.
DATA : BEGIN OF IT_TEMP4 OCCURS 0,
       MEINS     LIKE  EKPO-MEINS,        " 수량단위.
       BAMNG     LIKE  EKBE-BAMNG,        " 수량.
       DMBTR     LIKE  EKBE-DMBTR.        " 현지통화금액.
DATA : END OF IT_TEMP4.
*>> ITEM 내역.

DATA :  W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_TEXT(10),
        W_TEXT2(10),
        W_TO(04),
        W_FROM(04),
        ST_DMBTR      LIKE  EKBE-DMBTR,        " 전년도총액.
        ST_BAMNG      LIKE  EKBE-BAMNG,        " 전년도수량.
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_CHEK,
        W_CHECK_BIT   TYPE C,
        W_TABIX       LIKE SY-TABIX,
        W_PAGE        TYPE I,
        W_LIST_INDEX  LIKE SY-TABIX.
RANGES: R_BLDAT FOR ZTIVHST-BLDAT OCCURS 05.

*INCLUDE   ZRIMSORTCOM.    " REPORT Sort
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR  EKKO-BUKRS NO-EXTENSION NO INTERVALS.
   PARAMETERS    : P_ORIG  LIKE ZTREQORJ-ZFORIG OBLIGATORY.    " 원산지.
   SELECT-OPTIONS: S_BLDAT	 FOR  ZTIVHST-BLDAT OBLIGATORY,
                   S_WERKS  FOR  ZTIVHSTIT-WERKS NO-EXTENSION
                                                 NO INTERVALS, " 플랜트.
                   S_EKGRP  FOR  EKKO-EKGRP,
                   S_EKORG  FOR  EKKO-EKORG.

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
*>> 금액별 순위.
*   PERFORM  P2000_TOP_END_IT_TAB.
* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
*      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
*         W_FIELD_NM = 'ZFIDRNO'.
*         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*         PERFORM HANDLE_SORT TABLES  IT_TAB2
*                             USING   SY-UCOMM.
*      WHEN 'REFR'.
*          PERFORM P1000_READ_DATA USING W_ERR_CHK.
*          PERFORM  P2000_TOP_END_IT_TAB.
*          PERFORM RESET_LIST.
*      WHEN 'DISP'.
*          IF W_TABIX IS INITIAL.
*            MESSAGE S962.    EXIT.
*          ENDIF.
*           PERFORM P2000_PO_DOC_DISPLAY(SAPMZIM01)
*                                     USING IT_TAB2-ZFREBELN ''.
*      WHEN 'DISP1'.
*        IF W_TABIX IS INITIAL.
*           MESSAGE S962.    EXIT.
*        ENDIF.
*        PERFORM P2000_DISP_ZTIDS USING IT_TAB2-ZFIDRNO.
*      WHEN 'DOWN'.          " FILE DOWNLOAD....
*          PERFORM P3000_TO_PC_DOWNLOAD.
  ENDCASE.
*  CLEAR: IT_TAB2, W_TABIX.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  CLEAR T001W.
  SELECT SINGLE *
         FROM T001W
        WHERE WERKS = IT_TAB1-WERKS.
  CLEAR T005T.
  SELECT SINGLE *
         FROM T005T
        WHERE LAND1 = P_ORIG.
  IF W_PAGE = 1.
     SKIP 1.
     WRITE:/ '원산지:',P_ORIG,T005T-LANDX, 64'[원산지별 구매 실적표]'
              COLOR COL_HEADING INTENSIFIED OFF,
           /128 SY-DATUM.
     CLEAR W_PAGE.
  ENDIF.
  WRITE:/ 'Plant:',IT_TAB1-WERKS, T001W-NAME1, '금액: 천원' .

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(25) '자재코드/품명'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '수량'   NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '금액'    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(30) '거래선' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(19) '수량'   NO-GAP CENTERED,
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
*>> 당년도 범위 설정 체크.
  IF W_TO NE 0.
      W_ERR_CHK = 'S'.
      EXIT.
  ENDIF.
*>> 전년도 범위설정.
  MOVE : 'I'               TO  R_BLDAT-SIGN,
         'BT'              TO  R_BLDAT-OPTION.
  CONCATENATE  W_FROM  S_BLDAT-LOW+4(04)  INTO   R_BLDAT-LOW.
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

   DATA : L_WERKS LIKE  T001W-WERKS,
          L_MEINS LIKE  EKPO-MEINS.

   SORT IT_TAB1 BY WERKS.
   SET TITLEBAR  'ZIMR28'.
   SET PF-STATUS 'ZIMR28'.
   CLEAR W_COUNT.
   DESCRIBE TABLE IT_TAB2 LINES W_LINE.
   LOOP AT IT_TAB1.
      W_TABIX = SY-TABIX.
      IF W_TABIX EQ 1.
         L_WERKS = IT_TAB1-WERKS.
         L_MEINS = IT_TAB1-MEINS.
      ENDIF.
      IF L_WERKS NE IT_TAB1-WERKS.
          W_CHEK = 'N'.
          WRITE:/ SY-ULINE.
          WRITE:/ SY-VLINE,'당년도 Plant 계:',138 SY-VLINE.
          LOOP AT IT_TAB3 WHERE WERKS = L_WERKS.
               PERFORM P3000_LINE_CSTOTAL.
          ENDLOOP.
          PERFORM P3000_TOTAL_LINE_CSTOTAL.
          WRITE:/ SY-ULINE.
          WRITE:/ SY-VLINE,'전년도 Plant 계:',138 SY-VLINE.
          W_CHECK_BIT = 'N'.
          LOOP AT IT_TEMP3 WHERE WERKS = IT_TAB1-WERKS.
               PERFORM P3000_LINE_OSTOTAL.
          ENDLOOP.
          PERFORM P3000_TOTAL_LINE_OSTOTAL.
          WRITE:/ SY-ULINE.
          NEW-PAGE.
      ENDIF.
      PERFORM   P3000_LINE_WRITE.
      WRITE:/ SY-ULINE.
      LOOP AT IT_TAB2 WHERE WERKS = IT_TAB1-WERKS
                       AND MATNR = IT_TAB1-MATNR	
                       AND TXZ01 = IT_TAB1-TXZ01
                       AND MEINS = IT_TAB1-MEINS.
         PERFORM  P3000_SUB_TOTOL_WRITE.
      ENDLOOP.
      L_WERKS = IT_TAB1-WERKS.
      L_MEINS = IT_TAB1-MEINS.
      AT LAST.
          W_CHEK = 'N'.
          WRITE:/ SY-ULINE.
          WRITE:/ SY-VLINE,'당년도 Plant 계:',138 SY-VLINE.
          LOOP AT IT_TAB3 WHERE WERKS = L_WERKS.
               PERFORM P3000_LINE_CSTOTAL.
          ENDLOOP.
          PERFORM P3000_TOTAL_LINE_CSTOTAL.
          WRITE:/ SY-ULINE.
          WRITE:/ SY-VLINE,'전년도 Plant 계:',138 SY-VLINE.
          LOOP AT IT_TEMP3 WHERE WERKS = IT_TAB1-WERKS.
               PERFORM P3000_LINE_OSTOTAL.
          ENDLOOP.
          PERFORM P3000_TOTAL_LINE_OSTOTAL.
          WRITE:/ SY-ULINE.

      ENDAT.
   ENDLOOP.
   PERFORM P3000_LINE_TOTAL.
   PERFORM P3000_LINE_TOTAL1.
*  CLEAR: IT_TAB2, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR28'.

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

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/ SY-VLINE NO-GAP,(24) IT_TAB1-MATNR,
          SY-VLINE NO-GAP,(03) IT_TAB1-MEINS,
          (15) IT_TAB1-BAMNG UNIT IT_TAB1-MEINS NO-GAP,
          SY-VLINE NO-GAP,(19) IT_TAB1-DMBTR CURRENCY 'KRW' NO-GAP,
          138 SY-VLINE.
  W_CHECK_BIT = 'Y'.
* HIDE : IT_TAB1,W_TABIX.

ENDFORM.

*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED ON.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE,'당년도',138 SY-VLINE.
  WRITE:/ SY-VLINE,S_BLDAT-LOW,'-', S_BLDAT-HIGH,138 SY-VLINE.

  LOOP AT IT_TAB4.
       WRITE:/ SY-VLINE,(20)'',
                        (05) IT_TAB4-MEINS,
                        (19) IT_TAB4-BAMNG UNIT IT_TAB4-MEINS,
                        (19) '',
                        138 SY-VLINE .
      AT LAST.
         SUM.
         WRITE:/ SY-VLINE,(20)'',
                        (05) '',
                        (19) '',
                        (19) IT_TAB4-DMBTR CURRENCY 'KRW',
                        138 SY-VLINE .

      ENDAT.
  ENDLOOP.

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
*
*&      Form  P2000_TOP_END_IT_TAB
*&---------------------------------------------------------------------
*
FORM P2000_TOP_END_IT_TAB.

*  SORT  IT_TAB1 BY DMBTR DESCENDING.
**>> 당년도 전사총액,총수량.
*  LOOP AT IT_TAB1.
*     W_TABIX = SY-TABIX.
*     MOVE W_TABIX TO IT_TAB1-W_SEQ.
*     MODIFY IT_TAB1 INDEX W_TABIX.
*  ENDLOOP.
*
***>> 전년도 전사총액,총수량.
**  LOOP AT IT_TEMP1.
**     MOVE-CORRESPONDING IT_TEMP1 TO IT_TEMP4.
**     COLLECT IT_TEMP4.
**  ENDLOOP.

ENDFORM.                    " P2000_TOP_END_IT_TAB
*&---------------------------------------------------------------------
*
*&      Form  P3000_SUB_TOTOL_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_SUB_TOTOL_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  IF W_CHECK_BIT EQ 'Y'.
     WRITE:/  SY-VLINE NO-GAP,(25) IT_TAB1-TXZ01 NO-GAP.
     W_CHECK_BIT = 'N'.
  ELSE.
     WRITE:/  SY-VLINE NO-GAP,(25) '' NO-GAP.
  ENDIF.

*>> 전년도 자재별 수량/금액
  READ TABLE IT_TEMP1 WITH KEY WERKS =  IT_TAB1-WERKS
                                TXZ01 = IT_TAB1-TXZ01
                                MEINS = IT_TAB1-MEINS.
  IF SY-SUBRC EQ 0.
     W_CHEK = 'Y'.
  ENDIF.
*>> 첫번째 라인에만 찍어 주기 위해서.
  IF  W_CHEK = 'Y'.
     FORMAT RESET.
     FORMAT COLOR COL_TOTAL INTENSIFIED ON.
     WRITE:SY-VLINE NO-GAP," (03)IT_TEMP1-MEINS NO-GAP,
                             (19)IT_TEMP1-BAMNG UNIT
                                 IT_TEMP1-MEINS NO-GAP.
     WRITE:SY-VLINE NO-GAP,  (19)IT_TEMP1-DMBTR CURRENCY 'KRW' NO-GAP.
     CLEAR W_CHEK.
  ELSE.
     FORMAT RESET.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     WRITE:  SY-VLINE NO-GAP,(19) '' NO-GAP.
     WRITE:  SY-VLINE NO-GAP,(19) '' NO-GAP.
  ENDIF.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: SY-VLINE NO-GAP, (10) IT_TAB2-LLIEF NO-GAP,
                          (20) IT_TAB2-NAME1 NO-GAP,
         SY-VLINE NO-GAP,
         (19) IT_TAB2-BAMNG UNIT IT_TAB2-MEINS  NO-GAP,
         SY-VLINE NO-GAP,(19) IT_TAB2-DMBTR CURRENCY 'KRW'  NO-GAP,
         138 SY-VLINE.


ENDFORM.                    " P3000_SUB_TOTOL_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P1000_CURRENCT_BLDAT
*&---------------------------------------------------------------------
*
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
*>> 원산지 체크.
     SELECT COUNT( * ) INTO W_COUNT
          FROM ZTREQORJ
         WHERE ZFREQNO = ZTIVIT-ZFREQNO
           AND ZFORIG = P_ORIG.

     IF W_COUNT EQ 0.
        DELETE IT_ZTIVHST INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 오퍼 체크.
     CLEAR EKKO.
     SELECT   SINGLE *
        FROM  EKKO
        WHERE EBELN  =  IT_ZTIVHST-EBELN
          AND EKGRP  IN S_EKGRP
          AND EKORG  IN S_EKORG
          AND BUKRS  IN S_BUKRS.

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
     IT_ZTIVHST-DMBTR    =  EKBE-DMBTR / 1000.
     IF T156-SHKZG = 'H'.
        IT_ZTIVHST-DMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.
*>> 오퍼명.
     CLEAR LFA1.
     SELECT SINGLE *
            FROM LFA1
           WHERE LIFNR  = ZTREQHD-LIFNR.

     MOVE: ZTREQHD-LIFNR  TO IT_ZTIVHST-LLIEF,
           LFA1-NAME1     TO IT_ZTIVHST-NAME1,
           EKPO-MEINS     TO IT_ZTIVHST-MEINS,
           EKPO-TXZ01     TO IT_ZTIVHST-TXZ01,
           EKBE-BAMNG     TO  IT_ZTIVHST-BAMNG.  " 단위수량.
     MODIFY IT_ZTIVHST INDEX W_TABIX.
     MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB1.
     MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB2.
     MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB3.
     MOVE-CORRESPONDING IT_ZTIVHST TO IT_TAB4.
     COLLECT IT_TAB1.
     COLLECT IT_TAB2.
     COLLECT IT_TAB3.
     COLLECT IT_TAB4.
  ENDLOOP.
  DESCRIBE TABLE IT_TAB1 LINES W_LINE.
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
*>> 원산지 체크.
     SELECT COUNT( * ) INTO W_COUNT
          FROM ZTREQORJ
         WHERE ZFREQNO = ZTIVIT-ZFREQNO
           AND ZFORIG = P_ORIG.

     IF W_COUNT EQ 0.
        DELETE IT_ZTIVHST1 INDEX W_TABIX.
        CONTINUE.
     ENDIF.

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
          AND EKGRP  IN S_EKGRP
          AND EKORG  IN S_EKORG
          AND BUKRS  IN S_BUKRS.

*          AND EKORG IN  S_EKORG .
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
     IT_ZTIVHST1-DMBTR    =  EKBE-DMBTR / 1000.
     IF T156-SHKZG = 'H'.
        IT_ZTIVHST1-DMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.
     CLEAR LFA1.
     SELECT SINGLE *
            FROM LFA1
           WHERE LIFNR  = ZTREQHD-LIFNR.

     MOVE: ZTREQHD-LIFNR  TO IT_ZTIVHST1-LLIEF,
           LFA1-NAME1     TO IT_ZTIVHST1-NAME1,
           EKPO-MEINS     TO IT_ZTIVHST1-MEINS,
           EKPO-TXZ01     TO IT_ZTIVHST1-TXZ01,
           EKBE-BAMNG     TO  IT_ZTIVHST1-BAMNG.  " 단위수량.
     MODIFY IT_ZTIVHST1 INDEX W_TABIX.
     MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_TEMP1.
     MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_TEMP3.
     MOVE-CORRESPONDING IT_ZTIVHST1 TO IT_TEMP4.

     COLLECT IT_TEMP1.
     COLLECT IT_TEMP3.
     COLLECT IT_TEMP4.
  ENDLOOP.

ENDFORM.                    " P1000_OLDDAT_BLDAT
*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_TOTAL1
*&---------------------------------------------------------------------
*
FORM P3000_LINE_TOTAL1.

  DESCRIBE TABLE IT_TEMP4 LINES W_LINE.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE,'전년도',138 SY-VLINE.
  WRITE:/ SY-VLINE,R_BLDAT-LOW,'-', R_BLDAT-HIGH,138 SY-VLINE.
  IF W_LINE EQ 0.
      WRITE:/ SY-VLINE, (20)'',
                        (05) '',
                        (19)IT_TEMP4-BAMNG UNIT IT_TEMP4-MEINS,
                        138 SY-VLINE .

  ENDIF.
  LOOP AT IT_TEMP4.
       WRITE:/ SY-VLINE,(20)'',
                        (05) IT_TEMP4-MEINS,
                        (19) IT_TEMP4-BAMNG UNIT IT_TEMP4-MEINS,
                        (19) IT_TEMP4-DMBTR CURRENCY 'KRW',
                        138 SY-VLINE .
  ENDLOOP.
  WRITE:/ SY-ULINE.

ENDFORM.                  " P3000_LINE_TOTAL1
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_CSTOTAL
*&---------------------------------------------------------------------*
FORM P3000_LINE_CSTOTAL.

  IF W_CHECK_BIT EQ 'N'.
     MOVE '수량 ' TO  W_TEXT.
  ENDIF.

  WRITE:/ SY-VLINE,(18)  W_TEXT,
                   (05) '',
                   (05) IT_TAB3-MEINS,
                   (13) IT_TAB3-BAMNG UNIT IT_TAB3-MEINS,
                   (19) '',
                   138 SY-VLINE .
  CLEAR: W_TEXT,W_CHECK_BIT.
  ADD IT_TAB3-DMBTR TO ST_DMBTR.

ENDFORM.                    " P3000_LINE_CSTOTAL
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_OSTOTAL
*&---------------------------------------------------------------------*
FORM P3000_LINE_OSTOTAL.

  IF W_CHECK_BIT EQ 'N'.
     MOVE '수량 ' TO  W_TEXT.
  ENDIF.

  WRITE:/ SY-VLINE,(18) W_TEXT,
                   (05) '',
                   (05) IT_TEMP3-MEINS,
                   (13) IT_TEMP3-BAMNG UNIT IT_TEMP3-MEINS,
                   (19) '',
                   138 SY-VLINE .
  ADD IT_TEMP3-DMBTR TO ST_DMBTR.
  CLEAR: W_TEXT,W_CHECK_BIT.

ENDFORM.                    " P3000_LINE_OSTOTAL
*&---------------------------------------------------------------------*
*&      Form  P3000_TOTAL_LINE_CSTOTAL
*&---------------------------------------------------------------------*
FORM P3000_TOTAL_LINE_CSTOTAL.

  IF W_CHEK EQ 'N'.
     MOVE '금액 ' TO  W_TEXT.
  ENDIF.

  WRITE:/ SY-VLINE,(18)  W_TEXT,
                   (05) '',
                   (19) '',
                   (19) ST_DMBTR CURRENCY 'KRW',
                   138 SY-VLINE .
  CLEAR: ST_DMBTR,W_CHEK.

ENDFORM.                    " P3000_TOTAL_LINE_CSTOTAL
*&---------------------------------------------------------------------*
*&      Form  P3000_TOTAL_LINE_OSTOTAL
*&---------------------------------------------------------------------*
FORM P3000_TOTAL_LINE_OSTOTAL.

  IF W_CHEK EQ 'N'.
     MOVE '금액 ' TO  W_TEXT.
  ENDIF.

  WRITE:/ SY-VLINE,(18)  W_TEXT,
                   (05) '',
                   (19) '',
                   (19) ST_DMBTR CURRENCY 'KRW',
                   138 SY-VLINE .

  CLEAR: ST_DMBTR,W_CHEK.

ENDFORM.                    " P3000_TOTAL_LINE_OSTOTAL
