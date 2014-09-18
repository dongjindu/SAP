*&---------------------------------------------------------------------*
*& REPORT ZRIMMAT02                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 미착재고 현황                                         *
*&      작성자 : 이 채 경 INFOLINK Ltd.                                *
*&      작성일 : 2001.10.09                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. P/O 별로 미착된 물품현황을 조회한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMMAT02  MESSAGE-ID ZIM
                     LINE-SIZE 112
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
*  사용 TABLE DECLARE
*-----------------------------------------------------------------------
TABLES  : EKKO,                " ABAP Standard Header Table..
          EKPO,                " ABAP Standard Item Table..
          ZTREQHD,             " 수입의뢰 Header Table..
          ZTREQST,             " 수입의뢰 상태 Table..
          ZTREQIT,             " 수입의뢰 Item Table..
          ZTIV,                " Invoice Table..
          ZTIVIT,              " Invoice Item Table..
          ZTIVHST,
          ZVEKKO_REQHD_ST,
          ZVEKPO_IT,
          ZVCOSTDIV,
          EKBE,                " 구매문서별 이력.
          MBEW,
          ZTIVHSTIT,
          T156,                " 이동유형.
          ZTCIVIT,             " [수입] Commercial Invoice Items.
          ZTCIVHST,
          ZTBDIV,
          T001W,
          EKET.                " 납기일 관련 Table..

*-----------------------------------------------*
* 수입의뢰 번호 조회를 위한 INTERNAL TABLE 선언 *
*-----------------------------------------------*

DATA: BEGIN OF IT_TEMP OCCURS 0,          " REQNO조회위한IT선언.
        WERKS     LIKE   EKPO-WERKS,        " Plant..
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        EBELP     LIKE   ZTREQIT-EBELP,    " P/O ITEM NO.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        MATKL     LIKE   EKPO-MATKL,       " 자재그룹.
        BEDAT     LIKE   EKKO-BEDAT,       " 구매증빙일.
        TXZ01     LIKE   ZTREQIT-TXZ01,    " Short Text.
        DMBTR1    LIKE   EKBE-DMBTR,       " 총재고.
        DMBTR2    LIKE   EKBE-DMBTR,       " 자창재고.
        DMBTR3    LIKE   EKBE-DMBTR.       " 미착재고.
 DATA:  END OF IT_TEMP.
*>> 자재별 미착금액.
 DATA: BEGIN OF IT_TAB OCCURS 0,          " REQNO조회위한IT선언.
        WERKS     LIKE   EKPO-WERKS,        " Plant.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        MATKL     LIKE   EKPO-MATKL,       " 자재그룹.
        TXZ01     LIKE   ZTREQIT-TXZ01,    " Short Text.
        DMBTR1    LIKE   EKBE-DMBTR,       " 총재고.
        DMBTR2    LIKE   EKBE-DMBTR,       " 자창재고.
        DMBTR3    LIKE   EKBE-DMBTR.       " 미착재고.
 DATA:  END OF IT_TAB.
*>> P/O 별 미착금액.
 DATA:  BEGIN OF IT_TAB1 OCCURS 0,          " REQNO조회위한IT선언.
        WERKS     LIKE   EKPO-WERKS,        " Plant..
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        EBELP     LIKE   EKPO-EBELP,
        BEDAT     LIKE   EKKO-BEDAT,       " 구매증빙일.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        MATKL     LIKE   EKPO-MATKL,       " 자재그룹.
        TXZ01     LIKE   ZTREQIT-TXZ01,    " Short Text.
        DMBTR3    LIKE   EKBE-DMBTR.       " P/O 별미착금액.
 DATA:  END OF IT_TAB1.

 DATA : W_ERR_CHK     TYPE C,
        W_FIELD_NM    TYPE C,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX,
        W_PAGE        TYPE I.

*>> 입고/AP.
 DATA:  W_GRDMBTR LIKE EKBE-DMBTR,  " 입고금액.
        W_APDMBTR LIKE EKBE-DMBTR.  " AP  금액.
*-----------------------------------------------------------------------
* 검색조건 WINDOW CREATE
*--------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_BUKRS   FOR EKPO-BUKRS,       " 회사코드.
               S_WERKS   FOR EKPO-WERKS,       " PLANT.
               S_MATNR   FOR EKPO-MATNR,       " 자재코드.
               S_MATKL   FOR EKPO-MATKL,       " 자재그룹.
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_EKORG   FOR EKKO-EKORG,       " Purch. Org.
               S_EKGRP   FOR EKKO-EKGRP.    "    Purch. Grp.

SELECTION-SCREEN END OF BLOCK B1.

*>> 선택구분.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-021, POSITION 1.
     PARAMETERS : P_AP  RADIOBUTTON GROUP RDG.     " 비용미착.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-022, POSITION 1.
     PARAMETERS : P_GR      RADIOBUTTON GROUP RDG. " 물대미착.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN : BEGIN OF LINE,  POSITION 1.
     SELECTION-SCREEN : COMMENT 4(18) TEXT-023, POSITION 1.
     PARAMETERS : P_APGR   RADIOBUTTON GROUP RDG.   "전체미착.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* TITLE 초기셋팅.
*-----------------------------------------------------------------------
INITIALIZATION.
   PERFORM   P2000_INIT.

*title Text Write
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* SELECT 실행시....
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 수입의뢰 No.
   PERFORM P1000_READ_DATA USING W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738.  EXIT.
   ENDIF.
*>> 비용미착.(입고는 잡히고,AP는 안잡힌것)
   IF P_AP =  'X' OR P_APGR = 'X'.
      PERFORM P1000_READ_DATA_AP.
   ENDIF.
*>> 물대미착.(AP는 잡히고,입고는 안잡힌것)
   IF P_GR =  'X' OR P_APGR = 'X'.
      PERFORM P1000_READ_DATA_GR.
   ENDIF.

   DELETE IT_TAB  WHERE DMBTR3 LE 0.
   DELETE IT_TAB1 WHERE DMBTR3 LE 0.

   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE EQ 0.
      MESSAGE S738.  EXIT.
   ENDIF.

* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* SELECT 실행후에....
*-----------------------------------------------------------------------
END-OF-SELECTION.






*-----------------------------------------------------------------------
* DEFINE 된 COMMAND 실행시.
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
      WHEN 'DISP'.
         IF IT_TAB1-EBELN IS INITIAL.
            MESSAGE S962.
         ELSE.
            SUBMIT ZRIMCSTCHA
                   WITH S_EBELN EQ IT_TAB1-EBELN
                   WITH S_EBELP EQ IT_TAB1-EBELP
                   AND RETURN.
         ENDIF.
      WHEN 'DSPO'.
         IF IT_TAB1-EBELN IS INITIAL.
            MESSAGE S962.
         ELSE.
            SET PARAMETER ID 'BSP' FIELD ''.
            SET PARAMETER ID 'BES' FIELD IT_TAB1-EBELN.

            CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
         ENDIF.
      WHEN 'DSMT'.
         IF IT_TAB1-MATNR IS INITIAL.
            MESSAGE S962.
         ELSE.
            SELECT SINGLE * FROM EKKO
                            WHERE EBELN EQ IT_TAB1-EBELN.

            SET PARAMETER ID 'MAT' FIELD IT_TAB1-MATNR.
            SET PARAMETER ID 'BUK' FIELD EKKO-BUKRS.
            SET PARAMETER ID 'WRK' FIELD IT_TAB1-WERKS.
            SET PARAMETER ID 'LAG' FIELD ''.
            SET PARAMETER ID 'MXX' FIELD 'BDEKLPQSVXZA'.
*       MM03_START_SICHT(15) TYPE C  VALUE 'BDEKLPQSVXZA',

            CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.
         ENDIF.
   ENDCASE.
   CLEAR : IT_TAB, IT_TAB1.


*AT LINE-SELECTION.




*&---------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*       수입의뢰내역 & 수입의뢰 ITEM 내역을 조회한다.
*----------------------------------------------------------------------*
FORM P1000_READ_DATA USING W_ERR_CHECK.

   W_ERR_CHK = 'N'.
   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_TEMP
     FROM ZVEKKO_REQHD_ST AS H INNER JOIN ZVEKPO_IT AS I
       ON H~EBELN   EQ I~EBELN
    WHERE  H~EBELN  IN  S_EBELN
      AND  I~MATNR  IN  S_MATNR
      AND  H~BUKRS  IN  S_BUKRS
      AND  H~EKORG  IN  S_EKORG
      AND  H~EKGRP  IN  S_EKGRP
      AND  I~WERKS  IN  S_WERKS
      AND I~ELIKZ = SPACE
      AND H~ZFREQTY NOT IN  ('LO', 'PU').
   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

   P_APGR  = 'X'.
   SET TITLEBAR  'ZIMR42'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE. SKIP 1.

  SKIP 2.
  WRITE:/45 '[  수입 미착 현황  ]'
      COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 2.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,(35) '품      명' NO-GAP,
            SY-VLINE NO-GAP,(20) '자재코드'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '미착재고'  NO-GAP,
            SY-VLINE NO-GAP,(17) '총 재 고'  NO-GAP,
            SY-VLINE NO-GAP,(17) '자창재고'  NO-GAP,
            SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/   SY-VLINE NO-GAP,(35) 'P l a n t'     NO-GAP,
            SY-VLINE NO-GAP,(09) 'P/O No'  NO-GAP,
            SY-VLINE NO-GAP,(10) 'P/OItem '      NO-GAP,
            SY-VLINE NO-GAP,(17) '금   액'      NO-GAP,
            SY-VLINE NO-GAP,(17) '구매증빙일'       NO-GAP,
            SY-VLINE NO-GAP,(17) '월평균사용액' NO-GAP,
            SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

   SET TITLEBAR  'ZIMR42'.
   SET PF-STATUS 'ZIMR42'.

   SORT IT_TAB  BY WERKS MATNR.
   SORT IT_TAB1 BY WERKS MATNR EBELN EBELP.


   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM  P3000_HEAD_WRITE.      " 자재별.
      LOOP AT IT_TAB1 WHERE  WERKS  = IT_TAB-WERKS    " Plant.
                        AND  MATNR  = IT_TAB-MATNR    " Material Number.
                        AND  MATKL  = IT_TAB-MATKL    " 자재그룹.
                        AND  TXZ01  = IT_TAB-TXZ01.   " Short Text.
           PERFORM  P3000_ITEM_WRITE.  " P/O 별.
      ENDLOOP.
      WRITE:/ SY-ULINE.
   ENDLOOP.
   CLEAR : IT_TAB1, IT_TAB.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
  FORM P3000_HEAD_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

*----> 자창재고금액 구하기..   미착금액 + 자창금액 ==> 총재고.
*>> 자재별/품명별
  CLEAR : MBEW.
  IF NOT IT_TAB-WERKS IS INITIAL AND NOT IT_TAB-MATNR IS INITIAL.
     SELECT * FROM MBEW UP TO 1 ROWS
            WHERE MATNR EQ IT_TAB-MATNR
            AND   BWKEY EQ IT_TAB-WERKS.
     ENDSELECT.
     IF SY-SUBRC EQ 0.
        IT_TAB-DMBTR2 = MBEW-SALK3.
     ELSE.
        IT_TAB-DMBTR2 = 0.
     ENDIF.
  ELSE.
     IT_TAB-DMBTR2 = 0.
  ENDIF.

  IT_TAB-DMBTR1 = IT_TAB-DMBTR2 + IT_TAB-DMBTR3.
*----> 최근 3개월간 사용량(사용금액)


  WRITE : / SY-VLINE NO-GAP,(35) IT_TAB-TXZ01  NO-GAP,  " 품    명'
            SY-VLINE NO-GAP,(20) IT_TAB-MATNR  NO-GAP,  " 자재코드.
            SY-VLINE NO-GAP,(17) IT_TAB-DMBTR3 CURRENCY 'KRW' NO-GAP, "
            SY-VLINE NO-GAP,(17) IT_TAB-DMBTR2 CURRENCY 'KRW' NO-GAP, "
            SY-VLINE NO-GAP,(17) IT_TAB-DMBTR1 CURRENCY 'KRW' NO-GAP, "
            SY-VLINE.

  HIDE : IT_TAB.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_ITEM_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ITEM_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  DATA: W_WERKS LIKE EKPO-WERKS,
        W_MATKL LIKE EKPO-MATKL.
  CLEAR T001W.
  ON CHANGE OF IT_TAB-WERKS OR IT_TAB-MATNR.
  SELECT SINGLE *
         FROM T001W
        WHERE WERKS = IT_TAB-WERKS.

    W_WERKS = IT_TAB-WERKS.W_MATKL = IT_TAB-MATKL.
  ENDON.
*>> 자재별/품명별
  WRITE : / SY-VLINE NO-GAP,(05) W_WERKS ,(29) T001W-NAME1 NO-GAP,  " Pl
*            SY-VLINE NO-GAP,(09) W_MATKL  NO-GAP,  " 자재그룹.
            SY-VLINE NO-GAP,(10) IT_TAB1-EBELN  NO-GAP,
            SY-VLINE NO-GAP,(09) IT_TAB1-EBELP  NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB1-DMBTR3 CURRENCY 'KRW'   NO-GAP,
            SY-VLINE NO-GAP,(17) IT_TAB1-BEDAT NO-GAP RIGHT-JUSTIFIED,
            SY-VLINE NO-GAP,(17) ''       NO-GAP,
            SY-VLINE.
  HIDE : IT_TAB1.

ENDFORM.                    " P3000_ITEM_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_AP
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_AP.

*>> DATA MODIFY.
 LOOP AT IT_TEMP.
     W_TABIX = SY-TABIX.
*>> 비용 전기이력.

     SELECT SUM( DMBTR ) INTO IT_TEMP-DMBTR3
            FROM  ZVCOSTDIV
            WHERE ZFPOSYN EQ 'Y'
*            AND   ZFPOYN  NE 'N'
            AND   EBELN   EQ IT_TEMP-EBELN
            AND   EBELP   EQ IT_TEMP-EBELP.

     MOVE-CORRESPONDING IT_TEMP TO IT_TAB.
     MOVE-CORRESPONDING IT_TEMP TO IT_TAB1.
     COLLECT IT_TAB.
     COLLECT IT_TAB1.

   ENDLOOP.

ENDFORM.                    " P1000_READ_DATA_AP
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA_GR
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA_GR.

*>> DATA MODIFY.
 LOOP AT IT_TEMP.
     W_TABIX = SY-TABIX.
*>> 입고이력.
     CLEAR: ZTIVHST,ZTIVHSTIT.
     SELECT SINGLE *
       FROM ZTIVHSTIT
      WHERE ZFGRST  = 'Y'
        AND  EBELN = IT_TEMP-EBELN
        AND  EBELP = IT_TEMP-EBELP.

     SELECT SINGLE *
        FROM ZTIVHST
       WHERE ZFIVNO  =  ZTIVHSTIT-ZFIVNO
         AND ZFIVHST =  ZTIVHSTIT-ZFIVHST.

*>> 현지통화금액.
     CLEAR EKBE.
     SELECT SINGLE *
       FROM EKBE
      WHERE EBELN  = IT_TEMP-EBELN
        AND EBELP  = IT_TEMP-EBELP
        AND MATNR  = IT_TEMP-MATNR    " 자재번호.
        AND BELNR  = ZTIVHST-MBLNR    " 자재문서번호.
        AND GJAHR  = ZTIVHST-MJAHR    " 자재문서연도.
        AND BWART  = ZTIVHST-BWART    " 이동유형.
        AND BEWTP  = 'E'.             " 이력종류.

     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = ZTIVHST-BWART.
*>> 이동 유형에 따른 차/대변 지시자.
     W_GRDMBTR = EKBE-DMBTR.
     IF T156-SHKZG = 'H'.
        W_GRDMBTR    =  ( EKBE-DMBTR  ) * -1.
     ENDIF.

*>> AP 이력.
     CLEAR: ZTCIVIT.
     SELECT SINGLE *
       FROM ZTCIVIT
      WHERE EBELN = IT_TEMP-EBELN
        AND EBELP = IT_TEMP-EBELP.

     CLEAR: ZTCIVHST.
     SELECT SINGLE *
        FROM ZTCIVHST
       WHERE  CGJAHR  = SPACE
         AND  ZFCIVRN = ZTCIVIT-ZFCIVRN.
*        AND  ZFCIVHST = ZTCIVIT-ZFCIVHST.
*>> 현지통화금액.
     CLEAR EKBE.
     SELECT SINGLE *
       FROM EKBE
      WHERE EBELN  = IT_TEMP-EBELN
        AND EBELP  = IT_TEMP-EBELP
        AND MATNR  = IT_TEMP-MATNR    " 자재번호.
        AND BELNR  = ZTCIVHST-BELNR    " 자재문서번호.
        AND GJAHR  = ZTCIVHST-GJAHR    " 자재문서연도.
*        AND BWART  = ZTCIVHST-BWART    " 이동유형.
        AND BEWTP  = 'Q'.              " 이력종류.

     CLEAR T156.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = EKBE-BWART.
*>> 이동 유형에 따른 차/대변 지시자.
     W_APDMBTR = EKBE-DMBTR.
     IF T156-SHKZG = 'H'.
          W_APDMBTR =  ( EKBE-DMBTR  ) * -1.
     ENDIF.

     IT_TEMP-DMBTR3    =  W_APDMBTR - W_GRDMBTR.

*> 미착재고가 없으면....
     IF IT_TEMP-DMBTR3 LE 0.
       CONTINUE.
     ENDIF.

     MOVE-CORRESPONDING IT_TEMP TO IT_TAB.
     MOVE-CORRESPONDING IT_TEMP TO IT_TAB1.
     COLLECT IT_TAB.
     COLLECT IT_TAB1.

   ENDLOOP.

ENDFORM.                    " P1000_READ_DATA_GR
