*&---------------------------------------------------------------------*
*& Report  ZRIMBLNLIST                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 선적정보 미등록 현황                                  *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.24                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :                                                       *
*&
*&---------------------------------------------------------------------*
*& [변경내용]                                                          *
*& 2001.11.09 김영광(LG-EDS)                                           *
*&            개설일->의뢰일, 구매그룹 필드 추가                       *
*&---------------------------------------------------------------------*

REPORT  ZRIMBLNLIST   MESSAGE-ID ZIM
                     LINE-SIZE 142
                     NO STANDARD PAGE HEADING.

* TABLES
TABLES : ZTREQIT, ZTBLIT, ZTREQHD, ZTREQST, LFA1.
TABLES : ZVREQHD_IT.
TABLES : ZSREQHD, SPOP, ZTIMIMG00.

* BLIT용 INTERNAL TABLE.
DATA : BEGIN OF IT_ZTBLIT OCCURS 0,
       ZFREQNO   LIKE    ZTBLIT-ZFREQNO,
       ZFITMNO   LIKE    ZTBLIT-ZFITMNO,
       BLMENGE   LIKE    ZTBLIT-BLMENGE.
DATA : END OF IT_ZTBLIT.

* VIEW READ 용 수입의뢰관리번호 TABLE.
DATA : BEGIN OF IT_REQNO OCCURS 0,
       ZFREQNO   LIKE    ZTREQST-ZFREQNO.
DATA : END OF IT_REQNO.

* LIST 용 INTERNAL TABLE.
DATA : BEGIN OF IT_TAB OCCURS 0.
       INCLUDE STRUCTURE ZVREQHD_IT.
DATA : SUM_BL      LIKE    ZTBLIT-BLMENGE,
       SUM_REQ     LIKE    ZTBLIT-BLMENGE,
       RE_ITEM     LIKE    ZTBLIT-BLMENGE,
       EKGRP_NM(30) TYPE C.
DATA : END OF IT_TAB.

DATA : W_EKGRP LIKE ZTREQST-EKGRP.

DATA : ANTWORT(1),
       OK-CODE    LIKE  SY-UCOMM,
       W_ERR_CHK(1),
       W_MOD(2),
       W_TABIX    LIKE   SY-TABIX,
       W_FIELD_NM(20),
       W_COUNT TYPE I,
       P_BUKRS    LIKE   ZTREQHD-BUKRS.

*----------------------------------------------------------------------*
* INCLUDE.                                                             *
*----------------------------------------------------------------------*
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

*----------------------------------------------------------------------*
* SELECTION SCREEN.                                                    *
*----------------------------------------------------------------------*
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS
                                 NO-EXTENSION NO INTERVALS,
                   S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_EKGRP   FOR ZTREQST-EKGRP,
                   S_MATNR   FOR ZTREQIT-MATNR,    " 자재코드.
                   S_TXZ01   FOR ZTREQIT-TXZ01,    " 품명.
                   S_OPNNO   FOR ZTREQHD-ZFOPNNO,  " 신용장 번호.
                   S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type
                   S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 관리번호.
                   S_OPNNM   FOR ZTREQST-ZFOPNNM,  " 개설 담당자.
                   S_CDAT    FOR ZTREQST-CDAT,     "의뢰일.
                   S_SHCU    FOR ZTREQHD-ZFSHCU,   " 선적국.
                   S_INCO1   FOR ZTREQHD-INCO1.    " 인코텀즈.
SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
* INITIALIZATION.                                                      *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM  P1000_SET_BUKRS.
  SET TITLEBAR  'ZIMR21'.                      "타이틀바.

TOP-OF-PAGE.
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ELSE.
     PERFORM   P3000_TITLE_WRITE_UN.
  ENDIF.

*----------------------------------------------------------------------*
* START OF SELECTION.                                                  *
*----------------------------------------------------------------------*
START-OF-SELECTION.
   PERFORM   P1000_READ_DATA   USING  W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.
      MESSAGE S009. EXIT.
   ENDIF.

   PERFORM   P3000_DATA_WRITE.
   PERFORM   P3000_LAST_WRITE.


*----------------------------------------------------------------------*
* FORM  P1000_READ_DATA.                                               *
*----------------------------------------------------------------------*
FORM P1000_READ_DATA     USING W_ERR_CHK.

  RANGES : R_REQNO  FOR   ZTREQHD-ZFREQNO  OCCURS 0.

  REFRESH : IT_TAB, IT_REQNO, IT_ZTBLIT.   ", IT_ZVREQHD_IT.
  CLEAR   :  W_ERR_CHK.

* 조건에 맞는 수입의뢰 관리번호 추출.
  SELECT DISTINCT H~ZFREQNO
              INTO CORRESPONDING FIELDS OF TABLE IT_REQNO
              FROM  ZTREQHD AS H INNER JOIN  ZTREQST AS S
              ON    H~ZFREQNO = S~ZFREQNO
                    WHERE H~EBELN    IN  S_EBELN
                      AND H~BUKRS    IN  S_BUKRS
                      AND S~EKGRP    IN  S_EKGRP
                      AND H~ZFOPNNO  IN  S_OPNNO
                      AND NOT H~ZFREQTY IN ('PU', 'LO')
                      AND H~ZFREQNO  IN  S_REQNO
                      AND H~ZFSHCU   IN  S_SHCU
                      AND H~INCO1    IN  S_INCO1
                      AND S~ZFOPNNM  IN  S_OPNNM
                      AND S~CDAT     IN  S_CDAT.
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  SORT IT_REQNO BY ZFREQNO.

* FOR ALL ENTRIES가 SUM과 함께 쓰이지 못하기 땜시.
* RANGE 변수에 넣고 처리한다.
  LOOP AT IT_REQNO.
     MOVE : 'I'              TO R_REQNO-SIGN,
            'EQ'             TO R_REQNO-OPTION,
            IT_REQNO-ZFREQNO TO R_REQNO-LOW,
            SPACE            TO R_REQNO-HIGH.
     APPEND  R_REQNO.
  ENDLOOP.

* 추출한 수입의뢰 관리번호로 VIEW 조회.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZVREQHD_IT
           WHERE  ZFREQNO   IN   R_REQNO
           AND    MATNR     IN   S_MATNR
           AND    TXZ01     IN   S_TXZ01.
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  SELECT ZFREQNO ZFITMNO SUM( BLMENGE ) AS BLMENGE
         INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLIT
         FROM ZTBLIT
         WHERE  ZFREQNO   IN   R_REQNO
         GROUP BY ZFREQNO ZFITMNO.

  LOOP AT IT_TAB.
      CLEAR W_EKGRP.
      W_TABIX = SY-TABIX.

      SELECT SINGLE EKGRP
        INTO W_EKGRP
        FROM ZTREQST
       WHERE ZFREQNO = IT_TAB-ZFREQNO.

      SELECT SINGLE EKNAM INTO IT_TAB-EKGRP_NM
        FROM T024
       WHERE EKGRP = W_EKGRP.

     MODIFY  IT_TAB INDEX W_TABIX.
  ENDLOOP.

  SORT IT_TAB BY ZFREQNO.

ENDFORM.                                       "P1000_READ_DATA.

*----------------------------------------------------------------------*
* FORM P3000_DATA_WRITE                                                *
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE.
   SET TITLEBAR  'ZIMR21'.
   SET PF-STATUS 'ZIMR21'.                      " GUI STATUS SETTING

   CLEAR : W_TABIX, W_COUNT.

   LOOP AT IT_TAB.
       W_TABIX = SY-TABIX.
       READ TABLE IT_ZTBLIT WITH KEY ZFREQNO = IT_TAB-ZFREQNO
                                     ZFITMNO = IT_TAB-ZFITMNO.
       IF SY-SUBRC NE 0.
          IT_TAB-SUM_REQ = IT_TAB-MENGE.
          IT_TAB-SUM_BL  = 0.
          IT_TAB-RE_ITEM = IT_TAB-MENGE.
          MODIFY IT_TAB INDEX W_TABIX.
       ELSE.
          IT_TAB-SUM_REQ = IT_TAB-MENGE.
          IT_TAB-SUM_BL  = IT_ZTBLIT-BLMENGE.
          IT_TAB-RE_ITEM = IT_TAB-MENGE - IT_ZTBLIT-BLMENGE.
          MODIFY IT_TAB INDEX W_TABIX.
       ENDIF.

       IF IT_TAB-RE_ITEM LE 0.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ELSE.

          W_MOD = W_COUNT MOD 2.

          IF W_MOD = 0.
             FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
          ELSE.
              FORMAT RESET.
          ENDIF.

          WRITE:/ SY-VLINE,
                  (10)IT_TAB-EBELN,          SY-VLINE,
                   (7)IT_TAB-ZFITMNO,        SY-VLINE NO-GAP,
                  (24)IT_TAB-ZFOPNNO,        SY-VLINE NO-GAP,
                   (8)IT_TAB-MATNR,          SY-VLINE NO-GAP,
                  (20)IT_TAB-TXZ01,          SY-VLINE NO-GAP,
                  (17)IT_TAB-MENGE UNIT IT_TAB-MEINS,SY-VLINE NO-GAP,
                  (17)IT_TAB-RE_ITEM UNIT IT_TAB-MEINS,SY-VLINE NO-GAP,
                  (10)IT_TAB-LIFNR,          SY-VLINE NO-GAP,
                   (8)IT_TAB-EKGRP_NM,
                                            SY-VLINE NO-GAP.
          W_COUNT = W_COUNT + 1.
          HIDE IT_TAB.
       ENDIF.
  ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE

*----------------------------------------------------------------------*
* FORM P3000_LAST_WRITE                                                *
*----------------------------------------------------------------------*
FORM P3000_LAST_WRITE.
   FORMAT RESET.
   IF W_COUNT GT 0.
      WRITE : / SY-ULINE.
      IF SY-LANGU EQ '3'.
         WRITE : /120 '총', W_COUNT, '건'.
      ELSE.
         WRITE : /110 'Total', W_COUNT, 'Case'.
      ENDIF.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*----------------------------------------------------------------------*
* P3000_TITLE_WRITE.                                                   *
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /56  '[ 선적정보 미등록 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /122 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE :   /  SY-VLINE,
         (10)  '구매문서'        CENTERED,  SY-VLINE,
          (7)  'Item'            CENTERED,  SY-VLINE NO-GAP,
         (24)  'L/C  No'         CENTERED,  SY-VLINE NO-GAP,
          (8)  '자재번호'        CENTERED,  SY-VLINE NO-GAP,
         (20)  '자재내역'        CENTERED,  SY-VLINE NO-GAP,
         (17)  '수입의뢰 수량'   CENTERED,  SY-VLINE NO-GAP,
         (17)  'B/L 미참조 수량' CENTERED,  SY-VLINE NO-GAP,
         (10)  '자재납기일'      CENTERED,  SY-VLINE NO-GAP,
         (8)   '구매그룹'        CENTERED,  SY-VLINE NO-GAP.
  WRITE:    /   SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE.

*----------------------------------------------------------------------*
* FORM RESET_LIST                                                      *
*----------------------------------------------------------------------*
FORM RESET_LIST.

   MOVE 0 TO SY-LSIND.

   IF SY-LANGU EQ '3'.
      PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
   ELSE.
      PERFORM   P3000_TITLE_WRITE_UN.
   ENDIF.

   PERFORM   P3000_DATA_WRITE.
   PERFORM   P3000_LAST_WRITE.

ENDFORM.                    " RESET_LIST

*----------------------------------------------------------------------*
* USER COMMAND.                                                        *
*----------------------------------------------------------------------*
AT USER-COMMAND.
   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.
         W_FIELD_NM = 'EBELN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES IT_TAB USING SY-UCOMM.
      WHEN 'CRBL'.
         IF IT_TAB IS INITIAL.
            MESSAGE S962.
         ELSE.
           IF SY-LANGU EQ '3'.
              SPOP-TITEL = 'House B/L No. 입력화면'.
           ELSE.
              SPOP-TITEL = 'House B/L No. Input Screen'.
           ENDIF.

           CALL SCREEN 0014 STARTING AT 27 6
                            ENDING   AT 70 8.
           IF ANTWORT EQ 'Y'.
              SET PARAMETER ID 'ZPHBLNO' FIELD ZSREQHD-ZFHBLNO.
              SET PARAMETER ID 'ZPREQNO' FIELD IT_TAB-ZFREQNO.
              CALL TRANSACTION 'ZIM21' AND SKIP FIRST SCREEN.

              PERFORM P1000_READ_DATA   USING W_ERR_CHK.
              IF W_ERR_CHK EQ 'Y'.
                 LEAVE TO SCREEN 0.
              ENDIF.
              PERFORM RESET_LIST.
            ENDIF.
          ENDIF.
       WHEN 'DISP'.
         IF IT_TAB IS INITIAL.
            MESSAGE S962.
         ELSE.
            SET PARAMETER ID 'ZPOPNNO'   FIELD  SPACE.
            SET PARAMETER ID 'BES'       FIELD  SPACE.
            SET PARAMETER ID 'ZPREQNO'   FIELD  IT_TAB-ZFREQNO.

            CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

            PERFORM P1000_READ_DATA   USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.
               LEAVE TO SCREEN 0.
            ENDIF.
            PERFORM RESET_LIST.
         ENDIF.
      WHEN 'REFR'.
         PERFORM P1000_READ_DATA   USING W_ERR_CHK.
         PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.
   CLEAR IT_TAB.
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0014  OUTPUT
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0014 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0014  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_BL_DOCUMENT_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_BL_DOCUMENT_SCR0014 INPUT.

   CASE SY-UCOMM.
     WHEN 'CANC' OR 'NO'.   EXIT.
     WHEN OTHERS.
   ENDCASE.

  IF ZSREQHD-ZFHBLNO IS INITIAL.   " B/L NO를 입력하지 않았을 경우.
        MESSAGE E304.
  ELSE.
* B/L NO에 Count
*        SELECT COUNT( * ) INTO  W_COUNT
*                          FROM  ZTBL
*                          WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
*        CASE W_COUNT.
*           WHEN 0.     MESSAGE E305 WITH ZSREQHD-ZFHBLNO.
*           WHEN 1.
*              SELECT ZFBLNO INTO ZSREQHD-ZFBLNO UP TO 1 ROWS
*                            FROM ZTBL
*                            WHERE ZFHBLNO EQ ZSREQHD-ZFHBLNO.
*                   EXIT.
*              ENDSELECT.
*           WHEN OTHERS.
*              PERFORM P2000_BL_DOC_ITEM_SELECT.
*              IF ANTWORT NE 'Y'.    EXIT.    ENDIF.
*              PERFORM P2000_SEARCH_FIELD_MOVE.
*        ENDCASE.
  ENDIF.


ENDMODULE.                 " CHECK_BL_DOCUMENT_SCR0014  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0014  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0014 INPUT.

  CASE OK-CODE.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN OTHERS.   EXIT.
  ENDCASE.

  LEAVE TO SCREEN 0.

ENDMODULE.                 " GET_OK_CODE_SCR3512  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_UN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_UN.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /52  '[ Shipment information not created ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /112 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE :   /  SY-VLINE,
         (10)  'P/O No'               CENTERED,  SY-VLINE,
          (7)  'Item No'              CENTERED,  SY-VLINE NO-GAP,
         (24)  'L/C  No'              CENTERED,  SY-VLINE NO-GAP,
          (8)  'Matr No'              CENTERED,  SY-VLINE NO-GAP,
         (20)  'Matr description'     CENTERED,  SY-VLINE NO-GAP,
         (17)  'Imp req quantity'     CENTERED,  SY-VLINE NO-GAP,
         (17)  'B/L not ref quan'     CENTERED,  SY-VLINE NO-GAP,
         (10)  'Vendor'               CENTERED,  SY-VLINE NO-GAP,
          (8)  'Pur grup'             CENTERED,  SY-VLINE NO-GAP.
  WRITE:    /   SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE_UN
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
