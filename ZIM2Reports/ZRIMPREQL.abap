*&---------------------------------------------------------------------*
*& Report  ZRIMPREQL                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입신고의뢰현황                                      *
*&      작성자 : 이석철 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.30                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :                                                       *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMPREQL    MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

* TABLE 및 변수 DEFINE.------------------------------------------------*
TABLES : ZTBL, ZTBLIT, ZTIV, ZTIVIT.

* INTERNAL TABLES.
DATA:   BEGIN OF IT_ZTBL OCCURS 0,                  ">>BL + ITEM.
        ZFBLIT   LIKE        ZTBLIT-ZFBLIT,
        EBELN    LIKE        ZTBLIT-EBELN,     ">>구매문서번호.
        EBELP    LIKE        ZTBLIT-EBELP,     ">>구매문서 품목번호.
        ZFREQNO  LIKE        ZTBLIT-ZFREQNO,   ">>수입의뢰 관리번호.
        ZFITMNO  LIKE        ZTBLIT-ZFITMNO,   ">>수입문서 품목번호.
        MATNR    LIKE        ZTBLIT-MATNR,     ">>자재번호.
        STAWN    LIKE        ZTBLIT-STAWN,     ">>상품코드/수입코드번호.
        TXZ01    LIKE        ZTBLIT-TXZ01,     ">>내역.
        BLMENGE  LIKE        ZTBLIT-BLMENGE,   ">>B/L 수량.
        MEINS    LIKE        ZTBLIT-MEINS,     ">>기본단위.
        NETPR    LIKE        ZTBLIT-NETPR,     ">>단가.
        PEINH    LIKE        ZTBLIT-PEINH,     ">>가격단위.
        BPRME    LIKE        ZTBLIT-BPRME,    ">>Order price unit.
        MATKL    LIKE        ZTBLIT-MATKL,     ">>자재그룹.
        WERKS    LIKE        ZTBLIT-WERKS,     ">>플랜트.
        LGORT    LIKE        ZTBLIT-LGORT,     ">>저장위치.
        BLOEKZ   LIKE        ZTBLIT-BLOEKZ.    ">>B/L 자재 삭제지시자.
        INCLUDE  STRUCTURE   ZTBL.            ">>BL TABLE.
DATA:   END   OF IT_ZTBL.

DATA:   BEGIN OF IT_IVSUM    OCCURS 0,
        ZFBLNO   LIKE        ZTIVIT-ZFBLNO,
        ZFBLIT   LIKE        ZTIVIT-ZFBLIT,
        IV_SUM   LIKE        ZTIVIT-CCMENGE.
DATA:   END  OF  IT_IVSUM.

DATA:   BEGIN OF IT_SELECTED    OCCURS 0,
        ZFBLNO   LIKE           ZTBL-ZFBLNO,
        ZFHBLNO  LIKE           ZTBL-ZFHBLNO.
*        BL_SUM   LIKE        ZTBLIT-BLMENGE.
DATA:   END  OF  IT_SELECTED.


DATA:   W_ERR_CHK(1),
        MARKFIELD(1),
        W_IDX_CNT(10),
        W_TEM_SUM(20),
        W_TEM_NET(20),
        W_SELECTED_LINES(4),
*        W_PEINH  TYPE        STRING,
        W_PEINH(3),
        W_IVSUM  LIKE        ZTIVIT-CCMENGE,
        W_BLSUM  LIKE        ZTBLIT-BLMENGE.
*        W_IVSUM  TYPE         P,
*        W_BLSUM  TYPE         P.

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_REBELN   FOR  ZTBL-ZFREBELN,     " 대표 P/O번호.
                   S_MATNR    FOR  ZTBLIT-MATNR,      " 자재번호.
                   S_TXZ01    FOR  ZTBLIT-TXZ01,      " 자재내역
                   S_OPNNO    FOR  ZTBL-ZFOPNNO,      " 대표 L/C
                   S_HBLNO    FOR  ZTBL-ZFHBLNO,      " House B/L No
                   S_LIFNR    FOR  ZTBL-LIFNR,        " VENDOR.
                   S_FORD     FOR  ZTBL-ZFFORD,       " FORWEDER,
                   S_VIA      FOR  ZTBL-ZFVIA,        " VIA.
                   S_ETA      FOR  ZTBL-ZFETA,        " ETA.
                   S_RETA     FOR  ZTBL-ZFRETA,       " 실입항일.
                   S_ZFCARC   FOR  ZTBL-ZFCARC,       " 선적국.
                   S_POYN     FOR  ZTBL-ZFPOYN        " 유무환구분.
                     NO-EXTENSION NO INTERVALS,
                   S_PONC     FOR  ZTBL-ZFPONC        " 수입거래구분.
                     NO-EXTENSION NO INTERVALS,
                   S_INCO     FOR  ZTBL-INCO1         " INCOTERMS.
                     NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
SET   TITLEBAR    'ZIML01'.

TOP-OF-PAGE.
    PERFORM P3000_TITLE_WRITE.             "헤더 출력.

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.

    PERFORM P1000_READ_TABLES USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.

    PERFORM P3000_DATA_WRITE.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'IMREQ'.                    " 통관요청.

          PERFORM P2000_MULTI_SELECTION .

          IF W_SELECTED_LINES EQ 0.
               MESSAGE S766. EXIT.
          ELSEIF W_SELECTED_LINES EQ 1.
               WRITE:/ '아자'.
          ELSE.
               MESSAGE E965. EXIT.
          ENDIF.
      WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /45  '[ 수입신고 의뢰대상 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  SKIP 1.
  WRITE:/100 ' Date : ', SY-DATUM, SY-ULINE.

  FORMAT COLOR COL_KEY INTENSIFIED OFF.
  WRITE:/ SY-VLINE, (3)SY-VLINE NO-GAP,
       (8)'P/O No.' CENTERED, SY-VLINE,
      (25)'L/C No.' CENTERED, SY-VLINE,
       (7)'Vender'  CENTERED, SY-VLINE,
      (10)'ETA'     CENTERED, SY-VLINE,
       (6)'선적국'  CENTERED, SY-VLINE,
      (20)'Vessel'  CENTERED, SY-VLINE,
      (20)'도착항'  CENTERED, 120 SY-VLINE.

  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, (3)SY-VLINE NO-GAP,
       (8)'Forwader'      CENTERED, SY-VLINE,
       (25)'B/L No.'      CENTERED, SY-VLINE,
        (7)'Via'          CENTERED, SY-VLINE,
       (10)'실입항일'     CENTERED, SY-VLINE,
       (6)'통 화'          CENTERED, SY-VLINE,
       (13)'유/무환구분'  CENTERED, SY-VLINE,
       (13)'수입거래구분' CENTERED, SY-VLINE,
       (10)'INCOTERMS'    CENTERED, 120 SY-VLINE.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE:/ SY-VLINE, (3)SY-VLINE NO-GAP,
      (8)'품목번호'          CENTERED,    SY-VLINE,
      (15)'자재번호'         CENTERED,    SY-VLINE,
      (39)'자  재  명'       CENTERED,    SY-VLINE,
      (20)'미통관수량(단위)' CENTERED, SY-VLINE,
      (20)'단가(단위)'       CENTERED, 120 SY-VLINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TABLES
*&---------------------------------------------------------------------*
FORM P1000_READ_TABLES USING    P_W_ERR_CHK.

    REFRESH IT_ZTBL.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBL
             FROM ZTBL AS H INNER JOIN ZTBLIT AS I
             ON H~ZFBLNO EQ I~ZFBLNO
          WHERE H~ZFREBELN IN S_REBELN
            AND I~MATNR    IN S_MATNR
            AND H~ZFOPNNO  IN S_OPNNO
            AND H~ZFHBLNO  IN S_HBLNO
            AND H~LIFNR    IN S_LIFNR
            AND H~ZFFORD   IN S_FORD
            AND H~ZFVIA    IN S_VIA
            AND H~ZFETA    IN S_ETA
            AND H~ZFRETA   IN S_RETA
            AND H~ZFCARC   IN S_ZFCARC
            AND H~ZFPOYN   IN S_POYN
            AND H~ZFPONC   IN S_PONC
            AND H~INCO1    IN S_INCO
            ORDER BY I~ZFBLNO MATNR.

     IF SY-SUBRC NE 0.
        W_ERR_CHK = 'Y'.
     ENDIF.

    REFRESH IT_IVSUM.
    SELECT ZFBLNO ZFBLIT SUM( CCMENGE ) INTO TABLE IT_IVSUM
                         FROM ZTIVIT
                         GROUP BY ZFBLNO ZFBLIT.

ENDFORM.                    " P1000_READ_TABLES
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

   SET PF-STATUS  'ZIML01'.
   SET TITLEBAR   'ZIML01'.

  CLEAR W_BLSUM.
  CLEAR W_IDX_CNT.

  LOOP AT IT_ZTBL.

    W_BLSUM = IT_ZTBL-BLMENGE.

      READ TABLE IT_IVSUM WITH KEY ZFBLNO = IT_ZTBL-ZFBLNO
                                   ZFBLIT = IT_ZTBL-ZFBLIT.
      W_IVSUM = IT_IVSUM-IV_SUM.


    IF W_BLSUM GT W_IVSUM.
      W_IDX_CNT = W_IDX_CNT + 1.
      W_BLSUM = W_BLSUM - W_IVSUM.

      ON CHANGE OF IT_ZTBL-ZFBLNO.   " HEADER WRITE.
        FORMAT RESET.
        FORMAT COLOR COL_KEY INTENSIFIED OFF.
        WRITE:/ SY-ULINE.
        WRITE:/ SY-VLINE NO-GAP, MARKFIELD AS CHECKBOX NO-GAP,
              (3)SY-VLINE NO-GAP,
              (8)IT_ZTBL-ZFREBELN, SY-VLINE,
             (25)IT_ZTBL-ZFOPNNO,  SY-VLINE,
             (7)IT_ZTBL-LIFNR,     SY-VLINE,
             (10)IT_ZTBL-ZFETA,    SY-VLINE,
             (6)IT_ZTBL-ZFCARC,   SY-VLINE,
             (20)IT_ZTBL-ZFCARNM,  SY-VLINE,
             (20)IT_ZTBL-ZFAPRT,  120 SY-VLINE.

        HIDE IT_ZTBL.
        FORMAT RESET.
        FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
        WRITE:/ SY-VLINE, (3)SY-VLINE NO-GAP,
             (8)IT_ZTBL-ZFFORD,     SY-VLINE,
             (25)IT_ZTBL-ZFHBLNO,  SY-VLINE,
             (7)IT_ZTBL-ZFVIA, SY-VLINE,
             (10)IT_ZTBL-ZFRETA,    SY-VLINE,
             (6)IT_ZTBL-ZFBLAMC, SY-VLINE,
             (13)IT_ZTBL-ZFPOYN,   SY-VLINE,
             (13)IT_ZTBL-ZFPONC,  SY-VLINE,
             (10)IT_ZTBL-INCO1,  120 SY-VLINE.
      ENDON.

        FORMAT RESET.
        FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
        CLEAR: W_TEM_SUM, W_TEM_NET.
        WRITE: W_BLSUM TO W_TEM_SUM UNIT IT_ZTBL-MEINS.
        CONCATENATE W_TEM_SUM '(' IT_ZTBL-MEINS ')'
                    INTO W_TEM_SUM.

        WRITE: IT_ZTBL-NETPR
               TO W_TEM_NET CURRENCY IT_ZTBL-ZFBLAMC. "
        WRITE: IT_ZTBL-PEINH TO W_PEINH.   " STIFIED.
        CONCATENATE W_TEM_NET '(' W_PEINH ')'
                    INTO W_TEM_NET.

        WRITE:/ SY-VLINE, (3)SY-VLINE NO-GAP,
             (8)IT_ZTBL-EBELN,    SY-VLINE,
             (15)IT_ZTBL-MATNR,    SY-VLINE,
             (39)IT_ZTBL-TXZ01,    SY-VLINE,
*             (12)W_BLSUM, '(', (5)IT_ZTBL-MEINS, ')', SY-VLINE,
             (20)W_TEM_SUM, SY-VLINE,
             (20)W_TEM_NET, 120 SY-VLINE.
    ENDIF.
  ENDLOOP.

   IF W_IDX_CNT EQ 0.
      W_ERR_CHK = 'Y'. EXIT.
   ELSEIF W_IDX_CNT NE 0.
      WRITE:/ SY-ULINE,
            / '총',  W_IDX_CNT, '건'.
   ENDIF.


ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

    REFRESH IT_SELECTED.
    CLEAR   IT_SELECTED.
    CLEAR W_SELECTED_LINES.

          DO.
             CLEAR MARKFIELD.
             READ LINE SY-INDEX FIELD VALUE MARKFIELD.

             IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
             IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
                MOVE : IT_ZTBL-ZFBLNO    TO IT_SELECTED-ZFBLNO,
                       IT_ZTBL-ZFHBLNO   TO IT_SELECTED-ZFHBLNO.
                APPEND IT_SELECTED.
                ADD 1 TO W_SELECTED_LINES.
             ENDIF.
          ENDDO.
ENDFORM.                    " P2000_MULTI_SELECTION
