*&---------------------------------------------------------------------*
*& Report  ZRIMINSVP                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : VP 현황관리 프로그램                                  *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.3.28                                             *
*&---------------------------------------------------------------------*
*&   DESC. :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMINSVP    NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 140.

*-----------------------------------------------------------------------
* Table Declaration for Program Configuration.
*-----------------------------------------------------------------------
TABLES: EKKO,                              " ABAP Standard Header Table.
        ZTREQHD,                           " 수입의뢰 Header Table.
        ZTREQIT,                           " 수입의뢰 Item Table.
        LFA1,                              " 거래처 Master Table.
        ZTINS,                             " 부험부보.
        ZTINSRSP,                          " 보험부보 Response.
        ZTMSHD,                            " 모선관리 Header Table.
        ZTMSIT,                            " 모선관리 ITEM Table.
        ZTREQORJ,                          " 수입의뢰 원산지 내역 Table.
        T005T.                             " 국가이름 Table.

*-----------------------------------------------------------------------
* Logic for P/O Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_PO OCCURS 1000,
        EBELN    LIKE   EKKO-EBELN,        " P/O Header No..
        EKGRP    LIKE   EKKO-EKGRP,        " 구매그룹.
        LIFNR    LIKE   EKKO-LIFNR,        " Vendor's Account No..
        NAME1    LIKE   LFA1-NAME1,        " 이름1.
      END OF IT_PO.

*-----------------------------------------------------------------------
* Logic for L/C Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        EBELP     LIKE   ZTREQIT-EBELP,    " P/O Item Number.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFOPNNO   LIKE   ZTREQHD-ZFOPNNO,  " 신용장-승인번호.
        BUKRS     LIKE   ZTREQHD-BUKRS,    " 회사코드.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " 수입문서 품목번호.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        MENGE     LIKE   ZTREQIT-MENGE,    " 수입의뢰수량.
        MEINS     LIKE   ZTREQIT-MEINS,    " Base Unit of Measure.
        NETPR     LIKE   ZTREQIT-NETPR,    " Net Price.
        PEINH     LIKE   ZTREQIT-PEINH,    " Price Unit.
        BPRME     LIKE   ZTREQIT-BPRME,    " Order Price Unit.
        TXZ01     LIKE   ZTREQIT-TXZ01,    " 내역.
        ZFORIG    LIKE   ZTREQORJ-ZFORIG,  " 자재원산국..
        LANDX     LIKE   T005T-LANDX,      " 국가이름..
      END OF IT_RN.

*-----------------------------------------------------------------------
* Logic for Insurance Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_INS_TMP OCCURS 1000,
        ZFREQNO   LIKE   ZTINS-ZFREQNO, " 수입의뢰 관리번호..
        ZFINSEQ   LIKE   ZTINS-ZFINSEQ,
        ZFAMDNO   LIKE   ZTINS-ZFAMDNO, " Amend Seq..
      END OF IT_INS_TMP.

*-----------------------------------------------------------------------
* Logic for Insurance Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_INS OCCURS 1000,
        ZFREQNO   LIKE   ZTINSRSP-ZFREQNO, " 수입의뢰 관리번호..
        ZFAMDNO   LIKE   ZTINSRSP-ZFAMDNO, " Amend Seq..
        ZFEXRT    LIKE   ZTINSRSP-ZFEXRT,  " 환율..
        ZFTAMI    LIKE   ZTINSRSP-ZFTAMI,  " 총보험금액..
        ZFTAMIC   LIKE   ZTINSRSP-ZFTAMIC, " 총보험금액 통화..
        ZFCAMI    LIKE   ZTINSRSP-ZFCAMI,  " Cargo 분..
        ZFCAMIC   LIKE   ZTINSRSP-ZFCAMIC, " Cargo 분 통화..
        ZFDAMI    LIKE   ZTINSRSP-ZFDAMI,  " Duty 분..
        ZFDAMIC   LIKE   ZTINSRSP-ZFDAMIC, " Duty 분 통화..
        ZFTPR     LIKE   ZTINSRSP-ZFTPR,   " Total Premium..
        ZFTPRC    LIKE   ZTINSRSP-ZFTPRC,  " Total Premium 통화..
        ZFCPR     LIKE   ZTINSRSP-ZFCPR,   " Cargo Premium..
        ZFCPRC    LIKE   ZTINSRSP-ZFCPRC,  " Cargo Premium 통화..
        ZFDPR     LIKE   ZTINSRSP-ZFDPR,   " Duty Premium..
        ZFDPRC    LIKE   ZTINSRSP-ZFDPRC,  " Duty Primium 통화..
        ZFVPR     LIKE   ZTINSRSP-ZFVPR,   " V/P Premium..
        ZFVPRC    LIKE   ZTINSRSP-ZFVPRC,  " V/P Premium 통화..
        ZFIPR     LIKE   ZTINSRSP-ZFIPR,   " ITE Premium..
        ZFIPRC    LIKE   ZTINSRSP-ZFIPRC,  " ITE Premium 통화..
        ZFSCANU   LIKE   ZTINSRSP-ZFSCANU, " 보조운송수단 항차..
        ZFILN     LIKE   ZTINSRSP-ZFILN,   " 내륙출하지 국가..
        ZFLINM    LIKE   ZTINSRSP-ZFLINM,  " 내륙출하지명..
        ZFISDT    LIKE   ZTINSRSP-ZFISDT, " 보험증권발급일..
        ZFISLO    LIKE   ZTINSRSP-ZFISLO,  " 보험증권발급지..
        ZFINNO    LIKE   ZTINS-ZFINNO,     " 보험증권번호.
        ZFINAMT   LIKE   ZTINS-ZFINAMT,    " 보험료.
        ZFINAMTC  LIKE   ZTINS-ZFINAMTC,   " 보험료통화.
*        ZFINSDT   LIKE   ZTINS-ZFINSDT,    " 보험개시일.
        ZFIVAMT   LIKE   ZTINS-ZFIVAMT,    " Invoice 금액.
        WAERS     LIKE   ZTINS-WAERS,      " 통화키.
      END OF IT_INS.

*-----------------------------------------------------------------------
* Logic for Mother Ship Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_MS OCCURS 1000,
        ZFMSNO    LIKE   ZTMSHD-ZFMSNO,    " 모선관리번호..
        ZFMSNM    LIKE   ZTMSHD-ZFMSNM,    " 모선명..
        ZFREQNO   LIKE   ZTMSIT-ZFREQNO,   " 수입의뢰관리번호..
        ZFSHSDF   LIKE   ZTMSHD-ZFSHSDF,   " 선적일(From)..
        ZFSHSDT   LIKE   ZTMSHD-ZFSHSDT,   " 선적일(To)..
      END OF IT_MS.

*-----------------------------------------------------------------------
* Logic for Vendor Master Data Reference.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_LFA OCCURS 1000,
        LIFNR     LIKE   LFA1-LIFNR,
        NAME1     LIKE   LFA1-NAME1,
      END OF IT_LFA.

*-----------------------------------------------------------------------
* Other Data Declaration.
*-----------------------------------------------------------------------
DATA: W_ERR_CHK   TYPE C,
      W_TABIX     TYPE I.
*-----------------------------------------------------------------------
* Search Condition Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               BUKRS     FOR ZTREQHD-BUKRS,          " 회사코드.
               S_EBELN   FOR EKKO-EBELN,             " P/O No.
               S_ISDT    FOR ZTINSRSP-ZFISDT,        " 증권 발급일.
               S_REQNO   FOR ZTREQHD-ZFREQNO,        " 수입의뢰 No.
               S_OPNNO   FOR ZTREQHD-ZFOPNNO,        " 신용장-승인번호.
               S_LIFNR   FOR ZTREQHD-LIFNR.          " Vendor.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* Initialization.
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.

*-----------------------------------------------------------------------
* Start-Of-Selection.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Read L/C Internal Table.
   PERFORM P1000_READ_RN_DATA.

* Read P/O Internal Table.
   PERFORM P1000_READ_PO_DATA.

* Read Mother Ship Internal Table.
   PERFORM P1000_READ_MS_DATA.

* Read Insurance Internal Table.
   PERFORM P1000_READ_INS_DATA.

* Read Vendor Master Internal Table.
   PERFORM P1000_READ_LFA_DATA.

*-----------------------------------------------------------------------
* Top-Of-Page.
*-----------------------------------------------------------------------
TOP-OF-PAGE.

   PERFORM P3000_WRITE_TITLE.

*-----------------------------------------------------------------------
* End-Of-Selection.
*-----------------------------------------------------------------------
END-OF-SELECTION.
   CHECK W_ERR_CHK NE 'Y'.
* Title Text Write.
   SET TITLEBAR 'TIT1'.
   SET PF-STATUS 'ZIMR15'.

   PERFORM P2000_SORT_RESULT_DATA.

   PERFORM P3000_WRITE_RESULT_DATA.


*-----------------------------------------------------------------------
* At User-Command.
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'INS'.
         IF NOT IT_INS-ZFREQNO IS INITIAL.
            SET PARAMETER ID 'ZPREQNO' FIELD IT_INS-ZFREQNO.
            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
            SET PARAMETER ID 'BES' FIELD ''.
            CALL TRANSACTION 'ZIM43' AND SKIP FIRST SCREEN.
         ENDIF.
   ENDCASE.
   CLEAR: IT_INS.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

   SELECT *
     INTO   CORRESPONDING FIELDS OF TABLE IT_PO
     FROM   EKKO
     FOR    ALL  ENTRIES  IN  IT_RN
     WHERE  EBELN    IN S_EBELN
     AND    LIFNR    IN S_LIFNR
     AND    EBELN    EQ IT_RN-EBELN.

   IF SY-SUBRC NE 0.
   WRITE 'NOT A VALID INPUT.'.
   EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_RN_DATA.

   W_ERR_CHK = 'N'.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_RN
     FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
     ON     H~ZFREQNO EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN  S_REQNO
     AND    H~ZFOPNNO  IN  S_OPNNO
     AND    H~EBELN    IN  S_EBELN
     AND    H~LIFNR    IN  S_LIFNR
     AND    I~ZFITMNO  EQ  '00010'
     AND    H~ZFINSYN  NE  SPACE.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

   LOOP AT IT_RN.
      ON CHANGE OF IT_RN-ZFREQNO.
         SELECT SINGLE * FROM ZTREQORJ
                        WHERE ZFREQNO = IT_RN-ZFREQNO
                          AND ZFLSG7O = '00010'.

         SELECT SINGLE * FROM T005T
                        WHERE LAND1   = ZTREQORJ-ZFORIG
                          AND SPRAS   = SY-LANGU.
      ENDON.

      MOVE: ZTREQORJ-ZFORIG TO IT_RN-ZFORIG,
            T005T-LANDX     TO IT_RN-LANDX.
      MODIFY IT_RN.
   ENDLOOP.

ENDFORM.                    " P1000_READ_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_MS_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_MS
     FROM   ZTMSHD AS H INNER JOIN ZTMSIT AS I
     ON     H~ZFMSNO EQ I~ZFMSNO
     FOR ALL ENTRIES   IN  IT_RN
     WHERE  I~ZFREQNO  EQ  IT_RN-ZFREQNO.

ENDFORM.                    " P1000_READ_MS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_INS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_INS_DATA.

   SELECT H~ZFREQNO MAX( H~ZFINSEQ ) AS ZFINSEQ
                    MAX( H~ZFAMDNO ) AS ZFAMDNO
   INTO   CORRESPONDING FIELDS OF TABLE IT_INS_TMP
   FROM   ZTINS AS H INNER JOIN ZTREQHD  AS I
   ON     H~ZFREQNO     EQ  I~ZFREQNO
   WHERE  I~ZFREQNO     IN  S_REQNO
   AND    I~ZFOPNNO     IN  S_OPNNO
   AND    I~EBELN       IN  S_EBELN
   AND    I~LIFNR       IN  S_LIFNR
   GROUP BY
         H~ZFREQNO.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_INS
     FROM   ZTINSRSP AS H INNER JOIN ZTINS AS I
     ON     H~ZFREQNO  EQ  I~ZFREQNO
     AND    H~ZFINSEQ  EQ  I~ZFINSEQ
     AND    H~ZFAMDNO  EQ  I~ZFAMDNO
     FOR ALL ENTRIES   IN  IT_INS_TMP
     WHERE  H~ZFREQNO  EQ  IT_INS_TMP-ZFREQNO
     AND    H~ZFINSEQ  EQ  IT_INS_TMP-ZFINSEQ
     AND    H~ZFAMDNO  EQ  IT_INS_TMP-ZFAMDNO.


ENDFORM.                    " P1000_READ_INS_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_LFA_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_LFA_DATA.

   SELECT *
     INTO   CORRESPONDING FIELDS OF TABLE IT_LFA
     FROM   LFA1
     FOR ALL ENTRIES IN IT_RN
     WHERE  LIFNR EQ IT_RN-LIFNR.

ENDFORM.                    " P1000_READ_LFA_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_RESULT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SORT_RESULT_DATA.

ENDFORM.                    " P2000_SORT_RESULT_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_TITLE.
   WRITE: 65 '[V/P 현황]' NO-GAP COLOR COL_HEADING INTENSIFIED ON.
   SKIP.
   WRITE: 125 'DATE:' NO-GAP, SY-DATUM(4) NO-GAP, '/' NO-GAP,
                  SY-DATUM+4(2) NO-GAP, '/' NO-GAP,
                  SY-DATUM+6(2) NO-GAP.
   FORMAT COLOR COL_HEADING INTENSIFIED OFF.
   WRITE: / SY-ULINE, SY-VLINE NO-GAP,
            'L/C 번호' NO-GAP,
            37 SY-VLINE NO-GAP, '공급사' NO-GAP,
            73 SY-VLINE NO-GAP, '원산지' NO-GAP,
            84 SY-VLINE NO-GAP, '보험료' NO-GAP,
           115 SY-VLINE NO-GAP, 'V/P 금액' NO-GAP,
           140 SY-VLINE NO-GAP,
          / SY-VLINE NO-GAP, '모선명' NO-GAP,
            37 SY-VLINE NO-GAP, '보험증권번호' NO-GAP,
            73 SY-VLINE NO-GAP, '발급일' NO-GAP,
            84 SY-VLINE NO-GAP, '자재명' NO-GAP,
           115 SY-VLINE NO-GAP, 'Invoice Amount' NO-GAP,
           140 SY-VLINE NO-GAP,
            SY-ULINE.
ENDFORM.                    " P3000_WRITE_TITLE

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_RESULT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_RESULT_DATA.

   W_TABIX = 0.
   LOOP AT IT_RN.
*      LOOP AT IT_PO
*         WHERE EBELN EQ IT_RN-EBELN.
         READ TABLE IT_PO   WITH KEY EBELN   = IT_RN-EBELN.
         READ TABLE IT_LFA  WITH KEY LIFNR   = IT_PO-LIFNR.
         IF SY-SUBRC NE 0. CLEAR IT_LFA. ENDIF.
         READ TABLE IT_MS   WITH KEY ZFREQNO = IT_RN-ZFREQNO.
         IF SY-SUBRC NE 0. CLEAR IT_MS.  ENDIF.
         READ TABLE IT_INS    WITH KEY ZFREQNO = IT_RN-ZFREQNO.
         IF SY-SUBRC NE 0. CONTINUE.     ENDIF.
         IF NOT IT_INS-ZFVPR IS INITIAL.
            FORMAT COLOR COL_NORMAL INTENSIFIED ON.
            WRITE: / SY-VLINE NO-GAP, IT_RN-ZFOPNNO NO-GAP,
                     SY-VLINE NO-GAP, IT_LFA-NAME1 NO-GAP,
                     SY-VLINE NO-GAP, IT_RN-LANDX NO-GAP,
                  84 SY-VLINE NO-GAP, IT_INS-ZFINAMTC NO-GAP,
                  96 IT_INS-ZFINAMT CURRENCY IT_INS-ZFINAMTC NO-GAP,
                 115 SY-VLINE NO-GAP, IT_INS-ZFVPRC NO-GAP,
                     IT_INS-ZFVPR CURRENCY IT_INS-ZFVPRC NO-GAP,
                 140 SY-VLINE NO-GAP.
            HIDE: IT_INS.
            FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
            WRITE: / SY-VLINE NO-GAP, IT_MS-ZFMSNM NO-GAP,
                  37 SY-VLINE NO-GAP, IT_INS-ZFINNO NO-GAP,
                     SY-VLINE NO-GAP, IT_INS-ZFISDT NO-GAP,
                     SY-VLINE NO-GAP, IT_RN-TXZ01(30) NO-GAP,
                     SY-VLINE NO-GAP, IT_INS-WAERS NO-GAP,
                     IT_INS-ZFIVAMT CURRENCY IT_INS-WAERS NO-GAP,
                     SY-VLINE NO-GAP.
            HIDE: IT_INS.
            WRITE: SY-ULINE.
            W_TABIX = W_TABIX + 1.
         ENDIF.
*      ENDLOOP.
   ENDLOOP.
   CLEAR: IT_INS.
   FORMAT RESET.
   WRITE: /125 '총:' NO-GAP, W_TABIX NO-GAP, '건'.
ENDFORM.                    " P3000_WRITE_RESULT_DATA
