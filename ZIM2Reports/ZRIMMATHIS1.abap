*&---------------------------------------------------------------------*
*& Report ZRIMMATHIS                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입자재별 현황관리 PROGRAM                           *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2000.12.21                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1. 나신호의 열번째 레포트 프로그램 씨익~ .
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT ZRIMMATHIS NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                     LINE-SIZE 293.

tables: EKKO,                " ABAP Standard Header Table..
        EKPO,                " ABAP Standard Item Table..
        ZTREQHD,             " 수입의뢰 Header Table..
        ZTREQIT,             " 수입의뢰 Item Table..
        ZTBL,                " B/L Table..
        ZTBLIT,              " B/L  Item Table..
        LFA1,                " 거래처 Master Table..
        ZTMSHD,              " 모선관리 Header Table..
        ZTCGHD,              " 하역 Header Table..
        ZTCGIT,              " 하역 자재 Table..
        ZTREQORJ,            " 수입의뢰 원산지 내역 Table..
        ZTIMIMG03,           " 보세구역 코드 Table..
        T005T.               " 국가이름 Table..

*------------------------------------------*
* P/O 번호 조회를 위한 INTERNAL TABLE 선언 *
*------------------------------------------*

DATA: BEGIN OF IT_PO OCCURS 1000,
        EBELN    LIKE   EKKO-EBELN,      " P/O Header No..
        LIFNR    LIKE   EKKO-LIFNR,      " Vendor's Account No..
        AEDAT    LIKE   EKKO-AEDAT,      " 레코드생성일..
        WAERS    LIKE   EKKO-WAERS,      " 통화키..
        EBELP    LIKE   EKPO-EBELP,      " P/O Item No..
        MATNR    LIKE   EKPO-MATNR,      " 자재번호..
        BUKRS    LIKE   EKPO-BUKRS,      " 회사코드..
        WERKS    LIKE   EKPO-WERKS,      " 플랜트..
        TXZ01    LIKE   EKPO-TXZ01,      " 내역..
        MENGE    LIKE   EKPO-MENGE,      " 구매오더수량..
        MEINS    LIKE   EKPO-MEINS,      " 오더단위..
        NETPR    LIKE   EKPO-NETPR,      " 구매문서의 단가 (문서통화).
        PEINH    LIKE   EKPO-PEINH,      " 가격단위..
        NAME1    LIKE   LFA1-NAME1,      " 이름1..
      END OF IT_PO.

*-----------------------------------------------*
* 수입의뢰 번호 조회를 위한 INTERNAL TABLE 선언 *
*-----------------------------------------------*

DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        EBELP     LIKE   ZTREQIT-EBELP,    " P/O Item Number.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFOPNNO   LIKE   ZTREQHD-ZFOPNNO,  " 신용장-승인번호.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " 수입문서 품목번호.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        MENGE     LIKE   ZTREQIT-MENGE,    " 수입의뢰수량.
        MEINS     LIKE   ZTREQIT-MEINS,    " Base Unit of Measure.
        NETPR     LIKE   ZTREQIT-NETPR,    " Net Price.
        PEINH     LIKE   ZTREQIT-PEINH,    " Price Unit.
        BPRME     LIKE   ZTREQIT-BPRME,    " Order Price Unit.
        ZFORIG    LIKE   ZTREQORJ-ZFORIG,  " 자재원산국..
        LANDX     LIKE   T005T-LANDX,      " 국가이름..
        END OF IT_RN.

*-----------------------------------------------------------------------
* B/L 번호 조회를 위한 Internal Table Declaration.
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_BL OCCURS 1000,
        ZFBLNO    LIKE   ZTBL-ZFBLNO,      " B/L 관리 번호..
        ZFMSNO    LIKE   ZTBL-ZFMSNO,      " 모선관리번호..
        ZFFORD    LIKE   ZTBL-ZFFORD,      " Forwarder..
        ZFAPRTC   LIKE   ZTBL-ZFAPRTC,     " 도착항 코드..
        ZFAPRT    LIKE   ZTBL-ZFAPRT,      " 도착항..
        ZFHBLNO   LIKE   ZTBL-ZFHBLNO,     " House B/L No..
        ZFREBELN  LIKE   ZTBL-ZFREBELN,    " 대표 P/O No..
        LIFNR     LIKE   ZTBL-LIFNR,       " Account No..
        ZFOPNNO   LIKE   ZTBL-ZFOPNNO,     " 신용장-승인번호.
        ZFETA     LIKE   ZTBL-ZFETA,       " 도착일(ETD)..
        ZFBLIT    LIKE   ZTBLIT-ZFBLIT,    " B/L 품목번호..
        EBELN     LIKE   ZTBLIT-EBELN,     " 구매문서번호..
        EBELP     LIKE   ZTBLIT-EBELP,     " 구매문서 품목번호..
        ZFREQNO   LIKE   ZTBLIT-ZFREQNO,   " 수입의뢰 관리번호..
        ZFITMNO   LIKE   ZTBLIT-ZFITMNO,   " 수입문서 품목번호..
        MATNR     LIKE   ZTBLIT-MATNR,     " 자재번호..
        BLMENGE   LIKE   ZTBLIT-BLMENGE,   " B/L 수량..
        MEINS     LIKE   ZTBLIT-MEINS,     " 기본단위..
      END OF IT_BL.

*-----------------------------------------------------------------------
* 모선관리를 위한 Internal Table 선언..
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_MS OCCURS 1000,
        ZFMSNO    LIKE   ZTMSHD-ZFMSNO,    " 모선관리번호..
        ZFMSNM    LIKE   ZTMSHD-ZFMSNM,    " 모선명..
        ZFREQNO   LIKE   ZTMSIT-ZFREQNO,   " 수입의뢰관리번호..
        ZFSHSDF   LIKE   ZTMSHD-ZFSHSDF,   " 선적일(From)..
        ZFSHSDT   LIKE   ZTMSHD-ZFSHSDT,   " 선적일(To)..
      END OF IT_MS.

*-----------------------------------------------------------------------
* 하역항 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_CG OCCURS 1000,
        ZFCGNO    LIKE   ZTCGHD-ZFCGNO,    " 하역관리번호..
        ZFMSNO    LIKE   ZTCGHD-ZFMSNO,    " 모선관리번호..
        ZFETA     LIKE   ZTCGHD-ZFETA,     " 도착일(ETA)..
        ZFCGPT    LIKE   ZTCGHD-ZFCGPT,    " 하역항..
        ZFCGIT    LIKE   ZTCGIT-ZFCGIT,    " 하역자재순번..
        EBELN     LIKE   ZTCGIT-EBELN,     " 구매문서번호..
        EBELP     LIKE   ZTCGIT-EBELP,     " 구매문서 품목번호..
        ZFREQNO   LIKE   ZTCGIT-ZFREQNO,   " 수입의뢰 관리번호..
        ZFITMNO   LIKE   ZTCGIT-ZFITMNO,   " 수입문서 품목번호..
        ZFBLNO    LIKE   ZTCGIT-ZFBLNO,    " B/L 관리번호..
        ZFBLIT    LIKE   ZTCGIT-ZFBLIT,    " B/L 품목번호..
        MATNR     LIKE   ZTCGIT-MATNR,     " 자재번호..
        CGMENGE   LIKE   ZTCGIT-CGMENGE,   " 하역자재수량..
        MEINS     LIKE   ZTCGIT-MEINS,     " 기본단위..
        ZFBNARCD  LIKE   ZTCGIT-ZFBNARCD,  " 보세구역 내부관리코드..
      END OF IT_CG.

*-----------------------------------------------------------------------
* 거래처마스터 테이블 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_LFA OCCURS 1000,
         LIFNR    LIKE   LFA1-LIFNR,
         NAME1    LIKE   LFA1-NAME1,
      END OF IT_LFA.

*-----------------------------------------------------------------------
* 보세구역코드 테이블 조회를 위한 Internal Table 선언...
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_IMG03 OCCURS 1000.
      INCLUDE STRUCTURE ZTIMIMG03.
DATA  END OF IT_IMG03.

*-----------------------------------------------------------------------
* 테이블의 사용에 관련된 변수 선언..
*-----------------------------------------------------------------------
DATA: W_TABIX     LIKE SY-TABIX,
      W_BEWTP     LIKE EKBE-BEWTP,
      W_ERR_CHK   TYPE C,
      W_PAGE      TYPE I.
*-----------------------------------------------------------------------
* ON CHANGE OF 기능을 대신하기 위한 변수 선언..
*-----------------------------------------------------------------------
DATA: TEMP_MATNR  LIKE EKPO-MATNR,         " 자재번호를 임시로 저장..
      TEMP_TXZ01  LIKE EKPO-TXZ01,         " 자재내역을 임시로 저장..
      TEMP_EBELN  LIKE EKPO-EBELN,         " P/O 번호를 임시로 저장..
      TEMP_EBELP  LIKE EKPO-EBELP,         " Item 번호를 임시로 저장..
      TEMP_REQNO  LIKE ZTREQHD-ZFREQNO,    " 수입의뢰관리번호 저장..
      TEMP_ITMNO  LIKE ZTREQIT-ZFITMNO,    " Item 번호를 임시로 저장..
      TEMP_BLNO   LIKE ZTBL-ZFBLNO,        " B/L 번호를 임시로 저장..
      TEMP_BLIT   LIKE ZTBLIT-ZFBLIT,      " B/L Item 번호 임시저장..
      TEMP_CGNO   LIKE ZTCGHD-ZFCGNO,      " 하역관리번호를 임시저장..
      TEMP_CGIT   LIKE ZTCGIT-ZFCGIT.      " 하역자재순번을 임시저장..

*-----------------------------------------------------------------------
* HIDE VARIABLE.
*-----------------------------------------------------------------------

DATA: BEGIN OF DOCU,
        TYPE(2)   TYPE C,
        CODE      LIKE EKKO-EBELN,
        ITMNO     LIKE EKPO-EBELP,
        YEAR      LIKE BKPF-GJAHR,
      END OF DOCU.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_MATNR   FOR EKPO-MATNR,       " 자재 No.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 No.
               S_OPNNO   FOR ZTREQHD-ZFOPNNO,  " 신용장-승인번호.
               S_LIFNR   FOR ZTREQHD-LIFNR.    " Vendor.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 수입의뢰 No.
   PERFORM P1000_READ_RN_DATA USING W_ERR_CHK.
   CHECK W_ERR_CHK NE 'Y'.

* P/O Table Select..
   PERFORM P1000_READ_PO_DATA.

* 보세구역코드 Table Select..
   PERFORM P1000_READ_IMG03_DATA.

* 구매처 Table Select..
   PERFORM P1000_READ_LFA_DATA.

* 모선 Table Select..
   PERFORM P1000_READ_MS_DATA.

* B/L Table Select..
   PERFORM P1000_READ_BL_DATA.

* 하역 Table Select..
   PERFORM P1000_READ_CG_DATA.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
   CHECK W_ERR_CHK NE 'Y'.
* Title Text Write.
   SET TITLEBAR 'TIT1'.
   SET PF-STATUS 'ZIMH1'.

   PERFORM P3000_TITLE_WRITE.             "헤더 출력...

* Sort P/O, Request No. Internal Table.
   PERFORM P2000_SORT_IT_DATA.

* List Write...
   PERFORM P3000_WRITE_PO_DATA.

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT LINE-SELECTION.

DATA : L_TEXT_AEDAT(20),
       L_TEXT_PO(20),
       L_TEXT_RN(20),
       L_TEXT_BL(24).
DATA : MM03_START_SICHT(15) TYPE C  VALUE 'BDEKLPQSVXZA'.

   GET CURSOR FIELD MM03_START_SICHT.
   CASE MM03_START_SICHT.   " 필드명..

      WHEN 'IT_PO-MATNR' OR 'IT_PO-TXZ01'.
         SET PARAMETER ID 'MAT' FIELD IT_PO-MATNR.
         SET PARAMETER ID 'BUK' FIELD IT_PO-BUKRS.
         SET PARAMETER ID 'WRK' FIELD IT_PO-WERKS.
         SET PARAMETER ID 'LAG' FIELD ''.
         SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
         CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.

   ENDCASE.

   GET CURSOR FIELD L_TEXT_AEDAT.
   CASE L_TEXT_AEDAT.   " 필드명..

      WHEN 'L_DATE'.
         SET PARAMETER ID 'BES'  FIELD IT_PO-EBELN.
         CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
   ENDCASE.

   GET CURSOR FIELD L_TEXT_PO.
   CASE L_TEXT_PO.   " 필드명..

      WHEN 'IT_PO-EBELN' OR 'IT_PO-EBELP'.
         SET PARAMETER ID 'BES'  FIELD IT_PO-EBELN.
         CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
   ENDCASE.
   CLEAR: IT_PO.

   GET CURSOR FIELD L_TEXT_RN.
   CASE L_TEXT_RN.   " 필드명..

      WHEN 'IT_RN-ZFOPNNO'.
         SET PARAMETER ID 'ZPREQNO' FIELD IT_RN-ZFREQNO.
         CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

   ENDCASE.
   CLEAR: IT_RN.

   GET CURSOR FIELD L_TEXT_BL.
   CASE L_TEXT_BL.   " 필드명..

      WHEN 'IT_BL-ZFHBLNO'.
         SET PARAMETER ID 'ZPBLNO' FIELD IT_BL-ZFBLNO.
         CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.

   ENDCASE.
   CLEAR: IT_BL.

*   GET CURSOR FIELD L_TEXT_CG.
*   CASE L_TEXT_CG.   " 필드명..
*
*      WHEN '' OR ''
*        OR '' OR ''
*        OR '' OR ''.
*         SET PARAMETER ID 'ZPBLNO' FIELD IT_BL-ZFBLNO.
*         CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
*
*   ENDCASE.
*   CLEAR: IT_CG.


*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
*AT USER-COMMAND.
*   CASE SY-UCOMM.
*      WHEN 'DISP'.
*      CASE DOCU-TYPE.
*         WHEN 'VD'.     " Vendor
*            MESSAGE I977 WITH DOCU-CODE.
*
*            SET PARAMETER ID 'KDY' FIELD '/110/120/130'.
*            SET PARAMETER ID 'LIF' FIELD DOCU-CODE.
*            SET PARAMETER ID 'EKO' FIELD ''.
*            CALL TRANSACTION 'MK03' AND SKIP FIRST SCREEN.
*         WHEN 'PO'.     " P/O
*            SET PARAMETER ID 'BES' FIELD DOCU-CODE.
*            SET PARAMETER ID 'BSP' FIELD DOCU-ITMNO.
*            CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.
*
*         WHEN 'RN'.     " L/C
*            SET PARAMETER ID 'ZPREQNO' FIELD DOCU-CODE.
*            SET PARAMETER ID 'ZPAMDNO' FIELD DOCU-ITMNO.
*            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
*            SET PARAMETER ID 'BES'     FIELD ''.
*            IF DOCU-ITMNO IS INITIAL.
*            CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
*            ELSE.
*            CALL TRANSACTION 'ZIM13' AND SKIP FIRST SCREEN.
*            ENDIF.
*
*         WHEN 'BL'.     "  Bill of Lading.
*            SET PARAMETER ID 'ZPHBLNO' FIELD ''.
*            SET PARAMETER ID 'ZPBLNO' FIELD DOCU-CODE.
*            CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
*
*         WHEN 'IV'.     "  Invoice
*            SET PARAMETER ID 'ZPCIVNO' FIELD ''.
*            SET PARAMETER ID 'ZPIVNO' FIELD DOCU-CODE.
*            CALL TRANSACTION 'ZIM58' AND SKIP FIRST SCREEN.
*
*   WHEN 'MD'.     " 자재문서.
**>>> 연도.
*      SET PARAMETER ID 'MJA'     FIELD DOCU-YEAR.
**>>> 문서번호.
*      SET PARAMETER ID 'MBN'     FIELD DOCU-CODE.
*      SET PARAMETER ID 'POS'     FIELD ''.
*      CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
*
*      ENDCASE.
*   ENDCASE.
*
*   CLEAR DOCU.
*
*&------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&------------------------------------------------------------------*
FORM P1000_READ_RN_DATA USING W_ERR_CHK.

   W_ERR_CHK = 'N'.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_RN
     FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
     ON     H~ZFREQNO EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN  S_REQNO
     AND    H~ZFOPNNO  IN  S_OPNNO
     AND    H~EBELN    IN  S_EBELN
     AND    I~MATNR    IN  S_MATNR
     AND    H~LIFNR    IN  S_LIFNR.

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
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

   SELECT *
     INTO   CORRESPONDING FIELDS OF TABLE IT_PO
     FROM   EKKO AS H INNER JOIN EKPO AS I
     ON     H~EBELN EQ I~EBELN
     FOR ALL ENTRIES IN IT_RN
     WHERE  H~EBELN EQ IT_RN-EBELN
     AND    I~MATNR EQ IT_RN-MATNR
     AND    H~LIFNR EQ IT_RN-LIFNR.

   IF SY-SUBRC NE 0.
   WRITE 'NOT A VALID INPUT.'.
   EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA

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

   SELECT *
     APPENDING  CORRESPONDING FIELDS OF TABLE IT_LFA
     FROM   LFA1
     FOR ALL ENTRIES IN IT_IMG03
     WHERE  LIFNR EQ IT_IMG03-LIFNR.

ENDFORM.                    " P1000_READ_LFA_DATA

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
*&      Form  P1000_READ_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_BL_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_BL
     FROM   ZTBL AS H INNER JOIN ZTBLIT AS I
     ON     H~ZFBLNO  EQ I~ZFBLNO
     FOR ALL ENTRIES IN IT_RN
     WHERE  I~EBELN   EQ IT_RN-EBELN
     AND    I~MATNR   EQ IT_RN-MATNR
     AND    I~ZFREQNO EQ IT_RN-ZFREQNO
     AND    H~ZFOPNNO EQ IT_RN-ZFOPNNO.

ENDFORM.                    " P1000_READ_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_CG_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_CG
     FROM   ZTCGHD AS H INNER JOIN ZTCGIT AS I
     ON     H~ZFCGNO EQ I~ZFCGNO
     FOR ALL ENTRIES   IN  IT_BL
     WHERE  I~EBELN    EQ  IT_BL-EBELN
     AND    I~EBELP    EQ  IT_BL-EBELP
     AND    I~ZFREQNO  EQ  IT_BL-ZFREQNO
     AND    I~ZFITMNO  EQ  IT_BL-ZFITMNO
     AND    I~ZFBLNO   EQ  IT_BL-ZFBLNO
     AND    I~ZFBLIT   EQ  IT_BL-ZFBLIT
     AND    I~MATNR    EQ  IT_BL-MATNR.

ENDFORM.                    " P1000_READ_CG_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IMG03_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IMG03_DATA.

   SELECT *
     INTO   CORRESPONDING FIELDS OF TABLE IT_IMG03
     FROM   ZTIMIMG03
     FOR ALL ENTRIES IN IT_CG
     WHERE  ZFBNARCD EQ IT_CG-ZFBNARCD.

ENDFORM.                    " P1000_READ_IMG03_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
   SKIP 2.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE: /55 '[자재별 진행현황]'
              COLOR COL_HEADING INTENSIFIED OFF.

   WRITE: / 'Date: ' COLOR COL_NORMAL INTENSIFIED ON,
             SY-DATUM COLOR COL_NORMAL INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_IT_DATA
*&---------------------------------------------------------------------*
*       SORTING INTERNAL TABLE..
*----------------------------------------------------------------------*
FORM P2000_SORT_IT_DATA.

   SORT IT_PO BY MATNR EBELN EBELP.
   SORT IT_RN BY MATNR EBELN ZFITMNO ZFREQNO.

ENDFORM.                    " P2000_SORT_IT_DATA

*----------------------------------------------------------------------*
* 결과화면 조회를 위한 PERFORM 문..
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  P3000_WEITE_PO_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_PO_DATA.
DATA : L_FIRST_LINE   VALUE  'Y',
       L_DATE(10).

*>>> 임시변수의 값들을 초기화..
   CLEAR: TEMP_MATNR,
          TEMP_EBELN,
          TEMP_EBELP,
          TEMP_TXZ01.
   SKIP.
   LOOP AT IT_PO.
      IF TEMP_TXZ01 NE IT_PO-TXZ01.
         FORMAT COLOR COL_HEADING INTENSIFIED ON.
         ULINE.
         WRITE: / IT_PO-MATNR NO-GAP, IT_PO-TXZ01 NO-GAP.
         HIDE: IT_PO.
         FORMAT RESET.
      ENDIF.

      IF IT_PO-EBELN NE TEMP_EBELN.
         READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         CONCATENATE  IT_PO-AEDAT(4)  IT_PO-AEDAT+4(2)
                       IT_PO-AEDAT+6(2)
                       INTO L_DATE
                       SEPARATED BY '/'.
         WRITE: / L_DATE.
         WRITE: 12 IT_PO-EBELN NO-GAP.

         READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         WRITE: 23 IT_PO-EBELP NO-GAP.
         WRITE: 29 IT_PO-NETPR CURRENCY IT_PO-WAERS NO-GAP,
                43 IT_PO-WAERS NO-GAP,
                49 IT_PO-PEINH NO-GAP, 55 IT_LFA-NAME1(20) NO-GAP.
         HIDE: IT_PO.

      ELSEIF IT_PO-EBELP NE TEMP_EBELP.
         READ TABLE IT_LFA WITH KEY LIFNR = IT_PO-LIFNR.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         WRITE: /23 IT_PO-EBELP NO-GAP.
         WRITE:  29 IT_PO-NETPR CURRENCY IT_PO-WAERS NO-GAP,
                 43 IT_PO-WAERS NO-GAP,
                 49 IT_PO-PEINH NO-GAP, 55 IT_LFA-NAME1(20) NO-GAP.
         HIDE: IT_PO.
      ENDIF.
      PERFORM P3000_WRITE_RN_DATA.
      TEMP_MATNR = IT_PO-MATNR.
      TEMP_TXZ01 = IT_PO-TXZ01.
      TEMP_EBELN = IT_PO-EBELN.
      TEMP_EBELP = IT_PO-EBELP.
      CLEAR: IT_PO.
   ENDLOOP.

ENDFORM.                    " P3000_WEITE_PO_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_RN_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_RN_DATA.
DATA : L_FIRST_LINE   VALUE  'Y',
       L_DATE_F(10),
       L_DATE_T(10).

*>>> 임시변수의 값들을 초기화..
CLEAR: TEMP_REQNO,
       TEMP_ITMNO.

   LOOP AT IT_RN
      WHERE EBELN = IT_PO-EBELN
        AND EBELP = IT_PO-EBELP.
      IF TEMP_REQNO NE IT_RN-ZFREQNO.
         IF L_FIRST_LINE = 'Y'.
            WRITE: 76 IT_RN-ZFOPNNO(20) NO-GAP.
            HIDE: IT_RN.
         ELSE.
            WRITE:/76 IT_RN-ZFOPNNO(20) NO-GAP.
            HIDE: IT_RN.
         ENDIF.
         L_FIRST_LINE = 'N'.

         WRITE: 97 IT_RN-ZFITMNO NO-GAP,
                104 IT_RN-LANDX NO-GAP,
                120 IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
                138 IT_RN-MEINS NO-GAP.
         HIDE: IT_RN.
      ELSEIF TEMP_ITMNO NE IT_RN-ZFITMNO.
         WRITE: /97 IT_RN-ZFITMNO NO-GAP,
                104 IT_RN-LANDX NO-GAP,
                120 IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
                138 IT_RN-MEINS NO-GAP.
         HIDE: IT_RN.
      ENDIF.

      LOOP AT IT_MS
            WHERE ZFREQNO = IT_RN-ZFREQNO.
            CONCATENATE IT_MS-ZFSHSDF(4)
                         IT_MS-ZFSHSDF+4(2)
                         IT_MS-ZFSHSDF+6(2)
                         INTO L_DATE_F
                         SEPARATED BY '/'.

            CONCATENATE IT_MS-ZFSHSDT(4)
                         IT_MS-ZFSHSDT+4(2)
                         IT_MS-ZFSHSDT+6(2)
                         INTO L_DATE_T
                         SEPARATED BY '/'.

            WRITE: 142 IT_MS-ZFMSNM(18) NO-GAP,
                   161 L_DATE_F NO-GAP,
                   172 L_DATE_T NO-GAP.

            PERFORM P3000_WRITE_BL_DATA.
         CLEAR: IT_RN.
      ENDLOOP.
      IF SY-SUBRC NE 0.
         PERFORM P3000_WRITE_BL_DATA.
      ENDIF.

      TEMP_REQNO = IT_RN-ZFREQNO.
      TEMP_ITMNO = IT_RN-ZFITMNO.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_RN_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_BL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_BL_DATA.
DATA : L_FIRST_LINE   VALUE  'Y'.

*>>> 임시변수의 값들을 초기화..
CLEAR : TEMP_BLNO,
        TEMP_BLIT.

   LOOP AT IT_BL
      WHERE ZFREQNO = IT_RN-ZFREQNO
        AND ZFITMNO = IT_RN-ZFITMNO.
      IF TEMP_BLNO NE IT_BL-ZFBLNO.
         WRITE: 183 IT_BL-ZFHBLNO NO-GAP,
                208 IT_BL-ZFBLIT NO-GAP, 214 IT_BL-ZFFORD NO-GAP,
                225 IT_BL-BLMENGE UNIT IT_BL-MEINS NO-GAP,
                239 IT_BL-MEINS NO-GAP, 243 IT_BL-ZFAPRTC NO-GAP.
         HIDE: IT_BL.
      ELSEIF TEMP_BLIT NE IT_BL-ZFBLIT.
         WRITE: /208 IT_BL-ZFBLIT NO-GAP, 214 IT_BL-ZFFORD NO-GAP,
                 225 IT_BL-BLMENGE UNIT IT_BL-MEINS NO-GAP,
                 239 IT_BL-MEINS NO-GAP, 243 IT_BL-ZFAPRTC NO-GAP.
         HIDE: IT_BL.
      ENDIF.

      PERFORM P3000_WRITE_CG_DATA.
      CLEAR: IT_BL.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_BL_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_WRITE_CG_DATA.

CLEAR : TEMP_CGNO,
        TEMP_CGIT.

   LOOP AT IT_CG
      WHERE ZFBLNO = IT_BL-ZFBLNO
        AND ZFBLIT = IT_BL-ZFBLIT.
      READ TABLE IT_IMG03 WITH KEY ZFBNARCD = IT_CG-ZFBNARCD.
      IF SY-SUBRC EQ 0.
         READ TABLE IT_LFA   WITH KEY LIFNR    = IT_IMG03-LIFNR.
         IF SY-SUBRC NE 0.
            CLEAR : IT_LFA.
         ENDIF.
      ELSE.
         CLEAR : IT_LFA.
      ENDIF.

      IF TEMP_CGNO NE IT_CG-ZFCGNO.
         WRITE 247 IT_CG-ZFCGPT NO-GAP.
         WRITE: 251 IT_LFA-NAME1(20) NO-GAP,
                272 IT_CG-CGMENGE UNIT IT_CG-MEINS NO-GAP,
                289 IT_CG-MEINS NO-GAP.
         HIDE: IT_CG.

      ELSEIF TEMP_CGIT NE IT_CG-ZFCGIT.
         WRITE: /251 IT_LFA-NAME1(20) NO-GAP,
                 272 IT_CG-CGMENGE UNIT IT_CG-MEINS NO-GAP,
                 289 IT_CG-MEINS NO-GAP.
         HIDE: IT_CG.
         TEMP_CGNO = IT_CG-ZFCGNO.
         TEMP_CGIT = IT_CG-ZFCGIT.
      ENDIF.

   ENDLOOP.


ENDFORM.                    " P3000_WRITE_CG_DATA
