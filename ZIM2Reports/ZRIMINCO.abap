*&---------------------------------------------------------------------*
*& Report  ZRIMINCO                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 자재별 거래 개설 현황                                 *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2002.02.15                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1.
*&
*&---------------------------------------------------------------------*
*& 변경내역 :
*&---------------------------------------------------------------------*

REPORT  ZRIMINCO  NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                  LINE-SIZE 173.

TABLES: EKKO,                " ABAP Standard Header Table..
        EKPO,                " ABAP Standard Item Table..
        T001W,               " PLANT TEXT
        EKET,                " 납품일정계약의 일정라인..
        ZTREQHD,             " 수입의뢰 Header Table..
        LFA1,                " 거래처 Master Table..
        ZTREQORJ,            " 수입의뢰 원산지 Table..
        ZTREQST,             " 수입의뢰 상태 Table..
        ZTREQIT,             " 수입의뢰 Item Table..
        ZTMLCHD,             " Master L/C Header..
        ZTMSIT.
*------------------------------------------*
* P/O 번호 조회를 위한 INTERNAL TABLE 선언 *
*------------------------------------------*
DATA: BEGIN OF IT_PO OCCURS 1000,          " Internal Table IT_PO..
        EBELN    LIKE   EKKO-EBELN,        " P/O Header No..
        BSART    LIKE   EKKO-BSART,        " Purchasing Document Type..
        LOEKZ    LIKE   EKKO-LOEKZ,        " Deletion indicator..
        LIFNR    LIKE   EKKO-LIFNR,        " Vendor's Account No..
        EKORG    LIKE   EKKO-EKORG,        " Purchasing Organization..
        EKGRP    LIKE   EKKO-EKGRP,        " Purchasing Group..
        WAERS    LIKE   EKKO-WAERS,        " Current Key..
        EBELP    LIKE   EKPO-EBELP,        " P/O Item No..
        TXZ01    LIKE   EKPO-TXZ01,        " Short Text..
        MATNR    LIKE   EKPO-MATNR,        " Material No..
        MENGE    LIKE   EKPO-MENGE,        " Purchase Order Quantity..
        MEINS    LIKE   EKPO-MEINS,        " Order Unit..
        ELIKZ    LIKE   EKPO-ELIKZ,        " Delivery Comience Indicator

      END OF IT_PO.

*-----------------------------------------------*
* 수입의뢰 번호 조회를 위한 INTERNAL TABLE 선언 *
*-----------------------------------------------*
DATA: BEGIN OF IT_TAB OCCURS 1000,          " REQNO조회위한IT선언.
        EBELN     LIKE   ZTREQIT-EBELN,
        EBELP     LIKE   ZTREQIT-EBELP,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " ITEM NO.
        ZFWERKS   LIKE   ZTREQHD-ZFWERKS,  " 플랜트.
        NAME1(20) TYPE   C,
        MATNR     LIKE   ZTREQIT-MATNR,
        TXZ01     LIKE   ZTREQIT-TXZ01,
        ZFTRANS   LIKE   ZTREQHD-ZFTRANS,
        TRANS_NM(9) TYPE C,
        INCO1     LIKE   ZTREQHD-INCO1,
        MENGE     LIKE   ZTREQIT-MENGE,
        MEINS     LIKE   ZTREQIT-MEINS,
        ZFAMT     LIKE   ZTREQHD-ZFOPAMT,
        NETPR     LIKE   ZTREQIT-NETPR,
        WAERS     LIKE   ZTREQHD-WAERS,
      END OF IT_TAB.

*-----------------------------------------------------------------------
* Error Check 과 Table Index 저장을 위한 변수 선언.
*-----------------------------------------------------------------------
DATA: W_ERR_CHK   TYPE  C,
      W_TABIX     TYPE  I.

DATA: W_COUNT     TYPE  I,
      W_PAGE      TYPE  I,
      W_LINE      TYPE  I,
      W_MOD       TYPE  I,
      W_FIELD_NM  TYPE  DD03D-FIELDNAME.

*>>> Sort를 위한 변수 및 FORM Include.
INCLUDE ZRIMSORTCOM.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_MATGB   FOR ZTREQHD-ZFMATGB,  " 자재구분.
               S_WERKS   FOR ZTREQHD-ZFWERKS,  " 플랜트,
               S_MATNR   FOR EKPO-MATNR,       " 자재 No.
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 No.
               S_OPNNO   FOR ZTREQHD-ZFOPNNO,  " L/C No.
               S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
               S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type.
               S_OPNDT   FOR ZTREQST-ZFOPNDT,  " 개설일자.
               S_CDT     FOR ZTREQST-CDAT,     " 생성일자.
               S_TERM    FOR ZTREQHD-ZTERM,    " 지급조건.
               S_INCO    FOR ZTREQHD-INCO1.    " 인도조건.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'ZIMR46'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TERM-LOW.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_TERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_TERM-HIGH.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_TERM-HIGH.

*-----------------------------------------------------------------------
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
   PERFORM P3000_TITLE_WRITE.             "헤더 출력...

*-----------------------------------------------------------------------
* Start-Of-Selection
*-----------------------------------------------------------------------
START-OF-SELECTION.

   PERFORM P1000_READ_DATA.

   IF W_ERR_CHK EQ 'Y'.
      EXIT.
   ENDIF.

*-----------------------------------------------------------------------
* End-Of-Selection
*-----------------------------------------------------------------------
END-OF-SELECTION.

* Title Text Write.
   SET TITLEBAR 'ZIMR46'.
   SET PF-STATUS 'ZIMR46'.

* List Write...
   PERFORM P3000_WRITE_LC_DATA.           "결과 출력...

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT LINE-SELECTION.

  SET PARAMETER ID 'BES'     FIELD ''.
  SET PARAMETER ID 'ZFOPNNO' FIELD ''.
  SET PARAMETER ID 'ZPREQNO' FIELD IT_TAB-ZFREQNO.
  CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

AT USER-COMMAND.
  CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.                  " SORT 선택
         W_FIELD_NM = 'MATNR'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
         PERFORM RESET_LIST.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

   SKIP 2.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE: /70 '[ 품목별 실적 현황 ]'
              COLOR COL_HEADING INTENSIFIED OFF.

   FORMAT RESET.
   WRITE: / 'Date: ', SY-DATUM.
   ULINE.

*>>> Field Name을 Title에 기술해준다.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE: /   SY-VLINE NO-GAP,
         (16) '수입의뢰 번호     ' NO-GAP,  SY-VLINE NO-GAP,
         (25) '플랜트'             NO-GAP,  SY-VLINE NO-GAP,
         (45) '자재              ' NO-GAP,  SY-VLINE NO-GAP,
         (9)  '운송수단'           NO-GAP,  SY-VLINE NO-GAP,
         (9)  'Incoterms'          NO-GAP,  SY-VLINE NO-GAP,
         (15) '       개설수량'    NO-GAP,  SY-VLINE NO-GAP,
         (5)  '단위'               NO-GAP,  SY-VLINE NO-GAP,
         (15) '           단가'    NO-GAP,  SY-VLINE NO-GAP,
         (24) '           금액'    NO-GAP,  SY-VLINE NO-GAP.

   FORMAT RESET.
   ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA.

   PERFORM P1000_READ_RN_DATA.

   IF W_ERR_CHK EQ 'Y'.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_LC_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_LC_DATA.

   SORT  IT_TAB  BY  ZFREQNO ZFITMNO.
   PERFORM P2000_WRITE_DATA.

ENDFORM.                    " P3000_WEITE_LC_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_RN_DATA.

   DATA: L_LINE_COUNT TYPE I.

   W_ERR_CHK = 'N'.

   SELECT   I~EBELN    I~EBELP  I~ZFREQNO  I~ZFITMNO
            H~ZFTRANS  H~INCO1  H~ZFWERKS  I~MENGE   I~NETPR
            I~MEINS    I~MATNR  I~TXZ01    I~MEINS   H~WAERS
     INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
     FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
     ON     H~ZFREQNO EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN  S_REQNO
     AND    H~EBELN    IN  S_EBELN
     AND    I~MATNR    IN  S_MATNR
     AND    H~ZFREQTY  IN  S_REQTY
     AND    H~ZFOPNNO  IN  S_OPNNO
     AND    H~ZTERM    IN  S_TERM
     AND    H~INCO1    IN  S_INCO
     AND    H~ZFWERKS  IN  S_WERKS
     AND    H~ZFMATGB  IN  S_MATGB.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

   LOOP AT IT_TAB.

     W_TABIX = SY-TABIX.

* PO ITEM 내역 SELECT!
     SELECT SINGLE * FROM EKPO WHERE EBELN EQ IT_TAB-EBELN
                               AND   EBELP EQ IT_TAB-EBELP.

     SELECT SINGLE * FROM ZTREQST
            WHERE  ZFREQNO  EQ  IT_TAB-ZFREQNO
            AND    EKGRP    IN  S_EKGRP
            AND    CDAT     IN  S_CDT
            AND    ZFOPNDT  IN  S_OPNDT
            AND    ZFAMDNO  EQ  ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                                  WHERE  ZFREQNO  EQ IT_TAB-ZFREQNO ).

     IF SY-SUBRC NE 0.
        DELETE  IT_TAB  INDEX  W_TABIX.
        CONTINUE.
     ENDIF.

     " 플랜트명 SET!
     CLEAR : T001W.
     SELECT SINGLE NAME1 INTO IT_TAB-NAME1
                         FROM T001W WHERE WERKS  EQ  IT_TAB-ZFWERKS.

     " 자재구분 SET!
     IF IT_TAB-ZFTRANS EQ 'A'.
        MOVE : 'AIR'  TO  IT_TAB-TRANS_NM.
     ELSEIF IT_TAB-ZFTRANS EQ 'O'.
        MOVE : 'OCEAN' TO  IT_TAB-TRANS_NM.
     ELSE.
        MOVE : 'AIR/OCEAN' TO IT_TAB-TRANS_NM.
     ENDIF.

     " ITEM 별 금액 COMPUTE.
     IF EKPO-BPUMN IS INITIAL.
        EKPO-BPUMN = 1.
     ENDIF.
     IF EKPO-PEINH IS INITIAL.
        EKPO-PEINH = 1.
     ENDIF.
     IT_TAB-ZFAMT = ( IT_TAB-MENGE * ( EKPO-BPUMZ / EKPO-BPUMN )
                  * ( IT_TAB-NETPR / EKPO-PEINH ) ).

     MODIFY  IT_TAB  INDEX  W_TABIX.

   ENDLOOP.

   DESCRIBE TABLE IT_TAB  LINES L_LINE_COUNT.
   IF L_LINE_COUNT = 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_DATA.

   LOOP AT IT_TAB.
      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P2000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.

   IF W_MOD EQ 1.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
   ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   ENDIF.

   WRITE:/ SY-VLINE NO-GAP,
          (10) IT_TAB-ZFREQNO         NO-GAP,
          '-'                         NO-GAP,
          (5)  IT_TAB-ZFITMNO         NO-GAP,  SY-VLINE NO-GAP,
          (4)  IT_TAB-ZFWERKS         NO-GAP,
          ' '                         NO-GAP,
          (20) IT_TAB-NAME1           NO-GAP,  SY-VLINE NO-GAP,
          (20) IT_TAB-MATNR NO-GAP,
          (25) IT_TAB-TXZ01 NO-GAP,   SY-VLINE NO-GAP, "자재명.
          (9)  IT_TAB-TRANS_NM        NO-GAP,  SY-VLINE NO-GAP,
          (9)  IT_TAB-INCO1           NO-GAP,  SY-VLINE NO-GAP,
          (15) IT_TAB-MENGE UNIT IT_TAB-MEINS  NO-GAP,
                                      SY-VLINE NO-GAP,
          (5)  IT_TAB-MEINS NO-GAP,   SY-VLINE NO-GAP,
          (15) IT_TAB-NETPR   CURRENCY  IT_TAB-WAERS
                              NO-GAP, SY-VLINE NO-GAP,
          (19) IT_TAB-ZFAMT   CURRENCY  IT_TAB-WAERS   NO-GAP,
          (5)  IT_TAB-WAERS   NO-GAP, SY-VLINE.
* hide
   HIDE: IT_TAB.

   WRITE : / SY-ULINE.

   W_COUNT = W_COUNT + 1.
   W_MOD =  W_COUNT MOD 2.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   IF W_COUNT GT 0.
      WRITE : / '총', W_COUNT, '건'.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_TERM_LOW  text
*----------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP USING    P_ZTERM.

   TABLES : T052.

   CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = P_ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
     P_ZTERM = T052-ZTERM.
  ENDIF.

ENDFORM.                    " P1000_PAY_TERM_HELP

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE  0  TO  SY-LSIND.

  W_COUNT = 0.
  PERFORM P3000_TITLE_WRITE.
  PERFORM P2000_WRITE_DATA.

ENDFORM.                    " RESET_LIST
