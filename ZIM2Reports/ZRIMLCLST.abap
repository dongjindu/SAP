*&---------------------------------------------------------------------*
*& Report  ZRIMLCLST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 거래개설내역.                                         *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.02                                            *
*&---------------------------------------------------------------------*
*&   DESC. : 1.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMLCLST NO STANDARD PAGE HEADING MESSAGE-ID ZIM
                  LINE-SIZE 187.

TABLES: EKKO,                " ABAP Standard Header Table..
        EKPO,                " ABAP Standard Item Table..
        EKET,                " 납품일정계약의 일정라인..
        ZTREQHD,             " 수입의뢰 Header Table..
        LFA1,                " 거래처 Master Table..
        ZTREQORJ,            " 수입의뢰 원산지 Table..
        ZTREQST,             " 수입의뢰 상태 Table..
        ZTREQIT,             " 수입의뢰 Item Table..
        ZTMLCHD.             " Master L/C Header..

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

DATA: BEGIN OF IT_RN OCCURS 1000,          " REQNO조회위한IT선언.
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호.
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,  " 수입의뢰 Type.
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        LLIEF     LIKE   ZTREQHD-LLIEF,    " Supplying Vendor.
        ZFBENI    LIKE   ZTREQHD-ZFBENI,   " Different Invoicing Party.
        ZTERM     LIKE   ZTREQHD-ZTERM,    " Terms of Payment Key.
        INCO1     LIKE   ZTREQHD-INCO1,    " Incoterms (Part1).
        ZFREQSD   LIKE   ZTREQHD-ZFREQSD,  " 최종선적일.
        ZFLASTAM  LIKE   ZTREQHD-ZFLASTAM, " 최종개설금액.
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFUSDAM   LIKE   ZTREQHD-ZFUSDAM,  " USD 환산금액.
        ZFMATGB   LIKE   ZTREQHD-ZFMATGB,  " 자재구분.
        ZFUSD     LIKE   ZTREQHD-ZFUSD,    " 미화통화.
        ZFAPRT    LIKE   ZTREQHD-ZFAPRT,   " 도착항.
        BUKRS     LIKE   ZTREQHD-BUKRS,    " 회사코드.
        ZFITMNO   LIKE   ZTREQIT-ZFITMNO,  " 수입문서 품목번호.
        MATNR     LIKE   ZTREQIT-MATNR,    " Material Number.
        STAWN     LIKE   ZTREQIT-STAWN,    " Commodity Code.
        MENGE     LIKE   ZTREQIT-MENGE,    " 수입의뢰수량.
        MEINS     LIKE   ZTREQIT-MEINS,    " Base Unit of Measure.
        NETPR     LIKE   ZTREQIT-NETPR,    " Net Price.
        PEINH     LIKE   ZTREQIT-PEINH,    " Price Unit.
        BPRME     LIKE   ZTREQIT-BPRME,    " Order Price Unit.
        TXZ01     LIKE   ZTREQIT-TXZ01,    " Short Text.
        ZFAMDNO   LIKE   ZTREQST-ZFAMDNO,  " Amend Seq.
        ZFDOCST   LIKE   ZTREQST-ZFDOCST,  " 문서 상태.
        ZFRTNYN   LIKE   ZTREQST-ZFRTNYN,  " 접수 반려 여부.
        ZFRLST1   LIKE   ZTREQST-ZFRLST1,  " 의뢰 Release 상태.
        ZFRLST2   LIKE   ZTREQST-ZFRLST2,  " 개설 Release 상태.
        CDAT      LIKE   ZTREQST-CDAT,     " Created on.
        ZFREQDT   LIKE   ZTREQST-ZFREQDT,  " 요개설일자.
        ERNAM     LIKE   ZTREQST-ERNAM,    " Creater.
        EKORG     LIKE   ZTREQST-EKORG,    " Purch. Org.
        EKGRP     LIKE   ZTREQST-EKGRP,    " Purch. Grp.
        ZFOPNNO   LIKE   ZTREQST-ZFOPNNO,  " 신용장-승인번호.
        ZFOPAMT   LIKE   ZTREQST-ZFOPAMT,  " 개설 금액.
        ZFREF1    LIKE   ZTREQST-ZFREF1,   " 참조사항-업무연락.
        ZFREF2    LIKE   ZTREQST-ZFREF2,   " 참조사항-업무연락.
      END OF IT_RN.

*-----------------------------------------------------------------------
* Master L/C Data 조회를 위한 Internal Table 선언.
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_MRN OCCURS 1000.
      INCLUDE STRUCTURE ZTMLCHD.
DATA  END OF IT_MRN.

*-----------------------------------------------------------------------
* 수입의뢰 원산지 조회를 위한 Internal Table 선언.
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_ORJ OCCURS 1000.
      INCLUDE STRUCTURE ZTREQORJ.
DATA  END OF IT_ORJ.

*-----------------------------------------------------------------------
* Vendor Master Data 조회를 위한 Internal Table 선언.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_VD OCCURS 1000.
      INCLUDE STRUCTURE LFA1.
DATA  END OF IT_VD.

*-----------------------------------------------------------------------
* Error Check 과 Table Index 저장을 위한 변수 선언.
*-----------------------------------------------------------------------
DATA: W_ERR_CHK   TYPE  C,
      W_TABIX     TYPE  I.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_BUKRS   FOR ZTREQHD-BUKRS,    " 회사코드.
               S_MATNR   FOR EKPO-MATNR,       " 자재 No.
               S_EBELN   FOR EKKO-EBELN,       " P/O No.
               S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 No.
               S_OPNNO   FOR ZTREQHD-ZFOPNNO,  " L/C No.
               S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
               S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
               S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type.
               S_DOCST   FOR ZTREQST-ZFDOCST DEFAULT 'O'.  " 문서상태.
SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.

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
   SET TITLEBAR 'TIT1'.
*   SET PF-STATUS 'ZIM92'.

* List Write...
   PERFORM P3000_WRITE_LC_DATA.           "결과 출력...

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT LINE-SELECTION.

DATA : L_TEXT(20).

  GET CURSOR FIELD L_TEXT.
  CASE L_TEXT.   " 필드명..

    WHEN 'IT_RN-EBELN'.
       SET PARAMETER ID 'BES'  FIELD IT_RN-EBELN.
       CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.

    WHEN 'IT_RN-ZFREQNO'.
       SET PARAMETER ID 'ZPREQNO' FIELD IT_RN-ZFREQNO.
       CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

  ENDCASE.
  CLEAR: IT_RN.

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

   SKIP 2.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE: /70 '[ 거 래 개 설 의 뢰 ]'
              COLOR COL_HEADING INTENSIFIED OFF.

   FORMAT RESET.
   WRITE: / 'Date: ', SY-DATUM.
   ULINE.

*>>> Field Name을 Title에 기술해준다.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE: /  SY-VLINE NO-GAP, '자재번호          ' NO-GAP,
             SY-VLINE NO-GAP,
            '품명                                    ' NO-GAP,
             SY-VLINE NO-GAP, '구매오더No' NO-GAP,
             SY-VLINE NO-GAP, '품목 ' NO-GAP,
             SY-VLINE NO-GAP, '수입의뢰No' NO-GAP,
             SY-VLINE NO-GAP, 'S/D       ' NO-GAP,
             SY-VLINE NO-GAP, '납품일    ' NO-GAP,
             SY-VLINE NO-GAP, '수량             ' NO-GAP,
             SY-VLINE NO-GAP, '단가            ' NO-GAP,
             SY-VLINE NO-GAP, 'ORJ' NO-GAP,
             SY-VLINE NO-GAP, '공급처코드' NO-GAP,
             SY-VLINE NO-GAP, '공급사                   ' NO-GAP,
             SY-VLINE NO-GAP.

   FORMAT COLOR COL_HEADING INTENSIFIED OFF.

    WRITE:  / SY-VLINE NO-GAP, '모선명            ' NO-GAP,
              SY-VLINE NO-GAP, '도착항' NO-GAP,
           61 SY-VLINE NO-GAP, '참조사항1' NO-GAP,
          129 SY-VLINE NO-GAP, '참조사항2' NO-GAP,
          187 SY-VLINE NO-GAP.
   FORMAT RESET.
   ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_DATA.

   PERFORM P1000_READ_RN_DATA.
   PERFORM P1000_READ_PO_DATA.
   PERFORM P1000_READ_ORJ_DATA.
   PERFORM P1000_READ_VD_DATA.

   IF W_ERR_CHK EQ 'Y'.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_LC_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_LC_DATA.

PERFORM P2000_SORT_IT_DATA.

LOOP AT IT_RN.

   READ TABLE IT_PO  WITH KEY EBELN = IT_RN-EBELN.

   SELECT * FROM  EKET UP TO 1 ROWS
            WHERE EBELN EQ IT_PO-EBELN
            AND   EBELP EQ IT_PO-EBELP
            ORDER BY EINDT.
      EXIT.
   ENDSELECT.

   READ TABLE IT_ORJ WITH KEY ZFREQNO = IT_RN-ZFREQNO.
   READ TABLE IT_VD  WITH KEY LIFNR = IT_PO-LIFNR.

   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

*>>> 자재번호를 Write 하기 위한 Logic.
   WRITE: SY-VLINE NO-GAP, IT_RN-MATNR NO-GAP.

*>>> 품명을 기술하기 위한 Logic.
   WRITE: SY-VLINE NO-GAP, IT_RN-TXZ01 NO-GAP.

*>>> 구매오더번호를 기술하기 위한 Logic.
   WRITE: SY-VLINE NO-GAP, IT_RN-EBELN NO-GAP.
   WRITE: SY-VLINE NO-GAP, IT_RN-ZFITMNO NO-GAP.

   WRITE:    SY-VLINE NO-GAP, IT_RN-ZFREQNO NO-GAP,
             SY-VLINE NO-GAP, IT_RN-ZFREQSD NO-GAP,
             SY-VLINE NO-GAP, EKET-EINDT NO-GAP,
             SY-VLINE NO-GAP, IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP,
             SY-VLINE NO-GAP, IT_RN-NETPR CURRENCY IT_PO-WAERS NO-GAP,
             SY-VLINE NO-GAP, IT_ORJ-ZFORIG NO-GAP,
             SY-VLINE NO-GAP, IT_PO-LIFNR NO-GAP,
             SY-VLINE NO-GAP, IT_VD-NAME1(25) NO-GAP,
             SY-VLINE NO-GAP.
      HIDE: IT_RN.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

   WRITE: / SY-VLINE NO-GAP, '                  ' NO-GAP,
            SY-VLINE NO-GAP, IT_RN-ZFAPRT NO-GAP,
         61 SY-VLINE NO-GAP, IT_RN-ZFREF1(67) NO-GAP,
            SY-VLINE NO-GAP, IT_RN-ZFREF2(57) NO-GAP,
        187 SY-VLINE.
   FORMAT RESET.
      HIDE: IT_RN.

   W_TABIX = W_TABIX + 1.

      CLEAR: IT_RN.
   ULINE.
ENDLOOP.

   WRITE: '총:', W_TABIX, '건'.

ENDFORM.                    " P3000_WEITE_LC_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_PO_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_PO
     FROM   EKKO AS H INNER JOIN EKPO AS I
     ON     H~EBELN EQ I~EBELN
            FOR ALL ENTRIES IN IT_RN
     WHERE  H~EBELN EQ IT_RN-EBELN
     AND    I~EBELP EQ IT_RN-ZFITMNO
     AND    H~BSTYP EQ 'F'.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
   EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_PO_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_RN_DATA.

   DATA: L_LINE_COUNT TYPE I.

   W_ERR_CHK = 'N'.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_RN
     FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
     ON     H~ZFREQNO EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN  S_REQNO
     AND    H~EBELN    IN  S_EBELN
     AND    I~MATNR    IN  S_MATNR
     AND    H~ZFREQTY  IN  S_REQTY
     AND    H~ZFOPNNO  IN  S_OPNNO.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

   LOOP AT IT_RN.
     W_TABIX = SY-TABIX.
     SELECT SINGLE ZFAMDNO ZFDOCST ZFRLST1 ZFRLST2 CDAT EKORG
                   EKGRP ZFOPNNO WAERS
              INTO (IT_RN-ZFAMDNO, IT_RN-ZFDOCST, IT_RN-ZFRLST1,
              IT_RN-ZFRLST2, IT_RN-CDAT, IT_RN-EKORG, IT_RN-EKGRP,
              IT_RN-ZFOPNNO, IT_RN-WAERS)
              FROM ZTREQST
              WHERE ZFREQNO EQ   IT_RN-ZFREQNO
              AND   EKORG   IN   S_EKORG
              AND   EKGRP   IN   S_EKGRP
              AND   ZFAMDNO EQ ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                                 WHERE ZFREQNO EQ IT_RN-ZFREQNO ).
     IF SY-SUBRC = 0.
        MODIFY IT_RN INDEX W_TABIX.
     ELSE.
        DELETE IT_RN INDEX W_TABIX.
     ENDIF.

   ENDLOOP.

   DESCRIBE TABLE IT_RN LINES L_LINE_COUNT.
   IF L_LINE_COUNT = 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MLC_DATA
*&---------------------------------------------------------------------*
*       Master LC Data를 끌어오기 위한 서브루틴.
*----------------------------------------------------------------------*
FORM P1000_READ_MLC_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_MRN
            FROM ZTMLCHD
            FOR ALL ENTRIES IN IT_RN
            WHERE ZFREQNO   EQ  IT_RN-ZFREQNO.
      IF SY-SUBRC NE 0.
         W_ERR_CHK = 'Y'.
         MESSAGE S353.
         EXIT.
      ENDIF.

ENDFORM.                    " P1000_READ_MLC_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_ORJ_DATA
*&---------------------------------------------------------------------*
*  원산지관련 데이타를 끌어오기 위한 서브루틴.
*----------------------------------------------------------------------*
FORM P1000_READ_ORJ_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ORJ
            FROM ZTREQORJ
            FOR ALL ENTRIES IN IT_RN
           WHERE ZFREQNO   EQ   IT_RN-ZFREQNO
             AND ZFLSG7O   EQ   '00010'.

ENDFORM.                    " P1000_READ_ORJ_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_VD_DATA
*&---------------------------------------------------------------------*
*  Vendor Master 테이블 데이터를 읽기 위한 서브루틴.
*----------------------------------------------------------------------*
FORM P1000_READ_VD_DATA.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_VD
            FROM LFA1
            FOR ALL ENTRIES IN IT_PO
            WHERE LIFNR   EQ   IT_PO-LIFNR.

ENDFORM.                    " P1000_READ_VD_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_IT_DATA
*&---------------------------------------------------------------------*
* Internal Table의 값들을 항목별로 정렬.
*----------------------------------------------------------------------*
FORM P2000_SORT_IT_DATA.

   SORT IT_RN BY TXZ01 EBELN ZFITMNO ZFREQNO.

ENDFORM.                    " P2000_SORT_IT_DATA
