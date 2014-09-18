*&---------------------------------------------------------------------*
*& Report  ZRIMLCREQ                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : LC 개설신청서.                                        *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.12                                            *
*&    적용회사 : 제일제당                                              *
*&---------------------------------------------------------------------*
*&   DESC.     : L/C 개설신청서를 출력하기 위한 레포트.                *
*
*&---------------------------------------------------------------------*
*& [변경내용]  :
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLCREQ    MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 120.

TABLES : ZTREQHD,                      " 수입의뢰 Header Table..
         ZTREQIT,                      " 수입의뢰 Item   Table..
         ZTREQST,                      " 수입의뢰 상태   Table..
         ZTRECST,                      " 수입의뢰 비용   Table..
         ZTMLCSG7O,                    " Master L/C Seg 7 원산지 Table..
         ZTMLCHD,                      " Master L/C Header Table..
         ZTMLCSG7G,                    " Master L/C Seg 7 상품명세..
         ZTMLCSG8E,                    " Master L/C Seg 8 기타부가조건..
         ZTMLCSG2,                     " Master L/C Seg 2..
         ZTMLCSG910,                   " Master L/C Seg 9-10..
         ZTMLCSG9O,                    " Master L/C Seg 9 기타구비서류..
         T005T,                        " 국가이름..
         T005.                         " 국가..

*----------------------------------------------------------------------*
* Internal Table 선언... Master L/C 관련사항 입력..                    *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MLC OCCURS 1000,
        ZFREQNO    LIKE  ZTMLCHD-ZFREQNO,    " 수입의뢰관리번호..
*        ZFOPBN     LIKE  ZTMLCHD-ZFOPBN,     " 개설은행 거래처코드..
        ZFOBNM     LIKE  ZTMLCHD-ZFOBNM,     " 개설은행명..
        ZFABNM     LIKE  ZTMLCHD-ZFABNM,     " 통지은행명..
        ZFTRMB     LIKE  ZTMLCHD-ZFTRMB,     " 화환/혼합/연지급 구분VV..
        ZFALCQ     LIKE  ZTMLCHD-ZFALCQ,     " 과부족허용율 사용여부..
        ZFALCP     LIKE  ZTMLCHD-ZFALCP,     " 과부족허용율..
        ZFPRMT     LIKE  ZTMLCHD-ZFPRMT,     " 분할선적허용여부..
        ZFTRMT     LIKE  ZTMLCHD-ZFTRMT,     " 환적허용여부..
        ZFAPRT     LIKE  ZTMLCHD-ZFAPRT,     " 도착항..
        ZFSPRT     LIKE  ZTMLCHD-ZFSPRT,     " 선적항..
        ZFOPME     LIKE  ZTMLCHD-ZFOPME,     " 개설방법..
        ZFCARR     LIKE  ZTMLCHD-ZFCARR,     " 선박회사/선박명..
        ZFUSAT     LIKE  ZTMLCHD-ZFUSAT,     " Usance/At Sight 구분..
        ZFADCD1    LIKE  ZTMLCHD-ZFADCD1,    " 주요부가조건1.
        ZFADCD2    LIKE  ZTMLCHD-ZFADCD2,    " 주요부가조건2.
        ZFADCD3    LIKE  ZTMLCHD-ZFADCD3,    " 주요부가조건3.
        ZFADCD4    LIKE  ZTMLCHD-ZFADCD4,    " 주요부가조건4.
        ZFLCTY     LIKE  ZTMLCHD-ZFLCTY,     " 신용장 종류..
        ZFADCD5    LIKE  ZTMLCHD-ZFADCD5,    " 주요부가조건5..
        ZFLTSD     LIKE  ZTMLCHD-ZFLTSD,     " 최종선적일..
        ZFEXDT     LIKE  ZTMLCHD-ZFEXDT,     " 유효기일..
        WAERS      LIKE  ZTMLCHD-WAERS,      " 통화키..
        ZFOPAMT    LIKE  ZTMLCHD-ZFOPAMT,    " 개설금액..
        INCO1      LIKE  ZTMLCHD-INCO1,      " 인도조건 (Part 1)..
        ZFTRTX1    LIKE  ZTMLCHD-ZFTRTX1,    " 지급조건명세1..
        ZFTRTX2    LIKE  ZTMLCHD-ZFTRTX2,    " 지급조건명세2..
        ZFTRTX3    LIKE  ZTMLCHD-ZFTRTX3,    " 지급조건명세3..
        ZFTRTX4    LIKE  ZTMLCHD-ZFTRTX4,    " 지급조건명세4..
        ZFUSPR     LIKE  ZTMLCHD-ZFUSPR,     " Usance 기간..
        ZFABBR     LIKE  ZTMLCHD-ZFABBR,     " 통지은행 지점명..
        ZFAPPNM    LIKE  ZTMLCSG2-ZFAPPNM,   " Applicant 상호/성명..
        ZFAPPAD1   LIKE  ZTMLCSG2-ZFAPPAD1,  " Applicant 주소 1..
        ZFAPPAD2   LIKE  ZTMLCSG2-ZFAPPAD2,  " Applicant 주소 2..
        ZFAPPAD3   LIKE  ZTMLCSG2-ZFAPPAD3,  " Applicant 주소 3..
        ZFBENI1    LIKE  ZTMLCSG2-ZFBENI1,   " 수익자 상호/주소 1..
        ZFBENI2    LIKE  ZTMLCSG2-ZFBENI2,   " 수익자 상호/주소 2..
        ZFBENI3    LIKE  ZTMLCSG2-ZFBENI3,   " 수익자 상호/주소 3..
        ZFBENI4    LIKE  ZTMLCSG2-ZFBENI4,   " 수익자 상호/주소 4..
        ZFBENIA    LIKE  ZTMLCSG2-ZFBENIA,   " 수익자 Account No..
        ZFOCEYN    LIKE  ZTMLCSG910-ZFOCEYN, " Ocean Bill 첨부여부..
        ZFOCEAC    LIKE  ZTMLCSG910-ZFOCEAC, " Ocean Bill 운임지불여부..
        ZFOCEC1    LIKE  ZTMLCSG910-ZFOCEC1, " Ocean Bill 하수인1..
        ZFOCEC2    LIKE  ZTMLCSG910-ZFOCEC2, " Ocean Bill 하수인2..
        ZFOCEAN    LIKE  ZTMLCSG910-ZFOCEAN, " Ocean Bill 통지처..
        ZFCOMYN    LIKE  ZTMLCSG910-ZFCOMYN, " 상업송장 첨부여부..
        ZFNOCOM    LIKE  ZTMLCSG910-ZFNOCOM, " 상업송장 통수..
        ZFAIRYN    LIKE  ZTMLCSG910-ZFAIRYN, " Air Bill 첨부여부..
        ZFAIRC1    LIKE  ZTMLCSG910-ZFAIRC1, " Air Bill 하수인 1..
        ZFAIRC2    LIKE  ZTMLCSG910-ZFAIRC2, " Air Bill 하수인 2..
        ZFAIRAC    LIKE  ZTMLCSG910-ZFAIRAC, " Air Bill 운임지불여부..
        ZFAIRAN    LIKE  ZTMLCSG910-ZFAIRAN, " Air Bill 통지처..
        ZFINYN     LIKE  ZTMLCSG910-ZFINYN,  " 보험증권 첨부여부..
        ZFINCO1    LIKE  ZTMLCSG910-ZFINCO1, " 부보조건 1..
        ZFINCO2    LIKE  ZTMLCSG910-ZFINCO2, " 부보조건 2..
        ZFPACYN    LIKE  ZTMLCSG910-ZFPACYN, " Packing List 첨부여부..
        ZFNOPAC    LIKE  ZTMLCSG910-ZFNOPAC, " Packing List 통수..
        ZFCEOYN    LIKE  ZTMLCSG910-ZFCEOYN, " 원산지증명서 첨부여부..
        ZFOTDYN    LIKE  ZTMLCSG910-ZFOTDYN, " 기타구비서류 첨부여부..
      END OF IT_MLC.

*----------------------------------------------------------------------*
* Internal Table 선언... 수입의뢰관련 테이블..                         *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_RN OCCURS 1000,
        ZFREQNO   LIKE   ZTREQHD-ZFREQNO,  " 수입의뢰번호..
        ZFREQED   LIKE   ZTREQHD-ZFREQED,  " 의뢰 유효일..
        ZFREQSD   LIKE   ZTREQHD-ZFREQSD,  " 의뢰선적 유효일..
        ZFREQTY   LIKE   ZTREQHD-ZFREQTY,  " 수입의뢰 Type.
        EBELN     LIKE   ZTREQHD-EBELN,    " Purchasing Document Number.
        LIFNR     LIKE   ZTREQHD-LIFNR,    " Vendor's Account Number.
        LLIEF     LIKE   ZTREQHD-LLIEF,    " Supplying Vendor.
        ZFBENI    LIKE   ZTREQHD-ZFBENI,   " Different Invoicing Party.
        ZTERM     LIKE   ZTREQHD-ZTERM,    " Terms of Payment Key.
        INCO1     LIKE   ZTREQHD-INCO1,    " Incoterms (Part1).
        ZFLASTAM  LIKE   ZTREQHD-ZFLASTAM, " 최종개설금액.
        WAERS     LIKE   ZTREQHD-WAERS,    " Currency Key.
        ZFUSDAM   LIKE   ZTREQHD-ZFUSDAM,  " USD 환산금액.
        ZFMATGB   LIKE   ZTREQHD-ZFMATGB,  " 자재구분.
        ZFUSD     LIKE   ZTREQHD-ZFUSD,    " 미화통화.
        ZFSPRT    LIKE   ZTREQHD-ZFSPRT,   " 선적항.
        ZFAPRT    LIKE   ZTREQHD-ZFAPRT,   " 도착항.
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
      END OF IT_RN.

*----------------------------------------------------------------------*
* Internal Table 선언.. 반복수 Seg 8 기타부가조건을 읽기위한 Table..   *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_SG8E OCCURS 500,
        ZFREQNO   LIKE   ZTMLCSG8E-ZFREQNO, " 수입의뢰관리번호..
        ZFLSG8E   LIKE   ZTMLCSG8E-ZFLSG8E, " 반복수 Seg 8 기타부가조건.
        ZFOACD1   LIKE   ZTMLCSG8E-ZFOACD1, " 기타 부가조건..
      END OF IT_SG8E.

*----------------------------------------------------------------------*
* Internal Table 선언.. 반복수 Seg 7 상품명세를 읽기 위한 Table..      *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_SG7G OCCURS 500,
        ZFREQNO   LIKE   ZTMLCSG7G-ZFREQNO, " 수입의뢰관리번호..
        ZFLSG7G   LIKE   ZTMLCSG7G-ZFLSG7G, " 반복수 Seg 7 상품명세..
        ZFDSOG1   LIKE   ZTMLCSG7G-ZFDSOG1, " 상품용역명세..
      END OF IT_SG7G.

*----------------------------------------------------------------------*
* Internal Table 선언.. Master L/C Seg 7 원산지를 읽기 위한 Table..    *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_SG7O OCCURS 500,
        ZFREQNO   LIKE   ZTMLCSG7O-ZFREQNO, " 수입의뢰관리번호..
        ZFLSG7O   LIKE   ZTMLCSG7O-ZFLSG7O, " 반복수 Seg 7 원산지..
        ZFORIG    LIKE   ZTMLCSG7O-ZFORIG,  " 자재원산국..
      END OF IT_SG7O.

*----------------------------------------------------------------------*
* Internal Table 선언.. Master L/C Seg 7 원산지를 읽기 위한 Table..    *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_SG9O OCCURS 500,
        ZFREQNO   LIKE   ZTMLCSG9O-ZFREQNO, " 수입의뢰관리번호..
        ZFLSG9O   LIKE   ZTMLCSG9O-ZFLSG9O, " 반복수 Seg 9 기타구비서류.
        ZFODOC1   LIKE   ZTMLCSG9O-ZFODOC1, " 기타구비서류 1..
      END OF IT_SG9O.

*----------------------------------------------------------------------*
* Internal Table 선언.. 국가이름을 읽기 위한 Table..                   *
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_T005T OCCURS 500,
        LAND1     LIKE   T005T-LAND1,       " 국가키..
        LANDX     LIKE   T005T-LANDX,       " 국가이름..
      END OF IT_T005T.

*----------------------------------------------------------------------*
* Error Check과 Table Index를 읽기위한 변수 선언..
*----------------------------------------------------------------------*
DATA: W_ERR_CHK   TYPE  C,
      W_TABIX     TYPE  I,
      TEMP        TYPE  I,
      NETPR(8)    TYPE  C.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS: S_REQNO   FOR ZTREQHD-ZFREQNO      " 수입의뢰관리번호..
                NO INTERVALS NO-EXTENSION OBLIGATORY.

SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* INITIALIZATION: 검색화면 직전에 발생하는 이벤트..
*-----------------------------------------------------------------------
INITIALIZATION.
   SET TITLEBAR 'TIT1'.          " Title: '신용장 개설 신청서'.

*-----------------------------------------------------------------------
* START-OF-SELECTION: 검색화면을 구성하기위한 이벤트..
* IT_MLC 와 IT_RN 관련 데이터를 읽어들이기 위한 Logic..
*-----------------------------------------------------------------------
START-OF-SELECTION.

   PERFORM P1000_READ_RN_DATA.
   CHECK W_ERR_CHK EQ 'N'.
   PERFORM P1000_READ_MLC_DATA.
   PERFORM P1000_READ_SG7O_DATA.
   PERFORM P1000_READ_SG7G_DATA.
   PERFORM P1000_READ_SG8E_DATA.
   PERFORM P1000_READ_SG9O_DATA.
   PERFORM P1000_READ_T005T_DATA.


*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
   CHECK W_ERR_CHK EQ 'N'.
* Title Text Write.
   SET TITLEBAR 'TIT1'.
   PERFORM P3000_WRITE_DATA.

*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.


*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MLC_DATA
*&---------------------------------------------------------------------*
*       Master L/C Data를 끌어오기 위한 Logic.
*----------------------------------------------------------------------*
FORM P1000_READ_MLC_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_MLC
     FROM   ZTMLCHD AS H INNER JOIN ZTMLCSG2 AS I
     ON     H~ZFREQNO  EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN S_REQNO.

*>>> ZTMLCSG910 Table을 IT_MLC Table에 삽입..
   LOOP AT IT_MLC.
      W_TABIX = SY-TABIX.
      SELECT SINGLE  ZFOCEYN ZFOCEAC ZFOCEC1 ZFOCEC2
                     ZFCOMYN ZFNOCOM ZFOCEAN ZFAIRYN
                     ZFAIRC1 ZFAIRC2 ZFAIRAC ZFINYN
                     ZFINCO1 ZFINCO2 ZFPACYN ZFNOPAC
                     ZFCEOYN ZFOTDYN
               INTO  (IT_MLC-ZFOCEYN, IT_MLC-ZFOCEAC,
                     IT_MLC-ZFOCEC1, IT_MLC-ZFOCEC2,
                     IT_MLC-ZFCOMYN, IT_MLC-ZFNOCOM,
                     IT_MLC-ZFOCEAN, IT_MLC-ZFAIRYN,
                     IT_MLC-ZFAIRC1, IT_MLC-ZFAIRC1,
                     IT_MLC-ZFAIRAC, IT_MLC-ZFINYN,
                     IT_MLC-ZFINCO1, IT_MLC-ZFINCO2,
                     IT_MLC-ZFPACYN, IT_MLC-ZFNOPAC,
                     IT_MLC-ZFCEOYN, IT_MLC-ZFOTDYN)
               FROM  ZTMLCSG910
              WHERE  ZFREQNO EQ IT_MLC-ZFREQNO.

      MODIFY IT_MLC  INDEX W_TABIX.
   ENDLOOP.

ENDFORM.                    " P1000_READ_MLC_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_RN_DATA
*&---------------------------------------------------------------------*
*       수입의뢰 관련 Data를 끌어오기 위한 Logic.
*----------------------------------------------------------------------*
FORM P1000_READ_RN_DATA.

   W_ERR_CHK = 'N'.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_RN
     FROM   ZTREQHD AS H INNER JOIN ZTREQIT AS I
     ON     H~ZFREQNO  EQ I~ZFREQNO
     WHERE  H~ZFREQNO  IN  S_REQNO.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

   READ TABLE IT_RN INDEX 1.
   IF SY-SUBRC EQ 0.
      IF IT_RN-ZFREQTY NE 'LC'.
         W_ERR_CHK = 'Y'.
         MESSAGE S385 WITH IT_RN-ZFREQNO IT_RN-ZFREQTY.
         EXIT.
      ENDIF.
   ENDIF.

ENDFORM.                    " P1000_READ_RN_DATA.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_SG7O_DATA
*&---------------------------------------------------------------------*
*       Master L/C Seg 7 원산지 데이터를 읽기위한 구문..
*----------------------------------------------------------------------*
FORM P1000_READ_SG7O_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_SG7O
     FROM  ZTMLCSG7O
     WHERE ZFREQNO IN S_REQNO.

ENDFORM.                    " P1000_READ_SG7O_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_SG7G_DATA
*&---------------------------------------------------------------------*
*       Master L/C Seg 7 상품명세 데이터를 읽기위한 구문..
*----------------------------------------------------------------------*
FORM P1000_READ_SG7G_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_SG7G
     FROM  ZTMLCSG7G
     WHERE ZFREQNO IN S_REQNO.

ENDFORM.                    " P1000_READ_SG7G_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_SG8E_DATA
*&---------------------------------------------------------------------*
*       Master L/C Seg 8 기타부가조건 데이터를 읽기위한 구문..
*----------------------------------------------------------------------*
FORM P1000_READ_SG8E_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_SG8E
     FROM  ZTMLCSG8E
     WHERE ZFREQNO IN S_REQNO.

ENDFORM.                    " P1000_READ_SG8E_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_SG9O_DATA
*&---------------------------------------------------------------------*
*       Master L/C Seg 9 기타구비서류 데이터를 읽기위한 구문..
*----------------------------------------------------------------------*
FORM P1000_READ_SG9O_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_SG9O
     FROM  ZTMLCSG9O
     WHERE ZFREQNO IN S_REQNO.

ENDFORM.                    " P1000_READ_SG9O_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_T005T_DATA
*&---------------------------------------------------------------------*
*       국가이름 데이터를 읽기위한 구문..
*----------------------------------------------------------------------*
FORM P1000_READ_T005T_DATA.

   SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_T005T
     FROM  T005T
     FOR ALL ENTRIES IN IT_SG7O
     WHERE LAND1 EQ IT_SG7O-ZFORIG
     AND   SPRAS EQ 'EN'.

ENDFORM.                    " P1000_READ_T005T_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_DATA
*&---------------------------------------------------------------------*
*       신용장 개설 신청서를 작성하기위한 Write 구문.
*----------------------------------------------------------------------*
FORM P3000_WRITE_DATA.
   LOOP AT IT_MLC.
      PERFORM P3000_TITLE_WRITE.    " 타이틀 기술을 위한 구문..
      PERFORM P3000_HIGH_WRITE.     " 윗부분 기술을 위한 구문..
      PERFORM P3000_USANCE_WRITE.   " Usance 기술을 위한 구문..
      PERFORM P3000_DSCRTN_WRITE.   " Description 기술을 위한 구문..
      PERFORM P3000_ITEM_WRITE.     " Item 기술을 위한 구문..
      PERFORM P3000_ORIG_WRITE.     " 원산지 지급기간 기술을 위한 구문..
      PERFORM P3000_OTHER_WRITE.    " 나머지 데이터 기술을 위한 구문..
      PERFORM P3000_ADD_WRITE.      " Additional Data 기술을 위한 구문..
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_AL_DATA
*&---------------------------------------------------------------------*
*       과부족허용률 Data 출력을 위한 구문..
*----------------------------------------------------------------------*
FORM P3000_WRITE_AL_DATA.

   IF NOT IT_MLC-ZFALCP IS INITIAL AND NOT IT_MLC-ZFALCQ IS INITIAL.
      WRITE: '(' NO-GAP, IT_MLC-ZFALCP NO-GAP, '% ' NO-GAP.

      IF IT_MLC-ZFALCQ EQ 'T'.
        WRITE 'Plus/Minus' NO-GAP.
      ELSEIF IT_MLC-ZFALCQ EQ 'X'.
         WRITE 'Maximum' NO-GAP.
      ELSEIF IT_MLC-ZFALCQ EQ '2AA'.
         WRITE 'Up To' NO-GAP.
      ELSEIF IT_MLC-ZFALCQ EQ '2AB'.
         WRITE 'Not Exceeding' NO-GAP.
      ENDIF.
      WRITE ')' NO-GAP.
   ENDIF.

   IF NOT IT_MLC-ZFALCP IS INITIAL AND IT_MLC-ZFALCQ IS INITIAL.
      WRITE: '(' NO-GAP, IT_MLC-ZFALCP NO-GAP,
             '% ' NO-GAP, ')' NO-GAP.
   ENDIF.

   IF IT_MLC-ZFALCP IS INITIAL AND NOT IT_MLC-ZFALCQ IS INITIAL.
      WRITE '(' NO-GAP.

      IF IT_MLC-ZFALCQ EQ 'T'.
        WRITE 'Plus/Minus' NO-GAP.
      ELSEIF IT_MLC-ZFALCQ EQ 'X'.
         WRITE 'Maximum' NO-GAP.
      ELSEIF IT_MLC-ZFALCQ EQ '2AA'.
         WRITE 'Up To' NO-GAP.
      ELSEIF IT_MLC-ZFALCQ EQ '2AB'.
         WRITE 'Not Exceeding' NO-GAP.
      ENDIF.
      WRITE ')' NO-GAP.
   ENDIF.

ENDFORM.                    " P3000_WRITE_AL_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       프르그램의 타이틀을 출력하기 위한 서브루틴.
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

   SKIP.
   IF IT_MLC-ZFLCTY EQ '5'.          " '취소가능/불능'을 구별하려고..
      WRITE 50 '취소불능' NO-GAP.
   ELSE.
      WRITE 50 '취소가능' NO-GAP.
   ENDIF.

   WRITE: '화환신용장 개설 신청서' NO-GAP,
      /43 'APPLICATION FOR ' NO-GAP.

   IF IT_MLC-ZFLCTY EQ '5'.
      WRITE 'IRREVOCABLE ' NO-GAP.
   ELSE.
      WRITE 'IRREVOCABLE TRANSFER'.
   ENDIF.
   WRITE 'DOCUMENTARY CREDIT' NO-GAP.

   SKIP.
   WRITE: 'TO:' NO-GAP, IT_MLC-ZFOBNM NO-GAP,
        / 'HAVE REQUEST YOU TO ESTABLISH BY ' NO-GAP.

   IF IT_MLC-ZFOPME EQ 'DF'.        " 개설방법 구별하려고..
      WRITE 'Full Cable' NO-GAP.
   ELSEIF IT_MLC-ZFOPME EQ 'DG'.
      WRITE 'Short Cable' NO-GAP.
   ELSEIF IT_MLC-ZFOPME EQ 'DD'.
      WRITE 'Air Mail' NO-GAP.
   ENDIF.

   WRITE: 105 'DATE: ' NO-GAP, SY-DATUM.
   ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_HIGH_WRITE
*&---------------------------------------------------------------------*
*       프로그램의 윗부분을 출력하기위한 구문.
*----------------------------------------------------------------------*
FORM P3000_HIGH_WRITE.

   WRITE: / SY-VLINE NO-GAP, 'CREDIT NUMBER' NO-GAP,
         30 SY-VLINE NO-GAP, 120 SY-VLINE NO-GAP, SY-ULINE NO-GAP,
          / SY-VLINE NO-GAP, 'ADVISING BANK' NO-GAP,
         30 SY-VLINE NO-GAP, IT_MLC-ZFABNM NO-GAP,
        120 SY-VLINE NO-GAP.

*>>> 통지은행 지점명을 기술하기 위한 구문..
   IF  NOT IT_MLC-ZFABBR IS INITIAL.
      WRITE:
          / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP, IT_MLC-ZFABBR NO-GAP,
        120 SY-VLINE NO-GAP.
   ENDIF.

*>>> Beneficiary(수익자) 관련 Data 기술을 위한 구문..
   WRITE:   SY-ULINE NO-GAP,
          / SY-VLINE NO-GAP, 'BENEFICIARY' NO-GAP,
         30 SY-VLINE NO-GAP.

   IF NOT IT_MLC-ZFBENI1 IS INITIAL.
      WRITE: IT_MLC-ZFBENI1 NO-GAP.
   ENDIF.
   WRITE 120 SY-VLINE NO-GAP.

   IF NOT IT_MLC-ZFBENI2 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFBENI2 NO-GAP.
      IF NOT IT_MLC-ZFBENI3 IS INITIAL.
         WRITE: IT_MLC-ZFBENI3 NO-GAP.
      ENDIF.
      WRITE: 120 SY-VLINE NO-GAP.
   ENDIF.

   IF NOT IT_MLC-ZFBENI4 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFBENI4 NO-GAP.
      IF NOT IT_MLC-ZFBENIA IS INITIAL.
         WRITE: IT_MLC-ZFBENIA.
      ENDIF.
      WRITE: 120 SY-VLINE NO-GAP, SY-ULINE.
   ENDIF.

   WRITE: / SY-VLINE NO-GAP, 'APPLICANT' NO-GAP,
         30 SY-VLINE NO-GAP.

   IF NOT IT_MLC-ZFAPPNM IS INITIAL.
      WRITE: IT_MLC-ZFAPPNM NO-GAP.
   ENDIF.

   IF NOT IT_MLC-ZFAPPAD1 IS INITIAL.
      WRITE: IT_MLC-ZFAPPAD1 NO-GAP, 120 SY-VLINE NO-GAP.
   ENDIF.

   IF NOT IT_MLC-ZFAPPAD2 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFAPPAD2 NO-GAP, 120 SY-VLINE NO-GAP.
   ENDIF.

   IF NOT IT_MLC-ZFAPPAD3 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFAPPAD3 NO-GAP, 120 SY-VLINE NO-GAP.
   ENDIF.
   ULINE.

   WRITE: / SY-VLINE NO-GAP, 'AMOUNT' NO-GAP,
         30 SY-VLINE NO-GAP, IT_MLC-WAERS NO-GAP,
            IT_MLC-ZFOPAMT CURRENCY IT_MLC-WAERS.

   PERFORM P3000_WRITE_AL_DATA.

   WRITE: 120 SY-VLINE NO-GAP, SY-ULINE NO-GAP,
            / SY-VLINE NO-GAP, 'EXPIRY DATE' NO-GAP,
           30 SY-VLINE NO-GAP, IT_MLC-ZFEXDT(4) NO-GAP,
              '/' NO-GAP, IT_MLC-ZFEXDT+4(2) NO-GAP,
              '/' NO-GAP, IT_MLC-ZFEXDT+6(2) NO-GAP,
          120 SY-VLINE NO-GAP, SY-ULINE NO-GAP,
            / SY-VLINE NO-GAP, 'SHIPPING DATE' NO-GAP,
           30 SY-VLINE NO-GAP, IT_MLC-ZFLTSD(4) NO-GAP,
              '/' NO-GAP, IT_MLC-ZFLTSD+4(2) NO-GAP,
              '/' NO-GAP, IT_MLC-ZFLTSD+6(2) NO-GAP,
          120 SY-VLINE NO-GAP, SY-ULINE NO-GAP,
            / SY-VLINE NO-GAP, 'DRAFT`S TENOR' NO-GAP,
           30 SY-VLINE NO-GAP.

   IF NOT IT_MLC-ZFTRTX1 IS INITIAL.
      WRITE: IT_MLC-ZFTRTX1 NO-GAP.
   ENDIF.
   WRITE: 120 SY-VLINE NO-GAP.

   IF NOT IT_MLC-ZFTRTX2 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFTRTX2 NO-GAP, 120 SY-VLINE.
   ENDIF.

   IF NOT IT_MLC-ZFTRTX3 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFTRTX3 NO-GAP, 120 SY-VLINE.
   ENDIF.

   IF NOT IT_MLC-ZFTRTX4 IS INITIAL.
      WRITE: / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
               IT_MLC-ZFTRTX4 NO-GAP, 120 SY-VLINE.
   ENDIF.

   WRITE: SY-ULINE.
   WRITE: / SY-VLINE NO-GAP, 5 SY-VLINE NO-GAP,
           'USANCE L/C ONLY' NO-GAP, 50 SY-VLINE.

ENDFORM.                    " P3000_HIGH_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_USANCE_WRITE
*&---------------------------------------------------------------------*
*       USANCE 기술을 위한 구문..
*----------------------------------------------------------------------*
FORM P3000_USANCE_WRITE.

   IF IT_MLC-ZFUSAT EQ 'US'.
      WRITE 'SHIPPER`S' NO-GAP.
   ELSEIF IT_MLC-ZFUSAT EQ 'UB'.
      WRITE 'BANKER`S' NO-GAP.
   ENDIF.

   WRITE: 120 SY-VLINE NO-GAP, SY-ULINE.

ENDFORM.                    " P3000_USANCE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_DSCRTN_WRITE
*&---------------------------------------------------------------------*
*       Description 기술을 위한 구문..
*----------------------------------------------------------------------*
FORM P3000_DSCRTN_WRITE.

   IF IT_MLC-ZFOCEYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP,
              ' ■FULL SET OF CLEAN ON BOARD OCEAN BILLS OF ' NO-GAP,
              'LOADING MADE OUT TO THE ORDER OF ' NO-GAP,
               IT_MLC-ZFOCEC1 NO-GAP, 120 SY-VLINE NO-GAP,
             / SY-VLINE NO-GAP, '   ' NO-GAP,IT_MLC-ZFOCEC2 NO-GAP,
              ' MARKED FREIGHT ' NO-GAP.

*>>> Ocean Bill 운임지불여부 Check.
      IF IT_MLC-ZFOCEAC EQ '31'.
         WRITE 'Prepaid ' NO-GAP.
      ELSEIF IT_MLC-ZFOCEAC EQ '32'.
         WRITE 'Collect ' NO-GAP.
      ENDIF.
      WRITE: 'AND NOTIFY ' NO-GAP, IT_MLC-ZFOCEAN NO-GAP,
          120 SY-VLINE NO-GAP.
   ENDIF.

*>>> Commercial Invoice 첨부여부 Check.
   IF IT_MLC-ZFCOMYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP, ' ■INVOICE IN ' NO-GAP.
      IF IT_MLC-ZFNOCOM GE 10.
         WRITE: IT_MLC-ZFNOCOM NO-GAP, ' COPIES' NO-GAP.
      ELSEIF IT_MLC-ZFNOCOM LT 10.
         WRITE: IT_MLC-ZFNOCOM+1(1) NO-GAP, ' COPIES' NO-GAP.
      ENDIF.
      WRITE 120 SY-VLINE NO-GAP.
   ENDIF.

*>>> Air Bill 첨부여부 Check.
   IF IT_MLC-ZFAIRYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP, ' ■AIRWAY BILL CONSIGNED OT ' NO-GAP,
               IT_MLC-ZFAIRC1 NO-GAP, IT_MLC-ZFAIRC1 NO-GAP,
             / ' MARKED FREIGHT ' NO-GAP.
      IF IT_MLC-ZFAIRAC EQ '31'.
         WRITE 'PREPAID '.
      ELSE.
         WRITE 'COLLECT '.
      ENDIF.
      WRITE: 'AND NOTIFY ', IT_MLC-ZFAIRAN, 120 SY-VLINE.
   ENDIF.

*>>> 보험증권 첨부여부 Check.
   IF IT_MLC-ZFINYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP,
               ' ■FULL SET OF INSURANCE POLICIES OR ' NO-GAP,
               'CERTIFICATES, ENDORDED IN BANK' NO-GAP,
           120 SY-VLINE NO-GAP,
             / SY-VLINE NO-GAP,
               '   FOR 110% OF INVOICE VALUE, EXPRESSLY' NO-GAP,
               ' STIPULATING THAT CLAIMS' NO-GAP,
           120 SY-VLINE NO-GAP,
             / SY-VLINE NO-GAP,
               '   ARE PAYABLE IN KOREA AND IT MUST INCLUDE' NO-GAP,
               ' : INSTITUTE CARGO CLAUSE' NO-GAP,
           120 SY-VLINE NO-GAP,
             / SY-VLINE NO-GAP, '  ', IT_MLC-ZFINCO1 NO-GAP,
           120 SY-VLINE NO-GAP,
             / SY-VLINE NO-GAP, '  ', IT_MLC-ZFINCO2 NO-GAP,
           120 SY-VLINE NO-GAP.
   ENDIF.

*>>> Packing List 첨부여부 Check.
   IF IT_MLC-ZFPACYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP, ' ■PACKING LIST IN ' NO-GAP.
      IF IT_MLC-ZFNOPAC GE 10.
         WRITE IT_MLC-ZFNOPAC NO-GAP.
      ELSEIF IT_MLC-ZFNOPAC LT 10.
         WRITE IT_MLC-ZFNOPAC+1(1) NO-GAP.
      ENDIF.
      WRITE: ' Copies ' NO-GAP, 120 SY-VLINE.
   ENDIF.

*>>> 원산지증명서 첨부여부 Check.
   IF IT_MLC-ZFCEOYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP, ' ■CERTIFICATE OF ORIGIN' NO-GAP,
           120 SY-VLINE NO-GAP.
   ENDIF.

*>>> 기타구비서류첨부여부 Check.
   IF IT_MLC-ZFOTDYN EQ 'X'.
      WRITE: / SY-VLINE NO-GAP, ' ■OTHER DOCUMENT(S) [if any]' NO-GAP,
           120 SY-VLINE NO-GAP.

      SORT IT_SG8E BY ZFLSG8E.

      LOOP AT IT_SG8E WHERE ZFREQNO = IT_MLC-ZFREQNO.
         WRITE: / SY-VLINE NO-GAP, '   ' NO-GAP,
                  IT_SG8E-ZFOACD1 NO-GAP, 120 SY-VLINE NO-GAP.
      ENDLOOP.

   ENDIF.

ENDFORM.                    " P3000_DSCRTN_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_ITEM_WRITE
*&---------------------------------------------------------------------*
*       Item Description 기술을 위한 구문...
*----------------------------------------------------------------------*
FORM P3000_ITEM_WRITE.

   WRITE: / SY-VLINE NO-GAP, 3 SY-ULINE(116) NO-GAP,
        120 SY-VLINE NO-GAP,
          / SY-VLINE NO-GAP, 3 SY-VLINE NO-GAP, 'HS CODE' NO-GAP,
         18 SY-VLINE NO-GAP, 'DESCRIPTION' NO-GAP,
         54 SY-VLINE NO-GAP, 'QUANTITY' NO-GAP,
         75 SY-VLINE NO-GAP, 'UNIT PRICE' NO-GAP,
         98 SY-VLINE NO-GAP, 'AMOUNT' NO-GAP,
        118 SY-VLINE NO-GAP, 120 SY-VLINE NO-GAP,
          / SY-VLINE NO-GAP, 3 SY-ULINE(116) NO-GAP,
        120 SY-VLINE NO-GAP.

   LOOP AT IT_RN WHERE ZFREQNO = IT_MLC-ZFREQNO.

      IF IT_RN-PEINH EQ 1.
         TEMP = IT_RN-MENGE * IT_RN-NETPR.
      ELSE.
         TEMP = ( IT_RN-MENGE * IT_RN-NETPR ) / IT_RN-PEINH.
      ENDIF.

      NETPR(8) = IT_RN-NETPR.
      WRITE: / SY-VLINE NO-GAP, 3 SY-VLINE NO-GAP, IT_RN-STAWN NO-GAP,
            18 SY-VLINE NO-GAP, IT_RN-TXZ01,
            54 SY-VLINE NO-GAP,
               IT_RN-MENGE UNIT IT_RN-MEINS NO-GAP, IT_RN-MEINS NO-GAP,
            75 SY-VLINE NO-GAP, IT_MLC-WAERS NO-GAP,
               NETPR(8) CURRENCY IT_RN-WAERS NO-GAP.
*            IT_RN-NETPR CURRENCY IT_RN-WAERS NO-GAP.

      IF IT_RN-PEINH EQ 1.
         WRITE: '/' NO-GAP, IT_RN-BPRME NO-GAP.
      ELSE.
         WRITE: '/' NO-GAP, IT_RN-PEINH NO-GAP,
                IT_RN-BPRME NO-GAP.
      ENDIF.

      WRITE: 98 SY-VLINE NO-GAP, IT_MLC-WAERS NO-GAP, TEMP NO-GAP,
            118 SY-VLINE NO-GAP, 120 SY-VLINE,
              / SY-VLINE NO-GAP, 3 SY-VLINE NO-GAP,
             18 SY-VLINE NO-GAP,
             54 SY-VLINE NO-GAP.

      PERFORM P3000_WRITE_AL_DATA.
      WRITE:  75 SY-VLINE NO-GAP, 98 SY-VLINE NO-GAP.

      PERFORM P3000_WRITE_AL_DATA.
      WRITE: 118 SY-VLINE NO-GAP, 120 SY-VLINE NO-GAP.

   ENDLOOP.

   WRITE: / SY-VLINE NO-GAP, 3 SY-ULINE(116),
        120 SY-VLINE NO-GAP, SY-ULINE.

ENDFORM.                    " P3000_ITEM_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_ORIG_WRITE
*&---------------------------------------------------------------------*
*       원산지와 PRICE TERM을 기술하기위한 Logic.
*----------------------------------------------------------------------*
FORM P3000_ORIG_WRITE.

   WRITE: / SY-VLINE NO-GAP, 'PRICE TERM' NO-GAP,
         25 SY-VLINE NO-GAP, IT_MLC-INCO1 NO-GAP,
         59 SY-VLINE NO-GAP, 'COUNTRY OF ORIGIN' NO-GAP,
         89 SY-VLINE NO-GAP.

   READ TABLE IT_SG7O WITH KEY ZFREQNO  = IT_MLC-ZFREQNO
                               ZFLSG7O  = '00010'.

   READ TABLE IT_T005T WITH KEY LAND1 = IT_SG7O-ZFORIG.
   WRITE: IT_T005T-LANDX NO-GAP.

   WRITE: 120 SY-VLINE NO-GAP, SY-ULINE,
            / SY-VLINE NO-GAP,
             'DESCRIPTION OF GOODS / SERVICE :' NO-GAP,
          120 SY-VLINE NO-GAP.
   LOOP AT IT_SG7G.
      WRITE: / SY-VLINE NO-GAP, 34 IT_SG7G-ZFDSOG1 NO-GAP,
           120 SY-VLINE NO-GAP.
   ENDLOOP.
   WRITE: SY-ULINE.

ENDFORM.                    " P3000_ORIG_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_OTHER_WRITE
*&---------------------------------------------------------------------*
*       나머지 데이터 출력을 위한 구문..
*----------------------------------------------------------------------*
FORM P3000_OTHER_WRITE.

*>>> SHIPMENT 등등의 데이터를 기술하기위한 구문..
   WRITE: / SY-VLINE NO-GAP, 'SHIPMENT' NO-GAP,
         30 SY-VLINE NO-GAP, 'FROM' NO-GAP,
         45 SY-VLINE NO-GAP, IT_RN-ZFSPRT NO-GAP,
        120 SY-VLINE NO-GAP,
          / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
         30 SY-ULINE, 120 SY-VLINE NO-GAP,
          / SY-VLINE NO-GAP, 30 SY-VLINE NO-GAP,
            'TO' NO-GAP, 45 SY-VLINE NO-GAP,
            IT_RN-ZFAPRT NO-GAP, 120 SY-VLINE NO-GAP, SY-ULINE.

*>>> 분할선적, 환적여부를 기술하기위한 구문...
   WRITE:  SY-VLINE NO-GAP, 'PARTIAL SHIPMENT :' NO-GAP.

      IF IT_MLC-ZFPRMT EQ 10.
         WRITE: 'PROHIBITED' NO-GAP.
      ELSE.
         WRITE: 'ALLOWED' NO-GAP.
      ENDIF.

   WRITE: 59 SY-VLINE NO-GAP, 'TRANSHIPMENT :' NO-GAP.

      IF IT_MLC-ZFTRMT EQ 8.
         WRITE: 'PROHIBITED' NO-GAP.
      ELSE.
         WRITE: 'ALLOWED' NO-GAP.
      ENDIF.

   WRITE: 120 SY-VLINE NO-GAP, SY-ULINE.

ENDFORM.                    " P3000_OTHER_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_ADD_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_ADD_WRITE.

*>>> ADDITIONAL CONDITION 기술을 위한 구문...
   WRITE: / SY-VLINE NO-GAP, 'ADDITIONAL CONDITION :' NO-GAP,
        120 SY-VLINE NO-GAP.
   WRITE: / SY-VLINE NO-GAP, 120 SY-VLINE.

   IF IT_MLC-ZFADCD1 EQ 'X'.
      WRITE: / SY-VLINE NO-GAP, ' ■SHIPMENT BY' NO-GAP,
               IT_MLC-ZFCARR, 120 SY-VLINE NO-GAP.
   ENDIF.

   IF IT_MLC-ZFADCD2 EQ 'X'.
      WRITE: / SY-VLINE NO-GAP,
               ' ■ACCEPTANCE COMMISSION AND DISCOUNT ' NO-GAP,
               'CHARGES ARE FOR BUYER`S ACCOUNT' NO-GAP,
           120 SY-VLINE.
   ENDIF.

   IF IT_MLC-ZFADCD3 EQ 'X'.
     WRITE: / SY-VLINE NO-GAP,
              ' ■ALL DOCUMENT MUST BEAR OUR CREDIT NUMBER' NO-GAP,
          120 SY-VLINE.
   ENDIF.

   IF IT_MLC-ZFADCD4 EQ 'X'.
      WRITE: / SY-VLINE NO-GAP,
              ' ■LATE PRESENTATION B/L ACCEPTANCE.' NO-GAP,
           120 SY-VLINE.
   ENDIF.

   IF IT_MLC-ZFADCD5 EQ 'X'.
*         READ TABLE IT_SG9O WITH KEY ZFREQNO = IT_MLC-ZFREQNO.
      SORT IT_SG9O BY ZFLSG9O.
      WRITE: / SY-VLINE NO-GAP,
             ' ■Other Additional Conditions [If Any]' NO-GAP,
           120 SY-VLINE.
      LOOP AT IT_SG9O WHERE ZFREQNO = IT_MLC-ZFREQNO.
         WRITE: / SY-VLINE NO-GAP, '   ' NO-GAP,
                  IT_SG9O-ZFODOC1 NO-GAP,
              120 SY-VLINE NO-GAP.
      ENDLOOP.
   ENDIF.
      ULINE.

ENDFORM.                    " P3000_ADD_WRITE
