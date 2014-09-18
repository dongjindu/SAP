*&---------------------------------------------------------------------*
*& Report  ZRIMLCLST02                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입실적(개설기준)                                    *
*&      작성자 : 맹성호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.13                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLCLST02    MESSAGE-ID ZIM
                       LINE-SIZE 147
                       NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO     LIKE ZTREQHD-ZFREQNO,         " 수입의뢰 번호.
       EBELN       LIKE ZTREQHD-EBELN,           " P/O No.
       ZFSHCU      LIKE ZTREQHD-ZFSHCU,          " 선적국.
       PAMDNO(16)  TYPE C,                       " ALL AMEND 회차.
       WAERS       LIKE ZTREQHD-WAERS,           " Currency
       ZFBENI      LIKE ZTREQHD-ZFBENI,          " Beneficiary
       NAME2(24),                                " Name 1
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " 개설은행.
       NAME3(24),                                " Name 1
       TXZ01       LIKE ZTREQIT-TXZ01,           " 자재내역.
       ZFUSAT      LIKE ZTMLCHD-ZFUSAT,          " 지급조건.
       ZFAMDNO     LIKE ZTREQST-ZFAMDNO,         " Amend 회차.
       ZFUSD       LIKE ZTREQST-ZFUSD,           " USD Currency
       ZFOPNNO     LIKE ZTREQST-ZFOPNNO,         " L/C No.
       ZFOPNDT     LIKE ZTREQST-ZFOPNDT,         " 개설일.
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM.         " USD 환산금액.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
       ZFREQNO     LIKE ZTREQHD-ZFREQNO,         " 수입의뢰 번호.
       EBELN       LIKE ZTREQHD-EBELN,           " P/O No.
       ZFSHCU      LIKE ZTREQHD-ZFSHCU,          " 선적국.
       PAMDNO(16)  TYPE C,                       " ALL AMEND 회차.
       WAERS       LIKE ZTREQHD-WAERS,           " Currency
       ZFBENI      LIKE ZTREQHD-ZFBENI,          " Beneficiary
       NAME2(24),                                " Name 1
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " 개설은행.
       NAME3(24),                                " Name 1
       TXZ01       LIKE ZTREQIT-TXZ01,           " 자재내역.
       ZFUSAT      LIKE ZTMLCHD-ZFUSAT,          " 지급조건.
       ZFAMDNO     LIKE ZTREQST-ZFAMDNO,         " Amend 회차.
       ZFUSD       LIKE ZTREQST-ZFUSD,           " USD Currency
       ZFOPNNO     LIKE ZTREQST-ZFOPNNO,         " L/C No.
       ZFOPNDT     LIKE ZTREQST-ZFOPNDT,         " 개설일.
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM.         " USD 환산금액.
DATA : END OF IT_TAB_DOWN.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
INCLUDE ZRIMLSLSTTOP.
INCLUDE ZRIMSORTCOM.                        " 수입의뢰 Report Sort
INCLUDE ZRIMUTIL01.                         " Utility Function 모음.

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: S_RLDT   FOR SY-DATUM,          " 개설일.
                    S_OPBN   FOR ZTREQHD-ZFOPBN,    " 개설은행.
                    S_MATGB  FOR ZTREQHD-ZFMATGB,   " 자재구분.
                    S_REQTY  FOR ZTREQHD-ZFREQTY,   " 수입의뢰 Type
                    S_WERKS  FOR ZTREQHD-ZFWERKS,   " 대표 Plant
                    S_EKORG  FOR ZTREQST-EKORG,     " Purch. Org.
                    S_EBELN  FOR ZTREQHD-EBELN,     " P/O No.
                    S_LIFNR  FOR ZTREQHD-LIFNR,     " Vendor
                    S_ZFBENI FOR ZTREQHD-ZFBENI,    " Beneficiary
                    S_EKGRP  FOR ZTREQST-EKGRP,     " Purch. Group
                    S_REQNO  FOR ZTREQHD-ZFREQNO.   " 수입의뢰 관리번호.
SELECTION-SCREEN END OF BLOCK B1.

* Parameter 초기값 Setting
INITIALIZATION.                                     " 초기값 SETTING
    PERFORM P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
    PERFORM P3000_TITLE_WRITE.                      " 헤더 출력...

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 테이블 Select
    PERFORM P1000_GET_ZTREQHD         USING W_ERR_CHK.

* 레포트 Write
    PERFORM P3000_DATA_WRITE          USING W_ERR_CHK.
    IF W_ERR_CHK EQ 'Y'.    EXIT.     ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_GET_ZTREQHD USING    P_W_ERR_CHK.

ENDFORM.                    " P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESET_LIST.

ENDFORM.                    " RESET_LIST
