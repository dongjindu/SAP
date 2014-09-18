*&---------------------------------------------------------------------*
*& INCLUDE ZRIMLCPLSTTOP.                                              *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입실적(개설일기준) Include.                         *
*&      작성자 : 홍재풍 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.26                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Tables 및 변수 Define                                                *
*----------------------------------------------------------------------*
TABLES : ZTREQST,ZTPMTHD,LFA1,ZTREQIT,ZTREQHD.

DATA : BEGIN OF IT_SELECTED OCCURS 0,
       ZFREQNO    LIKE ZTREQST-ZFREQNO,          " 수입의뢰 관리번호.
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
END OF IT_SELECTED.

DATA: BEGIN OF IT_SELECTED_PN OCCURS 0,
      ZFPNNO     LIKE ZTPMTHD-ZFPNNO,    " Payment Notice 관리번호.
END OF IT_SELECTED_PN.

DATA : W_LINE            TYPE  I,
       W_COUNT           TYPE  I,
       W_LIST_INDEX      LIKE SY-TABIX,
       W_SELECTED_LINES  TYPE  I,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,
       W_PAGE            TYPE  I.
