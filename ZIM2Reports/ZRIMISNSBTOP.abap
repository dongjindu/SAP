*----------------------------------------------------------------------*
*   INCLUDE ZRIMISNSTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : 적하보험 부보 현황                                    *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.05                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

INCLUDE : <ICON>.

TABLES : ZTINSB,
         ZTBL,
         ZTINSBSG2,
         ZTINSBSG3,
         ZTBLCST,
         ZTINSBAGR,
         ZTMLCSG7O,
         ZTIMIMG00,
         T005,
         ZTBLIT.

*-----------------------------------------------------------------------
* SELECT RECORD 용.
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFBLNO       LIKE ZTINSB-ZFBLNO,        " 수입의뢰 관리번호.
*      ZFAMDNO       LIKE ZTINS-ZFAMDNO,        " 보험 Amend Seq.
      ZFINSEQ       LIKE ZTINSB-ZFINSEQ.
*      W_AMDNO       LIKE ZTREQST-ZFAMDNO.
DATA: END OF IT_SELECTED.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C.
*              ZFIVNO       LIKE ZTIV-ZFIVNO.
DATA : END OF IT_ERR_LIST.


*DATA : W_MAX_ZFAMDNO     LIKE ZTINS-ZFAMDNO,
DATA : W_ZFORIG          LIKE ZTMLCSG7O-ZFORIG,
       INCLUDE(08),
       W_SUBRC           LIKE SY-SUBRC,
       W_MOD             TYPE I,
       W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       LINE(3)           TYPE N,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME, " 필드명.
       W_PRINT_COUNT     TYPE I,             " 출력을위해 선택된 개수.
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_ZFINSEQ         LIKE ZTINSBRSP-ZFINSEQ.
