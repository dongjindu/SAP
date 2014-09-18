*----------------------------------------------------------------------*
*   INCLUDE ZRIMISNSTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : 적하보험 부보 현황                                    *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.15                                            *
*&                                                                     *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

INCLUDE : <ICON>.

TABLES : ZTINS,
         ZTREQHD,
         ZTINSSG2,
         ZTINSSG3,
         ZTIMIMG00,
         ZTRECST,
         ZTINSAGR,
         ZTMLCSG7O,
         T005,
         ZTREQST.

*-----------------------------------------------------------------------
* SELECT RECORD
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFREQNO       LIKE ZTINS-ZFREQNO,
      ZFAMDNO       LIKE ZTINS-ZFAMDNO,
      ZFINSEQ       LIKE ZTINS-ZFINSEQ,
      W_AMDNO       LIKE ZTREQST-ZFAMDNO.
DATA: END OF IT_SELECTED.

*>>> ERROR .
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.

DATA : W_MAX_ZFAMDNO     LIKE ZTINS-ZFAMDNO,
       W_ZFORIG          LIKE ZTMLCSG7O-ZFORIG,
       INCLUDE(08),
       W_SUBRC           LIKE SY-SUBRC,
       W_MOD             TYPE I,
       W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,
       W_PAGE            TYPE I,
       W_LINE            TYPE I,
       LINE(3)           TYPE N,
       W_COUNT           TYPE I,
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,
       W_PRINT_COUNT     TYPE I,
       W_TABIX           LIKE SY-TABIX,
       P_BUKRS           LIKE ZTINS-BUKRS.
