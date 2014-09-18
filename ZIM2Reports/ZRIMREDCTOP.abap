*----------------------------------------------------------------------*
*   INCLUDE ZRIMREDCTOP                                                *
*----------------------------------------------------------------------*
*&  프로그램명 : 인수증 편성취소/EDI Create                            *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&
*&      작성일 : 2000.03.08                                            *
*&  적용회사PJT: 현대전자산업 주식회사                                 *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTIMIMG00,
         ZTVT,
         ZTVTSG1,
         ZTVTSG3,
         ZTRED,           " 인수?
         ZTREDSG1,       " 인수증 Seg1
         ZTVTIV,          " 세금계산서용 Invoice
         LFA1,    " Vendor Master (General Section)
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?
*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFREDNO     LIKE ZTRED-ZFREDNO,      "인수증 관리번호
      LIFNR       LIKE LFA1-LIFNR,       " Vendor Code
      ZFDOCNO     LIKE ZTVT-ZFDOCNO,       " 전자 문서번?
END OF IT_SELECTED.

DATA : W_ZFDHENO       LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC       LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHREF       LIKE   ZTDHF1-ZFDHREF,
*       W_ZFDHDDB       LIKE   ZTDHF1-ZFDHDDB,
       W_ZFDHSRO       LIKE   ZTDHF1-ZFDHSRO,
       W_ZFREDNO       LIKE   ZTRED-ZFREDNO,
       W_CNT           TYPE I.             " 처리건?


DATA : OPTION(1)       TYPE C,             " 공통 popup Screen에서 사?
       ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
       CANCEL_OPTION   TYPE C,             " 공통 popup Screen에서 사?
       TEXTLEN         TYPE I,             " 공통 popup Screen에서 사용
       W_PROC_CNT      TYPE I.             " 처리건?

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C.
