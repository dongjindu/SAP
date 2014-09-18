*----------------------------------------------------------------------*
*   INCLUDE ZRIMPMTTOP                                                 *
*----------------------------------------------------------------------*
TABLES: ZTPMTEDI," EDI 수신 Payment Notice 정보.
        ZTPMTHD, " Payment Notice Header
        ZTRED,
        EKKO,
        ZTBL,
        ZTREQST,
        ZTPMTIV. " Payment Notice Invoice

DATA:   BEGIN OF IT_TAB OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   ZTPMTEDI.
DATA:   END   OF IT_TAB.
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       ZFDHENO    LIKE ZTPMTEDI-ZFDHENO,
       ZFPNNO     LIKE ZTPMTEDI-ZFPNNO,
       ZFREQNO    LIKE ZTREQST-ZFREQNO,          " 수입의뢰 관리번호.
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       EBELN      LIKE ZTPMTEDI-EBELN,
       ZFOPNNO    LIKE ZTPMTEDI-ZFOPNNO,
       ZFHBLNO    LIKE ZTPMTEDI-ZFHBLNO,
       ZFISNO     LIKE ZTPMTEDI-ZFISNO,
END OF IT_SELECTED.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       W_ITCOUNT(3),                             " 품목 COUNT.
       W_FIELD_NM        LIKE DD03D-FIELDNAME.   " 필드명.
