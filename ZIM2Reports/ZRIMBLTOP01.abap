*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMBLTOP01                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 B/L 관련 Data Define용 Include                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.21                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* 사용 Table Define
*-----------------------------------------------------------------------
TABLES : ZTBL,        " BILL OF LADING
         LFA1,
         ZTBLCON,     " B/L CONTAINER
         ZTBLCST,     " B/L 비?
         ZTBLINOU,    " B/L 보세운?
         ZTBLINR,     " B/L 반입신?
        *ZTBLINR,     " B/L 반입신?
         ZTBLOUR,     " B/L 반출신?
         ZTIMIMG02,   "
         ZTIMIMG00,
         ZTIMIMGTX,
         ZTBLUG,      " 긴급보세운송 의?
         ZTIV,        " 통관.
         ZTBLUGC.     " 긴급보세운송 차?

*-----------------------------------------------------------------------
* 사용할 View Define
*-----------------------------------------------------------------------
TABLES : ZVBL_INOU.  " 반입예정정보 + 반입신고 DATABASE VIEW


*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       GUBUN      TYPE C,                        " 최초 구?
       ZFBLNO     LIKE ZTBLINR-ZFBLNO,           " B/L 관리번?
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,
       ZFBTSEQ    LIKE ZTBLINR-ZFBTSEQ,          " 보세운송 일련번?
       ZFUSCD     LIKE ZTBLINR-ZFUSCD,
       ZFPRIN     LIKE ZTBLINR-ZFPRIN,
       ZFINTY     LIKE ZTBLINR-ZFINTY,
       CHECK(1),
       ZFLOCK     TYPE C VALUE 'N',
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
DATA : IT_ZVREQ   LIKE ZVREQHD_ST OCCURS 0 WITH HEADER LINE.

*-----------------------------------------------------------------------
* Menu Statsu Function을 Inactive하기 위한 Internal Table
*-----------------------------------------------------------------------

DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.


DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_SUBRC           LIKE SY-SUBRC,
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_ZFINOU          LIKE ZTIMIMG00-ZFINOU,
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_OK_CODE         LIKE SY-UCOMM,      " OK-CODE
       W_OK_CODE_OLD     LIKE SY-UCOMM,      " OK-CODE
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C,
       W_STATUS          TYPE C.
