*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMINSCRTBTOP                                             *
*&---------------------------------------------------------------------*
*&  프로그램명 : B/L 기준 부보 다중생성용 Include                      *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.09.06                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.      :
*&
*&---------------------------------------------------------------------*

TABLES : ZTBL,            " B/L
         ZTBLIT,          " B/L Item
         ZTPMTHD,         " Payment Notice Header
         ZTPMTIV,         " Payment Notice Invoice
         ZTBLINR,         " 반입신고.
         ZTIDS,           " 수입면허.
         DD03D,           " Dynpro fields for table fields
         T024E,           " 구매조직.
         T024,            " 구매그룹.
         LFA1,            " 구매처마스터 (일반섹션)
         TINC,            " 고객: 인도조건.
         EKPO,            " Purchasing Document Item
         ZVREQHD_ST,      " 수입의뢰 Header + Status View
         ZVEKKO_REQHD_ST, " EKKO + 수입의뢰 Header + Status View
         ZTOFF,           " OFFER SHEET
         ZTOFFFTX,        " OFFER SHEET FTX
         ZTIMIMGTX,       " EDI TEXT.
         ZTDHF1,          " 표준 EDI Flat Head
         ZTCDF1,          " 전자문서번호 채번(EDI)
         ZTIMIMG03,       " 보세구역 코드.
         ZTIMIMG00.       " 수입시스템 Basic Config

*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       GUBUN      TYPE C,                        " 최초 구분.
       ZFBLNO     LIKE ZTBL-ZFBLNO,              " B/L 관리번호.
       ZFMATGB    LIKE ZTBL-ZFMATGB,             " 자재구분.
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
DATA : IT_BLTMP      LIKE TABLE OF ZTBL WITH HEADER LINE.
DATA : BAPIMEPOITEM   LIKE  BAPIMEPOITEM   OCCURS 0 WITH HEADER LINE.
DATA : BAPIMEPOITEMX  LIKE  BAPIMEPOITEMX  OCCURS 0 WITH HEADER LINE.

DATA: DYNPROG            LIKE SY-REPID,
      DYNNR              LIKE SY-DYNNR,
      WINDOW_TITLE(30)   TYPE C.

*-----------------------------------------------------------------------
* Internal Table Define: IT_TAB_DOWN.
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
         W_EDI_RECORD(65535)  TYPE C,
       END   OF IT_TAB_DOWN.

*> RETURN MESSAGE 처리용.
 DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
         INCLUDE STRUCTURE   BAPIRET2.
 DATA:   END   OF RETURN.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       LINE(3)           TYPE N,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_TABIX          LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT     TYPE I,
       W_BUTTON_ANSWER  TYPE C,
       W_ITEM_CNT      LIKE SY-TABIX,          " 품목 count
       W_AMOUNT        LIKE ZTIV-ZFIVAMT,     " 수입의뢰 Amount
       W_TOT_AMOUNT    LIKE ZTIV-ZFIVAMT,      " 수입의뢰 Amount
       W_LOCAL_AMT     LIKE ZTIV-ZFIVAMT,      " USD 환산 Amount
       W_ZFPWDT        LIKE ZTPMTHD-ZFPWDT,
       W_EBELN         LIKE EKPO-EBELN,
       W_LFA1          LIKE LFA1,
       W_MENGE         LIKE ZTREQIT-MENGE,
       W_ZSREQIT       LIKE ZSREQIT,
       W_MAX_ZFAMDNO   LIKE ZTREQST-ZFAMDNO,
       OK-CODE         LIKE SY-UCOMM,
       ANTWORT         TYPE C,
       P_BUKRS         LIKE ZTREQHD-BUKRS.

RANGES: R_ZFRLST1 FOR ZTREQST-ZFRLST1 OCCURS 0.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_LFA1_SELECT
*&---------------------------------------------------------------------*
FORM P1000_GET_LFA1_SELECT USING    P_LIFNR
                           CHANGING P_LFA1.
   CLEAR : P_LFA1.
   IF P_LIFNR IS INITIAL.
      EXIT.
   ENDIF.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
   CALL FUNCTION 'READ_LFA1'
        EXPORTING
              XLIFNR          = P_LIFNR
        IMPORTING
              XLFA1           = P_LFA1
        EXCEPTIONS
              KEY_INCOMPLETE  = 01
              NOT_AUTHORIZED  = 02
              NOT_FOUND       = 03.

   CASE SY-SUBRC.
      WHEN 01.     MESSAGE I025.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
   ENDCASE.

   TRANSLATE P_LFA1  TO UPPER CASE.

ENDFORM.                    " P1000_GET_LFA1_SELECT
