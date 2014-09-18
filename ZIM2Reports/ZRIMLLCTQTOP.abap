*----------------------------------------------------------------------*
*   INCLUDE ZRIMLLCQTTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : Local L/C 어음도착통보?
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.06.21                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES :  ZTVTIV,    " 세금계산서용 Invoice Header
          ZTVTIVIT,    "  Item Database View
          ZTREQHD,   " 수입의?
          ZTPMTHD,   " Payment Notice Head
          ZTPMTIV,    " Payment Notice Invoice
          ZTLLCHD,   " Local L/C Head
          ZTLLCAMHD. " Local L/C Amend

DATA: W_ZFREQNO        LIKE ZTREQHD-ZFREQNO, " 수입의뢰 관리번?
      W_ZFOPNNO        LIKE ZTREQHD-ZFOPNNO, " L/C No
      W_ZFBENI         LIKE ZTREQHD-ZFBENI,  " Vendor
      W_ZFBENI_NM(20)  TYPE C,
      W_ZFOPBN         LIKE ZTREQHD-ZFOPBN,  " 개설은?
      W_ZFOPBN_NM(20)  TYPE C,
      W_ZFOPNDT        LIKE ZTREQST-ZFOPNDT, " 개설?
      W_ZFEXDT         LIKE ZTLLCHD-ZFEXDT,  " 유효기?
      W_ZFEXRT         LIKE ZTLLCHD-ZFEXRT,  " 환?
      W_ZFOPAMT        LIKE ZTLLCHD-ZFOPAMT, " 개설금?
      W_ZFOPAMTC       LIKE ZTLLCHD-ZFOPAMTC, "개설금액통?
      W_ZFOPKAM        LIKE ZTLLCHD-ZFOPKAM, " 개설금액(원화)
      W_ZFOPAMT_1      LIKE ZTLLCHD-ZFOPAMT, " 발생금?
      W_ZFOPKAM_1      LIKE ZTLLCHD-ZFOPKAM, " 발생금액(원화)
      W_ZFOPAMT_2      LIKE ZTLLCHD-ZFOPAMT, " 결제금?
      W_ZFOPKAM_2      LIKE ZTLLCHD-ZFOPKAM, " 결제금액(원화)
      W_ZFOPAMT_3      LIKE ZTLLCHD-ZFOPAMT, " 잔?
      W_ZFOPKAM_3      LIKE ZTLLCHD-ZFOPKAM, " 잔액(원화)
      W_ZFOPAMT_4      LIKE ZTLLCHD-ZFOPAMT, " 미입고잔?
      W_ZFOPKAM_4      LIKE ZTLLCHD-ZFOPKAM, " 미입고잔액(원화)
      W_ZFAMDNO        LIKE ZTREQST-ZFAMDNO,
      W_ZFPNNO         LIKE ZTPMTHD-ZFPNNO.

DATA : OPTION(1)       TYPE C,             " 공통 popup Screen에서 사?
       ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
       CANCEL_OPTION   TYPE C,             " 공통 popup Screen에서 사?
       TEXTLEN         TYPE I,             " 공통 popup Screen에서 사?
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
