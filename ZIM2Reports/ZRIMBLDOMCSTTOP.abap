*&---------------------------------------------------------------------*
*& Include ZRIMBLDOMCSTTOP                                            *
*&---------------------------------------------------------------------*
*&  프로그램명 : 하역운송비 검토서 DATA DEFINE                         *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.12                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
  TABLES: ZTBL, ZTIMIMG08.

  DATA : MAX-LINE TYPE I VALUE 152.     " REPORT 넓이.

  DATA : W_ERR_CHK(1)      TYPE C,
         W_SELECTED_LINES  TYPE P,                 " 선택 LINE COUNT
         W_PAGE            TYPE I,                 " Page Counter
         W_LINE            TYPE I,                 " 페이지당 LINE COUNT
         LINE(3)           TYPE N,                 " 페이지당 LINE COUNT
         W_COUNT           TYPE I,                 " 전체 COUNT
         W_TABIX           LIKE SY-TABIX,
         W_ITCOUNT(3),                             " 품목 COUNT.
         W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
         W_LIST_INDEX      LIKE SY-TABIX,
         W_LFA1            LIKE LFA1,
         TEMP              TYPE  F,
         W_SUBRC           LIKE SY-SUBRC.

  DATA : BEGIN OF ST_HEAD,
         ZFBLNO    LIKE    ZTBL-ZFBLNO,    " B/L No.
         ZFREBELN  LIKE    ZTBL-ZFREBELN,  " P/O 번호.
         ZFSHNO    LIKE    ZTBL-ZFSHNO,    " 선적차수.
         ZFMBLNO   LIKE    ZTBL-ZFMBLNO,
         ZFHBLNO   LIKE    ZTBL-ZFHBLNO,
         ZFCARNM   LIKE    ZTBL-ZFCARNM,   " 선기명.
         ZFRETA    LIKE    ZTBL-ZFRETA,    " 입항일.
         ZFINDT    LIKE    ZTBLINR_TMP-ZFINDT,   " 반입일.
         ZFTBLNO   LIKE    ZTBLINR_TMP-ZFTBLNO,  " 반입관리번호.
         ZFHSCL    LIKE    ZTBLINR_TMP-ZFHSCL,   " 화물관리.
         W_HSCL(30) TYPE C,
         ZFPKCN    LIKE    ZTBL-ZFPKCN,    " Packing수량.
         ZFPKCNM   LIKE    ZTBL-ZFPKCNM,
         ZFNEWT    LIKE    ZTBL-ZFNEWT,    " 선적중량.
         ZFNEWTM   LIKE    ZTBL-ZFNEWTM,
*         ZFTOWT    LIKE    ZTBL-ZFTOWT,   " 운임계산중량.
*         ZFTOWTM   LIKE    ZTBL-ZFTOWTM,
         ZFTOVL    LIKE    ZTBL-ZFTOVL,    " 선적용적.
         ZFTOVLM   LIKE    ZTBL-ZFTOVLM,
         ZFFORD    LIKE    ZTBL-ZFFORD,    "Forwarder
         W_FORD(28) TYPE C,
         ZFHAYEK   LIKE    ZTBL-ZFHAYEK,   "하역업체.
         W_HAYEK(28) TYPE C.
  DATA : END OF ST_HEAD .

  DATA : BEGIN OF IT_TAB_COST OCCURS 0,
         ZFBLNO     LIKE  ZTBLCST-ZFBLNO,  " 비용그룹.
         ZFCSCD     LIKE  ZTBLCST-ZFCSCD,
         ZFCDNM     LIKE  ZTIMIMG08-ZFCDNM,
*         ZFCAMT     LIKE  ZTBLCST-ZFCAMT,
         WAERS      LIKE  ZTBLCST-WAERS,
         ZFCKAMT    LIKE  ZTBLCST-ZFCKAMT,
         ZFVAT      LIKE  ZTBLCST-ZFVAT.
  DATA : END OF IT_TAB_COST .

 DATA : W_REMARK(56).

 DATA : W_SUB_VAT LIKE ZTBLCST-ZFCAMT,
        W_SUB_KRW LIKE ZTBLCST-ZFCKAMT,
        W_GRD_TOT LIKE ZTBKPF-WRBTR.
