*&---------------------------------------------------------------------*
*& Include ZRIMBLVSLCSTTOP                                             *
*&---------------------------------------------------------------------*
*&  프로그램명 : 해상 운임 검토서  DATA DEFINE                         *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.11                                            *
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
         TEMP              TYPE  F.

  DATA : BEGIN OF ST_HEAD,
         ZFBLNO    LIKE    ZTBL-ZFBLNO,   " B/L No.
         ZFGMNO    LIKE    ZTBL-ZFGMNO,   " 화물관리번호.
         ZFMSN     LIKE    ZTBL-ZFMSN,
         ZFHSN     LIKE    ZTBL-ZFHSN,
         ZFREBELN  LIKE    ZTBL-ZFREBELN, " P/O 번호.
         ZFSHNO    LIKE    ZTBL-ZFSHNO,   " 선적차수.
         ZFMBLNO   LIKE    ZTBL-ZFMBLNO,
         ZFHBLNO   LIKE    ZTBL-ZFHBLNO,
         ZFCARNM   LIKE    ZTBL-ZFCARNM,  " 선기명.
         ZFRETA    LIKE    ZTBL-ZFRETA,   " 입항일.
         ZFINDT    LIKE    ZTBLINR_TMP-ZFINDT,   " 반입일.
         ZFTBLNO   LIKE    ZTBLINR_TMP-ZFTBLNO,  " 반입관리번호.
         ZFETD     LIKE    ZTBL-ZFETD,    " 선적일.
         INCO1     LIKE    ZTBL-INCO1,    " 인도방법.
         ZFSPRT    LIKE    ZTBL-ZFSPRT,   " 선적지.
         ZFAPRT    LIKE    ZTBL-ZFAPRT,   " 도착지.
         ZFCDTY    LIKE    ZTBL-ZFCDTY,   " 코드 구분.
         ZFCD      LIKE    ZTBL-ZFCD,     " 요율상 선적지코드.
         W_AREA    LIKE    ZTIMIMG08-ZFCDNM, " 요율상 선적지.
         ZFEXRTT   LIKE    ZTBL-ZFEXRTT,  " 환율.
         ZFEXDTT   LIKE    ZTBL-ZFEXDTT,  " 환율적용일.
         ZFWERKS   LIKE    ZTBL-ZFWERKS,  " 수용 사업소.
         W_WERKS(30) TYPE   C,             " PLANT명.
         ZFNEWT    LIKE    ZTBL-ZFNEWT,   " 선적중량.
         ZFNEWTM   LIKE    ZTBL-ZFNEWTM,
         ZFTOWT    LIKE    ZTBL-ZFTOWT,   " 운임계산중량.
         ZFTOWTM   LIKE    ZTBL-ZFTOWTM,
         ZFTOVL    LIKE    ZTBL-ZFTOVL,   " 선적용적.
         ZFTOVLM   LIKE    ZTBL-ZFTOVLM,
         ZF20FT    LIKE    ZTBL-ZF20FT,
         ZF40FT    LIKE    ZTBL-ZF40FT,
         ZFGITA    LIKE    ZTBL-ZFGITA,
         ZFGTPK    LIKE    ZTBL-ZFGTPK,
         ZFMRATE   LIKE    ZTBL-ZFMRATE,  " 요율.
         ZFNETPR1  LIKE    ZTBL-ZFNETPR1,
         ZFNETPR2  LIKE    ZTBL-ZFNETPR2,
         ZFUPWT    LIKE    ZTBL-ZFUPWT,    " 차상위 중량.
         ZFDFUP    LIKE    ZTBL-ZFDFUP,    " 차상위 사용여부.
         ZFCHARGE  LIKE    ZTBL-ZFCHARGE,  " 협의운임사용여부.
         ZFFRE     LIKE    ZTBL-ZFFRE,     " 협의 운임.
         ZFTRCUR   LIKE    ZTBL-ZFTRCUR.   " 운임통화.
  DATA : END OF ST_HEAD .

  DATA : BEGIN OF IT_TAB_COST OCCURS 0,
         ZFBLNO     LIKE  ZTBLCST-ZFBLNO,    " 비용그룹.
         ZFCSCD     LIKE  ZTBLCST-ZFCSCD,
         ZFCDNM     LIKE  ZTIMIMG08-ZFCDNM,
         ZFCAMT     LIKE  ZTBLCST-ZFCAMT,
         WAERS      LIKE  ZTBLCST-WAERS,
         ZFCKAMT    LIKE  ZTBLCST-ZFCKAMT.
  DATA : END OF IT_TAB_COST .

  DATA : W_OTH_WT  LIKE ZTBL-ZFTOWT,
         W_OTH_WTM LIKE ZTBL-ZFTOWTM,
         W_DF_CHA  LIKE ZTBKPF-WRBTR,
         W_UP_CHA  LIKE ZTBKPF-WRBTR.

 DATA : W_REMARK(40).

 DATA : W_SUB_TOT LIKE ZTBLCST-ZFCAMT,
        W_SUB_KRW LIKE ZTBLCST-ZFCKAMT,
        W_GRD_TOT LIKE ZTBLCST-ZFCAMT,
        W_GRD_KRW LIKE ZTBLCST-ZFCKAMT.
