*&---------------------------------------------------------------------*
*&  INCLUDE ZRIMIVPRC3TOP                                              *
*&---------------------------------------------------------------------*
*&  프로그램명 :  G/R Cancel doc. matching                             *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2003.05.17                                            *
*&  적용회사PJT: TGSY                                                  *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*

TABLES : ZTIVHST, ZTIV, ZTIVHSTIT, ZTIVIT, ZTBL.

DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFIVNO     LIKE ZTIV-ZFIVNO,           "Invoice 관리번?
      ZFIVHST    LIKE ZTIVHST-ZFIVHST,
      MBLNR      LIKE ZTIVHST-MBLNR,
      MJAHR      LIKE ZTIVHST-MJAHR,
      CMBLNR     LIKE ZTIVHST-CMBLNR,
      CMJAHR     LIKE ZTIVHST-CMJAHR,
      END OF IT_SELECTED.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C,
       ZFIVNO       LIKE ZTIV-ZFIVNO.
DATA : END OF IT_ERR_LIST.

*>>> MAIN INTERNAL TABLE.
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       MBLNR           LIKE   ZTIVHST-MBLNR,
       MJAHR           LIKE   ZTIVHST-MJAHR,
       BUDAT           LIKE   ZTIVHST-BUDAT,
       ZFIVNO          LIKE   ZTIVHST-ZFIVNO,  "통관요청관리번호.
       ZFIVHST         LIKE   ZTIVHST-ZFIVHST,
       ERNAM           LIKE   ZTIVHST-ERNAM,
       ZFGRST          LIKE   ZTIVHST-ZFGRST,
       CMBLNR          LIKE   ZTIVHST-CMBLNR,
       CMJAHR          LIKE   ZTIVHST-CMJAHR,
       EBELN           LIKE   ZTIVHSTIT-EBELN,
       EBELP           LIKE   ZTIVHSTIT-EBELP,
       MATNR           LIKE   ZTIVHSTIT-MATNR,
       TXZ01           LIKE   ZTIVHSTIT-TXZ01,
       GRMENGE         LIKE   ZTIVHSTIT-GRMENGE,
       MEINS           LIKE   ZTIVHSTIT-MEINS,
       END OF IT_TAB.

DATA : W_CMBLNR          LIKE   ZTIVHST-CMBLNR,
       W_CMJAHR          LIKE   ZTIVHST-CMJAHR,
       W_ZFGRST_NM(10).

DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_ROW_MARK        TYPE C,
       INCLUDE(8)        TYPE C,             "
       OPTION(1)         TYPE C,             " 공통 popup Screen에서 사?
       ANTWORT(1)        TYPE C,             " 공통 popup Screen에서 사?
       CANCEL_OPTION     TYPE C,             " 공통 popup Screen에서 사?
       TEXTLEN           TYPE I,             " 공통 popup Screen에서 사?
       W_OK_CODE1        LIKE SY-UCOMM,
       W_SY_UCOMM        LIKE SY-UCOMM,
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C,
       W_FIELD_NM        LIKE   DD03D-FIELDNAME,   " 필드명.
       OK-CODE           LIKE  SY-UCOMM,
       W_ZFGRST,
       W_TEXT70(30).

DATA : IT_ZSIVIT          LIKE ZTIVIT    OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVHSTIT       LIKE ZSIVHSTIT OCCURS 0 WITH HEADER LINE.
DATA : IT_EKBE            LIKE EKBE      OCCURS 0 WITH HEADER LINE.
