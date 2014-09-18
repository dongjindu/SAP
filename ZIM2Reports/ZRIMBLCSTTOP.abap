*----------------------------------------------------------------------*
*   INCLUDE ZRIMBLCSTTOP                                               *
*----------------------------------------------------------------------*
*&  프로그램명 : B/L 비용 Posting                          *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.05.23                                            *
*&  적용회사PJT: 현대전자산업 주식회사                                 *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
TABLES : ZTBL,            " Bill of Lading
         BSEG,
         COBL,
         ZTCIVHST,
         ZTCIVHD,
         ZTBLCST,         " B/L 비?
         LFA1,            " Vendor Master
         ZTIMIMG00,       " 관리코?
         ZTIMIMG08,       " 관리코?
         ZTIMIMG11,       " G/R, I/V, 비용처리 Configuration
         J_1BT001WV,      " Assign Branch to Plant
         ZVT001W,
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?

*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED OCCURS 0,
      BUKRS       LIKE ZTBLCST-BUKRS,
      ZFVEN       LIKE ZTBLCST-ZFVEN,
      ZFPAY       LIKE ZTBLCST-ZFPAY,
      ZTERM       LIKE ZTBLCST-ZTERM,
      MWSKZ       LIKE ZTBLCST-MWSKZ,
      ZFWERKS     LIKE ZTBLCST-ZFWERKS,
      ZFCAMT      LIKE ZTBLCST-ZFCAMT,
      WAERS       LIKE ZTBLCST-WAERS,
      ZFVAT       LIKE ZTBLCST-ZFVAT,
      ZFPOYN      LIKE ZTBL-ZFPOYN,
      ZFIMDTY     LIKE ZTIVCD-ZFIMDTY,
      ZFIMDNO     LIKE ZTIVCD-ZFIMDNO,
      ZFCLSEQ     LIKE ZTIVCD-ZFCLSEQ,
      ZFCSQ       LIKE ZTIVCD-ZFCSQ,
      GRP_MARK(10)    TYPE   C,
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* 비용관련 CODE INTERNAL TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTIMIMG08 OCCURS 0.
         INCLUDE STRUCTURE ZTIMIMG08.
DATA     END OF IT_ZTIMIMG08.

*-----------------------------------------------------------------------
* BL 비용 TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTBLCST OCCURS 0.
         INCLUDE STRUCTURE ZTBLCST.
DATA     END OF IT_ZTBLCST.

*-----------------------------------------------------------------------
* LOCK OF TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_LOCKED OCCURS 0,
         ZFBLNO     LIKE   ZTBL-ZFBLNO.
DATA     END OF IT_LOCKED.

*-----------------------------------------------------------------------
* BDC 용 Table
*-----------------------------------------------------------------------
DATA:    BEGIN OF ZBDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.

*-----------------------------------------------------------------------
* ERROR LIST 용 Table
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_ERR_CNT         TYPE I,
       W_LOOP_CNT        TYPE I,             " Loop Count
       W_MOD             TYPE I,             " Loop Count
       SV_ZFVEN          LIKE ZTBLCST-ZFVEN,
       SV_ZFPAY          LIKE ZTBLCST-ZFPAY,
       SV_WAERS          LIKE ZTBLCST-WAERS,
       SV_ZFPOYN         LIKE ZTBL-ZFPOYN,
       SV_ZFVEN_NM(20)   TYPE C,
       SV_ZFPAY_NM(20)   TYPE C,
       SV_ZTERM          LIKE ZTBLCST-ZTERM,
       SV_MWSKZ          LIKE ZTBLCST-MWSKZ,
       SV_BUKRS          LIKE ZTBLCST-BUKRS,
       SV_ZFWERKS        LIKE ZTBLCST-ZFWERKS,
       SV_ZFOCDT         LIKE ZTBLCST-ZFOCDT,
       W_GRP_MARK(10)    TYPE C,
       SUM_ZFCAMT        LIKE ZTBLCST-ZFCAMT,
       SUM_ZFVAT         LIKE ZTBLCST-ZFVAT,
       W_POSDT           LIKE SY-DATUM,
       W_DOCDT           LIKE SY-DATUM,
       ZFFIYR            LIKE ZTBLCST-ZFFIYR,
       ZFACDO            LIKE ZTBLCST-ZFACDO,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       DISPMODE(1)       TYPE C,
       MARKFIELD         TYPE C,
       INCLUDE(8)        TYPE C,
       TEMP_WRBTR(16),
       TEMP_WMWST(16),
       TEMP_KOSTL(10),
       TEMP_AUFNR(12),
       TEMP_ZZWORK(10),
       W_LOCK_CHK(1)    TYPE  C,
       ANWORT        ,
       W_WRBTR           LIKE ZTBLCST-ZFCKAMT,
       OK-CODE           LIKE SY-UCOMM.

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

DATA : W_J_1BT001WV    LIKE J_1BT001WV.
