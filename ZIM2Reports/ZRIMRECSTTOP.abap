*----------------------------------------------------------------------*
*   INCLUDE ZRIMRECSTTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰비용 Posting                                  *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.03.23                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : 수입의뢰 비용을 조회하여 회계처리 수정.
*&
*&---------------------------------------------------------------------*
TABLES : ZTREQHD,         " 수입의?
         ZTRECST,         " 수입의뢰 비?
         LFA1,            " Vendor Master
         T000,
         TTYP,
         COBL,            "..
         ZTIMIMG00,       " 관리코드.
         ZTIMIMG08,       " 관리코드.
         ZTIMIMG11,       " G/R, I/V, 비용처리 Configuration
         BSEG,
         LFB1,
         J_1BT001WV,      " Assign Branch to Plant
         ZVT001W,
         SPOP.     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?
*>> MESSAGE 출력용.
TABLES : BAL_S_DMSG.
*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.



*-----------------------------------------------------------------------
* SELECT RECORD
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED OCCURS 0,
      BUKRS       LIKE ZTRECST-BUKRS,
      ZFCSCD      LIKE ZTRECST-ZFCSCD,
      ZFREQNO     LIKE ZTRECST-ZFREQNO,
      ZFVEN       LIKE ZTRECST-ZFVEN,
      ZFPAY       LIKE ZTRECST-ZFPAY,
      ZTERM       LIKE ZTRECST-ZTERM,
      MWSKZ       LIKE ZTRECST-MWSKZ,
      ZFCKAMT     LIKE ZTRECST-ZFCKAMT,
      ZFVAT       LIKE ZTRECST-ZFVAT,
      ZFVPR       LIKE ZTRECST-ZFVPR,
      ZFWERKS     LIKE ZTRECST-ZFWERKS,
      ZFCSQ       LIKE ZTRECST-ZFCSQ,
      ZFTRIPLE    LIKE ZTREQHD-ZFTRIPLE, " 삼국무역구분자.
      GRP_MARK(10)    TYPE   C,
END OF IT_SELECTED.

*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTIMIMG08 OCCURS 0.
         INCLUDE STRUCTURE ZTIMIMG08.
DATA     END OF IT_ZTIMIMG08.

DATA:    BEGIN OF IT_ZTRECST OCCURS 0.
         INCLUDE STRUCTURE ZTRECST.
DATA     END OF IT_ZTRECST.

DATA:    BEGIN OF IT_LOCKED OCCURS 0,
         ZFREQNO     LIKE   ZTREQHD-ZFREQNO.
DATA     END OF IT_LOCKED.


DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_ERR_CNT         TYPE I,
       W_LOOP_CNT        TYPE I,             " Loop Count
       INCLUDE(8)        TYPE C,             "
       SV_ZFREQNO        LIKE ZTRECST-ZFREQNO,
       SV_ZFCSQ          LIKE ZTRECST-ZFCSQ,
       SV_ZFVEN          LIKE ZTRECST-ZFVEN,
       SV_BUKRS          LIKE ZTRECST-BUKRS,
       SV_ZFCSCD         LIKE ZTRECST-ZFCSCD,
       SV_ZFVEN_NM(20)   TYPE C,
       SV_ZFPAY_NM(20)   TYPE C,
       SV_ZFPAY          LIKE ZTRECST-ZFPAY,
       SV_ZTERM          LIKE ZTRECST-ZTERM,
       SV_MWSKZ          LIKE ZTRECST-MWSKZ,
       SV_ZFTRIPLE       LIKE ZTREQHD-ZFTRIPLE,
       SV_ZFVPR          LIKE ZTRECST-ZFVPR,
       W_GRP_MARK(10)    TYPE C,
       SUM_ZFCKAMT       LIKE ZTRECST-ZFCKAMT,
       SUM_ZFVAT         LIKE ZTRECST-ZFVAT,
       TMP_ZFVAT         LIKE ZTRECST-ZFVAT,
       SUM_ZFVPR         LIKE ZTRECST-ZFVPR,
       W_POSDT           LIKE SY-DATUM,
       W_DOCDT           LIKE SY-DATUM,
       ZFFIYR            LIKE ZTRECST-ZFFIYR,
       ZFACDO            LIKE ZTRECST-ZFACDO,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       W_LOCK_CHK(1)     TYPE C,
       TEMP_WRBTR(16),
       TEMP_WMWST(16),
       TMP_WRBTR(22),
       TMP_WMWST(22),
       ANWORT,
       W_TEXT_AMOUNT(30)      TYPE  C,
       W_WRBTR           LIKE ZTRECST-ZFCKAMT,
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
       W_BUTTON_ANSWER   TYPE C,
       W_MOD             TYPE I.

DATA : W_J_1BT001WV    LIKE J_1BT001WV.
DATA  SV_ZFWERKS       LIKE ZTRECST-ZFWERKS.
