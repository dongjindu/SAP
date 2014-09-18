*----------------------------------------------------------------------*
*   INCLUDE ZRIMCSCSTNTOP                                              *
*----------------------------------------------------------------------*
*&  프로그램명 : 통관비용 Posting - 무환                               *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.09.08                                            *
*&  적용회사PJT: 현대전자산업 주식회사                                 *
*&---------------------------------------------------------------------*
*&   DESC.     : 무환 통관비용을 조회하여 회계처리한?
*&
*&---------------------------------------------------------------------*
TABLES : ZTBL,            " Bill of Lading
         ZTCUCLCST,       " 통관비?
         ZTIDS,           " 수입면?
         LFA1,            " Vendor Master
         ZTIMIMG00,       " 관리코?
         ZTIMIMG08,       " 관리코?
         ZTIMIMG11,       " G/R, I/V, 비용처리 Configuration
         J_1BT001WV,      " Assign Branch to Plant
         ZVT001W,
         SPOP,     " POPUP_TO_CONFIRM_... function 모듈 팝업화면 필?
         COBL,
         BSEG.

*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED OCCURS 0,
      BUKRS       LIKE ZTCUCLCST-BUKRS,
      ZFBLNO      LIKE ZTIDS-ZFBLNO,
      ZFCLSEQ     LIKE ZTIDS-ZFCLSEQ,
      ZFCSQ       LIKE ZTCUCLCST-ZFCSQ,
      ZFCSCD      LIKE ZTCUCLCST-ZFCSCD,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
      WAERS       LIKE ZTCUCLCST-WAERS,
      ZFCAMT      LIKE ZTCUCLCST-ZFCAMT,
      ZFVAT       LIKE ZTCUCLCST-ZFVAT,
      GRP_MARK(10)    TYPE   C,
END OF IT_SELECTED.
*-----------------------------------------------------------------------
* SELECT RECORD SUM
*-----------------------------------------------------------------------
DATA: BEGIN OF    IT_SELECTED_SUM OCCURS 0,
      BUKRS       LIKE ZTCUCLCST-BUKRS,
      ZFVEN       LIKE ZTCUCLCST-ZFVEN,
      ZFPAY       LIKE ZTCUCLCST-ZFPAY,
      ZTERM       LIKE ZTCUCLCST-ZTERM,
      MWSKZ       LIKE ZTCUCLCST-MWSKZ,
      ZFWERKS     LIKE ZTCUCLCST-ZFWERKS,
      WAERS       LIKE ZTCUCLCST-WAERS,
      ZFCAMT      LIKE ZTCUCLCST-ZFCAMT,
      ZFVAT       LIKE ZTCUCLCST-ZFVAT,
      GRP_MARK(10) TYPE C,
END OF IT_SELECTED_SUM.
*-----------------------------------------------------------------------
* BDC 용 Table
*-----------------------------------------------------------------------
DATA:    BEGIN OF ZBDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.
*-----------------------------------------------------------------------
* 통관 비용관련 TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTCUCST OCCURS 0.
         INCLUDE STRUCTURE ZTCUCLCST.
DATA     END OF IT_ZTCUCST.
*-----------------------------------------------------------------------
* LOCK TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_LOCKED OCCURS 0,
         ZFBLNO     LIKE   ZTIDS-ZFBLNO,
         ZFCLSEQ    LIKE   ZTIDS-ZFCLSEQ.
DATA     END OF IT_LOCKED.
*-----------------------------------------------------------------------
* ERROR 처리용 TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.
*-----------------------------------------------------------------------
* 비용관련 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTIMIMG08 OCCURS 0.
         INCLUDE STRUCTURE ZTIMIMG08.
DATA     END OF IT_ZTIMIMG08.

DATA : W_PROC_CNT        TYPE I,             " 처리건?
       W_LOOP_CNT        TYPE I,             " Loop Count
       W_ERR_CNT         TYPE I,             " Loop Count
       W_MOD             TYPE I,
       SV_BUKRS          LIKE ZTCUCLCST-BUKRS,
       SV_ZFVEN          LIKE ZTCUCLCST-ZFVEN,
       SV_ZFPAY          LIKE ZTCUCLCST-ZFPAY,
       SV_ZTERM          LIKE ZTCUCLCST-ZTERM,
       SV_MWSKZ          LIKE ZTCUCLCST-MWSKZ,
       SV_ZFWERKS        LIKE ZTCUCLCST-ZFWERKS,
       SV_WAERS          LIKE ZTCUCLCST-WAERS,
       SV_ZFOCDT         LIKE ZTCUCLCST-ZFOCDT,
       SV_KOSTL          LIKE ZTBL-KOSTL,
       W_SY_SUBRC        LIKE SY-SUBRC,
       W_GRP_MARK(10)    TYPE C,
       GRP_MARK_F(10)    TYPE C,
       GRP_MARK_T(10)    TYPE C,
       SUM_ZFCAMT        LIKE ZTCUCLCST-ZFCAMT,
       SUM_ZFVAT         LIKE ZTCUCLCST-ZFVAT,
       W_SUM_ZFVAT       LIKE ZTCUCLCST-ZFVAT,
       W_ZFCAMT          LIKE ZTCUCLCST-ZFCAMT,
       W_POSDT           LIKE SY-DATUM,
       W_DOCDT           LIKE SY-DATUM,
       ZFFIYR            LIKE ZTCUCLCST-ZFFIYR,
       ZFACDO            LIKE ZTCUCLCST-ZFACDO,
       RADIO_NONE(1)     TYPE C,
       RADIO_ALL(1)      TYPE C,
       RADIO_ERROR(1)    TYPE C,
       DISPMODE(1)       TYPE C,
       ANWORT(1)         TYPE C,
       W_LOCK_CHK        TYPE C,
       TEMP_WRBTR(16),
       TEMP_WMWST(16),
       TEMP_KOSTL(10),
       TEMP_AUFNR(12),
       TEMP_ZZWORK(10),
       W_WRBTR           LIKE ZTCUCLCST-ZFCAMT,
       W_WMWST           LIKE ZTCUCLCST-ZFVAT,
       OK-CODE           LIKE SY-UCOMM,
       INCLUDE(8)        TYPE C,
       MARKFIELD(1)      TYPE C.

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
