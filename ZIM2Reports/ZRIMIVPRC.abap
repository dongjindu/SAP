*&---------------------------------------------------------------------*
*& Report  ZRIMIVPRC                                                   *
*&---------------------------------------------------------------------*
*&  Program Name : G/R Processing                                      *
*&  Created by   : Kang. SukBond. INFOLINK Ltd.                        *
*&  Created on   : 2001.03.02                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMIVPRC   MESSAGE-ID ZIM
                    LINE-SIZE 140
                    NO STANDARD PAGE HEADING.

INCLUDE : <ICON>,
           ZRIMBDCCOM.
*>>>>> LIV BAPI FUNCTION.
TABLES : BAPI_INCINV_CREATE_HEADER,
         BAPI_INCINV_CREATE_ITEM,
         BAPI_INCINV_CREATE_TAX,
         BAPIRET2.

TABLES : BAPI2017_GM_HEAD_01,
         BAPI2017_GM_CODE,
         BAPI2017_GM_HEAD_RET,
         BAPI2017_GM_ITEM_CREATE,
         BAPI2017_GM_SERIALNUMBER,
         BAPIMEPOHEADER,
         BAPIMEPOHEADERX.

DATA : INVOICEDOCNUMBER  LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR        LIKE    BAPI_INCINV_FLD-FISC_YEAR,
       MATERIALDOCUMENT  TYPE    BAPI2017_GM_HEAD_RET-MAT_DOC,
       MATDOCUMENTYEAR   TYPE    BAPI2017_GM_HEAD_RET-DOC_YEAR,
       GR_DOC            TYPE    BAPI2017_GM_HEAD_RET,
       P_INVOICE,
       P_CREDITMENO.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF XRETURN OCCURS 0.   ">> RETURN.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF XRETURN.

*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
CONTROLS: TC_0050    TYPE TABLEVIEW USING SCREEN 0050,
          TC_0060    TYPE TABLEVIEW USING SCREEN 0060.

*>> MESSAGE Display.
TABLES : BAL_S_DMSG.
TABLES : T001W,
         ZTIVHST,
         ZSIVHST,
        *ZSIVHST,
        *ZTIVHST,
         ZTCIVIT,
         ZTCIVHD,
         ZTIVHSTIT,
         ZSIVHSTIT,
         ZTCIVHST,
         ZTIVHST1,
         ZTIDS,
         EKKO.

*>> TEMP TABLE
DATA : IT_BLIV       LIKE  ZVBL_IV    OCCURS 0 WITH HEADER LINE.
DATA : IT_ZTIVHSTIT  LIKE  ZTIVHSTIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVHSTIT  LIKE  ZSIVHSTIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVHST    LIKE  ZSIVHST    OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVIT     LIKE  ZSIVIT     OCCURS 0 WITH HEADER LINE.

*>>> ERROR.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C,
       ZFIVNO       LIKE ZTIV-ZFIVNO.
DATA : END OF IT_ERR_LIST.

*>>> MAIN INTERNAL TABLE.
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       BUKRS           LIKE   ZTIV-BUKRS,          " Company Code
       ZFBLNO          LIKE   ZTIV-ZFBLNO,         " B/L Document No.
       ZFCLSEQ         LIKE   ZTCUCLIV-ZFCLSEQ,    " Clearance Seq.
       ZFHBLNO         LIKE   ZTBL-ZFHBLNO,        " House B/L No.
       ZFBLDT          LIKE   ZTBL-ZFBLDT,         " B/L Issuing Date
       ZFETA           LIKE   ZTBL-ZFETA,          " ETA
       ZFMBLNO         LIKE   ZTBL-ZFMBLNO,        " Master B/L No
       EKORG           LIKE   ZTBL-EKORG,          " Purchasing Org.
       EKGRP           LIKE   ZTBL-EKGRP,          " Purchasing Group.
       ZFMSNO          LIKE   ZTBL-ZFMSNO,         " Mothership Code
       ZFMSNM          LIKE   ZTMSHD-ZFMSNM,       " Mothership Name
       ZFIVNO          LIKE   ZTIV-ZFIVNO,         " Clearance Doc. No.
       ZFCCDT          LIKE   ZTIV-ZFCCDT,         " Clearance Request
       ZFPHVN          LIKE   ZTIV-ZFPHVN,         " Phanda Vendor
       PVNAME1         LIKE   LFA1-NAME1,          " Vendor Name
       ZFPOYN          LIKE   ZTIV-ZFPOYN,         " Monetary Yes/No
       ZFPOYN_NM(05),                              " Monetary Text
       ZFCLCD          LIKE   ZTIV-ZFCLCD,         " Clearance Type
       ZFCLCD_NM(10),                              " Clearance Type Text
       ZFCUST          LIKE   ZTIV-ZFCUST,         " Clearance Status
       ZFCUST_NM(24),                              " Status Check
       ZFCDST          LIKE   ZTIV-ZFCDST,         " Expense Distribu.
       ZFCDST_NM(11),                              " Expense TEXT.
       ZFGRST          LIKE   ZTIV-ZFGRST,         " Good Receipt Statu.
       ZFGRST_NM(19),                              " G/R STATUS TEXT.
       ZFCIVST         LIKE   ZTIV-ZFCIVST,        " Import Expense I/V
       ZFCIVST_NM(10),                             " CIV STATUS TEXT.
       ZFPONMA         LIKE   ZTIV-ZFPONMA,        " Non-Monetary Yes/N.
       CDAT            LIKE   ZTIV-CDAT,           " Created on
       LIFNR           LIKE   ZTIV-LIFNR,          " Vendor
       NAME1           LIKE   LFA1-NAME1,          " Vendor Name.
       WERKS           LIKE   ZTIVIT-WERKS,        " PLANT
       LGORT           LIKE   ZTIVIT-LGORT,        " Storage Location.
       LGOBE           LIKE   T001L-LGOBE,         " Location Text
       ZFIDRNO         LIKE   ZTIDS-ZFIDRNO,       " Entry Number
       NAME2           LIKE   T001W-NAME1,         " PLANT Name
       END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables & Variance Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMIVPRCTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*>> RANGE for Selection.
RANGES : R_ZFPOYN FOR  ZTIV-ZFPOYN  OCCURS 2,    ">Monetary Status.
         R_ZFCUST FOR  ZTIV-ZFCUST  OCCURS 5,    ">Clearance Stataus
         R_ZFCDST FOR  ZTIV-ZFCDST  OCCURS 3,    ">Import Expense status
         R_ZFGRST FOR  ZTIV-ZFGRST  OCCURS 3,    ">G/R status
         R_ZFCIVST FOR ZTIV-ZFCIVST OCCURS 2,    ">Import expe I/V.
         R_ZFPONMA FOR ZTIV-ZFPONMA OCCURS 2.    ">Non-Monetary Yes/No.

DATA : W_TEXT70(70),
       INCLUDE(8)        TYPE C,
       W_STATUS_CHK      TYPE C VALUE 'D',
       W_MOD             TYPE I,
       OPTION(1)         TYPE C,
       ANTWORT(1)        TYPE C,
       CANCEL_OPTION     TYPE C,
       TEXTLEN           TYPE I,
       W_OK_CODE1        LIKE SY-UCOMM,
       P_BUKRS           LIKE ZTIMIMG00-ZFBUKRS.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS  FOR ZTIV-BUKRS,
                S_ZFCLCD FOR ZTIV-ZFCLCD,       ">Clearance Type.
                S_YSDST  FOR ZTIV-ZFYSDST,      ">Send/Receive Status.
                S_CDAT   FOR ZTIV-CDAT,         ">Created on
                S_CCDT   FOR ZTIV-ZFCCDT,       ">Clearance Request Date
                S_HBLNO  FOR ZTBL-ZFHBLNO,      ">House B/L No
                S_BLNO   FOR ZTIV-ZFBLNO,       ">B/L Document No.
                S_ETA    FOR ZTBL-ZFETA,        ">ETA
                S_IVNO   FOR ZTIV-ZFIVNO,       ">Clearance Request No
                S_MBLNO  FOR ZTBL-ZFMBLNO,      ">Master B/L No
                S_IDRNO  FOR ZTIDSUS-ZFENTNO,   ">Entry Number
                S_LIFNR  FOR ZTIV-LIFNR,        ">Vendor
                S_EKORG  FOR ZTBL-EKORG,        ">Purchasing Group
                S_EKGRP  FOR ZTBL-EKGRP,        ">Purchasing Org.
                S_WERKS  FOR ZTIVIT-WERKS.      ">Plant
PARAMETERS : P_ITEM      AS CHECKBOX DEFAULT 'X'. ">Display Item

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* Monetary
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(21) TEXT-003, POSITION 1.
SELECTION-SCREEN : COMMENT 28(8) TEXT-031, POSITION 38.
PARAMETERS : P_POY    AS CHECKBOX.              " Monetary
SELECTION-SCREEN : COMMENT 42(8) TEXT-032, POSITION 51.
PARAMETERS : P_PON    AS CHECKBOX.              " Non-Monetary
SELECTION-SCREEN : COMMENT 55(8) TEXT-033, POSITION 65.
PARAMETERS : P_POM    AS CHECKBOX.              " Combine.
SELECTION-SCREEN END OF LINE.

* Clearance Status.
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(20) TEXT-007, POSITION 1.
SELECTION-SCREEN : COMMENT 25(11) TEXT-075, POSITION 38.
PARAMETERS : P_CUN    AS CHECKBOX.              " Not Object.
SELECTION-SCREEN : COMMENT 42(8) TEXT-071, POSITION 51.
PARAMETERS : P_CUY    AS CHECKBOX.              " Completed Clearance
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 28(8) TEXT-072, POSITION 38.
PARAMETERS : P_CU1    AS CHECKBOX.              " Object to create.
SELECTION-SCREEN : COMMENT 42(8) TEXT-073, POSITION 51.
PARAMETERS : P_CU2    AS CHECKBOX.              " Object to declare.
SELECTION-SCREEN : COMMENT 55(8) TEXT-074, POSITION 65.
PARAMETERS : P_CU3    AS CHECKBOX.              " Declaring.
SELECTION-SCREEN END OF LINE.
* G/R Statys.
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(20) TEXT-006, POSITION 1.
SELECTION-SCREEN : COMMENT 34(2) TEXT-061, POSITION 38.
PARAMETERS : P_GRN    AS CHECKBOX.              " No
SELECTION-SCREEN : COMMENT 45(3) TEXT-062, POSITION 51.
PARAMETERS : P_GRY    AS CHECKBOX.              " Yes
SELECTION-SCREEN : COMMENT 56(8) TEXT-115, POSITION 65.
PARAMETERS : P_GRP    AS CHECKBOX.              " Yes
SELECTION-SCREEN : COMMENT 68(8) TEXT-114, POSITION 77.
PARAMETERS : P_GRX    AS CHECKBOX.              " Not Object.
SELECTION-SCREEN END OF LINE.

* Import Expense distribution Yes/No.
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(20) TEXT-004, POSITION 1.
SELECTION-SCREEN : COMMENT 34(2) TEXT-041,  POSITION 38.
PARAMETERS : P_CDN    AS CHECKBOX.              " No
SELECTION-SCREEN : COMMENT 45(3) TEXT-042,  POSITION 51.
PARAMETERS : P_CDY    AS CHECKBOX.              " Yes
SELECTION-SCREEN : COMMENT 56(8) TEXT-043,  POSITION 65.
PARAMETERS : P_CDX    AS CHECKBOX.              " Not Object
SELECTION-SCREEN END OF LINE.

* Import Expense I/V Status
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(31) TEXT-011, POSITION 1.
SELECTION-SCREEN : COMMENT 38(2) TEXT-111, POSITION 41.
PARAMETERS : P_CIVN   AS CHECKBOX.              " No
SELECTION-SCREEN : COMMENT 49(3) TEXT-112, POSITION 53.
PARAMETERS : P_CIVY   AS CHECKBOX.              " Yes
SELECTION-SCREEN : COMMENT 56(8) TEXT-113,  POSITION 65.
PARAMETERS : P_CIVX   AS CHECKBOX.              " Not Object.
SELECTION-SCREEN END OF LINE.

* Non-Monetary Yes/No.
SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-010, POSITION 1.
SELECTION-SCREEN : COMMENT 34(2) TEXT-101, POSITION 38.
PARAMETERS : P_POMN    AS CHECKBOX.              " No
SELECTION-SCREEN : COMMENT 45(3) TEXT-102, POSITION 51.
PARAMETERS : P_POMY    AS CHECKBOX.              " Yes
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.

*-----------------------------------------------------------------------
* PBO
*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  PERFORM  P2000_SCREEN_FIELD_SET.

* PARAMETER Initial Value Setting
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_INIT_VALUE.

* Title Text Write
TOP-OF-PAGE.
  CASE INCLUDE.
    WHEN 'POPU'.
    WHEN OTHERS.
      PERFORM   P3000_TITLE_WRITE.
  ENDCASE.

*-----------------------------------------------------------------------
* START OF SELECTION Case
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Data Read
  PERFORM   P1000_READ_BASIC_DATA      USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Data Write
  PERFORM   P3000_DATA_WRITE          USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.
      W_FIELD_NM = 'ZFREQDT'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

    WHEN 'COM1' OR 'GRBD' OR 'IVBD' OR         " CREATE.
         'COM2' OR 'GRB2' OR 'IVB2' OR 'IVB3'. " CANCEL.
      W_OK_CODE1 = SY-UCOMM.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.

        PERFORM P4000_COST_DISTR         USING W_ERR_CHK.

        IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
          EXIT.
        ENDIF.

        DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
        IF W_LINE GT 0.
          INCLUDE = 'POPU'.
          CALL SCREEN 0100 STARTING AT  05   3
                           ENDING   AT  100 12.
          CLEAR : INCLUDE.
        ENDIF.
        PERFORM   P1000_READ_BASIC_DATA  USING W_ERR_CHK.
        IF W_PROC_CNT EQ 0.
          MESSAGE S916.
        ELSE.
          CASE W_OK_CODE1.
            WHEN 'COM1'.         ">배용배부.
              MESSAGE S747 WITH W_PROC_CNT.
            WHEN 'GRBD'.         ">G/R
              MESSAGE S803 WITH W_PROC_CNT.
            WHEN 'IVBD'.         ">제비용 IV
              MESSAGE S757 WITH W_PROC_CNT.
            WHEN 'GRB2'.
              MESSAGE S915 WITH W_PROC_CNT.
          ENDCASE.
        ENDIF.
        IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
        PERFORM RESET_LIST.

      ELSEIF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ELSE.
        MESSAGE S965.   EXIT.
      ENDIF.
    WHEN 'AMTI'.                    " 통관요청 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_IV USING IT_SELECTED-ZFIVNO.
        CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
    WHEN 'STCG'.                    " 통관요청 상태변경.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_IV USING IT_SELECTED-ZFIVNO.
        CALL TRANSACTION 'ZIM34' AND SKIP FIRST SCREEN.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
    WHEN 'DSBL'.                    " B/L 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_BL USING IT_SELECTED-ZFIVNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE S965.
      ENDIF.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
    WHEN 'ZIM63' OR 'ZIM76'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_CC_DOC USING IT_SELECTED-ZFIVNO
                                        SY-UCOMM.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE S965.
      ENDIF.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
      PERFORM   P1000_READ_BASIC_DATA  USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.                " 종?
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.

*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.
  CLEAR : IT_ERR_LIST.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'POPU'.
      IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
        MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
                NUMBER IT_ERR_LIST-MSGNR
                WITH   IT_ERR_LIST-MSGV1
                       IT_ERR_LIST-MSGV2
                       IT_ERR_LIST-MSGV3
                       IT_ERR_LIST-MSGV4.
      ENDIF.
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_VALUE
*&---------------------------------------------------------------------*
FORM P2000_SET_INIT_VALUE.
*>> IMG MASTER SELECT.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
    MESSAGE E961.
  ENDIF.
  IF ZTIMIMG00-ZFCSTMD IS INITIAL.
    MESSAGE E986.
  ENDIF.
*>> 비용코?
  SELECT SINGLE * FROM ZTIMIMG11.
  IF SY-SUBRC NE 0.
    MESSAGE E961.
  ENDIF.
 IF SY-LANGU EQ '3'.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    SET TITLEBAR 'POPU' WITH '수입입고 처리'.
  ELSEIF ZTIMIMG00-ZFCSTMD EQ 'I'.
    SET TITLEBAR 'ZIM56' WITH '[비용배부, 입고처리, 제비용처리]'.
  ENDIF.
 ELSE.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    SET TITLEBAR 'POPU' WITH 'G/R Processing'.
  ELSEIF ZTIMIMG00-ZFCSTMD EQ 'I'.
    SET TITLEBAR 'ZIM56' WITH '[Expense disr, G/R, Expense I/V]'.
  ENDIF.
 ENDIF.
  MOVE 'X'   TO : P_POY,          ">유환.
                  P_POM,          ">Combine.
                 P_CUY, P_CUN,    ">통관완료/미통관 대상.
          P_CDX, P_CDY, P_CDN,    ">비용배부 상태.
          P_GRP, P_GRN,           ">입고상태.
                 P_CIVN,          ">제비용상태.
                 P_POMN.          ">무환수출입여부.

ENDFORM.                    " P2000_SET_INIT_VALUE

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

 CASE SY-LANGU.
  WHEN '3'.
   PERFORM P3000_KOREAN_TITLE.
  WHEN OTHERS.
   PERFORM P3000_ENGLISH_TITLE.
 ENDCASE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.
*-----------------------------------------------------------------------
* Menu Statsu Function을 Inactive하기 위한 Internal Table
*-----------------------------------------------------------------------
  DATA: BEGIN OF IT_EXCL OCCURS 20,
        FCODE    LIKE RSMPE-FUNC.
  DATA: END   OF IT_EXCL.

  CLEAR : INCLUDE.
  MOVE 'N' TO W_ERR_CHK.

  REFRESH : IT_EXCL.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    MOVE 'COM1'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">부대비용.
    MOVE 'COM2'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">부대비용취소.
    MOVE 'IVBD'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">제비용처리.
    MOVE 'IVB2'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">대변메모.
    MOVE 'IVB3'  TO IT_EXCL-FCODE.    APPEND IT_EXCL. ">제비용취소.
  ENDIF.

 IF SY-LANGU EQ '3'.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    SET TITLEBAR 'POPU' WITH '수입입고 처리'.
  ELSEIF ZTIMIMG00-ZFCSTMD EQ 'I'.
    SET TITLEBAR 'ZIM56' WITH '[비용배부, 입고처리, 제비용처리]'.
  ENDIF.
 ELSE.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    SET TITLEBAR 'POPU' WITH 'G/R Processing'.
  ELSEIF ZTIMIMG00-ZFCSTMD EQ 'I'.
    SET TITLEBAR 'ZIM56' WITH '[Expense disr, G/R, Expense I/V]'.
  ENDIF.
 ENDIF.

  SET PF-STATUS 'ZIM56' EXCLUDING IT_EXCL.

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.

  ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-ZFHBLNO  NO-GAP,
       SY-VLINE NO-GAP,               " House B/L No
       IT_TAB-ZFBLNO   NO-GAP,
       SY-VLINE NO-GAP,               " B/L 관리번호.
*       IT_TAB-ZFETA    NO-GAP,
       IT_TAB-ZFCLSEQ    NO-GAP,
       SY-VLINE NO-GAP,               " 순번( 도착일 )
       IT_TAB-ZFBLDT   NO-GAP,
       SY-VLINE NO-GAP,               " B/L 발행일자.
       IT_TAB-WERKS        NO-GAP,
       '-'                 NO-GAP,
       IT_TAB-NAME2(19)    NO-GAP,
       SY-VLINE NO-GAP,               " 플랜?
       IT_TAB-EKORG    NO-GAP,
       SY-VLINE NO-GAP,               " 구매조직.
       IT_TAB-EKGRP    NO-GAP,
       SY-VLINE NO-GAP,               " 구매그룹.
       IT_TAB-LIFNR    NO-GAP,
       SY-VLINE NO-GAP,               " VENDOR.
       IT_TAB-NAME1    NO-GAP,
   140 SY-VLINE NO-GAP.               " VENDOR NAME.
* Hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE,
  8 IT_TAB-ZFIVNO,
  SY-VLINE NO-GAP,               " 통관요청 관리번호.
  IT_TAB-ZFCCDT    NO-GAP,
  SY-VLINE NO-GAP,               " 통관요청 일자.
  IT_TAB-CDAT      NO-GAP,
  SY-VLINE NO-GAP,               " 통관요청 생성일자.
  IT_TAB-ZFPOYN_NM NO-GAP,
  SY-VLINE NO-GAP,               " 유무환 구분.
  IT_TAB-ZFCLCD_NM NO-GAP,
  SY-VLINE NO-GAP.               " 통관구분.

*>> 통관 상태.
  IF IT_TAB-ZFCUST EQ 'Y' OR IT_TAB-ZFCUST EQ 'N'.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_TAB-ZFCUST_NM NO-GAP.    " 통관상태.
  WRITE : SY-VLINE NO-GAP.
*>> 비용배부 상태.
*  IF ZTIMIMG00-ZFCSTMD EQ 'S'.
*    WRITE: (12) SPACE            NO-GAP, SY-VLINE NO-GAP.
*  ELSE.
*    IF IT_TAB-ZFCDST EQ 'Y' OR IT_TAB-ZFCDST EQ 'X'.
*      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
*    ELSE.
*      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
*    ENDIF.
*    WRITE : SY-VLINE NO-GAP,
*            IT_TAB-ZFCDST_NM NO-GAP,
*        SY-VLINE NO-GAP.               " 배용배부상태.
*  ENDIF.

*>> 입고 상태.
  IF IT_TAB-ZFGRST EQ 'Y'.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  ELSEIF IT_TAB-ZFGRST EQ 'N'.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ELSEIF IT_TAB-ZFGRST EQ 'P'.
    FORMAT COLOR COL_GROUP    INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL   INTENSIFIED OFF.
  ENDIF.
  WRITE : IT_TAB-ZFGRST_NM NO-GAP.   " Good Receipt 상태.
  WRITE : SY-VLINE NO-GAP.
*>> 제비용 상태.
*  IF ZTIMIMG00-ZFCSTMD EQ 'S'.
*    WRITE: (11) SPACE            NO-GAP, SY-VLINE NO-GAP.
*  ELSE.
*    IF IT_TAB-ZFCIVST EQ 'Y' OR IT_TAB-ZFCIVST EQ 'X'.
*      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
*    ELSE.
*      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
*    ENDIF.
*    WRITE : SY-VLINE NO-GAP,
*            IT_TAB-ZFCIVST_NM NO-GAP,
*        SY-VLINE NO-GAP.               " 제비용 상태.
*  ENDIF.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : IT_TAB-ZFIDRNO NO-GAP COLOR COL_NORMAL INTENSIFIED OFF,
      140 SY-VLINE NO-GAP.           " 수입면허번호.

  FORMAT RESET.
* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

*IT_TAB-ZFIVNO
  IF P_ITEM EQ 'X'.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHSTIT
             FROM  ZTIVIT
             WHERE ZFIVNO EQ IT_TAB-ZFIVNO
             AND   UMSON  EQ 'X'
             AND   ZFPOTY NE 'S'.
    IF SY-SUBRC NE 0.
      MESSAGE S549 WITH ZTIV-ZFIVNO.
    ENDIF.
*>
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

    LOOP AT IT_ZSIVHSTIT.
      W_TABIX = SY-TABIX.
      MOVE:  IT_ZSIVHSTIT-GRMENGE   TO   IT_ZSIVHSTIT-CCMENGE,
             ZTIVHST-ZFIVHST        TO   IT_ZSIVHSTIT-ZFIVHST.

      IT_ZSIVHSTIT-GRMENGE = IT_ZSIVHSTIT-CCMENGE
                           - IT_ZSIVHSTIT-GRTOTMN.

      IF IT_ZSIVHSTIT-GRMENGE GT 0.
        IT_ZSIVHSTIT-UMSON = 'X'.
      ELSE.
        CLEAR : IT_ZSIVHSTIT-UMSON.
      ENDIF.
      IT_ZSIVHSTIT-ZFGRST = 'Y'.

      CLEAR :T001W.
      IF NOT IT_ZSIVHSTIT-WERKS IS INITIAL.
        SELECT SINGLE * FROM T001W
               WHERE WERKS EQ IT_ZSIVHSTIT-WERKS.
      ENDIF.

      MODIFY IT_ZSIVHSTIT INDEX W_TABIX.
      WRITE : /  SY-VLINE, ' ', SY-VLINE,
              (22) IT_ZSIVHSTIT-MATNR, SY-VLINE,
              (25) IT_ZSIVHSTIT-TXZ01, SY-VLINE,
              (18) IT_ZSIVHSTIT-CCMENGE UNIT IT_ZSIVHSTIT-MEINS,
              (03) IT_ZSIVHSTIT-MEINS  ,
                   SY-VLINE,
              (17) IT_ZSIVHSTIT-GRMENGE UNIT IT_ZSIVHSTIT-MEINS,
                   SY-VLINE,
              (04) IT_ZSIVHSTIT-WERKS,
              (29) T001W-NAME1,
                   SY-VLINE.

    ENDLOOP.
  ENDIF.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / 'Total', W_COUNT, 'cases'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_KOREAN_TITLE.
  ELSE.
     PERFORM   P3000_ENGLISH_TITLE.
  ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_BASIC_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_BASIC_DATA  USING    W_ERR_CHK.
  REFRESH :IT_TAB, IT_BLIV.

  W_ERR_CHK = 'N'.
  SELECT H~ZFBLNO  I~ZFHBLNO  I~ZFBLDT  I~ZFETA  I~ZFMBLNO
         I~EKORG   I~EKGRP    I~ZFMSNO  H~ZFIVNO H~ZFCCDT
         H~ZFPHVN  H~ZFPOYN   H~ZFCLCD  H~ZFCUST H~ZFCDST
         H~ZFGRST  H~ZFCIVST  H~ZFPONMA H~CDAT   H~LIFNR
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM   ZTIV  AS  H  LEFT OUTER JOIN  ZTBL AS I
  ON     H~ZFBLNO     EQ   I~ZFBLNO
  WHERE  H~BUKRS      IN   S_BUKRS
  AND    H~ZFIVNO     IN   S_IVNO
  AND    H~ZFCCDT     IN   S_CCDT
  AND    H~CDAT       IN   S_CDAT
  AND    H~ZFBLNO     IN   S_BLNO
  AND    H~LIFNR      IN   S_LIFNR
  AND    H~ZFYSDST    IN   S_YSDST
  AND    H~ZFCLCD     IN   S_ZFCLCD
  AND    H~ZFPOYN     IN   R_ZFPOYN    ">유환여부.
  AND    H~ZFCUST     IN   R_ZFCUST    ">통관상태.
  AND    H~ZFCDST     IN   R_ZFCDST    ">부대비용 배부여부.
  AND    H~ZFGRST     IN   R_ZFGRST    ">G/R 상태.
  AND    H~ZFCIVST    IN   R_ZFCIVST   ">수입제비용 I/V 상태.
  AND    H~ZFPONMA    IN   R_ZFPONMA.   ">무환수출입여부.
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S966.   EXIT.
  ENDIF.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
*>> 플랜트가 입력되었을 경우....
    IF NOT S_WERKS[] IS INITIAL.
      CLEAR: W_COUNT.
      SELECT COUNT( * ) INTO W_COUNT
             FROM   ZTIVIT
             WHERE  ZFIVNO   EQ    IT_TAB-ZFIVNO
             AND    WERKS    IN    S_WERKS
             AND    ZFPOTY   NE    'S'.
      IF W_COUNT LE  0.
        DELETE  IT_TAB  INDEX  W_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF NOT ( S_HBLNO[] IS INITIAL AND S_MBLNO[] IS INITIAL AND
             S_ETA[]   IS INITIAL AND S_EKORG[] IS INITIAL AND
             S_EKGRP[] IS INITIAL ).
      IF IT_TAB-ZFBLNO IS INITIAL.
        DELETE  IT_TAB  INDEX  W_TABIX.
        CONTINUE.
      ELSE.
*>> 조건여부 CHECK!
        CLEAR  W_COUNT.
        SELECT COUNT( * )  INTO  W_COUNT
        FROM   ZTBL
        WHERE  ZFHBLNO     IN    S_HBLNO
        AND    ZFMBLNO     IN    S_MBLNO
        AND    ZFETA       IN    S_ETA
        AND    EKORG       IN    S_EKORG
        AND    EKGRP       IN    S_EKGRP
        AND    ZFBLNO      EQ    IT_TAB-ZFBLNO
        AND    ZFPOTY      NE    'S'.

        IF W_COUNT LE  0.
          DELETE  IT_TAB  INDEX  W_TABIX.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    "---------------------------------------
    " P/O Document Category 'KD' => Delete.
    "---------------------------------------
    SELECT * FROM  ZTIVIT UP TO 1 ROWS
             WHERE ZFIVNO EQ IT_TAB-ZFIVNO.
    ENDSELECT.

    SELECT SINGLE * FROM EKKO
    WHERE  EBELN    EQ   ZTIVIT-EBELN.
    IF EKKO-BSART EQ 'KD'.
       DELETE  IT_TAB  INDEX  W_TABIX.
       CONTINUE.
    ENDIF.
    "-----------------------------------------

*>> 플랜트.
    SELECT SINGLE WERKS LGORT
           INTO (IT_TAB-WERKS, IT_TAB-LGORT)
           FROM  ZTIVIT
           WHERE ZFIVNO  EQ IT_TAB-ZFIVNO
           AND   ZFIVDNO EQ '00010'.
    IF NOT IT_TAB-WERKS IS INITIAL.
      SELECT SINGLE NAME1 INTO IT_TAB-NAME2
             FROM   T001W
             WHERE  WERKS EQ IT_TAB-WERKS.
      IF NOT IT_TAB-LGORT IS INITIAL.
        SELECT SINGLE LGOBE INTO IT_TAB-LGOBE
               FROM T001L
               WHERE WERKS  EQ IT_TAB-WERKS
               AND   LGORT  EQ IT_TAB-LGORT.
      ENDIF.
    ENDIF.

*>> VENDOR가 바뀔 때 마다..
    IF IT_TAB-LIFNR IS INITIAL.
      CLEAR : W_LFA1.
    ENDIF.
    ON CHANGE OF IT_TAB-LIFNR.
      SELECT SINGLE * INTO W_LFA1 FROM LFA1
             WHERE LIFNR = IT_TAB-LIFNR.
    ENDON.
    MOVE W_LFA1-NAME1    TO    IT_TAB-NAME1.
*>> 가 VENDOR가 바뀔 때 마다..
    IF IT_TAB-ZFPHVN IS INITIAL.
      CLEAR : W_LFA12.
    ENDIF.
    ON CHANGE OF IT_TAB-ZFPHVN.
      SELECT SINGLE * INTO W_LFA12 FROM LFA1
             WHERE LIFNR = IT_TAB-ZFPHVN.
    ENDON.
    MOVE W_LFA12-NAME1    TO    IT_TAB-PVNAME1.

    IF SY-LANGU EQ '3'.
       PERFORM P3000_KOREAN_TEXT.
    ELSE.
       PERFORM P3000_ENGLISH_TEXT.
    ENDIF.

    SELECT SINGLE ZFENTNO INTO IT_TAB-ZFIDRNO
           FROM   ZTIDSUS
            WHERE  ZFIVNO  EQ IT_TAB-ZFIVNO
              AND  ZFENTNO IN S_IDRNO.
    IF SY-SUBRC NE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.
    MODIFY  IT_TAB INDEX W_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES  W_LINE.
  IF W_LINE EQ 0.
    W_ERR_CHK = 'Y'.   MESSAGE S966.   EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IV
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IV USING    P_ZFIVNO.

  SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
  SET PARAMETER ID 'ZPBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPIVNO'   FIELD P_ZFIVNO.

ENDFORM.                    " P2000_SHOW_IV
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFIVNO  LIKE ZTIV-ZFIVNO.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFIVNO   TO ZFIVNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : W_LIST_INDEX    TO INDEX,
             IT_TAB-ZFIVNO   TO IT_SELECTED-ZFIVNO.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P4000_COST_DISTR
*&---------------------------------------------------------------------*
FORM P4000_COST_DISTR USING   W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR : W_PROC_CNT, W_ERR_CNT.
  REFRESH : IT_ERR_LIST.
  CASE W_OK_CODE1.
    WHEN 'GRBD'.                            ">입고처리.
      PERFORM P4000_GET_GR_INIVAL.
      IF ANTWORT NE 'Y'.
        EXIT.
      ENDIF.
    WHEN 'GRB2'.                            ">입고취소.
      PERFORM P4000_GET_GR_INIT_VALUE.
      IF ANTWORT NE 'Y'.
        EXIT.
      ENDIF.
    WHEN 'IVBD' OR 'IVB2'.                  ">제비용처리.(대변메모)
      PERFORM P4000_GET_IV_INIVAL.
      IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
        EXIT.
      ENDIF.
    WHEN 'IVB3'.                            ">제비용 취소.
      PERFORM P4000_GET_IV_CANCEL_INIT.
      IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
        EXIT.
      ENDIF.
    WHEN 'COM1' OR 'COM2'.                  " 비용배부/취소.
      IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
        EXIT.
      ENDIF.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  LOOP AT IT_SELECTED.
*-----------------------------------------------------------------------
*>> LOCK OBJECT.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIV'
         EXPORTING
              ZFIVNO = IT_SELECTED-ZFIVNO
         EXCEPTIONS
              OTHERS = 1.

    IF SY-SUBRC NE 0.         "> ERROR 발생시..
      PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
      ADD    1    TO    W_ERR_CNT.
      CONTINUE.
    ENDIF.

    IF W_OK_CODE1 EQ 'COM1'.  ">비용배부.
*>> 입고 BAPIs FUCNTION
      CALL FUNCTION 'ZIM_IMPORT_COST_DIVISION'
           EXPORTING
                ZFIVNO    = IT_SELECTED-ZFIVNO
           EXCEPTIONS
                DIV_ERROR = 4.

      IF SY-SUBRC NE 0.           ">> 오류 발생시...
        PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
        CONTINUE.
      ENDIF.
    ENDIF.

    IF W_OK_CODE1 EQ 'COM2'.  ">비용배부취소.
*>> 입고 BAPIs FUCNTION
      CALL FUNCTION 'ZIM_COST_DIVISION_CANCEL'
           EXPORTING
                ZFIVNO    = IT_SELECTED-ZFIVNO
           EXCEPTIONS
                DIV_ERROR = 4.

      IF SY-SUBRC NE 0.           ">> 오류 발생시...
        PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
        CONTINUE.
      ENDIF.
    ENDIF.

    IF W_OK_CODE1 EQ 'IVBD' OR W_OK_CODE1 EQ 'IVB2'.  ">제비용처리.
      IF W_OK_CODE1 EQ 'IVBD'.
        P_INVOICE = 'X'.
        CLEAR : P_CREDITMENO.
      ELSE.
        CLEAR : P_INVOICE.
        P_CREDITMENO = 'X'.
      ENDIF.
      CALL FUNCTION 'ZIM_BAPI_COST_INVOICEVERIFY'
           EXPORTING
               P_ZFIVNO            =   IT_SELECTED-ZFIVNO
               P_CHG_MODE          =   'X'
               P_DOC_TYPE          =   'RE'
               I_INVOICE           =   P_INVOICE
               I_CREDITMEMO        =   P_CREDITMENO
               P_BLDAT             =   W_ZFIVDDT
               P_BUDAT             =   W_ZFIVPDT
*            I_INVOICE           =   SPACE
*            I_CREDITMEMO        =   'X'
           IMPORTING
               INVOICEDOCNUMBER    =   INVOICEDOCNUMBER
               FISCALYEAR          =   FISCALYEAR
           TABLES
               RETURN              =   RETURN
           EXCEPTIONS
               OTHERS              =   4.
      IF SY-SUBRC NE 0.           ">> 오류 발생시...
        IF RETURN[] IS INITIAL.
          PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
        ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
        CONTINUE.
      ENDIF.

    ENDIF.

    IF W_OK_CODE1 EQ 'IVB3'.  ">제비용처리 취소.

      CALL FUNCTION 'ZIM_BAPI_COST_INVOICE_CANCEL'
           EXPORTING
                P_ZFIVNO                  = IT_SELECTED-ZFIVNO
                INVOICEDOCNUMBER          = ZTIVHST1-BELNR
                FISCALYEAR                = ZTIVHST1-GJAHR
                REASONREVERSAL            = ZTIVHST1-STGRD
                POSTINGDATE               = ZTIVHST1-CBUDAT
           IMPORTING
                INVOICEDOCNUMBER_REVERSAL = ZTIVHST1-CBELNR
                FISCALYEAR_REVERSAL       = ZTIVHST1-CGJAHR
           TABLES
                RETURN                    = RETURN
           EXCEPTIONS
                OTHERS                    = 4.

      IF SY-SUBRC NE 0.           ">> 오류 발생시...
        IF RETURN[] IS INITIAL.
          PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
        ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
        CONTINUE.
      ENDIF.

    ENDIF.
*-----------------------------------------------------------------------
* 입고처리.
*-----------------------------------------------------------------------
    IF W_OK_CODE1 EQ 'GRBD'.  ">입고처리.
*>> 입고 BAPIs FUCNTION
      CALL FUNCTION 'ZIM_BAPI_GOODSMVT_CREATE'
           EXPORTING
                P_ZFIVNO         = IT_SELECTED-ZFIVNO
                P_CHG_MODE       = 'X'
                P_MVT_TYPE       = ZTIVHST-BWART
                P_BLDAT          = ZTIVHST-BLDAT
                P_BUDAT          = ZTIVHST-BUDAT
                P_GRUND          = ZTIVHST-GRUND
           IMPORTING
                MATERIALDOCUMENT = MATERIALDOCUMENT
                MATDOCUMENTYEAR  = MATDOCUMENTYEAR
           TABLES
                RETURN           = RETURN
                IT_ZSIVHSTIT     = IT_ZSIVHSTIT
           EXCEPTIONS
                MVT_ERROR        = 4.

      IF SY-SUBRC NE 0.           ">> 오류 발생시...
        ROLLBACK WORK.
        IF RETURN[] IS INITIAL.
          PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
        ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
        CONTINUE.
      ELSE.
        COMMIT WORK.
        MESSAGE S012(MIGO) WITH MATERIALDOCUMENT.
        PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.

*>고정환율 지시자가 MARKING 되어있고 입고시 환율과 물대 환율 동일.
        IF ZTIMIMG00-ZFEXFIX EQ 'X' AND ZTIMIMG00-ZFEXMTD EQ 'G'.

          LOOP AT IT_ZSIVIT.

            SELECT * FROM  ZTCIVIT UP TO 1 ROWS
                     WHERE EBELN   EQ ZTIVIT-EBELN
                     AND   EBELP   EQ ZTIVIT-EBELP.
            ENDSELECT.

            IF ZTCIVIT-ZFPRPYN  EQ 'Y'.

              SELECT SINGLE * FROM ZTCIVHD
                     WHERE ZFCIVRN  EQ  ZTCIVIT-ZFCIVRN.

              CLEAR : BAPIMEPOHEADER,
                      BAPIMEPOHEADERX.

              MOVE : ' '            TO  BAPIMEPOHEADERX-EXCH_RATE,
                     ' '            TO  BAPIMEPOHEADERX-EX_RATE_FX,
                     ZTIVIT-EBELN   TO  BAPIMEPOHEADER-PO_NUMBER,
                     ZTCIVHD-ZFEXRT TO  BAPIMEPOHEADER-EXCH_RATE,
                     ' '            TO  BAPIMEPOHEADER-EX_RATE_FX.

              CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
                   EXPORTING
                        PURCHASEORDER = ZTIVIT-EBELN
                        POHEADER      = BAPIMEPOHEADER
                        POHEADERX     = BAPIMEPOHEADERX
                   TABLES
                        RETURN        = XRETURN.

              LOOP AT XRETURN WHERE TYPE EQ 'E'.
                MOVE-CORRESPONDING  XRETURN TO RETURN.
                APPEND RETURN.
              ENDLOOP.
              IF SY-SUBRC EQ 0.
                PERFORM  P2000_MULTI_MSG_MAKE TABLES  IT_ERR_LIST.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.
    ENDIF.

    IF W_OK_CODE1 EQ 'GRB2'.  ">입고취소처리.
*        CLEAR : ZTIVHST.
*        SELECT COUNT( * ) INTO W_COUNT
*                          FROM
*        SELECT SINGLE * FROM ZTIVHST
*               WHERE ZFIVNO  EQ  IT_SELECTED-ZFIVNO
*               AND   ZFIVHST EQ  ( SELECT MAX( ZFIVHST )
*                                  FROM  ZTIVHST
*                                  WHERE ZFIVNO   EQ  IT_SELECTED-ZFIVNO
*                                  AND ( CMBLNR   IS  NULL
*                                  OR    CMBLNR   EQ  SPACE ) ).
*>> 입고 BAPIs FUCNTION
      CALL FUNCTION 'ZIM_BAPI_GOODSMVT_CANCEL'
           EXPORTING
                P_ZFIVNO            = IT_SELECTED-ZFIVNO
                P_ZFIVHST           = ZTIVHST-ZFIVHST
                MATERIALDOCUMENT    = ZTIVHST-MBLNR
                MATDOCUMENTYEAR     = ZTIVHST-MJAHR
                GOODSMVT_PSTNG_DATE = *ZTIVHST-BUDAT
                GOODSMVT_PR_UNAME   = SY-UNAME
           IMPORTING
                GOODSMVT_HEADRET    = GR_DOC
           TABLES
                RETURN              = RETURN
           EXCEPTIONS
                MVT_ERROR           = 4.

      IF SY-SUBRC NE 0.           ">> 오류 발생시...
        ROLLBACK WORK.
        IF RETURN[] IS INITIAL.
          PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
        ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
        CONTINUE.
      ELSE.
        COMMIT WORK.
        MESSAGE S060(M7) WITH GR_DOC-MAT_DOC.
        PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
      ENDIF.
    ENDIF.

*-----------------------------------------------------------------------
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
         EXPORTING
              ZFIVNO = IT_SELECTED-ZFIVNO.
*-----------------------------------------------------------------------
    ADD 1       TO W_PROC_CNT.
  ENDLOOP.

ENDFORM.                    " P4000_COST_DISTR
*&---------------------------------------------------------------------*
*&      Form  P4000_INVO_VERIF
*&---------------------------------------------------------------------*
FORM P4000_INVO_VERIF USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.
  PERFORM P4000_GET_IV_INIVAL.
  IF OK-CODE NE 'YES'.
    EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
    SELECT SINGLE *
      FROM ZTIV
     WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    PERFORM P4000_IV_VALID_CHECK.
    IF W_ERR_CHK EQ 'Y'.
      EXIT.
    ENDIF.

    PERFORM P4000_IV_BDC.
    IF W_ERR_CHK EQ 'Y'.
      EXIT.
    ENDIF.
    IF ZTIV-ZFGRST = 'Y' AND ZTIV-ZFCIVST = 'N'.
      PERFORM P4000_IV_COST_BDC.
      IF W_ERR_CHK EQ 'Y'.
        EXIT.
      ENDIF.
    ENDIF.
    ADD 1       TO W_PROC_CNT.
  ENDLOOP.

ENDFORM.                    " P4000_INVO_VERIF
*&---------------------------------------------------------------------*
*&      Form  P4000_IV_VALID_CHECK
*&---------------------------------------------------------------------*
FORM P4000_IV_VALID_CHECK.

  IF ZTIV-ZFCUST NE 'Y'.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '통관 미완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
  IF ZTIV-ZFCDST NE 'Y'.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '부대비용계산 미완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
*  IF ZTIV-ZFIVST NE 'N'.
*     MESSAGE I787 WITH ZTIV-ZFIVNO.
*     MESSAGE E600 WITH 'Invoice Verification 완료'.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.
  IF ZTIV-ZFGRST NE 'N'.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH 'Good Receipt 완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
  IF ZTIV-ZFCIVST NE 'N'.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '수입제비용 Invoice Verification 완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
*  IF ZTIV-ZFPAYYN NE 'N'.
*     MESSAGE I787 WITH ZTIV-ZFIVNO.
*     MESSAGE E600 WITH 'Payment 완료'.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.
  IF ZTIV-ZFPOYN NE 'Y'.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '무환 Invoice'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM ZTBL
   WHERE ZFBLNO = ZTIV-ZFBLNO.
  IF SY-SUBRC NE 0.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E038 WITH ZTIV-ZFBLNO.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  W_ZFIVDDT = ZTBL-ZFETD.
  IF ZTBL-ZFETD > SY-DATUM.
    W_ZFIVDDT = SY-DATUM.
  ENDIF.

  IF W_ZFIVPDT IS INITIAL.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E793.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF W_ZFIVDDT IS INITIAL.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E794.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

*  SELECT SINGLE *
*    FROM ZTREQHD
*   WHERE ZFREQNO = ZTIV-ZFREQNO.
*  IF SY-SUBRC NE 0.
*     MESSAGE I787 WITH ZTIV-ZFIVNO.
*     MESSAGE E018 WITH ZTIV-ZFREQNO.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.


  SELECT *
    FROM ZTIVIT
   WHERE ZFIVNO = ZTIV-ZFIVNO
   ORDER BY ZFIVDNO.
    SELECT SINGLE *
      FROM EKPO
     WHERE EBELN EQ ZTREQHD-EBELN
       AND EBELP EQ ZTIVIT-ZFIVDNO.
    IF SY-SUBRC NE 0.
      MESSAGE I787 WITH ZTIV-ZFIVNO.
      MESSAGE E786 WITH ZTIVIT-ZFIVDNO.
      W_ERR_CHK = 'Y'.
      EXIT.
    ELSE.
      IF NOT ( EKPO-LOEKZ IS INITIAL ).   " 삭제 상?
*               NOT ( EKPO-ELIKZ IS INITIAL ).   " 납품완료 상?
        MESSAGE I787 WITH ZTIV-ZFIVNO.
        MESSAGE E788 WITH ZTIVIT-ZFIVDNO.
        W_ERR_CHK = 'Y'.
        EXIT.
      ENDIF.
*           IF EKPO-MWSKZ IS INITIAL.               " Tax Code
*              MESSAGE I787 WITH ZTIV-ZFIVNO.
*              MESSAGE E796 WITH ZTIVIT-ZFIVDNO.
*              W_ERR_CHK = 'Y'.
*              EXIT.
*           ENDIF.
    ENDIF.
  ENDSELECT.
*
  IF SY-SUBRC NE 0.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E789.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF ZTIV-LIFNR IS INITIAL.                              " Vendor
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E136.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

*  IF ZTIV-ZFIVAMP IS INITIAL. " 처리금?
*     MESSAGE I787 WITH ZTIV-ZFIVNO.
*     MESSAGE E791.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.

*  SELECT SUM( ZFIVAMP ) INTO W_ZFIVAMP
*    FROM ZTIVIT
*   WHERE ZFIVNO = ZTIV-ZFIVNO.
*  ADD ZTIV-ZFHDCHGP TO W_ZFIVAMP.
*  ADD ZTIV-ZFPKCHGP TO W_ZFIVAMP.
*  IF W_ZFIVAMP NE ZTIV-ZFIVAMP.
*     MESSAGE I787 WITH ZTIV-ZFIVNO.
*     MESSAGE E795.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.

*  IF ZTIV-ZFPRPYN = 'Y'.
*     EXIT.
*  ENDIF.

  IF ZTIV-ZFIVAMC NE 'KRW'.   " 환?
    IF ( ZTIV-ZFEXRT IS INITIAL ) OR ( ZTIV-ZFEXRT EQ 0 ).
      MESSAGE I787 WITH ZTIV-ZFIVNO.
      MESSAGE E743.
      W_ERR_CHK = 'Y'.
      EXIT.
    ENDIF.
  ENDIF.

  W_ZFCST = ZTIV-ZFDAMT + ZTIV-ZFUPCST + ZTIV-ZFPCST.
  IF ( W_ZFCST IS INITIAL ) OR ( W_ZFCST EQ 0 ). " 수입제비?
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E792.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF ZTIV-ZFPHVN IS INITIAL. " Phandom Vendor
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    MESSAGE E790.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

ENDFORM.                                  " P4000_IV_VALID_CHECK
*&---------------------------------------------------------------------*
*&      Form  P4000_IV_BDC
*&---------------------------------------------------------------------*
FORM P4000_IV_BDC.

  CLEAR SY-SUBRC.
  IF ZTIV-ZFIVAMT > 0.
    CASE ZTIMIMG00-ZFIVTY.
      WHEN 'C'.                            " CIV
        PERFORM P4000_CIV_BDC_INSERT.
        CALL TRANSACTION 'MRHR'
             USING       ZBDCDATA
             MODE        DISPMODE
             UPDATE      UMODE.
      WHEN 'L'.                            " LIV
        PERFORM P4000_LIV_BDC_INSERT.
        CALL TRANSACTION 'MIRO'
             USING       ZBDCDATA
             MODE        DISPMODE
             UPDATE      UMODE.
      WHEN  OTHERS.
        MESSAGE I787 WITH ZTIV-ZFIVNO.
        W_ERR_CHK = 'Y'.
        EXIT.
    ENDCASE.
  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE I787 WITH ZTIV-ZFIVNO.
    W_ERR_CHK = 'Y'.
    EXIT.
  ELSE.
*     CLEAR : ZFGFDNO, ZFGFDYR.
*     CASE ZTIMIMG11-ZFIVTY.
*          WHEN 'C'.                            " CIV
*                WHILE ZFGFDNO IS INITIAL AND ZFGFDYR IS INITIAL.
*                  GET PARAMETER ID 'BLN' FIELD ZFGFDNO. " 전표번호.
*                  GET PARAMETER ID 'GJR' FIELD ZFGFDYR. " 회계년도.
*                ENDWHILE.
*          WHEN 'L'.                            " LIV
*                WHILE ZFGFDNO IS INITIAL AND ZFGFDYR IS INITIAL.
*                  GET PARAMETER ID 'RBN' FIELD ZFGFDNO. " 송장문서번호.
*                  GET PARAMETER ID 'GJR' FIELD ZFGFDYR. " 회계년도.
*                ENDWHILE.
*          WHEN  OTHERS.
*                MESSAGE I787 WITH ZTIV-ZFIVNO.
*                W_ERR_CHK = 'Y'.
*                EXIT.
*     ENDCASE.
*     ZTIV-ZFIVST  = 'Y'.            " Invoice Verify 상태.

    CLEAR W_WEPOS.
    SELECT MAX( WEPOS ) INTO W_WEPOS
      FROM EKPO
     WHERE EBELN = ZTREQHD-EBELN.
    IF W_WEPOS IS INITIAL. " Non-GR 대?
      ZTIV-ZFGRST = 'Y'.
    ENDIF.
*     IF ZTIV-ZFPRPYN = 'Y'. " 선급금.
*        ZTIV-ZFGRST = 'Y'.
*        ZTIV-ZFCIVST = 'Y'.
*        IF ZTREQHD-ZTERM EQ ZTIMIMG11-ZTERM3. " 사전송금.
*           PERFORM P2000_PAYMENT_NOTICE_INSERT USING W_ERR_CHK.
*           IF W_ERR_CHK = 'Y'.
*              MESSAGE E893.
*              EXIT.
*           ENDIF.
*        ENDIF.
*     ENDIF.

*     CLEAR : ZTIV-ZFGFDNO, ZTIV-ZFGFDYR.
*     IF ZTIV-ZFIVAMP > 0.
*        ZTIV-ZFGFDNO = ZFGFDNO.        " 전표번호.
*        ZTIV-ZFGFDYR = ZFGFDYR.        " 회계년도.
*        ZTIV-ZFIVPDT = W_ZFIVPDT.      " Posting Date
*        ZTIV-ZFIVDDT = W_ZFIVDDT.      " Document Date
*     ENDIF.
*     ZTIV-UNAM    = SY-UNAME.
*     ZTIV-UDAT    = SY-DATUM.
*     UPDATE ZTIV.
  ENDIF.

ENDFORM.                    " P4000_IV_BDC
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_IV_INIVAL
*&---------------------------------------------------------------------*
FORM P4000_GET_IV_INIVAL.

  MOVE 'Initial Value' TO SPOP-TITEL.
*  MOVE 'X'            TO RADIO_NONE.
  IF W_ZFIVPDT IS INITIAL.
    MOVE SY-DATUM    TO W_ZFIVPDT.
  ENDIF.

  CALL SCREEN 0010 STARTING AT 15 1
                   ENDING   AT 56 09.

ENDFORM.                    " P4000_GET_iv_inival
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  IF OK-CODE NE 'YES'.
    SET SCREEN 0.
    LEAVE SCREEN.
  ENDIF.

  IF W_ZFIVPDT IS INITIAL.
    MOVE SY-DATUM    TO W_ZFIVPDT.
  ENDIF.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Form  A_ZBDCDATA
*&---------------------------------------------------------------------*
FORM A_ZBDCDATA USING BEGIN_CHECK OBJNAM VALUE.

  CLEAR ZBDCDATA.
  IF BEGIN_CHECK = 'X'.
    MOVE : OBJNAM TO ZBDCDATA-PROGRAM,
           VALUE  TO ZBDCDATA-DYNPRO,
           BEGIN_CHECK TO ZBDCDATA-DYNBEGIN.
  ELSE.
    MOVE : OBJNAM TO ZBDCDATA-FNAM,
           VALUE  TO ZBDCDATA-FVAL.
  ENDIF.
  APPEND ZBDCDATA.

ENDFORM.                    " A_ZBDCDATA
*&---------------------------------------------------------------------*
*&      Form  P4000_IV_COST_BDC
*&---------------------------------------------------------------------*
FORM P4000_IV_COST_BDC.

  W_ZFCST = ZTIV-ZFDAMT + ZTIV-ZFUPCST + ZTIV-ZFPCST.

  CLEAR SY-SUBRC.
  IF W_ZFCST > 0.
    CASE ZTIMIMG00-ZFIVTY.
      WHEN 'C'.                            " CIV
        PERFORM P4000_CIV_BDC_COST_INSERT.
        CALL TRANSACTION 'MRHR'
             USING       ZBDCDATA
             MODE        DISPMODE
             UPDATE      UMODE.
      WHEN 'L'.                            " LIV
        PERFORM P4000_LIV_BDC_COST_INSERT.
        CALL TRANSACTION 'MIRO'
             USING       ZBDCDATA
             MODE        DISPMODE
             UPDATE      UMODE.
      WHEN  OTHERS.
        MESSAGE I601 WITH ZTIV-ZFIVNO.
        W_ERR_CHK = 'Y'.
        EXIT.
    ENDCASE.
  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    W_ERR_CHK = 'Y'.
    EXIT.
  ELSE.
    W_ZFCST = ZTIV-ZFDAMT + ZTIV-ZFUPCST + ZTIV-ZFPCST.
    IF W_ZFCST > 0.
*        CLEAR : ZFGFDNO, ZFGFDYR.
*        CASE ZTIMIMG11-ZFIVTY.
*             WHEN 'C'.                            " CIV
*                   WHILE ZFGFDNO IS INITIAL AND ZFGFDYR IS INITIAL.
*                     GET PARAMETER ID 'BLN' FIELD ZFGFDNO. " 전표번?
*                     GET PARAMETER ID 'GJR' FIELD ZFGFDYR. " 회계년?
*                   ENDWHILE.
*             WHEN 'L'.                            " LIV
*                   WHILE ZFGFDNO IS INITIAL AND ZFGFDYR IS INITIAL.
*                    GET PARAMETER ID 'RBN' FIELD ZFGFDNO. " 송장문서번?
*                     GET PARAMETER ID 'GJR' FIELD ZFGFDYR. " 회계년?
*                   ENDWHILE.
*             WHEN  OTHERS.
*                   MESSAGE I787 WITH ZTIV-ZFIVNO.
*                   W_ERR_CHK = 'Y'.
*                   EXIT.
*        ENDCASE.
      ZTIV-ZFCIVST  = 'Y'.            " 수입제비용 Invoice Verify 상?

*        CLEAR : ZTIV-ZFCFDNO, ZTIV-ZFCFDYR.
*        ZTIV-ZFCFDNO = ZFGFDNO.        " 전표번?
*        ZTIV-ZFCFDYR = ZFGFDYR.        " 회계년?
    ENDIF.
    ZTIV-UNAM    = SY-UNAME.
    ZTIV-UDAT    = SY-DATUM.
    UPDATE ZTIV.
  ENDIF.

ENDFORM.                    " P4000_IV_COST_BDC
*&---------------------------------------------------------------------*
*&      Form  P4000_GOOD_RECEI
*&---------------------------------------------------------------------*
FORM P4000_GOOD_RECEI USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.
  PERFORM P4000_GET_GR_INIVAL.
  IF OK-CODE NE 'YES'.
    EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
    SELECT SINGLE *
      FROM ZTIV
     WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    PERFORM P4000_GR_VALID_CHECK.
    IF W_ERR_CHK EQ 'Y' OR W_ERR_CHK EQ '2'.
      EXIT.
    ENDIF.

    IF W_ERR_CHK EQ 'N'.
      PERFORM P4000_GR_BDC.
      IF W_ERR_CHK EQ 'Y'.
        EXIT.
      ENDIF.
    ENDIF.
    IF ZTIV-ZFGRST = 'Y' AND ZTIV-ZFCIVST = 'N'.
      W_ZFIVDDT = W_ZFGRDDT.                  " G/R Document Date
      W_ZFIVPDT = W_ZFGRPDT.                  " G/R Posting Date
*          IF W_ZFIVDDT > SY-DATUM.
*             W_ZFIVDDT = SY-DATUM.
*          ENDIF.
      PERFORM P4000_IV_COST_BDC.
      IF W_ERR_CHK EQ 'Y'.
        EXIT.
      ENDIF.
    ENDIF.
    ADD 1       TO W_PROC_CNT.
  ENDLOOP.

ENDFORM.                    " P4000_GOOD_RECEI

*&---------------------------------------------------------------------*
*&      Form  P4000_GET_GR_inival
*&---------------------------------------------------------------------*
FORM P4000_GET_GR_INIVAL.

  CLEAR  W_ZFIDSDT.
  READ TABLE IT_SELECTED INDEX 1.
  SELECT SINGLE * FROM ZTIV
                  WHERE ZFIVNO EQ IT_SELECTED-ZFIVNO.

  IF ZTIV-ZFGRST EQ 'Y'.
    MESSAGE E500 WITH IT_SELECTED-ZFIVNO.
  ENDIF.

  IF ZTIV-ZFGRST EQ 'X'.
    MESSAGE E501 WITH IT_SELECTED-ZFIVNO.
  ENDIF.

  IF ZTIV-ZFCUST EQ 'Y'.
    SELECT  MAX( ZFEDT )  INTO  W_ZFIDSDT
    FROM    ZTIDSUS
    WHERE   ZFIVNO   EQ  IT_SELECTED-ZFIVNO.
  ELSE.
    CLEAR : W_ZFIDSDT.
  ENDIF.

  IF SY-LANGU EQ '3'.
  MOVE:'전기 데이타' TO SPOP-TITEL.
  ELSE.
  MOVE: 'Posting Data' TO SPOP-TITEL.
  ENDIF.

  MOVE: ZTIV-ZFIVNO   TO ZTIVHST-ZFIVNO.

  SELECT MAX( ZFIVHST ) INTO ZTIVHST-ZFIVHST
         FROM ZTIVHST
         WHERE ZFIVNO   EQ   ZTIV-ZFIVNO.
  ADD  1  TO  ZTIVHST-ZFIVHST.

  IF ZTIVHST-BLDAT IS INITIAL.
    MOVE SY-DATUM    TO ZTIVHST-BLDAT.
  ENDIF.
  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'.
    MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
  ENDIF.
*> Non-Monetary.
  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'.
    MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
  ENDIF.
*> Monetary.
  IF ZTIV-ZFPOYN = 'Y'.
    MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
  ENDIF.
  MOVE SY-DATUM     TO ZTIVHST-BUDAT.

*> Item Data SELECT..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHSTIT
           FROM  ZTIVIT
           WHERE ZFIVNO EQ ZTIV-ZFIVNO
           AND   UMSON  EQ 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE E549. EXIT.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
           FROM  ZTIVIT
           WHERE ZFIVNO EQ ZTIV-ZFIVNO
           AND   UMSON  EQ 'X'.
  IF SY-SUBRC NE 0.
    MESSAGE E549.  EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFEXFIX EQ 'X' AND ZTIMIMG00-ZFEXMTD EQ 'G'.

    LOOP  AT  IT_ZSIVIT.

      SELECT SINGLE * FROM  ZTCIVIT
                      WHERE EBELN   EQ  ZTIVIT-EBELN
                      AND   EBELP   EQ  ZTIVIT-EBELP.

      IF ZTCIVIT-ZFPRPYN  EQ 'Y'.

        SELECT SINGLE * FROM ZTCIVHD
               WHERE ZFCIVRN  EQ  ZTCIVIT-ZFCIVRN.

        CLEAR : BAPIMEPOHEADER,
                BAPIMEPOHEADERX.

        MOVE : 'X'            TO      BAPIMEPOHEADERX-EXCH_RATE,
               'X'            TO      BAPIMEPOHEADERX-EX_RATE_FX,
               ZTIVIT-EBELN   TO      BAPIMEPOHEADER-PO_NUMBER,
               ZTCIVHD-ZFEXRT TO      BAPIMEPOHEADER-EXCH_RATE,
               'X'            TO      BAPIMEPOHEADER-EX_RATE_FX.

        CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
             EXPORTING
                  PURCHASEORDER = ZTIVIT-EBELN
                  POHEADER      = BAPIMEPOHEADER
                  POHEADERX     = BAPIMEPOHEADERX
             TABLES
                  RETURN        = XRETURN.

        REFRESH : RETURN.
        LOOP AT XRETURN WHERE TYPE EQ 'E'.
          MOVE-CORRESPONDING  XRETURN TO RETURN.
          APPEND RETURN.
        ENDLOOP.
        IF SY-SUBRC EQ 0.
          PERFORM  P2000_MULTI_MSG_MAKE TABLES  IT_ERR_LIST.
        ENDIF.
      ENDIF.
    ENDLOOP.

    DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
    IF W_LINE GT 0.
      INCLUDE = 'POPU'.
      CALL SCREEN 0015            STARTING AT  05   3
                                  ENDING   AT  102 12.
      CLEAR : INCLUDE.
      EXIT.
    ENDIF.
  ENDIF.

  LOOP AT IT_ZSIVHSTIT.
    W_TABIX = SY-TABIX.
    MOVE:  IT_ZSIVHSTIT-GRMENGE   TO   IT_ZSIVHSTIT-CCMENGE,
           ZTIVHST-ZFIVHST        TO   IT_ZSIVHSTIT-ZFIVHST.

    IT_ZSIVHSTIT-GRMENGE = IT_ZSIVHSTIT-CCMENGE
                         - IT_ZSIVHSTIT-GRTOTMN.

    IF IT_ZSIVHSTIT-GRMENGE GT 0.
      IT_ZSIVHSTIT-UMSON = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHSTIT-UMSON.
    ENDIF.
    IT_ZSIVHSTIT-ZFGRST = 'Y'.

    MODIFY IT_ZSIVHSTIT INDEX W_TABIX.
  ENDLOOP.

  IF SY-LANGU EQ '3'.
     SPOP-TEXTLINE1 = '입고 전기데이타를 입력하세요.'.
     SPOP-TEXTLINE2 = '입고 전기하시겠습니까?'.
  ELSE.
     SPOP-TEXTLINE1 = 'Fill in G/R posting data.'.
     SPOP-TEXTLINE2 = 'Do you want to post Good receipt?'.
  ENDIF.

  IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' OR
     ZTIMIMG00-GRPARTX NE 'X'.
    CALL SCREEN 0020 STARTING AT 15 3
                     ENDING   AT 56 11.
  ELSE.
    CALL SCREEN 0050 STARTING AT 1  2
                     ENDING   AT 90 19.
  ENDIF.

ENDFORM.                    " P4000_GET_GR_INIVAL
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0020 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0020 INPUT.

  IF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
    ANTWORT = 'N'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'YES'.
    IF SY-DYNNR EQ '0060'.
      READ TABLE IT_ZSIVHST WITH KEY ZFMARK = 'X'.
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING IT_ZSIVHST TO ZTIVHST.
      ELSE.
        MESSAGE E962.
      ENDIF.
    ENDIF.
    ANTWORT = 'Y'.
    SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
    ANTWORT = 'N'.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_GR_VALID_CHECK
*&---------------------------------------------------------------------*
FORM P4000_GR_VALID_CHECK.

  W_ERR_CHK = 'N'.

  IF ZTIV-ZFCUST NE 'Y'.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    IF SY-LANGU EQ '3'.
    MESSAGE E599 WITH '통관 미완료'.
    ELSE.
      MESSAGE E599 WITH 'Clearance is not yet completed'.
    ENDIF.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
  IF ZTIV-ZFCDST NE 'Y'.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E599 WITH '부대비용계산 미완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
*  IF ZTIV-ZFIVST NE 'Y'.
*     MESSAGE I802 WITH ZTIV-ZFIVNO.
*     MESSAGE E599 WITH 'Invoice Verification 미완료'.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.
  IF ZTIV-ZFGRST NE 'N'.
*     MESSAGE I802 WITH ZTIV-ZFIVNO.
    IF SY-LANGU EQ '3'.
       MESSAGE I599 WITH 'Good Receipt 완료'.
    ELSE.
       MESSAGE I599 WITH 'Good Receipt was already completed'.
    ENDIF.
    W_ERR_CHK = '1'.
    EXIT.
  ENDIF.
  IF ZTIV-ZFCIVST NE 'N'.
     MESSAGE I802 WITH ZTIV-ZFIVNO.
     MESSAGE E599 WITH '수입제비용 Invoice Verification 완료'.
     W_ERR_CHK = '2'.
     EXIT.
  ENDIF.
*  IF ZTIV-ZFPAYYN NE 'N'.
*     MESSAGE I802 WITH ZTIV-ZFIVNO.
*     MESSAGE E599 WITH 'Payment 완료'.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.

  SELECT SINGLE *
    FROM ZTBL
   WHERE ZFBLNO = ZTIV-ZFBLNO.
  IF SY-SUBRC NE 0.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E038 WITH ZTIV-ZFBLNO.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF ZTIV-ZFPOYN EQ 'N' AND ZTIV-ZFPONMA = 'N'.
    EXIT.
  ENDIF.

  IF W_ZFGRPDT IS INITIAL.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E793.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF W_ZFGRDDT IS INITIAL.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E794.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF ZTIV-ZFPOYN = 'Y'.
    IF SY-SUBRC NE 0.
      MESSAGE I802 WITH ZTIV-ZFIVNO.
      W_ERR_CHK = 'Y'.
      EXIT.
    ENDIF.
  ENDIF.

  SELECT *
    FROM ZTIVIT
   WHERE ZFIVNO = ZTIV-ZFIVNO
   ORDER BY ZFIVDNO.
    SELECT SINGLE *
      FROM EKPO
     WHERE EBELN EQ W_EBELN
       AND EBELP EQ ZTIVIT-ZFIVDNO.
    IF SY-SUBRC NE 0.
      MESSAGE I802 WITH ZTIV-ZFIVNO.
      MESSAGE E786 WITH ZTIVIT-ZFIVDNO.
      W_ERR_CHK = 'Y'.
      EXIT.
    ELSE.
      IF NOT ( EKPO-LOEKZ IS INITIAL ). " 삭제 상?
        MESSAGE I802 WITH ZTIV-ZFIVNO.
        MESSAGE E788 WITH ZTIVIT-ZFIVDNO.
        W_ERR_CHK = 'Y'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
*
  IF SY-SUBRC NE 0.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E789.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  W_ZFCST = ZTIV-ZFDAMT + ZTIV-ZFUPCST + ZTIV-ZFPCST.
  IF ( W_ZFCST IS INITIAL ) OR ( W_ZFCST EQ 0 ). " 수입제비?
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E792.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF ZTIV-ZFPHVN IS INITIAL. " Phandom Vendor
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    MESSAGE E790.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

ENDFORM.                    " P4000_GR_VALID_CHECK
*&---------------------------------------------------------------------*
*&      Form  P4000_GR_BDC
*&---------------------------------------------------------------------*
FORM P4000_GR_BDC.

  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환.
    PERFORM P4000_GR_BDC_INSERT_NPO.
    CALL TRANSACTION 'MB1C'
         USING       ZBDCDATA
         MODE        DISPMODE
         UPDATE      UMODE.
  ELSE.
    PERFORM P4000_GR_BDC_INSERT.  " With Reference P/O
    CALL TRANSACTION 'MB01'
         USING       ZBDCDATA
         MODE        DISPMODE
         UPDATE      UMODE.
  ENDIF.

  IF SY-SUBRC <> 0.
    MESSAGE I802 WITH ZTIV-ZFIVNO.
    W_ERR_CHK = 'Y'.
    EXIT.
  ELSE.
*     GET PARAMETER ID 'MBN' FIELD ZFMDNO.       " 자재문서번호.
*     GET PARAMETER ID 'MJA' FIELD ZFMDYR.       " 자재문서년도.
    ZTIV-ZFGRST  = 'Y'.            " Good Receipt 상태.
*     ZTIV-ZFMDNO  = ZFMDNO.         " 자재문서번호.
*     ZTIV-ZFMDYR  = ZFMDYR.         " 자재문서년도.
*     ZTIV-ZFGRPDT = W_ZFGRPDT.      " Posting Date
*     ZTIV-ZFGRDDT = W_ZFGRDDT.      " Document Date
    ZTIV-UNAM    = SY-UNAME.
    ZTIV-UDAT    = SY-DATUM.
* 01/01/15 김연중.
*     IF ZTIV-ZFPONMA = 'Y'. "무환수출입.
    IF ZTIV-ZFPOYN = 'N'. "무환.
      MOVE 'Y' TO ZTIV-ZFCIVST.
    ENDIF.
    UPDATE ZTIV.
  ENDIF.

ENDFORM.                    " P4000_GR_BDC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFIVNO.

  SELECT SINGLE *
    FROM ZTIV
   WHERE ZFIVNO = P_ZFIVNO.

  IF SY-SUBRC EQ 0 AND NOT ZTIV-ZFBLNO IS INITIAL.
    SET PARAMETER ID 'ZPBLNO'   FIELD ZTIV-ZFBLNO.
    SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
    CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.
  ELSE.
    MESSAGE S611 WITH P_ZFIVNO 'B/L'.
  ENDIF.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P3000_DOCUMENT_TYPE
*&---------------------------------------------------------------------*
FORM P3000_DOCUMENT_TYPE.

* SKC
  MOVE 'RE' TO TEMP_BLART.
  EXIT.
*
  CLEAR TEMP_BLART.
  IF ZTREQHD-ZFJEWGB NE '1'.  " 타인자?
    MOVE 'RF' TO TEMP_BLART.
    EXIT.
  ENDIF.
*  IF ZTREQHD-ZTERM = ZTIMIMG11-ZTERM3  OR  " 사후송?
*     ZTREQHD-ZFREQTY = 'DA'  OR  " D/A
*     ZTREQHD-ZFREQTY = 'DP'.     " D/P
*     MOVE 'RF' TO TEMP_BLART.
*     EXIT.
*  ENDIF.
  IF ZTREQHD-ZTERM = 'LC' . " At Sight
    MOVE 'RF' TO TEMP_BLART.
    EXIT.
  ENDIF.
  MOVE 'RH' TO TEMP_BLART.

ENDFORM.                    " P3000_DOCUMENT_TYPE

*&---------------------------------------------------------------------*
*&      Form  P2000_PAYMENT_NOTICE_INSERT
*&---------------------------------------------------------------------*
FORM P2000_PAYMENT_NOTICE_INSERT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR : ZTPMTHD.

  MOVE ZTREQHD-ZFREQNO TO ZTPMTHD-ZFREQNO. " 수입의뢰 관리번?
  MOVE ZTREQHD-EBELN   TO ZTPMTHD-EBELN. " P/O No
  MOVE ZTREQHD-ZTERM   TO ZTPMTHD-ZTERM. " Payment Term

  CLEAR ZTIMIMG01.
  SELECT SINGLE *
    FROM ZTIMIMG01
   WHERE ZTERM = ZTPMTHD-ZTERM.
  IF SY-SUBRC = 0.
    MOVE ZTIMIMG01-ZFLCKN TO ZTPMTHD-ZFLCKN. " L/C Type
  ELSE.
    MOVE '9'              TO ZTPMTHD-ZFLCKN. " L/C Type
  ENDIF.

  MOVE ZTREQHD-ZFJEWGB TO ZTPMTHD-ZFJEWGB. " 재원구?
  MOVE '5' TO ZTPMTHD-ZFTRTY.
  IF ZTREQHD-ZFMATGB = '1' OR
     ZTREQHD-ZFMATGB = '3' OR
     ZTREQHD-ZFMATGB = '5'.
    MOVE '1' TO ZTPMTHD-ZFTRTY.
  ENDIF.
  IF ZTREQHD-ZFMATGB = '4'.
    MOVE '2' TO ZTPMTHD-ZFTRTY.
  ENDIF.
*  IF ZTREQHD-ZTERM = ZTIMIMG11-ZTERM3.
*     MOVE '3' TO ZTPMTHD-ZFTRTY.
*  ENDIF.
*  IF ZTREQHD-ZTERM = ZTIMIMG11-ZTERM4.
*     MOVE '4' TO ZTPMTHD-ZFTRTY.
*  ENDIF.

  MOVE ZTREQHD-ZFBENI  TO ZTPMTHD-ZFBENI. " Benificiary
  MOVE ZTREQHD-ZFOPBN  TO ZTPMTHD-ZFOPBN. " 개설은행.
  SELECT MAX( BANKL )  INTO ZTPMTHD-ZFOPBNK
    FROM LFBK
   WHERE LIFNR = ZTPMTHD-ZFOPBN.

  MOVE ZTREQHD-ZFOPBN  TO ZTPMTHD-ZFPNBN. " 통지은행.
  SELECT MAX( BANKL )  INTO ZTPMTHD-ZFPNBNK
    FROM LFBK
   WHERE LIFNR = ZTPMTHD-ZFPNBN.

  MOVE ZTREQHD-ZFLEVN  TO ZTPMTHD-ZFLEVN. " 차입기관.
  SELECT MAX( BANKL )  INTO ZTPMTHD-ZFLEVNK
    FROM LFBK
   WHERE LIFNR = ZTPMTHD-ZFLEVN.

  SELECT MAX( ZFPNNO ) INTO ZTPMTHD-ZFPNNO      " 관리번?
    FROM ZTPMTHD.
  ADD   1               TO ZTPMTHD-ZFPNNO.
  MOVE 'N'              TO ZTPMTHD-ZFBKCHT.
  MOVE 'N'              TO ZTPMTHD-ZFBKCHA.
  MOVE 'C'              TO ZTPMTHD-ZFPYST.
  MOVE 'N'              TO ZTPMTHD-ZFPYT.
  MOVE 'N'              TO ZTPMTHD-ZFPYL.
  MOVE 'N'              TO ZTPMTHD-ZFPYA.
  MOVE SY-UNAME         TO ZTPMTHD-UNAM.
  MOVE SY-DATUM         TO ZTPMTHD-UDAT.
  MOVE SY-UNAME         TO ZTPMTHD-ERNAM.
  MOVE SY-DATUM         TO ZTPMTHD-CDAT.
  MOVE ZTIV-ZFIVAMT     TO ZTPMTHD-ZFPNAM.  "Notice Amt
  MOVE ZTIV-ZFIVAMT     TO ZTPMTHD-ZFTIVAM. "Invoice Amt
  INSERT ZTPMTHD.
  IF SY-SUBRC NE 0.
    MOVE 'Y' TO W_ERR_CHK.
    EXIT.
  ENDIF.

  MOVE  ZTIV-ZFIVNO     TO ZTPMTIV-ZFIVNO.  "관리번?
*  MOVE  ZTIV-ZFCIVNO    TO ZTPMTIV-ZFCIVNO. "Commercial I/V NO
  MOVE  ZTIV-ZFIVAMT    TO ZTPMTIV-ZFIVAMT. "Invoice Amt
  MOVE  ZTIV-ZFIVAMT    TO ZTPMTIV-ZFPBAMT. "Payable Amt
  MOVE  ZTIV-ZFIVAMT    TO ZTPMTIV-ZFPNAM.  "Notice Amt
  MOVE  ZTIV-ZFIVAMC    TO ZTPMTIV-ZFIVAMC. "Invoice Amt Curr
  MOVE 'N'              TO ZTPMTIV-ZFPPYYN. "Partial Payment
  MOVE  ZTPMTHD-ZFPNNO  TO ZTPMTIV-ZFPNNO.
  MOVE  10              TO ZTPMTIV-ZFPNIT.
  INSERT   ZTPMTIV.
  IF SY-SUBRC NE 0.
    MOVE 'Y' TO W_ERR_CHK.
    EXIT.
  ENDIF.

ENDFORM.                    " P2000_PAYMENT_NOTICE_INSERT
*&---------------------------------------------------------------------*
*&      Form  P4000_CIV_BDC_INSERT
*&---------------------------------------------------------------------*
FORM P4000_CIV_BDC_INSERT.

  REFRESH ZBDCDATA.
*  CLEAR  W_ZFIVDNO.
*  SELECT MIN( ZFIVDNO ) INTO W_ZFIVDNO
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO.
*  SELECT SINGLE *
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO
*     AND ZFIVDNO = W_ZFIVDNO.

  PERFORM P3000_DOCUMENT_TYPE.
  IF ZTIV-ZFIVAMC = 'KRW'.
    CLEAR TEMP_KURSF.
  ELSE.
    MOVE ZTIV-ZFEXRT       TO TEMP_KURSF.
  ENDIF.
*  MOVE ZTIV-ZFCIVNO         TO TEMP_XBLNR.
  CONCATENATE ZTREQHD-ZFOPNNO '/' ZTREQHD-ZFJEWGB INTO TEMP_BKTXT.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0100'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'BKPF-BLDAT' W_ZFIVDDT,            " Document Date
      ' ' 'BKPF-BLART' TEMP_BLART,           " Document Type
*     ' ' 'BKPF-BUKRS' '1000',               " Company Code
      ' ' 'BKPF-BUKRS' ZTREQHD-BUKRS,        " Company Code
      ' ' 'BKPF-BUDAT' W_ZFIVPDT,            " Posting Date
      ' ' 'BKPF-WAERS' ZTIV-ZFIVAMC,         " Currency
      ' ' 'BKPF-KURSF' TEMP_KURSF,           " Exchange Rate
      ' ' 'BKPF-XBLNR' TEMP_XBLNR,           " Reference
      ' ' 'BKPF-BKTXT' TEMP_BKTXT,           " Reference Header Text
      ' ' 'RM08R-EBELN' ZTREQHD-EBELN,       " Purchase Order
*      ' ' 'RM08R-EBELP' W_ZFIVDNO,           " Purchase Order Item
      ' ' 'RM08R-KONTO' ZTIV-LIFNR,         " Vendor
      ' ' 'BDC_OKCODE' '/00'.                " ENTER

  WRITE ZTIV-ZFIVAMT CURRENCY ZTIV-ZFIVAMC TO TEMP_WRBTR.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '2110'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'BSEG-WRBTR' TEMP_WRBTR,           " 전체 금?
*     ' ' 'BSEG-ZFBDT'           ,           " Baseline Date
      ' ' 'BSEG-ZLSPR' 'R',                  " Payment Block
      ' ' 'BSEG-SGTXT' ZTBL-ZFHBLNO,         " Text
      ' ' 'BDC_OKCODE' '/00'.                " ENTER

  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0150'.
* 선택지시?
  CONCATENATE 'EK08R-SELKZ' '(01)' INTO TEMP_FNAM.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM 'X'.
* 금?
  CONCATENATE 'EK08R-WRBTR' '(01)' INTO TEMP_FNAM.
  WRITE ZTIVIT-ZFIVAMT CURRENCY ZTIVIT-ZFIVAMC TO TEMP_WRBTR.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_WRBTR.
* 수?
  CONCATENATE 'EK08R-MENGE' '(01)' INTO TEMP_FNAM.
*  WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_MENGE.

*  SELECT *
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO
*     AND ZFIVDNO NE W_ZFIVDNO.

  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=ESEL'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0200'.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=NBES'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0505'.
  PERFORM A_ZBDCDATA USING :
    ' ' 'RM08R-EBELN' ZTREQHD-EBELN,       " Purchase Order
    ' ' 'RM08R-EBELP' ZTIVIT-ZFIVDNO,      " Purchase Order Item
    ' ' 'BDC_OKCODE' '/00'.                " ENTER
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0150'.
* 선택지시?
  CONCATENATE 'EK08R-SELKZ' '(01)' INTO TEMP_FNAM.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM 'X'.
* 금?
  CONCATENATE 'EK08R-WRBTR' '(01)' INTO TEMP_FNAM.
  WRITE ZTIVIT-ZFIVAMT CURRENCY ZTIVIT-ZFIVAMC TO TEMP_WRBTR.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_WRBTR.
* 수?
  CONCATENATE 'EK08R-MENGE' '(01)' INTO TEMP_FNAM.
*    WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_MENGE.
*  ENDSELECT.
* 저?
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=ESEL'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0200'.
*  W_ZFIVAMT = ZTIV-ZFPKCHGP + ZTIV-ZFHDCHGP.
*  IF W_ZFIVAMT > 0.
*    PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=BNEI'.
*    PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '1800'.
*    WRITE W_ZFIVAMT CURRENCY ZTIV-ZFIVAMC TO TEMP_WRBTR.
*    PERFORM A_ZBDCDATA USING :
*            ' ' 'RM08R-BEZNK' TEMP_WRBTR,           " Unplanned Cost
*            ' ' 'BDC_OKCODE' '=OK'.                         " ENTER
*    PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0200'.
*  ENDIF.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' 'BU'.

ENDFORM.                    " P4000_CIV_BDC
*&---------------------------------------------------------------------*
*&      Form  P4000_CIV_BDC_COST_INSERT 수입제비?
*&---------------------------------------------------------------------*
FORM P4000_CIV_BDC_COST_INSERT.

  REFRESH ZBDCDATA.
*  SELECT SINGLE *
*    FROM ZTREQHD
*   WHERE ZFREQNO = ZTIV-ZFREQNO.
*  CLEAR  W_ZFIVDNO.
*  SELECT MIN( ZFIVDNO ) INTO W_ZFIVDNO
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO.
*  SELECT SINGLE *
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO
*     AND ZFIVDNO = W_ZFIVDNO.

*  PERFORM P3000_DOCUMENT_TYPE.
*  MOVE ZTIV-ZFCIVNO         TO TEMP_XBLNR.
  CONCATENATE ZTREQHD-ZFOPNNO '/' ZTREQHD-ZFJEWGB INTO TEMP_BKTXT.
  PERFORM A_ZBDCDATA USING :
      ' ' 'BKPF-BLDAT' W_ZFIVDDT,            " Document Date
      ' ' 'BKPF-BLART' 'KV',                 " Document Type
*     ' ' 'BKPF-BUKRS' '1000',               " Company Code
      ' ' 'BKPF-BUKRS' ZTREQHD-BUKRS,        " Company Code
      ' ' 'BKPF-BUDAT' W_ZFIVPDT,            " Posting Date
      ' ' 'BKPF-WAERS' ZTIV-ZFKRW,           " Currency
      ' ' 'BKPF-XBLNR' TEMP_XBLNR,           " Reference
      ' ' 'BKPF-BKTXT' TEMP_BKTXT,           " Reference Header Text
      ' ' 'RM08R-EBELN' ZTREQHD-EBELN,       " Purchase Order
*`      ' ' 'RM08R-EBELP' W_ZFIVDNO,           " Purchase Order Item
      ' ' 'RM08R-KONTO' ZTIV-ZFPHVN,         " Phandom Vendor
      ' ' 'RM08R-TBTKZ' ZTIMIMG00-TBTKZ,     " 후속 차변/대변(제비)
      ' ' 'BDC_OKCODE' '/00'.                " ENTER

  WRITE W_ZFCST CURRENCY ZTIV-ZFKRW TO TEMP_WRBTR.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '2110'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'BSEG-WRBTR' TEMP_WRBTR,           " 전체 금?
*     ' ' 'BSEG-ZFBDT'           ,           " Baseline Date
      ' ' 'BSEG-ZLSPR' 'R',                  " Payment Block
      ' ' 'BSEG-SGTXT' ZTBL-ZFHBLNO,         " Text
      ' ' 'BDC_OKCODE' '/00'.                " ENTER

  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0150'.
* 선택지시?
  CONCATENATE 'EK08R-SELKZ' '(01)' INTO TEMP_FNAM.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM 'X'.
* 금?
  CONCATENATE 'EK08R-WRBTR' '(01)' INTO TEMP_FNAM.
  W_ZFCST = ZTIVIT-ZFDAMT + ZTIVIT-ZFUPCST + ZTIVIT-ZFPCST.
  WRITE W_ZFCST CURRENCY ZTIVIT-ZFKRW TO TEMP_WRBTR.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_WRBTR.
* 수?
  CONCATENATE 'EK08R-MENGE' '(01)' INTO TEMP_FNAM.
*  WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
*  IF ZTIV-ZFPRPYN = 'N' AND ZTREQHD-ZTERM = ZTIMIMG11-ZTERM4. " 사전송?
*     WRITE ZTIVIT-MENGE UNIT ZTIVIT-MEINS TO TEMP_MENGE.
*  ENDIF.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_MENGE.

*  SELECT *
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO
*     AND ZFIVDNO NE W_ZFIVDNO.

  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=ESEL'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0200'.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=NBES'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0505'.
  PERFORM A_ZBDCDATA USING :
    ' ' 'RM08R-EBELN' ZTREQHD-EBELN,       " Purchase Order
    ' ' 'RM08R-EBELP' ZTIVIT-ZFIVDNO,      " Purchase Order Item
    ' ' 'BDC_OKCODE' '/00'.                " ENTER
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0150'.
* 선택지시?
  CONCATENATE 'EK08R-SELKZ' '(01)' INTO TEMP_FNAM.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM 'X'.
* 금?
  CONCATENATE 'EK08R-WRBTR' '(01)' INTO TEMP_FNAM.
  W_ZFCST = ZTIVIT-ZFDAMT + ZTIVIT-ZFUPCST + ZTIVIT-ZFPCST.
  WRITE W_ZFCST CURRENCY ZTIVIT-ZFKRW TO TEMP_WRBTR.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_WRBTR.
* 수?
  CONCATENATE 'EK08R-MENGE' '(01)' INTO TEMP_FNAM.
*    WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
*    IF ZTIV-ZFPRPYN = 'N' AND
*       ZTREQHD-ZTERM = ZTIMIMG11-ZTERM4. " 사전송?
*       WRITE ZTIVIT-MENGE UNIT ZTIVIT-MEINS TO TEMP_MENGE.
*    ENDIF.
  PERFORM A_ZBDCDATA USING ' ' TEMP_FNAM TEMP_MENGE.
*  ENDSELECT.
* 저?
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=ESEL'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM08R' '0200'.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' 'BU'.

ENDFORM.                    " P4000_CIV_BDC_COST_INSERT
*&---------------------------------------------------------------------*
*&      Form  P4000_GR_BDC_INSERT
*&---------------------------------------------------------------------*
FORM P4000_GR_BDC_INSERT.

  REFRESH ZBDCDATA.
*  CLEAR  W_ZFIVDNO.
*  SELECT MIN( ZFIVDNO ) INTO W_ZFIVDNO
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO.
*  SELECT SINGLE *
*    FROM ZTIVIT
*   WHERE ZFIVNO  = ZTIV-ZFIVNO
*     AND ZFIVDNO = W_ZFIVDNO.

  PERFORM A_ZBDCDATA USING 'X' 'SAPMM07M' '0200'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'MKPF-BLDAT' W_ZFGRDDT,                  " G/R Document Date
      ' ' 'MKPF-BUDAT' W_ZFGRPDT,                  " G/R Posting Date
      ' ' 'MKPF-FRBNR' ZTBL-ZFHBLNO,               " Bill of Lading
      ' ' 'RM07M-BWARTWE' W_BWARTWE,               " Movement Type
      ' ' 'RM07M-EBELN' W_EBELN,                   " Purchasing Document
*     ' ' 'RM07M-EBELP' W_ZFIVDNO,            " Purchasing Document Item
      ' ' 'RM07M-WERKS' '    ',                             " Plant
*      ' ' 'RM07M-LGORT' W_LGORT,                   " Storage Location
      ' ' 'BDC_OKCODE' '/00'.

  PERFORM A_ZBDCDATA USING : 'X'  'SAPMM07M' '0221'.
*  WRITE ZTIVIT-MENGE UNIT ZTIVIT-MEINS TO TEMP_ERFMG.
  CONCATENATE 'MSEG-ERFMG' '(01)'  INTO TEMP_FNAM. " 수?
  PERFORM A_ZBDCDATA USING ' '     TEMP_FNAM TEMP_ERFMG.
  CONCATENATE 'MSEG-LGORT' '(01)'  INTO TEMP_FNAM.
  PERFORM A_ZBDCDATA USING ' '     TEMP_FNAM W_LGORT.
  PERFORM A_ZBDCDATA USING ' '    'BDC_OKCODE' '/00'.

  PERFORM A_ZBDCDATA USING 'X'    'SAPMM07M' '0221'.
  PERFORM A_ZBDCDATA USING ' '    'BDC_OKCODE' '=SP'.

  PERFORM A_ZBDCDATA USING 'X'    'SAPMM07M' '0210'.
* SKC
*  PERFORM A_ZBDCDATA USING ' '    'MSEG-GRUND' '0001'.
* SKC
  PERFORM A_ZBDCDATA USING ' '    'BDC_OKCODE' '/00'.

  PERFORM A_ZBDCDATA USING 'X'    'SAPMM07M' '0221'.

*  SELECT *
*    FROM ZTIVIT
*   WHERE ZFIVNO = ZTIV-ZFIVNO
*     AND ZFIVDNO NE W_ZFIVDNO.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=NFBL'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM07M' '1201'.
  PERFORM A_ZBDCDATA USING ' ' 'RM07M-LGORT' W_LGORT.
  PERFORM A_ZBDCDATA USING ' ' 'RM07M-EBELN(01)' W_EBELN.
  PERFORM A_ZBDCDATA USING ' ' 'RM07M-EBELP(01)' ZTIVIT-ZFIVDNO.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '=SP'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM07M' '0210'.
*         WRITE ZTIVIT-MENGE UNIT ZTIVIT-MEINS TO TEMP_ERFMG.
  PERFORM A_ZBDCDATA USING ' ' 'MSEG-ERFMG' TEMP_ERFMG.
* SKC
*         PERFORM A_ZBDCDATA USING ' '    'MSEG-GRUND' '0001'.
* SKC
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' '/00'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMM07M' '0221'.
*  ENDSELECT.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' 'BU'.

ENDFORM.                    " P4000_GR_BDC_INSERT
*&---------------------------------------------------------------------*
*&      Form  P4000_COST_INVO_VERIF
*&---------------------------------------------------------------------*
FORM P4000_COST_INVO_VERIF USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.
  PERFORM P4000_GET_IV_INIVAL.
  IF OK-CODE NE 'YES'.
    EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
    SELECT SINGLE *
      FROM ZTIV
     WHERE ZFIVNO = IT_SELECTED-ZFIVNO.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    PERFORM P4000_IV_COST_VALID_CHECK.
    IF W_ERR_CHK EQ 'Y'.
      EXIT.
    ENDIF.

    PERFORM P4000_IV_COST_BDC.
    IF W_ERR_CHK EQ 'Y'.
      EXIT.
    ENDIF.
    ADD 1       TO W_PROC_CNT.
  ENDLOOP.

ENDFORM.                    " P4000_COST_INVO_VERIF

*&---------------------------------------------------------------------*
*&      Form  P4000_IV_COST_VALID_CHECK
*&---------------------------------------------------------------------*
FORM P4000_IV_COST_VALID_CHECK.

  IF ZTIV-ZFCUST NE 'Y'.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '통관 미완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
  IF ZTIV-ZFCDST NE 'Y'.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '부대비용계산 미완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
*  IF ZTIV-ZFIVST NE 'Y'.
*     MESSAGE I601 WITH ZTIV-ZFIVNO.
*     MESSAGE E600 WITH 'Invoice Verification 완료'.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.
  IF ZTIV-ZFGRST NE 'Y'.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH 'Good Receipt 완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
  IF ZTIV-ZFCIVST NE 'N'.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '수입제비용 Invoice Verification 완료'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.
*  IF ZTIV-ZFPAYYN NE 'N'.
*     MESSAGE I601 WITH ZTIV-ZFIVNO.
*     MESSAGE E600 WITH 'Payment 완료'.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.
  IF ZTIV-ZFPOYN NE 'Y'.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E600 WITH '무환 Invoice'.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM ZTBL
   WHERE ZFBLNO = ZTIV-ZFBLNO.
  IF SY-SUBRC NE 0.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E038 WITH ZTIV-ZFBLNO.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  W_ZFIVDDT = ZTBL-ZFETD.
*  IF ZTBL-ZFETD > SY-DATUM.
*     W_ZFIVDDT = SY-DATUM.
*  ENDIF.

  IF W_ZFIVPDT IS INITIAL.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E793.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF W_ZFIVDDT IS INITIAL.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E794.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

*  SELECT SINGLE *
*    FROM ZTREQHD
*   WHERE ZFREQNO = ZTIV-ZFREQNO.
*  IF SY-SUBRC NE 0.
*     MESSAGE I601 WITH ZTIV-ZFIVNO.
*     MESSAGE E018 WITH ZTIV-ZFREQNO.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.


  SELECT *
    FROM ZTIVIT
   WHERE ZFIVNO = ZTIV-ZFIVNO
   ORDER BY ZFIVDNO.
    SELECT SINGLE *
      FROM EKPO
     WHERE EBELN EQ ZTREQHD-EBELN
       AND EBELP EQ ZTIVIT-ZFIVDNO.
    IF SY-SUBRC NE 0.
      MESSAGE I601 WITH ZTIV-ZFIVNO.
      MESSAGE E786 WITH ZTIVIT-ZFIVDNO.
      W_ERR_CHK = 'Y'.
      EXIT.
    ELSE.
      IF NOT ( EKPO-LOEKZ IS INITIAL ).   " 삭제 상?
*               NOT ( EKPO-ELIKZ IS INITIAL ).   " 납품완료 상?
        MESSAGE I601 WITH ZTIV-ZFIVNO.
        MESSAGE E788 WITH ZTIVIT-ZFIVDNO.
        W_ERR_CHK = 'Y'.
        EXIT.
      ENDIF.
*           IF EKPO-MWSKZ IS INITIAL.               " Tax Code
*              MESSAGE I601 WITH ZTIV-ZFIVNO.
*              MESSAGE E796 WITH ZTIVIT-ZFIVDNO.
*              W_ERR_CHK = 'Y'.
*              EXIT.
*           ENDIF.
    ENDIF.
  ENDSELECT.
*
  IF SY-SUBRC NE 0.
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E789.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  W_ZFCST = ZTIV-ZFDAMT + ZTIV-ZFUPCST + ZTIV-ZFPCST.
  IF ( W_ZFCST IS INITIAL ) OR ( W_ZFCST EQ 0 ). " 수입제비?
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E792.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

  IF ZTIV-ZFPHVN IS INITIAL. " Phandom Vendor
    MESSAGE I601 WITH ZTIV-ZFIVNO.
    MESSAGE E790.
    W_ERR_CHK = 'Y'.
    EXIT.
  ENDIF.

ENDFORM.                    " P4000_IV_COST_VALID_CHECK

*&---------------------------------------------------------------------*
*&      Form  P4000_LIV_BDC_INSERT
*&---------------------------------------------------------------------*
FORM P4000_LIV_BDC_INSERT.
  DATA : L_BUKRS   LIKE   BKPF-BUKRS.

  REFRESH ZBDCDATA.

  PERFORM P3000_DOCUMENT_TYPE.

  WRITE ZTIV-ZFIVAMT CURRENCY ZTIV-ZFIVAMC TO TEMP_WRBTR.
*>> COMPANY CODE.
  GET PARAMETER ID 'BUK' FIELD L_BUKRS.
  IF L_BUKRS IS INITIAL.
    PERFORM A_ZBDCDATA USING 'X' 'SAPLACHD' '1000'.
    PERFORM A_ZBDCDATA USING :
        ' ' 'BKPF-BUKRS' ZTREQHD-BUKRS,        " Company Code.
        ' ' 'BDC_OKCODE' '/00'.                " ENTER
  ENDIF.
* 기본데이타.
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'RM08M-VORGANG' '1',
      ' ' 'INVFO-BLDAT' W_ZFIVDDT,           " Document Date
      ' ' 'INVFO-BUDAT' W_ZFIVPDT,           " Posting Date
      ' ' 'INVFO-WRBTR' TEMP_WRBTR,          " 금?
      ' ' 'INVFO-WAERS' ZTIV-ZFIVAMC,        " Currency
      ' ' 'BDC_OKCODE' '=HEADER_PAY'.
* 지?
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'INVFO-ZLSPR' 'R',                 " Payment Block
      ' ' 'BDC_OKCODE' 'HEADER_FI'.          " 세부사항.
* 세부사항.
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  CONCATENATE ZTREQHD-ZFOPNNO '/' ZTREQHD-ZFJEWGB INTO TEMP_BKTXT.
*  W_ZFIVAMT = ZTIV-ZFPKCHGP + ZTIV-ZFHDCHGP.
*  IF W_ZFIVAMT > 0.
*    WRITE W_ZFIVAMT CURRENCY ZTIV-ZFIVAMC TO TEMP_WRBTR.
*    PERFORM A_ZBDCDATA USING :
*            ' ' 'RM08R-BEZNK' TEMP_WRBTR.    " Unplanned Cost
*  ENDIF.

  IF ZTIV-ZFIVAMC NE 'KRW'.
    MOVE ZTIV-ZFEXRT       TO TEMP_KURSF.
    PERFORM A_ZBDCDATA USING :
       ' ' 'INVFO-KURSF' TEMP_KURSF.        " Exchange Rate
  ENDIF.
  PERFORM A_ZBDCDATA USING :
      ' ' 'INVFO-LIFRE' ZTIV-LIFNR,         " Vendor
      ' ' 'INVFO-BKTXT' TEMP_BKTXT,          " Reference Header Text
      ' ' 'RM08M-EBELN' ZTREQHD-EBELN,       " Purchase Order
      ' ' 'BDC_OKCODE' '/00'.                " ENTER
* Text, Reference,

  CLEAR LOOP_CNT.
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  SELECT *
    FROM EKPO
   WHERE EBELN = ZTREQHD-EBELN
     AND LOEKZ = '' "삭제지시자.
   ORDER BY EBELP.
*    and elikz = ''. "납품완료지시자.
    ADD 1 TO LOOP_CNT.

    SELECT SINGLE *
      FROM ZTIVIT
     WHERE ZFIVNO  = ZTIV-ZFIVNO
       AND ZFIVDNO = EKPO-EBELP.

    IF SY-SUBRC = 0.
      PERFORM A_ZBDCDATA USING :
          ' ' 'RM08M-SKIP_TO' LOOP_CNT,          " Skip-To
          ' ' 'BDC_OKCODE' '=POS'.               " Position
      PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
* 금액.
      CONCATENATE 'DRSEG-WRBTR' '(1)' INTO TEMP_FNAM.
      WRITE ZTIVIT-ZFIVAMT CURRENCY ZTIVIT-ZFIVAMC TO TEMP_WRBTR.
      PERFORM A_ZBDCDATA USING ' '    TEMP_FNAM TEMP_WRBTR.
* 수량.
      CONCATENATE 'DRSEG-MENGE' '(1)' INTO TEMP_FNAM.
*        WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
      PERFORM A_ZBDCDATA USING ' '    TEMP_FNAM TEMP_MENGE.
* 선택지시자.
      CONCATENATE 'DRSEG-OK' '(1)'   INTO TEMP_FNAM.
      PERFORM A_ZBDCDATA USING ' '   TEMP_FNAM 'X'.
    ENDIF.
  ENDSELECT.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' 'BU'.

ENDFORM.                    " P4000_LIV_BDC_INSERT

*&---------------------------------------------------------------------*
*&      Form  P4000_LIV_BDC_COST_INSERT
*&---------------------------------------------------------------------*
FORM P4000_LIV_BDC_COST_INSERT.
  DATA : L_BUKRS   LIKE  BKPF-BUKRS.

  REFRESH ZBDCDATA.

*  PERFORM P3000_DOCUMENT_TYPE.

  WRITE W_ZFCST CURRENCY ZTIV-ZFKRW TO TEMP_WRBTR.
*>> COMPANY CODE.
  GET PARAMETER ID 'BUK' FIELD L_BUKRS.
  IF L_BUKRS IS INITIAL.
    PERFORM A_ZBDCDATA USING 'X' 'SAPLACHD' '1000'.
    PERFORM A_ZBDCDATA USING :
        ' ' 'BKPF-BUKRS' ZTREQHD-BUKRS,        " Company Code.
        ' ' 'BDC_OKCODE' '/00'.                " ENTER
  ENDIF.

* 기본데이타.
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'RM08M-VORGANG' '4',               " Sub DB/CR
      ' ' 'INVFO-BLDAT' W_ZFIVDDT,           " Document Date
      ' ' 'INVFO-BUDAT' W_ZFIVPDT,           " Posting Date
      ' ' 'INVFO-WRBTR' TEMP_WRBTR,          " 금?
      ' ' 'INVFO-WAERS' ZTIV-ZFKRW,          " Currency
      ' ' 'BDC_OKCODE' '=HEADER_PAY'.
* 지급.
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'INVFO-ZLSPR' 'R',                 " Payment Block
      ' ' 'BDC_OKCODE' 'HEADER_FI'.          " 세부사?
* 세부사?
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  CONCATENATE ZTREQHD-ZFOPNNO '/' ZTREQHD-ZFJEWGB INTO TEMP_BKTXT.
  PERFORM A_ZBDCDATA USING :
      ' ' 'RM08M-EBELN' ZTREQHD-EBELN,       " Purchase Order
      ' ' 'INVFO-LIFRE' ZTIV-ZFPHVN,         " Phantom Vendor
      ' ' 'INVFO-BKTXT' TEMP_BKTXT,          " Reference Header Text
      ' ' 'BDC_OKCODE' '/00'.                " ENTER
*환율? Text, Reference,

  CLEAR LOOP_CNT.
  PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
  SELECT *
    FROM EKPO
   WHERE EBELN = ZTREQHD-EBELN
     AND LOEKZ = '' "삭제지시?
   ORDER BY EBELP.
*    and elikz = ''. "납품완료지시?
    ADD 1 TO LOOP_CNT.

    SELECT SINGLE *
      FROM ZTIVIT
     WHERE ZFIVNO  = ZTIV-ZFIVNO
       AND ZFIVDNO = EKPO-EBELP.

    IF SY-SUBRC = 0.
      PERFORM A_ZBDCDATA USING :
          ' ' 'RM08M-SKIP_TO' LOOP_CNT,          " Skip-To
          ' ' 'BDC_OKCODE' '=POS'.               " Position
      PERFORM A_ZBDCDATA USING 'X' 'SAPLMR1M' '6000'.
* 금?
      CONCATENATE 'DRSEG-WRBTR' '(1)' INTO TEMP_FNAM.
      W_ZFCST = ZTIVIT-ZFDAMT + ZTIVIT-ZFUPCST + ZTIVIT-ZFPCST.
      WRITE W_ZFCST CURRENCY ZTIVIT-ZFKRW TO TEMP_WRBTR.
      PERFORM A_ZBDCDATA USING ' '    TEMP_FNAM TEMP_WRBTR.
* 수?
      CONCATENATE 'DRSEG-MENGE' '(1)' INTO TEMP_FNAM.
*        WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_MENGE.
*        IF ZTIV-ZFPRPYN = 'N' AND
*           ZTREQHD-ZTERM = ZTIMIMG11-ZTERM4. " 사전송?
*           WRITE ZTIVIT-MENGE UNIT ZTIVIT-MEINS TO TEMP_MENGE.
*        ENDIF.
      PERFORM A_ZBDCDATA USING ' '    TEMP_FNAM TEMP_MENGE.
* 선택지시?
      CONCATENATE 'DRSEG-OK' '(1)'   INTO TEMP_FNAM.
      PERFORM A_ZBDCDATA USING ' '   TEMP_FNAM 'X'.
    ENDIF.
  ENDSELECT.
  PERFORM A_ZBDCDATA USING ' ' 'BDC_OKCODE' 'BU'.

ENDFORM.                    " P4000_LIV_BDC_COST_INSERT

*&---------------------------------------------------------------------*
*&      Form  P4000_GR_BDC_INSERT_NPO
*&---------------------------------------------------------------------*
FORM P4000_GR_BDC_INSERT_NPO.
  REFRESH ZBDCDATA.

  PERFORM A_ZBDCDATA USING 'X' 'SAPMM07M' '0400'.
  PERFORM A_ZBDCDATA USING :
      ' ' 'RM07M-WERKS'   '    ',                    " Plant
      ' ' 'RM07M-LGORT' W_LGORT,                   " Storage Location
      ' ' 'BDC_OKCODE' '=NPE'.

  SELECT  * FROM ZTIVIT
            WHERE ZFIVNO  = ZTIV-ZFIVNO.

    PERFORM A_ZBDCDATA USING : 'X'  'SAPMM07M' '0410'.
    PERFORM A_ZBDCDATA USING :
         ' ' 'MSEG-MATNR' ZTIVIT-MATNR,               " 자재.
         ' ' 'MSEG-WEMPF' SY-UNAME,                    " 사용자.
         ' ' 'MSEG-LGORT' W_LGORT,                   " Storage Location
         ' ' 'MSEG-ERFME' ZTIVIT-MEINS,
         ' ' 'BDC_OKCODE' '=NPE'.

*     WRITE ZTIVIT-ZFPRQN UNIT ZTIVIT-MEINS TO TEMP_ERFMG.
    PERFORM A_ZBDCDATA USING  ' '  'MSEG-ERFMG' TEMP_ERFMG.
  ENDSELECT.

  PERFORM A_ZBDCDATA USING :  ' ' 'BDC_OKCODE' 'BU'.

ENDFORM.                    " P4000_GR_BDC_INSERT_NPO
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK USING    W_ERR_CHK.

  MOVE : 'N'       TO      W_ERR_CHK.
  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
    MOVE 'X'   TO :  P_CDN,  P_CDY,  P_CDX,
                     P_CIVN, P_CIVY, P_CIVX.
  ENDIF.

*>> SELECT STATUS MARK FIELD CHECK.
  IF P_POY = ' ' AND P_PON = ' ' AND P_POM = ' '.
    MOVE 'Y' TO W_ERR_CHK.  MESSAGE S416 WITH '유무환 구분'.    EXIT.
  ENDIF.
  IF P_CUY = ' ' AND P_CU1 = ' ' AND P_CU2 = ' ' AND P_CU3 = ' ' AND
     P_CUN = ' '.
    MOVE 'Y' TO W_ERR_CHK.  MESSAGE S416 WITH '통관구분'.       EXIT.
  ENDIF.

  IF  P_CDN = ' ' AND P_CDY = ' ' AND P_CDX = ' '.
    MOVE 'Y' TO W_ERR_CHK.  MESSAGE S416 WITH '비용배부 상태'.  EXIT.
  ENDIF.
  IF P_GRN = ' ' AND P_GRY = ' ' AND P_GRX = ' ' AND P_GRP = ' '.
    MOVE 'Y' TO W_ERR_CHK.  MESSAGE S416 WITH '입고상태'.       EXIT.
  ENDIF.
  IF P_CIVN = ' ' AND P_CIVY = ' ' AND P_CIVX = ' '.
    MOVE 'Y' TO W_ERR_CHK.  MESSAGE S416 WITH '제비용 상태'.    EXIT.
  ENDIF.
  IF P_POMN = ' ' AND P_POMY = ' '.
    MOVE 'Y' TO W_ERR_CHK.  MESSAGE S416 WITH '무환수출입 상태'. EXIT.
  ENDIF.

*>> SELECT FILED MAKE.
*>> 검색을 위한 RANGE.
  REFRESH : R_ZFPOYN, R_ZFCUST, R_ZFCDST, R_ZFGRST, R_ZFCIVST,
            R_ZFPONMA.
*                       R_ZFPAYYN.
*>> 유환/무환 구분.
  IF P_POY EQ 'X'.
    MOVE : 'I'      TO      R_ZFPOYN-SIGN,
           'EQ'     TO      R_ZFPOYN-OPTION,
           'Y'      TO      R_ZFPOYN-LOW,
           SPACE    TO      R_ZFPOYN-HIGH.
    APPEND  R_ZFPOYN.
  ENDIF.
  IF P_PON EQ 'X'.
    MOVE : 'I'      TO      R_ZFPOYN-SIGN,
           'EQ'     TO      R_ZFPOYN-OPTION,
           'N'      TO      R_ZFPOYN-LOW,
           SPACE    TO      R_ZFPOYN-HIGH.
    APPEND  R_ZFPOYN.
  ENDIF.
  IF P_POM EQ 'X'.
    MOVE : 'I'      TO      R_ZFPOYN-SIGN,
           'EQ'     TO      R_ZFPOYN-OPTION,
           'M'      TO      R_ZFPOYN-LOW,
           SPACE    TO      R_ZFPOYN-HIGH.
    APPEND  R_ZFPOYN.
  ENDIF.
*>> 통관상태.
  IF P_CUN EQ 'X'.
    MOVE : 'I'      TO      R_ZFCUST-SIGN,
           'EQ'     TO      R_ZFCUST-OPTION,
           'N'      TO      R_ZFCUST-LOW,
           SPACE    TO      R_ZFCUST-HIGH.
    APPEND  R_ZFCUST.
  ENDIF.
  IF P_CUY EQ 'X'.
    MOVE : 'I'      TO      R_ZFCUST-SIGN,
           'EQ'     TO      R_ZFCUST-OPTION,
           'Y'      TO      R_ZFCUST-LOW,
           SPACE    TO      R_ZFCUST-HIGH.
    APPEND  R_ZFCUST.
  ENDIF.
  IF P_CU1 EQ 'X'.
    MOVE : 'I'      TO      R_ZFCUST-SIGN,
           'EQ'     TO      R_ZFCUST-OPTION,
           '1'      TO      R_ZFCUST-LOW,
           SPACE    TO      R_ZFCUST-HIGH.
    APPEND  R_ZFCUST.
  ENDIF.
  IF P_CU2 EQ 'X'.
    MOVE : 'I'      TO      R_ZFCUST-SIGN,
           'EQ'     TO      R_ZFCUST-OPTION,
           '2'      TO      R_ZFCUST-LOW,
           SPACE    TO      R_ZFCUST-HIGH.
    APPEND  R_ZFCUST.
  ENDIF.
  IF P_CU3 EQ 'X'.
    MOVE : 'I'      TO      R_ZFCUST-SIGN,
           'EQ'     TO      R_ZFCUST-OPTION,
           '3'      TO      R_ZFCUST-LOW,
           SPACE    TO      R_ZFCUST-HIGH.
    APPEND  R_ZFCUST.
  ENDIF.
*>> 부대비용 상태.
  IF P_CDN EQ 'X'.
    MOVE : 'I'      TO      R_ZFCDST-SIGN,
           'EQ'     TO      R_ZFCDST-OPTION,
           'N'      TO      R_ZFCDST-LOW,
           SPACE    TO      R_ZFCDST-HIGH.
    APPEND  R_ZFCDST.
  ENDIF.
  IF P_CDY EQ 'X'.
    MOVE : 'I'      TO      R_ZFCDST-SIGN,
           'EQ'     TO      R_ZFCDST-OPTION,
           'Y'      TO      R_ZFCDST-LOW,
           SPACE    TO      R_ZFCDST-HIGH.
    APPEND  R_ZFCDST.
  ENDIF.
  IF P_CDX EQ 'X'.
    MOVE : 'I'      TO      R_ZFCDST-SIGN,
           'EQ'     TO      R_ZFCDST-OPTION,
           'X'      TO      R_ZFCDST-LOW,
           SPACE    TO      R_ZFCDST-HIGH.
    APPEND  R_ZFCDST.
  ENDIF.
*>> 입고처리 상태.
  IF P_GRN EQ 'X'.
    MOVE : 'I'      TO      R_ZFGRST-SIGN,
           'EQ'     TO      R_ZFGRST-OPTION,
           'N'      TO      R_ZFGRST-LOW,
           SPACE    TO      R_ZFGRST-HIGH.
    APPEND  R_ZFGRST.
  ENDIF.
  IF P_GRY EQ 'X'.
    MOVE : 'I'      TO      R_ZFGRST-SIGN,
           'EQ'     TO      R_ZFGRST-OPTION,
           'Y'      TO      R_ZFGRST-LOW,
           SPACE    TO      R_ZFGRST-HIGH.
    APPEND  R_ZFGRST.
  ENDIF.
  IF P_GRP EQ 'X'.
    MOVE : 'I'      TO      R_ZFGRST-SIGN,
           'EQ'     TO      R_ZFGRST-OPTION,
           'P'      TO      R_ZFGRST-LOW,
           SPACE    TO      R_ZFGRST-HIGH.
    APPEND  R_ZFGRST.
  ENDIF.
  IF P_GRX EQ 'X'.
    MOVE : 'I'      TO      R_ZFGRST-SIGN,
           'EQ'     TO      R_ZFGRST-OPTION,
           'X'      TO      R_ZFGRST-LOW,
           SPACE    TO      R_ZFGRST-HIGH.
    APPEND  R_ZFGRST.
  ENDIF.
*>> 수입제비용 처리 상태.
  IF P_CIVN EQ 'X'.
    MOVE : 'I'      TO      R_ZFCIVST-SIGN,
           'EQ'     TO      R_ZFCIVST-OPTION,
           'N'      TO      R_ZFCIVST-LOW,
           SPACE    TO      R_ZFCIVST-HIGH.
    APPEND  R_ZFCIVST.
  ENDIF.
  IF P_CIVY EQ 'X'.
    MOVE : 'I'      TO      R_ZFCIVST-SIGN,
           'EQ'     TO      R_ZFCIVST-OPTION,
           'Y'      TO      R_ZFCIVST-LOW,
           SPACE    TO      R_ZFCIVST-HIGH.
    APPEND  R_ZFCIVST.
  ENDIF.
  IF P_CIVX EQ 'X'.
    MOVE : 'I'      TO      R_ZFCIVST-SIGN,
           'EQ'     TO      R_ZFCIVST-OPTION,
           'X'      TO      R_ZFCIVST-LOW,
           SPACE    TO      R_ZFCIVST-HIGH.
    APPEND  R_ZFCIVST.
  ENDIF.
*>> 무환수출입여부. 상태.
  IF P_POMN EQ 'X'.
    MOVE : 'I'      TO      R_ZFPONMA-SIGN,
           'EQ'     TO      R_ZFPONMA-OPTION,
           'N'      TO      R_ZFPONMA-LOW,
           SPACE    TO      R_ZFPONMA-HIGH.
    APPEND  R_ZFPONMA.
  ENDIF.
  IF P_POMY EQ 'X'.
    MOVE : 'I'      TO      R_ZFPONMA-SIGN,
           'EQ'     TO      R_ZFPONMA-OPTION,
           'Y'      TO      R_ZFPONMA-LOW,
           SPACE    TO      R_ZFPONMA-HIGH.
    APPEND  R_ZFPONMA.
  ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
*  IF W_STATUS_CHK = 'D'.
*     SET PF-STATUS 'STDLISA'.
*  ELSE.
  SET PF-STATUS 'STDLISW'.
*  ENDIF.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Error List'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.
  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      FORMAT COLOR COL_HEADING INTENSIFIED OFF.
      WRITE : / SY-ULINE(107), / SY-VLINE NO-GAP,
                'Type'   NO-GAP, SY-VLINE NO-GAP,
                'Clr.req.no' NO-GAP, SY-VLINE NO-GAP,
                'Message Text ', 105 SY-VLINE NO-GAP,
                'T'      NO-GAP, SY-VLINE,
              / SY-ULINE(107).
*         MESSAGE
      LOOP AT IT_ERR_LIST.
        W_MOD  =  SY-TABIX MOD 2.
        FORMAT RESET.
        IF W_MOD EQ 0.
          FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
        ELSE.
          FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
        ENDIF.
        WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-ZFIVNO  NO-GAP,
                  SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
                  SY-VLINE NO-GAP.

        CASE IT_ERR_LIST-MSGTYP.
          WHEN 'E'.
            FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
          WHEN 'W'.
            FORMAT COLOR COL_KEY      INTENSIFIED OFF.
          WHEN 'I'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
          WHEN 'S'.
            FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
        ENDCASE.

        WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
        HIDE:IT_ERR_LIST.
      ENDLOOP.
      WRITE : / SY-ULINE(107).
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

  MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
         IT_SELECTED-ZFIVNO  TO     IT_ERR_LIST-ZFIVNO.

  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = IT_ERR_LIST-MSGID
            MSGNR               = IT_ERR_LIST-MSGNR
            MSGV1               = IT_ERR_LIST-MSGV1
            MSGV2               = IT_ERR_LIST-MSGV2
            MSGV3               = IT_ERR_LIST-MSGV3
            MSGV4               = IT_ERR_LIST-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

  CASE IT_ERR_LIST-MSGTYP.
    WHEN 'E'.
      MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
    WHEN 'I'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'S'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'W'.
      MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
  ENDCASE.

  APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
           IT_SELECTED-ZFIVNO  TO     IT_ERR_LIST-ZFIVNO.

    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E'.
        MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
        MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
    ENDCASE.

    APPEND  IT_ERR_LIST.

  ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_SCREEN_FIELD_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SCREEN_FIELD_SET.

  IF ZTIMIMG00-ZFCSTMD EQ 'S' OR ZTIMIMG00-ZFCSTMD EQ 'P'.
*>> SCREEN MODIFY
    LOOP AT SCREEN.
      IF SCREEN-NAME EQ 'P_CDN' OR                "> 비용배부 상태.
         SCREEN-NAME EQ 'P_CDY' OR
         SCREEN-NAME EQ 'P_CDX' OR
         SCREEN-NAME EQ '%C041072_1000' OR
         SCREEN-NAME EQ '%C042075_1000' OR
         SCREEN-NAME EQ '%C043078_1000' OR
         SCREEN-NAME EQ '%C011083_1000' OR
         SCREEN-NAME EQ 'P_CIVN' OR               "> 제비용 상태.
         SCREEN-NAME EQ 'P_CIVY' OR
         SCREEN-NAME EQ 'P_CIVX' OR
         SCREEN-NAME EQ '%C111085_1000' OR
         SCREEN-NAME EQ '%C112088_1000' OR
         SCREEN-NAME EQ '%C113091_1000' OR
         SCREEN-NAME EQ '%C010096_1000' OR
         SCREEN-NAME EQ 'P_POMN'        OR
         SCREEN-NAME EQ '%C101098_1000' OR
         SCREEN-NAME EQ 'P_POMY'        OR
         SCREEN-NAME EQ '%C102101_1000' .

        SCREEN-INVISIBLE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " P2000_SCREEN_FIELD_SET

*&---------------------------------------------------------------------*
*&      Module  CHECK_GR_POSTING_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_GR_POSTING_DATE INPUT.

  IF OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR'.
    IF *ZTIVHST-BUDAT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BUDAT'.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_GR_POSTING_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_GR_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_GET_GR_INIT_VALUE.

  CLEAR : ZTIVHST.
  IF SY-LANGU EQ '3'.
  MOVE '입고취소 전기일자' TO SPOP-TITEL.
  ELSE.
  MOVE 'Posting date of G/R cancelation' TO SPOP-TITEL.
  ENDIF.
*  MOVE 'X'            TO RADIO_NONE.
  IF *ZTIVHST-BUDAT IS INITIAL.
    MOVE SY-DATUM    TO *ZTIVHST-BUDAT.
  ENDIF.

  SELECT SINGLE * FROM ZTIV
                  WHERE ZFIVNO EQ IT_SELECTED-ZFIVNO.
  CASE ZTIV-ZFGRST.
    WHEN 'N'.
     IF SY-LANGU EQ '3'.
        MESSAGE E648 WITH IT_SELECTED-ZFIVNO '입고대상'.
     ELSE.
        MESSAGE E648 WITH IT_SELECTED-ZFIVNO 'object to G/R'.
     ENDIF.
    WHEN 'X'.   MESSAGE E555.
    WHEN OTHERS.
  ENDCASE.

  IF ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' OR
     ZTIMIMG00-GRPARTX NE 'X'.
    CLEAR : ZTIVHST.
    SELECT SINGLE * FROM ZTIVHST
           WHERE ZFIVNO  EQ  ZTIV-ZFIVNO
           AND   ZFIVHST EQ  ( SELECT MAX( ZFIVHST )
                                   FROM  ZTIVHST
                                   WHERE ZFIVNO   EQ  ZTIV-ZFIVNO
                                   AND ( CMBLNR   IS  NULL
                                   OR    CMBLNR   EQ  SPACE ) ).
    IF SY-SUBRC NE 0.
      MESSAGE E647.
    ENDIF.
  ELSE.
    SELECT COUNT( * ) INTO W_COUNT
                      FROM ZTIVHST
                      WHERE ZFIVNO  EQ  IT_SELECTED-ZFIVNO
                      AND ( CMBLNR  EQ  SPACE
                      OR    CMBLNR  IS  NULL ).

    CASE W_COUNT.
      WHEN 1.
        SELECT SINGLE * FROM  ZTIVHST
                        WHERE ZFIVNO  EQ  IT_SELECTED-ZFIVNO
                        AND ( CMBLNR  EQ  SPACE
                        OR    CMBLNR  IS  NULL ).
        IF SY-SUBRC NE 0.
          MESSAGE E647.
        ENDIF.
      WHEN 0.
        MESSAGE E647.
      WHEN OTHERS.
        REFRESH : IT_ZSIVHST.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHST
                 FROM ZTIVHST
                 WHERE ZFIVNO  EQ  IT_SELECTED-ZFIVNO
                 AND ( CMBLNR  EQ  SPACE
                 OR    CMBLNR  IS  NULL ).
    ENDCASE.
  ENDIF.

  IF W_COUNT EQ 1.
    CALL SCREEN 0030 STARTING AT 15 3
                     ENDING   AT 56 05.
  ELSEIF W_COUNT GT 1.
    CALL SCREEN 0060 STARTING AT 1  3
                     ENDING   AT 65 15.
  ENDIF.

ENDFORM.                    " P4000_GET_GR_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_VALUE_SCR0010  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_VALUE_SCR0010 INPUT.

  IF W_ZFIVPDT IS INITIAL.
    MESSAGE E167 WITH '증빙일자'.
  ENDIF.

  IF W_ZFIVDDT IS INITIAL.
    MESSAGE E167 WITH '전기일자'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_VALUE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0040 OUTPUT.
  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

  IF OPTION = '1'.
    SET CURSOR FIELD 'SPOP-OPTION1'.
  ELSE.
    SET CURSOR FIELD 'SPOP-OPTION2'.
  ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0040 OUTPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
      IF CANCEL_OPTION = SPACE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCR0040 INPUT.

  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0040 INPUT.

  IF NOT ( SY-UCOMM EQ 'YES' OR SY-UCOMM EQ 'ENTR' ).
    EXIT.
  ENDIF.

  IF ZTIVHST1-CBUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST1' 'CBUDAT'.
  ENDIF.

  IF ZTIVHST1-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST1' 'STGRD'.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0040 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'N'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      ANTWORT = '1'.
  ENDCASE.

  IF  ANTWORT = 'Y' OR  ANTWORT = 'N'.
    SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_IV_CANCEL_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_GET_IV_CANCEL_INIT.

  MOVE 'Initial Value' TO SPOP-TITEL.
*  MOVE 'X'            TO RADIO_NONE.
  IF W_ZFIVPDT IS INITIAL.
    MOVE SY-DATUM    TO W_ZFIVPDT.
  ENDIF.

  CLEAR : ZTIVHST1.
  SELECT SINGLE * FROM ZTIVHST1
         WHERE ZFIVNO   EQ  IT_SELECTED-ZFIVNO
         AND   ZFCIVHST EQ  ( SELECT MAX( ZFCIVHST )
                              FROM  ZTIVHST1
                              WHERE ZFIVNO   EQ  IT_SELECTED-ZFIVNO
                              AND ( CBELNR   IS  NULL
                              OR    CBELNR   EQ  SPACE ) ).

  IF ZTIV-ZFCIVST EQ 'N'.
    MESSAGE E498 WITH IT_SELECTED-ZFIVNO.
  ENDIF.
  IF ZTIV-ZFCIVST EQ 'X'.
    MESSAGE E499 WITH IT_SELECTED-ZFIVNO.
  ENDIF.

  CALL SCREEN 0040 STARTING AT 34 3
                   ENDING   AT 72 14.

ENDFORM.                    " P4000_GET_IV_CANCEL_INIT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0020 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' OR OK-CODE EQ 'DDCL' ).
    EXIT.
  ENDIF.

  IF ZTIVHST-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BUDAT'.
    EXIT.
  ENDIF.
  IF ZTIVHST-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BLDAT'.
    EXIT.
  ENDIF.
  IF ZTIVHST-BWART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BWART'.
    EXIT.
  ENDIF.
  IF ZTIVHST-BWART EQ '511'.
     IF ZTIVHST-GRUND IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'GRUND'.
        EXIT.
     ENDIF.
  ENDIF.

*> 증빙일과 면허일 체크.
  IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
    IF NOT ZTIVHST-BLDAT IS INITIAL.
      IF  ZTIVHST-BLDAT LT W_ZFIDSDT.
        MESSAGE  E560.
      ENDIF.
    ENDIF.
  ENDIF.

  IF ZTIVHST-BWART IS INITIAL.
    MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'. "무환수출입.
      MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
    ENDIF.
    IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환.
      MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
    ENDIF.
  ELSE.
    SELECT SINGLE *
      FROM T156
     WHERE BWART = ZTIVHST-BWART.
    IF SY-SUBRC NE 0.
      MESSAGE E799.
      EXIT.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_CC_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SELECTED_ZFIVNO  text
*      -->P_SY_UCOMM  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_CC_DOC USING    P_ZFIVNO
                                P_TCODE.

  READ TABLE IT_TAB WITH KEY ZFIVNO  =  P_ZFIVNO.
  IF SY-SUBRC EQ 0.
    IF P_TCODE EQ 'ZIM63'.
       SET PARAMETER ID 'ZPBLNO'    FIELD IT_TAB-ZFBLNO.
       SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
       SET PARAMETER ID 'ZPCLSEQ'   FIELD IT_TAB-ZFCLSEQ.
       CALL TRANSACTION 'ZIMCD3' AND SKIP FIRST SCREEN.
    ELSE.
       SET PARAMETER ID 'ZPBLNO'    FIELD IT_TAB-ZFBLNO.
       SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
       SET PARAMETER ID 'ZPCLSEQ'   FIELD IT_TAB-ZFCLSEQ.
       SET PARAMETER ID 'ZPENTNO'   FIELD ''.
       CALL TRANSACTION 'ZIMCC3' AND SKIP FIRST SCREEN.

    ENDIF.
  ELSE.
    MESSAGE S951.
  ENDIF.

ENDFORM.                    " P2000_SHOW_CC_DOC
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0050_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0050_SCR0050 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0050-CURRENT_LINE GT TC_0050-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVHSTIT   INDEX TC_0050-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHSTIT   TO ZSIVHSTIT. " DATA MOVE
    MOVE: IT_ZSIVHSTIT-ZFMARK         TO W_ROW_MARK.  " MARK SET
  ENDIF.

ENDMODULE.                 " IT_TO_TC0050_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0050 OUTPUT.

  DESCRIBE TABLE IT_ZSIVHSTIT LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0050-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0050 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0050-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_GR_MENGE_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_GR_MENGE_SCR0050 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' OR OK-CODE EQ 'DDCL' ).
    EXIT.
  ENDIF.

  READ TABLE IT_ZSIVHSTIT INDEX TC_0050-CURRENT_LINE.
  MOVE: SY-SUBRC TO W_OLD_SUBRC,
        SY-TABIX TO W_TABIX.

*-----------------------------------------------------------------------
*>> 입고 구분자 체크.
  IF ZSIVHSTIT-UMSON EQ 'X'.
    SELECT SINGLE * FROM EKPO
    WHERE  EBELN    EQ   ZSIVHSTIT-EBELN
    AND    EBELP    EQ   ZSIVHSTIT-EBELP.
    IF EKPO-KNTTP IS INITIAL.
       IF ZSIVHSTIT-LGORT IS INITIAL.
          PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVHSTIT' 'LGORT'.
       ENDIF.
    ENDIF.

    IF ZSIVHSTIT-GRMENGE IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSIVHSTIT' 'GRMENGE'.
    ENDIF.
*> 수량체크.
    W_MENGE = ZSIVHSTIT-CCMENGE - ZSIVHSTIT-GRTOTMN.
    IF ZSIVHSTIT-GRMENGE GT W_MENGE.
      PERFORM P2000_NO_INPUT  USING  'ZSIVHSTIT' 'GRMENGE'.
      WRITE :  W_MENGE TO W_AMTTXT1 UNIT ZSIVHSTIT-MEINS.
      W_AMTLEN1 = STRLEN( W_AMTTXT1 ).
      WRITE :ZSIVHSTIT-GRMENGE TO W_AMTTXT2 UNIT ZSIVHSTIT-MEINS .
      W_AMTLEN2 = STRLEN( W_AMTTXT2 ).
      MESSAGE E642 WITH W_AMTTXT2(W_AMTLEN2)  W_AMTTXT1(W_AMTLEN1).
    ENDIF.
  ENDIF.

*  MOVE-CORRESPONDING ZSIVHSTIT TO IT_ZSIVHSTIT.
  MOVE : ZSIVHSTIT-GRMENGE     TO IT_ZSIVHSTIT-GRMENGE,
         ZSIVHSTIT-UMSON       TO IT_ZSIVHSTIT-UMSON,
         ZSIVHSTIT-LGORT       TO IT_ZSIVHSTIT-LGORT,
         ZSIVHSTIT-BATCH       TO IT_ZSIVHSTIT-BATCH,
         ZSIVHSTIT-LICHA       TO IT_ZSIVHSTIT-LICHA.
*  MOVE W_ROW_MARK              TO IT_ZSIVHSTIT-ZFMARK.

  IF W_OLD_SUBRC EQ 0.
    MODIFY IT_ZSIVHSTIT   INDEX TC_0050-CURRENT_LINE.
  ELSE.
    APPEND IT_ZSIVHSTIT.
  ENDIF.

ENDMODULE.                 " CHECK_GR_MENGE_SCR0050  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_NO_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_8167   text
*      -->P_8168   text
*----------------------------------------------------------------------*
FORM P2000_NO_INPUT USING    NOI_TAB NOI_FIELD.

  DATA: BEGIN OF FELD,
          TABLE(10),
          STRICH,
          NAME(10),
        END OF FELD.

  FELD-TABLE = NOI_TAB.
  FELD-STRICH = '-'.
  CONDENSE FELD.
  SEARCH FELD FOR '-'.
  SY-FDPOS = SY-FDPOS - 1.
  WRITE '-' TO FELD+SY-FDPOS.
  SY-FDPOS = SY-FDPOS + 1.
  WRITE NOI_FIELD TO FELD+SY-FDPOS.
  IF SY-STEPL EQ 0.
    SET CURSOR FIELD FELD.
  ELSE.
    SET CURSOR FIELD FELD LINE SY-STEPL.
  ENDIF.

ENDFORM.                    " P2000_NO_INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0060_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0060_SCR0060 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0060-CURRENT_LINE GT TC_0060-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVHST   INDEX TC_0060-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_ZSIVHST   TO ZSIVHST. " DATA MOVE
    MOVE ZTIV-BUKRS                 TO ZTIV-BUKRS.
    MOVE IT_ZSIVHST-ZFMARK          TO W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0060_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0060 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0060-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0060_MARK_TC_0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0060_MARK_TC_0060 INPUT.

  READ TABLE IT_ZSIVHST INDEX TC_0060-CURRENT_LINE.
  IF SY-SUBRC EQ 0.

    IF NOT ( W_ROW_MARK IS INITIAL ).
      IT_ZSIVHST-ZFMARK = 'X'.
    ELSE.
      CLEAR : IT_ZSIVHST-ZFMARK.
    ENDIF.
    MODIFY IT_ZSIVHST  INDEX SY-TABIX.
  ENDIF.

ENDMODULE.                 " SET_SCR0060_MARK_TC_0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.
  CLEAR : OK-CODE.
ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0060 OUTPUT.

  DESCRIBE TABLE IT_ZSIVHST LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0060-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0060  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_KOREAN_TITLE
*&---------------------------------------------------------------------*
FORM P3000_KOREAN_TITLE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60  ' [ 입고 처리 현황 ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 122 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'House B/L No '       NO-GAP,  30 SY-VLINE NO-GAP,
            'B/L 관리No'          NO-GAP,     SY-VLINE NO-GAP,
*            'ETA       '          NO-GAP,     SY-VLINE NO-GAP,
            ' Seq.'               NO-GAP,     SY-VLINE NO-GAP,
            'B/L 발행일'          NO-GAP,     SY-VLINE NO-GAP,
            '대표 플랜트'         NO-GAP,  83 SY-VLINE NO-GAP,
            'POrg'                NO-GAP,     SY-VLINE NO-GAP,
            'Pgp'                 NO-GAP,     SY-VLINE NO-GAP,
            'Vendor '             NO-GAP, 140 SY-VLINE NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            '  통관요청No.'       NO-GAP, SY-VLINE NO-GAP,
            '통관요청일'          NO-GAP, SY-VLINE NO-GAP,
            ' 생성일자 '          NO-GAP, SY-VLINE NO-GAP,
*            '유무환여부'          NO-GAP, SY-VLINE NO-GAP,
            '유/무'               NO-GAP, SY-VLINE NO-GAP,
            ' 통관구분 '          NO-GAP, SY-VLINE NO-GAP,
            ' 통관상태               ' NO-GAP, SY-VLINE NO-GAP.
*           '  통관상태  '        NO-GAP.
* IF ZTIMIMG00-ZFCSTMD EQ 'S'.
*   WRITE: (12) SPACE            NO-GAP, SY-VLINE NO-GAP.
* ELSE.
*   WRITE:  SY-VLINE NO-GAP,
*          '비용배부 YN'         NO-GAP, SY-VLINE NO-GAP.
* ENDIF.
* WRITE:
*           'G/R 상태'            NO-GAP.
* IF ZTIMIMG00-ZFCSTMD EQ 'S'.
*   WRITE: (11) ''               NO-GAP, SY-VLINE NO-GAP.
* ELSE.
*   WRITE: SY-VLINE NO-GAP,
*          '제비용상태'          NO-GAP, SY-VLINE NO-GAP.
* ENDIF.

  WRITE:     'Good Receipt 상 태 '  NO-GAP, SY-VLINE NO-GAP.

  WRITE:
            ' 수입면허번호 '      NO-GAP, 140 SY-VLINE NO-GAP.
*            ' 수입면허번호 '
  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE : /  SY-VLINE, ' ', SY-VLINE,
            (22) ' 자재명 ',  SY-VLINE,
            (25) ' 품명',     SY-VLINE,
            (22) ' 통관수량', SY-VLINE,
            (17) ' 입고수량', SY-VLINE,
            (34) ' 플랜트',   SY-VLINE.
  ENDIF.

  WRITE : / SY-ULINE NO-GAP.


ENDFORM.                    " P3000_KOREAN_TITLE

*&---------------------------------------------------------------------*
*&      Form  P3000_ENGLISH_TITLE
*&---------------------------------------------------------------------*
FORM P3000_ENGLISH_TITLE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60  ' [ G/R handling status ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 122 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'House B/L No '       NO-GAP,  30 SY-VLINE NO-GAP,
            'B/L Doc No'          NO-GAP,     SY-VLINE NO-GAP,
*            'ETA       '          NO-GAP,     SY-VLINE NO-GAP,
            ' Seq.'               NO-GAP,     SY-VLINE NO-GAP,
            'B/L Issue '          NO-GAP,     SY-VLINE NO-GAP,
            'Plant      '         NO-GAP,  83 SY-VLINE NO-GAP,
            'POrg'                NO-GAP,     SY-VLINE NO-GAP,
            'Pgp'                 NO-GAP,     SY-VLINE NO-GAP,
            'Vendor '             NO-GAP, 140 SY-VLINE NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            '  Clr req No.'       NO-GAP, SY-VLINE NO-GAP,
            'Cl req dte'          NO-GAP, SY-VLINE NO-GAP,
            ' Cre date '          NO-GAP, SY-VLINE NO-GAP,
*            '유무환여부'          NO-GAP, SY-VLINE NO-GAP,
            'Monet'               NO-GAP, SY-VLINE NO-GAP,
            'ClearancTY'          NO-GAP, SY-VLINE NO-GAP,
            ' Clearance  status      ' NO-GAP, SY-VLINE NO-GAP.
*           '  통관상태  '        NO-GAP.
* IF ZTIMIMG00-ZFCSTMD EQ 'S'.
*   WRITE: (12) SPACE            NO-GAP, SY-VLINE NO-GAP.
* ELSE.
*   WRITE:  SY-VLINE NO-GAP,
*          '비용배부 YN'         NO-GAP, SY-VLINE NO-GAP.
* ENDIF.

* WRITE:    'G/R 상태'            NO-GAP.
* IF ZTIMIMG00-ZFCSTMD EQ 'S'.
*   WRITE: (11) ''               NO-GAP, SY-VLINE NO-GAP.
* ELSE.
*   WRITE: SY-VLINE NO-GAP,
*          '제비용상태'          NO-GAP, SY-VLINE NO-GAP.
* ENDIF.

  WRITE:    'Good Receipt Status'  NO-GAP, SY-VLINE NO-GAP.
  WRITE:    'Import licence'       NO-GAP, 140 SY-VLINE NO-GAP.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE : /  SY-VLINE, ' ', SY-VLINE,
            (22) ' Item code        ',  SY-VLINE,
            (25) ' Item description',   SY-VLINE,
            (22) ' Cleared quantity',   SY-VLINE,
            (17) ' G/R remn', SY-VLINE,
            (34) '  Plant',   SY-VLINE.
  ENDIF.

  WRITE : / SY-ULINE NO-GAP.


ENDFORM.                    " P3000_ENGLISH_TITLE
*&---------------------------------------------------------------------*
*&      Form  P3000_KOREAN_TEXT
*&---------------------------------------------------------------------*
FORM P3000_KOREAN_TEXT.
*>> 유환/무환 여부 TEXT.
    IF IT_TAB-ZFPOYN EQ 'Y'.
      IT_TAB-ZFPOYN_NM = '유환'.
    ELSEIF IT_TAB-ZFPOYN EQ 'N'.
      IT_TAB-ZFPOYN_NM = '무환'.
    ELSEIF IT_TAB-ZFPOYN EQ 'M'.
      IT_TAB-ZFPOYN_NM = 'Comb.'.
    ENDIF.
*>> 통관 여부 TEXT.
    CASE IT_TAB-ZFCLCD.
      WHEN 'A'.
        IT_TAB-ZFCLCD_NM = '보세운송'.
      WHEN 'B'.
        IT_TAB-ZFCLCD_NM = '과세통관'.
      WHEN 'C'.
        IT_TAB-ZFCLCD_NM = '입항지통관'.
      WHEN 'X'.
        IT_TAB-ZFCLCD_NM = '통관불가'.
      WHEN OTHERS.
        IT_TAB-ZFCLCD_NM = 'None'.
    ENDCASE.
*>> 통관 상태 TEXT.
    CASE IT_TAB-ZFCUST.
      WHEN 'Y'.
        IT_TAB-ZFCUST_NM = '통관완료'.
      WHEN '1'.
        IT_TAB-ZFCUST_NM = '통관생성대상'.
      WHEN '2'.
        IT_TAB-ZFCUST_NM = '통관의뢰대상'.
      WHEN '3'.
        IT_TAB-ZFCUST_NM = '통관 진행 중'.
      WHEN 'N'.
        IT_TAB-ZFCUST_NM = '통관 불가 건'.
    ENDCASE.

*>> 비용배부 상태 TEXT.
    CASE IT_TAB-ZFCDST.
      WHEN 'Y'.
        IT_TAB-ZFCDST_NM = '배부완료'.
      WHEN 'N'.
        IT_TAB-ZFCDST_NM = '배부대상'.
      WHEN 'X'.
        IT_TAB-ZFCDST_NM = '배부불가'.
    ENDCASE.

*>> G/R 상태 TEXT.
    CASE IT_TAB-ZFGRST.
      WHEN 'Y'.
        IT_TAB-ZFGRST_NM = '입고완료'.
      WHEN 'P'.
        IT_TAB-ZFGRST_NM = '분할입고'.
      WHEN 'N'.
        IT_TAB-ZFGRST_NM = '입고대상'.
      WHEN 'X'.
        IT_TAB-ZFGRST_NM = '입고불가'.
    ENDCASE.

*>> CIV 상태 TEXT.
    CASE IT_TAB-ZFCIVST.
      WHEN 'Y'.
        IT_TAB-ZFCIVST_NM = '처리완료'.
      WHEN 'N'.
        IT_TAB-ZFCIVST_NM = '처리대상'.
      WHEN 'X'.
        IT_TAB-ZFCIVST_NM = '처리불가'.
    ENDCASE.

ENDFORM.                    " P3000_KOREAN_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_ENGLISH_TEXT
*&---------------------------------------------------------------------*
FORM P3000_ENGLISH_TEXT.
*>> 유환/무환 여부 TEXT.
    IF IT_TAB-ZFPOYN EQ 'Y'.
      IT_TAB-ZFPOYN_NM = 'Mone'.
    ELSEIF IT_TAB-ZFPOYN EQ 'N'.
      IT_TAB-ZFPOYN_NM = 'Non-'.
    ELSEIF IT_TAB-ZFPOYN EQ 'M'.
      IT_TAB-ZFPOYN_NM = 'Comb.'.
    ENDIF.
*>> 통관 여부 TEXT.
    CASE IT_TAB-ZFCLCD.
      WHEN 'A'.
        IT_TAB-ZFCLCD_NM = 'Bond.Trans'.
      WHEN 'B'.
        IT_TAB-ZFCLCD_NM = 'Taxable '.
      WHEN 'C'.
        IT_TAB-ZFCLCD_NM = 'In Quay   '.
      WHEN 'X'.
        IT_TAB-ZFCLCD_NM = 'Not object'.
      WHEN OTHERS.
        IT_TAB-ZFCLCD_NM = 'None'.
    ENDCASE.
*>> 통관 상태 TEXT.
    CASE IT_TAB-ZFCUST.
      WHEN 'Y'.
        IT_TAB-ZFCUST_NM = 'Completed Clearance'.
      WHEN '1'.
        IT_TAB-ZFCUST_NM = 'Declaration Creation'.
      WHEN '2'.
        IT_TAB-ZFCUST_NM = 'Object to declare'.
      WHEN '3'.
        IT_TAB-ZFCUST_NM = 'in declaring'.
      WHEN 'N'.
        IT_TAB-ZFCUST_NM = 'Not object to clear'.
    ENDCASE.

*>> 비용배부 상태 TEXT.
*    CASE IT_TAB-ZFCDST.
*      WHEN 'Y'.
*        IT_TAB-ZFCDST_NM = '배부완료'.
*      WHEN 'N'.
*        IT_TAB-ZFCDST_NM = '배부대상'.
*      WHEN 'X'.
*        IT_TAB-ZFCDST_NM = '배부불가'.
*    ENDCASE.

*>> G/R 상태 TEXT.
    CASE IT_TAB-ZFGRST.
      WHEN 'Y'.
        IT_TAB-ZFGRST_NM = 'Completed G/R'.
      WHEN 'P'.
        IT_TAB-ZFGRST_NM = 'Partial G/R'.
      WHEN 'N'.
        IT_TAB-ZFGRST_NM = 'Object to G/R'.
      WHEN 'X'.
        IT_TAB-ZFGRST_NM = 'Not object to G/R'.
    ENDCASE.

*>> CIV 상태 TEXT.
*    CASE IT_TAB-ZFCIVST.
*      WHEN 'Y'.
*        IT_TAB-ZFCIVST_NM = '처리완료'.
*      WHEN 'N'.
*        IT_TAB-ZFCIVST_NM = '처리대상'.
*      WHEN 'X'.
*        IT_TAB-ZFCIVST_NM = '처리불가'.
*    ENDCASE.

ENDFORM.                    " P3000_ENGLISH_TEXT
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
