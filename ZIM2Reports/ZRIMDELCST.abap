*&---------------------------------------------------------------------*
*& Report  ZRIMDELCST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : Delivery Cost Posting (LIV Posting)                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.03.12                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Delivery Cost를 조회하여 BAPIs Function으로 Posting함.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMDELCST   MESSAGE-ID ZIM
                     LINE-SIZE 124
                     NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE:  <ICON>.

*>> TABLE DEFINE.
TABLES : ZTIVCD,
         ZTIMIMG00,
         ZTCGCST,
         MKPF,
*>> MESSAGE 출력용.
         BAL_S_DMSG,
         BDCMSGCOLL,
         BAPIRET2,
         ZTBL,            " Bill of Lading
         BSEG,
         COBL,
         ZTCIVHST,
         ZTCIVHD,
         ZTBLCST,         " B/L 비?
         LFA1,            " Vendor Master
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
      ZFCKAMT     LIKE ZTBLCST-ZFCKAMT,
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


*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C,
              ZFIMDNO       LIKE ZTIVCD-ZFIMDNO,
              ZFIMDTY       LIKE ZTIVCD-ZFIMDTY,
              ZFCLSEQ       LIKE ZTIVCD-ZFCLSEQ,
              ZFCSQ         LIKE ZTIVCD-ZFCSQ.
DATA : END OF IT_ERR_LIST.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

*-----------------------------------------------------------------------
* INTERNAL TABLE DEFINE.
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFHBLNO(16)     TYPE   C,                " House B/L No
       ZFIMDTY         LIKE   ZTIVCD-ZFIMDTY,   " 문서번호.
       ZFIMDNO         LIKE   ZTBLCST-ZFBLNO,   " 문서관리번호.
       ZFCLSEQ         LIKE   ZTCUCLIV-ZFCLSEQ, ">통관순번.
       ZFCSQ           LIKE   ZTBLCST-ZFCSQ,    " 비용순번.
       ZFCSCD          LIKE   ZTBLCST-ZFCSCD,   " 비용구분.
       ZFCDNM          LIKE   ZTIMIMG08-ZFCDNM, " 비용명.
       ZFCAMT          LIKE   ZTBLCST-ZFCAMT,   " 비용금액.
       WAERS           LIKE   ZTBLCST-WAERS,    " 통화.
       BUKRS           LIKE   ZTBLCST-BUKRS,    " 회사코드.
       ZFCKAMT         LIKE   ZTBLCST-ZFCKAMT,  " 비용원화금액_유환.
       KRW             LIKE   ZTBLCST-KRW,      " 원화통화.
       ZFEXRT          LIKE   ZTBLCST-ZFEXRT,   " Exchange Rate
       ZFVAT           LIKE   ZTBLCST-ZFVAT,    " V.A.T
       ZFOCDT          LIKE   ZTBLCST-ZFOCDT,   " 지급일.
       ZFVEN           LIKE   ZTBLCST-ZFVEN,    " Vendor
       ZFVEN_NM        LIKE   LFA1-NAME1,       " Vendor 명.
       ZFPAY           LIKE   ZTBLCST-ZFPAY,    " 지불처.
       ZFPAY_NM        LIKE   LFA1-NAME1,       " 지불처명.
       ZTERM           LIKE   ZTBLCST-ZTERM,    " Terms of Payment
       MWSKZ           LIKE   ZTBLCST-MWSKZ,    " Tax Code
       ZFWERKS         LIKE   ZTBLCST-ZFWERKS,  " Plant
       ZFPOYN          LIKE   ZTBL-ZFPOYN,      " 유환여부.
       SUM_MARK        TYPE   C.
DATA : END OF IT_TAB.


*>> 비용코드 Internal Table.
DATA : IT_ZTRECST       LIKE ZTRECST     OCCURS 0 WITH HEADER LINE,
       IT_ZTBLCST       LIKE ZTBLCST     OCCURS 0 WITH HEADER LINE,
       IT_ZTCGCST       LIKE ZTCGCST     OCCURS 0 WITH HEADER LINE,
       IT_ZTCUCLCST     LIKE ZTCUCLCST   OCCURS 0 WITH HEADER LINE.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_IMDTY  FOR ZTIVCD-ZFIMDTY     ">문서종류.
                                OBLIGATORY
                                NO-EXTENSION
                                NO INTERVALS,
                   S_VEN    FOR ZTBLCST-ZFVEN,     " Vendor
                   S_PAY    FOR ZTBLCST-ZFPAY,     " 지불처.
                   S_ZTERM  FOR ZTBLCST-ZTERM,     " Terms Of Payment
                   S_MWSKZ  FOR ZTBLCST-MWSKZ,     " Tax Code
                   S_WERKS  FOR ZTBLCST-ZFWERKS,   " Plant
                   S_OCDT   FOR ZTBLCST-ZFOCDT,    " 지불일자.
                   S_CSCD   FOR ZTBLCST-ZFCSCD     " 비용구분.
                                NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.


AT SELECTION-SCREEN ON S_IMDTY.
   PERFORM   P2000_COST_CODE_CHECK.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
   PERFORM   P2000_PAYMENT_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
   PERFORM   P2000_PAYMENT_TERM_HELP  USING  S_ZTERM-HIGH.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Import System Configuration Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 비용코드 SELECT.
  PERFORM   P1000_GET_COST_CODE    USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_COST_DATA   USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE      USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.


*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'POST'.                  " 비용처리 Posting
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM P4000_COST_POST         USING W_ERR_CHK.
            DESCRIBE  TABLE IT_ERR_LIST     LINES  W_LINE.
            IF W_LINE GT 0.
               INCLUDE = 'POPU'.
               CALL SCREEN 0100 STARTING AT  05   3
                                ENDING   AT  100 12.
               CLEAR : INCLUDE.
            ENDIF.
            PERFORM   P1000_READ_COST_DATA  USING W_ERR_CHK.
            MESSAGE S826 WITH W_PROC_CNT.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
            PERFORM   P1000_READ_COST_DATA   USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
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
   ENDCASE.

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
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

   SET  TITLEBAR 'ZIMV2'.           " GUI TITLE SETTING..

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  CASE S_IMDTY-LOW.
     WHEN 'RD'.
        WRITE : /47  '[ 수입의뢰 비용 POSTING ]'
                     COLOR COL_HEADING INTENSIFIED OFF.
     WHEN 'BL'.
        WRITE : /50  '[ B/L 비용 POSTING ]'
                     COLOR COL_HEADING INTENSIFIED OFF.
     WHEN 'CW'.
        WRITE : /50  '[ 하역 비용 POSTING ]'
                     COLOR COL_HEADING INTENSIFIED OFF.
     WHEN 'CC'.
        WRITE : /50  '[ 통관 비용 POSTING ]'
                     COLOR COL_HEADING INTENSIFIED OFF.
  ENDCASE.
  WRITE : / 'Date : ', SY-DATUM, 106 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            '비용구분'                       NO-GAP, 37 SY-VLINE NO-GAP,
            '          발생금액      '       NO-GAP, SY-VLINE NO-GAP,
            '             V.A.T '            NO-GAP, SY-VLINE NO-GAP,
            '지급일    '                     NO-GAP, SY-VLINE NO-GAP,
            'Vendor                        ' NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                           SY-VLINE NO-GAP,
            '문서관리No'                     NO-GAP, SY-VLINE NO-GAP,
            'Seq  '                          NO-GAP, SY-VLINE NO-GAP,
            '환율          '                 NO-GAP, SY-VLINE NO-GAP,
            '          원화금액      '       NO-GAP, SY-VLINE NO-GAP,
            'Comp/Term/Tax/Plant/유환'       NO-GAP,
            '      '                         NO-GAP, SY-VLINE NO-GAP,
            '지불처                        ' NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMV2'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMV2'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
*      W_LINE = W_LINE + 1.
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
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCSCD           NO-GAP,             " 비용구분.
       ' '                     NO-GAP,
       IT_TAB-ZFCDNM(27)       NO-GAP,             " 비용명칭.
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCAMT CURRENCY IT_TAB-WAERS NO-GAP, " 비용금액.
       IT_TAB-WAERS            NO-GAP,             " 금액통화.
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVAT CURRENCY IT_TAB-KRW NO-GAP, " V.A.T
       SY-VLINE                NO-GAP,
       IT_TAB-ZFOCDT           NO-GAP,             " 지급일자.
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVEN            NO-GAP,             " Vendor
       IT_TAB-ZFVEN_NM(20)     NO-GAP,             " Vendor 명.
       SY-VLINE                NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFIMDNO          NO-GAP,               " 문서 관리번호.
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCSQ            NO-GAP,               " 비용순번.
       SY-VLINE                NO-GAP,
       '  '                    NO-GAP,
       IT_TAB-ZFEXRT           NO-GAP,               " Exchange Rate
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT  CURRENCY IT_TAB-KRW NO-GAP, " 비용원화금액.
       IT_TAB-KRW              NO-GAP,              " 원화통화.
       SY-VLINE                NO-GAP,
       IT_TAB-BUKRS            NO-GAP,              " Company code.
       '/'                     NO-GAP,
       IT_TAB-ZTERM            NO-GAP,              " Terms Of Payment
       '/'                     NO-GAP,
       IT_TAB-MWSKZ            NO-GAP,              " Tax Code
       ' /'                    NO-GAP,
       IT_TAB-ZFWERKS          NO-GAP,              " Plant
       ' / '                   NO-GAP,
       IT_TAB-ZFPOYN           NO-GAP,              " 유/무환 구분.
       '        '              NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFPAY            NO-GAP,              " 지불처.
       IT_TAB-ZFPAY_NM(20)     NO-GAP,              " 지불처 명.
       SY-VLINE                NO-GAP.

* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
*  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / '총', W_COUNT, '건'.
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
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_COST_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_COST_DATA USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.

  CASE S_IMDTY-LOW.
*>> 수입의뢰 비용.
     WHEN 'RD'.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTRECST
                 FROM  ZTRECST
                 WHERE ZFCSCD    IN S_CSCD
                 AND ZFVEN       IN S_VEN
                 AND ZFPAY       IN S_PAY
                 AND ZTERM       IN S_ZTERM
                 AND MWSKZ       IN S_MWSKZ
                 AND ZFWERKS     IN S_WERKS
                 AND ZFOCDT      IN S_OCDT
                 AND ZFACDO      EQ SPACE
                 AND ZFCAMT      NE 0.
*                 AND ZFACDO      IS NULL.
     WHEN 'BL'.
*>> B/L 비용.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLCST
                 FROM  ZTBLCST
                 WHERE ZFCSCD    IN S_CSCD
                 AND ZFVEN       IN S_VEN
                 AND ZFPAY       IN S_PAY
                 AND ZTERM       IN S_ZTERM
                 AND MWSKZ       IN S_MWSKZ
                 AND ZFWERKS     IN S_WERKS
                 AND ZFOCDT      IN S_OCDT
                 AND ZFACDO      EQ SPACE
                 AND ZFCAMT      NE 0.
*                 AND ZFACDO      IS NULL.
     WHEN 'CW'.
*>> 하역 비용.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCGCST
                 FROM  ZTCGCST
                 WHERE ZFCSCD    IN S_CSCD
                 AND LIFNR       IN S_VEN
                 AND ZFPAY       IN S_PAY
                 AND ZTERM       IN S_ZTERM
                 AND MWSKZ       IN S_MWSKZ
                 AND WERKS       IN S_WERKS
                 AND ZFOCDT      IN S_OCDT
                 AND BELNR       EQ SPACE
                 AND ZFCKAMT     NE 0.
*                 AND BELNR       IS NULL.
     WHEN 'CC'.
*>> 통관 비용.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCUCLCST
                 FROM  ZTCUCLCST
                 WHERE ZFCSCD    IN S_CSCD
                 AND ZFVEN       IN S_VEN
                 AND ZFPAY       IN S_PAY
                 AND ZTERM       IN S_ZTERM
                 AND MWSKZ       IN S_MWSKZ
                 AND ZFWERKS     IN S_WERKS
                 AND ZFOCDT      IN S_OCDT
                 AND ZFCAMT      NE 0
*                 AND ZFACDO      IS NULL.
                 AND ZFACDO      EQ SPACE.
     WHEN OTHERS.
  ENDCASE.

  IF SY-SUBRC NE 0.
     MESSAGE S738.     MOVE 'Y' TO W_ERR_CHK.   EXIT.
  ENDIF.

  CASE S_IMDTY-LOW.
*>> 수입의뢰 비용.
     WHEN 'RD'.
        LOOP AT IT_ZTRECST.
           CLEAR : IT_TAB.
           MOVE-CORRESPONDING   IT_ZTRECST   TO   IT_TAB.
           MOVE: IT_ZTRECST-ZFREQNO          TO   IT_TAB-ZFIMDNO,
                 'RD'                        TO   IT_TAB-ZFIMDTY.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFVEN_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFVEN.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFPAY_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFPAY.

           READ TABLE IT_ZTIMIMG08 WITH KEY ZFCDTY = '003'
                                            ZFCD   = IT_TAB-ZFCSCD.
           IF SY-SUBRC EQ 0.
              MOVE: IT_ZTIMIMG08-ZFCDNM         TO   IT_TAB-ZFCDNM.
           ELSE.
              CLEAR : IT_TAB-ZFCDNM.
           ENDIF.

           APPEND IT_TAB.
        ENDLOOP.
*>> B/L 비용.
     WHEN 'BL'.
        LOOP AT IT_ZTBLCST.
           CLEAR : IT_TAB.
           MOVE-CORRESPONDING   IT_ZTBLCST   TO   IT_TAB.
           MOVE: IT_ZTBLCST-ZFBLNO           TO   IT_TAB-ZFIMDNO,
                 'BL'                        TO   IT_TAB-ZFIMDTY.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFVEN_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFVEN.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFPAY_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFPAY.
           IF IT_TAB-ZFCSQ GT '10000'.
              READ TABLE IT_ZTIMIMG08 WITH KEY ZFCDTY = '004'
                                               ZFCD   = IT_TAB-ZFCSCD.
           ELSE.
              READ TABLE IT_ZTIMIMG08 WITH KEY ZFCDTY = '005'
                                               ZFCD   = IT_TAB-ZFCSCD.
           ENDIF.

           IF SY-SUBRC EQ 0.
              MOVE: IT_ZTIMIMG08-ZFCDNM         TO   IT_TAB-ZFCDNM.
           ELSE.
              CLEAR : IT_TAB-ZFCDNM.
           ENDIF.

           SELECT SINGLE ZFPOYN INTO IT_TAB-ZFPOYN
                  FROM ZTBL
                  WHERE ZFBLNO  EQ  IT_TAB-ZFIMDNO.
           IF IT_TAB-ZFPOYN EQ 'N'.
              CONTINUE.
           ENDIF.
           APPEND IT_TAB.
        ENDLOOP.
*>> 하역 비용.
     WHEN 'CW'.
        LOOP AT IT_ZTCGCST.
           CLEAR : IT_TAB.
           MOVE-CORRESPONDING   IT_ZTCGCST   TO   IT_TAB.
           MOVE: IT_ZTCGCST-ZFCGNO           TO   IT_TAB-ZFIMDNO,
                 'CW'                        TO   IT_TAB-ZFIMDTY,
                 IT_ZTCGCST-LIFNR            TO   IT_TAB-ZFVEN.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFVEN_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFVEN.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFPAY_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFPAY.

           READ TABLE IT_ZTIMIMG08 WITH KEY ZFCDTY = '007'
                                            ZFCD   = IT_TAB-ZFCSCD.
           IF SY-SUBRC EQ 0.
              MOVE: IT_ZTIMIMG08-ZFCDNM         TO   IT_TAB-ZFCDNM.
           ELSE.
              CLEAR : IT_TAB-ZFCDNM.
           ENDIF.
           MOVE : 'KRW'         TO   IT_TAB-WAERS.
           APPEND IT_TAB.
        ENDLOOP.
*>> 통관 비용.
     WHEN 'CC'.
        LOOP AT IT_ZTCUCLCST.
           CLEAR : IT_TAB.
           MOVE-CORRESPONDING   IT_ZTCUCLCST TO   IT_TAB.
           MOVE: IT_ZTCUCLCST-ZFBLNO         TO   IT_TAB-ZFIMDNO,
                 IT_ZTCUCLCST-ZFCLSEQ        TO   IT_TAB-ZFCLSEQ,
                 'CC'                        TO   IT_TAB-ZFIMDTY.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFVEN_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFVEN.

           SELECT SINGLE NAME1  INTO IT_TAB-ZFPAY_NM
                  FROM   LFA1
                  WHERE  LIFNR   EQ  IT_TAB-ZFPAY.

           READ TABLE IT_ZTIMIMG08 WITH KEY ZFCDTY = '006'
                                            ZFCD   = IT_TAB-ZFCSCD.
           IF SY-SUBRC EQ 0.
              MOVE: IT_ZTIMIMG08-ZFCDNM         TO   IT_TAB-ZFCDNM.
           ELSE.
              CLEAR : IT_TAB-ZFCDNM.
           ENDIF.
           MOVE : 'KRW'         TO   IT_TAB-WAERS,
                  'KRW'         TO   IT_TAB-KRW.

           SELECT SINGLE ZFPOYN INTO IT_TAB-ZFPOYN
                  FROM ZTBL
                  WHERE ZFBLNO  EQ  IT_TAB-ZFIMDNO.
           IF IT_TAB-ZFPOYN EQ 'N'.
              CONTINUE.
           ENDIF.

           APPEND IT_TAB.
        ENDLOOP.

     WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P1000_READ_COST_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX       TYPE P,
        ZFVEN       LIKE ZTBLCST-ZFVEN,
        ZFPAY       LIKE ZTBLCST-ZFPAY,
        ZTERM       LIKE ZTBLCST-ZTERM,
        MWSKZ       LIKE ZTBLCST-MWSKZ,
        ZFWERKS     LIKE ZTBLCST-ZFWERKS,
        ZFCKAMT     LIKE ZTBLCST-ZFCKAMT,
        ZFVAT       LIKE ZTBLCST-ZFVAT,
        ZFIMDTY     LIKE ZTIVCD-ZFIMDTY,
        ZFIMDNO     LIKE ZTIVCD-ZFIMDNO,
        ZFCLSEQ     LIKE ZTIVCD-ZFCLSEQ,
        ZFCSQ       LIKE ZTIVCD-ZFCSQ.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX       TO INDEX,
         IT_TAB-ZFIMDTY     TO ZFIMDTY,
         IT_TAB-ZFIMDNO     TO ZFIMDNO,
         IT_TAB-ZFCLSEQ     TO ZFCLSEQ,
         IT_TAB-ZFCSQ       TO ZFCSQ,
         IT_TAB-ZFVEN       TO ZFVEN,
         IT_TAB-ZFPAY       TO ZFPAY,
         IT_TAB-ZTERM       TO ZTERM,
         IT_TAB-MWSKZ       TO MWSKZ,
         IT_TAB-ZFWERKS     TO ZFWERKS,
         IT_TAB-ZFCKAMT     TO ZFCKAMT,
         IT_TAB-ZFVAT       TO ZFVAT.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX       TO INDEX,
                IT_TAB-ZFVEN       TO IT_SELECTED-ZFVEN,
                IT_TAB-ZFPAY       TO IT_SELECTED-ZFPAY,
                IT_TAB-ZTERM       TO IT_SELECTED-ZTERM,
                IT_TAB-MWSKZ       TO IT_SELECTED-MWSKZ,
                IT_TAB-ZFWERKS     TO IT_SELECTED-ZFWERKS,
                IT_TAB-ZFCKAMT     TO IT_SELECTED-ZFCKAMT,
                IT_TAB-ZFCSQ       TO IT_SELECTED-ZFCSQ,
                IT_TAB-ZFIMDTY     TO IT_SELECTED-ZFIMDTY,
                IT_TAB-ZFIMDNO     TO IT_SELECTED-ZFIMDNO,
                IT_TAB-ZFCLSEQ     TO IT_SELECTED-ZFCLSEQ,
                IT_TAB-ZFVAT       TO IT_SELECTED-ZFVAT.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P4000_COST_POST
*&---------------------------------------------------------------------*
FORM P4000_COST_POST USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR: W_PROC_CNT, W_ERR_CNT.
  PERFORM P4000_GET_INIVAL.
  CHECK OK-CODE EQ 'YES'.

  REFRESH : IT_ERR_LIST.
  LOOP AT IT_SELECTED.

       CALL FUNCTION 'ZIM_BAPI_COST_INVOICE_CREATE'
            EXPORTING
               P_ZFIMDTY       =     IT_SELECTED-ZFIMDTY
               P_ZFREQNO       =     IT_SELECTED-ZFIMDNO
               P_ZFCLSEQ       =     IT_SELECTED-ZFCLSEQ
               P_ZFCSQ         =     IT_SELECTED-ZFCSQ
               P_CHG_MODE      =     'X'
               P_DOC_TYPE      =     'RE'
               I_INVOICE       =     'X'
               I_CREDITMEMO    =      SPACE
               P_BLDAT         =      MKPF-BLDAT
               P_BUDAT         =      MKPF-BUDAT
            IMPORTING
               INVOICEDOCNUMBER =     ZFACDO
               FISCALYEAR       =     ZFFIYR
            TABLES
               RETURN           =     RETURN
            EXCEPTIONS
               LIV_ERROR        =     4.

       IF SY-SUBRC NE 0.           ">> 오류 발생시...
          IF RETURN[] IS INITIAL.
             PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
          ELSE.
             PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
          ENDIF.
          ADD    1    TO    W_ERR_CNT.
       ELSE.
          ADD 1       TO W_PROC_CNT.
       ENDIF.
  ENDLOOP.

ENDFORM.                    " P4000_COST_POST

*&---------------------------------------------------------------------*
*&      Form  P4000_GET_INIVAL
*&---------------------------------------------------------------------*
FORM P4000_GET_INIVAL.

  MOVE 'Initial Value' TO SPOP-TITEL.
*  MOVE 'X'             TO RADIO_NONE.
  IF MKPF-BLDAT IS INITIAL.
     MOVE SY-DATUM    TO MKPF-BLDAT.
  ENDIF.
  IF MKPF-BUDAT IS INITIAL.
     MOVE SY-DATUM    TO MKPF-BUDAT.
  ENDIF.

  CALL SCREEN 0010 STARTING AT 15 1
                   ENDING   AT 52 6.

ENDFORM.                    " P4000_GET_INIVAL

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

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT


*&---------------------------------------------------------------------*
*&      Form  P1000_GET_COST_CODE
*&---------------------------------------------------------------------*
FORM P1000_GET_COST_CODE USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

*> 수입 LIV TYPE.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
     MESSAGE S963.      W_ERR_CHK = 'Y'.    EXIT.
  ELSE.
     IF ZTIMIMG00-ZFCSTMD EQ 'I'.
        MESSAGE S983.      W_ERR_CHK = 'Y'.    EXIT.
     ENDIF.
  ENDIF.

*> 계정코드 GET.
  SELECT SINGLE * FROM ZTIMIMG11.
  IF SY-SUBRC NE 0.
     MESSAGE S961.      W_ERR_CHK = 'Y'.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_COST_CODE
*&---------------------------------------------------------------------*
*&      Form  P2000_COST_CODE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_COST_CODE_CHECK.

RANGES : R_ZFCDTY FOR ZTIMIMG08-ZFCDTY  OCCURS 5.

   CLEAR : R_ZFCDTY.
   CASE S_IMDTY-LOW.
      WHEN 'RD'.
         MOVE : 'I'       TO    R_ZFCDTY-SIGN,
                'EQ'      TO    R_ZFCDTY-OPTION,
                '003'     TO    R_ZFCDTY-LOW,
                SPACE     TO    R_ZFCDTY-HIGH.
         APPEND  R_ZFCDTY.
      WHEN 'BL'.
         MOVE : 'I'       TO    R_ZFCDTY-SIGN,
                'EQ'      TO    R_ZFCDTY-OPTION,
                '004'     TO    R_ZFCDTY-LOW,
                SPACE     TO    R_ZFCDTY-HIGH.
         APPEND  R_ZFCDTY.
         MOVE : 'I'       TO    R_ZFCDTY-SIGN,
                'EQ'      TO    R_ZFCDTY-OPTION,
                '005'     TO    R_ZFCDTY-LOW,
                SPACE     TO    R_ZFCDTY-HIGH.
         APPEND  R_ZFCDTY.
      WHEN 'CW'.
         MOVE : 'I'       TO    R_ZFCDTY-SIGN,
                'EQ'      TO    R_ZFCDTY-OPTION,
                '007'     TO    R_ZFCDTY-LOW,
                SPACE     TO    R_ZFCDTY-HIGH.
         APPEND  R_ZFCDTY.
      WHEN 'CC'.
         MOVE : 'I'       TO    R_ZFCDTY-SIGN,
                'EQ'      TO    R_ZFCDTY-OPTION,
                '006'     TO    R_ZFCDTY-LOW,
                SPACE     TO    R_ZFCDTY-HIGH.
         APPEND  R_ZFCDTY.
      WHEN OTHERS.
         MESSAGE E985 WITH S_IMDTY-LOW.
   ENDCASE.

*> 비용코드 Get.
   SELECT * INTO TABLE IT_ZTIMIMG08
                 FROM  ZTIMIMG08
                 WHERE ZFCDTY      IN R_ZFCDTY
                 AND   ZFCD1       EQ 'Y'
                 AND   COND_TYPE   IS NOT NULL.

   IF SY-SUBRC NE 0.
      MESSAGE E984.     W_ERR_CHK = 'Y'.    EXIT.
   ENDIF.

   LOOP AT IT_ZTIMIMG08 WHERE COND_TYPE NE SPACE.
      MOVE : 'I'                 TO      S_CSCD-SIGN,
             'EQ'                TO      S_CSCD-OPTION,
             IT_ZTIMIMG08-ZFCD   TO      S_CSCD-LOW,
             SPACE               TO      S_CSCD-HIGH.
      APPEND S_CSCD.
   ENDLOOP.

ENDFORM.                    " P2000_COST_CODE_CHECK
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF MKPF-BLDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'MKPF' 'BLDAT'.
  ENDIF.

  IF MKPF-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'MKPF' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

   MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
          SY-MSGID            TO     IT_ERR_LIST-MSGID,
          SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
          SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
          SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
          SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
          SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
          IT_SELECTED-ZFIMDTY TO    IT_ERR_LIST-ZFIMDTY,
          IT_SELECTED-ZFIMDNO TO    IT_ERR_LIST-ZFIMDNO,
          IT_SELECTED-ZFCLSEQ TO    IT_ERR_LIST-ZFCLSEQ,
          IT_SELECTED-ZFCSQ   TO    IT_ERR_LIST-ZFCSQ.

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
             IT_SELECTED-ZFIMDTY TO     IT_ERR_LIST-ZFIMDTY,
             IT_SELECTED-ZFIMDNO TO     IT_ERR_LIST-ZFIMDNO,
             IT_SELECTED-ZFCLSEQ TO     IT_ERR_LIST-ZFCLSEQ,
             IT_SELECTED-ZFCSQ   TO     IT_ERR_LIST-ZFCSQ.

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
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '오류 LIST'.
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
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
                   ' 문서번호 ' NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 105 SY-VLINE NO-GAP,
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
                      SY-VLINE NO-GAP, IT_ERR_LIST-ZFIMDNO  NO-GAP,
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
*&      Form  P2000_PAYMENT_TERM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZTERM_HIGH  text
*----------------------------------------------------------------------*
FORM P2000_PAYMENT_TERM_HELP USING    P_ZTERM.

   TABLES : T052.

   CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = P_ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.


  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
     P_ZTERM = T052-ZTERM.
  ENDIF.

ENDFORM.                    " P2000_PAYMENT_TERM_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFPSMS NE 1.
     W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
