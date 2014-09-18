*&---------------------------------------------------------------------*
*& Report  ZRIMCSTCNC                                                  *
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
REPORT  ZRIMCSTCNC   MESSAGE-ID ZIM
                     LINE-SIZE 105
                     NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE:  <ICON>.

*>> TABLE DEFINE.
TABLES : ZTIVCD,
         ZTIMIMG00,
         ZTCGCST,
         UF05A,
         BSIS,
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
      IMDTY(2)    TYPE C,
      BUKRS       LIKE ZTBLCST-BUKRS,
      ZFFIYR      LIKE ZTBLCST-ZFFIYR,
      ZFACDO      LIKE ZTBLCST-ZFACDO,
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
       SV_GUBN           TYPE C,
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
              BUKRS         LIKE ZTBLCST-BUKRS,
              ZFFIYR        LIKE ZTBLCST-ZFFIYR,
              ZFACDO        LIKE ZTBLCST-ZFACDO.
DATA : END OF IT_ERR_LIST.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

*-----------------------------------------------------------------------
* INTERNAL TABLE DEFINE.
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       GUBN(1)         TYPE   C,
       IMDTY(2)        TYPE   C,
       CSTNM(12)       TYPE   C,
       BUKRS           LIKE   ZTBLCST-BUKRS,
       ZFFIYR          LIKE   ZTBLCST-ZFFIYR,
       ZFACDO          LIKE   ZTBLCST-ZFACDO,
       ZFCKAMT         LIKE   ZTBLCST-ZFCKAMT,
       ZFVAT           LIKE   ZTBLCST-ZFVAT,
       ZFPSDT          LIKE   ZTBLCST-ZFPSDT,
       ZFVEN           LIKE   ZTBLCST-ZFVEN,
       ZFPAY           LIKE   ZTBLCST-ZFPAY,
       ZFVEN_NM(20)    TYPE   C,
       ZFPAY_NM(20)    TYPE   C,
       ZTERM           LIKE   ZTBLCST-ZTERM,
       MWSKZ           LIKE   ZTBLCST-MWSKZ,
       ZFWERKS         LIKE   ZTBLCST-ZFWERKS.
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
                   S_BUKRS  FOR ZTBLCST-BUKRS,
                   S_YEAR   FOR ZTBLCST-ZFFIYR,
                   S_ACDO   FOR ZTBLCST-ZFACDO,
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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
   PERFORM   P2000_PAYMENT_TERM_HELP  USING  S_ZTERM-HIGH.

AT SELECTION-SCREEN ON HELP-REQUEST FOR S_ZTERM-LOW.
   PERFORM   P2000_PAYMENT_TERM_HELP  USING  S_ZTERM-LOW.

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
            PERFORM   P1000_READ_COST_DATA USING W_ERR_CHK.
            MESSAGE S826 WITH W_PROC_CNT.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'DPDO'.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES GT 1.
               MESSAGE S965.
               EXIT.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM  P2000_FI_DOC_DISPLAY   USING IT_SELECTED-ZFACDO
                                                  IT_SELECTED-BUKRS
                                                  IT_SELECTED-ZFFIYR.

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
  WRITE : /50  '[ FI Document Cancel ]'
           COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',                   SY-VLINE NO-GAP,
            '비용구분    '                   NO-GAP, SY-VLINE NO-GAP,
            '회사'                           NO-GAP, SY-VLINE NO-GAP,
            '문서번호       '                NO-GAP, SY-VLINE NO-GAP,
            '          비용금액 '            NO-GAP, SY-VLINE NO-GAP,
            'Post Date     '                 NO-GAP, SY-VLINE NO-GAP,
            'Vendor                        ' NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                           SY-VLINE NO-GAP,
            '             '                  NO-GAP,
            '     '                          NO-GAP,
            '               '                NO-GAP, SY-VLINE NO-GAP,
            '             V.A.T '            NO-GAP, SY-VLINE NO-GAP,
            'Term/Tax/Plant'                 NO-GAP, SY-VLINE NO-GAP,
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
       IT_TAB-CSTNM            NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-BUKRS            NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFFIYR           NO-GAP,
       '-'                     NO-GAP,
       IT_TAB-ZFACDO           NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFCKAMT CURRENCY 'KRW'            NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFPSDT           NO-GAP,
       '    '                  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFVEN            NO-GAP,
       IT_TAB-ZFVEN_NM         NO-GAP, SY-VLINE NO-GAP.
* Hide
       HIDE: IT_TAB.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       '             '         NO-GAP,               " 문서 관리번호.
       '     '                 NO-GAP,
       '               '       NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFVAT CURRENCY 'KRW' NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZTERM            NO-GAP,
       '/'                     NO-GAP,
       IT_TAB-MWSKZ            NO-GAP,
       ' '                     NO-GAP,
       '/'                     NO-GAP,
       IT_TAB-ZFWERKS          NO-GAP,
       ' '                     NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFPAY            NO-GAP,
       IT_TAB-ZFPAY_NM         NO-GAP, SY-VLINE NO-GAP.

* Stored value...
  HIDE: IT_TAB.
  W_COUNT = W_COUNT  +  1.
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
          SELECT  BUKRS  ZFFIYR  ZFACDO SUM( ZFCKAMT ) AS ZFCKAMT
                  SUM( ZFVAT ) AS ZFVAT MAX( ZFPSDT )  AS ZFPSDT
                  MAX( MWSKZ ) AS MWSKZ MAX( ZFWERKS ) AS ZFWERKS
                  MAX( ZTERM ) AS ZTERM MAX( ZFVEN )   AS ZFVEN
                  MAX( ZFPAY ) AS ZFPAY
          INTO    CORRESPONDING FIELDS OF TABLE IT_ZTRECST
          FROM    ZTRECST
          WHERE   BUKRS            IN  S_BUKRS
          AND     ZFFIYR           IN  S_YEAR
          AND     ZFACDO           IN  S_ACDO
          AND     ZFVEN            IN  S_VEN
          AND     ZFPAY            IN  S_PAY
          AND     ZTERM            IN  S_ZTERM
          AND     MWSKZ            IN  S_MWSKZ
          AND     ZFWERKS          IN  S_WERKS
          AND     ZFOCDT           IN  S_OCDT
          AND     ZFACDO           NE  SPACE
          AND     ZFFIYR           NE  SPACE
          GROUP BY
                BUKRS  ZFFIYR  ZFACDO.
*>>  BL 비용.
     WHEN 'BL'.
          SELECT  BUKRS  ZFFIYR  ZFACDO SUM( ZFCKAMT ) AS ZFCKAMT
                  SUM( ZFVAT ) AS ZFVAT MAX( ZFPSDT )  AS ZFPSDT
                  MAX( MWSKZ ) AS MWSKZ MAX( ZFWERKS ) AS ZFWERKS
                  MAX( ZTERM ) AS ZTERM MAX( ZFVEN )   AS ZFVEN
                  MAX( ZFPAY ) AS ZFPAY
          INTO    CORRESPONDING FIELDS OF TABLE IT_ZTBLCST
          FROM    ZTBLCST
          WHERE   BUKRS            IN  S_BUKRS
          AND     ZFFIYR           IN  S_YEAR
          AND     ZFACDO           IN  S_ACDO
          AND     ZFVEN            IN  S_VEN
          AND     ZFPAY            IN  S_PAY
          AND     ZTERM            IN  S_ZTERM
          AND     MWSKZ            IN  S_MWSKZ
          AND     ZFWERKS          IN  S_WERKS
          AND     ZFOCDT           IN  S_OCDT
          AND     ZFACDO           NE  SPACE
          AND     ZFFIYR           NE  SPACE
          GROUP BY
                  BUKRS  ZFFIYR  ZFACDO.
*>> 하역비용.
     WHEN 'CW'.
          SELECT  BUKRS  GJAHR BELNR    SUM( ZFCKAMT ) AS ZFCKAMT
                  SUM( ZFVAT ) AS ZFVAT MAX( ZFPSDT )  AS ZFPSDT
                  MAX( MWSKZ ) AS MWSKZ MAX( WERKS )   AS WERKS
                  MAX( ZTERM ) AS ZTERM MAX( LIFNR )   AS LIFNR
                  MAX( ZFPAY ) AS ZFPAY
          INTO    CORRESPONDING FIELDS OF TABLE IT_ZTCGCST
          FROM    ZTCGCST
          WHERE   BUKRS            IN  S_BUKRS
          AND     GJAHR            IN  S_YEAR
          AND     BELNR            IN  S_ACDO
          AND     LIFNR            IN  S_VEN
          AND     ZFPAY            IN  S_PAY
          AND     ZTERM            IN  S_ZTERM
          AND     MWSKZ            IN  S_MWSKZ
          AND     WERKS            IN  S_WERKS
          AND     ZFOCDT           IN  S_OCDT
          AND     BELNR            NE  SPACE
          AND     GJAHR            NE  SPACE
          GROUP BY
                 BUKRS  GJAHR  BELNR.
     WHEN 'CC'.
*>> 통관비용
          SELECT  BUKRS  ZFFIYR  ZFACDO SUM( ZFCKAMT ) AS ZFCKAMT
                  SUM( ZFVAT ) AS ZFVAT MAX( ZFPSDT )  AS ZFPSDT
                  MAX( MWSKZ ) AS MWSKZ MAX( ZFWERKS ) AS ZFWERKS
                  MAX( ZTERM ) AS ZTERM MAX( ZFVEN )   AS ZFVEN
                  MAX( ZFPAY ) AS ZFPAY
          INTO    CORRESPONDING FIELDS OF TABLE IT_ZTCUCLCST
          FROM    ZTCUCLCST
          WHERE   BUKRS            IN  S_BUKRS
          AND     ZFFIYR           IN  S_YEAR
          AND     ZFACDO           IN  S_ACDO
          AND     ZFVEN            IN  S_VEN
          AND     ZFPAY            IN  S_PAY
          AND     ZTERM            IN  S_ZTERM
          AND     MWSKZ            IN  S_MWSKZ
          AND     ZFWERKS          IN  S_WERKS
          AND     ZFOCDT           IN  S_OCDT
          AND     ZFACDO           NE  SPACE
          AND     ZFFIYR           NE  SPACE
          GROUP BY
                  BUKRS  ZFFIYR  ZFACDO.
     WHEN OTHERS.
  ENDCASE.

  IF SY-SUBRC NE 0.
     MESSAGE S738.     MOVE 'Y' TO W_ERR_CHK.   EXIT.
  ENDIF.

*>> INTERNAL TABLE APPEND.
  CASE S_IMDTY-LOW.
*>> 수입의뢰 비용.
     WHEN 'RD'.
          LOOP  AT  IT_ZTRECST.
*>> 구매처, 지불처 SELECT
               SELECT  SINGLE NAME1  INTO  IT_TAB-ZFVEN_NM
               FROM    LFA1          WHERE LIFNR  =  IT_ZTRECST-ZFVEN.

               SELECT  SINGLE NAME1  INTO  IT_TAB-ZFPAY_NM
               FROM    LFA1          WHERE LIFNR  =  IT_ZTRECST-ZFPAY.

               MOVE  :  '1'                 TO  IT_TAB-GUBN,
                        'RD'                TO  IT_TAB-IMDTY,
                        '수입의뢰비용'      TO  IT_TAB-CSTNM,
                        IT_ZTRECST-BUKRS    TO  IT_TAB-BUKRS,
                        IT_ZTRECST-ZFFIYR   TO  IT_TAB-ZFFIYR,
                        IT_ZTRECST-ZFACDO   TO  IT_TAB-ZFACDO,
                        IT_ZTRECST-ZFCKAMT  TO  IT_TAB-ZFCKAMT,
                        IT_ZTRECST-ZFVAT    TO  IT_TAB-ZFVAT,
                        IT_ZTRECST-ZFPSDT   TO  IT_TAB-ZFPSDT,
                        IT_ZTRECST-ZFVEN    TO  IT_TAB-ZFVEN,
                        IT_ZTRECST-ZFPAY    TO  IT_TAB-ZFPAY,
                        IT_ZTRECST-ZTERM    TO  IT_TAB-ZTERM,
                        IT_ZTRECST-MWSKZ    TO  IT_TAB-MWSKZ,
                        IT_ZTRECST-ZFWERKS  TO  IT_TAB-ZFWERKS.
               APPEND   IT_TAB.
            ENDLOOP.
     WHEN 'BL'.
          LOOP  AT  IT_ZTBLCST.
*>> 구매처, 지불처 SELECT
               SELECT  SINGLE NAME1  INTO  IT_TAB-ZFVEN_NM
               FROM    LFA1          WHERE LIFNR  =  IT_ZTBLCST-ZFVEN.

               SELECT  SINGLE NAME1  INTO  IT_TAB-ZFPAY_NM
               FROM    LFA1          WHERE LIFNR  =  IT_ZTBLCST-ZFPAY.

               MOVE  :  '2'                 TO  IT_TAB-GUBN,
                        'BL 비용'           TO  IT_TAB-CSTNM,
                        'BL'                TO  IT_TAB-IMDTY,
                        IT_ZTBLCST-BUKRS    TO  IT_TAB-BUKRS,
                        IT_ZTBLCST-ZFFIYR   TO  IT_TAB-ZFFIYR,
                        IT_ZTBLCST-ZFACDO   TO  IT_TAB-ZFACDO,
                        IT_ZTBLCST-ZFCKAMT  TO  IT_TAB-ZFCKAMT,
                        IT_ZTBLCST-ZFVAT    TO  IT_TAB-ZFVAT,
                        IT_ZTBLCST-ZFPSDT   TO  IT_TAB-ZFPSDT,
                        IT_ZTBLCST-ZFVEN    TO  IT_TAB-ZFVEN,
                        IT_ZTBLCST-ZFPAY    TO  IT_TAB-ZFPAY,
                        IT_ZTBLCST-ZTERM    TO  IT_TAB-ZTERM,
                        IT_ZTBLCST-MWSKZ    TO  IT_TAB-MWSKZ,
                        IT_ZTBLCST-ZFWERKS  TO  IT_TAB-ZFWERKS.
               APPEND   IT_TAB.
          ENDLOOP.
     WHEN 'CW'.
          LOOP  AT  IT_ZTCGCST.
*>> 구매처, 지불처 SELECT
            SELECT  SINGLE NAME1  INTO  IT_TAB-ZFVEN_NM
            FROM    LFA1          WHERE LIFNR  =  IT_ZTCGCST-LIFNR.

            SELECT  SINGLE NAME1  INTO  IT_TAB-ZFPAY_NM
            FROM    LFA1          WHERE LIFNR  =  IT_ZTCGCST-ZFPAY.

            MOVE  :  '3'                 TO  IT_TAB-GUBN,
                     'CW'                TO  IT_TAB-IMDTY,
                     '하역 비용'         TO  IT_TAB-CSTNM,
                     IT_ZTCGCST-BUKRS    TO  IT_TAB-BUKRS,
                     IT_ZTCGCST-GJAHR    TO  IT_TAB-ZFFIYR,
                     IT_ZTCGCST-BELNR    TO  IT_TAB-ZFACDO,
                     IT_ZTCGCST-ZFCKAMT  TO  IT_TAB-ZFCKAMT,
                     IT_ZTCGCST-ZFVAT    TO  IT_TAB-ZFVAT,
                     IT_ZTCGCST-ZFPSDT   TO  IT_TAB-ZFPSDT,
                     IT_ZTCGCST-LIFNR    TO  IT_TAB-ZFVEN,
                     IT_ZTCGCST-ZFPAY    TO  IT_TAB-ZFPAY,
                     IT_ZTCGCST-ZTERM    TO  IT_TAB-ZTERM,
                     IT_ZTCGCST-MWSKZ    TO  IT_TAB-MWSKZ,
                     IT_ZTCGCST-WERKS    TO  IT_TAB-ZFWERKS.
             APPEND  IT_TAB.
          ENDLOOP.
     WHEN 'CC'.
          LOOP  AT  IT_ZTCUCLCST.
*>> 구매처, 지불처 SELECT
            SELECT  SINGLE NAME1  INTO  IT_TAB-ZFVEN_NM
            FROM    LFA1          WHERE LIFNR  =  IT_ZTCUCLCST-ZFVEN.

            SELECT  SINGLE NAME1  INTO  IT_TAB-ZFPAY_NM
            FROM    LFA1          WHERE LIFNR  =  IT_ZTCUCLCST-ZFPAY.

            MOVE  :  '4'                   TO  IT_TAB-GUBN,
                     'CC'                  TO  IT_TAB-IMDTY,
                     '통관 비용'           TO  IT_TAB-CSTNM,
                     IT_ZTCUCLCST-BUKRS    TO  IT_TAB-BUKRS,
                     IT_ZTCUCLCST-ZFFIYR   TO  IT_TAB-ZFFIYR,
                     IT_ZTCUCLCST-ZFACDO   TO  IT_TAB-ZFACDO,
                     IT_ZTCUCLCST-ZFCKAMT  TO  IT_TAB-ZFCKAMT,
                     IT_ZTCUCLCST-ZFVAT    TO  IT_TAB-ZFVAT,
                     IT_ZTCUCLCST-ZFPSDT   TO  IT_TAB-ZFPSDT,
                     IT_ZTCUCLCST-ZFVEN    TO  IT_TAB-ZFVEN,
                     IT_ZTCUCLCST-ZFPAY    TO  IT_TAB-ZFPAY,
                     IT_ZTCUCLCST-ZTERM    TO  IT_TAB-ZTERM,
                     IT_ZTCUCLCST-MWSKZ    TO  IT_TAB-MWSKZ,
                     IT_ZTCUCLCST-ZFWERKS  TO  IT_TAB-ZFWERKS.
            APPEND   IT_TAB.
        ENDLOOP.
  ENDCASE.

  SORT  IT_TAB  BY  BUKRS  ZFFIYR  ZFACDO.
  DESCRIBE  TABLE IT_TAB  LINES  W_LINE.
  IF W_LINE < 1.
     MESSAGE  S738.
     MOVE 'N' TO W_ERR_CHK.
  ENDIF.

ENDFORM.                    " P1000_READ_COST_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : IT_TAB-IMDTY       TO IT_SELECTED-IMDTY,
                IT_TAB-BUKRS       TO IT_SELECTED-BUKRS,
                IT_TAB-ZFFIYR      TO IT_SELECTED-ZFFIYR,
                IT_TAB-ZFACDO      TO IT_SELECTED-ZFACDO.
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
  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
     EXIT.
  ENDIF.

  REFRESH : IT_ERR_LIST.
  LOOP AT IT_SELECTED.

       CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
            EXPORTING
              P_ZFIMDTY        =     IT_SELECTED-IMDTY
              P_BUKRS          =     IT_SELECTED-BUKRS
              INVOICEDOCNUMBER =     IT_SELECTED-ZFACDO
              FISCALYEAR       =     IT_SELECTED-ZFFIYR
              REASONREVERSAL   =     UF05A-STGRD
              POSTINGDATE      =     BSIS-BUDAT
           IMPORTING
              INVOICEDOCNUMBER_REVERSAL =     ZFACDO
              FISCALYEAR_REVERSAL       =     ZFFIYR
           TABLES
              RETURN           =     RETURN
           EXCEPTIONS
              LIV_ERROR        =     4.

       IF SY-SUBRC NE 0.           ">> 오류 발생시...
*          IF RETURN[] IS INITIAL.
*            PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
*         ELSE.
             PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
*         ENDIF.
          ADD    1    TO    W_ERR_CNT.
       ELSE.
          PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
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
  IF BSIS-BUDAT IS INITIAL.
     MOVE SY-DATUM    TO BSIS-BUDAT.
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
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF UF05A-STGRD IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

   MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
          RETURN-ID           TO     IT_ERR_LIST-MSGID,
          RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
          RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
          RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
          RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
          RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
          IT_SELECTED-BUKRS   TO     IT_ERR_LIST-BUKRS,
          IT_SELECTED-ZFFIYR  TO     IT_ERR_LIST-ZFFIYR,
          IT_SELECTED-ZFACDO  TO     IT_ERR_LIST-ZFACDO.

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
             IT_SELECTED-BUKRS   TO     IT_ERR_LIST-BUKRS,
             IT_SELECTED-ZFFIYR  TO     IT_ERR_LIST-ZFFIYR,
             IT_SELECTED-ZFACDO  TO     IT_ERR_LIST-ZFACDO.

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
         WRITE : / SY-ULINE(105), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
                   ' 문서번호 ' NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 103 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(105).
*         MESSAGE
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)   NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-ZFACDO      NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(85) NO-GAP,
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
         WRITE : / SY-ULINE(105).
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
*&      Form  P2000_FI_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_ZFACDO  text
*      -->P_IT_TAB_BUKRS  text
*      -->P_IT_TAB_ZFFIYR  text
*----------------------------------------------------------------------*
FORM P2000_FI_DOC_DISPLAY USING    P_BELNR
                                   P_BUKRS
                                   P_GJAHR.
  IF P_BELNR IS INITIAL.
     MESSAGE S814.
     EXIT.
  ENDIF.
  IF P_BUKRS IS INITIAL.
     MESSAGE S167 WITH '회사코드'.
     EXIT.
  ENDIF.
  IF P_GJAHR IS INITIAL.
     MESSAGE S167 WITH '회계년도'.
     EXIT.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD P_BELNR.
  SET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SET PARAMETER ID 'GJR' FIELD P_GJAHR.

  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_FI_DOC_DISPLAY
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

ENDFORM.                    " P2000_COST_CODE_CHECK
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
