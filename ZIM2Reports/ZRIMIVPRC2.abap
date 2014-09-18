*&---------------------------------------------------------------------*
*& Report  ZRIMIVPRC2                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 물대 Invoice Verification                             *
*&      작성자 : 나신호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.23                                            *
*&    적용회사 : 엘지화학                                              *
*&---------------------------------------------------------------------*
*&   DESC.     : 물대 Invoice Verification 자료 출력위한 레포트.       *
*&---------------------------------------------------------------------*
*& [변경내용]  :                                                       *
*&---------------------------------------------------------------------*
REPORT  ZRIMIVPRC2   MESSAGE-ID ZIM NO STANDARD PAGE HEADING
                     LINE-SIZE 137.
INCLUDE : <ICON>,
           ZRIMBDCCOM.

TABLES : ZTCIVHD,    " Commercial Invoice Header Table..
         ZTCIVIT,    " Commercial Invoice Items Table..
         ZTCIVHST,
         LFA1,       " 구매처마스터 (일반섹션) Table..
         SPOP,
         ZTIMIMG00,
         ZTIMIMG11,
         ZTBKPF,
         BKPF,
         RBKP.

DATA : W_TABIX  LIKE   SY-TABIX,
       W_LFA1   LIKE   LFA1,
       W_LFA1_1 LIKE   LFA1.

*>> FUNCTION CALL 한후 RETURN 되는 내역.
DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF IT_ZSCIVIT OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   ZSCIVIT.
DATA:   END   OF IT_ZSCIVIT.

*>> SELECTED 된 FILE COPY
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFCIVRN     LIKE ZTCIVHD-ZFCIVRN,
      ZFCIVNO     LIKE ZTCIVHD-ZFCIVNO,
END OF IT_SELECTED.

*>>> Commercial Invoice 관련자료를 읽기위한 Internal Table 선언..
DATA: BEGIN OF IT_CIV OCCURS 1000,
        ZTERM      LIKE  ZTCIVHD-ZTERM,   " 지급조건.
        ZFCIVRN    LIKE  ZTCIVHD-ZFCIVRN, " Commercial Invoice 관리번호.
        ZFCIVNO    LIKE  ZTCIVHD-ZFCIVNO, " Commercial Invoice Number..
        ZFPRPYN    LIKE  ZTCIVHD-ZFPRPYN, " 선급금여부..
        ZFPRTE     LIKE  ZTCIVHD-ZFPRTE,  " 선급금비율..
        ZFPOYN     LIKE  ZTCIVHD-ZFPOYN,  " 유환여부..
        ZFIVST     LIKE  ZTCIVHD-ZFIVST,  " Invoice Verify 상태..
        ZFMAVN     LIKE  ZTCIVHD-ZFMAVN,  " 물대 거래처코드..
        ZFOPBN     LIKE  ZTCIVHD-ZFOPBN,  " 개설은행 거래처코드..
        ZFIVAMP    LIKE  ZTCIVHD-ZFIVAMP, " Invoice 처리금액..
        ZFIVAMC    LIKE  ZTCIVHD-ZFIVAMC, " Invoice 처리통화..
        ZFIVAMK    LIKE  ZTCIVHD-ZFIVAMK, " Invoice 금액(원화)..
        ZFEXRT     LIKE  ZTCIVHD-ZFEXRT,  " 환율..
        ZFREQNO    LIKE  ZTCIVIT-ZFREQNO, " 수입의뢰 관리번호..
        ZFBLNO     LIKE  ZTCIVIT-ZFBLNO,  " B/L 관리번호..
        EBELN      LIKE  ZTCIVIT-EBELN,   " 구매문서번호..
        BUDAT      LIKE  ZTCIVHD-BUDAT,   " 전기일.
        ZFCIDT     LIKE  ZTCIVHD-ZFCIDT,  " 송장일자.
      END OF IT_CIV.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C,
              ZFCIVRN       LIKE ZTCIVHD-ZFCIVRN.
DATA : END OF IT_ERR_LIST.

*>>> HIDE를 위한 구조체 선언..
DATA: BEGIN OF DOCU,
        TYPE   TYPE  C,
        CODE   LIKE  ZTCIVHD-ZFCIVRN,
        CIVN   LIKE  ZTCIVHD-ZFCIVNO,
        IVST   LIKE  ZTCIVHD-ZFIVST,
        BELNR  LIKE  ZTCIVHST-BELNR,             " NCW 추가
      END OF DOCU.

DATA : HEADERDATA        LIKE    BAPI_INCINV_CREATE_HEADER,
       I_INVOICE         LIKE    RBKP-XRECH,
       I_CREDITMEMO      LIKE    RBKP-XRECH,
       INVOICEDOCNUMBER  LIKE    BAPI_INCINV_FLD-INV_DOC_NO,
       FISCALYEAR        LIKE    BAPI_INCINV_FLD-FISC_YEAR.

DATA: W_ERR_CHK         TYPE C,
      MARKFIELD         TYPE C,
      PRP1              TYPE C,
      PRP2              TYPE C,
      IVST1             TYPE C,
      IVST2             TYPE C,
      W_COUNT           TYPE I,
      W_SELECTED_LINES  TYPE I,
      W_LINE            TYPE I,
      W_PROC_CNT        TYPE I,
      W_MOD             TYPE I,
      W_ERR_CNT         TYPE I,
      INCLUDE(8)        TYPE C,
      W_LIST_INDEX      LIKE SY-TABIX,
      OK-CODE           LIKE SY-UCOMM,
      W_OK_CODE         LIKE SY-UCOMM,
      W_STGRD           LIKE ZTCIVHST-STGRD,
      OPTION(1)         TYPE C,
      ANTWORT(1)        TYPE C,
      CANCEL_OPTION     TYPE C,
      TEXTLEN           TYPE I,
      RADIO_NONE(1)     TYPE C,
      RADIO_ALL(1)      TYPE C,
      RADIO_ERROR(1)    TYPE C,
      DISPMODE(1)       TYPE C,
      P_BUKRS           LIKE ZTREQHD-BUKRS.

*-----------------------------------------------------------------------
* 검색조건 Selection Window.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS:
               S_BUKRS   FOR ZTCIVHD-BUKRS,
               S_EBELN   FOR ZTCIVIT-EBELN,
               S_ZTERM   FOR ZTCIVHD-ZTERM,
               S_REQNO   FOR ZTCIVIT-ZFREQNO,
               S_BLNO    FOR ZTCIVIT-ZFBLNO,
               S_CIVNO   FOR ZTCIVHD-ZFCIVNO,
               S_CIVRN   FOR ZTCIVHD-ZFCIVRN,
               S_BUDAT   FOR ZTCIVHD-BUDAT,   " 전표전기일.
               S_CIDT  FOR ZTCIVHD-ZFCIDT.  " 송장일자.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
*>>> 선급금 여부..
   SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-003, POSITION 1.

      SELECTION-SCREEN : COMMENT 36(3) TEXT-010, POSITION 40.
      PARAMETERS : P_Y   AS CHECKBOX.               " Yes.

      SELECTION-SCREEN : COMMENT 51(2) TEXT-011, POSITION 54.
      PARAMETERS : P_N   AS CHECKBOX DEFAULT 'X'.   " No.

   SELECTION-SCREEN END OF LINE.

*>>> Verify 상태..
   SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-004, POSITION 1.

      SELECTION-SCREEN : COMMENT 36(3) TEXT-010, POSITION 40.
      PARAMETERS : P_VY   AS CHECKBOX.              " Yes.

      SELECTION-SCREEN : COMMENT 51(2) TEXT-011, POSITION 54.
      PARAMETERS : P_VN   AS CHECKBOX DEFAULT 'X'.  " No.

   SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
  PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

*-----------------------------------------------------------------------
* INITIALIZATION.
*-----------------------------------------------------------------------
INITIALIZATION.

   PERFORM  P1000_SET_BUKRS.
   SET TITLEBAR 'TIT1'.

*-----------------------------------------------------------------------
* TOP-OF-PAGE.
*-----------------------------------------------------------------------
TOP-OF-PAGE.
   IF  INCLUDE  NE 'POPU'.
       PERFORM P3000_TITLE_WRITE.
   ENDIF.

*-----------------------------------------------------------------------
* START-OF-SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

   PERFORM P1000_READ_CIV_DATA.
   CHECK W_ERR_CHK EQ 'N'.

*-----------------------------------------------------------------------
* END-OF-SELECTION.
*-----------------------------------------------------------------------
END-OF-SELECTION.
   CHECK W_ERR_CHK EQ 'N'.
   SET TITLEBAR 'TIT1'.
   SET PF-STATUS 'ZIM55'.

   PERFORM P3000_WRITE_CIV_DATA.
*-----------------------------------------------------------------------
* AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
      WHEN 'IV'  OR 'CM' OR 'MIR2'.      " 부대비용 계산.
            MOVE  SY-UCOMM   TO   W_OK_CODE.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               PERFORM P4000_IV_PROCESS      USING  W_ERR_CHK.
               IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
                  EXIT.
               ENDIF.

               DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
               IF W_LINE GT 0.
                  INCLUDE = 'POPU'.
                  CALL SCREEN 0100 STARTING AT  05   3
                                   ENDING   AT  100 12.
                  CLEAR : INCLUDE.

                  PERFORM   P1000_READ_CIV_DATA.
                  IF W_PROC_CNT EQ 0.
                     MESSAGE S919.
                  ELSE.
                     IF W_OK_CODE = 'IV'.
                        MESSAGE  S486  WITH  W_PROC_CNT.
                     ELSEIF W_OK_CODE = 'CM'.
                        MESSAGE  S487  WITH  W_PROC_CNT.
                     ELSE.
                        MESSAGE  S488  WITH  W_PROC_CNT.
                     ENDIF.
                  ENDIF.

                  IF W_ERR_CHK EQ 'Y'.
                     LEAVE TO SCREEN 0.
                  ELSE.
                     PERFORM RESET_LIST.
                  ENDIF.
               ENDIF.
            ELSEIF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ELSE.
               MESSAGE S965.
            ENDIF.
      WHEN 'CIV'.
         IF DOCU IS INITIAL.
            MESSAGE S962.
            EXIT.
         ELSE.
            SET PARAMETER ID 'ZPCIVRN' FIELD DOCU-CODE.
            SET PARAMETER ID 'ZPCIVNO' FIELD ''.
            CALL TRANSACTION 'ZIM37' AND SKIP FIRST SCREEN.

            PERFORM P1000_READ_CIV_DATA.
            PERFORM RESET_LIST.
         ENDIF.
      WHEN 'MDDP'.                         " NCW
        DATA : L_AWKEY       LIKE   BKPF-AWKEY,
               W_NUMBER(10)  TYPE   C.
        CLEAR : L_AWKEY, W_NUMBER.
        CLEAR : ZTCIVHST.

         IF DOCU IS INITIAL.
            MESSAGE S962.
            EXIT.
         ELSE.
* FI Document
*             MOVE : ZTCIVHST-BELNR   TO   L_AWKEY(10),
*                    ZTCIVHST-BUKRS   TO   L_AWKEY+10(4),
*                    ZTCIVHST-GJAHR   TO   L_AWKEY+10(4).

*            CLEAR : BKPF.
*            SELECT * FROM BKPF UP TO 1 ROWS
*             WHERE AWKEY  EQ  L_AWKEY.
*            ENDSELECT.

*            IF SY-SUBRC EQ 0.
*               SET  PARAMETER ID  'BUK'   FIELD   BKPF-BUKRS.
*               SET  PARAMETER ID  'GJR'   FIELD   BKPF-GJAHR.
*               SET  PARAMETER ID  'BLN'   FIELD   BKPF-BELNR.
*               CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*            ENDIF.

            SELECT MAX( ZFCIVHST ) INTO W_NUMBER
              FROM ZTCIVHST
             WHERE ZFCIVRN = DOCU-CODE.

            SELECT SINGLE *
              FROM ZTCIVHST
             WHERE ZFCIVRN = DOCU-CODE
               AND ZFCIVHST = W_NUMBER.

            IF SY-SUBRC EQ 0.
               SET  PARAMETER ID  'RBN'   FIELD   ZTCIVHST-BELNR.
               SET  PARAMETER ID  'GJR'   FIELD   ZTCIVHST-GJAHR.
               CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
            ELSE.
               MESSAGE I977 WITH 'Does not exist.'.
            ENDIF.

            PERFORM P1000_READ_CIV_DATA.
            PERFORM RESET_LIST.
         ENDIF.
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
   CLEAR: DOCU.
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
*&      Form  P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_CIV_DATA.

   W_ERR_CHK  =  'N'.

   IF P_Y EQ 'X'.
      PRP1 = 'Y'.
   ENDIF.

   IF P_N EQ 'X'.
      PRP2 = 'N'.
   ENDIF.

   IF P_VY EQ 'X'.
      IVST1 = 'Y'.
   ENDIF.
   IF P_VN EQ 'X'.
      IVST2 = 'N'.
   ENDIF.

   IF P_Y = ' ' AND P_N = ' '.
      W_ERR_CHK = 'Y'.
      MESSAGE S650.
      EXIT.
   ENDIF.

   IF P_VY = ' ' AND P_VN = ' '.
      W_ERR_CHK = 'Y'.
      MESSAGE S651.
      EXIT.
   ENDIF.

   W_ERR_CHK = 'N'.
   SELECT H~ZFCIVRN AS ZFCIVRN          MAX( H~ZFCIVNO ) AS ZFCIVNO
          MAX( H~ZFPRPYN ) AS ZFPRPYN   MAX( H~ZFPRTE )  AS ZFPRTE
          MAX( H~ZFPOYN )  AS ZFPOYN    MAX( H~ZFIVST )  AS ZFIVST
          MAX( H~ZFMAVN )  AS ZFMAVN    MAX( H~ZFOPBN )  AS ZFOPBN
          MAX( H~ZFIVAMP ) AS ZFIVAMP   MAX( H~ZFIVAMC ) AS ZFIVAMC
          MAX( H~ZFIVAMK ) AS ZFIVAMK   MAX( H~ZFEXRT )  AS ZFEXRT
          MAX( I~ZFREQNO ) AS ZFREQNO   MAX( I~ZFBLNO )  AS ZFBLNO
          MAX( I~EBELN )   AS EBELN     MAX( H~ZTERM  )  AS ZTERM
          MAX( H~BUDAT )   AS BUDAT     MAX( H~ZFCIDT )  AS ZFCIDT
     INTO CORRESPONDING FIELDS OF TABLE IT_CIV
          FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
          ON  H~ZFCIVRN   EQ  I~ZFCIVRN
     WHERE  H~ZFCIVRN  IN  S_CIVRN
     AND    H~ZFCIVNO  IN  S_CIVNO
     AND    I~EBELN    IN  S_EBELN
     AND    I~ZFBLNO   IN  S_REQNO
     AND    I~ZFREQNO  IN  S_BLNO
     AND    H~ZTERM    IN  S_ZTERM
     AND    H~BUDAT    IN  S_BUDAT
     AND    H~ZFCIDT   IN  S_CIDT
     AND  ( H~ZFPRPYN  EQ  PRP1
     OR     H~ZFPRPYN  EQ  PRP2 )
     AND  ( H~ZFIVST   EQ  IVST1
     OR     H~ZFIVST   EQ  IVST2 )
     GROUP BY
            H~ZFCIVRN.

   IF SY-SUBRC NE 0.
      W_ERR_CHK = 'Y'.
      MESSAGE S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P1000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
FORM P3000_WRITE_CIV_DATA.

   SORT  IT_CIV  BY  EBELN  ZFCIVRN.

   LOOP AT IT_CIV.

      ON CHANGE OF IT_CIV-ZFMAVN.
         CLEAR : W_LFA1.
         SELECT SINGLE * INTO W_LFA1
                FROM LFA1 WHERE LIFNR = IT_CIV-ZFMAVN.
      ENDON.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      WRITE: / SY-VLINE NO-GAP, '  ' NO-GAP,
               MARKFIELD  AS CHECKBOX, ' ' NO-GAP,
             7 SY-VLINE NO-GAP, IT_CIV-EBELN   NO-GAP,
            18 SY-VLINE NO-GAP, IT_CIV-ZFCIVRN NO-GAP,
            46 SY-VLINE NO-GAP, W_LFA1-NAME1 NO-GAP.

      ON CHANGE OF IT_CIV-ZFMAVN.
         CLEAR : W_LFA1_1.
         SELECT SINGLE * INTO W_LFA1_1
                FROM LFA1 WHERE LIFNR = IT_CIV-ZFOPBN.
      ENDON.

      WRITE:   SY-VLINE NO-GAP, IT_CIV-ZFIVAMC NO-GAP,
               IT_CIV-ZFIVAMP CURRENCY IT_CIV-ZFIVAMC NO-GAP,
               115 SY-VLINE NO-GAP.

      IF IT_CIV-ZFIVST EQ 'Y'.
         WRITE: '   Yes    ' NO-GAP COLOR COL_HEADING INTENSIFIED OFF,
                 SY-VLINE NO-GAP.
      ELSEIF IT_CIV-ZFIVST EQ 'N'.
         WRITE: '   No     ' NO-GAP COLOR COL_NEGATIVE INTENSIFIED OFF,
                 SY-VLINE NO-GAP.
      ENDIF.

      WRITE : IT_CIV-BUDAT NO-GAP,   " 전기일.
            137 SY-VLINE NO-GAP.


      PERFORM P2000_HIDE_VAR_MOVE
              USING 'CIV' IT_CIV-ZFCIVRN IT_CIV-ZFCIVNO IT_CIV-ZFIVST.

      FORMAT INTENSIFIED OFF.
      WRITE: / SY-VLINE NO-GAP,
             7 SY-VLINE NO-GAP, IT_CIV-ZTERM CENTERED  NO-GAP,
            18 SY-VLINE NO-GAP, IT_CIV-ZFCIVNO NO-GAP,
            46 SY-VLINE NO-GAP, W_LFA1_1-NAME1 NO-GAP,
            82 SY-VLINE NO-GAP,
               IT_CIV-ZFIVAMK CURRENCY 'KRW' NO-GAP,
               SY-VLINE NO-GAP, IT_CIV-ZFEXRT NO-GAP,
           115 SY-VLINE NO-GAP.

*>>> 선급금 여부 Check.
      IF IT_CIV-ZFPRPYN EQ 'Y'.
         WRITE:  '   Yes    ' NO-GAP,
              126 SY-VLINE NO-GAP.
      ELSEIF IT_CIV-ZFPRPYN EQ 'N'.
         WRITE:  '   No     ' NO-GAP,
              126 SY-VLINE NO-GAP.
      ENDIF.

      WRITE : IT_CIV-ZFCIDT NO-GAP,
              137 SY-VLINE.

      PERFORM P2000_HIDE_VAR_MOVE
              USING 'CIV' IT_CIV-ZFCIVRN IT_CIV-ZFCIVNO IT_CIV-ZFIVST.
      WRITE : SY-ULINE.
   ENDLOOP.

ENDFORM.                    " P3000_WRITE_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

   SKIP.
   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE 50 '[Gross Price Invoice Verification]' NO-GAP
             COLOR COL_HEADING INTENSIFIED ON.

   SKIP.
   WRITE: 120 'DATE : ' NO-GAP,
               SY-DATUM(4) NO-GAP, '/' NO-GAP,
               SY-DATUM+4(2) NO-GAP, '/' NO-GAP,
               SY-DATUM+6(2) NO-GAP.

   ULINE.
   FORMAT COLOR COL_HEADING INTENSIFIED ON.
   WRITE: / SY-VLINE NO-GAP, 'Check'                       NO-GAP,
            SY-VLINE NO-GAP, ' P/O Doc. '                  NO-GAP,
            SY-VLINE NO-GAP, 'Gross Price Document Number' NO-GAP,
            SY-VLINE NO-GAP, 'Benificiary'                 NO-GAP,
         82 SY-VLINE NO-GAP, ' Invoice Amount '            NO-GAP,
        115 SY-VLINE NO-GAP, 'Verify St '                  NO-GAP,
            SY-VLINE NO-GAP, 'PostingDat'                  NO-GAP,
        137 SY-VLINE.

   FORMAT COLOR COL_HEADING INTENSIFIED OFF.
   WRITE: / SY-VLINE NO-GAP,
          7 SY-VLINE NO-GAP, ' Pay Term '                  NO-GAP,
         18 SY-VLINE NO-GAP, 'Commercial Invoice No.'      NO-GAP,
         46 SY-VLINE NO-GAP, 'OpenBank'                    NO-GAP,
         82 SY-VLINE NO-GAP, 'Amount(Local)'               NO-GAP,
        102 SY-VLINE NO-GAP, 'ExchangeRate'                NO-GAP,
        115 SY-VLINE NO-GAP, 'Pre-Paymnt'                  NO-GAP,
            SY-VLINE NO-GAP, 'InvoiceDat'                  NO-GAP,
        137 SY-VLINE NO-GAP,
            SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_HIDE_VAR_MOVE
*&---------------------------------------------------------------------*
FORM P2000_HIDE_VAR_MOVE USING   P_TYPE P_CODE P_CIVN P_IVST.

CLEAR :  DOCU.

MOVE: P_TYPE    TO   DOCU-TYPE,
      P_CODE    TO   DOCU-CODE,
      P_CIVN    TO   DOCU-CIVN,
      P_IVST    TO   DOCU-IVST.

HIDE  :  DOCU.
CLEAR :  DOCU.

ENDFORM.                    " P2000_HIDE_VAR_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX    TYPE P,
        ZFCIVRN  LIKE ZTCIVHD-ZFCIVRN.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR   W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_CIV-ZFCIVRN  TO ZFCIVRN.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                DOCU-CODE  TO IT_SELECTED-ZFCIVRN,
                DOCU-CIVN  TO IT_SELECTED-ZFCIVNO.
         IF  W_OK_CODE = 'IV'.
             IF  DOCU-IVST  = 'Y'.
                 MESSAGE  E489  WITH  DOCU-CODE.
             ENDIF.
         ELSEIF  W_OK_CODE  = 'CM'.
             IF  DOCU-IVST  =  'N'.
                 MESSAGE  E490  WITH  DOCU-CODE.
             ENDIF.
         ELSE.
             IF  DOCU-IVST  =  'N'.
                 MESSAGE  E490  WITH  DOCU-CODE.
             ENDIF.
         ENDIF.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P4000_IV_PROCESS
*&---------------------------------------------------------------------*
FORM P4000_IV_PROCESS USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR   : W_PROC_CNT, W_ERR_CNT.
  REFRESH : IT_ERR_LIST.
  PERFORM  P5000_MIRO_MESSAGE.
  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
     EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
*-----------------------------------------------------------------------
*>> LOCK OBJECT.
     CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCIVHD'
           EXPORTING
               ZFCIVRN               =     IT_SELECTED-ZFCIVRN
           EXCEPTIONS
               OTHERS                = 1.

     IF SY-SUBRC NE 0.         "> ERROR 발생시..
        PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
        ADD    1    TO    W_ERR_CNT.
        CONTINUE.
     ENDIF.
* 수입의뢰 Header Table
     CALL FUNCTION 'ZIM_GET_COMMERCIAL_DOC_HEADER'
         EXPORTING
            ZFCIVRN         = IT_SELECTED-ZFCIVRN
         IMPORTING
            W_ZTCIVHD       = ZTCIVHD
         TABLES
            IT_ZSCIVIT      = IT_ZSCIVIT
         EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

     CASE SY-SUBRC.
       WHEN 4.
         MESSAGE E374 WITH IT_SELECTED-ZFCIVRN.
       WHEN 8.
         MESSAGE E729.
     ENDCASE.

*-----------------------------------------------------------------------
* INVOICE VERIFICATION
*-----------------------------------------------------------------------
     IF W_OK_CODE EQ 'IV' OR W_OK_CODE  EQ  'CM'.
        PERFORM  P3000_CALL_INVOICE_VERIFY
                 USING IT_SELECTED-ZFCIVRN.
     ELSE.
         PERFORM  P3000_CALL_INVOICE_CANCEL
                  USING IT_SELECTED-ZFCIVRN.
     ENDIF.

  ENDLOOP.

ENDFORM.                    " P4000_IV_PROCESS
*&---------------------------------------------------------------------*
*&      Form  P5000_MIRO_MESSAGE
*&---------------------------------------------------------------------*
FORM P5000_MIRO_MESSAGE.

   SELECT SINGLE * FROM ZTIMIMG00.

   IF  W_OK_CODE  =  'CM'  OR  W_OK_CODE  = 'IV'.
       ZTCIVHST-BUDAT = SY-DATUM.   "> TO DAY.
       READ TABLE IT_SELECTED INDEX  1.
       SELECT SINGLE ZFCIDT INTO ZTCIVHST-BLDAT
              FROM   ZTCIVHD
              WHERE  ZFCIVRN  EQ  IT_SELECTED-ZFCIVRN.
       ZTCIVHST-ZFBDT  =  SY-DATUM.

       PERFORM P5000_MESSAGE_BOX USING
                     'Gross price creation confirmation'
                     'Creating the gross price slip.'
                     'Do you want to create?' " Message #2
                     'Y'             " 취소 버튼 유.
                     '1'.            " default button
   ELSE.

        IF ZTIMIMG00-CSREALYN EQ 'X'.
           OK-CODE  =  'YES'.
        ELSE.
           ZTCIVHST-CBUDAT = SY-DATUM.   "> TO DAY.

           PERFORM P5000_MESSAGE_BOX USING
                         'Slip cancellation confirmation'
                         'Cancellation of appropriate slip.'
                         'Do you want to continue?' " Message #2
                         'N'             " 취소 버튼 유.
                         '1'.            " default button
        ENDIF.
   ENDIF.

ENDFORM.                    " P5000_MIRO_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P5000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P5000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

   SPOP-TITEL = TITLE.
   SPOP-TEXTLINE1 = TEXT1.
   SPOP-TEXTLINE2 = TEXT2.
   IF CANCEL EQ 'Y'.
      CANCEL_OPTION = 'Y'.
   ELSE.
      CLEAR : CANCEL_OPTION.
   ENDIF.
   OPTION = DEFAULT.
   TEXTLEN = 40.

   SELECT SINGLE * FROM ZTIMIMG00.
   CLEAR : RADIO_ALL, RADIO_ERROR.
   RADIO_NONE = 'X'.

   IF W_OK_CODE EQ 'IV' OR W_OK_CODE EQ 'CM'.
      IF SY-SUBRC EQ 0.
         IF ZTIMIMG00-ZFIVTY EQ 'L'.
            CALL SCREEN 3515 STARTING AT 12 3
                             ENDING   AT 86 15.
         ELSE.
            CALL SCREEN 3515 STARTING AT 12 3
                             ENDING   AT 86 15.
         ENDIF.
      ELSE.
         RADIO_NONE = 'X'.
         CALL SCREEN 3515 STARTING AT 12 3
                          ENDING   AT 86 15.
      ENDIF.
      IF RADIO_NONE = 'X'.
         DISPMODE = 'N'.
      ENDIF.
      IF RADIO_ALL = 'X'.
         DISPMODE = 'A'.
      ENDIF.
      IF RADIO_ERROR = 'X'.
         DISPMODE = 'E'.
      ENDIF.
   ELSEIF  W_OK_CODE EQ 'MIR2'.

      CALL SCREEN 3516 STARTING AT 34 3
                       ENDING   AT 72 12.
   ELSE.
      CALL SCREEN 0001 STARTING AT 30 6
                       ENDING   AT 78 10.
   ENDIF.

ENDFORM.                    " P5000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE TABLES   P_IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
          SY-MSGID            TO     IT_ERR_LIST-MSGID,
          SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
          SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
          SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
          SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
          SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
          IT_SELECTED-ZFCIVRN TO     IT_ERR_LIST-ZFCIVRN.

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
*&      Form  P3000_CALL_INVOICE_VERIFY
*&---------------------------------------------------------------------*
FORM P3000_CALL_INVOICE_VERIFY USING    P_ZTCIVHD_ZFCIVRN.

  CLEAR : ZTIMIMG11.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
     MESSAGE E961.
  ENDIF.

*>> INVOICE VERIFY 종류..
  IF ZTIMIMG00-ZFIVTY EQ 'L'.      ">Logistics Invoice Verification
     IF W_OK_CODE EQ 'IV'.
        PERFORM P3000_BAPI_CALL         USING IT_SELECTED-ZFCIVRN
                                              'X'  SPACE
                                              ZTCIVHST-BLDAT
                                              ZTCIVHST-BUDAT
                                              ZTCIVHST-ZFBDT.
     ELSE.
        PERFORM P3000_BAPI_CALL         USING IT_SELECTED-ZFCIVRN
                                              SPACE 'X'
                                              ZTCIVHST-BLDAT
                                              ZTCIVHST-BUDAT
                                              ZTCIVHST-ZFBDT.
     ENDIF.
  ELSEIF ZTIMIMG00-ZFIVTY EQ 'C'.  ">Conventional Invoice Verification
     REFRESH : BDCDATA.
     PERFORM P3000_MRHR_DATA_MAKE    USING ZTCIVHD-ZFCIVRN
                                           'X'.
     PERFORM P2000_CALL_TRANSACTION  USING 'MRHR'
                                     CHANGING  W_SUBRC.
     IF W_SUBRC NE 0.
        PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
        ADD    1    TO    W_ERR_CNT.
     ENDIF.
  ENDIF.

*>>> UNLOCK.
   CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
        EXPORTING
        ZFCIVRN      =     IT_SELECTED-ZFCIVRN.


ENDFORM.                    " P3000_CALL_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*&      Form  P3000_BAPI_CALL
*&---------------------------------------------------------------------*
FORM P3000_BAPI_CALL USING    P_ZFCIVRN
                              P_INVOICE
                              P_CREDITMENO
                              P_BLDAT
                              P_BUDAT
                              P_ZFBDT.

DATA : BEGIN OF IT_EBELN  OCCURS 0.
       INCLUDE  STRUCTURE EKKO.
DATA : END   OF IT_EBELN.

*>> 환율고정 지시자 및 환율 고정 PROCESS.....
   IF P_INVOICE         EQ 'X' AND
      ZTIMIMG00-ZFEXMTD EQ 'I' AND
      ZTIMIMG00-ZFEXFIX EQ 'X' AND
      NOT ( ZTCIVHD-ZFREQTY   EQ 'LO' OR
            ZTCIVHD-ZFREQTY   EQ 'PU' ).
      REFRESH : IT_EBELN, RETURN.
      CLEAR : W_COUNT.
      LOOP AT IT_ZSCIVIT.
         CLEAR : IT_EBELN.
         IF NOT IT_ZSCIVIT-EBELN IS INITIAL.
            MOVE : IT_ZSCIVIT-EBELN TO IT_EBELN-EBELN.
            COLLECT IT_EBELN.
            ADD 1 TO W_COUNT.
         ENDIF.
      ENDLOOP.
      IF W_COUNT GT 0.
         LOOP AT IT_EBELN.
            W_TABIX = SY-TABIX.
            MOVE : 'X'             TO  IT_EBELN-KUFIX,
                   ZTCIVHD-ZFEXRT  TO  IT_EBELN-WKURS.
            MODIFY IT_EBELN INDEX W_TABIX.
         ENDLOOP.
*>> P/O 고정환율,,,,,
         CALL FUNCTION 'ZIM_BAPI_PO_EXRT_CHANGE'
              TABLES
                  IT_EBELN     =    IT_EBELN
                  RETURN       =    RETURN
              EXCEPTIONS
                  POST_ERROR   =    4.
         IF SY-SUBRC NE 0.
            IF RETURN[] IS INITIAL.
               PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
            ELSE.
               PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
            ENDIF.
            ADD    1    TO    W_ERR_CNT.
            W_SUBRC = 4.
            EXIT.
         ENDIF.
      ENDIF.
   ENDIF.

*>> INVOICE VERIFICATION FUNCTION....
   CALL FUNCTION 'ZIM_BAPI_INVOICE_CREATE'
        EXPORTING
            P_ZFCIVRN           =   P_ZFCIVRN
            P_CHG_MODE          =   'X'
            I_INVOICE           =   P_INVOICE
            I_CREDITMEMO        =   P_CREDITMENO
            P_BLDAT             =   P_BLDAT
            P_BUDAT             =   P_BUDAT
            P_ZFBDT             =   P_ZFBDT
        IMPORTING
            INVOICEDOCNUMBER    =   INVOICEDOCNUMBER
            FISCALYEAR          =   FISCALYEAR
        TABLES
            RETURN              =   RETURN
        EXCEPTIONS
            OTHERS              =   4.
*-----------------------------------------------------------------------
*>>> UNLOCK.
*       CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
*            EXPORTING
*              ZFCIVRN      =     IT_SELECTED-ZFCIVRN.
*-----------------------------------------------------------------------

     IF SY-SUBRC NE 0.           ">> 오류 발생시...
        IF RETURN[] IS INITIAL.
           PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ELSE.
           PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
        ADD    1    TO    W_ERR_CNT.
        W_SUBRC = 4.
     ELSE.
        MESSAGE S255(M8) WITH INVOICEDOCNUMBER.
        PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
        ADD    1    TO    W_PROC_CNT.
        W_SUBRC = 0.
     ENDIF.
ENDFORM.                    " P3000_BAPI_CALL
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

   IF SY-DYNNR NE '3118'.
      IF OPTION = '1'.
         SET CURSOR FIELD 'SPOP-OPTION1'.
      ELSE.
         SET CURSOR FIELD 'SPOP-OPTION2'.
      ENDIF.
   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

  AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
                  ID 'ACTVT' FIELD '*'.
  W_SUBRC = SY-SUBRC.

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

    IF W_SUBRC NE 0 AND SY-DYNNR EQ '3515'.
       IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
          SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
          SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
          SCREEN-NAME(06) EQ 'BLOCK2'.
          SCREEN-INVISIBLE = 1.
       ENDIF.
    ELSEIF ZTIMIMG00-ZFIVTY EQ 'L' AND SY-DYNNR EQ '3515'.
       IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
          SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
          SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
          SCREEN-NAME(06) EQ 'BLOCK2'.
          SCREEN-INVISIBLE = 1.
       ENDIF.
    ELSEIF SY-DYNNR EQ '3516'.
       IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
          SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
          SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
          SCREEN-NAME(06) EQ 'BLOCK2'.
          SCREEN-INVISIBLE = 1.
       ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCR0002 INPUT.

  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
  ENDCASE.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR3516  INPUT
*&---------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR3516 INPUT.

   IF NOT ( SY-UCOMM EQ 'ENTR' OR SY-UCOMM EQ 'YES' ).
      EXIT.
   ENDIF.

   IF ZTCIVHST-CBUDAT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHST' 'CBUDAT'.
   ENDIF.

   IF ZTCIVHST-STGRD IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTCIVHST' 'STGRD'.
   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR3516  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_MRHR_DATA_MAKE
*&---------------------------------------------------------------------*
FORM P3000_MRHR_DATA_MAKE USING    P_ZTIV_ZFIVNO
                                   VALUE(P_1513).

ENDFORM.                    " P3000_MRHR_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*FORM P2000_CALL_TRANSACTION USING    TCODE
*                            CHANGING P_W_SUBRC.
*
*  DATA : L_LINE LIKE SY-TABIX.
*
*  REFRESH : MESSTAB.
*  CALL TRANSACTION TCODE   USING       BDCDATA
*                           MODE        DISP_MODE
*                           UPDATE      UMODE
*                           MESSAGES    INTO   MESSTAB.
*
*  P_SUBRC = SY-SUBRC.
*
*  DESCRIBE TABLE MESSTAB LINES L_LINE.
*  READ TABLE MESSTAB  INDEX L_LINE.
*
*  call function 'MESSAGE_TEXT_BUILD'
*       exporting
*             msgid               = messtab-msgid
*             msgnr               = messtab-msgnr
*             msgv1               = messtab-msgv1
*             msgv2               = messtab-msgv2
*             msgv3               = messtab-msgv3
*             msgv4               = messtab-msgv4
*       importing
*             message_text_output = MESSTXT.
*
*ENDFORM.                    " P2000_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH 'Message List'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.
   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POPU'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(107), / SY-VLINE NO-GAP,
                   'Type'   NO-GAP, SY-VLINE NO-GAP,
                   ' CI/V NO  ' NO-GAP, SY-VLINE NO-GAP,
                   'Message Text', 105 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(107).
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-ZFCIVRN  NO-GAP,
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
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_WRITE_CIV_DATA.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES  IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
             IT_SELECTED-ZFCIVRN TO     IT_ERR_LIST-ZFCIVRN.

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
*&      Form  P3000_CALL_INVOICE_CANCEL
*&---------------------------------------------------------------------*
FORM P3000_CALL_INVOICE_CANCEL USING    P_ZFCIVRN.

   W_STGRD  =  ZTCIVHST-STGRD.

   SELECT SINGLE * FROM ZTCIVHST
   WHERE  ZFCIVRN   =  P_ZFCIVRN
   AND    ZFCIVHST  =  ( SELECT  MAX( ZFCIVHST )  FROM  ZTCIVHST
                         WHERE   ZFCIVRN  =  P_ZFCIVRN ).
   MOVE  W_STGRD  TO  ZTCIVHST-STGRD.

   CALL FUNCTION 'ZIM_BAPI_INVOICE_CANCEL'
        EXPORTING
            P_ZFCIVRN           =   P_ZFCIVRN
            INVOICEDOCNUMBER    =   ZTCIVHST-BELNR
            FISCALYEAR          =   ZTCIVHST-GJAHR
            REASONREVERSAL      =   ZTCIVHST-STGRD
            POSTINGDATE         =   ZTCIVHST-CBUDAT
        IMPORTING
            INVOICEDOCNUMBER_REVERSAL    =   ZTCIVHST-CBELNR
            FISCALYEAR_REVERSAL          =   ZTCIVHST-CGJAHR
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
    ELSE.
       MESSAGE S282(M8) WITH ZTCIVHST-CBELNR.
       PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
       ADD    1    TO    W_PROC_CNT.
       W_SUBRC = 0.
    ENDIF.
*>>> UNLOCK.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCIVHD'
         EXPORTING
         ZFCIVRN      =     IT_SELECTED-ZFCIVRN.

ENDFORM.                    " P3000_CALL_INVOICE_CANCEL
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP  USING    P_ZTERM.

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
    MESSAGE S177(06) WITH P_ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
    P_ZTERM = T052-ZTERM.
  ENDIF.

ENDFORM.                    " P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
