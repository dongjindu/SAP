*&---------------------------------------------------------------------*
*& Report  ZRIMRECST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입의뢰 비용 Posting
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.03.23                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 수입의뢰 비용을 조회하여 BDC로 Posting한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&   1. 2000/03/24 : Posting BDC Function으로 변경.
*&---------------------------------------------------------------------*
REPORT  ZRIMRECST    MESSAGE-ID ZIM
                     LINE-SIZE 124
                     NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          ZRIMRECSTTOP,
          ZRIMBDCCOM,
          ZRIMBAIPTOP.

*>> INTERNAL TABLES
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFOPNNO(16)     TYPE   C,                " L/C No
       BUKRS           LIKE   ZTRECST-BUKRS,    ">회사코드.
       ZFREQNO         LIKE   ZTRECST-ZFREQNO,  " 수입의뢰 관리번?
       ZFCSQ           LIKE   ZTRECST-ZFCSQ,    " 비용순?
       ZFCSCD          LIKE   ZTRECST-ZFCSCD,   " 비용구?
       ZFCSCD_NM(10)   TYPE   C,                " 비용명?
       ZFCAMT          LIKE   ZTRECST-ZFCAMT,   " 비용금?
       WAERS           LIKE   ZTRECST-WAERS,    " 통?
       ZFCKAMT         LIKE   ZTRECST-ZFCKAMT,  " 비용원화금?
       KRW             LIKE   ZTRECST-ZFKRW,    " 원화통?
       ZFEXRT          LIKE   ZTRECST-ZFEXRT,   " Exchange Rate
       ZFVAT           LIKE   ZTRECST-ZFVAT,    " V.A.T
       ZFVPR           LIKE   ZTRECST-ZFVPR,    " VP AMOUNT
       ZFOCDT          LIKE   ZTRECST-ZFOCDT,   " 지급?
       ZFVEN           LIKE   ZTRECST-ZFVEN,    " Vendor
       ZFVEN_NM(20)    TYPE   C,                " Vendor ?
       ZFPAY           LIKE   ZTRECST-ZFPAY,    " 지불?
       ZFPAY_NM(20)    TYPE   C,                " 지불처?
       ZTERM           LIKE   ZTRECST-ZTERM,    " Terms of Payment
       MWSKZ           LIKE   ZTRECST-MWSKZ,    " Tax Code
       ZFWERKS         LIKE   ZTRECST-ZFWERKS,  " Plant
       ZFTRIPLE        LIKE   ZTREQHD-ZFTRIPLE, " 삼국무역구분자.
       SUM_MARK        TYPE   C.
DATA : END OF IT_TAB.

INCLUDE : ZRIMSORTCOM,    " Sort를 위한 Include
          ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_VEN    FOR ZTRECST-ZFVEN,   " Vendor
                   S_PAY    FOR ZTRECST-ZFPAY,   " 지불?
                   S_ZTERM  FOR ZTRECST-ZTERM,   " Terms Of Payment
                   S_MWSKZ  FOR ZTRECST-MWSKZ,   " Tax Code
                   S_WERKS  FOR ZTRECST-ZFWERKS, " Plant
                   S_OCDT   FOR ZTRECST-ZFOCDT,  " 지불?
                   S_CSCD   FOR ZTRECST-ZFCSCD   " 비용구분.
                            MATCHCODE OBJECT ZIC5L,
                   S_OPNNO  FOR ZTREQHD-ZFOPNNO, " L/C No
                   S_REQNO  FOR ZTREQHD-ZFREQNO. "수입의뢰 관리No
SELECTION-SCREEN END OF BLOCK B1.

*>> 초기값 SETTING.
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Import System Configuration Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.

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
            IF ANWORT EQ 'Y'.
               DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
               IF W_LINE GT 0.
                  INCLUDE = 'POPU'.
                  CALL SCREEN 0100 STARTING AT  05   3
                                   ENDING   AT  100 12.
                  CLEAR : INCLUDE.
               ENDIF.
               PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
               PERFORM RESET_LIST.
               IF W_ERR_CHK EQ 'Y'.
                  MESSAGE S826 WITH W_PROC_CNT.
                  LEAVE TO SCREEN 0.
               ENDIF.
               MESSAGE S826 WITH W_PROC_CNT.
            ELSE.
               MESSAGE S957.
            ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
            PERFORM RESET_LIST.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
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
   SET  TITLEBAR 'ZIMV1'.           " GUI TITLE SETTING..
   SELECT SINGLE * FROM ZTIMIMG11.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 수입의뢰 비용 A/P POSTING ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 106 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'L/C No          '               NO-GAP, SY-VLINE NO-GAP,
            '비용구분      '                 NO-GAP, SY-VLINE NO-GAP,
            '          발생금액      '       NO-GAP, SY-VLINE NO-GAP,
            '     V.A.T (Or V/P)'            NO-GAP, SY-VLINE NO-GAP,
            '지급일    '                     NO-GAP, SY-VLINE NO-GAP,
            'Vendor                        ' NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                           SY-VLINE NO-GAP,
            '수입관리NO'                     NO-GAP, SY-VLINE NO-GAP,
            'Seq  '                          NO-GAP, SY-VLINE NO-GAP,
            '환율          '                 NO-GAP, SY-VLINE NO-GAP,
            '          원화금액      '       NO-GAP, SY-VLINE NO-GAP,
            'Comp/Term/Tax/Plant'            NO-GAP,
            '           '                    NO-GAP, SY-VLINE NO-GAP,
            '지불처                        ' NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT EQ 0.
     MESSAGE S738.   EXIT.
  ENDIF.

  CLEAR : SV_ZFVEN, SV_ZFPAY, SV_ZTERM, SV_MWSKZ, SV_ZFWERKS, SV_ZFVPR,
          SV_ZFTRIPLE, SUM_ZFCKAMT, SUM_ZFVAT, SUM_ZFVPR,
          SV_ZFREQNO,  SV_ZFCSQ,
          W_LOOP_CNT.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMV1'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMV1'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
*>> 초기 비교값 MOVE.
      IF W_LINE  =  1.
         MOVE   IT_TAB-ZFCSQ    TO  SV_ZFCSQ.
         MOVE   IT_TAB-ZFREQNO  TO  SV_ZFREQNO.
         MOVE   IT_TAB-BUKRS    TO  SV_BUKRS.
         MOVE   IT_TAB-ZFCSCD   TO  SV_ZFCSCD.
         MOVE   IT_TAB-ZFVEN    TO  SV_ZFVEN.
         MOVE   IT_TAB-ZFPAY    TO  SV_ZFPAY.
         MOVE   IT_TAB-ZTERM    TO  SV_ZTERM.
         MOVE   IT_TAB-MWSKZ    TO  SV_MWSKZ.
         MOVE   IT_TAB-ZFWERKS  TO  SV_ZFWERKS.
         MOVE   IT_TAB-ZFTRIPLE TO  SV_ZFTRIPLE.
         MOVE   IT_TAB-ZFVEN_NM TO  SV_ZFVEN_NM.
         MOVE   IT_TAB-ZFPAY_NM TO  SV_ZFPAY_NM.
         MOVE   IT_TAB-ZFVPR    TO  SV_ZFVPR.
      ENDIF.
*>> 비교값이 틀리면 SUM WRITE.
      IF SV_ZFVEN    NE  IT_TAB-ZFVEN  OR
         SV_ZFPAY    NE  IT_TAB-ZFPAY  OR
         SV_ZTERM    NE  IT_TAB-ZTERM  OR
         SV_MWSKZ    NE  IT_TAB-MWSKZ  OR
         SV_BUKRS    NE  IT_TAB-BUKRS  OR
         SV_ZFCSCD   NE  IT_TAB-ZFCSCD OR
         SV_ZFTRIPLE NE  IT_TAB-ZFTRIPLE OR
         SV_ZFWERKS  NE  IT_TAB-ZFWERKS OR
         IT_TAB-ZFVPR NE  0             OR
         SV_ZFVPR     NE  0.

         IF W_LINE NE 1.
            PERFORM P3000_SUM_LINE_WRITE.
         ENDIF.

         MOVE   IT_TAB-ZFREQNO  TO  SV_ZFREQNO.
         MOVE   IT_TAB-ZFCSQ    TO  SV_ZFCSQ.
         MOVE   IT_TAB-ZFCSCD   TO  SV_ZFCSCD.
         MOVE   IT_TAB-BUKRS    TO  SV_BUKRS.
         MOVE   IT_TAB-ZFVEN    TO  SV_ZFVEN.
         MOVE   IT_TAB-ZFPAY    TO  SV_ZFPAY.
         MOVE   IT_TAB-ZTERM    TO  SV_ZTERM.
         MOVE   IT_TAB-MWSKZ    TO  SV_MWSKZ.
         MOVE   IT_TAB-ZFWERKS  TO  SV_ZFWERKS.
         MOVE   IT_TAB-ZFTRIPLE TO  SV_ZFTRIPLE.
         MOVE   IT_TAB-ZFVEN_NM TO  SV_ZFVEN_NM.
         MOVE   IT_TAB-ZFPAY_NM TO  SV_ZFPAY_NM.
         MOVE   IT_TAB-ZFVPR    TO  SV_ZFVPR.
         MOVE   0               TO  SUM_ZFCKAMT.
         MOVE   0               TO  SUM_ZFVAT.
         MOVE   0               TO  SUM_ZFVPR.
      ENDIF.

      PERFORM P3000_LINE_WRITE.
      ADD       IT_TAB-ZFCKAMT  TO  SUM_ZFCKAMT.
      ADD       IT_TAB-ZFVAT    TO  SUM_ZFVAT.
      ADD       IT_TAB-ZFVPR    TO  SUM_ZFVPR.

      AT LAST.
         PERFORM P3000_SUM_LINE_WRITE.
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

  IF IT_TAB-ZFVPR NE 0.
     TMP_ZFVAT = IT_TAB-ZFVPR.
  ELSE.
     TMP_ZFVAT = IT_TAB-ZFVAT.
  ENDIF.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE, '  '       NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFOPNNO          NO-GAP, " L/C No
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCSCD           NO-GAP, " 비용구?
       ' '                     NO-GAP,
       IT_TAB-ZFCSCD_NM        NO-GAP, " 비용명?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCAMT CURRENCY IT_TAB-WAERS NO-GAP, " 비용금?
       IT_TAB-WAERS            NO-GAP, " 금액통?
       SY-VLINE                NO-GAP,
*       IT_TAB-ZFVAT  CURRENCY IT_TAB-KRW   NO-GAP, " V.A.T
       TMP_ZFVAT     CURRENCY IT_TAB-KRW   NO-GAP, " V.A.T
       SY-VLINE                NO-GAP,
       IT_TAB-ZFOCDT           NO-GAP, " 지급?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVEN            NO-GAP, " Vendor
       IT_TAB-ZFVEN_NM         NO-GAP, " Vendor ?
       SY-VLINE                NO-GAP.

       FORMAT COLOR COL_BACKGROUND.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO           NO-GAP, " 수입의뢰 관리번?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCSQ            NO-GAP, " 비용순?
       SY-VLINE                NO-GAP,
       '  '                    NO-GAP,
       IT_TAB-ZFEXRT           NO-GAP, " Exchange Rate
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT  CURRENCY IT_TAB-KRW NO-GAP, " 비용원화금?
       IT_TAB-KRW              NO-GAP, " 원화통?
       SY-VLINE                NO-GAP,
       IT_TAB-BUKRS            NO-GAP, " COMPANY CODE.
       '/'                     NO-GAP,
       IT_TAB-ZTERM            NO-GAP, " Terms Of Payment
       '/'                     NO-GAP,
       IT_TAB-MWSKZ            NO-GAP, " Tax Code
       ' /'                    NO-GAP,
       IT_TAB-ZFWERKS          NO-GAP, " Plant
       '            '          NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFPAY            NO-GAP, " 지불?
       IT_TAB-ZFPAY_NM         NO-GAP, " 지불처 ?
       SY-VLINE                NO-GAP.

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

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT EQ 0.
     MESSAGE S738.   EXIT.
  ENDIF.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH : IT_TAB, IT_ZTIMIMG08.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG08
  FROM    ZTIMIMG08
  WHERE   ZFCDTY  =  '003'  AND  ZFCD1  =  'N'.

  SELECT  H~ZFOPNNO  I~ZFREQNO  I~ZFCSQ  I~ZFCSCD  I~ZFEXRT
          I~ZFCAMT   I~ZFCKAMT  I~ZFVAT  I~ZTERM   I~MWSKZ
          I~ZFWERKS  I~ZFPSDT   I~ZFVEN  I~ZFPAY   I~WAERS
          I~BUKRS    H~ZFTRIPLE I~ZFVPR
  INTO    CORRESPONDING  FIELDS  OF  TABLE  IT_TAB
  FROM    ZTREQHD    AS  H  INNER  JOIN  ZTRECST  AS  I
  ON      H~ZFREQNO  EQ  I~ZFREQNO
  FOR ALL ENTRIES    IN  IT_ZTIMIMG08
  WHERE   I~ZFVEN    IN  S_VEN
  AND     I~ZFCSCD   IN  S_CSCD
  AND     I~ZFPAY    IN  S_PAY
  AND     I~ZTERM    IN  S_ZTERM
  AND     I~MWSKZ    IN  S_MWSKZ
  AND     I~ZFWERKS  IN  S_WERKS
  AND     I~ZFOCDT   IN  S_OCDT
  AND     I~ZFREQNO  IN  S_REQNO
  AND     H~ZFOPNNO  IN  S_OPNNO
  AND     I~ZFCSCD   EQ  IT_ZTIMIMG08-ZFCD
  AND   ( I~ZFACDO   EQ  SPACE
  OR      I~ZFACDO   IS  NULL )
  AND     I~ZFCKAMT  NE  0.

*>> INTERNAL TABLE 에 VENDOR 명, 지급처명, 비용명 DISPLAY
  LOOP  AT  IT_TAB.

    W_TABIX  =  SY-TABIX.
    SELECT SINGLE  NAME1  INTO  IT_TAB-ZFVEN_NM
    FROM   LFA1    WHERE  LIFNR EQ  IT_TAB-ZFVEN.

    SELECT SINGLE  NAME1  INTO  IT_TAB-ZFPAY_NM
    FROM   LFA1    WHERE  LIFNR EQ  IT_TAB-ZFPAY.

    CLEAR : IT_ZTIMIMG08.
    READ TABLE IT_ZTIMIMG08 WITH KEY ZFCDTY = '003'
                                     ZFCD   = IT_TAB-ZFCSCD.
    IF SY-SUBRC NE 0.
       MESSAGE S430 WITH '수입의뢰비용' IT_TAB-ZFCSCD.
       MOVE 'Y' TO W_ERR_CHK.
    ENDIF.
    MOVE:  IT_ZTIMIMG08-ZFCDNM  TO IT_TAB-ZFCSCD_NM.
    MOVE  'KRW'   TO  IT_TAB-KRW.
    MODIFY  IT_TAB  INDEX  W_TABIX.

  ENDLOOP.

  SORT  IT_TAB  BY  BUKRS ZFVEN  ZFPAY  ZTERM  MWSKZ  ZFWERKS ZFTRIPLE.

ENDFORM.                    " P1000_READ_TEXT
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
         MOVE : SV_BUKRS       TO IT_SELECTED-BUKRS,
                SV_ZFCSCD      TO IT_SELECTED-ZFCSCD,
                SV_ZFREQNO     TO IT_SELECTED-ZFREQNO,
                SV_ZFCSQ       TO IT_SELECTED-ZFCSQ,
                SV_ZFVEN       TO IT_SELECTED-ZFVEN,
                SV_ZFPAY       TO IT_SELECTED-ZFPAY,
                SV_ZTERM       TO IT_SELECTED-ZTERM,
                SV_MWSKZ       TO IT_SELECTED-MWSKZ,
                SV_ZFWERKS     TO IT_SELECTED-ZFWERKS,
                SV_ZFTRIPLE    TO IT_SELECTED-ZFTRIPLE, "삼국무역구분자.
                SUM_ZFCKAMT    TO IT_SELECTED-ZFCKAMT,
                SUM_ZFVAT      TO IT_SELECTED-ZFVAT,
                SUM_ZFVPR      TO IT_SELECTED-ZFVPR.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUM_LINE_WRITE.
DATA : L_TEXT16(16).

  IF SV_ZFTRIPLE EQ 'X'.
     L_TEXT16 = '*** 삼국무역 ***'.
  ELSE.
     CLEAR : L_TEXT16.
  ENDIF.

  IF SUM_ZFVPR NE 0.
     L_TEXT16 = '*** V/P 전표 ***'.
     TMP_ZFVAT = SUM_ZFVPR.
  ELSE.
     TMP_ZFVAT = SUM_ZFVAT.
     CLEAR : SV_ZFREQNO, SV_ZFCSQ.
  ENDIF.

  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE         NO-GAP,
*       '                '           NO-GAP,
       L_TEXT16         NO-GAP,
       '               '            NO-GAP,
       SY-VLINE         NO-GAP,
       SUM_ZFCKAMT CURRENCY 'KRW'   NO-GAP, " 비용금?
       '     '          NO-GAP,
       SY-VLINE         NO-GAP,
       TMP_ZFVAT   CURRENCY 'KRW'   NO-GAP,   " VAT
       SY-VLINE         NO-GAP,
       '          '     NO-GAP,
       SY-VLINE         NO-GAP,
       SV_ZFVEN         NO-GAP,                  " Vendor
       SV_ZFVEN_NM      NO-GAP,                  " Vendor ?
       SY-VLINE         NO-GAP.
* Hide
       HIDE: SV_BUKRS,    SV_ZTERM,    SV_MWSKZ,   SV_ZFWERKS, SV_ZFVEN,
             SV_ZFTRIPLE, SV_ZFCSCD,   SV_ZFREQNO, SV_ZFCSQ,
             SV_ZFPAY,    SUM_ZFCKAMT, SUM_ZFVAT,  SUM_ZFVPR.

       WRITE : / SY-VLINE, ' ', SY-VLINE  NO-GAP,
       '                '        NO-GAP,
       '               '         NO-GAP,
       SY-VLINE   NO-GAP,
       '            '            NO-GAP,
       '            '            NO-GAP,
       SY-VLINE   NO-GAP,
       SV_BUKRS                  NO-GAP, " COMPANY CODE.
       '/'                       NO-GAP,
       SV_ZTERM                  NO-GAP, " Terms Of Payment
       '/'                       NO-GAP,
       SV_MWSKZ                  NO-GAP, " Tax Code
       ' /'                      NO-GAP,
       SV_ZFWERKS                NO-GAP, " Plant
       ' '                       NO-GAP,
       SY-VLINE                  NO-GAP,
       '          '              NO-GAP,
       SY-VLINE                  NO-GAP,
       SV_ZFPAY                  NO-GAP, " 지불?
       SV_ZFPAY_NM               NO-GAP, " 지불처 ?
       SY-VLINE                  NO-GAP.

  HIDE: SV_BUKRS,  SV_ZTERM,    SV_MWSKZ,  SV_ZFWERKS,   SV_ZFVEN,
        SV_ZFCSCD, SV_ZFREQNO,  SV_ZFCSQ,  SV_ZFTRIPLE,
        SV_ZFPAY,  SUM_ZFCKAMT, SUM_ZFVAT, SUM_ZFVPR.

  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_SUM_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P4000_COST_POST
*&---------------------------------------------------------------------*
FORM P4000_COST_POST USING    P_W_ERR_CHK.
DATA : L_TCODE   LIKE   SY-TCODE.

  REFRESH : IT_ERR_LIST.
  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.

*>> MESSAGE BOX.
  PERFORM P4000_GET_INIVAL.
  IF ANWORT NE 'Y'.
     EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.
     REFRESH : IT_LOCKED.
*>> 오류 검증....
     IF IT_SELECTED-BUKRS IS INITIAL.
        MESSAGE S167 WITH '회사코드'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF IT_SELECTED-ZTERM IS INITIAL.
        MESSAGE S167 WITH '지급 조건'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF IT_SELECTED-ZFVEN IS INITIAL.
        MESSAGE S167 WITH '거래처 코드'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF IT_SELECTED-ZFWERKS IS INITIAL.
        MESSAGE S167 WITH '플랜트'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
*>> 해당 수입비용.
     REFRESH  : IT_ZTRECST.
     LOOP AT IT_TAB WHERE  BUKRS   EQ   IT_SELECTED-BUKRS
                    AND    ZFVEN   EQ   IT_SELECTED-ZFVEN
                    AND    ZFPAY   EQ   IT_SELECTED-ZFPAY
                    AND    ZTERM   EQ   IT_SELECTED-ZTERM
                    AND    MWSKZ   EQ   IT_SELECTED-MWSKZ
                    AND    ZFWERKS EQ   IT_SELECTED-ZFWERKS.

        IF NOT IT_SELECTED-ZFREQNO IS INITIAL.
           IF IT_SELECTED-ZFREQNO EQ IT_TAB-ZFREQNO AND
              IT_SELECTED-ZFCSQ   EQ IT_TAB-ZFCSQ.
              SELECT *  APPENDING TABLE IT_ZTRECST
                        FROM   ZTRECST
                        WHERE  ZFREQNO   EQ   IT_TAB-ZFREQNO
                        AND    ZFCSQ     EQ   IT_TAB-ZFCSQ.
           ENDIF.
        ELSE.
           SELECT *  APPENDING TABLE IT_ZTRECST
                        FROM   ZTRECST
                        WHERE  ZFREQNO   EQ   IT_TAB-ZFREQNO
                        AND    ZFCSQ     EQ   IT_TAB-ZFCSQ.
        ENDIF.

     ENDLOOP.
*>> LOCK MODE.
     CLEAR : W_LOCK_CHK.
     SORT IT_ZTRECST BY ZFREQNO.
     LOOP AT IT_ZTRECST.
        ON CHANGE OF IT_ZTRECST-ZFREQNO.
           PERFORM  P2000_SET_LOCK_MODE   USING IT_ZTRECST-ZFREQNO
                                                'L'    W_SUBRC.
           IF W_SUBRC EQ 0.
              MOVE IT_ZTRECST-ZFREQNO TO IT_LOCKED-ZFREQNO.
              APPEND IT_LOCKED.
           ELSE.
              W_LOCK_CHK = 'Y'.
              PERFORM  P2000_SINGLE_MESS_MAKE.
              EXIT.
           ENDIF.
        ENDON.
     ENDLOOP.

     IF W_LOCK_CHK EQ 'Y'.
        CONTINUE.
     ENDIF.
*>> BAPIs Function
*     PERFORM  P2000_GL_DATA_MAKE    USING   W_SUBRC.
**>> BDC DATA MAKE.
     IF IT_SELECTED-ZFCSCD EQ '1AB' AND IT_SELECTED-ZFVPR NE 0.
        PERFORM VP_COST_BDC_INSERT.
        L_TCODE = 'FB01'.
     ELSE.
        PERFORM COST_BDC_INSERT.
        L_TCODE = 'F-22'.
     ENDIF.

*>>PARAMETER CLEAR.
     SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
     SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.

*>> BDC CALL.
     PERFORM P2000_CALL_TRANSACTION USING     L_TCODE
                                              W_SUBRC.

     IF W_SUBRC NE 0.      ">> ERROR 발생시.
        PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
        ADD    1    TO    W_ERR_CNT.
        CONTINUE.
     ELSE.                 ">> SUCCESS 시.
        GET PARAMETER ID 'BLN' FIELD ZFACDO.        " 전표번호.
        GET PARAMETER ID 'GJR' FIELD ZFFIYR.        " 회계년도.
*>> 전표번호가 전달되지 않을 경우.
        IF ZFACDO IS INITIAL AND ZFFIYR IS INITIAL.
*>>> 오류..(사용자 종결 등....)
           MESSAGE S494.
           PERFORM  P2000_SINGLE_MESS_MAKE.
           CONTINUE.
        ENDIF.

        LOOP AT IT_ZTRECST.
           MOVE : ZFACDO      TO     IT_ZTRECST-ZFACDO,
                  ZFFIYR      TO     IT_ZTRECST-ZFFIYR,
                  W_POSDT     TO     IT_ZTRECST-ZFPSDT,
                  W_DOCDT     TO     IT_ZTRECST-ZFOCDT,
                  SY-UNAME    TO     IT_ZTRECST-UNAM,
                  SY-DATUM    TO     IT_ZTRECST-UDAT.
           MODIFY  IT_ZTRECST  INDEX   SY-TABIX.
        ENDLOOP.
        MODIFY ZTRECST FROM TABLE IT_ZTRECST.
        PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
       ENDIF.
       ADD 1       TO W_PROC_CNT.
  ENDLOOP.

ENDFORM.                    " P4000_COST_POST

*&---------------------------------------------------------------------*
*&      Form  P4000_GET_INIVAL
*&---------------------------------------------------------------------*
FORM P4000_GET_INIVAL.

  MOVE 'Initial Value' TO SPOP-TITEL.
*  MOVE 'X'             TO RADIO_NONE.
  IF W_POSDT IS INITIAL.
     MOVE SY-DATUM    TO W_POSDT.
  ENDIF.
  IF W_DOCDT IS INITIAL.
     MOVE SY-DATUM    TO W_DOCDT.
  ENDIF.

  CALL SCREEN 0010 STARTING AT 15 1
                   ENDING   AT 56 14.

  IF RADIO_NONE = 'X'.
     DISP_MODE = 'N'.
  ENDIF.
  IF RADIO_ALL = 'X'.
     DISP_MODE = 'A'.
  ENDIF.
  IF RADIO_ERROR = 'X'.
     DISP_MODE = 'E'.
  ENDIF.

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

  IF OK-CODE EQ 'ENTR' OR OK-CODE EQ 'YES'.
     ANWORT = 'Y'.
     SET SCREEN 0.
     LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
     ANWORT = 'N'.
     SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT

*&---------------------------------------------------------------------*
*&      Form  COST_BDC_INSERT
*&---------------------------------------------------------------------*
FORM COST_BDC_INSERT.
DATA :   L_ZFPAY     LIKE   IT_SELECTED-ZFPAY,
         L_ZFIOCAC1  LIKE   ZTIMIMG11-ZFIOCAC1.

  IF IT_SELECTED-ZFTRIPLE EQ 'X'.
     L_ZFIOCAC1 = ZTIMIMG11-ZFIOCAC11.
  ELSE.
     L_ZFIOCAC1 = ZTIMIMG11-ZFIOCAC1.
  ENDIF.

  W_WRBTR = IT_SELECTED-ZFCKAMT + IT_SELECTED-ZFVAT.
  WRITE W_WRBTR CURRENCY 'KRW' TO TEMP_WRBTR.
  WRITE IT_SELECTED-ZFVAT CURRENCY 'KRW' TO TEMP_WMWST.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ IT_SELECTED-ZFWERKS.
*>> 지불처가 다를 경우.
  IF NOT ( IT_SELECTED-ZFPAY IS INITIAL ) AND
         ( IT_SELECTED-ZFVEN NE IT_SELECTED-ZFPAY ).
     L_ZFPAY  =  IT_SELECTED-ZFPAY. " Payee
  ELSE.
     CLEAR L_ZFPAY.
  ENDIF.

  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   W_DOCDT,            " Document Date
      ' ' 'BKPF-BLART'  'KR',                " Type
      ' ' 'BKPF-BUKRS'   IT_SELECTED-BUKRS,  " Company Code
      ' ' 'BKPF-BUDAT'   W_POSDT,            " Posting Date
      ' ' 'BKPF-BLDAT'   W_DOCDT,            " Document Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  SPACE,               " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  '수입의뢰 비용',     " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
      ' ' 'RF05A-NEWBS' '31',                " Posting Key
      ' ' 'RF05A-NEWKO'  IT_SELECTED-ZFVEN,  " Account
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

* NEXT SCREEN.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0302',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
      ' ' 'BSEG-WMWST'  TEMP_WMWST,            " Tax
      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
      ' ' 'BSEG-MWSKZ'  IT_SELECTED-MWSKZ,     " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
      ' ' 'BSEG-ZTERM'  IT_SELECTED-ZTERM,     " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '수입의뢰 비용',       " 텍스트.
      ' ' 'RF05A-NEWBS' '40',                  " Posting Key
      ' ' 'RF05A-NEWKO' L_ZFIOCAC1,            " ACCOUNT
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
      ' ' 'BSEG-WRBTR'  '*',          " Amount
*      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,  " Amount
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
*      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'COBL-KOSTL'  COBL-KOSTL,            " Cost center.
*      ' ' 'COBL-PRCTR'  COBL-PRCTR,            " 손익센터.
      ' ' 'BSEG-SGTXT'  '수입의뢰 비용',       " 텍스트.
      ' ' 'BDC_OKCODE'  'BU'.        " 저장.

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
*     ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER

ENDFORM.                    " COST_BDC_INSERT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0010 OUTPUT.

  AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
                  ID 'ACTVT' FIELD '*'.
  W_SY_SUBRC = SY-SUBRC.

  LOOP AT SCREEN.

    IF W_SY_SUBRC NE 0 AND SY-DYNNR EQ '0010'.
       IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
          SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
          SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
          SCREEN-NAME(06) EQ 'BLOCK2'.
          SCREEN-INVISIBLE = 1.
       ENDIF.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0010 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
     EXIT.
  ENDIF.

   IF W_POSDT IS INITIAL.
      MESSAGE E167 WITH '증빙일'.
   ENDIF.
   IF W_DOCDT IS INITIAL.
      MESSAGE E167 WITH '전기일'.
   ENDIF.
   IF BSEG-GSBER IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'BSEG' 'GSBER'.
   ENDIF.
*   IF COBL-KOSTL IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'COBL' 'KOSTL'.
*   ENDIF.
*   IF COBL-PRCTR IS INITIAL.
*      PERFORM NO_INPUT(SAPFMMEX) USING 'COBL' 'PRCTR'.
*   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

   LOOP AT MESSTAB.

      MOVE : MESSTAB-MSGTYP           TO     IT_ERR_LIST-MSGTYP,
             MESSTAB-MSGID            TO     IT_ERR_LIST-MSGID,
             MESSTAB-MSGNR            TO     IT_ERR_LIST-MSGNR,
             MESSTAB-MSGV1            TO     IT_ERR_LIST-MSGV1,
             MESSTAB-MSGV2            TO     IT_ERR_LIST-MSGV2,
             MESSTAB-MSGV3            TO     IT_ERR_LIST-MSGV3,
             MESSTAB-MSGV4            TO     IT_ERR_LIST-MSGV4.


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
         WHEN OTHERS.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      ENDCASE.

      APPEND  IT_ERR_LIST.

   ENDLOOP.
*>> UNLOCKED.
   LOOP AT IT_LOCKED.
      PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFREQNO
                                           'U'    W_SUBRC.
   ENDLOOP.

ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '상태 LIST'.
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
         WRITE : / SY-ULINE(96),    /   SY-VLINE NO-GAP,
                   '유형'   NO-GAP,     SY-VLINE NO-GAP,
                   '메세지 텍스트',  94 SY-VLINE NO-GAP,
                   'T'      NO-GAP,     SY-VLINE,
                 / SY-ULINE(96).
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
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
         WRITE : / SY-ULINE(96).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SINGLE_MESS_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SINGLE_MESS_MAKE.

   REFRESH : MESSTAB.
   MOVE : 'E'                 TO     MESSTAB-MSGTYP,
          SY-MSGID            TO     MESSTAB-MSGID,
          SY-MSGNO            TO     MESSTAB-MSGNR,
          SY-MSGV1            TO     MESSTAB-MSGV1,
          SY-MSGV2            TO     MESSTAB-MSGV2,
          SY-MSGV3            TO     MESSTAB-MSGV3,
          SY-MSGV4            TO     MESSTAB-MSGV4.
   APPEND MESSTAB.

   PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.

*>> UNLOCKED.
   LOOP AT IT_LOCKED.
      PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFREQNO
                                           'U'    W_SUBRC.
   ENDLOOP.

ENDFORM.                    " P2000_SINGLE_MESS_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1489   text
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM P2000_SET_LOCK_MODE USING    P_ZFREQNO
                                  PA_MODE
                                  W_SUBRC.
  IF PA_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTRECST'
         EXPORTING
              ZFREQNO = P_ZFREQNO
         EXCEPTIONS
              OTHERS  = 1.

    W_SUBRC = SY-SUBRC.

    IF W_SUBRC <> 0.
       MESSAGE S510 WITH SY-MSGV1 'L/C Cost'
                         P_ZFREQNO ''
                    RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTRECST'
         EXPORTING
              ZFREQNO = P_ZFREQNO.

    W_SUBRC = SY-SUBRC.
  ENDIF.

ENDFORM.                    " P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_S_ZTERM_LOW  text
*----------------------------------------------------------------------*
FORM P1000_PAY_TERM_HELP USING    P_ZTERM.

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


ENDFORM.                    " P1000_PAY_TERM_HELP
*&---------------------------------------------------------------------*
*&      Form  P2000_GL_DATA_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM P2000_GL_DATA_MAKE USING    W_SUBRC.
DATA :   L_ZFPAY     LIKE   IT_SELECTED-ZFPAY.

  REFRESH : ACCOUNTGL,
            CURRENCYAMOUNT,
            EXTENSION1.
  CLEAR : ACCOUNTGL,
          CURRENCYAMOUNT,
          EXTENSION1,
          DOCUMENTHEADER.

*>>> CLIENT.
  CLEAR : T000.
  SELECT SINGLE * FROM T000.
*>>>
  CLEAR : TTYP.
  SELECT SINGLE * FROM TTYP
         WHERE    AWTYP   EQ   'BKPF'.

*>>HEADER DATA.
  MOVE: TTYP-AWTYP  TO        DOCUMENTHEADER-OBJ_TYPE, "참조절차.
*       TTYP-       TO        DOCUMENTHEADER-OBJ_KEY,  "오브젝트키.
        T000-LOGSYS TO        DOCUMENTHEADER-OBJ_SYS,  "원본 논리시스템.
        SY-UNAME    TO        DOCUMENTHEADER-USERNAME, "사용자이름.
  '수입의뢰 비용'   TO        DOCUMENTHEADER-HEADER_TXT, "전표텍스트.
        SPACE       TO        DOCUMENTHEADER-OBJ_KEY_R, " 취소:
        IT_SELECTED-BUKRS TO  DOCUMENTHEADER-COMP_CODE, " 회사코드.
        SPACE       TO        DOCUMENTHEADER-AC_DOC_NO, " 회계전표번호.
        SPACE       TO        DOCUMENTHEADER-FISC_YEAR, " 회계연도.
        W_DOCDT     TO        DOCUMENTHEADER-DOC_DATE,  " 증빙일.
        W_POSDT     TO        DOCUMENTHEADER-PSTNG_DATE, " 전표전기일.
        SPACE       TO        DOCUMENTHEADER-TRANS_DATE, " 환산일.
        SPACE       TO        DOCUMENTHEADER-FIS_PERIOD, " 회계기간.
        'KR'        TO        DOCUMENTHEADER-DOC_TYPE,   " 전표유형.
        SPACE       TO        DOCUMENTHEADER-REF_DOC_NO, " 참조문서번호.
        SPACE       TO        DOCUMENTHEADER-COMPO_ACC, "ACC 인터페이스.
        SPACE       TO        DOCUMENTHEADER-REASON_REV. "역분개사유


*>> 금액.
  W_WRBTR = IT_SELECTED-ZFCKAMT + IT_SELECTED-ZFVAT.
  WRITE      W_WRBTR   CURRENCY   'KRW'    TO  W_TEXT_AMOUNT.
  PERFORM    P2000_WRITE_NO_MASK(SAPLZIM1) CHANGING  W_TEXT_AMOUNT.

*  WRITE IT_SELECTED-ZFVAT CURRENCY 'KRW' TO TMP_WMWST.
*  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

  MOVE: 1           TO  CURRENCYAMOUNT-ITEMNO_ACC, "개별항목번호.
        '10'        TO  CURRENCYAMOUNT-CURR_TYPE,  "통화유형 및 평가뷰.
        'KRW'       TO  CURRENCYAMOUNT-CURRENCY,   " 통화키.
        SPACE       TO  CURRENCYAMOUNT-CURRENCY_ISO, "ISO 코드통화.
     W_TEXT_AMOUNT  TO  CURRENCYAMOUNT-AMT_DOCCUR,  "전표통화금액.
        1           TO  CURRENCYAMOUNT-EXCH_RATE,   "환율.
        SPACE       TO  CURRENCYAMOUNT-EXCH_RATE_V. "간접호가환율.
  APPEND  CURRENCYAMOUNT.

  MOVE: 2           TO  CURRENCYAMOUNT-ITEMNO_ACC, "개별항목번호.
        '10'        TO  CURRENCYAMOUNT-CURR_TYPE,  "통화유형 및 평가뷰.
        'KRW'       TO  CURRENCYAMOUNT-CURRENCY,   " 통화키.
        SPACE       TO  CURRENCYAMOUNT-CURRENCY_ISO, "ISO 코드통화.
     W_TEXT_AMOUNT  TO  CURRENCYAMOUNT-AMT_DOCCUR,  "전표통화금액.
        1           TO  CURRENCYAMOUNT-EXCH_RATE,   "환율.
        SPACE       TO  CURRENCYAMOUNT-EXCH_RATE_V. "간접호가환율.
  APPEND  CURRENCYAMOUNT.

  MOVE: 1            TO   ACCOUNTGL-ITEMNO_ACC, "회계전표 개별항목번호
  IT_SELECTED-ZFVEN  TO   ACCOUNTGL-GL_ACCOUNT, "총계정원장계정
  IT_SELECTED-BUKRS  TO   ACCOUNTGL-COMP_CODE, "회사코드
        W_POSDT      TO   ACCOUNTGL-PSTNG_DATE, "전표전기일
        'KR'         TO   ACCOUNTGL-DOC_TYPE, "전표유형
        SPACE        TO   ACCOUNTGL-AC_DOC_NO, "회계전표번호
        SPACE        TO   ACCOUNTGL-FISC_YEAR, "회계연도
        SPACE        TO   ACCOUNTGL-FIS_PERIOD, "회계기간
        SPACE        TO   ACCOUNTGL-STAT_CON, "통계용 개별항목지시자
        SPACE        TO   ACCOUNTGL-REF_KEY_1, "사업자 등록번호
        SPACE        TO   ACCOUNTGL-REF_KEY_2, "거래처참조키
        SPACE        TO   ACCOUNTGL-REF_KEY_3, "개별항목참조키
        SPACE        TO   ACCOUNTGL-CUSTOMER, "고객번호
  IT_SELECTED-ZFVEN  TO   ACCOUNTGL-VENDOR_NO, "구매처.
        SPACE        TO   ACCOUNTGL-ALLOC_NMBR, "지정번호
        '품목텍스트' TO   ACCOUNTGL-ITEM_TEXT, "품목텍스트
        SPACE        TO   ACCOUNTGL-BUS_AREA, "사업영역
        SPACE        TO   ACCOUNTGL-COSTCENTER, "코스트센터
        SPACE        TO   ACCOUNTGL-ACTTYPE, "액티비티유형
        SPACE        TO   ACCOUNTGL-ORDERID, "오더번호
        SPACE        TO   ACCOUNTGL-ORIG_GROUP, "원가요소분할 오리진그룹
        SPACE        TO   ACCOUNTGL-COST_OBJ, "원가집적대상
        SPACE        TO   ACCOUNTGL-PROFIT_CTR, "손익센터
        SPACE        TO   ACCOUNTGL-PART_PRCTR, "파트너 손익센터
        SPACE        TO   ACCOUNTGL-WBS_ELEMENT, "작업분할구조(WBS 요소)
        SPACE        TO   ACCOUNTGL-NETWORK, "계정지정 네트웍번호
        SPACE        TO   ACCOUNTGL-ROUTING_NO. "오더의 작업공정번호
  APPEND ACCOUNTGL.

  MOVE: 2            TO   ACCOUNTGL-ITEMNO_ACC, "회계전표 개별항목번호
  ZTIMIMG11-ZFIOCAC1 TO   ACCOUNTGL-GL_ACCOUNT, "총계정원장계정
  IT_SELECTED-BUKRS  TO   ACCOUNTGL-COMP_CODE, "회사코드
        W_POSDT      TO   ACCOUNTGL-PSTNG_DATE, "전표전기일
        'KR'         TO   ACCOUNTGL-DOC_TYPE, "전표유형
        SPACE        TO   ACCOUNTGL-AC_DOC_NO, "회계전표번호
        SPACE        TO   ACCOUNTGL-FISC_YEAR, "회계연도
        SPACE        TO   ACCOUNTGL-FIS_PERIOD, "회계기간
        SPACE        TO   ACCOUNTGL-STAT_CON, "통계용 개별항목지시자
        SPACE        TO   ACCOUNTGL-REF_KEY_1, "사업자 등록번호
        SPACE        TO   ACCOUNTGL-REF_KEY_2, "거래처참조키
        SPACE        TO   ACCOUNTGL-REF_KEY_3, "개별항목참조키
        SPACE        TO   ACCOUNTGL-CUSTOMER, "고객번호
  IT_SELECTED-ZFVEN  TO   ACCOUNTGL-VENDOR_NO, "구매처.
        SPACE        TO   ACCOUNTGL-ALLOC_NMBR, "지정번호
        '품목텍스트' TO   ACCOUNTGL-ITEM_TEXT, "품목텍스트
        SPACE        TO   ACCOUNTGL-BUS_AREA, "사업영역
        SPACE        TO   ACCOUNTGL-COSTCENTER, "코스트센터
        SPACE        TO   ACCOUNTGL-ACTTYPE, "액티비티유형
        SPACE        TO   ACCOUNTGL-ORDERID, "오더번호
        SPACE        TO   ACCOUNTGL-ORIG_GROUP, "원가요소분할 오리진그룹
        SPACE        TO   ACCOUNTGL-COST_OBJ, "원가집적대상
        SPACE        TO   ACCOUNTGL-PROFIT_CTR, "손익센터
        SPACE        TO   ACCOUNTGL-PART_PRCTR, "파트너 손익센터
        SPACE        TO   ACCOUNTGL-WBS_ELEMENT, "작업분할구조(WBS 요소)
        SPACE        TO   ACCOUNTGL-NETWORK, "계정지정 네트웍번호
        SPACE        TO   ACCOUNTGL-ROUTING_NO. "오더의 작업공정번호
  APPEND ACCOUNTGL.



*-----------------------------------------------------------------------
*>> G/L Posting.
  CALL FUNCTION  'ZIM_BAPI_ACC_GL_POSTING_POST'
       EXPORTING
           DOCUMENTHEADER      =   DOCUMENTHEADER
       IMPORTING
         OBJ_TYPE              =   OBJ_TYPE
         OBJ_KEY               =   OBJ_KEY
         OBJ_SYS               =   OBJ_SYS
       TABLES
         ACCOUNTGL             =   ACCOUNTGL
         CURRENCYAMOUNT        =   CURRENCYAMOUNT
         RETURN                =   RETURN
         EXTENSION1            =   EXTENSION1
       EXCEPTIONS
         OTHERS                =  1.

  W_SUBRC = SY-SUBRC.
* IF SY-SUBRC NE 0.
  IF NOT RETURN[] IS INITIAL.
     LOOP AT RETURN.
        WRITE : / RETURN.
     ENDLOOP.
*     READ TABLE RETURN INDEX 1.

*     MESSAGE ID   SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     W_SUBRC = 4.
  ENDIF.


  EXIT.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ IT_SELECTED-ZFWERKS.
*>> 지불처가 다를 경우.
  IF NOT ( IT_SELECTED-ZFPAY IS INITIAL ) AND
         ( IT_SELECTED-ZFVEN NE IT_SELECTED-ZFPAY ).
     L_ZFPAY  =  IT_SELECTED-ZFPAY. " Payee
  ELSE.
     CLEAR L_ZFPAY.
  ENDIF.

  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   W_DOCDT,            " Document Date
      ' ' 'BKPF-BLART'  'KR',                " Type
      ' ' 'BKPF-BUKRS'   IT_SELECTED-BUKRS,  " Company Code
      ' ' 'BKPF-BUDAT'   W_POSDT,            " Posting Date
      ' ' 'BKPF-BLDAT'   W_DOCDT,            " Document Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  SPACE,               " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  '수입의뢰 비용',     " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
      ' ' 'RF05A-NEWBS' '31',                " Posting Key
      ' ' 'RF05A-NEWKO'  IT_SELECTED-ZFVEN,  " Account
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

* NEXT SCREEN.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0302',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
      ' ' 'BSEG-WMWST'  TEMP_WMWST,            " Tax
      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
      ' ' 'BSEG-MWSKZ'  IT_SELECTED-MWSKZ,     " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
*      ' ' 'BSEG-GSBER' IT_SELECTED-ZFWERKS,    " Business Area
      ' ' 'BSEG-ZTERM'  IT_SELECTED-ZTERM,     " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '수입의뢰 비용',       " 텍스트.
      ' ' 'RF05A-NEWBS' '40',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFIOCAC1,    " ACCOUNT
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER

  W_WRBTR = IT_SELECTED-ZFCKAMT.
  WRITE W_WRBTR CURRENCY 'KRW' TO TEMP_WRBTR.

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,  " Amount
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-GSBER'  SPACE,                 " Business Area
      ' ' 'COBL-KOSTL'  SPACE,                 " Cost center.
      ' ' 'COBL-PRCTR'  SPACE,                 " 손익센터.
      ' ' 'BSEG-SGTXT'  '수입의뢰 비용',       " 텍스트.
      ' ' 'BDC_OKCODE'  'BU'.        " 저장.

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   SPACE,       " 사업영역.TEST
      ' ' 'COBL-KOSTL'   SPACE,       " COST CENTER TEST
      ' ' 'BDC_OKCODE'   '/00'.       " ENTER


ENDFORM.                    " P2000_GL_DATA_MAKE

*&---------------------------------------------------------------------*
*&      Form  VP_COST_BDC_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM VP_COST_BDC_INSERT.

DATA :   L_ZFPAY     LIKE   IT_SELECTED-ZFPAY,
         L_ZFIOCAC1  LIKE   ZTIMIMG11-ZFIOCAC1.

  IF IT_SELECTED-ZFTRIPLE EQ 'X'.
     L_ZFIOCAC1 = ZTIMIMG11-ZFIOCAC11.
  ELSE.
     L_ZFIOCAC1 = ZTIMIMG11-ZFIOCAC1.
  ENDIF.

  W_WRBTR = IT_SELECTED-ZFCKAMT - IT_SELECTED-ZFVPR.
*  W_WRBTR = IT_SELECTED-ZFCKAMT.

  WRITE W_WRBTR CURRENCY 'KRW' TO TEMP_WRBTR.
  WRITE IT_SELECTED-ZFVPR CURRENCY 'KRW' TO TEMP_WMWST.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ IT_SELECTED-ZFWERKS.
*>> 지불처가 다를 경우.
  IF NOT ( IT_SELECTED-ZFPAY IS INITIAL ) AND
         ( IT_SELECTED-ZFVEN NE IT_SELECTED-ZFPAY ).
     L_ZFPAY  =  IT_SELECTED-ZFPAY. " Payee
  ELSE.
     CLEAR L_ZFPAY.
  ENDIF.

  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   W_DOCDT,            " Document Date
      ' ' 'BKPF-BLART'  'SA',                " Type
      ' ' 'BKPF-BUKRS'   IT_SELECTED-BUKRS,  " Company Code
      ' ' 'BKPF-BUDAT'   W_POSDT,            " Posting Date
      ' ' 'BKPF-BLDAT'   W_DOCDT,            " Document Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  SPACE,               " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  '수입의뢰 비용(VP)', " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
      ' ' 'RF05A-NEWBS' '40',                " Posting Key
      ' ' 'RF05A-NEWKO'  '90204000',         " GR/IR-본사수입비용.
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

*>>>>
  CLEAR : LFA1.
  SELECT SINGLE * FROM LFA1
                  WHERE LIFNR EQ
                            ( SELECT LIFNR
                              FROM   ZTREQHD
                              WHERE  ZFREQNO EQ IT_SELECTED-ZFREQNO ).
*                                 IT_SELECTED-ZFVEN.

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
*      ' ' 'BSEG-WRBTR'  '*',          " Amount
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,  " Amount
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
*      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'COBL-KOSTL'  COBL-KOSTL,            " Cost center.
*      ' ' 'COBL-PRCTR'  COBL-PRCTR,            " 손익센터.
      ' ' 'BSEG-SGTXT'  '보험료(VP발생)',       " 텍스트.
      ' ' 'RF05A-NEWBS' '01',                  " Posting Key
      ' ' 'RF05A-NEWKO' LFA1-KUNNR,            " CUSTOMER CODE.
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER..

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
*     ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER

* NEXT SCREEN.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0301',
      ' ' 'BSEG-HKONT'  '15100300',            ">미수금-VP로 HARD CODING
      ' ' 'BSEG-WRBTR'  TEMP_WMWST,            " Amount(VP금액).
*      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
*      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
*      ' ' 'BSEG-MWSKZ'  IT_SELECTED-MWSKZ,     " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
      ' ' 'BSEG-ZTERM'  IT_SELECTED-ZTERM,     " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  'VP 금액',             " 텍스트.
      ' ' 'RF05A-NEWBS' '31',                  " Posting Key
      ' ' 'RF05A-NEWKO' IT_SELECTED-ZFVEN,     " ACCOUNT(거래처)
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER

* NEXT SCREEN.
  W_WRBTR = IT_SELECTED-ZFCKAMT.
  WRITE W_WRBTR CURRENCY 'KRW' TO TEMP_WRBTR.

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0302',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
*      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
*      ' ' 'BSEG-MWSKZ'  IT_SELECTED-MWSKZ,     " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
      ' ' 'BSEG-ZTERM'  IT_SELECTED-ZTERM,     " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '수입의뢰 비용',       " 텍스트.
*      ' ' 'RF05A-NEWBS' '40',                  " Posting Key
*      ' ' 'RF05A-NEWKO' L_ZFIOCAC1,            " ACCOUNT
      ' ' 'BDC_OKCODE'  '=BU'.                 " ENTER

ENDFORM.                    " VP_COST_BDC_INSERT
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
