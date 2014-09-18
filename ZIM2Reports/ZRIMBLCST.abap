*&---------------------------------------------------------------------*
*& Report  ZRIMBLCST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : B/L 비용 Posting
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.05.23                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : B/L 비용을 조회하여 BDC로 Posting한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBLCST    MESSAGE-ID ZIM
                     LINE-SIZE 124
                     NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          ZRIMBLCSTTOP,
          ZRIMBDCCOM,
          ZRIMBAIPTOP.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       BUKRS           LIKE   ZTBLCST-BUKRS,
       ZFHBLNO(16)     TYPE   C,                " House B/L No
       ZFBLNO          LIKE   ZTBLCST-ZFBLNO,   " B/L 관리번?
       ZFCSQ           LIKE   ZTBLCST-ZFCSQ,    " 비용순?
       ZFCSCD          LIKE   ZTBLCST-ZFCSCD,   " 비용구?
       ZFCSCD_NM(10)   TYPE   C,                " 비용명?
       ZFCAMT          LIKE   ZTBLCST-ZFCAMT,   " 비용금?
       WAERS           LIKE   ZTBLCST-WAERS,    " 통?
       ZFCKAMT         LIKE   ZTBLCST-ZFCKAMT,  " 비용원화금액_유?
       KRW             LIKE   ZTBLCST-KRW,      " 원화통?
       ZFEXRT          LIKE   ZTBLCST-ZFEXRT,   " Exchange Rate
       ZFVAT           LIKE   ZTBLCST-ZFVAT,    " V.A.T
       ZFOCDT          LIKE   ZTBLCST-ZFOCDT,   " 지급?
       ZFVEN           LIKE   ZTBLCST-ZFVEN,    " Vendor
       ZFVEN_NM(20)    TYPE   C,                " Vendor ?
       ZFPAY           LIKE   ZTBLCST-ZFPAY,    " 지불?
       ZFPAY_NM(20)    TYPE   C,                " 지불처?
       ZTERM           LIKE   ZTBLCST-ZTERM,    " Terms of Payment
       MWSKZ           LIKE   ZTBLCST-MWSKZ,    " Tax Code
       ZFWERKS         LIKE   ZTBLCST-ZFWERKS,  " Plant
       ZFPOYN          LIKE   ZTBL-ZFPOYN,      " 유환여?
       SUM_MARK        TYPE   C.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_VEN    FOR ZTBLCST-ZFVEN,     " Vendor
                   S_PAY    FOR ZTBLCST-ZFPAY,     " 지불?
                   S_ZTERM  FOR ZTBLCST-ZTERM,     " Terms Of Payment
                   S_MWSKZ  FOR ZTBLCST-MWSKZ,     " Tax Code
                   S_WERKS  FOR ZTBLCST-ZFWERKS,   " Plant
                   S_OCDT   FOR ZTBLCST-ZFOCDT,    " 지불처.
                   S_CSCD   FOR ZTBLCST-ZFCSCD     " 비용구분.
                            MATCHCODE OBJECT ZIC8,
                   S_CSCD1  FOR ZTBLCST-ZFCSCD     " 보세비용코드.
                            MATCHCODE OBJECT ZIC6A,
                   S_HBLNO  FOR ZTBL-ZFHBLNO.      " House B/L No

SELECTION-SCREEN END OF BLOCK B1.

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
*            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
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

  SELECT SINGLE *
    FROM ZTIMIMG11.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ B/L 비용 POSTING ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 106 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'House B/L No    '               NO-GAP, SY-VLINE NO-GAP,
            '비용구분      '                 NO-GAP, SY-VLINE NO-GAP,
            '          발생금액      '       NO-GAP, SY-VLINE NO-GAP,
            '             V.A.T '            NO-GAP, SY-VLINE NO-GAP,
            '발생일    '                     NO-GAP, SY-VLINE NO-GAP,
            'Vendor                        ' NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                           SY-VLINE NO-GAP,
            'B/L 관리NO'                     NO-GAP, SY-VLINE NO-GAP,
            'Seq  '                          NO-GAP, SY-VLINE NO-GAP,
            '환율          '                 NO-GAP, SY-VLINE NO-GAP,
            '          원화금액      '       NO-GAP, SY-VLINE NO-GAP,
            '회사/Term/Tax/Plant/유환'       NO-GAP,
            '      '                         NO-GAP, SY-VLINE NO-GAP,
            '지불처                        ' NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

  CLEAR : SV_ZFVEN, SV_ZFPAY,  SV_ZTERM, SV_MWSKZ, SV_ZFWERKS,
          SV_WAERS, SV_ZFPOYN, SUM_ZFVAT, SUM_ZFCAMT, W_LOOP_CNT.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMV2'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMV2'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
*>> 초기 비교값 MOVE.
      IF W_LINE  =  1.
         MOVE   IT_TAB-BUKRS    TO  SV_BUKRS.
         MOVE   IT_TAB-ZFVEN    TO  SV_ZFVEN.
         MOVE   IT_TAB-ZFPAY    TO  SV_ZFPAY.
         MOVE   IT_TAB-ZTERM    TO  SV_ZTERM.
         MOVE   IT_TAB-MWSKZ    TO  SV_MWSKZ.
         MOVE   IT_TAB-WAERS    TO  SV_WAERS.
         MOVE   IT_TAB-ZFPOYN   TO  SV_ZFPOYN.
         MOVE   IT_TAB-ZFWERKS  TO  SV_ZFWERKS.
         MOVE   IT_TAB-ZFVEN_NM TO  SV_ZFVEN_NM.
         MOVE   IT_TAB-ZFPAY_NM TO  SV_ZFPAY_NM.
      ENDIF.
*>> 비교값이 틀리면 SUM WRITE.
      IF SV_ZFVEN    NE  IT_TAB-ZFVEN   OR
         SV_ZFPAY    NE  IT_TAB-ZFPAY   OR
         SV_ZTERM    NE  IT_TAB-ZTERM   OR
         SV_MWSKZ    NE  IT_TAB-MWSKZ   OR
         SV_ZFWERKS  NE  IT_TAB-ZFWERKS OR
         SV_WAERS    NE  IT_TAB-WAERS   OR
         SV_ZFPOYN   NE  IT_TAB-ZFPOYN.
         PERFORM P3000_SUM_LINE_WRITE.

         MOVE   IT_TAB-BUKRS    TO  SV_BUKRS.
         MOVE   IT_TAB-ZFVEN    TO  SV_ZFVEN.
         MOVE   IT_TAB-ZFPAY    TO  SV_ZFPAY.
         MOVE   IT_TAB-ZTERM    TO  SV_ZTERM.
         MOVE   IT_TAB-MWSKZ    TO  SV_MWSKZ.
         MOVE   IT_TAB-WAERS    TO  SV_WAERS.
         MOVE   IT_TAB-ZFWERKS  TO  SV_ZFWERKS.
         MOVE   IT_TAB-ZFVEN_NM TO  SV_ZFVEN_NM.
         MOVE   IT_TAB-ZFPAY_NM TO  SV_ZFPAY_NM.
         MOVE   IT_TAB-ZFPOYN   TO  SV_ZFPOYN.
         MOVE   0               TO  SUM_ZFCAMT.
         MOVE   0               TO  SUM_ZFVAT.
      ENDIF.

      PERFORM P3000_LINE_WRITE.
      ADD       IT_TAB-ZFCAMT   TO  SUM_ZFCAMT.
      ADD       IT_TAB-ZFVAT    TO  SUM_ZFVAT.

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

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE, '  '       NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFHBLNO          NO-GAP, " House B/L No
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCSCD           NO-GAP, " 비용구?
       ' '                     NO-GAP,
       IT_TAB-ZFCSCD_NM        NO-GAP, " 비용명?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCAMT CURRENCY IT_TAB-WAERS NO-GAP, " 비용금?
       IT_TAB-WAERS            NO-GAP, " 금액통?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVAT CURRENCY IT_TAB-KRW NO-GAP, " V.A.T
       SY-VLINE                NO-GAP,
       IT_TAB-ZFOCDT           NO-GAP, " 지급?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFVEN            NO-GAP, " Vendor
       IT_TAB-ZFVEN_NM         NO-GAP, " Vendor ?
       SY-VLINE                NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_BACKGROUND.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFBLNO           NO-GAP, " B/L 관리번?
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCSQ            NO-GAP, " 비용순?
       SY-VLINE                NO-GAP,
       '  '                    NO-GAP,
       IT_TAB-ZFEXRT           NO-GAP, " Exchange Rate
       SY-VLINE                NO-GAP,
       IT_TAB-ZFCKAMT  CURRENCY IT_TAB-KRW NO-GAP, " 비용원화금?
       IT_TAB-KRW              NO-GAP, " 원화통?
       SY-VLINE                NO-GAP,
       IT_TAB-BUKRS            NO-GAP,
       '/'                     NO-GAP,
       IT_TAB-ZTERM            NO-GAP, " Terms Of Payment
       '/'                     NO-GAP,
       IT_TAB-MWSKZ            NO-GAP, " Tax Code
       ' /'                    NO-GAP,
       IT_TAB-ZFWERKS          NO-GAP, " Plant
       ' / '                   NO-GAP,
       IT_TAB-ZFPOYN           NO-GAP, " 유?
       '        '              NO-GAP,
       SY-VLINE                NO-GAP,
       IT_TAB-ZFPAY            NO-GAP, " 지불?
       IT_TAB-ZFPAY_NM         NO-GAP, " 지불처 ?
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
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH : IT_TAB, IT_ZTIMIMG08.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG08
  FROM    ZTIMIMG08
  WHERE  ( ZFCDTY  =  '004' OR       ZFCDTY  =  '005' )
  AND      ZFCD1   =  'N'.

  SELECT  H~ZFHBLNO  I~ZFBLNO   I~ZFCSQ  I~ZFCSCD  I~ZFEXRT
          I~ZFCAMT   I~ZFCKAMT  I~ZFVAT  I~ZTERM   I~MWSKZ
          I~ZFWERKS  I~ZFOCDT   I~ZFVEN  I~ZFPAY   I~WAERS
          H~ZFPOYN   I~BUKRS    I~WAERS
  INTO    CORRESPONDING  FIELDS  OF  TABLE  IT_TAB
  FROM    ZTBL    AS  H  INNER  JOIN  ZTBLCST  AS  I
  ON      H~ZFBLNO  EQ  I~ZFBLNO
  FOR ALL ENTRIES    IN  IT_ZTIMIMG08
  WHERE   I~ZFVEN    IN  S_VEN
  AND     I~ZFCSCD   IN  S_CSCD
  AND     I~ZFCSCD   IN  S_CSCD1
  AND     I~ZFPAY    IN  S_PAY
  AND     I~ZTERM    IN  S_ZTERM
  AND     I~MWSKZ    IN  S_MWSKZ
  AND     I~ZFWERKS  IN  S_WERKS
  AND     I~ZFOCDT   IN  S_OCDT
  AND     H~ZFHBLNO  IN  S_HBLNO
  AND     I~ZFCSCD   EQ  IT_ZTIMIMG08-ZFCD
  AND     I~ZFCAMT   GT  0
  AND   ( I~ZFACDO   EQ  SPACE
  OR      I~ZFACDO   IS  NULL ).

* 무환이면서 PLANED COST RATE 인 비용 SELECT.
  REFRESH  IT_ZTIMIMG08.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG08
  FROM    ZTIMIMG08
  WHERE  ( ZFCDTY  =  '004' OR       ZFCDTY  =  '005' )
  AND      ZFCD1   =  'Y'.

  SELECT  H~ZFHBLNO  I~ZFBLNO   I~ZFCSQ  I~ZFCSCD  I~ZFEXRT
          I~ZFCAMT   I~ZFCKAMT  I~ZFVAT  I~ZTERM   I~MWSKZ
          I~ZFWERKS  I~ZFOCDT   I~ZFVEN  I~ZFPAY   I~WAERS
          H~ZFPOYN   I~BUKRS    I~WAERS
  APPENDING CORRESPONDING  FIELDS  OF  TABLE  IT_TAB
  FROM    ZTBL    AS  H  INNER  JOIN  ZTBLCST  AS  I
  ON      H~ZFBLNO  EQ  I~ZFBLNO
  FOR ALL ENTRIES    IN  IT_ZTIMIMG08
  WHERE   I~ZFVEN    IN  S_VEN
  AND     I~ZFCSCD   IN  S_CSCD
  AND     I~ZFPAY    IN  S_PAY
  AND     I~ZTERM    IN  S_ZTERM
  AND     I~MWSKZ    IN  S_MWSKZ
  AND     I~ZFWERKS  IN  S_WERKS
  AND     I~ZFOCDT   IN  S_OCDT
  AND     H~ZFHBLNO  IN  S_HBLNO
  AND     I~ZFCSCD   EQ  IT_ZTIMIMG08-ZFCD
  AND     I~ZFCAMT   GT  0
  AND     H~ZFPOYN   EQ  'N'
  AND   ( I~ZFACDO   EQ  SPACE
  OR      I~ZFACDO   IS  NULL ).

  REFRESH  IT_ZTIMIMG08.

  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_ZTIMIMG08
  FROM    ZTIMIMG08
  WHERE  ( ZFCDTY  =  '004' OR       ZFCDTY  =  '005' ).

*>> INTERNAL TABLE 에 VENDOR 명, 지급처명, 비용명 DISPLAY
  LOOP  AT  IT_TAB.

    W_TABIX  =  SY-TABIX.
    SELECT SINGLE  NAME1  INTO  IT_TAB-ZFVEN_NM
    FROM   LFA1    WHERE  LIFNR EQ  IT_TAB-ZFVEN.

    SELECT SINGLE  NAME1  INTO  IT_TAB-ZFPAY_NM
    FROM   LFA1    WHERE  LIFNR EQ  IT_TAB-ZFPAY.

    READ  TABLE IT_ZTIMIMG08  WITH KEY ZFCD = IT_TAB-ZFCSCD.
    IF SY-SUBRC EQ 0.
       MOVE  IT_ZTIMIMG08-ZFCDNM  TO  IT_TAB-ZFCSCD_NM.
    ENDIF.

    MOVE  'KRW'   TO  IT_TAB-KRW.
    MODIFY  IT_TAB  INDEX  W_TABIX.

  ENDLOOP.

  SORT  IT_TAB  BY  ZFVEN  ZFPAY  ZTERM  MWSKZ  ZFWERKS  WAERS ZFPOYN.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 1.
     MESSAGE S738. W_ERR_CHK = 'Y'.
  ENDIF.

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
         MOVE : SV_ZFVEN       TO IT_SELECTED-ZFVEN,
                SV_BUKRS       TO IT_SELECTED-BUKRS,
                SV_ZFPAY       TO IT_SELECTED-ZFPAY,
                SV_ZTERM       TO IT_SELECTED-ZTERM,
                SV_MWSKZ       TO IT_SELECTED-MWSKZ,
                SV_WAERS       TO IT_SELECTED-WAERS,
                SV_ZFPOYN      TO IT_SELECTED-ZFPOYN,
                SV_ZFWERKS     TO IT_SELECTED-ZFWERKS,
                SUM_ZFCAMT     TO IT_SELECTED-ZFCAMT,
                SUM_ZFVAT      TO IT_SELECTED-ZFVAT.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUM_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE         NO-GAP,
       '                     '              NO-GAP,
       '    합계 : '    NO-GAP,
       SUM_ZFCAMT    CURRENCY SV_WAERS NO-GAP, " 유환금?
       SV_WAERS      NO-GAP,
       SY-VLINE         NO-GAP,
       '                              '     NO-GAP,
       SY-VLINE         NO-GAP,
       SV_ZFVEN         NO-GAP,                      " Vendor
       SV_ZFVEN_NM      NO-GAP,                      " Vendor ?
       SY-VLINE         NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: SV_BUKRS,   SV_ZFVEN,    SV_ZFPAY, SV_ZTERM, SV_MWSKZ,
             SV_WAERS,   SV_ZFWERKS,  SUM_ZFCAMT, SUM_ZFVAT, SV_ZFPOYN.

       WRITE : / SY-VLINE, ' ', SY-VLINE  NO-GAP,
       '                     '    NO-GAP, " 오류금?
       '  V.A.T :  '        NO-GAP,
       IT_TAB-ZFVAT CURRENCY IT_TAB-KRW NO-GAP, " VAT
       'KRW  '                          NO-GAP,
       SY-VLINE   NO-GAP,
       SV_BUKRS                   NO-GAP,
       '/'                        NO-GAP,
       SV_ZTERM                   NO-GAP, " Terms Of Payment
       '/'                        NO-GAP,
       SV_MWSKZ                   NO-GAP, " Tax Code
       ' /'                       NO-GAP,
       SV_ZFWERKS                 NO-GAP, " Plant
       ' /'                       NO-GAP,
       SV_ZFPOYN                  NO-GAP,
       '         '                NO-GAP,
       SY-VLINE                   NO-GAP,
       SV_ZFPAY                   NO-GAP, " 지불?
       SV_ZFPAY_NM                NO-GAP, " 지불처 ?
       SY-VLINE                   NO-GAP.

* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: SV_BUKRS,   SV_ZFVEN,    SV_ZFPAY, SV_ZTERM, SV_MWSKZ,
        SV_ZFWERKS, SV_WAERS,    SV_ZFPOYN, SUM_ZFCAMT,  SUM_ZFVAT.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_SUM_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P4000_COST_POST
*&---------------------------------------------------------------------*
FORM P4000_COST_POST USING    W_ERR_CHK.
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
        MESSAGE S167 WITH 'Company code'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF IT_SELECTED-ZTERM IS INITIAL.
        MESSAGE S167 WITH 'Payment notice'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF IT_SELECTED-ZFVEN IS INITIAL.
        MESSAGE S167 WITH 'Vendor code'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF IT_SELECTED-ZFWERKS IS INITIAL.
        MESSAGE S167 WITH 'Plant'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
*>> 해당 수입비용.
     REFRESH  : IT_ZTBLCST.
     LOOP AT IT_TAB WHERE  BUKRS   EQ   IT_SELECTED-BUKRS
                    AND    ZFVEN   EQ   IT_SELECTED-ZFVEN
                    AND    ZFPAY   EQ   IT_SELECTED-ZFPAY
                    AND    ZTERM   EQ   IT_SELECTED-ZTERM
                    AND    MWSKZ   EQ   IT_SELECTED-MWSKZ
                    AND    ZFWERKS EQ   IT_SELECTED-ZFWERKS
                    AND    WAERS   EQ   IT_SELECTED-WAERS
                    AND    ZFPOYN  EQ   IT_SELECTED-ZFPOYN.

        SELECT *  APPENDING TABLE IT_ZTBLCST
                  FROM   ZTBLCST
                  WHERE  ZFBLNO   EQ   IT_TAB-ZFBLNO
                  AND    ZFCSQ    EQ   IT_TAB-ZFCSQ.

     ENDLOOP.
*>> LOCK MODE.
*    CLEAR : W_LOCK_CHK.
*    SORT IT_ZTBLCST BY ZFBLNO.
*   LOOP AT IT_ZTBLCST.
*       ON CHANGE OF IT_ZTBLCST-ZFBLNO.
*           PERFORM  P2000_SET_LOCK_MODE   USING IT_ZTBLCST-ZFBLNO
*                                               'L'    W_SUBRC.
*         IF SY-SUBRC EQ 0.
*            MOVE IT_ZTBLCST-ZFBLNO TO IT_LOCKED-ZFBLNO.
*            APPEND IT_LOCKED.
*         ELSE.
*            W_LOCK_CHK = 'Y'.
*            PERFORM  P2000_SINGLE_MESS_MAKE.
*            EXIT.
*         ENDIF.
*      ENDON.
*   ENDLOOP.

     IF W_LOCK_CHK EQ 'Y'.
        CONTINUE.
     ENDIF.
*>> BAPIs Function
*     PERFORM  P2000_GL_DATA_MAKE    USING   W_SUBRC.
**>> BDC DATA MAKE.
     PERFORM COST_BDC_INSERT.
*>>PARAMETER CLEAR.
     SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
     SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.

*>> BDC CALL.
     PERFORM P2000_CALL_TRANSACTION USING     'F-42'
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

        LOOP AT IT_ZTBLCST.
           MOVE : ZFACDO      TO     IT_ZTBLCST-ZFACDO,
                  ZFFIYR      TO     IT_ZTBLCST-ZFFIYR,
                  W_POSDT     TO     IT_ZTBLCST-ZFPSDT,
                  W_DOCDT     TO     IT_ZTBLCST-ZFOCDT,
                  SY-UNAME    TO     IT_ZTBLCST-UNAM,
                  SY-DATUM    TO     IT_ZTBLCST-UDAT.
           MODIFY  IT_ZTBLCST  INDEX   SY-TABIX.
        ENDLOOP.
        MODIFY ZTBLCST FROM TABLE IT_ZTBLCST.
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

  IF IT_SELECTED-ZFPOYN = 'Y'.
     L_ZFIOCAC1 = ZTIMIMG11-ZFIOCAC2.
  ELSE.
     L_ZFIOCAC1 = ZTIMIMG11-ZFIOCAC3.
  ENDIF.

  IF IT_SELECTED-WAERS  =  'KRW'.
     W_WRBTR = IT_SELECTED-ZFCAMT + IT_SELECTED-ZFVAT.
     WRITE W_WRBTR CURRENCY 'KRW' TO TEMP_WRBTR.
  ELSE.
     WRITE IT_SELECTED-ZFCAMT CURRENCY IT_SELECTED-WAERS TO TEMP_WRBTR.
  ENDIF.
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
      ' ' 'BKPF-WAERS'  IT_SELECTED-WAERS,    " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  SPACE,               " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  'BL 비용',           " 전표헤더텍스트.
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
      ' ' 'BSEG-SGTXT'  'BL 비용',             " 텍스트.
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
      ' ' 'BSEG-SGTXT'  'BL 비용',              " 텍스트.
      ' ' 'BDC_OKCODE'  'BU'.                   " 저장.

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
*     ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER


ENDFORM.                    " COST_BDC_INSERT

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
*      PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFBLNO
*                                           'U'    W_SUBRC.
   ENDLOOP.

ENDFORM.                    " P2000_SINGLE_MESS_MAKE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE TABLES   IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

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
*      PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFBLNO
*                                           'U'    W_SUBRC.
   ENDLOOP.


ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_SELECT_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UCOMM  text
*----------------------------------------------------------------------*
FORM P2000_SELECT_RECORD USING    P_SY_UCOMM.
DATA : WL_MARK.

   IF P_SY_UCOMM EQ 'MKAL'.
      WL_MARK = 'X'.
   ELSEIF P_SY_UCOMM EQ 'MKLO'.
      CLEAR : WL_MARK.
   ENDIF.
   DO.
      CLEAR MARKFIELD.
      READ LINE SY-INDEX FIELD VALUE MARKFIELD.
      IF SY-SUBRC NE 0.    EXIT.   ENDIF.
      MODIFY CURRENT LINE FIELD VALUE MARKFIELD FROM WL_MARK.
   ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH 'Status LIST'.
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
                   'Type'   NO-GAP,     SY-VLINE NO-GAP,
                   'Message text',  94 SY-VLINE NO-GAP,
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
*&      Module  CHECK_INPUT_FIELD_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0010 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
     EXIT.
  ENDIF.

   IF W_POSDT IS INITIAL.
      MESSAGE E167 WITH 'Evidence date'.
   ENDIF.
   IF W_DOCDT IS INITIAL.
      MESSAGE E167 WITH 'Posting date'.
   ENDIF.
   IF BSEG-GSBER IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'BSEG' 'GSBER'.
   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0010  INPUT
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
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0010 OUTPUT.

ENDMODULE.                 " MODIFY_SCREEN_SCR0010  OUTPUT
