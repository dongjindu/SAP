*&---------------------------------------------------------------------*
*& Report  ZRIMCGCSTLST                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : 하역 비용 회계처리 현황                               *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2001.02.28                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 하역비용을 조회한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBLCSTLST    MESSAGE-ID ZIM
                        LINE-SIZE 128
                        NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* TABLE 및 INTERNAL TABLE, 변수 Define
*-----------------------------------------------------------------------
TABLES : ZTCGCST,
         ZTCGHD ,
         ZTMSHD ,
         ZTIMIMG00,
         LFA1   .

DATA : BEGIN OF IT_MSNO OCCURS 0,
       ZFMSNO          LIKE   ZTMSHD-ZFMSNO.
DATA : END   OF IT_MSNO.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFCD3           LIKE   ZTIMIMG08-ZFCD3,
       ZFCSCD          LIKE   ZTCGCST-ZFCSCD,
       ZFCDNM(10)      TYPE   C,
       ZFCKAMT         LIKE   ZTCGCST-ZFCKAMT,
       ZFKRW           LIKE   ZTCGCST-ZFKRW,
       BUKRS           LIKE   ZTCGCST-BUKRS,
       GJAHR           LIKE   ZTCGCST-GJAHR,
       BELNR           LIKE   ZTCGCST-BELNR,
       ZFOCDT          LIKE   ZTCGCST-ZFOCDT,
       ZFPSDT          LIKE   ZTCGCST-ZFPSDT,
       LIFNR           LIKE   ZTCGCST-LIFNR,
       ZFUPCST         LIKE   ZTCGCST-ZFUPCST,
       ZFPCST          LIKE   ZTCGCST-ZFPCST,
       NAME1(25)       TYPE   C,
       ZFPAY           LIKE   ZTCGCST-ZFPAY,
       NAME2(25)       TYPE   C,
       ZFMSNO          LIKE   ZTCGHD-ZFMSNO,
       ZFMSNM(25)      TYPE   C,
       WERKS           LIKE   ZTCGCST-WERKS,
       MWSKZ           LIKE   ZTCGCST-MWSKZ,
       ZTERM           LIKE   ZTCGCST-ZTERM,
       ZFCGPT          LIKE   ZTCGHD-ZFCGPT.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* 변수 선언.
*-----------------------------------------------------------------------
DATA : W_ERR_CHK    TYPE  C,
       W_LIFNR      LIKE  ZTCGCST-LIFNR,
       W_GJAHR      LIKE  ZTCGCST-GJAHR,
       W_BELNR      LIKE  ZTCGCST-BELNR,
       W_ZFCSCD     LIKE  ZTCGCST-ZFCSCD,
       W_TOTAMT     LIKE  ZTCGCST-ZFCKAMT,
       W_HAB        LIKE  ZTCGCST-ZFCKAMT,
       W_ZFCST      LIKE  ZTCGCST-ZFUPCST,
       W_ZFMSNM(25) TYPE  C,
       W_NAME1(25)  TYPE  C,
       W_ZFCDNM(10) TYPE  C,
       W_LIST_INDEX LIKE  SY-TABIX,
       W_PAGE       TYPE  I,
       W_LINE       TYPE  I,
       W_JUL        TYPE  C,
       W_COUNT      TYPE  I.
INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_VEN    FOR ZTCGCST-LIFNR,     " Vendor
                   S_PAY    FOR ZTCGCST-ZFPAY,     " 지불?
                   S_WERKS  FOR ZTCGCST-WERKS,   " Plant
                   S_OCDT   FOR ZTCGCST-ZFOCDT,    " 발생?
                   S_PSDT   FOR ZTCGCST-ZFPSDT,    " 회계처리?
                   S_MSNM   FOR ZTMSHD-ZFMSNM,     " 모선명.
                   S_CGPT   FOR ZTCGHD-ZFCGPT,     " 하역항.
                   S_ZTERM  FOR ZTCGCST-ZTERM,     " Terms Of Payment
                   S_MWSKZ  FOR ZTCGCST-MWSKZ,     " Tax Code
                   S_CSCD   FOR ZTCGCST-ZFCSCD     " 비용구분.
                            MATCHCODE OBJECT ZIC9.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* 회계처리 여?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-002, POSITION 1.
     SELECTION-SCREEN : COMMENT 36(3) TEXT-021, POSITION 40.
     PARAMETERS : P_Y   RADIOBUTTON GROUP RDG.  " Yes
     SELECTION-SCREEN : COMMENT 51(2) TEXT-022, POSITION 54.
     PARAMETERS : P_N   RADIOBUTTON GROUP RDG.  " No
     SELECTION-SCREEN : COMMENT 64(3) TEXT-023, POSITION 68.
     PARAMETERS : P_A   RADIOBUTTON GROUP RDG.  " ALL
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
* Sort By
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-003, POSITION 1.
     SELECTION-SCREEN : COMMENT 33(6) TEXT-031, POSITION 40.
     PARAMETERS : P_V   RADIOBUTTON GROUP RDG1. " Vendor?
     SELECTION-SCREEN : COMMENT 43(8) TEXT-033, POSITION 52.
     PARAMETERS : P_S   RADIOBUTTON GROUP RDG1. " 전표?
     SELECTION-SCREEN : COMMENT 55(8) TEXT-035, POSITION 64.
     PARAMETERS : P_M   RADIOBUTTON GROUP RDG1. " 모선명.
     SELECTION-SCREEN : COMMENT 67(8) TEXT-034, POSITION 76.
     PARAMETERS : P_C   RADIOBUTTON GROUP RDG1. " 집계구분.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

INITIALIZATION.                          " 초기값 SETTING
   SET  TITLEBAR 'ZIMR45'.           " GUI TITLE SETTING..
   PERFORM   P2000_INIT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-LOW.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-LOW.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_ZTERM-HIGH.
   PERFORM   P1000_PAY_TERM_HELP  USING  S_ZTERM-HIGH.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

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
      WHEN 'DOWN'.          " FILE DOWNLOAD....
*            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'FB03'.                    " 회계전표 조?
            IF IT_TAB-GJAHR IS INITIAL.
               MESSAGE S252. EXIT.
            ENDIF.
            IF IT_TAB-BELNR EQ SPACE.
               MESSAGE S252. EXIT.
            ENDIF.
            PERFORM P2000_SHOW_SL USING IT_TAB-GJAHR
                                        IT_TAB-BELNR.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.
  P_V = 'X'.
  P_Y = 'X'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 하역 비용 회계처리 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / ' Date : ', SY-DATUM, 110 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,
            '비용          '              NO-GAP, SY-VLINE NO-GAP,
            '모선명                   '   NO-GAP, SY-VLINE NO-GAP,
            '          원화금액       '   NO-GAP, SY-VLINE NO-GAP,
            '발생일    '                  NO-GAP, SY-VLINE NO-GAP,
            'Vendor              '        NO-GAP,
            '               '             NO-GAP, SY-VLINE NO-GAP,
            'Plnt/Tx/Term'                NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,
            '              '              NO-GAP, SY-VLINE NO-GAP,
            '전표번호                 '   NO-GAP, SY-VLINE NO-GAP,
            '          배부금액       '   NO-GAP, SY-VLINE NO-GAP,
            '회계처리일'                  NO-GAP, SY-VLINE NO-GAP,
            '지불처              '        NO-GAP,
            '               '             NO-GAP, SY-VLINE NO-GAP,
            '하역항      '                NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING     W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMR45'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR45'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.
   CLEAR : W_LIFNR, W_GJAHR, W_BELNR, W_ZFMSNM, W_ZFCSCD.
   CLEAR : W_TOTAMT, W_HAB.

   LOOP AT IT_TAB.
      W_LINE   = W_LINE         + 1.
      W_HAB    = W_HAB          + IT_TAB-ZFCKAMT.
      W_ZFCST  = IT_TAB-ZFUPCST + IT_TAB-ZFPCST.
* 모선명 SELECT!
      SELECT SINGLE ZFMSNM  INTO   IT_TAB-ZFMSNM
      FROM   ZTMSHD         WHERE  ZFMSNO = IT_TAB-ZFMSNO.
* 비용코드명 SELECT!
      SELECT SINGLE ZFCDNM  INTO  IT_TAB-ZFCDNM
      FROM   ZTIMIMG08
      WHERE  ZFCDTY EQ '007'
      AND    ZFCD   EQ IT_TAB-ZFCSCD.
* 지불처, Vendor 명 구하기.
      SELECT SINGLE NAME1  INTO  IT_TAB-NAME1
      FROM   LFA1
      WHERE  LIFNR  =  IT_TAB-LIFNR.

      SELECT SINGLE NAME1 INTO  IT_TAB-NAME2
      FROM   LFA1
      WHERE  LIFNR  =  IT_TAB-ZFPAY .

      IF IT_TAB-GJAHR EQ SPACE.
         W_JUL = ' '.
      ELSE.
         W_JUL = '-'.
      ENDIF.

      PERFORM P2000_PAGE_CHECK.
      IF W_LINE = 1.
         W_LIFNR  = IT_TAB-LIFNR.
         W_NAME1  = IT_TAB-NAME1.
         W_GJAHR  = IT_TAB-GJAHR.
         W_BELNR  = IT_TAB-BELNR.
         W_ZFCSCD = IT_TAB-ZFCSCD.
         W_ZFCDNM = IT_TAB-ZFCDNM.
         W_ZFMSNM = IT_TAB-ZFMSNM.
      ENDIF.

*>> VENDOR 순으로 WRITE.
      IF P_V = 'X'.
         IF W_LIFNR NE IT_TAB-LIFNR.
            PERFORM P3000_SUM_LINE_WRITE.
            CLEAR W_TOTAMT.
            W_LIFNR = IT_TAB-LIFNR.
            W_NAME1 = IT_TAB-NAME1.
         ENDIF.
*>> 전표순으로 WRITE.
      ELSEIF P_S = 'X'.
         IF W_GJAHR NE IT_TAB-GJAHR OR W_BELNR NE IT_TAB-BELNR.
            PERFORM P3000_SUM_LINE_WRITE.
            CLEAR W_TOTAMT.
            W_GJAHR = IT_TAB-GJAHR.
            W_BELNR = IT_TAB-BELNR.
         ENDIF.
*>> 모선명으로 WRITE.
      ELSEIF P_M = 'X'.
         IF W_ZFMSNM NE IT_TAB-ZFMSNM.
            PERFORM P3000_SUM_LINE_WRITE.
            CLEAR W_TOTAMT.
            W_ZFMSNM = IT_TAB-ZFMSNM.
         ENDIF.
*>> 비용코드순으로 WRITE.
      ELSEIF P_C = 'X'.
         IF W_ZFCSCD NE IT_TAB-ZFCSCD.
            PERFORM P3000_SUM_LINE_WRITE.
            CLEAR W_TOTAMT.
            W_ZFCSCD = IT_TAB-ZFCSCD.
            W_ZFCDNM = IT_TAB-ZFCDNM.
         ENDIF.
      ENDIF.
      PERFORM P3000_LINE_WRITE.
      W_TOTAMT = W_TOTAMT + IT_TAB-ZFCKAMT.

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

  WRITE:/ SY-VLINE NO-GAP,
       IT_TAB-ZFCSCD           NO-GAP,
       ' '                     NO-GAP,
       IT_TAB-ZFCDNM           NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFMSNM           NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFCKAMT CURRENCY 'KRW'   NO-GAP,
       ' '                     NO-GAP,
       IT_TAB-ZFKRW            NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFOCDT           NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-LIFNR            NO-GAP,
       IT_TAB-NAME1            NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-WERKS            NO-GAP,
       '/'                     NO-GAP,
       IT_TAB-MWSKZ            NO-GAP,
       '/'                     NO-GAP,
       IT_TAB-ZTERM            NO-GAP, SY-VLINE NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_BACKGROUND.
       WRITE :/ SY-VLINE        NO-GAP,
       '          '          NO-GAP,
       '    '                  NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-GJAHR            NO-GAP,
       W_JUL                   NO-GAP,
       IT_TAB-BELNR            NO-GAP,
       '          '            NO-GAP, SY-VLINE NO-GAP,
       '                    '  NO-GAP,
       '     '                 NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFPSDT           NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFPAY            NO-GAP,
       IT_TAB-NAME2            NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFCGPT           NO-GAP,
       '         '             NO-GAP, SY-VLINE NO-GAP.
* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
     WRITE:/ SY-VLINE          NO-GAP,
       '총계          '        NO-GAP,
       '                    '  NO-GAP,
       '      '                NO-GAP, SY-VLINE NO-GAP,
       W_HAB   CURRENCY 'KRW'  NO-GAP,
       ' '                     NO-GAP,
       'KRW  '                 NO-GAP,
       '                    '  NO-GAP,
       '                    '  NO-GAP,
       '                    '  NO-GAP, SY-VLINE NO-GAP.
     WRITE : / SY-ULINE.
     FORMAT RESET.
     WRITE : / ' 총', W_COUNT, '건'.
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

* 조건의 모선명에 해당하는 모선번호 SELECT.
  SELECT ZFMSNO
  INTO   CORRESPONDING FIELDS OF TABLE IT_MSNO
  FROM   ZTMSHD
  WHERE  ZFMSNM    IN  S_MSNM .

* 해당 화면의 조건에 해당하는 DATA SETTING!
  SELECT *
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM   ZTCGCST AS H INNER JOIN ZTCGHD AS I
  ON     H~ZFCGNO     EQ    I~ZFCGNO
  FOR ALL ENTRIES     IN    IT_MSNO
  WHERE  H~LIFNR      IN    S_VEN
  AND    H~ZFPAY      IN    S_PAY
  AND    H~WERKS      IN    S_WERKS
  AND    H~ZFOCDT     IN    S_OCDT
  AND    H~ZFPSDT     IN    S_PSDT
  AND    I~ZFCGPT     IN    S_CGPT
  AND    H~MWSKZ      IN    S_MWSKZ
  AND    H~ZTERM      IN    S_ZTERM
  AND    H~ZFCSCD     IN    S_CSCD
  AND    I~ZFMSNO     EQ    IT_MSNO-ZFMSNO
  AND    H~ZFCKAMT    GT    0.

* 회계처리 자료 SETTING!
  IF P_Y = 'X'.
     DELETE IT_TAB WHERE BELNR EQ SPACE.
  ELSEIF P_N = 'X'.
     DELETE IT_TAB WHERE BELNR NE SPACE.
  ENDIF.
* 조건에 맞춰서 SORT.
  IF P_V = 'X'.
     SORT IT_TAB  BY  LIFNR   GJAHR  BELNR.
  ELSEIF P_S = 'X'.
     SORT IT_TAB  BY  GJAHR   BELNR.
  ELSEIF P_M = 'X'.
     SORT IT_TAB  BY  ZFMSNM  GJAHR  BELNR.
  ELSEIF P_C = 'X'.
     SORT IT_TAB  BY  ZFCSCD  GJAHR  BELNR.
  ENDIF.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0. MESSAGE S738. ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_SL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_SL USING    P_ZFFIYR P_ZFACDO.

  SET PARAMETER ID 'BUK'     FIELD IT_TAB-BUKRS.
  SET PARAMETER ID 'GJR'     FIELD P_ZFFIYR.
  SET PARAMETER ID 'BLN'     FIELD P_ZFACDO.
  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_SL

*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_SUM_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

  IF P_V = 'X'.  "Sort By Vendor
     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       '                    '  NO-GAP,
       '      '                NO-GAP, SY-VLINE NO-GAP,
       W_TOTAMT CURRENCY 'KRW' NO-GAP,
       ' '                     NO-GAP,
       'KRW  '                 NO-GAP, SY-VLINE NO-GAP,
       W_LIFNR                 NO-GAP,
       ' '                     NO-GAP,
       W_NAME1                 NO-GAP,
       '                    '  NO-GAP,
       '   '                   NO-GAP, SY-VLINE NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.

  IF P_S = 'X'.  "Sort By 전표.
     IF W_GJAHR EQ SPACE. W_JUL = ' '.
     ELSE.  W_JUL = '-'.
     ENDIF.

     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       '                    '  NO-GAP,
       '      '                NO-GAP, SY-VLINE NO-GAP,
       W_TOTAMT CURRENCY 'KRW' NO-GAP,
       ' '                     NO-GAP,
       'KRW  '                 NO-GAP, SY-VLINE NO-GAP,
       W_GJAHR                 NO-GAP,
       W_JUL                   NO-GAP,
       W_BELNR                 NO-GAP,
       '                    '  NO-GAP,
       '                    '  NO-GAP,
       '    '                  NO-GAP, SY-VLINE NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.

  IF P_M = 'X'.  "Sort By 모선.
     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       '                    '  NO-GAP,
       '      '                NO-GAP, SY-VLINE NO-GAP,
       W_TOTAMT CURRENCY 'KRW' NO-GAP,
       ' '                     NO-GAP,
       'KRW  '                 NO-GAP, SY-VLINE NO-GAP,
       W_ZFMSNM                NO-GAP,
       '                    '  NO-GAP,
       '              '        NO-GAP, SY-VLINE NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.

  IF P_C = 'X'.  "Sort By 비용구분.
     WRITE:/ SY-VLINE          NO-GAP,
       '합계          '        NO-GAP,
       '                    '  NO-GAP,
       '      '                NO-GAP, SY-VLINE NO-GAP,
       W_TOTAMT CURRENCY 'KRW' NO-GAP,
       ' '                     NO-GAP,
       'KRW  '                 NO-GAP, SY-VLINE NO-GAP,
       W_ZFCSCD                NO-GAP,
       ' '                     NO-GAP,
       W_ZFCDNM                NO-GAP,
       '                    '  NO-GAP,
       '                    '  NO-GAP,
       '     '                 NO-GAP, SY-VLINE NO-GAP.
     WRITE : / SY-ULINE.
  ENDIF.
ENDFORM.                    " P3000_SUM_LINE_WRITE
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
