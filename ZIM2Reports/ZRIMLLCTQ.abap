*&---------------------------------------------------------------------*
*& Report  ZRIMLLCTQ                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : Local L/C별 거래내역현?
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.06.26                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Local L/C별 거래내역현황을 조회한다.
*&                                                                     *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLLCTQ    MESSAGE-ID ZIM
                     LINE-SIZE 105
                     NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFGFDYR         LIKE   ZTVTIV-ZFGFDYR,    " 물대 회계전표연?
       ZFGFDNO         LIKE   ZTVTIV-ZFGFDNO,    " 물대 회계전표번?
       ZFIVAMT         LIKE   ZTVTIVIT-ZFIVAMT,  " 발생금?
       ZFKAMT          LIKE   ZTVTIVIT-ZFKAMT,   " 발생금액(원화)
       ZFIVAMT_1       LIKE   ZTVTIVIT-ZFIVAMT,  " 접수금?
       ZFKAMT_1        LIKE   ZTVTIVIT-ZFKAMT,   " 접수금액(원화)
       ZFREDNO         LIKE   ZTVTIV-ZFREDNO,    " 인수증 관리번?
       ZFVTNO          LIKE   ZTVTIV-ZFVTNO,     " 세금계산서 관리번?
       ZFDODT          LIKE   ZTVTIV-ZFDODT,     " 인수일(작성일)
       ZFIVAMT_2       LIKE   ZTVTIVIT-ZFIVAMT,  " 결제금?
       ZFKAMT_2        LIKE   ZTVTIVIT-ZFKAMT.   " 결제금액(원화)
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMLLCTQTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(12) TEXT-001, POSITION 15.
        PARAMETERS : P_OPNNO LIKE ZTREQHD-ZFOPNNO.  " L/C No
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
*  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'DSRQ'.                   " 수입의뢰 조?
            PERFORM P2000_SHOW_LC USING W_ZFREQNO.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /35  '[ Local L/C별 거래내역 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM,  87 'Page : ', W_PAGE.
  WRITE : /.
  WRITE : /2  'L/C NO : '   NO-GAP, W_ZFOPNNO,
           40 '관리번호 : ' NO-GAP, W_ZFREQNO,
           86 '개설일 : '   NO-GAP, W_ZFOPNDT.
  WRITE : /2  '거래선 : '   NO-GAP, W_ZFBENI NO-GAP, W_ZFBENI_NM,
           40 '개설은행 : ' NO-GAP, W_ZFOPBN NO-GAP, W_ZFOPBN_NM,
           86 '유효일 : '   NO-GAP, W_ZFEXDT.
  WRITE : /2  '환율   : '   NO-GAP, W_ZFEXRT.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE                    NO-GAP,
            '              개설금액 '   NO-GAP, SY-VLINE NO-GAP,
            '          발생금액 '       NO-GAP, SY-VLINE NO-GAP,
            '          결제금액 '       NO-GAP, SY-VLINE NO-GAP,
            '              잔액 '       NO-GAP, SY-VLINE NO-GAP,
            '        미입고잔액 '       NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE                         NO-GAP,
            W_ZFOPAMTC                       NO-GAP,
            W_ZFOPAMT   CURRENCY W_ZFOPAMTC  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPAMT_1 CURRENCY W_ZFOPAMTC  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPAMT_2 CURRENCY W_ZFOPAMTC  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPAMT_3 CURRENCY W_ZFOPAMTC  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPAMT_4 CURRENCY W_ZFOPAMTC  NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-VLINE                    NO-GAP,
            'KRW  '                     NO-GAP,
            W_ZFOPKAM   CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPKAM_1 CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPKAM_2 CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPKAM_3 CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
            ' '                              NO-GAP,
            W_ZFOPKAM_4 CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,
            '              전표번호 '     NO-GAP, SY-VLINE NO-GAP,
            '          발생금액 ' NO-GAP, SY-VLINE NO-GAP,
            '          접수금액 ' NO-GAP, SY-VLINE NO-GAP,
            '            인수일 ' NO-GAP, SY-VLINE NO-GAP,
            '          결제금액 ' NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

   SET PF-STATUS 'ZIMX3'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMX3'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
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

  WRITE:/ SY-VLINE NO-GAP,
       '        '         NO-GAP,
       IT_TAB-ZFGFDYR                       NO-GAP, " 물대 회계전표연?
       '-'                                  NO-GAP,
       IT_TAB-ZFGFDNO                       NO-GAP, " 물대 회계전표번?
       SY-VLINE NO-GAP,
       IT_TAB-ZFIVAMT CURRENCY W_ZFOPAMTC   NO-GAP, " 발생금?
       SY-VLINE NO-GAP,
       IT_TAB-ZFIVAMT_1 CURRENCY W_ZFOPAMTC NO-GAP, " 접수금?
       SY-VLINE NO-GAP,
       '         '                          NO-GAP,
       IT_TAB-ZFDODT                        NO-GAP, " 인수?
       SY-VLINE NO-GAP,
       IT_TAB-ZFIVAMT_2 CURRENCY W_ZFOPAMTC NO-GAP, " 결제금?
       SY-VLINE NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE:/ SY-VLINE NO-GAP,
       '                      ', SY-VLINE      NO-GAP,
       IT_TAB-ZFKAMT CURRENCY 'KRW'    NO-GAP, " 발생금?
       SY-VLINE NO-GAP,
       IT_TAB-ZFKAMT_1 CURRENCY 'KRW'  NO-GAP, " 접수금?
       SY-VLINE NO-GAP,
       '                   '                    NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFKAMT_2 CURRENCY 'KRW'  NO-GAP, " 결제금?
       SY-VLINE NO-GAP.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / SY-ULINE.    WRITE : / '총', W_COUNT, '건'.
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

  PERFORM P1000_READ_HEAD.

  REFRESH IT_TAB.
  SELECT *
    FROM ZTVTIV.
*   WHERE EBELN = ZTREQHD-EBELN.
         CLEAR IT_TAB.
         MOVE-CORRESPONDING ZTVTIV TO IT_TAB.
         SELECT SUM( ZFIVAMT ) SUM( ZFKAMT )
           INTO (IT_TAB-ZFIVAMT, IT_TAB-ZFKAMT)
           FROM ZTVTIVIT
          WHERE ZFGFDYR  = IT_TAB-ZFGFDYR
            AND ZFGFDNO = IT_TAB-ZFGFDNO.
         IF IT_TAB-ZFREDNO IS INITIAL.
            APPEND  IT_TAB.
            CONTINUE.
         ENDIF.
         CLEAR W_ZFPNNO.
         SELECT MAX( ZFPNNO ) INTO W_ZFPNNO
           FROM ZTPMTIV
          WHERE ZFGFDYR = IT_TAB-ZFGFDYR
            AND ZFGFDNO = IT_TAB-ZFGFDNO.
         IF W_ZFPNNO IS INITIAL.
            APPEND  IT_TAB.
            CONTINUE.
         ENDIF.
         CLEAR ZTPMTHD.
         SELECT SINGLE *
           FROM ZTPMTHD
          WHERE ZFPNNO = W_ZFPNNO.
         IF ZTPMTHD-ZFPYST = 'N'.
            MOVE IT_TAB-ZFIVAMT TO IT_TAB-ZFIVAMT_1. " 접수금?
            MOVE IT_TAB-ZFKAMT  TO IT_TAB-ZFKAMT_1.  " 접수금액-원?
         ENDIF.
         IF ZTPMTHD-ZFPYST = 'C'.
            MOVE IT_TAB-ZFIVAMT TO IT_TAB-ZFIVAMT_1. " 접수금?
            MOVE IT_TAB-ZFKAMT  TO IT_TAB-ZFKAMT_1.  " 접수금액-원?
            MOVE IT_TAB-ZFIVAMT TO IT_TAB-ZFIVAMT_2. " 결제금?
            MOVE IT_TAB-ZFKAMT  TO IT_TAB-ZFKAMT_2.  " 결제금액-원?
         ENDIF.
         APPEND  IT_TAB.
  ENDSELECT.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
      PERFORM P3000_LINE_WRITE.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_HEAD
*&---------------------------------------------------------------------*
FORM P1000_READ_HEAD.

  CLEAR : W_ZFREQNO,   W_ZFOPNNO,   W_ZFBENI,    W_ZFBENI_NM, W_ZFOPBN,
          W_ZFOPBN_NM, W_ZFOPNDT,   W_ZFEXDT,    W_ZFEXRT,    W_ZFOPAMT,
          W_ZFOPKAM,   W_ZFOPAMT_1, W_ZFOPKAM_1, W_ZFOPAMT_2,
          W_ZFOPKAM_2, W_ZFOPAMT_3, W_ZFOPKAM_3, W_ZFOPAMT_4,
          W_ZFOPKAM_4, W_ZFAMDNO,   W_ZFOPAMTC.

  SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
    FROM ZTREQHD
   WHERE ZFOPNNO = P_OPNNO
     AND ZFREQTY = 'LO'.

  CLEAR ZTREQHD.
  SELECT SINGLE *
    FROM ZTREQHD
   WHERE ZFREQNO = W_ZFREQNO.
  IF SY-SUBRC NE 0.
     MESSAGE E865.
     EXIT.
  ENDIF.

  MOVE ZTREQHD-ZFOPNNO  TO W_ZFOPNNO. "L/C No
  MOVE ZTREQHD-ZFBENI   TO W_ZFBENI. " Vendor
  SELECT SINGLE NAME1   INTO W_ZFBENI_NM
    FROM LFA1
   WHERE LIFNR = W_ZFBENI.
  MOVE ZTREQHD-ZFOPBN TO W_ZFOPBN. " 개설은?
  SELECT SINGLE NAME1 INTO W_ZFOPBN_NM
    FROM LFA1
   WHERE LIFNR = W_ZFOPBN.
  SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
    FROM ZTREQST
   WHERE ZFREQNO = W_ZFREQNO
     AND ZFDOCST = 'O'.
  SELECT SINGLE ZFOPNDT INTO W_ZFOPNDT "개설?
    FROM ZTREQST
   WHERE ZFREQNO = W_ZFREQNO
     AND ZFAMDNO = W_ZFAMDNO.
  IF W_ZFAMDNO = 0.
     CLEAR ZTLLCHD.
     SELECT SINGLE *
       FROM ZTLLCHD
      WHERE ZFREQNO = W_ZFREQNO.
     MOVE ZTLLCHD-ZFEXDT   TO W_ZFEXDT.  " 유효기?
     MOVE ZTLLCHD-ZFEXRT   TO W_ZFEXRT.  " 환?
     MOVE ZTLLCHD-ZFOPAMT  TO W_ZFOPAMT. " 개설금?
     MOVE ZTLLCHD-ZFOPAMTC TO W_ZFOPAMTC. " 개설금액통?
     MOVE ZTLLCHD-ZFOPKAM  TO W_ZFOPKAM. " 개설금액(원화)
  ELSE.
     CLEAR ZTLLCAMHD.
     SELECT SINGLE *
       FROM ZTLLCAMHD
      WHERE ZFREQNO = W_ZFREQNO
        AND ZFAMDNO = W_ZFAMDNO.
     MOVE ZTLLCAMHD-ZFNEXDT   TO W_ZFEXDT. " 유효기?
     MOVE ZTLLCAMHD-ZFEXRT    TO W_ZFEXRT. " 환?
     MOVE ZTLLCAMHD-ZFNOAMT   TO W_ZFOPAMT. " 개설금?
     MOVE ZTLLCAMHD-WAERS     TO W_ZFOPAMTC. " 개설금액통?
     MOVE ZTLLCAMHD-ZFNOPKAM  TO W_ZFOPKAM. " 개설금액(원화)
  ENDIF.

  SELECT SUM( ZFIVAMT )  SUM( ZFKAMT ) " 발생금?
    INTO (W_ZFOPAMT_1,  W_ZFOPKAM_1)
    FROM  ZVVTIV_IT
   WHERE  EBELN = ZTREQHD-EBELN.
  W_ZFOPAMT_4 = W_ZFOPAMT - W_ZFOPAMT_1. " 미입고잔?
  W_ZFOPKAM_4 = W_ZFOPKAM - W_ZFOPKAM_1. " 미입고잔액(원화)
  SELECT SUM( ZFTIVAM )  SUM( ZFTIVAMK ) " 결제금?
    INTO (W_ZFOPAMT_2,  W_ZFOPKAM_2)
    FROM  ZTPMTHD
   WHERE  EBELN = ZTREQHD-EBELN
     AND  ZFPYST = 'C'.
  W_ZFOPAMT_3 = W_ZFOPAMT - W_ZFOPAMT_2. " 결?
  W_ZFOPKAM_3 = W_ZFOPKAM - W_ZFOPKAM_2. " 결제잔액(원화)


ENDFORM.                    " P1000_READ_HEAD

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC
