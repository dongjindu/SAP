*&---------------------------------------------------------------------*
*& Report  ZRIMLCDTLST
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입실적                                              *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.04                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLCDLST    MESSAGE-ID ZIM
                     LINE-SIZE 143
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREQNO     LIKE ZTREQHD-ZFREQNO,         " 수입의뢰 번호.
       PAMDNO(16)  TYPE C,                       " ALL AMEND 회차.
       WAERS       LIKE ZTREQHD-WAERS,           " Currency
       ZFBENI      LIKE ZTREQHD-ZFBENI,          " Beneficairy
       NAME2(24),                                " Name 1
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " Open Bank
       NAME3(24),                                " Name 1
       MENGE       LIKE ZTREQIT-MENGE,           " 수량.
       EBELN       LIKE ZTREQHD-EBELN,
       TXZ01       LIKE ZTREQIT-TXZ01,           " 자재내역.
       MEINS       LIKE ZTREQIT-MEINS,           " 수량단위.
       ZFAMDNO     LIKE ZTREQST-ZFAMDNO,         " AMEND 회차.
       ZFUSD       LIKE ZTREQST-ZFUSD,           " USD Currency
       ZFOPNNO     LIKE ZTREQST-ZFOPNNO,         " L/C NO
       ZFOPNDT     LIKE ZTREQST-ZFOPNDT,         " 개설일.
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       ZFRECNO     LIKE ZTREQIL-ZFRECNO.         " 수입추천번호.
DATA : END OF IT_TAB.
DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
       ZFREQNO     LIKE ZTREQHD-ZFREQNO,         " 수입의뢰 번호.
       PAMDNO(16)  TYPE C,                       " ALL AMEND 회차.
       WAERS       LIKE ZTREQHD-WAERS,           " Currency
       ZFBENI      LIKE ZTREQHD-ZFBENI,          " Beneficairy
       NAME2(24),                                " Name 1
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " Open Bank
       NAME3(24),                                " Name 1
       MENGE(18)   TYPE C,                       " 수량.
       TXZ01       LIKE ZTREQIT-TXZ01,           " 자재내역.
       MEINS       LIKE ZTREQIT-MEINS,           " 수량단위.
       ZFAMDNO     LIKE ZTREQST-ZFAMDNO,         " AMEND 회차.
       ZFUSD       LIKE ZTREQST-ZFUSD,           " USD Currency
       ZFOPNNO     LIKE ZTREQST-ZFOPNNO,         " L/C NO
       ZFOPNDT     LIKE ZTREQST-ZFOPNDT,         " 개설일.
       ZFOPAMT(18) TYPE C,                       " 개설금액.
       ZFUSDAM(18) TYPE C,                       " USD 환산금액.
       ZFRECNO     LIKE ZTREQIL-ZFRECNO.         " 수입추천번호.
DATA : END OF IT_TAB_DOWN.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE   ZRIMLCDLSTTOP.
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                                NO INTERVALS,
                   S_RLDT    FOR ZTREQST-ZFOPNDT,  " 개설일.
                   S_OPBN    FOR ZTREQHD-ZFOPBN,   " 개설은행.
                   S_MATGB   FOR ZTREQHD-ZFMATGB,  " 자재구분.
                   S_REQTY   FOR ZTREQHD-ZFREQTY,  " 수입의뢰 Type
                   S_WERKS   FOR ZTREQHD-ZFWERKS,  " 대표 plant
                   S_EKORG   FOR ZTREQST-EKORG,    " Purch. Org.
                   S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                   S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                   S_REQNO   FOR ZTREQHD-ZFREQNO.  " 수입의뢰 관리번호.
SELECTION-SCREEN END OF BLOCK B1.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
*  테이블 SELECT
   PERFORM   P1000_GET_ZTREQHD      USING   W_ERR_CHK.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   CASE SY-UCOMM.
* SORT 선택.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택.
         W_FIELD_NM = 'ZFOPBN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'DISP'.                    " L/C 조회.
         PERFORM P2000_SHOW_LC USING  IT_TAB-ZFREQNO
                                      IT_TAB-ZFAMDNO.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_CREATE_DOWNLOAD_FILE.
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
*  테이블 SELECT
           PERFORM   P1000_GET_ZTREQHD       USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR24'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /65  '[ Open Result List by open date ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,
                 (10)  'Open date',       SY-VLINE,
                 (25)  'Open Bank',       SY-VLINE,
                 (35)  'Purchase Ord. - Approve No', SY-VLINE,
                  (5)  'Cur.',            SY-VLINE,
                 (15)  '   Open Amount',  SY-VLINE,
                 (15)  'Convert Amount',  SY-VLINE,
                 (16)  'Import Req. No',  SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE,
                 (38)  'Vendor',          SY-VLINE,
                 (35)  'Item',            SY-VLINE,
                  (5)  'Unit  ',          SY-VLINE,
                 (15)  '      Quantity',  SY-VLINE,
                 (14) 'I/L NO' RIGHT-JUSTIFIED,124 SY-VLINE.
  FORMAT COLOR COL_TOTAL INTENSIFIED.
  WRITE:         (16) 'Amend.',           SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTREQHD   USING   W_ERR_CHK.

  REFRESH IT_TAB.
  SELECT *  FROM ZTREQHD    WHERE   ZFOPBN     IN     S_OPBN
                             AND    ZFMATGB    IN     S_MATGB
                             AND    ZFREQTY    IN     S_REQTY
                             AND    BUKRS      IN     S_BUKRS
                             AND    ZFWERKS    IN     S_WERKS
                             AND    EBELN      IN     S_EBELN
                             AND    LIFNR      IN     S_LIFNR
                             AND    ZFBENI     IN     S_ZFBENI
                             AND    ZFREQNO    IN     S_REQNO.
     CLEAR IT_TAB.
     MOVE-CORRESPONDING ZTREQHD TO IT_TAB.

     SELECT MAX( ZFAMDNO ) INTO W_MAX_ZFAMDNO
       FROM ZTREQST
      WHERE ZFREQNO = ZTREQHD-ZFREQNO.
     MOVE W_MAX_ZFAMDNO TO W_MAX_ZFAMDNO_OLD.

     SELECT SINGLE *
       FROM ZTREQST
      WHERE ZFREQNO = ZTREQHD-ZFREQNO
        AND ZFAMDNO = W_MAX_ZFAMDNO .

     IF ZTREQST-ZFDOCST NE 'O'.
        IF W_MAX_ZFAMDNO  = '00000'.
           CONTINUE.
        ELSE.
           W_MAX_ZFAMDNO  = W_MAX_ZFAMDNO  - 1.
        ENDIF.
     ENDIF.
     SELECT SINGLE *
       FROM ZTREQST
      WHERE ZFREQNO = ZTREQHD-ZFREQNO
        AND ZFAMDNO = W_MAX_ZFAMDNO
        AND ZFOPNDT    IN     S_RLDT
        AND EKORG      IN     S_EKORG.
     IF SY-SUBRC NE 0 .
        CONTINUE.
     ENDIF.
     MOVE:   ZTREQST-ZFAMDNO TO IT_TAB-ZFAMDNO,
             ZTREQST-ZFUSD   TO IT_TAB-ZFUSD,
             ZTREQST-ZFOPNNO TO IT_TAB-ZFOPNNO,
             ZTREQST-ZFOPNDT TO IT_TAB-ZFOPNDT,
             ZTREQST-ZFOPAMT TO IT_TAB-ZFOPAMT,
             ZTREQST-ZFUSDAM TO IT_TAB-ZFUSDAM.
     IF IT_TAB-ZFAMDNO =  W_MAX_ZFAMDNO_OLD.
         MOVE IT_TAB-ZFAMDNO TO IT_TAB-PAMDNO.
     ELSE.
         CONCATENATE:
          IT_TAB-ZFAMDNO  '/'  W_MAX_ZFAMDNO_OLD  INTO IT_TAB-PAMDNO.
     ENDIF.

*>> 수입추천 회수.
     CLEAR W_MAX_ZFILSEQ.
     SELECT MAX( ZFILSEQ  ) INTO W_MAX_ZFILSEQ
       FROM ZTREQIL
      WHERE ZFREQNO = IT_TAB-ZFREQNO.
     IF NOT W_MAX_ZFILSEQ IS INITIAL.
        SELECT SINGLE *
          FROM ZTREQIL
         WHERE ZFREQNO = IT_TAB-ZFREQNO
           AND ZFILSEQ = W_MAX_ZFILSEQ .
        MOVE ZTREQIL-ZFRECNO TO  IT_TAB-ZFRECNO.
     ENDIF.

*>> SELECT TEXT.
     CLEAR LFA1.
     SELECT SINGLE *
       FROM LFA1
      WHERE LIFNR = IT_TAB-ZFBENI.
     MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.
     CLEAR LFA1.
     SELECT SINGLE *
       FROM LFA1
      WHERE LIFNR = IT_TAB-ZFOPBN.
     MOVE: LFA1-NAME1   TO   IT_TAB-NAME3.

*>> SELECT ITEM.
    SELECT COUNT( DISTINCT ZFITMNO  ) INTO W_COUNT
      FROM ZTREQIT
     WHERE ZFREQNO = IT_TAB-ZFREQNO.
    SELECT MIN( ZFITMNO  ) INTO W_MIN_ZFITMNO
       FROM ZTREQIT
      WHERE ZFREQNO = IT_TAB-ZFREQNO.
    CLEAR ZTREQIT.
    SELECT SINGLE *
      FROM ZTREQIT
     WHERE ZFREQNO = IT_TAB-ZFREQNO
       AND ZFITMNO = W_MIN_ZFITMNO.

     MOVE: ZTREQIT-TXZ01 TO IT_TAB-TXZ01,
           ZTREQIT-MEINS TO IT_TAB-MEINS,
           ZTREQIT-MENGE TO IT_TAB-MENGE.
    IF W_COUNT > 1.
       CLEAR: W_ITCOUNT.
       W_ITCOUNT = W_COUNT - 1.
       CONCATENATE:
          IT_TAB-TXZ01  'And'  W_ITCOUNT 'Case'   INTO IT_TAB-TXZ01.
    ENDIF.
    APPEND IT_TAB.
  ENDSELECT.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0. MESSAGE S009. EXIT.ENDIF.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMR24'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR24'.           " GUI TITLE SETTING..

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
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFAMDNO  TO ZFAMDNO.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFAMDNO TO IT_SELECTED-ZFAMDNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S951.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION

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
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   FORMAT RESET.
*   FORMAT COLOR COL_NEGATIVE INVERSE.
   SUM.
   WRITE:/ SY-VLINE,96 'TOTAL:',
          105 IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD,
          143 SY-VLINE.
   WRITE:/ SY-ULINE.
   FORMAT COLOR OFF.
   IF W_COUNT GT 0.
      WRITE : / 'Total', W_COUNT, 'Case'.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   WRITE:/ SY-VLINE,
          (10) IT_TAB-ZFOPNDT,   SY-VLINE,   " 개설일.
          (25) IT_TAB-NAME3,     SY-VLINE,   " 개설은.
          (10) IT_TAB-EBELN, '-',
          (22) IT_TAB-ZFOPNNO,   SY-VLINE,   " L/C NO
           (5) IT_TAB-WAERS,     SY-VLINE,   " currency
          (15) IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS,SY-VLINE,
          (15) IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD,SY-VLINE,
          (16) IT_TAB-ZFREQNO, SY-VLINE.
* hide
  HIDE: IT_TAB.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE:/ SY-VLINE,
          (38) IT_TAB-NAME2,     SY-VLINE,   " 송화인.
          (35) IT_TAB-TXZ01,     SY-VLINE,   " 품목.
           (5) IT_TAB-MEINS,     SY-VLINE,   " 단위.
          (15) IT_TAB-MENGE UNIT IT_TAB-MEINS,SY-VLINE, " 수량.
          (14) IT_TAB-ZFRECNO RIGHT-JUSTIFIED, 124 SY-VLINE.
   FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
   WRITE: (16) IT_TAB-PAMDNO,    SY-VLINE,
         / SY-ULINE.
* hide
   HIDE: IT_TAB.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.

   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
   EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

   IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

  REFRESH IT_TAB_DOWN.
  LOOP AT IT_TAB.
    CLEAR IT_TAB_DOWN.
    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
    WRITE : IT_TAB-MENGE   UNIT     IT_TAB-MEINS TO IT_TAB_DOWN-MENGE,
            IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD TO IT_TAB_DOWN-ZFUSDAM,
            IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFOPAMT.
    APPEND IT_TAB_DOWN.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
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
