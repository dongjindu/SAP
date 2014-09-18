*&---------------------------------------------------------------------*
*& Report  ZRIMLLCNI
*&---------------------------------------------------------------------*
*&  프로그램명 : Local L/C 미입고현?
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.06.22                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Local L/C에 대한 미입고 현황을 조회한다.              *
*&                                                                     *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLLCNI    MESSAGE-ID ZIM
                     LINE-SIZE 101
                     NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFBENI          LIKE   ZTREQHD-ZFBENI,     " Benificiary(Vendor)
       ZFBENI_NM(20)   TYPE   C,                  " Vendor Name
       ZFOPNNO(19)     TYPE   C,                  " L/C No
       EBELN           LIKE   ZTREQHD-EBELN,      " P/O No
       ZFREQNO         LIKE   ZTREQHD-ZFREQNO,    " 수입의뢰 관리번?
       ZFCLOSE         LIKE   ZTREQHD-ZFCLOSE,    " L/C Close 여?
       ZFLASTAM        LIKE   ZTREQHD-ZFLASTAM,   " 개설금?
       WAERS           LIKE   ZTREQHD-WAERS,
       ZFOPKAM         LIKE   ZTLLCHD-ZFOPKAM,    " 개설금액-원?
       ZFIVAMT         LIKE   ZTVTIVIT-ZFIVAMT,   " 입고금?
       ZFKAMT          LIKE   ZTVTIVIT-ZFKAMT,    " 입고금액-원?
       ZFIVAMT_D       LIKE   ZTVTIVIT-ZFIVAMT,   " 미입고금?
       ZFKAMT_D        LIKE   ZTVTIVIT-ZFKAMT.    " 미입고금액-원?
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMLLCNITOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BENI    FOR ZTREQHD-ZFBENI,    " Vendor
                   S_OPNNO   FOR ZTREQHD-ZFOPNNO,   " L/C No
                   S_EBELN   FOR ZTREQHD-EBELN.     " P/O No
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(10) TEXT-021, POSITION 33.
        PARAMETERS : P_ZFDODT LIKE ZTVTIV-ZFDODT.      " Document Date
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-031, POSITION 1.
     SELECTION-SCREEN : COMMENT 33(3) TEXT-032, POSITION 37.
     PARAMETERS : P_Y    AS CHECKBOX.              " Yes
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

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
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
            W_FIELD_NM = 'ZFREQDT'.
            ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
            PERFORM HANDLE_SORT TABLES  IT_TAB
                                USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DSRQ'.                   " 수입의뢰 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  P_Y = 'X'.
  SELECT MAX( ZFDODT ) INTO P_ZFDODT
    FROM ZTVTIV.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /37  '[ Local L/C 미입고현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM,  83 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'Vendor                        ' NO-GAP, SY-VLINE NO-GAP,
            '통화 '                          NO-GAP, SY-VLINE NO-GAP,
            '          개설금액'             NO-GAP, SY-VLINE NO-GAP,
            '           입고금액'            NO-GAP, SY-VLINE NO-GAP,
            '         미입고금액'            NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            'P/O No    '                     NO-GAP, SY-VLINE NO-GAP,
            'L/C No             '            NO-GAP, SY-VLINE NO-GAP,
            '     '                          NO-GAP, SY-VLINE NO-GAP,
            '                  '             NO-GAP, SY-VLINE NO-GAP,
            '                   '            NO-GAP, SY-VLINE NO-GAP,
            '                   '            NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.

   SET PF-STATUS 'ZIMX2'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMX2'.           " GUI TITLE SETTING..

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

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI             NO-GAP, " Vendor
       IT_TAB-ZFBENI_NM          NO-GAP, " Vendor ?
       SY-VLINE NO-GAP,
       IT_TAB-WAERS              NO-GAP, " 통?
       SY-VLINE NO-GAP,
       IT_TAB-ZFLASTAM CURRENCY  IT_TAB-WAERS  NO-GAP, " 개설금?
       SY-VLINE NO-GAP,
       IT_TAB-ZFIVAMT CURRENCY   IT_TAB-WAERS  NO-GAP, " 입고금?
       SY-VLINE NO-GAP,
       IT_TAB-ZFIVAMT_D CURRENCY IT_TAB-WAERS  NO-GAP, " 미입고금?
       SY-VLINE NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-EBELN                     NO-GAP, " P/O No
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPNNO                   NO-GAP, " L/C No
       SY-VLINE NO-GAP,
       '     '                          NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFOPKAM CURRENCY 'KRW'    NO-GAP, " 개설금액-원?
       SY-VLINE NO-GAP,
       IT_TAB-ZFKAMT CURRENCY  'KRW'    NO-GAP, " 입고금액-원?
       SY-VLINE NO-GAP,
       IT_TAB-ZFKAMT_D CURRENCY 'KRW'   NO-GAP, " 미입고금액-원?
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
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.
  SELECT *
    FROM ZTREQHD
   WHERE ZFBENI  IN S_BENI
     AND ZFOPNNO IN S_OPNNO
     AND EBELN   IN S_EBELN
     AND ZFREQTY = 'LO'.
         IF P_Y = 'X'.
            IF ZTREQHD-ZFCLOSE = 'X'.
               CONTINUE.
            ENDIF.
         ENDIF.
         CLEAR    IT_TAB.
         MOVE ZTREQHD-ZFBENI  TO IT_TAB-ZFBENI.
         MOVE ZTREQHD-ZFOPNNO TO IT_TAB-ZFOPNNO.
         MOVE ZTREQHD-EBELN   TO IT_TAB-EBELN.
         MOVE ZTREQHD-ZFREQNO TO IT_TAB-ZFREQNO.
         MOVE ZTREQHD-ZFCLOSE TO IT_TAB-ZFCLOSE.
         MOVE ZTREQHD-WAERS TO IT_TAB-WAERS.
         SELECT SINGLE NAME1
           INTO IT_TAB-ZFBENI_NM
           FROM LFA1
          WHERE LIFNR = IT_TAB-ZFBENI.
         SELECT MAX( ZFAMDNO )
           INTO W_ZFAMDNO
           FROM ZTREQST
          WHERE ZFREQNO = IT_TAB-ZFREQNO
          AND ZFDOCST = 'O'.
         IF W_ZFAMDNO = 0.
            CLEAR ZTLLCHD.
            SELECT SINGLE *
              FROM ZTLLCHD
             WHERE ZFREQNO = IT_TAB-ZFREQNO.
            MOVE ZTLLCHD-ZFOPAMT TO IT_TAB-ZFLASTAM.
            MOVE ZTLLCHD-ZFOPKAM TO IT_TAB-ZFOPKAM.
         ELSE.
            CLEAR ZTLLCAMHD.
            SELECT SINGLE *
              FROM ZTLLCAMHD
             WHERE ZFREQNO = IT_TAB-ZFREQNO
               AND ZFAMDNO = W_ZFAMDNO.
            MOVE ZTLLCAMHD-ZFNOAMT  TO IT_TAB-ZFLASTAM.
            MOVE ZTLLCAMHD-ZFNOPKAM TO IT_TAB-ZFOPKAM.
         ENDIF.
         SELECT SUM( ZFIVAMT )  SUM( ZFKAMT )
           INTO (IT_TAB-ZFIVAMT,  IT_TAB-ZFKAMT)
           FROM  ZVVTIV_IT
          WHERE  EBELN = IT_TAB-EBELN
            AND  ZFDODT <=  P_ZFDODT.
         IT_TAB-ZFIVAMT_D = IT_TAB-ZFLASTAM - IT_TAB-ZFIVAMT.
         IT_TAB-ZFKAMT_D  = IT_TAB-ZFOPKAM  - IT_TAB-ZFKAMT.
         APPEND  IT_TAB.
  ENDSELECT.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     MESSAGE S738.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX    TYPE P,
        ZFREQNO  LIKE ZTREQHD-ZFREQNO.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX     TO INDEX,
         IT_TAB-ZFREQNO   TO ZFREQNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFREQNO   TO IT_SELECTED-ZFREQNO.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC
