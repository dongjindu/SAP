*&---------------------------------------------------------------------*
*& Report  ZRIMREDC                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 인수증 편성취소/EDI Flat File 형성                    *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2000.03.07                                            *
*&  적용회사PJT:                                                       *
*&---------------------------------------------------------------------*
*&   DESC.     : 인수증을 검색하여 편성을 취소하거나 EDI Data를 형성.  *
*&                                                                     *
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMREDC    MESSAGE-ID ZIM
                    LINE-SIZE 122
                    NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       LIFNR           LIKE   ZTVTIV-LIFNR,        " Vendor
       LIFNR_NM(21)    TYPE   C,                   " Vendor Name
       EBELN           LIKE   ZTRED-EBELN,         " P/O No
       ZFVTNO          LIKE   ZTRED-ZFVTNO,        " 세금계산서 관리번?
       ZFISUDT         LIKE   ZTRED-ZFISUDT,       " 발급?
       ZFDOCNO         LIKE   ZTRED-ZFDOCNO,       " 전자문서번?
       ZFISNO          LIKE   ZTRED-ZFISNO,        " 인수증 발급번?
       ZFREDNO         LIKE   ZTRED-ZFREDNO,       " 인수증 관리번?
       ZFREAMF         LIKE   ZTRED-ZFREAMF,       " 인수금액 외?
       ZFREAMFC        LIKE   ZTRED-ZFREAMFC,      " 인수금액 외화 통?
       ZFREAMK         LIKE   ZTRED-ZFREAMK,       " 인수금액 원?
       ZFEDIST         LIKE   ZTVT-ZFEDIST,        " EDI Status
       ZFEDICK         LIKE   ZTVT-ZFEDICK,        " EDI Check
       EDIST           LIKE   DD07T-DDTEXT,        " EDI Status
       EDICK           LIKE  DD07T-DDTEXT.         " EDI Check

DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMREDCTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTRED-BUKRS,
                   S_LIFNR   FOR ZTVTIV-LIFNR,     " Vendor
                   S_EBELN   FOR ZTRED-EBELN,      " P/O No
                   S_ISUDT   FOR ZTRED-ZFISUDT,    " 발급?
                   S_DOCNO   FOR ZTRED-ZFDOCNO,    " 전자문서번?
                   S_ISNO    FOR ZTRED-ZFISNO,     " 인수증 발급번?
                   S_REDNO   FOR ZTRED-ZFREDNO.    " 인수증 관리번?

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
      WHEN 'DISPR'.                " 인수증 조?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_ZTRED USING IT_SELECTED-ZFREDNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'DISCR'.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_PRINT_ZTRED USING IT_SELECTED-ZFREDNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.

      WHEN 'CAVR'.                   " 편성취?
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE S965.
               EXIT.
            ENDIF.
            PERFORM P3000_ZTRED_DELETE USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
            PERFORM RESET_LIST.
            MESSAGE S836 WITH W_PROC_CNT.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.
   SET  TITLEBAR 'ZIMA8'.           " GUI TITLE SETTING.

   SELECT SINGLE * FROM ZTIMIMG00.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ 인수증 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM,  100 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'Vendor    '                   NO-GAP, SY-VLINE NO-GAP,
            '발급일    '                   NO-GAP, SY-VLINE NO-GAP,
            '전자문서번호     '            NO-GAP, SY-VLINE NO-GAP,
            '인수증 발급번호 '             NO-GAP,
            '                            ' NO-GAP, SY-VLINE NO-GAP,
            'P/O No          '             NO-GAP, SY-VLINE NO-GAP,
        (14)'EDI Status'                   NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
            '                     '        NO-GAP, SY-VLINE NO-GAP,
            '관리번호         '            NO-GAP, SY-VLINE NO-GAP,
            '      인수금액-원화'          NO-GAP, SY-VLINE NO-GAP,
            '           '                  NO-GAP,
            '인수금액-외화'                NO-GAP, SY-VLINE NO-GAP,
            '세금계산서번호  '             NO-GAP, SY-VLINE NO-GAP,
       (14) 'EDI Check '                   NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.

   SET PF-STATUS 'ZIMA8'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMA8'.           " GUI TITLE SETTING..

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
       IT_TAB-LIFNR           NO-GAP, " Vendor
       SY-VLINE NO-GAP,
       IT_TAB-ZFISUDT         NO-GAP, " 발급?
       SY-VLINE NO-GAP,
       IT_TAB-ZFDOCNO         NO-GAP, " 전자문서번?
       SY-VLINE NO-GAP,
       IT_TAB-ZFISNO          NO-GAP, " 인수증 발급번?
       '         '            NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-EBELN           NO-GAP, " P/O No
       '      '               NO-GAP,
       SY-VLINE NO-GAP,
       (14)IT_TAB-EDIST       NO-GAP, " EDI Status

       SY-VLINE NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-LIFNR_NM        NO-GAP, " Vendor Name
       SY-VLINE NO-GAP,
       IT_TAB-ZFREDNO         NO-GAP, " 인수증 관리번?
       '       '              NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREAMK CURRENCY 'KRW'  NO-GAP, " 인수금액-원?
       SY-VLINE NO-GAP,
       IT_TAB-ZFREAMFC        NO-GAP, " 인수금액-외화 통?
       IT_TAB-ZFREAMF CURRENCY IT_TAB-ZFREAMFC  NO-GAP, " 인수금액-원?
       SY-VLINE NO-GAP,
       IT_TAB-ZFVTNO          NO-GAP, " 세금계산서 관리번?
       '      '               NO-GAP,
       SY-VLINE NO-GAP,
       (14)IT_TAB-EDICK       NO-GAP, " EDI Check
       SY-VLINE NO-GAP.

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
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.
  SELECT *
    FROM ZTRED
   WHERE BUKRS    IN S_BUKRS
     AND LIFNR    IN S_LIFNR
     AND EBELN    IN S_EBELN
     AND ZFISUDT  IN S_ISUDT
     AND ZFDOCNO  IN S_DOCNO
     AND ZFISNO   IN S_ISNO
     AND ZFREDNO  IN S_REDNO.
         CLEAR    IT_TAB.
         MOVE-CORRESPONDING ZTRED TO IT_TAB.
         SELECT SINGLE NAME1 INTO IT_TAB-LIFNR_NM
           FROM LFA1
          WHERE LIFNR = IT_TAB-LIFNR.

         PERFORM GET_DD07T_SELECT(SAPMZIM00) USING 'ZDEDIST'
                                   ZTRED-ZFEDIST
                                   CHANGING   IT_TAB-EDIST.
         PERFORM GET_DD07T_SELECT(SAPMZIM00) USING 'ZDOX' ZTRED-ZFEDICK
                                   CHANGING   IT_TAB-EDICK.

         APPEND  IT_TAB.

  ENDSELECT.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     MESSAGE S738.
     W_ERR_CHK = 'Y'.
     EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX    TYPE P,
        ZFREDNO  LIKE ZTRED-ZFREDNO.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX     TO INDEX,
         IT_TAB-ZFREDNO   TO ZFREDNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFREDNO   TO IT_SELECTED-ZFREDNO.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTRED_DELETE
*&---------------------------------------------------------------------*
FORM P3000_ZTRED_DELETE USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  CLEAR W_PROC_CNT.

  PERFORM P2000_DELETE_MESSAGE.

  IF ANTWORT NE 'Y'.
     EXIT.
  ENDIF.

  LOOP AT IT_SELECTED.

     SELECT SINGLE *
            FROM ZTRED
            WHERE ZFREDNO = IT_SELECTED-ZFREDNO.      " 인수?
     IF ZTRED-ZFEDIST NE 'N'.
        CONTINUE.
     ENDIF.

*> IMG 체크 ( 2001/10/08 KSB INSERT )
     IF ZTIMIMG00-ZFTAXYN IS INITIAL.
        DELETE FROM ZTVT
               WHERE ZFVTNO IN
                   ( SELECT ZFVTNO FROM ZTVTIV
                            WHERE ZFREDNO = IT_SELECTED-ZFREDNO ).

        DELETE FROM ZTVTSG1
               WHERE ZFVTNO IN
                   ( SELECT ZFVTNO FROM ZTVTIV
                            WHERE ZFREDNO = IT_SELECTED-ZFREDNO ).

        DELETE FROM ZTVTSG3
               WHERE ZFVTNO IN
                   ( SELECT ZFVTNO FROM ZTVTIV
                            WHERE ZFREDNO = IT_SELECTED-ZFREDNO ).

     ENDIF.

     DELETE FROM ZTRED
            WHERE ZFREDNO = IT_SELECTED-ZFREDNO.  " 인수증.

     DELETE FROM ZTREDSG1
            WHERE ZFREDNO = IT_SELECTED-ZFREDNO.  " 인수증 Seg1

     IF ZTIMIMG00-ZFTAXYN IS INITIAL.

        UPDATE ZTVTIV SET   ZFVTNO  = SPACE
                            ZFREDNO = SPACE
                            UNAM    = SY-UNAME
                            UDAT    = SY-DATUM
                      WHERE ZFREDNO = IT_SELECTED-ZFREDNO.
     ELSE.
        UPDATE ZTVTIV SET   ZFREDNO = SPACE
                            UNAM    = SY-UNAME
                            UDAT    = SY-DATUM
                      WHERE ZFREDNO = IT_SELECTED-ZFREDNO.
     ENDIF.

     IF SY-SUBRC NE 0.
        MESSAGE E856.
        MOVE 'N' TO W_ERR_CHK.
        EXIT.
     ENDIF.

     ADD 1 TO W_PROC_CNT.

  ENDLOOP.

ENDFORM.                    " P3000_ZTRED_DELETE

*&---------------------------------------------------------------------*
*&      Form  P2000_DELETE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_DELETE_MESSAGE.

   PERFORM P2000_MESSAGE_BOX USING '삭제 확인'             " 타이틀...
                           '현재 Document를 삭제합니다.'
                           '삭제하시겠습니까?' " MSG2
                           'N'                 " 취소 버튼 유/?
                           '1'.                " default button

ENDFORM.                    " P2000_DELETE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
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

   CALL SCREEN 0001 STARTING AT 30 6
                    ENDING   AT 78 10.

   IF ANTWORT = 'C'.       " Cancel
       SET SCREEN SY-DYNNR.
   ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX

*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

   IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
   ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
   ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

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

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_ZTRED
*&---------------------------------------------------------------------*
FORM P2000_SHOW_ZTRED USING    P_ZFREDNO.
   SET PARAMETER ID 'BES'      FIELD ''.
   SET PARAMETER ID 'ZPREDNO'  FIELD P_ZFREDNO.
   EXPORT 'ZPREDNO'  TO MEMORY ID 'ZPREDNO'.
   CALL TRANSACTION 'ZIMA7' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_ZTRED
*&---------------------------------------------------------------------*
*&      Form  P2000_PRINT_ZTRED
*&---------------------------------------------------------------------*
FORM P2000_PRINT_ZTRED USING    P_ZFREDNO.

     SUBMIT ZRIMLOCRCT WITH P_REDNO EQ P_ZFREDNO
                         AND RETURN. " 다시 돌아 올수 있게.

ENDFORM.                    " P2000_PRINT_ZTRED
