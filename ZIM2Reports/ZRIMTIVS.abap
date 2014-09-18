*&---------------------------------------------------------------------
*& Report  ZRIMTIVS
*&---------------------------------------------------------------------
*&  프로그램명 : 세금계산서/인수증용 Invoice 검색 및 처리             *
*&      작성자 : 김연중 INFOLINK Ltd.
*&      작성일 : 2000.03.07
*&  적용회사PJT:
*&---------------------------------------------------------------------
*&   DESC.     : 세금계산서용 Invoice를 검색하여.
*&               Invoice를 조회하거나, 세금계산서/인수증을 편성한다.
*&---------------------------------------------------------------------
*& [변경내용]
*&    1. 2001.10.08 강석봉.
*&       - 수입 IMG TABLE을 읽어, 세금계산서 편성 및 인수증 편성 작업
*&         을 구분하여 처리한다.
*&---------------------------------------------------------------------
REPORT  ZRIMTIVS    MESSAGE-ID ZIM
                    LINE-SIZE 119
                    NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFPOSDT         LIKE   ZTVTIV-ZFPOSDT,      " Posting Date
       ZFDODT          LIKE   ZTVTIV-ZFDODT,       " Document Date
       LIFNR           LIKE   ZTVTIV-LIFNR,        " Vendor
       LIFNR_NM(20)    TYPE   C,                   " Vendor Name
       EBELN           LIKE   ZTVTIVIT-EBELN,      " P/O No
       EBELP           LIKE   ZTVTIVIT-EBELP,      " P/O 품목 No.
       ZFOPNNO         LIKE   ZTREQHD-ZFOPNNO,     " L/C No
       ZFDOCST         LIKE   ZTREQST-ZFDOCST,     " 문서상태.
       ZFREQTY         LIKE   ZTVTIVIT-ZFREQTY,    " L/C Type
       ZFGFDYR         LIKE   ZTVTIV-ZFGFDYR,      " 물대전표 연?
       ZFGFDNO         LIKE   ZTVTIV-ZFGFDNO,      " 물대전표 번?
       BUKRS           LIKE   ZTVTIV-BUKRS,        " 회사코드.
       ZFVTNO          LIKE   ZTVTIV-ZFVTNO,       " 세금계산서관리번?
       ZFREDNO         LIKE   ZTVTIV-ZFREDNO,      " 인수증 관리번?
       ZFIVAMT         LIKE   ZTVTIVIT-ZFIVAMT,    " Invoice 금?
       ZFIVAMC         LIKE   ZTVTIVIT-ZFIVAMC.    " Invoice 금액 통?
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Menu Statsu Function을 Inactive하기 위한 Internal Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_EXCL OCCURS 20,
      FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.

DATA : W_OK_CODE  LIKE SY-UCOMM,
       W_ERR_CNT         TYPE I.             " 전체 COUNT
*----------------------------------------------------------------------
* Tables 및 변수 Define
*----------------------------------------------------------------------
INCLUDE   ZRIMTIVSTOP.

INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

INCLUDE   <ICON>.

DATA : G_ZFGFDYR         LIKE   ZTVTIV-ZFGFDYR,      " 물대전표 연?
       G_ZFGFDNO         LIKE   ZTVTIV-ZFGFDNO,      " 물대전표 번?
       G_BUKRS           LIKE   ZTVTIV-BUKRS.        " 회사코드.

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS  FOR ZTVTIV-BUKRS,
                   S_POSDT  FOR ZTVTIV-ZFPOSDT,    " Posting Date
                   S_DODT   FOR ZTVTIV-ZFDODT,     " Document Date
                   S_LIFNR  FOR ZTVTIV-LIFNR,      " Vendor
                   S_EBELN  FOR ZTVTIVIT-EBELN,    " P/O No
                   S_OPNNO  FOR ZTREQHD-ZFOPNNO.   " L/C No
*                   S_REQTY  FOR ZTVTIVIT-ZFREQTY.  " L/C Type

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

* 후속작업 진행상?
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-002, POSITION 1.
     SELECTION-SCREEN : COMMENT 32(4) TEXT-021, POSITION 37.
     PARAMETERS : P_N      RADIOBUTTON GROUP RDG.     " None
     SELECTION-SCREEN : COMMENT 40(10) TEXT-022, POSITION 51.
     PARAMETERS : P_V      RADIOBUTTON GROUP RDG.     " 세금계산?
     SELECTION-SCREEN : COMMENT 54(6) TEXT-023, POSITION 61.
     PARAMETERS : P_R      RADIOBUTTON GROUP RDG.     " 인수?
     SELECTION-SCREEN : COMMENT 64(3) TEXT-024, POSITION 68.
     PARAMETERS : P_A      RADIOBUTTON GROUP RDG.     " ALL
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU' AND INCLUDE  NE 'POER'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.

* 파라메타 설?
   PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TEXT TABLE SELECT
*  PERFORM   P1000_READ_TEXT        USING   W_ERR_CHK.
  PERFORM   P1000_READ_DATA        USING   W_ERR_CHK.
  IF W_COUNT = 0. MESSAGE S738. EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.
   W_OK_CODE = SY-UCOMM.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
            W_FIELD_NM = 'ZFREQDT'.
            ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
            PERFORM HANDLE_SORT TABLES  IT_TAB
                                USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP'.                    " 세금계산서용 Invoice 조회.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_ZTVTIV
                       USING IT_SELECTED-ZFGFDYR
                             IT_SELECTED-ZFGFDNO
                             IT_SELECTED-BUKRS.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'DISPT'.                   " 세금계산서 조회.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_ZTVT
                       USING IT_SELECTED-ZFGFDYR IT_SELECTED-ZFGFDNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'DISPR'.                   " 인수증 조회.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_ZTRED
                       USING IT_SELECTED-ZFGFDYR
                             IT_SELECTED-ZFGFDNO
                             IT_SELECTED-BUKRS.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'CRVT'.                    " 세금계산서 편성.
*            PERFORM P2000_MULTI_SELECTION.
*            IF W_SELECTED_LINES EQ 0.
*               MESSAGE S766.
*               EXIT.
*            ENDIF.
            DESCRIBE  TABLE IT_ZTVT LINES W_LINE.
            IF W_LINE LE  0.
               MESSAGE  S766.
               EXIT.
            ENDIF.
*>>
            SET UPDATE TASK LOCAL.

            PERFORM P3000_CREATE_ZTVT USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.
               ROLLBACK WORK.
*               PERFORM  P3000_DB_ROLLBACK.
               EXIT.
            ELSE.
               IF ZTIMIMG00-ZFTAXYN IS INITIAL AND
                  W_OK_CODE EQ 'CRRD'.
                  PERFORM P3000_CREATE_ZTRED USING W_ERR_CHK.
                  IF W_ERR_CHK EQ 'Y'.
                     ROLLBACK WORK.
                     EXIT.
                  ELSE.
                     COMMIT WORK.
                  ENDIF.
               ELSE.
                  COMMIT WORK.
               ENDIF.
            ENDIF.
*            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
*           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*           PERFORM RESET_LIST.
*           IF W_PROC_CNT > 0.
*              W_PROC_CNT = W_PROC_CNT - 1.
*           ENDIF.
            W_PROC_CNT = W_PROC_CNT - 1.
            MESSAGE S829 WITH W_PROC_CNT.
            SET SCREEN 0. LEAVE SCREEN.
*      WHEN 'CRRD'.                    " 인수증 편?
*            PERFORM P2000_MULTI_SELECTION.
*            IF W_SELECTED_LINES EQ 0.
*               MESSAGE S766.
*               EXIT.
*            ENDIF.
*
*            SET UPDATE TASK LOCAL.
*
**> IMG 체크 ( 2001/10/08 KSB INSERT )
*            IF ZTIMIMG00-ZFTAXYN IS INITIAL.
*               PERFORM P3000_CREATE_ZTVT USING W_ERR_CHK.
*               IF W_ERR_CHK EQ 'Y'.
*                  ROLLBACK WORK.
*                  EXIT.
*               ENDIF.
*            ENDIF..
*
*            PERFORM P3000_CREATE_ZTRED USING W_ERR_CHK.
*            IF W_ERR_CHK EQ 'Y'.
*               ROLLBACK WORK.
*               EXIT.
*            ELSE.
*               COMMIT WORK.
*            ENDIF.
*
*            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
*            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*            PERFORM RESET_LIST.
*            MESSAGE S850 WITH W_PROC_CNT.
*            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_COUNT = 0.
               MESSAGE S738.
               SET SCREEN 0. LEAVE SCREEN.
            ENDIF.
            PERFORM RESET_LIST.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN 'CKEK' OR 'CRRD'.                  ">세금계산서/인수증 편성.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.

            PERFORM P2000_IT_WRITE.
            IF W_COUNT LE 0. MESSAGE S619. EXIT. ENDIF.
*> 세금계산서 절차 미사용시.
            IF ZTIMIMG00-ZFTAXYN IS INITIAL AND
               W_OK_CODE EQ 'CRRD'.

               DESCRIBE  TABLE IT_ZTVT LINES W_LINE.
               IF W_LINE LE  0.
                  MESSAGE  S766.
                  EXIT.
               ENDIF.

*>> BEGIN TRANSACTION...
               SET UPDATE TASK LOCAL.
*>> 세금계산서 생성.
               PERFORM P3000_CREATE_ZTVT USING W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.
                  ROLLBACK WORK.
                  EXIT.
               ENDIF.

*>> 인수증용 기본 데이타 SELECT.
               PERFORM P2000_WRITE_ZTVTIT.

               DESCRIBE  TABLE IT_ZTRED LINES W_LINE.
               IF W_LINE LE  0.
                  MESSAGE  S620.
                  PERFORM  P2000_SINGLE_MESS_MAKE.
               ENDIF.

*>> ERROR 사항 CHECK하여서 ERROR LIST DISPLAY.
               DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
               IF W_LINE GT 0.
                  INCLUDE = 'POER'.
                  CALL SCREEN 0200 STARTING AT  05   3
                                   ENDING   AT  100 12.
                  CLEAR : INCLUDE.
                  ROLLBACK WORK.
                  EXIT.
               ENDIF.

*>> SIMULATION 화면 CALL.
               W_PROC_CNT = 0.
               PERFORM P3000_CREATE_LOCCRT USING W_ERR_CHK.
*               MOVE  'POPU'   TO  INCLUDE.
*               CALL  SCREEN   0100  STARTING  AT 002  03
*                                    ENDING    AT 122  12.
*               CLEAR  INCLUDE.
*               W_PROC_CNT = W_PROC_CNT - 1.
               IF W_ERR_CHK EQ 'Y'.
                  MESSAGE I849.
                  ROLLBACK WORK.
                  EXIT.
               ELSE.
                  COMMIT WORK.
               ENDIF.

               MESSAGE S850 WITH W_PROC_CNT.
               SET SCREEN 0. LEAVE SCREEN.
            ELSE.
*>> SIMULATION 화면 CALL.
               MOVE  'POPU'   TO  INCLUDE.
               CALL  SCREEN   0100  STARTING  AT 005  03
                                    ENDING    AT 125  12.
               CLEAR  INCLUDE.
            ENDIF.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
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

      WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  P_N = 'X'.

*>> 수입 IMG 사항 GET
  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
     MESSAGE S961.
     LEAVE PROGRAM.
  ENDIF.

*> IMG 체크 ( 2001/10/08 KSB INSERT )
  IF ZTIMIMG00-ZFTAXYN IS INITIAL.
     SET  TITLEBAR 'ZIMA1' WITH '인수증'.
  ELSE.
     SET  TITLEBAR 'ZIMA1' WITH '세금계산서'.
  ENDIF.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   IF ZTIMIMG00-ZFTAXYN IS INITIAL.
      WRITE : /40  '[ 인수증용 Invoice Processing ]'
               COLOR COL_HEADING INTENSIFIED OFF.
   ELSE.
      WRITE : /40  '[ 세금계산서용 Invoice Processing ]'
               COLOR COL_HEADING INTENSIFIED OFF.
   ENDIF.

  WRITE : / 'Date : ', SY-DATUM,  94 'Page : ', W_PAGE.
  WRITE : / SY-ULINE(113).
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
      (10)  '전기일',
            SY-VLINE,
      (10)  '증빙일',
            SY-VLINE,
      (04)  '회사',
            SY-VLINE,
      (15)  '회계전표번호',
            SY-VLINE NO-GAP,
      (30)  '공급업체'       NO-GAP,
            SY-VLINE,
      (10)  '세금계산서',
            SY-VLINE,
      (10)  '인수증' ,
            SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',
            SY-VLINE,
      (10)  '',
            SY-VLINE,
      (10)  '',
            SY-VLINE,
      (04)  '',
            SY-VLINE,
      (15)  'Local L/C No.',
            SY-VLINE NO-GAP,
      (30)  '송장 금액'       NO-GAP,
            SY-VLINE,
      (10)  '구매오더',
            SY-VLINE,
      (10)  '품목번호' ,
            SY-VLINE.
  WRITE : / SY-ULINE(113).

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE USING    P_W_ERR_CHK.

   REFRESH : IT_EXCL.              " Inactive Function용 Internal Table
   IF ZTIMIMG00-ZFTAXYN IS INITIAL.
      MOVE 'CKEK' TO IT_EXCL-FCODE.  APPEND IT_EXCL." 세금계산서.
   ELSE.
      MOVE 'CRRD' TO IT_EXCL-FCODE.  APPEND IT_EXCL." 인수증.
   ENDIF.

   SET PF-STATUS 'ZIMA1' EXCLUDING IT_EXCL.


*> IMG 체크 ( 2001/10/08 KSB INSERT )
   IF ZTIMIMG00-ZFTAXYN IS INITIAL.
      SET  TITLEBAR 'ZIMA1' WITH '인수증'.
   ELSE.
      SET  TITLEBAR 'ZIMA1' WITH '세금계산서'.
   ENDIF.

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   SORT IT_TAB BY BUKRS ZFGFDYR ZFGFDNO.

   CLEAR: G_ZFGFDYR, G_ZFGFDNO, G_BUKRS.

   LOOP AT IT_TAB.
*      W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
      IF IT_TAB-ZFGFDYR NE G_ZFGFDYR OR
         IT_TAB-ZFGFDNO NE G_ZFGFDNO OR
         IT_TAB-BUKRS   NE G_BUKRS.
         IF NOT G_BUKRS IS INITIAL.
            WRITE : / SY-ULINE(113).
         ENDIF.
         PERFORM P3000_HEADER_WRITE.
         ADD 1 TO W_COUNT.
      ENDIF.

      PERFORM P3000_LINE_WRITE.

      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
      MOVE : IT_TAB-ZFGFDYR    TO    G_ZFGFDYR,
             IT_TAB-ZFGFDNO    TO    G_ZFGFDNO,
             IT_TAB-BUKRS      TO    G_BUKRS.
   ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------
FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE(113).
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
FORM P3000_LINE_WRITE.

  FORMAT RESET.
*  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, '',
          SY-VLINE,
   (10)   '',
          SY-VLINE,
   (10)   '',
          SY-VLINE,
   (04)   '',
          SY-VLINE NO-GAP,
   (17)   IT_TAB-ZFOPNNO NO-GAP,
          SY-VLINE,
   (05)   IT_TAB-ZFIVAMC, " Invoice 통?
   (22)   IT_TAB-ZFIVAMT CURRENCY IT_TAB-ZFIVAMC, " 송장금액.
          SY-VLINE,
   (10)   IT_TAB-EBELN,
          SY-VLINE,
   (10)   IT_TAB-EBELP,
          SY-VLINE.
* Hide
*       MOVE SY-TABIX  TO W_LIST_INDEX.
*       HIDE: W_LIST_INDEX, IT_TAB.
*       MODIFY IT_TAB INDEX SY-TABIX.

*       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
*       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
*       IT_TAB-ZFDODT     NO-GAP, " Document Date
*       '   '             NO-GAP,
*       SY-VLINE NO-GAP,
*       '               ' NO-GAP,
*       SY-VLINE NO-GAP,
*       IT_TAB-ZFVTNO     NO-GAP, " 세금계산서 관리번?
*       SY-VLINE NO-GAP,
*       IT_TAB-ZFREDNO    NO-GAP, " 인수증 관리번?
*       '         '       NO-GAP,
*       SY-VLINE NO-GAP,
*       IT_TAB-ZFREQTY    NO-GAP, " L/C Type
*       '        '        NO-GAP,
*       SY-VLINE NO-GAP,
*       IT_TAB-ZFIVAMC    NO-GAP, " Invoice 통?
*       '           '     NO-GAP,
*       IT_TAB-ZFIVAMT CURRENCY IT_TAB-ZFIVAMC NO-GAP, " Invoice 금?
*       SY-VLINE NO-GAP.
*
*  MOVE SY-TABIX  TO W_LIST_INDEX.
*  HIDE: W_LIST_INDEX, IT_TAB.
*  MODIFY IT_TAB INDEX SY-TABIX.
*  W_COUNT = W_COUNT + 1.

*  WRITE : / SY-ULINE(113).

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / SY-ULINE(113).
     WRITE : / '총', W_COUNT, '건'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.

  SELECT  A~ZFGFDYR         A~ZFGFDNO       A~BUKRS       B~EBELN
          MAX( A~ZFPOSDT ) AS ZFPOSDT  MAX( A~ZFDODT )  AS ZFDODT
          MAX( A~LIFNR )   AS LIFNR    MAX( A~ZFVTNO )  AS ZFVTNO
          MAX( A~ZFREDNO ) AS ZFREDNO  MAX( B~ZFREQTY ) AS ZFREQTY
          SUM( B~ZFIVAMT ) AS ZFIVAMT  MAX( B~ZFIVAMC ) AS ZFIVAMC
  INTO    CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM    ZTVTIV  AS  A  INNER  JOIN  ZTVTIVIT  AS  B
  ON      A~ZFGFDYR   EQ  B~ZFGFDYR
  AND     A~ZFGFDNO   EQ  B~ZFGFDNO
  AND     A~BUKRS     EQ  B~BUKRS
  WHERE   A~ZFPOSDT   IN  S_POSDT
  AND     A~ZFDODT    IN  S_DODT
  AND     A~LIFNR     IN  S_LIFNR
  AND     B~EBELN     IN  S_EBELN
*  AND     B~ZFREQTY   IN  S_REQTY
  GROUP BY
          A~ZFGFDYR
          A~ZFGFDNO
          A~BUKRS
          B~EBELN.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    CLEAR : W_ZFOPNNO.
    SELECT SINGLE ZFOPNNO INTO W_ZFOPNNO
           FROM  ZTREQST
           WHERE ZFAMDNO EQ '00000'
           AND   ZFREQNO EQ ( SELECT MAX( ZFREQNO )
                                     FROM   ZTREQHD
                                     WHERE  EBELN = IT_TAB-EBELN
                                     AND    ZFOPNNO IN S_OPNNO ).

*    IF W_ZFOPNNO IS INITIAL.
*       CONTINUE.
*    ENDIF.

    MOVE W_ZFOPNNO TO IT_TAB-ZFOPNNO.
*>> NONE.
    IF P_N = 'X'.
       IF NOT ( IT_TAB-ZFVTNO IS INITIAL AND
                IT_TAB-ZFREDNO IS INITIAL ).
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

*> 세금계산서 편성.
    IF P_V = 'X'.
       IF NOT ( NOT IT_TAB-ZFVTNO  IS INITIAL AND
                    IT_TAB-ZFREDNO IS INITIAL ).
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

*> 인수증 편성.
    IF P_R = 'X'.
       IF IT_TAB-ZFREDNO IS INITIAL.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

    SELECT SINGLE NAME1
    INTO   IT_TAB-LIFNR_NM
    FROM   LFA1
    WHERE  LIFNR = IT_TAB-LIFNR.

    MODIFY  IT_TAB.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      READ TABLE IT_SELECTED WITH KEY BUKRS   = IT_TAB-BUKRS
                                      ZFGFDYR = IT_TAB-ZFGFDYR
                                      ZFGFDNO = IT_TAB-ZFGFDNO.
      IF SY-SUBRC NE 0.
         MOVE : IT_TAB-ZFGFDYR  TO IT_SELECTED-ZFGFDYR,
                IT_TAB-ZFGFDNO  TO IT_SELECTED-ZFGFDNO,
                IT_TAB-BUKRS    TO IT_SELECTED-BUKRS.
         APPEND IT_SELECTED.
         ADD 1 TO W_SELECTED_LINES.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------
*&      Form  P2000_SHOW_ZTIV
*&---------------------------------------------------------------------
FORM P2000_SHOW_ZTVTIV USING    P_ZFGFDYR
                                P_ZFGFDNO
                                P_BUKRS.

   SELECT SINGLE * FROM ZTVTIV
          WHERE ZFGFDYR EQ P_ZFGFDYR
          AND   ZFGFDNO EQ P_ZFGFDNO
          AND   BUKRS   EQ P_BUKRS.

   IF SY-SUBRC NE 0.
      MESSAGE E506 WITH P_ZFGFDYR P_ZFGFDNO '세금계산서용 Invoice'.
   ENDIF.

   SET PARAMETER ID 'ZPGFDYR'  FIELD P_ZFGFDYR.
   SET PARAMETER ID 'ZPGFDNO'  FIELD P_ZFGFDNO.
   SET PARAMETER ID 'BUK'      FIELD P_BUKRS.

   CALL TRANSACTION 'ZIMA2' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_ZTIV

*&---------------------------------------------------------------------
*&      Form  P3000_CREATE_ZTVT
*&---------------------------------------------------------------------
FORM P3000_CREATE_ZTVT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.

  CLEAR : SV_BUKRS, SV_ZFREQTY, SV_LIFNR, SV_ZFWERKS, W_PROC_CNT.
  SORT IT_ZTVT BY BUKRS ZFREQTY LIFNR ZFWERKS.

  LOOP AT IT_ZTVT.
*> 회사코드, 문서유형, 거래처, 플랜트별로 세금계산서를 만듬..
       IF SV_BUKRS NE IT_ZTVT-BUKRS OR SV_ZFREQTY NE IT_ZTVT-ZFREQTY OR
          SV_LIFNR NE IT_ZTVT-LIFNR OR SV_ZFWERKS NE IT_ZTVT-ZFWERKS.

          PERFORM P3000_INSERT_ZTVT USING W_ERR_CHK.
          IF W_ERR_CHK = 'Y'. EXIT. ENDIF.

          PERFORM P2000_GET_NUMBER_NEXT USING 'VT' W_ZFVTNO.

          MOVE  : IT_ZTVT-BUKRS    TO   SV_BUKRS,
                  IT_ZTVT-ZFREQTY  TO   SV_ZFREQTY,
                  IT_ZTVT-LIFNR    TO   SV_LIFNR,
                  IT_ZTVT-ZFWERKS  TO   SV_ZFWERKS,
                  IT_ZTVT-ZFDODT   TO   SV_ZFDODT.
       ENDIF.

       CLEAR ZTVTSG3.
       MOVE SY-MANDT          TO ZTVTSG3-MANDT.
       MOVE W_ZFVTNO          TO ZTVTSG3-ZFVTNO.
*>> 수입의뢰 번호, 승인번호 GET
       SELECT SINGLE * FROM EKKO
              WHERE    EBELN EQ IT_ZTVT-EBELN.

       CLEAR : W_ZFOPNNO, W_ZFREQNO.

       IF EKKO-BSTYP EQ 'F' OR EKKO-BSTYP EQ 'L'.
          SELECT MAX( ZFREQNO )
                 INTO   W_ZFREQNO
                 FROM   ZTREQHD
                 WHERE  EBELN EQ IT_ZTVT-EBELN.
       ENDIF.
       IF W_ZFREQNO IS INITIAL.
          SELECT MAX( ZFREQNO )
          INTO   W_ZFREQNO
          FROM   ZTREQHD
          WHERE  EBELN EQ
          ( SELECT MAX( KONNR ) FROM EKAB
                   WHERE EBELN = IT_ZTVT-EBELN ).
       ENDIF.

       SELECT SINGLE * FROM ZTREQHD
              WHERE    ZFREQNO EQ W_ZFREQNO.

**>> 자재내역별로 ITEM INSERT
*       SELECT COUNT( * ) INTO W_COUNT
*       FROM   ZTVTSG3
*       WHERE  MATNR    = IT_ZTVT-MATNR
*       AND    ZFREMK1  = W_ZFOPNNO.
*       IF W_COUNT = 0.
*>> 반복수 SET
          SELECT MAX( ZFLSG3 ) INTO W_ZFLSG3
          FROM   ZTVTSG3
          WHERE  ZFVTNO = W_ZFVTNO.
*>> 자재내역 SET
          SELECT SINGLE MAKTX  INTO  ZTVTSG3-ZFGONM
          FROM   MAKT
          WHERE  MATNR    EQ  IT_ZTVT-MATNR
          AND    SPRAS    EQ  SY-LANGU.

          ADD 1              TO W_ZFLSG3.
          MOVE W_ZFLSG3      TO ZTVTSG3-ZFLSG3.
          MOVE IT_ZTVT-MATNR TO ZTVTSG3-MATNR.
*>> 공급가액, 수량, 단가, 단위, 가격단위 SUM
          SELECT SUM( ZFIVAMT ) MAX( ZFIVAMC ) SUM( ZFKAMT )
                 SUM( ZFQUN )   MAX( ZFQUNM )  MAX( NETPR )
                 MAX( PEINH )   MAX( BPRME )
          INTO   (ZTVTSG3-ZFSAMF, ZTVTSG3-ZFSAMFC, ZTVTSG3-ZFSAMK,
                  ZTVTSG3-ZFQUN,  ZTVTSG3-ZFQUNM,  ZTVTSG3-NETPR,
                  ZTVTSG3-PEINH,  ZTVTSG3-BPRME )
          FROM   ZVVTIV_IT
          WHERE  EBELN   EQ  IT_ZTVT-EBELN
          AND    MATNR   EQ  IT_ZTVT-MATNR
          AND    ZFWERKS EQ  IT_ZTVT-ZFWERKS
          AND    LIFNR   EQ  IT_ZTVT-LIFNR
          AND    ZFREQTY EQ  IT_ZTVT-ZFREQTY.

*>> 참조사항1 항목에 LC OPEN NO.
          MOVE W_ZFOPNNO TO  ZTVTSG3-ZFREMK1.
          MOVE 'KRW'     TO  ZTVTSG3-ZFKRW.
*>> LOCAL 인 경우 AMEND CHECK하여서 OPEN시의 환율 GET
          IF IT_ZTVT-ZFREQTY = 'LO'.
             MOVE  ZTREQHD-KURSF TO ZTVTSG3-ZFEXRT.
*              SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
*              FROM   ZTREQST
*              WHERE  ZFREQNO = W_ZFREQNO.
*              IF W_ZFAMDNO > 0.
*                 SELECT SINGLE * FROM ZTLLCAMHD
*                 WHERE  ZFREQNO = W_ZFREQNO
*                 AND    ZFAMDNO = W_ZFAMDNO.
*                 MOVE  ZTLLCAMHD-ZFEXRT TO ZTVTSG3-ZFEXRT.  " Open 환?
*              ELSE.
*                 SELECT SINGLE * FROM   ZTLLCHD
*                 WHERE  ZFREQNO = W_ZFREQNO.
*                 MOVE  ZTLLCHD-ZFEXRT   TO ZTVTSG3-ZFEXRT. " Open 환?
*              ENDIF.
          ENDIF.
*>> 물품내역 INSERT
          INSERT  ZTVTSG3.
          IF SY-SUBRC NE 0.
             W_ERR_CHK = 'Y'. EXIT.
          ENDIF.
*       ENDIF.

*>> 세금계산서 번호 UPDATE.
       UPDATE ZTVTIV
       SET    ZFVTNO = W_ZFVTNO
              UNAM   = SY-UNAME
              UDAT   = SY-DATUM
       WHERE  BUKRS   = IT_ZTVT-BUKRS
       AND    ZFGFDYR = IT_ZTVT-ZFGFDYR
       AND    ZFGFDNO = IT_ZTVT-ZFGFDNO.

  ENDLOOP.

  PERFORM P3000_INSERT_ZTVT USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.  EXIT. ENDIF.

  LOOP AT IT_SELECTED.
     W_TABIX = SY-TABIX.

     SELECT SINGLE * FROM ZTVTIV
            WHERE  BUKRS   = IT_SELECTED-BUKRS
            AND    ZFGFDYR = IT_SELECTED-ZFGFDYR
            AND    ZFGFDNO = IT_SELECTED-ZFGFDNO.

     IF SY-SUBRC EQ 0.
        IF ZTVTIV-ZFVTNO IS INITIAL.
           DELETE IT_SELECTED INDEX W_TABIX.
           CONTINUE.
        ELSE.
           MOVE : ZTVTIV-ZFVTNO TO IT_SELECTED-ZFVTNO.
           MODIFY IT_SELECTED INDEX W_TABIX.
           CONTINUE.
        ENDIF.
     ENDIF.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_ZTVT

*&---------------------------------------------------------------------
*&      Form  P3000_INSERT_ZTVT
*&---------------------------------------------------------------------
FORM P3000_INSERT_ZTVT USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  ADD   1  TO W_PROC_CNT.
  IF W_PROC_CNT = 1. EXIT. ENDIF.

  CLEAR : ZTVT, ZTVTSG1.

  MOVE SV_BUKRS          TO ZTVT-BUKRS.
  MOVE W_ZFVTNO          TO ZTVT-ZFVTNO.
  MOVE 'KRW'             TO ZTVT-ZFKRW.
  MOVE '9'               TO ZTVT-ZFEDFN.  " 전자문서기?
  MOVE SV_ZFDODT         TO ZTVT-ZFVCDT.  " 작성?

*>> 접수구분 SET
  IF ZTIMIMG00-ZFTAXYN IS INITIAL.
     MOVE  'M'    TO  ZTVT-ZFVTRYN.
  ELSE.
     MOVE  'N'    TO  ZTVT-ZFVTRYN.
  ENDIF.
*>> 공급가액, 세액, 공란수
  SELECT SUM( ZFSAMK ) SUM( ZFTXAMT ) SUM( ZFSAMF ) MAX( ZFSAMFC )
  INTO   (ZTVT-ZFTSAMK, ZTVT-ZFTTXAM, ZTVT-ZFTSAMF, ZTVT-ZFTSAMFC)
  FROM   ZTVTSG3
  WHERE  ZFVTNO = ZTVT-ZFVTNO.
*>> 공란수.
  SELECT  COUNT( * ) INTO  ZTVT-ZFVERC
  FROM    ZTVTSG3    WHERE ZFVTNO = ZTVT-ZFVTNO.

  ZTVT-ZFTOTAM = ZTVT-ZFTSAMK + ZTVT-ZFTTXAM.
  ZTVT-ZFVTAMT = ZTVT-ZFTSAMK.
  MOVE 'N'               TO ZTVT-ZFEDIST.
  MOVE 'O'               TO ZTVT-ZFEDICK.
  MOVE SV_BUKRS          TO ZTVT-BUKRS.
  MOVE SV_LIFNR          TO ZTVT-LIFNR.
  MOVE SY-UNAME          TO ZTVT-UNAM.
  MOVE SY-DATUM          TO ZTVT-UDAT.
  MOVE SY-UNAME          TO ZTVT-ERNAM.
  MOVE SY-DATUM          TO ZTVT-CDAT.
  INSERT ZTVT.

  IF SY-SUBRC <> 0.
     W_ERR_CHK = 'Y'.  EXIT.
  ENDIF.

  MOVE W_ZFVTNO TO ZTVTSG1-ZFVTNO.
*----------------------------------------------------------------------
* CALL FUNCTION    'ZIM_GET_VENDOR_ADDRESS_FORMAT'
*-----------------------------------------------------------------------

  CLEAR : W_LFA1, W_ADRC.

  DATA: L_NAME1(255)     TYPE   C,
        L_NAME2(255)     TYPE   C,
        L_NAME3(255)     TYPE   C,
        L_NAME4(255)     TYPE   C.

   CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
        EXPORTING
            LIFNR         =  SV_LIFNR
        IMPORTING
               NAME1         =  L_NAME1
               NAME2         =  L_NAME2
               NAME3         =  L_NAME3
               NAME4         =  L_NAME4
               P_LFA1        =  W_LFA1
               P_ADRC        =  W_ADRC
        EXCEPTIONS
               NO_INPUT      = 01
               NOT_FOUND     = 03.

   CASE SY-SUBRC.
      WHEN 01.     MESSAGE I025.
      WHEN 03.     MESSAGE E020   WITH    SV_LIFNR.
   ENDCASE.

   TRANSLATE : L_NAME2  TO   UPPER CASE,
               L_NAME3  TO   UPPER CASE,
               L_NAME4  TO   UPPER CASE.

   MOVE : L_NAME2       TO   ZTVTSG1-ZFADD11,
          L_NAME3       TO   ZTVTSG1-ZFADD21,
          L_NAME4       TO   ZTVTSG1-ZFADD31.

*   MOVE: W_LFA1-NAME1   TO   P_NAME1.

  MOVE W_LFA1-STCD2         TO ZTVTSG1-ZFTXN1.  " 공급자 사업자등록번?
  MOVE W_LFA1-NAME2         TO ZTVTSG1-ZFCONM1. " 공급자 상?
  MOVE W_LFA1-J_1KFREPRE    TO ZTVTSG1-ZFCHNM1. " 공급자 대표자?
  MOVE W_LFA1-BAHNS         TO ZTVTSG1-ZFLEID1. " 공급자 전자서?
*  MOVE LFA1-STRAS          TO ZTVTSG1-ZFADD11. " 공급자 주소1
*  MOVE LFA1-ORT01          TO ZTVTSG1-ZFADD21. " 공급자 주소2
*  MOVE LFA1-ORT02          TO ZTVTSG1-ZFADD31. " 공급자 주소3
  MOVE W_LFA1-J_1KFTBUS     TO ZTVTSG1-ZFBUTY1. " 공급자 업?
  MOVE W_LFA1-J_1KFTIND     TO ZTVTSG1-ZFBUGR1. " 공급자 종?
*>>>KSB
*  CLEAR : ZTBPADDR.
  SELECT SINGLE *
    FROM ZVT001W
   WHERE WERKS = SV_ZFWERKS.
*>> 공급받는자 DATA GET.
  SELECT  SINGLE * FROM ZTIMIMGTX  WHERE  BUKRS  EQ  SV_BUKRS.
*>>>KSB
*  SELECT SINGLE *
*    FROM ZTBPADDR
*   WHERE BUKRS = '1000'
*     AND BRANCH = ZVT001W-J_1BBRANCH.
*  MOVE ZTBPADDR-STCD2     TO ZTVTSG1-ZFTXN2. " 공급받는자사업자등록번?

  MOVE ZTIMIMGTX-ZFELENML TO ZTVTSG1-ZFCONM2. " 공급받는자 상?
  MOVE ZTIMIMGTX-ZFTDNM2  TO ZTVTSG1-ZFCHNM2. " 공급받는자 대표자?
*  MOVE ZTIMIMGTX-ZFREPREL TO ZTVTSG1-ZFCHNM2. " 공급받는자 대표자?
*>>>KSB
*  MOVE ZTBPADDR-ZFADDR    TO ZTVTSG1-ZFADD12. " 공급받는자 주소1
  INSERT ZTVTSG1.
  IF SY-SUBRC <> 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.

  PERFORM P3000_VATBIL_EDI_CHECK.

ENDFORM.                    " P3000_INSERT_ZTVT

*&---------------------------------------------------------------------
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------
FORM P2000_SET_SELETE_OPTION USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.


ENDFORM.                    " P2000_SET_SELETE_OPTION

*&---------------------------------------------------------------------
*&      Form  P2000_SHOW_ZTVT
*&---------------------------------------------------------------------
FORM P2000_SHOW_ZTVT USING    P_ZFGFDYR
                              P_ZFGFDNO.

   SELECT SINGLE *
     FROM ZTVTIV
    WHERE ZFGFDYR = P_ZFGFDYR
      AND ZFGFDNO = P_ZFGFDNO.

*   IF P_ZFGFDYR IS INITIAL OR P_ZFGFDNO IS INITIAL.
   IF SY-SUBRC NE 0.
      MESSAGE E506 WITH P_ZFGFDYR P_ZFGFDNO '세금계산서'.
   ENDIF.

   SET PARAMETER ID 'ZPVTNO'  FIELD ZTVTIV-ZFVTNO.
   EXPORT 'ZPVTNO'  TO MEMORY ID 'ZPVTNO'.
   CALL TRANSACTION 'ZIMA4' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_ZTVT

*&---------------------------------------------------------------------
*&      Form  P3000_CREATE_ZTRED
*&---------------------------------------------------------------------
FORM P3000_CREATE_ZTRED USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  REFRESH IT_ZTRED.
  CLEAR : W_PROC_CNT, IT_ZTRED.

  LOOP AT IT_SELECTED.

       SELECT  A~ZFGFDYR   A~ZFGFDNO   A~LIFNR   A~ZFVTNO
               B~EBELN     B~ZFWERKS   A~BUKRS
       APPENDING  CORRESPONDING FIELDS OF TABLE IT_ZTRED
       FROM    ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
       ON      A~ZFGFDYR    EQ   B~ZFGFDYR
       AND     A~ZFGFDNO    EQ   B~ZFGFDNO
       AND     A~BUKRS      EQ   B~BUKRS
       WHERE   A~ZFGFDYR    EQ   IT_SELECTED-ZFGFDYR
       AND     A~ZFGFDNO    EQ   IT_SELECTED-ZFGFDNO
       AND     A~BUKRS      EQ   IT_SELECTED-BUKRS
       AND     A~ZFVTNO     NE   SPACE
       AND     A~ZFREDNO    EQ   SPACE
       AND     B~ZFREQTY    EQ   'LO'.

       LOOP  AT  IT_ZTRED.

          W_TABIX  =  SY-TABIX.

          CLEAR ZTVT.

          SELECT SINGLE *
          FROM   ZTVT
          WHERE  ZFVTNO = IT_ZTRED-ZFVTNO.

          IF SY-SUBRC NE 0 OR ZTVT-ZFVTRYN = 'N'.
             DELETE   IT_ZTRED  INDEX  W_TABIX.
          ENDIF.

       ENDLOOP.

  ENDLOOP.

  DESCRIBE TABLE IT_ZTRED LINES W_COUNT.
  IF W_COUNT = 0.
     EXIT.
  ENDIF.

  CLEAR : SV_EBELN.
  SORT IT_ZTRED BY EBELN.
  LOOP AT IT_ZTRED.
       IF SV_EBELN NE IT_ZTRED-EBELN.
          PERFORM P2000_GET_NUMBER_NEXT USING 'RE' W_ZFREDNO.
          PERFORM P3000_INSERT_ZTRED USING W_ERR_CHK.
          IF W_ERR_CHK = 'Y'.
             EXIT.
          ENDIF.
       ENDIF.
*       UPDATE ZTVTIV SET ZFREDNO = W_ZFREDNO
*                         UNAM = SY-UNAME
*                         UDAT = SY-DATUM
*        WHERE ZFGFDYR = IT_ZTRED-ZFGFDYR
*          AND ZFGFDNO = IT_ZTRED-ZFGFDNO
*          AND BUKRS   = IT_ZTRED-BUKRS.

       MOVE IT_ZTRED-EBELN TO SV_EBELN.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_ZTRED

*&---------------------------------------------------------------------
*&      Form  P3000_INSERT_ZTRED
*&---------------------------------------------------------------------
FORM P3000_INSERT_ZTRED USING    W_ERR_CHK.

  MOVE 'N' TO W_ERR_CHK.
  ADD   1  TO W_PROC_CNT.

  CLEAR : ZTRED, ZTREDSG1.

  MOVE W_ZFREDNO          TO ZTRED-ZFREDNO. " 인수증 관리번?
*  MOVE IT_ZTRED-ZFVTNO    TO ZTRED-ZFVTNO. " 세금계산서 관리번?
*  MOVE IT_ZTRED-EBELN     TO ZTRED-EBELN. " P/O No
*  MOVE EKAB-KONNR         TO ZTRED-EBELN.
*  MOVE IT_ZTRED-LIFNR     TO ZTRED-LIFNR. " Vendor
  MOVE : SV_ZFVTNO         TO ZTRED-ZFVTNO, " 세금계산서.
*         SV_EBELN          TO ZTRED-EBELN,
         SV_BSTYP          TO ZTRED-BSTYP,
         SV_KONNR          TO ZTRED-EBELN,
         SV_BUKRS          TO ZTRED-BUKRS,
         SV_LIFNR          TO ZTRED-LIFNR.

  IF SV_BSTYP EQ 'K'.
     SELECT MAX( ZFREQNO )
            INTO W_ZFREQNO
            FROM ZTREQHD
            WHERE EBELN EQ SV_KONNR.
  ELSE.
     SELECT MAX( ZFREQNO )
            INTO W_ZFREQNO
            FROM ZTREQHD
            WHERE EBELN EQ SV_EBELN.
  ENDIF.

  SELECT SINGLE *
         FROM ZTREQHD
         WHERE ZFREQNO EQ W_ZFREQNO.
*   WHERE EBELN = IT_ZTRED-EBELN.

  CONCATENATE 'S' SY-DATUM 'LOCRCT' ZTREQHD-ZFOPNNO INTO ZTRED-ZFISNO.

"발급No
  SELECT SINGLE ZFVCDT INTO ZTRED-ZFREVDT " 인수?
         FROM   ZTVT
         WHERE  ZFVTNO = SV_ZFVTNO.
  ZTRED-ZFISUDT = ZTRED-ZFREVDT + 9. " 발급?

  SELECT SINGLE NAME1 INTO ZTRED-ZFSCONM " 공급자 상?
         FROM LFA1
         WHERE LIFNR = SV_LIFNR.

  MOVE ZTIMIMGTX-ZFELENML TO ZTRED-ZFRCONM. " 수령인 상?
  MOVE ZTIMIMGTX-ZFTDNM2  TO ZTRED-ZFRCHNM. " 수령인 대표자?
*  MOVE ZTIMIMGTX-ZFREPREL TO ZTRED-ZFRCHNM. " 수령인 대표자?
*  CLEAR : ZTBPADDR, LFA1.

*  SELECT SINGLE *
*    FROM ZVT001W
*   WHERE WERKS = IT_ZTRED-ZFWERKS.

*  SELECT SINGLE *
*    FROM ZTBPADDR
*   WHERE BUKRS = '1000'
*     AND BRANCH = ZVT001W-J_1BBRANCH.
*  MOVE ZTBPADDR-STCD2     TO ZTRED-ZFTXN4. " 수령인 사업자등록번?

  SELECT SUM( ZFIVAMT ) MAX( ZFIVAMC ) SUM( ZFKAMT ) MAX( MATNR )
    INTO (ZTRED-ZFREAMF, ZTRED-ZFREAMFC, ZTRED-ZFREAMK, W_MATNR)
    FROM ZVVTIV_IT                   " 인수금액, 원?
    WHERE ZFVTNO EQ ZTVT-ZFVTNO.
*    WHERE EBELN = IT_ZTRED-EBELN
*    AND   EBELP = IT_ZTRED-EBELP.

  MOVE 'KRW'             TO ZTRED-ZFKRW.
  MOVE ZTRED-ZFREAMF     TO ZTRED-ZFTOTAM. " 총금?
  MOVE ZTRED-ZFREAMFC    TO ZTRED-ZFTOTAMC. " 총금액통?

  SELECT SINGLE * FROM ZTREQHD
         WHERE ZFREQNO = W_ZFREQNO.

  MOVE ZTREQHD-ZFOPNNO   TO ZTRED-ZFLLCON. "L/C No

  SELECT SINGLE *
         FROM LFA1
         WHERE LIFNR = ZTREQHD-ZFOPBN.

  MOVE LFA1-BAHNS          TO ZTRED-ZFOBNEID. " 개설은행 식별?
  MOVE ZTREQHD-ZFLASTAM    TO ZTRED-ZFOPAMF.  " 개설금액-외?
  MOVE ZTREQHD-WAERS       TO ZTRED-ZFOPAMFC. " 개설금액-외화통?

  SELECT SINGLE *
         FROM ZTLLCHD
         WHERE ZFREQNO = W_ZFREQNO.
  MOVE ZTLLCHD-ZFOBNM      TO ZTRED-ZFOBNM. " 개설은행?
  MOVE ZTLLCHD-ZFOBBR      TO ZTRED-ZFOBBR. " 개설은행지점?

  SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
         FROM ZTREQST
         WHERE ZFREQNO = W_ZFREQNO.

  IF W_ZFAMDNO > 0.
     SELECT SINGLE *
       FROM ZTLLCAMHD
      WHERE ZFREQNO = W_ZFREQNO
        AND ZFAMDNO = W_ZFAMDNO.
     MOVE  ZTLLCAMHD-ZFNGDDT TO ZTRED-ZFGDDT. " 물품인도기?
     MOVE  ZTLLCAMHD-ZFNEXDT TO ZTRED-ZFEXDT. " 유효기?
     MOVE  ZTLLCAMHD-ZFEXRT  TO ZTRED-ZFLEXRT. " 환?
  ELSE.
     SELECT SINGLE *
            FROM ZTLLCHD
            WHERE ZFREQNO = W_ZFREQNO.
     MOVE  ZTLLCHD-ZFGDDT    TO ZTRED-ZFGDDT. " 물품인도기?
     MOVE  ZTLLCHD-ZFEXDT    TO ZTRED-ZFEXDT. " 유효기?
     MOVE  ZTLLCHD-ZFEXRT    TO ZTRED-ZFLEXRT. " 환?
  ENDIF.

  MOVE 'N'                 TO ZTRED-ZFEDIST. " EDI 상?
  MOVE 'O'                 TO ZTRED-ZFEDICK. " EDI Check
  MOVE SY-UNAME            TO ZTRED-UNAM.
  MOVE SY-DATUM            TO ZTRED-UDAT.
  MOVE SY-UNAME            TO ZTRED-ERNAM.
  MOVE SY-DATUM            TO ZTRED-CDAT.
  INSERT ZTRED.
  IF SY-SUBRC <> 0.
     MESSAGE I849.
     W_ERR_CHK = 'Y'.
     EXIT.
  ENDIF.

*  MOVE W_ZFREDNO           TO ZTREDSG1-ZFREDNO. " 인수증 관리번?
*  MOVE 1                   TO ZTREDSG1-ZFLSG1. " 반복?
*
*  SELECT SINGLE MAKTX INTO ZTREDSG1-MAKTX     " Material Descr.
*    FROM MAKT
*   WHERE SPRAS = SY-LANGU
*     AND MATNR = W_MATNR.
*
*  INSERT ZTREDSG1.
*
*  IF SY-SUBRC <> 0.
*     MESSAGE I849.
*     W_ERR_CHK = 'Y'.
*     EXIT.
*  ENDIF.
*
*  PERFORM P3000_LOCRCT_EDI_CHECK.

ENDFORM.                    " P3000_INSERT_ZTRED

*&---------------------------------------------------------------------
*&      Form  P2000_SHOW_ZTRED
*&---------------------------------------------------------------------
FORM P2000_SHOW_ZTRED USING    P_ZFGFDYR
                               P_ZFGFDNO
                               P_BUKRS.

   SELECT SINGLE *
     FROM ZTVTIV
    WHERE ZFGFDYR = P_ZFGFDYR
      AND ZFGFDNO = P_ZFGFDNO
      AND BUKRS   = P_BUKRS.

   IF SY-SUBRC NE 0.
      MESSAGE E506 WITH P_ZFGFDYR P_ZFGFDNO '인수증'.
   ENDIF.

   IF P_ZFGFDYR IS INITIAL OR P_ZFGFDNO IS INITIAL OR
      P_BUKRS   IS INITIAL.
      MESSAGE E506 WITH P_ZFGFDYR P_ZFGFDNO '인수증'.
   ENDIF.

   SET PARAMETER ID 'BES'      FIELD ''.
   SET PARAMETER ID 'ZPREDNO'  FIELD ZTVTIV-ZFREDNO.
   EXPORT 'ZPREDNO'  TO MEMORY ID 'ZPREDNO'.
   CALL TRANSACTION 'ZIMA7' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_ZTRED

*&---------------------------------------------------------------------
*&      Form  P3000_VATBIL_EDI_CHECK
*&---------------------------------------------------------------------
FORM P3000_VATBIL_EDI_CHECK.

  MOVE 'O'    TO ZTVT-ZFEDICK.  " EDI Check
  IF ZTVTSG1-ZFTXN1 IS INITIAL. " 공급자 사업자등록번?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCONM1 IS INITIAL. " 공급자 상?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCHNM1 IS INITIAL. " 공급자 대표자?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFADD11 IS INITIAL. " 공급자 주소 1
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFTXN2 IS INITIAL. " 공급받는자 사업자등록번?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCONM2 IS INITIAL. " 공급받는자 상?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFCHNM2 IS INITIAL. " 공급받는자 대표자?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVTSG1-ZFADD12 IS INITIAL. " 공급받는자 주소 1
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVT-ZFVCDT IS INITIAL. " 세금계산서 작성?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVT-ZFVERC IS INITIAL. " 공란?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF ZTVT-ZFVTAMT IS INITIAL. " 공급가?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  IF LFA1-BAHNS IS INITIAL. " EDI 식별?
     MOVE 'X' TO ZTVT-ZFEDICK.
  ENDIF.
  UPDATE ZTVT.
  IF SY-SUBRC <> 0.
     MESSAGE I828.
     W_ERR_CHK = 'Y'.
     EXIT.
  ENDIF.

ENDFORM.                    " P3000_VATBIL_EDI_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_LOCRCT_EDI_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LOCRCT_EDI_CHECK.






ENDFORM.                    " P3000_LOCRCT_EDI_CHECK
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '세금계산서 Simulation'.
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
         PERFORM  P1000_CREATE_ZTVT_SCR0100.
         PERFORM  P2000_WRITE_TITLE_SCR0100.
         PERFORM  P3000_WRITE_ZTVT_SCR0100.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  P1000_CREATE_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_CREATE_ZTVT_SCR0100.

  REFRESH : IT_SIM.
  CLEAR :   W_PROC_CNT.

*  LOOP AT IT_SELECTED.
*
*      SELECT  A~ZFGFDYR A~ZFGFDNO A~LIFNR   A~ZFDODT B~MATNR
*              B~ZFREQTY B~EBELN   B~ZFWERKS A~BUKRS  B~EBELP
*      APPENDING  CORRESPONDING FIELDS OF TABLE IT_ZTVT
*      FROM    ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
*      ON      A~ZFGFDYR    EQ   B~ZFGFDYR
*      AND     A~ZFGFDNO    EQ   B~ZFGFDNO
*      AND     A~BUKRS      EQ   B~BUKRS
*      WHERE   A~ZFGFDYR    EQ   IT_SELECTED-ZFGFDYR
*      AND     A~ZFGFDNO    EQ   IT_SELECTED-ZFGFDNO
*      AND     A~BUKRS      EQ   IT_SELECTED-BUKRS
*      AND     A~ZFVTNO     EQ   SPACE.
*
* ENDLOOP.
*
* DESCRIBE TABLE IT_ZTVT LINES W_COUNT.
* IF W_COUNT = 0.
*    SET SCREEN 0. LEAVE SCREEN.
*    EXIT.
* ENDIF.

  CLEAR : SV_ZFREQTY, SV_LIFNR, SV_ZFWERKS.
  SORT IT_ZTVT BY ZFREQTY LIFNR ZFWERKS  MATNR.

  LOOP AT IT_ZTVT.

       IF SV_ZFREQTY NE IT_ZTVT-ZFREQTY OR SV_LIFNR NE IT_ZTVT-LIFNR OR
          SV_ZFWERKS NE IT_ZTVT-ZFWERKS.
          PERFORM P3000_INSERT_ZTVT_SCR0100.
          CLEAR : SUM_ZFTOTAM, SUM_ZFTSAMK, SUM_ZFTTXAM,
                  SUM_ZFTSAMF, SV_ZFTSAMFC.
          MOVE  : IT_ZTVT-ZFREQTY   TO   SV_ZFREQTY,
                  IT_ZTVT-LIFNR     TO   SV_LIFNR,
                  IT_ZTVT-ZFWERKS   TO   SV_ZFWERKS,
                  IT_ZTVT-EBELN     TO   SV_EBELN,
                  IT_ZTVT-MATNR     TO   SV_MATNR.
       ENDIF.

       CLEAR  IT_SIM.
*>> 자재내역별로 ITEM INSERT
       READ TABLE IT_SIM WITH KEY ZFREQTY  = IT_ZTVT-ZFREQTY
                                  LIFNR    = IT_ZTVT-LIFNR
                                  ZFWERKS  = IT_ZTVT-ZFWERKS
                                  MATNR    = IT_ZTVT-MATNR
                                  EBELN    = IT_ZTVT-EBELN.
       IF SY-SUBRC NE 0.

*>> 자재내역 SET
          SELECT SINGLE MAKTX  INTO  IT_SIM-ZFGONM
          FROM   MAKT
          WHERE  MATNR    EQ  IT_ZTVT-MATNR
          AND    SPRAS    EQ  SY-LANGU.
*>> 수량, 금액 SUM.
          SELECT SUM( ZFIVAMT ) MAX( ZFIVAMC ) SUM( ZFKAMT )
                 SUM( ZFQUN )   MAX( ZFQUNM )
          INTO   (IT_SIM-ZFSAMF, IT_SIM-ZFSAMFC, IT_SIM-ZFSAMK,
                  IT_SIM-ZFQUN,  IT_SIM-ZFQUNM)
          FROM   ZVVTIV_IT
          WHERE  ZFREQTY   EQ  IT_ZTVT-ZFREQTY
          AND    LIFNR     EQ  IT_ZTVT-LIFNR
          AND    ZFWERKS   EQ  IT_ZTVT-ZFWERKS
          AND    MATNR     EQ  IT_ZTVT-MATNR
          AND    EBELN     EQ  IT_ZTVT-EBELN.

          MOVE : IT_ZTVT-EBELN    TO IT_SIM-EBELN,
                 IT_ZTVT-ZFWERKS  TO IT_SIM-ZFWERKS,
                 IT_ZTVT-ZFREQTY  TO IT_SIM-ZFREQTY,
                 IT_ZTVT-LIFNR    TO IT_SIM-LIFNR,
                 IT_ZTVT-MATNR    TO IT_SIM-MATNR,
                 '2'              TO IT_SIM-SORT,
                 IT_SIM-ZFSAMFC   TO SV_ZFTSAMFC.

          ADD  : IT_SIM-ZFSAMF TO SUM_ZFTSAMF,
                 IT_SIM-ZFSAMK TO SUM_ZFTSAMK.

          APPEND IT_SIM.

        ENDIF.

  ENDLOOP.

  PERFORM P3000_INSERT_ZTVT_SCR0100.
  SORT IT_SIM BY ZFREQTY  LIFNR ZFWERKS SORT MATNR.

ENDFORM.                    " P1000_CREATE_ZTVT_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P3000_INSERT_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_INSERT_ZTVT_SCR0100 .

  ADD   1  TO W_PROC_CNT.
  IF W_PROC_CNT = 1. EXIT. ENDIF.

  CLEAR : IT_SIM.
  SUM_ZFTOTAM  =  SUM_ZFTSAMK  +  SUM_ZFTTXAM.

  MOVE : '1'               TO IT_SIM-SORT,
         SV_EBELN          TO IT_SIM-EBELN,
         SV_ZFWERKS        TO IT_SIM-ZFWERKS,
         SV_ZFREQTY        TO IT_SIM-ZFREQTY,
         SV_LIFNR          TO IT_SIM-LIFNR,
         SV_ZFWERKS        TO IT_SIM-ZFWERKS,
         SV_ZFREQTY        TO IT_SIM-ZFREQTY,
         SUM_ZFTOTAM       TO IT_SIM-ZFTOTAM,
         SUM_ZFTSAMK       TO IT_SIM-ZFTSAMK,
         SUM_ZFTTXAM       TO IT_SIM-ZFTTXAM,
         SUM_ZFTSAMF       TO IT_SIM-ZFTSAMF,
         SV_ZFTSAMFC       TO IT_SIM-ZFTSAMFC.

*>> 구매처명 DISPLAY.
   SELECT SINGLE NAME1  INTO  IT_SIM-NAME
   FROM   LFA1          WHERE LIFNR  EQ  SV_LIFNR.

   APPEND  IT_SIM.

ENDFORM.                    " P3000_INSERT_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_TITLE_SCR0100.

  SKIP 1.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE ,
            '공급처                        ' NO-GAP, SY-VLINE NO-GAP,
            '             총금액'            NO-GAP, SY-VLINE NO-GAP,
            '                세액'           NO-GAP, SY-VLINE NO-GAP,
            '     공급가액(원화)'            NO-GAP, SY-VLINE NO-GAP,
            '     공급가액(외화)     '       NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE ,
            '      '                         NO-GAP,
            '품목                          ' NO-GAP,
            '              '                 NO-GAP, SY-VLINE NO-GAP,
            '              수량  '           NO-GAP, SY-VLINE NO-GAP,
            '         금액(원화)'            NO-GAP, SY-VLINE NO-GAP,
            '         금액(외화)     '       NO-GAP, SY-VLINE NO-GAP.

ENDFORM.                    " P2000_WRITE_TITLE_SCR0100

*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_ZTVT_SCR0100.

   LOOP  AT  IT_SIM.

      IF IT_SIM-NAME NE SPACE.
         WRITE / SY-ULINE.
         FORMAT COLOR COL_NORMAL INTENSIFIED ON.
         WRITE :                                       SY-VLINE ,
                IT_SIM-LIFNR                   NO-GAP,
                IT_SIM-NAME                    NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFTOTAM CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
                ' '                            NO-GAP,
                IT_SIM-ZFTTXAM CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFTSAMK CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFTSAMF CURRENCY IT_SIM-ZFTSAMFC NO-GAP,
                IT_SIM-ZFTSAMFC                NO-GAP, SY-VLINE NO-GAP.

      ELSE.
         FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
         WRITE :                                       SY-VLINE ,
                '            '                 NO-GAP,
                IT_SIM-MATNR                   NO-GAP,
                IT_SIM-ZFGONM                  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFQUN  UNIT IT_SIM-ZFQUNM NO-GAP,
                IT_SIM-ZFQUNM                  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFSAMK  CURRENCY 'KRW'  NO-GAP, SY-VLINE NO-GAP,
                IT_SIM-ZFSAMF  CURRENCY IT_SIM-ZFSAMFC NO-GAP,
                IT_SIM-ZFSAMFC                 NO-GAP, SY-VLINE NO-GAP.
      ENDIF.

   ENDLOOP.
   WRITE / SY-ULINE.

ENDFORM.                    " P3000_WRITE_ZTVT_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_IT_WRITE.

  REFRESH : IT_ZTVT.
  CLEAR :   IT_ZTVT, W_COUNT.

  LOOP AT IT_SELECTED.

       SELECT  A~ZFGFDYR A~ZFGFDNO A~LIFNR   A~ZFDODT B~MATNR
               B~ZFREQTY B~EBELN   B~ZFWERKS A~BUKRS  B~EBELP
       APPENDING  CORRESPONDING FIELDS OF TABLE IT_ZTVT
       FROM    ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
       ON      A~ZFGFDYR    EQ   B~ZFGFDYR
       AND     A~ZFGFDNO    EQ   B~ZFGFDNO
       AND     A~BUKRS      EQ   B~BUKRS
       WHERE   A~ZFGFDYR    EQ   IT_SELECTED-ZFGFDYR
       AND     A~ZFGFDNO    EQ   IT_SELECTED-ZFGFDNO
       AND     A~BUKRS      EQ   IT_SELECTED-BUKRS
       AND     A~ZFVTNO     EQ   SPACE.

  ENDLOOP.

  DESCRIBE TABLE IT_ZTVT LINES W_COUNT.

ENDFORM.                    " P2000_IT_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DB_ROLLBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_DB_ROLLBACK.

   LOOP  AT  IT_ZTVT.

      SELECT  SINGLE * FROM  ZTVTIV
      WHERE   ZFGFDYR  EQ    IT_ZTVT-ZFGFDYR
      AND     ZFGFDNO  EQ    IT_ZTVT-ZFGFDNO
      AND     BUKRS    EQ    IT_ZTVT-BUKRS.

*>> 세금계산서 내역 DELETE
      IF SY-SUBRC EQ  0.
         DELETE  FROM  ZTVT      WHERE  ZFVTNO  EQ  ZTVTIV-ZFVTNO.
         DELETE  FROM  ZTVTSG1   WHERE  ZFVTNO  EQ  ZTVTIV-ZFVTNO.
         DELETE  FROM  ZTVTSG3   WHERE  ZFVTNO  EQ  ZTVTIV-ZFVTNO.
      ENDIF.
*>> 세금계산서용 송장 DB UPDATE
      UPDATE  ZTVTIV
      SET     ZFVTNO   =  SPACE
              UNAM     =  SY-UNAME
              UDAT     =  SY-DATUM
      WHERE   ZFGFDYR  =  IT_ZTVT-ZFGFDYR
      AND     ZFGFDNO  =  IT_ZTVT-ZFGFDNO
      AND     BUKRS    =  IT_ZTVT-BUKRS.

   ENDLOOP.

ENDFORM.                    " P3000_DB_ROLLBACK
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_ZTVTIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_ZTVTIT.
DATA : BEGIN OF IT_VTNO  OCCURS 0,
       ZFVTNO   LIKE   ZTVT-ZFVTNO,
       END   OF IT_VTNO.

  REFRESH : IT_ZTRED, IT_ERR_LIST.
  CLEAR :   IT_ZTRED, W_ERR_CNT.

  LOOP AT IT_SELECTED.
     MOVE IT_SELECTED-ZFVTNO TO IT_VTNO-ZFVTNO.
     COLLECT IT_VTNO.
  ENDLOOP.

  LOOP AT IT_VTNO.

       SELECT  B~EBELN          B~EBELP     MAX( A~BUKRS )   AS BUKRS
               MAX( A~LIFNR )   AS  LIFNR   MAX( A~ZFVTNO )  AS ZFVTNO
               SUM( B~ZFQUN )   AS  ZFQUN   MAX( B~ZFQUNM )  AS ZFQUNM
               SUM( B~ZFIVAMT ) AS  ZFREAM  MAX( B~ZFIVAMC ) AS ZFREAMC
               SUM( B~ZFKAMT )  AS  ZFREAMK MAX( B~NETPR )   AS NETPR
               MAX( B~BPRME )   AS  BPRME   MAX( B~PEINH )   AS PEINH
               MAX( B~MATNR )   AS  MATNR
       APPENDING  CORRESPONDING FIELDS OF TABLE IT_ZTRED
       FROM    ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
       ON      A~ZFGFDYR    EQ   B~ZFGFDYR
       AND     A~ZFGFDNO    EQ   B~ZFGFDNO
       AND     A~BUKRS      EQ   B~BUKRS
       WHERE   A~ZFVTNO     EQ   IT_VTNO-ZFVTNO
       AND     A~ZFREDNO    EQ   SPACE
       AND     B~ZFREQTY    EQ   'LO'
       GROUP BY B~EBELN   B~EBELP.

*>> ERROR 사항 CHECK!
       SELECT  B~ZFREQTY   A~ZFREDNO
        INTO   (W_ZFREQTY, W_ZFREDNO)
        FROM   ZTVTIV AS  A INNER  JOIN  ZTVTIVIT  AS  B
        ON     A~ZFGFDYR    EQ   B~ZFGFDYR
        AND    A~ZFGFDNO    EQ   B~ZFGFDNO
        AND    A~BUKRS      EQ   B~BUKRS
        WHERE  A~ZFVTNO     EQ   IT_VTNO-ZFVTNO.

*>> ERROR MESSAGE 생성.
       IF  W_ZFREQTY  NE 'LO'.
           MESSAGE S625 WITH IT_VTNO-ZFVTNO.
           PERFORM  P2000_SINGLE_MESS_MAKE.
       ENDIF.

       IF  W_ZFREDNO NE  SPACE.
           MESSAGE S626 WITH IT_VTNO-ZFVTNO.
           PERFORM  P2000_SINGLE_MESS_MAKE.
       ENDIF.
     ENDSELECT.
  ENDLOOP.

  DESCRIBE TABLE IT_ZTRED LINES W_COUNT.

ENDFORM.                    " P2000_WRITE_ZTVTIT
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

ENDFORM.                    " P2000_SINGLE_MESS_MAKE
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
         WHEN 'E' OR 'A'.
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

ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0200 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POER'.
        SET TITLEBAR 'POPU' WITH '상태 LIST'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0200_LIST_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0200_LIST_CHECK_SCR0200 INPUT.

   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POER'.
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
               WHEN 'E' OR 'A'.
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

ENDMODULE.                 " D0200_LIST_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_LOCCRT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_CREATE_LOCCRT USING    W_ERR_CHK.

DATA : L_SUBRC LIKE SY-SUBRC.

*> 계약번호 발췌..
  LOOP AT IT_ZTRED.
     W_TABIX = SY-TABIX.
     SELECT SINGLE * FROM EKAB
            WHERE EBELN EQ IT_ZTRED-EBELN
            AND   EBELP EQ IT_ZTRED-EBELP.
     IF SY-SUBRC EQ 0.
        MOVE : EKAB-KONNR   TO  IT_ZTRED-KONNR,
               EKAB-KTPNR   TO  IT_ZTRED-KTPNR,
               'K'          TO  IT_ZTRED-BSTYP.
     ELSE.
        SELECT SINGLE * FROM EKKO
               WHERE    EBELN EQ IT_ZTRED-EBELN.
        MOVE : IT_ZTRED-EBELN   TO  IT_ZTRED-KONNR,
               IT_ZTRED-EBELP   TO  IT_ZTRED-KTPNR,
               'L'              TO  IT_ZTRED-BSTYP.
     ENDIF.
     MODIFY IT_ZTRED INDEX W_TABIX.
  ENDLOOP.

  CLEAR  : W_ZFREDNO, SV_EBELN, SV_LIFNR, SV_ZFVTNO, SV_BUKRS,
           W_PROC_CNT.

  SORT IT_ZTRED BY LIFNR KONNR KTPNR BSTYP EBELN EBELP.

  LOOP AT IT_ZTRED.
       L_SUBRC = SY-TABIX.

*>> 접수된 세금계산서에 한해서 인수증 발급.
       SELECT SINGLE * FROM  ZTVT
       WHERE  ZFVTNO   EQ    IT_ZTRED-ZFVTNO.
       IF ZTVT-ZFVTRYN EQ    'N'.  CONTINUE.  ENDIF.

*--------------------------------------------------------------------
*& 주의 : 기본적으로 구매문서와 수입의뢰의 관계는 1:1로 설정되어야 함.
*--------------------------------------------------------------------
*> 수입의뢰 아이템에는 계약일 경우,
*>     - 일괄계약의 품목번호가 들어가고
*>       세금계산서용 Invoice에는 릴리즈된 PO번호가 들어감.
*--------------------------------------------------------------------
*> 구매오더나 납품일정계약일 경우.
*--------------------------------------------------------------------
*>> PO 별로 인수증 발급.
       IF SV_KONNR NE IT_ZTRED-KONNR.
          IF L_SUBRC NE 1.
             PERFORM P3000_INSERT_ZTRED USING W_ERR_CHK.
          ENDIF.

          PERFORM P2000_GET_NUMBER_NEXT USING 'RE' W_ZFREDNO.

          MOVE : IT_ZTRED-EBELN  TO  SV_EBELN,
                 IT_ZTRED-KONNR  TO  SV_KONNR,
                 IT_ZTRED-LIFNR  TO  SV_LIFNR,
                 IT_ZTRED-BSTYP  TO  SV_BSTYP,
                 IT_ZTRED-ZFVTNO TO  SV_ZFVTNO,
                 IT_ZTRED-BUKRS  TO  SV_BUKRS.
       ENDIF.
       CLEAR : ZTREDSG1, ZVVTIV_IT.

*>> 반복수 GET.
       SELECT MAX( ZFLSG1 )  INTO  W_ZFLSG1
       FROM   ZTREDSG1
       WHERE  ZFREDNO        EQ    W_ZFREDNO.
       W_ZFLSG1  =  W_ZFLSG1 +  1.

*>> HS CODE GET!
        SELECT SINGLE *
               FROM ZTREQIT
               WHERE  EBELN  EQ  IT_ZTRED-KONNR
               AND    EBELP  EQ  IT_ZTRED-KTPNR.

*       WHERE  EBELN    EQ   IT_ZTRED-EBELN
*       AND    EBELP    EQ   IT_ZTRED-EBELP.
*>> 자재내역 GET.
*       SELECT SINGLE * FROM MAKT
*       WHERE  MATNR  EQ  ZTREDSG1-MATNR
*       AND    SPRAS  EQ  SY-LANGU.

       MOVE : SY-MANDT           TO  ZTREDSG1-MANDT,
              W_ZFREDNO          TO  ZTREDSG1-ZFREDNO,
              W_ZFLSG1           TO  ZTREDSG1-ZFLSG1,
              ZTREQIT-ZFITMNO    TO  ZTREDSG1-ZFITMNO,
              ZTREQIT-TXZ01      TO  ZTREDSG1-MAKTX,
              IT_ZTRED-EBELN     TO  ZTREDSG1-EBELN,
              IT_ZTRED-EBELP     TO  ZTREDSG1-EBELP,
              IT_ZTRED-ZFREAM    TO  ZTREDSG1-ZFREAM,
              IT_ZTRED-ZFREAMC   TO  ZTREDSG1-ZFREAMC,
              IT_ZTRED-ZFQUN     TO  ZTREDSG1-ZFQUN,
              IT_ZTRED-ZFQUNM    TO  ZTREDSG1-ZFQUNM,
              IT_ZTRED-NETPR     TO  ZTREDSG1-NETPR,
              IT_ZTRED-PEINH     TO  ZTREDSG1-PEINH,
              IT_ZTRED-BPRME     TO  ZTREDSG1-BPRME,
              IT_ZTRED-MATNR     TO  ZTREDSG1-MATNR,
              ZTREQIT-STAWN      TO  ZTREDSG1-ZFGDNO.
*              MAKT-MAKTX         TO  ZTREDSG1-MAKTX.
       INSERT ZTREDSG1.
       IF SY-SUBRC <> 0.
          W_ERR_CHK = 'Y'. EXIT.
       ENDIF.
*>> 세금계산서 송장 DB UPDATE.
       SELECT  BUKRS      ZFGFDYR     ZFGFDNO
       INTO    (W_BUKRS,  W_ZFGFDYR,  W_ZFGFDNO)
       FROM    ZTVTIVIT
       WHERE   EBELN    EQ  IT_ZTRED-EBELN
       AND     EBELP    EQ  IT_ZTRED-EBELP
       GROUP BY
               BUKRS    ZFGFDYR   ZFGFDNO.

       UPDATE  ZTVTIV
           SET     ZFREDNO  =  W_ZFREDNO
                   UNAM     =  SY-UNAME
                   UDAT     =  SY-DATUM
           WHERE   BUKRS    =  W_BUKRS
           AND     ZFGFDYR  =  W_ZFGFDYR
           AND     ZFGFDNO  =  W_ZFGFDNO.
       ENDSELECT.

  ENDLOOP.

  PERFORM P3000_INSERT_ZTRED USING W_ERR_CHK.

ENDFORM.                    " P3000_CREATE_LOCCRT
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA USING    W_ERR_CHK.
  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.

*> 세금계산서 데이타 발췌.
  SELECT  A~ZFGFDYR         A~ZFGFDNO       A~BUKRS
          B~EBELN           B~EBELP
          MAX( A~ZFPOSDT ) AS ZFPOSDT  MAX( A~ZFDODT )  AS ZFDODT
          MAX( A~LIFNR )   AS LIFNR    MAX( A~ZFVTNO )  AS ZFVTNO
          MAX( A~ZFREDNO ) AS ZFREDNO  MAX( B~ZFREQTY ) AS ZFREQTY
          SUM( B~ZFIVAMT ) AS ZFIVAMT  MAX( B~ZFIVAMC ) AS ZFIVAMC
  INTO    CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM    ZTVTIV  AS  A  INNER  JOIN  ZTVTIVIT  AS  B
  ON      A~ZFGFDYR   EQ  B~ZFGFDYR
  AND     A~ZFGFDNO   EQ  B~ZFGFDNO
  AND     A~BUKRS     EQ  B~BUKRS
  WHERE   A~BUKRS     IN  S_BUKRS
  AND     A~ZFPOSDT   IN  S_POSDT
  AND     A~ZFDODT    IN  S_DODT
  AND     A~LIFNR     IN  S_LIFNR
  AND     B~EBELN     IN  S_EBELN
*  AND     B~ZFREQTY   IN  S_REQTY
  GROUP BY
          A~ZFGFDYR
          A~ZFGFDNO
          A~BUKRS
          B~EBELN
          B~EBELP.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    CLEAR : W_ZFOPNNO.
*> 구매오더 발췌...
    SELECT SINGLE * FROM EKKO
           WHERE EBELN   EQ  IT_TAB-EBELN.

*--------------------------------------------------------------------
*& 주의 : 기본적으로 구매문서와 수입의뢰의 관계는 1:1로 설정되어야 함.
*--------------------------------------------------------------------
*> 수입의뢰 아이템에는 계약일 경우,
*>     - 일괄계약의 품목번호가 들어가고
*>       세금계산서용 Invoice에는 릴리즈된 PO번호가 들어감.
*--------------------------------------------------------------------
*> 구매오더나 납품일정계약일 경우.
*--------------------------------------------------------------------
    CLEAR : W_ZFREQNO.
    IF EKKO-BSTYP EQ 'F' OR EKKO-BSTYP EQ 'L'.
       SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
              FROM  ZTREQHD
              WHERE ZFOPNNO IN S_OPNNO
              AND   EBELN   EQ IT_TAB-EBELN
              AND   ZFREQTY EQ 'LO'.
       IF SY-SUBRC  NE 0 OR
          W_ZFREQNO IS INITIAL.
          SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
                 FROM  ZTREQHD
                 WHERE  ZFOPNNO IN S_OPNNO
                 AND   ZFREQTY EQ 'LO'
                 AND    EBELN   IN ( SELECT KONNR
                                     FROM   EKAB
                                     WHERE  EBELN EQ IT_TAB-EBELN ).
       ENDIF.
*--------------------------------------------------------------------
*> 일괄계약일 경우.
*--------------------------------------------------------------------
    ELSEIF EKKO-BSTYP EQ 'K'.
       SELECT MAX( ZFREQNO ) INTO W_ZFREQNO
              FROM  ZTREQHD
              WHERE  ZFOPNNO IN S_OPNNO
              AND   ZFREQTY EQ 'LO'
              AND    EBELN   IN ( SELECT KONNR
                                  FROM   EKAB
                                  WHERE  EBELN EQ IT_TAB-EBELN ).
    ENDIF.

*> 마지막 수입문서 상태 체크를 위해..
    SELECT SINGLE * FROM ZTREQST
           WHERE ZFREQNO  EQ  W_ZFREQNO
           AND   ZFAMDNO  EQ ( SELECT MAX( ZFAMDNO )
                               FROM  ZTREQST
                               WHERE ZFREQNO EQ W_ZFREQNO ).
    IF SY-SUBRC EQ 0.
       MOVE : ZTREQST-ZFOPNNO TO IT_TAB-ZFOPNNO,
              ZTREQST-ZFDOCST TO IT_TAB-ZFDOCST.
       IF IT_TAB-ZFDOCST NE 'O'.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ELSE.
       DELETE IT_TAB INDEX W_TABIX.
       CONTINUE.
    ENDIF.

*-----------------------------------------------------
*>> NONE. -> 송장집계만 해놓은 상태.
*-----------------------------------------------------
    IF P_N = 'X'.
       IF NOT ( IT_TAB-ZFVTNO IS INITIAL AND
                IT_TAB-ZFREDNO IS INITIAL ).
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

*> 세금계산서만 편성된 자료.
    IF P_V = 'X'.
       IF NOT ( NOT IT_TAB-ZFVTNO  IS INITIAL AND
                    IT_TAB-ZFREDNO IS INITIAL ).
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

*> 인수증 편성까지 된 자료.
    IF P_R = 'X'.
       IF IT_TAB-ZFREDNO IS INITIAL.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
       ENDIF.
    ENDIF.

    SELECT SINGLE NAME1 INTO   IT_TAB-LIFNR_NM
           FROM   LFA1
           WHERE  LIFNR = IT_TAB-LIFNR.

    MODIFY  IT_TAB.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_HEADER_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_HEADER_WRITE.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   WRITE:/ SY-VLINE,
           MARKFIELD  AS CHECKBOX,
           SY-VLINE,
           IT_TAB-ZFPOSDT,  " Posting Date
           SY-VLINE,
           IT_TAB-ZFDODT,  " Document Date
           SY-VLINE,
           IT_TAB-BUKRS,
           SY-VLINE,
           IT_TAB-ZFGFDYR    NO-GAP, " 회계전표연?
           '-'               NO-GAP,
           IT_TAB-ZFGFDNO, " 회계전표번?
           SY-VLINE NO-GAP,
           IT_TAB-LIFNR      NO-GAP, " Vendor
           IT_TAB-LIFNR_NM   NO-GAP,
           SY-VLINE,
           IT_TAB-ZFVTNO,
           SY-VLINE,
           IT_TAB-ZFREDNO,
           SY-VLINE.
*     '          '      NO-GAP,  SY-VLINE  NO-GAP,
*     '     '           NO-GAP,  SY-VLINE  NO-GAP,
*     '    '            NO-GAP,  SY-VLINE  NO-GAP,
*     '                        '   NO-GAP,
*     SY-VLINE  NO-GAP,
*     IT_TAB-CHK_YN     NO-GAP,
*     '    '            NO-GAP, SY-VLINE NO-GAP.
     HIDE    IT_TAB.

ENDFORM.                    " P3000_HEADER_WRITE
