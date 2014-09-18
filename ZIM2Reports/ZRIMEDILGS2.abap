*&---------------------------------------------------------------------*
*& Report  ZRIMEDILGS2                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입신고의뢰 EDI Send TO RD-Korea                     *
*&      작성자 : 김연중 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.09                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&   2001/09/27 KSB MODIFY
*&     1. SELECT시 로직 변경...
*&---------------------------------------------------------------------*
REPORT  ZRIMEDILGS2   MESSAGE-ID ZIM
                      LINE-SIZE 132
                      NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFINRC          LIKE   ZTIDR-ZFINRC,        " 신고지 세관.
       INRC            LIKE   DD07T-DDTEXT,        " 신고지 세관.
       ZFINRCD         LIKE   ZTIDR-ZFINRCD,       " 세관의 담당과.
       INRCD           LIKE   DD07T-DDTEXT,        " 담당과.
       ZFBNARCD        LIKE   ZTIDR-ZFBNARCD,      " 보세구역 내부코드.
       ZFBNARM         LIKE   ZTIMIMG03-ZFBNARM,
       ZFIDRNO         LIKE   ZTIDR-ZFIDRNO,       " 수입신고번호.
       ZFIDWDT         LIKE   ZTIDR-ZFIDWDT,       " 신고희망일.
       ZFRPTTY         LIKE   ZTBL-ZFRPTTY,        " 수입신고형태.
       ZFRGDSR         LIKE   ZTBL-ZFRGDSR,        " 대표품명.
       RPTTY           LIKE   DD07T-DDTEXT,        " 수입신고형태.
       ZFPONC          LIKE   ZTIDR-ZFPONC,        " 수입거래구분.
       PONC            LIKE   ZTIMIMG08-ZFCDNM,    "
       ZFITKD          LIKE   ZTIDR-ZFITKD,        " 수입종류.
       ITKD            LIKE   DD07T-DDTEXT,        " 수입종류.
       ZFCOCD          LIKE   ZTIDR-ZFCOCD,        " 관세징수형태.
       COCD            LIKE   DD07T-DDTEXT,        " 관세징수형태.
       ZFHBLNO         LIKE   ZTIDR-ZFHBLNO,       " House B/L No.
       ZFREBELN        LIKE   ZTBL-ZFREBELN,       " 대표 P/O NO.
       ZFGOMNO         LIKE   ZTIDR-ZFGOMNO,       " 화물관리번호.
       ZFCUT           LIKE   ZTIDR-ZFCUT,         " 관세사.
       NAME1           LIKE   LFA1-NAME1,
       ZFBLNO          LIKE   ZTIDR-ZFBLNO,        " B/L 관리번호.
       ZFCLSEQ         LIKE   ZTIDR-ZFCLSEQ,       " 통관순번.
       ZFCUST          LIKE   ZTCUCL-ZFCUST,       " 통관상태.
       ZFDNCD          LIKE   ZTIDR-ZFDNCD,        " Download 상태.
       ZFDOCST         LIKE   ZTIDR-ZFDOCST,
       ZFEDICK         LIKE   ZTIDR-ZFEDICK.       " EDI Check.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TAB_DOWN OCCURS 0,
       ZFINRC          LIKE   ZTIDR-ZFINRC,        " 신고지 세관.
       ZFINRCD         LIKE   ZTIDR-ZFINRCD,       " 세관의 담당과.
       ZFBNARCD        LIKE   ZTIDR-ZFBNARCD,      " 보세구역 내부코드.
       ZFIDRNO         LIKE   ZTIDR-ZFIDRNO,       " 수입신고번호.
       ZFIDWDT         LIKE   ZTIDR-ZFIDWDT,       " 신고희망일.
       ZFPONC          LIKE   ZTIDR-ZFPONC,        " 수입거래구분.
       ZFITKD          LIKE   ZTIDR-ZFITKD,        " 수입종류.
       ZFCOCD          LIKE   ZTIDR-ZFCOCD,        " 관세징수형태.
       ZFHBLNO         LIKE   ZTIDR-ZFHBLNO,       " House B/L No.
       ZFGOMNO         LIKE   ZTIDR-ZFGOMNO,       " 화물관리번호.
       ZFCUT           LIKE   ZTIDR-ZFCUT,         " 관세사.
       ZFBLNO          LIKE   ZTIDR-ZFBLNO,        " B/L 관리번호.
       ZFCLSEQ         LIKE   ZTIDR-ZFCLSEQ,       " 통관순번.
       ZFCUST          LIKE   ZTCUCL-ZFCUST,       " 통관상태.
       ZFDNCD          LIKE   ZTIDR-ZFDNCD,        " Download 상태.
       ZFEDICK         LIKE   ZTIDR-ZFEDICK.       " EDI Check.
DATA : END OF IT_TAB_DOWN.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES : ZTIDR,           " 수입신고.
         ZTIDRHS,         " 수입신고 란사항.
         ZTIDRHSD,        " 수입신고 규격(행)사항.
         ZTIDRHSL,        " 수입신고 요건확인.
         ZTCUCL,          " 통관.
         ZTBL,            " Bill of Ladding
         ZTIMIMGTX,       ">IMG.
         ZTIMIMG10,       " 관세사.
         ZTIMIMG06,       " 통관환율.
         ZTIMIMG08,       " 관리코드.
         ZTIMIMG00,       " 수입시스템 Basic Configuration.
         T005T,           " 국가이름.
         LFA1,            " 구매처마스터 (일반섹션).
         USR01.           " User Master.

*-----------------------------------------------------------------------
* SELECT RECORD
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
      ZFBLNO     LIKE ZTIDR-ZFBLNO,           " B/L 관리번호.
      ZFHBLNO    LIKE ZTIDR-ZFHBLNO,          " House B/L No.
      ZFCLSEQ    LIKE ZTIDR-ZFCLSEQ,          " 통관순번.
      ZFCUST     LIKE ZTCUCL-ZFCUST,          " 통관상태.
      ZFEDICK    LIKE ZTIDR-ZFEDICK,          " EDI CHECK
      ZFDOCST    LIKE ZTIDR-ZFDOCST,          " 문서상태.
      ZFDNCD     LIKE ZTIDR-ZFDNCD,           " Download 상태.
END OF IT_SELECTED.
*------ EDI
DATA : W_OK_CODE    LIKE   SY-UCOMM,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF.
TABLES: ZTIMIMG03.
DATA  W_EDI_RECORD(65535).
DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDIFILE.
DATA: BEGIN OF IT_EDIFILE_ITEM1 OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDIFILE_ITEM1.
DATA: BEGIN OF IT_EDIFILE_ITEM2 OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDIFILE_ITEM2.
DATA: BEGIN OF IT_EDIFILE_ITEM3 OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
END OF IT_EDIFILE_ITEM3.
*-----------------------------------------------------------------------
FIELD-SYMBOLS : <FS_F>.
*-----------------------------------------------------------------------
DATA : W_TEXT_AMOUNT(18) TYPE C,
       W_ZFCONO          LIKE ZTIDRHS-ZFCONO,
       W_ZFRONO          LIKE ZTIDRHSD-ZFRONO,
       W_ZFAPLDT         LIKE ZTIMIMG06-ZFAPLDT,
       L_TEXT            LIKE DD07T-DDTEXT.
*-----------------------------------------------------------------------
DATA : W_ERR_CHK(1)      TYPE C,
       W_SELECTED_LINES  TYPE P,             " 선택 LINE COUNT
       W_PAGE            TYPE I,             " Page Counter
       W_LINE            TYPE I,             " 페이지당 LINE COUNT
       LINE(3)           TYPE N,             " 페이지당 LINE COUNT
       W_COUNT           TYPE I,             " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드명.
       W_TABIX           LIKE SY-TABIX,      " TABLE INDEX
       W_UPDATE_CNT      TYPE I,
       W_BUTTON_ANSWER   TYPE C.

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include.

INCLUDE   ZRIMUTIL01.     " Utility function Module.

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                          " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS  FOR ZTIDR-BUKRS.
   SELECT-OPTIONS: S_INRC   FOR ZTIDR-ZFINRC,     " 신고지 세관.
                   S_INRCD  FOR ZTIDR-ZFINRCD,    " 세관의 담당과.
                   S_EBELN  FOR ZTIDR-ZFREBELN,   " 구매문서.
                   S_IDRNO  FOR ZTIDR-ZFIDRNO,    " 수입신고번호.
                   S_IDWDT  FOR ZTIDR-ZFIDWDT,    " 신고희망일.
                   S_PONC   FOR ZTIDR-ZFPONC,     " 수입거래구분.
                   S_ITKD   FOR ZTIDR-ZFITKD,     " 수입종류.
                   S_BNARCD FOR ZTIDR-ZFBNARCD,   " 보세구역 내부코드.
                   S_COCD   FOR ZTIDR-ZFCOCD,     " 관세징수형태.
                   S_HBLNO  FOR ZTIDR-ZFHBLNO,    " House B/L No
                   S_GOMNO  FOR ZTIDR-ZFGOMNO.    " 화물관리번호.
*                   S_CUT    FOR ZTIDR-ZFCUT.      " 관세사.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS : P_DNN      AS CHECKBOX.
   PARAMETERS : P_DNY      AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2. " Document Status(Download 상태)

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
   PARAMETERS : P_OK       AS CHECKBOX.
   PARAMETERS : P_NOTOK    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3. " EDI Check Bit

*SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
*   PARAMETERS : P_CU2      AS CHECKBOX. " 의뢰대상.
*   PARAMETERS : P_CU3      AS CHECKBOX. " 의뢰중(User Confirm).
*SELECTION-SCREEN END OF BLOCK B4. " 통관상?

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION.
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   W_OK_CODE = SY-UCOMM.
   CASE SY-UCOMM.
* SORT 선택.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택.
         W_FIELD_NM = 'ZFOPBN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
* 전체 선택 및 선택해제.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP'.          " 수입신고 조회.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_IDR USING IT_SELECTED-ZFBLNO
                                            IT_SELECTED-ZFCLSEQ.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE S965.
         ENDIF.
         IF W_SELECTED_LINES EQ 0.
            MESSAGE S766.
            EXIT.
         ENDIF.
      WHEN 'DSBL'.                    " Bill of Lading 조회.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_BL USING IT_SELECTED-ZFBLNO.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE S965.
            ENDIF.
            IF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
      WHEN 'FRGS' OR 'FRGR'.     " EDI FILE CREATE / EDI FILE 취소.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM P2000_POPUP_MESSAGE.     " 메세지 박스.
            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경우.
               PERFORM P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
               LEAVE TO SCREEN 0.
            ENDIF.
         ELSE.
            MESSAGE S951.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_CREATE_DOWNLOAD_FILE.
            PERFORM P3000_TO_PC_DOWNLOAD.
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

*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  SET  TITLEBAR 'ZIME02'.           " GUI TITLE SETTING..

*  P_CU3 = 'X'.
  P_OK  = 'X'.
  P_DNN = 'X'.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S025.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /45  '[ 수입신고 EDI 송신 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.    ", 115 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ', SY-VLINE     NO-GAP,
          (20) '신고지 세관  '           NO-GAP, SY-VLINE NO-GAP,
          (10) '대표 P/O 번호'          NO-GAP, SY-VLINE NO-GAP,
          (20) '수입거래구분'          NO-GAP, SY-VLINE NO-GAP,
          (20) '보세구역 '             NO-GAP, SY-VLINE NO-GAP,
          (20) '수입신고형태 '         NO-GAP, SY-VLINE NO-GAP,
          (10) '통관/D/E'              NO-GAP, SY-VLINE NO-GAP,
          (20) '관세사'          NO-GAP, SY-VLINE NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE    NO-GAP,
          (20) '세관의 담당과'         NO-GAP, SY-VLINE NO-GAP,
          (10) '신고희망일'            NO-GAP, SY-VLINE NO-GAP,
          (20) '수입종류'              NO-GAP, SY-VLINE NO-GAP,
          (20) '관세 징수형태'         NO-GAP, SY-VLINE NO-GAP,
          (20) '화물관리번호'          NO-GAP, SY-VLINE NO-GAP,
          (31) '대표품명'              NO-GAP, SY-VLINE NO-GAP.

  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.


   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIME02'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIME02'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
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

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,SY-VLINE NO-GAP,
          (20)IT_TAB-INRC NO-GAP, SY-VLINE NO-GAP,     " 신고지 세관  '
          (10)IT_TAB-ZFREBELN NO-GAP, SY-VLINE NO-GAP, " 대표 P/O 번호'
          (20)IT_TAB-PONC   NO-GAP, SY-VLINE NO-GAP,   " 수입거래구분'
          (20)IT_TAB-ZFBNARM  NO-GAP, SY-VLINE NO-GAP," 보세구역 '
          (20)IT_TAB-RPTTY NO-GAP, SY-VLINE NO-GAP, " 수입신고형태 '
          (02)IT_TAB-ZFCUST NO-GAP,(02)'/' NO-GAP, " 통관상태.
          (02)IT_TAB-ZFDNCD NO-GAP,(02)'/' NO-GAP,    " Download 상태.
          (02)IT_TAB-ZFEDICK NO-GAP, SY-VLINE NO-GAP, " EDI Check.
          (20)IT_TAB-NAME1    NO-GAP, SY-VLINE NO-GAP. " 관세사',

  HIDE:  IT_TAB.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ', SY-VLINE    NO-GAP,
          (20) IT_TAB-INRCD NO-GAP, SY-VLINE NO-GAP,   " 세관의 담당과'
          (10) IT_TAB-ZFIDWDT NO-GAP, SY-VLINE NO-GAP, " 신고희망일'
          (20)IT_TAB-ITKD     NO-GAP, SY-VLINE NO-GAP, " 수입종류'
          (20)IT_TAB-COCD    NO-GAP, SY-VLINE NO-GAP,  " 관세 징수형태'
          (20)IT_TAB-ZFGOMNO NO-GAP, SY-VLINE NO-GAP,  " 화물관리번호'
          (31)IT_TAB-ZFRGDSR  NO-GAP, SY-VLINE NO-GAP. " 대표품명'

* Stored value...
  HIDE:  IT_TAB.
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
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  RANGES : " R_ZFCUST  FOR   ZTIDR-ZFCUST  OCCURS 5,
           R_EDICK   FOR   ZTIDR-ZFEDICK OCCURS 5,
           R_DOCST   FOR   ZTIDR-ZFDOCST OCCURS 5.

  MOVE 'N' TO W_ERR_CHK.

  IF P_DNN = ' ' AND P_DNY = ' '.
     MOVE 'Y' TO W_ERR_CHK.
     MESSAGE S351.
     EXIT.
  ENDIF.
  IF P_OK = ' ' AND P_NOTOK = ' '.
     MOVE 'Y' TO W_ERR_CHK.
     MESSAGE S352.
     EXIT.
  ENDIF.
*  IF P_CU2 = ' ' AND P_CU3 = ' '.
*     MESSAGE S351.
*     EXIT.
*  ENDIF.
*-----------------------------------------------------------------------
* EDI CREATE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_DNN  EQ 'X'.
     MOVE: 'I'      TO R_DOCST-SIGN,
           'EQ'     TO R_DOCST-OPTION,
           'N'      TO R_DOCST-LOW.
     APPEND R_DOCST.
  ENDIF.
*-----------------------------------------------------------------------
* EDI CREATE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_DNY  EQ 'X'.
     MOVE: 'I'      TO R_DOCST-SIGN,
           'EQ'     TO R_DOCST-OPTION,
           'R'      TO R_DOCST-LOW.
     APPEND R_DOCST.
  ENDIF.
*-----------------------------------------------------------------------
* EDI CHECK BIT  SETTING
*-----------------------------------------------------------------------
  IF P_OK EQ 'X'.
     MOVE: 'I'      TO R_EDICK-SIGN,
           'EQ'     TO R_EDICK-OPTION,
           'O'      TO R_EDICK-LOW.
     APPEND R_EDICK.
  ENDIF.
*-----------------------------------------------------------------------
* EDI CHECK BIT  SETTING
*-----------------------------------------------------------------------
  IF P_NOTOK EQ 'X'.
     MOVE: 'I'      TO R_EDICK-SIGN,
           'EQ'     TO R_EDICK-OPTION,
           'X'      TO R_EDICK-LOW.
     APPEND R_EDICK.
  ENDIF.


  REFRESH IT_TAB.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZTIDR
           WHERE BUKRS      IN S_BUKRS
           AND   ZFINRC     IN S_INRC
           AND   ZFINRCD    IN S_INRCD
           AND   ZFIDRNO    IN S_IDRNO
           AND   ZFIDWDT    IN S_IDWDT
           AND   ZFPONC     IN S_PONC
           AND   ZFITKD     IN S_ITKD
           AND   ZFBNARCD   IN S_BNARCD
           AND   ZFCOCD     IN S_COCD
           AND   ZFHBLNO    IN S_HBLNO
           AND   ZFGOMNO    IN S_GOMNO
           AND   ZFREBELN   IN S_EBELN
*           AND   ZFCUT      IN S_CUT
           AND   ZFDOCST    IN R_DOCST
           AND   ZFEDICK    IN R_EDICK.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
*>> 대표 PO SELECT.
     SELECT SINGLE *
            FROM   ZTBL
            WHERE ZFBLNO    EQ   IT_TAB-ZFBLNO.
     IF SY-SUBRC EQ 0.
        MOVE: ZTBL-ZFREBELN TO IT_TAB-ZFREBELN,
              ZTBL-ZFRPTTY  TO IT_TAB-ZFRPTTY,
              ZTBL-ZFRGDSR  TO IT_TAB-ZFRGDSR.
     ENDIF.
     SELECT SINGLE ZFCUST
            INTO IT_TAB-ZFCUST
            FROM  ZTCUCL
            WHERE ZFBLNO  = IT_TAB-ZFBLNO
            AND   ZFCLSEQ = IT_TAB-ZFCLSEQ.
     IF IT_TAB-ZFCUST EQ 'Y' OR IT_TAB-ZFCUST EQ 'N'.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
     ENDIF.
*>> 세관.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' IT_TAB-ZFINRC
                               CHANGING   IT_TAB-INRC.
*>> 담당과부호.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDINRCO'
                                          IT_TAB-ZFINRCD
                                        CHANGING IT_TAB-INRCD.
*>> 수입신고형태.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDRPTTY'
                                          IT_TAB-ZFRPTTY
                                        CHANGING IT_TAB-RPTTY.
*>> 관세징수형태.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOCD'
                                          IT_TAB-ZFCOCD
                                        CHANGING IT_TAB-COCD.
*>> 수입신고종류.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDITKD'
                                          IT_TAB-ZFITKD
                                        CHANGING IT_TAB-ITKD.

*>> 관세사.
     CLEAR ZTIMIMG10.
     SELECT SINGLE *
      FROM ZTIMIMG10
     WHERE ZFCUT = IT_TAB-ZFCUT.

     SELECT SINGLE *
       FROM LFA1
      WHERE LIFNR = ZTIMIMG10-ZFVEN.
     IF SY-SUBRC EQ 0.
         MOVE  LFA1-NAME1 TO IT_TAB-NAME1.
     ENDIF.
*>> 수입거래구분.
     PERFORM  GET_ZTIIMIMG08_SELECT USING '001' IT_TAB-ZFPONC
                                  CHANGING   IT_TAB-PONC.
**>> 보세구역.
     CLEAR ZTIMIMG03.
     SELECT SINGLE *
       FROM ZTIMIMG03
      WHERE  ZFBNARCD = IT_TAB-ZFBNARCD.
     IF SY-SUBRC EQ 0.
         IT_TAB-ZFBNARM = ZTIMIMG03-ZFBNARM.
     ENDIF.
     MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     MOVE 'Y' TO W_ERR_CHK.
     MESSAGE S738.
  ENDIF.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFBLNO  LIKE ZTIDR-ZFBLNO,
        ZFHBLNO LIKE ZTIDR-ZFHBLNO,
        ZFCLSEQ LIKE ZTIDR-ZFCLSEQ,
        ZFCUST  LIKE ZTCUCL-ZFCUST,
        ZFEDICK LIKE ZTIDR-ZFEDICK,
        ZFDNCD  LIKE ZTIDR-ZFDNCD.

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFHBLNO  TO ZFHBLNO,
         IT_TAB-ZFCLSEQ  TO ZFCLSEQ,
         IT_TAB-ZFCUST   TO ZFCUST,
         IT_TAB-ZFEDICK  TO ZFEDICK,
         IT_TAB-ZFDNCD   TO ZFDNCD.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
                IT_TAB-ZFHBLNO  TO IT_SELECTED-ZFHBLNO,
                IT_TAB-ZFCLSEQ  TO IT_SELECTED-ZFCLSEQ,
                IT_TAB-ZFCUST   TO IT_SELECTED-ZFCUST,
                IT_TAB-ZFEDICK  TO IT_SELECTED-ZFEDICK,
                IT_TAB-ZFDOCST  TO IT_SELECTED-ZFDOCST,
                IT_TAB-ZFDNCD   TO IT_SELECTED-ZFDNCD.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

  REFRESH IT_TAB_DOWN.
  LOOP AT IT_TAB.
    CLEAR IT_TAB_DOWN.
    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
    APPEND IT_TAB_DOWN.
  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.

DATA : TEXT100(100) TYPE  C.
   IF W_OK_CODE EQ 'FRGS'.
      TEXT100 = 'EDI FILE CREATE 작업을 계속 진행하시겠습니까?'.
   ELSEIF W_OK_CODE EQ 'FRGR'.
      TEXT100 = 'EDI FILE CANCLE 작업을 계속 진행하시겠습니까?'.
   ENDIF.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = 'EDI FILE CREATE/CANCLE 확인'
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = TEXT100
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE USING    W_GUBUN.

DATA : L_COUNT     TYPE I,
       L_RETURN    LIKE SY-SUBRC,
       W_EDI_CNT   TYPE I.

   REFRESH : IT_EDIFILE,       IT_EDIFILE_ITEM1,
             IT_EDIFILE_ITEM2, IT_EDIFILE_ITEM3.

   CLEAR : W_EDI_CNT, L_COUNT.

   SORT IT_SELECTED BY ZFBLNO ZFCLSEQ.
   LOOP AT IT_SELECTED.
      W_TABIX = SY-TABIX.
*>>> 진행상태바..
      line = ( sy-tabix / w_selected_lines ) * 100.
      out_text = 'JOB PROGRESS %99999%%'.
      replace '%99999%' with line into out_text.
      perform p2000_show_bar using out_text line.
*>> EDI BIT CHECK
      IF W_GUBUN EQ 'FRGS'.      " EDI CREATE
         IF IT_SELECTED-ZFEDICK EQ 'X'.
            MESSAGE I119 WITH IT_SELECTED-ZFHBLNO IT_SELECTED-ZFCLSEQ.
            CONTINUE.
         ENDIF.
         IF IT_SELECTED-ZFCUST NE '3'.
            MESSAGE I653 .
            CONTINUE.
         ENDIF.
         IF IT_SELECTED-ZFDOCST NE 'N'.
            MESSAGE I104 WITH IT_SELECTED-ZFHBLNO IT_SELECTED-ZFCLSEQ
                              IT_SELECTED-ZFDOCST.
            CONTINUE.
        ENDIF.
      ENDIF.

*      CLEAR  ZTIDR.
*     SELECT SINGLE *
*       FROM ZTIDR
*      WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
*        AND ZFCLSEQ EQ IT_SELECTED-ZFCLSEQ.
*     IF ZTIDR-ZFCUT IS INITIAL.
*        MESSAGE I360 WITH IT_SELECTED-ZFHBLNO IT_SELECTED-ZFCLSEQ.
*        CONTINUE.
*     ENDIF.

*      CLEAR  ZTIMIMG10.
*     SELECT SINGLE *
*       FROM ZTIMIMG10
*      WHERE ZFCUT EQ ZTIDR-ZFCUT.
*     IF ZTIMIMG10-ZFVEN IS INITIAL.
*        MESSAGE I361 WITH IT_SELECTED-ZFHBLNO IT_SELECTED-ZFCLSEQ.
*        CONTINUE.
*     ENDIF.

*>> 수입신고 자료 SELECT!
      CLEAR ZTIDR.
      SELECT SINGLE * FROM ZTIDR WHERE ZFBLNO  EQ  IT_SELECTED-ZFBLNO
                                 AND   ZFCLSEQ EQ  IT_SELECTED-ZFCLSEQ.

*>> 관세사의 거래처 번호 SELECT!
      CLEAR : ZTIMIMG10, LFA1.
      SELECT SINGLE * FROM ZTIMIMG10 WHERE ZFCUT EQ ZTIDR-ZFCUT.

*>> 관세사의 구매처 정보 SELECT!
      SELECT SINGLE * FROM LFA1  WHERE  LIFNR  EQ  ZTIMIMG10-ZFVEN.

      IF LFA1-BAHNS IS INITIAL.
         MESSAGE I274 WITH ZTIMIMG10-ZFVEN.
         CONTINUE.
      ENDIF.

*      CLEAR  LFA1.
*      SELECT SINGLE *
*        FROM LFA1
*       WHERE LIFNR  EQ ZTIMIMG10-ZFVEN.
*      IF LFA1-BAHNS IS INITIAL.
*         MESSAGE I274 WITH ZTIMIMG10-ZFVEN.
*         CONTINUE.
*      ENDIF.

* LOCK CHECK
      PERFORM   P2000_LOCK_MODE_SET  USING    'L'
                                              IT_SELECTED-ZFBLNO
                                              IT_SELECTED-ZFCLSEQ
                                              L_RETURN.
      CHECK L_RETURN EQ 0.

      IF W_GUBUN EQ 'FRGS'.      " EDI CREATE
         PERFORM   P3000_FILE_CREATE.
*>>> LG-EDS VAN. SAM-FILE WRITE FUNCTION
         CALL  FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
               EXPORTING
                      ZFCDDOC       = W_ZFCDDOC
                      BUKRS         = ZTIDR-BUKRS
               TABLES
                      EDIFILE       = IT_EDIFILE.
         REFRESH : IT_EDIFILE.
      ELSE.
         CALL  FUNCTION 'ZIM_EDI_SAMFILE_DELETE'
               EXPORTING
                      ZFDHENO       = ZTIDR-ZFDOCNO.
      ENDIF.

      ADD 1   TO    L_COUNT.      "---> 마지막을 알기.

      MOVE : SY-UNAME      TO    ZTIDR-UNAM,
             SY-DATUM      TO    ZTIDR-UDAT.
      IF W_GUBUN EQ 'FRGS'.      " EDI CREATE
         MOVE : 'R'          TO    ZTIDR-ZFDOCST,
                'S'          TO    ZTIDR-ZFEDIST,
                W_ZFDHENO    TO    ZTIDR-ZFDOCNO,
*                W_ZFDHENO    TO    ZTIDR-ZFIMCR,
                W_ZFDHENO+7  TO    ZTIDR-ZFIDRNO.
      ENDIF.
      IF W_GUBUN EQ 'FRGR'.      " EDI CANCLE
         MOVE : 'N'        TO    ZTIDR-ZFDNCD,
                'N'        TO    ZTIDR-ZFDOCST,
                'N'        TO    ZTIDR-ZFEDIST.
      ENDIF.

      UPDATE ZTIDR.

*>>> UNLOCK SETTTING.
     PERFORM   P2000_LOCK_MODE_SET  USING    'U'
                                              IT_SELECTED-ZFBLNO
                                              IT_SELECTED-ZFCLSEQ
                                              L_RETURN.

   ENDLOOP.

ENDFORM.                    " P3000_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_CREATE
*&---------------------------------------------------------------------*
FORM P3000_FILE_CREATE.

   W_ZFCDDOC = 'IMPREQ'.

   W_ZFDHSRO = LFA1-BAHNS.          " 식별자.
   W_ZFDHREF = ZTIDR-ZFBLNO.        " 참조번호.
   MOVE '-'             TO W_ZFDHREF+10(1).
   MOVE ZTIDR-ZFCLSEQ   TO W_ZFDHREF+11(5).
*   W_ZFDHDDB = ZTIDR-UNAM.          " 부서.
   W_ZFDHENO = ZTIDR-ZFDOCNO.       " 문서번호.

   CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
        EXPORTING
             W_ZFCDDOC = W_ZFCDDOC
             W_ZFDHSRO = W_ZFDHSRO
             W_ZFDHREF = W_ZFDHREF
             W_BUKRS     = ZTIDR-BUKRS
        CHANGING
             W_ZFDHENO = W_ZFDHENO
        EXCEPTIONS
             DB_ERROR  = 4
             NO_TYPE   = 8.

   CASE SY-SUBRC.
        WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
        WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
   ENDCASE.

*>> EDI FLAT FILE CREATE FUNCTION CALL
   CALL  FUNCTION 'ZIM_LG_IMPREQ_EDI_DOC'
         EXPORTING
            W_ZFBLNO      = ZTIDR-ZFBLNO
            W_ZFCLSEQ     = ZTIDR-ZFCLSEQ
            W_ZFDHENO     = W_ZFDHENO
            W_BAHNS       = LFA1-BAHNS
         IMPORTING
            W_EDI_RECORD  = W_EDI_RECORD
         EXCEPTIONS
            CREATE_ERROR      = 4.

   CASE SY-SUBRC.
      WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
      WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
   ENDCASE.

*>>> INTERNAL TABLE WRITE....
   IT_EDIFILE-W_RECORD = W_EDI_RECORD.
   APPEND IT_EDIFILE.

ENDFORM.                    " P3000_FILE_CREATE

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_EDI_IMPREQ
*&---------------------------------------------------------------------*
FORM P4000_CREATE_EDI_IMPREQ.

  CLEAR : IT_EDIFILE, W_EDI_RECORD.
* 관리번호(01).
  MOVE : W_ZFDHENO                             TO W_EDI_RECORD.
* 기능코드(02).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 기능의미(03).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 응답유형코드(04).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 응답유형의미(10).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 거래구분코드(06).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFPONC    INTO W_EDI_RECORD.
* 거래구분내용(07).
  CLEAR  ZTIMIMG08.
  SELECT SINGLE *
    FROM ZTIMIMG08
   WHERE ZFCDTY = '001'
     AND ZFCD = ZTIDR-ZFPONC.
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIMIMG08-ZFCDNM.
  CONCATENATE W_EDI_RECORD '^' ZTIMIMG08-ZFCDNM INTO W_EDI_RECORD.
* 수입종류코드(08).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFITKD    INTO W_EDI_RECORD.
* 수입종류내용(09).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDITKD'
                                               ZTIDR-ZFITKD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 신고의뢰일(10).
  CONCATENATE W_EDI_RECORD '^' SY-DATUM        INTO W_EDI_RECORD.
* 신고희망일(11).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFIDWDT   INTO W_EDI_RECORD.
* 입항일(12).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFENDT    INTO W_EDI_RECORD.
* 반입일(13).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFINDT    INTO W_EDI_RECORD.
* 분할수입여부(14).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 수입자구분(15).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFIMCD    INTO W_EDI_RECORD.
* 수입자내용(16).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDIMCD'
                                               ZTIDR-ZFIMCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 신고구분(17).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFIDRCD   INTO W_EDI_RECORD.
* 신고구분내용(18).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDIDRCD'
                                               ZTIDR-ZFIDRCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 결제방법(19).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFAMCD    INTO W_EDI_RECORD.
* 결제방법내용(20).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDAMCD'
                                               ZTIDR-ZFAMCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 징수형태(21).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFCOCD    INTO W_EDI_RECORD.
* 징수형태내용(22).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDCOCD'
                                               ZTIDR-ZFCOCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 통관계획(23).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFCUPR    INTO W_EDI_RECORD.
* 통관계획내용(24).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDCUPR'
                                               ZTIDR-ZFCUPR
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 신고세관코드(25).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFINRC    INTO W_EDI_RECORD.
* 신고세관내용(26).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDCOTM'
                                               ZTIDR-ZFINRC
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 신고과코드(27).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFINRCD   INTO W_EDI_RECORD.
* 신고과내용(28).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDINRCO'
                                               ZTIDR-ZFINRCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 도착항코드(29).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFAPRTC   INTO W_EDI_RECORD.
* 도착항내용(30).
  CLEAR  ZTIMIMG08.
*  SELECT SINGLE *
*    FROM ZTIMIMG08
*   WHERE ZFCDTY = '002'
*     AND ZFCD = ZTIDR-ZFAPRTC.
*  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIMIMG08-ZFCDNM.
  SELECT SINGLE PORTT INTO ZTIMIMG08-ZFCDNM
         FROM ZTIEPORT
         WHERE    LAND1 EQ 'KR'
         AND      PORT  EQ ZTIDR-ZFAPRTC.

  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIMIMG08-ZFCDNM.
  CONCATENATE W_EDI_RECORD '^' ZTIMIMG08-ZFCDNM INTO W_EDI_RECORD.
* 적출국코드(31).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFSCON    INTO W_EDI_RECORD.
* 적출국내용(32).
  CLEAR  T005T.
  SELECT SINGLE *
    FROM T005T
   WHERE SPRAS = SY-LANGU
     AND LAND1 = ZTIDR-ZFSCON.
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING T005T-LANDX.
  CONCATENATE W_EDI_RECORD '^' T005T-LANDX     INTO W_EDI_RECORD.
* 검사장소코드(33).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFISPL    INTO W_EDI_RECORD.
* 검사장소내용(34).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 운송용기코드(35).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTRCN    INTO W_EDI_RECORD.
* 운송용기내용(36).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDTRCN'
                                               ZTIDR-ZFTRCN
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* B/L 번호(37).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFHBLNO.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFHBLNO   INTO W_EDI_RECORD.
* 화물관리번호(38).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFGOMNO   INTO W_EDI_RECORD.
* 예비란(39).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 타요건부호(40).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* Master B/L(41).
  CLEAR  ZTBL.
  SELECT SINGLE *
    FROM ZTBL
   WHERE ZFBLNO = ZTIDR-ZFBLNO.
  CONCATENATE W_EDI_RECORD '^' ZTBL-ZFMBLNO    INTO W_EDI_RECORD.
* 참조번호(42).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 운송수단코드(43).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTRMET   INTO W_EDI_RECORD.
* 운송수단내용(44).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDTRMET'
                                               ZTIDR-ZFTRMET
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 운송선명(45).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFCARNM   INTO W_EDI_RECORD.

* 선박국적코드(46).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFCAC     INTO W_EDI_RECORD.
* 선박국적내용(47).
  CLEAR  T005T.
  SELECT SINGLE *
    FROM T005T
   WHERE SPRAS = SY-LANGU
     AND LAND1 = ZTIDR-ZFCAC.
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING T005T-LANDX.
  CONCATENATE W_EDI_RECORD '^' T005T-LANDX     INTO W_EDI_RECORD.
* 의뢰상호(48).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDNM1.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDNM1   INTO W_EDI_RECORD.
* 수입무역번호(49).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFAPNO    INTO W_EDI_RECORD.
* 수입상호(50).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFIAPNM.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFIAPNM   INTO W_EDI_RECORD.
* 납세통관부호(51).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDNO.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDNO    INTO W_EDI_RECORD.
* 납세상호(52).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDNM1.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDNM1   INTO W_EDI_RECORD.
* 납세성명(53).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDNM2.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDNM2   INTO W_EDI_RECORD.
* 납세주소1(54).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDAD1.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDAD1   INTO W_EDI_RECORD.
* 납세주소2(55).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDAD2.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDAD2   INTO W_EDI_RECORD.
* 납세우편번호(56).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 납세사업구분(57)
  CONCATENATE W_EDI_RECORD '^' 'AHP'           INTO W_EDI_RECORD.
* 납세사업번호(58)
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTDTC.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTDTC    INTO W_EDI_RECORD.
* 대리점부호(59)
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTRDNO.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTRDNO   INTO W_EDI_RECORD.
* 대리점상호(60)
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFTRDNM.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFTRDNM   INTO W_EDI_RECORD.
* 공급자부호(61)
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFSUPNO.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFSUPNO   INTO W_EDI_RECORD.
* 공급자상호1(62)
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDR-ZFSUPNM.
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFSUPNM   INTO W_EDI_RECORD.
* 공급자상호2(63)
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 공급국적코드(64)
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFSUPC    INTO W_EDI_RECORD.
* 공급국적내용(65)
  CLEAR  T005T.
  SELECT SINGLE *
    FROM T005T
   WHERE SPRAS = SY-LANGU
     AND LAND1 = ZTIDR-ZFSUPC.
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING T005T-LANDX.
  CONCATENATE W_EDI_RECORD '^' T005T-LANDX     INTO W_EDI_RECORD.
* 운수기관부호(66)
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 운수기관내용(67)
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 특송업체부호(68)
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFSTRCD   INTO W_EDI_RECORD.
* 특송업체내용(69)
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 인도조건코드(70)
  CONCATENATE W_EDI_RECORD '^' ZTIDR-INCO1     INTO W_EDI_RECORD.
* 인도조건내용(71)
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 결제금액(72)
  WRITE   ZTIDR-ZFSTAMT      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFSTAMC.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 결제금액통화(73)
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFSTAMC   INTO W_EDI_RECORD.
* 환율(74)
  MOVE    ZTIMIMG06-ZFEXRT   TO      L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 과세원화(75)
  WRITE   ZTIDR-ZFTBAK       TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 과세미화(76)
  WRITE   ZTIDR-ZFTBAU        TO     W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFUSD.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 운임(77)
  WRITE   ZTIDR-ZFTRT         TO     W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 보험료(78)
  WRITE   ZTIDR-ZFINAMT       TO     W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 가산금액(79)
  WRITE   ZTIDR-ZFADAMK       TO     W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 공제금액(80)
  WRITE   ZTIDR-ZFDUAMK       TO     W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총중량(81)
  WRITE   ZTIDR-ZFTOWT        TO     W_TEXT_AMOUNT
                            UNIT     ZTIDR-ZFTOWTM.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총포장개수(82).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFPKCNT   INTO W_EDI_RECORD.
* 총포장종류(83).
  CONCATENATE W_EDI_RECORD '^' ZTIDR-ZFPKNM    INTO W_EDI_RECORD.
* 총란수(84).
  CLEAR W_ZFCONO.
  SELECT MAX( ZFCONO ) INTO W_ZFCONO
    FROM ZTIDRHS
   WHERE ZFBLNO  = ZTIDR-ZFBLNO
     AND ZFCLSEQ = ZTIDR-ZFCLSEQ.
  CONCATENATE W_EDI_RECORD '^' W_ZFCONO        INTO W_EDI_RECORD.
* 총관세(85).
  WRITE   ZTIDR-ZFCUAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총특소세(86).
  WRITE   ZTIDR-ZFSCAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총교통세(87).
  WRITE   ZTIDR-ZFTRAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총주세(88).
  WRITE   ZTIDR-ZFDRAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총교육세(89).
  WRITE   ZTIDR-ZFEDAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총농특세(90).
  WRITE   ZTIDR-ZFAGAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 부가세표합계(91).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 총부가세(92).
  WRITE   ZTIDR-ZFVAAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 면세표합계(93).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 가산세액(94).
  WRITE   ZTIDR-ZFIDAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 총세액(95).
  WRITE   ZTIDR-ZFTXAMTS      TO      W_TEXT_AMOUNT
                        CURRENCY     ZTIDR-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 오류(96).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 송신(97).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 수신처(98).
  CLEAR : ZTIMIMG10, LFA1.
  SELECT SINGLE *
    FROM ZTIMIMG10
   WHERE ZFCUT = ZTIDR-ZFCUT.
  SELECT SINGLE *
    FROM LFA1
   WHERE LIFNR = ZTIMIMG10-ZFVEN.
  CONCATENATE W_EDI_RECORD '^' LFA1-BAHNS      INTO W_EDI_RECORD.
* 상태(99).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 입력일(100).
  CONCATENATE W_EDI_RECORD '^' SY-DATUM        INTO W_EDI_RECORD.

  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

ENDFORM.                    " P4000_CREATE_EDI_IMPREQ

*&---------------------------------------------------------------------*
*&      Form  P2000_SPACE_AND_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
FORM P2000_SPACE_AND_CHANGE_SYMBOL CHANGING P_TEXT.

   ASSIGN  P_TEXT      TO    <FS_F>.

   PERFORM P2000_SPACE_CUT    USING <FS_F>.   " 첫 SPACE 제거.
* 특수문자 제거(==> Space로 대체)
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '~' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '`' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '_' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '@' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '#' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '$' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '|' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '\' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '[' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> ']' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '{' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '}' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '^' ' '.
*SKC
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '!' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> '?' ' '.
   PERFORM P2000_CHANGE_SYMBOL    USING <FS_F> ':' ' '.

ENDFORM.                    " P2000_SPACE_AND_CHANGE_SYMBOL

*&---------------------------------------------------------------------*
*&      Form  P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
FORM P2000_CHANGE_SYMBOL USING    P_AMOUNT  P_FROM  P_TO.

  DO.
     REPLACE  P_FROM   WITH   P_TO  INTO    P_AMOUNT.
        IF  SY-SUBRC  <>    0.
            EXIT.
        ENDIF.
  ENDDO.

ENDFORM.                    " P2000_CHANGE_SYMBOL

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DD07T_TEXT
*&---------------------------------------------------------------------*
FORM P1000_GET_DD07T_TEXT USING    P_DOMNAME
                                   P_FIELD
                          CHANGING P_W_NAME.

   CLEAR : DD07T, P_W_NAME.

   SELECT * FROM DD07T UP TO 1 ROWS
                       WHERE DOMNAME     EQ P_DOMNAME
                       AND   DDLANGUAGE  EQ SY-LANGU
                       AND   AS4LOCAL    EQ 'A'
                       AND   DOMVALUE_L  EQ P_FIELD
                       ORDER BY AS4VERS DESCENDING.
   ENDSELECT.

   P_W_NAME   = DD07T-DDTEXT.

*   TRANSLATE P_W_NAME TO UPPER CASE.

   PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING P_W_NAME.

ENDFORM.                    " P1000_GET_DD07T_TEXT

*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_NO_MASK
*&---------------------------------------------------------------------*
FORM P2000_WRITE_NO_MASK CHANGING P_TEXT_AMOUNT.

  SELECT SINGLE * FROM USR01 WHERE BNAME EQ SY-UNAME.

  CASE USR01-DCPFM.
     WHEN 'X'.    " Decimal point is period: N,NNN.NN
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT ',' ' '.
        CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
     WHEN 'Y'.    " Decimal point is N NNN NNN,NN
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
        CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
     WHEN OTHERS. " Decimal point is comma: N.NNN,NN
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  '.' ' '.
        PERFORM    P2000_CHANGE_SYMBOL    USING P_TEXT_AMOUNT  ',' '.'.
        CONDENSE         P_TEXT_AMOUNT    NO-GAPS.
  ENDCASE.

ENDFORM.                    " P2000_WRITE_NO_MASK

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_EDI_IMPREQ1
*&---------------------------------------------------------------------*
FORM P4000_CREATE_EDI_IMPREQ1.

  CLEAR : IT_EDIFILE_ITEM1, W_EDI_RECORD.
* 관리번호(01).
  MOVE : W_ZFDHENO                             TO W_EDI_RECORD.
* 란번호(02).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFCONO  INTO W_EDI_RECORD.
* PCODE(03).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* HS부호(04).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHS-STAWN.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-STAWN   INTO W_EDI_RECORD.
* 품명(10).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHS-ZFGDNM.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFGDNM  INTO W_EDI_RECORD.
* 거래품명(06).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHS-ZFTGDNM.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFTGDNM INTO W_EDI_RECORD.
* 상표코드(07).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHS-ZFGCCD.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFGCCD  INTO W_EDI_RECORD.
* 상표명(08).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHS-ZFGCNM.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFGCNM  INTO W_EDI_RECORD.
* 예비란(09).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 원산지코드(10).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFORIG  INTO W_EDI_RECORD.
* 원산지내용(11).
  CLEAR  T005T.
  SELECT SINGLE *
    FROM T005T
   WHERE SPRAS = SY-LANGU
     AND LAND1 = ZTIDRHS-ZFORIG.
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING T005T-LANDX.
  CONCATENATE W_EDI_RECORD '^' T005T-LANDX     INTO W_EDI_RECORD.
* 사후기관1(12).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFMOR1  INTO W_EDI_RECORD.
* 사후기관명1(13).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 사후기관2(14).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFMOR2  INTO W_EDI_RECORD.
* 사후기관명2(15).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 사후기관3(16).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFMOR3  INTO W_EDI_RECORD.
* 사후기관명3(17).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 중량(18).
  WRITE      ZTIDRHS-ZFWET          TO         W_TEXT_AMOUNT
                                    UNIT       ZTIDRHS-ZFWETM.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 수량(19).
  WRITE      ZTIDRHS-ZFQNT          TO         W_TEXT_AMOUNT
                                    UNIT       ZTIDRHS-ZFQNTM.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 수량단위(20).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFQNTM  INTO W_EDI_RECORD.
* 물량(21).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 물량단위(22).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 총요건서류수(23).
  CLEAR W_COUNT.
  SELECT COUNT( * ) INTO W_COUNT
    FROM ZTIDRHSL
   WHERE ZFBLNO  = ZTIDRHS-ZFBLNO
     AND ZFCLSEQ = ZTIDRHS-ZFCLSEQ
     AND ZFCONO  = ZTIDRHS-ZFCONO.
  MOVE W_COUNT TO L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 총규격수(24).
  CLEAR W_COUNT.
  SELECT COUNT( * ) INTO W_COUNT
    FROM ZTIDRHSD
   WHERE ZFBLNO  = ZTIDRHS-ZFBLNO
     AND ZFCLSEQ = ZTIDRHS-ZFCLSEQ
     AND ZFCONO  = ZTIDRHS-ZFCONO.
  MOVE W_COUNT TO L_TEXT.
  CONCATENATE W_EDI_RECORD '^' W_ZFRONO        INTO W_EDI_RECORD.
* 과세원화(25).
  WRITE      ZTIDRHS-ZFTBAK         TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 과세미화(26).
  WRITE      ZTIDRHS-ZFTBAU         TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFUSD.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 관세구분코드(27).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFTXCD  INTO W_EDI_RECORD.
* 관세구분내용(28).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDTXCD'
                                               ZTIDRHS-ZFTXCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 관세율(29).
  MOVE        ZTIDRHS-ZFCURT   TO     L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 관세당세액(30). 단위당세액 확인요.
  WRITE      ZTIDRHS-ZFTXPER        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 감면율(31).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFRDRT  INTO W_EDI_RECORD.
* 관세기준코드(32).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFTXAMCD INTO W_EDI_RECORD.
* 관세기준내용(33).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDTXAMCD'
                                               ZTIDRHS-ZFTXAMCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 관세액(34).
  WRITE      ZTIDRHS-ZFCUAMT        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 감면액(35).
  WRITE      ZTIDRHS-ZFCCAMT        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 분납구분코드(36).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFCDPCD INTO W_EDI_RECORD.
* 분납구분내용(37).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDCDPCD'
                                               ZTIDRHS-ZFCDPCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 분납부호(38).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFCDPNO INTO W_EDI_RECORD.
* 내국세구분(39).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFHMTCD INTO W_EDI_RECORD.
* 내국세구분명(40).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDHMTCD'
                                               ZTIDRHS-ZFHMTCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 내국세율(41).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFHMTRT INTO W_EDI_RECORD.
* 내국세종부호(42).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFHMTTY INTO W_EDI_RECORD.
* 내국세종내용(43).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDHMTTY'
                                               ZTIDRHS-ZFHMTTY
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 특소세부호(44).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFSCCD  INTO W_EDI_RECORD.
* 특소세내용(45).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDSCCD'
                                               ZTIDRHS-ZFSCCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 내국세액(46).
  WRITE      ZTIDRHS-ZFHMAMT        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 교육세구분(47).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFETXCD INTO W_EDI_RECORD.
* 교육세구분명(48).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDETXCD'
                                               ZTIDRHS-ZFETXCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 교육세액(49).
  WRITE      ZTIDRHS-ZFEDAMT        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 농특세구분(50).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFATXCD INTO W_EDI_RECORD.
* 농특세구분명(51).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDATXCD'
                                               ZTIDRHS-ZFATXCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 농특세액(52).
  WRITE      ZTIDRHS-ZFAGAMT        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 부가세구분(53).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHS-ZFVTXCD INTO W_EDI_RECORD.
* 부가세구분명(54).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDCTXCD'
                                               ZTIDRHS-ZFVTXCD
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 부가세과세표(55).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 부가세부호(56).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 부가세내용(57).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 부가세액(58).
  WRITE      ZTIDRHS-ZFVAAMT        TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHS-ZFKRW.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 부가세면세표(59).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 특소세근거(60).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 입력일(61).
  CONCATENATE W_EDI_RECORD '^' SY-DATUM        INTO W_EDI_RECORD.

  IT_EDIFILE_ITEM1-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE_ITEM1.

ENDFORM.                    " P4000_CREATE_EDI_IMPREQ1

*&---------------------------------------------------------------------*
*&      Form  P2000_SPACE_CUT
*&---------------------------------------------------------------------*
FORM P2000_SPACE_CUT USING    PARA_STRING.
DATA : CTEXT     TYPE C,
       CPOSITION TYPE I,
       CLEN      TYPE I.

  CPOSITION = 0.
  CLEN = STRLEN( PARA_STRING ).

  MOVE PARA_STRING+CPOSITION(1) TO CTEXT.
  WHILE CTEXT EQ ' '.
     IF CPOSITION >= CLEN.
        EXIT.
     ENDIF.
     CPOSITION = CPOSITION + 1.
     MOVE PARA_STRING+CPOSITION(1) TO CTEXT.
  ENDWHILE.
  MOVE PARA_STRING+CPOSITION TO PARA_STRING.

ENDFORM.                    " P2000_SPACE_CUT

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDR
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IDR USING    P_ZFBLNO
                             P_ZFCLSEQ.

   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPCLSEQ'  FIELD P_ZFCLSEQ.
   SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
   SET PARAMETER ID 'ZPIDRNO'  FIELD ''.

   EXPORT 'ZPBLNO'        TO   MEMORY ID 'ZPBLNO'.
   EXPORT 'ZPCLSEQ'       TO   MEMORY ID 'ZPCLSEQ'.

   CALL TRANSACTION 'ZIM63' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_IDR

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFBLNO.

   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD ''.

   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.

   CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL

*&---------------------------------------------------------------------*
*&      Form  P4000_EDI_SAMFILE_WRITE
*&---------------------------------------------------------------------*
FORM P4000_EDI_SAMFILE_WRITE.
DATA : UNIXFILE(300) TYPE C.

  CONCATENATE   ZTIMIMGTX-ZFPATH SY-UNAME 'IMPREQ' '.TXT'
                INTO   UNIXFILE.
  OPEN DATASET UNIXFILE FOR OUTPUT IN TEXT MODE.
  IF SY-SUBRC NE 0.
     MESSAGE E910 WITH UNIXFILE.
     EXIT.
  ENDIF.
  LOOP AT IT_EDIFILE.
       TRANSFER   IT_EDIFILE  TO     UNIXFILE.
  ENDLOOP.
  CLOSE DATASET    UNIXFILE.

  CONCATENATE   ZTIMIMGTX-ZFPATH SY-UNAME 'IMPREQ1' '.TXT'
                INTO   UNIXFILE.
  OPEN DATASET UNIXFILE FOR OUTPUT IN TEXT MODE.
  IF SY-SUBRC NE 0.
     MESSAGE E910 WITH UNIXFILE.
     EXIT.
  ENDIF.
  LOOP AT IT_EDIFILE_ITEM1.
       TRANSFER   IT_EDIFILE_ITEM1  TO     UNIXFILE.
  ENDLOOP.
  CLOSE DATASET    UNIXFILE.

  CONCATENATE   ZTIMIMGTX-ZFPATH SY-UNAME 'IMPREQ2' '.TXT'
                INTO   UNIXFILE.
  OPEN DATASET UNIXFILE FOR OUTPUT IN TEXT MODE.
  IF SY-SUBRC NE 0.
     MESSAGE E910 WITH UNIXFILE.
     EXIT.
  ENDIF.
  LOOP AT IT_EDIFILE_ITEM2.
       TRANSFER   IT_EDIFILE_ITEM2  TO     UNIXFILE.
  ENDLOOP.
  CLOSE DATASET    UNIXFILE.

  CONCATENATE   ZTIMIMGTX-ZFPATH SY-UNAME 'IMPREQ3' '.TXT'
                INTO   UNIXFILE.
  OPEN DATASET UNIXFILE FOR OUTPUT IN TEXT MODE.
  IF SY-SUBRC NE 0.
     MESSAGE E910 WITH UNIXFILE.
     EXIT.
  ENDIF.
  LOOP AT IT_EDIFILE_ITEM3.
       TRANSFER   IT_EDIFILE_ITEM3  TO     UNIXFILE.
  ENDLOOP.
  CLOSE DATASET    UNIXFILE.

ENDFORM.                    " P4000_EDI_SAMFILE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_EDI_IMPREQ2
*&---------------------------------------------------------------------*
FORM P4000_CREATE_EDI_IMPREQ2.

  CLEAR : IT_EDIFILE_ITEM2, W_EDI_RECORD.
* 관리번호(01).
  MOVE : W_ZFDHENO                             TO W_EDI_RECORD.
* 관리번호+란번호.
  CONCATENATE W_EDI_RECORD ZTIDRHSL-ZFCONO INTO W_EDI_RECORD
                                          SEPARATED BY SPACE.
* 일련번호(02).
  MOVE        W_COUNT   TO     L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 요건서류구분(03).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSL-ZFCNDC INTO W_EDI_RECORD.
* 요건서류내역(04).
  PERFORM   P1000_GET_DD07T_TEXT               USING   'ZDCNDC'
                                               ZTIDRHSL-ZFCNDC
                                               CHANGING L_TEXT.
  CONCATENATE W_EDI_RECORD '^' L_TEXT          INTO W_EDI_RECORD.
* 발급서류명(10).
  CONCATENATE W_EDI_RECORD '^'                 INTO W_EDI_RECORD.
* 서류번호(06).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHSL-ZFCNNO.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSL-ZFCNNO INTO W_EDI_RECORD.
* 법령코드(07).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSL-ZFLACD INTO W_EDI_RECORD.
* 발급일(08).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSL-ZFISZDT INTO W_EDI_RECORD.
* 통관수량(09).
  WRITE      ZTIDRHSL-ZFCUQN        TO         W_TEXT_AMOUNT
                                    UNIT       ZTIDRHSL-ZFCUQNM.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 통관수량단위(10).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSL-ZFCUQNM INTO W_EDI_RECORD.
* 입력일(11).
  CONCATENATE W_EDI_RECORD '^' SY-DATUM        INTO W_EDI_RECORD.

  IT_EDIFILE_ITEM2-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE_ITEM2.

ENDFORM.                    " P4000_CREATE_EDI_IMPREQ2

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_EDI_IMPREQ3
*&---------------------------------------------------------------------*
FORM P4000_CREATE_EDI_IMPREQ3.

  CLEAR : IT_EDIFILE_ITEM3, W_EDI_RECORD.
* 관리번호(01).
  MOVE : W_ZFDHENO                             TO W_EDI_RECORD.
* 관리번호+란번호.
*  CONCATENATE W_EDI_RECORD     ZTIDRHSD-ZFCONO INTO W_EDI_RECORD.
  CONCATENATE W_EDI_RECORD ZTIDRHSD-ZFCONO INTO W_EDI_RECORD
                                          SEPARATED BY SPACE.
* 일련번호(02).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFRONO INTO W_EDI_RECORD.
* 규격번호(03).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFRONO INTO W_EDI_RECORD.
* 규격1(04).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHSD-ZFGDDS1.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFGDDS1 INTO W_EDI_RECORD.
* 규격2(10).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHSD-ZFGDDS2.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFGDDS2 INTO W_EDI_RECORD.
* 규격3(06).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHSD-ZFGDDS3.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFGDDS3 INTO W_EDI_RECORD.
* 성분1(07).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHSD-ZFGDIN1.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFGDIN1 INTO W_EDI_RECORD.
* 성분2(08).
  PERFORM  P2000_SPACE_AND_CHANGE_SYMBOL CHANGING ZTIDRHSD-ZFGDIN2.
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFGDIN2 INTO W_EDI_RECORD.
* 수량(09).
  WRITE      ZTIDRHSD-ZFQNT         TO         W_TEXT_AMOUNT
                                    UNIT       ZTIDRHSD-ZFQNTM.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 수량단위(10).
  CONCATENATE W_EDI_RECORD '^' ZTIDRHSD-ZFQNTM INTO W_EDI_RECORD.
* 단가(11).
  WRITE      ZTIDRHSD-NETPR         TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHSD-ZFCUR.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 금액(12).
  WRITE      ZTIDRHSD-ZFAMT         TO         W_TEXT_AMOUNT
                                    CURRENCY   ZTIDRHSD-ZFCUR.
  PERFORM    P2000_WRITE_NO_MASK    CHANGING   W_TEXT_AMOUNT.
  CONCATENATE W_EDI_RECORD '^' W_TEXT_AMOUNT   INTO W_EDI_RECORD.
* 입력일(13).
  CONCATENATE W_EDI_RECORD '^' SY-DATUM        INTO W_EDI_RECORD.

  IT_EDIFILE_ITEM3-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE_ITEM3.

ENDFORM.                    " P4000_CREATE_EDI_IMPREQ3
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1690   text
*      -->P_IT_SELECTED_ZFREQNO  text
*      -->P_IT_SELECTED_ZFAMDNO  text
*      -->P_L_RETURN  text
*----------------------------------------------------------------------*
FORM P2000_LOCK_MODE_SET USING    VALUE(P_MODE)
                                  VALUE(P_ZFBLNO)
                                  VALUE(P_ZFCLSEQ)
                                  P_RETURN.
* LOCK CHECK
   IF P_MODE EQ 'L'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIDR'
           EXPORTING
                ZFBLNO                 =     P_ZFBLNO
                ZFCLSEQ                =     P_ZFCLSEQ
           EXCEPTIONS
                OTHERS        = 1.

      MOVE SY-SUBRC     TO     P_RETURN.
      IF SY-SUBRC NE 0.
         MESSAGE I510 WITH SY-MSGV1 'Import Document' P_ZFBLNO P_ZFCLSEQ
                      RAISING DOCUMENT_LOCKED.
      ENDIF.
   ELSEIF P_MODE EQ 'U'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIDR'
           EXPORTING
             ZFBLNO                 =     P_ZFBLNO
             ZFCLSEQ                =     P_ZFCLSEQ.
   ENDIF.

ENDFORM.                    " P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
*&      Form  GET_ZTIIMIMG08_SELECT
*&---------------------------------------------------------------------*
FORM GET_ZTIIMIMG08_SELECT USING  P_KEY
                                  P_ZFCD
                           CHANGING P_NAME.
  CLEAR ZTIMIMG08.
  SELECT SINGLE *
         FROM ZTIMIMG08
         WHERE ZFCDTY = P_KEY
           AND ZFCD =  P_ZFCD.

  P_NAME = ZTIMIMG08-ZFCDNM.

ENDFORM.                    " GET_ZTIIMIMG08_SELECT
