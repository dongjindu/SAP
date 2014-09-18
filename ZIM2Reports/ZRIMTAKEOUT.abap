*&---------------------------------------------------------------------*
*& Report  ZRIMTAKEOUT                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 반출신고 Report Program                               *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.11                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMTAKEOUT   MESSAGE-ID ZIM
                      LINE-SIZE 144
                      NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 반입예정/반입/반출 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFBLNO      LIKE     ZTBLOUR-ZFBLNO,   "B/L 관리번?
       ZFHBLNO     LIKE     ZTBL-ZFHBLNO,     "HOUSE BL NO.
       ZFBTSEQ     LIKE     ZTBLOUR-ZFBTSEQ,  "보세운송 일련?
       ZFINRNO     LIKE     ZTBLOUR-ZFOURNO,  "반출신고번?
       ZFABNAR     LIKE     ZTBLOUR-ZFABNAR,  "보세구역 CODE
       ZFBNARM     LIKE     ZTIMIMG03-ZFBNARM,"보세구역명.
       ZFBNARCD    LIKE     ZTBLOUR-ZFBNARCD, "보세구역 ID
       ZFYR        LIKE     ZTBLOUR-ZFYR,     "연?
       ZFSEQ       LIKE     ZTBLOUR-ZFSEQ,    "일련번호.
       ZFEDINF     LIKE     ZTBLOUR-ZFEDINF,  "전자문서기?
       ZFINRC      LIKE     ZTBLOUR-ZFINRC,   "신고지 세?
       INRC        LIKE     DD07T-DDTEXT,     "세관명.
       ZFINRCD     LIKE     ZTBLOUR-ZFINRCD,  "세관의 담당 과?
       ZFPINS      LIKE     ZTBLOUR-ZFPOUS,   "분할반출 차?
       ZFPRIN      LIKE     ZTBLOUR-ZFPROU,   "분할반출구?
       ZFPRDS      LIKE     ZTBLOUR-ZFPRDS,   "반출기간 연장?
       ZFCYCFS     LIKE     ZTBLOUR-ZFCYCFS,  "CY/CFS 구?
       ZFPKCN      LIKE     ZTBLOUR-ZFOUQN,   "총반출개?
       ZFPKCNM     LIKE     ZTBLOUR-ZFOUQNM,  "총반출개수 단?
       ZFINWT      LIKE     ZTBLOUR-ZFOUWT,   "반출중?
       ZFINTWT     LIKE     ZTBLOUR-ZFOUTWT,  "누계반출중?
       ZFINTQN     LIKE     ZTBLOUR-ZFOUTQN,  "누계반출개?
       ZFKG        LIKE     ZTBLOUR-ZFKG,     "무게단?
       ZFCT        LIKE     ZTBLOUR-ZFCT,     "개수단?
       ZFUSCD      LIKE     ZTBLINR-ZFUSCD,   "용도.
       USCD        LIKE     DD07T-DDTEXT,
       ZFINDT      LIKE     ZTBLINR-ZFINDT,   "반입일.
       ZFEDIST     LIKE     ZTBLINR-ZFEDIST,   " EDI STATUS
       EDIST       LIKE     DD07T-DDTEXT,
       ZFEDICK     LIKE     ZTBLINR-ZFEDICK,   " EDI CHECK
       EDICK       LIKE     DD07T-DDTEXT,
       CHECK(1)    VALUE 'N',                 "반출생성여부.
     END OF IT_TAB.
*-----------------------------------------------------------------------
* BDC 용 Table
*-----------------------------------------------------------------------
DATA:    BEGIN OF ZBDCDATA OCCURS 0.
         INCLUDE STRUCTURE BDCDATA.
DATA     END OF ZBDCDATA.

DATA : W_ZFUSCD        LIKE    ZTBLINR-ZFUSCD,
       W_ZFOTDT        LIKE    ZTBLOUR-ZFOTDT,
       OK-CODE         LIKE    SY-UCOMM,
       DISPMODE(1)     TYPE C,
       W_PROC_CNT      TYPE I,        " 처리건수.
       UMODE           VALUE 'S'.     " Async, Sync


*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
TABLES: ZTIMIMG03.
INCLUDE   ZRIMBLTOP01.    " B/L 관련 Data Define용 Include
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?
INCLUDE   ZRIMMESSAGE.    " Message


*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BNARCD  FOR ZTBLINR-ZFBNARCD,  " 도착?
                   S_BLNO    FOR ZTBLINR-ZFBLNO,   " B/L 관리번?
                   S_HBLNO   FOR ZTBL-ZFHBLNO,      " House B/L No.
                   S_ZFYR    FOR ZTBLINR-ZFYR       " 보세구역 반입년?
                             NO-EXTENSION
                             NO INTERVALS,
                   S_INRCD   FOR ZTBLINR-ZFINRCD,   " 세관.
                   S_ZFSEQ   FOR ZTBLINR-ZFSEQ,     " 보세구역 반입SEQ
                   S_INDT    FOR ZTBLINR-ZFINDT,    " 반입일?
                   S_EDIST   FOR ZTBLINR-ZFEDIST,   " EDI STATUS
                   S_EDICK   FOR ZTBLINR-ZFEDICK,   " EDI CHECK
                   S_USCD    FOR ZTBLINR-ZFUSCD.    " 용도구?
*   PARAMETERS :    P_BNAME   LIKE USR02-BNAME.      " 입력?
   SELECTION-SCREEN SKIP 1.                         " 1 LINE SKIP
   PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.
*-----------------------------------------------------------------------
* 반입신고 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 반입신고 SELECT
   PERFORM   P1000_GET_ZTBL_INR        USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
   W_OK_CODE = SY-UCOMM.
   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFGMNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP' OR 'DIS1' OR 'DIS2' OR 'DIS3'.         " 조회.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_DOC  USING IT_SELECTED-ZFBLNO
                                          IT_SELECTED-ZFBTSEQ
                                          IT_SELECTED-CHECK.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
         PERFORM   P1000_GET_ZTBL_INR        USING   W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.
            LEAVE TO SCREEN 0.
         ELSE.
            PERFORM RESET_LIST.
         ENDIF.

      WHEN 'OUCR1' .          " 반출신고 생성.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM P4000_GET_INIVAL.
* 반입신고정보 SELECT
            PERFORM   P1000_GET_ZTBL_INR        USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.
               LEAVE TO SCREEN 0.
            ELSE.
               PERFORM RESET_LIST.
            ENDIF.
         ENDIF.
      WHEN 'OUCR2' .          " 반출신고생성 및 EDI CREATE
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
* BDC CALL.
            PERFORM P4000_GET_INIVAL.

* 반입예정정보 SELECT
            PERFORM   P1000_GET_ZTBL_INR        USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.
               LEAVE TO SCREEN 0.
            ELSE.
               PERFORM RESET_LIST.
            ENDIF.
         ENDIF.

      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* 반입예정정보 SELECT
           PERFORM   P1000_GET_ZTBL_INR        USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMO0'.          " TITLE BAR
  S_ZFYR = SY-DATUM(2).

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60  '  [ Carry-in Declaration List ]  '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 125 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,
            ' ',                  SY-VLINE,
            (24)  'Carry-in No.', SY-VLINE,
            (14)  'Carry-out Y/N', SY-VLINE,
            (06)  'Car.in',        SY-VLINE,
            (18)  'Bonded Area',     SY-VLINE,
            (15)  'Customs.of.Decl',   SY-VLINE,
            (10)  'Car.in Amt',
            (04)  '  ',     SY-VLINE,
            (13)  'EDI Status',    SY-VLINE,
            (10)  'Usage Type', SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',        SY-VLINE,
            (24)  'B/L No',       SY-VLINE,
            (14)  'Bond.Trans.Seq',  SY-VLINE,
            (06)  'Year',         SY-VLINE,
            (10)  'Code',
            (07)  'ID',           SY-VLINE,
            (10)  'Code',
            (04)  'Sign',         SY-VLINE,
            (10)  'Car.in.Wt',
            (04)  ' ',     SY-VLINE,
            (13)  'EDI Check',    SY-VLINE,
            (10)  'Car.in.Dat',       SY-VLINE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMO0'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMO0'.           " GUI TITLE SETTING..

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
        ZFBLNO  LIKE ZTBLINOU-ZFBLNO,
        ZFHBLNO LIKE ZTBL-ZFHBLNO,
        ZFBTSEQ LIKE ZTBLINOU-ZFBTSEQ,
        CHECK(1).


  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFHBLNO  TO ZFHBLNO,
         IT_TAB-ZFBTSEQ  TO ZFBTSEQ,
         IT_TAB-CHECK    TO CHECK.

  DO.
    CLEAR: MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.

    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      W_TABIX = SY-INDEX + 1.
      MOVE : IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFHBLNO  TO IT_SELECTED-ZFHBLNO,
             IT_TAB-ZFBTSEQ  TO IT_SELECTED-ZFBTSEQ,
             IT_TAB-CHECK    TO IT_SELECTED-CHECK.


      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFBLNO  TO IT_SELECTED-ZFBLNO,
             ZFHBLNO TO IT_SELECTED-ZFHBLNO,
             ZFBTSEQ TO IT_SELECTED-ZFBTSEQ,
             CHECK   TO IT_SELECTED-CHECK.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
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

  IF W_COUNT GT 0.
     FORMAT RESET.
     WRITE : / 'Total', W_COUNT, 'Case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE : / SY-VLINE,
            MARKFIELD  AS CHECKBOX
            COLOR COL_NORMAL INTENSIFIED OFF, SY-VLINE,
            (24) IT_TAB-ZFINRNO,    SY-VLINE, " 반입신고번호.
            (14) IT_TAB-CHECK,      SY-VLINE, " 반출생성.
            (06) IT_TAB-ZFSEQ,      SY-VLINE, " 일련번호.
            (18) IT_TAB-ZFBNARM,    SY-VLINE, " 보세구역.
            (15) IT_TAB-INRC,       SY-VLINE, " 신고지세관.
            (10) IT_TAB-ZFPKCN UNIT IT_TAB-ZFCT,  " 총반입개수.
            (04) IT_TAB-ZFPKCNM,     SY-VLINE,  " 포장종류.
            (13) IT_TAB-EDIST,       SY-VLINE,
            (10) IT_TAB-USCD,        SY-VLINE.  " 용도구분.

  HIDE: IT_TAB.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',          SY-VLINE,
            (24) IT_TAB-ZFHBLNO,    SY-VLINE,
            (14) IT_TAB-ZFBTSEQ,    SY-VLINE,
            (06) IT_TAB-ZFYR,       SY-VLINE,  " 연도.
            (10) IT_TAB-ZFABNAR,               " 보세구역.
            (07) IT_TAB-ZFBNARCD,   SY-VLINE,  " 보세구역ID.
            (10) IT_TAB-ZFINRC,                " CODE.
            (04) IT_TAB-ZFINRCD,    SY-VLINE,  " 세관과부호.
            (10) IT_TAB-ZFINWT UNIT IT_TAB-ZFKG,   " 반입중량
            (04) IT_TAB-ZFKG,       SY-VLINE,  " 중량단위.
            (13) IT_TAB-EDICK,      SY-VLINE,
            (10) IT_TAB-ZFINDT,     SY-VLINE.

  WRITE: SY-ULINE.
  HIDE: IT_TAB.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE.

*  LOOP AT IT_TAB   WHERE UPDATE_CHK EQ 'U'.
* 수입의뢰 상태 table Select
*     SELECT SINGLE * FROM   ZTREQST
*                     WHERE  ZFREQNO EQ IT_TAB-ZFREQNO
*                     AND    ZFAMDNO EQ '00'.

*-----------------------------------------------------------------------
* 이전 data를 Temp Table로 Move
*-----------------------------------------------------------------------

* 변경 데이타 Move
*     MOVE : IT_TAB-ZFRLST1  TO  ZTREQST-ZFRLST1,     " 릴리즈 상?
*            IT_TAB-ZFRLDT1  TO  ZTREQST-ZFRLDT1,     " 릴리즈 일?
*            IT_TAB-ZFRLNM1  TO  ZTREQST-ZFRLNM1.     " 담당?

*     UPDATE ZTREQST.                                 " DATA UPDATE
*      IF SY-SUBRC EQ 0.
*-----------------------------------------------------------------------
* 변경이력 작?
**----------------------------------------------------------------------
**        PERFORM  SET_LC_HEADER_CHANGE_DOCUMENT.      " 변경 이?
*      ELSE.
**        MESSAGE E031 WITH ZTREQHD-ZFREQNO.
*         ROLLBACK WORK.                               " 오류?
*      ENDIF.
*
*  ENDLOOP.
*
*  IF SY-SUBRC EQ 0.
*     COMMIT WORK.                                   " 정상적인 경?
*  ENDIF.

ENDFORM.                    " P3000_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_REFRESH_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = '리스트 REFRESH 확인'
             DIAGNOSE_OBJECT = ''
            TEXT_QUESTION = '먼저 릴리즈(승인) 작업을 저장하시겠습니까?'
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.
ENDFORM.                    " P2000_REFRESH_POPUP_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = '리스트종료 확인'
             DIAGNOSE_OBJECT = ''
            TEXT_QUESTION = '먼저 릴리즈(승인) 작업을 저장하시겠습니까?'
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_EXIT_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTBL_INR
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTBL_INR  USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH IT_TAB.
  CLEAR IT_TAB.
  SELECT  *
     FROM ZTBLINR
     WHERE ZFBLNO   IN S_BLNO
       AND ZFBNARCD IN S_BNARCD  " 도착?
       AND ZFBLNO   IN S_BLNO    " B/L 관리번?
       AND ZFYR     IN S_ZFYR    " 보세구역 반입년?
       AND ZFSEQ    IN S_ZFSEQ   " 보세구역 반입SEQ
       AND ZFINRCD  IN S_INRCD   " 세관.
       AND ZFINDT   IN S_INDT    " 반입일?
       AND ZFEDIST  IN S_EDIST   " EDI STATUS
       AND ZFEDICK  IN S_EDICK   " EDI CHECK
       AND ZFUSCD   IN S_USCD.    " 용도구분.
*      AND ERNAM    =  P_BNAME.   " 생성인.
     SELECT  SINGLE *
        FROM ZTBL
        WHERE ZFBLNO  = ZTBLINR-ZFBLNO
         AND  ZFHBLNO IN S_HBLNO.
     IT_TAB-CHECK = 'N'.
     IF SY-SUBRC NE 0.
        CONTINUE.
     ENDIF.
     SELECT SINGLE *
       FROM ZTBLOUR
      WHERE ZFBLNO  = ZTBLINR-ZFBLNO
        AND ZFBTSEQ = ZTBLINR-ZFBTSEQ.
     IF SY-SUBRC EQ 0.
        IF P_OPEN  EQ 'X'.    " 반출데이타 포함시.
           IT_TAB-CHECK = 'Y'.
        ELSE.
           CONTINUE.
        ENDIF.
     ENDIF.

     PERFORM GET_DD07T_SELECT(SAPMZIM00) USING 'ZDUSCD' ZTBLINR-ZFUSCD
                                   CHANGING   IT_TAB-USCD.

     PERFORM GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCOTM' ZTBLINR-ZFINRC
                                   CHANGING   IT_TAB-INRC.
     PERFORM GET_DD07T_SELECT(SAPMZIM00) USING 'ZDEDIST'
                                   ZTBLINR-ZFEDIST
                                   CHANGING   IT_TAB-EDIST.
     PERFORM GET_DD07T_SELECT(SAPMZIM00) USING 'ZDOX' ZTBLINR-ZFEDICK
                                   CHANGING   IT_TAB-EDICK.
     SELECT SINGLE *
         FROM ZTIMIMG03
         WHERE ZFBNAR = ZTBLINR-ZFABNAR.

     MOVE-CORRESPONDING ZTBLINR TO IT_TAB.
     MOVE: ZTBL-ZFHBLNO      TO IT_TAB-ZFHBLNO,
           ZTIMIMG03-ZFBNARM TO IT_TAB-ZFBNARM.

     APPEND IT_TAB.
  ENDSELECT.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0.
      MESSAGE S738.
      W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_GET_ZTBL_INR
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_DOC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_DOC USING    P_ZFBLNO P_ZFBTSEQ P_CHECK.

   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
   SET PARAMETER ID 'ZPBTSEQ'  FIELD P_ZFBTSEQ.
   EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.
   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
   EXPORT 'ZPBTSEQ'       TO MEMORY ID 'ZPBTSEQ'.

   IF SY-UCOMM EQ 'DISP'.               " 반입예정정?
      CALL TRANSACTION 'ZIMI3' AND SKIP  FIRST SCREEN.
   ELSEIF SY-UCOMM EQ 'DIS1'.           " 반입신?
      CALL TRANSACTION 'ZIMI8' AND SKIP  FIRST SCREEN.
   ELSEIF SY-UCOMM EQ 'DIS2'.           " 반출신?
      IF P_CHECK EQ 'N'.
         MESSAGE E055 WITH P_ZFBLNO P_ZFBTSEQ.
      ENDIF.
      CALL TRANSACTION 'ZIMO3' AND SKIP  FIRST SCREEN.

   ELSEIF SY-UCOMM EQ 'DIS3'.           " B/L
      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_DOC
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

*  SET TITLEBAR 'POPU' WITH '반출신고'.
*  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  IF OK-CODE NE 'YES'.
     SET SCREEN 0.
     LEAVE SCREEN.
  ENDIF.

 IF W_ZFOTDT IS INITIAL.
     MOVE SY-DATUM    TO W_ZFOTDT.
  ENDIF.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_INIVAL
*&---------------------------------------------------------------------*
FORM P4000_GET_INIVAL.

  PERFORM P2000_POPUP_MESSAGE USING
                    'Confirmation'
                    'Do you apply with selected document?'
*                    '선택한 문서로 반영하시겠습니까?'
                    '  Yes   '
                    '   No   '
                    '1'
                    W_BUTTON_ANSWER.
  CLEAR W_PROC_CNT.
  IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
     LOOP AT IT_SELECTED.

        PERFORM P4000_BDC_VALID_CHECK.
        PERFORM ZTBLOUR_BDC_INSERT.
        DISPMODE = 'N'.
        CALL TRANSACTION 'ZIMO1'
                  USING       ZBDCDATA
                  MODE        DISPMODE
                  UPDATE      UMODE.
       IF SY-SUBRC <> 0.
          MESSAGE I952.
          W_ERR_CHK = 'Y'.
          EXIT.
        ENDIF.
        ADD 1       TO W_PROC_CNT.
     ENDLOOP.
     MESSAGE S924 WITH W_PROC_CNT.
     EXIT.
  ENDIF.

ENDFORM.                    " P4000_GET_INIVAL
*&---------------------------------------------------------------------*
*&      Form  P4000_BDC_VALID_CHECK
*&---------------------------------------------------------------------*
FORM P4000_BDC_VALID_CHECK.

  IF IT_SELECTED-CHECK EQ 'Y'. " 이미 반출신고가 있는 경우.
     DELETE  FROM ZTBLOUR WHERE ZFBLNO  = IT_SELECTED-ZFBLNO
                            AND ZFBTSEQ = IT_SELECTED-ZFBTSEQ.

  ENDIF.

ENDFORM.                    " P4000_BDC_VALID_CHECK
*&---------------------------------------------------------------------*
*&      Form  ZTBLOUR_BDC_INSERT
*&---------------------------------------------------------------------*
FORM ZTBLOUR_BDC_INSERT.

  REFRESH ZBDCDATA.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMZIM01' '9221'.

  PERFORM A_ZBDCDATA USING :
      ' ' 'BDC_CURSOR' 'ZSREQHD-ZFBLNO',
      ' ' 'BDC_OKCODE' '=ENTR',                   " ENTER
      ' ' 'ZSREQHD-ZFBLNO'   IT_SELECTED-ZFBLNO,  " B/L NUMBER.
      ' ' 'ZSREQHD-ZFBTSEQ'	  IT_SELECTED-ZFBTSEQ.

  PERFORM A_ZBDCDATA USING 'X' 'SAPMZIM01' '9220'.
  PERFORM A_ZBDCDATA USING :
     ' ' 'BDC_CURSOR'	'ZTBLOUR-ZFINRCD',
     ' ' 'BDC_OKCODE'	'=SAVE'.
  PERFORM A_ZBDCDATA USING 'X' 'SAPMZIM01' '0001'.
  PERFORM A_ZBDCDATA USING :
     ' ' 'BDC_OKCODE'	'=YES'.

ENDFORM.                    " ZTBLOUR_BDC_INSERT
*&---------------------------------------------------------------------*
*&      Form  A_ZBDCDATA
*&---------------------------------------------------------------------*
FORM A_ZBDCDATA USING   BEGIN_CHECK OBJNAM VALUE.

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
