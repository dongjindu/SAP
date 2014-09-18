*&---------------------------------------------------------------------*
*& Report  ZRIMTAKEIN1                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 반입신고 Report Program                               *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.21                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMTAKEIN1   MESSAGE-ID ZIM
                      LINE-SIZE 100
                      NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 반입예정/반입/반출 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                       " MARK
       W_GB01(1)  TYPE C VALUE ';',
       UPDATE_CHK TYPE C,                       " DB 반영 여부...
       W_GB02(1)  TYPE C VALUE ';',
       ZFBLNO     LIKE ZTBLINOU-ZFBLNO,         " B/L 관리번?
       W_GB03(1)  TYPE C VALUE ';',
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,            " House B/L No.
       W_GB04(1)  TYPE C VALUE ';',
       ZFBTSEQ    LIKE ZTBLINOU-ZFBTSEQ,        " 보세운송 일련번?
       W_GB05(1)  TYPE C VALUE ';',
       ZFINRNO    LIKE ZTBLINR-ZFINRNO,         " 반입신고번?
       W_GB06(1)  TYPE C VALUE ';',
       ZFBNARCD   LIKE ZTBLINR-ZFBNARCD,        " 도착보세구역 관리코?
       W_GB07(1)  TYPE C VALUE ';',
       ZFYR       LIKE ZTBLINR-ZFYR,            " 연?
       W_GB08(1)  TYPE C VALUE ';',
       ZFSEQ      LIKE ZTBLINR-ZFSEQ,           " 일련번?
       W_GB09(1)  TYPE C VALUE ';',
       ZFPKCN     LIKE ZTBLINR-ZFPKCN,          " 포장수량.
       W_GB10(1)  TYPE C VALUE ';',
       ZFPKCNM    LIKE ZTBLINR-ZFPKCNM,         " 포장수량 단위.
       W_GB11(1)  TYPE C VALUE ';',
       ZFINWT     LIKE ZTBLINR-ZFINWT,          " 중량.
       W_GB12(1)  TYPE C VALUE ';',
       ZFKG       LIKE ZTBLINR-ZFKG,            " 중량단위
       W_GB13(1)  TYPE C VALUE ';',
       ZFBTRNO    LIKE ZTBLINOU-ZFBTRNO,        " 보세운송 신고번호.
       W_GB14(1)  TYPE C VALUE ';',
       ZFGMNO     LIKE ZTBLINOU-ZFGMNO,         " 적하목록 관리번호.
       W_GB15(1)  TYPE C VALUE ';',
       ZFGINM     LIKE ZTBLINR-ZFGINM,          " 반입담당.
       W_GB16(1)  TYPE C VALUE ';',
       ZFTDDT     LIKE ZTBLINOU-ZFTDDT,         " 운송기한일.
       W_GB17(1)  TYPE C VALUE ';',
       ZFRCDT     LIKE ZTBLINOU-ZFRCDT,         " 수신일.
       W_GB18(1)  TYPE C VALUE ';',
       ZFINDT     LIKE ZTBLINR-ZFINDT,          " 반입일(인수일자)
       W_GB19(1)  TYPE C VALUE ';',
       ZFINTM     LIKE ZTBLINR-ZFINTM,          " 반입시간.
       W_GB20(1)  TYPE C VALUE ';',
       ZFBOUYN    LIKE ZTBLINOU-ZFBOUYN,        " 반출신고여부.
       W_GB21(1)  TYPE C VALUE ';',
       ZFGIRNM    LIKE ZTBLINR-ZFGIRNM,         " 반입신고담당자.
       W_GB22(1)  TYPE C VALUE ';',
       ZFUSCD     LIKE ZTBLINR-ZFUSCD,          " 용도구분.
       W_GB23(1)  TYPE C VALUE ';',
*      ZFUSCD     LIKE ZTBLINR-ZFUSCD,          " 관세여?
*      W_GB24(1)  TYPE C VALUE ';',
       ZFTXYN     LIKE ZTBLINR-ZFTXYN,          " 관세여?
       W_GB24(1)  TYPE C VALUE ';',
       ZFINTY     LIKE ZTBLINR-ZFINTY,          " 반입유?
       W_GB26(1)  TYPE C VALUE ';',
       ZFPRIN     LIKE ZTBLINR-ZFPRIN,          " 분할구?
       W_GB27(1)  TYPE C VALUE ';',
       ZFEDIST    LIKE ZTBLINR-ZFEDIST,         " EDI Status
       W_GB28(1)  TYPE C VALUE ';',
       ZFMATGB    LIKE ZTBL-ZFMATGB,            " 자재구분..
       W_GB30(1)  TYPE C VALUE ';',
       WL_TEXT12(12),
       W_GB29(1)  TYPE C VALUE ';',
       ZFPKCN1    LIKE ZTBLINR-ZFINWT.          " 수량..
DATA : END OF IT_TAB.
DATA : W_ZFUSCD        LIKE    ZTBLINR-ZFUSCD,
       W_ZFPRIN        LIKE    ZTBLINR-ZFPRIN,
       W_ZFINTY        LIKE    ZTBLINR-ZFINTY.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
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
                   S_BLNO    FOR ZTBLINOU-ZFBLNO,   " B/L 관리번?
                   S_HBLNO   FOR ZTBL-ZFHBLNO,      " House B/L No.
                   S_ZFYR    FOR ZTBLINR-ZFYR       " 보세구역 반입년?
                             NO-EXTENSION
                             NO INTERVALS,
                   S_ZFSEQ   FOR ZTBLINR-ZFSEQ,     " 보세구역 반입SEQ
                   S_RCDT    FOR ZTBLINOU-ZFRCDT,   " 수신일?
                   S_INDT    FOR ZTBLINR-ZFINDT,    " 반입일?
                   S_TDDT    FOR ZTBLINOU-ZFTDDT,   " 운송기?
                   S_EDIST   FOR ZTBLINR-ZFEDIST,   " EDI STATUS
                   S_EDICK   FOR ZTBLINR-ZFEDICK,   " EDI CHECK
                   S_USCD    FOR ZTBLINR-ZFUSCD.    " 용도구?
   PARAMETERS :    P_BNAME   LIKE USR02-BNAME.      " 입력?
   SELECTION-SCREEN SKIP 1.                         " 1 LINE SKIP
   PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.
*-----------------------------------------------------------------------
* 반입상태 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_STATUS FOR ZTBLINOU-ZFBINYN NO INTERVALS NO-DISPLAY.


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

* 파라메타 설?
   PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 반입예정정보 SELECT
   PERFORM   P1000_GET_ZVBL_INR        USING   W_ERR_CHK.
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
      WHEN 'DISP' OR 'DIS1' OR 'DIS2' OR 'DIS3'.         " L/C 조?
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.

            PERFORM P2000_SHOW_DOC  USING IT_SELECTED-ZFBLNO
                                          IT_SELECTED-ZFBTSEQ.

         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'INOU' .          " 반입신고 변경 및 EDI CREATE
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
* Message Box
            PERFORM P2000_POPUP_MESSAGE USING
                    'Confirmation'
                    '선택한 문서로 반영하시겠습니까?'
                    '확    인'
                    '아 니 오'
                    '1'
                    W_BUTTON_ANSWER.

            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
* 반입신고 변?
               PERFORM P3000_INPUT_DATA.
* 반입예정정보 SELECT
               PERFORM   P1000_GET_ZVBL_INR        USING   W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.
                  LEAVE TO SCREEN 0.
               ELSE.
                  PERFORM RESET_LIST.
               ENDIF.
            ENDIF.
         ENDIF.
      WHEN 'INCR' .          " 반입신고 변경 및 EDI CREATE
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
* Message Box
            PERFORM P2000_POPUP_MESSAGE USING
                    'Confirmation'
                    '선택한 문서로 EDI Create 하시겠습니까?'
                    '확    인'
                    '아 니 오'
                    '1'
                    W_BUTTON_ANSWER.

            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
* 반입신고 변?
               PERFORM P3000_INPUT_DATA.
* 반입예정정보 SELECT
               PERFORM   P1000_GET_ZVBL_INR        USING   W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.
                  LEAVE TO SCREEN 0.
               ELSE.
                  PERFORM RESET_LIST.
               ENDIF.
            ENDIF.
         ENDIF.

      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* 반입예정정보 SELECT
           PERFORM   P1000_GET_ZVBL_INR        USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIMI4'.          " TITLE BAR

  S_ZFYR = SY-DATUM(2).
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  '  [ 보세구역 반입내역 ]  '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 82 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE,
            ' ',                             SY-VLINE,
            '    보세운송번호    ',          SY-VLINE,
            '       B/L Number      ',       SY-VLINE,
            '     수    량     ',            SY-VLINE NO-GAP,
            '과세 ' NO-GAP,                  SY-VLINE NO-GAP,
            '분할 ' NO-GAP,                  SY-VLINE,
            '반입담당자',                    SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',                   SY-VLINE,
            '    보세구역입고    ',          SY-VLINE,
            ' 수신일자     운송기한 ',       SY-VLINE,
            '     중    량     ',            SY-VLINE NO-GAP,
            '반입 ' NO-GAP,                  SY-VLINE NO-GAP,
            'Stat.' NO-GAP,                  SY-VLINE,
            '신고담당자',                    SY-VLINE.

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
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.

  IF P_BNAME IS INITIAL.       P_BNAME  = '%'.      ENDIF.
*  IF P_ZFGMNO IS INITIAL.      P_ZFGMNO = '%'.      ENDIF.
*  IF P_ZFMSN  IS INITIAL.      P_ZFMSN  = '%'.      ENDIF.
*  IF P_ZFHSN  IS INITIAL.      P_ZFHSN  = '%'.      ENDIF.

   MOVE: 'I'      TO S_STATUS-SIGN,
         'EQ'     TO S_STATUS-OPTION,
         ''       TO S_STATUS-LOW.
   APPEND S_STATUS.
*-----------------------------------------------------------------------
* 생성문서 포함....
*-----------------------------------------------------------------------
   IF P_OPEN EQ 'X'.
      MOVE: 'I'      TO S_STATUS-SIGN,
            'EQ'     TO S_STATUS-OPTION,
            'X'      TO S_STATUS-LOW.
      APPEND S_STATUS.
   ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMI4'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMI4'.           " GUI TITLE SETTING..

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
        ZFBTSEQ LIKE ZTBLINOU-ZFBTSEQ,
        ZFUSCD  LIKE ZTBLINR-ZFUSCD,
        ZFPRIN  LIKE ZTBLINR-ZFPRIN,
        ZFINTY  LIKE ZTBLINR-ZFINTY.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFBTSEQ  TO ZFBTSEQ,
         IT_TAB-ZFUSCD   TO ZFUSCD,
         IT_TAB-ZFPRIN   TO ZFPRIN,
         IT_TAB-ZFINTY   TO ZFINTY.

  DO.
    CLEAR: MARKFIELD, W_ZFUSCD, W_ZFPRIN, W_ZFINTY.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.

    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      READ LINE SY-INDEX FIELD VALUE  W_ZFUSCD.
      READ LINE SY-INDEX FIELD VALUE  W_ZFPRIN.
      W_TABIX = SY-INDEX + 1.
      READ LINE W_TABIX FIELD VALUE  W_ZFINTY.
      MOVE : IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFBTSEQ  TO IT_SELECTED-ZFBTSEQ,
             W_ZFUSCD        TO IT_SELECTED-ZFUSCD,
             W_ZFPRIN        TO IT_SELECTED-ZFPRIN,
             W_ZFINTY        TO IT_SELECTED-ZFINTY.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFBLNO  TO IT_SELECTED-ZFBLNO,
             ZFBTSEQ TO IT_SELECTED-ZFBTSEQ,
             ZFUSCD  TO IT_SELECTED-ZFUSCD,
             ZFPRIN  TO IT_SELECTED-ZFPRIN,
             ZFINTY  TO IT_SELECTED-ZFINTY.

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
     WRITE : / '총', W_COUNT, '건'.
  ENDIF.


ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  MOVE : SY-TABIX         TO W_LIST_INDEX,
         IT_TAB-ZFUSCD    TO W_ZFUSCD,
         IT_TAB-ZFPRIN    TO W_ZFPRIN,
         IT_TAB-ZFINTY    TO W_ZFINTY.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  MOVE : IT_TAB-ZFPKCN   TO    IT_TAB-ZFPKCN1.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX
       COLOR COL_BACKGROUND INTENSIFIED OFF,
       SY-VLINE,
       IT_TAB-ZFBTRNO,                             " 보세운송번?
       SY-VLINE,
       IT_TAB-ZFHBLNO NO-GAP,                      " B/L Number
       SY-VLINE NO-GAP,
       IT_TAB-ZFPKCN1 UNIT IT_TAB-ZFPKCNM NO-GAP,   " 수량
       IT_TAB-ZFPKCNM,
       SY-VLINE,
    78 W_ZFUSCD       INPUT ON COLOR COL_NORMAL INTENSIFIED ON,
*   78 IT_TAB-ZFUSCD  INPUT ON COLOR COL_NORMAL INTENSIFIED ON,
*    78 IT_TAB-ZFTXYN AS CHECKBOX,                  " 과세여?
    81 SY-VLINE,
    84 W_ZFPRIN       INPUT ON COLOR COL_NORMAL INTENSIFIED ON,  "
*   84 IT_TAB-ZFPRIN  INPUT ON COLOR COL_NORMAL INTENSIFIED ON,  "
    87 SY-VLINE NO-GAP,
       IT_TAB-ZFGINM NO-GAP,                       " 반입담당?
       SY-VLINE.
  HIDE: W_LIST_INDEX, IT_TAB.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, 5 SY-VLINE, 10 IT_TAB-WL_TEXT12,
     28  SY-VLINE,
       IT_TAB-ZFRCDT,
*      SY-VLINE,
       43 IT_TAB-ZFTDDT,
       SY-VLINE NO-GAP,
       IT_TAB-ZFINWT UNIT IT_TAB-ZFKG    NO-GAP,
       IT_TAB-ZFKG    NO-GAP,
       SY-VLINE,
       W_ZFINTY      INPUT ON,
*      IT_TAB-ZFINTY INPUT ON,
    81 SY-VLINE,
    84 IT_TAB-ZFEDIST NO-GAP,
    87 SY-VLINE NO-GAP,
       IT_TAB-ZFGIRNM NO-GAP,
       SY-VLINE.
  HIDE: W_LIST_INDEX, IT_TAB.

  WRITE : / SY-ULINE.
* stored value...
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE.

  LOOP AT IT_TAB   WHERE UPDATE_CHK EQ 'U'.
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
      IF SY-SUBRC EQ 0.
*-----------------------------------------------------------------------
* 변경이력 작?
*-----------------------------------------------------------------------
*        PERFORM  SET_LC_HEADER_CHANGE_DOCUMENT.      " 변경 이?
      ELSE.
*        MESSAGE E031 WITH ZTREQHD-ZFREQNO.
         ROLLBACK WORK.                               " 오류?
      ENDIF.

  ENDLOOP.

  IF SY-SUBRC EQ 0.
     COMMIT WORK.                                   " 정상적인 경?
  ENDIF.

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
*&      Form  P1000_GET_ZVBL_INOU
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVBL_INR  USING    W_ERR_CHK.
DATA : WL_TABIX   LIKE   SY-TABIX.
  DATA : W_ZFBTSEQ LIKE ZTBLOUR-ZFBTSEQ.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
                               FROM  ZVBL_INR
                               WHERE ZFBNARCD   IN     S_BNARCD
                               AND   ZFBLNO     IN     S_BLNO
                               AND   ZFHBLNO    IN     S_HBLNO
                               AND   ZFYR       IN     S_ZFYR
                               AND   ZFSEQ      IN     S_ZFSEQ
                               AND   ZFRCDT     IN     S_RCDT
                               AND   ZFTDDT     IN     S_TDDT
                               AND   ZFEDIST    IN     S_EDIST
                               AND   ZFDOCST    EQ     'N'
                               AND   ZFINDT     IN     S_INDT
                               AND   ZFUSCD     IN     S_USCD
                               AND   ZFINDT     GT     '00000000'
                               AND   ZFBOUYN    IN     S_STATUS
                               AND   ERNAM      LIKE   P_BNAME.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S046.    EXIT.
  ENDIF.

  LOOP AT IT_TAB.
     IF NOT ( IT_TAB-ZFBNARCD IS INITIAL AND IT_TAB-ZFYR IS INITIAL AND
              IT_TAB-ZFSEQ    IS INITIAL ).
        CONCATENATE IT_TAB-ZFBNARCD '-' IT_TAB-ZFYR '-'
                      IT_TAB-ZFSEQ INTO IT_TAB-WL_TEXT12.
     ELSE.
        CLEAR : IT_TAB-WL_TEXT12.
     ENDIF.
*-----------------------------------------------------------------------
* 2000/06/17 안덕기 과장 DEFINE
* >>>>>> 반입 Create시로 로직 변경.........( 2000/06/28 )
*     CASE IT_TAB-ZFMATGB.
*        WHEN '1'.    IT_TAB-ZFUSCD = 'P'.   " 수출용 원자?
*        WHEN '2'.    IT_TAB-ZFUSCD = 'P'.   " LOCAL
*        WHEN '3'.    IT_TAB-ZFUSCD = 'C'.   " 내수용 원자?
*        WHEN '4'.    IT_TAB-ZFUSCD = 'C'.   " 시설?
*        WHEN '5'.    IT_TAB-ZFUSCD = 'C'.   " 상?
*        WHEN OTHERS.                        " 기타는 DB 그대로 ....
*     ENDCASE.
* 이고반입일 경우.  ==> 반입유형을 '21'로 SET...
*     IF IT_TAB-ZFBTSEQ NE 1.
*        W_ZFBTSEQ = IT_TAB-ZFBTSEQ - 1.
*        SELECT SINGLE * FROM ZTBLOUR
*                        WHERE ZFBLNO   EQ IT_TAB-ZFBLNO
*                        AND   ZFBTSEQ  EQ W_ZFBTSEQ.
*        IF ZTBLOUR-ZFOUTY EQ '61'.     " 이고반출일 경우...
*           IT_TAB-ZFINTY = '21'.
*        ENDIF.
*     ENDIF.
*-----------------------------------------------------------------------
     MODIFY IT_TAB  INDEX SY-TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZVBL_INR
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_DOC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_DOC USING    P_ZFBLNO P_ZFBTSEQ.

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
      CALL TRANSACTION 'ZIMO3' AND SKIP  FIRST SCREEN.
   ELSEIF SY-UCOMM EQ 'DIS3'.           " B/L
      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_INPUT_DATA
*&---------------------------------------------------------------------*
FORM P3000_INPUT_DATA.
DATA : WL_SUBRC LIKE SY-SUBRC.
* Data 검?
 LOOP  AT   IT_SELECTED.
    IF NOT IT_SELECTED-ZFUSCD IS INITIAL.
       PERFORM   GET_DD07T_SELECT USING     'ZDUSCD' IT_SELECTED-ZFUSCD
                                  CHANGING  W_DDTEXT
                                            W_SY_SUBRC.
       IF W_SY_SUBRC NE 0.
          MESSAGE E207 WITH IT_SELECTED-ZFUSCD.
       ENDIF.
    ENDIF.
    IF NOT IT_SELECTED-ZFPRIN IS INITIAL.
       PERFORM   GET_DD07T_SELECT USING 'ZDPRINO' IT_SELECTED-ZFPRIN
                                  CHANGING  W_DDTEXT
                                            W_SY_SUBRC.
       IF W_SY_SUBRC NE 0.
          MESSAGE E207 WITH IT_SELECTED-ZFPRIN.
       ENDIF.
    ENDIF.
    IF NOT IT_SELECTED-ZFINTY IS INITIAL.
       PERFORM   GET_DD07T_SELECT USING 'ZDINTY' IT_SELECTED-ZFINTY
                                  CHANGING  W_DDTEXT
                                            W_SY_SUBRC.
       IF W_SY_SUBRC NE 0.
          MESSAGE E207 WITH IT_SELECTED-ZFINTY.
       ENDIF.
    ENDIF.
 ENDLOOP.

*  LOCK OBJECT 추가할 부?
 PERFORM   ZTBLINOU_LOCK_EXEC  USING   'L'.

* Data Update
 LOOP  AT   IT_SELECTED.
    SELECT SINGLE * FROM ZTBLINR WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
                                 AND   ZFBTSEQ EQ IT_SELECTED-ZFBTSEQ.
    WL_SUBRC = SY-SUBRC.
    *ZTBLINR = ZTBLINR.

    IF SY-SUBRC NE 0.
       MESSAGE E044 WITH IT_SELECTED-ZFBLNO IT_SELECTED-ZFBTSEQ.
    ENDIF.
*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
    MOVE : IT_SELECTED-ZFUSCD   TO     ZTBLINR-ZFUSCD,   " 용도구?
           IT_SELECTED-ZFPRIN   TO     ZTBLINR-ZFPRIN,   " 분할반입구?
           IT_SELECTED-ZFINTY   TO     ZTBLINR-ZFINTY,   " 반입유?
           SY-DATUM             TO     ZTBLINR-UDAT,     " UPDATE DATE
           SY-UNAME             TO     ZTBLINR-UNAM,     " UPDATE NAME
           SY-UNAME             TO     ZTBLINR-ZFGIRNM.  "

    PERFORM   P2000_EDI_DATA_CHECK.

    UPDATE  ZTBLINR.
    IF SY-SUBRC NE 0.
       MESSAGE E047  WITH  ZTBLINR-ZFBLNO   ZTBLINR-ZFBTSEQ.
    ENDIF.

*-----------------------------------------------------------------------
* ZTBLINOU UPDATE
*-----------------------------------------------------------------------
    W_UPDATE_CNT = W_UPDATE_CNT + 1.
    IF W_OK_CODE EQ  'INCR'.
       IF ZTBLINR-ZFEDICK EQ 'X'.
          MESSAGE I119 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ.
          CONTINUE.
       ENDIF.
       IF ZTBLINR-ZFEDIST NE 'N'.
          MESSAGE I105 WITH ZTBLINR-ZFBLNO ZTBLINR-ZFBTSEQ
                            ZTBLINR-ZFEDIST.
          CONTINUE.
       ENDIF.
* EDI CRETE
       PERFORM  P2000_EDI_CREATE.
    ENDIF.
  ENDLOOP.
*-----------------------------------------------------------------------
*  LOCK OBJECT 추가할 부?
*-----------------------------------------------------------------------
  PERFORM   ZTBLINOU_LOCK_EXEC  USING   'U'.
*-----------------------------------------------------------------------

ENDFORM.                    " P3000_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  ZTBLINOU_LOCK_EXEC
*&---------------------------------------------------------------------*
FORM ZTBLINOU_LOCK_EXEC USING    VALUE(PA_LOCK).
DATA : L_TABIX  LIKE SY-TABIX,
       WL_SUBRC LIKE SY-SUBRC.

  LOOP AT IT_SELECTED.
     L_TABIX = SY-TABIX.

     SELECT SINGLE * FROM ZTBLINR  WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
                                   AND   ZFBTSEQ EQ IT_SELECTED-ZFBTSEQ.
     WL_SUBRC = SY-SUBRC.

     IF PA_LOCK EQ 'L'.
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLINOU'
           EXPORTING
               ZFBLNO                =     IT_SELECTED-ZFBLNO
               ZFBTSEQ               =     IT_SELECTED-ZFBTSEQ
           EXCEPTIONS
               OTHERS        = 1.

       IF SY-SUBRC <> 0.
          MESSAGE I510 WITH SY-MSGV1 '반입예정정보 Document'
                                IT_SELECTED-ZFBLNO
                                IT_SELECTED-ZFBTSEQ
                       RAISING DOCUMENT_LOCKED.
          DELETE IT_SELECTED INDEX L_TABIX.
          CONTINUE.
       ENDIF.

       IF WL_SUBRC EQ 0.
          CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLINR'
               EXPORTING
                   ZFBLNO                =     IT_SELECTED-ZFBLNO
                   ZFBTSEQ               =     IT_SELECTED-ZFBTSEQ
               EXCEPTIONS
                   OTHERS        = 1.

          IF SY-SUBRC EQ  0.
             IT_SELECTED-ZFLOCK = 'Y'.
             MODIFY IT_SELECTED INDEX L_TABIX.
          ELSE.
             CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINOU'
                  EXPORTING
                     ZFBLNO                 =     IT_SELECTED-ZFBLNO
                     ZFBTSEQ                =     IT_SELECTED-ZFBTSEQ.

             MESSAGE I510 WITH SY-MSGV1 '반입정보 Document'
                                IT_SELECTED-ZFBLNO
                                IT_SELECTED-ZFBTSEQ
                       RAISING DOCUMENT_LOCKED.
             DELETE IT_SELECTED INDEX L_TABIX.
             CONTINUE.
          ENDIF.

       ENDIF.

     ELSEIF PA_LOCK EQ 'U'.
       CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINOU'
            EXPORTING
                ZFBLNO                 =     IT_SELECTED-ZFBLNO
                ZFBTSEQ                =     IT_SELECTED-ZFBTSEQ.

       IF IT_SELECTED-ZFLOCK = 'Y'.
          CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLINR'
               EXPORTING
                   ZFBLNO                 =     IT_SELECTED-ZFBLNO
                   ZFBTSEQ                =     IT_SELECTED-ZFBTSEQ.
       ENDIF.
     ENDIF.
  ENDLOOP.
ENDFORM.                    " ZTBLINOU_LOCK_EXEC
*&---------------------------------------------------------------------*
*&      Form  P2000_EDI_DATA_CHECK
*&---------------------------------------------------------------------*
FORM P2000_EDI_DATA_CHECK.
* 반입예정정보 조?
  CLEAR : ZTBLINOU.
  SELECT SINGLE * FROM ZTBLINOU WHERE ZFBLNO  EQ ZTBLINR-ZFBLNO
                                AND   ZFBTSEQ EQ ZTBLINR-ZFBTSEQ.

  ZTBLINR-ZFEDICK = 'O'.
* 반입신고번?
  IF ZTBLINR-ZFINRNO IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 전자문서기?
  IF ZTBLINR-ZFEDINF IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 반입?
  IF ZTBLINR-ZFINDT  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 반입시?
  IF ZTBLINR-ZFINTM  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 신고지 세?
  IF ZTBLINR-ZFINRC  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 세관의 담당 과부?
  IF ZTBLINR-ZFINRCD IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 화물반입유형구?
  IF ZTBLINR-ZFINTY  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 분할반입구?
  IF ZTBLINR-ZFPRIN  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 분할반입차?
  IF NOT ( ZTBLINR-ZFPRIN IS INITIAL OR ZTBLINR-ZFPRIN EQ 'A' ).
     IF ZTBLINR-ZFPINS IS INITIAL.
        ZTBLINR-ZFEDICK = 'X'.   EXIT.
     ENDIF.
  ENDIF.
* 반입정보의 적하목록관리번?
  IF ZTBLINOU-ZFGMNO IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 반입정보의 MSN
  IF ZTBLINOU-ZFMSN  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 반입정보의 HSN
  IF ZTBLINOU-ZFMSN  IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 반입정보의 반입근거번?
  IF ZTBLINOU-ZFBTRNO IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 포장개수 단?
  IF ZTBLINR-ZFPKCNM IS INITIAL.
     ZTBLINR-ZFEDICK = 'X'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_EDI_DATA_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_EDI_CREATE
*&---------------------------------------------------------------------*
FORM P2000_EDI_CREATE.
DATA : W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO.
*       W_ZFDHDDB         LIKE   ZTDHF1-ZFDHDDB.

  W_ZFCDDOC = 'CUSCAR'.
*-----------------------------------------------------------------------
*>>> 2000/12/27 KSB 수정
  PERFORM   P1000_GET_EDI_INDICATE(SAPMZIM01)
                          USING  ZTBLINR-ZFINRC
                                 W_ZFDHSRO.
*  W_ZFDHSRO = 'KCS0104'.           " 식별자.
*-----------------------------------------------------------------------
  W_ZFDHREF = ZTBLINR-ZFINRNO.     " 참조번호 ( 반입신고번호 )
*  W_ZFDHDDB = ''.                  " 부?
  W_ZFDHENO = ZTBLINR-ZFDOCNO.     " 문서번호.
*
  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
       EXPORTING
             W_ZFCDDOC    =   W_ZFCDDOC
             W_ZFDHSRO    =   W_ZFDHSRO
             W_ZFDHREF    =   W_ZFDHREF
*             W_ZFDHDDB    =   W_ZFDHDDB
             W_BUKRS        =   ZTBLINR-BUKRS
       CHANGING
             W_ZFDHENO    =   W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4
             NO_TYPE      =   8.

  CASE SY-SUBRC.
     WHEN  4.    MESSAGE I118 WITH   W_ZFDHENO.   EXIT.
     WHEN  8.    MESSAGE I117 WITH   W_ZFCDDOC.   EXIT.
  ENDCASE.

*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CALL FUNCTION 'ZIM_CUSCAR_EDI_SEND'
       EXPORTING
             W_ZFBLNO     =    ZTBLINR-ZFBLNO
             W_ZFBTSEQ    =    ZTBLINR-ZFBTSEQ
             W_ZFDHENO    =    W_ZFDHENO
       EXCEPTIONS
             DB_ERROR     =   4.

  CASE SY-SUBRC.
     WHEN  4.    MESSAGE I118 WITH   W_ZFDHENO.    EXIT.
     WHEN  8.    MESSAGE I117 WITH   W_ZFCDDOC.    EXIT.
  ENDCASE.

* 문서 STATUS 변?
  ZTBLINR-ZFDOCST = 'R'.
  ZTBLINR-ZFDOCNO = W_ZFDHENO.

  UPDATE ZTBLINR.

ENDFORM.                    " P2000_EDI_CREATE
