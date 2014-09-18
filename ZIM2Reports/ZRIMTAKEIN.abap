*&---------------------------------------------------------------------*
*& Report  ZRIMTAKEIN                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 반입관리 Multi-Create Report Program                  *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.02.21                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :  반입신고번호를 일괄적으로 채번 및 관리한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용] 이채경 2001.08.29
*&  엘지화학 프로세스 추가: B/L 등록후 반입예정정보 반입신고 동시 생성.
*&  ZTIMIMG00 반입예정정보 사용지시자 체크 후
*& 어느 프로세스를 탈 것인지 결정한다. 선택 조건또한 이 사항에 따라 변한
*& 다.
*&---------------------------------------------------------------------*
REPORT  ZRIMTAKEIN   MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

DATA :  MAX_LINSZ TYPE SY-LINSZ.
*-----------------------------------------------------------------------
* 반입예정/반입/반출 INTERNAL TABLE
*-----------------------------------------------------------------------

DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFREBELN   LIKE ZTBL-ZFREBELN,
       ZFBNDT     LIKE ZTBL-ZFBNDT,
       ZFWERKS    LIKE ZTBL-ZFWERKS,
       ZFCAGTY    LIKE ZTBL-ZFCAGTY,
       CAGTY      LIKE DD07T-DDTEXT,
       ZFTRTEC    LIKE ZTBL-ZFTRTEC,
       ZFETA      LIKE ZTBL-ZFETA,
       ZFETD      LIKE ZTBL-ZFETD,              "
       ZFBLSDT    LIKE ZTBL-ZFBLSDT,
       ZFVIA      LIKE ZTBL-ZFVIA,
       VIA        LIKE DD07T-DDTEXT,
       ZFSHTY     LIKE ZTBL-ZFSHTY,
       ZFBLST     LIKE ZTBL-ZFBLST,            " B/L 상태.
       BLST       LIKE DD07T-DDTEXT,
       IMTRD      LIKE ZTBL-IMTRD,             " 수입자구분.
       TRD        LIKE DD07T-DDTEXT,
       SHTY       LIKE DD07T-DDTEXT,
       ZFTOWT     LIKE ZTBL-ZFTOWT,
       ZFTOWTM    LIKE ZTBL-ZFTOWTM,
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
       ZFABNARC   LIKE ZTBLINOU-ZFABNARC,       " 도착보세구역 관리코?
       W_GB07(1)  TYPE C VALUE ';',
       ZFYR       LIKE ZTBLINR-ZFYR,            " 연도.
       W_GB08(1)  TYPE C VALUE ';',
       ZFSEQ      LIKE ZTBLINR-ZFSEQ,           " 일련번호.
       W_GB09(1)  TYPE C VALUE ';',
       ZFPKCN     LIKE ZTBLINOU-ZFPKCN,         " 포장수량.
       W_GB10(1)  TYPE C VALUE ';',
       ZFPKCNM    LIKE ZTBLINOU-ZFPKCNM,        " 포장수량 단위.
       W_GB11(1)  TYPE C VALUE ';',
       ZFWEIG     LIKE ZTBLINOU-ZFWEIG,         " 중량.
       W_GB12(1)  TYPE C VALUE ';',
       ZFWEINM    LIKE ZTBLINOU-ZFWEINM,        " 중량단위.
       W_GB13(1)  TYPE C VALUE ';',
       ZFBTRNO    LIKE ZTBLINOU-ZFBTRNO,        " 보세운송 신고번호.
       W_GB14(1)  TYPE C VALUE ';',
       ZFGMNO     LIKE ZTBLINOU-ZFGMNO,         " 적하목록 관리번호.
       W_GB15(1)  TYPE C VALUE ';',
       ZFGSNM     LIKE ZTBLINOU-ZFGSNM,         " 송하인.
       W_GB16(1)  TYPE C VALUE ';',
       ZFTDDT     LIKE ZTBLINOU-ZFTDDT,         " 운송기한일.
       W_GB17(1)  TYPE C VALUE ';',
       ZFRCDT     LIKE ZTBLINOU-ZFRCDT,         " 수신ㅓ일.
       W_GB18(1)  TYPE C VALUE ';',
       ZFINDT     LIKE ZTBLINR-ZFINDT,          " 반입일(인수일자)
       W_GB19(1)  TYPE C VALUE ';',
       ZFINTM     LIKE ZTBLINR-ZFINTM,          " 반입시간.
       W_GB20(1)  TYPE C VALUE ';',
       ZFBINYN    LIKE ZTBLINOU-ZFBINYN,        " 반입신고여부.
       W_GB21(1)  TYPE C VALUE ';',
       WL_TEXT12(12),
       W_GB22(1)  TYPE C VALUE ';',
       ZFPKCN1    LIKE ZTBLINOU-ZFWEIG,         " 포장수량.
       ZFMBLNO    LIKE ZTBL-ZFMBLNO,
       W_GB23(1)  TYPE C VALUE ';',
       BUKRS      LIKE ZTBL-BUKRS.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMBLTOP01.    " B/L 관련 Data Define용 Include

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?


*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS: S_EBELN  FOR ZTBL-ZFREBELN       " 대표 P/O NO
                   MODIF ID  BL,
                   S_BLNO    FOR ZTBLINOU-ZFBLNO    " B/L 관리번?
                   MODIF ID COM,
                   S_HBLNO   FOR ZTBL-ZFHBLNO       " House B/L No.
                   MODIF ID COM,
                   S_BNARCD  FOR ZTBL-ZFBNARCD  " 도착?
                   MODIF ID COM,
                   S_DNARCD  FOR ZTBLINOU-ZFDBNARC  " 발송?
                   MODIF ID INO,
                   S_ZFTRCK  FOR ZTBL-ZFTRCK       " TRUCKER
                   MODIF ID INO,
                   S_VIA     FOR  ZTBL-ZFVIA       " VIA
                   MODIF ID BL,
                   S_FORD    FOR ZTBL-ZFFORD       " Forwarder
                   MODIF ID BL,
                   S_CAGTY   FOR ZTBL-ZFCAGTY      " Cargo Type
                   MODIF ID BL.
   SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-002, POSITION 1.
     SELECTION-SCREEN : POSITION 33.
     PARAMETERS : P_ZFGMNO LIKE ZTBL-ZFGMNO MODIF ID COM."적하목록
     SELECTION-SCREEN : COMMENT 45(1) TEXT-032, POSITION 47.
     PARAMETERS : P_ZFMSN  LIKE ZTBL-ZFMSN MODIF ID COM.      " MSN
     SELECTION-SCREEN : COMMENT 52(1) TEXT-032, POSITION 54.
     PARAMETERS : P_ZFHSN  LIKE ZTBL-ZFHSN MODIF ID COM.      " HSN
   SELECTION-SCREEN END OF LINE.
   SELECT-OPTIONS: S_RCDT    FOR ZTBLINOU-ZFRCDT    " 수신일?
                   MODIF ID INO,
                   S_INDT    FOR ZTBLINR-ZFINDT     " 반입일?
                   MODIF ID INO,
                   S_TDDT    FOR ZTBLINOU-ZFTDDT    " 운송기?
                   MODIF ID INO.
*                   S_USCD    FOR ZTBLINR-ZFUSCD.    " 용도 구?
   PARAMETERS :    P_BNAME   LIKE USR02-BNAME        " 입력?
                   MODIF ID COM.
   SELECTION-SCREEN SKIP 1.                         " 1 LINE SKIP
   PARAMETERS : P_OPEN MODIF ID COM  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.
*-----------------------------------------------------------------------
* 반입상태 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_STATUS FOR ZTBLINOU-ZFBINYN
                 MODIF ID INO NO INTERVALS NO-DISPLAY.

AT SELECTION-SCREEN OUTPUT.
   PERFORM  P2000_SCREEN_FIELD_SET.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.
* Title Text Write
TOP-OF-PAGE.
   IF W_ZFINOU EQ 'X'.
      MAX_LINSZ = 100.
      PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
   ELSE.
      MAX_LINSZ = 120.
      PERFORM   P3000_TITLE_WRITE2.
   ENDIF.
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
* 반입예정정보.
   IF W_ZFINOU EQ 'X'.
      PERFORM   P1000_GET_ZVBL_INOU       USING   W_ERR_CHK.
   ELSE.
      PERFORM   P1000_GET_ZTBL_INOU       USING   W_ERR_CHK.
   ENDIF.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   W_OK_CODE_OLD = SY-UCOMM.

   CASE SY-UCOMM.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFGMNO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP' OR 'DIS1'.         " 반입예정정보/반입신고.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.

            PERFORM P2000_SHOW_DOC  USING IT_SELECTED-ZFBLNO
                                          IT_SELECTED-ZFBTSEQ.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'INOU'.          " 반입신고 생?
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
* Message Box
            PERFORM P2000_POPUP_MESSAGE.

            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
* 반입신고 생?
               PERFORM P3000_INPUT_DATA.
* 반입예정정보 SELECT
               PERFORM   P1000_GET_ZVBL_INOU       USING   W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.
                  LEAVE TO TRANSACTION 'ZIMI5'.   EXIT.
               ENDIF.
               PERFORM RESET_LIST.
            ENDIF.
         ENDIF.
      WHEN 'INCR'.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
* Message Box
            PERFORM P2000_POPUP_MESSAGE.
* 반입정보 생성.
            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
               PERFORM P3000_INPUT_DATA.
               PERFORM   P1000_GET_ZVBL_INOU       USING   W_ERR_CHK.
               IF W_ERR_CHK EQ 'Y'.
                  LEAVE TO TRANSACTION 'ZIMI5'.   EXIT.
               ENDIF.
               PERFORM RESET_LIST.
            ENDIF.
         ENDIF.

      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* 반입예정정보 SELECT
           PERFORM   P1000_GET_ZVBL_INOU       USING   W_ERR_CHK.
           IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
           PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  CLEAR ZTIMIMG00.
  SELECT SINGLE ZFINOU INTO W_ZFINOU
         FROM ZTIMIMG00.
  SET  TITLEBAR 'ZIMI5'.          " TITLE BAR

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
  NEW-LINE. ULINE AT 1(MAX_LINSZ).
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '화물관리No.',  SY-VLINE,
            '       B/L Number      ',  SY-VLINE,
            '     수    량     ',  SY-VLINE,
            '     중    량     ',  SY-VLINE,
            ' 인수시간 ',  SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',   SY-VLINE NO-GAP,
            '보세구역입고', SY-VLINE,
*            ' 운송기한     수신일자 ',  SY-VLINE,
            ' 수신일자     운송기한 ',  SY-VLINE,
            '   보세운송번호   ', SY-VLINE,
            '      송 화 인    ',  SY-VLINE NO-GAP,
            'B/L Doc. No.'         NO-GAP,  SY-VLINE.

  NEW-LINE. ULINE AT 1(MAX_LINSZ).
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
  IF P_ZFGMNO IS INITIAL.      P_ZFGMNO = '%'.      ENDIF.
  IF P_ZFMSN  IS INITIAL.      P_ZFMSN  = '%'.      ENDIF.
  IF P_ZFHSN  IS INITIAL.      P_ZFHSN  = '%'.      ENDIF.

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

   PERFORM P2000_SET_STATUS_SCR_DISABLE. " PF-STATUS DISABLE SETTING.


   SET PF-STATUS 'ZIMI5' EXCLUDING IT_EXCL." GUI STATUS SETTING
   SET  TITLEBAR 'ZIMI5'.                  " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      IF W_ZFINOU EQ 'X'.
         PERFORM P3000_LINE_WRITE.
      ELSE.
         PERFORM P3000_LINE_WRITE2.
      ENDIF.
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
  IF W_ZFINOU EQ 'X'.
     PERFORM   P3000_TITLE_WRITE.     " 해더 출력...
  ELSE.
     PERFORM   P3000_TITLE_WRITE2.      " 해더 출력...
  ENDIF.
  PERFORM   P3000_DATA_WRITE      USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFBLNO  LIKE ZTBLINOU-ZFBLNO,
        ZFBTSEQ LIKE ZTBLINOU-ZFBTSEQ.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFBLNO,
         IT_TAB-ZFBTSEQ  TO ZFBTSEQ.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
             IT_TAB-ZFBTSEQ  TO IT_SELECTED-ZFBTSEQ.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFBLNO  TO IT_SELECTED-ZFBLNO,
             ZFBTSEQ TO IT_SELECTED-ZFBTSEQ.

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
      NEW-LINE. ULINE AT 1(MAX_LINSZ).
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

  IF SY-UCOMM EQ 'FRGS' OR SY-UCOMM EQ 'FRGR'.
     IF IT_TAB-MARK EQ 'X' AND IT_TAB-UPDATE_CHK EQ 'U'.
        MARKFIELD = 'X'.
     ELSE.
        CLEAR : MARKFIELD.
     ENDIF.
  ENDIF.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  MOVE : IT_TAB-ZFPKCN   TO    IT_TAB-ZFPKCN1.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX
       COLOR COL_BACKGROUND INTENSIFIED OFF,
       SY-VLINE,
       IT_TAB-ZFGMNO,
       SY-VLINE,
       IT_TAB-ZFHBLNO NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFPKCN1 UNIT IT_TAB-ZFPKCNM NO-GAP,
       IT_TAB-ZFPKCNM,
       SY-VLINE NO-GAP,
       IT_TAB-ZFWEIG UNIT IT_TAB-ZFWEINM NO-GAP,
       IT_TAB-ZFWEINM NO-GAP,
       SY-VLINE,
       IT_TAB-ZFINDT,
       SY-VLINE.
  HIDE: W_LIST_INDEX, IT_TAB.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, 5 SY-VLINE, IT_TAB-WL_TEXT12 NO-GAP,
       SY-VLINE,
       IT_TAB-ZFRCDT,
       34 IT_TAB-ZFTDDT,
*       34 IT_TAB-ZFRCDT,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBTRNO NO-GAP,
       SY-VLINE,
       IT_TAB-ZFGSNM(19) NO-GAP,
       SY-VLINE,
       IT_TAB-ZFBLNO,
       SY-VLINE.
  HIDE: W_LIST_INDEX, IT_TAB.

* CASE IT_TAB-ZFBINYN.
*    WHEN 'X'.
*       FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
*    WHEN ' '.
*       FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
*    WHEN OTHERS.
*       FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
* ENDCASE.
*
* WRITE : IT_TAB-ZFBINYN NO-GAP, SY-VLINE NO-GAP.
  NEW-LINE. ULINE AT 1(MAX_LINSZ).
* stored value...
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.
DATA : L_TEXT(255) TYPE C.

   IF W_OK_CODE_OLD EQ 'INCR'.
      L_TEXT = '반입 및 EDI 생성 작업을 계속 진행하시겠습니까?'.
   ELSE.
      L_TEXT = '반입 생성 작업을 계속 진행하시겠습니까?'.
   ENDIF.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = '저장 확인'
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = L_TEXT
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
FORM P1000_GET_ZVBL_INOU USING    W_ERR_CHK.

  DATA : WL_TABIX   LIKE   SY-TABIX.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
                               FROM  ZVBL_INOU
                               WHERE ZFABNARC   IN     S_BNARCD
                               AND   ZFBLNO     IN     S_BLNO
                               AND   ZFHBLNO    IN     S_HBLNO
                               AND   ZFRCDT     IN     S_RCDT
*                              AND   ZFINDT     IN     S_INDT
                               AND   ZFTDDT     IN     S_TDDT
                               AND   ZFBINYN    IN     S_STATUS
*                              AND   ZFUSCD     IN     S_USCD
                               AND   ZFGMNO     LIKE   P_ZFGMNO
                               AND   ZFMSN      LIKE   P_ZFMSN
                               AND   ZFHSN      LIKE   P_ZFHSN
                               AND   ERNAM      LIKE   P_BNAME.

  IF SY-SUBRC NE 0 AND SY-UCOMM NE 'INOU'.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S046.    EXIT.
  ENDIF.

  LOOP AT IT_TAB.

     WL_TABIX = SY-TABIX.

     CLEAR : ZTBLINR.
     SELECT SINGLE * FROM ZTBLINR WHERE ZFBLNO  EQ IT_TAB-ZFBLNO
                                  AND   ZFBTSEQ EQ IT_TAB-ZFBTSEQ.

     MOVE : ZTBLINR-ZFINRNO     TO    IT_TAB-ZFINRNO,
            ZTBLINR-ZFYR        TO    IT_TAB-ZFYR,
            ZTBLINR-ZFSEQ       TO    IT_TAB-ZFSEQ,
            ZTBLINR-ZFINDT      TO    IT_TAB-ZFINDT,
            ZTBLINR-ZFINTM      TO    IT_TAB-ZFINTM.

     IF NOT IT_TAB-ZFINRNO IS INITIAL.
        CONCATENATE IT_TAB-ZFABNARC '-' IT_TAB-ZFYR '-'
                      IT_TAB-ZFSEQ INTO IT_TAB-WL_TEXT12.
     ELSE.
        CLEAR : IT_TAB-WL_TEXT12.
     ENDIF.
*>> HOUSE BL NO 없으면 MASTER BL NO DISPLAY.
     IF IT_TAB-ZFHBLNO IS INITIAL.
        MOVE  IT_TAB-ZFMBLNO  TO  IT_TAB-ZFHBLNO.
     ENDIF.
     MODIFY IT_TAB  INDEX WL_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZVBL_INOU
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_DOC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_DOC USING    P_ZFBLNO P_ZFBTSEQ.

   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
   SET PARAMETER ID 'ZPBTSEQ'  FIELD P_ZFBTSEQ.
   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
   EXPORT 'ZPBTSEQ'       TO MEMORY ID 'ZPBTSEQ'.

   IF SY-UCOMM EQ 'DISP'.               " 반입예정정?
      CALL TRANSACTION 'ZIMI3' AND SKIP  FIRST SCREEN.
   ELSEIF SY-UCOMM EQ 'DIS1'.           " 반입신?
      CALL TRANSACTION 'ZIMI8' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_DOC
*&---------------------------------------------------------------------*
*&      Form  P3000_INPUT_DATA
*&---------------------------------------------------------------------*
FORM P3000_INPUT_DATA.
DATA : WL_SUBRC    LIKE SY-SUBRC.
DATA : W_ZFBTSEQ   LIKE ZTBLINR-ZFBTSEQ,
       L_ZFSEQ     LIKE ZTBLINR-ZFSEQ.

*-----------------------------------------------------------------------
*  LOCK OBJECT 추가할 부?
*-----------------------------------------------------------------------
  PERFORM   ZTBLINOU_LOCK_EXEC  USING   'L'.
*-----------------------------------------------------------------------

 LOOP  AT   IT_SELECTED.
    SELECT SINGLE * FROM ZTBL     WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO.

    SELECT SINGLE * FROM ZTBLINOU WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
                                  AND   ZFBTSEQ EQ IT_SELECTED-ZFBTSEQ.
    IF SY-SUBRC NE 0.
       MESSAGE E044 WITH IT_SELECTED-ZFBLNO IT_SELECTED-ZFBTSEQ.
    ENDIF.

    SELECT SINGLE * FROM ZTBLINR  WHERE ZFBLNO  EQ IT_SELECTED-ZFBLNO
                                  AND   ZFBTSEQ EQ IT_SELECTED-ZFBTSEQ.
    IF SY-SUBRC NE 0.
       W_STATUS = 'C'.
       CLEAR  *ZTBLINR.
    ELSE.
       W_STATUS = 'U'.
       *ZTBLINR = ZTBLINR.
    ENDIF.

*-----------------------------------------------------------------------
* DATA MOVE
*-----------------------------------------------------------------------
    MOVE : SY-MANDT             TO     ZTBLINR-MANDT,    " CLIENT
           ZTBLINOU-ZFBLNO      TO     ZTBLINR-ZFBLNO,   " B/L 관리번?
           ZTBLINOU-ZFBTSEQ     TO     ZTBLINR-ZFBTSEQ,  " 보세운송 일?
           ZTBLINOU-ZFABNAR     TO     ZTBLINR-ZFABNAR,  " 보세구역 CD
           ZTBLINOU-ZFABNARC    TO     ZTBLINR-ZFBNARCD, " 보세구역 ID
           SY-DATUM+2(2)        TO     ZTBLINR-ZFYR,     " 연?
           '9'                  TO     ZTBLINR-ZFEDINF,  " 전자문서기?
           '33'                 TO     ZTBLINR-ZFINRCD,  " 신고?
           '20'                 TO     ZTBLINR-ZFINTY,   " 반입유?
           'OK'                 TO     ZTBLINR-ZFINACD,  " 반입사고유?
           'A'                  TO     ZTBLINR-ZFPRIN,   " 분할반입구?
           ZTBLINOU-ZFPKCNM     TO     ZTBLINR-ZFCT,     " 개수단?
           SY-DATUM             TO     ZTBLINR-ZFINDT,   " 반입?
           SY-UZEIT             TO     ZTBLINR-ZFINTM,   " 반입시?
           SY-UNAME             TO     ZTBLINR-ZFGINM,   " 반입담당?
           ZTBLINOU-ZFABNAR(3)  TO     ZTBLINR-ZFINRC,   " 신고지세?
           ZTBLINOU-ZFPKCN      TO     ZTBLINR-ZFPKCN,   " 포장수?
           ZTBLINOU-ZFPKCNM     TO     ZTBLINR-ZFPKCNM,  " 포장수량 단?
           ZTBLINOU-ZFWEIG      TO     ZTBLINR-ZFINWT,   " 반입중?
           ZTBLINOU-ZFWEINM     TO     ZTBLINR-ZFKG,     " 무게단?
           'N'                  TO     ZTBLINR-ZFDOCST,  " 문서상?
           'N'                  TO     ZTBLINR-ZFEDIST,  " EDI 상?
           'X'                  TO     ZTBLINR-ZFEDICK,  " EDI CHECK
           SY-UNAME             TO     ZTBLINR-ZFGIRNM.  " 반입담당?
*           SY-DATUM             TO     ZTBLINR-UDAT,     " UPDATE DATE
*           SY-UNAME             TO     ZTBLINR-UNAM.     " UPDATE NAME
*    IF ZTBLINR-CDAT IS INITIAL.
*       MOVE : SY-DATUM          TO     ZTBLINR-CDAT,     " CREATE DATE
*              SY-UNAME          TO     ZTBLINR-ERNAM.    " CREATE NAME
*    ENDIF.
* 일련번호.
    CLEAR : L_ZFSEQ.
    SELECT MAX( ZFSEQ ) INTO   L_ZFSEQ   FROM  ZTBLINR
                        WHERE  ZFYR      EQ    ZTBLINR-ZFYR
                        AND    ZFBNARCD  EQ    ZTBLINR-ZFBNARCD.
    IF L_ZFSEQ IS INITIAL.
       L_ZFSEQ = '000001'.
    ELSE.
       L_ZFSEQ = L_ZFSEQ + 1.
    ENDIF.

    ZTBLINR-ZFSEQ = L_ZFSEQ.

    CONCATENATE ZTBLINOU-ZFABNAR ZTBLINR-ZFYR ZTBLINR-ZFSEQ
                INTO ZTBLINR-ZFINRNO.

*-----------------------------------------------------------------------
* 2000/06/17 안덕기 과장 DEFINE
     CASE ZTBL-ZFMATGB.            " 자재구?
        WHEN '1'.    ZTBLINR-ZFUSCD = 'P'.   " 수출용 원자?
        WHEN '2'.    ZTBLINR-ZFUSCD = 'P'.   " LOCAL
        WHEN '3'.    ZTBLINR-ZFUSCD = 'C'.   " 내수용 원자?
        WHEN '4'.    ZTBLINR-ZFUSCD = 'C'.   " 시설?
        WHEN '5'.    ZTBLINR-ZFUSCD = 'C'.   " 상?
        WHEN OTHERS. ZTBLINR-ZFUSCD = 'C'.   " 무?
     ENDCASE.
* 이고반입일 경우.  ==> 반입유형을 '21'로 SET...
     IF ZTBLINR-ZFBTSEQ GT 1.
        W_ZFBTSEQ = ZTBLINR-ZFBTSEQ - 1.
        SELECT SINGLE * FROM ZTBLOUR
                        WHERE ZFBLNO   EQ ZTBL-ZFBLNO
                        AND   ZFBTSEQ  EQ W_ZFBTSEQ.

        IF ZTBLOUR-ZFOUTY EQ '61'.     " 이고반출일 경우...
           ZTBLINR-ZFINTY = '21'.
        ENDIF.
     ENDIF.
* EDI 문서 검?
     PERFORM  P2000_EDI_CHECK.

*-----------------------------------------------------------------------
* ZTBLINR INSERT / UPDATE
*-----------------------------------------------------------------------
    CALL FUNCTION 'ZIM_BLINR_DOC_MODIFY'
         EXPORTING
               W_OK_CODE           =   W_OK_CODE
               ZFBLNO              =   ZTBLINR-ZFBLNO
               ZFBTSEQ             =   ZTBLINR-ZFBTSEQ
               ZFSTATUS            =   W_STATUS
               W_ZTBLINR           =   ZTBLINR
               W_ZTBLINR_OLD       =  *ZTBLINR
         EXCEPTIONS
                ERROR_UPDATE       =   4
                NOT_MODIFY         =   8.

    IF SY-SUBRC EQ 4.
       MESSAGE E047  WITH  ZTBLINR-ZFBLNO   ZTBLINR-ZFBTSEQ.
    ENDIF.
* 반입신고여?
*    ZTBLINOU-ZFBINYN  =   'X'.
*    UPDATE ZTBLINOU.
*    IF SY-SUBRC NE 0.
*       MESSAGE E048  WITH  ZTBLINR-ZFBLNO   ZTBLINR-ZFBTSEQ.
*    ENDIF.

*-----------------------------------------------------------------------
* ZTBLINOU UPDATE
*-----------------------------------------------------------------------
    IF W_OK_CODE_OLD EQ 'INCR'.
       PERFORM  P2000_EDI_CREATE.
    ENDIF.
    W_UPDATE_CNT = W_UPDATE_CNT + 1.

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
FORM ZTBLINOU_LOCK_EXEC USING    PA_LOCK.
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
*&      Form  P2000_EDI_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EDI_CHECK.
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


ENDFORM.                    " P2000_EDI_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_EDI_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EDI_CREATE.

DATA : W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO.
*       W_ZFDHDDB         LIKE   ZTDHF1-ZFDHDDB.

  W_ZFCDDOC = 'CUSCAR'.
*-----------------------------------------------------------------------
*>>> 2000/12/27 KSB 수정
  PERFORM   P1000_GET_EDI_INDICATE
                          USING  ZTBLINR-ZFINRC
                                 W_ZFDHSRO.
  IF W_ZFDHSRO IS INITIAL.
     EXIT.
  ENDIF.
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
             W_BUKRS        =   ZTBLINR-BUKRS
*             W_ZFDHDDB    =   W_ZFDHDDB
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
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_EDI_INDICATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTBLINR_ZFINRC  text
*      -->P_W_ZFDHSRO  text
*----------------------------------------------------------------------*
FORM P1000_GET_EDI_INDICATE USING    P_ZFINRC
                                     P_ZFDHSRO.
  CLEAR : P_ZFDHSRO.

  IF P_ZFINRC IS INITIAL.
     MESSAGE I167 WITH '세관코드'.
     EXIT.
  ELSE.
     SELECT SINGLE * FROM ZTIMIMG02
                     WHERE ZFCOTM EQ P_ZFINRC.
     IF SY-SUBRC NE 0.
        MESSAGE I231 WITH P_ZFINRC.
        EXIT.
     ELSE.
        SELECT SINGLE * FROM LFA1
                        WHERE LIFNR EQ ZTIMIMG02-ZFVEN.
        IF SY-SUBRC EQ 0.
           IF LFA1-BAHNS IS INITIAL.
              MESSAGE I198 WITH ZTIMIMG02-ZFVEN.
              EXIT.
           ENDIF.
        ELSE.
           MESSAGE I020 WITH ZTIMIMG02-ZFVEN.
           EXIT.
        ENDIF.
     ENDIF.
  ENDIF.

  P_ZFDHSRO = LFA1-BAHNS.

ENDFORM.                    " P1000_GET_EDI_INDICATE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
FORM P2000_SET_STATUS_SCR_DISABLE.

  READ TABLE IT_TAB INDEX 1.
  SELECT SINGLE *
      FROM ZTIMIMGTX
      WHERE BUKRS  = IT_TAB-BUKRS.
  IF ZTIMIMGTX-CUSINF EQ SPACE.
     MOVE 'INCR' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " 반입&EDI.
  ENDIF.
  IF W_ZFINOU EQ SPACE AND  P_OPEN = SPACE.
     MOVE 'DISP' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " 반입예정정보.
     MOVE 'DIS1' TO IT_EXCL-FCODE. APPEND IT_EXCL.   " 반입신고.
  ENDIF.

ENDFORM.                    " P2000_SET_STATUS_SCR_DISABLE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTBL_INOU
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTBL_INOU USING   W_ERR_CHK.

  RANGES : R_ZFRPTTY FOR  ZTBL-ZFRPTTY  OCCURS 5.
  MOVE :    'I'       TO  R_ZFRPTTY-SIGN,
            'EQ'      TO  R_ZFRPTTY-OPTION,
            'B'       TO  R_ZFRPTTY-LOW,
            SPACE     TO  R_ZFRPTTY-HIGH.
  APPEND R_ZFRPTTY.
  MOVE :    'I'       TO  R_ZFRPTTY-SIGN,
            'EQ'      TO  R_ZFRPTTY-OPTION,
            'N'       TO  R_ZFRPTTY-LOW,
            SPACE     TO  R_ZFRPTTY-HIGH.
  APPEND R_ZFRPTTY.


  DATA : WL_TABIX   LIKE   SY-TABIX.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
                              FROM  ZTBL
                             WHERE  ZFREBELN IN    S_EBELN
                               AND  ZFBLNO    IN     S_BLNO
                               AND  ZFHBLNO   IN     S_HBLNO
                               AND  ZFGMNO    LIKE   P_ZFGMNO
                               AND  ZFMSN     LIKE   P_ZFMSN
                               AND  ZFHSN     LIKE   P_ZFHSN
                               AND  ERNAM     LIKE   P_BNAME
                               AND  ZFRPTTY   IN    R_ZFRPTTY
                               AND  ZFTRCK    IN     S_ZFTRCK
                               AND  ZFVIA     IN     S_VIA
                               AND  ZFFORD    IN     S_FORD
                               AND  ZFCAGTY   IN     S_CAGTY.

  IF SY-SUBRC NE 0 AND SY-UCOMM NE 'INOU'.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S738.    EXIT.
  ENDIF.

  LOOP AT IT_TAB.
     WL_TABIX = SY-TABIX.
     CLEAR : ZTBLINR.
*>> 반입예정정보.
     SELECT SINGLE *
            FROM ZTBLINOU
           WHERE ZFBLNO   EQ IT_TAB-ZFBLNO.
     W_SUBRC = SY-SUBRC.
     IF P_OPEN = SPACE.
        IF W_SUBRC EQ 0.
           DELETE IT_TAB INDEX WL_TABIX.
           CONTINUE.
        ENDIF.
     ELSE.              " 반입신고 재생성시(반입-YES 반출 NO)
*>> 반입신고.
        SELECT SINGLE *
            FROM ZTBLINR
           WHERE ZFBLNO   EQ IT_TAB-ZFBLNO.

        IF SY-SUBRC EQ 0.
*>> 반출 데이타 체크.
           SELECT SINGLE *
              FROM ZTBLOUR
              WHERE ZFBLNO  = ZTBLINR-ZFBLNO
                AND ZFBTSEQ = ZTBLINR-ZFBTSEQ.
           IF SY-SUBRC EQ 0.
              DELETE IT_TAB INDEX WL_TABIX.
              CONTINUE.
           ENDIF.
        ELSE.
           DELETE IT_TAB INDEX WL_TABIX.
           CONTINUE.
        ENDIF.
     ENDIF.
*>> 후속작업이 진행된것 체크.
     CLEAR ZTIV.
     SELECT SINGLE *
         FROM ZTIV
         WHERE ZFBLNO = IT_TAB-ZFBLNO.
     IF ZTIV-ZFCLCD EQ 'A'.
         DELETE IT_TAB INDEX WL_TABIX.
         CONTINUE.
     ENDIF.
* DOMAIN.-----------------------------------------------------------
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCAGTY' IT_TAB-ZFCAGTY
                                   CHANGING IT_TAB-CAGTY.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDVIA' IT_TAB-ZFVIA
                                   CHANGING IT_TAB-VIA.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDSHTY' IT_TAB-ZFSHTY
                                   CHANGING  IT_TAB-SHTY.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDBLST' IT_TAB-ZFBLST
                                   CHANGING  IT_TAB-BLST.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDIMTRD' IT_TAB-IMTRD
                                   CHANGING  IT_TAB-TRD.
     MODIFY IT_TAB  INDEX WL_TABIX.
  ENDLOOP.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE = 0.
     W_ERR_CHK = 'Y'.  MESSAGE S738.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZTBL_INOU
*&---------------------------------------------------------------------*
*&      Form  P2000_SCREEN_FIELD_SET
*&---------------------------------------------------------------------*
FORM P2000_SCREEN_FIELD_SET.


*>> SCREEN MODIFY
   LOOP AT SCREEN.
     IF  W_ZFINOU EQ 'X'.
          CASE SCREEN-GROUP1.
               WHEN 'BL'.
                  SCREEN-INVISIBLE = '1'.
                  SCREEN-INPUT     = '0'.
                  MODIFY SCREEN.
               WHEN OTHERS.
            ENDCASE.

     ELSE.
         CASE SCREEN-GROUP1.
              WHEN 'INO'.
                  SCREEN-INVISIBLE = '1'.
                  SCREEN-INPUT     = '0'.
                  MODIFY SCREEN.
               WHEN OTHERS.
         ENDCASE.
     ENDIF.

   ENDLOOP.

ENDFORM.                    " P2000_SCREEN_FIELD_SET
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE2
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE2.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  '  [ B/L 미참조 반입신고내역 ]  '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /'Date : ', SY-DATUM.  ",  'Page : ', W_PAGE.
  NEW-LINE. ULINE AT 1(MAX_LINSZ).
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',
            SY-VLINE, (20) 'House B/L No',
            SY-VLINE ,(15) '대표 P/O No ',
            SY-VLINE ,(10) 'ETD',
            SY-VLINE ,(10) '보운일',
            SY-VLINE ,(10) 'Plant',
            SY-VLINE ,(10) 'Cargo Type',
            SY-VLINE ,(19) 'Package',  SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',
            SY-VLINE NO-GAP, (09)'수입구분' NO-GAP,
            SY-VLINE NO-GAP, (12)'B/L 상태'   NO-GAP,
            SY-VLINE, (15)'B/L관리번호',
            SY-VLINE, (10)'ETA',
            SY-VLINE, (10)'송부일',
            SY-VLINE, (10)'VIA',
            SY-VLINE, (10)'선적유형',
            SY-VLINE, (19)'Gross Weight',SY-VLINE.

  NEW-LINE. ULINE AT 1(MAX_LINSZ).
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE2
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE2
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE2.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  MOVE IT_TAB-ZFPKCN TO IT_TAB-ZFPKCN1.
  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX
       COLOR COL_BACKGROUND INTENSIFIED OFF,
        SY-VLINE,(20)IT_TAB-ZFHBLNO,  " HOUSE B/L NO
        SY-VLINE,(15)IT_TAB-ZFREBELN,
        SY-VLINE,(10) IT_TAB-ZFETD,   " ETD
        SY-VLINE,(10) IT_TAB-ZFBNDT,  " 보운?
        SY-VLINE,(10) IT_TAB-ZFWERKS, " PLANT
        SY-VLINE,(10) IT_TAB-CAGTY,   " Cargo Type
        SY-VLINE,(15) IT_TAB-ZFPKCN1 UNIT IT_TAB-ZFPKCNM, " PKG.
                 (03)IT_TAB-ZFPKCNM,
        SY-VLINE.
* hide
  HIDE:  IT_TAB.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',
          SY-VLINE ,(08)IT_TAB-TRD NO-GAP,  " 수입자구분.
          SY-VLINE NO-GAP,(12)IT_TAB-BLST NO-GAP, " B/L STATUS
          SY-VLINE,(15)IT_TAB-ZFBLNO,
          SY-VLINE,(10)IT_TAB-ZFETA,    " ETA
          SY-VLINE,(10)IT_TAB-ZFBLSDT,  "
          SY-VLINE,(10)IT_TAB-VIA,    " VIA
          SY-VLINE,(10)IT_TAB-SHTY,   " 선적유형.
          SY-VLINE,(15)IT_TAB-ZFTOWT UNIT IT_TAB-ZFTOWTM,
                   (03)IT_TAB-ZFTOWTM,
          SY-VLINE .
* stored value...
  HIDE IT_TAB.
  W_COUNT = W_COUNT + 1.
  NEW-LINE. ULINE AT 1(MAX_LINSZ).

ENDFORM.
