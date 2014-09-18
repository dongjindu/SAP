*&---------------------------------------------------------------------*
*& Report  ZRIMEDIUP1                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : READY KOREA LTD. EDI Document Receipt                 *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.08                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : READY Korea Interface용( EDI Document Receipt )
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMEDIUP1   MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

*TABLES: ZTDHF1.

*------ EDI
DATA  :  UPLOAD_PATH(300)     TYPE       C     " loading data
                              VALUE      '/ABAP/EDI/bin/skcimport'.
DATA  :  FILE_NAME(300)       TYPE       C.

DATA : W_OK_CODE    LIKE   SY-UCOMM,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF.
*       W_ZFDHDDB         LIKE   ZTDHF1-ZFDHDDB.

DATA  W_EDI_RECORD(65535).
DATA: BEGIN OF IT_TAB OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_TAB.

DATA: BEGIN OF IT_TAB1 OCCURS 0,
      ZFDHENO     LIKE     ZTDHF1-ZFDHENO,  " 문서관리번호.
      ZFDHDOC     LIKE     ZTDHF1-ZFDHDOC,  " 문서 종류.
      ZFFILE(100),
      END OF IT_TAB1.

DATA: BEGIN OF IT_SELECT OCCURS 0,
      ZFDHENO     LIKE     ZTDHF1-ZFDHENO,  " 문서관리번호.
      ZFDHDOC     LIKE     ZTDHF1-ZFDHDOC,  " 문서 종류.
      END OF IT_SELECT.

DATA: TEXT100(100),
      TEXT50(50).

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모?
INCLUDE   ZRIMBDCCOM.     " 수입의뢰 BDC 공통 Include

*-----------------------------------------------------------------------
* Selection Screen 절.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_DHENO FOR ZTDHF1-ZFDHENO,  " 문서관리번호.
                   S_DHDOC FOR ZTDHF1-ZFDHDOC.  " 전자문서 종류.
SELECTION-SCREEN END OF BLOCK B1.

*&---------------------------------------------------------------------*
*&  BDC MODE 선택 추가...
*&---------------------------------------------------------------------*
*SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-004.
*selection-screen: begin of line,  comment 1(13) text-021, position 1.
*      selection-screen: comment 32(1) text-022, position 34.
*         parameters j1 radiobutton group rad1.              " Display
*      selection-screen: comment 40(1) text-023, position 42.
*         parameters j2 radiobutton group rad1 default 'X'.  " Backgroud
*     selection-screen: comment 48(1) text-024, position 50.
*        parameters j3 radiobutton group rad1.               " Error
*selection-screen end of line.
*selection-screen: comment /1(79) text-025.
*selection-screen end of block b4.

* Screen Selection
*at selection-screen.
*   if j1 = 'X'. disp_mode = 'A'. endif.
*   if j2 = 'X'. disp_mode = 'N'. endif.
*   if j3 = 'X'. disp_mode = 'E'. endif.

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
* Import System Config Check
   PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 권한 검증.
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* UPLOAD용 FILENAME을 발췌하는 서브루틴.
   PERFORM   P1000_GET_UPLOAD_FILE     USING   W_ERR_CHK.
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
* SORT 선택.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택시...
*         W_FIELD_NM = 'ZFOPBN'.
*         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*         PERFORM HANDLE_SORT TABLES  IT_TAB
*                             USING   SY-UCOMM.
* 전체 선택 및 선택해제.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해체.
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP'.          " L/C 조회.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECT INDEX 1.
            PERFORM P2000_GET_DOC_KEY.
            PERFORM P2000_SHOW_DOCUMENT.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'EDII'.     " EDI RECEIPT.
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            TEXT50  = 'EDI File Receipt 확인'.
            TEXT100 = 'EDI File Receipt 작업을 계속 진행하시겠습니까?'.
            PERFORM P2000_POPUP_MESSAGE USING TEXT50 TEXT100.
            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경우.
               PERFORM P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
               LEAVE TO SCREEN 0.
            ENDIF.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
* UPLOAD용 FILENAME을 발췌하는 서브루틴.
         PERFORM   P1000_GET_UPLOAD_FILE     USING   W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
         PERFORM RESET_LIST.
      WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIME10'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
*  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ EDI RECEIPT DOCUMENT 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.

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

   if disp_mode ne 'N'.            " BACKGROUND이 아닐 경우.
      authority-check object 'ZM_BDC_MGT'
              id 'ACTVT' field '*'.
      if sy-subrc ne 0.
         message s960 with sy-uname 'BDC 관리자'.
         w_err_chk = 'Y'.   exit.
      ENDIF.
   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK           USING   W_ERR_CHK.
*  W_ERR_CHK = 'N'.
** Import Config Select
*  SELECT SINGLE * FROM ZTIMIMG00.
** Not Found
*  IF SY-SUBRC NE 0.
*     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
*  ENDIF.
*
*  IF ZTIMIMG00-ZFRECV IS INITIAL.
*     W_ERR_CHK = 'Y'.   MESSAGE S978.   EXIT.
*  ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIME10'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIME10'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   SORT IT_TAB1 BY ZFDHDOC ZFFILE ZFDHENO.
   CLEAR : IT_TAB1.

   LOOP AT IT_TAB1.
      W_LINE = W_LINE + 1.
*     PERFORM P2000_PAGE_CHECK.
*>>> File Name이 바뀔때마다.
      ON CHANGE OF IT_TAB1-ZFFILE.
         PERFORM P3000_FILE_NAME_WRITE.
      ENDON.
*>>> Line Record Write.
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

  DATA: ZFDHENO LIKE ZTDHF1-ZFDHENO,
        ZFDHDOC LIKE ZTDHF1-ZFDHDOC.

  REFRESH IT_SELECT.
  CLEAR W_SELECTED_LINES.

  MOVE: IT_TAB1-ZFDHENO  TO   ZFDHENO,
        IT_TAB1-ZFDHDOC  TO   ZFDHDOC.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB1-ZFDHENO  TO IT_SELECT-ZFDHENO,
             IT_TAB1-ZFDHDOC  TO IT_SELECT-ZFDHDOC.

      APPEND IT_SELECT.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF NOT ZFDHENO IS INITIAL.
      MOVE : ZFDHENO TO IT_SELECT-ZFDHENO,
             ZFDHDOC TO IT_SELECT-ZFDHDOC.

      APPEND IT_SELECT.
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

     WRITE : / SY-ULINE,
             / '총', W_COUNT, '건'.
  ENDIF.


ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

   WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
         5 SY-VLINE,
           IT_TAB1-ZFDHDOC,            " 문서종류.
           SY-VLINE,
           IT_TAB1-ZFDHENO,            " 문서번호.
       120 SY-VLINE.
** stored value...
   HIDE: IT_TAB1.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_UNRELEASE_CHECK USING    P_ZFREQNO.
* Amend 존재여부 체?

* Invoice 체?

ENDFORM.                    " P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE  USING   P_HEADER   P_TEXT.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = P_HEADER
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = P_TEXT
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = ' '
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE   USING   W_GUBUN.
DATA : L_REQTY   LIKE   ZTREQHD-ZFREQTY,
       L_RETURN  LIKE   SY-SUBRC,
       O_ZTREQST LIKE   ZTREQST,
       L_COUNT   TYPE   C.

   CLEAR : L_REQTY, L_COUNT.

   LOOP AT IT_SELECT.
      W_TABIX = SY-TABIX.
*>>> 진행상태바..
      line = ( sy-tabix / w_selected_lines ) * 100.
      out_text = 'JOB PROGRESS %99999%%'.
      replace '%99999%' with line into out_text.
      perform p2000_show_bar using out_text line.

*>>> 수입의뢰 헤더, 상태 테이블 조회...
      PERFORM P2000_GET_DOC_KEY.

      SELECT SINGLE * FROM ZTREQHD
                      WHERE ZFREQNO EQ ZTREQST-ZFREQNO.

*>>> Receipt전 문서의 상태 검증...
      IF ZTREQHD-ZFCLOSE EQ 'X'.
         MESSAGE I354 WITH ZTREQST-ZFREQNO. CONTINUE.
      ENDIF.
      IF ZTREQST-ZFRTNYN EQ 'X'.
         MESSAGE I355 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO. CONTINUE.
      ENDIF.
      IF ZTREQST-ZFDOCST EQ 'O'.
         TEXT50  = '재작업 여부 확인'.
         TEXT100 = '이미 개설된 상태입니다. 계속 진행하시겠습니까?'.
         PERFORM P2000_POPUP_MESSAGE USING TEXT50 TEXT100.
         IF W_BUTTON_ANSWER NE '1'.       " 확인일 경우.
            CONTINUE.
         ENDIF.
      ENDIF.

*>> 변경이력..
      O_ZTREQST = ZTREQST.

* LOCK CHECK
      PERFORM   P2000_LOCK_MODE_SET  USING    'L'
                                              ZTREQST-ZFREQNO
                                              ZTREQST-ZFAMDNO
                                              L_RETURN.
      CHECK L_RETURN EQ 0.

* 상태 변경
*>>>>> .....
      MOVE : SY-UNAME    TO    ZTREQST-UNAM,
             SY-DATUM    TO    ZTREQST-UDAT,
             'O'         TO    ZTREQST-ZFDOCST,
             'R'         TO    ZTREQST-ZFEDIST,
*>>>>>>> 향후 수정작업 ====> 문서승인번호/개설일자...

             IT_SELECT-ZFDHENO TO ZTREQST-ZFOPNNO,
             SY-DATUM          TO ZTREQST-ZFOPNDT,
             ZTREQST-ZFOPNNO   TO ZTREQHD-ZFOPNNO,
             ZTREQHD-ZFREQED   TO ZTREQHD-ZFLASTSD,
             ZTREQHD-ZFREQSD   TO ZTREQHD-ZFLASTED,
             W_ZFDHENO   TO    ZTREQST-ZFDOCNOR.


*>>> 변경...
     UPDATE ZTREQHD.
     UPDATE ZTREQST.

* CHANGE DOCUMENT
     CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
     EXPORTING
        W_ZFREQNO      =     ZTREQST-ZFREQNO
        W_ZFAMDNO      =     ZTREQST-ZFAMDNO
        N_ZTREQST      =     ZTREQST
        O_ZTREQST      =     O_ZTREQST.

*>>> UNLOCK SETTTING.
     PERFORM   P2000_LOCK_MODE_SET  USING    'U'
                                              IT_SELECTED-ZFREQNO
                                              IT_SELECTED-ZFAMDNO
                                              L_RETURN.

     L_REQTY = IT_SELECTED-ZFREQTY.

   ENDLOOP.

ENDFORM.                    " P3000_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO  P_ZFAMDNO.
   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.

   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.

   IF P_ZFAMDNO IS INITIAL.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2559   text
*----------------------------------------------------------------------*
FORM P2000_LOCK_MODE_SET USING    VALUE(P_MODE)
                                  VALUE(P_REQNO)
                                  VALUE(P_AMDNO)
                                  P_RETURN.
* LOCK CHECK
   IF P_MODE EQ 'L'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
                ZFREQNO                =     P_REQNO
                ZFAMDNO                =     P_AMDNO
           EXCEPTIONS
                OTHERS        = 1.

      MOVE SY-SUBRC     TO     P_RETURN.
      IF SY-SUBRC NE 0.
         MESSAGE I510 WITH SY-MSGV1 'Import Document' P_REQNO P_AMDNO
                      RAISING DOCUMENT_LOCKED.
      ENDIF.
   ELSEIF P_MODE EQ 'U'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
           EXPORTING
             ZFREQNO                =     P_REQNO
             ZFAMDNO                =     P_AMDNO.
   ENDIF.
ENDFORM.                    " P2000_LOCK_MODE_SET


*&---------------------------------------------------------------------*
*&      Form  P1000_GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
*       UPLOAD용 FILENAME을 발췌하는 서브루틴.
*----------------------------------------------------------------------*
FORM P1000_GET_UPLOAD_FILE USING    W_ERR_CHK.
DATA : L_COUNT    TYPE   I.

  FREE : IT_TAB, IT_TAB1.
  CLEAR : IT_TAB, IT_TAB1.

  MOVE 'N'        TO     W_ERR_CHK.

  OPEN    DATASET    UPLOAD_PATH   FOR   INPUT   IN  TEXT  MODE.
  DO.
      READ     DATASET     UPLOAD_PATH    INTO    FILE_NAME.
      IF       SY-SUBRC    =      4.
               EXIT.
      ENDIF.
*>>   file ---> internal table
      PERFORM  P1000_FILE_TO_ITAB.
*>>   FILE COUNTER 증가.
      ADD    1    TO    L_COUNT.
  ENDDO.

  CLOSE    DATASET   UPLOAD_PATH.

  IF L_COUNT EQ 0.
     MOVE 'Y'        TO     W_ERR_CHK.
     MESSAGE  S920.  EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_UPLOAD_FILE

*&---------------------------------------------------------------------*
*&      Form  P1000_FILE_TO_ITAB
*&---------------------------------------------------------------------*
*       SAM-FILE 내역을 Internal Table로 Append
*----------------------------------------------------------------------*
FORM P1000_FILE_TO_ITAB.

  OPEN    DATASET    FILE_NAME     FOR     INPUT   IN  TEXT  MODE.
  DO.
      READ    DATASET    FILE_NAME     INTO    W_EDI_RECORD.
      IF       SY-SUBRC    =      4.
               EXIT.
      ENDIF.

      MOVE    W_EDI_RECORD             TO      IT_TAB-W_RECORD.
      APPEND  IT_TAB.

      MOVE:   FILE_NAME                TO      IT_TAB1-ZFFILE,
              IT_TAB(14)               TO      IT_TAB1-ZFDHENO,
              IT_TAB(06)               TO      IT_TAB1-ZFDHDOC.
      APPEND  IT_TAB1.
  ENDDO.

ENDFORM.                    " P1000_FILE_TO_ITAB

*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_NAME_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_NAME_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
   WRITE : / SY-ULINE.
*   WRITE:/ SY-VLINE, MARKFIELD1  AS CHECKBOX,
   WRITE:/ SY-VLINE,
      5 SY-VLINE,
        IT_TAB1-ZFFILE NO-GAP,
    120 SY-VLINE.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE :/  SY-VLINE,
           5 SY-VLINE,
           6 SY-ULINE.

ENDFORM.                    " P3000_FILE_NAME_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_GET_DOC_KEY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_DOC_KEY.

  CASE IT_SELECT-ZFDHDOC.
     WHEN 'INF700' OR 'INF707' OR           " MASTER L/C
          'PURLIC' OR                       " 구매승인서.
          'LOCADV' OR 'LOCAMR'              " LOCAL L/C
          OR 'APP700'.

        SELECT * FROM ZTREQST UP TO 1 ROWS
                 WHERE ZFDOCNO EQ IT_SELECT-ZFDHENO
                 AND   ZFAMDNO EQ '00000'.
        ENDSELECT.

     WHEN OTHERS.                           ">> 기타 다른 문서등 추가.
  ENDCASE.

  W_SY_SUBRC = SY-SUBRC.

ENDFORM.                    " P2000_GET_DOC_KEY

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SHOW_DOCUMENT.
  CASE IT_SELECT-ZFDHDOC.
     WHEN 'INF700' OR 'INF707' OR           " MASTER L/C
          'PURLIC' OR                       " 구매승인서.
          'LOCADV' OR 'LOCAMR'              " LOCAL L/C
          OR 'APP700'.
        IF W_SY_SUBRC EQ 0.
           PERFORM P2000_SHOW_LC USING   ZTREQST-ZFREQNO
                                         ZTREQST-ZFAMDNO.
        ENDIF.

     WHEN OTHERS.                           ">> 기타 다른 문서등 추가.
  ENDCASE.

ENDFORM.                    " P2000_SHOW_DOCUMENT
