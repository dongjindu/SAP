*&---------------------------------------------------------------------*
*& Report  ZRIMEDILGR2                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI RECEIPT(L/G EDS VAN I/F용) - BackGroud Job용      *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.12.23                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : EDI RECEIPT(L/G EDS VAN I/F용)
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMEDILGR2  MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

INCLUDE : <ICON>.

*------ EDI
DATA  :  UPLOAD_PATH(300)     TYPE       C.     " loading data
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
      ZFDHENO     LIKE     ZTDHF1-ZFDHENO,    ">문서관리번호.
      ZFDHDOC     LIKE     ZTDHF1-ZFDHDOC,    ">문서 종류.
      ZFDOCNO     LIKE     ZTREQST-ZFDOCNO,   ">SEND 문서 번호.
      ZFDOCNOR    LIKE     ZTREQST-ZFDOCNOR,  ">RECEIPT 문서 번호.
      EBELN       LIKE     ZTREQHD-EBELN,     ">구매오더 번호.
      ZFREQNO     LIKE     ZTREQST-ZFREQNO,   ">수입의뢰번호.
      ZFAMDNO     LIKE     ZTREQST-ZFAMDNO,   ">AMEND SEQ.
      ZFOPNNO     LIKE     ZTREQST-ZFOPNNO,   ">승인번호.
      ZFOPNDT     LIKE     ZTREQST-ZFOPNDT,   ">개설일자.
      ZFFILE(100),
      END OF IT_TAB1.

DATA: BEGIN OF IT_SELECT OCCURS 0,
      ZFDHENO     LIKE     ZTDHF1-ZFDHENO,  " 문서관리번호.
      ZFDHDOC     LIKE     ZTDHF1-ZFDHDOC,  " 문서 종류.
      FILENAME(100),
      END OF IT_SELECT.

DATA: BEGIN OF IT_FLATFILE OCCURS 0,
      FILENAME(100),
      END OF IT_FLATFILE.

DATA: BEGIN OF IT_FILENAME OCCURS 0,
      FILENAME(300),
      END OF IT_FILENAME.

DATA: TEXT100(100),
      TEXT50(50).

DATA : L_COUNT    TYPE   I.
DATA: BEGIN OF MTAB_DATA OCCURS 0,
      LINE(132)   TYPE C,
END OF MTAB_DATA.

*> RETURN MESSAGE 처리용.
*DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
*       INCLUDE STRUCTURE   BAPIRET2.
*DATA:   END   OF RETURN.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(266) TYPE C.
DATA : END OF IT_ERR_LIST.

DATA : IT_ZTIMIMGTX  LIKE ZTIMIMGTX  OCCURS 10 WITH HEADER LINE.

DATA:
      MI_HANDLE       TYPE I,
      ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
      L_LEN           TYPE I,
      L_STRLEN        TYPE I,
      L_SIZE          TYPE I,
      W_MOD           TYPE I,
      L_DATE          TYPE SY-DATUM,
      INCLUDE(8)      TYPE C,             "
      L_TIME          TYPE SY-UZEIT,
      W_BUKRS         LIKE ZTIMIMGTX-BUKRS.


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
*SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*    PARAMETERS: P_BUKRS LIKE ZTIMIMGTX-BUKRS OBLIGATORY.
**   SELECT-OPTIONS: S_DHENO FOR ZTDHF1-ZFDHENO,  " 문서관리번호.
**                   S_DHDOC FOR ZTDHF1-ZFDHDOC.  " 전자문서 종류.
*SELECTION-SCREEN END OF BLOCK B1.


* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.


* Title Text Write
TOP-OF-PAGE.
   IF INCLUDE NE 'POPU'..
      PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
   ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION 절.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
   PERFORM   P100_GET_EDI_IMG  USING W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

   LOOP AT IT_ZTIMIMGTX.
      PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    CONTINUE.    ENDIF.

* UPLOAD용 FILENAME을 발췌하는 서브루틴.
      PERFORM   P1000_GET_UPLOAD_FILE     USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    CONTINUE.    ENDIF.

* 레포트 Write
      PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
      PERFORM   P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
   ENDLOOP.
*   DESCRIBE  TABLE IT_ERR_LIST   LINES  W_LINE.
*   IF W_LINE GT 0.
*      INCLUDE = 'POPU'.
*      CALL SCREEN 0100 STARTING AT  01   3
*                       ENDING   AT  107 12.
*      CLEAR : INCLUDE.
*   ENDIF.
*  LEAVE TO SCREEN 0.
*  CLEAR : IT_TAB1.
*

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
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK           USING   W_ERR_CHK.

DATA : L_LEN    TYPE I.

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
   CALL FUNCTION 'ZIM_LOCAL_EDI_DATA_SPLIT'
      EXPORTING
          BUKRS        =     IT_ZTIMIMGTX-BUKRS.

*        TABLES
*              IT_FILENAME   =   IT_FILENAME.

*   LOOP AT IT_FILENAME.
*      L_LEN = STRLEN( IT_FILENAME ).
*      L_LEN = L_LEN - 30.
*      IF IT_FILENAME+L_LEN(30) NE 'I'.
*         DELETE IT_FILENAME INDEX SY-TABIX.
*      ENDIF.
*   ENDLOOP.
*
*   IF IT_FILENAME[] IS INITIAL.
*      W_ERR_CHK = 'Y'.   MESSAGE S920.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIME10'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIME10'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   SORT IT_TAB1 BY ZFDHDOC ZFFILE ZFDHENO.
   REFRESH : IT_SELECT.
   CLEAR : IT_TAB1, IT_SELECT, w_selected_lines.

   LOOP AT IT_TAB1.
      ADD 1 TO w_selected_lines.
*      W_LINE = W_LINE + 1.
**     PERFORM P2000_PAGE_CHECK.
**>>> File Name이 바뀔때마다.
*      ON CHANGE OF IT_TAB1-ZFFILE.
*         PERFORM P3000_FILE_NAME_WRITE.
*      ENDON.
**>>> Line Record Write.
*      PERFORM P3000_LINE_WRITE.
*      AT LAST.
*         PERFORM P3000_LAST_WRITE.
*      ENDAT.
      MOVE : IT_TAB1-ZFDHENO   TO IT_SELECT-ZFDHENO,
             IT_TAB1-ZFDHDOC   TO IT_SELECT-ZFDHDOC,
             IT_TAB1-ZFFILE    TO IT_SELECT-FILENAME.

      APPEND IT_SELECT.

   ENDLOOP.
   CLEAR : IT_TAB1, IT_SELECT.

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

  DATA: ZFDHENO  LIKE ZTDHF1-ZFDHENO,
        ZFDHDOC  LIKE ZTDHF1-ZFDHDOC,
        FILENAME LIKE ZTDHF1-FILENAME.

  REFRESH IT_SELECT.
  CLEAR W_SELECTED_LINES.

  MOVE: IT_TAB1-ZFDHENO  TO   ZFDHENO,
        IT_TAB1-ZFDHDOC  TO   ZFDHDOC,
        IT_TAB1-ZFFILE   TO   FILENAME.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB1-ZFDHENO   TO IT_SELECT-ZFDHENO,
             IT_TAB1-ZFDHDOC   TO IT_SELECT-ZFDHDOC,
             IT_TAB1-ZFFILE    TO IT_SELECT-FILENAME.

      APPEND IT_SELECT.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

*  IF W_SELECTED_LINES EQ 0.
*    IF NOT ZFDHENO IS INITIAL.
*      MOVE : ZFDHENO TO IT_SELECT-ZFDHENO,
*             ZFDHDOC TO IT_SELECT-ZFDHDOC.
*
*      APPEND IT_SELECT.
*      ADD 1 TO W_SELECTED_LINES.
*    ELSE.
*      MESSAGE S962.
*    ENDIF.
*  ENDIF.

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

*   WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
     WRITE:/ SY-VLINE,
           5 SY-VLINE,
             IT_TAB1-ZFDHDOC,            " 문서종류.
             SY-VLINE,
             IT_TAB1-ZFDHENO,            " 문서번호.
             SY-VLINE,
             IT_TAB1-ZFDOCNO,            " 문서번호.
             SY-VLINE,
             IT_TAB1-ZFOPNNO,            " 문서번호.
             SY-VLINE,
             IT_TAB1-ZFOPNDT,            " 문서번호.
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

   REFRESH : IT_ERR_LIST.
   CLEAR : L_REQTY, L_COUNT, IT_ERR_LIST.

   LOOP AT IT_SELECT.
      W_TABIX = SY-TABIX.
*>>> 진행상태바..
      line = ( sy-tabix / w_selected_lines ) * 100.
      out_text = 'JOB PROGRESS %99999%%'.
      replace '%99999%' with line into out_text.
      perform p2000_show_bar using out_text line.

      CASE IT_SELECT-ZFDHDOC.
         WHEN 'INF700'.    ">L/C 개설응답서.
            CALL FUNCTION 'ZIM_LG_INF700_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'INF707'.    ">L/C 변경응답서.
            CALL FUNCTION 'ZIM_LG_INF707_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'LOCADV'.    ">Local L/C 개설응답서.
            CALL FUNCTION 'ZIM_LG_LOCADV_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'LOCAMA'.    ">Local L/C 변경응답서.
            CALL FUNCTION 'ZIM_LG_LOCAMA_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'DEBADV'.    ">출금통지서.
            CALL FUNCTION 'ZIM_LG_DEBADV_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'IMPRES'.    ">수입면장 정보.
            CALL FUNCTION 'ZIM_LG_IMPRES_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'DOANTC'.    ">선적서류 도착통보서.
            CALL FUNCTION 'ZIM_LG_DOANTC_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'LDANTC'.    ">내국신용장 어음도착통보서.
            CALL FUNCTION 'ZIM_LG_LDANTC_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'DISCHG'.    ">수입어음 DISCOUNT 내역통보서.
            CALL FUNCTION 'ZIM_LG_DISCHG_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN 'FINBIL'.    ">세금계산서..
            CALL FUNCTION 'ZIM_LG_FINBIL_EDI_DOC'
               EXPORTING
                  W_FILENAME = IT_SELECT-FILENAME
                  BACK_PATH  = IT_ZTIMIMGTX-ZFRBAK
               TABLES
                  RETURN     = RETURN
               EXCEPTIONS
                  OTHERS     = 4.

         WHEN OTHERS.
      ENDCASE.

      W_SUBRC = SY-SUBRC.

      IF W_SUBRC NE 0.           ">> 오류 발생시...
         ROLLBACK WORK.
         IF RETURN[] IS INITIAL.
            PERFORM  P2000_SINGLE_MSG_MAKE.
         ELSE.
            PERFORM  P2000_MULTI_MSG_MAKE.
         ENDIF.
      ELSE.
         COMMIT WORK.
         PERFORM  P2000_MULTI_MSG_MAKE.
      ENDIF.

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

   FREE : IT_TAB, IT_TAB1.
   CLEAR : IT_TAB, IT_TAB1.

   MOVE 'N'        TO     W_ERR_CHK.

*> FTP CONNECT.
   PERFORM  P2000_FTP_CONNECT.
   CHECK : W_ERR_CHK EQ 'N'.

*> CURRENT DIR.
   PERFORM  P2000_GET_CURRENT_DIR USING UPLOAD_PATH
                                        MI_HANDLE.
   CHECK : W_ERR_CHK EQ 'N'.

*> FILE LIST GET.
   PERFORM  P2000_GET_FILE_LIST.
   CHECK : W_ERR_CHK EQ 'N'.

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

  LOOP AT IT_FLATFILE.
*>>   file ---> internal table
      MOVE IT_FLATFILE-FILENAME TO FILE_NAME.
      PERFORM  P1000_FILE_TO_ITAB.
*>>   FILE COUNTER 증가.
      ADD    1    TO    L_COUNT.
  ENDLOOP.

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

DATA : L_FIRST_CHK  TYPE C VALUE 'N',
       L_ZFDHDOC    LIKE IT_TAB1-ZFDHDOC.

  CLEAR : L_ZFDHDOC,  W_TABIX.
  OPEN    DATASET    FILE_NAME     FOR     INPUT   IN  TEXT  MODE.
  IF SY-SUBRC NE 0.
     EXIT.
  ENDIF.

  DO.
      READ    DATASET    FILE_NAME     INTO    W_EDI_RECORD.
      IF SY-SUBRC    EQ    4.
         EXIT.
      ENDIF.

*>> 문서의 시작.
      IF W_EDI_RECORD(2) EQ '<<'.
         CLEAR : IT_TAB1.
         MOVE:   FILE_NAME                TO      IT_TAB1-ZFFILE,
                 W_EDI_RECORD+24(30)      TO      IT_TAB1-ZFDHENO,
                 W_EDI_RECORD+65(06)      TO      IT_TAB1-ZFDHDOC.
         APPEND  IT_TAB1.
         W_TABIX = SY-TABIX.
         L_ZFDHDOC = W_EDI_RECORD+65(06).
      ELSE.
         CASE L_ZFDHDOC.
            WHEN 'INF700'.  ">개설응답서(L/C).
               CASE  W_EDI_RECORD(02).
                  WHEN '01'.     ">송신전자문서번호.
                      MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                      MODIFY IT_TAB1 INDEX W_TABIX.
                  WHEN '05'.     ">L/C No.
                     IF W_EDI_RECORD+2(03) EQ 'AAC'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '06'.     ">개설일자.
                     IF W_EDI_RECORD+2(03) EQ '182'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
*                        EXIT.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.
            WHEN 'INF707'.  ">변경응답서(L/C)
               CASE  W_EDI_RECORD(02).
                  WHEN '01'.     ">송신전자문서번호.
                      MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                      MODIFY IT_TAB1 INDEX W_TABIX.
                  WHEN '03'.     ">L/C No.
                     IF W_EDI_RECORD+2(03) EQ '2AD'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '04'.     ">조건변경일자.
                     IF W_EDI_RECORD+2(03) EQ '2AB'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
*                        EXIT.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.
            WHEN 'LOCADV'.  ">LOCAL 개설응답서.
               CASE  W_EDI_RECORD(02).
                  WHEN '03'.     ">L/C No.
                     IF W_EDI_RECORD+2(03) EQ 'DM '. ">송신전자문서번호.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ELSEIF W_EDI_RECORD+2(03) EQ 'LC '.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '04'.     ">조건변경일자.
                     IF W_EDI_RECORD+2(03) EQ '182'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
*                        EXIT.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.

            WHEN 'LOCAMA'.  ">LOCAL 변경응답서.
               CASE  W_EDI_RECORD(02).
                  WHEN '02'.     ">L/C No.
                     IF W_EDI_RECORD+2(03) EQ 'DM '. ">송신전자문서번호.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ELSEIF W_EDI_RECORD+2(03) EQ 'LC '.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '03'.     ">조건변경일자.
                     IF W_EDI_RECORD+2(03) EQ '2AB'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
*                        EXIT.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.

            WHEN 'DEBADV'.  ">출금통지서.
               CASE  W_EDI_RECORD(02).
                  WHEN '01'.
                     IF W_EDI_RECORD+2(03) EQ '456'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '03'.     ">L/C No.
                     IF W_EDI_RECORD+2(03) EQ 'ACD'. ">송신전자문서번호.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '02'.     ">조건변경일자.
                     IF W_EDI_RECORD+2(03) EQ '193'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
*                        EXIT.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.
            WHEN 'DOANTC'.  ">선적서류도착통보서.
               CASE  W_EDI_RECORD(02).
                 WHEN '03'.
                     IF W_EDI_RECORD+2(03) EQ 'AAC '.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ELSEIF W_EDI_RECORD+2(03) EQ 'BM'
                         OR W_EDI_RECORD+2(03) EQ 'AWB'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '05'.
                     IF W_EDI_RECORD+2(03) EQ '265'.
*-----------------------------------------------------------------------
* DATE CONVERT
*-----------------------------------------------------------------------
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.

            WHEN 'LDANTC'.  ">내국신용장 어음도착통보서.
               CASE  W_EDI_RECORD(02).
                 WHEN '03'.
                     IF W_EDI_RECORD+2(03) EQ 'LC '.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ELSEIF W_EDI_RECORD+2(03) EQ 'REN'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '06'.
                     IF W_EDI_RECORD+2(03) EQ '2AD'.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.

            WHEN 'DISCHG'.  ">수입어음DISCOUNT 내역통보서.
               CASE  W_EDI_RECORD(02).
                 WHEN '03'.
                     IF W_EDI_RECORD+2(03) EQ 'AAC '.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ELSEIF W_EDI_RECORD+2(03) EQ 'BM'
                         OR W_EDI_RECORD+2(03) EQ 'AWB'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '06'.
                     IF W_EDI_RECORD+2(03) EQ '140'.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.
            WHEN 'IMPRES'.  ">수입면허필증 정보.
               CASE  W_EDI_RECORD(02).
                 WHEN '01'.
                     IF W_EDI_RECORD+2(03) EQ '962'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFDOCNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '05'.
                     IF W_EDI_RECORD+2(03) EQ 'ABA'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '04'.
                     IF W_EDI_RECORD+2(03) EQ '97 '.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.
            WHEN 'FINBIL'.  ">세금계산서.
               CASE  W_EDI_RECORD(02).
                 WHEN '02'.
                     MOVE: W_EDI_RECORD+5(03) TO IT_TAB1-ZFDOCNO.
                     MODIFY IT_TAB1 INDEX W_TABIX.
                  WHEN '03'.
                     IF W_EDI_RECORD+2(03) EQ 'AAC' OR
                        W_EDI_RECORD+2(03) EQ 'ACK'.
                        MOVE: W_EDI_RECORD+5(35) TO IT_TAB1-ZFOPNNO.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN '04'.
                     IF W_EDI_RECORD+2(03) EQ '97 '.
                        CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
                             EXPORTING
                                 DATE_EXTERNAL = W_EDI_RECORD+5(08)
                             IMPORTING
                                 DATE_INTERNAL = IT_TAB1-ZFOPNDT.
                        MODIFY IT_TAB1 INDEX W_TABIX.
                     ENDIF.
                  WHEN OTHERS.
               ENDCASE.
            WHEN OTHERS.
         ENDCASE.
         L_FIRST_CHK = 'N'.
      ENDIF.

  ENDDO.

  CLOSE DATASET    FILE_NAME.

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
        MARKFIELD  AS CHECKBOX,
      5 SY-VLINE,
        IT_TAB1-ZFFILE NO-GAP,
    120 SY-VLINE.
   HIDE : IT_TAB1.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
   WRITE :/ SY-ULINE.
*   WRITE :/  SY-VLINE,
*           5 SY-VLINE,
*           6 SY-ULINE.

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
                 WHERE ZFDOCNO EQ IT_SELECT-ZFDHENO.
*                 AND   ZFAMDNO EQ '00000'.
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
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_CURRENT_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_CURRENT_DIR  USING UPLOAD_PATH   MI_HANDLE.
DATA: L_STRLEN           TYPE I,
      L_START            TYPE I,
      L_END              TYPE I,
      L_LEN              TYPE I,
      L_POSITION1        TYPE I,
      L_POSITION2        TYPE I,
      L_FIRST_CHK        TYPE C VALUE 'N',
      L_COMMAND(50).

   CONCATENATE 'cd' IT_ZTIMIMGTX-ZFRECV
                    INTO L_COMMAND SEPARATED BY SPACE.

   PERFORM P2000_SHOW_BAR
           USING 'Receipt된 Flat Files 정보를 읽는 중입니다...' 0.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH IT_ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

*> DIRECTORY 정보 GET.
   REFRESH : MTAB_DATA.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = 'pwd'
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH IT_ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

  READ TABLE MTAB_DATA INDEX 2.
  IF SY-SUBRC EQ 0.
     L_STRLEN   = STRLEN( MTAB_DATA ).
     L_POSITION1 = 0.
     L_POSITION2 = 0.
     DO.
        IF L_POSITION1 GT L_STRLEN.
           EXIT.
        ENDIF.
        IF MTAB_DATA+L_POSITION1(1) EQ '"'.
           IF L_FIRST_CHK EQ 'N'.
              L_START = L_POSITION1 + 1.
              L_FIRST_CHK = 'Y'.
           ELSE.
              L_END   = L_POSITION1.
           ENDIF.
        ENDIF.
        ADD 1  TO  L_POSITION1.
     ENDDO.
     L_LEN = L_END - L_START.
     UPLOAD_PATH = MTAB_DATA+L_START(L_LEN).
     CONCATENATE UPLOAD_PATH '/' INTO UPLOAD_PATH.
  ELSE.
     MOVE 'Y'        TO     W_ERR_CHK.
     MESSAGE S207(04).
  ENDIF.

ENDFORM.                    " P2000_GET_CURRENT_DIR
*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_CONNECT.

   PERFORM P2000_SHOW_BAR USING 'File Server에 로그인 중입니다...' 0.

*  DESCRIBE FIELD MC_PASSWORD LENGTH MI_PWD_LEN.
*
*-- FTP_CONNECT requires an encrypted password to work
*  CALL 'AB_RFC_X_SCRAMBLE_STRING'
*     ID 'SOURCE' FIELD MC_PASSWORD ID 'KEY'         FIELD MI_KEY
*     ID 'SCR'    FIELD 'X'         ID 'DESTINATION' FIELD MC_PASSWORD
*     ID 'DSTLEN' FIELD MI_PWD_LEN.

  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
           USER            = IT_ZTIMIMGTX-UNAME
           PASSWORD        = IT_ZTIMIMGTX-PASSW
           HOST            = IT_ZTIMIMGTX-HOST
           RFC_DESTINATION = 'SAPFTP'
      IMPORTING
           HANDLE          = MI_HANDLE
      EXCEPTIONS
           NOT_CONNECTED   = 1
           OTHERS          = 2.

   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S201(04) WITH IT_ZTIMIMGTX-HOST.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S202(04) WITH IT_ZTIMIMGTX-UNAME IT_ZTIMIMGTX-HOST.
   ENDCASE.

ENDFORM.                    " P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_FILE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_GET_FILE_LIST.

   REFRESH : MTAB_DATA.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = 'dir *.TXT -Nf'
*        IMPORTING
*           FILEDATE      = L_DATE
*           FILETIME      = L_TIME
*           FILESIZE      = L_SIZE
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

   W_SUBRC = SY-SUBRC.

   IF W_SUBRC EQ 0.
      REFRESH : IT_FLATFILE.
      DESCRIBE TABLE MTAB_DATA LINES W_LINE.
      IF W_LINE LT 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S920.
         EXIT.
      ENDIF.
      W_LINE = W_LINE - 1.
      LOOP AT MTAB_DATA FROM 4 TO W_LINE.
*         CASE MTAB_DATA+54(6).
         CASE MTAB_DATA+54(1).
*            WHEN 'INF700' OR 'INF707' OR 'LOCADV' OR 'LOCAMA' OR
*                 'DEBADV' OR 'DOANTC' OR 'LDANTC' OR 'DISCHG' OR
*                 'PURLIC' OR 'FINBIL' OR 'IMPRES'.
            WHEN 'I'.
            WHEN OTHERS.
               CONTINUE.
         ENDCASE.
         CLEAR : IT_FLATFILE.
         CONCATENATE UPLOAD_PATH MTAB_DATA+54 INTO IT_FLATFILE.
         APPEND IT_FLATFILE.
      ENDLOOP.
   ELSE.
* do some error checking.
      CASE SY-SUBRC.
         WHEN 1.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S101(04) WITH IT_ZTIMIMGTX-HOST.   EXIT.
         WHEN 2.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S200(04).      EXIT.
         WHEN 3.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S207(04).      EXIT.
         WHEN 4.
            MOVE 'Y'        TO     W_ERR_CHK.
            MESSAGE S208(04).      EXIT.
      ENDCASE.
   ENDIF.

ENDFORM.                    " P2000_GET_FILE_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT.
*             ZTIV-ZFIVNO         TO     IT_ERR_LIST-ZFIVNO.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E' OR 'A'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
      ENDCASE.
      APPEND  IT_ERR_LIST.
   ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'ERRLIST'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' ." '오류 LIST'.
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
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(107), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
*                   'Doc.관리No' NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 105 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(107).
*         MESSAGE
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
*                      SY-VLINE NO-GAP, IT_ERR_LIST-ZFIVNO  NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(98) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E'.
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
         WRITE : / SY-ULINE(107).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SINGLE_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SINGLE_MSG_MAKE.

   MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
          SY-MSGID            TO     IT_ERR_LIST-MSGID,
          SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
          SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
          SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
          SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
          SY-MSGV4            TO     IT_ERR_LIST-MSGV4.

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
   ENDCASE.

   APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_SINGLE_MSG_MAKE

*&---------------------------------------------------------------------*
*&      Form  P100_GET_EDI_IMG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P100_GET_EDI_IMG USING    P_ERR_CHK.

   SELECT * INTO TABLE IT_ZTIMIMGTX
            FROM ZTIMIMGTX.

   IF SY-SUBRC NE 0.
       P_ERR_CHK = 'Y'.
   ENDIF.

ENDFORM.                    " P100_GET_EDI_IMG
