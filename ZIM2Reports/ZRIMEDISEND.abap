*&---------------------------------------------------------------------*
*& Report  ZRIMEDISEND                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 EDI SEND용 Report Program                        *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.01.28                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMEDISEND  MESSAGE-ID ZIM
                     LINE-SIZE 119
                     NO STANDARD PAGE HEADING.

TABLES : ZTDDF1,     " 표준 EDI FLAT DETAIL
         ZTDHF1,     " 표준 EDI FLAT HEAD
         DD03D.      " Dynpro fields for table fields

*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
DATA : IT_ZTDHF1      LIKE ZTDHF1 OCCURS 0 WITH HEADER LINE.
DATA : IT_ERR_ZTDHF1  LIKE ZTDHF1 OCCURS 0 WITH HEADER LINE.

*-----------------------------------------------------------------------
* Error용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_ERR_TEXT OCCURS 0,
       ZFDHENO    LIKE ZTDHF1-ZFDHENO,
       ZFDHDOC    LIKE ZTDHF1-ZFDHDOC,
       ZFDHREF    LIKE ZTDHF1-ZFDHREF,
       ERROR_TEXT(67)   TYPE   C.                  " MARK
DATA : END OF IT_ERR_TEXT.

*-----------------------------------------------------------------------
* EDI Send용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                        " MARK
       W_GB01(1)  TYPE C VALUE ';',
       UPDATE_CHK TYPE C,                        " DB 반영 여부...
       W_GB02(1)  TYPE C VALUE ';',
       ZFDHENO    LIKE ZTDHF1-ZFDHENO,           " 문서관리번?
       W_GB03(1)  TYPE C VALUE ';',
       ZFDHDOC    LIKE ZTDHF1-ZFDHDOC,           " 전자문서?
       W_GB04(1)  TYPE C VALUE ';',
       ZFDHSRO    LIKE ZTDHF1-ZFDHSRO,           " 거래상대방 ID
       W_GB05(1)  TYPE C VALUE ';',
       ZFDHREF    LIKE ZTDHF1-ZFDHREF,           " 발신인이 부여한 참?
       W_GB06(1)  TYPE C VALUE ';',
*       ZFDHDDB    LIKE ZTDHF1-ZFDHDDB,           " 문서담당부?
*       W_GB07(1)  TYPE C VALUE ';',
       ZFDHJSD(8) TYPE C,                        " IBM <-> FEP 전송/수?
*      ZFDHJSD    LIKE ZTDHF1-ZFDHJSD,           " IBM <-> FEP 전송/수?
       W_GB08(1)  TYPE C VALUE ';',
       ZFDHJSH(8) TYPE C,                        " IBM <-> FEP 전송/수?
*      ZFDHJSH    LIKE ZTDHF1-ZFDHJSH,           " IBM <-> FEP 전송/수?
       W_GB09(1)  TYPE C VALUE ';',
       ZFDHJSC    LIKE ZTDHF1-ZFDHJSC,
       W_GB10(1)  TYPE C VALUE ';',
       ZFDHTRA    LIKE ZTDHF1-ZFDHTRA,
       W_GB11(1)  TYPE C VALUE ';',
       ZFDHSSD    LIKE ZTDHF1-ZFDHSSD,
       W_GB12(1)  TYPE C VALUE ';',
       ZFDHSST    LIKE ZTDHF1-ZFDHSST,
       W_GB13(1)  TYPE C VALUE ';',
       ZFDHAPP    LIKE ZTDHF1-ZFDHAPP,
       W_GB14(1)  TYPE C VALUE ';',
       ZFDHPRT    LIKE ZTDHF1-ZFDHPRT,
*       W_GB15(1)  TYPE C VALUE ';',
*       ZFDHPTI    LIKE ZTDHF1-ZFDHPTI,
*       W_GB16(1)  TYPE C VALUE ';',
*       ZFDHPTE    LIKE ZTDHF1-ZFDHPTE,
*       W_GB17(1)  TYPE C VALUE ';',
*       ZFDHRSO    LIKE ZTDHF1-ZFDHRSO,
       W_GB18(1)  TYPE C VALUE ';'.
DATA : END OF IT_TAB.

DATA : W_TEXT(80)      TYPE C.
DATA : W_TABIX         LIKE SY-TABIX.
DATA : W_BUTTON_ANSWER.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMMESSAGE.    " MESSAGE FUNCTION
INCLUDE   ZRIMUTIL01.     " Utility function 모?


*-----------------------------------------------------------------------
* SELECT RECORD?
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_SELECTED OCCURS 0,
       GUBUN      TYPE C,                        " 최초 구?
       ZFDHENO    LIKE ZTDHF1-ZFDHENO,
       ZFDHDOC    LIKE ZTDHF1-ZFDHDOC,
       ZFDHREF    LIKE ZTDHF1-ZFDHREF,
END OF IT_SELECTED.

*-----------------------------------------------------------------------
* DATA DEFINE
*-----------------------------------------------------------------------
DATA : W_ERR_CHK         TYPE C,
       W_FIELD_NM        LIKE DD03D-FIELDNAME,   " 필드?
       W_PAGE            TYPE I,                 " Page Counter
       W_LINE            TYPE I,                 " 페이지당 LINE COUNT
       W_COUNT           TYPE I,                 " 전체 COUNT
       W_LIST_INDEX      LIKE SY-TABIX,
       W_NMOD            TYPE I,
       W_SELECTED_LINES  TYPE P.             " 선택 LINE COUNT

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_DHJSD FOR ZTDHF1-ZFDHJSD,  " 송신일?
                   S_DHENO FOR ZTDHF1-ZFDHENO,  " 문서관리번?
                   S_DHSST FOR ZTDHF1-ZFDHSST   " 서버송신 여?
                           NO INTERVALS    NO-EXTENSION,
                   S_DHTRA FOR ZTDHF1-ZFDHTRA   " 변환여?
                           NO INTERVALS    NO-EXTENSION,
                   S_DHDOC FOR ZTDHF1-ZFDHDOC,  " 전자문서 종?
                   S_DHREF FOR ZTDHF1-ZFDHREF.  " 발신전자문서 번?
*   SELECTION-SCREEN SKIP 1.                     " 1 LINE SKIP
** 출력 여?
*   PARAMETERS : P_PRTYN  AS CHECKBOX   DEFAULT   'X'.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* PARAMETER 초기값 Setting
*-----------------------------------------------------------------------
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* Title Text Write
*-----------------------------------------------------------------------
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

* 구매의뢰 테이블 SELECT
   PERFORM   P1000_GET_ZTDHF1          USING   W_ERR_CHK.
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
         W_FIELD_NM = 'ZFDHENO'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
         CLEAR : W_LIST_INDEX, IT_TAB.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
       WHEN 'ERRL'.                      " ERROR LOG
          PERFORM P3000_LOG_DISPLAY.
       WHEN 'CLEA'.                      " ERROR LOG
          REFRESH : IT_ERR_TEXT.
      WHEN 'DISP' OR 'EDIR' OR 'FLAT'.   " L/C 조회  OR EDI REPORT
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            IF SY-UCOMM EQ 'DISP'.
               PERFORM P2000_SHOW_DOCUMENT USING IT_SELECTED-ZFDHENO
                                                 IT_SELECTED-ZFDHDOC
                                                 IT_SELECTED-ZFDHREF.
            ELSEIF SY-UCOMM EQ 'EDIR'.
               PERFORM P2000_SHOW_REPORT   USING IT_SELECTED-ZFDHENO
                                                 IT_SELECTED-ZFDHDOC
                                                 IT_SELECTED-ZFDHREF.
            ELSEIF SY-UCOMM EQ 'FLAT'.
               PERFORM P2000_SHOW_FLAT     USING IT_SELECTED-ZFDHENO.
            ENDIF.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
         PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.          " REFRESH
* 구매의뢰 테이블 SELECT
         PERFORM   P1000_GET_ZTDHF1          USING   W_ERR_CHK.
         IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* REPORT REWRITE.
         PERFORM RESET_LIST.
      WHEN 'EDIS'.          " EDI SEND
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 0.
            MESSAGE E247.
         ELSE.
* Message Box
            PERFORM P2000_POPUP_MESSAGE USING
                    'Send Confirmation'
                    '선택한 전자문서를 Send하시겠습니까?'
                    '확    인'
                    '아 니 오'
                    '1'
                    W_BUTTON_ANSWER.

            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
               PERFORM P3000_EDI_SEND_UPDATE.
               IF W_LINE EQ 0.
                  MESSAGE S251 WITH W_COUNT.
                  LEAVE TO SCREEN 0.
               ELSE.
* 구매의뢰 테이블 SELECT
                  PERFORM   P1000_GET_ZTDHF1          USING   W_ERR_CHK.
                  IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
* REPORT REWRITE.
                  PERFORM RESET_LIST.
               ENDIF.
            ENDIF.
         ENDIF.
      WHEN OTHERS.
   ENDCASE.

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
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

   SET  TITLEBAR 'ZIME2'.           " GUI TITLE SETTING..

   MOVE: 'I'                TO S_DHJSD-SIGN,
         'EQ'               TO S_DHJSD-OPTION,
         SY-DATUM+2(6)      TO S_DHJSD-LOW,
         SY-DATUM+2(6)      TO S_DHJSD-HIGH.
   APPEND S_DHJSD.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 송 신 문 서 현 황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 101 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',           SY-VLINE,
            ' 문서관리번호 ',        SY-VLINE,
            'Doc.Ty',                SY-VLINE,
            '    수   신   처    ',  SY-VLINE,
            '송신일자',              SY-VLINE,
            '송신시간',              SY-VLINE,
            '변환',                  SY-VLINE,
            '송신',                  SY-VLINE,
            '    내부관리번호    ',  SY-VLINE,
            'PRN',                   SY-VLINE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.


ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTDHF1
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTDHF1 USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO TABLE IT_ZTDHF1 FROM ZTDHF1
                               WHERE ( ZFDHJSD    IN   S_DHJSD
                               OR      ZFDHJSD    EQ     SPACE   )
                               AND     ZFDHENO    IN     S_DHENO
                               AND     ZFDHSST    IN     S_DHSST
                               AND     ZFDHTRA    IN     S_DHTRA
                               AND     ZFDHDOC    IN     S_DHDOC
                               AND     ZFDHREF    IN     S_DHREF
                               AND     ZFDHSRG    EQ     'S'.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

  REFRESH : IT_TAB.
  LOOP AT IT_ZTDHF1.
     MOVE-CORRESPONDING   IT_ZTDHF1    TO     IT_TAB.
     IF NOT IT_ZTDHF1-ZFDHJSD IS INITIAL.
        MOVE : IT_ZTDHF1-ZFDHJSD(2)    TO  IT_TAB-ZFDHJSD(2),
               '/'                     TO  IT_TAB-ZFDHJSD+2(1),
               IT_ZTDHF1-ZFDHJSD+2(2)  TO  IT_TAB-ZFDHJSD+3(2),
               '/'                     TO  IT_TAB-ZFDHJSD+5(1),
               IT_ZTDHF1-ZFDHJSD+4(2)  TO  IT_TAB-ZFDHJSD+6(2).
     ENDIF.
     IF NOT IT_ZTDHF1-ZFDHJSH IS INITIAL.
        MOVE : IT_ZTDHF1-ZFDHJSH(2)    TO  IT_TAB-ZFDHJSH(2),
              ':'                     TO  IT_TAB-ZFDHJSH+2(1),
              IT_ZTDHF1-ZFDHJSH+2(2)  TO  IT_TAB-ZFDHJSH+3(2),
              ':'                     TO  IT_TAB-ZFDHJSH+5(1),
              IT_ZTDHF1-ZFDHJSH+4(2)  TO  IT_TAB-ZFDHJSH+6(2).
     ENDIF.
     APPEND IT_TAB.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZTDHF1
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

   SET PF-STATUS 'ZIME2'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIME2'.           " GUI TITLE SETTING..

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
      W_PAGE = W_PAGE + 1.    W_LINE = 1.
      NEW-PAGE.
   ENDIF.


ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

* IF SY-UCOMM EQ 'EDIS' OR SY-UCOMM EQ 'FRGR'.
*    IF IT_TAB-MARK EQ 'X' AND IT_TAB-UPDATE_CHK EQ 'U'.
*       MARKFIELD = 'X'.
*    ELSE.
*       CLEAR : MARKFIELD.
*    ENDIF.
* ENDIF.

  FORMAT RESET.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX
       COLOR COL_NORMAL INTENSIFIED OFF,
       SY-VLINE.

  W_NMOD = W_LINE MOD 2.
  IF W_NMOD EQ 1.
     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
     FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE : IT_TAB-ZFDHENO,             " 문서관리번?
       SY-VLINE,
       IT_TAB-ZFDHDOC,                " 문서 종?
       SY-VLINE,
       IT_TAB-ZFDHSRO,                " 수신?
       SY-VLINE,
       IT_TAB-ZFDHJSD,                " 서버송신일?
       SY-VLINE,
       IT_TAB-ZFDHJSH,                " 서버송신시?
       SY-VLINE,
       IT_TAB-ZFDHTRA,                " 변환 상?
       SY-VLINE,
       IT_TAB-ZFDHSST,                " 송신상?
       SY-VLINE,
       IT_TAB-ZFDHREF,                " 내부 참조번?
       SY-VLINE,
       IT_TAB-ZFDHPRT,                " 출력횟?
       SY-VLINE.

* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.
  CLEAR : W_LIST_INDEX, IT_TAB.
ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.
   CLEAR : W_LIST_INDEX, IT_TAB.
   FORMAT RESET.
   WRITE : / SY-ULINE.
   WRITE : / 'Total Count :', W_COUNT NO-GAP, '건'.
ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'EDI 송신'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX      TYPE P,
        ZFDHENO    LIKE ZTDHF1-ZFDHENO,
        ZFDHDOC    LIKE ZTDHF1-ZFDHDOC,
        ZFDHREF    LIKE ZTDHF1-ZFDHREF.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFDHENO  TO ZFDHENO,
         IT_TAB-ZFDHDOC  TO ZFDHDOC,
         IT_TAB-ZFDHREF  TO ZFDHREF.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFDHENO  TO IT_SELECTED-ZFDHENO,
             IT_TAB-ZFDHDOC  TO IT_SELECTED-ZFDHDOC,
             IT_TAB-ZFDHREF  TO IT_SELECTED-ZFDHREF.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFDHENO TO IT_SELECTED-ZFDHENO,
             ZFDHREF TO IT_SELECTED-ZFDHREF,
             ZFDHDOC TO IT_SELECTED-ZFDHDOC.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
    ENDIF.
  ENDIF.

  CLEAR : W_LIST_INDEX, IT_TAB.

ENDFORM.                    " P2000_MULTI_SELECTION

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_SHOW_DOCUMENT USING    P_ZFDHENO   P_ZFDHDOC    P_ZFDHREF.
DATA : LW_DHREF    LIKE    ZTDHF1-ZFDHREF.
DATA : L_ZTREQST   LIKE    ZTREQST,
       L_ZTINS     LIKE    ZTINS,
       L_ZTOFF     LIKE    ZTOFF,
       L_ZTLG      LIKE    ZTLG.

   MOVE : P_ZFDHREF   TO   LW_DHREF.

   CASE   P_ZFDHDOC.
      WHEN 'APP700' OR 'ZPP707' OR 'LOCAPP' OR 'LOCAMR' OR 'APPPUR'.
         SELECT SINGLE * INTO L_ZTREQST FROM ZTREQST
                         WHERE ZFDOCNO   EQ   P_ZFDHENO.
         IF SY-SUBRC EQ 0.
            SET PARAMETER ID 'ZPREQNO' FIELD L_ZTREQST-ZFREQNO.
            SET PARAMETER ID 'ZPAMDNO' FIELD L_ZTREQST-ZFAMDNO.
            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
            IF L_ZTREQST-ZFAMDNO IS INITIAL.
               CALL TRANSACTION 'ZIM03'   AND SKIP FIRST SCREEN.
            ELSE.
               CALL TRANSACTION 'ZIM13'   AND SKIP FIRST SCREEN.
            ENDIF.
         ELSE.
            MESSAGE E250 WITH   P_ZFDHENO.
         ENDIF.
      WHEN 'APPCIP' OR 'APPEND'.    " 적하보?
         SELECT SINGLE * INTO L_ZTINS FROM ZTINS
                         WHERE ZFDOCNO   EQ    P_ZFDHENO.
         IF SY-SUBRC EQ 0.
            SET PARAMETER ID 'ZPREQNO' FIELD L_ZTINS-ZFREQNO.
            SET PARAMETER ID 'ZPAMDNO' FIELD L_ZTINS-ZFAMDNO.
            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
            SET PARAMETER ID 'ZPOPNNO' FIELD ''.
            IF L_ZTINS-ZFAMDNO IS INITIAL.
               CALL TRANSACTION 'ZIM43'   AND SKIP FIRST SCREEN.
            ELSE.
               CALL TRANSACTION 'ZIM47'   AND SKIP FIRST SCREEN.
            ENDIF.
         ELSE.
            MESSAGE E250 WITH   P_ZFDHENO.
         ENDIF.
      WHEN 'DOMOFR'.       " LOCAL OFFER SHEET
         SELECT SINGLE * INTO L_ZTOFF FROM ZTOFF
                         WHERE ZFDOCNO   EQ   P_ZFDHENO.
         IF SY-SUBRC EQ 0.
            SET PARAMETER ID 'ZPREQNO' FIELD L_ZTOFF-ZFREQNO.
            SET PARAMETER ID 'BES'     FIELD ''.
            CALL TRANSACTION 'ZIML3'   AND SKIP FIRST SCREEN.
         ELSE.
            MESSAGE E250 WITH   P_ZFDHENO.
         ENDIF.
      WHEN 'APPLOG'.       " L/G
         SELECT SINGLE * INTO L_ZTLG     FROM ZTLG
                         WHERE ZFDOCNO   EQ   P_ZFDHENO.
         IF SY-SUBRC EQ 0.
            SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
            SET PARAMETER ID 'ZPBLNO'   FIELD L_ZTLG-ZFBLNO.
            SET PARAMETER ID 'ZPLGSEQ'  FIELD L_ZTLG-ZFLGSEQ.
            CALL TRANSACTION 'ZIM28'   AND SKIP FIRST SCREEN.
         ELSE.
            MESSAGE E250 WITH   P_ZFDHENO.
         ENDIF.

*      WHEN 'APP700' OR 'APPPUR' OR 'LOCAPP'.
*
*         SET PARAMETER ID 'ZPREQNO' FIELD P_ZFDHREF.
*         SET PARAMETER ID 'ZPAMDNO' FIELD ''.
*         SET PARAMETER ID 'ZPOPNNO' FIELD ''.
*         CALL TRANSACTION 'ZIM03'   AND SKIP FIRST SCREEN.
*      WHEN 'APP707' OR 'LOCAMR'.
*         SET PARAMETER ID 'ZPREQNO' FIELD LW_DHREF(10).
*         SET PARAMETER ID 'ZPAMDNO' FIELD LW_DHREF+11(5).
*         SET PARAMETER ID 'ZPOPNNO' FIELD ''.
*         CALL TRANSACTION 'ZIM13'   AND SKIP FIRST SCREEN.
      WHEN OTHERS.
         MESSAGE E033 WITH  P_ZFDHDOC.
   ENDCASE.

ENDFORM.                    " P2000_SHOW_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.

  IF S_DHSST IS INITIAL.
     MOVE: 'I'                TO S_DHSST-SIGN,
           'EQ'               TO S_DHSST-OPTION,
           'S_OK'             TO S_DHSST-LOW.
   APPEND S_DHSST.
     MOVE: 'I'                TO S_DHSST-SIGN,
           'EQ'               TO S_DHSST-OPTION,
           ''                 TO S_DHSST-LOW.
   APPEND S_DHSST.
  ENDIF.

  IF S_DHTRA IS INITIAL.
     MOVE: 'I'                TO S_DHTRA-SIGN,
           'EQ'               TO S_DHTRA-OPTION,
           'T_OK'             TO S_DHTRA-LOW.
   APPEND S_DHTRA.
     MOVE: 'I'                TO S_DHTRA-SIGN,
           'EQ'               TO S_DHTRA-OPTION,
           ''                 TO S_DHTRA-LOW.
   APPEND S_DHTRA.
  ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_REPORT
*&---------------------------------------------------------------------*
FORM P2000_SHOW_REPORT USING     P_ZFDHENO   P_ZFDHDOC   P_ZFDHREF.

   CASE   P_ZFDHDOC.
      WHEN 'APP700'.
*        SUBMIT  ZRIMMLCO01  WITH S_ZFDNO EQ P_ZFDHREF SIGN 'I'
*                 AND RETURN.
      WHEN 'APP707'.
*        SUBMIT  ZRIMMLCA01  WITH S_ZFDNO EQ P_ZFDHENO SIGN 'I'
*                 AND RETURN.
      WHEN OTHERS.
         MESSAGE E033 WITH  P_ZFDHDOC.
   ENDCASE.

ENDFORM.                    " P2000_SHOW_REPORT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_FLAT
*&---------------------------------------------------------------------*
FORM P2000_SHOW_FLAT USING    P_ZFDHENO.

    SUBMIT  ZRIMFLAT_DISP  WITH P_DDENO  EQ P_ZFDHENO
            AND RETURN.

ENDFORM.                    " P2000_SHOW_FLAT
*&---------------------------------------------------------------------*
*&      Form  P3000_EDI_SEND_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_EDI_SEND_UPDATE.

   REFRESH : IT_ZTDHF1, IT_ERR_ZTDHF1.

   LOOP AT IT_SELECTED.
      SELECT SINGLE * INTO CORRESPONDING FIELDS OF IT_ZTDHF1
*     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTDHF1
                    FROM   ZTDHF1
                    WHERE  ZFDHENO EQ IT_SELECTED-ZFDHENO.
      APPEND IT_ZTDHF1.
   ENDLOOP.
* 변환 여부 체?
   W_COUNT = 0.
   LOOP AT IT_ZTDHF1.
      W_TABIX   =    SY-TABIX.
* 전송일시가 존재하면 안됨...
      IF NOT ( IT_ZTDHF1-ZFDHJSD IS INITIAL AND
               IT_ZTDHF1-ZFDHJSH IS INITIAL ).
         DELETE IT_ZTDHF1 INDEX W_TABIX.
         MESSAGE I246 WITH IT_ZTDHF1-ZFDHENO.
      ELSE.
         W_COUNT = W_COUNT + 1.
      ENDIF.
   ENDLOOP.
   IF W_COUNT EQ 0.   MESSAGE E247.   ENDIF.

* SEND CALL
   CALL FUNCTION 'ZIM_SEND_TO_EDIFEP'
        TABLES
            IT_ZTDHF1        =    IT_ZTDHF1
            IT_ERR_ZTDHF1    =    IT_ERR_ZTDHF1.

   LOOP AT IT_ERR_ZTDHF1.
     CONCATENATE '전자문서[' IT_ERR_ZTDHF1-ZFDHENO ']를 송신 중 오류가 '
                 '발생하였습니다.'      INTO W_TEXT
                 SEPARATED BY SPACE.
     PERFORM   P3000_SET_ERROR_TEXT  USING W_TEXT.
   ENDLOOP.

   DESCRIBE TABLE IT_ERR_ZTDHF1 LINES W_LINE.
   IF W_LINE NE 0.
      MESSAGE I996 WITH W_LINE.
   ENDIF.

ENDFORM.                    " P3000_EDI_SEND_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P3000_LOG_DISPLAY
*&---------------------------------------------------------------------*
FORM P3000_LOG_DISPLAY.
  DESCRIBE TABLE IT_ERR_TEXT  LINES W_COUNT.
  IF W_COUNT EQ 0.
     MESSAGE E145.
  ENDIF.

  SET PF-STATUS 'ZIMLG'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMLG'.           " GUI TITLE SETTING..
  FORMAT RESET.
  SKIP 2.
  WRITE : /50  ' [ ERROR LOG 현황 ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' 문서관리번호 ',
            SY-VLINE, 'Doc.Ty',
            SY-VLINE, '    내부관리번호    ',
            SY-VLINE, 80 '    LOG    ',   119 SY-VLINE.
  WRITE : / SY-ULINE.
  LOOP AT IT_ERR_TEXT.
     W_LINE = SY-TABIX.
     W_NMOD = W_LINE MOD 2.
     IF W_NMOD EQ 1.    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
     ELSE.              FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     ENDIF.

     WRITE:/ SY-VLINE, IT_ERR_TEXT-ZFDHENO,
             SY-VLINE, IT_ERR_TEXT-ZFDHDOC,
             SY-VLINE, IT_ERR_TEXT-ZFDHREF,
             SY-VLINE, IT_ERR_TEXT-ERROR_TEXT NO-GAP,
             SY-VLINE.
  ENDLOOP.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  WRITE : / 'Total Count :', W_LINE NO-GAP, '건'.

ENDFORM.                    " P3000_LOG_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  P3000_SET_ERROR_TEXT
*&---------------------------------------------------------------------*
FORM P3000_SET_ERROR_TEXT USING    P_TEXT.

  CLEAR : IT_ERR_TEXT.

  MOVE-CORRESPONDING IT_ERR_ZTDHF1 TO IT_ERR_TEXT.
  MOVE : P_TEXT                    TO IT_ERR_TEXT-ERROR_TEXT.

  APPEND    IT_ERR_TEXT.

ENDFORM.                    " P3000_SET_ERROR_TEXT
