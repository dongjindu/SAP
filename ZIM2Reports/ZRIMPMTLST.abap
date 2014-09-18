*&---------------------------------------------------------------------*
*& Report  ZRIMPMTLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 :Payment Notice EDI 수신현?
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.08.16
*$     적용회사: LG 화학.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMPMTLST   MESSAGE-ID ZIM
                     LINE-SIZE 210
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
 INCLUDE   <ICON>.
 INCLUDE   ZRIMPMTTOP.
 INCLUDE   ZRIMSORTCOM.    " Report Sort를 위한 Include
 INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C.
DATA : END OF IT_ERR_LIST.


DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA: W_BUTTON_ANSWER   TYPE C,
      W_TABIX           LIKE SY-TABIX,
      W_MOD             TYPE I,
      INCLUDE(08)       TYPE C.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS: S_EBELN  FOR  ZTPMTEDI-EBELN,   " P/O No
                   S_OPNNO  FOR  ZTPMTEDI-ZFOPNNO, " L/C No
                   S_HBLNO  FOR  ZTPMTEDI-ZFHBLNO, " B/L No
                   S_ISNO   FOR  ZTPMTEDI-ZFISNO,  " 인수증발급번호.
                   S_NGDT   FOR  ZTPMTEDI-NEGODT,  " NEGO DATE.
                   S_NTDT   FOR  ZTPMTEDI-ZFNTDT,  " 통지일.
                   S_PYDT   FOR  ZTPMTEDI-ZFPYDT,  " 결제완료일.
                   S_DHDOC  FOR  ZTPMTEDI-ZFDHDOC, " 전자문서명.
                   S_OPBN   FOR  ZTPMTEDI-ZFRCVNM, " 발급은행.
                   S_PNBN   FOR  ZTPMTEDI-ZFADVNM. " 통지은행.
SELECTION-SCREEN END OF BLOCK B1.

*>>> 반제여부 상태..
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(17) TEXT-003, POSITION 1.
      SELECTION-SCREEN : COMMENT 31(8) TEXT-010, POSITION 40.
      PARAMETERS : P_Y   AS CHECKBOX.               " Yes.
      SELECTION-SCREEN : COMMENT 46(8) TEXT-011, POSITION 55.
      PARAMETERS : P_N   AS CHECKBOX DEFAULT 'X'.   " No.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.


* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
    PERFORM   P2000_SET_PARAMETER.
* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
     PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 권한 검증 함수.
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  테이블 SELECT
   PERFORM   P1000_GET_ZTPMTEDI     USING   W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.   EXIT.  ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
       WHEN 'STUP' OR 'STDN'.         " SORT 선택.
          W_FIELD_NM = 'ZFDHENO'.
          ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
          PERFORM HANDLE_SORT TABLES  IT_TAB
                              USING   SY-UCOMM.
* 전체 선택 및 선택해제.
      WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
          PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP'.
          PERFORM P2000_MULTI_SELECTION.
          IF W_SELECTED_LINES EQ 1.
             READ TABLE IT_SELECTED INDEX 1.
             IF IT_SELECTED-ZFPNNO IS INITIAL.
                MESSAGE E977 WITH '아직 미반영된 건입니다.'.
             ENDIF.
             PERFORM P2000_SHOW_PN USING  IT_SELECTED-ZFPNNO.
          ELSEIF W_SELECTED_LINES GT 1.
             MESSAGE E965.
          ENDIF.
      WHEN 'DISP1'.                    " 구매오더조회.
          PERFORM P2000_MULTI_SELECTION.
          IF W_SELECTED_LINES EQ 1.
             READ TABLE IT_SELECTED INDEX 1.
             IF IT_SELECTED-EBELN IS INITIAL.
                MESSAGE E977 WITH '참조 할 구매오더가 없습니다.'.
             ENDIF.
             PERFORM P2000_SHOW_PO USING  IT_SELECTED-EBELN.
          ELSEIF W_SELECTED_LINES GT 1.
             MESSAGE E965.
          ENDIF.
      WHEN 'DISP2'.                    " L/C 조회.
          PERFORM P2000_MULTI_SELECTION.
          IF W_SELECTED_LINES EQ 1.
             READ TABLE IT_SELECTED INDEX 1.
             PERFORM P2000_SHOW_LC USING  IT_SELECTED-ZFOPNNO.

          ELSEIF W_SELECTED_LINES GT 1.
             MESSAGE E965.
          ENDIF.
       WHEN 'DISP3'.                    " B/L 조회.
          PERFORM P2000_MULTI_SELECTION.
          IF W_SELECTED_LINES EQ 1.
             READ TABLE IT_SELECTED INDEX 1.
             PERFORM P2000_SHOW_BL USING  IT_SELECTED-ZFHBLNO.
          ELSEIF W_SELECTED_LINES GT 1.
             MESSAGE E965.
          ENDIF.
       WHEN 'DISP4'.                    " 인수증 조회.
          PERFORM P2000_MULTI_SELECTION.
          IF W_SELECTED_LINES EQ 1.
             READ TABLE IT_SELECTED INDEX 1.
             PERFORM P2000_SHOW_RED USING  IT_SELECTED-ZFISNO.
          ELSEIF W_SELECTED_LINES GT 1.
             MESSAGE E965.
          ENDIF.
      WHEN 'FB03'.
          PERFORM P2000_MULTI_SELECTION.
          IF W_SELECTED_LINES NE 0.
             PERFORM P2000_POPUP_MESSAGE USING
                    'Confirmation'
                    '선택한 문서를 Notice 반영처리 하시겠습니까?'
                    '확    인'
                    '아 니 오'
                    '1'
                     W_BUTTON_ANSWER.

            IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
               PERFORM P2000_CHECK_STATUS  USING   W_ERR_CHK.

               DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.

               IF W_LINE GT 0.
                  INCLUDE = 'POPU'.
                  CALL SCREEN 0100 STARTING AT  05   3
                                   ENDING   AT  100 12.
                  CLEAR : INCLUDE.
               ENDIF.

               LEAVE TO SCREEN 0.
            ENDIF.
         ENDIF.

      WHEN 'REFR'.
            PERFORM   P1000_GET_ZTPMTEDI     USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
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

  CLEAR : IT_ERR_LIST, IT_TAB, W_TABIX.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
   CASE INCLUDE.
      WHEN 'POPU'.
         IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
*            MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
*                    NUMBER IT_ERR_LIST-MSGNR
*                    WITH   IT_ERR_LIST-MSGV1
*                           IT_ERR_LIST-MSGV2
*                           IT_ERR_LIST-MSGV3
*                           IT_ERR_LIST-MSGV4.
            CALL FUNCTION 'MASS_MESSAGE_SHOW_LONGTEXT'
                 EXPORTING
                    SPRSL     = SY-LANGU
                    ARBGB     = IT_ERR_LIST-MSGID
                    MSGNR     = IT_ERR_LIST-MSGNR
                    MSGV1     = IT_ERR_LIST-MSGV1
                    MSGV2     = IT_ERR_LIST-MSGV2
                    MSGV3     = IT_ERR_LIST-MSGV3
                    MSGV4     = IT_ERR_LIST-MSGV4
                 EXCEPTIONS
                    NOT_FOUND = 1
                    OTHERS    = 2.

         ENDIF.
     WHEN OTHERS.
  ENDCASE.
  CLEAR : IT_ERR_LIST, IT_TAB, W_TABIX.






*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

*  W_ERR_CHK = 'N'.
**----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*----------------------------------------------------------------------
*  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*           ID 'ACTVT' FIELD '*'.
*  IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTPMTEDI
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTPMTEDI   USING   W_ERR_CHK.

RANGES : R_DBYN  FOR  ZTPMTEDI-ZFDBYN OCCURS 2.

  W_ERR_CHK = 'N'.

  IF P_N EQ SPACE AND P_Y EQ SPACE.
     MESSAGE S204(ZIM1).
     W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

  IF P_N EQ 'X'.
     CLEAR : R_DBYN.
     MOVE : 'I'         TO  R_DBYN-SIGN,
            'EQ'        TO  R_DBYN-OPTION,
            'N'         TO  R_DBYN-LOW.
     APPEND  R_DBYN.
  ENDIF.

  IF P_Y EQ 'X'.
     CLEAR : R_DBYN.
     MOVE : 'I'         TO  R_DBYN-SIGN,
            'EQ'        TO  R_DBYN-OPTION,
            'Y'         TO  R_DBYN-LOW.
     APPEND  R_DBYN.
  ENDIF.

  W_ERR_CHK = 'N'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM   ZTPMTEDI
      WHERE  EBELN   IN   S_EBELN  "P/O No
      AND    ZFOPNNO IN   S_OPNNO  "L/C No
      AND    ZFHBLNO IN   S_HBLNO  "B/L No
      AND    ZFISNO  IN   S_ISNO   "인수증발급번호.
      AND    NEGODT  IN   S_NGDT   "NEGO DATE.
      AND    ZFNTDT  IN   S_NTDT   "통지일.
      AND    ZFPYDT  IN   S_PYDT   "결제완료일.
      AND    ZFDHDOC IN   S_DHDOC  "전자문서명.
      AND    ZFRCVNM IN   S_OPBN   "발급은행.
      AND    ZFDBYN  IN   R_DBYN   "반영여부.
      AND    ZFADVNM IN   S_PNBN.  "통지은행.

   IF SY-SUBRC NE 0.
      MESSAGE S738.
      W_ERR_CHK = 'Y'.
      EXIT.
   ENDIF.

*>> MODIFY.
   PERFORM   P1000_GET_TEXT.

ENDFORM.                    " P1000_GET_ZTPMTEDI
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIME05'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIME05'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_LINE = W_LINE + 1.
      PERFORM P2000_PAGE_CHECK.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         WRITE : / '총', W_COUNT, '건'.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE:/ SY-VLINE,  MARKFIELD  AS CHECKBOX,
          SY-VLINE,(17)IT_TAB-ZFDHENO," '문서관리번호',
          SY-VLINE,(20)IT_TAB-ZFRCVNM," '수신업체명',
          SY-VLINE,(20)IT_TAB-EBELN,  " '구매문서번호',
          SY-VLINE,(20)IT_TAB-ZFHBLNO," 'House B/L No',
          SY-VLINE,(03)IT_TAB-ZFPNAMC,
                   (16)IT_TAB-ZFPNAM  " 'Notice Amount',
                    CURRENCY IT_TAB-ZFPNAMC,
          SY-VLINE,(03)IT_TAB-ZFUSITC,
                   (16)IT_TAB-ZFUSIT
                       CURRENCY IT_TAB-ZFUSITC , " 'Usance Interest',
          SY-VLINE,(14)IT_TAB-ZFEXRT, " '환율',
          SY-VLINE,(10)IT_TAB-ZFNTDT, " '통지일',
          SY-VLINE,(10)IT_TAB-ZFPWDT, " '만기일',
          SY-VLINE,(10)IT_TAB-NEGODT, " 'Nego 일자',
          SY-VLINE,(10)IT_TAB-ZFDBTM, " '반영시간',
          SY-VLINE.
  HIDE: IT_TAB.
  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  DATA: W_DOM_TEX1 LIKE DD07T-DDTEXT.
  PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDDHDOC' IT_TAB-ZFDHDOC
                                   CHANGING   W_DOM_TEX1.
  WRITE:/ SY-VLINE,' ',
          SY-VLINE,(17)W_DOM_TEX1,     " '전자문서명',
          SY-VLINE,(20)IT_TAB-ZFADVNM, "'통지은행명',
          SY-VLINE,(20)IT_TAB-ZFOPNNO, "'L/C No',
          SY-VLINE,(20)IT_TAB-ZFISNO,  "'인수증 발급번호',
          SY-VLINE,(03)IT_TAB-ZFBKCHC,
                   (16)IT_TAB-ZFBKCH
                       CURRENCY IT_TAB-ZFBKCHC,  "'Banking Charge',
          SY-VLINE,(20)IT_TAB-ZFMOA
                       CURRENCY IT_TAB-WAERS,   "'원화금액',
          SY-VLINE,(14)IT_TAB-ZFUSIT
                       CURRENCY IT_TAB-ZFUSITC,  " 'Usance이자율',
          SY-VLINE,(10)IT_TAB-ZFDSDT,  " '할인일',
          SY-VLINE,(10)IT_TAB-ZFPYDT,  " '결제완료일',
          SY-VLINE,(10)IT_TAB-ZFDBDT,  " '반영일자',
          SY-VLINE,(10)IT_TAB-ZFDBID,  " '반영자',
          SY-VLINE.
   WRITE:/ SY-ULINE.
   W_COUNT = W_COUNT + 1.
   HIDE: IT_TAB.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_TEXT
*&---------------------------------------------------------------------*
FORM P1000_GET_TEXT.

ENDFORM.                    " P1000_GET_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /80 '[ Payment Notice EDI 수신현황 ]'.
  WRITE : /3 'Date : ', SY-DATUM, 190 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE,' ',
          SY-VLINE,(17) '문서관리번호',
          SY-VLINE,(20) '수신업체명',
          SY-VLINE,(20) '구매문서번호',
          SY-VLINE,(20) 'House B/L No',
          SY-VLINE,(20) 'Notice Amount',
          SY-VLINE,(20) 'Usance Interest',
          SY-VLINE,(14) '환율',
          SY-VLINE,(10) '통지일',
          SY-VLINE,(10) '만기일',
          SY-VLINE,(10) 'Nego 일자',
          SY-VLINE,(10) '반영시간', SY-VLINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ SY-VLINE,' ',
          SY-VLINE,(17) '전자문서명',
          SY-VLINE,(20) '통지은행명',
          SY-VLINE,(20) 'L/C No',
          SY-VLINE,(20) '인수증 발급번호',
          SY-VLINE,(20) 'Banking Charge',
          SY-VLINE,(20) '원화금액',
          SY-VLINE,(14) 'Usance이자율',
          SY-VLINE,(10) '할인일',
          SY-VLINE,(10) '결제완료일',
          SY-VLINE,(10) '반영일자',
          SY-VLINE,(10) '반영자',
          SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

   SET  TITLEBAR 'ZIME05'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
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
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-EBELN    TO IT_SELECTED-EBELN,
             IT_TAB-ZFDHENO  TO IT_SELECTED-ZFDHENO,
             IT_TAB-ZFOPNNO  TO IT_SELECTED-ZFOPNNO,
             IT_TAB-ZFHBLNO  TO IT_SELECTED-ZFHBLNO,
             IT_TAB-ZFPNNO   TO IT_SELECTED-ZFPNNO,
             IT_TAB-ZFISNO   TO IT_SELECTED-ZFISNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.
  IF W_SELECTED_LINES EQ 0.
      MESSAGE S951.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PO
*&---------------------------------------------------------------------*
FORM P2000_SHOW_PO USING    P_EBELN.

  SELECT SINGLE *
     FROM EKKO
     WHERE EBELN = P_EBELN.
  IF SY-SUBRC NE 0.
      MESSAGE E977 WITH '구매오더가 없습니다.'.
  ENDIF.
  SET PARAMETER ID 'BES' FIELD P_EBELN.
  SET PARAMETER ID 'BSP' FIELD ' '.

  CALL TRANSACTION 'ME23' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_PO
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFOPNNO.

   DATA: W_MAX_AMD LIKE ZTREQST-ZFAMDNO.
   SELECT SINGLE *
     FROM ZTREQST
    WHERE ZFOPNNO = P_ZFOPNNO.
   IF SY-SUBRC NE 0.
      MESSAGE E009.
   ENDIF.
   SELECT MAX( ZFAMDNO ) INTO W_MAX_AMD
       FROM ZTREQST
       WHERE ZFOPNNO = P_ZFOPNNO.

   SET PARAMETER ID 'ZPREQNO' FIELD ''.
   SET PARAMETER ID 'ZPAMDNO' FIELD ''.
   SET PARAMETER ID 'ZPOPNNO' FIELD  P_ZFOPNNO.
   SET PARAMETER ID 'BES'     FIELD ''.

   IF W_MAX_AMD EQ '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFHBLNO.

   DATA: W_MAX_BLNO LIKE ZTBL-ZFBLNO.
   SELECT SINGLE *
     FROM ZTBL
    WHERE ZFHBLNO = P_ZFHBLNO.
   IF SY-SUBRC NE 0.
      MESSAGE E897.
   ENDIF.
   SELECT MAX( ZFBLNO ) INTO W_MAX_BLNO
          FROM ZTBL
          WHERE ZFHBLNO = P_ZFHBLNO.
   SET PARAMETER ID 'ZPBLNO'  FIELD W_MAX_BLNO.
   SET PARAMETER ID 'ZPHBLNO' FIELD P_ZFHBLNO .
   CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_RED
*&---------------------------------------------------------------------*
FORM P2000_SHOW_RED USING    P_ZFISNO.

   DATA: W_MAX_REDNO LIKE ZTRED-ZFREDNO.
   SELECT SINGLE *
     FROM ZTRED
    WHERE ZFISNO = P_ZFISNO.
   IF SY-SUBRC NE 0.
      MESSAGE E854.
   ENDIF.

   SELECT MAX( ZFREDNO ) INTO W_MAX_REDNO
         FROM ZTRED
         WHERE ZFISNO = P_ZFISNO.

   SET PARAMETER ID 'ZPREDNO'  FIELD W_MAX_REDNO.
   CALL TRANSACTION 'ZIMA7' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_RED
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0250   text
*      -->P_0251   text
*      -->P_0252   text
*      -->P_0253   text
*      -->P_0254   text
*      -->P_W_BUTTON_ANSWER  text
*----------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE  USING VALUE(P_TITLE)
                                VALUE(P_QUESTION)
                                VALUE(P_BUTTON1)
                                VALUE(P_BUTTON2)
                                VALUE(P_DEFAULT)
                          CHANGING    P_ANSWER.

   CLEAR : P_ANSWER.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = P_TITLE
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   = P_QUESTION
             TEXT_BUTTON_1   = P_BUTTON1
             TEXT_BUTTON_2   = P_BUTTON2
             DEFAULT_BUTTON  = P_DEFAULT
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  P_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_CHECK_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_CHECK_STATUS USING    W_ERR_CHK.

   LOOP AT IT_SELECTED.
      W_TABIX = SY-TABIX.

*>>> 진행상태바..
      line = ( sy-tabix / w_selected_lines ) * 100.
      out_text = 'JOB PROGRESS %99999%%'.
      replace '%99999%' with line into out_text.
      perform p2000_show_bar using out_text line.


      SELECT SINGLE * FROM ZTPMTEDI
             WHERE ZFDHENO EQ IT_SELECTED-ZFDHENO.

      IF SY-SUBRC NE 0.
         MESSAGE S200(ZIM1) WITH IT_SELECTED-ZFDHENO.
         PERFORM  P2000_MESSAGE_MAKE(ZRIMSUBBIZFEE)
                                     TABLES IT_ERR_LIST
                                     USING  'E'.

         CONTINUE.
      ELSE.
         IF ZTPMTEDI-ZFDBYN EQ 'Y'.
            MESSAGE  S201(ZIM1) WITH ZTPMTEDI-ZFDHENO ZTPMTEDI-ZFPNNO.
            PERFORM  P2000_MESSAGE_MAKE(ZRIMSUBBIZFEE)
                                        TABLES IT_ERR_LIST
                                        USING  'E'.
            CONTINUE.
         ENDIF.
      ENDIF.

      CALL FUNCTION 'ZIM_BDC_CALL_TRANSACTION_ZIMP2'
           EXPORTING
               ZFDHENO    =    IT_SELECTED-ZFDHENO
           IMPORTING
               ZFPNNO     =    ZTPMTEDI-ZFPNNO
           TABLES
               RETURN     =    RETURN
           EXCEPTIONS
               OTHERS     =    4.

      IF SY-SUBRC EQ 0.
         IF ZTPMTEDI-ZFPNNO IS INITIAL.
            MESSAGE S202(ZIM1) WITH ZTPMTEDI-ZFDHENO.
            PERFORM  P2000_MESSAGE_MAKE(ZRIMSUBBIZFEE)
                                        TABLES IT_ERR_LIST
                                        USING  'E'.
         ELSE.
            MESSAGE S203(ZIM1) WITH ZTPMTEDI-ZFDHENO ZTPMTEDI-ZFPNNO.
            PERFORM  P2000_MESSAGE_MAKE(ZRIMSUBBIZFEE)
                                        TABLES IT_ERR_LIST
                                        USING  'S'.
         ENDIF.
      ELSE.
         IF RETURN[] IS INITIAL.
            MESSAGE S202(ZIM1) WITH ZTPMTEDI-ZFDHENO.
            PERFORM  P2000_MESSAGE_MAKE(ZRIMSUBBIZFEE)
                                        TABLES IT_ERR_LIST
                                        USING  'E'.
         ELSE.
            PERFORM  P2000_MULTI_MSG_MAKE  TABLES IT_ERR_LIST.
         ENDIF.
      ENDIF.

   ENDLOOP.


ENDFORM.                    " P2000_CHECK_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT.

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

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '메시지 LIST'.
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
         WRITE : / SY-ULINE(105), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 103 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(105).

         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)   NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(96) NO-GAP,
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
         WRITE : / SY-ULINE(105).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SELECTED_ZFPNNO  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_PN USING    P_ZFPNNO.

  SET PARAMETER ID 'ZPPNNO' FIELD P_ZFPNNO.

  CALL TRANSACTION 'ZIMP4' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_PN
