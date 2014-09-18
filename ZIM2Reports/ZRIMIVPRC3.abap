*&---------------------------------------------------------------------*
*& Report  ZRIMIVPRC3                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : G/R Cancel document matching                          *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2003.05.15                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Standard에서 직접 입고취소된 건들을 수입시스템에
*&               update시켜 상태를 맞춰주기위한 프로그램.
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMIVPRC3  MESSAGE-ID ZIM
                    LINE-SIZE 126
                    NO STANDARD PAGE HEADING.

INCLUDE : <ICON>,
           ZRIMBDCCOM.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMIVPRC3TOP.
INCLUDE   ZRIMSORTCOM.    " Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?

*-----------------------------------------------------------------------
* Selection Screen Clause.
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_MBLNR  FOR ZTIVHST-MBLNR,     "> 자재문서.
                S_EBELN  FOR ZTIVHSTIT-EBELN,     ">통관종류.
                S_BUDAT  FOR ZTIVHST-BUDAT,
                S_IVNO   FOR ZTIV-ZFIVNO,       ">양수/양도구분.
                S_ERNAM  FOR ZTIVHST-ERNAM.    " G/R처리자.
PARAMETERS : P_ITEM      AS CHECKBOX DEFAULT 'X'. " 자재출력여부.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_SET_INIT_VALUE.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION 건.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 TABLE SELECT
  PERFORM   P1000_READ_BASIC_DATA      USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*END-OF-SELECTION.
* Call ABAP/4 List Viewer
*  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
*       EXPORTING
*            I_STRUCTURE_NAME = 'ZTIV'
*       TABLES
*            T_OUTTAB         = IT_TAB.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택시.
      W_FIELD_NM = 'ZFREQDT'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

    WHEN 'MTCD'.
      W_OK_CODE1 = SY-UCOMM.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ELSEIF W_SELECTED_LINES GE 1.
*>>주 프로세스...
        W_SY_UCOMM = SY-UCOMM.
        PERFORM P2000_POPUP_MESSAGE USING
                'Cancel data'
                'Do you want to input cancelation data?'
                'Y e s'
                'N   o'
                '1'
                W_BUTTON_ANSWER.

        IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
* B/L 수정.
          PERFORM P3000_INPUT_DATA_SAVE.
          MESSAGE   S000(ZIM01) WITH W_UPDATE_CNT
                                     'matched cancelation doc.'.
* 정보 다시 읽기.
          PERFORM   P1000_READ_BASIC_DATA     USING   W_ERR_CHK.
          IF W_ERR_CHK EQ 'Y'.
            LEAVE TO SCREEN 0.
          ELSE.
            PERFORM RESET_LIST.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'MTCL'. " CANCEL 취소.
      W_OK_CODE1 = SY-UCOMM.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ELSEIF W_SELECTED_LINES GE 1.
*>>주 프로세스...
        W_SY_UCOMM = SY-UCOMM.
        PERFORM P2000_POPUP_MESSAGE USING
                'Cancel Matching'
                'Do you want to undo cancelation data?'
                'Y e s'
                'N   o'
                '1'
                W_BUTTON_ANSWER.

        IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
* B/L 수정.
          PERFORM P3000_INPUT_DATA_CANCEL.
          MESSAGE   S000(ZIM01) WITH W_UPDATE_CNT
                                     'undo matching cancelation doc.'.
* 정보 다시 읽기.
          PERFORM   P1000_READ_BASIC_DATA     USING   W_ERR_CHK.
          IF W_ERR_CHK EQ 'Y'.
            LEAVE TO SCREEN 0.
          ELSE.
            PERFORM RESET_LIST.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'AMTI'.                    " 통관요청 조회.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_IV USING IT_SELECTED-ZFIVNO.
        CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ENDIF.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
      PERFORM   P1000_READ_BASIC_DATA  USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.                " 종?
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
  CLEAR : IT_ERR_LIST.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
  CASE INCLUDE.
    WHEN 'POPU'.
      IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
        MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
                NUMBER IT_ERR_LIST-MSGNR
                WITH   IT_ERR_LIST-MSGV1
                       IT_ERR_LIST-MSGV2
                       IT_ERR_LIST-MSGV3
                       IT_ERR_LIST-MSGV4.
      ENDIF.
      CLEAR : IT_ERR_LIST.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_INIT_VALUE
*&---------------------------------------------------------------------*
FORM P2000_SET_INIT_VALUE.

ENDFORM.                    " P2000_SET_INIT_VALUE

*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  ' [ G/R Cancel document matching ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 100 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE, ' ',                   SY-VLINE NO-GAP,
         (15) 'Matrl doc.'         NO-GAP, SY-VLINE NO-GAP,
         (10) 'year'               NO-GAP, SY-VLINE NO-GAP,
         (20) 'Posting date'       NO-GAP, SY-VLINE NO-GAP,
         (45) 'Goods recipient'    NO-GAP, SY-VLINE NO-GAP,
         (10) 'G/R status'         NO-GAP, SY-VLINE NO-GAP,
         (10) 'CancelDoc.'         NO-GAP, SY-VLINE NO-GAP,
         (04) 'year'               NO-GAP, SY-VLINE NO-GAP.
  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE:/ SY-VLINE, ' ',                  SY-VLINE NO-GAP,
           (15) 'PO No.'            NO-GAP, SY-VLINE NO-GAP,
           (10) 'item'              NO-GAP, SY-VLINE NO-GAP,
           (20) 'Material code'     NO-GAP, SY-VLINE NO-GAP,
           (45) 'Description'       NO-GAP, SY-VLINE NO-GAP,
           (26) 'G/R Quantity'      NO-GAP, SY-VLINE NO-GAP.
  ENDIF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

  SET TITLEBAR  'ZIM56' .
  SET PF-STATUS 'ZIM56' .

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.

  ENDLOOP.
ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  ON CHANGE OF IT_TAB-MBLNR .
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX, SY-VLINE NO-GAP,
           (15) IT_TAB-MBLNR   NO-GAP, SY-VLINE NO-GAP,  " Material doc.
           (10) IT_TAB-MJAHR   NO-GAP, SY-VLINE NO-GAP,  " Year
           (20) IT_TAB-BUDAT   NO-GAP, SY-VLINE NO-GAP,  " Posting date
           (45) IT_TAB-ERNAM   NO-GAP, SY-VLINE NO-GAP.  " Created by

    CLEAR ZTIV.
    SELECT SINGLE * FROM ZTIVHST
            WHERE ZFIVNO  EQ  IT_TAB-ZFIVNO
              AND ZFIVHST EQ  IT_TAB-ZFIVHST.
*>> 입고 상태.
    IF ZTIVHST-ZFGRST EQ 'Y'.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
    ELSEIF ZTIVHST-ZFGRST EQ 'P'.
      FORMAT COLOR COL_GROUP    INTENSIFIED OFF.
    ENDIF.
*>> G/R 상태 TEXT.
    CASE ZTIVHST-ZFGRST.
      WHEN 'Y'.
        W_ZFGRST_NM = 'Completed'.
      WHEN 'P'.
        W_ZFGRST_NM = 'PartialG/R'.
    ENDCASE.

    WRITE : (10) W_ZFGRST_NM NO-GAP, SY-VLINE NO-GAP.
    CLEAR : ZTIVHST.
*>> 취소 문서.
    CLEAR  : W_CMBLNR, W_CMJAHR.
    MOVE   : IT_TAB-CMBLNR     TO  W_CMBLNR,
             IT_TAB-CMJAHR     TO  W_CMJAHR.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
    WRITE: (10) W_CMBLNR INPUT ON   NO-GAP
                         COLOR COL_POSITIVE INTENSIFIED ON,
               SY-VLINE NO-GAP,
           (04) W_CMJAHR INPUT ON   NO-GAP
                         COLOR COL_POSITIVE INTENSIFIED ON,
               SY-VLINE NO-GAP.
    W_COUNT =  W_COUNT + 1.
    HIDE: IT_TAB.
  ENDON.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE:/ SY-VLINE, ' ',             SY-VLINE NO-GAP,
           (15) IT_TAB-EBELN   NO-GAP, SY-VLINE NO-GAP,
           (10) IT_TAB-EBELP   NO-GAP, SY-VLINE NO-GAP,
           (20) IT_TAB-MATNR   NO-GAP, SY-VLINE NO-GAP,
           (45) IT_TAB-TXZ01   NO-GAP, SY-VLINE NO-GAP,
           (17) IT_TAB-GRMENGE    UNIT  IT_TAB-MEINS
                                RIGHT-JUSTIFIED NO-GAP,
           (09) IT_TAB-MEINS   NO-GAP, SY-VLINE NO-GAP.
  ENDIF.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  WRITE : / SY-ULINE.
  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / 'Total', W_COUNT, 'case'.
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
*&      Form  P1000_READ_BASIC_DATA
*&---------------------------------------------------------------------*
FORM P1000_READ_BASIC_DATA  USING    W_ERR_CHK.
  REFRESH :IT_TAB.
  W_ERR_CHK = 'N'.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
        FROM ZTIVHST AS H INNER JOIN ZTIVHSTIT AS I
          ON   H~ZFIVNO   EQ    I~ZFIVNO
         AND   H~ZFIVHST  EQ    I~ZFIVHST
       WHERE   H~ZFIVNO   IN    S_IVNO
         AND   H~MBLNR    IN    S_MBLNR
         AND   H~BUDAT    IN    S_BUDAT
         AND   H~ERNAM    IN    S_ERNAM
         AND   I~EBELN    IN    S_EBELN
         AND ( H~ZFGRST   EQ    'Y' OR
               H~ZFGRST   EQ    'P' ).

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S966.   EXIT.
  ENDIF.

ENDFORM.                    " P1000_READ_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IV
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IV USING    P_ZFIVNO.

  SET PARAMETER ID 'ZPHBLNO'  FIELD ''.
  SET PARAMETER ID 'ZPBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPIVNO'   FIELD P_ZFIVNO.

ENDFORM.                    " P2000_SHOW_IV
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  REFRESH IT_SELECTED.
  CLEAR : IT_SELECTED, W_SELECTED_LINES.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      READ LINE SY-INDEX FIELD VALUE  W_CMBLNR.
      READ LINE SY-INDEX FIELD VALUE  W_CMJAHR.
      MOVE : IT_TAB-ZFIVNO   TO   IT_SELECTED-ZFIVNO,
             IT_TAB-ZFIVHST  TO   IT_SELECTED-ZFIVHST,
             IT_TAB-MBLNR    TO   IT_SELECTED-MBLNR,
             IT_TAB-MJAHR    TO   IT_SELECTED-MJAHR,
             W_CMBLNR        TO   IT_SELECTED-CMBLNR,
             W_CMJAHR        TO   IT_SELECTED-CMJAHR.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK USING    W_ERR_CHK.


ENDFORM.                    " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.
*  IF W_STATUS_CHK = 'D'.
*     SET PF-STATUS 'STDLISA'.
*  ELSE.
  SET PF-STATUS 'STDLISW'.
*  ENDIF.

  CASE INCLUDE.
    WHEN 'POPU'.
      SET TITLEBAR 'POPU' WITH 'Message LIST'.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

  MOVE : SY-MSGTY            TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
         IT_SELECTED-ZFIVNO  TO     IT_ERR_LIST-ZFIVNO.

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
    WHEN 'E'.
      MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
    WHEN 'I'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'S'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'W'.
      MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
  ENDCASE.

  APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE

*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE  USING VALUE(P_TITLE)
                                VALUE(P_QUESTION)
                                VALUE(P_BUTTON1)
                                VALUE(P_BUTTON2)
                                VALUE(P_DEFAULT)
                          CHANGING    P_ANSWER.

  CLEAR : P_ANSWER.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = P_TITLE
            DIAGNOSE_OBJECT       = ''
            TEXT_QUESTION         = P_QUESTION
            TEXT_BUTTON_1         = P_BUTTON1
            TEXT_BUTTON_2         = P_BUTTON2
            DEFAULT_BUTTON        = P_DEFAULT
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN          = 30
            START_ROW             = 8
       IMPORTING
            ANSWER                = P_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_INPUT_DATA_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_INPUT_DATA_SAVE.
  CLEAR : W_UPDATE_CNT.
  LOOP AT IT_SELECTED.
    CLEAR   : ZTIV, IT_ZSIVIT, ZTIVHST, IT_ZSIVHSTIT,
              ZTBL, IT_EKBE,   ZTIVHSTIT.
    REFRESH : IT_ZSIVIT, IT_ZSIVHSTIT, IT_EKBE.

*>> 취소문서 확인------------------------------------------->
    IF IT_SELECTED-CMBLNR IS INITIAL.
      MESSAGE I167 WITH 'material cancel doc' .
      CONTINUE.
    ENDIF.

    IF IT_SELECTED-CMJAHR IS INITIAL.
      MESSAGE I167 WITH 'material cancel doc year'.
      CONTINUE.
    ENDIF.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_EKBE
             FROM EKBE
            WHERE GJAHR   EQ   IT_SELECTED-CMJAHR
              AND BELNR   EQ   IT_SELECTED-CMBLNR
              AND SHKZG   EQ   'H'.

    IF SY-SUBRC NE 0.
      MESSAGE I064(ZIM1) WITH IT_SELECTED-CMBLNR IT_SELECTED-CMJAHR.
      CONTINUE.
    ENDIF.

*>> 수입시스템 데이타 확인-------------------------------------->
    SELECT SINGLE * FROM ZTIV
                   WHERE ZFIVNO  EQ IT_SELECTED-ZFIVNO.
    IF SY-SUBRC NE 0.
      MESSAGE  I413 WITH IT_SELECTED-ZFIVNO.
      CONTINUE.
    ENDIF.

*>>> 입고 상태 체크.
    CASE ZTIV-ZFGRST.
      WHEN 'X' OR 'N'.     ">미대상/입고대상.(오류)
        PERFORM   GET_DD07T_SELECT(SAPMZIM01)
                  USING      'ZDGRST'  ZTIV-ZFGRST
                  CHANGING    W_TEXT70.
        MESSAGE I422 WITH ZTIV-ZFIVNO W_TEXT70 'G/R Cancel'.
        CONTINUE.
      WHEN 'Y'.     ">입고완료.(NONE)
    ENDCASE.

*>> B/L DOCUMENT.
    SELECT SINGLE * FROM ZTBL
                    WHERE ZFBLNO   EQ   ZTIV-ZFBLNO.

    SELECT SINGLE * FROM ZTIVHST
                    WHERE ZFIVNO  EQ IT_SELECTED-ZFIVNO
                      AND ZFIVHST EQ IT_SELECTED-ZFIVHST.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
             FROM ZTIVIT
            WHERE ZFIVNO   EQ   IT_SELECTED-ZFIVNO
              AND UMSON    EQ   'X'.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHSTIT
             FROM ZTIVHSTIT
              FOR ALL ENTRIES  IN  IT_EKBE
            WHERE ZFIVNO   EQ   IT_SELECTED-ZFIVNO
              AND ZFIVHST  EQ   IT_SELECTED-ZFIVHST
              AND EBELN    EQ   IT_EKBE-EBELN
              AND EBELP    EQ   IT_EKBE-EBELP.

      IF SY-SUBRC NE 0.
        MESSAGE I065(ZIM1) WITH IT_SELECTED-CMBLNR ZTIVHST-MBLNR.
        CONTINUE.
      ENDIF.

*>> DATA INPUT.
      LOOP AT IT_ZSIVHSTIT.
        W_TABIX = SY-TABIX.
        IT_ZSIVHSTIT-ZFGRST = 'N'.
        MODIFY IT_ZSIVHSTIT INDEX W_TABIX.

        READ TABLE IT_ZSIVIT WITH KEY ZFIVNO  = IT_ZSIVHSTIT-ZFIVNO
                                     ZFIVDNO = IT_ZSIVHSTIT-ZFIVDNO.
        W_TABIX = SY-TABIX.
        IF SY-SUBRC EQ 0.
          READ TABLE IT_EKBE WITH KEY EBELN = IT_ZSIVIT-EBELN
                                      EBELP = IT_ZSIVIT-EBELP.
          IF SY-SUBRC EQ 0.
            IF IT_ZSIVIT-GRTOTMN NE 0.
              IT_ZSIVIT-GRTOTMN = IT_ZSIVIT-GRTOTMN
                                    - IT_EKBE-MENGE.
              IF IT_ZSIVIT-GRTOTMN LT 0.
                 IT_ZSIVIT-GRTOTMN = 0.
              ENDIF.
              MODIFY IT_ZSIVIT INDEX W_TABIX.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      W_ZFGRST = 'N'.
      LOOP AT IT_ZSIVIT.
        IF IT_ZSIVIT-GRMENGE NE IT_ZSIVIT-GRTOTMN AND
           IT_ZSIVIT-GRTOTMN NE 0.
          W_ZFGRST = 'P'.  EXIT.
        ENDIF.
      ENDLOOP.


      MOVE : W_ZFGRST          TO      ZTIV-ZFGRST,
             SY-DATUM          TO      ZTIV-UDAT,
             SY-UNAME          TO      ZTIV-UNAM.
      UPDATE  ZTIV.

      SELECT SINGLE * FROM ZTIVHSTIT
              WHERE ZFIVNO  EQ ZTIVHST-ZFIVNO
                AND ZFIVHST EQ ZTIVHST-ZFIVHST
                AND ZFGRST  NE 'N'.
      IF SY-SUBRC EQ 0.
         MOVE :  'N'         TO  ZTIVHST-ZFGRST.
      ELSE.
         MOVE :  'P'         TO  ZTIVHST-ZFGRST.
      ENDIF.

      MOVE : IT_SELECTED-CMBLNR  TO  ZTIVHST-CMBLNR,
             IT_SELECTED-CMJAHR  TO  ZTIVHST-CMJAHR,
             IT_EKBE-BUDAT       TO  ZTIVHST-CBUDAT,
             SY-UNAME            TO  ZTIVHST-UNAM,
             SY-DATUM            TO  ZTIVHST-UDAT.
*             SY-UZEIT            TO  ZTIVHST-UTM.
*             SY-TCODE            TO  ZTIVHST-ZFTCODE.

      UPDATE ZTIVHST.
      IF SY-SUBRC NE 0.
        MESSAGE I644 .
        CONTINUE.
      ENDIF.

    MODIFY ZTIVIT FROM TABLE IT_ZSIVIT.
    IF SY-SUBRC NE 0.
      MESSAGE I646 .  CONTINUE.
    ENDIF.

    MODIFY ZTIVHSTIT FROM TABLE IT_ZSIVHSTIT.
    IF SY-SUBRC NE 0.
      MESSAGE I645 .  CONTINUE.
    ENDIF.

      IF ZTBL-ZFELIKZ EQ 'X'.
        MOVE : SPACE         TO      ZTBL-ZFELIKZ,
               SY-UNAME      TO      ZTBL-UNAM,
               SY-DATUM      TO      ZTBL-UDAT.
        UPDATE  ZTBL.
      ENDIF.

      ADD   1    TO    W_UPDATE_CNT.
    ENDLOOP.

  ENDFORM.                    " P3000_INPUT_DATA_SAVE
*&---------------------------------------------------------------------*
*&      Form  P3000_INPUT_DATA_CANCEL
*&---------------------------------------------------------------------*
FORM P3000_INPUT_DATA_CANCEL.

ENDFORM.                    " P3000_INPUT_DATA_CANCEL
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
