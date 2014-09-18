*&---------------------------------------------------------------------*
*& Report  ZRIMPAYM                                                    *
*&---------------------------------------------------------------------*
*& Program Name : Payment Notice List                                  *
*&   Created by : Kim Yun-Joong INFOLINK Ltd.                          *
*& Created Date : 2000.02.22                                           *
*&---------------------------------------------------------------------*
*&   DESC.     : Diplay Payment Notice List.
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMPAYM    MESSAGE-ID ZIM
                    LINE-SIZE 161
                    NO STANDARD PAGE HEADING.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK          TYPE C,
       ZFPNNO        LIKE ZTPMTHD-ZFPNNO,   " Payment Notice 관리번?
       ZFPNBN        LIKE ZTPMTHD-ZFPNBN,   " 통지은행 거래처코?
       ZFPNBN_NM(16) TYPE C,                " 통지은행?
       ZFOPBN        LIKE ZTPMTHD-ZFOPBN,   " 개설은행 거래처코?
       ZFOPBN_NM(16) TYPE C,                " 개설은행?
       ZFOPNNO(15)   TYPE C,                " 신용장-승인번?
       EBELN         LIKE ZTPMTHD-EBELN,    " Purchasing document number
       ZTERM         LIKE ZTPMTHD-ZTERM,    " Terms of payment key
       ZFTRTY        LIKE ZTPMTHD-ZFTRTY,   " 거래유?
       ZFLCKN        LIKE ZTPMTHD-ZFLCKN,   " L/C 종류(Payment Notice)
       ZFPNAMC       LIKE ZTPMTHD-ZFPNAMC,  " Notice Currency
       ZFPNAM        LIKE ZTPMTHD-ZFPNAM,   " Notice Amount
       ZFTIVAM       LIKE ZTPMTHD-ZFTIVAM,  " Total Invoice Amount
       ZFNTDT        LIKE ZTPMTHD-ZFNTDT,   " 통지?
       ZFDSDT        LIKE ZTPMTHD-ZFDSDT,   " 할인?
       ZFPWDT        LIKE ZTPMTHD-ZFPWDT,   " 만기?
       ZFPYDT        LIKE ZTPMTHD-ZFPYDT,   " 결제완료?
       ZFUSIT        LIKE ZTPMTHD-ZFUSIT,   " Usance Interest
       ZFUSITC       LIKE ZTPMTHD-ZFUSITC,  " Usance Interest Currency
       ZFUSIRDT      LIKE ZTPMTHD-ZFUSIRDT, " Usance Interest 결제?
       ZFBKCH        LIKE ZTPMTHD-ZFBKCH,   " Banking Charge
       ZFBKCHC       LIKE ZTPMTHD-ZFBKCHC,  " Banking Charge Currency
       ZFBKCHDT      LIKE ZTPMTHD-ZFBKCHDT, " Banking Charge 결제?
       ZFPYA         LIKE ZTPMTHD-ZFPYA,    ">반제여부.
       ZFPYST        LIKE ZTPMTHD-ZFPYST.   " Payment Notice Status
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPAYMTOP.
INCLUDE   ZRIMSORTCOM.    " Include for Sorting.
INCLUDE   ZRIMUTIL01.     " Utility function Collection.

*-----------------------------------------------------------------------
* Selection Screen Clause.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_PNNO   FOR ZTPMTHD-ZFPNNO,  " Payment 관리번?
                   S_PNBN   FOR ZTPMTHD-ZFPNBN,  " 통지은?
                   S_OPBN   FOR ZTPMTHD-ZFOPBN,  " 개설은?
                   S_OPNNO  FOR ZTPMTHD-ZFOPNNO, " L/C No
                   S_EBELN  FOR ZTPMTHD-EBELN,   " P/O No
                   S_NTDT   FOR ZTPMTHD-ZFNTDT,  " 통지?
                   S_PWDT   FOR ZTPMTHD-ZFPWDT,  " 만기?
                   S_PYDT   FOR ZTPMTHD-ZFPYDT,  " 결제완료?
                   S_DSDT   FOR ZTPMTHD-ZFDSDT,  " 할인?
                   S_LCKN   FOR ZTPMTHD-ZFLCKN,  " L/C Type
                   S_TRTY   FOR ZTPMTHD-ZFTRTY.  " 거래유?
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
* Payment Notice Status
  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(19) TEXT-002, POSITION 1.
     SELECTION-SCREEN : COMMENT 30(10) TEXT-021, POSITION 41.
     PARAMETERS : P_N    AS CHECKBOX.       " Not Yet
     SELECTION-SCREEN : COMMENT 46(12) TEXT-022, POSITION 58.
     PARAMETERS : P_C    AS CHECKBOX.        " Confirm
     SELECTION-SCREEN : COMMENT 63(9)  TEXT-023, POSITION 74.
     PARAMETERS : P_P    AS CHECKBOX.        " Confirm
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_INIT.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
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

   CASE SY-UCOMM.
     WHEN 'STUP' OR 'STDN'.         " SORT 선택?
        W_FIELD_NM = 'ZFREQDT'.
        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
        PERFORM HANDLE_SORT TABLES  IT_TAB
                            USING   SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
            PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'CHDC'.                " Payment Notice 변경.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_PMTHD USING IT_SELECTED-ZFPNNO.
               CALL TRANSACTION 'ZIMP3' AND SKIP FIRST SCREEN.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
      WHEN 'DISP'.                " Payment Notice 조회.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES EQ 1.
               READ TABLE IT_SELECTED INDEX 1.
               PERFORM P2000_SHOW_PMTHD USING IT_SELECTED-ZFPNNO.
               CALL TRANSACTION 'ZIMP4' AND SKIP FIRST SCREEN.
            ELSEIF W_SELECTED_LINES GT 1.
               MESSAGE E965.
            ENDIF.
      WHEN 'SAV1'.
            PERFORM P2000_MULTI_SELECTION.
            IF W_SELECTED_LINES NE 0.
* Message Box
               PERFORM P2000_POPUP_MESSAGE USING
                    'Confirmation'
                    'Do you apply selected document?'
*                    '선택한 문서로 반영하시겠습니까?'
*                    '확    인'
*                    '아 니 오'
                    ' Y E S '
                    ' N   O '
                    '1'
                    W_BUTTON_ANSWER.

               IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
* NOTICE 수정.
                  PERFORM P3000_INPUT_DATA_SAVE.
* 정보 다시 읽기.
                  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
                  IF W_ERR_CHK EQ 'Y'.
                     LEAVE TO SCREEN 0.
                  ELSE.
                     PERFORM RESET_LIST.
                  ENDIF.
               ENDIF.
            ELSEIF W_SELECTED_LINES EQ 0.
               MESSAGE S766.
            ENDIF.

      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종?
      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  P2000_INIT
*&---------------------------------------------------------------------*
FORM P2000_INIT.

  P_N = 'X'.
*  P_C = 'X'.

  SET  TITLEBAR 'ZIMP1'.           " GUI TITLE SETTING..

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /60  '[ Payment Notice List ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 143 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',    SY-VLINE     NO-GAP,
            'Advising Bank              '  NO-GAP, SY-VLINE NO-GAP,
            'L/C No.        '              NO-GAP, SY-VLINE NO-GAP,
            'L/C Type'                     NO-GAP, SY-VLINE NO-GAP,
            'Py. Term'                     NO-GAP, SY-VLINE NO-GAP,
            '           Notice Amount'     NO-GAP, SY-VLINE NO-GAP,
            'Notice Dat'                   NO-GAP, SY-VLINE NO-GAP,
            ' Due Date '                   NO-GAP, SY-VLINE NO-GAP,
            '                Interest'     NO-GAP, SY-VLINE NO-GAP,
            'Settl.Date'                   NO-GAP, SY-VLINE NO-GAP,
            'Manage No.'                   NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',   SY-VLINE      NO-GAP,
            'Open Bank.                 '  NO-GAP, SY-VLINE NO-GAP,
            'P/O No         '              NO-GAP, SY-VLINE NO-GAP,
            'Trade.Ty'                     NO-GAP, SY-VLINE NO-GAP,
            'Py. Stat'                     NO-GAP, SY-VLINE NO-GAP,
            '    Total Invoice Amount'     NO-GAP, SY-VLINE NO-GAP,
            'Paymt.dat '                   NO-GAP, SY-VLINE NO-GAP,
            'Discnt.dat'                   NO-GAP, SY-VLINE NO-GAP,
            '          Banking Charge'     NO-GAP, SY-VLINE NO-GAP,
            '          '                   NO-GAP, SY-VLINE NO-GAP,
            '          '                   NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMP1'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMP1'.           " GUI TITLE SETTING..

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

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE          NO-GAP,
       IT_TAB-ZFPNBN     NO-GAP, " 통지은행 거래처코?
       ' '               NO-GAP,
       IT_TAB-ZFPNBN_NM  NO-GAP, " 통지은행?
       SY-VLINE          NO-GAP,
       IT_TAB-ZFOPNNO    NO-GAP, " L/C No
       SY-VLINE          NO-GAP,
       IT_TAB-ZFLCKN     NO-GAP, " L/C 종?
       '       '         NO-GAP,
       SY-VLINE          NO-GAP,
       IT_TAB-ZTERM      NO-GAP, " Payment Term
       '    '            NO-GAP,
       SY-VLINE          NO-GAP,
       IT_TAB-ZFPNAMC    NO-GAP, " Notice Currency
       IT_TAB-ZFPNAM CURRENCY IT_TAB-ZFPNAMC NO-GAP, " Notice Amount
       SY-VLINE          NO-GAP,
       IT_TAB-ZFNTDT     NO-GAP, " 통지?
       SY-VLINE          NO-GAP,
       IT_TAB-ZFPWDT     NO-GAP, " 만기?
       SY-VLINE          NO-GAP,
       IT_TAB-ZFUSITC    NO-GAP, " Usance Interest Currency
       IT_TAB-ZFUSIT CURRENCY IT_TAB-ZFUSITC NO-GAP, " Usance Interest
       SY-VLINE          NO-GAP,
       IT_TAB-ZFUSIRDT   NO-GAP, " Usance Interest 결제?
       SY-VLINE          NO-GAP,
       IT_TAB-ZFPNNO     NO-GAP,         " Payment Notice 관리번?
       SY-VLINE          NO-GAP.
* Hide
       MOVE SY-TABIX  TO W_LIST_INDEX.
       HIDE: W_LIST_INDEX, IT_TAB.
       MODIFY IT_TAB INDEX SY-TABIX.

       FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

       WRITE IT_TAB-ZFPYDT TO W_ZFPYDT.

       WRITE : / SY-VLINE, ' ', SY-VLINE NO-GAP,
       IT_TAB-ZFOPBN  NO-GAP, " 개설은행 거래처코?
       ' '      NO-GAP,
       IT_TAB-ZFOPBN_NM  NO-GAP, " 개설은행?
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   NO-GAP,         " P/O No
       '     '      NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFTRTY   NO-GAP, " 거래유?
       '       '      NO-GAP,
       SY-VLINE NO-GAP,
*       IT_TAB-ZFPYST   NO-GAP, " Payment Status
       IT_TAB-ZFPYA   NO-GAP,
       '       '      NO-GAP,
       SY-VLINE NO-GAP,
       '     '      NO-GAP,
       IT_TAB-ZFTIVAM CURRENCY IT_TAB-ZFPNAMC NO-GAP, " Invoice Amount
       SY-VLINE NO-GAP,
       W_ZFPYDT               NO-GAP INPUT ON
                              COLOR COL_POSITIVE
                              INTENSIFIED ON,
       SY-VLINE NO-GAP,
       IT_TAB-ZFDSDT   NO-GAP, " 할인?
       SY-VLINE NO-GAP,
       IT_TAB-ZFBKCHC   NO-GAP, " Banking Carge Currency
       IT_TAB-ZFBKCH CURRENCY IT_TAB-ZFBKCHC NO-GAP, " Banking Charge
       SY-VLINE NO-GAP,
       IT_TAB-ZFBKCHDT   NO-GAP, " Banking Charge 결제?
       SY-VLINE NO-GAP,
       '          '      NO-GAP,
       SY-VLINE NO-GAP.

* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE

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

  RANGES : R_ZFPYA  FOR ZTPMTHD-ZFPYA  OCCURS 2.

  MOVE 'N' TO W_ERR_CHK.

  REFRESH IT_TAB.
  IF P_N = ' ' AND P_C = ' ' AND P_P = ' '.
     CLEAR   IT_TAB.
     MOVE 'Y' TO W_ERR_CHK.
     MESSAGE S977 WITH 'AP반제여부를 선택하세요'.
     EXIT.
  ENDIF.

  IF P_N = ' '.
     MOVE : 'I'       TO      R_ZFPYA-SIGN,
            'EQ'      TO      R_ZFPYA-OPTION,
            'Y'       TO      R_ZFPYA-LOW,
            SPACE     TO      R_ZFPYA-HIGH.
     APPEND R_ZFPYA.
  ENDIF.

  IF P_C = ' '.
     MOVE : 'I'       TO      R_ZFPYA-SIGN,
            'EQ'      TO      R_ZFPYA-OPTION,
            'N'       TO      R_ZFPYA-LOW,
            SPACE     TO      R_ZFPYA-HIGH.
     APPEND R_ZFPYA.
  ENDIF.

  IF P_P EQ 'X'.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
              FROM ZTPMTHD
              WHERE ZFPNNO    IN S_PNNO
              AND   ZFPNBN    IN S_PNBN
              AND   ZFOPBN    IN S_OPBN
              AND   ZFOPNNO   IN S_OPNNO
              AND   EBELN     IN S_EBELN
              AND   ZFNTDT    IN S_NTDT
              AND   ZFPWDT    IN S_PWDT
              AND   ZFPYDT    IN S_PYDT
              AND   ZFDSDT    IN S_DSDT
              AND   ZFLCKN    IN S_LCKN
              AND   ZFTRTY    IN S_TRTY
              AND ( ZFPYA     IN R_ZFPYA
                       OR   ZFPMYN    EQ 'Y' ).
   ELSE.
     SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
              FROM ZTPMTHD
              WHERE ZFPNNO    IN S_PNNO
              AND   ZFPNBN    IN S_PNBN
              AND   ZFOPBN    IN S_OPBN
              AND   ZFOPNNO   IN S_OPNNO
              AND   EBELN     IN S_EBELN
              AND   ZFNTDT    IN S_NTDT
              AND   ZFPWDT    IN S_PWDT
              AND   ZFPYDT    IN S_PYDT
              AND   ZFDSDT    IN S_DSDT
              AND   ZFLCKN    IN S_LCKN
              AND   ZFTRTY    IN S_TRTY
              AND ( ZFPYA     IN R_ZFPYA
                       OR   ZFPMYN    EQ 'N' ).
   ENDIF.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      SELECT SINGLE NAME1 INTO IT_TAB-ZFPNBN_NM
             FROM   LFA1
             WHERE  LIFNR = IT_TAB-ZFPNBN.

      SELECT SINGLE NAME1 INTO IT_TAB-ZFOPBN_NM
             FROM   LFA1
             WHERE  LIFNR = IT_TAB-ZFOPBN.

      MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0.
     MOVE 'Y' TO W_ERR_CHK.
     CLEAR   IT_TAB.
     APPEND  IT_TAB.
     MESSAGE S738.
  ENDIF.


ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFPNNO  LIKE ZTPMTHD-ZFPNNO,
        ZFPYDT(10).

  REFRESH IT_SELECTED.
  CLEAR   IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFPNNO   TO ZFPNNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         READ LINE SY-INDEX FIELD VALUE  W_ZFPYDT.
         W_TABIX = SY-INDEX + 1.
         READ LINE W_TABIX FIELD VALUE  W_ZFPYDT.

         MOVE : W_LIST_INDEX    TO INDEX,
                IT_TAB-ZFPNNO   TO IT_SELECTED-ZFPNNO,
                W_ZFPYDT(4)     TO IT_SELECTED-ZFPYDT,
                W_ZFPYDT+5(2)   TO IT_SELECTED-ZFPYDT+4(2),
                W_ZFPYDT+8(2)   TO IT_SELECTED-ZFPYDT+6(2).

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_IDR
*&---------------------------------------------------------------------*
FORM P2000_SHOW_IDR USING    P_ZFPNNO.

*   SET PARAMETER ID 'ZPBLNO'   FIELD P_ZFBLNO.
*   SET PARAMETER ID 'ZPclseq'  FIELD P_ZFCLSEQ.
*   EXPORT 'ZPBLNO'        TO   MEMORY ID 'ZPBLNO'.
*   EXPORT 'ZPCLSEQ'       TO   MEMORY ID 'ZPCLSEQ'.

ENDFORM.                    " P2000_SHOW_IDR
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_BL
*&---------------------------------------------------------------------*
FORM P2000_SHOW_BL USING    P_ZFPNNO.

*   SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
*   EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.

*   CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_BL
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_PMTHD
*&---------------------------------------------------------------------*
FORM P2000_SHOW_PMTHD USING    P_ZFPNNO.

   SET PARAMETER ID 'ZPPNNO'  FIELD P_ZFPNNO.
   EXPORT 'ZPPNNO'        TO MEMORY ID 'ZPPNNO'.

ENDFORM.                    " P2000_SHOW_PMTHD
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*       text
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
*&      Form  P3000_INPUT_DATA_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_INPUT_DATA_SAVE.
DATA : WL_SUBRC LIKE SY-SUBRC.


 SELECT * INTO TABLE IT_ZTPMTHD
          FROM ZTPMTHD
          FOR ALL ENTRIES IN IT_SELECTED
          WHERE ZFPNNO EQ IT_SELECTED-ZFPNNO.

* Data 검?
 LOOP  AT   IT_ZTPMTHD.
    W_TABIX = SY-TABIX.

    IF IT_ZTPMTHD-ZFPYA EQ 'Y'.
       MESSAGE E603(ZIM1) WITH IT_ZTPMTHD-ZFPNNO.
    ENDIF.

    READ TABLE  IT_SELECTED WITH KEY ZFPNNO =  IT_ZTPMTHD-ZFPNNO.
    IF SY-SUBRC NE 0.
       DELETE IT_ZTPMTHD INDEX W_TABIX.
       CONTINUE.
    ELSE.
       MOVE IT_SELECTED-ZFPYDT TO IT_ZTPMTHD-ZFPYDT.
    ENDIF.

*>
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'   " 날짜 유효성 검증
         EXPORTING
             DATE                      = IT_ZTPMTHD-ZFPYDT
         EXCEPTIONS
             PLAUSIBILITY_CHECK_FAILED = 4.
    IF SY-SUBRC NE 0.
       MESSAGE E602(ZIM1) WITH IT_ZTPMTHD-ZFPNNO IT_ZTPMTHD-ZFPYDT.
    ENDIF.

    MODIFY IT_ZTPMTHD INDEX W_TABIX.
 ENDLOOP.

*  LOCK OBJECT.
 PERFORM   P2000_LOCK_EXEC  USING   'L'.

 MODIFY    ZTPMTHD FROM TABLE IT_ZTPMTHD.

 PERFORM   P2000_LOCK_EXEC  USING   'U'.

ENDFORM.                    " P3000_INPUT_DATA_SAVE
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_EXEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1429   text
*----------------------------------------------------------------------*
FORM P2000_LOCK_EXEC USING    VALUE(PA_LOCK).
DATA : L_TABIX  LIKE SY-TABIX,
       WL_SUBRC LIKE SY-SUBRC.

  LOOP AT IT_ZTPMTHD.
     L_TABIX = SY-TABIX.
     IF PA_LOCK EQ 'L'.
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTPMTHD'
           EXPORTING
               ZFPNNO                =     IT_ZTPMTHD-ZFPNNO
           EXCEPTIONS
               OTHERS        = 1.

       IF SY-SUBRC <> 0.
          MESSAGE I510 WITH SY-MSGV1 'Payment Notice Document'
                       IT_ZTPMTHD-ZFPNNO ''
                       RAISING DOCUMENT_LOCKED.
          DELETE IT_ZTPMTHD INDEX L_TABIX.
          CONTINUE.
       ENDIF.

       IT_ZTPMTHD-LOCK = 'Y'.
       MODIFY IT_ZTPMTHD INDEX L_TABIX.

     ELSEIF PA_LOCK EQ 'U'.
       CALL FUNCTION 'DEQUEUE_EZ_IM_ZTPMTHD'
            EXPORTING
                     ZFPNNO                =     IT_ZTPMTHD-ZFPNNO.

     ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_LOCK_EXEC
