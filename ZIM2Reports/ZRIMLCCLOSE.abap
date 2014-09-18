*&---------------------------------------------------------------------*
*& Report  ZRIMLCCLOSE                                                 *
*&---------------------------------------------------------------------*
*& Program Name : Import L/C Document Close Program                    *
*& Created by   : Na Hyun Ju INFOLINK Ltd.                             *
*& Created on   : 2000.02.18                                           *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [Change Log]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLCCLOSE  MESSAGE-ID ZIM
                     LINE-SIZE 122
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Import Request INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                        " MARK
       UPDATE_CHK TYPE C,                        " DB Update Yes/No
       ZFREQDT    LIKE ZTREQST-ZFREQDT,          " Requested open date
       ZFREQTY    LIKE ZTREQST-ZFREQTY,          " Import Document Type
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       DMBTR      LIKE ZTBSEG-DMBTR,             " Import Expense
       HWAER      LIKE ZTBKPF-HWAER,             " Local Currency
       LIFNR      LIKE ZTREQHD-LIFNR,            " Vendor Code
       NAME1      LIKE LFA1-NAME1,               " Name 1
       ZFREQNO    LIKE ZTREQHD-ZFREQNO,          " Import Request Doc.no
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       EKORG      LIKE ZTREQST-EKORG,            " Purchasing organizati
       EKGRP      LIKE ZTREQST-EKGRP,            " Purchasing group
       ZFRLST1    LIKE ZTREQST-ZFRLST1,          " Release Status
       ZFRLDT1    LIKE ZTREQST-ZFRLDT1,          " Release Date
       ZFRLNM1    LIKE ZTREQST-ZFRLNM1,          " Release Person
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE,          " Close Yes/No
       ZFRLST2    LIKE ZTREQST-ZFRLST2.          " Open Release Status
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " Report Data Define Include
INCLUDE   ZRIMSORTCOM.    " Report Sort
INCLUDE   ZRIMBDCCOM.
INCLUDE   ZRIMUTIL01.     " Utility function

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                   S_EBELN   FOR ZTREQHD-EBELN,    " Purchasing document
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_REQNO   FOR ZTREQHD-ZFREQNO,  " Imp.Req.Doc.No
                   S_REQDT   FOR ZTREQST-ZFREQDT,  " Requested open date
                   S_CDAT    FOR ZTREQST-CDAT,     " Created on
                   S_REQTY   FOR ZTREQHD-ZFREQTY.  " Import Request Type
   PARAMETERS :    P_EKORG   LIKE ZTREQST-EKORG,   " Purchasing Organiz.
                   P_EKGRP   LIKE ZTREQST-EKGRP,   " Purchasing Group.
                   P_ERNAM   LIKE ZTREQST-ERNAM.   " Created by
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS : P_NOOPEN   AS CHECKBOX.
   PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.
*
*-----------------------------------------------------------------------
* L/C Close Status PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_STATUS FOR ZTREQHD-ZFCLOSE NO INTERVALS NO-DISPLAY.

*-----------------------------------------------------------------------
* Initialization(Parameter, Company Code)
*-----------------------------------------------------------------------
INITIALIZATION.
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* Parameter Set.
   PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Import Request table select
   PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Text Table SELECT
   PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Report Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
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

      WHEN 'DISP'.          " L/C 조?
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'FRGS'.          " Set Close
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            PERFORM P3000_CLOSED_UPDATE USING 'X'.
            PERFORM RESET_LIST.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.  EXIT.
         ENDIF.
      WHEN 'FRGR'.          " Cancel Close
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM  P3000_CLOSED_UPDATE USING ' '.
            PERFORM  RESET_LIST.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.  EXIT.
         ENDIF.
      WHEN 'SAVE'.          " FILE DOWNLOAD....
         LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.
            EXIT.
         ENDLOOP.
         IF SY-SUBRC EQ 0.
             PERFORM P2000_POPUP_MESSAGE.     " Messagebox Pop-up
             IF W_BUTTON_ANSWER EQ '1'.       " OK Button
                PERFORM P3000_DATA_UPDATE.    " Data Save
                PERFORM P2000_DATA_UNLOCK.    " Unlocking
                LEAVE TO SCREEN 0.
             ENDIF.
         ELSE.
             MESSAGE E032.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
          LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.
             EXIT.
          ENDLOOP.
          IF SY-SUBRC EQ 0.
             PERFORM P2000_REFRESH_POPUP_MESSAGE.
             IF W_BUTTON_ANSWER EQ '1'.       " Save -> Exit
                PERFORM P3000_DATA_UPDATE.    " Data Save
                PERFORM P2000_DATA_UNLOCK.    " Unlocking
                " Import Request Table Select
                PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.      EXIT.     ENDIF.
                " Report Text Table SELECT
                PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.      EXIT.     ENDIF.
                PERFORM RESET_LIST.
            ELSEIF W_BUTTON_ANSWER EQ '2'.
                PERFORM P2000_DATA_UNLOCK.    " Unlocking
                " Import Request Table Select
                PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.      EXIT.     ENDIF.
                " Report Text Table Select
                PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.      EXIT.     ENDIF.
                PERFORM RESET_LIST.
            ENDIF.
         ELSE.
            " Import Request Table Select
            PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.      EXIT.     ENDIF.
            " Report Text Table SELECT
            PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.      EXIT.     ENDIF.
            PERFORM RESET_LIST.
         ENDIF.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
         LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.  " Refresh
            EXIT.
         ENDLOOP.

         IF SY-SUBRC EQ 0.                     " DATA
            PERFORM P2000_EXIT_POPUP_MESSAGE.  " Messagebox Pop-up
            IF W_BUTTON_ANSWER EQ '1'.
               PERFORM P3000_DATA_UPDATE.      " Data Save
               LEAVE TO SCREEN 0.              " Exit
            ELSEIF W_BUTTON_ANSWER EQ '2'.
              LEAVE TO SCREEN 0.
            ENDIF.
         ELSE.
            LEAVE TO SCREEN 0.                " Exit
         ENDIF.
      WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIM04'.          " TITLE BAR
  P_NOOPEN = 'X'.                 " Object to close
  CLEAR : P_OPEN.                 " Object to cancel

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /58  '[ L/C Close Status records ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 81 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',   SY-VLINE NO-GAP,
            'Opening Da'     NO-GAP,  SY-VLINE NO-GAP,
            'P/O Number'     NO-GAP,  SY-VLINE NO-GAP,
            'IM Req. No'     NO-GAP,  SY-VLINE NO-GAP,
            'Ty'             NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '     NO-GAP,  SY-VLINE NO-GAP,
            'Name',                88 SY-VLINE NO-GAP,
            'POrg'           NO-GAP,  SY-VLINE NO-GAP,
            'PGp'            NO-GAP,  SY-VLINE NO-GAP,
       (22) 'Occured Amount' CENTERED NO-GAP,
                                      SY-VLINE NO-GAP,
            'C'              NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.

  IF P_NOOPEN IS INITIAL AND P_OPEN IS INITIAL.
     W_ERR_CHK = 'Y'.   MESSAGE S034.   EXIT.
  ENDIF.

  IF P_EKORG IS INITIAL.       P_EKORG = '%'.      ENDIF.
  IF P_EKGRP IS INITIAL.       P_EKGRP = '%'.      ENDIF.
  IF P_ERNAM IS INITIAL.       P_ERNAM = '%'.      ENDIF.
*-----------------------------------------------------------------------
* Close Object SETTING
*-----------------------------------------------------------------------
  IF P_NOOPEN EQ 'X'.           " Close 대?
     MOVE: 'I'      TO S_STATUS-SIGN,
           'EQ'     TO S_STATUS-OPTION,
           ' '      TO S_STATUS-LOW.
     APPEND S_STATUS.
  ENDIF.

*-----------------------------------------------------------------------
* Close Canceled Object SETTING
*-----------------------------------------------------------------------
  IF P_OPEN EQ 'X'.
     MOVE: 'I'      TO S_STATUS-SIGN,
           'EQ'     TO S_STATUS-OPTION,
           'X'      TO S_STATUS-LOW.
     APPEND S_STATUS.
  ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVREQHD_ST   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO TABLE IT_ZVREQ FROM ZVREQHD_ST
                               WHERE EBELN      IN     S_EBELN
                               AND   LIFNR      IN     S_LIFNR
                               AND   ZFREQNO    IN     S_REQNO
                               AND   ZFREQDT    IN     S_REQDT
                               AND   CDAT       IN     S_CDAT
                               AND   ZFREQTY    IN     S_REQTY
                               AND   ZFCLOSE    IN     S_STATUS
                               AND   EKORG      LIKE   P_EKORG
                               AND   EKGRP      LIKE   P_EKGRP
                               AND   ERNAM      LIKE   P_ERNAM
                               AND   ZFAMDNO    EQ     '00000'.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.
   REFRESH : IT_TAB.

   LOOP AT IT_ZVREQ.
*     CLEAR : IT_TAB.
      MOVE-CORRESPONDING IT_ZVREQ  TO  IT_TAB.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-LIFNR
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      CASE SY-SUBRC.
         WHEN 01.     MESSAGE E022.
         WHEN 02.     MESSAGE E950.
         WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
      ENDCASE.
      MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.

      ">> Import Expense Compute.
      PERFORM  P4000_EXPENSE_COMPUTE    USING IT_TAB-ZFREQNO
                                     CHANGING IT_TAB-DMBTR
                                              IT_TAB-HWAER.

      APPEND  IT_TAB.
   ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM04'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM04'.           " GUI TITLE SETTING..

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
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
        ZFCLOSE LIKE ZTREQHD-ZFCLOSE,
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFAMDNO  TO ZFAMDNO,
         IT_TAB-ZFCLOSE  TO ZFCLOSE,
         IT_TAB-ZFRLST1  TO ZFRLST1,
         IT_TAB-ZFRLST2  TO ZFRLST2.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_TAB-ZFCLOSE  TO IT_SELECTED-ZFCLOSE,
             IT_TAB-ZFRLST1  TO IT_SELECTED-ZFRLST1,
             IT_TAB-ZFRLST2  TO IT_SELECTED-ZFRLST2.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFAMDNO TO IT_SELECTED-ZFAMDNO,
             ZFCLOSE TO IT_SELECTED-ZFCLOSE,
             ZFRLST1 TO IT_SELECTED-ZFRLST1,
             ZFRLST2 TO IT_SELECTED-ZFRLST2.

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
     WRITE : / SY-ULINE.    WRITE : / 'Total', W_COUNT, 'records'.
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

  FORMAT RESET.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX
       COLOR COL_NORMAL INTENSIFIED OFF,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQDT COLOR COL_KEY   INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   COLOR COL_KEY   INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO COLOR COL_TOTAL INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQTY COLOR COL_TOTAL INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-LIFNR   COLOR COL_NORMAL INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-NAME1   COLOR COL_NORMAL INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-EKORG   COLOR COL_NORMAL INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-EKGRP   COLOR COL_NORMAL INTENSIFIED OFF NO-GAP,
       SY-VLINE NO-GAP.

  WRITE : IT_TAB-DMBTR  CURRENCY IT_TAB-HWAER NO-GAP,
          ' '                                 NO-GAP,
          IT_TAB-HWAER  NO-GAP,  SY-VLINE NO-GAP.

  CASE IT_TAB-ZFCLOSE.
     WHEN 'X'.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
     WHEN ' '.
        FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
     WHEN OTHERS.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDCASE.

  WRITE : IT_TAB-ZFCLOSE NO-GAP, SY-VLINE NO-GAP.

* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form   P3000_CLOSED_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_CLOSED_UPDATE USING  P_CLOSE.

  LOOP  AT   IT_SELECTED.
    IF P_CLOSE  EQ 'X'.
       IF IT_SELECTED-ZFCLOSE EQ 'X'.
          MESSAGE E035 WITH IT_SELECTED-ZFREQNO.
       ENDIF.
    ELSEIF P_CLOSE  EQ ' '.
       IF IT_SELECTED-ZFCLOSE EQ ' '.
          MESSAGE E036 WITH IT_SELECTED-ZFREQNO.
       ENDIF.
    ENDIF.

    READ TABLE IT_TAB WITH KEY  ZFREQNO = IT_SELECTED-ZFREQNO.
    IF SY-SUBRC NE 0.
       MESSAGE E030 WITH IT_SELECTED-ZFREQNO.
    ENDIF.
    W_TABIX = SY-TABIX.

*-----------------------------------------------------------------------
*   후속 작업 체크....( 미결된 Invoice가 존재할 경우 )
*-----------------------------------------------------------------------
    IF P_CLOSE  EQ 'X'.
       PERFORM P2000_UNRELEASE_CHECK USING IT_TAB-ZFREQNO.
    ENDIF.

    IT_TAB-MARK = 'X'.
    IT_TAB-ZFCLOSE = P_CLOSE.                  " close 지?
    IF IT_TAB-UPDATE_CHK NE 'U'.
       IT_TAB-UPDATE_CHK = 'U'.                " DB 반영 여?
*-----------------------------------------------------------------------
* lock checking...
*-----------------------------------------------------------------------
*      CALL FUNCTION 'ENQUEUE_EZLCHEAD'
*           EXPORTING
*                ZILCDN   = LST-ZILCDN.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
    W_UPDATE_CNT = W_UPDATE_CNT + 1.
  ENDLOOP.

ENDFORM.                    "  P3000_CLOSED_UPDATE
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
FORM P2000_POPUP_MESSAGE.

   IF NOT P_NOOPEN IS INITIAL.
      PERFORM  P2000_CLOSE_MESSAGE.
   ELSE.
      PERFORM  P2000_CANCEL_MESSAGE.
   ENDIF.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE.

  LOOP AT IT_TAB   WHERE UPDATE_CHK EQ 'U'.

      CLEAR : ZTIMIMG00, ZTREQHD, ZTREQST.

      SELECT SINGLE * FROM   ZTIMIMG00.
* Import Request Header table Select
      SELECT SINGLE * FROM   ZTREQHD
                      WHERE  ZFREQNO EQ IT_TAB-ZFREQNO.
* Import Request Status table Select
      SELECT SINGLE * FROM   ZTREQST
                      WHERE  ZFREQNO EQ IT_TAB-ZFREQNO
                      AND    ZFAMDNO EQ '00'.

*-----------------------------------------------------------------------
* Before data Temp Table Move
*-----------------------------------------------------------------------
* Change Data Move
      MOVE : IT_TAB-ZFCLOSE  TO  ZTREQHD-ZFCLOSE,     " Close  Status
             SY-DATUM        TO  ZTREQST-UDAT,        " Change Date
             SY-UNAME        TO  ZTREQST-UNAM.        " Change Owner

      UPDATE ZTREQHD.                                 " DATA UPDATE
      IF SY-SUBRC NE 0.
         MESSAGE E031 WITH ZTREQHD-ZFREQNO.
         ROLLBACK WORK.
      ENDIF.

*-----------------------------------------------------------------------
* L/C Expense Processing
*-----------------------------------------------------------------------
     IF NOT  IT_TAB-ZFCLOSE  IS  INITIAL.
        PERFORM  P3000_EXPENSE_POSTING  USING IT_TAB-ZFREQNO.
     ELSE.
        PERFORM  P3000_EXPENSE_CANCEL   USING IT_TAB-ZFREQNO.
     ENDIF.

*-----------------------------------------------------------------------
* PO DATA Change. ( CLOSE => Marking Delivery Complete Mark)
*-----------------------------------------------------------------------
      IF ZTIMIMG00-ZFPOMYN = 'X'.
         PERFORM   P3000_PO_CHANGE     USING  IT_TAB-ZFREQNO
                                              IT_TAB-ZFCLOSE.

         IF SY-SUBRC NE 0.
            MESSAGE E031 WITH ZTREQHD-ZFREQNO.
            ROLLBACK WORK.                            " Error
         ENDIF.
      ENDIF.

      UPDATE ZTREQST.                                 " DATA UPDATE
      IF SY-SUBRC EQ 0.
*-----------------------------------------------------------------------
* Change History
*-----------------------------------------------------------------------
      ELSE.
         MESSAGE E031 WITH ZTREQHD-ZFREQNO.
         ROLLBACK WORK.
      ENDIF.

  ENDLOOP.

  IF SY-SUBRC EQ 0.
     COMMIT WORK.
  ENDIF.

ENDFORM.                    " P3000_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_REFRESH_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = 'List REFRESH confirmation'
             DIAGNOSE_OBJECT = ''
            TEXT_QUESTION =
             'Do you want to save the close operation first?'
*                           '먼저 Close 작업을 저장하시겠습니까?'
             TEXT_BUTTON_1   = '   YES  '
             TEXT_BUTTON_2   = '   NO   '
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
             TITLEBAR        = 'List exit confirmation'
             DIAGNOSE_OBJECT = ''
            TEXT_QUESTION =
             'Do you want to save the close operation first?'
             TEXT_BUTTON_1   = '   YES  '
             TEXT_BUTTON_2   = '   NO   '
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_EXIT_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_UNLOCK
*&---------------------------------------------------------------------*
FORM P2000_DATA_UNLOCK.










ENDFORM.                    " P2000_DATA_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.

   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.

   CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P3000_PO_CHANGE
*&---------------------------------------------------------------------*
FORM P3000_PO_CHANGE USING    P_ZFREQNO
                              P_CLOSE.

   REFRESH : BAPIMEPOITEM, BAPIMEPOITEMX.
   CLEAR   : BAPIMEPOITEM, BAPIMEPOITEMX.
   IF  NOT  P_CLOSE  IS  INITIAL.

       SELECT  *  FROM  ZTREQIT  WHERE  ZFREQNO  EQ  P_ZFREQNO.

         CLEAR : BAPIMEPOITEM, BAPIMEPOITEMX.
         SELECT SINGLE * FROM  EKPO
                         WHERE EBELN  EQ  ZTREQIT-EBELN
                         AND   EBELP  EQ  ZTREQIT-EBELP.

         MOVE : EKPO-EBELN  TO  W_EBELN,
                EKPO-EBELP  TO  BAPIMEPOITEM-PO_ITEM,
                EKPO-EBELP  TO  BAPIMEPOITEMX-PO_ITEM,
                'X'         TO  BAPIMEPOITEMX-NO_MORE_GR,
                'X'         TO  BAPIMEPOITEM-NO_MORE_GR.
         APPEND : BAPIMEPOITEM, BAPIMEPOITEMX.
       ENDSELECT.
   ELSE.
       SELECT  *  FROM  ZTREQIT  WHERE  ZFREQNO  EQ  P_ZFREQNO.

         CLEAR : BAPIMEPOITEM, BAPIMEPOITEMX.

         SELECT SINGLE * FROM  EKPO
                         WHERE EBELN  EQ  ZTREQIT-EBELN
                         AND   EBELP  EQ  ZTREQIT-EBELP.

         MOVE : EKPO-EBELN  TO  W_EBELN,
                EKPO-EBELP  TO  BAPIMEPOITEM-PO_ITEM,
                EKPO-EBELP  TO  BAPIMEPOITEMX-PO_ITEM,
                'X'         TO  BAPIMEPOITEMX-NO_MORE_GR,
                SPACE       TO  BAPIMEPOITEM-NO_MORE_GR.
         APPEND : BAPIMEPOITEM, BAPIMEPOITEMX.

       ENDSELECT.
   ENDIF.

*>> PO Change FUNCTION CALL.
   CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
        EXPORTING
           PURCHASEORDER = W_EBELN
        TABLES
           POITEM        = BAPIMEPOITEM
           POITEMX       = BAPIMEPOITEMX
           RETURN        = RETURN.
   IF SY-SUBRC NE 0.
      MESSAGE  S450.
   ENDIF.

ENDFORM.                    " P3000_PO_CHANGE
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> Company Code Set
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P4000_EXPENSE_COMPUTE
*&---------------------------------------------------------------------*
FORM P4000_EXPENSE_COMPUTE USING    P_ZFREQNO
                           CHANGING P_DMBTR
                                    P_HWAER.

   DATA : W_DMBTR  LIKE  ZTBDIV-DMBTR,
          W_HWAER  LIKE  ZTBDIV-HWAER.

   CLEAR : P_DMBTR, P_HWAER.

   SELECT *  FROM  ZTREQIT
   WHERE  ZFREQNO  EQ   P_ZFREQNO.

      ">> P/O Amount Sum
      SELECT  SUM( DMBTR ) MAX( HWAER )
      INTO    (W_DMBTR, W_HWAER)
      FROM    ZTBDIV
      WHERE   EBELN   EQ  ZTREQIT-EBELN
      AND     EBELP   EQ  ZTREQIT-EBELP.
      P_DMBTR   =  P_DMBTR  +  W_DMBTR.
      P_HWAER   =  W_HWAER.

   ENDSELECT.

ENDFORM.                    " P4000_EXPENSE_COMPUTE
*&---------------------------------------------------------------------*
*&      Form  P2000_CLOSE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CLOSE_MESSAGE.

   READ TABLE IT_SELECTED INDEX 1.
   SELECT COUNT( * )  INTO  W_REQ_CNT
   FROM   ZTBLIT
   WHERE  ZFREQNO     EQ    IT_SELECTED-ZFREQNO.

   READ TABLE IT_TAB  WITH KEY  ZFREQNO  = IT_SELECTED-ZFREQNO.

   IF W_REQ_CNT GT 0 OR IT_TAB-DMBTR EQ 0.
      PERFORM  P3000_COMMOND_POPUP_MESSAGE.
   ELSE.
      PERFORM  P3000_SCREEN_MESSAGE.
   ENDIF.

ENDFORM.                    " P2000_CLOSE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P3000_COMMOND_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P3000_COMMOND_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = 'Close(Exit)operation save confirmation'
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   =
                      'Do you want to continue the close operation?'
             TEXT_BUTTON_1   = '   YES  '
             TEXT_BUTTON_2   = '   NO   '
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P3000_COMMOND_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0010 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
     EXIT.
  ENDIF.

   IF W_POSDT IS INITIAL.
      MESSAGE E167 WITH 'Document date'.
      EXIT.
   ENDIF.
   IF W_DOCDT IS INITIAL.
      MESSAGE E167 WITH 'Posting date'.
      EXIT.
   ENDIF.
   IF COBL-KOSTL IS INITIAL.
       MESSAGE E167 WITH 'Cost Center'.
       EXIT.
   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0020 INPUT.

  IF OK-CODE EQ 'ENTR' OR OK-CODE EQ 'YES'.
     ANTWORT = 'Y'.
     SET SCREEN 0.
     LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
     ANTWORT = 'N'.
     SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_SCREEN_MESSAGE
*&---------------------------------------------------------------------*
FORM P3000_SCREEN_MESSAGE.

  MOVE 'Initial Value' TO SPOP-TITEL.
  IF W_POSDT IS INITIAL.
     MOVE SY-DATUM    TO W_POSDT.
  ENDIF.
  IF W_DOCDT IS INITIAL.
     MOVE SY-DATUM    TO W_DOCDT.
  ENDIF.

  CALL SCREEN 0020 STARTING AT 15 1
                   ENDING   AT 56 11.

  IF ANTWORT  EQ 'Y'.
     W_BUTTON_ANSWER = '1'.
  ELSE.
     CLEAR : W_BUTTON_ANSWER.
  ENDIF.

ENDFORM.                    " P3000_SCREEN_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

   CLEAR : ZTREQHD.
   READ TABLE IT_SELECTED INDEX 1.
   SELECT SINGLE * FROM ZTREQHD
   WHERE  ZFREQNO  EQ   IT_SELECTED-ZFREQNO.

   READ TABLE IT_TAB  WITH KEY  ZFREQNO  = IT_SELECTED-ZFREQNO.

   IF ZTREQHD-ZFACDO IS INITIAL.
      PERFORM  P3000_COMMOND_POPUP_MESSAGE.
   ELSE.
      PERFORM  P3000_SCREEN_CANCLE_MESSAGE.
   ENDIF.

ENDFORM.                    " P2000_CANCEL_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P3000_EXPENSE_POSTING
*&---------------------------------------------------------------------*
FORM P3000_EXPENSE_POSTING USING    P_ZFREQNO.

   DATA : W_MODE TYPE C.

   REFRESH : IT_ZSBDIV.
   CLEAR   : W_REQ_CNT, ZTREQHD, ZTIMIMG00.

   SELECT COUNT( * ) INTO W_REQ_CNT
   FROM   ZTBLIT
   WHERE  ZFREQNO    EQ   P_ZFREQNO.

   SELECT SINGLE * FROM ZTREQHD
   WHERE  ZFREQNO  EQ   P_ZFREQNO.

   IF W_REQ_CNT GT 0.  EXIT.  ENDIF.

   SELECT B~BUKRS   B~GJAHR   B~BUZEI     B~DBUZEI B~ZFBSEQ B~ZFCSTGRP
          B~ZFCD    B~ZFDCSTX B~COND_TYPE B~TBTKZ  B~KNUMV  B~ZFIMDNO
          B~ZFIMDIT B~EBELN   B~EBELP     B~ZFAMT  B~MATNR  B~TXZ01
          B~WERKS   B~MENGE   B~MEINS     B~NETPR  B~PEINH  B~BPRME
          B~WAERS1  B~BPUMN   B~BPUMZ     B~ZBUZEI B~NEWBS  B~NEWKO
          B~AKONT   B~ZTERM   B~XMWST     B~MWSKZ  B~ZUONR  B~ZFBDT
          B~ZFPOYN  B~BUPLA   B~PS_POSID  B~GSBER  B~KOSTL  B~PRCTR
          B~WAERS   B~HWAER   B~WRBTR     B~WMWST  B~DMBTR  B~FWBAS
          B~KURSF   B~WWERT   B~SGTXT     B~GJAHRH B~BELNRH B~BUZEIH
          B~ZFSETYN
   INTO CORRESPONDING FIELDS OF TABLE IT_ZSBDIV
   FROM ( ZTBSEG AS A INNER JOIN ZTBDIV AS B
   ON     A~BUKRS     EQ    B~BUKRS
   AND    A~BELNR     EQ    B~BELNR
   AND    A~GJAHR     EQ    B~GJAHR
   AND    A~BUZEI     EQ    B~BUZEI   )
   INNER  JOIN ZTBKPF AS    C
   ON     A~BUKRS     EQ    C~BUKRS
   AND    A~BELNR     EQ    C~BELNR
   AND    A~GJAHR     EQ    C~GJAHR
   WHERE  A~ZFCSTGRP  EQ    '003'
   AND    A~ZFIMDNO   EQ    P_ZFREQNO
   AND    C~ZFPOSYN   EQ    'Y'.

   IF IT_ZSBDIV[] IS INITIAL.  EXIT.  ENDIF.

   ">> BDC Data Make.
   REFRESH : BDCDATA.
   CLEAR   : L_MENGE, L_MEINS.

    " User Setting Convert.
    PERFORM  P2000_DATE_USER_CONVERT      USING W_DOCDT
                                       CHANGING W_BLDAT.
    PERFORM  P2000_DATE_USER_CONVERT      USING W_POSDT
                                       CHANGING W_BUDAT.

   " First Screen.
   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0100',
           ' ' 'BKPF-BLDAT'  W_BLDAT,
           ' ' 'BKPF-BUDAT'  W_BUDAT,
           ' ' 'BKPF-BLART'  'RE',
           ' ' 'BKPF-BKTXT' 'L/C Close',
           ' ' 'BKPF-BUKRS'  ZTREQHD-BUKRS,
           ' ' 'BKPF-WAERS'  ZTREQHD-WAERS.

   LOOP AT IT_ZSBDIV.

      IF IT_ZSBDIV-EBELN IS INITIAL. CONTINUE.  ENDIF.
      IF IT_ZSBDIV-DMBTR LE 0.       CONTINUE.  ENDIF.

      PERFORM  P4000_ACCOUNT_GET.

      PERFORM P2000_DYNPRO USING :
              ' ' 'RF05A-NEWBS' '50',
              ' ' 'RF05A-NEWKO' W_ACT_ACC,
              ' ' 'BDC_OKCODE'  '/00'.

      WRITE IT_ZSBDIV-MENGE   TO  L_MENGE UNIT  IT_ZSBDIV-MEINS.
      WRITE IT_ZSBDIV-MEINS   TO  L_MEINS.

      WRITE IT_ZSBDIV-DMBTR TO  TEMP_WRBTR CURRENCY IT_ZSBDIV-HWAER.
      PERFORM P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.

      PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'    '0300',
              ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
              ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
              ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
              ' ' 'BSEG-MENGE'  L_MENGE,
              ' ' 'BSEG-MEINS'  L_MEINS,
              ' ' 'BSEG-SGTXT'  IT_ZSBDIV-MATNR,
              ' ' 'BDC_OKCODE'  '=ZK'.

      PERFORM P2000_DYNPRO USING :
              'X' 'SAPLKACB' '0002',
              ' ' 'COBL-MATNR'  IT_ZSBDIV-MATNR,
              ' ' 'BDC_OKCODE'  '=ENTE'.

      PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'    '0330',
              ' ' 'BSEG-XREF3'  ZTREQHD-ZFOPNNO,
              ' ' 'BDC_OKCODE'  '/00'.

      W_TOT_AMOUNT = W_TOT_AMOUNT + IT_ZSBDIV-DMBTR.

   ENDLOOP.

   ">> End Screen.
   PERFORM P2000_DYNPRO USING :
           ' ' 'RF05A-NEWBS' '40',
           ' ' 'RF05A-NEWKO' W_PRI_ACC,
           ' ' 'BDC_OKCODE'  '/00'.

   WRITE W_TOT_AMOUNT TO  TEMP_WRBTR CURRENCY  IT_ZSBDIV-HWAER.
   PERFORM P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.

   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0300',
           ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
           ' ' 'BSEG-SGTXT'  'L/C Close',
           ' ' 'BDC_OKCODE'  '=ZK'.
   PERFORM P2000_DYNPRO USING :
           'X' 'SAPLKACB'   '0002',
           ' ' 'COBL-KOSTL'  COBL-KOSTL,
           ' ' 'BDC_OKCODE'  '=ENTE'.
   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0330',
           ' ' 'BDC_OKCODE'  '=BU'.

   SET PARAMETER ID 'BLN' FIELD ''.        " Document No
   SET PARAMETER ID 'GJR' FIELD ''.        " Year

   " BDC CALL.
   CALL TRANSACTION 'FB01'  USING       BDCDATA
                            MODE        'E'
                            UPDATE      'S'
                            MESSAGES    INTO   MESSTAB.
   W_SUBRC = SY-SUBRC.

   IF W_SUBRC NE 0.      ">> ERROR
      LOOP AT MESSTAB.
         MOVE : MESSTAB-MSGTYP  TO     XRETURN-MSGTYP,
                MESSTAB-MSGID   TO     XRETURN-MSGID,
                MESSTAB-MSGNR   TO     XRETURN-MSGNR,
                MESSTAB-MSGV1   TO     XRETURN-MSGV1,
                MESSTAB-MSGV2   TO     XRETURN-MSGV2,
                MESSTAB-MSGV3   TO     XRETURN-MSGV3,
                MESSTAB-MSGV4   TO     XRETURN-MSGV4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = XRETURN-MSGID
                         MSGNR     = XRETURN-MSGNR
                         MSGV1     = XRETURN-MSGV1
                         MSGV2     = XRETURN-MSGV2
                         MSGV3     = XRETURN-MSGV3
                         MSGV4     = XRETURN-MSGV4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = XRETURN-MESSTXT.
         APPEND  RETURN.
      ENDLOOP.
      W_SUBRC = 4.
   ELSE.
      GET PARAMETER ID 'BLN' FIELD W_BELNR.
      GET PARAMETER ID 'GJR' FIELD W_GJAHR.

      IF W_BELNR IS INITIAL.
         W_SUBRC = 4.
         MOVE : 'E'             TO     XRETURN-MSGTYP,
                'ZIM1'          TO     XRETURN-MSGID,
                '003'           TO     XRETURN-MSGNR,
                SPACE           TO     XRETURN-MSGV1,
                SPACE           TO     XRETURN-MSGV2,
                SPACE           TO     XRETURN-MSGV3,
                SPACE           TO     XRETURN-MSGV4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = XRETURN-MSGID
                         MSGNR     = XRETURN-MSGNR
                         MSGV1     = XRETURN-MSGV1
                         MSGV2     = XRETURN-MSGV2
                         MSGV3     = XRETURN-MSGV3
                         MSGV4     = XRETURN-MSGV4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = XRETURN-MESSTXT.
         APPEND  XRETURN.
      ELSE.
         UPDATE  ZTREQHD
         SET     ZFACDO    =   W_BELNR
                 ZFFIYR    =   W_GJAHR
         WHERE   ZFREQNO   =   P_ZFREQNO.
         W_SUBRC = 0.
      ENDIF.
   ENDIF.

ENDFORM.                    " P3000_EXPENSE_POSTING
*&---------------------------------------------------------------------*
*&      Form  P4000_ACCOUNT_GET
*&---------------------------------------------------------------------*
FORM P4000_ACCOUNT_GET.

   CLEAR : EKPO, W_ACT_ACC, W_PRI_ACC.
   SELECT SINGLE * FROM EKPO
   WHERE  EBELN    EQ   IT_ZSBDIV-EBELN
   AND    EBELP    EQ   IT_ZSBDIV-EBELP.

   SELECT SINGLE * FROM ZTIMIMG11
   WHERE  BUKRS    EQ   ZTREQHD-BUKRS.

   " Actual Account
   PERFORM   P5000_GET_ACCOUNT  USING EKPO-MATNR
                                      EKPO-WERKS
                                      'ZR3'
                             CHANGING W_ACT_ACC.

   " Price Difference Account
   W_PRI_ACC   =  ZTIMIMG11-ZFIOCAC33.

ENDFORM.                    " P4000_ACCOUNT_GET

*&---------------------------------------------------------------------*
*&      Form  P5000_GET_ACCOUNT
*&---------------------------------------------------------------------*
FORM P5000_GET_ACCOUNT USING    P_MATNR
                                P_WERKS
                                P_ACC_KEY
                       CHANGING P_ACCOUNT.

   CLEAR : MBEW, T001W, T001K, T030.

   " Valuation Class
   SELECT SINGLE * FROM MBEW
   WHERE  MATNR    EQ   P_MATNR
   AND    BWKEY    EQ   P_WERKS.

   " Valuation Modification Key
   SELECT SINGLE * FROM T001W
   WHERE  WERKS    EQ   P_WERKS.

   " Valuation Modification Group
   SELECT SINGLE * FROM T001K
   WHERE  BUKRS    EQ   ZTIMIMG11-BUKRS
   AND    BWKEY    EQ   T001W-BWKEY.

   IF ZTIMIMG11-ZFVCYN EQ 'X'.
      IF ZTIMIMG11-ZFVMYN EQ 'X'.
         SELECT SINGLE *  FROM  T030
         WHERE  KTOPL     EQ    ZTIMIMG11-KTOPL
         AND    KTOSL     EQ    P_ACC_KEY
         AND    BWMOD     EQ    T001K-BWMOD
         AND    KOMOK     EQ    ZTIMIMG11-KOMOK
         AND    BKLAS     EQ    MBEW-BKLAS.
      ELSE.
         SELECT SINGLE *  FROM  T030
         WHERE  KTOPL     EQ     ZTIMIMG11-KTOPL
         AND    KTOSL     EQ     P_ACC_KEY
         AND    KOMOK     EQ     ZTIMIMG11-KOMOK
         AND    BKLAS     EQ     MBEW-BKLAS.
      ENDIF.
   ELSE.
      IF ZTIMIMG11-ZFVMYN EQ 'X'.
         SELECT SINGLE * FROM T030
         WHERE  KTOPL    EQ   ZTIMIMG11-KTOPL
         AND    KTOSL    EQ   P_ACC_KEY
         AND    BWMOD    EQ   T001K-BWMOD
         AND    KOMOK    EQ   ZTIMIMG11-KOMOK.
      ELSE.
         SELECT SINGLE * FROM T030
         WHERE  KTOPL    EQ   ZTIMIMG11-KTOPL
         AND    KTOSL    EQ   P_ACC_KEY
         AND    KOMOK    EQ   ZTIMIMG11-KOMOK.
      ENDIF.
   ENDIF.

   P_ACCOUNT  = T030-KONTS.

ENDFORM.                    " P7000_GET_ACCOUNT
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
*&      Form  P2000_DATE_USER_CONVERT
*&---------------------------------------------------------------------*
FORM P2000_DATE_USER_CONVERT USING    P_DATE
                             CHANGING P_CON_DATE.

   CALL FUNCTION 'ZIM_BDC_DATE_CONVERT_EXTERNAL'
        EXPORTING
           I_DATE     =      P_DATE
        IMPORTING
            E_DATE     =     P_CON_DATE
        EXCEPTIONS
            OTHERS     =     4.

ENDFORM.                    " P2000_DATE_USER_CONVERT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF OK-CODE NE 'YES' AND OK-CODE NE 'ENTR'.
    EXIT.
  ENDIF.

  IF UF05A-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  IF OK-CODE NE 'YES' AND OK-CODE NE 'ENTR'.
     ANTWORT  =  'Y'.
     SET SCREEN 0.
     LEAVE SCREEN.
  ENDIF.

  OK-CODE = 'YES'.
  ANTWORT =  'Y'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_SCREEN_CANCLE_MESSAGE
*&---------------------------------------------------------------------*
FORM P3000_SCREEN_CANCLE_MESSAGE.

  MOVE 'Initial Value' TO SPOP-TITEL.
  SELECT SINGLE * FROM BKPF
  WHERE  BELNR    EQ   ZTREQHD-ZFACDO
  AND    GJAHR    EQ   ZTREQHD-ZFFIYR.

  MOVE : '03'        TO   UF05A-STGRD,
         BKPF-BUDAT  TO   BSIS-BUDAT.

  CALL SCREEN 0010 STARTING AT 15 1
                   ENDING   AT 56 6.

  IF ANTWORT  EQ 'Y'.
     W_BUTTON_ANSWER = '1'.
  ELSE.
     CLEAR : W_BUTTON_ANSWER.
  ENDIF.

ENDFORM.                    " P3000_SCREEN_CANCLE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_EXPENSE_CANCEL
*&---------------------------------------------------------------------*
FORM P3000_EXPENSE_CANCEL USING    P_ZFREQNO.

   CLEAR : ZTREQHD.
   SELECT SINGLE * FROM ZTREQHD
   WHERE  ZFREQNO  EQ   P_ZFREQNO.
   IF ZTREQHD-ZFACDO IS INITIAL.  EXIT.  ENDIF.

   REFRESH : BDCDATA.

   " User Setting Convert.
   PERFORM  P2000_DATE_USER_CONVERT      USING BSIS-BUDAT
                                      CHANGING W_BUDAT.

   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0105',
           ' ' 'RF05A-BELNS' ZTREQHD-ZFACDO,
           ' ' 'BKPF-BUKRS'  ZTREQHD-BUKRS,
           ' ' 'RF05A-GJAHS' ZTREQHD-ZFFIYR,
           ' ' 'UF05A-STGRD' UF05A-STGRD,
           ' ' 'BSIS-BUDAT'  W_BUDAT,
           ' ' 'BSIS-MONAT'  SPACE,
           ' ' 'RF05A-VOIDR' SPACE,
           ' ' 'BDC_OKCODE'  '=BU'.

   SET PARAMETER ID 'BLN' FIELD ''.
   SET PARAMETER ID 'GJR' FIELD ''.

   CALL TRANSACTION 'FB08'  USING       BDCDATA
                            MODE        'E'
                            UPDATE      'V'
                            MESSAGES    INTO   MESSTAB.

   IF SY-SUBRC NE 0.
      LOOP AT MESSTAB.
         MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
                MESSTAB-MSGID   TO     RETURN-ID,
                MESSTAB-MSGNR   TO     RETURN-NUMBER,
                MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
                MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
                MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
                MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                 MSGID     = RETURN-ID
                 MSGNR     = RETURN-NUMBER
                 MSGV1     = RETURN-MESSAGE_V1
                 MSGV2     = RETURN-MESSAGE_V2
                 MSGV3     = RETURN-MESSAGE_V3
                 MSGV4     = RETURN-MESSAGE_V4
              IMPORTING
                 MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
      ENDLOOP.
   ELSE.                 ">> SUCCESS 시.
      CLEAR : W_BELNR, W_GJAHR.
      GET PARAMETER ID 'BLN' FIELD W_BELNR.
      GET PARAMETER ID 'GJR' FIELD W_GJAHR.

      " Error Occured
      IF W_BELNR IS INITIAL.
         MOVE : 'E'             TO     RETURN-TYPE,
                'ZIM'           TO     RETURN-ID,
                '494'           TO     RETURN-NUMBER,
                SPACE           TO     RETURN-MESSAGE_V1,
                SPACE           TO     RETURN-MESSAGE_V2,
                SPACE           TO     RETURN-MESSAGE_V3,
                SPACE           TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                 MSGID     = RETURN-ID
                 MSGNR     = RETURN-NUMBER
                 MSGV1     = RETURN-MESSAGE_V1
                 MSGV2     = RETURN-MESSAGE_V2
                 MSGV3     = RETURN-MESSAGE_V3
                 MSGV4     = RETURN-MESSAGE_V4
              IMPORTING
                 MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
      ELSE.
         MESSAGE  S977 WITH 'Cancelation is completed succesfully.'.
         MOVE : SY-MSGTY   TO     RETURN-TYPE,
                SY-MSGID   TO     RETURN-ID,
                SY-MSGNO   TO     RETURN-NUMBER,
                SY-MSGV1   TO     RETURN-MESSAGE_V1,
                SY-MSGV2   TO     RETURN-MESSAGE_V2,
                SY-MSGV3   TO     RETURN-MESSAGE_V3,
                SY-MSGV4   TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
              EXPORTING
                 MSGID     = RETURN-ID
                 MSGNR     = RETURN-NUMBER
                 MSGV1     = RETURN-MESSAGE_V1
                 MSGV2     = RETURN-MESSAGE_V2
                 MSGV3     = RETURN-MESSAGE_V3
                 MSGV4     = RETURN-MESSAGE_V4
              IMPORTING
                 MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.

         UPDATE  ZTREQHD
         SET     ZFFIYR    =  ' '
                 ZFACDO    =  ' '
         WHERE   ZFREQNO   =  P_ZFREQNO.

      ENDIF.
   ENDIF.

ENDFORM.                    " P3000_EXPENSE_CANCEL
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
