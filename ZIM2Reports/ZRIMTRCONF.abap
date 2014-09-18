*&---------------------------------------------------------------------*
*& Report            ZRIMTRCONF                                        *
*&---------------------------------------------------------------------*
*&  Program Name : The Object list to Confirm delivery order           *
*&  Created by   : Na Hyun Joo                                         *
*&  Created on   : 2003.12.19                                          *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [Change Log]
*&---------------------------------------------------------------------*
PROGRAM  ZRIMTRCONF  MESSAGE-ID ZIM
                     LINE-SIZE 153
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE   ZRIMTRCONFTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen .
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS  FOR ZTTRHD-BUKRS NO INTERVALS
                                          NO-EXTENSION,
                S_EBELN  FOR ZTTRHD-ZFREBELN,
                S_ERNAM  FOR ZTTRHD-ERNAM,
                S_SENDER FOR ZTTRHD-ZFSENDER,
                S_TRAID  FOR LIKP-TRAID,
                S_GIDT   FOR ZTTRHD-ZFGIDT,
                S_TRNO   FOR ZTTRHD-ZFTRNO,
                S_WERKS  FOR ZTTRHD-WERKS,
                S_TRCO   FOR ZTTRHD-ZFTRCO.
PARAMETERS :    P_ITEM  AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 15(10) TEXT-021, POSITION 26.
PARAMETERS :       P_NO    AS CHECKBOX.
SELECTION-SCREEN : COMMENT 50(10) TEXT-022, POSITION 61.
PARAMETERS :       P_YES   AS CHECKBOX.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

*---------------------------------------------------------------------*
* EVENT INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF P_NO IS INITIAL AND P_YES IS INITIAL.
    MESSAGE E351(ZIM1).
  ENDIF.

*---------------------------------------------------------------------*
* EVENT TOP-OF-PAGE.
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*---------------------------------------------------------------------*
* EVENT START-OF-SELECTION.
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

  SORT IT_TAB BY ZFTRNO DESCENDING.

  PERFORM   P3000_DATA_WRITE .
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  CLEAR : IT_TAB.
*-----------------------------------------------------------------------
* EVENT AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.              " SORT 선택?
      IF IT_TAB-ZFTRNO IS INITIAL.
        MESSAGE S962.
      ELSE.
        W_FIELD_NM = 'ZFTRNO'.
        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
        PERFORM HANDLE_SORT TABLES  IT_TAB
                            USING   SY-UCOMM.
      ENDIF.
    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

    WHEN 'DISP'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ENDIF.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_TR USING IT_SELECTED-ZFTRNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.

      "---------------------------
      " Delivery Order Confirm
      "---------------------------
    WHEN 'EXTR'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ELSE.
        PERFORM P3000_GIYN_CHECK.
      ENDIF.
      PERFORM P2000_POPUP_MESSAGE USING
              'Confirmation'
              'Do you want to confirm this delivery order?'
              'Yes'
              'No'
              '1'
              W_BUTTON_ANSWER.
      IF W_BUTTON_ANSWER EQ '1'.
*        PERFORM P3000_DATE_CHECK.
        PERFORM P3000_TRANS_COMPLATE.

        PERFORM   P1000_READ_TEXT          USING   W_ERR_CHK.
        IF W_ERR_CHK EQ 'Y'.
          LEAVE TO SCREEN 0.
        ELSE.
          PERFORM RESET_LIST.
        ENDIF.
      ENDIF.

      "-----------------------------------
      " Confirm Cancel
      "-----------------------------------
    WHEN 'CLTR'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ELSE.
        PERFORM P3000_GIYN_CHECK.
      ENDIF.
      PERFORM P2000_POPUP_MESSAGE USING
              'Confirmation'
              'Do you want to cancle this delivery order confirm?'
              'Yes'
              'No'
              '1'
              W_BUTTON_ANSWER.
      IF W_BUTTON_ANSWER EQ '1'.

        PERFORM P3000_TRANS_CANCEL.

        PERFORM   P1000_READ_TEXT          USING   W_ERR_CHK.
        IF W_ERR_CHK EQ 'Y'.
          LEAVE TO SCREEN 0.
        ELSE.
          PERFORM RESET_LIST.
        ENDIF.
      ENDIF.
    WHEN 'REFR'.
      PERFORM   P1000_READ_TEXT  USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      SORT IT_TAB BY ZFTRNO DESCENDING.
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

    WHEN 'BACK' .
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.
  CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET TITLEBAR  'ZIMT4'.
  P_NO = 'X'.
  P_ITEM = 'X'.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.

  WRITE : /57  '[ Delivery Order (Confirmation) list ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 123 'Page : ', W_PAGE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /  SY-VLINE NO-GAP, (03) ' '      CENTERED    NO-GAP,
             SY-VLINE NO-GAP, (10) 'D/O Doc No'         NO-GAP,
             SY-VLINE NO-GAP, (10) 'P/O'                NO-GAP,
             SY-VLINE NO-GAP, (30) 'Arriving Plant'     NO-GAP,
             SY-VLINE NO-GAP, (10) 'Delivery'           NO-GAP,
             SY-VLINE NO-GAP, (10) 'Trans.due.'         NO-GAP,
             SY-VLINE NO-GAP, (12) 'Sender'             NO-GAP,
             SY-VLINE NO-GAP, (12) 'Delivery Status'    NO-GAP,
             SY-VLINE NO-GAP, (46) 'Transportation agent'
                      NO-GAP, SY-VLINE.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    WRITE : / SY-VLINE NO-GAP, (03) ' '                NO-GAP,
              SY-VLINE NO-GAP, (21) 'Container'        NO-GAP,
              SY-VLINE NO-GAP, (41) 'House B/L'        NO-GAP,
              SY-VLINE NO-GAP, (10) 'ETD'              NO-GAP,
              SY-VLINE NO-GAP, (12) 'ETA'              NO-GAP,
              SY-VLINE NO-GAP, (12) 'Real Arrived'     NO-GAP,
              SY-VLINE NO-GAP, (25) 'Entry Date'       NO-GAP,
              SY-VLINE NO-GAP, (20) 'Representive P/O' NO-GAP,
              SY-VLINE.
  ENDIF.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

  CLEAR  IT_TMP. REFRESH IT_TMP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TMP
           FROM ZTTRHD
          WHERE BUKRS    IN S_BUKRS
            AND ZFREBELN IN S_EBELN
            AND ERNAM    IN S_ERNAM
            AND ZFSENDER IN S_SENDER
            AND ZFGIDT   IN S_GIDT
            AND ZFTRNO   IN S_TRNO
            AND WERKS    IN S_WERKS
            AND ZFTRCO   IN S_TRCO.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'. MESSAGE S966. EXIT.
  ENDIF.

  CLEAR  IT_TAB. REFRESH IT_TAB.
  LOOP AT IT_TMP.
    IF P_YES EQ 'X' AND P_NO IS INITIAL.
      IF IT_TMP-ZFGIYN EQ 'N'.                   "INITIAL.
        CONTINUE.
      ENDIF.
    ELSEIF P_NO EQ 'X' AND P_YES IS INITIAL.
      IF IT_TMP-ZFGIYN EQ 'Y'.                   "x
        CONTINUE.
      ENDIF.
    ENDIF.
    " Arriving Plant
    IF NOT IT_TMP-WERKS IS INITIAL.
      SELECT SINGLE NAME1 INTO IT_TMP-W_WERKS
                          FROM T001W
                         WHERE WERKS = IT_TMP-WERKS.
    ENDIF.
    " Transportation Agent
    IF NOT IT_TMP-ZFTRCO IS INITIAL.
      PERFORM  P1000_GET_VENDOR   USING      IT_TMP-ZFTRCO
                                  CHANGING   IT_TMP-W_TRCO.
    ENDIF.

    " Delivery Status
    IF IT_TMP-ZFGIYN EQ 'N'.
      IT_TMP-W_GIYN = 'No Confirm'.
    ELSE.
      IT_TMP-W_GIYN = 'Confirm'.
    ENDIF.

    MOVE-CORRESPONDING IT_TMP TO IT_TAB.
    APPEND IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.

  IF W_COUNT LE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S966.
    EXIT.
  ENDIF.

  REFRESH : IT_DOIT, IT_TABIT.
  IF P_ITEM = 'X'.

    LOOP AT IT_TAB.

      REFRESH : IT_DOIT.

      " Inbound Delivery Information Get.
      SELECT  * FROM ZTTRITD
      WHERE  ZFTRNO   EQ   IT_TAB-ZFTRNO.

        CLEAR : IT_DOIT.

        SELECT SINGLE * FROM  ZTTRIT
        WHERE  ZFTRNO   EQ    IT_TAB-ZFTRNO
        AND    ZFTRIT   EQ    ZTTRITD-ZFTRIT.

        SELECT SINGLE * FROM MAKT
        WHERE  MATNR    EQ   ZTTRIT-MATNR
        AND    SPRAS    EQ   SY-LANGU.
        MOVE  : ZTTRIT-ZFBLNO  TO     IT_DOIT-ZFBLNO,
                MAKT-MAKTX     TO     IT_DOIT-MAKTX.

        SELECT  SINGLE   A~VGBEL  A~VGPOS  B~TRAID  A~KDMAT  A~MATNR
                A~WERKS  A~LFIMG  A~MEINS  B~VBELN
        INTO    (IT_DOIT-VGBEL,   IT_DOIT-VGPOS,  IT_DOIT-TRAID,
                 IT_DOIT-KDMAT,   IT_DOIT-MATNR,  IT_DOIT-WERKS,
                 IT_DOIT-LFIMG,   IT_DOIT-MEINS,  IT_DOIT-VBELN)
        FROM    LIPS  AS  A  INNER  JOIN  LIKP  AS  B
        ON      A~VBELN      EQ     B~VBELN
        WHERE   A~VBELN      EQ     ZTTRITD-VBELN
        AND     A~VGBEL      EQ     ZTTRIT-EBELN
        AND     B~TRAID      IN     S_TRAID
        AND     A~VGPOS      EQ     ZTTRIT-EBELP.
        APPEND  IT_DOIT.

      ENDSELECT.

      " Container Grouping.
      CLEAR : IT_TABIT, WL_TRAID.
      SORT  IT_DOIT  BY  TRAID.
      LOOP  AT  IT_DOIT.
        IF WL_TRAID  NE  IT_DOIT-TRAID.
          SELECT  SINGLE * FROM ZTBL
          WHERE   ZFBLNO   EQ   IT_DOIT-ZFBLNO.
          MOVE : IT_TAB-ZFTRNO  TO  IT_TABIT-ZFTRNO,
                 ZTBL-ZFBLNO    TO  IT_TABIT-ZFBLNO,
                 ZTBL-ZFHBLNO   TO  IT_TABIT-ZFHBLNO,
                 ZTBL-ZFETD     TO  IT_TABIT-ZFETD,
                 ZTBL-ZFETA     TO  IT_TABIT-ZFETA,
                 ZTBL-ZFRETA    TO  IT_TABIT-ZFRETA,
                 ZTBL-ZFREBELN  TO  IT_TABIT-ZFREBELN,
                 IT_DOIT-TRAID  TO  IT_TABIT-TRAID.
          SELECT SINGLE * FROM ZTIDSUS
          WHERE  ZFBLNO   EQ   IT_DOIT-ZFBLNO.
          MOVE : ZTIDSUS-ZFEDT  TO  IT_TABIT-ZFEDT.
          APPEND  IT_TABIT.
          CLEAR : IT_TABIT.
        ENDIF.
        MOVE  IT_DOIT-TRAID  TO  WL_TRAID.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET PF-STATUS 'ZIMT4'.
  SET TITLEBAR  'ZIMT4'.

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
*&----------------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE .

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_VENDOR
*&---------------------------------------------------------------------*
FORM P1000_GET_VENDOR USING    P_LIFNR
                      CHANGING P_NAME1.
  DATA : L_TEXT(35).

  CLEAR : P_NAME1, W_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.

* VENDOR MASTER SELECT( LFA1 )----------------------->
  CALL FUNCTION 'READ_LFA1'
       EXPORTING
            XLIFNR         = P_LIFNR
       IMPORTING
            XLFA1          = W_LFA1
       EXCEPTIONS
            KEY_INCOMPLETE = 01
            NOT_AUTHORIZED = 02
            NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.
*   TRANSLATE W_LFA1 TO UPPER CASE.
  MOVE: W_LFA1-NAME1   TO   L_TEXT.
  TRANSLATE L_TEXT TO UPPER CASE.
  P_NAME1 = L_TEXT.

ENDFORM.                    " P1000_GET_VENDOR
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE : / SY-VLINE,  MARKFIELD AS CHECKBOX,
            SY-VLINE NO-GAP,  (10) IT_TAB-ZFTRNO      NO-GAP,
            SY-VLINE NO-GAP,  (10) IT_TAB-ZFREBELN    NO-GAP,
            SY-VLINE NO-GAP,  (05) IT_TAB-WERKS       NO-GAP,
                              (25) IT_TAB-W_WERKS     NO-GAP,
            SY-VLINE NO-GAP,  (10) IT_TAB-ZFGIDT      NO-GAP,
            SY-VLINE NO-GAP,  (10) IT_TAB-ZFDRDT      NO-GAP,
            SY-VLINE NO-GAP,  (12) IT_TAB-ZFSENDER    NO-GAP,
            SY-VLINE NO-GAP.

  IF IT_TAB-ZFGIYN EQ 'N'.
    FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ELSEIF IT_TAB-ZFGIYN EQ 'Y'.
    FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
  ENDIF.
  WRITE :   (12) IT_TAB-W_GIYN      NO-GAP.

  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE :   SY-VLINE NO-GAP,  (11) IT_TAB-ZFTRCO      NO-GAP,
                              (35) IT_TAB-W_TRCO      NO-GAP,
            SY-VLINE NO-GAP.

  FORMAT RESET.
* Stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  W_COUNT = W_COUNT + 1.

*> 자재내역.
  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

    LOOP AT IT_TABIT WHERE ZFTRNO = IT_TAB-ZFTRNO.
      WRITE : / SY-VLINE NO-GAP, (03) ' '                NO-GAP,
                SY-VLINE NO-GAP, (21) IT_TABIT-TRAID     NO-GAP,
                SY-VLINE NO-GAP, (41) IT_TABIT-ZFHBLNO   NO-GAP,
                SY-VLINE NO-GAP, (10) IT_TABIT-ZFETD     NO-GAP,
                SY-VLINE NO-GAP, (12) IT_TABIT-ZFETA
                                      CENTERED           NO-GAP,
                SY-VLINE NO-GAP, (12) IT_TABIT-ZFRETA
                                      CENTERED           NO-GAP,
                SY-VLINE NO-GAP, (25) IT_TABIT-ZFEDT
                                      CENTERED           NO-GAP,
                SY-VLINE NO-GAP, (20) IT_TABIT-ZFREBELN
                                      CENTERED           NO-GAP,
                SY-VLINE NO-GAP.
    ENDLOOP.

  ENDIF.
  ULINE.
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
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  CLEAR   IT_SELECTED.
  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.
  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      READ LINE SY-INDEX FIELD VALUE  W_ZFGIDT.
      MOVE : IT_TAB-ZFTRNO   TO IT_SELECTED-ZFTRNO,
             IT_TAB-ZFDRDT   TO IT_SELECTED-ZFDRDT,
             IT_TAB-ZFGIYN   TO IT_SELECTED-ZFGIYN.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_TR
*&---------------------------------------------------------------------*
FORM P2000_SHOW_TR USING    P_ZFTRNO.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD ''.
  SET PARAMETER ID 'ZPTRNO'  FIELD P_ZFTRNO.

  CALL TRANSACTION 'ZIMT3'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_TR
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
*&      Form  P3000_TRANS_COMPLATE.
*&---------------------------------------------------------------------*
FORM P3000_TRANS_COMPLATE.

  DATA : W_UPDATE_CNT2 TYPE I.
  DATA : W_UPDATE_CNT  TYPE I.

  REFRESH IT_TRHD.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TRHD
              FROM ZTTRHD
               FOR ALL ENTRIES IN IT_SELECTED
             WHERE ZFTRNO = IT_SELECTED-ZFTRNO.

  CLEAR: W_UPDATE_CNT.
  LOOP AT IT_TRHD.
    W_TABIX = SY-TABIX.
    READ TABLE IT_SELECTED WITH KEY ZFTRNO = IT_TRHD-ZFTRNO.

    MOVE :  'Y'      TO IT_TRHD-ZFGIYN,
            SY-UNAME TO IT_TRHD-UNAM,
            SY-DATUM TO IT_TRHD-UDAT.
    MODIFY IT_TRHD INDEX W_TABIX.
    ADD 1 TO W_UPDATE_CNT.
  ENDLOOP.

  PERFORM   P2000_LOCK_EXEC  USING   'L'.
  DESCRIBE TABLE IT_TRHD LINES W_UPDATE_CNT2.
  IF W_UPDATE_CNT2 GT 0.
    MODIFY    ZTTRHD FROM TABLE  IT_TRHD.
    IF SY-SUBRC EQ 0.
      IF W_UPDATE_CNT2 EQ W_UPDATE_CNT.
        MESSAGE   S000(ZIM1) WITH W_UPDATE_CNT 'Confirm' .
      ELSE.
        MESSAGE   S299(ZIM1) WITH W_UPDATE_CNT W_UPDATE_CNT2
                                  'Confirm'.
      ENDIF.
    ELSE.
      CLEAR W_UPDATE_CNT2.
      MESSAGE  S298(ZIM1) WITH 'Confirm'.
    ENDIF.
  ELSE.
    MESSAGE  S298(ZIM1) WITH 'Confirm'.
  ENDIF.
  PERFORM   P2000_LOCK_EXEC  USING   'U'.

ENDFORM.                    " P3000_TRANS_COMPLATE.
*&---------------------------------------------------------------------*
*&      Form  P3000_TRANS_CANCEL.
*&---------------------------------------------------------------------*
FORM P3000_TRANS_CANCEL.

  DATA : W_UPDATE_CNT2 TYPE I.
  DATA : W_UPDATE_CNT  TYPE I.

  REFRESH IT_TRHD.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TRHD
              FROM ZTTRHD
               FOR ALL ENTRIES IN IT_SELECTED
             WHERE ZFTRNO = IT_SELECTED-ZFTRNO.

  CLEAR: W_UPDATE_CNT.
  LOOP AT IT_TRHD.
    W_TABIX = SY-TABIX.
    READ TABLE IT_SELECTED WITH KEY ZFTRNO = IT_TRHD-ZFTRNO.

    MOVE :  'N'      TO IT_TRHD-ZFGIYN,
            SY-UNAME TO IT_TRHD-UNAM,
            SY-DATUM TO IT_TRHD-UDAT.
    MODIFY IT_TRHD INDEX W_TABIX.
    ADD 1 TO W_UPDATE_CNT.
  ENDLOOP.

  PERFORM   P2000_LOCK_EXEC  USING   'L'.
  DESCRIBE TABLE IT_TRHD LINES W_UPDATE_CNT2.
  IF W_UPDATE_CNT2 GT 0.
    MODIFY    ZTTRHD FROM TABLE  IT_TRHD.
    IF SY-SUBRC EQ 0.
      IF W_UPDATE_CNT2 EQ W_UPDATE_CNT.
        MESSAGE   S000(ZIM1) WITH W_UPDATE_CNT 'Cancel Confirm' .
      ELSE.
        MESSAGE   S299(ZIM1) WITH W_UPDATE_CNT W_UPDATE_CNT2
                                  'Cancel Confirm'.
      ENDIF.
    ELSE.
      CLEAR W_UPDATE_CNT2.
      MESSAGE  S298(ZIM1) WITH 'Cancel Confirm'.
    ENDIF.
  ELSE.
    MESSAGE  S298(ZIM1) WITH 'Cancel Confirm'.
  ENDIF.
  PERFORM   P2000_LOCK_EXEC  USING   'U'.

ENDFORM.                    " P3000_TRANS_CANCEL.
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_EXEC
*&---------------------------------------------------------------------*
FORM P2000_LOCK_EXEC USING    VALUE(PA_LOCK).

  DATA : L_TABIX  LIKE SY-TABIX,
         WL_SUBRC LIKE SY-SUBRC.

  LOOP AT IT_TRHD.
    L_TABIX = SY-TABIX.
    IF PA_LOCK EQ 'L'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTTRHD'
           EXPORTING
                ZFTRNO = IT_TRHD-ZFTRNO
           EXCEPTIONS
                OTHERS = 1.

      IF SY-SUBRC <> 0.
        MESSAGE I510   WITH SY-MSGV1  '보세창고출고(수송)'
                            SY-MANDT ZTTRHD-ZFTRNO
                            RAISING DOCUMENT_LOCKED.
        DELETE IT_TRHD INDEX L_TABIX.
        CONTINUE.
      ENDIF.

    ELSEIF PA_LOCK EQ 'U'.
      CALL FUNCTION 'DEQUEUE_EZ_IM_ZTTRHD'
           EXPORTING
                ZFTRNO = IT_TRHD-ZFTRNO.

    ENDIF.
  ENDLOOP.

ENDFORM.                    "P2000_LOCK_EXEC
**&---------------------------------
**------------------------------------*
**&      Form  P3000_DATE_CHECK
**&---------------------------------
**------------------------------------*
*FORM P3000_DATE_CHECK.
*  DATA : WL_IDSDT LIKE ZTIDS-ZFIDSDT,
*         WL_WDT(10).
*
*  LOOP AT IT_SELECTED.
**> 날짜 표현이 알맞은지.
*    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
*      EXPORTING
*        DATE                      = IT_SELECTED-ZFGIDT
*      EXCEPTIONS
*        PLAUSIBILITY_CHECK_FAILED = 4.
*    IF SY-SUBRC NE 0.
*      MESSAGE E398(ZIM1) WITH IT_SELECTED-ZFTRNO
*                              IT_SELECTED-ZFGIDT.
*      EXIT.
*    ENDIF.
**> 통관일보다 큰지.
*    SELECT MAX( ZFIDSDT ) INTO WL_IDSDT
*           FROM ZTIDS AS I INNER JOIN ZTTRIT AS T
*                ON I~ZFIVNO EQ T~ZFIVNO
*           WHERE T~ZFTRNO EQ IT_SELECTED-ZFTRNO.
*
*    IF IT_SELECTED-ZFGIDT LT WL_IDSDT.
*      WRITE WL_IDSDT TO WL_WDT.
*      MESSAGE E397(ZIM1) WITH IT_SELECTED-ZFTRNO
*                              IT_SELECTED-ZFGIDT
*                              WL_WDT.
*      EXIT.
*    ENDIF.
**> 수송기한일보다 작은지.
*    IF IT_SELECTED-ZFGIDT GT IT_SELECTED-ZFDRDT.
*      MESSAGE E314(ZIM1).
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.                    " P3000_DATE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_GIYN_CHECK
*&---------------------------------------------------------------------*
FORM P3000_GIYN_CHECK.
  DATA : WL_TABIX LIKE SY-TABIX.

  LOOP AT IT_SELECTED.
    WL_TABIX = SY-TABIX.
    IF SY-UCOMM = 'EXTR'.
      IF IT_SELECTED-ZFGIYN NE 'N'.
        MESSAGE W340(ZIM1) WITH IT_SELECTED-ZFTRNO.
        DELETE IT_SELECTED INDEX WL_TABIX.
      ENDIF.

    ELSEIF SY-UCOMM = 'CLTR'.
      IF IT_SELECTED-ZFGIYN NE 'Y'.
        MESSAGE W341(ZIM1) WITH IT_SELECTED-ZFTRNO.
        DELETE IT_SELECTED INDEX WL_TABIX.
      ENDIF.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE IT_SELECTED LINES W_SELECTED_LINES.
  IF W_SELECTED_LINES EQ 0.
    MESSAGE S951.EXIT.
  ENDIF.

ENDFORM.                    " P3000_GIYN_CHECK

*&---------------------------------------------------------------------*
*&      Form  P4000_DB_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_DB_UPDATE .

*>> 수송 DB 입고여부 UPDATE.
  LOOP  AT  IT_SELECTED.
    SELECT  SINGLE * FROM ZTTRHD WHERE ZFTRNO EQ IT_SELECTED-ZFTRNO.
    MOVE  'Y'   TO  ZTTRHD-ZFGRST.
    UPDATE  ZTTRHD.
  ENDLOOP.

ENDFORM.                    " P4000_DB_UPDATE
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
*       text
*----------------------------------------------------------------------*
FORM P2000_CHANGE_SYMBOL USING    P_AMOUNT  P_FROM  P_TO.

  DO.
    REPLACE  P_FROM   WITH   P_TO  INTO    P_AMOUNT.
    IF  SY-SUBRC  <>    0.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_CHANGE_SYMBOL
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

  " Company Code Set.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
