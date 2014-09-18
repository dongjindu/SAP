*&---------------------------------------------------------------------*
*& Report  ZRIMNOCCLST                                                 *
*&---------------------------------------------------------------------*
*&  Program Name : Not yet Cleared clearance object list               *
*&  Created by   : Na Hyun Ju INFOLINK Ltd.                            *
*&  Created On   : 2003.02.10                                          *
*&---------------------------------------------------------------------*
*& [Change Log]
*&---------------------------------------------------------------------*
REPORT  ZRIMNOCCLST  MESSAGE-ID ZIM
                     LINE-SIZE 118
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,
         ZTBLIT,
         EKKO,
         MBEW,
         MARA,
         ZTIMIMG00,
         ZTBLINR,
         ZTBLINR_TMP,
         ZTIV.

*-----------------------------------------------------------------------
* B/L INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFWERKS    LIKE ZTBL-ZFWERKS,             " PLANT.
       EBELN      LIKE ZTBLIT-EBELN,             " P/O.
       ZFBLIT     LIKE ZTBLIT-ZFBLIT,            " P/O ITEM.
       ZFSHNO     LIKE ZTBL-ZFSHNO,              " Shipping Sequence
       ZFAPRT     LIKE ZTBL-ZFAPRT,              " Arriving Port
       MATNR      LIKE ZTBLIT-MATNR,             " Material
       ZFBLNO     LIKE ZTBL-ZFBLNO,              " B/L Document No
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,             " House B/L
       ZFETA      LIKE ZTBL-ZFETA,               " ETA
       ZFCARNM    LIKE ZTBL-ZFCARNM,             " Vessel Name
       ZFETD      LIKE ZTBL-ZFETD,               " ETD
       BLMENGE    LIKE ZTBLIT-BLMENGE,           " B/L QTY.
       MEINS      LIKE ZTBLIT-MEINS,             " UOM.
       CCMENGE    LIKE ZTBLIT-BLMENGE,           " CC QTY
       NCMENGE    LIKE ZTBLIT-BLMENGE,           " NO CC QTY
       ZFCCRDT    LIKE ZTBL-ZFCCRDT,             " Clerance Request DT
       ZFINRNO    LIKE ZTBL-ZFINRNO,             " Bonded No
       ZFAVDT     LIKE ZTBL-ZFAVDT,              " Arrive Date(Bonded)
       ZFCCNO     LIKE ZTBL-ZFCCNO,              " CC Request No
       ZFCCCNAM   LIKE ZTBL-ZFCCCNAM,            " In charge
       ZFPOYN     LIKE ZTBL-ZFPOYN,              " Nonmonetary
       TXZ01      LIKE ZTBLIT-TXZ01.             " Text
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Menu Status Function Inactive Internal Table
*-----------------------------------------------------------------------
DATA: BEGIN OF IT_EXCL OCCURS 20,
       FCODE    LIKE RSMPE-FUNC.
DATA: END   OF IT_EXCL.

DATA : BEGIN OF IT_SELECTED OCCURS 0,
       ZFBLNO     LIKE ZTBL-ZFBLNO,
       ZFCCRST    LIKE ZTBL-ZFCCRST,
       ZFCCNAM    LIKE ZTBL-ZFCCNAM,
       ZFCCRDT    LIKE ZTBL-ZFCCRDT,
       ZFCCCNAM   LIKE ZTBL-ZFCCCNAM,
       ZFCCCDT    LIKE ZTBL-ZFCCCDT,
       ZFINRNO    LIKE ZTBL-ZFINRNO.
DATA : END OF IT_SELECTED.

DATA : BEGIN OF IT_BL OCCURS 0.
        INCLUDE STRUCTURE ZTBL.
DATA : LOCK     TYPE C VALUE 'N'.
DATA : END   OF IT_BL.

DATA :  W_ERR_CHK         TYPE C,
        W_LINE_CHK        TYPE C,
        W_GUBUN(08)       TYPE C,
        W_SUBRC           LIKE SY-SUBRC,
        W_SY_UCOMM        LIKE SY-UCOMM,
        W_UPDATE_CNT      TYPE I,
        W_SELECTED_LINES  TYPE P,
        W_PAGE            TYPE I,
        EGRKZ             LIKE T007A-EGRKZ,
        W_INRNO           LIKE ZTBL-ZFINRNO,
        W_CNAM            LIKE SY-UNAME,
        W_LINE            TYPE I,
        W_KRWAMT(18)      TYPE C,
        W_ZFCCRDT(10)     TYPE C,
        W_COUNT           TYPE I,
        W_ZFCLCD          LIKE ZTIV-ZFCLCD,
        W_TABIX           LIKE SY-TABIX,
        W_FIELD_NM        LIKE DD03D-FIELDNAME,
        W_LIST_INDEX      LIKE SY-TABIX,
        W_BUTTON_ANSWER   TYPE C,
        P_BUKRS           LIKE ZTBL-BUKRS.

*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen.
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR  ZTBL-BUKRS  NO-EXTENSION
                                           NO INTERVALS,
                S_BEDAT   FOR  EKKO-BEDAT,
                S_MATNR   FOR  ZTBLIT-MATNR,
                S_EKORG   FOR  EKKO-EKORG,
                S_EKGRP   FOR  EKKO-EKGRP,
                S_WERKS   FOR  ZTBL-ZFWERKS,
                S_ETD     FOR  ZTBL-ZFETD,
                S_ETA     FOR  ZTBL-ZFETA,
                S_CDAT    FOR  ZTBL-CDAT,
                S_EBELN   FOR  ZTBLIT-EBELN.

SELECTION-SCREEN END OF BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
SELECTION-SCREEN : BEGIN OF LINE.
SELECTION-SCREEN : COMMENT 23(09) TEXT-031.
PARAMETERS : P_POY RADIOBUTTON GROUP RDG.
SELECTION-SCREEN : COMMENT 44(13) TEXT-032.
PARAMETERS : P_PON RADIOBUTTON GROUP RDG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK B3.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_ITEM AS CHECKBOX.
SELECTION-SCREEN END   OF BLOCK B2.

INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  IF P_ITEM EQ 'X'.
    PERFORM   P3000_TITLE_WRITE.       " Display Header Text with Item..
  ELSE.
    PERFORM   P3000_TITLE_WRITE_HEADER. " Display Header Text w/o Item..
  ENDIF.
*-----------------------------------------------------------------------
* START OF SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

* BL SELECT
  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.MESSAGE S738.    EXIT.    ENDIF.

* Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT
      W_FIELD_NM = 'ZFBLNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'REFR'.
      PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM RESET_LIST.
    WHEN 'DISP'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ENDIF.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_DISP_ZTBL USING IT_SELECTED-ZFBLNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'CCRQ'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S951.EXIT.
      ENDIF.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_CALL_ZIM62 USING IT_SELECTED-ZFBLNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
      PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.
        LEAVE TO SCREEN 0.
      ELSE.
        PERFORM RESET_LIST.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR70' .
*  MOVE  'X'   TO  P_CCFM.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  ' [ Not yet Clearance Object List  ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE,'',
          SY-VLINE,(15) 'Purchase order',
          SY-VLINE,(20) 'House B/L No.',
          SY-VLINE,(20) 'Arrival Port',
          SY-VLINE,(10) 'ETD',
          SY-VLINE,(10) 'ETA',
          SY-VLINE,(20) 'Vessel Name',
          SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ SY-VLINE,'',
          SY-VLINE,(15) 'Material No',
          SY-VLINE,(20) 'Material Text',
          SY-VLINE,(20) 'B/L Quanity',
          SY-VLINE,(23) 'Clearance Qty ',
          SY-VLINE,(20) 'Req.Clear.Qty ',
          SY-VLINE.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_HEADER
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_HEADER.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  ' [ Not yet Clearance Object List  ] '
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE,'',
          SY-VLINE,(15) 'Purchase order',
          SY-VLINE,(20) 'House B/L No.',
          SY-VLINE,(20) 'Arrival Port',
          SY-VLINE,(10) 'ETD',
          SY-VLINE,(10) 'ETA',
          SY-VLINE,(20) 'Vessel Name',
          SY-VLINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE_HEADER.
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET TITLEBAR  'ZIMR70'.
  SET PF-STATUS 'ZIMR70'.

  W_COUNT = 0.

  SORT IT_TAB BY ZFBLNO ZFCCRDT.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
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

  IF P_ITEM EQ 'X'.
    PERFORM   P3000_TITLE_WRITE.       " Display Header Text with Item..
  ELSE.
    PERFORM   P3000_TITLE_WRITE_HEADER. " Display Header Text w/o Item..
  ENDIF.

* Report Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  WRITE : / SY-ULINE.
  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / 'Total', W_COUNT, 'Case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ON CHANGE OF IT_TAB-ZFBLNO.
    WRITE:/ SY-ULINE.
    WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
            SY-VLINE,(10)IT_TAB-EBELN,
            '-', IT_TAB-ZFSHNO,
            SY-VLINE,(20) IT_TAB-ZFHBLNO,
            SY-VLINE,(20) IT_TAB-ZFAPRT,
            SY-VLINE,(10) IT_TAB-ZFETD,
            SY-VLINE,(10) IT_TAB-ZFETA,
            SY-VLINE,(20) IT_TAB-ZFCARNM, SY-VLINE.

    HIDE: IT_TAB.
    W_COUNT = W_COUNT + 1.
  ENDON.

  IF P_ITEM EQ 'X'.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    WRITE:/ SY-VLINE,'',
            SY-VLINE,(15)IT_TAB-MATNR,
            SY-VLINE,(20)IT_TAB-TXZ01,
            SY-VLINE,(17)IT_TAB-BLMENGE
                          UNIT IT_TAB-MEINS,
                     (03)IT_TAB-MEINS NO-GAP,
            SY-VLINE,(20)IT_TAB-CCMENGE UNIT IT_TAB-MEINS NO-GAP,
                     (03)IT_TAB-MEINS,
            SY-VLINE,(17)IT_TAB-NCMENGE UNIT IT_TAB-MEINS NO-GAP,
                     (03)IT_TAB-MEINS,
            SY-VLINE.
  ENDIF.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB.
  W_ERR_CHK = 'N'.

  SELECT SINGLE * FROM ZTIMIMG00.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBL AS H INNER JOIN ZTBLIT AS I
             ON H~ZFBLNO = I~ZFBLNO
            WHERE I~EBELN   IN S_EBELN
              AND H~BUKRS   IN S_BUKRS
              AND I~MATNR   IN S_MATNR
              AND H~ZFWERKS IN S_WERKS
              AND H~ZFETA   IN S_ETA
              AND H~ZFETD   IN S_ETD .
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

*>> Data Check...
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

    SELECT SINGLE *
           FROM EKKO
          WHERE EBELN  =   IT_TAB-EBELN
            AND EKGRP  IN  S_EKGRP
            AND EKORG  IN  S_EKORG
            AND BEDAT  IN  S_BEDAT.

    IF SY-SUBRC NE 0.
      IF IT_TAB-ZFPOYN EQ 'Y'.
        IF P_PON EQ 'X'.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
        ENDIF.
      ELSEIF IT_TAB-ZFPOYN EQ 'N'.
        IF P_POY EQ 'X'.
          DELETE IT_TAB INDEX W_TABIX.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    " Customs Clerarance Request Quantity Get
    SELECT SUM( CCMENGE ) INTO IT_TAB-CCMENGE
    FROM   ZTIVIT
    WHERE  ZFBLNO         EQ   IT_TAB-ZFBLNO
    AND    ZFBLIT         EQ   IT_TAB-ZFBLIT.

    IT_TAB-NCMENGE  =  IT_TAB-BLMENGE - IT_TAB-CCMENGE.
    IF IT_TAB-NCMENGE  LE 0.
      DELETE  IT_TAB  INDEX  W_TABIX.
      CONTINUE.
    ENDIF.
    MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.
  LOOP AT IT_TAB.
    IF IT_TAB-ZFPOYN EQ 'Y'.
      IF P_PON EQ 'X'.
        DELETE IT_TAB INDEX SY-TABIX.
        CONTINUE.
      ENDIF.
    ELSEIF IT_TAB-ZFPOYN EQ 'N'.
      IF P_POY EQ 'X'.
        DELETE IT_TAB INDEX SY-TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0.
    W_ERR_CHK = 'Y'.
  ENDIF.

ENDFORM.                    " P1000_GET_IT_TAB
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
      READ LINE SY-INDEX FIELD VALUE  W_ZFCCRDT.
      READ LINE SY-INDEX FIELD VALUE  W_INRNO.
      READ LINE SY-INDEX FIELD VALUE  W_CNAM.
      MOVE : IT_TAB-ZFBLNO   TO IT_SELECTED-ZFBLNO,
             W_ZFCCRDT(4)    TO IT_SELECTED-ZFCCRDT,
             W_ZFCCRDT+5(2)  TO IT_SELECTED-ZFCCRDT+4(2),
             W_ZFCCRDT+8(2)  TO IT_SELECTED-ZFCCRDT+6(2),
             W_INRNO         TO IT_SELECTED-ZFINRNO,
             W_CNAM          TO IT_SELECTED-ZFCCCNAM.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
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
FORM P3000_INPUT_DATA_SAVE.

  REFRESH IT_BL.
  RANGES: R_ZFBLNO FOR ZTBL-ZFBLNO OCCURS 5.
*>> Status Check
  LOOP AT IT_SELECTED.
    MOVE :    'I'                 TO  R_ZFBLNO-SIGN,
              'EQ'                TO  R_ZFBLNO-OPTION,
              IT_SELECTED-ZFBLNO  TO  R_ZFBLNO-LOW,
              SPACE               TO  R_ZFBLNO-HIGH.
    APPEND  R_ZFBLNO.
  ENDLOOP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BL
              FROM ZTBL
             WHERE ZFBLNO IN R_ZFBLNO.

  CLEAR: W_UPDATE_CNT.
  LOOP AT IT_BL.
    W_TABIX = SY-TABIX.
    IF  W_SY_UCOMM EQ 'CCREQ'.
      READ TABLE IT_SELECTED WITH KEY ZFBLNO = IT_BL-ZFBLNO.
      IF SY-SUBRC EQ 0.
*-----------------------------------------------------------------------
* CHECK DATE
*-----------------------------------------------------------------------
        CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
             EXPORTING
                  DATE                      = IT_SELECTED-ZFCCRDT
             EXCEPTIONS
                  PLAUSIBILITY_CHECK_FAILED = 4.
        IF SY-SUBRC NE 0.
          MESSAGE E422(ZIM1) WITH IT_BL-ZFBLNO
                                  IT_SELECTED-ZFCCRDT.
          EXIT.
        ENDIF.
      ENDIF.
      MOVE:  'R'       TO IT_BL-ZFCCRST,
              SY-UNAME TO IT_BL-ZFCCNAM,
              IT_SELECTED-ZFCCRDT TO IT_BL-ZFCCRDT,
              SY-UNAME TO IT_BL-UNAM,
              SY-DATUM TO IT_BL-UDAT.

    ENDIF.
    IF W_SY_UCOMM EQ 'CCCNF'.
      MOVE:  'C'                  TO IT_BL-ZFCCRST,
             IT_SELECTED-ZFCCCNAM TO IT_BL-ZFCCCNAM,
             IT_SELECTED-ZFINRNO  TO IT_BL-ZFCCNO,
             SY-DATUM             TO IT_BL-ZFCCCDT,
             SY-UNAME             TO IT_BL-UNAM,
             SY-DATUM             TO IT_BL-UDAT.

    ENDIF.
    MODIFY IT_BL INDEX W_TABIX.
    ADD 1 TO  W_UPDATE_CNT.
  ENDLOOP.
  PERFORM   P2000_LOCK_EXEC  USING   'L'.
  MODIFY    ZTBL FROM TABLE  IT_BL.
  PERFORM   P2000_LOCK_EXEC  USING   'U'.

ENDFORM.                    " P3000_INPUT_DATA_SAVE
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_EXEC
*&---------------------------------------------------------------------*
FORM P2000_LOCK_EXEC USING    VALUE(PA_LOCK).

  DATA : L_TABIX  LIKE SY-TABIX,
         WL_SUBRC LIKE SY-SUBRC.

  LOOP AT IT_BL.
    L_TABIX = SY-TABIX.
    IF PA_LOCK EQ 'L'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
           EXPORTING
                ZFBLNO = IT_BL-ZFBLNO
           EXCEPTIONS
                OTHERS = 1.

      IF SY-SUBRC <> 0.
        MESSAGE I510 WITH SY-MSGV1 'B/L Document'
                     IT_BL-ZFBLNO ''
                     RAISING DOCUMENT_LOCKED.
        DELETE IT_BL INDEX L_TABIX.
        CONTINUE.
      ENDIF.

      IT_BL-LOCK = 'Y'.
      MODIFY IT_BL INDEX L_TABIX.
    ELSEIF PA_LOCK EQ 'U'.
      CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
           EXPORTING
                ZFBLNO = IT_BL-ZFBLNO.

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P2000_CALL_ZIM62
*&---------------------------------------------------------------------*
FORM P2000_CALL_ZIM62  USING    P_ZFBLNO.

  CLEAR : ZTBL.
  SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ P_ZFBLNO.
*>> If Import declaration type is In Bond Transit.
  IF ZTBL-ZFRPTTY EQ 'B'.
    W_ZFCLCD   =  'A'.
  ELSE.
    W_ZFCLCD   =  'C'.
  ENDIF.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'ZPCLCD'  FIELD W_ZFCLCD.

  CALL TRANSACTION 'ZIM31'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_CALL_ZIM62
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> Company code SET.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P2000_DISP_ZTBL
*&---------------------------------------------------------------------*
FORM P2000_DISP_ZTBL USING    P_ZFBLNO.

  SET PARAMETER ID 'ZPHBLNO' FIELD ''.
  SET PARAMETER ID 'ZPBLNO'  FIELD P_ZFBLNO.
  SET PARAMETER ID 'BES'     FIELD ' '.

  CALL TRANSACTION 'ZIM23'  AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISP_ZTBL
