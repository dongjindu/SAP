*&---------------------------------------------------------------------*
*& Report  ZRIMBKDOWNUP                                                *
*&---------------------------------------------------------------------*
*&  Program    : Broker Interface File Upload and Download.            *
*&  Developer  :                                                       *
*&  Created on : 2003.12.12                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
REPORT  ZRIMBKDOWNUP MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL, ZTCIVHD, ZTCIVIT, ZTIDRUS, ZTIDSUS, EKKO.
TYPE-POOLS : SLIS.
*-----------------------------------------------------------------------
* B/L INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
        ZFREBELN     LIKE ZTBL-ZFREBELN,
        ZFBLNO       LIKE ZTBL-ZFBLNO,
        W_PO(13)     TYPE C,
        ZFSHNO       LIKE ZTBL-ZFSHNO,
        ZFPOYN       LIKE ZTBL-ZFPOYN,
        ZFETD        LIKE ZTBL-ZFETD,
        ZFCARNM      LIKE ZTBL-ZFCARNM,
        ZFSPRT       LIKE ZTBL-ZFSPRT,
        EKGRP        LIKE ZTBL-EKGRP,
        W_EKGRP(20)  TYPE C,
        LIFNR        LIKE ZTBL-LIFNR,
        W_LIFNR(30)  TYPE C,
        ZFHBLNO      LIKE ZTBL-ZFHBLNO,
        ZFRGDSR      LIKE ZTBL-ZFRGDSR,
        ZFETA        LIKE ZTBL-ZFETA,
        ZFRETA       LIKE ZTBL-ZFRETA,
        ZFVIA        LIKE ZTBL-ZFVIA,
        ZFAPRT       LIKE ZTBL-ZFAPRT,
        ZFFORD       LIKE ZTBL-ZFFORD,
        W_ZFFORD(30) TYPE C,
        ZFBENI       LIKE ZTBL-ZFBENI,
        W_ZFBENI(30) TYPE C,
        ZFREQNO      LIKE ZTREQHD-ZFREQNO,
        ZFREQTY      LIKE ZTREQHD-ZFREQTY,
        ZFOPNNO      LIKE ZTREQHD-ZFOPNNO.
DATA : END OF IT_TAB.
DATA  W_EDI_RECORD(65535).
*>> Declaration of internal table for file download.
DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILE.

*>> Declaration of variable for ALV Display.
DATA: G_REPID LIKE SY-REPID.
DATA: G_LAYOUT          TYPE SLIS_LAYOUT_ALV.
DATA: G_STATUS          TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.
DATA: GT_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV.
DATA: GT_SORT           TYPE SLIS_T_SORTINFO_ALV.
DATA: LS_FIELDCAT       TYPE SLIS_FIELDCAT_ALV.
DATA: LS_SORT           TYPE SLIS_SORTINFO_ALV.
DATA: POS               TYPE I.
DATA: G_SAVE(1)         TYPE C.
DATA: G_VARIANT         LIKE DISVARIANT.
DATA: G_USER_COMMAND    TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.

*>> Declaration of variable.
DATA: W_FILENAME   LIKE RLGRAP-FILENAME.
DATA : W_OK_CODE   LIKE SY-UCOMM.
DATA: CANCEL       TYPE C.
*-----------------------------------------------------------------------
* Define Tables & Variables..
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " Released  Report Data Define Include
INCLUDE   ZRIMSORTCOM.    " Report Sort Include
INCLUDE   ZRIMUTIL01.     " Utility function

*-----------------------------------------------------------------------
* Selection Screen..
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO INTERVALS
                                         NO-EXTENSION,
                S_EBELN   FOR ZTBL-ZFREBELN,
                S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L No.
                S_BLADT   FOR ZTBL-ZFBLADT      " B/L Date.
                          NO-EXTENSION,
                S_RPTTY   FOR ZTBL-ZFRPTTY,
                S_EKGRP   FOR ZTBL-EKGRP,       " Purchasing Group
                S_ZFTRCK  FOR ZTBL-ZFTRCK,      " TRUCKER
                S_ETA     FOR ZTBL-ZFETA        " ETA
                          NO-EXTENSION,
                S_SPRTC   FOR ZTBL-ZFSPRTC      " Shipping Port
                          NO INTERVALS.
PARAMETERS :    P_VIA     LIKE ZTBL-ZFVIA.      " VIA
SELECT-OPTIONS: S_FORD    FOR ZTBL-ZFFORD.      " Forwarder
PARAMETERS :    P_POYN    LIKE ZTBL-ZFPOYN.     " Monetary Yes/No
SELECT-OPTIONS: S_SHTY    FOR ZTBL-ZFSHTY,      " Transportation Method.
                S_WERKS   FOR ZTBL-ZFWERKS.     " Representive PLANT
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-021, POSITION 1.
PARAMETERS : P_DN     RADIOBUTTON GROUP RDG.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : BEGIN OF LINE,POSITION 1.
SELECTION-SCREEN : COMMENT 4(18) TEXT-022, POSITION 1.
PARAMETERS : P_UP      RADIOBUTTON GROUP RDG.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_YES AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK B3.
* PARAMETER Setting
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START OF SELECTION Clause..
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Paramenter Setting
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.
      W_FIELD_NM = 'ZFBLNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.

      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'DISP'.
      PERFORM P2000_MULTI_SELECTION.

      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.

    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766.
        EXIT.
      ELSE.
        PERFORM P2000_POPUP_MESSAGE.
        IF W_BUTTON_ANSWER EQ '1'.
          PERFORM P3000_DATA_DOWNLOAD TABLES IT_EDIFILE.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.

    WHEN 'REFR'.
      PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.

      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.

      IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIME6'.          " TITLE BAR
  MOVE 'X' TO P_DN.
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[  B/L Receipt Detail ]' CENTERED
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /100 'Date : ', SY-DATUM.  ", 101 'Page : ', W_PAGE.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP,
            (20) 'P/O No'            CENTERED, SY-VLINE NO-GAP,
            (20) 'Money/Non-Money'   CENTERED, SY-VLINE NO-GAP,
            (10) 'E.T.D'             CENTERED, SY-VLINE NO-GAP,
            (15) 'Vessel name'       CENTERED, SY-VLINE NO-GAP,
            (15) 'Port of loading'   CENTERED, SY-VLINE NO-GAP,
            (15) 'Purchase group'    CENTERED, SY-VLINE NO-GAP,
            (20) 'Vendor'            CENTERED, SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE : / SY-VLINE NO-GAP,
            (20) 'B/L No'            CENTERED, SY-VLINE NO-GAP,
            (20) 'Main Item'         CENTERED, SY-VLINE NO-GAP,
            (10) 'E.T.A'             CENTERED, SY-VLINE NO-GAP,
            (15) 'VIA'               CENTERED, SY-VLINE NO-GAP,
            (15) 'Port of arrival'   CENTERED, SY-VLINE NO-GAP,
            (15) 'Forwarder'         CENTERED, SY-VLINE NO-GAP,
            (20) 'Beneficiary'       CENTERED, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF P_VIA  IS INITIAL.  P_VIA  = '%'.   ENDIF.
  IF P_POYN IS INITIAL.  P_POYN = '%'.   ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : LFA1.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFFORD
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFFORD.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : LFA1.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-LIFNR
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-W_LIFNR.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : LFA1.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFBENI
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFBENI.

*-----------------------------------------------------------------------
* T024 SELECT( 구매그룹)
*-----------------------------------------------------------------------
    SELECT SINGLE EKNAM INTO IT_TAB-W_EKGRP
      FROM T024
     WHERE EKGRP = IT_TAB-EKGRP.

    MODIFY  IT_TAB INDEX W_TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIM24'.           " GUI Status Setting...
  SET  TITLEBAR 'ZIME6'.           " GUI Title Setting...

  W_COUNT = 0.

  IF P_YES EQ 'X'.
    PERFORM P3000_APPEND_FIELDCAT.      " ALV Report TiTle.
    PERFORM P3000_DATA_WRITE_ALV.
  ELSE.
    LOOP AT IT_TAB.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
        PERFORM P3000_LAST_WRITE.
      ENDAT.
    ENDLOOP.
  ENDIF.

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
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFBLNO   TO ZFREQNO.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFBLNO  TO IT_SELECTED-ZFREQNO.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO.

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
    WRITE : /102 'Total', W_COUNT, 'case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
  DATA: W_PO(20),
        W_DOM_TEXT(20).

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO W_PO.

  "DOMAIN - 유환여부.
  PERFORM  GET_DD07T USING 'ZDPOYN' IT_TAB-ZFPOYN
                     CHANGING   W_DOM_TEXT.

  WRITE : / SY-VLINE NO-GAP,
            (20) W_PO                NO-ZERO, SY-VLINE NO-GAP,
            (20) W_DOM_TEXT         CENTERED, SY-VLINE NO-GAP,
            (10) IT_TAB-ZFETD       CENTERED, SY-VLINE NO-GAP,
            (15) IT_TAB-ZFCARNM             , SY-VLINE NO-GAP,
            (15) IT_TAB-ZFSPRT              , SY-VLINE NO-GAP,
            (15) IT_TAB-W_EKGRP             , SY-VLINE NO-GAP,
            (20) IT_TAB-W_LIFNR             , SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  FORMAT RESET.
  WRITE : / SY-VLINE NO-GAP,
            (20) IT_TAB-ZFHBLNO             , SY-VLINE NO-GAP,
            (20) IT_TAB-ZFRGDSR             , SY-VLINE NO-GAP,
            (10) IT_TAB-ZFRETA      CENTERED, SY-VLINE NO-GAP,
            (15) IT_TAB-ZFVIA               , SY-VLINE NO-GAP,
            (15) IT_TAB-ZFAPRT              , SY-VLINE NO-GAP,
            (15) IT_TAB-W_ZFFORD            , SY-VLINE NO-GAP,
            (20) IT_TAB-W_ZFBENI            , SY-VLINE NO-GAP.

* hide...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFREQNO.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.
  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

* 구매의뢰 테이블 SELECT
  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  PERFORM RESET_LIST.
ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_TAB  FROM ZTBL
                               WHERE BUKRS      IN     S_BUKRS
                               AND   ZFBLSDT    NE     SPACE
                               AND   ZFBLADT    IN     S_BLADT
                               AND   ZFTRCK     IN     S_ZFTRCK
                               AND   ZFETA      IN     S_ETA
                               AND   ZFRETA     NE     '00000000'
                               AND   ZFWERKS    IN     S_WERKS
                               AND   ZFSPRTC    IN     S_SPRTC
                               AND   ZFREBELN   IN     S_EBELN
                               AND   ZFHBLNO    IN     S_HBLNO
                               AND   EKGRP      IN     S_EKGRP
                               AND   ZFRPTTY    IN     S_RPTTY
                               AND   ZFVIA      LIKE   P_VIA
                               AND   ZFFORD     IN     S_FORD
                               AND   ZFPOYN     LIKE   P_POYN
                               AND   ZFSHTY     IN     S_SHTY
                               AND   ZFWERKS    IN     S_WERKS.

*>> Document for download..
  IF NOT P_DN IS INITIAL.
    LOOP AT IT_TAB.
      SELECT SINGLE *
        FROM ZTIDRUS
       WHERE ZFBLNO = IT_TAB-ZFBLNO.
      IF SY-SUBRC EQ 0.
        DELETE IT_TAB INDEX SY-TABIX.
        CONTINUE.
      ENDIF.
    ENDLOOP.
*>> Document for Upload..
  ELSEIF NOT P_UP IS INITIAL.
    LOOP AT IT_TAB.
      SELECT SINGLE *
        FROM ZTIDRUS
       WHERE ZFBLNO = IT_TAB-ZFBLNO.
      IF SY-SUBRC NE 0.
        DELETE IT_TAB INDEX SY-TABIX.
        CONTINUE.
      ELSE.
        SELECT SINGLE *
          FROM ZTIDSUS
         WHERE ZFBLNO = IT_TAB-ZFBLNO.
        IF SY-SUBRC EQ 0.
          DELETE IT_TAB INDEX SY-TABIX.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT IT_TAB.
    CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO IT_TAB-W_PO.
    MODIFY IT_TAB INDEX SY-TABIX.
  ENDLOOP.
  LOOP AT IT_TAB.
    SELECT SINGLE *
             FROM ZTCIVIT
            WHERE ZFBLNO = IT_TAB-ZFBLNO.
    IF SY-SUBRC NE 0.
      DELETE IT_TAB INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.

  IF SY-SUBRC NE 0.               " Not Found.
    W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T      USING  P_DOMNAME P_FIELD
                 CHANGING  P_W_NAME.
  CLEAR : DD07T, P_W_NAME.

  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> 회사코드 SET.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
FORM P3000_APPEND_FIELDCAT.
  CLEAR: GT_FIELDCAT, GT_SORT, POS.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFPOYN'.
  LS_FIELDCAT-SELTEXT_M      = 'Monetary'.
  LS_FIELDCAT-OUTPUTLEN      = 1.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'ZFPOYN'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFHBLNO'.
  LS_FIELDCAT-SELTEXT_M      = 'House B/L No.'.
  LS_FIELDCAT-OUTPUTLEN      = 15.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'ZFHBLNO'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'W_PO'.
  LS_FIELDCAT-SELTEXT_M      = 'P/O No.'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFRGDSR'.
  LS_FIELDCAT-SELTEXT_M      = 'Repre.Good.Desc'.
  LS_FIELDCAT-OUTPUTLEN      = 15.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFETD'.
  LS_FIELDCAT-SELTEXT_M      = 'E.T.D'.
  LS_FIELDCAT-OUTPUTLEN      = 8.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFRETA'.
  LS_FIELDCAT-SELTEXT_M      = 'Real E.T.A'.
  LS_FIELDCAT-OUTPUTLEN      = 8.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFCARNM'.
  LS_FIELDCAT-SELTEXT_M      = 'Vessel Name'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFVIA'.
  LS_FIELDCAT-SELTEXT_M      = 'VIA'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFSPRT'.
  LS_FIELDCAT-SELTEXT_M      = 'Loading Port'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFAPRT'.
  LS_FIELDCAT-SELTEXT_M      = 'Arrival Port'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.


  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'W_ZFFORD'.
  LS_FIELDCAT-SELTEXT_M      = 'Forwarder.'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'W_EKGRP'.
  LS_FIELDCAT-SELTEXT_M      = 'Purchaing Group'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'W_ZFBENI'.
  LS_FIELDCAT-SELTEXT_M      = 'Beneficiary'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'W_LIFNR'.
  LS_FIELDCAT-SELTEXT_M      = 'Vendor'.
  LS_FIELDCAT-OUTPUTLEN      = 13.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*& FORM P3000_DATA_WRITE_ALV
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE_ALV.
  DATA: L_TEXT(70) TYPE C.
  IF P_DN EQ 'X'.
    MOVE 'Broker Interface File List for Download' TO L_TEXT.
  ELSEIF P_UP EQ 'X'.
    MOVE 'Broker Interface File List for Upload' TO L_TEXT.
  ENDIF.
  G_REPID = SY-REPID.
  DATA: SLIS_FORMNAME(30)  TYPE C.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            IS_LAYOUT                = G_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_SORT                  = GT_SORT[]
            I_CALLBACK_PF_STATUS_SET = G_STATUS
*            I_CALLBACK_TOP_OF_PAGE   = SLIS_FORMNAME
*            I_HTML_HEIGHT_TOP        = 0
            I_CALLBACK_USER_COMMAND = G_USER_COMMAND
            I_GRID_TITLE             =  L_TEXT
*            I_GRID_TITLE             = 'Broker Interface File List'
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
*            I_SCREEN_START_COLUMN = 1000
*            I_SCREEN_START_LINE = 30
       TABLES
            T_OUTTAB           = IT_TAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E977 WITH 'An Error occured during Grid Dispaly .'.
  ENDIF.

ENDFORM.                    "P3000_DATA_WRITE_ALV
*&---------------------------------------------------------------------*
*& FORM P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
FORM P2000_ALV_PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZIM24A'.
ENDFORM.                    " P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
FORM P2000_ALV_COMMAND USING R_UCOMM      LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA: MM03_START_SICHT(15) TYPE C  VALUE 'BDEKLPQSVXZA'.
  CASE R_UCOMM.
      READ TABLE IT_TAB INDEX RS_SELFIELD-TABINDEX.
*>> File Download..
    WHEN 'DOWN'.
      PERFORM P2000_DOWNLOAD TABLES IT_TAB.

*>> File Upload..
    WHEN 'UP'.
      PERFORM P2000_UPLOAD   TABLES  IT_TAB.

*>> Display Bill of Lading..
    WHEN 'DSBL'.
      PERFORM P2000_DSBL USING IT_TAB-ZFBLNO.
*>> Display Purchase Order..
    WHEN 'DSPO'.
      PERFORM P2000_DSPO USING IT_TAB-ZFREBELN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P2000_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P2000_DOWNLOAD TABLES   IT_TAB.

  CLEAR : W_FILENAME.

  CONCATENATE 'C:\' 'DOWNLOAD' '.TXT' INTO W_FILENAME.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            FILENAME = W_FILENAME
            FILETYPE = 'DAT'
       IMPORTING
            CANCEL   = CANCEL
       TABLES
            DATA_TAB = IT_TAB.

  IF SY-SUBRC EQ 0.
    IF CANCEL IS INITIAL.
      MESSAGE I460(ZIM1) WITH W_FILENAME.
    ENDIF.
  ELSE.
    MESSAGE E461(ZIM1) WITH 'DOWNLOAD'.
  ENDIF.

ENDFORM.                    " P2000_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_UPLOAD
*&---------------------------------------------------------------------*
FORM P2000_UPLOAD TABLES   IT_TAB.

  CLEAR : W_FILENAME.
  MOVE 'UPLOAD' TO W_FILENAME.

  CONCATENATE 'C:\' 'UPLOAD' '.TXT' INTO W_FILENAME.
  CALL FUNCTION 'UPLOAD'
       EXPORTING
            FILENAME     = W_FILENAME
            FILETYPE     = 'DAT'
       TABLES
            DATA_TAB     = IT_TAB.

  IF SY-SUBRC NE 0.
    MESSAGE E463(ZIM1) WITH W_FILENAME.
  ENDIF.

ENDFORM.                    " P2000_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_DSBL
*&---------------------------------------------------------------------*
FORM P2000_DSBL USING    ZFBLNO.

  SET PARAMETER ID 'ZPBLNO'  FIELD ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DSBL
*&---------------------------------------------------------------------*
*&      Form  P2000_DSPO
*&---------------------------------------------------------------------*
FORM P2000_DSPO USING    EBELN.

  SELECT SINGLE * FROM EKKO
         WHERE    EBELN EQ EBELN.
  IF SY-SUBRC EQ 0.

    IF EKKO-BSTYP EQ 'K'.
      SET PARAMETER ID 'CTR' FIELD EBELN.
      CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
    ELSEIF EKKO-BSTYP EQ 'L'.
      SET PARAMETER ID 'SAG' FIELD EBELN.
      CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BSP' FIELD ''.
      EXPORT 'BSP' TO MEMORY ID 'BSP'.
      SET PARAMETER ID 'BES' FIELD EBELN.
      EXPORT 'BES'  TO MEMORY ID 'BES'.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_DSPO
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_DATA_DOWNLOAD TABLES IT_EDIFILE.

  PERFORM P3000_TO_LOCAL_DOWNLOAD.

ENDFORM.                    " P3000_DATA_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P3000_TO_LOCAL_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_LOCAL_DOWNLOAD.
DATA: W_FILENAME  LIKE RLGRAP-FILENAME.
  CONCATENATE 'C:\' 'IMPREQ' SY-DATUM SY-UZEIT '.itf'
         INTO W_FILENAME.
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = W_FILENAME
            FILETYPE = 'ASC'
       TABLES
            DATA_TAB = IT_EDIFILE.
ENDFORM.                    " P3000_TO_LOCAL_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.
  DATA : TEXT100(100) TYPE  C.
  IF W_OK_CODE EQ 'FRGS' OR W_OK_CODE EQ 'DOWN'.
    TEXT100 = 'Entry/Immediate Delivery file create continue?'.
  ELSEIF W_OK_CODE EQ 'UP'.
    TEXT100 = 'Entry Summary file upload continue?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'EDI FILE CREATE/CANCEL ??'
      DIAGNOSE_OBJECT       = ''
      TEXT_QUESTION         = TEXT100
      TEXT_BUTTON_1         = '?    ?'
      TEXT_BUTTON_2         = '? ? ?'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = 'X'
      START_COLUMN          = 30
      START_ROW             = 8
    IMPORTING
      ANSWER                = W_BUTTON_ANSWER.



ENDFORM.                    " P2000_POPUP_MESSAGE
