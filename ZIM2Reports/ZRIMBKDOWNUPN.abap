*&---------------------------------------------------------------------*
*& Report  ZRIMBLLIST                                                  *
*&---------------------------------------------------------------------*
*&  Program    : Broker Interface File Upload and Download.            *
*&  Developer  : SHINHO NA                                             *
*&  Created On : 2003.12.12                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
REPORT  ZRIMBLLIST   MESSAGE-ID ZIM
                     LINE-SIZE 148
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,     ZTBLIT,   ZTBLCON,
         ZTCIVHD,  ZTCIVIT,  ZTIDRUS, ZTIV,
         ZTIDSUS, *ZTIDSUS,  ZTIEPORT,
         EKKO,     LIKP,     LIPS.

TYPE-POOLS : SLIS.
*-----------------------------------------------------------------------
* Declaration Internal Table for B/L Receipt date..
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
        BUKRS        LIKE ZTBL-BUKRS,          " Company Code..
        ZFCIVNO      LIKE ZTCIVHD-ZFCIVNO,     " Commercial Invoice No.
        ZFEDIST      LIKE ZTCIVHD-ZFEDIST,     " EDI Status..
        ZFDOCST      LIKE ZTCIVHD-ZFDOCST,     " Document Status..
        ZFHBLNO      LIKE ZTBL-ZFHBLNO,        " House B/L No.
        IMPORTER     LIKE ZTIMIMGTX-ZFAPPNM,   " Applicant..
        ZFREBELN     LIKE ZTBL-ZFREBELN,       " Representative P/O No.
        ZFBLNO       LIKE ZTBL-ZFBLNO,         " B/L Document No.
        W_PO(13)     TYPE C,
        ZFSHNO       LIKE ZTBL-ZFSHNO,         " Shipment Sequence..
        ZFPOYN       LIKE ZTBL-ZFPOYN,         " Monetary Y/N..
        ZFETD        LIKE ZTBL-ZFETD,          " E.T.D.
        ZFCARNM      LIKE ZTBL-ZFCARNM,        " Vessel Name..
        ZFSSEQ       LIKE ZTLG-ZFSSEQ,         " Voyage No.
        ZFCARC       LIKE ZTBL-ZFCARC,         " Country of Origin.
        ZFSPRT       LIKE ZTBL-ZFSPRT,         " Loading Port.
        ZFSPRTC      LIKE ZTBL-ZFSPRTC,        " Loading Port code.
        POE_NAME     LIKE ZTIEPORT-PORTT,      " Loading Port Name.
        ZFAPPC       LIKE ZTBL-ZFAPPC,         " Arrival Country Code.
        ZFAPRT       LIKE ZTBL-ZFAPRT,         " Arrival Port.
        ZFAPRTC      LIKE ZTBL-ZFAPRTC,        " Arrival Port Code.
        POI_NAME     LIKE ZTIEPORT-PORTT,      " Arrival Port Name.
        EKGRP        LIKE ZTBL-EKGRP,          " Purchasing Group.
        W_EKGRP(20)  TYPE C,                   " Purchasing Group Name.
        LIFNR        LIKE ZTBL-LIFNR,          " Supplier.
        SUP_NAME     LIKE LFA1-NAME1,          " Supplier Name.
        ZFRGDSR      LIKE ZTBL-ZFRGDSR,        " Repre. Goods Name.
        ZFETA        LIKE ZTBL-ZFETA,          " E.T.A.
        ZFRETA       LIKE ZTBL-ZFRETA,         " Real E.T.A.
        ZFVIA        LIKE ZTBL-ZFVIA,          " V.I.A.
        ZFFORD       LIKE ZTBL-ZFFORD,         " Shipping Company.
        W_ZFFORD(25) TYPE C,                   " Shipping Company Name.
        ZFBENI       LIKE ZTBL-ZFBENI,         " Beneficiary.
        W_ZFBENI(30) TYPE C,                   " Beneficiary Name.
        ZFNEWT       LIKE ZTBL-ZFNEWT,         " Net Weight.
        ZFNEWTM      LIKE ZTBL-ZFNEWTM,        " Net Weight UOM.
        ZFBLAMT      LIKE ZTBL-ZFBLAMT,        " B/L Amount..
        ZFBLAMC      LIKE ZTBL-ZFBLAMC,        " B/L Amount Currency..
        ZFFRGHT      LIKE ZTBLCST-ZFCAMT,      " Total Freight.
        ZFFRGHTC     LIKE ZTBLCST-WAERS,       " Freight Currency.
        INCO1        LIKE ZTBL-INCO1,          " Incoterms (part 1).
        ZFREQTY      LIKE ZTREQHD-ZFREQTY,     " Import Payment Type.
        ZFOPNNO      LIKE ZTREQHD-ZFOPNNO.     " L/C-approval No..
DATA : END OF IT_TAB.
*----------------------------------------------------------------------*
* Customs Clearance(US)
*----------------------------------------------------------------------*
DATA : IT_ZSIDSUSH      LIKE ZSIDSUSH OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSUSH_ORG  LIKE ZSIDSUSH OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Customs Declaration(US-HS Detail) Internal Table..
*----------------------------------------------------------------------*
DATA : IT_ZSIDSUSD      LIKE ZSIDSUSD OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIDSUSD_ORG  LIKE ZSIDSUSD OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Internal Table Declaration for BDC..
*----------------------------------------------------------------------*
DATA:    BEGIN OF BDCDATA OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA END OF BDCDATA.

*----------------------------------------------------------------------*
* Internal Table Declaration for BDC Message Displaying
*----------------------------------------------------------------------*
DATA:   MESSTAB LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE.

DATA : W_CIVNO        LIKE  ZTCIVHD-ZFCIVNO.
DATA : W_ZFBLAMT(19)  TYPE  C.
DATA : W_ZFFRGHT(19)  TYPE  C.
DATA : W_ZFNEWT(17)   TYPE  C.
DATA : PARA_LENG      TYPE  I.
DATA : BUFFER_POINTER TYPE  I.
DATA : L_LINENO       TYPE  I.
DATA : L_LINENO1      TYPE  I.
DATA : W_LINENO(3)    TYPE  C.
DATA : W_LINES        TYPE  I.
DATA : W_RC           TYPE  I.
DATA : W_MATNR(18)    TYPE  C.
DATA : W_IVAMT(19)    TYPE  C.
DATA : W_ZFTOWT(17)   TYPE  C.
DATA : W_ZFCOAMT(19)  TYPE  C.
DATA : W_BLMENGE(17)  TYPE  C.
DATA : W_TRAID        LIKE  LIKP-TRAID.
DATA : W_SUBRC        LIKE  SY-SUBRC.
DATA : W_FILENAME     LIKE  RLGRAP-FILENAME.
DATA : W_PIECE        TYPE  I.
DATA : W_PIECE1(8)    TYPE  C.
DATA : W_EDI_RECORD(65535).

*>> Declaration of internal table for file download.
DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILE.

*>> Declaration of internal table for file download.
DATA: BEGIN OF IT_EDIFILEC OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILEC.

DATA : BEGIN OF PATH OCCURS 0,
         NAME(300)   TYPE C,
       END OF PATH.

DATA : BEGIN OF IT_TXT OCCURS 0,
         W_EDI_RECORD(7000),
       END   OF IT_TXT.

*-----------------------------------------------------------------------
* Tables & Variable Definition.
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen Clause..
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO INTERVALS
                                         NO-EXTENSION,
                S_EBELN   FOR ZTBL-ZFREBELN, " Representative P/O No.
                S_HBLNO   FOR ZTBL-ZFHBLNO,  " House B/L No.
                S_BLADT   FOR ZTBL-ZFBLADT   " B/L Date.
                          NO-EXTENSION,
                S_RPTTY   FOR ZTBL-ZFRPTTY,  " Import Declaration Ty.
                S_EKGRP   FOR ZTBL-EKGRP,    " Purchasing Group.
                S_ZFTRCK  FOR ZTBL-ZFTRCK,   " Trucker
                S_ETA     FOR ZTBL-ZFETA     " E.T.A.
                          NO-EXTENSION,
                S_SPRTC   FOR ZTBL-ZFSPRTC   " Loading Port..
                          NO INTERVALS.
PARAMETERS :    P_VIA     LIKE ZTBL-ZFVIA.   " VIA
SELECT-OPTIONS: S_FORD    FOR ZTBL-ZFFORD.   " Forwarder
PARAMETERS :    P_POYN    LIKE ZTBL-ZFPOYN.  " Monetary Transaction Y/N.
SELECT-OPTIONS: S_SHTY    FOR ZTBL-ZFSHTY,   "
                S_WERKS   FOR ZTBL-ZFWERKS.  " 대표 PLANT
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

SELECTION-SCREEN : BEGIN OF LINE,
                   COMMENT 01(15) TEXT-R01.
SELECTION-SCREEN : COMMENT 27(5) TEXT-R03.
PARAMETERS : P_DN RADIOBUTTON GROUP RDG DEFAULT 'X'.
SELECTION-SCREEN : COMMENT 52(5) TEXT-R02.
PARAMETERS : P_DY RADIOBUTTON GROUP RDG.
SELECTION-SCREEN : END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

TABLES SSCRFIELDS.
SELECTION-SCREEN FUNCTION KEY 1.

*>> Initial Value Setting.
INITIALIZATION.
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

AT SELECTION-SCREEN.
  IF SY-UCOMM EQ 'FC01'.                        " File Upload..
    PERFORM P2000_GET_UPLOAD_FILE.
    PERFORM P2000_UPLOAD_DATA.
  ENDIF.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " Header Display...

*-----------------------------------------------------------------------
* START OF SELECTION Clause.
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Parameter setting.
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Read data.
  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Report refered Text Table SELECT.
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* Report Write.
  PERFORM   P3000_DATA_WRITE.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*>> Sort Ascending & Descending..
    WHEN 'STUP' OR 'STDN'.
      W_FIELD_NM = 'ZFBLNO'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.

*>> Selection & Deselection..
    WHEN 'MKAL' OR 'MKLO'.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

*>> File Download..
    WHEN 'DOWN'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766. EXIT.
      ELSE.
        PERFORM P2000_POPUP_MESSAGE.
        IF W_BUTTON_ANSWER EQ '1'.
          PERFORM P3000_DATA_DOWNLOAD TABLES IT_SELECTED.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.

*>> Download Cancel..
    WHEN 'DNCL'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766. EXIT.
      ELSE.
        IF IT_SELECTED-ZFEDIST EQ 'N'.
          MESSAGE S977 WITH 'It is already canceled document.'.
          EXIT.
        ELSE.
        ENDIF.
        MOVE IT_SELECTED-ZFCIVNO TO ZTCIVHD-ZFCIVNO.
        SELECT SINGLE *
                 FROM ZTCIVIT
                WHERE ZFBLNO = IT_SELECTED-ZFBLNO.
        IF SY-SUBRC EQ 0.
          SELECT SINGLE *
                   FROM ZTCIVHD
                  WHERE ZFCIVRN = ZTCIVIT-ZFCIVRN.
        ENDIF.
        MOVE 'N' TO ZTCIVHD-ZFEDIST.
        MOVE 'N' TO ZTCIVHD-ZFDOCST.
        UPDATE ZTCIVHD.
        SELECT SINGLE *
                 FROM ZTBL
                WHERE ZFBLNO = IT_SELECTED-ZFBLNO.
        MOVE '3' TO ZTBL-ZFBLST.
        UPDATE ZTBL.
        IF SY-SUBRC EQ 0.
          MESSAGE S977 WITH 'Successfully Cancel Download Status'.
          LEAVE TO SCREEN 0.
        ELSE.
          MESSAGE S977 WITH 'An Error Occured during Cancel Download.'.
          EXIT.
        ENDIF.
      ENDIF.

*>> Display P/O, B/L Document..
    WHEN 'DSBL' OR 'DSPO'.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 0.
        MESSAGE S766. EXIT.
      ELSEIF W_SELECTED_LINES EQ 1.
        PERFORM P_2000_SHOW_DOCUMENT.
      ELSE.
        MESSAGE S569. EXIT.
      ENDIF.

*>> Refresh..
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
  MOVE 'UPLOAD' TO SSCRFIELDS-FUNCTXT_01.
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE:/55  '[  Broker Interface File List ]' CENTERED
             COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/129 'Date : ', SY-DATUM.  ", 101 'Page : ', W_PAGE.

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/ SY-VLINE, '  ' NO-GAP,        SY-VLINE NO-GAP,
      (10)'P/O No.'
                    NO-GAP CENTERED, SY-VLINE NO-GAP,
      (20)'Invoice No.'
                              NO-GAP CENTERED, SY-VLINE NO-GAP,
      (20)'Importer'          NO-GAP CENTERED, SY-VLINE NO-GAP,
      (10)'Exp.Port'          NO-GAP CENTERED, SY-VLINE NO-GAP,
      (10)'Exp.Date'          NO-GAP CENTERED, SY-VLINE NO-GAP,
      (24)'Total USD Value'   NO-GAP CENTERED, SY-VLINE NO-GAP,
      (17)'Vessel Name', (12)'Voyage No.' NO-GAP, SY-VLINE NO-GAP,
      (03)'Ori'               NO-GAP CENTERED, SY-VLINE NO-GAP,
      (03)'Inc'               NO-GAP CENTERED, SY-VLINE NO-GAP,
      (01)'E'                 NO-GAP CENTERED, SY-VLINE NO-GAP,
      (01)'D' NO-GAP CENTERED, SY-VLINE NO-GAP.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE:/ SY-VLINE, '  ' NO-GAP,        SY-VLINE NO-GAP,
      (10)'B/L No.' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (20)'House B/L No.' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (20)'Supplier' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (10)'Imp.Port' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (10)'Imp.Date' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (24)'Total Freight' NO-GAP CENTERED, SY-VLINE NO-GAP,
      (20)'Carrier'           NO-GAP CENTERED, SY-VLINE NO-GAP,
      (21)'Gross Weight' NO-GAP CENTERED, SY-VLINE NO-GAP.

  WRITE:/ SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
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

    MOVE: LFA1-NAME1   TO   IT_TAB-SUP_NAME.

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
              KEY_INCOMPLETE = 1
              NOT_AUTHORIZED = 2
              NOT_FOUND      = 3.

    MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFBENI.

*-----------------------------------------------------------------------
* T024 SELECT( 구매그룹)
*-----------------------------------------------------------------------
    SELECT SINGLE EKNAM INTO IT_TAB-W_EKGRP
      FROM T024
     WHERE EKGRP = IT_TAB-EKGRP.

    SELECT SINGLE ZFAPPNM
             INTO IT_TAB-IMPORTER
             FROM ZTIMIMGTX
            WHERE BUKRS = IT_TAB-BUKRS.

    MODIFY  IT_TAB INDEX W_TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET PF-STATUS 'ZIM24'.           " GUI Status Setting...
  SET  TITLEBAR 'ZIME6'.           " GUI Title Setting...

  LOOP AT IT_TAB.
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
  W_PAGE = 1. W_LINE = 1. W_COUNT = 0.

  PERFORM P3000_TITLE_WRITE.                      " Header Display..
  PERFORM P3000_DATA_WRITE.                       " Data Write..

ENDFORM.                    " RESET_LIST
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
      MOVE-CORRESPONDING IT_TAB TO IT_SELECTED.
      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : /125 'Total:', W_COUNT, 'case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD AS CHECKBOX, SY-VLINE NO-GAP,
      (10)IT_TAB-ZFREBELN          NO-GAP, SY-VLINE NO-GAP,
      (20)IT_TAB-ZFCIVNO           NO-GAP, SY-VLINE NO-GAP,
      (20)IT_TAB-IMPORTER          NO-GAP, SY-VLINE NO-GAP,
      (10)IT_TAB-ZFAPRT            NO-GAP, SY-VLINE NO-GAP,
      (10)IT_TAB-ZFETD             NO-GAP, SY-VLINE NO-GAP,
          IT_TAB-ZFBLAMT CURRENCY IT_TAB-ZFBLAMC NO-GAP,
          IT_TAB-ZFBLAMC           NO-GAP, SY-VLINE NO-GAP,
      (17)IT_TAB-ZFCARNM,
      (12)IT_TAB-ZFSSEQ            NO-GAP, SY-VLINE NO-GAP,
      (03)IT_TAB-ZFCARC            NO-GAP, SY-VLINE NO-GAP,
      (03)IT_TAB-INCO1             NO-GAP, SY-VLINE NO-GAP,
      (01)IT_TAB-ZFEDIST           NO-GAP, SY-VLINE NO-GAP, " EDI St..
      (01)IT_TAB-ZFDOCST           NO-GAP, SY-VLINE NO-GAP. " DOC St...

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  FORMAT RESET.
  WRITE: / SY-VLINE, '  '          NO-GAP, SY-VLINE NO-GAP,
      (10)IT_TAB-ZFBLNO            NO-GAP, SY-VLINE NO-GAP,
      (20)IT_TAB-ZFHBLNO           NO-GAP, SY-VLINE NO-GAP,
      (20)IT_TAB-SUP_NAME          NO-GAP, SY-VLINE NO-GAP,
      (10)IT_TAB-ZFSPRT            NO-GAP, SY-VLINE NO-GAP,
      (10)IT_TAB-ZFETA             NO-GAP, SY-VLINE NO-GAP,
          IT_TAB-ZFFRGHT CURRENCY IT_TAB-ZFFRGHTC   NO-GAP,
          IT_TAB-ZFFRGHTC          NO-GAP, SY-VLINE NO-GAP,
      (20)IT_TAB-W_ZFFORD          NO-GAP, SY-VLINE NO-GAP,
          IT_TAB-ZFNEWT UNIT IT_TAB-ZFNEWTM,
          IT_TAB-ZFNEWTM           NO-GAP, SY-VLINE NO-GAP.

  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE:/ SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE
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

  LOOP AT IT_TAB.
    SELECT SINGLE *
      FROM ZTIDRUS
     WHERE ZFBLNO = IT_TAB-ZFBLNO.
    IF SY-SUBRC EQ 0.
      DELETE IT_TAB INDEX SY-TABIX.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  LOOP AT IT_TAB.
    CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO IT_TAB-W_PO.
    SELECT SINGLE ZFSSEQ
             INTO IT_TAB-ZFSSEQ
             FROM ZTLG
            WHERE ZFBLNO = IT_TAB-ZFBLNO.
    MODIFY IT_TAB INDEX SY-TABIX.
  ENDLOOP.
  LOOP AT IT_TAB.
    SELECT SINGLE ZFCAMT WAERS
             INTO (IT_TAB-ZFFRGHT, IT_TAB-ZFFRGHTC)
             FROM ZTBLCST
            WHERE ZFBLNO = IT_TAB-ZFBLNO
              AND ZFCSCD = 'OBC'.
    MODIFY IT_TAB INDEX SY-TABIX.
  ENDLOOP.
  LOOP AT IT_TAB.
    SELECT SINGLE *
             FROM ZTCIVIT
            WHERE ZFBLNO = IT_TAB-ZFBLNO.
    IF SY-SUBRC EQ 0.
      SELECT SINGLE ZFCIVNO ZFEDIST ZFDOCST
               INTO (IT_TAB-ZFCIVNO, IT_TAB-ZFEDIST, IT_TAB-ZFDOCST)
               FROM ZTCIVHD
              WHERE ZFCIVRN = ZTCIVIT-ZFCIVRN.
      MODIFY IT_TAB INDEX SY-TABIX.
    ELSE.
      DELETE IT_TAB INDEX SY-TABIX.
    ENDIF.
  ENDLOOP.
  LOOP AT IT_TAB.
    IF P_DN EQ 'X'.
      IF IT_TAB-ZFEDIST NE 'N'.             " If Downlaoded file.
        DELETE IT_TAB INDEX SY-TABIX.
      ENDIF.
    ELSE.
      IF IT_TAB-ZFEDIST EQ 'N' OR IT_TAB-ZFEDIST EQ 'R'.
        DELETE IT_TAB INDEX SY-TABIX.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF SY-SUBRC NE 0.               " Not Found.
    W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> Company Code Setting.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_DATA_DOWNLOAD TABLES IT_SELECTED.
  LOOP AT IT_SELECTED.
    PERFORM P2000_MAKE_DOWNLOAD_DATA TABLES IT_SELECTED.
    WAIT UP TO 1 SECONDS.
    PERFORM P3000_TO_LOCAL_DOWNLOAD TABLES IT_EDIFILE.
  ENDLOOP.
ENDFORM.                    " P3000_DATA_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P3000_TO_LOCAL_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_LOCAL_DOWNLOAD TABLES IT_EDIFILE.
  CONCATENATE 'C:\' 'Broker_Invoice_' SY-DATUM SY-UZEIT '.itf'
         INTO W_FILENAME.
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME                = W_FILENAME
            FILETYPE                = 'ASC'
       TABLES
            DATA_TAB                = IT_EDIFILE
       EXCEPTIONS
            FILE_WRITE_ERROR        = 1
            INVALID_FILESIZE        = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            GUI_REFUSE_FILETRANSFER = 5
            CUSTOMER_ERROR          = 6.

  IF SY-SUBRC NE 0.
    MESSAGE S910 WITH W_FILENAME.
  ELSE.
*>> Change B/L Status..
    MOVE '4' TO ZTBL-ZFBLST.
    UPDATE ZTBL.
*>> Change Commercial Invoice Status..
    MOVE 'S' TO ZTCIVHD-ZFEDIST.
    MOVE 'R' TO ZTCIVHD-ZFDOCST.
    UPDATE ZTCIVHD.
  ENDIF.
  REFRESH IT_EDIFILE.

  CONCATENATE 'C:\' 'Broker_Container_' SY-DATUM SY-UZEIT '.itf'
         INTO W_FILENAME.
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = W_FILENAME
            FILETYPE = 'ASC'
       TABLES
            DATA_TAB = IT_EDIFILEC.
  REFRESH IT_EDIFILEC.

ENDFORM.                    " P3000_TO_LOCAL_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.
  DATA : TEXT100(100) TYPE  C.
  IF SY-UCOMM EQ 'FRGS' OR SY-UCOMM EQ 'DOWN'.
    TEXT100 = 'Continue Entry/Immediate Delivery file create?'.
  ELSEIF SY-UCOMM EQ 'UP'.
    TEXT100 = 'Continue Entry Summary file upload?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Entry/Immediate Delivery file create?'
      DIAGNOSE_OBJECT       = ''
      TEXT_QUESTION         = TEXT100
      TEXT_BUTTON_1         = 'Y E S'
      TEXT_BUTTON_2         = 'N   O'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = 'X'
      START_COLUMN          = 30
      START_ROW             = 8
    IMPORTING
      ANSWER                = W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P_2000_SHOW_DOCUMENT
*&---------------------------------------------------------------------*
FORM P_2000_SHOW_DOCUMENT.
  CASE SY-UCOMM.
    WHEN 'DSBL'.
      SET PARAMETER ID 'ZPBLNO'  FIELD IT_SELECTED-ZFBLNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.
      CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

    WHEN 'DSPO'.
      SELECT SINGLE * FROM EKKO
             WHERE    EBELN EQ IT_SELECTED-ZFREBELN.
      IF SY-SUBRC EQ 0.
        IF EKKO-BSTYP EQ 'K'.
          SET PARAMETER ID 'CTR' FIELD IT_SELECTED-ZFREBELN.
          CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
        ELSEIF EKKO-BSTYP EQ 'L'.
          SET PARAMETER ID 'SAG' FIELD IT_SELECTED-ZFREBELN.
          CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
        ELSE.
          SET PARAMETER ID 'BSP' FIELD ''.
          EXPORT 'BSP' TO MEMORY ID 'BSP'.
          SET PARAMETER ID 'BES' FIELD IT_SELECTED-ZFREBELN.
          EXPORT 'BES'  TO MEMORY ID 'BES'.
          CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDFORM.                    " P_2000_SHOW_DOCUMENT`
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
FORM P2000_MAKE_DOWNLOAD_DATA TABLES IT_SELECTED.
  CLEAR: L_LINENO, LIKP, LIPS.
  PERFORM P2000_MAKE_HEADER.
  SELECT SINGLE *
           FROM ZTCIVHD
          WHERE ZFCIVNO = IT_TAB-ZFCIVNO.

  IF SY-SUBRC EQ 0.
    SELECT *
      FROM ZTCIVIT
     WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE *
                 FROM ZTBL
                WHERE ZFBLNO = ZTCIVIT-ZFBLNO.
        IF NOT ZTBL-ZFHBLNO IS INITIAL.
          SELECT SINGLE *
                   FROM LIPS
                  WHERE VGBEL = ZTCIVIT-EBELN
                    AND VGPOS = ZTCIVIT-EBELP.
          IF SY-SUBRC EQ 0.
            SELECT SINGLE *
              FROM LIKP
             WHERE VBELN = LIPS-VBELN
               AND BOLNR = ZTCIVHD-ZFCIVNO.
            IF SY-SUBRC EQ 0.
              L_LINENO = L_LINENO + 1.
              PERFORM P2000_MAKE_ITEM USING L_LINENO.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDSELECT.
*>> Container Data Select.
    SELECT *
      FROM ZTCIVIT
     WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.
      IF SY-SUBRC EQ 0.
        ON CHANGE OF ZTCIVIT-ZFBLNO.
          SELECT *
            FROM ZTBLCON
           WHERE ZFBLNO = ZTCIVIT-ZFBLNO.
            L_LINENO1 = L_LINENO1 + 1.
            PERFORM P2000_MAKE_CONTAINER USING L_LINENO1.
          ENDSELECT.
        ENDON.
      ENDIF.
    ENDSELECT.
  ENDIF.
ENDFORM.                    " P2000_MAKE_DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_HEADER
*&---------------------------------------------------------------------*
FORM P2000_MAKE_HEADER.
  CLEAR: W_EDI_RECORD, BUFFER_POINTER.
  READ TABLE IT_TAB WITH KEY ZFBLNO = IT_SELECTED-ZFBLNO.
  SELECT SINGLE *
           FROM ZTBL
          WHERE ZFBLNO = IT_SELECTED-ZFBLNO.
*>> 01 Record Type.
  PARA_LENG = 3.
  MOVE 'CIH' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 02 Invoice Number.
  PARA_LENG = 35.
  IF NOT IT_TAB-ZFCIVNO IS INITIAL.
    MOVE IT_TAB-ZFCIVNO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 03 Importer.
  PARA_LENG = 35.
  IF NOT IT_TAB-IMPORTER IS INITIAL.
    MOVE IT_TAB-IMPORTER TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 04 Supplier.
  PARA_LENG = 35.
  IF NOT IT_TAB-SUP_NAME IS INITIAL.
    MOVE IT_TAB-SUP_NAME TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 05 Country of Origin.
  PARA_LENG = 3.
  IF NOT IT_TAB-ZFCARC IS INITIAL.
    MOVE IT_TAB-ZFCARC TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 06 Port of Export.
  SELECT SINGLE *
           FROM ZTIEPORT
          WHERE LAND1 = IT_TAB-ZFCARC
            AND PORT  = IT_TAB-ZFSPRTC.

  PARA_LENG = 5.
  IF NOT ZTIEPORT-ZFREFCD IS INITIAL.
    MOVE ZTIEPORT-ZFREFCD TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.
  CLEAR ZTIEPORT.

*>> 07 Date of Export.
  PARA_LENG = 8.
  IF NOT IT_TAB-ZFETD IS INITIAL.
    MOVE IT_TAB-ZFETD TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 08 Port of Import.
  SELECT SINGLE *
           FROM ZTIEPORT
          WHERE LAND1 = IT_TAB-ZFAPPC
            AND PORT  = IT_TAB-ZFAPRTC.

  PARA_LENG = 5.
  IF NOT ZTIEPORT-ZFREFCD IS INITIAL.
    MOVE ZTIEPORT-ZFREFCD TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.
  CLEAR ZTIEPORT.

*>> 09 Carrier.
  PARA_LENG = 35.
  IF NOT IT_TAB-W_ZFFORD IS INITIAL.
    MOVE IT_TAB-W_ZFFORD TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 10 Vessel.
  PARA_LENG = 35.
  IF NOT IT_TAB-ZFCARNM IS INITIAL.
    MOVE IT_TAB-ZFCARNM TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 11 Voyage..
  PARA_LENG = 17.
  IF NOT IT_TAB-ZFSSEQ IS INITIAL.
    MOVE IT_TAB-ZFSSEQ TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 12 ETA..
  PARA_LENG = 8.
  IF NOT IT_TAB-ZFETA IS INITIAL.
    MOVE IT_TAB-ZFETA TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 13 Total Value in USD.
  PARA_LENG = 1.                      " FOR QUAN FIELD.
  ADD PARA_LENG TO BUFFER_POINTER.
  IF NOT IT_TAB-ZFBLAMT IS INITIAL.
    WRITE IT_TAB-ZFBLAMT TO W_ZFBLAMT
          CURRENCY IT_TAB-ZFBLAMC RIGHT-JUSTIFIED.
    MOVE W_ZFBLAMT TO W_EDI_RECORD+BUFFER_POINTER(18).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(18).
  ENDIF.
  PARA_LENG = 18.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 14 Total Freight/Intl Chgs in USD..
  PARA_LENG = 1.                      " FOR QUAN FIELD.
  ADD PARA_LENG TO BUFFER_POINTER.
  IF NOT IT_TAB-ZFFRGHT IS INITIAL.
    WRITE IT_TAB-ZFFRGHT TO W_ZFFRGHT
          CURRENCY IT_TAB-ZFFRGHTC RIGHT-JUSTIFIED.
    MOVE W_ZFFRGHT TO W_EDI_RECORD+BUFFER_POINTER(18).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(18).
  ENDIF.
  PARA_LENG = 18.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 15 Total Gross Weight in Kilos..
  IF NOT IT_TAB-ZFNEWT IS INITIAL.
    WRITE IT_TAB-ZFNEWT TO W_ZFNEWT
          UNIT IT_TAB-ZFNEWTM RIGHT-JUSTIFIED.
    IF W_ZFNEWT+12(1) EQ '.'.
      ADD 1 TO BUFFER_POINTER.
      MOVE W_ZFNEWT(15) TO W_ZFNEWT.
      MOVE W_ZFNEWT TO W_EDI_RECORD+BUFFER_POINTER(16).
      BUFFER_POINTER = BUFFER_POINTER - 1.
    ELSE.
      MOVE W_ZFNEWT(17) TO W_BLMENGE.
      MOVE W_ZFNEWT TO W_EDI_RECORD+BUFFER_POINTER(16).
    ENDIF.
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  PARA_LENG = 16.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 16 # Pieces..
  PARA_LENG = 8.
  SELECT *
    FROM ZTCIVIT
   WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.
    IF SY-SUBRC EQ 0.
      ON CHANGE OF ZTCIVIT-ZFBLNO.
        SELECT *
          FROM ZTBLCON
         WHERE ZFBLNO = ZTCIVIT-ZFBLNO.
          SELECT COUNT( * )
            INTO W_PIECE
            FROM LIKP
           WHERE BOLNR = ZTCIVHD-ZFCIVNO
             AND TRAID = ZTBLCON-ZFCONNO.
        ENDSELECT.
      ENDON.
    ENDIF.
  ENDSELECT.

  WRITE W_PIECE TO W_PIECE1 RIGHT-JUSTIFIED.
  MOVE W_PIECE TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 17 Piece Type..
  PARA_LENG = 3.
  MOVE 'EA' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 18 DDU/DDP Indicator..
  PARA_LENG = 3.
  IF NOT IT_TAB-INCO1 IS INITIAL.
    MOVE IT_TAB-INCO1 TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 19 Related Indicator..
  PARA_LENG = 1.
  MOVE 'N' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 20 FTZ Indicator..
*>> Logic for reading Non FTZ item needs..
  PARA_LENG = 1.
  MOVE 'N' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

ENDFORM.                    " P2000_MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_ITEM
*&---------------------------------------------------------------------*
FORM P2000_MAKE_ITEM USING L_LINENO.
  CLEAR: W_EDI_RECORD, BUFFER_POINTER, W_TRAID.

  SELECT SINGLE *
           FROM ZTBLIT
          WHERE ZFBLNO = ZTCIVIT-ZFBLNO
            AND EBELN  = ZTCIVIT-EBELN
            AND EBELP  = ZTCIVIT-EBELP.
*>> 01 Record Type.
  PARA_LENG = 3.
  MOVE 'CII' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 02 Invoice Number.
  PARA_LENG = 35.
  MOVE ZTCIVHD-ZFCIVNO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 03 Line Item Number.
  WRITE L_LINENO TO W_LINENO LEFT-JUSTIFIED.
  PARA_LENG = 5.
  MOVE W_LINENO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 04 Part Number.
  WRITE ZTCIVIT-MATNR TO W_MATNR RIGHT-JUSTIFIED.
  PARA_LENG = 18.
  MOVE W_MATNR TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 05 Part Description.
  PARA_LENG = 40.
  MOVE ZTCIVIT-TXZ01 TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 06 Bill of Lading.
  PARA_LENG = 24.
  IF NOT ZTBL-ZFHBLNO IS INITIAL.
    MOVE ZTBL-ZFHBLNO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 07 Container Number.
  PARA_LENG = 20.
  IF NOT LIKP-TRAID IS INITIAL.
    WRITE LIKP-TRAID TO W_TRAID LEFT-JUSTIFIED.
    MOVE W_TRAID TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 08 Value in USD..
  PARA_LENG = 1.                      " FOR QUAN FIELD.
  ADD PARA_LENG TO BUFFER_POINTER.
  IF NOT ZTCIVIT-ZFIVAMP IS INITIAL.
    WRITE ZTCIVIT-ZFIVAMP TO W_IVAMT
          CURRENCY ZTCIVIT-ZFIVAMC RIGHT-JUSTIFIED.
    MOVE W_IVAMT TO W_EDI_RECORD+BUFFER_POINTER(18).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  PARA_LENG = 18.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 09 Net Quantity in Reportable Qty.
  IF NOT ZTBLIT-BLMENGE IS INITIAL.
    WRITE ZTBLIT-BLMENGE TO W_BLMENGE UNIT ZTBLIT-MEINS RIGHT-JUSTIFIED.
    IF W_BLMENGE+12(1) EQ '.'.
      ADD 1 TO BUFFER_POINTER.
      MOVE W_BLMENGE(15) TO W_BLMENGE.
      MOVE W_BLMENGE TO W_EDI_RECORD+BUFFER_POINTER(16).
      BUFFER_POINTER = BUFFER_POINTER - 1.
    ELSE.
      MOVE W_BLMENGE(17) TO W_BLMENGE.
      MOVE W_BLMENGE TO W_EDI_RECORD+BUFFER_POINTER(16).
    ENDIF.
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(16).
  ENDIF.
  PARA_LENG = 16.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 10 HTS No.
  PARA_LENG = 17.
  IF NOT ZTBLIT-STAWN IS INITIAL.
    MOVE ZTBLIT-STAWN TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 11 Quality UOM..
  PARA_LENG = 3.
  IF NOT ZTBLIT-MEINS IS INITIAL.
    MOVE ZTBLIT-MEINS TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

ENDFORM.                    " P2000_MAKE_ITEM
*&---------------------------------------------------------------------*
*&      Form  P2000_MAKE_CONTAINER
*&---------------------------------------------------------------------*
FORM P2000_MAKE_CONTAINER USING    L_LINENO1.

  CLEAR: W_EDI_RECORD, BUFFER_POINTER, W_TRAID.

*>> 01 Record Type.
  PARA_LENG = 3.
  MOVE 'CIC' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 02 Invoice Number.
  PARA_LENG = 35.
  MOVE ZTCIVHD-ZFCIVNO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 03 Line Item Number.
  WRITE L_LINENO1 TO W_LINENO LEFT-JUSTIFIED.
  PARA_LENG = 5.
  MOVE W_LINENO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 04 Container Number.
  PARA_LENG = 20.
  IF NOT ZTBLCON-ZFCONNO IS INITIAL.
    MOVE ZTBLCON-ZFCONNO TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 05 Vessel Name..
  PARA_LENG = 35.
  IF NOT ZTBL-ZFCARNM IS INITIAL.
    MOVE ZTBL-ZFCARNM TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 06 Voyage Number..
  PARA_LENG = 17.
  IF NOT ZTBL-ZFSSEQ IS INITIAL.
    MOVE ZTBL-ZFSSEQ TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 07 Number of Package..
  PARA_LENG = 8.
  IF NOT ZTBLCON-ZFPKCNT IS INITIAL.
    MOVE ZTBLCON-ZFPKCNT TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 08 UOM of Package..
  PARA_LENG = 2.
  MOVE 'EA' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 09 Gross weight of container..
  IF NOT ZTBLCON-ZFTOWT IS INITIAL.
    WRITE ZTBLCON-ZFTOWT TO W_ZFTOWT
          UNIT ZTBLCON-ZFTOWTM RIGHT-JUSTIFIED.
    IF W_ZFTOWT+12(1) EQ '.'.
      ADD 1 TO BUFFER_POINTER.
      MOVE W_ZFTOWT(15) TO W_BLMENGE.
      MOVE W_ZFTOWT TO W_EDI_RECORD+BUFFER_POINTER(16).
      BUFFER_POINTER = BUFFER_POINTER - 1.
    ELSE.
      MOVE W_ZFTOWT(17) TO W_ZFTOWT.
      MOVE W_ZFTOWT TO W_EDI_RECORD+BUFFER_POINTER(16).
    ENDIF.
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ENDIF.
  PARA_LENG = 16.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 10 Gross value..
  PARA_LENG = 1.                      " FOR QUAN FIELD.
  ADD PARA_LENG TO BUFFER_POINTER.
  IF NOT ZTBLCON-ZFCOAMT IS INITIAL.
    WRITE ZTBLCON-ZFCOAMT TO W_ZFCOAMT
          CURRENCY ZTBLCON-ZFCOAMC RIGHT-JUSTIFIED.
    MOVE W_ZFCOAMT TO W_EDI_RECORD+BUFFER_POINTER(18).
  ELSE.
    MOVE '' TO W_EDI_RECORD+BUFFER_POINTER(18).
  ENDIF.
  PARA_LENG = 18.
  ADD PARA_LENG TO BUFFER_POINTER.

*>> 11 Description..
  PARA_LENG = 14.
  MOVE 'Automatic Part' TO W_EDI_RECORD+BUFFER_POINTER(PARA_LENG).
  ADD PARA_LENG TO BUFFER_POINTER.

  IT_EDIFILEC-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILEC.

ENDFORM.                    " P2000_MAKE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM P2000_GET_UPLOAD_FILE.
  DATA : WL_COUNT     TYPE I.
  DATA : WL_LENGTH    TYPE I.
  CLEAR : WL_COUNT, WL_LENGTH, PATH.
  REFRESH : PATH.

  CALL FUNCTION 'TMP_GUI_FILE_OPEN_DIALOG'
       EXPORTING
            WINDOW_TITLE   = 'Broker Upload File Selection'
            INIT_DIRECTORY = 'C:\'
       IMPORTING
            RC             = W_RC
       TABLES
            FILE_TABLE     = PATH
       EXCEPTIONS
            CNTL_ERROR     = 4.
  IF W_RC EQ 1.
    READ TABLE PATH INDEX 1.
    WL_COUNT = 0.
    WL_LENGTH = STRLEN( PATH-NAME ).
    IF WL_LENGTH GT 0.
      MOVE PATH-NAME(WL_LENGTH) TO W_FILENAME.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_GET_UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  P2000_UPLOAD_DATA
*&---------------------------------------------------------------------*
FORM P2000_UPLOAD_DATA.
  CLEAR:   W_COUNT.

  IF W_RC EQ 1.
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              FILENAME                = W_FILENAME
              FILETYPE                = 'ASC'
         TABLES
              DATA_TAB                = IT_TXT
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8
              CUSTOMER_ERROR          = 9
              OTHERS                  = 10.

    IF SY-SUBRC NE 0. EXIT. ENDIF.

    READ TABLE IT_TXT INDEX 1.
*>> Select Commercial Invoice Data.
    MOVE IT_TXT-W_EDI_RECORD+21(35) TO W_CIVNO.
    PERFORM P2000_READ_CIV_DATA USING W_CIVNO.

*>> Make Header Data.
    PERFORM P3000_MAKE_HEADER USING IT_TXT-W_EDI_RECORD.

*>> Make Item Data.
    DESCRIBE TABLE IT_TXT LINES W_LINES.
    LOOP AT IT_TXT FROM 2 TO W_LINES.
      PERFORM P3000_MAKE_ITEM USING IT_TXT-W_EDI_RECORD.
      APPEND IT_ZSIDSUSH.
    ENDLOOP.

*>> Make Clearance Request Data.
    PERFORM P3000_MAKE_CC USING ZTIDSUS-ZFBLNO.

*>> Update Upload Status..
    PERFORM P3000_UPDATE_UPLOAD_STATUS USING ZTIDSUS-ZFBLNO.

*>> Make Entry Summary Data..
    CALL FUNCTION 'ZIM_ZTIDSUS_DOC_MODIFY'
         EXPORTING
              W_OK_CODE       = 'SAVE'
              ZFIVNO          = ZTIDSUS-ZFIVNO
              ZFCLSEQ         = ZTIDSUS-ZFCLSEQ
              ZFSTATUS        = 'C'
              W_ZTIDSUS_OLD   = *ZTIDSUS
              W_ZTIDSUS       = ZTIDSUS
         TABLES
              IT_ZSIDSUSH_OLD = IT_ZSIDSUSH_ORG
              IT_ZSIDSUSH     = IT_ZSIDSUSH
              IT_ZSIDSUSD_OLD = IT_ZSIDSUSD_ORG
              IT_ZSIDSUSD     = IT_ZSIDSUSD
         EXCEPTIONS
              ERROR_UPDATE    = 1
              ERROR_DELETE    = 2
              ERROR_INSERT    = 3.

    IF SY-SUBRC EQ 0.

    ELSE.
      RAISE UPDATE_ERROR.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.

ENDFORM.                    " P2000_UPLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_CIV_DATA
*&---------------------------------------------------------------------*
FORM P2000_READ_CIV_DATA USING W_CIVNO.

  SELECT COUNT( * )
    INTO W_COUNT
    FROM ZTCIVHD
   WHERE ZFCIVNO = W_CIVNO
     AND ZFEDICK = 'S'.
  CASE W_COUNT.
    WHEN 0.         "> Commercial Invoice data is not exist.
      MESSAGE S977
        WITH 'There is no appropriated Commercial Invoice data'.
      EXIT.
    WHEN 1.         "> Only one Commercial Invoice data is exist.
      SELECT SINGLE *
               FROM ZTCIVIT
              WHERE ZFCIVRN EQ ZTCIVHD-ZFCIVRN.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE *
                 FROM ZTBL
                WHERE ZFBLNO = ZTCIVIT-ZFBLNO.
        MOVE ZTBL-ZFBLNO TO ZTIDSUS-ZFBLNO.
      ENDIF.
    WHEN OTHERS.    "> More than one commercial Invoice is exist.
      SELECT MAX( ZFCIVRN )
        INTO ZTCIVHD-ZFCIVRN
        FROM ZTCIVHD
       WHERE ZFCIVNO = W_CIVNO.
      SELECT SINGLE *
               FROM ZTCIVIT
              WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.
      IF SY-SUBRC EQ 0.
        SELECT SINGLE *
                 FROM ZTBL
                WHERE ZFBLNO = ZTCIVIT-ZFBLNO.
        MOVE ZTBL-ZFBLNO TO ZTIDSUS-ZFBLNO.
      ENDIF.
  ENDCASE.

ENDFORM.                    " P2000_READ_CIV_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_HEADER
*&---------------------------------------------------------------------*
FORM P3000_MAKE_HEADER USING IT_TXT-W_EDI_RECORD.

*>> 01 Entry No.
  MOVE IT_TXT-W_EDI_RECORD(13) TO ZTIDSUS-ZFENTNO.     " Entry No..
  MOVE IT_TXT-W_EDI_RECORD+8(3) TO ZTIDSUS-ZFCTW.      " Broker Code..

*>> 02 Entry Date.
  IF NOT IT_TXT-W_EDI_RECORD+13(8) IS INITIAL.
*>> Convert Date to Internal Field.
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
         EXPORTING
              DATE_EXTERNAL = IT_TXT-W_EDI_RECORD+13(8)
         IMPORTING
              DATE_INTERNAL = ZTIDSUS-ZFEDT.
  ENDIF.

*>> 04 Date of arrival in port.
  IF NOT IT_TXT-W_EDI_RECORD+56(8) IS INITIAL.
*>> Convert Date to Internal Field.
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
         EXPORTING
              DATE_EXTERNAL = IT_TXT-W_EDI_RECORD+56(8)
         IMPORTING
              DATE_INTERNAL = ZTIDSUS-ZFCTW.
  ENDIF.

*>> 05 Exchange Rate.
  MOVE IT_TXT-W_EDI_RECORD+64(12) TO ZTIDSUS-ZFEXRT.

*>> 07 Duty Currency.
  IF NOT IT_TXT-W_EDI_RECORD+94(3) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+94(3) TO ZTIDSUS-ZFTOCUR.

*>> 06 Duty.
    IF IT_TXT-W_EDI_RECORD+76(18) IS INITIAL.
*>> Convert Amount to Internal Field.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
           EXPORTING
                CURRENCY             = ZTIDSUS-ZFTOCUR
                AMOUNT_EXTERNAL      = IT_TXT-W_EDI_RECORD+76(18)
                MAX_NUMBER_OF_DIGITS = 18
           IMPORTING
                AMOUNT_INTERNAL      = ZTIDSUS-ZFDUTY.
    ENDIF.

*>> 08 Other Fee.
    IF NOT IT_TXT-W_EDI_RECORD+97(18) IS INITIAL.
*>> Convert Amount to Internal Field.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
           EXPORTING
                CURRENCY             = ZTIDSUS-ZFTOCUR
                AMOUNT_EXTERNAL      = IT_TXT-W_EDI_RECORD+97(18)
                MAX_NUMBER_OF_DIGITS = 18
           IMPORTING
                AMOUNT_INTERNAL      = ZTIDSUS-ZFOTFE.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_MAKE_HEADER
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_ITEM
*&---------------------------------------------------------------------*
FORM P3000_MAKE_ITEM USING IT_TXT-W_EDI_RECORD.

*>> 01 Commercial Invoice No.
  IF W_CIVNO EQ IT_TXT-W_EDI_RECORD(35).
    MESSAGE S375 WITH W_CIVNO.
    EXIT.
  ENDIF.

*>> 02 Entry No.
  IF NOT IT_TXT-W_EDI_RECORD+35(13) IS INITIAL.
    IF ZTIDSUS-ZFENTNO NE IT_TXT-W_EDI_RECORD+35(13).
      MESSAGE S585 WITH 'Entry Summary No.' IT_TXT-W_EDI_RECORD+35(13).
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE S977
       WITH 'Entry No. is not exit in the upload file item data'.
  ENDIF.

*>> 03 Column Sequence No.
  IF NOT IT_TXT-W_EDI_RECORD+48(3) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+48(3) TO IT_ZSIDSUSH-ZFCONO.
  ELSE.
    MESSAGE S977 WITH 'Entry Summary item data is not exist'.
    EXIT.
  ENDIF.

*>> 04 H/S Code.
  IF NOT IT_TXT-W_EDI_RECORD+51(17) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+51(17) TO IT_ZSIDSUSH-STAWN.
  ELSE.
    MESSAGE S977 WITH 'H/S Code data of is not exist'.
    EXIT.
  ENDIF.

*>> 05 Names of Goods.
  IF NOT IT_TXT-W_EDI_RECORD+68(50) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+68(50) TO IT_ZSIDSUSH-ZFGDNM.
  ELSE.
    MESSAGE S977 WITH 'Names of Goods are not exist.'.
    EXIT.
  ENDIF.

*>> 06 Tarriff Rate.
  IF NOT IT_TXT-W_EDI_RECORD+118(6) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+118(6) TO IT_ZSIDSUSH-ZFCURT.

*>> 07 Tarriff.
    IF NOT IT_TXT-W_EDI_RECORD+124(19) IS INITIAL.
*>> Convert Amount to Internal Field.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
           EXPORTING
                CURRENCY             = ZTIDSUS-ZFTOCUR
                AMOUNT_EXTERNAL      = IT_TXT-W_EDI_RECORD+124(19)
                MAX_NUMBER_OF_DIGITS = 19
           IMPORTING
                AMOUNT_INTERNAL      = IT_ZSIDSUSH-ZFCUAMT.
    ENDIF.
  ENDIF.

*>> 08 MPF Rate.
  IF NOT IT_TXT-W_EDI_RECORD+143(7) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+143(7) TO IT_ZSIDSUSH-ZFMPRT.

*>> 09 MPF.
    IF NOT IT_TXT-W_EDI_RECORD+150(19) IS INITIAL.
*>> Convert Amount to Internal Field.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
           EXPORTING
                CURRENCY             = ZTIDSUS-ZFTOCUR
                AMOUNT_EXTERNAL      = IT_TXT-W_EDI_RECORD+150(19)
                MAX_NUMBER_OF_DIGITS = 19
           IMPORTING
                AMOUNT_INTERNAL      = IT_ZSIDSUSH-ZFMPAMT.
    ENDIF.
  ENDIF.

*>> 10 HMF Rate.
  IF NOT IT_TXT-W_EDI_RECORD+169(7) IS INITIAL.
    MOVE IT_TXT-W_EDI_RECORD+169(7) TO IT_ZSIDSUSH-ZFHMRT.

*>> 11 HMF.
    IF NOT IT_TXT-W_EDI_RECORD+176(17) IS INITIAL.
*>> Convert Amount to Internal Field.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
           EXPORTING
                CURRENCY             = ZTIDSUS-ZFTOCUR
                AMOUNT_EXTERNAL      = IT_TXT-W_EDI_RECORD+176(17)
                MAX_NUMBER_OF_DIGITS = 17
           IMPORTING
                AMOUNT_INTERNAL      = IT_ZSIDSUSH-ZFHMAMT.
    ENDIF.
  ENDIF.

ENDFORM.                    " P3000_MAKE_ITEM
*&---------------------------------------------------------------------*
*&      Form  P3000_MAKE_CC
*&---------------------------------------------------------------------*
FORM P3000_MAKE_CC USING ZTIDSUS-ZFBLNO.

  SELECT SINGLE *
           FROM ZTIV
          WHERE ZFBLNO = ZTIDSUS-ZFBLNO.
  IF SY-SUBRC NE 0.
    SELECT SINGLE * FROM ZTIMIMG00.

    IF ZTIMIMG00-ZFBDCYN EQ 'X'.
      MOVE 'Y' TO W_MODE.
    ENDIF.

    PERFORM P2000_DYNPRO USING :
            'X' 'SAPMZIM01'       '3100',
            ' ' 'ZSREQHD-ZFBLNO'  ZTIDSUS-ZFBLNO,
            ' ' 'BDC_OKCODE'      '=ENTR'.

    PERFORM P2000_DYNPRO USING :
            'X' 'SAPMZIM01'       '3110',
            ' ' 'BDC_OKCODE'      '=SAVE'.

    PERFORM P2000_DYNPRO USING :
            'X' 'SAPMZIM01'       '0001',
            ' ' 'BDC_OKCODE'      '=YES'.

    CALL TRANSACTION 'ZIM31'  USING       BDCDATA
                              MODE        W_MODE
                              MESSAGES    INTO   MESSTAB.
  ENDIF.

ENDFORM.                    " P3000_MAKE_CC
*----------------------------------------------------------------------*
*    FORM: BDCDATA DYNPRO ;
*----------------------------------------------------------------------*
FORM P2000_DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDCDATA.
    MOVE : NAME  TO BDCDATA-PROGRAM,
           VALUE TO BDCDATA-DYNPRO,
           'X'   TO BDCDATA-DYNBEGIN.
    APPEND BDCDATA.
  ELSE.
    CLEAR BDCDATA.
    MOVE : NAME  TO BDCDATA-FNAM,
           VALUE TO BDCDATA-FVAL.
    APPEND BDCDATA.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P3000_UPDATE_UPLOAD_STATUS
*&---------------------------------------------------------------------*
FORM P3000_UPDATE_UPLOAD_STATUS USING ZTIDSUS-ZFBLNO.

  SELECT SINGLE *
           FROM ZTBL
          WHERE ZFBLNO = ZTIDSUS-ZFBLNO.

  MOVE '4' TO ZTBL-ZFBLST.
  UPDATE ZTBL.
  IF SY-SUBRC NE 0. MESSAGE S041. EXIT. ENDIF.

  SELECT SINGLE *
           FROM ZTCIVHD
          WHERE ZFCIVRN = ZTCIVHD-ZFCIVRN.
  MOVE 'R' TO ZTCIVHD-ZFEDICK.
  MOVE 'O' TO ZTCIVHD-ZFDOCST.
  UPDATE ZTCIVHD.
  IF SY-SUBRC NE 0. MESSAGE S722. EXIT. ENDIF.

ENDFORM.                    " P3000_UPDATE_UPLOAD_STATUS
