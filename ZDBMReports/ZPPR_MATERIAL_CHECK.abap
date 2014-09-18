REPORT YCHECK_EXCEL_MARC
                NO STANDARD PAGE HEADING
                LINE-SIZE  100
                LINE-COUNT 65
                MESSAGE-ID ZMBM.

TABLES: MARC,
        MARA.
 DATA: BEGIN OF IT_EXCL OCCURS 0,
         MATNR(18),
         WERKS(4),
       END   OF IT_EXCL.
 DATA: BEGIN OF IT_MARC OCCURS 0,
         MATNR LIKE MARC-MATNR,
         WERKS LIKE MARC-WERKS,
       END   OF IT_MARC.
 DATA: BEGIN OF IT_CHEC OCCURS 0,
         MATNR LIKE MARC-MATNR,
         WERKS LIKE MARC-WERKS,
         CHECK,
         MESSA(50),
       END   OF IT_CHEC.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT' MODIF ID GR1.
*  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM01'.
SELECTION-SCREEN END   OF BLOCK B1.
AT SELECTION-SCREEN OUTPUT.
  PERFORM SELECTION_SCREEN_OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.
START-OF-SELECTION.
  PERFORM UPLOAD_PROCESS.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM WRITE_PROCESS.
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  TMP_MASK = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
*           MASK             = ',*.*,*.*.'
            MASK             = TMP_MASK
            MODE             = MODE
*           TITLE            = ' '
       IMPORTING
            FILENAME         = TMP_FILENAME
*         RC               =
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM UPLOAD_PROCESS.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            CODEPAGE                = ' '
            FILENAME                = P_FILE
            FILETYPE                = P_FILETY
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            DATA_TAB                = IT_EXCL
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
           OTHERS                  = 10
            .
  CASE SY-SUBRC.
    WHEN 0.
      DATA L_TEXT(132).
      CONCATENATE P_FILE TEXT-001
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000 WITH TEXT-002.
    WHEN 3.
      MESSAGE E000 WITH TEXT-003.
    WHEN OTHERS.
      MESSAGE E000 WITH TEXT-004.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PROCESS.
  DATA: L_MATNR LIKE MARC-MATNR,
        L_WERKS LIKE MARC-WERKS.
  LOOP AT IT_EXCL.
    SELECT SINGLE MATNR
           WERKS
         FROM MARC
         INTO (L_MATNR, L_WERKS)
         WHERE MATNR EQ IT_EXCL-MATNR
         AND   WERKS EQ IT_EXCL-WERKS.
   IF SY-SUBRC EQ 0.
     IT_MARC-MATNR = L_MATNR.
     IT_MARC-WERKS = L_WERKS.
     APPEND IT_MARC.
   ENDIF.
  CLEAR: L_MATNR, L_WERKS, IT_MARC, IT_EXCL.
  ENDLOOP.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  SORT IT_EXCL BY MATNR WERKS.
  SORT IT_MARC BY MATNR WERKS.
  LOOP AT IT_EXCL.
    READ TABLE IT_MARC WITH KEY MATNR = IT_EXCL-MATNR
                                WERKS = IT_EXCL-WERKS
                        BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING IT_EXCL TO IT_CHEC.
      IT_CHEC-CHECK = ' '.
      APPEND IT_CHEC.
    ELSE.
      MOVE-CORRESPONDING IT_EXCL TO IT_CHEC.
      IT_CHEC-CHECK = 'X'.
      IT_CHEC-MESSA = 'MARC NO DATA'.
      APPEND IT_CHEC.
    ENDIF.
  ENDLOOP.
  LOOP AT IT_MARC.
    READ TABLE IT_MARC WITH KEY MATNR = IT_MARC-MATNR
                                WERKS = IT_MARC-WERKS
                        BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE-CORRESPONDING IT_EXCL TO IT_CHEC.
      IT_CHEC-CHECK = ' '.
      COLLECT IT_CHEC.
    ELSE.
      MOVE-CORRESPONDING IT_EXCL TO IT_CHEC.
      IT_CHEC-CHECK = 'X'.
      IT_CHEC-MESSA = 'EXCEL NO DATA'.
      COLLECT IT_CHEC.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  SORT IT_CHEC BY CHECK MATNR WERKS.
    WRITE: /(18) 'MATERIAL',
            (10) 'PLANT',
            (10) 'CHECK',
            (50) 'MESSAGE'.
  FORMAT COLOR 6.
  WRITE: / 'MARC NO DATA LIST'.
  LOOP AT IT_CHEC WHERE CHECK EQ 'X'.
    WRITE: /(18) IT_CHEC-MATNR,
            (10) IT_CHEC-WERKS,
            (10) IT_CHEC-CHECK,
            (50) IT_CHEC-MESSA.
  ENDLOOP.
  FORMAT COLOR 5.
  WRITE: / 'MARC DATA LIST'.
  LOOP AT IT_CHEC WHERE CHECK EQ ' '.
    WRITE: /(18) IT_CHEC-MATNR,
            (10) IT_CHEC-WERKS,
            (10) IT_CHEC-CHECK,
            (50) IT_CHEC-MESSA.
  ENDLOOP.

ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN_OUTPUT.
  LOOP AT SCREEN.
   IF SCREEN-GROUP1 = 'GR1'.
      CHECK SCREEN-NAME = 'P_FILETY'.
      SCREEN-INPUT = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " SELECTION_SCREEN_OUTPUT
