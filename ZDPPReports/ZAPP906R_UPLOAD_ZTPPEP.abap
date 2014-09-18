************************************************************************
* Program Name      : ZAPP906R_UPLOAD_ZTPPEP
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Upload the data for the table ZTPPEP
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP906R_UPLOAD_ZTPPEP     NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTPPEP        .

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AEHD OCCURS 0,
        PDATE(10) TYPE C                   ,
        PLNUM(10) TYPE C                   ,
        PITEM(18) TYPE C                   ,
        GSMNG(13) TYPE C                   ,
        MEINS(03) TYPE C                   ,
        PLGRP     TYPE C                   ,
        EFLAG(02) TYPE C                   ,
*        FLAG(001) TYPE C                   ,
      END OF IT_AEHD.

*----------------------------------------------------------------------*
* Working Variables AREA
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Constants AREA
*----------------------------------------------------------------------*
CONSTANTS: C_MARK   VALUE 'X'.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT'.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (15) TEXT-100.
*-> Upload after all deletion
PARAMETERS: R1 RADIOBUTTON GROUP RA.
SELECTION-SCREEN COMMENT  (25) TEXT-101 FOR FIELD R1.
*-> Upload except deletion
PARAMETERS: R2 RADIOBUTTON GROUP RA.
SELECTION-SCREEN COMMENT  (25) TEXT-102 FOR FIELD R2.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END   OF BLOCK B1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
  PERFORM UPLOAD_PROCESS.
  PERFORM UPDATE_ZTPPEP        .

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING DEF_PATH LIKE RLGRAP-FILENAME
                                          MODE     TYPE C.

  DATA: TMP_FILENAME LIKE RLGRAP-FILENAME.
  DATA: TMP_MASK(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: FIELDLN TYPE I.
  FIELD-SYMBOLS: <TMP_SYM>.

  TMP_MASK = ',*.*,*.*.'.

  FIELDLN = STRLEN( DEF_PATH ) - 1.
  ASSIGN DEF_PATH+FIELDLN(1) TO <TMP_SYM>.
  IF <TMP_SYM> = '/' OR <TMP_SYM> = '\'.
    CLEAR <TMP_SYM>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            DEF_FILENAME     = P_FILE
            DEF_PATH         = DEF_PATH
            MASK             = TMP_MASK
            MODE             = MODE
       IMPORTING
            FILENAME         = TMP_FILENAME
       EXCEPTIONS
            INV_WINSYS       = 01
            NO_BATCH         = 02
            SELECTION_CANCEL = 03
            SELECTION_ERROR  = 04.

  IF SY-SUBRC = 0.
    P_FILE = TMP_FILENAME.
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
       TABLES
            DATA_TAB                = IT_AEHD
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
*&      Form  UPDATE_ZTPPEP
*&---------------------------------------------------------------------*
FORM UPDATE_ZTPPEP        .
  DATA LT_AEHD LIKE ZTPPEP         OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_AEHD.
    MOVE-CORRESPONDING IT_AEHD TO LT_AEHD.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        INPUT         = IT_AEHD-PLNUM
      IMPORTING
        OUTPUT        = LT_AEHD-PLNUM .

    LT_AEHD-ZSDAT = SY-DATUM.
    LT_AEHD-ZSTIM = SY-UZEIT.
    LT_AEHD-ZUSER = SY-UNAME.
    APPEND LT_AEHD.
    CLEAR: IT_AEHD, LT_AEHD.
  ENDLOOP.

  CASE C_MARK.
*-> Upload after all deletion
    WHEN R1.
      DELETE FROM ZTPPEP
             CLIENT SPECIFIED
             WHERE MANDT EQ SY-MANDT.
      INSERT ZTPPEP         FROM TABLE LT_AEHD ACCEPTING DUPLICATE KEYS.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
*-> Upload except deletion
    WHEN R2.
      MODIFY ZTPPEP         FROM TABLE LT_AEHD.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ENDCASE.
ENDFORM.                    " UPDATE_ZTPPEP
