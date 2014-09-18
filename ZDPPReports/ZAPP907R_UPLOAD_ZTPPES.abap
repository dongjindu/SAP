************************************************************************
* Program Name      : ZAPP906R_UPLOAD_ZTPPES
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Upload the data for the table ZTPPES
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP906R_UPLOAD_ZTPPES     NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTPPES        .

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AEHD OCCURS 0,
        EN_ITEM(18)     TYPE C             ,
        EN_VEH_MODEL(4) TYPE C             ,
        EN_HEAD(4)      TYPE C             ,
        EN_SPC01(5)     TYPE C             ,
        EN_SPC02(5)     TYPE C             ,
        EN_SPC03(5)     TYPE C             ,
        EN_SPC04(5)     TYPE C             ,
        EN_SPC05(5)     TYPE C             ,
        EN_SPC06(5)     TYPE C             ,
        EN_SPC07(5)     TYPE C             ,
        EN_SPC08(5)     TYPE C             ,
        EN_SPC09(5)     TYPE C             ,
        EN_SPC10(5)     TYPE C             ,
        EN_SPC11(5)     TYPE C             ,
        EN_SPC12(5)     TYPE C             ,
        EN_SPC13(5)     TYPE C             ,
        EN_SPC14(5)     TYPE C             ,
        EN_SPC15(5)     TYPE C             ,
        EN_SPC16(5)     TYPE C             ,
        EN_SPC17(5)     TYPE C             ,
        EN_SPC18(5)     TYPE C             ,
        EN_SPC19(5)     TYPE C             ,
        EN_SPC20(5)     TYPE C             ,
        EN_SPC21(5)     TYPE C             ,
        EN_SPC22(5)     TYPE C             ,
        EN_SPC23(5)     TYPE C             ,
        EN_SPC24(5)     TYPE C             ,
        EN_SPC25(5)     TYPE C             ,
        EN_SPC26(5)     TYPE C             ,
        EN_SPC27(5)     TYPE C             ,
        EN_SPC28(5)     TYPE C             ,
        EN_SPC29(5)     TYPE C             ,
        EN_SPC30(5)     TYPE C             ,
        EN_SPC31(5)     TYPE C             ,
        EN_SPC32(5)     TYPE C             ,
        EN_SPC33(5)     TYPE C             ,
        EN_SPC34(5)     TYPE C             ,
        EN_SPC35(5)     TYPE C             ,
        EN_SPC36(5)     TYPE C             ,
        EN_SPC37(5)     TYPE C             ,
        EN_SPC38(5)     TYPE C             ,
        EN_SPC39(5)     TYPE C             ,
        EN_SPC40(5)     TYPE C             ,
        EN_SPC41(5)     TYPE C             ,
        EN_SPC42(5)     TYPE C             ,
        EN_SPC43(5)     TYPE C             ,
        EN_SPC44(5)     TYPE C             ,
        EN_SPC45(5)     TYPE C             ,
        EN_SPC46(5)     TYPE C             ,
        EN_SPC47(5)     TYPE C             ,
        EN_SPC48(5)     TYPE C             ,
        EN_SPC49(5)     TYPE C             ,
        EN_SPC50(5)     TYPE C             ,
        EN_SPC51(5)     TYPE C             ,
        EN_SPC52(5)     TYPE C             ,
        EN_SPC53(5)     TYPE C             ,
        EN_SPC54(5)     TYPE C             ,
        EN_SPC55(5)     TYPE C             ,
        EN_SPC56(5)     TYPE C             ,
        EN_SPC57(5)     TYPE C             ,
        EN_SPC58(5)     TYPE C             ,
        EN_SPC59(5)     TYPE C             ,
        EN_SPC60(5)     TYPE C             ,
        EFLAG(02)       TYPE C             ,
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
  PERFORM UPDATE_ZTPPES        .

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
*&      Form  UPDATE_ZTPPES
*&---------------------------------------------------------------------*
FORM UPDATE_ZTPPES        .
  DATA LT_AEHD LIKE ZTPPES         OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_AEHD.
    MOVE-CORRESPONDING IT_AEHD TO LT_AEHD.
    LT_AEHD-ZSDAT = SY-DATUM.
    LT_AEHD-ZSTIM = SY-UZEIT.
    LT_AEHD-ZUSER = SY-UNAME.
    APPEND LT_AEHD.
    CLEAR: IT_AEHD, LT_AEHD.
  ENDLOOP.

  CASE C_MARK.
*-> Upload after all deletion
    WHEN R1.
      DELETE FROM ZTPPES
             CLIENT SPECIFIED
             WHERE MANDT EQ SY-MANDT.
      INSERT ZTPPES         FROM TABLE LT_AEHD ACCEPTING DUPLICATE KEYS.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
*-> Upload except deletion
    WHEN R2.
      MODIFY ZTPPES         FROM TABLE LT_AEHD.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ENDCASE.
ENDFORM.                    " UPDATE_ZTPPES
