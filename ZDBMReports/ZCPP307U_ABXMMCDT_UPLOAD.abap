************************************************************************
* Program Name      : ZCPP307U_ABXMMCDT_UPLOAD
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.22.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K902045
* Addl Documentation:
* Description       : MATERIAL MASTER TABLE Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZCPP307U_ABXMMCDT_UPLOAD
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXEHDDT.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_MMCD OCCURS 0,
        MANDT(03),  " TYPE ZTBM_ABXMMCDT-MANDT,
        MTNO(18),  " TYPE ZTBM_ABXMMCDT-MTNO,
        PLNT(04),  " TYPE ZTBM_ABXMMCDT-PLNT,
        MTYP(04),  " TYPE ZTBM_ABXMMCDT-MTYP,
        INDU(01),  " TYPE ZTBM_ABXMMCDT-INDU,
        ZDESC(40),  " TYPE ZTBM_ABXMMCDT-ZDESC,
        UNIT(03),  " TYPE ZTBM_ABXMMCDT-UNIT,
        MGRP(09),  " TYPE ZTBM_ABXMMCDT-MGRP,
        GICA(04),  " TYPE ZTBM_ABXMMCDT-GICA,
        SOUR(03),  " TYPE ZTBM_ABXMMCDT-SOUR,
        MTCN(01),  " TYPE ZTBM_ABXMMCDT-MTCN,
        MRPY(02),  " TYPE ZTBM_ABXMMCDT-MRPY,
        MRPC(03),  " TYPE ZTBM_ABXMMCDT-MRPC,
        LOTS(02),  " TYPE ZTBM_ABXMMCDT-LOTS,
        PRTY(01),  " TYPE ZTBM_ABXMMCDT-PRTY,
        SMKY(03),  " TYPE ZTBM_ABXMMCDT-SMKY,
        AVCK(02),  " TYPE ZTBM_ABXMMCDT-AVCK,
        CNMT(18),  " TYPE ZTBM_ABXMMCDT-CNMT,
        SLMD(01),  " TYPE ZTBM_ABXMMCDT-SLMD,
        INCO(01),  " TYPE ZTBM_ABXMMCDT-INCO,
        VESN(01),  " TYPE ZTBM_ABXMMCDT-VESN,
        ZUSER(12),  " TYPE ZTBM_ABXMMCDT-ZUSER,
        ZSDAT(08),  " TYPE ZTBM_ABXMMCDT-ZSDAT,
        ZSTIM(06),  " TYPE ZTBM_ABXMMCDT-ZSTIM,
        ZEDAT(08),  " TYPE ZTBM_ABXMMCDT-ZEDAT,
        ZETIM(06),  " TYPE ZTBM_ABXMMCDT-ZETIM,
      END OF IT_MMCD.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT'.
*  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM01'.
SELECTION-SCREEN END   OF BLOCK B1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
  PERFORM UPLOAD_PROCESS.
  PERFORM UPDATE_ZTBM_ABXMMCDT.
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
            DATA_TAB                = IT_MMCD
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
*&      Form  UPDATE_ZTBM_ABXMMCDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXMMCDT.
  DATA: LT_MMCD TYPE ZTBM_ABXMMCDT OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_MMCD.
    MOVE-CORRESPONDING IT_MMCD TO LT_MMCD.
    APPEND LT_MMCD.
    CLEAR: IT_MMCD, LT_MMCD.
  ENDLOOP.
  INSERT ZTBM_ABXMMCDT FROM TABLE LT_MMCD ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.

   ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXMMCDT
