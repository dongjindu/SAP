************************************************************************
* Program Name      : ZCPP308U_ABXEBMDT_UPLOAD
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.01.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K901990
* Addl Documentation:
* Description       : ECM BOM STRUCTURE TABLE Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZCPP308U_ABXEBMDT_UPLOAD
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
DATA: BEGIN OF IT_BMDT OCCURS 0,
        MANDT(03),  " TYPE ZTBM_ABXEBMDT-MANDT,
        MTNO(18),  " TYPE ZTBM_ABXEBMDT-MTNO,
        PLNT(04),  " TYPE ZTBM_ABXEBMDT-PLNT,
        USAG(01),  " TYPE ZTBM_ABXEBMDT-USAG,
        ALTN(02),  " TYPE ZTBM_ABXEBMDT-ALTN,
        PREF(04),  " TYPE ZTBM_ABXEBMDT-PREF,
        COMP(18),  " TYPE ZTBM_ABXEBMDT-COMP,
        SUFF(04),  " TYPE ZTBM_ABXEBMDT-SUFF,
        SEQC(04),  " TYPE ZTBM_ABXEBMDT-SEQC,
        SEQU(04),  " TYPE ZTBM_ABXEBMDT-SEQU,
        EONO(12),  " TYPE ZTBM_ABXEBMDT-EONO,
        BQTY(20),  " TYPE ZTBM_ABXEBMDT-BQTY,
        HUNT(03),  " TYPE ZTBM_ABXEBMDT-HUNT,
        STAT(01),  " TYPE ZTBM_ABXEBMDT-STAT,
        ITCA(01),  " TYPE ZTBM_ABXEBMDT-ITCA,
        QNTY(20),  " TYPE ZTBM_ABXEBMDT-QNTY,
        STGB(01),  " TYPE ZTBM_ABXEBMDT-STGB,
        UNIT(03),  " TYPE ZTBM_ABXEBMDT-UNIT,
        SPPR(02),  " TYPE ZTBM_ABXEBMDT-SPPR,
        EITM(01),  " TYPE ZTBM_ABXEBMDT-EITM,
        CLPT(01),  " TYPE ZTBM_ABXEBMDT-CLPT,
        DPID(30),  " TYPE ZTBM_ABXEBMDT-DPID,
        UPCT(01),  " TYPE ZTBM_ABXEBMDT-UPCT,
        UPGN(18),  " TYPE ZTBM_ABXEBMDT-UPGN,
        ZUSER(12),  " TYPE ZTBM_ABXEBMDT-ZUSER,
        ZSDAT(08),  " TYPE ZTBM_ABXEBMDT-ZSDAT,
        ZSTIM(06),  " TYPE ZTBM_ABXEBMDT-ZSTIM,
        ZEDAT(08),  " TYPE ZTBM_ABXEBMDT-ZEDAT,
        ZETIM(06),  " TYPE ZTBM_ABXEBMDT-ZETIM,
*        ZBDAT(08),
*        ZBTIM(06),
*        ZBNAM(12),
*        ZMODE(01),
*        ZRESULT(01),
*        ZMSG(220),
      END OF IT_BMDT.

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
  DATA L_CHK.
  PERFORM UPLOAD_PROCESS.
  PERFORM DATA_PROCESS CHANGING L_CHK.
  IF L_CHK EQ ' '.
    PERFORM UPDATE_ZTBM_ABXEBMDT.
  ELSE.
    LOOP AT IT_BMDT.

    ENDLOOP.
  ENDIF.

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
            DATA_TAB                = IT_BMDT
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
*&      Form  UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
FORM UPDATE_ZTBM_ABXEBMDT.
  DATA: LT_BMDT TYPE ZTBM_ABXEBMDT OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_BMDT.
    MOVE-CORRESPONDING IT_BMDT TO LT_BMDT.
    APPEND LT_BMDT.
    CLEAR: IT_BMDT, LT_BMDT.
  ENDLOOP.
  INSERT ZTBM_ABXEBMDT FROM TABLE LT_BMDT ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
    WRITE: / TEXT-005 COLOR 4.
  ELSE.

    ROLLBACK WORK.
    WRITE: / TEXT-006 COLOR 4.
  ENDIF.
ENDFORM.                    " UPDATE_ZTBM_ABXEBMDT
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS CHANGING P_CHK.
  DATA: L_TABIX LIKE SY-TABIX,
       L_LINE TYPE SY-INDEX,
       M_LINE TYPE SY-INDEX.
  DATA: BEGIN OF LT_BMDT OCCURS 0,
          MTNO(18),  " TYPE ZTBM_ABXEBMDT-MTNO,
          PLNT(04),  " TYPE ZTBM_ABXEBMDT-PLNT,
          USAG(01),  " TYPE ZTBM_ABXEBMDT-USAG,
          ALTN(02),  " TYPE ZTBM_ABXEBMDT-ALTN,
          PREF(04),  " TYPE ZTBM_ABXEBMDT-PREF,
          COMP(18),  " TYPE ZTBM_ABXEBMDT-COMP,
          SUFF(04),  " TYPE ZTBM_ABXEBMDT-SUFF,
          SEQU(04),  " TYPE ZTBM_ABXEBMDT-SEQU,
          SEQC(01),  " TYPE ZTBM_ABXEBMDT-SEQC,
          END OF LT_BMDT.
  LOOP AT IT_BMDT.
    MOVE-CORRESPONDING IT_BMDT TO LT_BMDT.
    COLLECT LT_BMDT.
    CLEAR: IT_BMDT, LT_BMDT.
  ENDLOOP.
  DESCRIBE TABLE LT_BMDT LINES L_LINE.
  DESCRIBE TABLE IT_BMDT LINES M_LINE.
  IF L_LINE EQ M_LINE.

  ELSE.
    CLEAR L_LINE.
    SORT IT_BMDT BY MTNO PLNT USAG ALTN PREF COMP SUFF SEQU SEQC.
    DELETE ADJACENT DUPLICATES FROM IT_BMDT
                               COMPARING MTNO PLNT USAG ALTN
                                         PREF COMP SUFF SEQU SEQC.




*    LOOP AT LT_BMDT.
*      LOOP AT IT_BMDT WHERE MTNO EQ LT_BMDT-MTNO
*                      AND   PLNT EQ LT_BMDT-PLNT
*                      AND   USAG EQ LT_BMDT-USAG
*                      AND   ALTN EQ LT_BMDT-ALTN
*                      AND   PREF EQ LT_BMDT-PREF
*                      AND   COMP EQ LT_BMDT-COMP
*                      AND   SUFF EQ LT_BMDT-SUFF
*                      AND   SEQU EQ LT_BMDT-SEQU
*                      AND   SEQC EQ LT_BMDT-SEQC.
*        L_TABIX = SY-TABIX.
*        L_LINE = L_LINE + 1.
*        IF L_LINE GE '2'.
*          WRITE: / IT_BMDT-MTNO,
*                   IT_BMDT-PLNT,
*                   IT_BMDT-USAG,
*                   IT_BMDT-ALTN,
*                   IT_BMDT-PREF,
*                   IT_BMDT-COMP,
*                   IT_BMDT-SUFF,
*                   IT_BMDT-SEQU,
*                   IT_BMDT-SEQC.
*          DELETE IT_BMDT INDEX L_TABIX.
*        ENDIF.
*      ENDLOOP.
*      CLEAR L_LINE.
*    ENDLOOP.
  ENDIF.

*  LOOP AT LT_BMDT WHERE ZMSG GE '2'.
*    WRITE: / LT_BMDT-MTNO,
*             LT_BMDT-PLNT,
*             LT_BMDT-USAG,
*             LT_BMDT-ALTN,
*             LT_BMDT-PREF,
*             LT_BMDT-COMP,
*             LT_BMDT-SUFF,
*             LT_BMDT-SEQU,
*             LT_BMDT-SEQC.
*  ENDLOOP.
ENDFORM.                    " DATA_PROCESS
