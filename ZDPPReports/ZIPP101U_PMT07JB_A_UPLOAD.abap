************************************************************************
* Program Name      : ZIPP101U_PMT07JB_A_UPLOAD
* Author            : Jong Oh, Kim
* Creation Date     : 2003.10.01.
* Specifications By : Jong Oh, Kim
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'ZTPP_PMT07JB_A' TABLE UPLOAD
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP101U_PMT07JB_A_UPLOAD
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMPP.
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTPP_PMT07JB_A.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*

DATA: BEGIN OF IT_AEHD OCCURS 0,
        SQDT(8), "TYPE ZTPP_PMT07JB_A-SQDT,
        PLNT TYPE ZTPP_PMT07JB_A-PLNT,
        LINE TYPE ZTPP_PMT07JB_A-LINE,
        MODL TYPE ZTPP_PMT07JB_A-MODL,
        MTGU TYPE ZTPP_PMT07JB_A-MTGU,
        SSR1(4), "TYPE ZTPP_PMT07JB_A-SSR1,
        SSR2(4), "TYPE ZTPP_PMT07JB_A-SSR2,
        SQCD TYPE ZTPP_PMT07JB_A-SQCD,
        VHNO TYPE ZTPP_PMT07JB_A-VHNO,
        ORDR TYPE ZTPP_PMT07JB_A-ORDR,
        DIST TYPE ZTPP_PMT07JB_A-DIST,
        EXTC TYPE ZTPP_PMT07JB_A-EXTC,
        INTC TYPE ZTPP_PMT07JB_A-INTC,
        BMDL TYPE ZTPP_PMT07JB_A-BMDL,
        OCNN TYPE ZTPP_PMT07JB_A-OCNN,
        VERS TYPE ZTPP_PMT07JB_A-VERS,
        EVL1 TYPE ZTPP_PMT07JB_A-EVL1,
        EVL2 TYPE ZTPP_PMT07JB_A-EVL2,
        EVL3 TYPE ZTPP_PMT07JB_A-EVL3,
        EVL4 TYPE ZTPP_PMT07JB_A-EVL4,
        EVL5 TYPE ZTPP_PMT07JB_A-EVL5,
        PQTY(5), "TYPE ZTPP_PMT07JB_A-PQTY,
        GUBB TYPE ZTPP_PMT07JB_A-GUBB,
        CDAT(8), "TYPE ZTPP_PMT07JB_A-CDAT,
        CTIM(6), "TYPE ZTPP_PMT07JB_A-CTIM,
        MOYE TYPE ZTPP_PMT07JB_A-MOYE,
        PVER TYPE ZTPP_PMT07JB_A-PVER,
        PLNUM TYPE ZTPP_PMT07JB_A-PLNUM,
        VINN TYPE ZTPP_PMT07JB_A-VINN,
*        ERDAT TYPE ZTPP_PMT07JB_A-ERDAT,
*        ERZET TYPE ZTPP_PMT07JB_A-ERZET,
*        ERNAM TYPE ZTPP_PMT07JB_A-ERNAM,
*        AEDAT TYPE ZTPP_PMT07JB_A-AEDAT,
*        AEZET TYPE ZTPP_PMT07JB_A-AEZET,
*        AENAM TYPE ZTPP_PMT07JB_A-AENAM,
      END OF IT_AEHD.

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
  PERFORM UPDATE_ZTPP_PMT07JB_A.

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
*&      Form  UPDATE_ZTPP_PMT07JB_A
*&---------------------------------------------------------------------*
FORM UPDATE_ZTPP_PMT07JB_A.
  DATA LT_AEHD LIKE ZTPP_PMT07JB_A OCCURS 0 WITH HEADER LINE.
  LOOP AT IT_AEHD.
    MOVE-CORRESPONDING IT_AEHD TO LT_AEHD.
    LT_AEHD-ERDAT = SY-DATUM.
    LT_AEHD-ERZET = SY-UZEIT.
    LT_AEHD-ERNAM = SY-UNAME.
    APPEND LT_AEHD.
    CLEAR: IT_AEHD, LT_AEHD.
  ENDLOOP.
  CASE C_MARK.
*-> Upload after all deletion
    WHEN R1.
      DELETE FROM ZTPP_PMT07JB_A
             CLIENT SPECIFIED
             WHERE MANDT EQ SY-MANDT.
      INSERT ZTPP_PMT07JB_A FROM TABLE LT_AEHD ACCEPTING DUPLICATE KEYS.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
*-> Upload except deletion
    WHEN R2.
      MODIFY ZTPP_PMT07JB_A FROM TABLE LT_AEHD.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.

  ENDCASE.


ENDFORM.                    " UPDATE_ZTPP_PMT07JB_A
