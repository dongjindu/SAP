************************************************************************
* Program Name      : ZAPP919R_UPLOAD_ZTPP_WOSUM
* Author            : Bobby
* Creation Date     : 2004.01.11.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K905549
* Addl Documentation:
* Description       : 'Upload the data for the table ZTPP_WOSUM
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZAPP919R_UPLOAD_ZTPP_WOSUM NO STANDARD PAGE HEADING
                                  LINE-SIZE  1023  LINE-COUNT 65
                                  MESSAGE-ID zmpp.

*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTPP_WOSUM    .

*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AEHD OCCURS 0,
        WO_SER(09)       TYPE C             ,
        NATION(03)       TYPE C             ,
        DEALER(02)       TYPE C             ,
        EXTC(03)         TYPE C             ,
        INTC(02)         TYPE C             ,
        INITQTY(5)       TYPE N             ,
        MODQTY(05)       TYPE N             ,
        SEQQTY(05)       TYPE N             ,
        PLANQTY(5)       TYPE N             ,
        FORECASTQTY(5)   TYPE N             ,
        MITUQTY(5)       TYPE N             ,
        WOCREDATE(10)    TYPE C             ,
        WOMODDATE(10)    TYPE C             ,
        FSC(18)          TYPE C             ,
        VERSION(03)      TYPE C             ,
        SALES(10)        TYPE C             ,
        RP01TQ(05)       TYPE N             ,
        RP02TQ(05)       TYPE N             ,
        RP03TQ(05)       TYPE N             ,
        RP04TQ(05)       TYPE N             ,
        RP05TQ(05)       TYPE N             ,
        RP06TQ(05)       TYPE N             ,
        RP07TQ(05)       TYPE N             ,
        RP08TQ(05)       TYPE N             ,
        RP09TQ(05)       TYPE N             ,
        RP10TQ(05)       TYPE N             ,
        RP11TQ(05)       TYPE N             ,
        RP12TQ(05)       TYPE N             ,
        RP13TQ(05)       TYPE N             ,
        RP14TQ(05)       TYPE N             ,
        RP15TQ(05)       TYPE N             ,
        RP16TQ(05)       TYPE N             ,
        RP01DQ(05)       TYPE N             ,
        RP02DQ(05)       TYPE N             ,
        RP03DQ(05)       TYPE N             ,
        RP04DQ(05)       TYPE N             ,
        RP05DQ(05)       TYPE N             ,
        RP06DQ(05)       TYPE N             ,
        RP07DQ(05)       TYPE N             ,
        RP08DQ(05)       TYPE N             ,
        RP09DQ(05)       TYPE N             ,
        RP10DQ(05)       TYPE N             ,
        RP11DQ(05)       TYPE N             ,
        RP12DQ(05)       TYPE N             ,
        RP13DQ(05)       TYPE N             ,
        RP14DQ(05)       TYPE N             ,
        RP15DQ(05)       TYPE N             ,
        RP16DQ(05)       TYPE N             ,
        T01PQ(005)       TYPE N             ,
        T06PQ(005)       TYPE N             ,
        T08PQ(005)       TYPE N             ,
        T12PQ(005)       TYPE N             ,
        T17PQ(005)       TYPE N             ,
        T20PQ(005)       TYPE N             ,
        T01DQ(005)       TYPE N             ,
        T06DQ(005)       TYPE N             ,
        T08DQ(005)       TYPE N             ,
        T12DQ(005)       TYPE N             ,
        T17DQ(005)       TYPE N             ,
        T20DQ(005)       TYPE N             ,
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
  PERFORM UPDATE_ZTPP_WOSUM    .

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
*&      Form  UPDATE_ZTPP_WOSUM
*&---------------------------------------------------------------------*
FORM UPDATE_ZTPP_WOSUM    .
  DATA LT_AEHD LIKE ZTPP_WOSUM     OCCURS 0 WITH HEADER LINE.
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
      DELETE FROM ZTPP_WOSUM
             CLIENT SPECIFIED
             WHERE MANDT EQ SY-MANDT.
      INSERT ZTPP_WOSUM     FROM TABLE LT_AEHD ACCEPTING DUPLICATE KEYS.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
*-> Upload except deletion
    WHEN R2.
      MODIFY ZTPP_WOSUM     FROM TABLE LT_AEHD.
      IF SY-SUBRC EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
  ENDCASE.
ENDFORM.                    " UPDATE_ZTPP_WOSUM
