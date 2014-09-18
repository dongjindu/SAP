************************************************************************
* Program Name      : ZTBM_FKCS02
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.17.
* Specifications By : Bongsoo, Kim
* Development Request No :
* Addl Documentation:
* Description       : CS02 PHANTOM UPDATE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZTBM_FKCS02
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MATNR TYPE MARA-MATNR,
        WERKS TYPE T001W-WERKS,
        STLAN TYPE RC29N-STLAN,
        STLAL TYPE RC29N-STLAL,
        POSNR TYPE RC29P-POSNR,
        IDNRK TYPE RC29P-IDNRK,
        ZSUFF TYPE ZSUFF,
        ZSEQU(04),
        AENNR TYPE RC29N-AENNR,
        BMENG(20),
        BMEIN TYPE RC29K-BMEIN,
        STLST TYPE RC29K-STLST,
        POSTP TYPE RC29P-POSTP,
        MENGE(20),
          ZSTGB(20),
        MEINS TYPE RC29P-MEINS,
        ITSOB TYPE RC29P-ITSOB,
        ZEITM TYPE ZEITM,
        CLPT(01),
        DPID  TYPE RCUKD-KNNAM,
        UPCT(01),
        ZUPGN TYPE ZUPGN,
        ZINFO TYPE ZINFO,
        ZRESULT LIKE SY-MSGTY,
        ZMSG LIKE CFGNL-MSGLIN,
      END   OF IT_EXCL.
*----------------------------------------------------------------------*
* BDC-DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.

DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.

DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT
             'C:\    .txt' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'CS02'.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.
*AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.

START-OF-SELECTION.
  PERFORM UPLOAD_PROCESS.
**    BOM ITEM CREATE

  PERFORM BDC_PROCESS.
*      PERFORM READ_PROCESS.
*      PERFORM DATA_PROCESS.
*      PERFORM WRITE_PROCESS.
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_FILETY' OR 'P_TCODE'.
        SCREEN-INPUT = 0.
    ENDCASE.
    MODIFY SCREEN.
    CLEAR SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
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
      CONCATENATE P_FILE ' DATA UPLOAD SUCCESS!!'
                  INTO L_TEXT.
      WRITE: / L_TEXT.
      SKIP.
    WHEN 2.
      MESSAGE E000 WITH 'FILE OPEN ERROR, FILE NO FOUND!'.
    WHEN 3.
      MESSAGE E000 WITH 'FILE READ ERROR'.
    WHEN OTHERS.
      MESSAGE E000 WITH 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA: L_STLKN TYPE STPO-STLKN.
  REFRESH: IT_MESS, IT_BDC. CLEAR: IT_MESS, IT_BDC.
  LOOP AT IT_EXCL.
    CLEAR L_STLKN.
    SELECT SINGLE B~STLKN
         FROM MAST AS A INNER JOIN STPO AS B
                        ON    B~STLTY EQ 'M'
                        AND   A~STLNR EQ B~STLNR
         INTO   L_STLKN
         WHERE A~MATNR EQ IT_EXCL-MATNR
         AND   A~WERKS EQ IT_EXCL-WERKS
         AND   A~STLAN EQ IT_EXCL-STLAN
         AND   A~STLAL EQ IT_EXCL-STLAL
         AND   B~POSNR EQ IT_EXCL-POSNR
         AND   B~IDNRK EQ IT_EXCL-IDNRK
         AND   B~SUFF  EQ IT_EXCL-ZSUFF.
*         AND   B~SEQU  EQ L_ZSEQU.
    IF SY-SUBRC EQ 0.
      PERFORM DYNPRO USING:
         'X' 'SAPLCSDI'    '0100',
         ' ' 'RC29N-MATNR' IT_EXCL-MATNR,    "NEXT MATERIAL
         ' ' 'RC29N-WERKS' IT_EXCL-WERKS,    "PLANT
         ' ' 'RC29N-STLAN' IT_EXCL-STLAN,    "BOM usage
         ' ' 'RC29N-STLAL' IT_EXCL-STLAL,    "ALT BOM
         ' ' 'RC29N-AENNR' IT_EXCL-AENNR,    "Change number
         ' ' 'BDC_OKCODE'  '=FCPU',

         'X' 'SAPLCSDI'    '0150',
         ' ' 'BDC_OKCODE'  '=SETP',

         'X' 'SAPLCSDI'    '0708',
         ' ' 'RC29P-SELPI' L_STLKN,    "
         ' ' 'BDC_OKCODE'  '=CLWI',

         'X' 'SAPLCSDI'    '0150',
         ' ' 'RC29P-AUSKZ(01)' 'X',    "
         ' ' 'BDC_OKCODE'  '=PALL',

         'X' 'SAPLCSDI'    '2130',
         ' ' 'RC29P-ITSOB' IT_EXCL-ITSOB,    "
         ' ' 'BDC_OKCODE'  '=FCBU'.
*     CALL TRANSACTION
      CALL TRANSACTION 'CS02'  USING IT_BDC
                               OPTIONS FROM WA_OPT
                               MESSAGES INTO IT_MESS.
    ENDIF.
    WRITE: / IT_EXCL-MATNR, IT_EXCL-IDNRK.
    LOOP AT IT_MESS.
     WRITE:/ IT_MESS.
    ENDLOOP.
    REFRESH: IT_MESS, IT_BDC. CLEAR: IT_MESS, IT_BDC.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.
