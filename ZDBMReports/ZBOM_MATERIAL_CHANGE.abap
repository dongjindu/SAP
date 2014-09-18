************************************************************************
* Program Name      : ZBOM_MATERIAL_CHANGE
* Author            : Bongsoo, Kim
* Creation Date     : 2003.10.08.
* Specifications By : Bongsoo, Kim
* Development Request No :
* Addl Documentation:
* Description       :  MATERIAL CHANGE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZBOM_MATERIAL_DELETE_MARKING
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        WERKS TYPE MARC-WERKS,
        MATNR TYPE MARA-MATNR,
        MTVFP type marc-MTVFP,
*        LGPRO type marc-LGPRO,
*        VSPVB TYPE MARC-VSPVB,
*        DISPO TYPE MARC-DISPO,
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
DATA: BEGIN OF IT_MARA OCCURS 0,
        WERKS TYPE MARC-WERKS,
        MATNR TYPE MARA-MATNR,
        MTART TYPE MARA-MTART,
*        LGPRO type marc-LGPRO,
      END OF IT_MARA.

DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
* EXCEL DATA UPLOAD
*PARAMETERS: P_MTART LIKE MARA-MTART.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM02'.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.

START-OF-SELECTION.
  REFRESH IT_EXCL. CLEAR IT_EXCL.
  PERFORM UPLOAD_PROCESS.
  PERFORM BDC_PROCESS.
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
  LOOP AT IT_EXCL.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_EXCL-MATNR,    "NEXT MATERIAL
*       ' ' 'RM03G-WERKS' SPACE,    "PLANT
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',    "
       ' ' 'BDC_OKCODE'  '=ENTR',
*       MRP 2
*       'X' 'SAPLMGMM'    '4004',
*       ' ' 'BDC_OKCODE'  '=SP13',
*      MRP 3
       'X' 'SAPLMGMM'    '4004',
       ' ' 'BDC_OKCODE'  '=SP14',

       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_EXCL-WERKS,    "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '4000',
       ' ' 'MARC-MTVFP'  IT_EXCL-MTVFP,
*       ' ' 'MARC-LGPRO'  IT_EXCL-LGPRO,
*       ' ' 'MARC-VSPVB'  IT_EXCL-VSPVB,
*       ' ' 'MARC-DISPO' IT_EXCL-DISPO,    "
       ' ' 'BDC_OKCODE'  '=BU'.
*       CALL TRANSACTION
    CALL TRANSACTION 'MM02'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    REFRESH: IT_BDC, IT_MESS.
    CLEAR: IT_BDC, IT_MESS.
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
