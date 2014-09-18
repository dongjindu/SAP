************************************************************************
* Program Name      : ZCPM03R_BOM
* Author            : Myoungho, Park
* Creation Date     : 2003.10.28.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :   This program Uploading for PM BOM
*
* Modification Logs
* Date       Developer        RequestNo     Description
*
************************************************************************



REPORT ZCPM03R_BOM .

FIELD-SYMBOLS : <FS>.

*** For File Select
DATA: WA_FNAME LIKE RLGRAP-FILENAME,
      WA_RC LIKE SY-SUBRC.

DATA:BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA:END OF BDC_TAB.

DATA:BEGIN OF IT_TREC OCCURS 0,
        MATNR     LIKE RC29N-MATNR,       "//Material number
        WERKS     LIKE RC29N-WERKS,       "//Plant
        STLAN     LIKE RC29N-STLAN,       "//BOM usage

        POSTP     LIKE RC29P-POSTP,       "//Item category
        IDNRK     LIKE RC29P-IDNRK,       "//BOM component
        MENGE(18),                        "//Component quantity

END OF IT_TREC.

DATA: WA_MATNR  LIKE RMCLF-MATNR,    "//temporary variable
      WA_CNT TYPE I.                 "//Count variable


*DATA: WA_FP_NAME LIKE RLGRAP-FILENAME,
*      WA_FTYPE LIKE RLGRAP-FILETYPE VALUE 'DAT'.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_FILE  LIKE RLGRAP-FILENAME MODIF ID BCD.
SELECTION-SCREEN PUSHBUTTON /70(8) PH_FNAME USER-COMMAND CHG.
SELECTION-SCREEN END OF BLOCK BLOCK1.

******************* INITIALIZATION ********************************
*******************************************************************
INITIALIZATION.
  MOVE 'Find' TO PH_FNAME.

***************** AT SELECTION-SCREEN ******************************
********************************************************************
AT SELECTION-SCREEN.
  CASE SY-UCOMM.
    WHEN 'CHG'.
      CLEAR: SY-UCOMM.
      PERFORM GET_FILE_LOCATION.

    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
      PERFORM MAIN_PROCESS.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  MAIN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIN_PROCESS.
  WA_CNT = 0.

  PERFORM UPLOAD_MASTER_TABLE.
  PERFORM BDC_OPEN_GROUP.
  PERFORM PROCESS_BDC_TABLE.

  PERFORM DYNPRO USING:  ' ' 'BDC_OKCODE' '/11'. "//Save

  PERFORM CALL_BDC_INSERT.
  PERFORM BDC_CLOSE_GROUP.

ENDFORM.                    " MAIN_PROCESS

*&----------------------------------------------------------------*
*&      Form  UPLOAD_MASTER_TABLE
*&----------------------------------------------------------------*
FORM UPLOAD_MASTER_TABLE.
  IF P_FILE IS INITIAL.
    PERFORM GET_FILE_LOCATION.
  ENDIF.
  PERFORM EXCEL_FILE_UPLOAD  TABLES   IT_TREC
                             USING    WA_FNAME
                                      WA_RC.

ENDFORM.                    " UPLOAD_MASTER_TABLE
*&----------------------------------------------------------------*
*&      Form  BDC_OPEN_GROUP
*&----------------------------------------------------------------*
FORM BDC_OPEN_GROUP.
* BDC OPEN GROUP
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT = SY-MANDT
            GROUP  = PH_FNAME
            KEEP   = 'X'
            USER   = SY-UNAME.

ENDFORM.                    " BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*&      Form  PROCESS_BDC_TABLE
*&---------------------------------------------------------------------*
FORM PROCESS_BDC_TABLE.
  LOOP AT IT_TREC.
    WA_CNT = WA_CNT + 1.

    PERFORM GENERATE_BDC_DATA.

  ENDLOOP.
ENDFORM.                    " PROCESS_BDC_TABLE
*&-----------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&-----------------------------------------------------------------*
FORM GENERATE_BDC_DATA.

  IF WA_MATNR <> IT_TREC-MATNR.

    IF WA_CNT <> 1.
      PERFORM DYNPRO USING:
         ' ' 'BDC_OKCODE' '/11'.       "//Save

      CALL FUNCTION 'BDC_INSERT'
           EXPORTING
                TCODE     = 'CS01'
           TABLES
                DYNPROTAB = BDC_TAB.
      WA_CNT = 1.
    ENDIF.

    REFRESH BDC_TAB.

    PERFORM DYNPRO USING:
      'X' 'SAPLCSDI'    '0100',
      ' ' 'RC29N-MATNR'  IT_TREC-MATNR,
      ' ' 'RC29N-WERKS'  IT_TREC-WERKS,
      ' ' 'RC29N-STLAN'  IT_TREC-STLAN,
      ' ' 'BDC_OKCODE' '/00',

      'X' 'SAPLCSDI'    '0110',
      ' ' 'BDC_OKCODE' '/00',

      'X' 'SAPLCSDI'    '0111',
      ' ' 'BDC_OKCODE' '/00'.

*---
    PERFORM DYNPRO USING:
    'X' 'SAPLCSDI'    '0140',
    ' ' 'RC29P-AUSKZ(01)'  'X',  "
    ' ' 'RC29P-IDNRK(01)'  IT_TREC-IDNRK,  "
    ' ' 'RC29P-MENGE(01)'  IT_TREC-MENGE,  "
    ' ' 'RC29P-POSTP(01)'  IT_TREC-POSTP,  "
    ' ' 'BDC_OKCODE' '/00',

    'X' 'SAPLCSDI'    '0130',
    ' ' 'BDC_OKCODE' '/00',

    'X' 'SAPLCSDI'    '0131',
    ' ' 'BDC_OKCODE' '/00'.
*---
    PERFORM DYNPRO USING:
      'X' 'SAPLCSDI'    '0140',
      ' ' 'BDC_OKCODE' '=FCNP'.

    WA_MATNR = IT_TREC-MATNR.

  ELSE.
*---
    PERFORM DYNPRO USING:
     'X' 'SAPLCSDI'    '0140',
     ' ' 'RC29P-AUSKZ(02)'  'X',  "
     ' ' 'RC29P-IDNRK(02)'  IT_TREC-IDNRK,  "
     ' ' 'RC29P-MENGE(02)'  IT_TREC-MENGE,  "
     ' ' 'RC29P-POSTP(02)'  IT_TREC-POSTP,  "
     ' ' 'BDC_OKCODE' '/00',

     'X' 'SAPLCSDI'    '0130',
     ' ' 'BDC_OKCODE' '/00',

     'X' 'SAPLCSDI'    '0131',
     ' ' 'BDC_OKCODE' '/00'.

*---
    PERFORM DYNPRO USING:
      'X' 'SAPLCSDI'    '0140',
      ' ' 'BDC_OKCODE' '=FCNP'.
  ENDIF.

ENDFORM.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------*
*&      Form  CALL_BDC_INSERT
*&---------------------------------------------------------------*
FORM CALL_BDC_INSERT.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE     = 'CS01'
       TABLES
            DYNPROTAB = BDC_TAB.
ENDFORM.                    " CALL_BDC_INSERT
*&---------------------------------------------------------------------*
*&      Form  BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
FORM BDC_CLOSE_GROUP.
*- BDC CLOSE GROUP

  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
FORM DYNPRO            USING    DYNBEGIN   P_NAME   VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE: P_NAME     TO BDC_TAB-PROGRAM,
          VALUE    TO BDC_TAB-DYNPRO,
          DYNBEGIN TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE: P_NAME    TO BDC_TAB-FNAM,
          VALUE   TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.

ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_LOCATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FILE_LOCATION.
  DATA: WA_RC LIKE SY-SUBRC.

*** Call file selector
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.xls,*.xls.'
            MODE             = 'O'
            TITLE            = 'Open'
       IMPORTING
            FILENAME         = P_FILE
            RC               = WA_RC
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPM) WITH TEXT-M01.
  ENDIF.
ENDFORM.                    " GET_FILE_LOCATION
*&---------------------------------------------------------------------*
*&      Form  EXCEL_FILE_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TREC  text
*      -->P_WA_FNAME  text
*      -->P_WA_RC  text
*----------------------------------------------------------------------*
FORM EXCEL_FILE_UPLOAD TABLES   IT_TREC
                       USING    P_FNAME
                                P_RC.

  DATA : IT_INTERN TYPE  KCDE_CELLS OCCURS 0 WITH HEADER LINE.
  DATA : LV_INDEX TYPE I.
  DATA : LV_START_COL TYPE I VALUE '1',
         LV_START_ROW TYPE I VALUE '1',
         LV_END_COL   TYPE I VALUE '256',
         LV_END_ROW   TYPE I VALUE '65536'.

*** Data transfer from PC files to internal Table.
  CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
       EXPORTING
            FILENAME                = P_FNAME
            I_BEGIN_COL             = LV_START_COL
            I_BEGIN_ROW             = LV_START_ROW
            I_END_COL               = LV_END_COL
            I_END_ROW               = LV_END_ROW
       TABLES
            INTERN                  = IT_INTERN
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2.

* mapping cells to fields
  MOVE : SY-SUBRC TO P_RC.
  CHECK NOT IT_INTERN[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE IT_INTERN WHERE ROW LE 2.

  SORT IT_INTERN BY ROW COL.

  LOOP AT IT_INTERN.
    MOVE : IT_INTERN-COL TO LV_INDEX.
    ASSIGN COMPONENT LV_INDEX OF STRUCTURE IT_TREC TO <FS>.
    MOVE : IT_INTERN-VALUE TO <FS>.
    AT END OF ROW.
      APPEND IT_TREC.
      CLEAR IT_TREC.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " EXCEL_FILE_UPLOAD
