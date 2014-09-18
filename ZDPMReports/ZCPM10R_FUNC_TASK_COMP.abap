************************************************************************
* Created by  : Myoungho Park
* Created on  : 2003.09.26.
* Description : Assign Component to Task List (Functional Location)
*
*
* Modification Log
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZCPM10R_FUNC_TASK_COMP             .

FIELD-SYMBOLS : <FS>.

*** For File Select
DATA: WA_FNAME LIKE RLGRAP-FILENAME,
      WA_RC LIKE SY-SUBRC.

DATA: BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDC_TAB.

DATA: BEGIN OF IT_TREC OCCURS 0,
        TPLNR(30),  "//Functional Location
        PLNAL(2),   "//Group counter
        VORNR(4),   "//Operation Number
        IDNRK(18),  "//Material number
        MENGE(18),  "//Component quantity
      END OF IT_TREC.

DATA: WA_COUNT TYPE I,
      WA_LINE_COUNT(2) TYPE N VALUE 1,
      WA_PREV_TPLNR(30),
      WA_PREV_PLNAL(2),
      WA_PREV_VORNR(4).

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
PARAMETER: P_FILE  LIKE RLGRAP-FILENAME MODIF ID BCD.
SELECTION-SCREEN PUSHBUTTON /68(10) PH_FNAME USER-COMMAND CHG.
PARAMETER: P_GROUP LIKE APQI-GROUPID DEFAULT SY-UNAME  OBLIGATORY.
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
      PERFORM FILENAME_GET.

    WHEN 'ONLI'.
      CLEAR: SY-UCOMM.
      PERFORM MAIN_PROCESS.

    WHEN OTHERS.
      CLEAR: SY-UCOMM.

  ENDCASE.
******** START-OF-SELECTION *****************
*********************************************
START-OF-SELECTION.
*********************************************


******** END-OF-SELECTION *******************
*********************************************
END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  FILENAME_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILENAME_GET.
*** Call file selector
  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            MASK             = ',*.XLS,*.XLS.'
            MODE             = 'O'
            TITLE            = 'Select Upload File'
       IMPORTING
            FILENAME         = P_FILE
            RC               = WA_RC
       EXCEPTIONS
            INV_WINSYS       = 1
            NO_BATCH         = 2
            SELECTION_CANCEL = 3
            SELECTION_ERROR  = 4
            OTHERS           = 5.
ENDFORM.                    " FILENAME_GET
*&---------------------------------------------------------------------*
*&      Form  MAIN_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAIN_PROCESS.
  CLEAR: IT_TREC,   IT_TREC[].

  IF P_FILE IS INITIAL.
    PERFORM FILENAME_GET.
  ENDIF.

  PERFORM EXCEL_FILE_UPLOAD  TABLES   IT_TREC
                             USING    P_FILE
                                      WA_RC.
  PERFORM BDC_PROCESS.

  MESSAGE S000(ZMPM) WITH WA_COUNT 'count uploading success.'.
  CALL TRANSACTION 'SM35'.
ENDFORM.                    " MAIN_PROCESS
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

*** Data transfer from PC files to Internal Table.
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

* mapping Excel cells to Interanl Table fields
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
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_PROCESS.
* BDC OPEN GROUP FUNCTION
  PERFORM CALL_BDC_OPEN_GROUP.

  IF SY-SUBRC EQ 0.
*   BDC DATA ?? & INSERT ????
    PERFORM BDC_GENERATE_PROCESS.
*   BDC CLOSE GROUP FUNCTION
    PERFORM CALL_BDC_CLOSE_GROUP.
  ELSE.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_BDC_OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = SY-MANDT
            GROUP               = P_GROUP
            KEEP                = ''
            USER                = SY-UNAME
       EXCEPTIONS
            RUNNING             = 1
            QUEUE_ERROR         = 2
            CLIENT_INVALID      = 3
            GROUP_INVALID       = 4
            USER_INVALID        = 5
            HOLDDATE_INVALID    = 6
            DESTINATION_INVALID = 7.
ENDFORM.                    " CALL_BDC_OPEN_GROUP
*&---------------------------------------------------------------------*
*&      Form  BDC_GENERATE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BDC_GENERATE_PROCESS.
  LOOP AT IT_TREC.
    PERFORM GENERATE_BDC_DATA.
    CLEAR IT_TREC.
  ENDLOOP.

  PERFORM DYNPRO USING:
           'X' 'SAPLCMDI'    '3500',
           ' ' 'BDC_OKCODE'  '=BU'.
  PERFORM CALL_BDC_INSERT.

ENDFORM.                    " BDC_GENERATE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_INSERT
*&---------------------------------------------------------------------*
FORM CALL_BDC_INSERT.
  CALL FUNCTION 'BDC_INSERT'
      EXPORTING
           TCODE            =  'IA12'
*           POST_LOCAL       = NOVBLOCAL
*           PRINTING         = NOPRINT
       TABLES
            DYNPROTAB        =  BDC_TAB
      EXCEPTIONS
           INTERNAL_ERROR   = 1
           NOT_OPEN         = 2
           QUEUE_ERROR      = 3
           TCODE_INVALID    = 4
           PRINTING_INVALID = 5
           POSTING_INVALID  = 6
           OTHERS           = 7
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " CALL_BDC_INSERT
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
FORM CALL_BDC_CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
*      EXCEPTIONS
*           NOT_OPEN    = 1
*           QUEUE_ERROR = 2
*           OTHERS      = 3
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                               " CALL_BDC_CLOSE_GROUP
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE: NAME TO BDC_TAB-PROGRAM,
          VALUE TO BDC_TAB-DYNPRO,
          DYNBEGIN TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE: NAME TO BDC_TAB-FNAM,
          VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM GENERATE_BDC_DATA                                        *
*---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA.

  IF IT_TREC-TPLNR NE WA_PREV_TPLNR OR
     IT_TREC-PLNAL NE WA_PREV_PLNAL.
    WA_COUNT = WA_COUNT + 1.

    IF  WA_COUNT NE 1.
      PERFORM DYNPRO USING:
           'X' 'SAPLCMDI'	'3500',
           ' ' 'BDC_OKCODE'  '=BU'.
      PERFORM CALL_BDC_INSERT.

    ENDIF.

    REFRESH BDC_TAB.
    PERFORM DYNPRO USING:
           'X' 'SAPLCPDI'    '3005',
           ' ' 'RC27E-TPLNR' IT_TREC-TPLNR,    "//Func. Loc
           ' ' 'RC271-PLNAL' IT_TREC-PLNAL,
           ' ' 'BDC_OKCODE'  '/00',

           'X' 'SAPLCPDI'   '3400',
           ' ' 'BDC_OKCODE'  '=MAAL',

           'X' 'SAPLCPDI'   '3400',
           ' ' 'BDC_OKCODE' '=MAPM',

           'X' 'SAPLCMDI'   '3500',
           ' ' 'BDC_OKCODE' '/00',
           ' ' 'RIHSTPX-IDNRK(01)' IT_TREC-IDNRK,
           ' ' 'RIHSTPX-MENGE(01)' IT_TREC-MENGE,

           'X' 'SAPLCMDI'   '3500',
           ' ' 'BDC_OKCODE'  '=P+'.

  ELSEIF IT_TREC-TPLNR EQ WA_PREV_TPLNR AND
         IT_TREC-PLNAL EQ WA_PREV_PLNAL.

    IF IT_TREC-VORNR NE WA_PREV_VORNR.
      PERFORM DYNPRO USING:
              'X' 'SAPLCMDI'    '3500',
              ' ' 'BDC_OKCODE'  '=NEPM',

              'X' 'SAPLCMDI'   '3500',
              ' ' 'BDC_OKCODE' '/00',
              ' ' 'RIHSTPX-IDNRK(01)' IT_TREC-IDNRK,
              ' ' 'RIHSTPX-MENGE(01)' IT_TREC-MENGE,

              'X' 'SAPLCMDI'   '3500',
              ' ' 'BDC_OKCODE'  '=P+'.
    ELSE.
      PERFORM DYNPRO USING:
              'X' 'SAPLCMDI'   '3500',
              ' ' 'BDC_OKCODE' '/00',
              ' ' 'RIHSTPX-IDNRK(02)' IT_TREC-IDNRK,
              ' ' 'RIHSTPX-MENGE(02)' IT_TREC-MENGE.
    ENDIF.
  ENDIF.

  MOVE : IT_TREC-TPLNR TO WA_PREV_TPLNR,
         IT_TREC-PLNAL TO WA_PREV_PLNAL,
         IT_TREC-VORNR TO WA_PREV_VORNR.

ENDFORM.
