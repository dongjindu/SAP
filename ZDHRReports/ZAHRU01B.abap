REPORT ZAHRU01B NO STANDARD PAGE HEADING LINE-SIZE 80 LINE-COUNT 65
                                                     MESSAGE-ID ZG.

*******************************************************
* 5/10/2012        t-code is deleted by APM Monitoring
******************************************************
TABLES: ZTHRBADGE.

****************************** constants *******************************
CONSTANTS:  FALSE VALUE ' ',
            TRUE  VALUE 'X'.

DATA: BEGIN OF ITAB OCCURS 0,
          BADGE(20),
          FIRSTNAME(40),
          LASTNAME(40),
          EMPLOYEENUMBER(20),
          DESCRIPTION(40),
          DEPARTMENT(40),
      END OF ITAB.

DATA: BEGIN OF IZTHRBADGE_ERR OCCURS 0.
        INCLUDE STRUCTURE ZTHRBADGE.
DATA: END   OF IZTHRBADGE_ERR.

DATA: CNT_INSERT TYPE I VALUE 0,
      CNT_ERROR  TYPE I VALUE 0.

PARAMETERS PDELETE AS CHECKBOX.
PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY
                    DEFAULT 'c:\temp\import.xls'
                    MODIF ID EXL.
PARAMETERS P_HEAD AS CHECKBOX MODIF ID EXL DEFAULT 'X'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM BROWSER CHANGING P_FILE.

START-OF-SELECTION.

  PERFORM UPLOAD_FILE USING P_FILE.

*  PERFORM UPLOADING.
  PERFORM INSERTING.

*&---------------------------------------------------------------------*
*&      Form  UPLOADING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPLOADING.

*  DATA CANCEL.
*  CALL FUNCTION 'UPLOAD'
*       EXPORTING
*            FILENAME            = 'C:\temp\'
*            FILETYPE            = 'ASC'
*       IMPORTING
*            CANCEL              = CANCEL
*       TABLES
*            DATA_TAB            = ITAB
*       EXCEPTIONS
*            CONVERSION_ERRO     = 1
*            INVALID_TABLE_WIDTH = 2
*            INVALID_TYPE        = 3.
*
*  IF CANCEL EQ 'x'.
*    LEAVE PROGRAM.
*  ENDIF.

  DELETE ITAB WHERE BADGE EQ SPACE.
ENDFORM.                    " UPLOADING

*&---------------------------------------------------------------------*
*&      Form  INSERTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERTING.
  IF PDELETE EQ 'X'.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'   " Progress bar
         EXPORTING TEXT  = 'Deleting data from BADGE master...'.
    DELETE FROM ZTHRBADGE  CLIENT SPECIFIED WHERE MANDT = SY-MANDT.
    COMMIT WORK.
  ENDIF.

  REFRESH IZTHRBADGE_ERR.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'   " Progress bar
       EXPORTING TEXT  = 'Inserting data into ZTHRBADGE...'.

  LOOP AT ITAB.
    CLEAR ZTHRBADGE.
    MOVE-CORRESPONDING ITAB TO ZTHRBADGE.

    INSERT ZTHRBADGE.
    IF SY-SUBRC NE 0.
      MOVE-CORRESPONDING ZTHRBADGE TO IZTHRBADGE_ERR.
      APPEND IZTHRBADGE_ERR .
      WRITE: / 'Already exist :', SY-TABIX,ITAB-BADGE.
      CNT_ERROR = CNT_ERROR + 1.
    ELSE.
      CNT_INSERT = CNT_INSERT + 1.
    ENDIF.
  ENDLOOP.

  WRITE: / CNT_INSERT, ' inserted',
         / CNT_ERROR,  ' error occured'.

ENDFORM.                    " INSERTING

*---------------------------------------------------------------------*
*       FORM browser                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FILENAME                                                      *
*---------------------------------------------------------------------*
FORM BROWSER CHANGING FILENAME.
  DATA: IT_TFILE TYPE FILETABLE ,
        GD_SUBRC TYPE I.

  CALL  METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
        EXPORTING
          WINDOW_TITLE = 'Select File Name'
          DEFAULT_EXTENSION = '*.*'
          DEFAULT_FILENAME = '*.*'
          FILE_FILTER = '*.*'
          INITIAL_DIRECTORY = 'c:\temp\'
*         MULTISELECTION =
*         WITH_ENCODING =
        CHANGING
          FILE_TABLE = IT_TFILE
          RC = GD_SUBRC.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE IT_TFILE INTO FILENAME INDEX 1.
  ENDIF.

ENDFORM.                    " BROWSER


*---------------------------------------------------------------------*
*       FORM upload_file                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FILENAME                                                      *
*---------------------------------------------------------------------*
FORM UPLOAD_FILE USING FILENAME.

  IF P_FILE EQ SPACE.
    EXIT.
  ENDIF.

  DATA: IT_ITAB LIKE STANDARD TABLE OF ALSMEX_TABLINE WITH HEADER LINE.
  FIELD-SYMBOLS : <FS>.
  DATA : V_INDEX TYPE I.
  DATA : BEGIN_ROW TYPE I VALUE 1.

  IF P_HEAD = TRUE.
    ADD 1 TO BEGIN_ROW.
  ENDIF.

*  IF P_TXT NE TRUE.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = FILENAME
            I_BEGIN_COL             = 1
            I_BEGIN_ROW             = BEGIN_ROW
            I_END_COL               = 4
            I_END_ROW               = 65535
       TABLES
            INTERN                  = IT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.

  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'Could not find the file.'.
    STOP.
  ENDIF.

  IF IT_ITAB[] IS INITIAL.
    MESSAGE S003(ZZ) WITH 'No Data was uploaded'.
    EXIT.
  ELSE.
    SORT IT_ITAB BY ROW COL.
    LOOP AT IT_ITAB.
      MOVE : IT_ITAB-COL TO V_INDEX.
      ASSIGN COMPONENT V_INDEX OF STRUCTURE ITAB TO <FS>.
      MOVE : IT_ITAB-VALUE TO <FS>.
      AT END OF ROW.
        APPEND ITAB.
      ENDAT.
    ENDLOOP.
  ENDIF.

*  ELSE.
*    DATA CANCEL.
*    CALL FUNCTION 'UPLOAD'
*         EXPORTING
*              FILENAME            = FILENAME
*              FILETYPE            = 'DAT'
*         IMPORTING
*              CANCEL              = CANCEL
*         TABLES
*              DATA_TAB            = ITAB
*         EXCEPTIONS
*              CONVERSION_ERRO     = 1
*              INVALID_TABLE_WIDTH = 2
*              INVALID_TYPE        = 3.
*
*    IF NOT CANCEL IS INITIAL OR SY-SUBRC NE 0.
*      MESSAGE S003(ZZ) WITH 'No Data was uploaded'.
*      STOP.
*    ENDIF.
*
*  ENDIF.
*
ENDFORM.                    " upload_file
