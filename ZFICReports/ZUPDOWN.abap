REPORT ZUPDOWN
        NO STANDARD PAGE HEADING
        LINE-SIZE  255.
*----------------------------------------------------------------------
* Declare Database Objects
*----------------------------------------------------------------------
 tables:
   DOKIL,
   TRDIR.
*----------------------------------------------------------------------
* Constants
 CONSTANTS:
   MC_TRDIR_IDENTIFIER(72)  TYPE C VALUE '%&%& RDIR',
   MC_REPORT_IDENTIFIER(72) TYPE C VALUE '%&%& REPO',
   MC_TEXT_IDENTIFIER(72)   TYPE C VALUE '%&%& TEXP',
   MC_THEAD_IDENTIFIER(72)  TYPE C VALUE '%&%& HEAD',
   MC_DOC_IDENTIFIER(72)    TYPE C VALUE '%&%& DOKL',

   MC_TRDIR_SHORT(4)        TYPE C VALUE 'RDIR',
   MC_REPORT_SHORT(4)       TYPE C VALUE 'REPO',
   MC_TEXT_SHORT(4)         TYPE C VALUE 'TEXP',
   MC_THEAD_SHORT(4)        TYPE C VALUE 'HEAD',
   MC_DOC_SHORT(4)          TYPE C VALUE 'DOKP'.

*----------------------------------------------------------------------
*----------------------------------------------------------------------
* Declare Module level data structures
*----------------------------------------------------------------------
 DATA: BEGIN OF MTAB_PROGRAM_SOURCE OCCURS 0,
         LINE(72) TYPE C,
       END OF MTAB_PROGRAM_SOURCE.

 DATA: MTAB_PROGRAM_TRDIR LIKE TRDIR OCCURS 0 WITH HEADER LINE.

 DATA: MTAB_PROGRAM_TEXTS LIKE TEXTPOOL OCCURS 0 WITH HEADER LINE.

 DATA: MSTR_THEAD LIKE THEAD.

 DATA: BEGIN OF MTAB_PROGRAM_FILE OCCURS 0,
         LINE(275) TYPE C,
       END OF MTAB_PROGRAM_FILE.

 DATA: BEGIN OF MTAB_DIRECTORY OCCURS 0,
         NAME LIKE TRDIR-NAME,
         DESC(72) TYPE C,
         SAVENAME LIKE RLGRAP-FILENAME,
       END OF MTAB_DIRECTORY.

 DATA: BEGIN OF MTAB_PROGRAM_DOCUMENTATION OCCURS 0,
         LINE(255) TYPE C,
       END OF MTAB_PROGRAM_DOCUMENTATION.

*----------------------------------------------------------------------
* Selection Screen
*----------------------------------------------------------------------

*-- Options for upload/download of programs
 SELECTION-SCREEN BEGIN OF BLOCK FRM_OPTIONS WITH FRAME TITLE TEXT-UDL.
 PARAMETERS:
   RB_DOWN RADIOBUTTON GROUP UDL DEFAULT 'X'.       " Download reports
 SELECTION-SCREEN BEGIN OF BLOCK FRM_TRDIR WITH FRAME TITLE TEXT-DIR.
 SELECT-OPTIONS:
   S_NAME  FOR TRDIR-NAME,              " Program Name
   S_SUBC  FOR TRDIR-SUBC               " Program Type
           DEFAULT 'F' OPTION EQ SIGN E," Exclude Functions by default
   S_CNAM  FOR TRDIR-CNAM               " Created by
           DEFAULT SY-UNAME,
   S_UNAM  FOR TRDIR-UNAM,              " Last Changed by
   S_CDAT  FOR TRDIR-CDAT,              " Creation date
   S_UDAT  FOR TRDIR-UDAT.              " Last update date
 SELECTION-SCREEN END OF BLOCK FRM_TRDIR.
*-- Options for uploading programs
 PARAMETERS:
   RB_UP   RADIOBUTTON GROUP UDL.       " Upload reports
 SELECTION-SCREEN BEGIN OF BLOCK FRM_UPLOAD WITH FRAME TITLE TEXT-UPL.
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 1(29) TEXT-SNG.
 PARAMETERS:
   RB_FILE RADIOBUTTON GROUP HOW DEFAULT 'X'.
 SELECTION-SCREEN COMMENT 33(42) TEXT-FNA.
 SELECTION-SCREEN END OF LINE.
 PARAMETERS:
   RB_LIST RADIOBUTTON GROUP HOW.
 SELECTION-SCREEN END OF BLOCK FRM_UPLOAD.
 SELECTION-SCREEN END OF BLOCK FRM_OPTIONS.

*-- Options for up/downloading programs
 SELECTION-SCREEN BEGIN OF BLOCK FRM_FILEN WITH FRAME TITLE TEXT-FIL.
 PARAMETERS:
   RB_DOS  RADIOBUTTON GROUP FIL DEFAULT 'X', " Save to local
   RB_UNIX RADIOBUTTON GROUP FIL,       " Save to UNIX
   P_PATH  LIKE RLGRAP-FILENAME         " Path to save files to
         DEFAULT 'c:\temp\src\'.
 SELECTION-SCREEN END OF BLOCK FRM_FILEN.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_NAME-LOW.
   CALL FUNCTION 'F4_PROGRAM'
        EXPORTING
             OBJECT             = S_NAME-LOW
             SUPPRESS_SELECTION = 'X'
        IMPORTING
             RESULT             = S_NAME-LOW
        EXCEPTIONS
             OTHERS             = 1.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_NAME-HIGH.
   CALL FUNCTION 'F4_PROGRAM'
        EXPORTING
             OBJECT             = S_NAME-HIGH
             SUPPRESS_SELECTION = 'X'
        IMPORTING
             RESULT             = S_NAME-HIGH
        EXCEPTIONS
             OTHERS             = 1.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_UNAM-LOW.
   PERFORM GET_NAME USING 'S_UNAM-LOW'
                 CHANGING S_UNAM-LOW.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_UNAM-HIGH.
   PERFORM GET_NAME USING 'S_UNAM-HIGH'
                 CHANGING S_UNAM-HIGH.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_CNAM-LOW.
   PERFORM GET_NAME USING 'S_CNAM-LOW'
                 CHANGING S_CNAM-LOW.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_CNAM-HIGH.
   PERFORM GET_NAME USING 'S_CNAM-HIGH'
                 CHANGING S_CNAM-HIGH.

 TOP-OF-PAGE.
   IF RB_LIST = 'X'.
     FORMAT COLOR COL_HEADING.

     NEW-LINE.
     WRITE: AT 3 TEXT-H01,
            AT 15 TEXT-H03.
     FORMAT COLOR OFF.
   ENDIF.

 AT LINE-SELECTION.
   CHECK RB_LIST = 'X'.                 " only do in list mode
   READ LINE SY-CUROW FIELD VALUE MTAB_DIRECTORY-SAVENAME.

*-- Read file into an internal table
   PERFORM READ_REPORT_FROM_DISK TABLES MTAB_PROGRAM_FILE
                                 USING  MTAB_DIRECTORY-SAVENAME.
*-- Split table into TADIR entry, report lines, and report text
   PERFORM SPLIT_INCOMING_FILE TABLES MTAB_PROGRAM_FILE
                                      MTAB_PROGRAM_SOURCE
                                      MTAB_PROGRAM_TEXTS
                                      MTAB_PROGRAM_DOCUMENTATION
                             CHANGING TRDIR
                                      MSTR_THEAD.
*-- Save all of the data
   PERFORM INSERT_NEW_REPORT TABLES MTAB_PROGRAM_SOURCE
                                    MTAB_PROGRAM_TEXTS
                                    MTAB_PROGRAM_DOCUMENTATION
                             USING  TRDIR
                                    MSTR_THEAD.

*----------------------------------------------------------------------
* Start of processing
*----------------------------------------------------------------------
 START-OF-SELECTION.
   FORMAT COLOR COL_NORMAL.

   IF RB_DOWN = 'X'.
     PERFORM DOWNLOAD_REPORTS.
   ELSEIF RB_UP = 'X'.
     PERFORM UPLOAD_REPORTS.
   ENDIF.

 END-OF-SELECTION.

   IF RB_DOWN = 'X'.
     CONCATENATE P_PATH
                 'directory.txt'
       INTO P_PATH.
     PERFORM SAVE_TABLE_TO_FILE TABLES MTAB_DIRECTORY
                                USING  P_PATH.
   ENDIF.

*---------------------------------------------------------------------*
*       FORM UPLOAD_REPORTS                                           *
*---------------------------------------------------------------------*
 FORM UPLOAD_REPORTS.

*-- Can upload a reports entered in selection criteria or
*-- select from a list.  List can be from index.txt in same directory
*-- (created by the download) or by reading the first line of each file
*-- in the directory.

   IF RB_FILE = 'X'. " Upload single program from a file
*-- Read file into an internal table
     PERFORM READ_REPORT_FROM_DISK TABLES MTAB_PROGRAM_FILE
                                   USING  P_PATH.
*-- Split table into TADIR entry, report lines, and report text
     PERFORM SPLIT_INCOMING_FILE TABLES MTAB_PROGRAM_FILE
                                        MTAB_PROGRAM_SOURCE
                                        MTAB_PROGRAM_TEXTS
                                        MTAB_PROGRAM_DOCUMENTATION
                               CHANGING TRDIR
                                        MSTR_THEAD.
*-- Save all of the data
     PERFORM INSERT_NEW_REPORT TABLES MTAB_PROGRAM_SOURCE
                                      MTAB_PROGRAM_TEXTS
                                      MTAB_PROGRAM_DOCUMENTATION
                               USING  TRDIR
                                      MSTR_THEAD.

   ELSEIF RB_LIST = 'X'. " Show list for user to choose from
*-- get list of report names/descriptions from directory text
     CONCATENATE P_PATH
                 'directory.txt'
     INTO P_PATH.

     PERFORM READ_REPORT_FROM_DISK TABLES MTAB_DIRECTORY
                                   USING  P_PATH.

     SORT MTAB_DIRECTORY.

*-- Write out list of report names/descriptions
     LOOP AT MTAB_DIRECTORY.
       WRITE:
         / MTAB_DIRECTORY-NAME UNDER TEXT-H01,
           MTAB_DIRECTORY-DESC UNDER TEXT-H03,
           MTAB_DIRECTORY-SAVENAME.

     ENDLOOP.
*-- Process user selections for reports to upload.
   ENDIF.

 ENDFORM.                               " upload_reports
*---------------------------------------------------------------------*
*       FORM DOWNLOAD_REPORTS                                         *
*---------------------------------------------------------------------*
*       From the user selections, get all programs that meet the      *
*       criteria, and save them in ftab_program_directory.            *
*       Also save the report to disk.                                 *
*---------------------------------------------------------------------*
 FORM DOWNLOAD_REPORTS.

   DATA:
     LC_FULL_FILENAME LIKE RLGRAP-FILENAME.

*-- The table is put into an internal table because the program will
*-- abend if multiple transfers to a dataset occur within a SELECT/
*-- ENDSELCT (tested on 3.1H)

   SELECT * FROM  TRDIR
          INTO TABLE MTAB_PROGRAM_TRDIR
          WHERE  NAME  IN S_NAME
          AND    SUBC  IN S_SUBC
          AND    CNAM  IN S_CNAM
          AND    UNAM  IN S_UNAM
          AND    CDAT  IN S_CDAT
          AND    UDAT  IN S_UDAT.

   LOOP AT MTAB_PROGRAM_TRDIR.

*-- Clear out text and source code tables
     CLEAR:
       MTAB_PROGRAM_FILE,
       MTAB_PROGRAM_SOURCE,
       MTAB_PROGRAM_TEXTS,
       MTAB_PROGRAM_DOCUMENTATION.

     REFRESH:
       MTAB_PROGRAM_FILE,
       MTAB_PROGRAM_SOURCE,
       MTAB_PROGRAM_TEXTS,
       MTAB_PROGRAM_DOCUMENTATION.

*-- Get the report
     READ REPORT MTAB_PROGRAM_TRDIR-NAME INTO MTAB_PROGRAM_SOURCE.

*-- Get the text for the report
     READ TEXTPOOL MTAB_PROGRAM_TRDIR-NAME INTO MTAB_PROGRAM_TEXTS.

*-- Get the documentation for the report
     CLEAR DOKIL.
     SELECT * UP TO 1 ROWS FROM DOKIL
            WHERE  ID          = 'RE'
            AND    OBJECT      = MTAB_PROGRAM_TRDIR-NAME
            AND    LANGU       = SY-LANGU
            AND    TYP         = 'E'
            ORDER BY VERSION DESCENDING.
     ENDSELECT.
*-- Documentation exists for this object
     IF SY-SUBRC = 0.
       CALL FUNCTION 'DOCU_READ'
            EXPORTING
                 ID      = DOKIL-ID
                 LANGU   = DOKIL-LANGU
                 OBJECT  = DOKIL-OBJECT
                 TYP     = DOKIL-TYP
                 VERSION = DOKIL-VERSION
            IMPORTING
                 HEAD    = MSTR_THEAD
            TABLES
                 LINE    = MTAB_PROGRAM_DOCUMENTATION
            EXCEPTIONS
                 OTHERS  = 1.

     ENDIF.

*-- Put the report code and texts into a single file

*-- Put the identifier line in so that the start of the TRDIR line
*-- is marked
     CONCATENATE MC_TRDIR_IDENTIFIER
     MTAB_PROGRAM_TRDIR-NAME
     INTO MTAB_PROGRAM_FILE-LINE.
     APPEND MTAB_PROGRAM_FILE.

*-- Add the TRDIR line
     MTAB_PROGRAM_FILE-LINE = MTAB_PROGRAM_TRDIR.
     APPEND MTAB_PROGRAM_FILE.

*-- Put the identifier line in so that the start of the report code
*-- is marked
     CONCATENATE MC_REPORT_IDENTIFIER
                 MTAB_PROGRAM_TRDIR-NAME
       INTO MTAB_PROGRAM_FILE-LINE.
     APPEND MTAB_PROGRAM_FILE.

*-- Add the report code
     LOOP AT MTAB_PROGRAM_SOURCE.
       MTAB_PROGRAM_FILE = MTAB_PROGRAM_SOURCE.
       APPEND MTAB_PROGRAM_FILE.
     ENDLOOP.

*-- Put the identifier line in so that the start of the report text
*-- is marked
     CONCATENATE MC_TEXT_IDENTIFIER
                 MTAB_PROGRAM_TRDIR-NAME
       INTO MTAB_PROGRAM_FILE-LINE.
     APPEND MTAB_PROGRAM_FILE.

*-- Add the report texts
     LOOP AT MTAB_PROGRAM_TEXTS.
       MTAB_PROGRAM_FILE = MTAB_PROGRAM_TEXTS.
       APPEND MTAB_PROGRAM_FILE.
     ENDLOOP.

*-- Put the identifier line in so that the start of the THEAD record
*-- is marked
     CONCATENATE MC_THEAD_IDENTIFIER
                 MTAB_PROGRAM_TRDIR-NAME
       INTO MTAB_PROGRAM_FILE-LINE.
     APPEND MTAB_PROGRAM_FILE.

     MTAB_PROGRAM_FILE = MSTR_THEAD.
     APPEND MTAB_PROGRAM_FILE.

*-- Put the identifier line in so that the start of the report
*-- documentation is marked
     CONCATENATE MC_DOC_IDENTIFIER
                 MTAB_PROGRAM_TRDIR-NAME
       INTO MTAB_PROGRAM_FILE-LINE.
     APPEND MTAB_PROGRAM_FILE.

*-- Add the report documentation
     LOOP AT MTAB_PROGRAM_DOCUMENTATION.
       MTAB_PROGRAM_FILE = MTAB_PROGRAM_DOCUMENTATION.
       APPEND MTAB_PROGRAM_FILE.
     ENDLOOP.

*-- Make the fully pathed filename that report will be saved to
     CONCATENATE P_PATH
                 MTAB_PROGRAM_TRDIR-NAME
                 '.txt'
       INTO LC_FULL_FILENAME.

     PERFORM SAVE_TABLE_TO_FILE TABLES MTAB_PROGRAM_FILE
                                USING  LC_FULL_FILENAME.

*-- Write out message with Program Name/Description
     READ TABLE MTAB_PROGRAM_TEXTS WITH KEY ID = 'R'.
     IF SY-SUBRC = 0.
       MTAB_DIRECTORY-NAME = MTAB_PROGRAM_TRDIR-NAME.
       MTAB_DIRECTORY-DESC = MTAB_PROGRAM_TEXTS-ENTRY.
       MTAB_DIRECTORY-SAVENAME = LC_FULL_FILENAME.
       APPEND MTAB_DIRECTORY.

       WRITE: / MTAB_PROGRAM_TRDIR-NAME,
                MTAB_PROGRAM_TEXTS-ENTRY(65) COLOR COL_HEADING.
     ELSE.
       MTAB_DIRECTORY-NAME = MTAB_PROGRAM_TRDIR-NAME.
       MTAB_DIRECTORY-DESC = 'No description available'.
       MTAB_DIRECTORY-SAVENAME = LC_FULL_FILENAME.
       APPEND MTAB_DIRECTORY.

       WRITE: / MTAB_PROGRAM_TRDIR-NAME.
     ENDIF.

   ENDLOOP.
 ENDFORM.                               " BUILD_PROGRAM_DIRECTORY
*---------------------------------------------------------------------*
*       FORM SAVE_TABLE_TO_FILE                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FTAB_TABLE                                                    *
*  -->  F_FILENAME                                                    *
*---------------------------------------------------------------------*
 FORM SAVE_TABLE_TO_FILE TABLES FTAB_TABLE
                         USING  F_FILENAME.

   IF RB_DOS = 'X'.                  " Save file to presentation server
     CALL FUNCTION 'WS_DOWNLOAD'
          EXPORTING
               FILENAME = F_FILENAME
               FILETYPE = 'ASC'
          TABLES
               DATA_TAB = FTAB_TABLE
          EXCEPTIONS
               OTHERS   = 4.

     IF SY-SUBRC NE 0.
       WRITE: / 'Error opening dataset' COLOR COL_NEGATIVE,
                F_FILENAME COLOR COL_NEGATIVE.
     ENDIF.
   ELSE.                                " Save file to application serve

     OPEN DATASET F_FILENAME FOR OUTPUT IN TEXT MODE.
     IF SY-SUBRC = 0.
       LOOP AT FTAB_TABLE.
         TRANSFER FTAB_TABLE TO F_FILENAME.
         IF SY-SUBRC NE 0.
           WRITE: / 'Error writing record to file;' COLOR COL_NEGATIVE,
                    F_FILENAME COLOR COL_NEGATIVE.
         ENDIF.
       ENDLOOP.
     ELSE.
       WRITE: / 'Error opening dataset' COLOR COL_NEGATIVE,
                F_FILENAME COLOR COL_NEGATIVE.
     ENDIF.
   ENDIF.                               " End RB_DOS
 ENDFORM.                               " SAVE_PROGRAM
*---------------------------------------------------------------------*
*       FORM READ_REPORT_FROM_DISK                                    *
*---------------------------------------------------------------------*
*       Read report into internal table.  Can read from local or      *
*       remote computer                                               *
*---------------------------------------------------------------------*
 FORM READ_REPORT_FROM_DISK TABLES FTAB_TABLE
                            USING  F_FILENAME.

   DATA:
      LC_MESSAGE(128) TYPE C.

   CLEAR   FTAB_TABLE.
   REFRESH FTAB_TABLE.

   IF RB_DOS = 'X'.
     TRANSLATE F_FILENAME USING '/\'.   " correct slash for Dos PC file
     CALL FUNCTION 'WS_UPLOAD'
          EXPORTING
               FILENAME            = F_FILENAME
               FILETYPE            = 'ASC'
          TABLES
               DATA_TAB            = FTAB_TABLE
          EXCEPTIONS
               CONVERSION_ERROR    = 1
               FILE_OPEN_ERROR     = 2
               FILE_READ_ERROR     = 3
               INVALID_TABLE_WIDTH = 4
               INVALID_TYPE        = 5
               NO_BATCH            = 6
               UNKNOWN_ERROR       = 7
               OTHERS              = 8.
     IF SY-SUBRC >< 0.
       WRITE: / 'Error reading file from local PC' COLOR COL_NEGATIVE.
     ENDIF.
   ELSEIF RB_UNIX = 'X'.
     TRANSLATE F_FILENAME USING '\/'.   " correct slash for unix
     OPEN DATASET F_FILENAME FOR INPUT MESSAGE LC_MESSAGE IN TEXT MODE.
     IF SY-SUBRC = 0.
       DO.
         READ DATASET F_FILENAME INTO FTAB_TABLE.
         IF SY-SUBRC = 0.
           APPEND FTAB_TABLE.
         ELSE.
           EXIT.
         ENDIF.
       ENDDO.
       CLOSE DATASET F_FILENAME.
     ELSE.
       WRITE: / 'Error reading file from remote computer'
                       COLOR COL_NEGATIVE,
              / LC_MESSAGE,
              / F_FILENAME.
       SY-SUBRC = 4.
     ENDIF.
   ENDIF.


 ENDFORM.                               " READ_REPORT_FROM_DISK

*---------------------------------------------------------------------*
*       FORM SPLIT_INCOMING_FILE                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FTAB_PROGRAM_FILE                                             *
*  -->  FTAB_PROGRAM_SOURCE                                           *
*  -->  `                                                             *
*  -->  FTAB_PROGRAM_TEXTS                                            *
*---------------------------------------------------------------------*
 FORM SPLIT_INCOMING_FILE TABLES FTAB_PROGRAM_FILE
                                      STRUCTURE MTAB_PROGRAM_FILE
                                 FTAB_PROGRAM_SOURCE
                                      STRUCTURE MTAB_PROGRAM_SOURCE
                                 FTAB_PROGRAM_TEXTS
                                      STRUCTURE MTAB_PROGRAM_TEXTS
                                 FTAB_PROGRAM_DOCUMENTATION
                                   STRUCTURE MTAB_PROGRAM_DOCUMENTATION
                        CHANGING FSTR_TRDIR
                                 FSTR_THEAD.

   DATA:
     LC_DATATYPE(4) TYPE C,             " Type of data, REPO, TEXP, RDIR

     LC_PROGRAM_FILE LIKE MTAB_PROGRAM_FILE.

   LOOP AT FTAB_PROGRAM_FILE.
     LC_PROGRAM_FILE = FTAB_PROGRAM_FILE.
     CASE LC_PROGRAM_FILE(9).
       WHEN MC_TRDIR_IDENTIFIER.
         LC_DATATYPE = MC_TRDIR_SHORT.
       WHEN MC_REPORT_IDENTIFIER.
         LC_DATATYPE = MC_REPORT_SHORT.
       WHEN MC_TEXT_IDENTIFIER.
         LC_DATATYPE = MC_TEXT_SHORT.
       WHEN MC_DOC_IDENTIFIER.
         LC_DATATYPE = MC_DOC_SHORT.
       WHEN MC_THEAD_IDENTIFIER.
         LC_DATATYPE = MC_THEAD_SHORT.
       WHEN OTHERS. " Actual contents of report, trdir, or text
         CASE LC_DATATYPE.
           WHEN MC_TRDIR_SHORT.
             FSTR_TRDIR = FTAB_PROGRAM_FILE.
           WHEN MC_REPORT_SHORT.
             FTAB_PROGRAM_SOURCE = FTAB_PROGRAM_FILE.
             APPEND FTAB_PROGRAM_SOURCE.
           WHEN MC_TEXT_SHORT.
             FTAB_PROGRAM_TEXTS = FTAB_PROGRAM_FILE.
             APPEND FTAB_PROGRAM_TEXTS.
           WHEN MC_THEAD_SHORT.
             FSTR_THEAD = FTAB_PROGRAM_FILE.
           WHEN MC_DOC_SHORT.
             FTAB_PROGRAM_DOCUMENTATION = FTAB_PROGRAM_FILE.
             APPEND FTAB_PROGRAM_DOCUMENTATION.
         ENDCASE.
     ENDCASE.
   ENDLOOP.
 ENDFORM.                               " SPLIT_INCOMING_FILE
*---------------------------------------------------------------------*
*       FORM INSERT_NEW_REPORT                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FTAB_PROGRAM_SOURCE                                           *
*  -->  FTAB_PROGRAM_TEXTS                                            *
*  -->  F_TRDIR                                                       *
*---------------------------------------------------------------------*
 FORM INSERT_NEW_REPORT TABLES FTAB_PROGRAM_SOURCE
                                   STRUCTURE MTAB_PROGRAM_SOURCE
                               FTAB_PROGRAM_TEXTS
                                    STRUCTURE MTAB_PROGRAM_TEXTS
                               FTAB_PROGRAM_DOCUMENTATION
                                    STRUCTURE MTAB_PROGRAM_DOCUMENTATION

                        USING  FSTR_TRDIR LIKE TRDIR
                               FSTR_THEAD LIKE MSTR_THEAD.
   DATA:
     LC_OBJ_NAME LIKE E071-OBJ_NAME,
     LC_LINE2(40)     TYPE C,
     LC_ANSWER(1)     TYPE C.

*-- read trdir to see if the report already exists, if it does, prompt
*-- user to overwrite or abort.
   SELECT SINGLE * FROM TRDIR WHERE NAME = FSTR_TRDIR-NAME.
   IF SY-SUBRC = 0.                     " Already exists
     CONCATENATE 'want to overwrite report'
                 FSTR_TRDIR-NAME
       INTO LC_LINE2 SEPARATED BY SPACE.

     CONCATENATE LC_LINE2
                 '?'
       INTO LC_LINE2.

     CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
              DEFAULTOPTION  = 'N'
              TEXTLINE1   = 'The selected report already exists, do you'

              TEXTLINE2      = LC_LINE2
              TITEL          = 'Report already exists'
              CANCEL_DISPLAY = SPACE
          IMPORTING
               ANSWER         = LC_ANSWER
          EXCEPTIONS
               OTHERS         = 1.
   ELSE.
     LC_ANSWER = 'J'.
   ENDIF.

   IF LC_ANSWER = 'J'.
*-- Create the TADIR entry.  (TRDIR entry created by INSERT REPORT)
     LC_OBJ_NAME = TRDIR-NAME.

     CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
          EXPORTING
               WI_E071_PGMID     = 'R3TR'
               WI_E071_OBJECT    = 'PROG'
               WI_E071_OBJ_NAME  = LC_OBJ_NAME
               WI_TADIR_DEVCLASS = '$TMP'
          EXCEPTIONS
               EXIT              = 3
               OTHERS            = 4.

     IF SY-SUBRC = 0.
*-- Create Report
       INSERT REPORT FSTR_TRDIR-NAME FROM FTAB_PROGRAM_SOURCE.
*-- Create Texts
       INSERT TEXTPOOL FSTR_TRDIR-NAME FROM FTAB_PROGRAM_TEXTS
              LANGUAGE SY-LANGU.
*-- Save Documentation
       CALL FUNCTION 'DOCU_UPDATE'
            EXPORTING
                 HEAD    = FSTR_THEAD
                 STATE   = 'A'
                 TYP     = 'E'
                 VERSION = '1'
            TABLES
                 LINE    = FTAB_PROGRAM_DOCUMENTATION
            EXCEPTIONS
                 OTHERS  = 1.

     ELSE.
       WRITE: / 'Error updating the TADIR entry' COLOR COL_NEGATIVE,
                'Program' COLOR COL_NEGATIVE INTENSIFIED OFF,
                FSTR_TRDIR-NAME, 'was not loaded into SAP.'
                   COLOR COL_NEGATIVE INTENSIFIED OFF.
     ENDIF.
   ELSE.
     WRITE: / FSTR_TRDIR-NAME COLOR COL_NEGATIVE,
              'was not uploaded into SAP.  Action cancelled by user'
                  COLOR COL_NEGATIVE INTENSIFIED OFF.
   ENDIF.
 ENDFORM.                               " INSERT_NEW_REPORT
*---------------------------------------------------------------------*
*       FORM GET_NAME                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(F_FIELD)                                                *
*  -->  F_NAME                                                        *
*---------------------------------------------------------------------*
 FORM GET_NAME USING VALUE(F_FIELD)
            CHANGING F_NAME.

   DATA: LTAB_FIELDS LIKE DYNPREAD OCCURS 0 WITH HEADER LINE,
         LC_PROG LIKE D020S-PROG,
         LC_DNUM LIKE D020S-DNUM.

   TRANSLATE F_FIELD TO UPPER CASE.

   refresh ltab_fields.
   LTAB_FIELDS-FIELDNAME = F_FIELD.
   append ltab_fields.
   LC_PROG =  SY-REPID .
   LC_DNUM =  SY-DYNNR .
   CALL FUNCTION 'DYNP_VALUES_READ'
        EXPORTING
             DYNAME     = LC_PROG
             DYNUMB     = LC_DNUM
        TABLES
             dynpfields = ltab_fields
        EXCEPTIONS
             OTHERS     = 01.
   read table ltab_fields index 1.
   IF SY-SUBRC EQ 0.
     F_NAME = LTAB_FIELDS-FIELDVALUE.
     refresh ltab_fields.
   ENDIF.

   CALL FUNCTION 'F4_USER'
        EXPORTING
             OBJECT = F_NAME
        IMPORTING
             RESULT = F_NAME.

 ENDFORM.                               " GET_NAME

* Title Text
* DIR     File Download Options (File Selection)
* FIL     File Options
* FNA     Enter filename below (under File Options)
* H01     Prog Name
* H03     Program Description
* SNG      Upload a single file
* UDL     Upload to SAP/Download from SAP
* UPL     File Upload Options

* Text Elements
* R        Backup/Restore program source code with texts
* P_PATH          Path to save programs to
* RB_DOS          Files on local computer
* RB_DOWN         Download Programs
* RB_FILE         Upload a single file
* RB_LIST         Select program(s) from a list
* RB_UNIX         Files on remote computer
* RB_UP           Upload Programs to SAP
* S_CDAT           Date Created
* S_CNAM          Created by UserID
* S_NAME          Program Name
* S_SUBC          Program Type
* S_UDAT          Date Changed
* S_UNAM          Last Changed by UserID
