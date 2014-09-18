* (c) Copyright 1999 SAP America, Inc.
* Accelerated HR Conversion Payroll Results Load
* Version 1.0  - August 2000

* Payroll results - table T574A/B direct load
* Authors : Hemang / Mrudula - Annance Consulting
*---------------------------------------------------------------------*
REPORT ZP574B46C MESSAGE-ID ZP.

TABLES :T574A, T574B.    " Qual Load Tabs

SELECTION-SCREEN SKIP 3.
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 2.
PARAMETERS: TABT574A AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN END OF BLOCK BLOCK1.

*parameters : ft574b like rlgrap-filename default'C:\574b.TXT'.
DATA : FT574B LIKE RLGRAP-FILENAME.
DATA: BEGIN OF ITABTT5B OCCURS 0.
   INCLUDE STRUCTURE t574b.
DATA :END OF ITABTT5B.

DATA: DELIMITER TYPE X VALUE'09', ERR(50), L TYPE I.
DATA: SRCFILE LIKE RLGRAP-FILENAME.
CALL FUNCTION 'WS_FILENAME_GET'
     EXPORTING
          DEF_FILENAME     = 'P'
          DEF_PATH         = 'c:\'
          MASK             = ',*.*,*.*.'
          MODE             = 'O'
          TITLE            = 'Select  Folder'
     IMPORTING
          FILENAME         = SRCFILE
     EXCEPTIONS
          INV_WINSYS       = 01
          NO_BATCH         = 02
          SELECTION_CANCEL = 03
          SELECTION_ERROR  = 04.
PERFORM CHECK_ERROR USING SY-SUBRC 'Get File name'.
IF SRCFILE EQ SPACE .
 WRITE:/'Selected Path name does not exist.'.EXIT.ENDIF.
L = STRLEN( SRCFILE ).
L = L - 1.
 MOVE '574a.txt' TO SRCFILE+L.
 CLEAR FT574B.
 MOVE SRCFILE TO FT574B.


PERFORM UPLOAD_t574b USING Ft574b ERR.
PERFORM APPEND.

*---------------------------------------------------------------------*
*       FORM UPLOAD_t574b                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  Ft574b                                                        *
*  -->  ERR                                                           *
*---------------------------------------------------------------------*
FORM UPLOAD_t574b USING Ft574b ERR.

  DATA: BEGIN OF ITAB OCCURS 0,
  FILE1(8192),
   END OF ITAB.
  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            FILENAME      = Ft574b
            FILETYPE      = 'ASC'
       TABLES
            DATA_TAB      = ITAB
       EXCEPTIONS
            UNKNOWN_ERROR = 7
            OTHERS        = 8.
  PERFORM CHECK_ERROR USING SY-SUBRC ERR.
  DATA : T." Used to store junk (tab) char if any ...
  LOOP AT ITAB.
    SPLIT ITAB-FILE1 AT DELIMITER INTO
            ITABTT5B-QUALI  ITABTT5B-QTEXT T.
    APPEND ITABTT5B.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM APPEND.
  LOOP AT ITABTT5B.
    T574A-QUALI =  ITABTT5B-QUALI.
    T574B-LANGU = 'E' .
    t574b-QUALI =  ITABTT5B-QUALI.
    t574b-QTEXT =  ITABTT5B-QTEXT.
   IF t574b-QUALI NE SPACE.
    INSERT T574A.
    INSERT  T574B.
    ENDIF.
    IF SY-SUBRC NE 0.
      WRITE:/ 'unable to insert into table t574b : '.
      WRITE t574b-QUALI.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CHECK_ERROR                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  ERR_CD                                                        *
*  -->  STAGE                                                         *
*---------------------------------------------------------------------*
FORM CHECK_ERROR USING ERR_CD STAGE.
  CASE ERR_CD.
    WHEN 0.
    WHEN OTHERS.
      WRITE:/ 'Error in the process ', STAGE, '. Error -', ERR_CD.
      STOP.
  ENDCASE.
ENDFORM.
