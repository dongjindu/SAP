REPORT ZBMR903_PO_PART_CHECK MESSAGE-ID ZMPP.

************************************************************************
* Program Name      : ZBMR901_BOM_NO_CHILD
* Author            : Yongping
* Creation Date     : 2004.09.13.
* Specifications By : Yongping
* Pattern           :
* Development Request No : UD1K912192
* Addl Documentation:
* Description       : Purchase order part indented BOM check list
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
*****************************************************************
*GLOBAL DATA
*****************************************************************
TABLES: MARA.



****************************************************************
*INTERNAL TABLES
****************************************************************

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MATNR TYPE MARA-MATNR,  "MATERIAL
      END   OF IT_EXCL.

*OUTPUT FORMAT
DATA: BEGIN OF IT_RESULT OCCURS 0,
        MATNR LIKE MARA-MATNR,
        FLAG  TYPE C,
        NEXT1 LIKE MARA-MATNR,
        NEXT2 LIKE MARA-MATNR,
        NEXT3 LIKE MARA-MATNR,
        NEXT4 LIKE MARA-MATNR,
        NEXT5 LIKE MARA-MATNR,
      END OF IT_RESULT.

*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_CASE1 RADIOBUTTON  GROUP RG USER-COMMAND UCOM    .
SELECTION-SCREEN COMMENT  (40) TEXT-002 FOR FIELD P_CASE1  .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  10(10) TEXT-003 FOR FIELD P_MATNR .
SELECT-OPTIONS: P_MATNR  FOR  MARA-MATNR.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.


SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_CASE2    RADIOBUTTON  GROUP RG DEFAULT 'X'   .
SELECTION-SCREEN COMMENT  (40) TEXT-004 FOR FIELD P_CASE2  .
SELECTION-SCREEN END OF LINE.

PARAMETERS:
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT'.
SELECTION-SCREEN END OF BLOCK B1.
**********************************************************************
*END OF SELECTION SCREEN
**********************************************************************

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.



START-OF-SELECTION.
  CASE 'X'.
    WHEN P_CASE1.  "INTERFACE TABLE
      PERFORM PART_GET.
    WHEN P_CASE2.  "EXCEL
      REFRESH IT_EXCL. CLEAR IT_EXCL.
      PERFORM UPLOAD_PROCESS.
  ENDCASE.
* CHECK IF PARTS HAVE FSC
  PERFORM FIND_FSC.
END-OF-SELECTION.

 PERFORM WRITE_PROCESS.





*********************************************************************
**** FORMS
*********************************************************************
FORM screen_modify.
  LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'P_FILETY'.
          screen-input = 0.
      ENDCASE.
    MODIFY SCREEN.
    CLEAR screen.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename
                                          mode     TYPE c.

  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.

  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
       EXPORTING
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       IMPORTING
            filename         = tmp_filename
*         RC               =
       EXCEPTIONS
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  ENDIF.

ENDFORM.                               " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  PART_GET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PART_GET.
   SELECT MATNR
    INTO CORRESPONDING FIELDS OF TABLE IT_EXCL
    FROM MARA
    WHERE MATNR IN P_MATNR.
   IF SY-SUBRC <> 0.
     MESSAGE E000 WITH 'NO DATA FOUND !'.
   ENDIF.
*  DELETE FSC MATERIAL
   DELETE IT_EXCL WHERE MATNR EQ 'FERT'.

ENDFORM.                    " PART_GET



*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PROCESS
*&---------------------------------------------------------------------*
FORM upload_process.

  CALL FUNCTION 'WS_UPLOAD'
       EXPORTING
            codepage                = ' '
            filename                = p_file
            filetype                = p_filety
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       TABLES
            data_tab                = it_excl
      EXCEPTIONS
           conversion_error        = 1
           file_open_error         = 2
           file_read_error         = 3
           invalid_table_width     = 4
           invalid_type            = 5
           no_batch                = 6
           unknown_error           = 7
           gui_refuse_filetransfer = 8
           customer_error          = 9
           OTHERS                  = 10
            .
  CASE sy-subrc.
    WHEN 0.
      SKIP.
    WHEN 1.
      MESSAGE E000 WITH 'FILE CONVERSION ERROR !'.
    WHEN 2.
      MESSAGE e000 WITH 'FILE OPEN ERROR, FILE NO FOUND!'.
    WHEN 3.
      MESSAGE e000 WITH 'FILE READ ERROR'.
    WHEN OTHERS.
      MESSAGE e000 WITH 'FILE UPLOAD ERROR, CHECK YOUR FILE!.'.
  ENDCASE.

ENDFORM.                               " UPLOAD_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
*       OUTPUT THE CHECK RESULT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WRITE_PROCESS.

  WRITE: / 'FILE CONTENT: '.
  WRITE: / SY-ULINE.
  LOOP AT IT_EXCL.
   WRITE: / IT_EXCL-MATNR.
  ENDLOOP.

ENDFORM.                    " WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  FIND_FSC
*&---------------------------------------------------------------------*
*       IMPLODE THE MATERIAL BOM AND CHECK IF FSC EXISTS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FIND_FSC.

*  LOOP IT_EXCL TO CHECK EACH MATERIAL
   LOOP AT IT_EXCL.


   ENDLOOP.
ENDFORM.                    " FIND_FSC
