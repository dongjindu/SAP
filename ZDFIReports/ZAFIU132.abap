*----------------------------------------------------------------------*
* Program ID        : ZAFIU132
* Title             : Excel Form Management
* Created on        : 04/06/2009
* Created by        : IG. Moon
* Specifications By : Andy Choi
* Description       : Excel Form Management
*----------------------------------------------------------------------*

 REPORT zafiu132 LINE-SIZE 132 LINE-COUNT 65
                       NO STANDARD PAGE HEADING MESSAGE-ID db .

****************************** constants ******************************
 CONSTANTS:  false VALUE ' ',
             true  VALUE 'X'.

****************************** macros *********************************
 DEFINE __cls.                          " clear & refresh
   clear &1.refresh &1.
 END-OF-DEFINITION.

****************************** data ***********************************
 DATA target_file(80).
 DATA: BEGIN OF ifile OCCURS 0,
         text(255),
       END OF ifile,
       nr_of_bytes             TYPE i.
 DATA gt_error.
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
 SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
 SELECTION-SCREEN  SKIP 1.

 SELECTION-SCREEN BEGIN OF BLOCK b1s WITH FRAME TITLE text-002.
 PARAMETERS: p_down RADIOBUTTON GROUP ru..
 PARAMETERS: p_updn RADIOBUTTON GROUP ru..
 SELECTION-SCREEN END OF BLOCK b1s.

 SELECTION-SCREEN  SKIP 1.
 PARAMETERS: p_file LIKE rlgrap-filename OBLIGATORY.
 SELECTION-SCREEN BEGIN OF LINE.
 SELECTION-SCREEN COMMENT 33(41) text-x01
                             MODIF ID exl.
 SELECTION-SCREEN END OF LINE.
 PARAMETER  r_a RADIOBUTTON GROUP r1."
 PARAMETER  r_b RADIOBUTTON GROUP r1."

 SELECTION-SCREEN END OF BLOCK b1.

 AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
   PERFORM f4_p_upfile USING p_file.

*--------------------------------------------------------------------*
*  START-OF-SELECTION.
*--------------------------------------------------------------------*
 START-OF-SELECTION.

   TRANSLATE p_file TO UPPER CASE.

   CLEAR gt_error.

   IF p_updn EQ true.
     PERFORM get_form_from_pc.
     PERFORM put_form_to_unix.
   ELSE.
     PERFORM get_form_from_unix.
   ENDIF.

*---------------------------------------------------------------------*
*       FORM f4_p_upfile                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FILE                                                        *
*---------------------------------------------------------------------*
 FORM f4_p_upfile USING    pp_file.

   CALL FUNCTION 'WS_FILENAME_GET'
        EXPORTING
             def_path         = p_file  "* File Name
             mask             = ',*.*,*.*.'
             mode             = 'O'
        IMPORTING
             filename         = pp_file
        EXCEPTIONS
             inv_winsys       = 1
             no_batch         = 2
             selection_cancel = 3
             selection_error  = 4
             OTHERS           = 5.

 ENDFORM.                    " f4_p_upfile
*&---------------------------------------------------------------------*
*&      Form  get_form_from_pc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM get_form_from_pc.

   __cls ifile.

   CALL FUNCTION 'WS_UPLOAD'
        EXPORTING
             filename   = p_file
             filetype   = 'BIN'
        IMPORTING
             filelength = nr_of_bytes
        TABLES
             data_tab   = ifile.

   IF sy-subrc NE 0.
     gt_error = true.
   ENDIF.
 ENDFORM.                    " get_form_from_pc
*&---------------------------------------------------------------------*
*&      Form  put_form_to_unix
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM put_form_to_unix.

   DATA par_file(80).
   DATA only_file(80).
   DATA sid(3).

   only_file = p_file.

   DO 10 TIMES.
     SHIFT only_file UP TO '\'.
     IF sy-subrc NE 0. EXIT. ENDIF.
     SHIFT only_file LEFT  DELETING LEADING  '\'.
   ENDDO.

   sid = sy-sysid.
   CONCATENATE '/usr/sap/' sid '/FI_FORMS/'
   only_file INTO par_file.

   OPEN DATASET par_file FOR OUTPUT IN BINARY MODE.

   IF sy-subrc NE 0.
     MESSAGE s000 WITH 'Error when opening the unix file.'.
     EXIT.
   ENDIF.

   LOOP AT ifile.
     TRANSFER ifile TO par_file.
   ENDLOOP.

   CLOSE DATASET par_file.

   IF sy-subrc <> 0.
     MESSAGE s000 WITH 'ERROR OPENING/DOWNLOADING TO UNIX FILE.'.
   ELSE.
     MESSAGE s000 WITH 'Created successfully!'.
   ENDIF.

 ENDFORM.                    " put_form_to_unix
*&---------------------------------------------------------------------*
*&      Form  get_form_from_unix
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
 FORM get_form_from_unix.

   DATA par_file(80).
   DATA flength TYPE p.
   DATA sid(3).

   sid = sy-sysid.

   IF r_a EQ true.
     CONCATENATE '/usr/sap/' sid '/FI_FORMS/'
     'IV_TEMPLATE.XLS' INTO par_file.
   ELSE.
     CONCATENATE '/usr/sap/' sid '/FI_FORMS/'
     'IV2_TEMPLATE.XLS' INTO par_file.
   ENDIF.

   OPEN DATASET par_file FOR INPUT IN BINARY MODE.
   IF sy-subrc NE 0.
     MESSAGE s000 WITH 'Error when opening the unix file.'.
     EXIT.
   ENDIF.

   nr_of_bytes = 0.
   __cls ifile.

   DO.
     READ DATASET par_file INTO ifile LENGTH flength.
     IF sy-subrc NE 0. EXIT. ENDIF.
     APPEND ifile.
     nr_of_bytes = nr_of_bytes + flength.
   ENDDO.

   CLOSE DATASET par_file.

   CALL FUNCTION 'WS_DOWNLOAD'
        EXPORTING
             bin_filesize        = nr_of_bytes
             filename            = p_file
             filetype            = 'BIN'
*    IMPORTING
*         FILELENGTH          =
        TABLES
             data_tab            = ifile
*         FIELDNAMES          =
        EXCEPTIONS
             file_open_error     = 1
             file_write_error    = 2
             invalid_filesize    = 3
             invalid_table_width = 4
             invalid_type        = 5
             no_batch            = 6
             unknown_error       = 7
             OTHERS              = 8.

   IF sy-subrc NE 0.
     MESSAGE e001 WITH 'File Download error :' sy-subrc.
   ELSE.
     MESSAGE s000 WITH 'Successfully Downloaded'.
   ENDIF.

 ENDFORM.                    " get_form_from_unix
