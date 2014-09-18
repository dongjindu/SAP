*----------------------------------------------------------------------
* Program ID        : ZIFI_UPLOAD_WARRANTY_DATA
* Title             : Upload Warranty Data to table ZTFI_WAR_POST
* Created on        : 4/18/2013
* Created by        : Valerian Utama
* Specifications By : Ravikumar
* Description       : Upload Warranty Data to table ZTFI_WAR_POST
*
*----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  04/18/2013  Valerian  UD1K956980  Initial Program Development
*----------------------------------------------------------------------

REPORT zifi_upload_warranty_data MESSAGE-ID zmfi
       NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: c_dfile(30) TYPE c VALUE '2010-01.txt'.

*---------------------------------------------------------------------*
* Data
*---------------------------------------------------------------------*
DATA: v_ans   TYPE i,
      it_data LIKE ztfi_war_post OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------
* Selection-Screen
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
PARAMETERS: p_file  LIKE rlgrap-filename.
SELECTION-SCREEN END   OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM browser CHANGING p_file v_ans.

AT SELECTION-SCREEN .
  IF p_file IS INITIAL.
    MESSAGE e000 WITH 'Please Enter Filename'.
  ENDIF.

*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM upload_data.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file USING filename.
  DATA: l_filename TYPE string,
        l_rec      TYPE i.

  l_filename = filename.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = l_filename
      has_field_separator     = 'X'
    TABLES
      data_tab                = it_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.

  IF sy-subrc <> 0.
    IF sy-subrc = 13.
      MESSAGE s011 WITH 'File has been opened'.
    ELSEIF NOT sy-msgid IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE s011 WITH 'Error when reading the file'.
    ENDIF.

    STOP.
  ENDIF.

  DELETE it_data WHERE mandt IS INITIAL.

* BEGIN OF VALTEST
*  DELETE FROM ztfi_war_post WHERE gjahr = '2010'
*                              AND monat = '1'.
*  COMMIT WORK.
* END OF VALTEST

  DESCRIBE TABLE it_data LINES l_rec.

  IF l_rec IS INITIAL.
    MESSAGE s011 WITH 'No data in the file'.
  ELSE.
    MODIFY ztfi_war_post FROM TABLE it_data.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE s000 WITH 'Total Records Uploaded:' l_rec.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_data_from_dkbz_dkpo
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM browser CHANGING filename answer.
  DATA: l_dfile TYPE string,
        lt_file TYPE filetable,
        l_rc    TYPE i.

  l_dfile = c_dfile.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Select File Name'
      default_extension       = 'txt'
      default_filename        = l_dfile
      file_filter             = 'TXT (*.txt)|*.txt| All (*.*)|*.*'
      initial_directory       = 'C:\'
    CHANGING
      file_table              = lt_file
      rc                      = l_rc
      user_action             = answer
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF answer = 0.
    READ TABLE lt_file INTO filename INDEX 1.
  ELSE.
    MESSAGE s118(ba).
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
*       Upload Data from Local/UNIX
*----------------------------------------------------------------------*
FORM upload_data.
  PERFORM upload_file USING p_file.
ENDFORM.                    " upload_data
