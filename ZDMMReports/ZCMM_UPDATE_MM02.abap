*&------------------------------------------------------------------
*& Program ID     : ZCMM_UPDATE_MM02
*& Program Name   : Update Storage Bin from Excel upload
*& Created by     : Victor
*& Created on     : 11.09.2012
*& Development ID :
*& Reference Pgm. :
*& Description    : Only One time USE
*& Modification Log
*&====================================================================

REPORT  zcmm_update_mm02 NO STANDARD PAGE HEADING
                                LINE-SIZE 200 MESSAGE-ID zz.

DATA : BEGIN OF it_excel OCCURS 0,
          matnr(18),
          plant(4),
          storage(4),
          bin(10),
        END OF it_excel.

DATA : l_return TYPE bapireturn.
DATA: lt_fileinfo  LIKE ocs_f_info OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------
* SELECTION-SCREEN
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS : p_fname LIKE rlgrap-filename OBLIGATORY
           DEFAULT 'C:\'.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*   AT SELECTION SCREEN ON VALUE-REQUEST                               *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.   "FILE UPLOAD
  PERFORM get_file_path.

*----------------------------------------------------------------------
* START-OF-SELECTION
*----------------------------------------------------------------------
START-OF-SELECTION.
  DATA: l_filename TYPE string.
  DATA : l_filety TYPE char10 VALUE 'ASC'.

  l_filename  = p_fname.

  CLEAR : it_excel[].

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = l_filename
      filetype                = l_filety
      has_field_separator     = 'X'       "tab
    TABLES
      data_tab                = it_excel
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

  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'Error during file upload'.
  ENDIF.


*  DELETE it_excel INDEX 1.   "delete header
  CHECK it_excel[] IS NOT INITIAL.


  DATA: ls_head    LIKE bapimathead,
        ls_mara    LIKE bapi_mara,
        ls_marax   LIKE bapi_marax,
        ls_mard    LIKE bapi_mard,
        ls_mardx   LIKE bapi_mardx,
        ls_return  LIKE bapiret2,
        lt_return2 LIKE bapi_matreturn2 OCCURS 0 WITH HEADER LINE.


  WRITE : /  text-red, 'Error List',  text-red.
  WRITE : / sy-uline.
  WRITE : / 'Material No.       Message'.
  WRITE : / sy-uline.

  LOOP AT it_excel.

    CLEAR : ls_head.
    ls_head-material     = it_excel-matnr.
    ls_head-storage_view = 'X'.

    ls_mard-plant    = it_excel-plant.
    ls_mard-stge_loc = it_excel-storage.
    ls_mard-stge_bin = it_excel-bin.

    ls_mardx-plant    = it_excel-plant.
    ls_mardx-stge_loc = it_excel-storage.
    ls_mardx-stge_bin = 'X'.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata             = ls_head
        storagelocationdata  = ls_mard
        storagelocationdatax = ls_mardx
      IMPORTING
        return               = ls_return
      TABLES
        returnmessages       = lt_return2.

    IF ls_return-type EQ 'E' OR ls_return-type EQ 'A' OR
       ls_return-type EQ 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      WRITE :/ it_excel-matnr , ls_return-message+0(150).

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.


  ENDLOOP.

*&---------------------------------------------------------------------*
*&      Form  GET_FILE_PATH
*&---------------------------------------------------------------------*
FORM get_file_path .

  CALL FUNCTION 'OCS_FILENAME_GET'
    EXPORTING
*     PI_DEF_FILENAME  = ' '
*     PI_DEF_PATH      = ' '
      pi_mask          = '*.*'
*     PI_MODE          = 'O'
      pi_title         = 'File Open'
    TABLES
      pt_fileinfo      = lt_fileinfo
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      general_error    = 5
      OTHERS           = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE lt_fileinfo INDEX 1.
    CHECK sy-subrc = 0.
    CONCATENATE lt_fileinfo-file_path lt_fileinfo-file_name
                                      INTO p_fname.
  ENDIF.
ENDFORM.                    " GET_FILE_PATH
