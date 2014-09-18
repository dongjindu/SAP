*&----------------------------------------------------------------------
*& Program ID        : ZHRI_ACCRUAL_EXPORT_TO_KRONOS
*& Title             : [HR] - Accrual data Export to Kronos
*& Created by        : Valerian Utama
*& Created on        : 02/17/2011
*& Specifications By : Euna Lee
*& Reference Pgm     : N/A
*& Description       : Extract vacation/personal day to coma delimited
*&                     file (*.csv File).
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 02/17/11    Valerian  UD1K950866  Initial Program Development
*& 09/26/11    Valerian  UD1K953021  Fix decimals point and UNIX Path
*& 09/28/11    Valerian  UD1K953047  Include in-active TMs in Selection
*& 01/04/12    Valerian  UD1K953675  Exclude inpatriates from data
*&                                   selections
*& 07/10/12    Valerian  UD1K955145  Replace 'hard code' to determine
*&                                   daily hours (from PA0007-ARBST)
*&----------------------------------------------------------------------

REPORT  zhri_accrual_export_to_kro_dev MESSAGE-ID zmfi.

CONSTANTS: c_dlmtd(1)  TYPE c VALUE ',',
           c_dfile(30) TYPE c VALUE 'SAP_Accrual.csv',
           c_unix_pre(30) TYPE c VALUE '/sapmnt/',          "UD1K953021
           c_unix_suf(30) TYPE c VALUE '/kronos/kronosftp'. "UD1K953021

TYPE-POOLS: slis.

FIELD-SYMBOLS: <fs> TYPE any.

DATA: v_file      TYPE string,
      v_ans       TYPE i.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv.

DATA : BEGIN OF it_file_dlmtd OCCURS 0,
         record(500) TYPE c,
       END OF it_file_dlmtd.

DATA : BEGIN OF it_file OCCURS 0,
         pernr(8)              TYPE c,
         vdbal(12)             TYPE c,
         pdbal(12)             TYPE c,
       END OF it_file.

DATA : BEGIN OF it_data OCCURS 0,
         pernr                 TYPE pa0000-pernr,
         vdbal                 TYPE p DECIMALS 2,           "UD1K953021
         pdbal                 TYPE p DECIMALS 2,           "UD1K953021
*        vdbal                 TYPE p DECIMALS 4,           "UD1K953021
*        pdbal                 TYPE p DECIMALS 4,           "UD1K953021
         schkz                 TYPE pa0007-schkz,
       END OF it_data.

DATA: BEGIN OF it_accrual OCCURS 0,
        pernr                  TYPE pa2006-pernr,
        subty                  TYPE pa2006-subty,
        anzhl                  TYPE pa2006-anzhl,
        kverb                  TYPE pa2006-kverb,
      END OF it_accrual.

* BEGIN OF UD1K955145
DATA: BEGIN OF it_wrkschd OCCURS 0,
        pernr                  TYPE pa0007-pernr,
        schkz                  TYPE pa0007-schkz,
        arbst                  TYPE pa0007-arbst,
      END OF it_wrkschd.
* END OF UD1K955145

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR it_data-pernr.
PARAMETERS: p_kdate TYPE datum DEFAULT sy-datum OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' USER-COMMAND chck.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
PARAMETERS: p_pres RADIOBUTTON GROUP serv USER-COMMAND rad
                                        MODIF ID chk DEFAULT 'X',
            p_appl RADIOBUTTON GROUP serv MODIF ID chk.
SELECTION-SCREEN SKIP.
PARAMETERS: p_file(1024) TYPE c LOWER CASE MODIF ID chk
                                VISIBLE LENGTH 45.
SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'CHK'.
      IF NOT p_test IS INITIAL.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CHECK sy-ucomm NE 'CHCK'.
  IF p_test IS INITIAL AND
     p_file IS INITIAL.
    MESSAGE e000 WITH 'Please Enter Filename'.
  ENDIF.

  v_file = p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  IF NOT p_pres IS INITIAL.
    PERFORM browser CHANGING p_file v_ans.
  ELSE.
    PERFORM display_unix CHANGING p_file v_ans.
  ENDIF.

START-OF-SELECTION.
  PERFORM select_data.
  PERFORM modify_data.
  PERFORM output_data.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       Select Data
*----------------------------------------------------------------------*
FORM select_data.
  SELECT a~pernr b~schkz
    INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM pa0000 AS a JOIN pa0007 AS b
         ON b~pernr = a~pernr
         JOIN pa0001 AS c                                   "UD1K953675
         ON c~pernr = a~pernr                               "UD1K953675
   WHERE a~pernr IN s_pernr
     AND ( a~stat2 = '3' OR a~stat2 = '1' )                 "UD1K953047
*    AND a~stat2 = '3'                                      "UD1K953047
     AND a~endda GE p_kdate
     AND a~begda LE p_kdate
     AND b~endda GE p_kdate
     AND b~begda LE p_kdate
     AND c~endda GE p_kdate                                 "UD1K953675
     AND c~begda LE p_kdate                                 "UD1K953675
     AND c~persg NE '9'.                                    "UD1K953675

  SELECT pernr subty anzhl kverb INTO TABLE it_accrual
    FROM pa2006
    FOR ALL ENTRIES IN it_data
   WHERE pernr = it_data-pernr
     AND desta LE p_kdate
     AND deend GE p_kdate.

* BEGIN OF UD1K955145
  SELECT pernr schkz arbst INTO TABLE it_wrkschd
    FROM pa0007
    FOR ALL ENTRIES IN it_data
   WHERE pernr = it_data-pernr
     AND schkz = it_data-schkz
     AND endda GE p_kdate
     AND begda LE p_kdate.
* END OF UD1K955145

  SORT: it_accrual BY pernr subty,
        it_data    BY pernr,
        it_wrkschd BY pernr schkz.                          "UD1K955145

  LOOP AT it_data.

* BEGIN OF UD1K955145
    CLEAR it_wrkschd.
    READ TABLE it_wrkschd WITH KEY pernr = it_data-pernr
                                   schkz = it_data-schkz
                                   BINARY SEARCH.
* END OF UD1K955145

    READ TABLE it_accrual WITH KEY pernr = it_data-pernr
                                   subty = '13'
                                   BINARY SEARCH.
    IF sy-subrc = 0.

* BEGIN OF UD1K955145
      it_data-vdbal = ( it_accrual-anzhl - it_accrual-kverb ) *
                        it_wrkschd-arbst.

*      IF it_data-schkz = '4001'   OR
*         it_data-schkz = '8000_A' OR
*         it_data-schkz = '8000_B' OR
*         it_data-schkz = '8000_C'.
*
*        it_data-vdbal = ( it_accrual-anzhl - it_accrual-kverb ) * 10.
*      ELSE.
*        it_data-vdbal = ( it_accrual-anzhl - it_accrual-kverb ) * 8.
*      ENDIF.
* END OF UD1K955145

    ENDIF.

    READ TABLE it_accrual WITH KEY pernr = it_data-pernr
                                   subty = '10'
                                   BINARY SEARCH.
    IF sy-subrc = 0.

* BEGIN OF UD1K955145
      it_data-pdbal = ( it_accrual-anzhl - it_accrual-kverb ) *
                        it_wrkschd-arbst.

*      IF it_data-schkz = '4001'   OR
*         it_data-schkz = '8000_A' OR
*         it_data-schkz = '8000_B' OR
*         it_data-schkz = '8000_C'.
*
*        it_data-pdbal = ( it_accrual-anzhl - it_accrual-kverb ) * 10.
*      ELSE.
*        it_data-pdbal = ( it_accrual-anzhl - it_accrual-kverb ) * 8.
*      ENDIF.
* END OF UD1K955145

    ENDIF.

    MODIFY it_data TRANSPORTING vdbal pdbal.
  ENDLOOP.

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
FORM modify_data .
  DATA: l_rec   TYPE i.

* Move data to output structure
  LOOP AT it_data.
    MOVE-CORRESPONDING it_data TO it_file.

    SHIFT: it_file-vdbal LEFT DELETING LEADING space,
           it_file-pdbal LEFT DELETING LEADING space.

    APPEND it_file. CLEAR it_file.
  ENDLOOP.

  FREE it_data.

  DESCRIBE TABLE it_file LINES l_rec.
  MESSAGE s000 WITH 'Total record(s) extracted:' l_rec.

ENDFORM.                    " MODIFY_DATA

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       Display Data
*----------------------------------------------------------------------*
FORM display_data.
  PERFORM fieldcat_init     USING gt_fieldcat[].
  PERFORM alv_grid_display  TABLES it_file.
ENDFORM.                    " DISPLAY_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       Transfer Data to Desktop/PC or UNIX/Presentation Server
*----------------------------------------------------------------------*
FORM transfer_data.
  DATA: l_totrec TYPE i.

  LOOP AT it_file.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE it_file TO <fs>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      IF NOT it_file_dlmtd IS INITIAL.
        CONCATENATE it_file_dlmtd-record c_dlmtd <fs>
               INTO it_file_dlmtd-record.
      ELSE.
        it_file_dlmtd-record = <fs>.
      ENDIF.
    ENDDO.

    APPEND it_file_dlmtd. CLEAR it_file_dlmtd.
  ENDLOOP.

  DESCRIBE TABLE it_file_dlmtd LINES l_totrec.

  CASE 'X'.
    WHEN p_appl.
      OPEN DATASET v_file FOR OUTPUT IN TEXT MODE.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      LOOP AT it_file_dlmtd.
        TRANSFER it_file_dlmtd TO v_file.
      ENDLOOP.

      CLOSE DATASET v_file.

    WHEN p_pres.
      IF NOT sy-batch IS INITIAL.
        MESSAGE s000
           WITH 'Writing File to Desktop Not Possible'
                'in Background Mode' ' ' ' '.
        EXIT.
      ENDIF.

      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = v_file
        TABLES
          data_tab                = it_file_dlmtd
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.

      IF sy-subrc <> 0.
        IF sy-subrc = 15.
          MESSAGE s011 WITH 'Access Denied'.
        ELSEIF NOT sy-msgid IS INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE s011 WITH 'Error when creating the file'.
        ENDIF.
      ENDIF.
  ENDCASE.

  IF sy-subrc EQ 0.
    MESSAGE s000 WITH 'File is written to:' v_file l_totrec
                      'Record(s)'.
  ENDIF.
ENDFORM.                    " TRANSFER_DATA

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->ft_fieldcat  Field Catalog Value
*----------------------------------------------------------------------*
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv .

  DATA: l_pos TYPE i,
        gs_fieldcat TYPE slis_fieldcat_alv.

  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fieldcat.
    gs_fieldcat-col_pos       = l_pos.
    gs_fieldcat-key           = &1.
    gs_fieldcat-fieldname     = &2.
    gs_fieldcat-seltext_s     = &3.        " Column heading
    gs_fieldcat-seltext_m     = &3.        " Column heading
    gs_fieldcat-outputlen     = &4.        " Column width
    gs_fieldcat-datatype      = &5.        " Data type
    gs_fieldcat-emphasize     = &6.
    gs_fieldcat-cfieldname    = &7.
    gs_fieldcat-lowercase     = 'X'.
    append gs_fieldcat to  ft_fieldcat.
  END-OF-DEFINITION.

  __catalog :
    'X'  'PERNR'      'Emp.No.'          8   'CHAR' '' '',
    ' '  'VDBAL'      'Vac.Day Bal.'     12  'CHAR' '' '',
    ' '  'PDBAL'      'Prs.Day Bal.'     12  'CHAR' '' ''.

ENDFORM.                    " fieldcat_init

*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab.

  DATA: l_repid TYPE sy-repid.
  DATA : gs_layout TYPE slis_layout_alv.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  l_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = ft_outtab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_GRID_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       Browse Desktop/PC Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM browser CHANGING filename answer.
  DATA  $filename TYPE string.
  DATA: l_path  TYPE string,
        l_fpath TYPE string,
        l_dfile TYPE string.

  l_dfile = c_dfile.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
      default_extension = 'csv'
      default_file_name = l_dfile
      file_filter       = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
      initial_directory = '\\10.121.233.22\Data'
    CHANGING
      filename          = $filename
      path              = l_path
      fullpath          = l_fpath
      user_action       = answer
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSEIF answer = 0.
    filename = l_fpath.
  ELSE.
    MESSAGE s118(ba).
  ENDIF.
ENDFORM.                    " BROWSER

*&---------------------------------------------------------------------*
*&      Form  output_data
*&---------------------------------------------------------------------*
*       Display data or write data to file
*----------------------------------------------------------------------*
FORM output_data.
  CHECK NOT it_file[] IS INITIAL.

  IF p_test IS INITIAL.
    PERFORM transfer_data.
  ELSE.
    PERFORM display_data.
  ENDIF.
ENDFORM.                    " output_data

*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
FORM display_unix CHANGING filename answer.
  DATA: BEGIN OF it_filename OCCURS 0,
          path(1024) TYPE c,
        END OF it_filename.

* BEGIN OF UD1K953021
  CONCATENATE c_unix_pre sy-sysid c_unix_suf INTO it_filename-path.
  APPEND it_filename.
*  SELECT dirname
*    FROM user_dir
*    INTO TABLE it_filename
*   WHERE aliass = 'DIR_KRONOS'.
* END OF UD1K953021

  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'
                     ID 'VALUE' FIELD it_filename-path.
  APPEND it_filename.

  CALL FUNCTION 'POPUP_WITH_TABLE'
    EXPORTING
      endpos_col   = '100'
      endpos_row   = '10'
      startpos_col = '1'
      startpos_row = '1'
      titletext    = 'Select UNIX Directory'
    IMPORTING
      choice       = filename
    TABLES
      valuetab     = it_filename
    EXCEPTIONS
      break_off    = 1
      OTHERS       = 2.

  answer = sy-subrc.

  IF sy-subrc = 0.
    CONCATENATE filename '/' c_dfile INTO filename.
  ELSE.
    MESSAGE s549(fibl).
  ENDIF.
ENDFORM.                    " display_unix
