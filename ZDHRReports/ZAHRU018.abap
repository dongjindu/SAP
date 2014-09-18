*&----------------------------------------------------------------------
*& Program ID        : ZAHRU018
*& Title             : [HR] - Extract TM data for DIR website
*& Created by        : Valerian Utama
*& Created on        : 07/12/2012
*& Specifications By : Grace Li
*& Reference Pgm     : N/A
*& Description       : Extract TM data for DIR website and download to
*&                     a text file (Fix length).
*&
*& Modification Log
*& Date        Developer Issue No    Description
*&======================================================================
*& 07/12/2012  Valerian  UD1K955137  Initial Program Development
*&
*&----------------------------------------------------------------------

REPORT  zahru018 MESSAGE-ID zmfi.

TABLES: pa0000, pa0001.

CONSTANTS:
  c_dfile        TYPE string VALUE 'ALDIR_',
  c_dpath        TYPE string VALUE 'C:\TEMP\ALDIR\',
  c_dextn        TYPE string VALUE '.TXT',
  c_unix_pre(30) TYPE c VALUE '/sapmnt/',
  c_unix_suf(30) TYPE c VALUE '/kronos/kronosftp'.

TYPE-POOLS: slis.

DATA: v_file      TYPE string,
      v_ans       TYPE i.

DATA : gt_fieldcat TYPE slis_t_fieldcat_alv,
       gs_fieldcat TYPE slis_fieldcat_alv.

DATA : BEGIN OF it_file_dlmtd OCCURS 0,
         record(200) TYPE c,
       END OF it_file_dlmtd.

DATA : BEGIN OF it_file OCCURS 0,
          perid(9)              TYPE c,
          accno(10)             TYPE c VALUE '0029595780',
          actdt(6)              TYPE c,
          indic(1)              TYPE c VALUE 'N',
          name(27)              TYPE c,
          stret(30)             TYPE c,
          city(20)              TYPE c,
          state(2)              TYPE c,
          zip(9)                TYPE c,
          efein(9)              TYPE c VALUE '371426698',
          ename(20)             TYPE c VALUE 'HMMA',
          eaddr(14)             TYPE c VALUE '700 Hyundai Bl',
          ecity(11)             TYPE c VALUE 'Montgomery',
          estat(2)              TYPE c VALUE 'AL',
          ezip(5)               TYPE c VALUE '36105',
          blank(25)             TYPE c,
       END OF it_file.

DATA : BEGIN OF it_data OCCURS 0,
          pernr                 LIKE pernr-pernr,
          nachn                 LIKE pa0002-nachn,
          vorna                 LIKE pa0002-vorna,
          midnm                 LIKE pa0002-midnm,
          stras                 LIKE tmlfgt-bezei,
          locat                 LIKE tmlfgt-bezei,
          ort01                 LIKE tmlfgt-bezei,
          state                 LIKE pa0006-state,
          pstlz                 LIKE pa0006-pstlz,
          perid                 LIKE pa0002-perid,
          hired                 LIKE pa0000-begda,
       END OF it_data.

DATA: BEGIN OF it_hiredate OCCURS 0,
        pernr TYPE pa0000-pernr,
        endda TYPE pa0000-endda,
        begda TYPE pa0000-begda,
        massn TYPE pa0000-massn,
      END OF it_hiredate.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR it_data-pernr,
                s_orgeh FOR pa0001-orgeh,
                s_werks FOR pa0001-werks,
                s_btrtl FOR pa0001-btrtl,
                s_persg FOR pa0001-persg,
                s_persk FOR pa0001-persk,
                s_stat2 FOR pa0000-stat2,
                s_hired FOR it_hiredate-begda.
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

INITIALIZATION.
  CONCATENATE c_dpath c_dfile sy-datum sy-uzeit c_dextn INTO p_file.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'CHK'.
      IF NOT p_test IS INITIAL.
        screen-active = 0.
        MODIFY SCREEN.
      ELSEIF screen-name = 'P_APPL'.
        screen-input = 0.
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

  IF NOT p_appl IS INITIAL.
    MESSAGE e000 WITH text-m01 text-m02.
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
  DATA: l_tabix TYPE sy-tabix.

  SELECT a~pernr c~nachn c~vorna c~midnm d~stras d~locat d~ort01 d~state
         d~pstlz c~perid

    INTO CORRESPONDING FIELDS OF TABLE it_data

    FROM pa0000 AS a JOIN pa0001 AS b
                       ON b~pernr = a~pernr

                     JOIN pa0002 AS c
                       ON c~pernr = a~pernr

                     LEFT JOIN pa0006 AS d
                       ON d~pernr = a~pernr
                      AND d~endda = '99991231'
                      AND d~subty = '5'

   WHERE a~pernr IN s_pernr
     AND a~endda = '99991231'
     AND b~endda = '99991231'
     AND c~endda = '99991231'
     AND b~orgeh IN s_orgeh
     AND b~werks IN s_werks
     AND b~btrtl IN s_btrtl
     AND b~persg IN s_persg
     AND b~persk IN s_persk
     AND a~stat2 IN s_stat2.

* Check the hire date
  IF NOT it_data[] IS INITIAL.
    SELECT pernr endda begda massn INTO TABLE it_hiredate
      FROM pa0000
      FOR ALL ENTRIES IN it_data
     WHERE pernr = it_data-pernr
       AND ( massn = 'Z0' OR massn = 'Z1' OR
             massn = 'Z6' OR massn = 'Z9' ).
    SORT it_hiredate BY pernr endda DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_hiredate COMPARING pernr.

    LOOP AT it_data.
      l_tabix = sy-tabix.
      READ TABLE it_hiredate WITH KEY pernr = it_data-pernr
                                  BINARY SEARCH.
      IF sy-subrc = 0.
        IF it_hiredate-begda IN s_hired.
          it_data-hired = it_hiredate-begda.
          MODIFY it_data INDEX l_tabix TRANSPORTING hired.
        ELSE.
          DELETE it_data INDEX l_tabix.
        ENDIF.
      ELSE.
        DELETE it_data INDEX l_tabix.
      ENDIF.
    ENDLOOP.

    FREE it_hiredate.
  ENDIF.

ENDFORM.                    " SELECT_DATA

*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       Process Data
*----------------------------------------------------------------------*
FORM modify_data .
  DATA: l_rec     TYPE i.

* Move data to output structure
  DESCRIBE TABLE it_data LINES l_rec.
  LOOP AT it_data.
    PERFORM show_progress2 USING 'Processing Data...' l_rec.

* Move relevant data
    it_file-perid = it_data-perid.

    CONCATENATE it_data-hired+4(2)
                it_data-hired+6(2)
                it_data-hired+2(2)
           INTO it_file-actdt.

    CONCATENATE it_data-nachn
                it_data-vorna
                it_data-midnm(1)
           INTO it_file-name SEPARATED BY '/'.

    CONCATENATE it_data-stras
                it_data-locat
           INTO it_file-stret SEPARATED BY space.

    it_file-city = it_data-ort01.

    it_file-state = it_data-state.

    it_file-zip = it_data-pstlz.
    IF NOT it_file-zip IS INITIAL.
      OVERLAY it_file-zip WITH '     0000'.
    ENDIF.

    APPEND it_file.
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
  DATA: l_totrec TYPE i,
        l_filter(50) TYPE c VALUE 'des -e -k abc123forme'.

  LOOP AT it_file.
    it_file_dlmtd = it_file.

    APPEND it_file_dlmtd. CLEAR it_file_dlmtd.
  ENDLOOP.

  FREE it_file.

  DESCRIBE TABLE it_file_dlmtd LINES l_totrec.

  CASE 'X'.
    WHEN p_appl.

      OPEN DATASET v_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
           FILTER l_filter.
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
          filename                  = v_file
          trunc_trailing_blanks_eol = ' '
        TABLES
          data_tab                  = it_file_dlmtd
        EXCEPTIONS
          file_write_error          = 1
          no_batch                  = 2
          gui_refuse_filetransfer   = 3
          invalid_type              = 4
          no_authority              = 5
          unknown_error             = 6
          header_not_allowed        = 7
          separator_not_allowed     = 8
          filesize_not_allowed      = 9
          header_too_long           = 10
          dp_error_create           = 11
          dp_error_send             = 12
          dp_error_write            = 13
          unknown_dp_error          = 14
          access_denied             = 15
          dp_out_of_memory          = 16
          disk_full                 = 17
          dp_timeout                = 18
          file_not_found            = 19
          dataprovider_exception    = 20
          control_flush_error       = 21
          OTHERS                    = 22.

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
FORM fieldcat_init USING ft_fieldcat TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'IT_FILE'
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = ft_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Set Key field.
  LOOP AT ft_fieldcat INTO gs_fieldcat
                     WHERE fieldname = 'PERNR'
                        OR fieldname = 'NACHN'
                        OR fieldname = 'VORNA'.
    gs_fieldcat-key = 'X'.
    MODIFY ft_fieldcat FROM gs_fieldcat TRANSPORTING key.
  ENDLOOP.

* Change Description
  PERFORM change_desc USING 'PERID' 'SSN'.
  PERFORM change_desc USING 'ACCNO' 'Account No'.
  PERFORM change_desc USING 'ACTDT' 'Act.Date'.
  PERFORM change_desc USING 'INDIC' 'Indicator'.
  PERFORM change_desc USING 'NAME'  'Employee''s Name'.
  PERFORM change_desc USING 'STRET' 'Employee''s Street Address'.
  PERFORM change_desc USING 'CITY'  'Employee''s City Name'.
  PERFORM change_desc USING 'STATE' 'Employee''s State Name'.
  PERFORM change_desc USING 'ZIP'   'Employee''s ZIP Code'.
  PERFORM change_desc USING 'EFEIN' 'Employer''s FEIN'.
  PERFORM change_desc USING 'ENAME' 'Employer''s Name'.
  PERFORM change_desc USING 'EADDR' 'Employer''s Address'.
  PERFORM change_desc USING 'ECITY' 'Employer''s City'.
  PERFORM change_desc USING 'ESTAT' 'Employer''s State'.
  PERFORM change_desc USING 'EZIP'  'Employer''s ZIP Code'.
  PERFORM change_desc USING 'BLANK' 'In-House Use'.

ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab TYPE table.

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
FORM browser CHANGING filename TYPE c answer TYPE i.
  DATA  $filename TYPE string.
  DATA: l_path  TYPE string,
        l_fpath TYPE string,
        l_dfile TYPE string.

  CONCATENATE c_dfile sy-datum sy-uzeit c_dextn INTO l_dfile.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select File Name'
      default_extension = 'txt'
      default_file_name = l_dfile
      file_filter       = 'TXT (*.txt)|*.txt| All (*.*)|*.*'
      initial_directory = c_dpath
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
  IF it_file[] IS INITIAL.
    MESSAGE s020.
    STOP.
  ENDIF.

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
FORM display_unix CHANGING filename TYPE c answer TYPE i.
  DATA: BEGIN OF it_filename OCCURS 0,
          path(1024) TYPE c,
        END OF it_filename.

  CONCATENATE c_unix_pre sy-sysid c_unix_suf INTO it_filename-path.
  APPEND it_filename.

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
    CONCATENATE filename '/' c_dfile sy-datum sy-uzeit c_dextn
           INTO filename.
  ELSE.
    MESSAGE s549(fibl).
  ENDIF.
ENDFORM.                    " display_unix
*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
*      -->P_FIELD   Field Name
*      -->P_DESC    Field Description
*----------------------------------------------------------------------*
FORM change_desc  USING    p_field TYPE c
                           p_desc  TYPE c.

  DATA: gs_fieldcat TYPE slis_fieldcat_alv.

  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    gs_fieldcat-seltext_l    = p_desc.
    gs_fieldcat-seltext_m    = p_desc.
    gs_fieldcat-seltext_s    = p_desc.
    gs_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS2
*&---------------------------------------------------------------------*
*       Show progress indicator
*----------------------------------------------------------------------*
*      -->PF_TEXT  Display Text
*      -->PF_VAL   Percentage calculated base
*----------------------------------------------------------------------*
FORM show_progress2 USING   pf_text TYPE c
                            pf_val  TYPE i.

  DATA:    l_prctx(3) TYPE c,
           l_tex1(50) TYPE c.

  STATICS: l_text(50) TYPE c,
           l_baseval  TYPE i,
           l_percent  TYPE i,
           l_counter  TYPE i.

  IF l_text NE pf_text.
    l_text = pf_text.
    CLEAR: l_baseval,
           l_percent,
           l_counter.
  ENDIF.

  IF NOT l_baseval IS INITIAL.
    l_counter = l_counter - 1.
    CHECK l_counter LE 0.
    l_percent = l_percent + 10.
    CHECK l_percent LE 100.
    l_counter = l_baseval.
  ELSE.
    l_baseval = pf_val DIV 10.
    l_counter = l_baseval - 1.
  ENDIF.

  IF NOT pf_val IS INITIAL.
    IF NOT l_percent IS INITIAL.
      l_prctx = l_percent.
    ELSE.
      l_prctx = 1.
      l_percent = 1.
    ENDIF.

    CONCATENATE pf_text l_prctx '%' INTO l_tex1 SEPARATED BY space.
  ELSE.
    l_tex1 = pf_text.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_percent
      text       = l_tex1.

  IF l_percent = 1.
    CLEAR l_percent.
  ENDIF.
ENDFORM.                    " SHOW_PROGRESS2
