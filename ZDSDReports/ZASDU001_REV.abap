************************************************************************
* Program Name      : ZASDU001_REV
* Author            : IG.Moon
* Creation Date     : 5/23/2008
* Specifications By : Venkat Dyava
* Pattern           : BDC
* Description       : Mass Credit Memo Processing
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT zasdu001 MESSAGE-ID zmco.
INCLUDE zasdui00.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
TYPE-POOLS: kcde.

* Tables
TABLES : csks, vbrk, *vbrp, vbuk, *a004.


* {
DATA:   BEGIN OF tkomk OCCURS 10.
        INCLUDE STRUCTURE komk.
DATA:   END   OF tkomk.

DATA:   BEGIN OF xkomfk OCCURS 0.
        INCLUDE STRUCTURE komfk.
DATA:   END   OF xkomfk.

DATA:    BEGIN OF xvbfs OCCURS 20.
        INCLUDE STRUCTURE vbfs.
DATA:    END OF xvbfs.

DATA:    BEGIN OF xvbrk OCCURS 10.
        INCLUDE STRUCTURE vbrkvb.
DATA:    END OF xvbrk.

DATA:  BEGIN OF xvbrp OCCURS 100.
        INCLUDE STRUCTURE vbrpvb.
DATA:  END OF xvbrp.

DATA: BEGIN OF xvbpa OCCURS 0.
        INCLUDE STRUCTURE vbpavb.
DATA: END OF xvbpa.

DATA:   BEGIN OF xkomv OCCURS 50.
        INCLUDE STRUCTURE komv.
DATA:   END   OF xkomv.

*}

TYPES: BEGIN OF ty_condi.
TYPES   kschl  TYPE kscha.
TYPES: END OF ty_condi.

* Value List for Condition Type
DATA: BEGIN OF con_list OCCURS 0,
          kschl LIKE t685t-kschl,
          vtext LIKE t685t-vtext,
      END OF con_list.

* For creating the help list on selection screen
DATA: BEGIN OF help_field OCCURS 0.
        INCLUDE STRUCTURE help_value.
DATA: END OF help_field.

DATA: BEGIN OF help_vtab OCCURS 0.
        INCLUDE STRUCTURE help_vtab.
DATA: END OF help_vtab.

DATA: BEGIN OF help_value OCCURS 0,
      value LIKE help_vtab-value,
      END OF help_value.

DATA: BEGIN OF it_file OCCURS 0,
         vbeln(10),
         kbetr(15),
      END OF it_file.

TYPES: BEGIN OF ty_row_tab.
        INCLUDE STRUCTURE zssdu001.
TYPES:
       icon TYPE icon-id,
       msg(100),
       err,
       chk(1).
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES   celltab  TYPE lvc_t_styl.
TYPES   tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

DATA : BEGIN OF it_post  OCCURS 0.
        INCLUDE STRUCTURE  zssdu001.
DATA : END OF   it_post.

DATA : gt_return         LIKE STANDARD TABLE OF bapiret2
                         WITH HEADER LINE.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        gt_condi   TYPE TABLE OF ty_condi   WITH HEADER LINE.

DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.

DEFINE __process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE __cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
END-OF-DEFINITION.

DEFINE __u_break.
  if err_brk eq true.
    break-point.
  endif.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ztcou131_k,
              co_area   TYPE kokrs,
              fisc_year TYPE gjahr,
              version   TYPE versn,
              kostl     TYPE kostl,
              kstar     TYPE kstar,
           END OF ztcou131_k.

    TYPES: ztcou131_key   TYPE STANDARD TABLE OF ztcou131_k,
           ztcou131_table TYPE STANDARD TABLE OF ztcou131.

    METHODS:
      handle_data_changed
         FOR EVENT data_changed OF cl_gui_alv_grid
             IMPORTING er_data_changed,
                       get_deleted_rows
             EXPORTING
                       deleted_rows TYPE ztcou131_table,

      refresh_delta_tables.

  PRIVATE SECTION.
    DATA deleted_rows TYPE STANDARD TABLE OF ztcou131.

* This flag is set if any error occured in one of the
* following methods:
    DATA: error_in_data TYPE c.
    METHODS:
      update_delta_tables
         IMPORTING
            pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Setting for Change data
  METHOD handle_data_changed.

* remember deleted lines for saving
    CALL METHOD update_delta_tables( er_data_changed ).

    PERFORM data_changed USING er_data_changed.
  ENDMETHOD.                    " handle_data_changed

  METHOD get_deleted_rows.
    deleted_rows = me->deleted_rows.
  ENDMETHOD.

  METHOD refresh_delta_tables.
    CLEAR me->deleted_rows[].
  ENDMETHOD.

  METHOD update_delta_tables.
    DATA: l_del_row TYPE lvc_s_moce,
          ls_ztcou131 TYPE ztcou131,
          ls_outtab LIKE LINE OF gt_out.

    LOOP AT pr_data_changed->mt_deleted_rows INTO l_del_row.
      READ TABLE gt_out INTO ls_outtab INDEX l_del_row-row_id.
      IF sy-subrc NE 0.
        MESSAGE i000(0k) WITH text-e01. "Internal error
      ELSE.
        MOVE-CORRESPONDING ls_outtab TO ls_ztcou131.
        APPEND ls_ztcou131 TO deleted_rows.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

************************************************************************
DATA  : flag_data_changed,
        info(80).
DATA: BEGIN OF ftab OCCURS 10,
        fcode(6),
      END OF ftab.
****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
PARAMETERS p_date LIKE sy-datum DEFAULT sy-datum.
PARAMETERS p_kschl TYPE kschl DEFAULT 'ZB01'  NO-DISPLAY. " OBLIGATORY
SELECTION-SCREEN END OF BLOCK bl1.

* block 3 {
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 35(12)  text-x00 FOR FIELD p_exl
                                 MODIF ID exl.
PARAMETERS p_exl   RADIOBUTTON GROUP radi DEFAULT 'X'
                                 MODIF ID exl.
SELECTION-SCREEN COMMENT 55(21) text-x01
                                 MODIF ID exl.
PARAMETERS p_txt     RADIOBUTTON GROUP radi
                                 MODIF ID exl.
SELECTION-SCREEN END   OF LINE.

PARAMETERS p_head AS CHECKBOX MODIF ID exl DEFAULT 'X'.
PARAMETERS: p_file  LIKE rlgrap-filename OBLIGATORY
                    DEFAULT 'c:\temp\upload.xls'
                    MODIF ID exl.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK bx WITH FRAME TITLE text-00x.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(69) text-t04 MODIF ID brt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bx.
SELECTION-SCREEN END   OF BLOCK b3.
* }

SELECTION-SCREEN BEGIN OF BLOCK b8 WITH FRAME TITLE text-02s.
PARAMETERS p_mode DEFAULT 'N'.
PARAMETERS p_test NO-DISPLAY.
PARAMETERS err_brk AS CHECKBOX.
PARAMETERS do_logic AS CHECKBOX DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 40(30)  text-xde .
SELECTION-SCREEN END OF BLOCK b8.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM browser CHANGING p_file.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM initialize.

* Upload File
  PERFORM upload_file USING p_file.

  PERFORM move_out.

  PERFORM set_output .

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

* Log.
*  PERFORM DISPLAY_LOG.


*----------------------------------------------------------------------*
* Sub-Rutines
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BROWSING_FILE_PATH
*&---------------------------------------------------------------------*
*       Browsing File Paths
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM browsing_file_path.
* Browsing
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            mask          = '*.xls'
            static        = 'X'
       CHANGING
            file_name     = p_file
       EXCEPTIONS
            mask_too_long = 1
            OTHERS        = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " BROWSING_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  CHK_FILE_EXISTENCY
*&---------------------------------------------------------------------*
*       Check File Existency
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_file_existency.
* Check File Existency
  DATA : lv_exist.
  CLEAR  lv_exist.
  CALL FUNCTION 'TMP_GUI_GET_FILE_EXIST'
    EXPORTING
      fname                = p_file
    IMPORTING
      exist                = lv_exist
*     ISDIR                =
*     FILESIZE             =
    EXCEPTIONS
      fileinfo_error       = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF lv_exist NE 'X'.
    MESSAGE e075 WITH p_file.
  ENDIF.

ENDFORM.                    " CHK_FILE_EXISTENCY

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       Upload File data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file USING filename.

  IF p_file EQ space.
    g_error = true.
    EXIT.
  ENDIF.

  IF p_file EQ space.
    g_error = true.
    EXIT.
  ENDIF.

  DATA: it_itab LIKE STANDARD TABLE OF alsmex_tabline WITH HEADER LINE.
  FIELD-SYMBOLS : <fs>.
  DATA : v_index TYPE i.
  DATA : begin_row TYPE i VALUE 1.

  __process 'Upload file...' '10'.
  IF p_head = true.
    ADD 1 TO begin_row.
  ENDIF.

  IF p_txt NE true.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
         EXPORTING
              filename                = filename
              i_begin_col             = 1
              i_begin_row             = begin_row
              i_end_col               = 2
              i_end_row               = 65535
         TABLES
              intern                  = it_itab
         EXCEPTIONS
              inconsistent_parameters = 1
              upload_ole              = 2
              OTHERS                  = 3.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'Could not find the file.'.
      STOP.
    ENDIF.

    __process 'Upload file...' '20'.

    IF it_itab[] IS INITIAL.
      MESSAGE s003(zz) WITH 'No Data was uploaded'.
      g_error = true .
      EXIT.
    ELSE.
      SORT it_itab BY row col.
      LOOP AT it_itab.
        MOVE : it_itab-col TO v_index.
        ASSIGN COMPONENT v_index OF STRUCTURE it_file TO <fs>.
        MOVE : it_itab-value TO <fs>.
        AT END OF row.
          APPEND it_file.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ELSE.
    DATA cancel.
    CALL FUNCTION 'UPLOAD'
         EXPORTING
              filename            = filename
              filetype            = 'DAT'
         IMPORTING
              cancel              = cancel
         TABLES
              data_tab            = it_file
         EXCEPTIONS
              conversion_erro     = 1
              invalid_table_width = 2
              invalid_type        = 3.

    IF NOT cancel IS INITIAL OR sy-subrc NE 0.
      MESSAGE s003(zz) WITH 'No Data was uploaded'.
      STOP.
    ENDIF.

  ENDIF.

  __process 'Check data...' '30'.

  LOOP AT it_file.
    IF it_file-vbeln IS INITIAL.
      DELETE it_file.
      CONTINUE.
    ENDIF.

    PERFORM check_num CHANGING it_file-kbetr.

    MODIFY it_file.

  ENDLOOP.

  __cls it_row_tab.
  LOOP AT it_file.
    MOVE-CORRESPONDING it_file TO it_row_tab.
    it_row_tab-fkdat = p_date.
    APPEND it_row_tab.
  ENDLOOP.
ENDFORM.                    " UPLOAD_FILE


*&---------------------------------------------------------------------*
*&      Form  PRE_FOR_POSTING
*&---------------------------------------------------------------------*
*       Preparation for posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_for_posting TABLES p_tab STRUCTURE gt_out.
  DATA l_no(3) TYPE n.

  CLEAR  it_file.

  DATA l_filename(40).

* Collect Data
  __cls it_post.
  LOOP AT p_tab.
    CLEAR   it_post.
    it_post-vbeln           = p_tab-vbeln.
    it_post-kbetr           = p_tab-kbetr.
    it_post-fkdat           = p_tab-fkdat.
    APPEND it_post.
  ENDLOOP.

* Sorting
  CLEAR   it_post.
  SORT it_post BY vbeln.

ENDFORM.                    " PRE_FOR_POSTING

*&---------------------------------------------------------------------*
*&      Form  POST_PL_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_ce.

  DATA: str_date(10).
  DATA : BEGIN OF it_vbrp OCCURS 0,
          matnr TYPE matnr,
         END OF it_vbrp.

  DATA $vbeln(10) TYPE n.
  DATA lt_condi TYPE TABLE OF ty_condi WITH HEADER LINE.
  DATA $kschl_text(30).
  DATA $selkz_text(30).


  DATA $kschl_indx(2) TYPE n.
  DATA $ix LIKE sy-tabix.

  DATA: msg LIKE cfgnl-msglin.
  DATA: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA:   BEGIN OF it_kposn OCCURS 0,
          kposn TYPE kposn,
          END   OF it_kposn.


  SORT it_post BY vbeln.

  LOOP AT it_post.
    $ix = sy-tabix.

    __cls : it_vbrp, lt_condi,xkomv, tkomk .

    MOVE it_post-vbeln TO $vbeln.

    vbrk-vbeln = $vbeln.

    SELECT SINGLE * INTO vbrk
    FROM vbrk WHERE vbeln EQ $vbeln.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'Could not find #' $vbeln.
      CONTINUE.
    ENDIF.

    tkomk-belnr = vbrk-vbeln.
    tkomk-knumv = vbrk-knumv.
    tkomk-vkorg = vbrk-vkorg.

    CALL FUNCTION 'RV_KONV_SELECT'
         EXPORTING
              comm_head_i           = tkomk
              general_read          = 'X'
              read_condition_record = 'X'
         TABLES
              tkomv                 = xkomv.
    IF sy-subrc <> 0.
    ENDIF.

    PERFORM get_condition TABLES xkomv lt_condi.

    __cls: gt_bdc, gt_msg.

    PERFORM convert_date USING it_post-fkdat CHANGING str_date.

    PERFORM dynpro USING:
     'X'  'SAPMV60A'        '0102',
     ' '  'BDC_OKCODE'      '/00',
     ' '  'RV60A-FKART'     'ZVL2',
     ' '  'RV60A-FKDAT'     str_date,
     ' '  'KOMFK-VBELN(01)' it_post-vbeln.

    PERFORM dynpro USING:
     'X'  'SAPMV60A'        '0103',
     ' '  'BDC_CURSOR'      '*TVFKT-VTEXT(02)',
     ' '  'BDC_OKCODE'     '=UEBP'.

    READ TABLE lt_condi WITH KEY kschl = 'ZB01' BINARY SEARCH.
    IF sy-subrc EQ 0.
      $kschl_indx = sy-tabix.

      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_CURSOR'      'VBRK-FKART',
       ' '  'BDC_OKCODE'     '=KFKO'.

      CONCATENATE 'KOMV-KSCHL(' $kschl_indx ')' INTO $kschl_text.
      CONDENSE $kschl_text.

      CONCATENATE 'RV61A-SELKZ(' $kschl_indx ')' INTO $selkz_text.
      CONDENSE $selkz_text.

      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '6001',
       ' '  'BDC_OKCODE'     '=V69A_KOLO',
       ' '  'BDC_SUBSCR'
       'SAPMV60A                                6011SUBSCREEN_HEADER',
       ' '  'BDC_SUBSCR'
       'SAPLV69A                                6201SUBSCREEN_BODY',
       ' '  'BDC_CURSOR'      $kschl_text,
       ' '  $selkz_text       'X'.

      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '6001',
       ' '  'BDC_OKCODE'     '=BACK',
       ' '  'BDC_SUBSCR'
       'SAPMV60A                                6011SUBSCREEN_HEADER',
       ' '  'BDC_SUBSCR'
       'SAPLV69A                                6201SUBSCREEN_BODY',
       ' '  'BDC_CURSOR'      'KOMV-KSCHL(05)'.

    ENDIF.

    SORT xkomv BY kposn.

    DATA $flag.
    DATA $line TYPE i.
    DATA $indx(2) TYPE n.
    DATA $rate(15).
    DATA $cnt TYPE i.

    READ TABLE xkomv INDEX 1.
    IF sy-subrc EQ 0.
      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_OKCODE'     '=MKAL'.
    ENDIF.

    __cls it_kposn.
    LOOP AT xkomv.
      it_kposn-kposn = xkomv-kposn.
      APPEND it_kposn.
    ENDLOOP.

    SORT it_kposn.
    DELETE ADJACENT DUPLICATES FROM it_kposn COMPARING kposn.

    DATA : prc_err, no_prc.

    DELETE it_kposn WHERE kposn IS initial.

    LOOP AT it_kposn.

      CHECK NOT it_kposn-kposn IS INITIAL.

      CLEAR : prc_err, no_prc.

      IF do_logic EQ true.
        SELECT SINGLE * INTO *vbrp
                  FROM vbrp
                  WHERE vbeln EQ vbrk-vbeln
                    AND posnr EQ it_kposn-kposn.

        IF sy-subrc EQ 0.

          SELECT SINGLE * INTO *a004
              FROM a004
           WHERE kappl = 'V'
             AND kschl = 'ZV00'
             AND matnr = *vbrp-matnr
             AND datab =< *vbrp-prsdt
             AND datbi >= *vbrp-prsdt.
          IF sy-subrc EQ 0.
          ELSE.
            prc_err = true.
            __u_break.
          ENDIF.
        ELSE.
          no_prc = true.
          __u_break.
        ENDIF.
      ENDIF.

      WRITE it_post-kbetr TO $rate.

      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_OKCODE'      '=PFKO'.

      IF no_prc NE true.
        IF prc_err NE true.
          PERFORM dynpro USING:
           'X'  'SAPMV60A'        '6002',
           ' '  'BDC_OKCODE'      '/00',
           ' '  'BDC_SUBSCR'
         'SAPMV60A                                6012SUBSCREEN_HEADER',
           ' '  'BDC_SUBSCR'
           'SAPLV69A                                6201SUBSCREEN_BODY',
           ' '  'BDC_CURSOR'      'KOMV-KBETR(05)',
           ' '  'KOMV-KSCHL(05)'  p_kschl,
           ' '  'KOMV-KBETR(01)'  '0',
           ' '  'KOMV-KBETR(05)'  $rate.
        ELSE.
          PERFORM dynpro USING:
           'X'  'SAPMV60A'        '6002',
           ' '  'BDC_OKCODE'      '/00',
           ' '  'BDC_SUBSCR'
         'SAPMV60A                                6012SUBSCREEN_HEADER',
           ' '  'BDC_SUBSCR'
         'SAPLV69A                                6201SUBSCREEN_BODY',
           ' '  'BDC_CURSOR'      'KOMV-KBETR(05)',
           ' '  'KOMV-KSCHL(04)'  'ZV00',
           ' '  'KOMV-KSCHL(05)'  p_kschl,
           ' '  'KOMV-KBETR(04)'  '0',
           ' '  'KOMV-KBETR(05)'  $rate.
        ENDIF.
      ENDIF.

      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '6002',
       ' '  'BDC_OKCODE'      '=BACK'.

      ADD 1 TO $cnt.

    ENDLOOP.

    IF p_test EQ false.
      PERFORM dynpro USING:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_CURSOR'      'VBRK-FKART',
       ' '  'BDC_OKCODE'      '=SICH'.
    ENDIF.

    PERFORM dynpro USING:
     'X'  'SAPMV60A'        '0104',
     ' '  'BDC_CURSOR'      'VBRK-FKART',
     ' '  'BDC_OKCODE'      '=BACK'.

    PERFORM dynpro USING:
     'X'  'SAPMV60A'        '0103',
     ' '  'BDC_OKCODE'      '=BACK'.

    __cls gt_msg.

    __u_break.

    CALL TRANSACTION 'VF01'   USING         gt_bdc
                              OPTIONS FROM  gs_opt
                              MESSAGES INTO gt_msg.

*    WAIT UP TO 10 SECONDS.

    CLEAR msg.

    PERFORM make_msg_string USING msg.

    READ TABLE gt_out WITH KEY vbeln = it_post-vbeln.
    IF sy-subrc EQ 0.
      CLEAR : gt_out-err, gt_out-msg.
      gt_out-err = 'N'.
      IF sy-msgno EQ '311'.
      ELSE.
        gt_out-err = true.
        __u_break.
      ENDIF.
      gt_out-msg = msg.
      MODIFY gt_out INDEX sy-tabix TRANSPORTING err msg.

    ENDIF.

    lv_cnt = lv_cnt + 1.

  ENDLOOP.

  IF lv_dcnt > 0 OR lv_cnt > 0.
    CONCATENATE 'Data''s been processed;'
                 lv_cnt  'rec(s).'
            INTO lv_msg SEPARATED BY space.
    MESSAGE s000 WITH lv_msg.
  ENDIF.

  CLEAR flag_data_changed.
  PERFORM  apply_icon.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Log.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.

  DATA : lv_execnt LIKE sy-tfill.
  DATA : lv_poscnt LIKE sy-tfill.

  DESCRIBE TABLE it_row_tab LINES lv_execnt.
  DESCRIBE TABLE it_post    LINES lv_poscnt.

  WRITE : / text-011 , lv_execnt.
  SKIP 1.
  WRITE : / text-012 , lv_poscnt.
  SKIP 1.
  WRITE : / text-013.

ENDFORM.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen.

  LOOP AT SCREEN.
    IF screen-group1 = 'BRT'.
      screen-intensified = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
FORM browser CHANGING filename.
  DATA: it_tfile TYPE filetable ,
        gd_subrc TYPE i.

  CALL  METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          window_title = 'Select File Name'
          default_extension = '*.*'
          default_filename = '*.*'
          file_filter = '*.*'
          initial_directory = 'c:\temp\'
*         MULTISELECTION =
*         WITH_ENCODING =
        CHANGING
          file_table = it_tfile
          rc = gd_subrc.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  IF sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE it_tfile INTO filename INDEX 1.
  ENDIF.

ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            percentage = pf_val
            text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<MONTH>  text
*----------------------------------------------------------------------*
FORM check_num CHANGING n_value.
*  DATA num(12) VALUE ' 0123456789.'.
*
*  REPLACE : '"' WITH '' INTO n_value,
*            '"' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value,
*            ',' WITH '' INTO n_value.
*  CONDENSE n_value NO-GAPS.
*  IF n_value CN num. n_value = 0. ENDIF.
ENDFORM.                    " CHECK_NUM
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
FORM data_changed USING rr_data_changed
                        TYPE REF TO cl_alv_changed_data_protocol.

  flag_data_changed = true.

  DATA: ls_mod_cells TYPE lvc_s_modi,
        ls_cells     TYPE lvc_s_modi,
        lt_values TYPE TABLE OF bapi_char_values WITH HEADER LINE.

  LOOP AT rr_data_changed->mt_good_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX ls_mod_cells-row_id.
    IF sy-subrc = 0.
      CALL METHOD rr_data_changed->modify_cell
                EXPORTING i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
    ENDIF.
  ENDLOOP.

  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_out.
  __process 'Preparing output...' '95'.

  __cls gt_out.

  LOOP AT it_row_tab.
    CLEAR gt_out.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.


ENDFORM.                    " MOVE_OUT
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_output.
  CHECK : g_error IS INITIAL.
  CLEAR flag_data_changed.
  CALL SCREEN 100.

ENDFORM.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET TITLEBAR '100'.
*   Exclude toolbar
  PERFORM exclude_functions.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions.
  PERFORM append_exclude_functions
           TABLES gt_exclude[]
           USING: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.
*   Display alv grid
    CALL METHOD g_grid->set_table_for_first_display
         EXPORTING is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         CHANGING  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  ELSE.
    CALL METHOD g_grid->refresh_table_display.
  ENDIF.
  __focus g_grid.
  PERFORM user_status.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CLEAR : g_error.

  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.

    WHEN 'BACK' OR 'CANC'.
      PERFORM free_container.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      CHECK sy-dynnr EQ '0100'.
      PERFORM really?.
      CHECK g_error NE true.

      PERFORM : apply_cre,
                refresh_alv.
      __focus g_grid.

    WHEN 'SWITCH'.
      IF sy-dynnr EQ '0100'.
        PERFORM switch_edit_mode.
      ENDIF.
      __focus g_grid.

    WHEN 'LOGV'.
      CALL SCREEN '300'.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_container.
  IF NOT g_event_receiver IS INITIAL.
    FREE g_event_receiver.
  ENDIF.

  IF NOT g_grid IS INITIAL.
    CALL METHOD g_grid->free.
  ENDIF.

  IF NOT g_custom_container IS INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  FREE : g_grid,g_custom_container.

  CLEAR :  gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].


ENDFORM.                    " FREE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM really?.
  DATA $exists(1).
  DATA l_answer(1).

  PERFORM pop_up USING
      'This will change Credit Memo massively.'
      'Do you really want to proceed?' ' '
                 CHANGING l_answer.

  IF l_answer NE 'J'.
    g_error = true.
    MESSAGE s000 WITH 'Processing was canceled by user.'.
  ENDIF.
ENDFORM.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1886   text
*      -->P_1887   text
*      -->P_1888   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
FORM pop_up USING    p_text p_text2 p_canc
            CHANGING p_answer.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
       EXPORTING
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       IMPORTING
            answer         = p_answer.


ENDFORM.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_alv.
  __set_refresh_mode true.
  CALL METHOD g_grid->refresh_table_display
       EXPORTING is_stable = stable.

ENDFORM.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv.
*   Create object
  PERFORM create_object.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

  CALL METHOD g_grid->register_edit_event
       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid->set_ready_for_input
     EXPORTING
            i_ready_for_input = 0.

  PERFORM sort_build USING gt_sort[].

*   Setting for layout
  PERFORM set_lvc_layout.

*   Set colors
  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
*  gs_variant-variant = p_vari.

*   Define cell attribute
  PERFORM build_cell_attr.


ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category USING mode_edit.
  DATA: l_pos       TYPE i.
  DEFINE __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  DATA : $ix(2) TYPE n,
         $mtxt(6).

  __catalog :
    'X'  'VBELN'             'DOC #'            10 'CHAR' '',
    ' '  'KBETR'             'Amount'           15 'CURR' '',
    ' '  'FKDAT'             'Date'              8 'DATS' '',
    ' '  'ICON'              'flg'               4 'ICON' '',
    ' '  'MSG' 'Remarks                               .' 80
              'CHAR' ''.

  LOOP AT gt_fcat INTO gs_fcat.
    IF gs_fcat-fieldname EQ 'KBETR'.
      gs_fcat-just = 'R'.
    ENDIF.
    gs_fcat-ref_table = 'ZSSDU001'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.
    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING ft_sort TYPE lvc_t_sort.
  DEFINE sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  END-OF-DEFINITION.

  sort_tab :
      'VBELN'             ' ' 'X' '' 'X' ''.
ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout.

  CLEAR gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.
ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color.
  DATA : $ix(2) TYPE n,
         $mtxt(6).

  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  DEFINE __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  END-OF-DEFINITION.

  __color :
            'VBELN'            '1' 0,
            'KBETR'            '2' 0,
            'FKDAT'            '2' 0.


  __color :
            'ICON'    '1' 0,
            'MSG'     '1' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr.
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl.

  CLEAR lt_celltab.
  REFRESH lt_celltab.

  CLEAR gs_fcat.

  LOOP AT gt_fcat INTO gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    IF ls_celltab-fieldname EQ 'KBETR' OR
          ls_celltab-fieldname EQ 'FKDAT'.

      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    ELSE.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    ENDIF.
    INSERT ls_celltab INTO TABLE lt_celltab.
  ENDLOOP.

  CLEAR gt_out-celltab.
  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial.
  PERFORM build_cell_attr1_lock.

ENDFORM.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_cell_attr1_lock.

*  DATA: LT_CELLTAB TYPE LVC_T_STYL,
*        LS_CELLTAB TYPE LVC_S_STYL.
*
*  CLEAR LT_CELLTAB.
*  REFRESH LT_CELLTAB.
*
*  __CLS GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*
*  CLEAR GS_FCAT.
*
*  LOOP AT GT_FCAT INTO GS_FCAT.
*    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
*    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
*  ENDLOOP.
*
*  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*

ENDFORM.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode.
  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 1.

    PERFORM info_text_set USING true.
  ELSE.
**    IF FLAG_DATA_CHANGED EQ TRUE.
**      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
**           EXPORTING
**                TEXTLINE1     = 'Data has not been saved yet.'
**                TEXTLINE2     = 'Do you want to continue anyway? '
**                TITEL         = 'Confirmation'
**                DEFAULTOPTION = 'N'
**           IMPORTING
**                ANSWER        = ANSWER.
**      CHECK ANSWER EQ 'J'.
**    ENDIF.
**    CLEAR FLAG_DATA_CHANGED.
    CALL METHOD g_grid->set_ready_for_input
                     EXPORTING i_ready_for_input = 0.
    PERFORM info_text_set USING false.
  ENDIF.

  PERFORM build_cell_attr.
ENDFORM.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
FORM info_text_set USING    p_true.
  IF p_true EQ true.
    info = text-015.
  ELSE.
    info = text-015.
  ENDIF.

ENDFORM.                    " INFO_TEXT_SET
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'ZLOG'.
  sy-title = 'Error log...'.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  PERFORM error_list.
ENDMODULE.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ERROR_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM error_list.

  LOOP AT gt_return.
    WRITE:/ gt_return-message_v1(20),
            gt_return-message_v2(10),
            gt_return-message(40).
  ENDLOOP.

ENDFORM.                    " ERROR_LIST
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM user_status.
  __cls ftab.

  IF g_grid->is_ready_for_input( ) EQ 1.
    ftab-fcode = 'SAVE'.
    APPEND ftab.
  ENDIF.

  ftab-fcode = 'LOGV'.
  APPEND ftab.
  SET PF-STATUS '100' EXCLUDING ftab.
ENDFORM.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  APPLY_CCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_cre.

  __cls $gt_out.

  DATA: lt_row   TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lt_roid  TYPE lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  DATA  lt_messages      LIKE messages       OCCURS 0 WITH HEADER LINE.
  DATA  $messages      LIKE messages       OCCURS 0 WITH HEADER LINE.


* Save seleted data to table ZTCOU135
  CLEAR: lv_cnt, lt_row[], lt_roid[].

  PERFORM get_selected_rows TABLES $gt_out.

* {
*  DATA  : I_ZTCOU135 LIKE ZTCOU135 OCCURS 0 WITH HEADER LINE,
*          LS_ZTCOU135 LIKE ZTCOU135,
*          LT_DEL_ROWS TYPE TABLE OF ZTCOU135.
*
*  CALL METHOD G_EVENT_RECEIVER->GET_DELETED_ROWS
*            IMPORTING DELETED_ROWS = LT_DEL_ROWS.
* }

* Preparation for posting
  PERFORM pre_for_posting TABLES $gt_out.
  PERFORM post_ce.

ENDFORM.                    " APPLY_CCA
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
        lt_row_no TYPE lvc_t_roid. "Numeric IDs of Selected Rows

  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-subrc NE 0.
    MESSAGE e000
    WITH 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE lt_rows INDEX 1.
  IF sy-subrc NE 0.
    $gt_out[] = gt_out[].
  ELSE.
    LOOP AT lt_rows WHERE rowtype IS initial.
      READ TABLE gt_out INDEX lt_rows-index.
      gt_out-chk = true .
      MODIFY gt_out INDEX lt_rows-index .
    ENDLOOP.
    LOOP AT gt_out.
      CHECK gt_out-chk EQ true.
      $gt_out = gt_out.
      APPEND $gt_out.
    ENDLOOP.
  ENDIF.

  gt_out-chk = false .
  MODIFY gt_out TRANSPORTING chk WHERE chk EQ true.

ENDFORM.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize.

  CLEAR : g_error.
  __cls : it_row_tab.

* Get BDC Options
  PERFORM get_opt USING p_mode.


ENDFORM.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  convert_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_POST_FKDAT  text
*      <--P_STR_DATE  text
*----------------------------------------------------------------------*
FORM convert_date  USING    f_date  LIKE sy-datum
                   CHANGING f_dtout TYPE char10.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = f_date
       IMPORTING
            date_external            = f_dtout
       EXCEPTIONS
            date_internal_is_invalid = 1
            OTHERS                   = 2.

ENDFORM.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      Form  get_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XKOMV  text
*      -->P_LT_CONDI  text
*----------------------------------------------------------------------*
FORM get_condition TABLES   p_xkomv STRUCTURE xkomv
                            p_lt_condi STRUCTURE gt_condi.


  __cls p_lt_condi.

  LOOP AT p_xkomv.
    p_lt_condi-kschl =  p_xkomv-kschl.
    COLLECT p_lt_condi.
  ENDLOOP.

  SORT p_lt_condi.

ENDFORM.                    " get_condition
*&---------------------------------------------------------------------*
*&      Form  make_msg_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG  text
*----------------------------------------------------------------------*
FORM make_msg_string USING    p_msg.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = p_msg.

ENDFORM.                    " MAKE_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM apply_icon.
  DATA $ix LIKE sy-tabix.

  LOOP AT gt_out.
    $ix = sy-tabix.

    CASE gt_out-err.
      WHEN 'X'.
        gt_out-icon = icon_led_red.
      WHEN 'N'.
        gt_out-icon = icon_led_green.
      WHEN OTHERS.
        CLEAR gt_out-icon.
    ENDCASE.

    MODIFY gt_out INDEX $ix TRANSPORTING icon .
  ENDLOOP.

ENDFORM.                    " apply_icon
*&---------------------------------------------------------------------*
*&      Form  kschl_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_KSCHL  text
*----------------------------------------------------------------------*
FORM kschl_input_help CHANGING p_p_kschl.
  DATA j LIKE sy-index.
  __cls con_list.

  SELECT kschl vtext
  INTO TABLE con_list
  FROM t685t
  WHERE spras = 'EN'
    AND kvewe = 'A'
    AND kappl = 'V'.

  SORT con_list BY kschl .

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'KSCHL'.
  help_field-selectflag = 'X'.
  APPEND help_field.

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'VTEXT'.
  help_field-selectflag = ' '.
  APPEND help_field.

  LOOP AT con_list.
    help_value-value = con_list-kschl.
    APPEND help_value.
    help_value-value = con_list-vtext.
    APPEND help_value.
  ENDLOOP.

  PERFORM value_help CHANGING j.

  IF j > 0.
    READ TABLE con_list INDEX j.
    p_p_kschl = con_list-kschl.
  ENDIF.

  dynpfields-fieldname  = 'KSCHL'.
  dynpfields-fieldvalue = con_list-kschl.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname     = sy-cprog
            dynumb     = sy-dynnr
       TABLES
            dynpfields = dynpfields.

  CLEAR: dynpfields.
  REFRESH: con_list, help_field, help_vtab, help_value, dynpfields.
ENDFORM.                    " KSCHL_INPUT_HELP

*---------------------------------------------------------------------*
*       FORM value_help                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_J                                                           *
*---------------------------------------------------------------------*
FORM value_help CHANGING p_j.
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
       EXPORTING
            display       = ' '
       IMPORTING
            index         = p_j
       TABLES
            fields        = help_field
            select_values = help_vtab
            valuetab      = help_value.
ENDFORM.                               " VALUE_HELP
