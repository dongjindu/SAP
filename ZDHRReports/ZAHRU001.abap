************************************************************************
* Program Name      : ZAHRU001
* Author            : IG.Moon
* Creation Date     : 12/09/2008
* Specifications By : Euna Lee
* Pattern           : Upload and Gather from tables
* Description       : Verification benefit data for Health Plan
* Modifications Log
* Date   Developer   Request ID    Description
************************************************************************
REPORT zahru001 MESSAGE-ID zmco.
INCLUDE zahrui00.
*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
TYPE-POOLS: kcde.

* Tables
TABLES : pa0002, pa0106.

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
*Group/Div	GRSI	Contract #	Member	DOB	SSN	Sex	Relation

TYPE-POOLS: truxs.

TYPES: BEGIN OF t_datatab,
         grp_div(9),
         grsi(9),
         con_no(12),
         member(40),
         dob(10),
         ssn(9),
         sex(1),
         relat(40),
      END OF t_datatab.
DATA: it_datatab TYPE STANDARD TABLE OF t_datatab,
      wa_datatab TYPE t_datatab.

DATA: it_raw TYPE truxs_t_text_data.

TYPES: BEGIN OF ty_row_tab,
         con_no(12),
         level(2) TYPE n,
         member(40),
         dob(10)," LIKE pa0002-gbdat,
         ssn LIKE pa0002-perid,
         sex(1),
         relat(40),
         grp_div(9),
         grsi(9),
         pernr LIKE pa0000-pernr,
         err,
         eperid(10),
         egbdat(10),
         line_no(10) TYPE n.

TYPES: END OF ty_row_tab.

TYPES BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES msg(100).
TYPES END OF ty_out.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA  : it_excel TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE.

DATA: BEGIN OF it_status OCCURS 0,
        pernr LIKE pa0000-pernr,
        begda LIKE pa0000-begda,
        massn LIKE pa0000-massn,
        massg LIKE pa0000-massg,
        stat2 LIKE pa0000-stat2,
      END OF it_status           .

DATA : BEGIN OF i_pa0167 OCCURS 0,
        pernr LIKE pa0167-pernr,
        begda LIKE pa0167-begda,
        endda LIKE pa0167-endda,
        depcv LIKE pa0167-depcv,
        dty01 LIKE pa0167-dty01,
        dty02 LIKE pa0167-dty02,
        dty03 LIKE pa0167-dty03,
        dty04 LIKE pa0167-dty04,
        dty05 LIKE pa0167-dty05,
        dty06 LIKE pa0167-dty06,
        dty07 LIKE pa0167-dty07,
        dty08 LIKE pa0167-dty08,
        dty09 LIKE pa0167-dty09,
        dty10 LIKE pa0167-dty10,
        dty11 LIKE pa0167-dty11,
        dty12 LIKE pa0167-dty12,
        dty13 LIKE pa0167-dty13,
        dty14 LIKE pa0167-dty14,
        dty15 LIKE pa0167-dty15,
        dty16 LIKE pa0167-dty16,
        dty17 LIKE pa0167-dty17,
        dty18 LIKE pa0167-dty18,
        dt119 LIKE pa0167-dty19,
        dty20 LIKE pa0167-dty20,

        did01 LIKE pa0167-did01,
        did02 LIKE pa0167-did02,
        did03 LIKE pa0167-did03,
        did04 LIKE pa0167-did04,
        did05 LIKE pa0167-did05,
        did06 LIKE pa0167-did06,
        did07 LIKE pa0167-did07,
        did08 LIKE pa0167-did08,
        did09 LIKE pa0167-did09,
        did10 LIKE pa0167-did10,
        did11 LIKE pa0167-did11,
        did12 LIKE pa0167-did12,
        did13 LIKE pa0167-did13,
        did14 LIKE pa0167-did14,
        did15 LIKE pa0167-did15,
        did16 LIKE pa0167-did16,
        did17 LIKE pa0167-did17,
        did18 LIKE pa0167-did18,
        did19 LIKE pa0167-did19,
        did20 LIKE pa0167-did20,

        vorna LIKE pa0002-vorna,
        midnm LIKE pa0002-midnm,
        nachn LIKE pa0002-nachn,
        gbdat LIKE pa0002-gbdat,
        perid LIKE pa0002-perid,
        gesch LIKE pa0002-gesch,
        anzkd LIKE pa0002-anzkd,
        stras LIKE pa0006-stras,
        locat LIKE pa0006-locat,
        ort01 LIKE pa0006-ort01,
        state LIKE pa0006-state,
        pstlz LIKE pa0006-pstlz,
        telnr LIKE pa0006-telnr,
        trdat LIKE pa0167-endda,
        pltyp LIKE pa0167-pltyp,
       END OF i_pa0167.

DATA : BEGIN OF i_pa0021 OCCURS 0,
        pernr LIKE pa0021-pernr,
        subty LIKE pa0021-subty,
        objps LIKE pa0021-objps,
        begda LIKE pa0021-begda,
        endda LIKE pa0021-endda,
        fgbdt LIKE pa0021-fgbdt,
        aedtm LIKE pa0021-aedtm,
        fasex LIKE pa0021-fasex,
        favor LIKE pa0021-favor,
        fanam LIKE pa0021-fanam,
        perid LIKE pa0106-perid,
        erbnr LIKE pa0021-erbnr,
        fnmzu LIKE pa0021-fnmzu,
        finit LIKE pa0021-finit,
     END OF i_pa0021.

DATA : BEGIN OF itab OCCURS 0,
        pernr LIKE pa0000-pernr,
        seqno(2) TYPE n,
        subty LIKE pa0021-subty,
        vorna LIKE pa0002-vorna,
        perid LIKE pa0002-perid,
        gbdat LIKE pa0002-gbdat,
        objps LIKE pa0021-objps,
        midnm LIKE pa0002-midnm,
        nachn LIKE pa0002-nachn,
        gesch LIKE pa0002-gesch,
        begda LIKE pa0167-begda,
        endda LIKE pa0167-endda,
        dtyxx LIKE pa0167-dty01,
        depcv LIKE pa0167-depcv,
        anzkd LIKE pa0002-anzkd,
        trdat LIKE pa0167-endda,
        begda_d LIKE pa0021-begda,
        endda_d LIKE pa0021-endda,
        fnmzu LIKE pa0021-fnmzu,
        fgbdt LIKE pa0021-fgbdt,
        flag,
       END OF itab.

DATA : BEGIN OF term_emp OCCURS 0,
        pernr LIKE pa0167-pernr,
       END OF term_emp.

DATA : icon_red_scr TYPE icon_d,
       icon_green_scr TYPE icon_d,
       icon_yellow_scr TYPE icon_d,
       icon_doc_scr  TYPE icon_d.

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
  if p_debug eq true.
    break-point.
  endif.
END-OF-DEFINITION.

DATA: r_date TYPE datum,
      r_time TYPE uzeit,
      r_user TYPE uname.

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


    METHODS:
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
              IMPORTING e_row
                        e_column
                        es_row_no.

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

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

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

DEFINE __define_not_important.
* { not important
* Total Doc. Count to be created.
  data  : total_doc_cnt type i,
          current_doc_cnt type i.
  data : percentage type p,$mod type i,
         $current_cnt(10),$total_cnt(10),$text(100) .
  clear : total_doc_cnt,current_doc_cnt.
* }
END-OF-DEFINITION.

****************************** constants *******************************
CONSTANTS:  false VALUE ' ',
            true  VALUE 'X'.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
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
                    DEFAULT 'c:\temp\verification1.xls'
                    MODIF ID exl.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK bx WITH FRAME TITLE text-00x.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 10(69) my_text MODIF ID brt.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bx.
SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK bd WITH FRAME TITLE text-d01.
PARAMETERS: p_date LIKE sy-datum DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK bd.

SELECTION-SCREEN BEGIN OF BLOCK pp WITH FRAME TITLE text-p01.
SELECT-OPTIONS s_pernr FOR pa0002-pernr.
PARAMETERS p_filter AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK pp.

* Layout
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-s13.
PARAMETER p_vari TYPE slis_vari.
SELECTION-SCREEN END OF BLOCK b4.
PARAMETERS p_debug AS CHECKBOX.

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

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM alv_variant_f4 CHANGING p_vari.
*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM initialize.
  PERFORM upload_file USING p_file.

  READ TABLE it_excel INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE s000 WITH 'No Data was uploaded'.
    STOP.
  ENDIF.

  __process 'Gathering data from SAP tables...' '50'.
  PERFORM get_table.

  PERFORM compare_excel_sap.

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

  __process 'Upload file...' '10'.

  __cls : it_datatab.

  IF p_txt NE true.
    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
         EXPORTING
              i_line_header        = space
              i_tab_raw_data       = it_raw
              i_filename           = filename
         TABLES
              i_tab_converted_data = it_datatab[]  "ACTUAL DATA
         EXCEPTIONS
              conversion_failed    = 1
              OTHERS               = 2.

    IF sy-subrc NE 0.
      MESSAGE s000 WITH 'Error was occured when opening file.'.
      STOP.
    ENDIF.

    IF p_head EQ true.
      DELETE it_datatab INDEX 1.
    ENDIF.

    __process 'Upload file...' '20'.

    IF it_datatab[] IS INITIAL.
      MESSAGE s003(zz) WITH 'No Data was uploaded'.
      g_error = true .
      EXIT.
    ENDIF.
  ELSE.

    DATA cancel.

    __process 'Check data...' '30'.

    CALL FUNCTION 'UPLOAD'
         EXPORTING
              filename            = filename
              filetype            = 'DAT'
         IMPORTING
              cancel              = cancel
         TABLES
              data_tab            = it_datatab
         EXCEPTIONS
              conversion_erro     = 1
              invalid_table_width = 2
              invalid_type        = 3.


    IF p_head EQ true.
      DELETE it_datatab INDEX 1.
    ENDIF.

    LOOP AT it_datatab INTO wa_datatab.
      PERFORM eliminate_qt CHANGING wa_datatab-member.
      MODIFY it_datatab FROM wa_datatab.
    ENDLOOP.

    IF NOT cancel IS INITIAL OR sy-subrc NE 0.
      STOP.
    ENDIF.

  ENDIF.

  DELETE it_datatab WHERE ssn IS initial AND con_no IS initial.
  DATA $level(2) TYPE n.
  DATA $line_no(10) TYPE n.
  $line_no = 0.
  __cls it_excel.
  LOOP AT it_datatab INTO wa_datatab.
    PERFORM check_num CHANGING wa_datatab-ssn.
    IF wa_datatab-ssn NE 'delete'.
      IF wa_datatab-relat CP '*Subscriber*'.
        CLEAR $level.
      ELSE.
        ADD 1 TO $level.
      ENDIF.
      it_excel-level = $level.
      MOVE-CORRESPONDING wa_datatab TO it_excel.
      ADD 1 TO $line_no.
      it_excel-line_no = $line_no.
      APPEND it_excel.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPLOAD_FILE


*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Log.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.


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

  my_text =  text-t05.

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
  DATA num(12) VALUE ' 0123456789'.

  IF n_value CN num. n_value = 'delete'. ENDIF.

ENDFORM.                    " CHECK_NUM
*---------------------------------------------------------------------*
*       FORM eliminate_qt                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  N_VALUE                                                       *
*---------------------------------------------------------------------*
FORM eliminate_qt CHANGING n_value.
  REPLACE : '"' WITH '' INTO n_value,
            '"' WITH '' INTO n_value.
  CONDENSE n_value NO-GAPS.

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

  DATA $ix TYPE i.
  LOOP AT gt_out.
    $ix = sy-tabix.
    PERFORM get_err_txt USING gt_out-err
                  CHANGING gt_out-msg.

    IF gt_out-err NE '8'.
      CLEAR : gt_out-dob, gt_out-egbdat.
    ENDIF.

    MODIFY gt_out INDEX $ix TRANSPORTING dob egbdat msg.
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
  PERFORM apply_icon.

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

  icon_red_scr = icon_led_red.
  icon_yellow_scr = icon_led_yellow.
  icon_green_scr = icon_led_green.
  icon_doc_scr = icon_document.
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
      'This will create Credit/Debit memos massively.'
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
  SET HANDLER : g_event_receiver->handle_data_changed FOR g_grid,
                g_event_receiver->handle_double_click FOR g_grid.

*   Create field category
  PERFORM create_field_category USING false.

*  CALL METHOD g_grid->register_edit_event
*       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

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

  __catalog :
    'X'  'PERNR'             'Emp.#'            10 'NUMC' '',
    'X'  'CON_NO'            'Con.#'            20 'CHAR' '',
    'X'  'MEMBER'            'Member'           40 'CHAR' '',
    ' '  'RELAT'             'Relation'         40 'CHAR' '',
    ' '  'MSG'               'Remarks'          100 'CHAR' '',
    ' '  'SSN'               'SSN(EXL)'         10 'CHAR' '',
    ' '  'EPERID'            'SSN(SAP)'         10 'CHAR' '',
    ' '  'DOB'               'DOB(EXL)'         10 'CHAR' '',
    ' '  'EGBDAT'            'DOB(SAP)'         10 'CHAR' ''.

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
      'PERNR'           ' ' 'X' '' 'X' '',
      'CON_NO'          ' ' 'X' '' 'X' ''.
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
*  DATA : $ix(2) TYPE n,
*         $mtxt(6).
*
*  CLEAR: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].
*
*  DEFINE __color.
*    gs_specialcol-fieldname = &1 .
*    gs_specialcol-color-col = &2 .
*    gs_specialcol-color-int = &3 .
*    append gs_specialcol to gt_specialcol .
*  END-OF-DEFINITION.
*
*  __color :
*            'VIN'              '2' 0,
*            'MATNR'            '2' 0,
*            'KBETR'            '3' 0,
*            'FKDAT'            '1' 0,
*            'ICON'             '1' 0,
*            'MSG'              '1' 0.
*
*  gt_out-tabcolor[] = gt_specialcol[].
*  MODIFY gt_out TRANSPORTING tabcolor WHERE tabcolor IS initial.

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
*  DATA: lt_celltab TYPE lvc_t_styl,
*        ls_celltab TYPE lvc_s_styl.
*
*  CLEAR lt_celltab.
*  REFRESH lt_celltab.
*
*  CLEAR gs_fcat.
*
*  LOOP AT gt_fcat INTO gs_fcat.
*    ls_celltab-fieldname = gs_fcat-fieldname.
*    IF ls_celltab-fieldname EQ 'KBETR' OR
*          ls_celltab-fieldname EQ 'VIN'.
*
*      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
*    ELSE.
*      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*    ENDIF.
*    INSERT ls_celltab INTO TABLE lt_celltab.
*  ENDLOOP.
*
*  CLEAR gt_out-celltab.
*  INSERT LINES OF lt_celltab INTO TABLE gt_out-celltab.
*  MODIFY gt_out TRANSPORTING celltab WHERE celltab IS initial.
*  PERFORM build_cell_attr1_lock.

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

*  LOOP AT gt_return.
*    WRITE:/ gt_return-message_v1(20),
*            gt_return-message_v2(10),
*            gt_return-message(40).
*  ENDLOOP.

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

ENDFORM.                    " APPLY_CCA
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
FORM get_selected_rows TABLES $gt_out STRUCTURE gt_out.

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
  __cls : it_excel.

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

*  LOOP AT gt_out.
*    $ix = sy-tabix.
*
*    CASE gt_out-err_bdc.
*      WHEN 'X'.
*        gt_out-icon = icon_led_red.
*      WHEN 'N'.
*        gt_out-icon = icon_led_green.
*      WHEN OTHERS.
*        CLEAR gt_out-icon.
*    ENDCASE.
*
*    CASE gt_out-err_prc.
*      WHEN 'X'.
*        gt_out-icon2 = icon_led_yellow.
*      WHEN OTHERS.
*        CLEAR gt_out-icon2.
*    ENDCASE.
*
*    CASE gt_out-err_body.
*      WHEN 'X'.
*        gt_out-icon2 = icon_led_red.
*      WHEN OTHERS.
*    ENDCASE.
*
*    MODIFY gt_out INDEX $ix TRANSPORTING icon2 icon.
*  ENDLOOP.

ENDFORM.                    " apply_icon

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

*---------------------------------------------------------------------*
*       FORM double_click                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E_ROW                                                         *
*  -->  E_COLUMN                                                      *
*  -->  ES_ROW_NO                                                     *
*---------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.
*  DATA l_index TYPE i.
*
*  l_index = e_row-index.
*
*  READ TABLE gt_out INDEX l_index.
*  IF sy-subrc = 0.
*    IF e_column = 'REF_DOC'.
*      CHECK gt_out-ref_doc NE space.
*      SET PARAMETER ID 'VF'  FIELD gt_out-ref_doc .
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*
*    ENDIF.
*    IF e_column = 'BILL_DOC'.
*      CHECK gt_out-bill_doc NE space.
*      SET PARAMETER ID 'VF'  FIELD gt_out-bill_doc.
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*    ENDIF.
*    IF e_column = 'VBELN'.
*      CHECK gt_out-vbeln NE space.
*      SET PARAMETER ID 'VF'  FIELD gt_out-vbeln.
*      CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
*    ENDIF.
*
*  ENDIF.
*
*  CALL METHOD cl_gui_control=>set_focus EXPORTING control = g_grid.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  get_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_table.

  __cls it_status.
  SELECT pernr  begda massn massg stat2 INTO TABLE it_status
  FROM pa0000
  WHERE pernr IN s_pernr
    AND begda <= p_date
    AND endda >= p_date
    AND stat2 EQ '0'.

  __cls i_pa0167.


  __u_break.

  SELECT a~pernr a~begda a~endda

         a~dty01 a~dty02 a~dty03
         a~dty04 a~dty05 a~dty06
         a~dty07 a~dty08 a~dty09
         a~dty10 a~dty11 a~dty12
         a~dty13 a~dty14 a~dty15
         a~dty16 a~dty17 a~dty18
         a~dty19 a~dty20

         a~did01 a~did02 a~did03
         a~did04 a~did05 a~did06
         a~did07 a~did08 a~did09
         a~did10 a~did11 a~did12
         a~did13 a~did14 a~did15
         a~did16 a~did17 a~did18
         a~did19 a~did20

         a~depcv
         b~vorna b~midnm b~nachn
         b~gbdat b~perid b~gesch
         b~anzkd
         a~pltyp

    INTO CORRESPONDING FIELDS OF TABLE i_pa0167
          FROM pa0167 AS a
         INNER JOIN pa0002 AS b
            ON b~pernr EQ a~pernr
           AND b~endda EQ '99991231'
         WHERE a~pernr IN s_pernr
           AND a~begda <= p_date
           AND a~endda >= p_date
           AND a~pltyp = 'MEDI'.

  SORT i_pa0167 BY  pernr.

  __cls i_pa0021.

  SELECT a~pernr a~subty a~objps
         a~begda a~endda a~fgbdt
         a~aedtm a~fasex
         a~favor a~fanam a~erbnr a~fnmzu a~finit
         b~perid
  INTO CORRESPONDING FIELDS OF TABLE i_pa0021
    FROM pa0021 AS a
    INNER JOIN pa0106 AS b
       ON b~pernr EQ a~pernr
      AND b~subty EQ a~subty
      AND b~objps EQ a~objps
      AND b~sprps EQ a~sprps
      AND b~endda EQ '99991231'
      AND b~begda EQ a~begda
      AND b~seqnr EQ a~seqnr
      WHERE  a~pernr IN s_pernr
        AND a~begda <= sy-datum
        AND a~endda >= sy-datum
        AND a~subty NE '7'.

  SORT i_pa0021 BY pernr subty objps begda .

  __cls itab.
  PERFORM get_itab_new TABLES i_pa0021
                              i_pa0167
                              itab.

  LOOP AT itab.
    TRANSLATE itab-vorna TO UPPER CASE.
    TRANSLATE itab-midnm TO UPPER CASE.
    TRANSLATE itab-nachn TO UPPER CASE.
    MODIFY itab INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " get_table
*---------------------------------------------------------------------*
*       FORM get_itab_new                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_I_PA0021                                                    *
*  -->  P_I_PA0167                                                    *
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM get_itab_new TABLES p_i_pa0021 STRUCTURE i_pa0021
                         p_i_pa0167 STRUCTURE i_pa0167
                         p_itab STRUCTURE itab.

  DATA : dtyxx TYPE ben_deptyp,
         didxx TYPE ben_depid,
         seqno(2) TYPE n,
         $fr TYPE i,
         $ix  TYPE i.
  DATA tmp_itab LIKE itab OCCURS 10 WITH HEADER LINE.
  data age like p_date.
  LOOP AT p_i_pa0167 .

    __cls tmp_itab.

    MOVE-CORRESPONDING p_i_pa0167 TO tmp_itab.

    CLEAR seqno.
    tmp_itab-dtyxx = 'H'.
    tmp_itab-flag = true.
    APPEND tmp_itab.
    CLEAR tmp_itab.
    READ TABLE p_i_pa0021 WITH KEY pernr = p_i_pa0167-pernr
                                   BINARY SEARCH.
    $fr = sy-tabix.
    IF sy-subrc NE 0.
      APPEND LINES OF tmp_itab TO p_itab.
      CONTINUE.
    ENDIF.

    LOOP AT  p_i_pa0021 FROM $fr.
      IF p_i_pa0021-pernr NE p_i_pa0167-pernr.
        EXIT.
      ENDIF.
      MOVE-CORRESPONDING p_i_pa0167 TO tmp_itab.
      tmp_itab-pernr = p_i_pa0021-pernr.
      tmp_itab-seqno = p_i_pa0021-erbnr(2).
      tmp_itab-subty = p_i_pa0021-subty.
      tmp_itab-objps = p_i_pa0021-objps.
      tmp_itab-begda = p_i_pa0021-begda.
      tmp_itab-endda = p_i_pa0021-endda.
      tmp_itab-vorna = p_i_pa0021-favor.
      tmp_itab-midnm = space.
      tmp_itab-nachn = p_i_pa0021-fanam.
      tmp_itab-gbdat = p_i_pa0021-fgbdt.
      tmp_itab-dtyxx = p_i_pa0021-subty.
      tmp_itab-gesch = p_i_pa0021-fasex.
      tmp_itab-begda_d = p_i_pa0021-begda.
      tmp_itab-endda_d = p_i_pa0021-endda.
      tmp_itab-perid = p_i_pa0021-perid.
      tmp_itab-fnmzu = p_i_pa0021-fnmzu.
      tmp_itab-fgbdt = p_i_pa0021-fgbdt.
      tmp_itab-midnm = p_i_pa0021-finit.
      CLEAR tmp_itab-anzkd.
      APPEND tmp_itab.
    ENDLOOP.

    SORT tmp_itab BY begda subty objps.

    SORT tmp_itab BY subty objps.

    DO 20 TIMES VARYING dtyxx FROM p_i_pa0167-dty01
                              NEXT p_i_pa0167-dty02
                VARYING didxx FROM p_i_pa0167-did01
                              NEXT p_i_pa0167-did02.
      IF dtyxx IS INITIAL.
        EXIT.
      ELSE.

        READ TABLE p_i_pa0021 WITH KEY pernr = p_i_pa0167-pernr
                                       subty = dtyxx
                                       objps = didxx
                                       BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE tmp_itab WITH KEY subty = dtyxx
                                       objps = didxx
                                       BINARY SEARCH.
          IF sy-subrc EQ 0.
            tmp_itab-flag = true.
            tmp_itab-begda = p_i_pa0167-begda.
            tmp_itab-endda = p_i_pa0167-endda.
           MODIFY tmp_itab INDEX sy-tabix TRANSPORTING flag begda endda.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.

    DELETE tmp_itab WHERE flag NE true.
    loop at tmp_itab.
      $ix = sy-tabix.
      if tmp_itab-subty eq '2' or tmp_itab-subty eq '6' or
         tmp_itab-subty eq '9' or tmp_itab-subty eq '13' .
        age = p_date - tmp_itab-fgbdt.
        if age >= '00200000'.

          select single * from pa0106
           where PERNR eq tmp_itab-pernr
             and SUBTY eq tmp_itab-subty
             and OBJPS eq tmp_itab-OBJPS
             and SPRPS eq space
             and ENDDA eq '99991231'
             and ( ben03 eq 'X' or disab eq 'X' ).

          if sy-subrc ne 0.
            delete tmp_itab index $ix.
          endif.

        endif.
      endif.

    endloop.

    APPEND LINES OF tmp_itab TO p_itab.
  ENDLOOP.

ENDFORM.                    " get_itab
*&---------------------------------------------------------------------*
*&      Form  compare_excel_sap
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM compare_excel_sap.

  DATA $ix TYPE i.
  DATA $flag_end.
  DATA $flag_new.
  DATA $err.
  DATA $string(40).
  DATA $found.
  DATA $it_excel LIKE it_excel OCCURS 0  WITH HEADER LINE.
  DATA wa_excel LIKE it_excel OCCURS 1  WITH HEADER LINE.
  DATA $excel LIKE it_excel OCCURS 10  WITH HEADER LINE.
  DATA $itab LIKE itab  OCCURS 10  WITH HEADER LINE.
  DATA $lastname LIKE pa0002-nachn.
  DATA $firstname LIKE pa0002-vorna.

  __cls $it_excel.
  LOOP AT it_excel WHERE relat EQ 'Subscriber'.
    $it_excel = it_excel.
    APPEND $it_excel.
  ENDLOOP.

  SORT  $it_excel BY ssn.

  LOOP AT itab WHERE subty IS initial.
    CHECK NOT itab-perid IS INITIAL.
    $ix = sy-tabix.

    READ TABLE $it_excel WITH KEY ssn = itab-perid BINARY SEARCH.
    IF sy-subrc EQ 0.
      $ix = sy-tabix.
      $it_excel-pernr = itab-pernr.
      MODIFY $it_excel INDEX $ix TRANSPORTING pernr.
    ENDIF.
  ENDLOOP.

  SORT  it_excel BY con_no.
  LOOP AT $it_excel.
   READ TABLE it_excel WITH KEY con_no = $it_excel-con_no BINARY SEARCH.
    IF sy-subrc EQ 0.
      it_excel-pernr = $it_excel-pernr.
      MODIFY it_excel TRANSPORTING pernr WHERE con_no EQ
$it_excel-con_no.
    ENDIF.
  ENDLOOP.

  IF p_filter EQ true.
    DELETE it_excel WHERE NOT pernr IN s_pernr.
  ENDIF.

  SORT it_excel BY line_no.
  SORT itab BY pernr seqno subty.
  __cls : gt_out, $it_excel.

  $it_excel[] = it_excel[].
  __cls $excel.

  __define_not_important.
  DESCRIBE TABLE  it_excel LINES total_doc_cnt.
  $total_cnt = total_doc_cnt.

  LOOP AT it_excel.

    ADD 1 TO current_doc_cnt.
    $current_cnt = current_doc_cnt.
    CONCATENATE 'Checking differences...' $current_cnt '/' $total_cnt
                           INTO $text.
    CONDENSE $text.
    percentage = current_doc_cnt MOD 10.
    IF percentage EQ 0.
      PERFORM show_progress USING $text 0.
    ENDIF.

    AT NEW con_no.
      $flag_new =  true.
    ENDAT.

    IF $flag_new EQ true.
      CLEAR $flag_new.
      wa_excel =  it_excel.
    ENDIF.

    AT END OF con_no.
      $flag_end =  true.
    ENDAT.

    $excel = it_excel.
    APPEND $excel.

    CHECK $flag_end EQ true.
    CLEAR $flag_end.

    __u_break.

    __cls $itab.
    READ TABLE itab WITH KEY pernr = it_excel-pernr BINARY SEARCH.
    IF sy-subrc EQ 0.
      LOOP AT itab FROM sy-tabix.
        IF itab-pernr NE it_excel-pernr.
          EXIT.
        ENDIF.
        $itab = itab.
        APPEND $itab.
      ENDLOOP.
      DELETE itab WHERE pernr = it_excel-pernr.
    ELSE.

      CLEAR gt_out.
      MOVE-CORRESPONDING wa_excel TO gt_out.
      IF NOT wa_excel-ssn IS INITIAL.
        CLEAR : $lastname,$firstname.
        SEARCH wa_excel-member FOR ','.
        IF sy-fdpos NE 0.
          $lastname = wa_excel-member+0(sy-fdpos).
          $firstname = wa_excel-member+sy-fdpos.
          REPLACE ',' WITH ' ' INTO $firstname.
          CONDENSE $lastname NO-GAPS.
          CONDENSE $firstname NO-GAPS.
          READ TABLE itab WITH KEY nachn = $lastname
                                   vorna = $firstname.
          IF sy-subrc EQ 0.
            gt_out-pernr = itab-pernr.
            gt_out-err = 'N'.
          ELSE.

            SELECT SINGLE * FROM pa0106 WHERE  pernr EQ it_excel-pernr
                                           AND perid EQ wa_excel-ssn
                                           AND ENDDA EQ '99991231'.
            IF sy-subrc EQ 0.
              gt_out-err = '1'.
            ELSE.
              gt_out-err = 'N'.
            ENDIF.
            PERFORM check_ssn_pa002 USING wa_excel-ssn.

          ENDIF.
        ENDIF.
      ELSE.
        SELECT SINGLE * FROM pa0106 WHERE  pernr EQ it_excel-pernr
                                       AND perid EQ wa_excel-ssn
                                       AND ENDDA EQ '99991231'.
        IF sy-subrc EQ 0.
          gt_out-err = '1'.
        ELSE.
          gt_out-err = 'N'.
        ENDIF.
        PERFORM check_ssn_pa002 USING wa_excel-ssn.
      ENDIF.

      APPEND gt_out.
      __cls $excel.
      CONTINUE.
    ENDIF.

    CLEAR $err.
    PERFORM check_table TABLES $excel $itab
                        CHANGING $err.

    READ TABLE $itab INDEX 1.
    IF sy-subrc EQ 0.
      LOOP AT $itab.
        CLEAR gt_out.
        MOVE-CORRESPONDING $itab TO gt_out.

        PERFORM get_full_name USING $itab-nachn $itab-vorna
                           CHANGING gt_out-member.

        CONCATENATE '*' gt_out-member '*' INTO $string.
        CONDENSE $string NO-GAPS.

        CLEAR $found.
        LOOP AT $excel.

          REPLACE ',' WITH '' INTO $excel-member.
          REPLACE '.' WITH '' INTO $excel-member.
          REPLACE '''' WITH '' INTO $excel-member.
          REPLACE ',' WITH '' INTO $string.
          REPLACE '.' WITH '' INTO $string.
          REPLACE '''' WITH '' INTO $string.

          CONDENSE $excel-member NO-GAPS.
          CONDENSE $string NO-GAPS.
          IF $excel-member CP $string.
            $found = true.
          ENDIF.

        ENDLOOP.
        IF $found EQ true.
        ELSE.
          gt_out-err = '3'. "'Missing at excel'.
          PERFORM get_relat_txt USING $itab-subty
                                CHANGING gt_out-relat.
          APPEND gt_out.
        ENDIF.
      ENDLOOP.
    ENDIF.
    __cls $excel.
  ENDLOOP.


ENDFORM.                    " compare_excel_sap
*&---------------------------------------------------------------------*
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL  text
*      -->P_ITAB  text
*      <--P_GT_OUT_ERR  text
*----------------------------------------------------------------------*
FORM check_data USING p_excel STRUCTURE it_excel
                      p_itab STRUCTURE itab
              CHANGING p_err.

  DATA $relat(40).
  DATA $sex(1).
  DATA date_internal     TYPE d.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
       EXPORTING
            date_external            = p_excel-dob
       IMPORTING
            date_internal            = date_internal
       EXCEPTIONS
            date_external_is_invalid = 1
            OTHERS                   = 2.
  IF sy-subrc <> 0. ENDIF.

  IF date_internal NE p_itab-gbdat.
    CONCATENATE p_err '8' INTO p_err. " different dob
    WRITE p_itab-gbdat TO p_excel-egbdat.
    WRITE date_internal TO p_excel-dob.
  ENDIF.


  IF p_excel-ssn NE p_itab-perid.
    CONCATENATE p_err '5' INTO p_err. " different dob
  ENDIF.

  IF p_itab-gesch EQ '1'.
    $sex = 'M'.
  ELSE.
    $sex = 'F'.
  ENDIF.

  IF p_excel-sex NE $sex.
    CONCATENATE p_err '6' INTO p_err. " different dob
  ENDIF.

  $relat = p_excel-relat.
  CONDENSE $relat.

  CASE $relat.
    WHEN 'Subscriber'.
      IF NOT p_itab-subty IS INITIAL.
      ENDIF.
    WHEN 'Spouse'.
      IF p_itab-subty NE '1' AND p_itab-subty NE '90'.
        CONCATENATE p_err '7' INTO p_err. " different dob
      ENDIF.
    WHEN 'Child'.
      IF p_itab-subty NE '2' AND p_itab-subty NE '6' AND
         p_itab-subty NE '9' AND p_itab-subty NE '13'.
        CONCATENATE p_err '7' INTO p_err. " different dob
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " check_data
*&---------------------------------------------------------------------*
*&      Form  check_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$EXCEL  text
*      -->P_$ITAB  text
*      <--P_GT_OUT_ERR  text
*----------------------------------------------------------------------*
FORM check_table TABLES   p_excel STRUCTURE it_excel
                          p_itab STRUCTURE  itab
                 CHANGING p_err.

  DATA $err(10).
  DATA $ix TYPE i.
  DATA $strlen TYPE i.
  DATA $offset TYPE i.
  DATA $errtxt(40).
  DATA $msg(100).
  DATA $member(40).
  DATA $string(40).
  DATA $found.
  DATA $eix TYPE i.
  LOOP AT p_excel.
    $eix = sy-tabix.
    CLEAR : $err, $msg.
    READ TABLE p_itab WITH KEY perid = p_excel-ssn.

    IF sy-subrc EQ 0.
      $ix = sy-tabix.
      PERFORM check_data USING p_excel
                               p_itab
                         CHANGING $err.
      DELETE p_itab INDEX $ix.
      MODIFY p_excel INDEX $eix TRANSPORTING egbdat dob.
    ELSE.
      SELECT SINGLE * FROM pa0106 WHERE  pernr EQ p_excel-pernr
                                     AND perid EQ p_excel-ssn
                                     and endda eq '99991231'.
      IF sy-subrc EQ 0.
        $err = '1'.
      ELSE.

        CLEAR $found.
        $member =  p_excel-member.
        REPLACE ',' WITH '' INTO $member.
        REPLACE '.' WITH '' INTO $member.
        REPLACE '''' WITH '' INTO $member.
        CONCATENATE '*' $member '*' INTO $member.
        CONDENSE $member NO-GAPS.

        LOOP AT p_itab.
          $ix = sy-tabix.
          PERFORM get_full_name USING p_itab-nachn p_itab-vorna
                             CHANGING $string.

          REPLACE ',' WITH '' INTO $string.
          REPLACE '.' WITH '' INTO $string.
          REPLACE '''' WITH '' INTO $string.
          CONDENSE $string NO-GAPS.

          IF $string CP $member.
            $found = true.
            p_excel-eperid = p_itab-perid.
            MODIFY p_excel INDEX $eix TRANSPORTING eperid.
          ENDIF.

        ENDLOOP.
        IF $found EQ true.
          $err = '5'. " Diff SSN
        ELSE.
          $err = '2'. " no ssn found
        ENDIF.

      ENDIF.
    ENDIF.

    CONDENSE $err.
    $strlen = strlen( $err ).
    IF $strlen > 1.
      DO $strlen TIMES.
        $offset = sy-index - 1.
        PERFORM get_err_txt USING $err+$offset(1)
                      CHANGING $errtxt.
        CONCATENATE  $msg '/' $errtxt INTO $msg.
      ENDDO.
      SHIFT $msg LEFT.
      $err = '9'.
    ENDIF.

    IF NOT $err IS INITIAL.
      CLEAR gt_out.
      MOVE-CORRESPONDING p_excel TO gt_out.
      gt_out-err = $err.
      IF $err EQ '9'.
        gt_out-msg = $msg.
      ENDIF.

      APPEND gt_out.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " check_table
*&---------------------------------------------------------------------*
*&      Form  get_err
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUTERR  text
*      <--P_GT_OUT_MSG  text
*----------------------------------------------------------------------*
FORM get_err_txt USING  p_err
               CHANGING p_msg.

  CASE p_err.
    WHEN '1'.
      p_msg = 'No benefit plan'.
    WHEN '2'.
      p_msg = 'No record in SAP'.

    WHEN 'N'.
      p_msg = 'Inconsistency in SSN'.
    WHEN '5'.
      p_msg = 'Inconsistency in SSN'.

    WHEN 'D'.
      p_msg = 'Dummy SSN in Excel'.
    WHEN '3'.
      p_msg = 'Not found in excel'.
    WHEN '6'.
      p_msg = 'Diff. Gen.'.
    WHEN '7'.
      p_msg = 'Diff. Subty'.
    WHEN '8'.
      p_msg = 'Diff. DOB.'.
  ENDCASE.

ENDFORM.                    " get_err
*&---------------------------------------------------------------------*
*&      Form  get_relat_txt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$ITAB_SUBTY  text
*      <--P_GT_OUT_RELAT  text
*----------------------------------------------------------------------*
FORM get_relat_txt USING    p_subty
                   CHANGING p_relat.

  IF p_subty IS INITIAL.
    p_relat = 'Subscriber'.
    EXIT.
  ENDIF.

  CASE p_subty.
    WHEN '1' OR '90'.
      p_relat = 'Spouse'.
    WHEN '2' OR '' OR '6' OR '9' OR '13'.
      p_relat = 'Child'.
    WHEN '10'.
      p_relat = 'Divorced Spouse'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " get_relat_txt
*&---------------------------------------------------------------------*
*&      Form  get_full_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$ITAB_NACHN  text
*      -->P_$ITAB_VORNA  text
*      <--P_GT_OUT_MEMBER  text
*----------------------------------------------------------------------*
FORM get_full_name USING    p_lastname
                            p_firstname
                   CHANGING p_member.

  CONCATENATE p_lastname ',^' p_firstname INTO p_member.
  TRANSLATE p_member TO UPPER CASE.
  REPLACE '^' WITH ' ' INTO p_member.
  CONDENSE p_member.

ENDFORM.                    " get_full_name
*&---------------------------------------------------------------------*
*&      Form  check_ssn_pa002
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EXCEL_SSN  text
*----------------------------------------------------------------------*
FORM check_ssn_pa002 USING  p_ssn.
  SELECT SINGLE pernr INTO gt_out-pernr
  FROM pa0002
  WHERE perid EQ p_ssn.

*  if P_SSN+0(2) eq '99'.
*    gt_out-err = 'D'.
*  endif.

ENDFORM.                    " check_ssn_pa002
