*----------------------------------------------------------------------
* Program ID        : ZITBPMANALYSIS
* Title             : APM Analysis/Plan
* Created on        : 10/28/2013
* Created by        : Victor
* Description       :
*----------------------------------------------------------------------
* Change History table : ZTITBPMA_H
*----------------------------------------------------------------------
REPORT  zitbpmanalysis MESSAGE-ID zmco.
INCLUDE zitbpmanalysis_alv.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
TYPE-POOLS: kcde.

* Tables
TABLES : ztitbpml, ztitbpma, ztitbpmlh, zthrappusge ,  bapiaddr3.

TYPES: BEGIN OF ty_bpmla.
        INCLUDE STRUCTURE ztitbpma.
TYPES: END OF ty_bpmla.

TYPES: BEGIN OF ty_row_tab.
TYPES:  usage TYPE i,
        prespmt TYPE swlmrespti, " Previous performance
*        respmt TYPE swlmrespti,
        l3t LIKE ztitbpml_log-l3t,
        l4t LIKE ztitbpml_log-l4t,
        l5t LIKE ztitbpml_log-l5t,
        ttext LIKE  ztitbpml_log-ttext,
        cstatus_nm(20),
        ptypea_nm(20),
        ptypef_nm(20),
        ureason_nm(20),
        presult_nm(20),
        uresult_nm(20),
        fullname LIKE bapiaddr3-fullname,
        e_mail   LIKE bapiaddr3-e_mail,
        e_mail_icon TYPE icon-id.

        INCLUDE STRUCTURE ztitbpma.
TYPES:  icon TYPE icon-id,
         msg(30),
         chk(1).
TYPES: END OF ty_row_tab.

TYPES: BEGIN OF ty_out.
INCLUDE  TYPE ty_row_tab.
TYPES      celltab  TYPE lvc_t_styl.
TYPES      tabcolor TYPE slis_t_specialcol_alv.
TYPES: END OF ty_out.

*-Processing data
DATA : it_processing LIKE ztitbpma  OCCURS 0 WITH HEADER LINE.
DATA : it_bpmlh      LIKE ztitbpmlh OCCURS 0 WITH HEADER LINE.

DATA : it_save       LIKE ztitbpma OCCURS 0 WITH HEADER LINE.
DATA : it_notused LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE,
       it_perform LIKE ztitbpml_log OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_notused_collect OCCURS 0,
             comp          LIKE  ztitbpml_log-comp  ,
             l4f           LIKE  ztitbpml_log-l4f     ,
             tcode         LIKE  ztitbpml_log-tcode   ,
             l1            LIKE  ztitbpml_log-l1      ,
             l2            LIKE  ztitbpml_log-l2      ,
             l3            LIKE  ztitbpml_log-l3      ,
             l4            LIKE  ztitbpml_log-l4      ,
             l5            LIKE  ztitbpml_log-l5      ,
             task          LIKE  ztitbpml_log-task    ,
             apm           LIKE  ztitbpml_log-apm     ,
             devclass      LIKE  ztitbpml_log-devclass ,
             pgmna         LIKE  ztitbpml_log-pgmna   ,
             cnt           LIKE  ztitbpml_log-cnt     ,
             l2t           LIKE  ztitbpml_log-l2t     ,
             l3t           LIKE  ztitbpml_log-l3t     ,
             l4t           LIKE  ztitbpml_log-l4t     ,
             l5t           LIKE  ztitbpml_log-l5t     ,
             ttext         LIKE  ztitbpml_log-ttext   ,
      END OF it_notused_collect.

DATA: g_error(1),
      g_repid  LIKE sy-repid,
      g_ix     LIKE sy-tabix.

DATA  : it_row_tab TYPE TABLE OF ty_row_tab WITH HEADER LINE,
        gt_out     TYPE TABLE OF ty_out     WITH HEADER LINE,
        gt_out1    TYPE TABLE OF ty_out     WITH HEADER LINE,
        wa_out     TYPE ty_out,
        wa_out1    TYPE ty_out.

DATA : BEGIN OF all_tcodes OCCURS 0,
       tcode TYPE tcode,
       l4f   TYPE zitbpl4f,
       END   OF  all_tcodes.

DATA  $gt_out LIKE gt_out OCCURS 0 WITH HEADER LINE.
DATA : it_apm_user    TYPE TABLE OF  ztit_apm_user WITH HEADER LINE,
       wa_apm_user    TYPE ztit_apm_user,
       wa_apm_manager TYPE ztit_apm_user,
       l_consultant_id TYPE bapibname-bapibname,
       l_answer(1),
       l_chk_modify(1).

*-Domain Text
TYPES  : BEGIN OF ty_text ,
           domvalue_l LIKE dd07t-domvalue_l,
           ddtext     LIKE dd07t-ddtext,
         END OF ty_text.

DATA : it_cstatus TYPE TABLE OF ty_text WITH HEADER LINE,
       it_ptype   TYPE TABLE OF ty_text WITH HEADER LINE,
       it_ureason TYPE TABLE OF ty_text WITH HEADER LINE,
       it_presult TYPE TABLE OF ty_text WITH HEADER LINE,
       it_uresult TYPE TABLE OF ty_text WITH HEADER LINE.

DATA: gv_timestamp_from TYPE cacstimestampdisp,
      gv_timestamp_to TYPE cacstimestampdisp,
      gv_date_ext(12),
      gv_time_ext(8).

RANGES : r_usage_d FOR sy-datum OCCURS 1,
         r_perfo_d FOR sy-datum OCCURS 1,
         r_module  FOR ztitbpml_log-l2 OCCURS 0.

DEFINE _process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.
DEFINE _cls.                          " clear & refresh
  clear &1.refresh &1.
END-OF-DEFINITION.
DEFINE __focus.
  call method cl_gui_control=>set_focus
    exporting
      control = &1.
END-OF-DEFINITION.
DEFINE _process.
  perform show_progress using &1 &2.
END-OF-DEFINITION.

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
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed  e_ucomm,

    handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no,

    handle_changed_finished
                  FOR EVENT data_changed_finished OF cl_gui_alv_grid
         IMPORTING
              e_modified
              et_good_cells,

     handle_on_f4  FOR EVENT onf4 OF cl_gui_alv_grid
          IMPORTING sender
               e_fieldname
               e_fieldvalue
               es_row_no
               er_event_data
               et_bad_cells
               e_display,

      handle_data_changed1 FOR EVENT data_changed OF cl_gui_alv_grid
              IMPORTING er_data_changed  e_ucomm,

    handle_double_click1 FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row
                      e_column
                      es_row_no,

    handle_hotspot_click1 FOR  EVENT hotspot_click OF cl_gui_alv_grid
             IMPORTING e_row_id
                       e_column_id
                       es_row_no,

     handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
*              IMPORTING e_oject e_ucomm,
              IMPORTING es_col_id es_row_no,

     handle_button_click1 FOR EVENT button_click OF cl_gui_alv_grid
              IMPORTING es_col_id es_row_no,

    handle_changed_finished1
                  FOR EVENT data_changed_finished OF cl_gui_alv_grid
         IMPORTING
              e_modified
              et_good_cells,

     handle_on_f41  FOR EVENT onf4 OF cl_gui_alv_grid
          IMPORTING sender
               e_fieldname
               e_fieldvalue
               es_row_no
               er_event_data
               et_bad_cells
               e_display.
ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

* Hotspot click
  METHOD handle_hotspot_click1.
    PERFORM hotspot_click1 USING e_row_id
                                  e_column_id
                                  es_row_no.

  ENDMETHOD.                    "handle_hotspot_click1

*--Handle Button Click
  METHOD handle_button_click .
    PERFORM handle_button_click USING es_col_id es_row_no.
  ENDMETHOD .                    "handle_button_click

*--Handle Button Click
  METHOD handle_button_click1 .
    PERFORM handle_button_click1 USING es_col_id es_row_no.
  ENDMETHOD .                    "handle_button_click

  METHOD handle_on_f4.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                    "handle_on_f4

* Setting for Change data
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed e_ucomm.
  ENDMETHOD.                    " handle_data_changed

* Setting for Change data
  METHOD handle_data_changed1.
    PERFORM data_changed1 USING er_data_changed e_ucomm.
  ENDMETHOD.                    " handle_data_changed1

* Double Click
  METHOD handle_double_click1.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click1

  METHOD handle_changed_finished.
*   In event handler method for event DATA_CHANGED_FINISHED
    PERFORM data_changed_finished USING e_modified et_good_cells.
    "Local
  ENDMETHOD.                    "handle_changed_finished

  METHOD handle_changed_finished1.
*   In event handler method for event DATA_CHANGED_FINISHED
    PERFORM data_changed_finished1 USING e_modified et_good_cells.
    "Local
  ENDMETHOD.                    "handle_changed_finished1

  METHOD handle_on_f41.
    PERFORM on_f4 USING sender
                        e_fieldname
                        e_fieldvalue
                        es_row_no
                        er_event_data
                        et_bad_cells
                        e_display.
  ENDMETHOD.                    "handle_on_f41
ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation

DATA g_event_receiver  TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*   Selection Screen                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_datum FOR sy-datum DEFAULT sy-datum
                                    NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS : p_usage  TYPE i  DEFAULT '365'.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-011 FOR FIELD p_perfo.
PARAMETERS : p_perfo  TYPE i  DEFAULT '30'.
SELECTION-SCREEN POSITION 60.
PARAMETERS : p_mins  TYPE p LENGTH 3 DECIMALS 0    DEFAULT '60'.
SELECTION-SCREEN COMMENT 67(10) text-012 FOR FIELD p_mins.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-003.
PARAMETERS p_proc  TYPE xfeld RADIOBUTTON GROUP meth DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_disp TYPE xfeld RADIOBUTTON GROUP meth.
SELECTION-SCREEN COMMENT 5(10) text-013 FOR FIELD p_disp.
SELECTION-SCREEN POSITION 20.
PARAMETERS p_incl AS CHECKBOX.
SELECTION-SCREEN COMMENT  25(30) text-014 FOR FIELD p_incl.
SELECTION-SCREEN END OF LINE.
PARAMETERS p_new  TYPE xfeld RADIOBUTTON GROUP meth.
*PARAMETER P_TEST AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE text-006.
SELECT-OPTIONS s_tcode FOR ztitbpma-tcode.
SELECTION-SCREEN END OF BLOCK b6.

* Layout
*SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-002.
*PARAMETER p_vari TYPE slis_vari.
*SELECTION-SCREEN END OF BLOCK b4.

*PARAMETERS : p_date TYPE sy-datum DEFAULT sy-datum NO-DISPLAY.

*&---------------------------------------------------------------------*
*&   Event INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM default.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM alv_variant_f4 CHANGING p_vari.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

*  PERFORM get_time_stamp.
  PERFORM get_time_period.
  PERFORM get_processing_data.

  IF p_new = 'X'.
    PERFORM get_notused_data.
    PERFORM get_performance_data.
  ENDIF.

  PERFORM make_processing_data.


  PERFORM fill_data.

*  PERFORM analysis.

  CHECK g_error EQ space .
  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Form  ANALYSIS
*&---------------------------------------------------------------------*
FORM analysis .
*ZTITBPML, ZTITBPMA, ZTITBPMLH, ZTHRAPPUSGE .
*  DATA IZTHRAPPUSGE LIKE ZTHRAPPUSGE OCCURS 0 WITH HEADER LINE.
  DATA lc_respmt TYPE swlmrespti.
  DATA lv_respmt TYPE swlmrespti. " previous 12 months average performance
  DATA lv_cnt TYPE i.

  _process 'Prepare Screen...' '10'.
  PERFORM get_all_tcodes.

  _cls : it_row_tab, gt_out.

  _process 'Prepare Screen...' '30'.
  LOOP AT all_tcodes.
    CLEAR it_row_tab.
    SELECT AVG( respmt ) COUNT( * ) INTO (lc_respmt,lv_cnt)
    FROM zthrappusge
    WHERE ldate IN s_datum AND tcode EQ all_tcodes-tcode.

    IF sy-subrc EQ 0.
      it_row_tab-usage = lv_cnt.
      PERFORM get_12_average_performance USING all_tcodes-tcode lv_respmt.
      IF lv_respmt < lc_respmt.
        it_row_tab-ptypea = 'P'.
      ENDIF.
      it_row_tab-prespmt = lv_respmt.                       " / 1000.
      it_row_tab-respmt = lc_respmt.                        " / 1000.
    ELSE.
      it_row_tab-ptypea = 'U'.
    ENDIF.
    it_row_tab-apmyear = s_datum-low.
    it_row_tab-l4f =  all_tcodes-l4f.
    it_row_tab-tcode = all_tcodes-tcode.
    APPEND it_row_tab.
  ENDLOOP.

  _process 'Prepare Screen...' '90'.

  PERFORM   get_gt_out.


ENDFORM.                    " ANALYSIS
*&---------------------------------------------------------------------*
*&      Form  SET_DEFAULT_VALUE
*&---------------------------------------------------------------------*
FORM default.
  CLEAR : wa_apm_user, it_apm_user[], it_apm_user, r_module[],
          r_module, wa_apm_manager.

  IF sy-tcode <> 'ZIT003'.
    MESSAGE i000 WITH 'NO allowed to run like this. Please use T-CODE'.
    LEAVE PROGRAM.
  ENDIF.

  SELECT SINGLE * INTO wa_apm_user
  FROM ztit_apm_user
  WHERE uname  = sy-uname
    AND status = 'X'.
  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'Your ID has No Authorization or Inactive'.
    LEAVE PROGRAM.
  ENDIF.

*-get Module code
  IF wa_apm_user-zrole <> 'A'.
    SELECT  * INTO  TABLE it_apm_user
    FROM ztit_apm_user
    WHERE uname  = sy-uname
      AND status = 'X'.
  ENDIF.

*-get Manager
  IF wa_apm_user-zrole = 'C'.
    SELECT  SINGLE * INTO  wa_apm_manager
    FROM ztit_apm_user
    WHERE zmodule  = wa_apm_user-zmodule
      AND zrole  = 'M'
      AND status = 'X'.
  ENDIF.

  PERFORM modify_screen.

  IF wa_apm_user-zrole <> 'A'.
    LOOP AT it_apm_user.

      r_module-sign   = 'I'.
      r_module-option = 'EQ'.
      r_module-low    = it_apm_user-zmodule.
      APPEND r_module.
    ENDLOOP.
  ENDIF.

*  DATA : lc_date      TYPE sy-datum,
*         lv_date      TYPE sy-datum,
*         lv_time      TYPE sy-uzeit,
*         lv_timestamp TYPE tzntstmpl.
*
*  lv_date = cl_reca_date=>add_to_date(
*            id_months = -1
*            id_years  = 0
*            id_days   = 0
*            id_date   = p_date ).
*
*  WRITE '01' TO lv_date+6(2).
*
*
*  lc_date = cl_reca_date=>add_to_date(
*              id_months = 1
*              id_years  = -1
*              id_days   = 0
*              id_date   = lv_date ).
*
*  lc_date = lc_date - 1.
*
*  s_datum = 'IBT'.
*  s_datum-low = lv_date.
*  s_datum-high = p_date.
*  APPEND s_datum.

ENDFORM.                    " SET_DEFAULT_VALUE
*&---------------------------------------------------------------------*
*&      Form  GET_TIME_STAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_time_stamp .

  gv_date_ext = s_datum-low.

  CALL FUNCTION 'CACS_DATE_GET_TIMESTAMP'
    EXPORTING
      i_date                   = gv_date_ext
    IMPORTING
      e_timestamp              = gv_timestamp_from
    EXCEPTIONS
      date_not_filled_but_time = 1
      date_has_no_valid_format = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
  ENDIF.

  gv_date_ext = s_datum-high.

  CALL FUNCTION 'CACS_DATE_GET_TIMESTAMP'
    EXPORTING
      i_date                   = gv_date_ext
    IMPORTING
      e_timestamp              = gv_timestamp_to
    EXCEPTIONS
      date_not_filled_but_time = 1
      date_has_no_valid_format = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
  ENDIF.


ENDFORM.                    " GET_TIME_STAMP
*&---------------------------------------------------------------------*
*&      Form  GET_ALL_TCODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_all_tcodes .

  _cls all_tcodes.

  SELECT DISTINCT tcode l4f INTO TABLE all_tcodes FROM ztitbpmlh
    WHERE timestamp BETWEEN gv_timestamp_from AND gv_timestamp_to
      AND tcode IN s_tcode.

  SELECT tcode l4f APPENDING TABLE all_tcodes FROM ztitbpml WHERE tcode IN s_tcode.

  SORT all_tcodes.
  DELETE ADJACENT DUPLICATES FROM all_tcodes COMPARING tcode.
  IF s_tcode IS INITIAL.
    DELETE all_tcodes WHERE tcode NP 'Z*'.
  ENDIF.

ENDFORM.                    " GET_ALL_TCODES
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA : l_title(40).
  IF p_disp = 'X'.
    l_title = '- Display'.
  ELSEIF p_new = 'X'.
    l_title = '- Processing + Create New Data'.
  ELSE.
    l_title = '- Processing'.
  ENDIF.

  SET TITLEBAR '100' WITH l_title.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_100 OUTPUT.
  IF g_custom_container IS INITIAL.
    PERFORM create_and_init_alv.

*-  Performance
    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out[]
        it_fieldcatalog      = gt_fcat[]
        it_sort              = gt_sort[].

*-  Not used
    CALL METHOD g_grid1->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layo1
        it_toolbar_excluding = gt_exclude
        i_save               = gc_var_save
        is_variant           = gs_variant
      CHANGING
        it_outtab            = gt_out1[]
        it_fieldcatalog      = gt_fcat1[]
        it_sort              = gt_sort1[].
  ELSE.

  ENDIF.
*  __focus g_grid.

ENDMODULE.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_alv .
  DATA : extab  TYPE TABLE OF sy-ucomm.

  PERFORM create_object.

*   Exclude toolbar
  PERFORM exclude_functions.

  SET PF-STATUS '100'.

  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  CALL METHOD g_grid1->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Event Cell Modified
  CALL METHOD g_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD g_grid1->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*  Create Object to verify input values.
  CREATE OBJECT g_event_receiver.
  SET :
    HANDLER g_event_receiver->handle_data_changed FOR g_grid,
    HANDLER g_event_receiver->handle_data_changed1 FOR g_grid1,
    HANDLER g_event_receiver->handle_double_click FOR g_grid,
    HANDLER g_event_receiver->handle_double_click1 FOR g_grid1,
    HANDLER g_event_receiver->handle_changed_finished FOR g_grid,
    HANDLER g_event_receiver->handle_changed_finished1 FOR g_grid1,
    HANDLER g_event_receiver->handle_on_f4            FOR g_grid,
    HANDLER g_event_receiver->handle_on_f41            FOR g_grid1,
*    HANDLER g_event_receiver->handle_hotspot_click1    FOR g_grid1,
    HANDLER g_event_receiver->handle_button_click      FOR g_grid,
    HANDLER g_event_receiver->handle_button_click1     FOR g_grid1.



*   Create field category
  PERFORM : create_field_category USING false,
            set_lvc_layout,
            set_color.

  PERFORM : create_field_category1 USING false,
            set_lvc_layout1,
            set_color1.

  IF p_disp = 'X'.
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.
    CALL METHOD g_grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.

    CLEAR : extab[], extab.
    APPEND 'SAVE'   TO extab.
    APPEND 'SWITCH' TO extab.
    SET PF-STATUS '100' EXCLUDING extab.

  ENDIF.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
*  gs_variant-variant = p_vari.

ENDFORM.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_functions .
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
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row,
                  cl_gui_alv_grid=>mc_fc_loc_delete_row.

ENDFORM.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
FORM create_field_category  USING    p_false.

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
    gs_fcat-emphasize    = &6.
    gs_fcat-just      = &7.
    gs_fcat-edit      = &8.
    gs_fcat-no_zero   = &9.
    append gs_fcat to gt_fcat.
  END-OF-DEFINITION.

  __catalog :
            'X'  'ICON'         'Status'            4  'CHAR' '' 'C' '' 'X' ,
          'X'  'CHK'          'Chk'               1  'CHAR' 'C310' 'C' 'X' 'X' ,
          'X'  'CSTATUS'      'Status'            2  'NUMC' 'X' 'C' '' 'X',
          'X'  'CSTATUS_NM'    'Status'          20  'CHAR' 'X' 'L' '' 'X',
*          'X'  'COMP'         'Company'           4  'CHAR' '' 'C' '' 'X',
*          'X'  'APMYEAR'      'Year'              4  'NUMC' '' 'L' '' 'X',
          'X'  'L2'           'Module'          6    'CHAR' '' 'L' '' 'X',
*          'X'  'L4F'          'L4F'              10  'CHAR' '' 'L' '' 'X',
          'X'  'TCODE'        'TCode'            20  'CHAR' '' 'L' '' 'X',
          'X'   'TTEXT'        'TCode Name'       30  'CHAR' '' 'L' '' 'X',
          'X'  'ZSEQ'         'Seq.'             4   'NUMC' '' 'R' '' 'X' ,
*          ''   'L3T'          'L3T'               4  'CHAR' '' 'L' '' 'X',
*          ''   'L4T'          'L4T'              20  'CHAR' '' 'L' '' 'X',
*          ' '  'L5T'          'L5T'              20  'CHAR' '' 'L' '' 'X',
*          'X'  'CSTATUS'      'Status'            2  'NUMC' '' 'C' 'X' 'X',
          ' '  'PTYPEA'       'Type'              1  'CHAR' '' 'L' '' 'X',
          ' '  'PTYPEA_NM'    'Type'             20  'CHAR' '' 'L' '' 'X',
          ' '  'RESPMT'       'Resp.Time'         6  'DECS' '' 'R' '' 'X' ,
          ' '  'CPUMT'        'Cpu Time'          6  'DECS' '' 'R' '' 'X' ,
          ' '  'DBMT'         'DB Time'           6  'DECS' '' 'R' '' 'X' ,
          ' '  'CREATCNT'     'Reg.Cnt'           10  'INT4' '' 'L' '' 'X' ,
          ' '  'CDATE'        'Reg.Init.Date'     8  'DATS' '' 'L' '' 'X',
          ' '  'PTYPEF'       'Revised Type'      1  'CHAR' 'C310' 'L' 'X' 'X',
          ' '  'PTYPEF_NM'     'Revised Type'     20  'CHAR' '' 'L' '' 'X',
          ' '  'PTYPEF_ID'     'Revised User'     12  'CHAR' '' 'L' '' 'X',
*          ' '  'UDATE'        'Reg.Upd.Date'      8  'DATS' '' 'L' '' 'X',
          ' '  'CONSULTANT'   'Consultant'        12  'CHAR' 'C310' 'L' 'X' 'X',
          ' '  'FULLNAME'     'Name       '     20 'CHAR' '' 'L' '' 'X',
          ' '  'E_MAIL_ICON'   'Send E-mail'       4  'CHAR' '' 'L' '' 'X',
          ' '  'E_MAIL_STAT'   'E-mail Sent'       10  'CHAR' '' 'L' '' 'X',
*          ' '  'E_MAIL'        'E-mail'           10  'CHAR' '' 'L' '' 'X',
          ' '  'PACCEPT'      'Accepted'     	    1  'CHAR' 'C510' 'L' 'X' 'X',
          ' '  'ACCEPTER'     'Accepted by'    	 12  'CHAR' '' 'L' '' 'X',
          ' '  'ACCEPTDT'     'Accepted on'    	  8  'DATS' '' 'L' '' 'X',
*          ' '  'UREASON'      'Unused Reason'     2  'CHAR' 'C510' 'L' 'X' 'X',
*          ' '  'UREASON_NM'   'Unused Reason'    20  'CHAR' '' 'L' '' 'X',
*          ' '  'URESULT'      'Analysis Result'   2  'CHAR' 'C510' 'R' 'X' 'X',
*          ' '  'URESULT_NM'   'Analysis Result'  20  'CHAR' '' 'L' '' 'X',

          ' '  'PRESULT'      'Analysis Result'   1  'CHAR' 'C510' 'L' 'X' 'X' ,
          ' '  'PRESULT_NM'   'Analysis Result'  20  'CHAR' '' 'L' '' 'X',

          ' '  'ENH_ID'       'Analyzed by'      12  'CHAR' '' 'L' '' 'X' ,
          ' '  'ENH_DT'       'Analyzed on'       8  'DATS' '' 'L' '' 'X' ,
          ' '  'RESULT_COMMENT'   'Analysis Result Comment'         50
                                                     'CHAR' 'C510' 'L' 'X' 'X' ,
          ' '  'ENH_PLANDT'   'Enhance Plan Date' 8  'DATS' 'C510' 'L' 'X' 'X' ,

          ' '  'PLAN_APPROVED' 'Plan Appr.Flag'   1  'CHAR' 'C710' 'L' 'X' 'X',
          ' '  'PLAN_APPR'     'Plan Appr. by'   12  'CHAR' '' 'L' '' 'X',
          ' '  'PLAN_APPRDT'   'Plan Appr. on'    8  'DATS' '' 'L' '' 'X',

          ' '  'CLOSED'     'Close Req.Indicator'  1  'CHAR' 'C510' 'L' 'X' 'X',
          ' '  'CLOSEDBY'   'Close Req. by'    	 12  'CHAR' '' 'L' '' 'X',
          ' '  'CLOSEDT'    'Close Req. on'         8  'DATS' '' 'L' '' 'X',

          ' '  'ZCONFIRM'   'Close Confirm. Indi.'     1  'CHAR' 'C710' 'L' 'X' 'X',
          ' '  'ZCONFIRMEDBY' 'Confirmed by'     12  'CHAR' '' 'L' '' 'X',
          ' '  'ZCONFIRMDT'   'Confirmed on'       8  'DATS' '' 'L' '' 'X',
          ' '  'CLOSE_COMMENT'    'Close Confirm. Comment'         50
                                                     'CHAR' 'C710' 'L' 'X' 'X',
          ' '  'ISSUE_COMMENT'    'Issue Comment'                   50
                                                     'CHAR' '' 'L' 'X' 'X' .

*---Delete below comment later
*          'X'  'ICON'         'Status'            4  'CHAR' '' 'C' '' 'X' ,
*          'X'  'CHK'          'Chk'               1  'CHAR' 'C310' 'C' 'X' 'X' ,
*          'X'  'COMP'         'Company'           4  'CHAR' '' 'C' '' 'X' ,
*          'X'  'APMYEAR'      'Year'              4  'NUMC' '' 'L' '' 'X' ,
*          'X'  'L4F'          'L4F'              10  'CHAR' '' 'L' '' 'X' ,
*          'X'  'TCODE'        'TCode'            20  'CHAR' '' 'L' '' 'X' ,
*          'X'  'ZSEQ'         'Seq.'             4   'NUMC' '' 'R' '' 'X' ,
*          ''   'TTEXT'        'TCode Name'       30  'CHAR' '' 'L' '' 'X' ,
*          ''   'L3T'          'L3T'               4  'CHAR' '' 'L' '' 'X' ,
*          ''   'L4T'          'L4T'              20  'CHAR' '' 'L' '' 'X' ,
*          ' '  'L5T'          'L5T'              20  'CHAR' '' 'L' '' 'X' ,
*          ' '  'CSTATUS'      'Status'            2  'NUMC' 'C310' 'C' 'X' 'X',
*          ' '  'CSTATUS_NM'    'Status'          20  'CHAR' '' 'L' '' 'X',
*          ' '  'PTYPEA'       'Type'              1  'CHAR' '' 'L' '' 'X' ,
*          ' '  'PTYPEA_NM'    'Type'             20  'CHAR' '' 'L' '' 'X',
*          ' '  'RESPMT'       'Resp.Time'         6  'DECS' '' 'R' '' 'X' ,
*          ' '  'CPUMT'        'Cpu Time'          6  'DECS' '' 'R' '' 'X' ,
*          ' '  'DBMT'         'DB Time'           6  'DECS' '' 'R' '' 'X' ,
*          ' '  'CREATCNT'     'Reg.Cnt'           10  'INT4' '' 'L' '' 'X' ,
*          ' '  'CDATE'        'Reg.Init.Date'     8  'DATS' '' 'L' '' 'X',
*          ' '  'PTYPEF'       'Revised Type'      1  'CHAR' 'C310' 'L' 'X' 'X',
*          ' '  'PTYPEF_NM'     'Revised Type'     20  'CHAR' '' 'L' '' 'X',
**          ' '  'UDATE'        'Reg.Upd.Date'      8  'DATS' '' 'L' '' 'X',
*          ' '  'PACCEPT'      'Accepted'           1  'CHAR' 'C310' 'L' 'X' 'X',
*          ' '  'ACCEPTER'     'Accepted by'       12  'CHAR' '' 'L' '' 'X',
*          ' '  'ACCEPTDT'     'Accepted on'        8  'DATS' '' 'L' '' 'X',
*          ' '  'PRESULT'      'Analysis Result'   1  'CHAR' 'C310' 'R' 'X' 'X' ,
*          ' '  'PRESULT_NM'   'Analysis Result'  20  'CHAR' '' 'L' '' 'X',
*          ' '  'ENH_ID'       'Analyzed by'      12  'CHAR' '' 'L' '' 'X' ,
*          ' '  'ENH_DT'       'Analyzed on'       8  'DATS' '' 'L' '' 'X' ,
*          ' '  'ENH_PLANDT'   'Enhance Plan Date' 8  'DATS' 'C310' 'L' 'X' 'X' ,
*          ' '  'CLOSED'       'Close.Flag'        1  'CHAR' 'C310' 'L' 'X' 'X' ,
*          ' '  'CLOSEDBY'     'Closed by'         12  'CHAR' '' 'L' '' 'X' ,
*          ' '  'CLOSEDT'      'Closed on'         8  'DATS' '' 'L' '' 'X' ,
**          ' '  'ZCONFIRMEDBY' 'Confirmed by'     12  'CHAR' '' 'L' '' 'X' ,
**          ' '  'ZCONFIRMDT'   'Confirm on'         8  'DATS' '' 'L' '' 'X' ,
*          ' '  'RESULT_COMMENT'   'Analysis Result Comment'         50
*                                                     'CHAR' 'C310' 'L' 'X' 'X' ,
*          ' '  'CLOSE_COMMENT'    'Close(Complete) Comment'         50
*                                                     'CHAR' 'C310' 'L' 'X' 'X' ,
*          ' '  'ISSUE_COMMENT'    'Issue Comment'                   50
*                                                     'CHAR' 'C310' 'L' 'X' 'X' .
**          ' '  'PRESPMT'       'P.AvgR'           6  'DECS' '' 'R' '' 'X' .



  LOOP AT gt_fcat INTO gs_fcat.
    IF gs_fcat-fieldname = 'CHK'.
      gs_fcat-checkbox = 'X'.
    ELSEIF gs_fcat-fieldname = 'E_MAIL_STAT'.
      gs_fcat-checkbox = 'X'.
    ELSEIF gs_fcat-fieldname = 'E_MAIL_ICON'.
      gs_fcat-icon = 'X'.
    ELSE.
      gs_fcat-ref_field = gs_fcat-fieldname.
      gs_fcat-ref_table = 'ZTITBPMA'.
    ENDIF.

    IF wa_apm_user-zrole = 'A'.     "Administrator
      IF gs_fcat-fieldname = 'PACCEPT'    OR  gs_fcat-fieldname = 'UREASON'
        OR gs_fcat-fieldname = 'URESULT'  OR gs_fcat-fieldname = 'ENH_PLANDT'
        OR gs_fcat-fieldname = 'CLOSED'   OR gs_fcat-fieldname = 'RESULT_COMMENT'.

        gs_fcat-edit = ''.
      ENDIF.
    ELSEIF wa_apm_user-zrole = 'C'. "Consultant
      IF gs_fcat-fieldname = 'PTYPEF'         OR  gs_fcat-fieldname = 'CONSULTANT'
        OR gs_fcat-fieldname = 'E_MAIL_STAT'  OR gs_fcat-fieldname = 'PLAN_APPROVED'
        OR gs_fcat-fieldname = 'ZCONFIRM'     OR gs_fcat-fieldname = 'CLOSE_COMMENT'.

        gs_fcat-edit = ''.
      ENDIF.

    ELSEIF wa_apm_user-zrole = 'M'. "Manager
      IF gs_fcat-fieldname = 'PTYPEF'         OR gs_fcat-fieldname = 'CONSULTANT'
        OR gs_fcat-fieldname = 'E_MAIL_STAT'  OR gs_fcat-fieldname = 'PACCEPT'
        OR gs_fcat-fieldname = 'UREASON'      OR gs_fcat-fieldname = 'URESULT'
        OR gs_fcat-fieldname = 'ENH_PLANDT'   OR gs_fcat-fieldname = 'CLOSED'
        OR gs_fcat-fieldname = 'RESULT_COMMENT'.

        gs_fcat-edit = ''.
      ENDIF.
    ENDIF.

    MODIFY gt_fcat FROM gs_fcat.
  ENDLOOP.

*  LOOP AT gt_fcat INTO gs_fcat.
*    IF gs_fcat-fieldname = 'CHK'.
*      gs_fcat-checkbox = 'X'.
*    ELSE.
*      gs_fcat-ref_field = gs_fcat-fieldname.
*      gs_fcat-ref_table = 'ZTITBPMA'.
*    ENDIF.
*    MODIFY gt_fcat FROM gs_fcat.
*  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
FORM data_changed USING pr_data_changed TYPE REF TO
                                     cl_alv_changed_data_protocol
                        p_ucomm.

  DATA: ls_mod_cells        TYPE lvc_s_modi,
        lt_mod_cells        TYPE lvc_t_modi.

  CLEAR: ls_mod_cells, lt_mod_cells,   bapiaddr3, l_address,
         it_return, it_return[], lt_mod_cells[], lt_mod_cells.

  lt_mod_cells = pr_data_changed->mt_good_cells.
  SORT lt_mod_cells BY row_id.

  LOOP AT lt_mod_cells INTO ls_mod_cells.
    READ TABLE gt_out INDEX   ls_mod_cells-row_id.

    CASE ls_mod_cells-fieldname.
      WHEN 'CSTATUS'.
        READ TABLE it_cstatus WITH KEY domvalue_l  = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out-cstatus_nm  =  it_cstatus-ddtext.
          IF ls_mod_cells-value < '20'.
            gt_out-icon    = '@5C@'.
          ELSEIF ls_mod_cells-value >= '20'
                                      AND ls_mod_cells-value < '39'.
            gt_out-icon    = '@5D@'.
          ELSE.
            gt_out-icon    = '@5B@'.
          ENDIF.

          gt_out-chk = 'X'.
          MODIFY gt_out INDEX  ls_mod_cells-row_id
                                  TRANSPORTING  chk cstatus_nm icon.
          PERFORM refresh_grid USING 'G_GRID'.
        ENDIF.

      WHEN 'PTYPEF'.
        IF gt_out-paccept IS NOT INITIAL. "Recover to Original Value
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                  TRANSPORTING ptypef.
          PERFORM refresh_grid USING 'G_GRID'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        READ TABLE it_ptype WITH KEY domvalue_l  = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out-ptypef_nm  = it_ptype-ddtext.
          gt_out-ptypef_id  = sy-uname.
          gt_out-chk = 'X'.
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                TRANSPORTING chk ptypef_nm ptypef_id.
          PERFORM refresh_grid USING 'G_GRID'.
        ENDIF.

      WHEN 'PACCEPT'.
        IF gt_out-e_mail_stat IS  INITIAL
                        OR gt_out-plan_approved = 'Y'.
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                  TRANSPORTING paccept.
          PERFORM refresh_grid USING 'G_GRID'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out-accepter  = sy-uname.
          gt_out-acceptdt  = sy-datum.
        ELSE.
          gt_out-accepter  = ''.
          gt_out-acceptdt  = ''.
        ENDIF.
        PERFORM set_status USING gt_out  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk accepter acceptdt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'CONSULTANT'.
        IF gt_out-paccept IS NOT INITIAL. "Recover to Original Value
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                  TRANSPORTING consultant.
          PERFORM refresh_grid USING 'G_GRID'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        IF ls_mod_cells-value IS NOT INITIAL.
          l_consultant_id = ls_mod_cells-value.
          CALL FUNCTION 'BAPI_USER_GET_DETAIL'
            EXPORTING
              username = l_consultant_id
            IMPORTING
              address  = l_address
            TABLES
              return   = it_return.

          CONCATENATE l_address-firstname l_address-lastname
                      INTO gt_out-fullname SEPARATED BY space.
        ELSE.
          gt_out-fullname = ''.
        ENDIF.

        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk fullname.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'PRESULT'.
        READ TABLE it_presult WITH KEY domvalue_l = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out-presult_nm  = it_presult-ddtext.
          gt_out-enh_id  = sy-uname.
          gt_out-enh_dt  = sy-datum.
          gt_out-chk = 'X'.
          MODIFY gt_out INDEX  ls_mod_cells-row_id
                          TRANSPORTING chk enh_id enh_dt presult_nm.
          PERFORM refresh_grid USING 'G_GRID'.
        ENDIF.

      WHEN 'ENH_PLANDT'.
        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          wa_apm_manager-uname
                                          gt_out.
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                   TRANSPORTING chk.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'RESULT_COMMENT'.
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                   TRANSPORTING chk.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'PLAN_APPROVED'.      "Plan Approval Indicator

        IF gt_out-enh_plandt IS  INITIAL OR gt_out-paccept <> 'Y'
              OR gt_out-closed = 'X'.
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                  TRANSPORTING plan_approved.
          PERFORM refresh_grid USING 'G_GRID'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          gt_out-consultant
                                          gt_out.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out-plan_appr     = sy-uname.
          gt_out-plan_apprdt   = sy-datum.
        ELSE.
          gt_out-plan_appr     = ''.
          gt_out-plan_apprdt   = ''.
        ENDIF.

        PERFORM set_status USING gt_out  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk plan_appr plan_apprdt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'CLOSED'.  "Close Request
        IF gt_out-zconfirm = 'Y' OR ( gt_out-paccept = 'Y'
              AND gt_out-plan_approved <> 'Y' ).
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                  TRANSPORTING zconfirm.
          PERFORM refresh_grid USING 'G_GRID'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          wa_apm_manager-uname
                                          gt_out.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out-closedby  = sy-uname.
          gt_out-closedt   = sy-datum.
        ELSE.
          gt_out-closedby  = ''.
          gt_out-closedt   = ''.
        ENDIF.
        PERFORM set_status USING gt_out  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk closedby closedt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'ZCONFIRM'.
        IF gt_out-closed <> 'X'.
          MODIFY gt_out INDEX ls_mod_cells-row_id
                                  TRANSPORTING closed.
          PERFORM refresh_grid USING 'G_GRID'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          gt_out-consultant
                                          gt_out.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out-zconfirmedby  = sy-uname.
          gt_out-zconfirmdt    = sy-datum.
        ELSE.
          gt_out-zconfirmedby  = ''.
          gt_out-zconfirmdt    = ''.
        ENDIF.
        PERFORM set_status USING gt_out  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk zconfirmedby zconfirmdt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID'.

      WHEN 'CLOSE_COMMENT' OR 'ISSUE_COMMENT'.
        gt_out-chk = 'X'.
        MODIFY gt_out INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk .
        PERFORM refresh_grid USING 'G_GRID1'.
    ENDCASE.

  ENDLOOP.

*
*
*
*
*
*
*  DATA: ls_mod_cells        TYPE lvc_s_modi,
*        lt_mod_cells        TYPE lvc_t_modi.
*
*  CLEAR: ls_mod_cells, lt_mod_cells.
*  REFRESH: lt_mod_cells.
*
*  lt_mod_cells = pr_data_changed->mt_good_cells.
*  SORT lt_mod_cells BY row_id.
*
*  LOOP AT lt_mod_cells INTO ls_mod_cells.
*
*
*
*
*    CASE ls_mod_cells-fieldname.
*      WHEN 'CSTATUS'.
*        READ TABLE it_cstatus WITH KEY domvalue_l  = ls_mod_cells-value.
*        IF sy-subrc = 0.
*          gt_out-cstatus_nm  =  it_cstatus-ddtext.
*          IF ls_mod_cells-value < '20'.
*            gt_out-icon    = '@5C@'.
*          ELSEIF ls_mod_cells-value >= '20'
*                                      AND ls_mod_cells-value < '30'.
*            gt_out-icon    = '@5D@'.
*          ELSE.
*            gt_out-icon    = '@5B@'.
*          ENDIF.
*          gt_out-chk = 'X'.
*          MODIFY gt_out INDEX  ls_mod_cells-row_id
*                                  TRANSPORTING chk cstatus_nm icon.
*          PERFORM refresh_grid USING 'G_GRID'.
*        ENDIF.
*      WHEN 'PTYPEF'.
*        READ TABLE it_ptype WITH KEY domvalue_l  = ls_mod_cells-value.
*        IF sy-subrc = 0.
*          gt_out-ptypef_nm  = it_ptype-ddtext.
*          gt_out-chk = 'X'.
*          MODIFY gt_out INDEX ls_mod_cells-row_id
*                              TRANSPORTING chk ptypef_nm.
*          PERFORM refresh_grid USING 'G_GRID'.
*        ENDIF.
*
*      WHEN 'PACCEPT'.
*        IF ls_mod_cells-value IS NOT INITIAL.
*          gt_out-accepter  = sy-uname.
*          gt_out-acceptdt  = sy-datum.
*        ELSE.
*          gt_out-accepter  = ''.
*          gt_out-acceptdt  = ''.
*        ENDIF.
*        gt_out-chk = 'X'.
*        MODIFY gt_out INDEX  ls_mod_cells-row_id
*                        TRANSPORTING chk accepter acceptdt.
*        PERFORM refresh_grid USING 'G_GRID'.
*
*      WHEN 'PRESULT'.
*        READ TABLE it_presult WITH KEY domvalue_l = ls_mod_cells-value.
*        IF sy-subrc = 0.
*          gt_out-presult_nm  = it_presult-ddtext.
*          gt_out-enh_id  = sy-uname.
*          gt_out-enh_dt  = sy-datum.
*          gt_out-chk = 'X'.
*          MODIFY gt_out INDEX  ls_mod_cells-row_id
*                          TRANSPORTING chk enh_id enh_dt presult_nm.
*          PERFORM refresh_grid USING 'G_GRID'.
*        ENDIF.
*
*      WHEN 'CLOSED'.
*        IF ls_mod_cells-value IS NOT INITIAL.
*          gt_out-closedby  = sy-uname.
*          gt_out-closedt   = sy-datum.
*        ELSE.
*          gt_out-closedby  = ''.
*          gt_out-closedt   = ''.
*        ENDIF.
*        gt_out-chk = 'X'.
*        MODIFY gt_out INDEX  ls_mod_cells-row_id
*                        TRANSPORTING chk closedby closedt.
*        PERFORM refresh_grid USING 'G_GRID'.
*    ENDCASE.
*
*  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM double_click USING  e_row     TYPE lvc_s_row
                         e_column  TYPE lvc_s_col
                         es_row_no TYPE lvc_s_roid.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_lvc_layout .
  DATA : l_cnt(3)..

  DESCRIBE TABLE gt_out LINES l_cnt.

  CLEAR gs_layo.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.
  CONCATENATE 'Performance Plan' '-lines:' l_cnt
                                        INTO gs_layo-grid_title.
ENDFORM.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_color .

ENDFORM.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PF_TEXT    text
*      -->VALUE      text
*      -->(PF_VAL)   text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            value(pf_val).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = pf_val
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  GET_GT_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gt_out .
  _cls gt_out.
  LOOP AT it_row_tab.
    MOVE-CORRESPONDING it_row_tab TO gt_out.
    APPEND gt_out.
  ENDLOOP.
ENDFORM.                    " GET_GT_OUT
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
  ok_code = sy-ucomm.
  CLEAR sy-ucomm.
  CASE ok_code.
    WHEN 'BACK' OR 'CANC' OR 'EXIT'.
      PERFORM check_modification.
*      LEAVE TO SCREEN 0.
*    WHEN 'EXIT'.
*      LEAVE PROGRAM.
    WHEN 'SAVE'.
      PERFORM save_data.

      CHECK l_answer = '1'.
      CALL METHOD g_grid->refresh_table_display.
      CALL METHOD g_grid1->refresh_table_display.

    WHEN 'SWITCH'.
      PERFORM switch_edit_mode.
      __focus g_grid.
  ENDCASE.
ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
FORM save_data .
  DATA : it_save_log LIKE ztitbpma_h OCCURS 0 WITH HEADER LINE.
  DATA : it_row  TYPE lvc_t_row,
         is_row  TYPE lvc_s_row,
         it_roid TYPE lvc_t_roid.

  DATA : l_tcode LIKE  ztitbpma-tcode,
         l_zseq  LIKE ztitbpma-zseq,
         l_cnt   TYPE i,
         l_index TYPE i,
         l_date_seq TYPE ztitbpma_h-zdate_seq.
  DATA : l_text(50) VALUE 'Do you want to save Data?'.

  CLEAR : it_save[], it_save, it_save_log[], it_save_log.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmation'
      text_question         = l_text
      text_button_1         = 'YES'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'NO'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = l_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_answer = '1'.

  SELECT zdate_seq INTO l_date_seq
  FROM  ztitbpma_h
    UP TO 1 ROWS
  WHERE zdate = sy-datum
  ORDER BY zdate_seq DESCENDING.
  ENDSELECT.

*-Performance
  LOOP AT gt_out WHERE chk = 'X'.
    MOVE-CORRESPONDING gt_out TO it_save.
    APPEND it_save. CLEAR it_save.
  ENDLOOP.

*  CALL METHOD g_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = it_row
*      et_row_no     = it_roid.
*
*  LOOP AT it_row INTO is_row.
*    CLEAR : it_save, wa_out.
*    READ TABLE gt_out INDEX is_row-index INTO wa_out.
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING wa_out TO it_save.
*      APPEND it_save.
*    ENDIF.
*  ENDLOOP.

*-Not used
  LOOP AT gt_out1 WHERE chk = 'X'.
    MOVE-CORRESPONDING gt_out1 TO it_save.
    APPEND it_save. CLEAR it_save.
  ENDLOOP.

*  CALL METHOD g_grid1->get_selected_rows
*    IMPORTING
*      et_index_rows = it_row
*      et_row_no     = it_roid.
*
*  LOOP AT it_row INTO is_row.
*    CLEAR : it_save, wa_out1.
*    READ TABLE gt_out1 INDEX is_row-index INTO wa_out1.
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING wa_out1 TO it_save.
*      APPEND it_save.
*    ENDIF.
*  ENDLOOP.


  IF it_save[] IS INITIAL.
    MESSAGE e000 WITH 'No Data was selected. Please check Data'.
  ENDIF.

  SORT it_save BY tcode zseq DESCENDING.

  LOOP AT it_save.
    l_index = sy-tabix.

    IF it_save-zseq IS INITIAL.
      IF it_save-tcode <> l_tcode.
        CLEAR : l_cnt, l_zseq.
        SELECT zseq INTO l_zseq
        FROM ztitbpma
          UP TO 1 ROWS
        WHERE tcode = it_save-tcode
        ORDER BY zseq DESCENDING.
        ENDSELECT.
      ENDIF.
      l_cnt = l_cnt + 1.
      it_save-zseq  = l_zseq + l_cnt.
      MODIFY it_save.
      l_tcode = it_save-tcode.

*-apply to ALV data
      IF it_save-ptypea = 'P'.
        READ TABLE gt_out WITH KEY l4f    = it_save-l4f
                                   tcode  = it_save-tcode.
        IF sy-subrc = 0.
          gt_out-zseq = it_save-zseq.
          MODIFY gt_out INDEX sy-tabix FROM gt_out TRANSPORTING zseq.
        ENDIF.
      ELSE.
        READ TABLE gt_out1 WITH KEY l4f    = it_save-l4f
                                    tcode  = it_save-tcode.
        IF sy-subrc = 0.
          gt_out1-zseq = it_save-zseq.
          MODIFY gt_out1 INDEX sy-tabix FROM gt_out1 TRANSPORTING zseq.
        ENDIF.

      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING it_save TO it_save_log.
    it_save_log-zdate       = sy-datum.
    it_save_log-zdate_seq   = l_date_seq + l_index.
    it_save_log-ztime       = sy-uzeit.
    it_save_log-ernam       = sy-uname.

    APPEND it_save_log.
  ENDLOOP.

  MODIFY ztitbpma FROM TABLE it_save.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE s000 WITH 'Data was successfully Saved'.
    INSERT ztitbpma_h FROM TABLE it_save_log ACCEPTING DUPLICATE KEYS.

    gt_out-chk  = ''.
    gt_out1-chk = ''.
    MODIFY gt_out TRANSPORTING chk WHERE chk = 'X'.
    MODIFY gt_out1 TRANSPORTING chk WHERE chk = 'X'.

  ELSE.
    ROLLBACK WORK.
    MESSAGE e000 WITH 'Error during Saving Data'.
  ENDIF.
ENDFORM.                    " SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_12_AVERAGE_PERFORMANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALL_TCODES_TCODE  text
*      -->P_LV_RESPMT  text
*----------------------------------------------------------------------*
FORM get_12_average_performance  USING    p_tcode
                                          p_respmt.

  SELECT AVG( respmt ) INTO p_respmt  FROM zthrappusge
    WHERE tcode = p_tcode
      AND ldate   IN r_usage_d.

*  P_RESPMT = P_RESPMT / 1000.

ENDFORM.                    " GET_12_AVERAGE_PERFORMANCE
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM switch_edit_mode .
  DATA answer.
  IF g_grid->is_ready_for_input( ) EQ 0.
    CLEAR flag_data_changed.
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CALL METHOD g_grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.
    SET PF-STATUS '100'.
  ELSE.
    IF flag_data_changed EQ true.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
        EXPORTING
          textline1     = 'Data has not been saved yet.'
          textline2     = 'Do you want to continue anyway? '
          titel         = 'Confirmation'
          defaultoption = 'N'
        IMPORTING
          answer        = answer.
      CHECK answer EQ 'J'.
    ENDIF.
    CLEAR flag_data_changed.
    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.
    CALL METHOD g_grid1->set_ready_for_input
      EXPORTING
        i_ready_for_input = 0.
    SET PF-STATUS '100' EXCLUDING 'SAVE'.
  ENDIF.

ENDFORM.                    " SWITCH_EDIT_MODE

*&---------------------------------------------------------------------*
*&      Form  GET_TIME_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_time_period .
  DATA : lc_date      TYPE sy-datum,
         lv_date      TYPE sy-datum,
         lv_time      TYPE sy-uzeit,
         lv_timestamp TYPE tzntstmpl.
  DATA : tmp_date TYPE i.

  _cls :r_usage_d, r_perfo_d.

  lc_date = s_datum-low - 1.

  tmp_date  = - p_usage.
  lv_date = cl_reca_date=>add_to_date(
              id_months = 0
              id_years  = 0
              id_days   = tmp_date
              id_date   = s_datum-low ).

  r_usage_d = 'IBT'.
  r_usage_d-low = lv_date.
  r_usage_d-high = lc_date.
  APPEND r_usage_d.

  tmp_date  = - p_perfo.
  lv_date = cl_reca_date=>add_to_date(
              id_months = 0
              id_years  = 0
              id_days   = tmp_date
              id_date   = s_datum-low ).

  r_perfo_d = 'IBT'.
  r_perfo_d-low = lv_date.
  r_perfo_d-high = lc_date.
  APPEND r_perfo_d.

ENDFORM.                    " GET_TIME_PERIOD
*&---------------------------------------------------------------------*
*&      Form  GET_NOTUSED_DATA
*&---------------------------------------------------------------------*
FORM get_notused_data .
  DATA : BEGIN OF it_usage_tcode OCCURS 0,
          tcode LIKE  ztitbpml_log-tcode,
         END OF it_usage_tcode.

  CLEAR : it_notused_collect, it_notused_collect[], gt_out1, gt_out1[].


  SELECT * INTO TABLE it_notused
  FROM ztitbpml_log
  WHERE zdate IN r_usage_d
    AND apm = 'X'
    AND l2    IN r_module.

*-data is not Enough -> use zthrappusge until 2015
  IF sy-datum < '20150101'.
    SELECT DISTINCT tcode
            INTO CORRESPONDING FIELDS OF TABLE it_usage_tcode
    FROM zthrappusge
    WHERE ldate IN  r_usage_d
      AND tcode LIKE 'Z%'.
  ENDIF.

  SORT it_notused BY tcode zdate.
  SORT it_usage_tcode BY tcode.

  LOOP AT it_notused.
    MOVE-CORRESPONDING it_notused TO it_notused_collect.
    COLLECT it_notused_collect.
  ENDLOOP.

*-Delete more than one time used t-code for the period
  DELETE it_notused_collect WHERE cnt > 0.

  SORT it_notused_collect BY tcode l4f.
  DELETE ADJACENT DUPLICATES FROM it_notused_collect
                                   COMPARING  tcode.
*-if ztitbpml_log data is Not enough, also use zthrappusge
  LOOP AT it_notused_collect.
    READ TABLE it_usage_tcode
          WITH KEY tcode = it_notused_collect-tcode
          BINARY SEARCH.
    IF sy-subrc = 0.
      DELETE it_notused_collect.
    ENDIF.
  ENDLOOP.

  LOOP AT it_notused_collect.
    MOVE-CORRESPONDING it_notused_collect  TO    gt_out1.
    gt_out1-cstatus = '10'.
    gt_out1-ptypea  = 'U'.  "Utilization low
    gt_out1-apmyear = sy-datum+0(4).
    gt_out1-cdate   = sy-datum.
    gt_out1-icon    = '@5C@'.
    APPEND gt_out1.
  ENDLOOP.

*-Merge data with ZTITBPMA
*  LOOP AT it_processing WHERE ptypea = 'U'.
*    CLEAR : it_bpmlh , gt_out1.
**    DELETE gt_out1        WHERE l4f   = it_processing-l4f
**                            AND tcode = it_processing-tcode.
*    DELETE gt_out1        WHERE  tcode = it_processing-tcode.
*    MOVE-CORRESPONDING it_processing TO gt_out1.
*
*    READ TABLE it_bpmlh WITH KEY l4f    = it_processing-l4f
*                                 tcode  = it_processing-tcode
*                                 BINARY SEARCH.
*
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING it_bpmlh TO  gt_out1.
*      IF gt_out1-cstatus <= '10'.
*        gt_out1-icon    = '@5C@'.
*      ELSEIF gt_out1-cstatus > '10' AND gt_out1-cstatus < '39'.
*        gt_out1-icon    = '@5D@'.
*      ELSE.
*        gt_out1-icon    = '@5B@'.
*      ENDIF.
*      gt_out1-l3t = it_bpmlh-l3.
*    ENDIF.
*    APPEND  gt_out1.
*  ENDLOOP.

* Fill the data
*  LOOP AT gt_out1.
*    READ TABLE it_cstatus WITH KEY domvalue_l  = gt_out1-cstatus.
*    IF sy-subrc = 0.
*      gt_out1-cstatus_nm  =  it_cstatus-ddtext.
*    ENDIF.
*    MODIFY gt_out1.
*  ENDLOOP.
ENDFORM.                    " GET_NOTUSED_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
FORM create_field_category1  USING    p_false.

  DATA: l_pos       TYPE i.
  DEFINE __catalog1.
    l_pos = l_pos + 1.
    clear gs_fcat1.
    gs_fcat1-col_pos       = l_pos.
    gs_fcat1-key           = &1.
    gs_fcat1-fieldname     = &2.
    gs_fcat1-coltext       = &3.     " Column heading
    gs_fcat1-outputlen     = &4.     " Column width
    gs_fcat1-datatype      = &5.     " Data type
    gs_fcat1-emphasize     = &6.
    gs_fcat1-just      = &7.
    gs_fcat1-edit      = &8.
    gs_fcat1-no_zero   = &9.
    append gs_fcat1 to gt_fcat1.
  END-OF-DEFINITION.

  __catalog1 :
          'X'  'ICON'         'Status'            4  'CHAR' '' 'C' '' 'X' ,
          'X'  'CHK'          'Chk'               1  'CHAR' 'C310' 'C' 'X' 'X' ,
          'X'  'CSTATUS'      'Status'            2  'NUMC' 'X' 'C' '' 'X',
          'X'  'CSTATUS_NM'    'Status'          20  'CHAR' 'X' 'L' '' 'X',
*          'X'  'COMP'         'Company'           4  'CHAR' '' 'C' '' 'X',
*          'X'  'APMYEAR'      'Year'              4  'NUMC' '' 'L' '' 'X',
          'X'  'L2'           'Module'          6    'CHAR' '' 'L' '' 'X',
*          'X'  'L4F'          'L4F'              10  'CHAR' '' 'L' '' 'X',
          'X'  'TCODE'        'TCode'            20  'CHAR' '' 'L' '' 'X',
          'X'   'TTEXT'        'TCode Name'       30  'CHAR' '' 'L' '' 'X',
          'X'  'ZSEQ'         'Seq.'             4   'NUMC' '' 'R' '' 'X' ,
*          ''   'L3T'          'L3T'               4  'CHAR' '' 'L' '' 'X',
*          ''   'L4T'          'L4T'              20  'CHAR' '' 'L' '' 'X',
*          ' '  'L5T'          'L5T'              20  'CHAR' '' 'L' '' 'X',
*          'X'  'CSTATUS'      'Status'            2  'NUMC' '' 'C' 'X' 'X',
          ' '  'PTYPEA'       'Type'              1  'CHAR' '' 'L' '' 'X',
          ' '  'PTYPEA_NM'    'Type'             20  'CHAR' '' 'L' '' 'X',
*          ' '  'CREATCNT'     'Reg.Cnt'       10  'INT4' '' 'L' '' 'X',
          ' '  'CDATE'        'Reg.Init.Date'     8  'DATS' '' 'L' '' 'X',
          ' '  'PTYPEF'       'Revised Type'      1  'CHAR' 'C310' 'L' 'X' 'X',
          ' '  'PTYPEF_NM'     'Revised Type'     20  'CHAR' '' 'L' '' 'X',
          ' '  'PTYPEF_ID'     'Revised User'     12  'CHAR' '' 'L' '' 'X',
*          ' '  'UDATE'        'Reg.Upd.Date'      8  'DATS' '' 'L' '' 'X',
          ' '  'CONSULTANT'   'Consultant'    12  'CHAR' 'C310' 'L' 'X' 'X',
          ' '  'FULLNAME'     'Name       '     20 'CHAR' '' 'L' '' 'X',
          ' '  'E_MAIL_ICON'   'Send E-mail'       4  'CHAR' '' 'L' '' 'X',
          ' '  'E_MAIL_STAT'   'E-mail Sent'       10  'CHAR' '' 'L' '' 'X',
*          ' '  'E_MAIL'        'E-mail'           10  'CHAR' '' 'L' '' 'X',
          ' '  'PACCEPT'      'Accepted'     	    1  'CHAR' 'C510' 'L' 'X' 'X',
          ' '  'ACCEPTER'     'Accepted by'    	 12  'CHAR' '' 'L' '' 'X',
          ' '  'ACCEPTDT'     'Accepted on'    	  8  'DATS' '' 'L' '' 'X',
          ' '  'UREASON'      'Unused Reason'     2  'CHAR' 'C510' 'L' 'X' 'X',
          ' '  'UREASON_NM'   'Unused Reason'    20  'CHAR' '' 'L' '' 'X',
          ' '  'URESULT'      'Analysis Result'   2  'CHAR' 'C510' 'R' 'X' 'X',
          ' '  'URESULT_NM'   'Analysis Result'  20  'CHAR' '' 'L' '' 'X',
          ' '  'ENH_ID'       'Analyzed by'      12  'CHAR' '' 'L' '' 'X' ,
          ' '  'ENH_DT'       'Analyzed on'       8  'DATS' '' 'L' '' 'X' ,
          ' '  'RESULT_COMMENT'   'Analysis Result Comment'         50
                                                     'CHAR' 'C510' 'L' 'X' 'X' ,
          ' '  'ENH_PLANDT'   'Enhance Plan Date' 8  'DATS' 'C510' 'L' 'X' 'X' ,

          ' '  'PLAN_APPROVED' 'Plan Appr.Flag'   1  'CHAR' 'C710' 'L' 'X' 'X',
          ' '  'PLAN_APPR'     'Plan Appr. by'   12  'CHAR' '' 'L' '' 'X',
          ' '  'PLAN_APPRDT'   'Plan Appr. on'    8  'DATS' '' 'L' '' 'X',

          ' '  'CLOSED'     'Close Req.Indicator'  1  'CHAR' 'C510' 'L' 'X' 'X',
          ' '  'CLOSEDBY'   'Close Req. by'    	 12  'CHAR' '' 'L' '' 'X',
          ' '  'CLOSEDT'    'Close Req. on'         8  'DATS' '' 'L' '' 'X',

          ' '  'ZCONFIRM'   'Close Confirm. Indi.'     1  'CHAR' 'C710' 'L' 'X' 'X',
          ' '  'ZCONFIRMEDBY' 'Confirmed by'     12  'CHAR' '' 'L' '' 'X',
          ' '  'ZCONFIRMDT'   'Confirmed on'       8  'DATS' '' 'L' '' 'X',
          ' '  'CLOSE_COMMENT'    'Close Confirm. Comment'         50
                                                     'CHAR' 'C710' 'L' 'X' 'X',
          ' '  'ISSUE_COMMENT'    'Issue Comment'                   50
                                                     'CHAR' '' 'L' 'X' 'X' .


  LOOP AT gt_fcat1 INTO gs_fcat1.
    IF gs_fcat1-fieldname = 'CHK'.
      gs_fcat1-checkbox = 'X'.
    ELSEIF gs_fcat1-fieldname = 'E_MAIL_STAT'.
      gs_fcat1-checkbox = 'X'.
    ELSEIF gs_fcat1-fieldname = 'E_MAIL_ICON'.
      gs_fcat1-icon = 'X'.
    ELSE.
      gs_fcat1-ref_field = gs_fcat1-fieldname.
      gs_fcat1-ref_table = 'ZTITBPMA'.
    ENDIF.

    IF wa_apm_user-zrole = 'A'.     "Administrator
      IF gs_fcat1-fieldname = 'PACCEPT'    OR gs_fcat1-fieldname = 'UREASON'
        OR gs_fcat1-fieldname = 'URESULT'  OR gs_fcat1-fieldname = 'ENH_PLANDT'
        OR gs_fcat1-fieldname = 'CLOSED'   OR gs_fcat1-fieldname = 'RESULT_COMMENT'.

        gs_fcat1-edit = ''.
      ENDIF.
    ELSEIF wa_apm_user-zrole = 'C'. "Consultant
      IF gs_fcat1-fieldname = 'PTYPEF'         OR gs_fcat1-fieldname = 'CONSULTANT'
        OR gs_fcat1-fieldname = 'E_MAIL_STAT'  OR gs_fcat1-fieldname = 'PLAN_APPROVED'
        OR gs_fcat1-fieldname = 'ZCONFIRM'     OR gs_fcat1-fieldname = 'CLOSE_COMMENT'.

        gs_fcat1-edit = ''.
      ENDIF.
    ELSEIF wa_apm_user-zrole = 'M'. "Manager
      IF gs_fcat1-fieldname = 'PTYPEF'         OR gs_fcat1-fieldname = 'CONSULTANT'
        OR gs_fcat1-fieldname = 'E_MAIL_STAT'  OR gs_fcat1-fieldname = 'PACCEPT'
        OR gs_fcat1-fieldname = 'UREASON'      OR gs_fcat1-fieldname = 'URESULT'
        OR gs_fcat1-fieldname = 'ENH_PLANDT'   OR gs_fcat1-fieldname = 'CLOSED'
        OR gs_fcat1-fieldname = 'RESULT_COMMENT'.

        gs_fcat1-edit = ''.
      ENDIF.
    ENDIF.

    MODIFY gt_fcat1 FROM gs_fcat1.
  ENDLOOP.

ENDFORM.                    " CREATE_FIELD_CATEGORY1
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT1
*&---------------------------------------------------------------------*
FORM set_lvc_layout1 .
  DATA : l_cnt1(3)..

  DESCRIBE TABLE gt_out1 LINES l_cnt1.

  CLEAR gs_layo1.
  gs_layo1-zebra      = 'X'.
  gs_layo1-sel_mode   = 'A'.       " Column and row selection
  gs_layo1-cwidth_opt = 'X'.
  gs_layo1-ctab_fname = 'TABCOLOR'.
  gs_layo1-stylefname = 'CELLTAB'.

  CONCATENATE 'Unused Plan' '-lines:' l_cnt1 INTO gs_layo1-grid_title.
*  gs_layo1-grid_title = 'Unused Plan'.

ENDFORM.                    " SET_LVC_LAYOUT1
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR1
*&---------------------------------------------------------------------*
FORM set_color1 .

ENDFORM.                    " SET_COLOR1
*&---------------------------------------------------------------------*
*&      Form  CREATE_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_html .
*..... create HTML header

  DATA: l_shellstyle TYPE i .
  DATA: v_notice TYPE sdydo_text_element.

  CREATE OBJECT g_html_cntl
    EXPORTING
      shellstyle         = l_shellstyle
      parent             = g_parent_html
    EXCEPTIONS
      cntl_error         = 1
      cntl_install_error = 2
      dp_install_error   = 3
      dp_error           = 4.

* build commentary

  DATA: i_logo             TYPE sdydo_value ,
        it_list_commentary TYPE slis_t_listheader ,
        i_background_id    TYPE sdydo_key   VALUE 'ALV_BACKGROUND' .

  PERFORM build_commentary.

  CREATE OBJECT g_document
    EXPORTING
      style = 'ALV_GRID'.


* export to memory
  EXPORT it_list_commentary  FROM gt_commentary
*         I_LOGO              FROM I_LOGO
         TO MEMORY ID 'DYNDOS_FOR_ALV'.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = g_document
      bottom   = space.


**--<HTML / Icon add
*  CALL METHOD g_document->new_line
*    EXPORTING
*      repeat = 1.


*  CALL METHOD g_document->add_text
*    EXPORTING
*      text         = 'Status'
*      sap_style    = cl_dd_document=>key
*      sap_color    = space
*      sap_fontsize = cl_dd_document=>medium
*      sap_emphasis = cl_dd_document=>strong
*      style_class  = space.

  CALL METHOD g_document->add_icon
    EXPORTING
      sap_icon = 'ICON_LED_RED'.

  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'To be Processed'
      sap_style    = space
      sap_color    = space
      sap_fontsize = cl_dd_document=>medium
      style_class  = space.

*  CALL METHOD g_document->new_line
*    EXPORTING
*      repeat = 0.

  CALL METHOD g_document->add_gap
    EXPORTING
      width = 10.


  CALL METHOD g_document->add_icon
    EXPORTING
      sap_icon = 'ICON_LED_YELLOW'.

  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'Processing'
      sap_style    = space
      sap_color    = space
      sap_fontsize = cl_dd_document=>medium
      style_class  = space.

*  CALL METHOD g_document->new_line
*    EXPORTING
*      repeat = 0.

  CALL METHOD g_document->add_gap
    EXPORTING
      width = 10.

  CALL METHOD g_document->add_icon
    EXPORTING
      sap_icon = 'ICON_LED_GREEN'.

  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'Processed'
      sap_style    = space
      sap_color    = space
      sap_fontsize = cl_dd_document=>medium
      style_class  = space.

**-new line : Color Description
*  CALL METHOD g_document->new_line
*    EXPORTING
*      repeat = 0.
  CALL METHOD g_document->add_gap
    EXPORTING
      width = 20.

  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'Field Colors:'
      sap_fontsize = cl_dd_document=>medium
*     sap_emphasis = cl_dd_document=>emphasis
      sap_emphasis = 'strong'
      style_class  = space.

  "Sap styles are heading,Key, success, etc
  "Sap font sizes are small,medium, large
  "Sap emphases are strong , emphasis
  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'for Administrator'
*     sap_style    = space
*     sap_style    = cl_dd_document=>heading
      sap_color    = cl_dd_document=>list_total
      sap_fontsize = cl_dd_document=>medium
*     sap_emphasis = cl_dd_document=>emphasis
*     sap_emphasis = 'strong'
      style_class  = space.

  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'for Consultant     '
      sap_color    = cl_dd_document=>list_positive
      sap_fontsize = cl_dd_document=>medium
      style_class  = space.

  CALL METHOD g_document->add_text
    EXPORTING
      text         = 'for Manager     '
      sap_color    = cl_dd_document=>list_group
      sap_fontsize = cl_dd_document=>medium
      style_class  = space.

* get ready
  CALL METHOD g_document->merge_document .



** set wallpaper
*  CALL METHOD G_DOCUMENT->SET_DOCUMENT_BACKGROUND
*    EXPORTING
*      PICTURE_ID = I_BACKGROUND_ID.

* connect TOP document to HTML-Control

  g_document->html_control = g_html_cntl .


* display TOP document
  CALL METHOD g_document->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = g_parent_html
    EXCEPTIONS
      html_display_error = 1.

  IF sy-subrc NE 0.
  ENDIF.

  CALL METHOD g_grid->set_html_header.

ENDFORM.                    " CREATE_HTML
*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENTARY
*&---------------------------------------------------------------------*
FORM build_commentary .
  DATA: l_info LIKE gs_commentary-info.
  DATA: v_date_low(10),                       "
        v_date_high(10).
  DATA: v_memo1(50),
        v_memo2(60).

  CLEAR: gt_commentary, gt_commentary[].


  CLEAR: gs_commentary.
  gs_commentary-typ   = 'S'.    " H = Header, S = Selection, A = Action

  gs_commentary-key   = 'Status'.
  gs_commentary-info  = ''.

  APPEND gs_commentary TO gt_commentary.
ENDFORM.                    " BUILD_COMMENTARY
*&---------------------------------------------------------------------*
*&      Form  GET_PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM get_processing_data .

  IF p_proc = 'X' OR  p_disp = 'X'.  "Currently processing data
    IF p_incl = 'X'.
      SELECT  * INTO TABLE  it_processing
      FROM ztitbpma AS a
      WHERE  a~tcode  IN s_tcode
        AND  a~l2     IN r_module.
    ELSE.
      SELECT  * INTO TABLE  it_processing
      FROM ztitbpma AS a
      WHERE a~zconfirm <> 'Y'
        AND a~tcode  IN s_tcode
        AND a~l2     IN r_module.
    ENDIF.
  ELSE.
    SELECT  * INTO TABLE  it_processing
    FROM ztitbpma AS a
    WHERE a~tcode  IN s_tcode
     AND  a~l2     IN r_module.
  ENDIF.

  SELECT  * INTO TABLE it_bpmlh
  FROM ztitbpmlh.

  SORT it_bpmlh BY l4f tcode timestamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_bpmlh COMPARING l4f tcode.

*-APM Status
  SELECT domvalue_l ddtext
      INTO CORRESPONDING FIELDS OF TABLE it_cstatus
  FROM dd07t
  WHERE domname = 'ZAPMSTATUS'.

*-Problem Type
  SELECT domvalue_l ddtext
      INTO CORRESPONDING FIELDS OF TABLE it_ptype
  FROM dd07t
  WHERE domname = 'ZAPMPTYPEA'.

*-Unused Reason
  SELECT domvalue_l ddtext
      INTO CORRESPONDING FIELDS OF TABLE it_ureason
  FROM dd07t
  WHERE domname = 'ZEUREASON'.

*-Performance Analysis Result(Plan)
  SELECT domvalue_l ddtext
      INTO CORRESPONDING FIELDS OF TABLE it_presult
  FROM dd07t
  WHERE domname = 'ZEPRESULT'.

*-Unused Analysis Result(Plan)
  SELECT domvalue_l ddtext
      INTO CORRESPONDING FIELDS OF TABLE it_uresult
  FROM dd07t
  WHERE domname = 'ZEURESULT'.
ENDFORM.                    " GET_PROCESSING_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PERFORMANCE_DATA
*&---------------------------------------------------------------------*
FORM get_performance_data .
  DATA : l_ms_sec TYPE ztitbpml-respmt.

  CLEAR : gt_out[], gt_out.

  l_ms_sec = p_mins * 60 * 1000.

  SELECT * INTO TABLE it_perform
  FROM ztitbpml_log
  WHERE zdate IN r_perfo_d
    AND respmt > l_ms_sec
    AND apm = 'X'
    AND l2    IN r_module.

  SORT it_perform BY tcode respmt DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_perform COMPARING tcode.

  LOOP AT it_perform.
    CLEAR : it_perform-zseq.
    MOVE-CORRESPONDING it_perform  TO    gt_out.
    gt_out-cstatus = '10'.
    gt_out-ptypea  = 'P'.  "Performance low
    gt_out-apmyear = sy-datum+0(4).
    gt_out-cdate   = sy-datum.
    gt_out-icon    = '@5C@'.
    APPEND gt_out.
  ENDLOOP.

*-Merge data with ZTITBPMA
*  LOOP AT it_processing WHERE ptypea = 'P'.
*    CLEAR : it_bpmlh , gt_out.
**    DELETE gt_out         WHERE l4f   = it_processing-l4f
**                            AND tcode = it_processing-tcode.
*    DELETE gt_out         WHERE tcode = it_processing-tcode.
*    MOVE-CORRESPONDING it_processing TO gt_out.
*
*    READ TABLE it_bpmlh WITH KEY l4f    = it_processing-l4f
*                                 tcode  = it_processing-tcode
*                                 BINARY SEARCH.
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING it_bpmlh TO  gt_out.
*      IF gt_out-cstatus <= '10'.
*        gt_out-icon    = '@5C@'.
*      ELSEIF gt_out-cstatus > '10' AND gt_out-cstatus < '39'.
*        gt_out-icon    = '@5D@'.
*      ELSE.
*        gt_out-icon    = '@5B@'.
*      ENDIF.
*      gt_out-l3t = it_bpmlh-l3.
*    ENDIF.
*    APPEND  gt_out.
*  ENDLOOP.

*-Fill the data
*  LOOP AT gt_out.
*
*
*  ENDLOOP.
ENDFORM.                    " GET_PERFORMANCE_DATA
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
FORM data_changed_finished  USING    p_e_modified
                                     et_good_cells TYPE lvc_t_modi.

  DATA: ls_good1 TYPE lvc_s_modi.
  DATA :  lt_celltab TYPE lvc_t_styl,
          ls_celltab TYPE lvc_s_styl,
          l_mode TYPE raw4.

*  READ TABLE et_good_cells INDEX 1 INTO ls_good1.
*  CHECK sy-subrc = 0.
**  IF sy-subrc = 0.
*  READ TABLE gt_out1 INDEX ls_good1-row_id.
*  IF sy-subrc EQ 0.
*    CASE ls_good1-fieldname.
*      WHEN 'PTYPEF'.
*        IF gt_out1-paccept IS NOT INITIAL.
*          MESSAGE e000 WITH 'Already processed,so can not change it'.
*        ENDIF.
**          READ TABLE it_cstatus WITH KEY domvalue_l  = ls_good1-value.
**          IF sy-subrc = 0.
**            gt_out1-cstatus_nm  =  it_cstatus-ddtext.
**          ENDIF.
**          MODIFY gt_out1 INDEX ls_good1-row_id TRANSPORTING cstatus_nm.
**          PERFORM refresh_grid USING 'G_GRID1'.
*
*    ENDCASE.
*  ENDIF.
**  ENDIF.
ENDFORM.                    " DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      Form  ON_F4
*&---------------------------------------------------------------------*
FORM on_f4  USING     p_c_sender    TYPE REF TO cl_gui_alv_grid
                     p_fieldname    TYPE lvc_fname
                     p_fieldvalue   TYPE lvc_value
                     p_s_row_no     TYPE lvc_s_roid
                     p_c_event_data TYPE REF TO cl_alv_event_data
                     p_t_bad_cells  TYPE lvc_t_modi
                     p_display      TYPE char01.


  CASE p_fieldname.
    WHEN 'CSTATUS'.



  ENDCASE.

ENDFORM.                    " ON_F4
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
FORM refresh_grid USING pv_grid.
  DATA: lv_grid(100).

  FIELD-SYMBOLS: <lfs_grid>      TYPE REF TO   cl_gui_alv_grid.

  lv_grid =  pv_grid  .
  ASSIGN:      (lv_grid)          TO   <lfs_grid>.

  __set_refresh_mode 'X'.
*
*  v_scroll-row = 'X'.
*  v_scroll-col = 'X'.
  CALL METHOD <lfs_grid>->refresh_table_display
    EXPORTING
      is_stable = stable.
ENDFORM.                    " REFRESH_GRID
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED1
*&---------------------------------------------------------------------*
FORM data_changed1  USING pr_data_changed TYPE REF TO
                                     cl_alv_changed_data_protocol
                          p_ucomm.

  DATA: ls_mod_cells        TYPE lvc_s_modi,
        lt_mod_cells        TYPE lvc_t_modi.

  CLEAR: ls_mod_cells, lt_mod_cells,   bapiaddr3, l_address,
         it_return, it_return[], lt_mod_cells[], lt_mod_cells.

  lt_mod_cells = pr_data_changed->mt_good_cells.
  SORT lt_mod_cells BY row_id.

  LOOP AT lt_mod_cells INTO ls_mod_cells.
    READ TABLE gt_out1 INDEX   ls_mod_cells-row_id.

    CASE ls_mod_cells-fieldname.
      WHEN 'CSTATUS'.
        READ TABLE it_cstatus WITH KEY domvalue_l  = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out1-cstatus_nm  =  it_cstatus-ddtext.
          IF ls_mod_cells-value < '20'.
            gt_out1-icon    = '@5C@'.
          ELSEIF ls_mod_cells-value >= '20'
                                      AND ls_mod_cells-value < '39'.
            gt_out1-icon    = '@5D@'.
          ELSE.
            gt_out1-icon    = '@5B@'.
          ENDIF.

          gt_out1-chk = 'X'.
          MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                                  TRANSPORTING  chk cstatus_nm icon.
          PERFORM refresh_grid USING 'G_GRID1'.
        ENDIF.

      WHEN 'PTYPEF'.
        IF gt_out1-paccept IS NOT INITIAL. "Recover to Original Value
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                  TRANSPORTING ptypef.
          PERFORM refresh_grid USING 'G_GRID1'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        READ TABLE it_ptype WITH KEY domvalue_l  = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out1-ptypef_nm  = it_ptype-ddtext.
          gt_out1-ptypef_id  = sy-uname.
          gt_out1-chk = 'X'.
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                TRANSPORTING chk ptypef_nm ptypef_id.
          PERFORM refresh_grid USING 'G_GRID1'.
        ENDIF.

      WHEN 'PACCEPT'.
        IF gt_out1-e_mail_stat IS  INITIAL
                        OR gt_out1-plan_approved = 'Y'.
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                  TRANSPORTING paccept.
          PERFORM refresh_grid USING 'G_GRID1'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out1-accepter  = sy-uname.
          gt_out1-acceptdt  = sy-datum.
        ELSE.
          gt_out1-accepter  = ''.
          gt_out1-acceptdt  = ''.
        ENDIF.
        PERFORM set_status USING gt_out1  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk accepter acceptdt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'CONSULTANT'.
        IF gt_out1-paccept IS NOT INITIAL. "Recover to Original Value
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                  TRANSPORTING consultant.
          PERFORM refresh_grid USING 'G_GRID1'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        IF ls_mod_cells-value IS NOT INITIAL.
          l_consultant_id = ls_mod_cells-value.
          CALL FUNCTION 'BAPI_USER_GET_DETAIL'
            EXPORTING
              username = l_consultant_id
            IMPORTING
              address  = l_address
            TABLES
              return   = it_return.

          CONCATENATE l_address-firstname l_address-lastname
                      INTO gt_out1-fullname SEPARATED BY space.
        ELSE.
          gt_out1-fullname = ''.
        ENDIF.

        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk fullname.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'UREASON'.
        READ TABLE it_ureason WITH KEY domvalue_l = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out1-ureason_nm  = it_ureason-ddtext.
          gt_out1-chk = 'X'.
          MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                                TRANSPORTING chk ureason_nm.
          PERFORM refresh_grid USING 'G_GRID1'.
        ENDIF.

      WHEN 'URESULT'.
        READ TABLE it_uresult WITH KEY domvalue_l = ls_mod_cells-value.
        IF sy-subrc = 0.
          gt_out1-uresult_nm  = it_uresult-ddtext.
          gt_out1-enh_id  = sy-uname.
          gt_out1-enh_dt  = sy-datum.
          gt_out1-chk = 'X'.
          MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                     TRANSPORTING chk enh_id enh_dt uresult_nm.
          PERFORM refresh_grid USING 'G_GRID1'.
        ENDIF.

      WHEN 'ENH_PLANDT'.
        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          wa_apm_manager-uname
                                          gt_out1.
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                   TRANSPORTING chk.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'RESULT_COMMENT'.
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                   TRANSPORTING chk.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'PLAN_APPROVED'.      "Plan Approval Indicator

        IF gt_out1-enh_plandt IS  INITIAL OR gt_out1-paccept <> 'Y'
              OR gt_out1-closed = 'X'.
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                  TRANSPORTING plan_approved.
          PERFORM refresh_grid USING 'G_GRID1'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          gt_out1-consultant
                                          gt_out1.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out1-plan_appr     = sy-uname.
          gt_out1-plan_apprdt   = sy-datum.
        ELSE.
          gt_out1-plan_appr     = ''.
          gt_out1-plan_apprdt   = ''.
        ENDIF.

        PERFORM set_status USING gt_out1  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk plan_appr plan_apprdt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'CLOSED'.
        IF gt_out1-zconfirm = 'Y' OR ( gt_out1-paccept = 'Y'
              AND gt_out1-plan_approved <> 'Y' ).
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                  TRANSPORTING zconfirm.
          PERFORM refresh_grid USING 'G_GRID1'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          wa_apm_manager-uname
                                          gt_out1.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out1-closedby  = sy-uname.
          gt_out1-closedt   = sy-datum.
        ELSE.
          gt_out1-closedby  = ''.
          gt_out1-closedt   = ''.
        ENDIF.
        PERFORM set_status USING gt_out1  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk closedby closedt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'ZCONFIRM'.
        IF gt_out1-closed <> 'X'.
          MODIFY gt_out1 INDEX ls_mod_cells-row_id
                                  TRANSPORTING closed.
          PERFORM refresh_grid USING 'G_GRID1'.
          MESSAGE e000 WITH text-t01.
        ENDIF.

        PERFORM generate_email_data USING ls_mod_cells-fieldname
                                          gt_out1-consultant
                                          gt_out1.

        IF ls_mod_cells-value IS NOT INITIAL.
          gt_out1-zconfirmedby  = sy-uname.
          gt_out1-zconfirmdt    = sy-datum.
        ELSE.
          gt_out1-zconfirmedby  = ''.
          gt_out1-zconfirmdt    = ''.
        ENDIF.
        PERFORM set_status USING gt_out1  ls_mod_cells-fieldname
                                 ls_mod_cells-value .
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk zconfirmedby zconfirmdt
                                     cstatus cstatus_nm icon.
        PERFORM refresh_grid USING 'G_GRID1'.

      WHEN 'CLOSE_COMMENT' OR 'ISSUE_COMMENT'.
        gt_out1-chk = 'X'.
        MODIFY gt_out1 INDEX  ls_mod_cells-row_id
                        TRANSPORTING chk .
        PERFORM refresh_grid USING 'G_GRID1'.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " DATA_CHANGED1
*&---------------------------------------------------------------------*
*&      Form  FILL_DATA
*&---------------------------------------------------------------------*
FORM fill_data .
  DATA ls_style TYPE lvc_s_styl .

  LOOP AT gt_out.
    IF gt_out-cstatus < '20'.
      gt_out-icon    = '@5C@'.
    ELSEIF gt_out-cstatus >= '20' AND gt_out-cstatus < '39'.
      gt_out-icon    = '@5D@'.
    ELSE.
      gt_out-icon    = '@5B@'.
    ENDIF.

    gt_out-e_mail_icon = '@1S@'.
    ls_style-fieldname = 'E_MAIL_ICON'.
    ls_style-style     = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO gt_out-celltab.

    IF gt_out-consultant IS NOT INITIAL.
      l_consultant_id = gt_out-consultant.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = l_consultant_id
        IMPORTING
          address  = l_address
        TABLES
          return   = it_return.

      CONCATENATE l_address-firstname l_address-lastname
                  INTO gt_out-fullname SEPARATED BY space.
    ENDIF.

    READ TABLE it_cstatus WITH KEY domvalue_l  = gt_out-cstatus.
    IF sy-subrc = 0.
      gt_out-cstatus_nm  =  it_cstatus-ddtext.
    ENDIF.

    READ TABLE it_ptype WITH KEY domvalue_l  = gt_out-ptypea.
    IF sy-subrc = 0.
      gt_out-ptypea_nm  =  it_ptype-ddtext.
    ENDIF.

    READ TABLE it_ptype WITH KEY domvalue_l  = gt_out-ptypef.
    IF sy-subrc = 0.
      gt_out-ptypef_nm  =  it_ptype-ddtext.
    ENDIF.

    READ TABLE it_presult WITH KEY domvalue_l  = gt_out-presult.
    IF sy-subrc = 0.
      gt_out-presult_nm  =  it_presult-ddtext.
    ENDIF.

    MODIFY gt_out.
  ENDLOOP.

  LOOP AT gt_out1.
    CLEAR : ls_style,  l_address, it_return[], it_return.

    IF gt_out1-cstatus < '20'.
      gt_out1-icon    = '@5C@'.
    ELSEIF gt_out1-cstatus >= '20' AND gt_out1-cstatus < '39'.
      gt_out1-icon    = '@5D@'.
    ELSE.
      gt_out1-icon    = '@5B@'.
    ENDIF.
**-<
    gt_out1-e_mail_icon = '@1S@'.
    ls_style-fieldname = 'E_MAIL_ICON'.
    ls_style-style     = cl_gui_alv_grid=>mc_style_button.
    APPEND ls_style TO gt_out1-celltab.

    IF gt_out1-consultant IS NOT INITIAL.
      l_consultant_id = gt_out1-consultant.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = l_consultant_id
        IMPORTING
          address  = l_address
        TABLES
          return   = it_return.

      CONCATENATE l_address-firstname l_address-lastname
                  INTO gt_out1-fullname SEPARATED BY space.
    ENDIF.

**-->

    READ TABLE it_cstatus WITH KEY domvalue_l  = gt_out1-cstatus.
    IF sy-subrc = 0.
      gt_out1-cstatus_nm  =  it_cstatus-ddtext.
    ENDIF.

    READ TABLE it_ptype WITH KEY domvalue_l  = gt_out1-ptypea.
    IF sy-subrc = 0.
      gt_out1-ptypea_nm  =  it_ptype-ddtext.
    ENDIF.

    READ TABLE it_ptype WITH KEY domvalue_l  = gt_out1-ptypef.
    IF sy-subrc = 0.
      gt_out1-ptypef_nm  =  it_ptype-ddtext.
    ENDIF.

    READ TABLE it_ureason WITH KEY domvalue_l  = gt_out1-ureason.
    IF sy-subrc = 0.
      gt_out1-ureason_nm  =  it_ureason-ddtext.
    ENDIF.

    READ TABLE it_uresult WITH KEY domvalue_l  = gt_out1-uresult.
    IF sy-subrc = 0.
      gt_out1-uresult_nm  =  it_uresult-ddtext.
    ENDIF.

    MODIFY gt_out1.
  ENDLOOP.

ENDFORM.                    " FILL_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM make_processing_data .
  DATA : l_delete_flag(1).

* Merge Data with New + Current processing
  LOOP AT it_processing.
    CLEAR : it_bpmlh , gt_out1, gt_out, l_delete_flag.

*-  Unused Issue Data
    IF  it_processing-ptypea = 'U'.

      IF p_new = 'X'. "delete duplicate

*-      Close Confirmation?
        IF it_processing-zconfirm = 'Y'.

*-      Issue?
          IF it_processing-paccept = 'N'. "No Issue
            DELETE gt_out1        WHERE  tcode = it_processing-tcode.
            CONTINUE.     "No display : Already finished.
          ELSE.   "Repoen
*            READ TABLE gt_out1 WITH KEY tcode = it_processing-tcode.
*            IF sy-subrc = 0.
*              gt_out1-cstatus = '15'.
*              MODIFY gt_out1 INDEX sy-tabix TRANSPORTING cstatus.
*            ENDIF.
            DELETE gt_out1        WHERE  tcode = it_processing-tcode.
            CONTINUE.
          ENDIF.
        ELSE.    "Current processing
          DELETE gt_out1        WHERE  tcode = it_processing-tcode.
        ENDIF.


**-      Issue?
*        IF it_processing-paccept = 'N'. "No Issue
*          DELETE gt_out1        WHERE  tcode = it_processing-tcode.
*          IF it_processing-zconfirm = 'Y'.
*            CONTINUE.     "No display : Already finished.
*          ENDIF.
*        ELSE.
**-        Close Confirmation?
*          IF it_processing-zconfirm = 'Y'.
*            CONTINUE.   "No display : Already finished.
*          ELSE.
*            DELETE gt_out1        WHERE  tcode = it_processing-tcode.
*          ENDIF.
*        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING it_processing TO gt_out1.

      READ TABLE it_bpmlh WITH KEY l4f    = it_processing-l4f
                                   tcode  = it_processing-tcode
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING it_bpmlh TO  gt_out1.
*        IF gt_out1-cstatus < '20'.
*          gt_out1-icon    = '@5C@'.
*        ELSEIF gt_out1-cstatus >= '20' AND gt_out1-cstatus < '30'.
*          gt_out1-icon    = '@5D@'.
*        ELSE.
*          gt_out1-icon    = '@5B@'.
*        ENDIF.
        gt_out1-l3t = it_bpmlh-l3.
      ENDIF.
      APPEND  gt_out1.

*-  Performance Issue Data
    ELSEIF it_processing-ptypea = 'P'.

      IF p_new = 'X'. "delete duplicate

*-      Close Confirmation?
        IF it_processing-zconfirm = 'Y'.

*-      Issue?
          IF it_processing-paccept = 'N'. "No Issue
            DELETE gt_out        WHERE  tcode = it_processing-tcode.
            CONTINUE.     "No display : Already finished.
          ELSE.   "Repoen
            READ TABLE gt_out WITH KEY tcode = it_processing-tcode.
            IF sy-subrc = 0.
              gt_out-cstatus = '15'.
              MODIFY gt_out INDEX sy-tabix TRANSPORTING cstatus.
            ENDIF.
            CONTINUE.
          ENDIF.
        ELSE.    "Current processing
          DELETE gt_out        WHERE  tcode = it_processing-tcode.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING it_processing TO gt_out.

      READ TABLE it_bpmlh WITH KEY l4f    = it_processing-l4f
                                   tcode  = it_processing-tcode
                                   BINARY SEARCH.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING it_bpmlh TO  gt_out.
*        IF gt_out-cstatus < '20'.
*          gt_out-icon    = '@5C@'.
*        ELSEIF gt_out-cstatus >= '20' AND gt_out-cstatus < '30'.
*          gt_out-icon    = '@5D@'.
*        ELSE.
*          gt_out-icon    = '@5B@'.
*        ENDIF.
        gt_out-l3t = it_bpmlh-l3.
      ENDIF.
      APPEND  gt_out.

    ENDIF.
  ENDLOOP.
ENDFORM.                    " MAKE_PROCESSING_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_screen .
  LOOP AT SCREEN.
    IF screen-name = 'P_NEW' AND wa_apm_user-zrole <> 'A'.
      screen-input    = 0.
*      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK1
*&---------------------------------------------------------------------*
FORM hotspot_click1  USING   e_row_id     TYPE  lvc_s_row
                             e_column_id  TYPE  lvc_s_col
                             es_row_no    TYPE  lvc_s_roid.

ENDFORM.                    " HOTSPOT_CLICK1
*&---------------------------------------------------------------------*
*&      Form  HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM handle_button_click  USING  e_column  TYPE lvc_s_col
                                 es_row_no TYPE lvc_s_roid.

  READ TABLE gt_out INDEX es_row_no-row_id.
  CHECK sy-subrc = 0 AND gt_out-e_mail_icon IS NOT INITIAL
                     AND gt_out-consultant  IS NOT INITIAL.

  CLEAR : l_address, it_return[], it_return.

  CASE e_column-fieldname.
    WHEN 'E_MAIL_ICON'.
      CHECK gt_out-paccept IS INITIAL.
      CHECK  wa_apm_user-zrole = 'A'. "Only administrator

      PERFORM generate_email_data USING e_column-fieldname
                                        gt_out-consultant
                                        gt_out.


      gt_out-chk = 'X'.
      MODIFY gt_out INDEX  es_row_no-row_id
                      TRANSPORTING chk e_mail_stat e_mail.
      PERFORM refresh_grid USING 'G_GRID'.

  ENDCASE.
ENDFORM.                    " HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
*&      Form  HANDLE_BUTTON_CLICK1
*&---------------------------------------------------------------------*
FORM handle_button_click1  USING  e_column  TYPE lvc_s_col
                                  es_row_no TYPE lvc_s_roid.

  READ TABLE gt_out1 INDEX es_row_no-row_id.
  CHECK sy-subrc = 0 AND gt_out1-e_mail_icon IS NOT INITIAL
                     AND gt_out1-consultant  IS NOT INITIAL.

  CLEAR : l_address, it_return[], it_return.

  CASE e_column-fieldname.
    WHEN 'E_MAIL_ICON'.
      CHECK gt_out1-paccept IS INITIAL.
      CHECK  wa_apm_user-zrole = 'A'. "Only administrator

*      l_consultant_id = gt_out1-consultant.
*      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*        EXPORTING
*          username = l_consultant_id
*        IMPORTING
*          address  = l_address
*        TABLES
*          return   = it_return.
*
*      gt_out1-e_mail =  l_address-e_mail.
*      gt_out1-e_mail_stat = 'X'.
*
*      IF gt_out1-e_mail IS INITIAL.
*        MESSAGE e000 WITH 'No E-mail Infomation.'
*                          'Please contact BC first'.
*      ENDIF.
      PERFORM generate_email_data USING e_column-fieldname
                                        gt_out1-consultant
                                        gt_out1.


      gt_out1-chk = 'X'.
      MODIFY gt_out1 INDEX  es_row_no-row_id
                      TRANSPORTING chk e_mail_stat e_mail.
      PERFORM refresh_grid USING 'G_GRID1'.



  ENDCASE.
ENDFORM.                    " HANDLE_BUTTON_CLICK1
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL_NOTICE
*&---------------------------------------------------------------------*
FORM send_email_notice  USING gs_out TYPE ty_out
                              p_status.

  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
           it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
           it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
           it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
           it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                   WITH HEADER LINE,
           gd_cnt TYPE i,
           gd_sent_all(1) TYPE c,
           gd_doc_data LIKE sodocchgi1.

  DATA : v_qty(9).
  DATA : lv_cnt(10).
  DATA : lv_desc(100).

  DATA : lv_seq_message1(100),  lv_seq_message2(100),
         lv_seq_message3(100).
  DATA : lv_date(10).
  DATA lv_count(8).

  CLEAR : it_mail[].


  PERFORM check_user_email USING gs_out.

  WRITE sy-datum TO lv_date.

  APPEND '=================================='  TO it_mail.
  CONCATENATE 'Status :' p_status INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
*  APPEND '     APM Enhancement Notice       '  TO it_mail.
  APPEND '=================================='  TO it_mail.
  CONCATENATE 'Date : ' lv_date INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.

*  CONCATENATE 'Date : ' lv_date INTO it_mail SEPARATED BY space.
*  APPEND it_mail.  CLEAR : it_mail.
*  CONCATENATE 'Status :' p_status INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.

  CONCATENATE 'Problem Type :'  gs_out-ptypea_nm
                                INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
  APPEND it_mail.  CLEAR : it_mail.
  CONCATENATE 'Process      :'  gs_out-l4f '' '' gs_out-l4t
                                INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
  CONCATENATE 'T-code       :'  gs_out-tcode '' '' gs_out-ttext
                                INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
*  CONCATENATE 'Program Name :'  gs_out-l4t
*                                INTO it_mail SEPARATED BY space.
*  APPEND it_mail.  CLEAR : it_mail.
  APPEND it_mail.  CLEAR : it_mail.

  CONCATENATE 'Please Update Status' 'with below Program'
                                INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.
  CONCATENATE sy-tcode ':' sy-title
                                INTO it_mail SEPARATED BY space.
  APPEND it_mail.  CLEAR : it_mail.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr =  ' APM Enhancement Notice'.
  gd_doc_data-sensitivty = 'F'.
  gd_doc_data-doc_size = 1.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

  CLEAR : it_receivers[].
*-  T-CODE :  mailing list  -> SO23
*  it_receivers-receiver = 'PP_ALERT'.
*  it_receivers-rec_type = 'C'.   "U - Internet address
  it_receivers-receiver = gs_out-e_mail.
  it_receivers-rec_type = 'U'.
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = ''.
  it_receivers-notif_ndel = ''.
  APPEND it_receivers. CLEAR it_receivers.

  CHECK it_receivers[] IS NOT INITIAL.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
      put_in_outbox              = ' '
      commit_work   = 'X'
*    IMPORTING
*      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.
  ELSE.
    MESSAGE e000 WITH 'Sending E-mail Failed.' 'Please contact IT'.
  ENDIF.

ENDFORM.                    " SEND_EMAIL_NOTICE
*&---------------------------------------------------------------------*
*&      Form  SET_STATUS
*&---------------------------------------------------------------------*
FORM set_status  USING    ps_out1 TYPE ty_out
                          p_fieldname
                          p_value.

  CASE p_fieldname.
    WHEN 'PACCEPT'.
      IF ps_out1-plan_approved = 'Y' OR ps_out1-closed = 'X'
         OR ps_out1-zconfirm = 'Y'.
        MESSAGE e000 WITH 'Can Not be changed : already processed'.
      ELSE.
        IF p_value = 'Y'.      "Accepted
          ps_out1-cstatus  = '20'.
        ELSEIF p_value = 'N'.  " No issue
*          ps_out1-cstatus  = '30'.
          ps_out1-cstatus  = '20'.
        ENDIF.
      ENDIF.

    WHEN 'PLAN_APPROVED'.
      IF ps_out1-closed = 'X' OR ps_out1-zconfirm = 'Y'.
*        MESSAGE e000 WITH 'Can Not be changed : already processed'.
      ELSE.
        IF p_value = 'Y'.      "Accepted
          ps_out1-cstatus  = '29'.
        ELSE.
*          ps_out1-cstatus  = '20'.  "Comment???
        ENDIF.
      ENDIF.
    WHEN 'CLOSED'.
      IF ps_out1-zconfirm = 'Y'.
*        MESSAGE e000 WITH 'Can Not be changed : already processed'.
      ELSE.
        IF p_value = 'X'.      "Close Request
          ps_out1-cstatus  = '30'.
        ELSE.
*          ps_out1-cstatus  = '20'.  "Comment???
        ENDIF.
      ENDIF.

    WHEN 'ZCONFIRM'.
*      IF ps_out1-zconfirm = 'Y'.
**        MESSAGE e000 WITH 'Can Not be changed : already processed'.
*      ELSE.

      IF p_value = 'Y'.      "Close Confirmed
        ps_out1-cstatus  = '39'.
      ELSE.
*          ps_out1-cstatus  = '20'.  "Comment???
      ENDIF.
*      ENDIF.

  ENDCASE.
  PERFORM set_traffic_icon USING ps_out1.  "location??
ENDFORM.                    " SET_STATUS
*&---------------------------------------------------------------------*
*&      Form  SET_TRAFFIC_ICON
*&---------------------------------------------------------------------*
FORM set_traffic_icon  USING    ps_out1 TYPE ty_out.
  IF ps_out1-cstatus < '20'.
    ps_out1-icon    = '@5C@'.
  ELSEIF ps_out1-cstatus >= '20'
                              AND ps_out1-cstatus < '39'.
    ps_out1-icon    = '@5D@'.
  ELSE.
    ps_out1-icon    = '@5B@'.
  ENDIF.

  READ TABLE it_cstatus WITH KEY domvalue_l  = ps_out1-cstatus.
  IF sy-subrc = 0.
    ps_out1-cstatus_nm  =  it_cstatus-ddtext.
  ENDIF.
ENDFORM.                    " SET_TRAFFIC_ICON
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED1
*&---------------------------------------------------------------------*
FORM data_changed_finished1   USING    p_e_modified
                                       et_good_cells TYPE lvc_t_modi.

  DATA: ls_good1 TYPE lvc_s_modi.
  DATA :  lt_celltab TYPE lvc_t_styl,
          ls_celltab TYPE lvc_s_styl,
          l_mode TYPE raw4.

ENDFORM.                    " DATA_CHANGED_FINISHED1
*&---------------------------------------------------------------------*
*&      Form  GENERATE_EMAIL_DATA
*&---------------------------------------------------------------------*
FORM generate_email_data  USING    p_fieldname
                                   p_user
                                   pt_data TYPE ty_out.

  l_consultant_id = p_user.
  CASE p_fieldname.
    WHEN 'E_MAIL_ICON'.
      PERFORM send_email_notice USING pt_data text-t11.
    WHEN 'ENH_PLANDT'.
      PERFORM send_email_notice USING pt_data text-t12.
    WHEN 'PLAN_APPROVED'.
      PERFORM send_email_notice USING pt_data text-t13.
    WHEN 'CLOSED'.
      PERFORM send_email_notice USING pt_data text-t14.
    WHEN 'ZCONFIRM'.
      PERFORM send_email_notice USING pt_data text-t15.
  ENDCASE.

ENDFORM.                    " GENERATE_EMAIL_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_USER_EMAIL
*&---------------------------------------------------------------------*
FORM check_user_email  USING gs_out TYPE ty_out..
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = l_consultant_id
    IMPORTING
      address  = l_address
    TABLES
      return   = it_return.

  IF l_address-e_mail IS INITIAL.
    MESSAGE e000 WITH 'No E-mail Infomation.'
                      'Please contact BC first'.
  ENDIF.

  gs_out-e_mail =  l_address-e_mail.
  gs_out-e_mail_stat = 'X'.

ENDFORM.                    " CHECK_USER_EMAIL
*&---------------------------------------------------------------------*
*&      Form  CHECK_MODIFICATION
*&---------------------------------------------------------------------*
FORM check_modification .
  DATA : l_text(50)
           VALUE 'Data has been changed.Do you still want to Exit?'.
  CLEAR : l_chk_modify.

  READ TABLE gt_out WITH KEY chk = 'X'.
  IF sy-subrc = 0.
    l_chk_modify  = 'X'.
  ELSE.
    READ TABLE gt_out1 WITH KEY chk = 'X'.
    IF sy-subrc = 0.
      l_chk_modify  = 'X'.
    ENDIF.
  ENDIF.

  IF l_chk_modify = 'X'.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Confirmation'
        text_question         = l_text
        text_button_1         = 'YES'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'NO'
        icon_button_2         = 'ICON_CANCEL'
        default_button        = '1'
        display_cancel_button = ''
      IMPORTING
        answer                = l_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF l_answer = '1'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ELSE.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDFORM.                    " CHECK_MODIFICATION
