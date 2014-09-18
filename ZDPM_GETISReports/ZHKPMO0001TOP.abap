*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0001TOP
*&---------------------------------------------------------------------*
*& # DATA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&  TABLES
*&---------------------------------------------------------------------*
TABLES : ekpo, eban, mara, mard, mbew, marc , t001w , zhkpmt0008 ,
         zvpm_mksg , zhkpmv0008 ,  zhkpms0030 .

*&---------------------------------------------------------------------*
*&  TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS : slis .
TYPE-POOLS : kcde  .
TYPE-POOLS : truxs .
TYPE-POOLS : sydes .

*&---------------------------------------------------------------------*
*&  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : c_a TYPE char01 VALUE 'A' ,
            c_b TYPE char01 VALUE 'B' ,
            c_c TYPE char01 VALUE 'C' ,
            c_d TYPE char01 VALUE 'D' ,
            c_e TYPE char01 VALUE 'E' ,
            c_f TYPE char01 VALUE 'F' ,
            c_h TYPE char01 VALUE 'H' ,
            c_i TYPE char01 VALUE 'I' ,
            c_n TYPE char01 VALUE 'N' ,
            c_p TYPE char01 VALUE 'P' ,
            c_s TYPE char01 VALUE 'S' ,
            c_x TYPE char01 VALUE 'X' .

CONSTANTS : c_01 TYPE char02 VALUE '01' ,
            c_02 TYPE char02 VALUE '02' ,
            c_03 TYPE char02 VALUE '03' ,
            c_04 TYPE char02 VALUE '04' ,
            c_05 TYPE char02 VALUE '05' ,
            c_06 TYPE char02 VALUE '06' ,
            c_07 TYPE char02 VALUE '07' ,
            c_08 TYPE char02 VALUE '08' ,
            c_09 TYPE char02 VALUE '09' ,
            c_10 TYPE char02 VALUE '10' ,
            c_99 TYPE char02 VALUE '99' .

*&---------------------------------------------------------------------*
*&  GLOBAL VARIABLES
*&---------------------------------------------------------------------*

* Job_start, Job_end.
DATA: g_job_start_date LIKE sy-datum,
      g_job_start_time TYPE syuzeit,
      g_job_end_date   LIKE sy-datum,
      g_job_end_time   TYPE syuzeit.

* Result count
DATA : g_total   LIKE sy-tabix ,
       g_success LIKE sy-tabix ,
       g_error   LIKE sy-tabix .

DATA : g_time_stamp  TYPE timestampl.

*. Destination
DATA: g_dest   TYPE rfcdest VALUE 'EAIPM'.

RANGES: r_budat FOR zvpm_mksg-budat .

*.
DATA : G_ENTER ,
       G_ERR .
*&---------------------------------------------------------------------*
*&  INTERNAL TABLES
*&---------------------------------------------------------------------*

DATA : BEGIN OF gt_data OCCURS 0 ,
        mark ,
        icon(4) .
        INCLUDE STRUCTURE zhkpmt0008 .
DATA   END OF gt_data .

DATA : BEGIN OF gt_item OCCURS 0 ,
        new ,
        celltab  TYPE lvc_t_styl .
        INCLUDE STRUCTURE zhkpms0030 .
DATA   END OF gt_item .

DATA : BEGIN OF gt_yymm OCCURS 0 ,
         yymm(6) TYPE n ,
       END OF gt_yymm .

DATA: v_return LIKE bapireturn OCCURS 0 WITH HEADER LINE,
      v_banfn  LIKE eban-banfn.

DATA: it_exten        LIKE TABLE OF bapiparex  WITH HEADER LINE.

DATA : g_afnam  LIKE  eban-ernam,
 gs_prheader    LIKE  bapimereqheader         ,
 gs_prheaderx   LIKE  bapimereqheaderx        ,
 g_banfn        LIKE  bapimereqheader-preq_no ,
 gt_return      LIKE  bapiret2           OCCURS 0 WITH HEADER LINE,
 gt_pritem      LIKE  bapimereqitemimp   OCCURS 0 WITH HEADER LINE,
 gt_pritemexp   LIKE  bapimereqitem      OCCURS 0 WITH HEADER LINE,
 gt_pritemx     LIKE  bapimereqitemx     OCCURS 0 WITH HEADER LINE,
 gt_pracct      LIKE  bapimereqaccount   OCCURS 0 WITH HEADER LINE,
 gt_pracctx     LIKE  bapimereqaccountx  OCCURS 0 WITH HEADER LINE,
 gt_praseg      LIKE  bapimereqaccountprofitseg  OCCURS 0 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
DATA : gt_fieldcat    TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       gt_fieldcat1   TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       gs_fieldcat    TYPE slis_fieldcat_alv,
       gt_event       TYPE slis_t_event,
       gs_event       TYPE slis_alv_event,
       gt_event_exit  TYPE slis_t_event_exit,
       gs_event_exit  TYPE slis_event_exit,
       gt_listheader  TYPE slis_t_listheader, "TOP-OF-PAGE
       gs_listheader  TYPE slis_listheader,
       gt_sortcat     TYPE slis_t_sortinfo_alv,
       gt_sortcat1    TYPE slis_t_sortinfo_alv,
       gs_sortcat     TYPE slis_sortinfo_alv,
       gs_layout      TYPE slis_layout_alv,
       g_variant      LIKE disvariant,
       g_repid        LIKE sy-repid,
       g_slis_print   TYPE slis_print_alv,
       g_save.

*DATA: gt_colinfo_table   TYPE slis_t_specialcol_alv WITH HEADER LINE,
*      gs_celltab         TYPE lvc_s_styl,
*      gt_celltab         TYPE lvc_t_styl.
*
DATA: g_ucomm      TYPE sy-ucomm,
      g_ok         TYPE c,
      ok_code      TYPE sy-ucomm.

DATA: g_write_gubun(2).

DATA: g_name(50)   TYPE c.

FIELD-SYMBOLS: <fs_tab>  TYPE STANDARD TABLE,
               <fs_tabe> TYPE STANDARD TABLE,
               <fs_tab1> TYPE STANDARD TABLE.

CONSTANTS : c_status_set       TYPE slis_formname VALUE 'PF_STATUS_SET',
            c_user_command     TYPE slis_formname VALUE 'USER_COMMAND',
            c_top_of_page      TYPE slis_formname VALUE 'TOP_OF_PAGE',
            c_top_of_list      TYPE slis_formname VALUE 'TOP_OF_LIST',
            c_end_of_list      TYPE slis_formname VALUE 'END_OF_LIST',
            c_data_changed     TYPE slis_formname VALUE 'DATA_CHANGED'.

*&---------------------------------------------------------------------*
*&  Define of ALV Variavles
*&---------------------------------------------------------------------*

CLASS lcl_event_receiver DEFINITION DEFERRED.
CLASS lcl_alv_grid      DEFINITION DEFERRED.

DATA: g_alv_doc_head   TYPE REF TO "cl_gui_container,
                                    cl_gui_docking_container,

      g_grid_head      TYPE REF TO lcl_alv_grid, "cl_gui_alv_grid,
      g_event_head     TYPE REF TO lcl_event_receiver.

DATA: g_alv_doc_item   TYPE REF TO   cl_gui_custom_container,"cl_gui_container,
                                   "CL_GUI_DOCKING_CONTAINER,

      g_grid_item      TYPE REF TO lcl_alv_grid, "cl_gui_alv_grid,
      g_event_item     TYPE REF TO lcl_event_receiver.


DATA: g_alv_doc_mdat   TYPE REF TO "cl_gui_container,
                                    cl_gui_docking_container,

      g_grid_mdat      TYPE REF TO lcl_alv_grid, "cl_gui_alv_grid,
      g_event_mdat     TYPE REF TO lcl_event_receiver.

DATA: g_alv_doc_wdat   TYPE REF TO "cl_gui_container,
                                    cl_gui_docking_container,

      g_grid_wdat      TYPE REF TO lcl_alv_grid, "cl_gui_alv_grid,
      g_event_wdat     TYPE REF TO lcl_event_receiver.

DATA:  g_title             TYPE lvc_title.

DATA: g_html_container   TYPE REF TO cl_gui_container,
      g_document         TYPE REF TO cl_dd_document,
      g_html_cntl        TYPE REF TO cl_gui_html_viewer.

DATA: g_rows_t           TYPE lvc_t_row,
      g_rows_s           LIKE lvc_s_row,
      g_index            LIKE sy-tabix,
      g_tcode            TYPE sy-tcode.

DATA: st_lay           TYPE lvc_s_layo.

DATA: gt_field         TYPE lvc_t_fcat,
      st_field         TYPE lvc_s_fcat,
      gt_sortd         TYPE lvc_t_sort,
      st_sortd         TYPE lvc_s_sort,
      gt_rows          TYPE lvc_t_row,
      st_rows          LIKE lvc_s_row,
      gt_exclude       TYPE ui_functions,
      st_variant       LIKE disvariant,     "variant
      gt_variant       LIKE disvariant,     "variant
      st_variant_0200  LIKE disvariant,     "variant
      gt_variant_0200  LIKE disvariant.     "variant

DATA: g_container TYPE scrfname VALUE 'WC_CONTAINER_0100'.

FIELD-SYMBOLS: <fs_container> TYPE REF TO cl_gui_custom_container,
               <fs_control>   TYPE        scrfname,
               <fs_alv>       TYPE REF TO cl_gui_alv_grid,
               <fs_itab>      TYPE STANDARD TABLE.


DEFINE set_fieldcat_m .
  st_field-reptext    = &1.
  st_field-scrtext_l  = &1.
  st_field-scrtext_m  = &1.
  st_field-scrtext_s  = &1.
  st_field-colddictxt = 'L'.
  st_field-outputlen  = &2.
END-OF-DEFINITION.

DATA: gt_colinfo_table   TYPE slis_t_specialcol_alv WITH HEADER LINE,
      st_celltab     TYPE lvc_s_styl,
      gt_celltab     TYPE lvc_t_styl.

DATA: it_edit        TYPE lvc_t_fcat  WITH HEADER LINE.
*---------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_alv_grid DEFINITION INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.
    METHODS: set_optimize_all_cols,
    set_cursor IMPORTING row  TYPE i
      col  TYPE i,
      set_fixed_col,
      set_resize_row.

ENDCLASS.                    "LCL_ALV_GRID DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD set_optimize_all_cols.

    CALL METHOD me->optimize_all_cols
      EXPORTING
        include_header = 1.

  ENDMETHOD.                    "SET_OPTIMIZE_ALL_COLS

  METHOD set_cursor.

    CALL METHOD me->set_current_cell_base
      EXPORTING
        row = row
        col = col.

  ENDMETHOD.                    "SET_CURSOR

  METHOD set_fixed_col.

    CALL METHOD me->set_fixed_cols
      EXPORTING
        cols = 11.

  ENDMETHOD.                    "SET_FIXED_COL

  METHOD set_resize_row.

    CALL METHOD me->set_resize_rows
      EXPORTING
        enable = 1.

  ENDMETHOD.                    "SET_RESIZE_ROW

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION

*---------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
* GRID Header
     double_click          FOR EVENT double_click
                           OF cl_gui_alv_grid
                           IMPORTING e_row
                                     e_column
                                     es_row_no,
* GRID Item
     hotspot_click_item    FOR EVENT hotspot_click
                           OF cl_gui_alv_grid
                           IMPORTING e_row_id
                                     e_column_id
                                     es_row_no,

      hotspot_click_mdat   FOR EVENT hotspot_click
                           OF cl_gui_alv_grid
                           IMPORTING e_row_id
                                     e_column_id
                                     es_row_no,

      hotspot_click_wdat   FOR EVENT hotspot_click
                           OF cl_gui_alv_grid
                           IMPORTING e_row_id
                                     e_column_id
                                     es_row_no,

     toolbar_item          FOR EVENT toolbar
                           OF cl_gui_alv_grid
                           IMPORTING e_object
                                     e_interactive,

     user_command_item    FOR EVENT user_command
                           OF cl_gui_alv_grid
                           IMPORTING e_ucomm sender,


     data_changed          FOR EVENT data_changed
                           OF cl_gui_alv_grid
                           IMPORTING er_data_changed
                                     e_onf4
                                     e_onf4_before
                                     e_onf4_after
                                     e_ucomm.



  PRIVATE SECTION.
    DATA: g_initial_table TYPE c.
ENDCLASS.                    "lcl_event_receiver DEFINITION

**---------------------------------------------------------
CLASS lcl_event_receiver IMPLEMENTATION.
*
*---------------------[ GRID bottom ]---------------------------------*
*---------------------------------------------------------------------*
*       METHOD double_click                                           *
*---------------------------------------------------------------------*
  METHOD double_click.
*    PERFORM EVENT_DOUBLE_CLICK USING E_ROW
*                                     E_COLUMN.
  ENDMETHOD.                    "toolbar
*---------------------------------------------------------------------*
*       METHOD hotspot_click_Item                                     *
*---------------------------------------------------------------------*
  METHOD hotspot_click_item.
*    PERFORM EVENT_HOTSPOT_CLICK_ITEM  USING E_ROW_ID
*                                            E_COLUMN_ID.

  ENDMETHOD.                    "hotspot_click
*---------------------------------------------------------------------*
*       METHOD hotspot_click_MDAT                                     *
*---------------------------------------------------------------------*
  METHOD hotspot_click_mdat .
*    PERFORM EVENT_HOTSPOT_CLICK_MDAT  USING E_ROW_ID
*                                            E_COLUMN_ID.

  ENDMETHOD.                    "hotspot_click

*---------------------------------------------------------------------*
*       METHOD hotspot_click_MDAT                                     *
*---------------------------------------------------------------------*
  METHOD hotspot_click_wdat .
*    PERFORM EVENT_HOTSPOT_CLICK_WDAT  USING E_ROW_ID
*                                            E_COLUMN_ID.

  ENDMETHOD.                    "hotspot_click

*---------------------------------------------------------------------*
*       METHOD toolbar_Item                                           *
*---------------------------------------------------------------------*
  METHOD toolbar_item.
    PERFORM event_toolbar_item  USING e_object
                                      e_interactive.
  ENDMETHOD.                    "toolbar
*---------------------------------------------------------------------*
*       METHOD user_command_Item                                      *
*---------------------------------------------------------------------*
  METHOD user_command_item.
    PERFORM event_ucomm_item   USING e_ucomm.
  ENDMETHOD.                    "user_command

*---------------------------------------------------------------------*
*       METHOD data_changed                                           *
*---------------------------------------------------------------------*
  METHOD data_changed.

*    PERFORM EVENT_DATA_CHANGED       USING ER_DATA_CHANGED
*                                           E_ONF4
*                                           E_ONF4_BEFORE
*                                           E_ONF4_AFTER
*                                           E_UCOMM.

  ENDMETHOD.                    "data_changed

*---------------------------------------------------------------------*

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*& # Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR mara-matnr ,
                 s_werks FOR t001w-werks ,
                 s_lgort FOR mard-lgort .
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_zdate  FOR zhkpmt0008-zdate .
PARAMETERS : p_rda RADIOBUTTON GROUP gr  DEFAULT 'X', " USER-COMMAND rda,
             p_rdb RADIOBUTTON GROUP gr .
SELECTION-SCREEN END OF BLOCK b2.
