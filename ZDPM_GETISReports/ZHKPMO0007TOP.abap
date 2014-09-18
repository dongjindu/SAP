*&---------------------------------------------------------------------*
*&  Include           ZHKPMO0007TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  TABLES
*&---------------------------------------------------------------------*
TABLES : mpla, mpos , t001w .
TABLES : ctu_params .
TABLES : sscrfields . .
*&---------------------------------------------------------------------*
*&  TYPE-POOL
*&---------------------------------------------------------------------*
TYPE-POOLS : slis.
TYPE-POOLS : kcde  .
TYPE-POOLS : truxs .
TYPE-POOLS : sydes .
TYPE-POOLS : icon.

*&---------------------------------------------------------------------*
*&  CONSTANTS
*&---------------------------------------------------------------------*
CONSTANTS : c_a TYPE char01 VALUE 'A' ,
            c_b TYPE char01 VALUE 'B' ,
            c_c TYPE char01 VALUE 'C' ,
            c_d TYPE char01 VALUE 'D' ,
            c_e TYPE char01 VALUE 'E' ,
            c_f TYPE char01 VALUE 'F' ,
            c_j TYPE char01 VALUE 'J' ,
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

*.# Data for BDC
DATA  : gt_bdctab   LIKE bdcdata    OCCURS 0 WITH HEADER LINE.
DATA  : gt_msgtab   LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA  : g_dismode   TYPE ctu_mode.

DATA : st_functxt  TYPE smp_dyntxt ,
       g_filename  TYPE string .

DATA : g_excel .
DATA : g_from_date LIKE sy-datum ,
       g_to_date   LIKE sy-datum .

*&---------------------------------------------------------------------*
*&  INTERNAL TABLES
*&---------------------------------------------------------------------*

DATA : BEGIN OF gt_data OCCURS 0 ,
        celltab  TYPE lvc_t_styl .
        INCLUDE STRUCTURE zhkpms0060 .
DATA   END OF gt_data .

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
*    PERFORM event_toolbar_item  USING e_object
*                                      e_interactive.
  ENDMETHOD.                    "toolbar
*---------------------------------------------------------------------*
*       METHOD user_command_Item                                      *
*---------------------------------------------------------------------*
  METHOD user_command_item.
*    PERFORM event_ucomm_item   USING e_ucomm.
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

SELECT-OPTIONS : s_WARPL FOR mpla-WARPL ,
                 s_WPTXT FOR mpla-WPTXT ,
                 s_STRAT FOR mpla-STRAT ,
                 s_MPTYP for mpla-MPTYP ,
                 s_SORT  FOR mpla-PLAN_SORT .
SELECTION-SCREEN skip 1 .
PARAMETERS : p_check as checkbox  DEFAULT 'X' .
SELECTION-SCREEN END OF BLOCK b1.
