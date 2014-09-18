*&---------------------------------------------------------------------*
*&  Include           ZEMM_CHANGE_PR_CLOSED_TOP
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
*   TABLES                                                           *
*--------------------------------------------------------------------*
TABLES: eban.

*--------------------------------------------------------------------*
*   DATA                                                             *
*--------------------------------------------------------------------*
DATA : BEGIN OF it_pr  OCCURS 0,
  banfn  TYPE eban-banfn,       "PR No.
  bnfpo  TYPE eban-bnfpo,       "PR item No.
  bsart  TYPE eban-bsart,       "PR Document Type
  erdat  TYPE eban-erdat,       "PR Created Date
  matnr  TYPE eban-matnr,       "Material Number
  knttp  TYPE eban-knttp,       "Account Assignment Category
  ebakz  TYPE eban-ebakz,       "Purchase Requisition Closed
  ebeln  TYPE eban-ebeln,       "PO No.
  ebelp  TYPE eban-ebelp,       "PO item No.
  maktx  TYPE makt-maktx.       "Material Description
DATA : END OF it_pr.

DATA : BEGIN OF it_pr_num  OCCURS 0,
  banfn  TYPE eban-banfn.       "PR No.
DATA : END OF it_pr_num.

DATA : BEGIN OF it_excl   OCCURS 0,
  banfn  TYPE eban-banfn,       "PR No.
  bnfpo  TYPE eban-bnfpo.       "PR item No.
DATA : END OF it_excl.

* SAPGUI_PROGRESS_INDICATOR
CONSTANTS :
*  c_text(35)      TYPE c   VALUE 'Processing..................',
  c_percentage(3) TYPE n   VALUE '18'.

* "for screen 100
DATA :
  g_ok_code      TYPE ok_code,
  g_save_ok      LIKE g_ok_code,

  g_tabix        LIKE sy-tabix,
  g_check(1)     TYPE c.

* "for CLASS
*--------------------------------------------------------------------*
*   CLASS Definitiion                                               *
*--------------------------------------------------------------------*
CLASS lcl_alv_grid DEFINITION  INHERITING FROM cl_gui_alv_grid.

  PUBLIC SECTION.
    CLASS-DATA f_alv  TYPE c.

    METHODS : set_fixed_column,
    set_optimizer.

ENDCLASS.                    "lcl_alv_grid DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_grid IMPLEMENTATION.

  METHOD set_fixed_column.
    CALL METHOD me->set_fixed_cols
      EXPORTING
        cols = 5.
  ENDMETHOD.                    "SET_FIXED_COLUMN

  METHOD set_optimizer.
    CALL METHOD me->optimize_all_cols
      EXPORTING
        include_header = 1.
  ENDMETHOD.                    "SET_OPTIMIZER

ENDCLASS.                    "LCL_ALV_GRID IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS : handle_toolbar
    FOR EVENT toolbar OF cl_gui_alv_grid
    IMPORTING e_object e_interactive.

    METHODS : handle_user_command
    FOR EVENT user_command OF cl_gui_alv_grid
    IMPORTING e_ucomm.

    METHODS : handle_after_user_command
    FOR EVENT after_user_command OF cl_gui_alv_grid
    IMPORTING  e_ucomm .

    METHODS : handle_data_changed
    FOR EVENT data_changed OF cl_gui_alv_grid
    IMPORTING er_data_changed e_onf4 e_ucomm.

    METHODS : handle_data_changed_finished
    FOR EVENT data_changed_finished OF cl_gui_alv_grid
    IMPORTING e_modified
      et_good_cells.

    METHODS : handle_hotspot_click
    FOR EVENT hotspot_click OF cl_gui_alv_grid
    IMPORTING e_row_id
      e_column_id.

    METHODS : handle_double_click
    FOR EVENT double_click OF cl_gui_alv_grid
    IMPORTING e_row
      e_column.

    METHODS : handle_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
    IMPORTING sender
      e_fieldname
      e_fieldvalue
      es_row_no
      er_event_data
      et_bad_cells
      e_display.


ENDCLASS.  "(LCL_EVENT_RECEIVER DEFINITION)

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

*-- ToolBar ????
  METHOD handle_toolbar.
    PERFORM toolbar_pros  USING e_object e_interactive.
  ENDMETHOD.                    "handle_toolbar

*-- when click ToolBar button
  METHOD handle_user_command.
    PERFORM user_command_pros USING e_ucomm.
  ENDMETHOD.                    "handle_user_command

*-- Event ???
  METHOD handle_after_user_command.
    PERFORM after_user_command USING e_ucomm.
  ENDMETHOD.                    "HANDLE_AFTER_USER_COMMAND

*-- Data change??
  METHOD handle_data_changed.
    PERFORM data_changed  USING er_data_changed e_onf4 e_ucomm.
  ENDMETHOD.                    "handle_data_changed

*-- Data change? ?
  METHOD handle_data_changed_finished.
    PERFORM data_changed_finished USING e_modified
          et_good_cells.
  ENDMETHOD.                    "HANDLE_DATA_CHANGED_FINISHED

*-- Hotspot click
  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id e_column_id.
  ENDMETHOD.                    "handle_hotspot_click

*-- Double Click
  METHOD handle_double_click.
    PERFORM double_click  USING e_row e_column.
  ENDMETHOD.    "handle_double_click

*-- On help f4 - Search Help
  METHOD handle_onf4.
    PERFORM on_f4 USING sender
          e_fieldname
          e_fieldvalue
          es_row_no
          er_event_data
          et_bad_cells
          e_display.
  ENDMETHOD.                    "HANDLE_ONF4



ENDCLASS. "LCL_EVENT_RECEIVER IMPLEMENTATION


* "for ALV
DATA :
  g_docking_container  TYPE REF TO cl_gui_docking_container,
  g_grid               TYPE REF TO lcl_alv_grid,
  g_event_handler      TYPE REF TO lcl_event_handler,
  gt_exclude           TYPE        ui_functions, "Tool Bar Button

  gt_fieldcat          TYPE        lvc_t_fcat,
  gs_fieldcat          TYPE        lvc_s_fcat,
  gs_layocat           TYPE        lvc_s_layo,  "Grid(Display)
  gs_f4                TYPE        lvc_s_f4,    "F4 help
  gt_f4                TYPE        lvc_t_f4,

  alv_variant          LIKE        disvariant,
  l_scroll             TYPE        lvc_s_stbl.

DATA : g_fname(40)     TYPE c.
FIELD-SYMBOLS <fs>     TYPE any.

CONSTANTS :
  c_st_nm        LIKE  dd02l-tabname VALUE 'IT_PR'.

DATA : BEGIN OF gt_display OCCURS 0,
         check(1),
         icon(5).
INCLUDE             STRUCTURE it_pr.
DATA :   f_col      TYPE lvc_t_scol,
         celltab    TYPE lvc_t_styl.
DATA :   result_msg TYPE bapi_msg.
DATA : END   OF gt_display.

DATA :
  gs_display     LIKE gt_display,
  g_continue(1)  TYPE c.

*--------------------------------------------------------------------*
*   SELECTION-SCREEN                                                 *
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME.
*--------------------------------------------------*
* Option (System data / Excel uploading)
*--------------------------------------------------*
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-004.

PARAMETER : ra_1  RADIOBUTTON GROUP grp1
                  USER-COMMAND radi
                  DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(20) text-002 FOR FIELD ra_1.

PARAMETER : ra_2  RADIOBUTTON GROUP grp1.
SELECTION-SCREEN COMMENT 45(40) text-003 FOR FIELD ra_2.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2  WITH FRAME  TITLE text-001.
*--------------------------------------------------*
* System data
*--------------------------------------------------*
SELECT-OPTIONS :
  s_banfn  FOR eban-banfn MODIF ID g1,  "PR No.
  s_bsart  FOR eban-bsart MODIF ID g1,  "PR Document Type
  s_knttp  FOR eban-knttp MODIF ID g1.  "Account Assignment Category

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS :
  s_erdat  FOR eban-erdat MODIF ID g1.  "PR Created Date
PARAMETERS :
  p_days(3) TYPE n        MODIF ID g1.  "Created date(Days before)


SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(15) text-005  MODIF ID g1.

PARAMETERS : chkbx_1   AS CHECKBOX       MODIF ID g1
                       DEFAULT 'X'.
SELECTION-SCREEN COMMENT 20(10) text-006 FOR FIELD chkbx_1
                                         MODIF ID g1.

PARAMETERS : chkbx_2   AS CHECKBOX       MODIF ID g1
                       DEFAULT 'X'.
SELECTION-SCREEN COMMENT 45(10) text-007 FOR FIELD chkbx_2
                                         MODIF ID g1.
SELECTION-SCREEN END OF LINE.

*--------------------------------------------------*
* "for upload Excel file
*--------------------------------------------------*
PARAMETERS :
  p_fname   TYPE rlgrap-filename   MODIF ID g2.

SELECTION-SCREEN END OF BLOCK b2.
