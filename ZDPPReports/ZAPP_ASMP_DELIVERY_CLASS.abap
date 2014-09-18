*&---------------------------------------------------------------------*
*&  Include           ZAPP_ASMP_DELIVERY_CLASS
*&---------------------------------------------------------------------*

* Define Variables and Internal Tables for ALV
TYPE-POOLS: slis.

INCLUDE <icon>.
INCLUDE <symbol>.

*TYPES : ddshretval_table TYPE TABLE OF ddshretval.
CLASS lcl_event_receiver DEFINITION DEFERRED.

*field-symbols : <pro> type ref to cl_alv_changed_data_protocol.

*-. Data..........................................................
DATA: gk_con TYPE REF TO cl_gui_custom_container,
      gk_con_sp        TYPE REF TO cl_gui_splitter_container,
      gk_docking       TYPE REF TO cl_gui_docking_container,
      gk_con_grid      TYPE REF TO cl_gui_container,
      gk_con_grid1     TYPE REF TO cl_gui_container,
      gk_grid          TYPE REF TO cl_gui_alv_grid,
      gk_grid1         TYPE REF TO cl_gui_alv_grid,
      gk_grid_pro      TYPE REF TO cl_alv_changed_data_protocol,
      gt_fieldcat      TYPE lvc_t_fcat,
      gs_fieldcat      TYPE lvc_s_fcat,
      gt_fieldcat_det  TYPE lvc_t_fcat,
      gt_sort          TYPE LVC_T_SORT WITH HEADER LINE,
      gs_sort          TYPE lvc_s_sort,
      gt_exclude       TYPE ui_functions,
      gs_exclude       TYPE ui_func,

      gs_layout      TYPE lvc_s_layo,
      gs_layout_det  TYPE lvc_s_layo,
      gt_f4          TYPE lvc_t_f4,
      gt_excluding   TYPE ui_functions,
      g_event_receiver TYPE REF TO lcl_event_receiver.

DATA : func_fieldcat  TYPE slis_t_fieldcat_alv. "Convert

DATA : gs_stbl TYPE lvc_s_stbl .   "fix line when refresh_table_display
DATA : l_lvc_title TYPE lvc_title.  "grid title to change it
DATA : g_repid LIKE sy-repid.
DATA : g_dynnr LIKE sy-dynnr.
DATA : container_name   TYPE scrfname VALUE 'CC_0100'.
DATA : gs_variant       TYPE disvariant.
DATA : g_parent_html    TYPE REF TO cl_gui_container,
       g_html_cntl      TYPE REF TO cl_gui_html_viewer,
       g_document       TYPE REF TO cl_dd_document.
DATA : gt_commentary    TYPE slis_t_listheader,
       gs_commentary    TYPE slis_listheader.

*-. Constants......................................................
CONSTANTS: gc_true   TYPE c VALUE 'X',
           gc_false  TYPE c VALUE space.
*CONSTANTS: c_stru_nm  LIKE  dd02l-tabname VALUE ''.

*-----/// LIST BOX DATA
TYPE-POOLS vrm.
DATA: list  TYPE vrm_values,
      value LIKE LINE OF list.

*-Screen field update
DATA : it_value    LIKE ddshretval OCCURS 0 WITH HEADER LINE.
DATA : dynpfields  LIKE  dynpread OCCURS 0 WITH  HEADER LINE.

*-BDC Objects
data: begin of it_bdc occurs 0.
        include structure bdcdata.
data: end of it_bdc.
*----------------------------------------------------------------------*
data: begin of it_opt occurs 0.
        include structure ctu_params.
data: end   of it_opt.

data : begin of mess_tab occurs 0.
        include structure bdcmsgcoll.
data : end of mess_tab.

data : disp_mode type ctu_params-dismode value 'N'.
data: lv_msg like cfgnl-msglin.

DATA : extab  TYPE TABLE OF sy-ucomm.
DATA : extab_main typE TABLE OF sy-ucomm.

*-. DATA
DATA : ok_code   LIKE sy-ucomm,
       save_ok   LIKE sy-ucomm.
DATA : g_save    TYPE c VALUE 'X'.
DATA : save_flag(1).


*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
                     IMPORTING er_data_changed
                               e_ucomm,

    handle_changed_finished
                  FOR EVENT data_changed_finished OF cl_gui_alv_grid
                     IMPORTING e_modified
                               et_good_cells,

    handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
                     IMPORTING e_row
                               e_column
                               es_row_no,

    handle_hotspot_click FOR  EVENT hotspot_click OF cl_gui_alv_grid
                      IMPORTING e_row_id
                                e_column_id
                                es_row_no,

     handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
                      IMPORTING es_col_id es_row_no,

     handle_on_f4  FOR EVENT onf4 OF cl_gui_alv_grid
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

* Setting for Change data
  METHOD handle_data_changed.
    PERFORM data_changed USING er_data_changed e_ucomm.
  ENDMETHOD.                    " handle_data_changed

*   In event handler method for event DATA_CHANGED_FINISHED
  METHOD handle_changed_finished.
    PERFORM data_changed_finished USING e_modified et_good_cells.

  ENDMETHOD.                    "handle_changed_finished

* Double Click
  METHOD handle_double_click.
    PERFORM double_click USING e_row
                               e_column
                               es_row_no.
  ENDMETHOD.                    " handle_double_click

* Hotspot click
  METHOD handle_hotspot_click.
    PERFORM hotspot_click USING e_row_id
                                e_column_id
                                es_row_no.

  ENDMETHOD.                    "handle_hotspot_click

*--Handle Button Click
  METHOD handle_button_click .
    PERFORM handle_button_click USING es_col_id es_row_no.
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

ENDCLASS.                   " LCL_EVENT_RECEIVER Implementation
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
FORM data_changed  USING pr_data_changed TYPE REF TO
                                     cl_alv_changed_data_protocol
                         p_ucomm.

  DATA: ls_mod_cells        TYPE lvc_s_modi,
        lt_mod_cells        TYPE lvc_t_modi.

  CLEAR: ls_mod_cells, lt_mod_cells, lt_mod_cells[], lt_mod_cells.

  lt_mod_cells = pr_data_changed->mt_good_cells.
  SORT lt_mod_cells BY row_id.

  LOOP AT lt_mod_cells INTO ls_mod_cells.


  ENDLOOP.

ENDFORM.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*

FORM data_changed_finished  USING    p_e_modified
                                     et_good_cells TYPE lvc_t_modi.

  DATA: ls_good1 TYPE lvc_s_modi.
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        l_mode TYPE raw4.

ENDFORM.                    " DATA_CHANGED_FINISHED
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK
*&---------------------------------------------------------------------*
FORM double_click  USING  e_row     TYPE lvc_s_row
                          e_column  TYPE lvc_s_col
                          es_row_no TYPE lvc_s_roid.

ENDFORM.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
FORM hotspot_click  USING   e_row_id     TYPE  lvc_s_row
                            e_column_id  TYPE  lvc_s_col
                            es_row_no    TYPE  lvc_s_roid.

ENDFORM.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  HANDLE_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM handle_button_click  USING  e_column  TYPE lvc_s_col
                                 es_row_no TYPE lvc_s_roid.

  CASE e_column-fieldname.
*    when ' '.

  ENDCASE.

ENDFORM.                    " HANDLE_BUTTON_CLICK
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
*    when ''.



  ENDCASE.
ENDFORM.                    " ON_F4
