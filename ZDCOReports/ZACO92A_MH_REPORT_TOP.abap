*----------------------------------------------------------------------*
*   INCLUDE ZACO92A_MH_REPORT_TOP                                      *
*----------------------------------------------------------------------*
REPORT zaco92a_mh_report MESSAGE-ID  zmco.

*Table definition
TYPE-POOLS: slis, vrm.

INCLUDE <icon>.
INCLUDE <symbol>.
CLASS cl_gui_resources DEFINITION LOAD.

*Internal table definition
DATA : it_stmh_at LIKE ztco_stmh_at OCCURS 0 WITH HEADER LINE.
DATA : it_stmh LIKE ztco_stmh OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF it_out OCCURS 0,
        annul_qty LIKE ztco_stmh_at-mbgbtr,
        standard_qty LIKE ztco_stmh_at-mbgbtr,
        chkbox       TYPE c,
        light        TYPE c,
        tabcolor     TYPE slis_t_specialcol_alv.
        include structure ztco_stmh_At.
data : END OF it_out.

*Data definition
DATA : w_int TYPE i.
*--- ALV------------------------------------
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv.
DATA: wa_repid LIKE sy-repid,
      wa_var_save(1) TYPE c             VALUE  'A',
      wa_default(1)  TYPE c,
      wa_exit(1) TYPE c,
      wa_variant LIKE disvariant,
      wa_var LIKE disvariant,
      wa_alv_function_name(30) TYPE c VALUE 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) TYPE c.

DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-001.
* General Info.
PARAMETERS : p_kokrs LIKE csks-kokrs   MEMORY ID cac  OBLIGATORY
               DEFAULT 'H201'.

SELECTION-SCREEN SKIP 1.
* Posted Yr.
PARAMETERS : p_bdatj LIKE keko-bdatj MEMORY ID bdtj OBLIGATORY
             DEFAULT sy-datum(4).
PARAMETERS: p_poper LIKE covja-perab MEMORY ID vpe
            MODIF ID per OBLIGATORY .
SELECTION-SCREEN END OF BLOCK bl1.
