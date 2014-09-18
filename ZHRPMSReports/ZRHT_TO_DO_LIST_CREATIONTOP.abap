*&---------------------------------------------------------------------*
*&  Include           ZRHT_TO_DO_LIST_CREATIONTOP
*&---------------------------------------------------------------------*
TABLES: pa0001, sscrfields.

DATA: ok_code       TYPE sy-ucomm.

DATA: icon_name(20),
      icon_text(20),
      info_text     TYPE icont-quickinfo.

* alv variable definition
DATA: gr_cont       TYPE REF TO cl_gui_custom_container,
      gr_grid       TYPE REF TO cl_gui_alv_grid.
DATA: gs_layo       TYPE lvc_s_layo.
DATA: gt_fcat       TYPE lvc_t_fcat WITH HEADER LINE,
      gt_sort       TYPE lvc_t_sort WITH HEADER LINE.
DATA: gt_excl_func  TYPE ui_functions.

DATA: gt_objec      TYPE TABLE OF objec WITH HEADER LINE,
      gt_struc      TYPE TABLE OF struc WITH HEADER LINE.
DATA: BEGIN OF gt_orglv OCCURS 0,
        objid       TYPE hrp1001-objid,
        sclas       TYPE hrp1001-sclas,
        sobid       TYPE hrp1001-sobid,
      END OF gt_orglv.

DATA: BEGIN OF gt_srows OCCURS 0,
        row_id      TYPE i,
      END OF gt_srows.

DATA: BEGIN OF gt_result OCCURS 0,
        begda       TYPE begda,
        endda       TYPE endda,
        appee       TYPE persno,
        appeenm     TYPE emnam,
        apper       TYPE persno,
        appernm     TYPE emnam,
        eorg1       TYPE orgeh,
        eorglv1     TYPE zdhr_orglv,
        cori1       TYPE persno,
        cornm1      TYPE emnam,
        evai2       TYPE persno,
        evanm2      TYPE emnam,
        eorg2       TYPE orgeh,
        eorglv2     TYPE zdhr_orglv,
        cori2       TYPE persno,
        cornm2      TYPE emnam,
        evai3       TYPE persno,
        evanm3      TYPE emnam,
        eorg3       TYPE orgeh,
        eorglv3     TYPE zdhr_orglv,
        cori3       TYPE persno,
        cornm3      TYPE emnam,
        appri       TYPE persno,
        apprnm      TYPE emnam,
        eorg4       TYPE orgeh,
        eorglv4     TYPE zdhr_orglv,
        cori4       TYPE persno,
        cornm4      TYPE emnam,
        hrtmi       TYPE persno,
        hrtmnm      TYPE emnam,
        error(1),
        etext(255),
        light(30),
      END OF gt_result.

DATA: g_change_flag(1).

*&********************************************************************
*    Selection Screen
*&********************************************************************
SELECTION-SCREEN FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t80.
PARAMETERS: p_date      TYPE sy-datum.
SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO INTERVALS MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t81.
PARAMETERS: p_hrtm      TYPE persno MATCHCODE OBJECT prem OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
*   INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_init_value.   " set initial value
  PERFORM set_button.       " set button
