*&---------------------------------------------------------------------*
*&  Include           ZRHR_STLT_GOAL_REPORTTOP
*&---------------------------------------------------------------------*
TYPE-POOLS: vrm.

TABLES: pa0001, hrp1001.

DATA: ok_code                 TYPE sy-ucomm.

* alv variable definition
DATA: gr_cont                 TYPE REF TO cl_gui_custom_container,
      gr_grid                 TYPE REF TO cl_gui_alv_grid.
DATA: gs_layo                 TYPE lvc_s_layo.
DATA: gt_fcat                 TYPE lvc_t_fcat WITH HEADER LINE,
      gt_sort                 TYPE lvc_t_sort WITH HEADER LINE.

* dropdown definition
DATA: gt_values               TYPE vrm_values,
      gs_value                LIKE LINE OF gt_values,
      g_fieldname             TYPE vrm_id.

DATA: BEGIN OF gt_hrp1000 OCCURS 0,
          plvar               TYPE hrp1000-plvar,
          otype               TYPE hrp1000-otype,
          objid               TYPE hrp1000-objid,
          stext               TYPE hrp1000-stext,
        END OF gt_hrp1000.

DATA: BEGIN OF gs_result.
INCLUDE  TYPE zshr_comp_result.
*DATA:   short    TYPE tdline,
*        long     TYPE tdline,
data:    pernr    TYPE persno,
         r15 type HAP_VALUE_NUM, "Z1
         r16 type HAP_VALUE_NUM, "Z3
         r17 type HAP_VALUE_NUM, "Z5
         r18 type HAP_VALUE_NUM, "Z7
         r19 type HAP_VALUE_NUM, "Z9
         r20 type HAP_VALUE_NUM. "Z0
DATA: END OF gs_result.

DATA: gt_result               LIKE TABLE OF gs_result.

dATA: BEGIN OF it_role_id OCCURS 0,
          seq(2) type n,
          role_id TYPE HAP_ROLE_ID,
     END OF it_role_id.
data: l_rating type HAP_VALUE_NUM.

CONSTANTS: c_column_iid_zp13  TYPE hap_column_iid VALUE 7,    " Column Self Appraisal (YE)
           c_column_iid_zp14  TYPE hap_column_iid VALUE 8.

*&********************************************************************
*    Selection Screen
*&********************************************************************
SELECTION-SCREEN BEGIN OF SCREEN 200 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(19) text-t80 FOR FIELD p_year.
PARAMETERS: p_year      TYPE zdhr_year AS LISTBOX VISIBLE LENGTH 10.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(16) text-t83 FOR FIELD s_kostl.
SELECT-OPTIONS: s_kostl FOR pa0001-kostl NO INTERVALS
                                         MATCHCODE OBJECT kost.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(16) text-t85 FOR FIELD s_pernr.
SELECT-OPTIONS: s_pernr FOR pa0001-pernr NO INTERVALS
                                         MATCHCODE OBJECT prem.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 2(19) text-t87 FOR FIELD p_year.
PARAMETERS: p_stats type hap_ap_status
*    OBLIGATORY
     as listbox visible length 18.
SELECTION-SCREEN END OF LINE.


SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF SCREEN 200 .
