*&---------------------------------------------------------------------*
*& Include MZMMO_IF010TOP                                              *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM  sapmzmmo_if010                .

TABLES: ztmm_if024.

DATA BEGIN OF it_sub OCCURS 0.
        INCLUDE STRUCTURE ztmm_if024.
DATA: lv_mark TYPE c.
DATA END OF it_sub.

DATA: st_sub LIKE ztmm_if024.

DATA: gv_first TYPE c,
      v_btn_flag TYPE c.

DATA: ok_code TYPE sy-ucomm.

CONTROLS tc_list TYPE TABLEVIEW USING SCREEN 100.
