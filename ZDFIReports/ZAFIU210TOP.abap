*&---------------------------------------------------------------------*
*&  Include           ZAFIU210TOP
*&---------------------------------------------------------------------*
data: ok_code  like sy-ucomm,
      g_save_ok like sy-ucomm,

      g_currency like t001-waers,
      g_dummy_okcode.

data: g_flag type c,  " C:Create  M:Modify  D:Display
      g_changed type c,
      g_error type c,
      g_answer type c.

data: gt_old_impr like standard table of impr_file
                  with header line.

data: begin of it_list occurs 0.
include    structure zsfiu20.
data:   objnr type im_objnr,
        prnam type im_prnam,
        kostl type kostl,
        imak  type imak,
        mark  type c,
        rowscol(4) type c,
        style  type lvc_t_styl.
data: end of it_list.

data: st_list like line of it_list.


ranges: r_poper for mlcd_key-poper.

constants: c_space(1)  value ' ',
           c_suc(1)    value 'S',           " Success
           c_err(1)    value 'E',           " Error
           c_yes(1)    value 'J',           " Yes
           c_no(1)     value 'N',           " No
           c_canc(1)   value 'A',           " Cancel

           con_msgty_abort   like sy-msgty value 'A',
           con_msgty_error   like sy-msgty value 'E'.

data: g_cursor type fieldname.
data: g_wrttp type co_wrttp.

* Container Name
constants c_cont_0100  type scrfname value 'CONTAINER_0100'.
data: g_cust_cont      type ref to cl_gui_custom_container,
      g_con_sp         type ref to cl_gui_splitter_container,
      g_con_left       type ref to cl_gui_container,
      g_con_right      type ref to cl_gui_container.

data: gs_variant type disvariant,
      g_colpos type i.

data: it_rowno  type lvc_t_roid,
      st_rowno  type lvc_s_roid.

* ALV Grid
class lcl_event_receiver definition deferred.
class cl_gui_cfw definition load.


* ALV Grid
data: g_cuctnr  type ref to cl_gui_custom_container,
*      g_alvgrd  type ref to cl_gui_alv_grid.
      g_alvgrd  type ref to cl_gui_alv_grid.


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver definition.

endclass.                    "lcl_event1 DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_event1 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

endclass.                    "lcl_event1 IMPLEMENTATION
