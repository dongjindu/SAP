*&---------------------------------------------------------------------*
*&  Include           ZRCO_BULK_MATERIALTOP
*&---------------------------------------------------------------------*

tables: zsco_bulk_material.

*TYPE-POOLS: VRM,
*            VLCH,
*            SX.

types: begin of ty_fsc,
        fsc    type zfsc,
       end of ty_fsc.
types: tt_fsc type table of ty_fsc.

types: begin of ty_color,
        color type zextc,
       end of ty_color.
types: tt_color type table of ty_color.

data: begin of it_list occurs 0.
include    structure zsco_bulk_material.
data:   fsc        type table of ty_fsc,
        color      type table of ty_color,
        mark       type      c,
        flag       type      c,
        rowscol(4) type      c,
        cellstyl   type      lvc_t_styl,
        cellscol   type      lvc_t_scol.
data: end of it_list.

data: st_list like line of it_list.

types: begin of ty_wosum,
        fsc    type zfsc,
        wo_ser type	zwo_ser,
        nation type znation,
        dealer type zdealer,
        extc type zextc,
        intc type zintc,
       end of ty_wosum.

types: begin of ty_atwrt,
          work_atwrt type atwrt,
          ext_atwrt type atwrt,
          int_atwrt type atwrt,
       end of ty_atwrt.

data: begin of gt_wosum occurs 0,
        sortf type sortp,
        atwrt type table of ty_atwrt,
        wosum type table of ty_wosum,
        end of gt_wosum.

constants: c_flag_x value 'X',
           c_flag_a value 'A',
           c_flag_b value 'B',
           c_flag_c value 'C',
           c_flag_s value 'S',
           c_flag_e value 'E',
           c_flag_f value 'F',
           c_flag_h value 'H',
           c_flag_k value 'K',
           c_flag_r value 'R',
           c_flag_u value 'U',
           c_flag_w value 'W'.

constants: c_true      type c        value 'X',
           c_false     type c        value space.

constants: c_new  type char01 value 'N',
           c_edit type char01 value 'E',
           c_disp type char01 value 'D'.

ranges: r_period for sy-datum.

data: gv_mode type char01.

data: ok_code  like sy-ucomm,
      g_save_ok like sy-ucomm,

      g_currency like t001-waers,
      g_dummy_okcode.

data: g_flag type c,  " C:Create  M:Modify  D:Display
      g_changed type c,
      g_error type c,
      g_answer type c.

ranges: r_poper for mlcd_key-poper.

constants: c_space(1)  value ' ',
           c_suc(1)    value 'S',           " Success
           c_err(1)    value 'E',           " Error
           c_yes(1)    value 'J',           " Yes
           c_no(1)     value 'N',           " No
           c_canc(1)   value 'A'.           " Cancel

data: g_cursor type fieldname.

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
class lcl_custom_alv_grid definition deferred.
class cl_gui_cfw definition load.

* ALV Grid
data: g_cuctnr  type ref to cl_gui_custom_container,
*      g_alvgrd  type ref to cl_gui_alv_grid.
      g_alvgrd  type ref to lcl_custom_alv_grid.

data: g_layout value 'V'.

data: gt_bulk_um type table of zsco_bulk_per_um
                      with header line,
      gt_color   type table of ztco_bulk_pcolor
                      with header line.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_custom_alv_grid definition inheriting from cl_gui_alv_grid.
  public section.

    methods: handle_user_command
                 for event user_command of cl_gui_alv_grid
                 importing e_ucomm.

*  protected section.
*    methods: set_data_table redefinition.

endclass.                    "LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_custom_alv_grid implementation.
  method handle_user_command.
*    perform handle_user_command using e_ucomm .
  endmethod.                    "HANDLE_USER_COMMAND      "set_fixed_rows_public
*  method set_data_table.
*    call method super->set_data_table
*      changing
*        data_table = data_table.
*  endmethod.                    "set_data_table

endclass.                    "LCL_EVENT_RECEIVER
