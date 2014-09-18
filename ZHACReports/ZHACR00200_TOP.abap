*&---------------------------------------------------------------------*
*&  Include           ZHARC00200_TOP
*&---------------------------------------------------------------------*
*- T y p e p o o l s --------------------------------------------------*
type-pools: cxtab,
            fmmd,
            vrm,
            abap.

*- T a b l e s --------------------------------------------------------*
tables: rlgrap,
        sscrfields,
        t001,
        rgsbs,
        sval,
        dd02l.

*- T y p e s ----------------------------------------------------------*
*  Prefix all constants with TY_
*** for finding  table  and fields
*TYPES :BEGIN OF gt_fieldcat.
*TYPES:  z_lvc_t_fcat TYPE lvc_t_fcat.
*TYPES: END OF gt_fieldcat.

*- V a r i a b l e s --------------------------------------------------*
*  Use the follwing prefixes:
*    GT_ : Internal tables
*    GR_ : Ranges
*    GS_ : General work areas
*    GV_ : Global Area Variance / Flags / Switchs
*    LV_ : Local Area Variance
*    FS_ : Field symbols

data: gv_filename          type string.
data: gv_filelength        type  i.
data: ldtab                type ref to data.
data: dref                 type ref to data.

data: gt_dfies             like dfies occurs 0 with header line.

data: gv_dialog_container  type ref to cl_gui_dialogbox_container,
      gv_grid              type ref to cl_gui_alv_grid.

*-Define Global Variables
data: gv_start(1)          type c,                     " start value
      gv_first_display,                                " first display
      gv_subrc             like sy-subrc,              " subrc
      gv_value             like sval-value,            " fix value
      gv_repid             like sy-repid.              " program name

data: feld                 like rsparams-low,
      ok_code              like sy-ucomm,
      gv_okcode            like sy-ucomm.

*-Define ALV Grid
data: gt_fieldcat          type lvc_t_fcat,        " Field catalog
      gs_fieldcat          type lvc_s_fcat,        " Field catalog
      gt_sort              type lvc_t_sort,        " Sort
      gs_layout            type lvc_s_layo,        " layout
      gt_exclude           type ui_functions,      " Status (Toolbar)
      gs_variant           type disvariant.        " Variant

data: name                 type string.

data: para_tab             type abap_func_parmbind_tab.
data: para_line            like line of para_tab.

data: excp_tab             type abap_func_excpbind_tab.
data: excp_line            like line of excp_tab.

*- C o n s t a n t s --------------------------------------------------*
*  Prefix all constants with GC_
constants:
      gc_yes(1)             type c             value  'Y',   " True
      gc_mark(1)            type c             value  'X',
      gc_no(1)              type c             value  'N'.   " False

field-symbols: <gt_table>   type table,
               <gt_temp>    type table,

               <fs>         type any,
               <gt_line>    type any.

*- S e l e c t i o n   s c r e e n ------------------------------------*
*  Use the follwing prefixes:
*    P_  : Parameters
*    S_  : Select-options
*    RB_ : Radio buttons
*    CB_ : Check box

selection-screen begin of block sel_block with frame title frm_titl.
parameters: p_table   like databrowse-tablename  obligatory DEFAULT 'ZBDCTABLE',
            p_file    like rlgrap-filename       obligatory.
selection-screen end of block sel_block.

selection-screen begin of block sel_blk with frame title frm_tit.

parameters: rb_dow radiobutton group g1  default 'X',
            rb_up  radiobutton group g1.
selection-screen end of block sel_blk.
