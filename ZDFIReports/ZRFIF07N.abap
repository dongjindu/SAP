*&---------------------------------------------------------------------*
*& Program: ZRFIF07N                                                    *
*& Type   : Report                                                     *
*& Author :
*& Title  : Expense Budget Report (FM)                                 *
*&---------------------------------------------------------------------*
*   Requested by:        Andy Choi                                     *
*&---------------------------------------------------------------------*
* RFFMEP1A

report zrfif07 line-size 132 line-count 65
                             no standard page heading message-id db .

*&---------------------------------------------------------------------*
*&  Include           ZRFIF07NTOP
*&---------------------------------------------------------------------*
*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
tables : fmioi,v_fmifi,
         ekbe,
         eban, ekpo,
         ebkn,       "PR account assignment
         fmhictr, fmfctr, tbpfm, fmci, fmfincode.

define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*

*Commitment reading from DB
data : begin of gt_cmmt occurs 0,
        gjahr like fmioi-gjahr,
        poper type co_perio,

        fistl like fmioi-fistl,
        fonds like fmioi-fonds,
        fipex like fmioi-fipex,

*------ for PM
        objnrz type j_objnr,
*        awtyp  TYPE awtyp,

        btart like fmioi-btart,
        wrttp like fmioi-wrttp,

        fkbtr like fmioi-fkbtr,  "AMOUNT

        hkont like fmioi-hkont,
        budat like fmioi-budat,

        lifnr like fmioi-lifnr,

        vrefbt like fmioi-vrefbt, "Predecessor document
        vrefbn like fmioi-vrefbn,
*       VRFORG like fmioi-VRFORG,
        vrfpos like fmioi-vrfpos,

        kngjahr type fm_kngjahr,
        refbn  like fmioi-refbn,
*       RFORG  like fmioi-RFORG,
        rfpos  like fmioi-rfpos,
        refbt  like fmioi-refbt,   "Reference document category

**------ for PM
*        aufnr  TYPE aufnr,
*        txz01  LIKE ekpo-txz01,
**        txz01 TYPE txz01,
        loekz  like fmioi-loekz,
       end of gt_cmmt.

*actual data reading
data : gt_fi like gt_cmmt occurs 0 with header line.

data : begin of it_fmci occurs 0,
         fipos    like fmci-fipos,
         posit    like fmep-posit,
         bezei  like fmcit-bezei,
         profil   type bp_bprofil,
       end of it_fmci.

data : begin of it_pr occurs 0,
         banfn   like eban-banfn,
         bnfpo   like eban-bnfpo,
       end of it_pr.

data : begin of it_po occurs 0,
         ebeln   like ekpo-ebeln,
         ebelp   like ekpo-ebelp,
       end of   it_po.

data e_level(3) type c.
data l_cousertype(3) type c.
data $ix type i.

type-pools: slis.

*structure: ZSFMLINE
types: begin of ty_output,
*         sort(1),
         fonds   like fmioi-fonds ,
         fistl   like fmioi-fistl ,
         kostl   like ebkn-kostl,
         fipex   like fmioi-fipex,
         hkont   like fmioi-hkont,
         aufnr   type aufnr,
         lifnr   like fmioi-lifnr,
         cffld(01),
         doctx(05),
         refbt like fmioi-refbt,
         refbn like fmioi-refbn,
         rfpos like fmioi-rfpos,
         fkbtr like fmioi-fkbtr,  "commitment/actual
*         fm_orig LIKE fmioi-fkbtr,
*         prghr  TYPE gjahr,
         prdoc(11),
         prpos like fmioi-vrfpos,
         pramt  type dmbtr,       "PR Amount
*         poghr  TYPE gjahr,
         podoc(11),
         popos like fmioi-vrfpos,
         poamt  type dmbtr,       "PO Amount
         grghr  type gjahr,
         grdoc(11),
         grcnt type i,
         grpos like fmioi-vrfpos,
** furong 08/05/14 (
*         gramt  type dmbtr,       "GR Amount
         gramt  type DMBTR_X8,
** )
         ivghr  type gjahr,
         ivdoc  type belnr_d,
         ivdocs(11),
         ivcnt  type i,
         ivamt  type dmbtr,       "IV Amount
         dpghr  type gjahr,
         dpdoc(11),
         dpcnt  type i,
         dpamt  type dmbtr,       "DP Amount
         tfghr  type gjahr,
         tfdoc(11),
         tfcnt  type i,
         tfamt  type dmbtr,       "transfer posting
         augdt  like bsak-augdt,  "clearing date

         pyghr  type gjahr,
         pydoc(11),  "Payment doc
         pyamt  type dmbtr,       "Payment Amount

         blamt  type dmbtr,       "commitment balance
       end of ty_output.

*DATA : gt_hash TYPE hashed TABLE OF zsfmline WITH HEADER LINE.
data : gt_out type ty_output occurs 0. " WITH HEADER LINE.
data : gt_inv type ty_output occurs 0, " WITH HEADER LINE,
       gt_dnp type ty_output occurs 0, " WITH HEADER LINE,
       gt_trf type ty_output occurs 0. "" WITH HEADER LINE.
*       INITIAL SIZE 0.

types: begin of gty_prpo,
        refbn type fm_vrefbn,
        rfpos type fm_vrfpos,
*        gjahr TYPE gjahr,
        fkbtr like fmioi-fkbtr,
        vrefbn type fm_vrefbn,
        vrfpos type fm_vrfpos,
*        vrfghr type gjahr,
        hkont type hkont,
      end of gty_prpo.
data: gt_pr   type table of gty_prpo. " WITH NON-UNIQUE KEY refbn rfpos gjahr.
data: gt_po   type table of gty_prpo. " WITH NON-UNIQUE KEY refbn rfpos gjahr.

*GR, IV, DP, TRF documents
data : begin of wa_detail,
         refbn like fmioi-refbn,
         rfpos like fmioi-rfpos,
         doctx(02),
         gjahr type gjahr,
         docnr like fmioi-refbn,
         docid like fmioi-rfpos,
         amtxx  type dmbtr,       "PO Amount
       end of wa_detail.
*DATA : it_detail LIKE STANDARD TABLE OF wa_detail  WITH HEADER LINE
*       INITIAL SIZE 0.
data : it_detail2 like standard table of wa_detail  with header line
       initial size 0.

types: begin of ty_ebkn,
        banfn like ebkn-banfn,
        bnfpo like ebkn-bnfpo,
        aufnr like ebkn-aufnr,
        kostl like ebkn-kostl,
        sakto like ebkn-sakto,
        fistl like ebkn-fistl,
        geber like ebkn-geber,
        fipos like ebkn-fipos,
        lifnr like ekko-lifnr,
        bedat like ekko-bedat,
      end of ty_ebkn.
data: it_ebkn type table of ty_ebkn with header line.

types: begin of ty_ekkn,
        ebeln like ekkn-ebeln,
        ebelp like ekkn-ebelp,
        aufnr like ekkn-aufnr,
        kostl like ekkn-kostl,
        sakto like ekkn-sakto,
        fistl like ekkn-fistl,
        geber like ekkn-geber,
        fipos like ekkn-fipos,
      end of ty_ekkn.
data: it_ekkn type table of ty_ekkn with header line.

types: begin of ty_gr,
         ebeln like ekbe-ebeln,
         ebelp like ekbe-ebelp,
         gjahr like ekbe-gjahr,
         belnr like ekbe-belnr,
         buzei like ekbe-buzei,
         bwart like ekbe-bwart,
         budat like ekbe-budat,
         shkzg like ekbe-shkzg,
         menge like ekbe-menge,
         wrbtr like ekbe-wrbtr,
         netpr like ekpo-netpr,
         peinh like ekpo-peinh,
       end of ty_gr.
data: gt_gr type table of ty_gr.

data: begin of it_aufk occurs 0,
        aufnr like aufk-aufnr,
        kostl like aufk-kostl,
        kostv like aufk-kostv,
      end of it_aufk.

data: begin of it_ekpo occurs 0,
        ebeln like ekpo-ebeln,
        ebelp like ekpo-ebelp,
        erekz like ekpo-erekz,  "Final invoice indicator
        netpr like ekpo-netpr,
        peinh like ekpo-peinh,
        pstyp like ekpo-pstyp,  "Item category in purchasing document
        lifnr like ekko-lifnr,
        bedat like ekko-bedat,
      end of it_ekpo.

*--------------------------------------------------------------------*
* ALV FIELD START
*--------------------------------------------------------------------*

*-- ALV Global Field
* ALV Variant
data: alv_variant   like disvariant,
      alv_repid     like sy-repid.

* ALV Attribute
data: gt_exclude   type ui_functions,   " Tool Bar ButtonAuAa
      gs_fieldcat  type lvc_s_fcat,     " CE#a 糘己 Set
      gt_fieldcat  type lvc_t_fcat,     " CE#a 糘己 AuAa
      gs_f4        type lvc_s_f4,                           " F4 CE#a
      gt_f4        type lvc_t_f4,
      gs_layocat   type lvc_s_layo,     " Grid 糘己 AuAa(Display)
      gs_sort      type lvc_s_sort,
      gt_sort      type lvc_t_sort,
      g_repid      type sy-repid,
      g_title      type lvc_title ,
      ls_col       type sy-tabix,
      l_scroll     type lvc_s_stbl.

data: alv_variant2  like disvariant,
      alv_repid2    like sy-repid.
data: gt_exclude2   type ui_functions,   " Tool Bar ButtonAuAa
      gs_fieldcat2  type lvc_s_fcat,     " CE#a 糘己 Set
      gt_fieldcat2  type lvc_t_fcat,     " CE#a 糘己 AuAa
      gs_f42        type lvc_s_f4,                           " F4 CE#a
      gt_f42        type lvc_t_f4,
      gs_layocat2   type lvc_s_layo,     " Grid 糘己 AuAa(Display)
      gs_sort2      type lvc_s_sort,
      gt_sort2      type lvc_t_sort,
      g_repid2      type sy-repid,
      g_title2      type lvc_title ,
      ls_col2       type sy-tabix,
      l_scroll2     type lvc_s_stbl.

data: f_color type lvc_t_scol with header line,
      t_col   type lvc_s_colo.

*-- Drop-Down List Box List.
data: gt_dropdown type lvc_t_dral,
      gs_dropdown type lvc_s_dral.

data: e_valid     type c,
      c_refresh   type c.

* search help
type-pools : f4typ.
data: gt_help_value  like help_value       occurs 0
      with header line,
      gt_heading_tab type f4typ_head_struc occurs 0
      with header line,
      g_choices      like sy-tabix.
data  gt_good_cells.

**-- CLASS Reference

class :lcl_alv_grid      definition deferred ,
       lcl_event_handler definition deferred .

data : g_customer         type ref to cl_gui_custom_container,
       g_event_handler    type ref to lcl_event_handler,
       gc_docking_container type ref to cl_gui_docking_container,
       g_grid             type ref to lcl_alv_grid .
data : g_customer2        type ref to cl_gui_custom_container,
       g_event_handler2   type ref to lcl_event_handler,
       g_grid2            type ref to lcl_alv_grid .
data container type ref to cl_gui_custom_container.
data g_splitter type ref to cl_gui_splitter_container.
data g_container_1 type ref to cl_gui_container.
data g_container_2  type ref to cl_gui_container.

data : x_check(1).
data : ok_code like sy-ucomm.
data : g_total type dmbtr.
*--------------------------------------------------------------------*
* CONSTANTS
*--------------------------------------------------------------------*
constants : c_container   type char20 value 'MY_CONTAINER' .

*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
selection-screen begin of block b1 with frame title  text-001.

parameters: p_fikrs like fmioi-fikrs obligatory memory id fik,
            p_gjahr like fmioi-gjahr obligatory memory id gjr.

parameters : p_dfctr type xfeld radiobutton group meth default 'X',
             p_dfund type xfeld radiobutton group meth,
             p_dopen type xfeld radiobutton group meth.
selection-screen end of block b1.

selection-screen begin of block b2 with frame title  text-002.
select-options: s_fistl  for fmioi-fistl memory id fis,
                s_fonds  for fmioi-fonds. " MEMORY ID fic.

select-options: s_gjahr for fmioi-gjahr no intervals,
                s_perio  for fmioi-perio,
                s_fipex  for fmioi-fipex memory id fps,
                s_prof   for tbpfm-profil.  "default 'B' option NE

selection-screen end of block b2.
select-options: s_refbt  for fmioi-refbt.
select-options: s_refbn  for fmioi-refbn.


*SELECT-OPTIONS :
*                p_prof  FOR tbpfm-profil,
*                p_knz   FOR fmci-knzaepo DEFAULT '3'.  " item category

*SELECT-OPTIONS : s_lifnr  FOR fmioi-lifnr,
*                 s_hkont  FOR fmioi-hkont,
*                 s_refbn  FOR fmioi-refbn,
*                 s_rfpos  FOR fmioi-rfpos,
*
*                 s_vrefbt FOR fmioi-vrefbt,
*                 s_vrefbn FOR fmioi-vrefbn,
*                 s_vrfpos FOR fmioi-vrfpos,
*                 s_budat  FOR fmioi-budat.
**                 s_aufnr FOR ebkn-aufnr.

*SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE  text-002.
*
*PARAMETERS p_op0   RADIOBUTTON GROUP radi. "Summary
*PARAMETERS p_op1   RADIOBUTTON GROUP radi. "Detail
*
*SELECTION-SCREEN END OF BLOCK b2.

* Layout
selection-screen begin of block b4 with frame title text-010.
parameter p_head as checkbox.
parameter p_vari type slis_vari.
selection-screen end of block b4.

ranges: s_ebeln for ekpo-ebeln,
        s_banfn for ebkn-banfn,
        s_bnfpo for ebkn-bnfpo,
        s_ebelp for ekpo-ebelp.

*
*&---------------------------------------------------------------------*
*&  Include           ZRFIR07NC01
*&---------------------------------------------------------------------*
************************************************************************
* LOCAL CLASSES: Definition
************************************************************************
class lcl_alv_grid definition inheriting from cl_gui_alv_grid.

  public section.

    methods:  set_toolbar_buttons1 . " REDEFINITION.
    methods:  set_toolbar_buttons2 . " REDEFINITION.


endclass.                    "LCL_ALV_GRID DEFINITION
*---------------------------------------------------------------------*
*       CLASS LCL_ALV_GRID IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
class lcl_alv_grid implementation.

  method set_toolbar_buttons1 .

    data lt_button  type ttb_button .

    lt_button = me->mt_toolbar_set .

    field-symbols : <f1>  type stb_button .

    loop at lt_button assigning <f1> .
      case <f1>-function .
        when 'FUN1' or 'FUN2' or 'FUN3' or 'FUN4' or 'FUN5'.

          <f1>-disabled = 'X' .
      endcase .
    endloop .

    call method me->set_toolbar_buttons
      exporting
        toolbar_table = lt_button.

  endmethod .                    "SET_TOOLBAR_BUTTONS

  method set_toolbar_buttons2 .

    data lt_button  type ttb_button .

    lt_button = me->mt_toolbar_set .

    field-symbols : <f1>  type stb_button .

    loop at lt_button assigning <f1> .
      case <f1>-function .
        when 'FUN3' .
*          READ TABLE gt_list  WITH KEY confm = c_b
*                              TRANSPORTING NO FIELDS .
*          CHECK sy-subrc IS INITIAL .
*          <f1>-disabled = 'X' .
      endcase .
    endloop .

    call method me->set_toolbar_buttons
      exporting
        toolbar_table = lt_button.

  endmethod .                    "SET_TOOLBAR_BUTTONS

endclass.                    "LCL_ALV_GRID IMPLEMENTATION



*----------------------------------------------------------------------*
*       CLASS lcl_event_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler definition .
  public section .
    methods : handle_toolbar
                 for event toolbar of cl_gui_alv_grid
                 importing e_object e_interactive.

    methods : handle_user_command
                 for event user_command of cl_gui_alv_grid
                 importing e_ucomm.

    methods : handle_after_user_command
                  for event after_user_command of cl_gui_alv_grid
                  importing  e_ucomm .

    methods : handle_data_changed
                 for event data_changed of cl_gui_alv_grid
                 importing er_data_changed e_onf4.

    methods : handle_data_changed_finished
                 for event data_changed_finished of cl_gui_alv_grid
                 importing e_modified
                           et_good_cells.

    methods : handle_onf4 for event onf4 of cl_gui_alv_grid
                 importing sender
                           e_fieldname
                           e_fieldvalue
                           es_row_no
                           er_event_data
                           et_bad_cells
                           e_display.

    methods : handle_hotspot_click for event hotspot_click
                                        of cl_gui_alv_grid
                 importing e_row_id
                           e_column_id
                           es_row_no.

endclass.  "(lcl_event_handler DEFINITION)
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
class lcl_event_handler implementation.
*-- ToolBar
  method handle_toolbar.
*    PERFORM toolbar_pros  USING e_object e_interactive.
  endmethod.                    "handle_toolbar

  method handle_user_command.
*    PERFORM user_command_pros USING e_ucomm.
  endmethod.                    "handle_user_command

  method handle_after_user_command.
*    PERFORM after_user_command_pros USING e_ucomm.
  endmethod.                    "handle_user_command

*-- Data change##
  method handle_data_changed.
*    PERFORM data_changed  USING er_data_changed e_onf4.
  endmethod.                    "handle_data_changed

*-- Data change# #
  method handle_data_changed_finished.
*    PERFORM data_changed_finished USING e_modified
*                                        et_good_cells.
  endmethod.                    "HANDLE_DATA_CHANGED_FINISHED

*-- On help f4 - Search Help
  method handle_onf4.
*    PERFORM on_f4 USING sender
*                        e_fieldname
*                        e_fieldvalue
*                        es_row_no
*                        er_event_data
*                        et_bad_cells
*                        e_display.
  endmethod.    "handle_onf4_click

  method handle_hotspot_click.
    perform hotspot_click using e_row_id
                                e_column_id
                                es_row_no.
  endmethod.    "hotspot_click

endclass. "lcl_event_handler IMPLEMENTATION


*global variables
ranges : r_btart   for fmioi-btart.
ranges : r_btart_o for fmioi-btart.
ranges : r_btart_c for fmioi-btart.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen on value-request for p_vari.
  perform alv_variant_f4 changing p_vari.

*-------------------------------------------------------------*
initialization.

  refresh: r_btart, r_btart_o, r_btart_c.
  r_btart+0(3) = 'IEQ'.
  r_btart-low = '0100'.    append r_btart.  "original
  r_btart-low = '0150'.    append r_btart.  "change
  r_btart-low = '0350'.    append r_btart.  "c/f prev year
*no need to consider c/f next year in the report - ANDY
*  r_btart-low = '0300'.    APPEND r_btart.  "c/f next year
  r_btart-low = '0200'.    append r_btart.  "reduction
  r_btart-low = '0500'.    append r_btart.  "adjust by f.u.doc

  r_btart_o+0(3) = 'IEQ'.
  r_btart_o-low = '0100'.    append r_btart_o.  "original
*  r_btart_o-low = '0150'.    APPEND r_btart_o.  "change
  r_btart_o-low = '0350'.    append r_btart_o.  "c/f prev year

  r_btart_c+0(3) = 'IEQ'.
*  r_btart_c-low = '0300'.    APPEND r_btart_c.  "c/f next year
  r_btart_c-low = '0200'.    append r_btart_c.  "reduction
  r_btart_c-low = '0500'.    append r_btart_c.  "adjust by f.u.doc


  refresh s_prof.
  s_prof-option = 'EQ'. s_prof-sign = 'I'.
  s_prof-low = 'F'. append s_prof.
  s_prof-low = 'M'. append s_prof.
  s_prof-low = 'Q'. append s_prof.
  s_prof-low = 'H'. append s_prof.
  s_prof-low = 'Y'. append s_prof.


*-------------------------------------------------------------*
start-of-selection.
*--------------------------------------------------------------*
  perform get_master.

* Select Data from  FMIOI
  perform get_data1.
* Process PO & PR Data
  perform process_commitment.

* Select Data from  actual
  perform get_data2.
  perform process_fidocs.
  perform merge_to_out.

  perform get_data_gr.
  perform fill_pay_info.


* Display ALV
  call screen 0100.
*-------------------------------------------------------------*
end-of-selection.
*--------------------------------------------------------------*


*NCLUDE ZRFIR07NO01.
*&---------------------------------------------------------------------*
*&  Include           ZRFIR07O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status '0100'.
  set titlebar '0100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_screen output.

  perform display_alv_ouput.

endmodule.                 " INIT_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.

  set pf-status '0200'.
  set titlebar '0200'.


endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_screen_0200 output.

  perform display_alv_ouput_0200.

endmodule.                 " INIT_SCREEN_0200  OUTPUT

*NCLUDE ZRFIR07NI01.
*&---------------------------------------------------------------------*
*&  Include           ZRFIR07I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  call method g_grid->check_changed_data.  "changed data

  data : l_code type sy-ucomm.
  l_code = ok_code.
  clear ok_code.
  case l_code.
    when 'BACK'.
      leave to screen 0.
    when others.

  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMNAD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit_commnad input.
  case ok_code.
    when 'EXIT' or 'CANC'.
      if sy-dynnr = '0110' or
         sy-dynnr = '0120'.
        leave to screen 0.
      else.
        leave program.
      endif.
  endcase.
endmodule.                 " EXIT_COMMNAD  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.

  call method g_grid2->check_changed_data.  "changed data

  data : l_code2 type sy-ucomm.
  l_code2 = ok_code.
  clear ok_code.
  case l_code2.
    when 'CANC'.
      leave to screen 0.
    when 'ENTR'.
      leave to screen 0.
    when others.

  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT

*NCLUDE ZRFIR07NF01.

*&---------------------------------------------------------------------*
*&  Include           ZRFIR07F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VARI  text
*----------------------------------------------------------------------*
form alv_variant_f4 changing p_vari.
  data: rs_variant like disvariant,
        lv_nof4 type c.

  clear lv_nof4.
  loop at screen.
    if screen-name = 'PA_VARI'.
      if screen-input = 0.
        lv_nof4 = 'X'.
      endif.
    endif.
  endloop.

  clear rs_variant.
  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant = rs_variant
      i_save     = 'A'
    importing
      es_variant = rs_variant
    exceptions
      others     = 1.

  if sy-subrc = 0 and lv_nof4 = space.
    p_vari = rs_variant-variant.
  endif.

endform.                    " ALV_VARIANT_F4
*&---------------------------------------------------------------------*
*&      Form  get_data1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data1 .

*  RANGES : r_erlkz FOR fmioi-erlkz.
*  r_erlkz+0(3) = 'EEQ'.
*  r_erlkz-low = 'F'.    "PR reduced
*  APPEND r_erlkz.
*  r_erlkz-low = 'X'.    "PO completed
*  APPEND r_erlkz.




* select pr/po details.
  if p_dopen = 'X'.
    select gjahr
           fistl
           fonds
           fipex
           objnrz
           btart
           wrttp
           fkbtr
           hkont
           lifnr
           vrefbt
           vrefbn
           vrfpos
           refbn
           rfpos
           refbt
           loekz
      into corresponding fields of table gt_cmmt
      from  fmioi
     where gjahr  =  p_gjahr   and
           fistl  in s_fistl   and
           fonds  in s_fonds   and
           erlkz  = space      and
           fipex  in s_fipex   and
*         gjahr  IN s_gjahr   AND   "ANDY
           perio  in s_perio   and
           btart  in r_btart   and
*          budat  IN s_budat   AND
*          hkont  IN s_hkont   AND
           refbt  in s_refbt   and
           refbn  in s_refbn   and
*           erlkz  IN r_erlkz   AND
           fikrs  =  p_fikrs   and
           fkbtr <> 0.
  elseif p_dfctr = 'X'.   "optimized for fund center
    select gjahr
           fistl
           fonds
           fipex
           objnrz
           btart
           wrttp
           fkbtr
           hkont
           lifnr
           vrefbt
           vrefbn
           vrfpos
           refbn
           rfpos
           refbt
           loekz
    into corresponding fields of table gt_cmmt
      from  fmioi
     where gjahr  =  p_gjahr   and
           fistl  in s_fistl   and
           fonds  in s_fonds   and
           fipex  in s_fipex   and
*         gjahr  IN s_gjahr   AND   "ANDY
           perio  in s_perio   and
           btart  in r_btart   and
*          budat  IN s_budat   AND
*          hkont  IN s_hkont   AND
           refbt  in s_refbt   and
           refbn  in s_refbn   and
*           erlkz  IN r_erlkz   AND
           fikrs  =  p_fikrs   and
           fkbtr <> 0.
  else.
    select gjahr
           fistl
           fonds
           fipex
           objnrz
           btart
           wrttp
           fkbtr
           hkont
           lifnr
           vrefbt
           vrefbn
           vrfpos
           refbn
           rfpos
           refbt
           loekz
    into corresponding fields of table gt_cmmt
      from  fmioi
     where fonds  in s_fonds   and
           fipex  in s_fipex   and
           fistl  in s_fistl   and
           gjahr  = p_gjahr    and   "ANDY
*        gjahr  IN s_gjahr   AND   "ANDY
           perio  in s_perio   and
           btart  in r_btart   and
*          budat  IN s_budat   AND
*          hkont  IN s_hkont   AND
           refbt  in s_refbt   and
           refbn  in s_refbn   and
*           erlkz  IN r_erlkz   AND
           fikrs  =  p_fikrs   and
           fkbtr <> 0.
  endif.

  data $ix type i.
  data $profil  like tbpfm-profil.

*** : C/F data that marked deletion are deleted.
*FIXME - ANDY
*  DELETE lt_tab WHERE btart = '0350'
*                  AND loekz = 'X'.

  exit.

* select only related budget profile of commitment item
  loop at gt_cmmt.
    $ix = sy-tabix.
    clear $profil.

    read table it_fmci with key fipos = gt_cmmt-fipex binary search.
*    PERFORM get_budget_period USING gt_cmmt-fistl
*                                    gt_cmmt-fipex
*                                    gt_cmmt-fonds
*                                    gt_cmmt-gjahr
*                              CHANGING $profil.
*    IF NOT $profil IN s_prof.
    if sy-subrc ne 0.
      delete gt_cmmt index $ix.
    endif.

*    if gt_cmmt-refbt = '010'. "PR
*       append gt_cmmt to gt_cmmt_pr.
*    endif.
  endloop.

*  sort gt_cmmt_pr by REFBN RFPOS.

endform.                    " get_data1
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_alv_ouput .

  data : l_offline           type char1.

  call method cl_gui_alv_grid=>offline
    receiving
      e_offline = l_offline.


*** : Full Screen
  if gc_docking_container is initial.

    if l_offline eq 0.
      create object gc_docking_container
        exporting
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = gc_docking_container->dock_at_left
          extension = 2000.
    endif.
  endif.

if p_head eq 'X'.

* Create a splitter with 2 rows and 1 column
  create object g_splitter
    exporting
      parent  = gc_docking_container
      rows    = 2
      columns = 1.

** Upper Container
  call method g_splitter->get_container
    exporting
      row       = 1
      column    = 1
    receiving
      container = g_container_1.

** Lower Container
  call method g_splitter->get_container
    exporting
      row       = 2
      column    = 1
    receiving
      container = g_container_2.

** Upper Container height

  call method g_splitter->set_row_height
    exporting
      id     = 1
      height = 13.

  create object g_grid
    exporting
      i_parent          = g_container_2
*     i_appl_events     = 'X'
    exceptions
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4.

  if sy-subrc <> 0.
    message a004 with 'Screen Initialization Error' .
  endif.

  perform header.

else.

* create an instance of alv control
  create object g_grid
    exporting
      i_parent          = gc_docking_container  "g_customer
    exceptions
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4.

endif.

*- Tool Bar
  perform exclude_of_toolbar_button using 'GT_EXCLUDE'.

*- GRID (Display): Display
  perform display_layout_attribute using gs_layocat.

  perform set_field_catalogs .

*- Edit Event   Event Handler
  perform event_handler_register.

*- Sorting
  perform build_sort_field.

*-- F4 FIELD
  perform set_f4_field.

  g_repid = sy-repid.

*- At least field REPORT of this structure has to be filled!
  alv_variant-report = g_repid.

*- ALV Grid Display
  perform alv_grid_display.

  call method g_grid->set_ready_for_input
    exporting
      i_ready_for_input = 0.


  clear g_title .

*   READ TABLE gt_extype  INTO ls_002
*                         WITH KEY zzcode = w_extype .
*   IF sy-subrc IS INITIAL .
*     CONCATENATE 'test' '[' '#### :'
*     INTO  g_title .
*   ENDIF .

*  CALL METHOD g_grid->set_gridtitle
*    EXPORTING
*      i_gridtitle = g_title.

endform.                    " DISPLAY_ALV_OUPUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_OF_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0495   text
*----------------------------------------------------------------------*
form exclude_of_toolbar_button  using  p_tabname.           "#EC *

  data : l_tab_name like feld-name.

*-?
  field-symbols : <table> type ui_functions.

*-?
  concatenate p_tabname '[]' into  l_tab_name.
  assign     (l_tab_name)    to <table>.

*-
  perform add_exclude_toolbar_button
         tables <table>
        using : cl_gui_alv_grid=>mc_fc_excl_all. " **
*        USING : cl_gui_alv_grid=>mc_fc_loc_undo, "
*                cl_gui_alv_grid=>mc_fc_auf,      "
*                cl_gui_alv_grid=>mc_fc_average,  " &AVERAGE
**                cl_gui_alv_grid=>mc_fc_back_classic,
**                cl_gui_alv_grid=>mc_fc_call_abc, " &ABC
**                cl_gui_alv_grid=>mc_fc_call_chain,
**                cl_gui_alv_grid=>mc_fc_call_crbatch,
**                cl_gui_alv_grid=>mc_fc_call_crweb,
**                cl_gui_alv_grid=>mc_fc_call_lineitems,
**                cl_gui_alv_grid=>mc_fc_call_master_data,
**                cl_gui_alv_grid=>mc_fc_call_more,
**                cl_gui_alv_grid=>mc_fc_call_report,
**                cl_gui_alv_grid=>mc_fc_call_xint,
**                cl_gui_alv_grid=>mc_fc_call_xxl,
**                cl_gui_alv_grid=>mc_fc_col_invisible,
**                cl_gui_alv_grid=>mc_fc_col_optimize,
**                cl_gui_alv_grid=>mc_fc_current_variant,
**                cl_gui_alv_grid=>mc_fc_data_save,
**                cl_gui_alv_grid=>mc_fc_delete_filter,
**                cl_gui_alv_grid=>mc_fc_deselect_all,
**                cl_gui_alv_grid=>mc_fc_detail,
**                cl_gui_alv_grid=>mc_fc_expcrdata,
**                cl_gui_alv_grid=>mc_fc_expcrdesig,
**                cl_gui_alv_grid=>mc_fc_expcrtempl,
**                cl_gui_alv_grid=>mc_fc_expmdb,
**                cl_gui_alv_grid=>mc_fc_extend,
**                cl_gui_alv_grid=>mc_fc_f4,
**                cl_gui_alv_grid=>mc_fc_filter,
**                cl_gui_alv_grid=>mc_fc_find,
**                cl_gui_alv_grid=>mc_fc_fix_columns,
*                cl_gui_alv_grid=>mc_fc_graph,
**                cl_gui_alv_grid=>mc_fc_help,
**                cl_gui_alv_grid=>mc_fc_info,
**                cl_gui_alv_grid=>mc_fc_load_variant,
*                cl_gui_alv_grid=>mc_fc_loc_copy,          "
**                cl_gui_alv_grid=>mc_fc_html,
*                cl_gui_alv_grid=>mc_fc_loc_copy_row,      "
*                cl_gui_alv_grid=>mc_fc_loc_cut,           "
*                cl_gui_alv_grid=>mc_fc_loc_delete_row,    "
*                cl_gui_alv_grid=>mc_fc_loc_insert_row,    "
*                cl_gui_alv_grid=>mc_fc_loc_move_row,
*                cl_gui_alv_grid=>mc_fc_loc_append_row,    "
*                cl_gui_alv_grid=>mc_fc_loc_paste,         "
*                cl_gui_alv_grid=>mc_fc_loc_paste_new_row. "
**                cl_gui_alv_grid=>mc_fc_maintain_variant,
**                cl_gui_alv_grid=>mc_fc_maximum,
**                cl_gui_alv_grid=>mc_fc_minimum,
**                cl_gui_alv_grid=>mc_fc_pc_file,
**                cl_gui_alv_grid=>mc_fc_print,
**                cl_gui_alv_grid=>mc_fc_print_back,
**                cl_gui_alv_grid=>mc_fc_print_prev,
**                cl_gui_alv_grid=>mc_fc_refresh,
**                cl_gui_alv_grid=>mc_fc_reprep,
**                cl_gui_alv_grid=>mc_fc_save_variant,
**                cl_gui_alv_grid=>mc_fc_select_all,
**                cl_gui_alv_grid=>mc_fc_send,
**                cl_gui_alv_grid=>mc_fc_separator,
**                cl_gui_alv_grid=>mc_fc_sort,
**                cl_gui_alv_grid=>mc_fc_sort_asc,
**                cl_gui_alv_grid=>mc_fc_sort_dsc,
**                cl_gui_alv_grid=>mc_fc_subtot,
**                cl_gui_alv_grid=>mc_mb_sum,
**                cl_gui_alv_grid=>mc_fc_sum.
**                cl_gui_alv_grid=>mc_fc_to_office,
**                cl_gui_alv_grid=>mc_fc_to_rep_tree,
**                cl_gui_alv_grid=>mc_fc_unfix_columns,
**                cl_gui_alv_grid=>mc_fc_views,
**                cl_gui_alv_grid=>mc_fc_view_crystal,
**                cl_gui_alv_grid=>mc_fc_view_excel,
**                cl_gui_alv_grid=>mc_fc_view_grid,
**                cl_gui_alv_grid=>mc_fc_word_processor.

endform.                    " EXCLUDE_OF_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*&      Form  ADD_EXCLUDE_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
form add_exclude_toolbar_button  tables   p_table
                                 using    p_value.          "#EC *

  data: l_exclude type ui_func.

  l_exclude = p_value.
  append l_exclude to p_table. "

endform.                    " ADD_EXCLUDE_TOOLBAR_BUTTON
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOGS
*&---------------------------------------------------------------------*
form set_field_catalogs .  "

  clear   gt_fieldcat.

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'SORT' ,
*          ' ' 'OUTPUTLEN'  '1' ,
*          ' ' 'COLTEXT'     'Type',
*          ' ' 'NO_OUT'     'X',
*          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'FONDS' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'Fund',
          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'FISTL' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'FundCtr',
          'E' 'KEY'         'X' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'KOSTL' ,
*          ' ' 'OUTPUTLEN'  '10' ,
*          ' ' 'COLTEXT'     'Cost Ctr',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'FIPEX' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'CommitItm',
          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'HKONT' ,
          ' ' 'OUTPUTLEN'  '8' ,
          ' ' 'COLTEXT'     'G/L Account',
          ' ' 'NO_OUT'      'X',
          'E' 'KEY'         'X' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'AUFNR' ,
*          ' ' 'OUTPUTLEN'  '12' ,
*          ' ' 'COLTEXT'     'Order',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         ' ' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'LIFNR' ,
          ' ' 'OUTPUTLEN'  '10' ,
          ' ' 'COLTEXT'     'Vendor',
          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'CFFLD' ,
          ' ' 'OUTPUTLEN'   '3' ,
          ' ' 'COLTEXT'     'C/F',
          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'DOCTX' ,
          ' ' 'OUTPUTLEN'  '5' ,
          ' ' 'COLTEXT'     'Stat',
          'E' 'KEY'         'X' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'REFBN' ,
*          ' ' 'OUTPUTLEN'  '10' ,
*          ' ' 'COLTEXT'     'Document',
*          ' ' 'HOTSPOT'     'X',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         'X' .
*
*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'RFPOS' ,
*          ' ' 'OUTPUTLEN'  '5' ,
*          ' ' 'COLTEXT'     'Itm',
*          ' ' 'NO_OUT'      'X',
*          'E' 'KEY'         'X' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'FKBTR' ,
          ' ' 'OUTPUTLEN'  '10' ,
          ' ' 'COLTEXT'    'Cmmts/Act',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'FM_ORIG' ,
*          ' ' 'OUTPUTLEN'  '10' ,
*          ' ' 'COLTEXT'    'CI-Org',
*          ' ' 'REF_TABLE'   'FMIOI',
*          ' ' 'REF_FIELD'   'FKBTR',
*          ' ' 'NO_OUT'      'X',
*          ' ' 'DO_SUM'      'X',
*          'E' 'KEY'         '' .
*
  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'PRDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PR Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'PRPOS' ,
          ' ' 'OUTPUTLEN'   '2' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'PRAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PR Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'PODOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PO Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'POPOS' ,
          ' ' 'OUTPUTLEN'   '2' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'POAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'PO Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'GRDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'GR Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'GRPOS' ,
          ' ' 'OUTPUTLEN'   '2' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'GRAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'GR Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'IVDOCS' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'IV Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'IVAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'IV Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'DPDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Downpay Doc',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'DPAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Downpay Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'TFDOC' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Trf Doc',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'TFAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Trf Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'AUGDT' ,
          ' ' 'OUTPUTLEN'   '08' ,
          ' ' 'COLTEXT'     'Pymt Date',
          'E' 'KEY'         '' .

*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'PYDOC' ,
*          ' ' 'OUTPUTLEN'   '10' ,
*          ' ' 'COLTEXT'     'Pymt Doc.',
*          ' ' 'HOTSPOT'     'X',
*          'E' 'KEY'         '' .
*
*  PERFORM fill_field_catalogs USING :
*          'S' 'FIELDNAME'   'PYAMT' ,
*          ' ' 'OUTPUTLEN'   '10' ,
*          ' ' 'COLTEXT'     'Pymt Amt',
*          ' ' 'REF_TABLE'   'FMIOI',
*          ' ' 'REF_FIELD'   'FKBTR',
*          ' ' 'DO_SUM'      'X',
*          'E' 'KEY'         '' .

  perform fill_field_catalogs using :
          'S' 'FIELDNAME'   'BLAMT' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Balance Amt',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          ' ' 'DO_SUM'      'X',
          'E' 'KEY'         '' .


endform.                    " SET_FIELD_CATALOGS

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATALOGS
*&---------------------------------------------------------------------*
form fill_field_catalogs  using  p_gub  p_fname  p_value.

*- 'S' -> Start
*- 'E' -> End

  if p_gub = 'S'.

    clear gs_fieldcat.

  endif.

*-
  data l_fname(40).
  field-symbols <fs> type any.
  concatenate 'GS_FIELDCAT-' p_fname into l_fname.

  assign (l_fname) to <fs>.
  <fs> = p_value.

  if p_gub = 'E'.

    append gs_fieldcat to gt_fieldcat.

  endif.

endform.                    " FILL_FIELD_CATALOGS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LAYOUT_ATTRIBUTE
*&---------------------------------------------------------------------*
form display_layout_attribute  using  p_layocat type lvc_s_layo.

*- General display options
*  p_layocat-cwidth_opt = 'X'.
  p_layocat-sel_mode   = 'A'.
*  p_layocat-edit       = C_X .
  p_layocat-smalltitle  = 'X' .
  p_layocat-stylefname = 'CELLTAB'.
*  P_LAYOCAT-CTAB_FNAME = 'F_COL'.


*  p_layocat-excp_fname = 'CHK' .
*  p_layocat-excp_led   = 'X' .


endform.                    " DISPLAY_LAYOUT_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
form alv_grid_display .
*-- Display
  call method g_grid->set_table_for_first_display
       exporting is_layout            = gs_layocat
*                 it_toolbar_excluding = gt_exclude
                 i_save               = 'A'   "
*                                       'U'
*                                       'X'   "
*                                       ' '   "
*                i_default            = 'X'   "
*  LAYOUT   Standard   BCALV_GRID_09  ##.
                 is_variant           = alv_variant  " ## ## display
       changing  it_outtab            = gt_out[]
                 it_sort              = gt_sort
                 it_fieldcatalog      = gt_fieldcat[].

endform.                    " ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT_FIELD
*&---------------------------------------------------------------------*
form build_sort_field .

** Sort FIELD SETTING
  gs_sort-fieldname = 'DOCTX'.
  gs_sort-spos      = '1'.
  gs_sort-up        = 'X'.
  gs_sort-subtot    = 'X'.
  append gs_sort to gt_sort.

endform.                    " build_sort_field
*&---------------------------------------------------------------------*
*&      Form  event_handler_register
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form event_handler_register .

*- Edit  REGISTER EVENT
* IF MC_EVT_MODIFY
  call method g_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*- Event Handler
  create object g_event_handler.

  set handler g_event_handler->handle_toolbar            for g_grid.
  set handler g_event_handler->handle_user_command       for g_grid.
  set handler g_event_handler->handle_after_user_command for g_grid.
  set handler g_event_handler->handle_onf4               for g_grid.
  set handler g_event_handler->handle_data_changed       for g_grid.
  set handler g_event_handler->handle_data_changed_finished
                                                         for g_grid.
  set handler g_event_handler->handle_hotspot_click      for g_grid.
endform.                    " event_handler_register
*&---------------------------------------------------------------------*
*&      Form  refresh_table_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_table_display .

  l_scroll-row = 'X'.
  l_scroll-col = 'X'.

  call method g_grid->refresh_table_display
    exporting
      i_soft_refresh = ' '
      is_stable      = l_scroll.     "refresh

*  CALL METHOD g_grid->refresh_table_display
*    EXPORTING
*      i_soft_refresh = c_x.

*  CALL METHOD g_grid->set_toolbar_buttons2( ) .

endform.                    " refresh_table_display
*&---------------------------------------------------------------------*
*&      Form  SET_F4_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_f4_field .

  refresh gt_f4 .
*-- register F4 FIELD
**-- [Caution] have to register fieldname ascending.

*   gs_f4-fieldname  = '*****'.
*   gs_f4-register   = 'X'.
*   APPEND gs_f4 TO gt_f4.
*
*   gs_f4-fieldname  = '*****'.
*   gs_f4-register   = 'X'.
*   APPEND gs_f4 TO gt_f4.
*

  call method g_grid->register_f4_for_fields
    exporting
      it_f4 = gt_f4.


endform.                    " SET_F4_FIELD
*&---------------------------------------------------------------------*
*&      Form  GET_BUDGET_PERIOD
*&---------------------------------------------------------------------*
form get_budget_period using    f_fictr f_fipos f_geber f_gjahr
                       changing f_profil .

  read table it_fmci  with key fipos = f_fipos.
  if sy-subrc = 0.
    perform determine_profile_fs using    p_fikrs
                                          f_fictr
                                          it_fmci-posit
                                          f_geber
                                          f_gjahr
                                 changing f_profil.
  else.
    f_profil = ' '.
  endif.

endform.                    " get_budget_period
*&---------------------------------------------------------------------*
*&      Form  determine_profile_fs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FIK  text
*      -->P_F_FICTR  text
*      -->P_IT_fmci_POSIT  text
*      -->P_F_GEBER  text
*      -->P_P_GJR  text
*      <--P_F_PROFIL  text
*----------------------------------------------------------------------*
form determine_profile_fs using    l_fikrs
                                   l_fictr
                                   l_posit
                                   l_geber
                                   l_gjahr
                          changing l_bprof.
  data: l_objnr like fmfctr-ctr_objnr.
  data: l_farea like  bpja-farea.


  l_objnr(2) = 'FS'.
  l_objnr+2(4) = l_fikrs.
  l_objnr+6  = l_fictr.

* Profile from TBPFM table.
  call function 'KBPA_FIFM_GET_PROFIL'
    exporting
      i_objnr         = l_objnr
      i_posit         = l_posit
      i_geber         = l_geber
      i_gjahr         = l_gjahr
      i_farea         = l_farea
    importing
      e_profil        = l_bprof
    exceptions
      no_profil_found = 01.

  if not sy-subrc is initial.
*   Profile from FundMgt Area
    call function 'FM5B_GET_PROFILE'
      exporting
        i_fikrs           = l_fikrs
        i_fincode         = l_geber
      importing
        e_profil          = l_bprof
      exceptions
        fm_area_not_found = 1
        others            = 2.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  endif.

endform.                    " GET_BUDGET_PERIOD
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
form hotspot_click  using    p_row
                             p_column
                             ps_row_no.
  data: ls_out   type ty_output,
        ls_inv   type ty_output,
        ls_dnp   type ty_output,
        ls_trf   type ty_output,
        ls_gr    type ty_gr.

  data: l_num(11) type c value ' 0123456789'.

  case p_column.
*    WHEN 'REFBN'.
*
*      REFRESH it_detail2.
*
*      READ TABLE gt_out INDEX p_row into ls_out.
*
*      CHECK ls_out-podoc IS NOT INITIAL.
*
*      IF ls_out-doctx = 'PR'.
*        SET PARAMETER ID: 'BAN' FIELD ls_out-prdoc.
*        CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
*      ELSEIF ls_out-doctx = 'PO'.
*        SET PARAMETER ID: 'BES' FIELD ls_out-podoc.
*        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*      ELSEIF ls_out-doctx = 'IV'.
*        SET PARAMETER ID: 'BLN' FIELD ls_out-ivdocs,
*                          'BUK' FIELD p_fikrs,
*                          'GJR' FIELD ls_out-ivghr.
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*      ENDIF.

    when 'PRDOC'.
** On 09/05/13
      check p_row co l_num.
** end on 09/05/13
      read table gt_out index p_row into ls_out.
      check ls_out-prdoc is not initial.

      set parameter id: 'BAN' field ls_out-prdoc.
      call transaction 'ME53N' and skip first screen.

    when 'PODOC'.
** On 09/05/13
      check p_row co l_num.
** end on 09/05/13
      refresh it_detail2.

      read table gt_out index p_row into ls_out.
      check ls_out-podoc is not initial.

*      IF ls_out-podoc+0(1) = '*'.
*        LOOP AT it_detail WHERE doctx = 'PO'
*                            AND refbn = ls_out-podoc
*                            AND rfpos = ls_out-prpos.
*          APPEND it_detail TO it_detail2.
*        ENDLOOP.
*
*        CALL SCREEN 0200 STARTING AT 10 10
*                         ENDING   AT 80 20.
*      ELSE.
      set parameter id: 'BES' field ls_out-podoc.
      call transaction 'ME23N' and skip first screen.
*      ENDIF.

    when 'GRDOC'.
** On 09/05/13
      check p_row co l_num.
** end on 09/05/13
      refresh it_detail2.

      read table gt_out index p_row into ls_out.

      check ls_out-grdoc is not initial.

      if ls_out-grdoc+0(1) = '*'.
        loop at gt_gr into ls_gr
                      where ebeln = ls_out-podoc
                        and ebelp = ls_out-popos.
          it_detail2-refbn = ls_gr-ebeln.
          it_detail2-rfpos = ls_gr-ebelp.
          it_detail2-doctx = 'GR'.
          it_detail2-gjahr = ls_gr-gjahr.
          it_detail2-docnr = ls_gr-belnr.
          it_detail2-docid = ls_gr-buzei.
          it_detail2-amtxx = ls_gr-wrbtr.

          append it_detail2.
        endloop.

        call screen 0200 starting at 10 10
                         ending   at 80 20.
      else.
        set parameter id: 'MBN' field ls_out-grdoc.
        set parameter id: 'MJA' field ls_out-grghr.
        call transaction 'MB03' and skip first screen.
      endif.

    when 'IVDOCS'.
** On 09/05/13
      check p_row co l_num.
** end on 09/05/13
      refresh it_detail2.

      read table gt_out index p_row into ls_out.
      check ls_out-ivdocs is not initial.

      if ls_out-ivdocs+0(1) = '*'.

        loop at gt_inv into ls_inv
           where podoc = ls_out-podoc
             and popos = ls_out-popos
             and prdoc = ls_out-prdoc
             and prpos = ls_out-prpos
             and fonds = ls_out-fonds
             and fistl = ls_out-fistl
             and fipex = ls_out-fipex
             and hkont = ls_out-hkont
             and lifnr = ls_out-lifnr.

          it_detail2-doctx = 'IV'.
          it_detail2-gjahr = ls_inv-ivghr.
          it_detail2-docnr = ls_inv-ivdocs.
          it_detail2-amtxx = ls_inv-ivamt.

          append it_detail2.
        endloop.

        call screen 0200 starting at 10 10
                         ending   at 70 20.
      else.
        set parameter id: 'BLN' field ls_out-ivdocs,
                          'BUK' field  p_fikrs,
                          'GJR' field ls_out-ivghr.
        call transaction 'FB03' and skip first screen.
      endif.

    when 'DPDOC'.
** On 09/05/13
      check p_row co l_num.
** end on 09/05/13
      refresh it_detail2.

      read table gt_out index p_row into ls_out.
      check ls_out-dpdoc is not initial.

      if ls_out-dpdoc+0(1) = '*'.
        loop at gt_dnp into ls_dnp
           where podoc = ls_out-podoc
             and popos = ls_out-popos
             and prdoc = ls_out-prdoc
             and prpos = ls_out-prpos
             and fonds = ls_out-fonds
             and fistl = ls_out-fistl
             and fipex = ls_out-fipex
             and hkont = ls_out-hkont
             and lifnr = ls_out-lifnr.

          it_detail2-doctx = 'IV'.
          it_detail2-gjahr = ls_dnp-dpghr.
          it_detail2-docnr = ls_dnp-dpdoc.
          it_detail2-amtxx = ls_dnp-dpamt.

          append it_detail2.
        endloop.

        call screen 0200 starting at 10 10
                         ending   at 80 20.
      else.
        set parameter id: 'BLN' field ls_out-dpdoc,
                          'BUK' field  p_fikrs,
                          'GJR' field ls_out-dpghr.
        call transaction 'FB03' and skip first screen.
      endif.

    when 'TFDOC'.
** On 09/05/13
      check p_row co l_num.
** end on 09/05/13
      refresh it_detail2.

      read table gt_out index p_row into ls_out.
      check ls_out-tfdoc is not initial.

      if ls_out-tfdoc+0(1) = '*'.
        loop at gt_trf into ls_trf
           where podoc = ls_out-podoc
             and popos = ls_out-popos
             and prdoc = ls_out-prdoc
             and prpos = ls_out-prpos
             and fonds = ls_out-fonds
             and fistl = ls_out-fistl
             and fipex = ls_out-fipex
             and hkont = ls_out-hkont.
*             and lifnr = ls_out-lifnr.

          it_detail2-doctx = 'TF'.
          it_detail2-gjahr = ls_dnp-tfghr.
          it_detail2-docnr = ls_dnp-tfdoc.
          it_detail2-amtxx = ls_dnp-tfamt.

          append it_detail2.
        endloop.

        call screen 0200 starting at 10 10
                         ending   at 80 20.
      else.
        set parameter id: 'BLN' field ls_out-tfdoc,
                          'BUK' field  p_fikrs,
                          'GJR' field ls_out-tfghr.
        call transaction 'FB03' and skip first screen.
      endif.


*** --> Screen 0200 command
    when 'DOCNR'.

      read table it_detail2 index p_row.

      case it_detail2-doctx.
        when 'IV' or 'DP' or 'TF' or 'P2'.
          set parameter id: 'BLN' field it_detail2-docnr,
                            'BUK' field  p_fikrs,
                            'GJR' field it_detail2-gjahr.
          call transaction 'FB03' and skip first screen.
        when 'PO'.
          set parameter id: 'BES' field it_detail2-docnr.
          call transaction 'ME23N' and skip first screen.
        when 'GR'.
          set parameter id: 'MBN' field it_detail2-docnr.
          set parameter id: 'MJA' field it_detail2-gjahr.
          call transaction 'MB03' and skip first screen.
        when others.
      endcase.

  endcase.

endform.                    " HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_OUPUT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_alv_ouput_0200 .

  if g_customer2 is initial.

    create object g_customer2
      exporting
        container_name              = c_container
      exceptions
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    if sy-subrc ne 0 .

    endif.

*   create an instance of alv control
    create object g_grid2
      exporting
        i_parent          = g_customer2
*       i_appl_events     = 'X'
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4.

    if sy-subrc <> 0.
      message a004 with 'Screen Initialization Error' .
    endif.

  endif.

  clear : gt_fieldcat2[].
*- Tool Bar
  perform exclude_of_toolbar_button2 using 'GT_EXCLUDE2'.

*- GRID (Display): Display
  perform display_layout_attribute2 using gs_layocat2.

  perform set_field_catalogs2 .

*- Edit Event   Event Handler
  perform event_handler_register2.

*- Sorting
*  PERFORM build_sort_field.

  g_repid2 = sy-repid.

*- At least field REPORT of this structure has to be filled!
  alv_variant2-report = g_repid2.

*- ALV Grid Display
  perform alv_grid_display2.

  call method g_grid2->set_ready_for_input
    exporting
      i_ready_for_input = 0.


  clear g_title2 .

*   READ TABLE gt_extype  INTO ls_002
*                         WITH KEY zzcode = w_extype .
*   IF sy-subrc IS INITIAL .
*     CONCATENATE g_title '[' '#### :'    ls_002-zzcdname ']'
*     INTO  g_title .
*   ENDIF .

  call method g_grid2->set_gridtitle
    exporting
      i_gridtitle = g_title2.

endform.                    " DISPLAY_ALV_OUPUT_0200
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_OF_TOOLBAR_BUTTON2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2786   text
*----------------------------------------------------------------------*
form exclude_of_toolbar_button2  using  p_tabname.

  data : l_tab_name like feld-name.

*-?
  field-symbols : <table> type ui_functions.

*-?
  concatenate p_tabname '[]' into  l_tab_name.
  assign     (l_tab_name)    to <table>.

*-
  perform add_exclude_toolbar_button
         tables <table>
        using : cl_gui_alv_grid=>mc_fc_excl_all. " **

endform.                    " EXCLUDE_OF_TOOLBAR_BUTTON2
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LAYOUT_ATTRIBUTE2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GS_LAYOCAT2  text
*----------------------------------------------------------------------*
form display_layout_attribute2  using  p_layocat type lvc_s_layo.

*- General display options
  p_layocat-cwidth_opt = 'X'.
  p_layocat-sel_mode   = 'A'.
*  p_layocat-edit       = C_X .
  p_layocat-smalltitle  = 'X' .
*  p_layocat-stylefname = 'CELLTAB'.
*  P_LAYOCAT-CTAB_FNAME = 'F_COL'.


*  p_layocat-excp_fname = 'CHK' .
*  p_layocat-excp_led   = 'X' .

endform.                    " DISPLAY_LAYOUT_ATTRIBUTE2
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_catalogs2 .

  clear   gt_fieldcat2.

  perform fill_field_catalogs2 using :
          'S' 'FIELDNAME'   'DOCTX' ,
          ' ' 'OUTPUTLEN'  '2' ,
          ' ' 'COLTEXT'     'Doc.Category',
          'E' 'KEY'         '' .

  perform fill_field_catalogs2 using :
          'S' 'FIELDNAME'   'GJAHR' ,
          ' ' 'OUTPUTLEN'  '4' ,
          ' ' 'COLTEXT'     'Fiscal Year',
          'E' 'KEY'         '' .

  perform fill_field_catalogs2 using :
          'S' 'FIELDNAME'   'DOCNR' ,
          ' ' 'OUTPUTLEN'  '10' ,
          ' ' 'COLTEXT'     'Document',
          ' ' 'HOTSPOT'     'X',
          'E' 'KEY'         '' .

  perform fill_field_catalogs2 using :
          'S' 'FIELDNAME'   'DOCID' ,
          ' ' 'OUTPUTLEN'  '5' ,
          ' ' 'COLTEXT'     'Itm',
          'E' 'KEY'         '' .

  perform fill_field_catalogs2 using :
          'S' 'FIELDNAME'   'AMTXX' ,
          ' ' 'OUTPUTLEN'   '10' ,
          ' ' 'COLTEXT'     'Amount',
          ' ' 'REF_TABLE'   'FMIOI',
          ' ' 'REF_FIELD'   'FKBTR',
          'E' 'KEY'         '' .

endform.                    " SET_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
form fill_field_catalogs2  using  p_gub  p_fname  p_value.

*- 'S' -> Start
*- 'E' -> End

  if p_gub = 'S'.

    clear gs_fieldcat2.

  endif.

*-
  data l_fname(40).
  field-symbols <fs> type any.
  concatenate 'GS_FIELDCAT2-' p_fname into l_fname.

  assign (l_fname) to <fs>.
  <fs> = p_value.

  if p_gub = 'E'.

    append gs_fieldcat2 to gt_fieldcat2.

  endif.

endform.                    " FILL_FIELD_CATALOGS2
*&---------------------------------------------------------------------*
*&      Form  EVENT_HANDLER_REGISTER2
*&---------------------------------------------------------------------*
form event_handler_register2 .
*- Edit  REGISTER EVENT
* IF MC_EVT_MODIFY
  call method g_grid2->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

*- Event Handler
  create object g_event_handler2.

  set handler g_event_handler2->handle_hotspot_click      for g_grid2.

endform.                    " EVENT_HANDLER_REGISTER2
*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY2
*&---------------------------------------------------------------------*
form alv_grid_display2 .
*-- Display
  call method g_grid2->set_table_for_first_display
       exporting
                 is_layout            = gs_layocat2
                 it_toolbar_excluding = gt_exclude2
                 i_save               = 'A'   "
*                                       'U'
*                                       'X'   "
*                                       ' '   "
*                i_default            = 'X'   "
*                is_variant           = alv_variant2  " ## ## display
       changing  it_outtab            = it_detail2[]
                 it_sort              = gt_sort2
                 it_fieldcatalog      = gt_fieldcat2[].

endform.                    " ALV_GRID_DISPLAY2
*&---------------------------------------------------------------------*
*&      Form  HEADER
*&---------------------------------------------------------------------*
form header .

  data: l_document type ref to cl_dd_document,
        l_doctable type ref to cl_dd_table_element,
        l_column1  type ref to cl_dd_area,
        l_column2  type ref to cl_dd_area.

  create object l_document.
*  CALL METHOD l_document->add_text
*    EXPORTING
*      text      = 'Commitment/Actual Line Items for Assigned Annual Budget'
*      sap_style = cl_dd_area=>small. "heading.

  call method l_document->add_table
    exporting
      no_of_columns               = 1
      cell_background_transparent = 'X'
      border                      = '0'
    importing
      table                       = l_doctable.
*
  call method l_doctable->add_column
    importing
      column = l_column1.

    perform titles changing l_column1.

  call method l_doctable->add_column
    importing
      column = l_column2.

  call method l_column2->add_gap
    exporting
      width = 150.

*  CALL METHOD l_column2->add_picture
*    EXPORTING
*      picture_id = 'ZPICTURE'. "WHAT EVER GRAPHIC YOU NEED

  call method l_document->merge_document.
  call method l_document->display_document
    exporting
      parent = g_container_1.

endform.                    " HEADER
*&---------------------------------------------------------------------*
*&      Form  TITLES
*&---------------------------------------------------------------------*
form titles changing dg_dyndoc_id type ref to cl_dd_area.

  data : dl_text(255) type c.  "Text
  data : l_bezei type FM_BEZEICH.

*  dl_text = sy-title.
*
*
*  CONDENSE dl_text.
*  CALL METHOD dg_dyndoc_id->add_text
*    EXPORTING
*      text         = dl_text
*      sap_fontsize = cl_dd_area=>large.
*
*  CALL METHOD dg_dyndoc_id->new_line.
*  CLEAR : dl_text.

*  CALL METHOD dg_dyndoc_id->new_line.

  dl_text = 'Funds Center :'.

  call method dg_dyndoc_id->add_gap.

  call method dg_dyndoc_id->add_text
    exporting
      text = dl_text.

  clear dl_text.

  select single BEZEICH
    into l_bezei
    from fmfctrt
   where spras = 'E'
     and fikrs =  p_fikrs
     and fictr = s_fistl-low.

  concatenate s_fistl-low ' - ' l_bezei into dl_text separated by space.

  call method dg_dyndoc_id->add_text
    exporting
      text = dl_text.

  call method dg_dyndoc_id->new_line.

  clear : dl_text.

  dl_text = 'Commitment Item :'.

  call method dg_dyndoc_id->add_gap.

  call method dg_dyndoc_id->add_text
    exporting
      text = dl_text.

  clear dl_text.

  read table s_fipex index 1.

  select single bezei into l_bezei
    from fmcit
   where spras = 'E'
     and fikrs =  p_fikrs
     and fipex = s_fipex-low.

  concatenate s_fipex-low ' - ' l_bezei into dl_text  separated by space.

  call method dg_dyndoc_id->add_text
    exporting
      text = dl_text.

  call method dg_dyndoc_id->new_line.

  dl_text = 'Cmmt/Actual : '.

  call method dg_dyndoc_id->add_gap.

  call method dg_dyndoc_id->add_text
    exporting
      text = dl_text.

  clear dl_text.

  write g_total to dl_text currency 'USD'.

  call method dg_dyndoc_id->add_text
    exporting
      text = dl_text.

endform.                    "TITLES
*&---------------------------------------------------------------------*
*&      Form  GET_DATA2
*&---------------------------------------------------------------------*
form get_data2 .

  refresh: gt_fi.

*FM Actual Line Items from FI
*paid next year... no need to consider...just show clearing document

  select gjahr  perio as poper
         fistl  fonds fipex hkont
         objnrz             "Object number (order)
         awtyp
         btart
         wrttp
         lifnr
         kngjahr
         knbelnr as refbn   "FI
         vrefbt
         vrefbn             "PO
         vrfpos
         sum( fkbtr ) as fkbtr
    into corresponding fields of table gt_fi
    from  v_fmifi
   where fikrs  =  p_fikrs   and
         gjahr  =  p_gjahr   and    "ANDY
         btart  =  '0100'    and    " ANDY , '0350' original only
         fonds  in s_fonds   and
         fistl  in s_fistl   and
         fipex  in s_fipex   and
         ( vrefbn in s_refbn   or fmbelnr in s_refbn ) and
         perio  in s_perio   and
*         zhldt  IN s_zhldt   AND
         fkbtr <> 0
  group by gjahr perio fistl fonds fipex hkont
         objnrz awtyp btart wrttp
         lifnr vrefbt vrefbn vrfpos
         kngjahr knbelnr
%_hints oracle 'FIRST_ROWS(10)'.


endform.                    " GET_DATA2
*&---------------------------------------------------------------------*
*&      Form  PROCESS_COMMITMENT
*&---------------------------------------------------------------------*
form process_commitment.
  data: ls_cmmt   like gt_cmmt.
  data: ls_cmmt_t like gt_cmmt.
  data: ls_out    type ty_output.
  data: ls_pr     type gty_prpo.
  data: ls_po     type gty_prpo.

  data: lt_tab_pr like ls_cmmt occurs 0 with header line.
  data: lt_tab_po like ls_cmmt occurs 0 with header line.

* proces PO, PR sequence
  sort gt_cmmt by refbt descending
                  refbn rfpos btart.

** : seperate PR/PO
  data: lv_idx    like sy-tabix.
  data: lv_sorted type char1.
  clear lv_sorted.
  loop at gt_cmmt into ls_cmmt.
    lv_idx = sy-tabix.
*    CHECK ls_cmmt-btart IN r_btart_o.   "original commitment only

    clear ls_out.

    move-corresponding ls_cmmt to ls_out.
    ls_out-fkbtr   = ls_cmmt-fkbtr * -1 .

*-- need to summarize
    clear: ls_out-refbt, ls_out-refbn, ls_out-rfpos.

*purchase request only w/o PO
    if ls_cmmt-refbt = '010'.
      if lv_sorted = space.
        sort gt_pr by refbn rfpos.
      endif.
      read table gt_pr with key refbn = ls_cmmt-refbn
                                rfpos = ls_cmmt-rfpos binary search
                            transporting no fields.
      if sy-subrc = 0.  "processed already
      else.
*        ls_out-doctx = 'PR'. "later...
*        ls_out-prghr = ls_cmmt-gjahr.
        ls_out-prdoc = ls_cmmt-refbn.
        ls_out-prpos = ls_cmmt-rfpos.

        if ls_cmmt-btart in r_btart_o.  "original
          move-corresponding ls_cmmt to ls_pr.
          ls_pr-fkbtr = ls_cmmt-fkbtr.
          append ls_pr to gt_pr.       "merge later due to collect
        endif.

      endif.

*"purchase order
    elseif ls_cmmt-refbt = '020'.

*---consider c/f only for PO... (no meaning PR...)
      if ls_cmmt-btart = '0300'. "c/f
        ls_out-cffld = 'X'.
      else.
        ls_out-cffld = ''.
      endif.

*      ls_out-doctx = 'PO'. " later...

      ls_out-podoc = ls_cmmt-refbn.
      ls_out-popos = ls_cmmt-rfpos.
*      ls_out-poghr = ls_cmmt-gjahr.

      if ls_cmmt-btart in r_btart_o.  "original
        move-corresponding ls_cmmt to ls_po.
*        ls_po-vrfghr = ls_cmmt-gjahr.
        append ls_po to gt_po.       "merge later due to collect
      endif.


      if ls_cmmt-vrefbn = space. " PO w/o PR

      else.   "find reference PR

        ls_out-prdoc = ls_cmmt-vrefbn.
        ls_out-prpos = ls_cmmt-vrfpos.
*        ls_out-prghr = ls_cmmt-gjahr.  "FIXME later

        clear ls_pr.
        read table gt_cmmt into ls_cmmt_t
              with key refbt = '010'
                       refbn = ls_cmmt-vrefbn
                       rfpos = ls_cmmt-vrfpos
                       btart = '0100'     "original
                   binary search.
        if sy-subrc <> 0.   "if failed, search c/f PR
          read table gt_cmmt into ls_cmmt_t
                with key refbt = '010'
                         refbn = ls_cmmt-vrefbn
                         rfpos = ls_cmmt-vrfpos
                         btart = '0350'     "c/f
                     binary search.
        endif.
        if sy-subrc <> 0.  "if not found, search table
          select single refbn rfpos gjahr fkbtr into corresponding fields of ls_cmmt_t
            from  fmioi
           where refbn  = ls_cmmt-vrefbn
             and refbt  = '010'          "FIXME what about PR change???
*            AND RFORG  = ls_cmmt-VRFORG
             and rfpos  = ls_cmmt-vrfpos
             and btart  in r_btart_o.                       " '0100'
        endif.

*----    fill PR data
        if sy-subrc = 0 and ls_cmmt-btart in r_btart_o.  "original
          read table gt_pr with key refbn = ls_cmmt-vrefbn
                                    rfpos = ls_cmmt-rfpos binary search
                                transporting no fields.

          if sy-subrc <> 0.
            ls_pr-refbn = ls_cmmt-vrefbn.
            ls_pr-rfpos = ls_cmmt-vrfpos.
*            ls_pr-gjahr = ls_cmmt_t-gjahr.
            ls_pr-fkbtr  = ls_cmmt_t-fkbtr.
            ls_pr-vrefbn = ls_cmmt-refbn.
            ls_pr-vrfpos = ls_cmmt-rfpos.
            append ls_pr to gt_pr.  "to skip PR processing
          endif.
        endif.


      endif.

    endif.

*now add to master table with accumulation of commitment total
    ls_out-blamt  = ls_cmmt-fkbtr * -1 .
    collect ls_out into gt_out.

  endloop.


endform.                    " PROCESS_COMMITMENT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_FIDOCS
*&---------------------------------------------------------------------*
form process_fidocs .
  data: ls_po type gty_prpo,
        ls_pr type gty_prpo.
  data: begin of ls_ekkn,
          brtwr like ekpo-brtwr,
          aedat like ekpo-aedat,
          banfn like ekpo-banfn,
          bnfpo like ekpo-bnfpo,
          hkont like ekkn-sakto,
        end of ls_ekkn.

  data : ls_out    type ty_output.

  sort gt_pr by vrefbn vrfpos.

* park/invoice (no downpay ???  61)
  loop at gt_fi.
    clear : ls_out.
    move-corresponding gt_fi to ls_out.

*-- without PO reference
    if gt_fi-vrefbn = space.
      if p_dopen = 'X'. continue. endif.     "open item only display
*---  performance issue for commitment, show in detail
*      IF gt_fi-objnrz IS NOT INITIAL.
*        ls_out-aufnr = gt_fi-objnrz+2(12).
*      ENDIF.

*-- with PO reference
    else.

*---  check PO reading is done or not...
      read table gt_po into ls_po with key refbn = gt_fi-vrefbn
                                           rfpos = gt_fi-vrfpos
                                  binary search.
      if sy-subrc <> 0.
        if p_dopen = 'X'. continue. endif.     "open item only display

        select single brtwr ekpo~aedat banfn bnfpo sakto into ls_ekkn
           from ekpo
           inner join ekkn on ekkn~ebeln = ekpo~ebeln and ekkn~ebelp = ekpo~ebelp
           where ekpo~ebeln = gt_fi-vrefbn
             and ekpo~ebelp = gt_fi-vrfpos.

        if sy-subrc = 0.
          ls_po-vrefbn = ls_ekkn-banfn.
          ls_po-vrfpos = ls_ekkn-bnfpo.
*          ls_po-vrfghr = ls_ekkn-aedat(4). "year

          ls_po-hkont  = ls_ekkn-bnfpo.

*          ls_po-gjahr  = ls_ekkn-aedat(4). "year
          ls_po-refbn = gt_fi-vrefbn.
          ls_po-rfpos = gt_fi-vrfpos.
          ls_po-fkbtr = - ls_po-fkbtr.  "reverse sign in FM.
          append ls_po to gt_po.
          sort gt_po by refbn rfpos.
        else.
*          BREAK-POINT. "critical problem...
          " PO WAS ARCHIVED?????????????????
        endif.
      endif.

*      ls_out-poghr = ls_po-gjahr.
      ls_out-podoc = ls_po-refbn.
      ls_out-popos = ls_po-rfpos.

* check if PR exist
*      ls_out-prghr = ls_po-gjahr.
      ls_out-prdoc = ls_po-vrefbn.
      ls_out-prpos = ls_po-vrfpos.

* redetermine G/L account, since FM-HKONT is wrong
      ls_out-hkont = ls_po-hkont.
    endif.

*-- need to summarize
    clear: ls_out-refbt, ls_out-refbn, ls_out-rfpos.

    if gt_fi-wrttp eq '61'. "If downpayment
      ls_out-dpamt  = - gt_fi-fkbtr. "for budget checking in the system
      ls_out-dpcnt = 1.
    elseif gt_fi-wrttp eq '54'. "invoice
      ls_out-fkbtr  = - gt_fi-fkbtr. "for budget checking in the system
      ls_out-ivamt  = - gt_fi-fkbtr.   "invoice = normal iv + downpayment
      ls_out-ivcnt = 1.
    else.                    "transfer posting
      ls_out-fkbtr  = - gt_fi-fkbtr. "for budget checking in the system
      ls_out-tfamt  = - gt_fi-fkbtr.   "invoice = normal iv + downpayment
      ls_out-tfcnt = 1.
    endif.
    ls_out-blamt  = - gt_fi-fkbtr.   "for total balance of spending

*ISSUE: if AP vendor is different from PO vendor... FIXME

    collect ls_out into gt_out.

    if gt_fi-objnrz(2) = 'OR'.
      ls_out-aufnr = gt_fi-objnrz+2(12).
    endif.
    if gt_fi-wrttp eq '61'. "Downpayment
      ls_out-dpghr  = gt_fi-kngjahr.
      ls_out-dpdoc  = gt_fi-refbn.
      append ls_out to gt_dnp.
    elseif gt_fi-wrttp eq '54'. "invoice
      ls_out-ivghr   = gt_fi-kngjahr.
      ls_out-ivdoc   = gt_fi-refbn.  "for BSAK select purpose
      ls_out-ivdocs  = gt_fi-refbn.
      append ls_out to gt_inv.
    else.                   "transfer posting
      ls_out-tfghr  = gt_fi-kngjahr.
      ls_out-tfdoc  = gt_fi-refbn.
      append ls_out to gt_trf.
    endif.

  endloop.


endform.                    " PROCESS_FIDOCS
*&---------------------------------------------------------------------*
*&      Form  MERGE_TO_OUT
*&---------------------------------------------------------------------*
form merge_to_out .
  data : ls_out    type ty_output,
         ls_inv    type ty_output, ls_dnp type ty_output, ls_trf  type ty_output.

  data: ls_check  like gt_cmmt.
  data: ls_check2 like gt_cmmt.
  data: lt_tab_pr like gt_cmmt occurs 0 with header line.
  data: lt_tab_po like gt_cmmt occurs 0 with header line.

  data : lt_fi     like gt_cmmt occurs 0 with header line.
  data : l_cnt type i.

  sort gt_inv by podoc popos prdoc prpos
                 fonds fistl fipex hkont aufnr lifnr.
  sort gt_dnp by podoc popos prdoc prpos
                 fonds fistl fipex hkont aufnr lifnr.
  sort gt_trf by podoc popos prdoc prpos
                 fonds fistl fipex hkont aufnr lifnr.

  loop at gt_out into ls_out.
    check ls_out-ivamt ne 0 or ls_out-dpamt ne 0 or ls_out-tfamt ne 0.
    $ix = sy-tabix.

    read table gt_inv into ls_inv with key
             podoc = ls_out-podoc
             popos = ls_out-popos
             prdoc = ls_out-prdoc
             prpos = ls_out-prpos
             fonds = ls_out-fonds
             fistl = ls_out-fistl
             fipex = ls_out-fipex
             hkont = ls_out-hkont
             lifnr = ls_out-lifnr binary search.
    read table gt_dnp into ls_dnp with key
             podoc = ls_out-podoc
             popos = ls_out-popos
             prdoc = ls_out-prdoc
             prpos = ls_out-prpos
             fonds = ls_out-fonds
             fistl = ls_out-fistl
             fipex = ls_out-fipex
             hkont = ls_out-hkont
             lifnr = ls_out-lifnr binary search.
    read table gt_trf into ls_trf with key
             podoc = ls_out-podoc
             popos = ls_out-popos
             prdoc = ls_out-prdoc
             prpos = ls_out-prpos
             fonds = ls_out-fonds
             fistl = ls_out-fistl
             fipex = ls_out-fipex
             hkont = ls_out-hkont
              binary search.

    if ls_out-ivcnt = 1.
      move ls_inv-ivdocs to ls_out-ivdocs.
      move ls_inv-ivghr to ls_out-ivghr.
    elseif ls_out-ivcnt > 1.
      concatenate '*' ls_inv-ivdocs into ls_out-ivdocs.
      move ls_inv-ivghr to ls_out-ivghr.
    endif.

    if ls_out-dpcnt = 1.
      move ls_dnp-dpdoc to ls_out-dpdoc.
      move ls_dnp-dpghr to ls_out-dpghr.
    elseif ls_out-dpcnt > 1.
      concatenate '*' ls_inv-dpdoc into ls_out-dpdoc.
      move ls_dnp-dpghr to ls_out-dpghr.
    endif.
    if ls_out-tfcnt = 1.
      move ls_trf-tfdoc to ls_out-tfdoc.
      move ls_trf-tfghr to ls_out-tfghr.
    elseif ls_out-tfcnt > 1.
      concatenate '*' ls_trf-tfdoc into ls_out-tfdoc.
      move ls_trf-tfghr to ls_out-tfghr.
    endif.

    modify gt_out index $ix from ls_out transporting
        ivdocs ivghr dpdoc dpghr tfdoc tfghr.

  endloop.


endform.                    " MERGE_TO_OUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_GR
*&---------------------------------------------------------------------*
form get_data_gr .
  data : ls_out    type ty_output,
         lt_gr     type table of ty_gr,
         ls_gr     type ty_gr,
         ls_pr     type gty_prpo,
         ls_po     type gty_prpo.

  data: lv_idx like sy-tabix.

  sort gt_pr by refbn rfpos.

  loop at gt_out into ls_out.
    lv_idx = sy-tabix.

* PO is used for IV, DP, other -> consider only IV, open commitment
    if ls_out-podoc ne space and
      ( ls_out-ivamt > 0 or ls_out-fkbtr ne 0 ).
      refresh lt_gr.
      select ekbe~ebeln ekbe~ebelp
             gjahr belnr buzei bwart budat shkzg
             ekbe~menge ekbe~wrbtr
             ekpo~netpr ekpo~peinh
        into table lt_gr
        from ekbe
        inner join ekpo
           on ekbe~ebeln = ekpo~ebeln
          and ekbe~ebelp = ekpo~ebelp
       where ekbe~ebeln = ls_out-podoc and
             ekbe~ebelp = ls_out-popos and
             ekbe~vgabe = '1'.

      ls_out-grcnt = sy-dbcnt.
      loop at lt_gr into ls_gr.
        if ls_gr-wrbtr = 0.  "if GR non valued...
          ls_gr-wrbtr = ls_gr-menge * ls_gr-netpr / ls_gr-peinh.
        endif.
        if ls_gr-shkzg = 'H'.
          ls_gr-wrbtr = - ls_gr-wrbtr.
          ls_gr-menge = - ls_gr-menge.
        endif.
        append ls_gr to gt_gr.
        ls_out-gramt = ls_out-gramt + ls_gr-wrbtr.
      endloop.

* - save back to out
      if ls_out-grcnt = 1.
        move ls_gr-belnr  to ls_out-grdoc.
      elseif ls_out-grcnt > 1.
        concatenate '*' ls_gr-belnr into ls_out-grdoc.
      endif.
      move ls_gr-gjahr to ls_out-grghr.

      modify gt_out index lv_idx from ls_out transporting
          grdoc grghr gramt.
    endif.


* fill back PO amount
    if ls_out-podoc ne space.
      read table gt_po into ls_po
                 with key refbn = ls_out-podoc
                          rfpos = ls_out-popos binary search.
      if sy-subrc = 0.
        ls_out-poamt = - ls_po-fkbtr.
      else.
        select single brtwr as poamt into ls_out-poamt
           from ekpo
           where ebeln = ls_out-podoc
             and ebelp = ls_out-popos.
      endif.
      modify gt_out index lv_idx from ls_out transporting poamt.
    endif.

* fill back PR amount
    if ls_out-prdoc ne space.
      read table gt_pr into ls_pr
                 with key refbn = ls_out-prdoc
                          rfpos = ls_out-prpos binary search.
      if sy-subrc = 0.
        ls_out-pramt = - ls_pr-fkbtr.
      else.
        select single * from eban
           where banfn = ls_out-prdoc
             and bnfpo = ls_out-prpos.
        if sy-subrc = 0.
          ls_out-poamt = eban-menge * eban-preis / eban-peinh.
        endif.
      endif.
      modify gt_out index lv_idx from ls_out transporting pramt.
    endif.


  endloop.

endform.                    " GET_DATA_GR
*&---------------------------------------------------------------------*
*&      Form  READ_PR_ACCOUNT
*&---------------------------------------------------------------------*
form read_pr_account using fp_refbn fp_rfpos
                     tables lt_ebkn type table of ty_ebkn.

  select a~banfn a~bnfpo
         a~aufnr a~kostl a~sakto a~fistl a~geber a~fipos
         b~lifnr b~bedat
       into table it_ebkn
       from  ebkn as a
       inner join eban as b
               on a~banfn = b~banfn
              and a~bnfpo = b~bnfpo
       where   a~banfn =   fp_refbn and
               a~bnfpo =   fp_rfpos .

*    SORT it_ebkn BY banfn bnfpo.

endform.                    " READ_PR_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  READ_PO_ACCOUNT
*&---------------------------------------------------------------------*
form read_po_account using fp_refbn fp_rfpos
                     tables lt_ekkn type table of ty_ekkn.

  select a~ebeln a~ebelp
         a~erekz a~netpr a~peinh a~pstyp
         b~lifnr b~bedat
     into table lt_ekkn
     from ekpo as a
          inner join ekko as b
             on a~ebeln = b~ebeln
     where a~ebeln = fp_refbn
       and a~ebelp = fp_rfpos.

*    SORT it_ekpo BY ebeln ebelp.

endform.                    " READ_PO_ACCOUNT
*&---------------------------------------------------------------------*
*&      Form  FILL_PAY_INFO
*&---------------------------------------------------------------------*
form fill_pay_info .
  data : ls_out    type ty_output,
         lv_idx    like sy-tabix,
         lv_belnr  like bsak-belnr.

  types: begin of ts_bsak,
          belnr like bsak-belnr,
          gjahr like bsak-gjahr,
          augdt like bsak-augdt,
          augbl like bsak-augbl,
         end of ts_bsak.
  data : lt_bsak type table of ts_bsak,
         ls_bsak type ts_bsak.

  if gt_inv[] is not initial.
    select belnr gjahr augdt augbl
      into table lt_bsak
      from bsak
      for all entries in gt_inv
     where bukrs =  p_fikrs
       and lifnr = gt_inv-lifnr
       and gjahr = gt_inv-ivghr
       and belnr = gt_inv-ivdoc.
  endif.
  sort lt_bsak by belnr gjahr.

  loop at gt_out into ls_out where ivdocs ne space.
    lv_idx = sy-tabix.

    clear : ls_bsak.

    if ls_out-ivdocs+0(1) = '*'.
      lv_belnr = ls_out-ivdocs+1(10).
    else.
      lv_belnr = ls_out-ivdocs.
    endif.
    read table lt_bsak into ls_bsak
                       with key belnr = lv_belnr
                                gjahr = ls_out-ivghr binary search.
    if sy-subrc = 0.
      ls_out-augdt = ls_bsak-augdt.
      modify gt_out index lv_idx from ls_out transporting augdt .
    endif.

  endloop.

endform.                    " FILL_PAY_INFO
*&---------------------------------------------------------------------*
*&      Form  GET_MASTER
*&---------------------------------------------------------------------*
form get_master .

* commitmemt master
  select fmci~fipos fmci~posit fmcit~bezei
     into corresponding fields of table it_fmci
     from fmci
     inner join fmcit
        on fmci~fikrs  = fmcit~fikrs
       and fmci~fipos  = fmcit~fipex
*       and fmci~datbis = fmcit~datbis
     where fmci~fikrs =  p_fikrs
       and fmci~fipos in s_fipex.

*       AND knzaepo IN p_knz.
  data: $ix like sy-tabix.
  loop at it_fmci.
    $ix = sy-tabix.
    perform determine_profile_fs using    p_fikrs
                                          ''
                                          it_fmci-posit
                                          ''
                                          p_gjahr
                                 changing it_fmci-profil.

    if it_fmci-profil not in s_prof.
      delete it_fmci index $ix.
    endif.

  endloop.

  sort it_fmci by fipos.

endform.                    " GET_MASTER
