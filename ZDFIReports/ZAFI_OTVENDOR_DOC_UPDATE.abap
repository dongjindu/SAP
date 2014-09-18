***********************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZAFI_ONETIME_VENDOR_UPDATE
*& Type   : Report - Utility                                           *
*& Author : Manjunath
*& Title  : One time vendor update program
*
*&---------------------------------------------------------------------*
* Help Desk Request No  : 64KI582682                                  *
* System Id:
*   Requested by:       Sudhakar  Manoharan                            *
*   Assigned to:        Manjunath Venkatesh
*
*
*
*  ABAP Analyst:  Manjunath Venkatesh                                  *
*                                                                      *
* Business Users:   Accounts Payable and Tax Team                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 05/04/06    Manju        UD1K920497   Initial Coding
* 05/16/06    Manju        UD1K920703   Navigation to FB03 on double
*                                       click
* 10/25/06    Manju        UD1K922715   Mass update changes
* 11/07/06    Manju        UD1K922946   Add name2 to output report
************************************************************************
report zafi_onetime_vendor_update  line-size 132 line-count 65  no
standard page heading message-id db.


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
tables : bsec,
         bkpf,
         bseg,
         with_item,
         zvend_onetime.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
data : begin of it_tab occurs 0,
        bukrs like bsec-bukrs,
        belnr like bsec-belnr,
        gjahr like bsec-gjahr,
        buzei like bsec-buzei,
        name1 like bsec-name1,
        name2 like bsec-name2,                              "UD1K922946
        stras like bsec-stras,
        ort01 like bsec-ort01,
        regio like bsec-regio,
        pstlz like bsec-pstlz,
        pfach like bsec-pfach,
        land1 like bsec-land1,
        stcd1 like bsec-stcd1,
        stcd2 like bsec-stcd2,
      end of it_tab.

data : begin of it_bkpf occurs 0,
       gjahr like bkpf-gjahr,
       belnr like bkpf-belnr,
       aedat like bkpf-aedat,
       cpudt like bkpf-cpudt,
       end of it_bkpf.

data : begin of it_bseg occurs 0,
       gjahr like bkpf-gjahr,
       belnr like bkpf-belnr,
       lifnr like bseg-lifnr,
       end of it_bseg.

data : begin of it_with occurs 0,
       gjahr     like bkpf-gjahr,
       belnr     like bkpf-belnr,
       buzei     like with_item-buzei,
       witht     like with_item-witht,
       wt_withcd like with_item-wt_withcd,
       wt_qsshb  like with_item-wt_qsshb,
       end of it_with.

data : begin of out_tab occurs 0,
        bukrs like bsec-bukrs,
        belnr like bsec-belnr,
        gjahr like bsec-gjahr,
        name1 like bsec-name1,
        name2 like bsec-name2,
        stras like bsec-stras,
        ort01 like bsec-ort01,
        regio like bsec-regio,
        pstlz like bsec-pstlz,
        pfach like bsec-pfach,
        land1 like bsec-land1,
        stcd1 like bsec-stcd1,
        stcd2 like bsec-stcd2,
        aedat like bkpf-aedat,
        lifnr like bseg-lifnr,
        witht like with_item-witht,
        wt_withcd like with_item-wt_withcd,
        dmbtr like bsak-dmbtr,
        bschl like bsak-bschl,
        chck  type c,
      end of out_tab.
data : wa_tab like line of out_tab.

data : begin of out_tab1 occurs 0,
        bukrs like bsec-bukrs,
        belnr like bsec-belnr,
        gjahr like bsec-gjahr,
        name1 like bsec-name1,
        name2 like bsec-name1,
        stras like bsec-stras,
        ort01 like bsec-ort01,
        regio like bsec-regio,
        pstlz like bsec-pstlz,
        land1 like bsec-land1,
        pfach like bsec-pfach,
        stcd1 like bsec-stcd1,
        stcd2 like bsec-stcd2,
        aedat like bkpf-aedat,
      end of out_tab1.

data:begin of bdc_tab occurs 0.
        include structure bdcdata.
data:end of bdc_tab.

data  begin of messtab occurs 0.     " BDC MESSAGE TABLE.
        include structure bdcmsgcoll.
data  end of messtab.


data: wa_custom_control type scrfname value 'ALV_CONTAINER',
      alv_grid          type ref to cl_gui_alv_grid,
      grid_container    type ref to cl_gui_custom_container,
      w_repid like sy-repid,
      tot type i,
      l_mode type c value 'N'.

data : l_str type i,
       str1(15) type c  ,
       str2(15) type c,
       str3(15) type c,
       pattern type c value '-',
       taxcod1 like bsec-stcd1,
       taxcod2 like bsec-stcd2.

* Global variables for attributes or etc of ALV GRID
data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldcat_fi  type lvc_t_fcat with header line,
       it_fieldcat_co  type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail


data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname  like line of it_fieldcat.

data : wa_variant type disvariant. "for parameter IS_VARIANT

data: wa_save    type c   value 'A'.   "for Parameter I_SAVE

data: gt_row   type lvc_t_row,
      gs_row   type lvc_s_row,
      gt_roid  type lvc_t_roid.

* -------------------------------------------------------------
* EVent class
*-----------------------------------------------------------
* local class to handle semantic checks
class lcl_event_receiver definition deferred.

data: g_event_receiver type ref to lcl_event_receiver.

*************************************************************
* LOCAL CLASS Definition
**************************************************************
*§4.Define and implement event handler to handle event DATA_CHANGED.
*
class lcl_event_receiver definition.

  public section.
    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
              importing er_data_changed,

        double_click   for event double_click
                         of cl_gui_alv_grid
                         importing e_row
                                   e_column
                                   es_row_no.

    data: error_in_data type c.

endclass.
data :it_lvc  like lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
class lcl_event_receiver implementation.
  method handle_data_changed.

    data: ls_good type lvc_s_modi,
          lv_value type lvc_value,
          flag type c,
          lvc_t_row type lvc_t_row.



    error_in_data = space.
    loop at er_data_changed->mt_good_cells into ls_good.
      case ls_good-fieldname.
* check if column Name1 of this row was changed
        when 'NAME1'.
          call method er_data_changed->get_cell_value
                     exporting
                        i_row_id  = ls_good-row_id
                        i_fieldname = ls_good-fieldname
                     importing
                        e_value =   lv_value.
          call method er_data_changed->modify_cell
                  exporting
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.
* Check if column STRAS of this row was changed
        when 'STRAS'.
          call method er_data_changed->get_cell_value
           exporting
              i_row_id  = ls_good-row_id
              i_fieldname = ls_good-fieldname
           importing
              e_value =   lv_value.
          call method er_data_changed->modify_cell
                  exporting
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.
        when 'ORT01'.
          call method er_data_changed->get_cell_value
           exporting
              i_row_id  = ls_good-row_id
              i_fieldname = ls_good-fieldname
           importing
              e_value =   lv_value.
          call method er_data_changed->modify_cell
                  exporting
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.
        when 'REGIO'.
          call method er_data_changed->get_cell_value
           exporting
              i_row_id  = ls_good-row_id
              i_fieldname = ls_good-fieldname
           importing
              e_value =   lv_value.
          call method er_data_changed->modify_cell
                  exporting
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.
        when 'PSTLZ'.
          call method er_data_changed->get_cell_value
            exporting
               i_row_id  = ls_good-row_id
               i_fieldname = ls_good-fieldname
            importing
               e_value =   lv_value.
          call method er_data_changed->modify_cell
                  exporting
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.
        when 'LAND1'.
          call method er_data_changed->get_cell_value
           exporting
              i_row_id  = ls_good-row_id
              i_fieldname = ls_good-fieldname
           importing
              e_value =   lv_value.
          call method er_data_changed->modify_cell
                  exporting
                       i_row_id = ls_good-row_id
                       i_fieldname = ls_good-fieldname
                       i_value     = lv_value.
        when 'STCD1'.
          call method er_data_changed->get_cell_value
            exporting
               i_row_id  = ls_good-row_id
               i_fieldname = ls_good-fieldname
            importing
               e_value =   lv_value.
          clear flag.
          call function 'Z_FFI_TAX_CODE_CHECK'
               exporting
                    lv_value = lv_value
               importing
                    flag     = flag.
          clear it_lvc.
          it_lvc-index  = ls_good-row_id.
          append it_lvc to lvc_t_row .
          call method alv_grid->set_selected_rows
          exporting  it_index_rows = lvc_t_row.

          if flag = 'X'.
            call method er_data_changed->add_protocol_entry
                        exporting
                              i_msgid =  'SU'
                              i_msgno =  '000'
                              i_msgty =  'E'
         i_msgv1 = 'Tax code format should be 111-11-1111 or 11-1111111'
         i_msgv2 = '(Value->)'
         i_msgv3 = lv_value
                              i_fieldname = ls_good-fieldname
                              i_row_id  =  ls_good-row_id.


          else.
            call method er_data_changed->modify_cell
                    exporting
                         i_row_id = ls_good-row_id
                         i_fieldname = ls_good-fieldname
                         i_value     = lv_value.
            call method er_data_changed->modify_cell
                   exporting
                        i_row_id = ls_good-row_id
                        i_fieldname = 'CHCK'
                       i_value     = 'X'.
          endif.
        when 'STCD2'.
*          call method er_data_changed->get_cell_value
*            exporting
*               i_row_id  = ls_good-row_id
*               i_fieldname = ls_good-fieldname
*            importing
*               e_value =   lv_value.
*          clear flag.
*          call function 'Z_FFI_TAX_CODE_CHECK'
*               exporting
*                    lv_value = lv_value
*               importing
*                    flag     = flag.
*          if flag = 'X'.
*            call method er_data_changed->add_protocol_entry
*                        exporting
*                              i_msgid =  'SU'
*                              i_msgno =  '000'
*                              i_msgty =  'I'
*         i_msgv1 = 'Tax code format should be 111-11-1111 or
*11-1111111'
*         i_msgv2 = '(Value->)'
*         i_msgv3 = lv_value
*                              i_fieldname = ls_good-fieldname
*                              i_row_id  =  ls_good-row_id.
*          else.
*            call method er_data_changed->modify_cell
*                    exporting
*                         i_row_id = ls_good-row_id
*                         i_fieldname = ls_good-fieldname
*                         i_value     = lv_value.
*            call method er_data_changed->modify_cell
*                    exporting
*                         i_row_id = ls_good-row_id
*                         i_fieldname = 'CHCK'
*                        i_value     = 'X'.
*
*          endif.

      endcase.
    endloop.

*§7.Display application log if an error has occured.
    if error_in_data eq 'X'.
      call method er_data_changed->display_protocol.
    endif.

  endmethod.

  method double_click.
    perform d0100_event_double_click using e_row
                                          e_column.
  endmethod.

endclass.

tables: bsak.
*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-bl1 .
parameters :  p_bukrs like bsec-bukrs default 'H201'.
parameters : p_gjahr like bsec-gjahr obligatory default sy-datum(4).

select-options : s_augdt for bsak-augdt,
                 s_lifnr for bseg-lifnr,

                 s_belnr for bsec-belnr,
                 s_budat for bkpf-budat,

                 s_bschl for bseg-bschl. "default '31'.
selection-screen end of block bl1.

selection-screen begin of block bl2 with frame title text-bl2 .
parameters : p_chg radiobutton group g1,
             p_dis radiobutton group g1.
selection-screen end of block bl2.

select-options :
        s_stcd1  for bsec-stcd1,
        s_stcd2  for bsec-stcd2,
        s_witht  for with_item-witht,
        s_withcd for with_item-wt_withcd.


*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
start-of-selection.

  perform get_data.

  perform display_data.

end-of-selection.

*-------------------------------------------------------------*
* END-of-selection
*--------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  describe table s_augdt lines sy-index.
  if sy-index = 0.
    s_augdt-sign   = 'I'.
    s_augdt-option = 'BT'.
    concatenate p_gjahr '0101' into s_augdt-low.
    concatenate p_gjahr '1231' into s_augdt-high.
    append s_augdt.
  endif.

  data: begin of gt_bsak occurs 0,
          bukrs like bsak-bukrs,
          belnr like bsak-belnr,
          gjahr like bsak-gjahr,
          buzei like bsak-buzei,
          bschl like bsak-bschl,
          dmbtr like bsak-dmbtr,
        end of gt_bsak.

* Select One time Vendor Data
  select  * into corresponding fields of table gt_bsak

     from lfa1 as b
       join bsak as a
         on a~lifnr = b~lifnr
       join bkpf as h
         on h~bukrs = a~bukrs
        and h~belnr = a~belnr
        and h~gjahr = a~gjahr

       where  b~lifnr in s_lifnr
         and  b~xcpdk = 'X'      "ONE TIME
         and  a~bukrs =  p_bukrs
         and  a~augdt in s_augdt
         and  a~budat in s_budat
         and  a~belnr in s_belnr
         and  a~bschl in s_bschl
         and  a~XZAHL = space
         and  h~stblg = space.

  sort gt_bsak by belnr gjahr.

  select  bukrs
          belnr
          gjahr
          buzei
          name1
          name2
          stras
          ort01
          regio
          pstlz
          pfach
          land1
          stcd1
          stcd2
          from bsec
              into table it_tab
              for all entries in gt_bsak
              where  bukrs =  gt_bsak-bukrs
                and  belnr =  gt_bsak-belnr
                and  gjahr =  gt_bsak-gjahr
                and  stcd1 in s_stcd1
                and  stcd2 in s_stcd2.

  if not it_tab[] is initial.
    select gjahr belnr aedat cpudt from bkpf into table it_bkpf
           for all entries
           in it_tab
           where bukrs = it_tab-bukrs and
                 belnr = it_tab-belnr and
                 gjahr = it_tab-gjahr.

    select gjahr belnr lifnr from bseg into table it_bseg
            for all entries in it_tab
            where bukrs = it_tab-bukrs and
                  belnr = it_tab-belnr and
                  gjahr = it_tab-gjahr and
                  koart in ('K', 'D').

    select gjahr belnr buzei witht wt_withcd wt_qsshb
            into table it_with
            from with_item
            for all entries in it_tab
            where bukrs = it_tab-bukrs and
                  belnr = it_tab-belnr and
                  gjahr = it_tab-gjahr.

  endif.

  sort it_tab  by belnr gjahr.
  sort it_bkpf by belnr gjahr.
  sort it_bseg by belnr gjahr.
  sort it_with by belnr gjahr.

  loop at it_bkpf.
    read table it_tab with key belnr = it_bkpf-belnr
                               gjahr = it_bkpf-gjahr binary search.

    if sy-subrc eq 0.
      clear it_bseg.
      read table it_bseg with key belnr = it_bkpf-belnr
                                  gjahr = it_bkpf-gjahr binary search.
      check it_bseg-lifnr in s_lifnr.

      move-corresponding it_tab to out_tab.
      move-corresponding it_bkpf to out_tab.

      out_tab-lifnr = it_bseg-lifnr.

      read table it_with with key belnr = it_bkpf-belnr
                                  gjahr = it_bkpf-gjahr binary search.
      if sy-subrc = 0.
        out_tab-witht     = it_with-witht.
        out_tab-wt_withcd = it_with-wt_withcd.
      endif.

      check out_tab-witht     in s_witht.
      check out_tab-wt_withcd in s_withcd.

      if out_tab-aedat is initial.
        move it_bkpf-cpudt to out_tab-aedat.
      endif.

      read table gt_bsak with key belnr = it_bkpf-belnr
                                  gjahr = it_bkpf-gjahr binary search.
      out_tab-dmbtr = gt_bsak-dmbtr.
      out_tab-bschl = gt_bsak-bschl.
      append out_tab.
    endif.

  endloop.

*  loop at out_tab.
*    read table it_bseg with key belnr = out_tab-belnr.
*    check it_bseg-lifnr in s_lifnr.
*    if sy-subrc eq 0.
*      out_tab-lifnr = it_bseg-lifnr.
*      modify out_tab transporting lifnr.
*    endif.
*  endloop.

  read table out_tab index 1.
  if sy-subrc ne 0.
    message i000 with ' No Records to Update'.
    stop.
  endif.

endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.

  call screen 100.

endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  if p_chg eq 'X'.
    set pf-status 'ZD1'.
    set titlebar 'ZD2'.
  else.
    set pf-status 'ZD2'.
    set titlebar 'ZD2'.
  endif.


endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  case sy-ucomm.
    when 'BACK' or 'EXIT' or 'CANC'.
      clear: sy-ucomm.
      leave to screen 0.

    when 'POST_CHAN'.
      perform get_selected_rows.
      perform post_changes.

    when 'MASS_UPDAT'.
      perform get_selected_rows.
      perform mass_update.

*    when 'TAXCD'.
*      perform get_selected_rows.
*      perform swap_taxcode.

  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.

  case sy-ucomm.
    when 'EXIT' .
      clear: sy-ucomm.
      leave to screen 0.
  endcase.

endmodule.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  create_ALVobject  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module create_alvobject output.
  if grid_container is initial.
    perform create_container.
    perform set_attributes.
*    perform build_field_catalog.
    perform create_grid.
  endif.
endmodule.                 " create_ALVobject  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container.
  create object grid_container
            exporting container_name = 'MY_CONT'
            exceptions
             cntl_error = 1
             cntl_system_error = 2
             create_error = 3
             lifetime_error = 4
             lifetime_dynpro_dynpro_link = 5.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  endif.

  create object alv_grid
         exporting i_parent = grid_container
                   i_appl_events = 'X'.

endform.                    " create_container
*&---------------------------------------------------------------------*
*&      Form  set_attributes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes.
  data : lw_s_dragdrop type lvc_s_dd01. "/ Drag&Drop control settings

  clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ''.       "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.      "/optimizes the column width
*  wa_is_layout-no_merging = 'X'.      "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

* Field Catalog
  call function 'LVC_FIELDCATALOG_MERGE'
       exporting
            i_structure_name
            ='ZVEND_ONETIME'
       changing
            ct_fieldcat      = it_fieldcat[].

  if p_chg eq 'X'.
    loop at it_fieldcat.
      case it_fieldcat-fieldname.
        when 'BUKRS'.
          it_fieldcat-key_sel = 'X'.
          it_fieldcat-emphasize = 'C410'.
        when 'BELNR '.
          it_fieldcat-key_sel = 'X'.
          it_fieldcat-emphasize = 'C410'.
        when 'GJAHR '.
          it_fieldcat-key_sel = 'X'.
          it_fieldcat-emphasize = 'C410'.
        when 'LIFNR'.
          it_fieldcat-key_sel = 'X'.
          it_fieldcat-emphasize = 'C410'.
        when 'STRAS'.
          it_fieldcat-edit = 'X'.
        when 'NAME1'.
          it_fieldcat-edit = 'X'.
        when 'NAME2'.
          it_fieldcat-edit = 'X'.
        when 'ORT01'.
          it_fieldcat-edit = 'X'.
        when 'REGIO'.
          it_fieldcat-edit = 'X'.
        when 'PSTLZ'.
          it_fieldcat-edit = 'X'.
        when 'PFACH'.
          it_fieldcat-edit = 'X'.
        when 'LAND1'.
          it_fieldcat-edit = 'X'.
        when 'STCD1'.
          it_fieldcat-edit = 'X'.
        when 'STCD2'.
          it_fieldcat-edit = 'X'.
        when 'AEDAT'.
          move  'Last Update Date' to :     it_fieldcat-coltext,
                                            it_fieldcat-scrtext_l,
                                            it_fieldcat-scrtext_m,
                                            it_fieldcat-scrtext_s.
          it_fieldcat-key_sel = 'X'.
        when 'CHCK'.
          it_fieldcat-checkbox = 'X'.
          it_fieldcat-edit = 'X'.
          it_fieldcat-fix_column = 'X'.
          it_fieldcat-outputlen = '1'.
*           MOVE  'Rec.Updated' TO :  it_fieldcat-COLTEXT,
*                                            it_fieldcat-SCRTEXT_L,
*                                            it_fieldcat-SCRTEXT_M,
*                                            it_fieldcat-SCRTEXT_S.
      endcase.
      modify it_fieldcat.
    endloop.
  endif.
endform.                    " set_attributes
*&---------------------------------------------------------------------*
*&      Form  create_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_grid.

  wa_is_layout-detailtitl  = 'Update One Time Vendor Data'.

  describe table out_tab lines tot.
  zvend_onetime-pstlz = tot.
  call method alv_grid->set_table_for_first_display
         exporting
                   is_layout        = wa_is_layout
                   i_save           = wa_save
                   is_variant       = wa_variant
                   i_default        = space
         changing  it_fieldcatalog  = it_fieldcat[]
                   it_outtab        = out_tab[].

* Enter---
  call method alv_grid->register_edit_event
                exporting
                   i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  call method alv_grid->register_edit_event
                exporting
                   i_event_id = cl_gui_alv_grid=>mc_evt_modified.

* Double Click - UD1K920703


  create object g_event_receiver.
  set handler g_event_receiver->handle_data_changed for alv_grid.
  set handler g_event_receiver->double_click  for alv_grid.



endform.                    " create_grid
*&---------------------------------------------------------------------*
*&      Form  get_selected_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_selected_rows.
  data: lt_rows type lvc_t_row with header line,
          lt_row_no type lvc_t_roid. "/Numeric IDs of Selected Rows
  data: l_line type i.
  refresh out_tab1. clear out_tab1.
  call method alv_grid->get_selected_rows
           importing et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    w_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         exporting
              titel = w_repid
              txt2  = sy-subrc
              txt1 =
                 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.
* For posting changes
  if  sy-ucomm eq 'POST_CHAN'.
    read table lt_rows index 1.
    if sy-subrc ne 0.
      message e000(zz) with text-m12.
    else.
      loop at lt_rows where index ne 0.
        read table out_tab index lt_rows-index.
        if sy-subrc eq 0.
          move-corresponding out_tab to out_tab1.
          append out_tab1.
        endif.
      endloop.
    endif.
* For mass update / SWAP Taxcode
  elseif   sy-ucomm ne 'POST_CHAN'.
    read table lt_rows index 1.
    if sy-subrc eq 0.
      loop at lt_rows where index ne 0.
        read table out_tab index lt_rows-index.
        if sy-subrc eq 0.
          move-corresponding out_tab to out_tab1.
          append out_tab1.
        endif.
      endloop.
    else.
      loop at out_tab.
        move-corresponding out_tab to out_tab1.
        append out_tab1.
      endloop.
    endif.

  endif.





endform.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  post_changes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_changes.
  loop at out_tab1.

* Initial Screen
    perform dynpro using:
      'X' 'SAPMF05L'          '0100',
      ' '  'RF05L-BELNR'      out_tab1-belnr,
      ' '  'RF05L-BUKRS'      out_tab1-bukrs,
      ' '  'RF05L-GJAHR'      out_tab1-gjahr,
      ' '  'BDC_OKCODE'       '=AZ'.
* Tax Screen
    perform dynpro using:
     'X'   'SAPLFCPD'         '0100',
     ''     'BDC_CURSOR'     'BSEC-NAME1',
     ''     'BSEC-NAME1'      out_tab1-name1,
     ''     'BSEC-NAME2'      out_tab1-name2,
     ''     'BSEC-STRAS'      out_tab1-stras,
     ''     'BSEC-ORT01'      out_tab1-ort01,
     ''     'BSEC-PSTLZ'      out_tab1-pstlz,
     ''     'BSEC-PFACH'      out_tab1-pfach,
     ''     'BSEC-LAND1'      out_tab1-land1,
     ''     'BSEC-REGIO'      out_tab1-regio,
     ''     'BSEC-STCD1'      out_tab1-stcd1,
     ''     'BSEC-STCD2'      out_tab1-stcd2,
     ''     'BDC_OKCODE'      '/00'.
* Save
    perform dynpro using:
           'X'   'SAPMF05L'         '0302',
           ''    'BDC_OKCODE'      '/11'.

    call transaction 'FB02' using bdc_tab
                        mode l_mode
                        messages into messtab.
    refresh bdc_tab. clear bdc_tab.
  endloop.
endform.                    " post_changes

*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  change_field_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form change_field_value.

endform.                    " change_field_value
*
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1168   text
*      -->P_1169   text
*      -->P_1170   text
*----------------------------------------------------------------------*
form dynpro using  dynbegin   p_name   value.

  if dynbegin = 'X'.
    clear bdc_tab.
    move: p_name     to bdc_tab-program,
          value    to bdc_tab-dynpro,
          dynbegin to bdc_tab-dynbegin.
    append bdc_tab.
  else.
    clear bdc_tab.
    move: p_name    to bdc_tab-fnam,
          value   to bdc_tab-fval.
    append bdc_tab.
  endif.



endform.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  d0100_event_double_click
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*      -->P_E_COLUMN  text
*----------------------------------------------------------------------*
form d0100_event_double_click using    p_e_row
                                       p_e_column.
  if p_e_column eq 'BELNR'.
    read table out_tab index p_e_row.
    if sy-subrc eq 0.
      set parameter id 'BLN' field out_tab-belnr.
      set parameter id 'BUK' field out_tab-bukrs.
      set parameter id 'GJR' field out_tab-gjahr.
      call transaction 'FB03' and skip first screen.
    endif.
  endif.
endform.                    " d0100_event_double_click
*&---------------------------------------------------------------------*
*&      Form  mass_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mass_update.
  data : flag.
  clear flag.
  if out_tab1[] is initial.

    Loop at out_tab into wa_tab.
      perform mass_update_taxcode using flag.
    endloop.
  else.
    loop at out_tab1.
      clear out_tab.
      flag = 'X'.
      read table out_tab into wa_tab with key belnr = out_tab1-belnr
                                              GJAHR = out_tab1-GJAHR.
      check  sy-subrc eq 0.
      perform mass_update_taxcode using flag.
    endloop.
  endif.

  CALL METHOD alv_grid->REFRESH_TABLE_DISPLAY.

endform.                    " mass_update
*&---------------------------------------------------------------------*
*&      Form  swap_taxcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form swap_taxcode.

  data : flag.
  clear flag.
  if out_tab1[] is initial.
    loop at out_tab into wa_tab.
      wa_tab-stcd1 = out_tab-stcd2.
      wa_tab-stcd2 = out_tab-stcd1.
      perform copy_taxcode using flag.
    endloop.
  else.
    loop at out_tab1.
      clear out_tab.
      flag = 'X'.
      read table out_tab into wa_tab with key belnr = out_tab1-belnr
                                              gjahr = out_tab1-gjahr.
      wa_tab-stcd1 = out_tab1-stcd2.
      wa_tab-stcd2 = out_tab1-stcd1.
      check  sy-subrc eq 0.
      perform copy_taxcode using flag.
    endloop.
  endif.

  call method alv_grid->refresh_table_display.

endform.                    " swap_taxcode
*&---------------------------------------------------------------------*
*&      Form  mass_update_taxcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mass_update_taxcode using p_flag.
  condense wa_tab-stcd1 no-gaps.
  condense wa_tab-stcd2 no-gaps.
  data :  st type i,
          st1 type i.
  field-symbols: <fs>.

  if not wa_tab-stcd1 is initial.
    taxcod1 = wa_tab-stcd1.

* Replace '-' with space and reformat with '-' again
    st = strlen( taxcod1 ) .
    st1 = 0.
    while st > 0.
      assign taxcod1+st1(1) to <fs>.
      if <fs> eq '-' .
        replace pattern  with space into taxcod1.
      endif.
      st = st - 1.
      st1 = st1 + 1.
    endwhile.

*   replace pattern with space into taxcod1.
    condense taxcod1 no-gaps.
    l_str = strlen( taxcod1 ).
    if l_str eq 9 .
      str1 = taxcod1+0(3).
      str2 = taxcod1+3(2).
      str3 = taxcod1+5(4).
      concatenate str1 str2 str3 into wa_tab-stcd1 separated by '-'.
    endif.
  endif.

*  if not wa_tab-stcd2 is initial.
*    taxcod2 = wa_tab-stcd2.
*    st = strlen( taxcod2 ) .
*    st1 = 0.
*    while st > 0.
*      assign taxcod2+st1(1) to <fs>.
*      if <fs> eq '-' .
*        replace pattern  with space into taxcod2.
*      endif.
*      st = st - 1.
*      st1 = st1 + 1.
*    endwhile.
*
*
**    replace pattern  with space into taxcod2.
*    condense taxcod2 no-gaps.
*    clear : str1, str2.
*    l_str = strlen( taxcod2 ).
*    if l_str eq 9 .
*      str1 = taxcod2+0(2).
*      str2 = taxcod2+2(7).
*      concatenate str1 str2 into wa_tab-stcd2
*        separated by '-'.
*    endif.
*  endif.

  if   p_flag  eq 'X'.
    modify out_tab from wa_tab transporting stcd1
                      where belnr = wa_tab-belnr and
                            gjahr = wa_tab-gjahr.
  else.
    modify out_tab from wa_tab transporting stcd1.
  endif.

  clear wa_tab.
endform.                    " mass_update_taxcode
*&---------------------------------------------------------------------*
*&      Form  copy_taxcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FLAG  text
*----------------------------------------------------------------------*
form copy_taxcode using    p_flag.

  if   p_flag  eq 'X'.
    modify out_tab from wa_tab transporting stcd1 stcd2
                      where belnr = wa_tab-belnr and
                            gjahr = wa_tab-gjahr.
  else.
    modify out_tab from wa_tab transporting stcd1 stcd2.
  endif.

endform.                    " copy_taxcode
