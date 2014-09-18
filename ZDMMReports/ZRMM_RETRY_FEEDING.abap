************************************************************************
* Program Name      : ZRMM_RETRY_FEEDING
* Creation Date     : 08/06/2014
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************
report  zrmm_retry_feedin no standard page heading
                     line-size 132
                     line-count 64(1)
                     message-id zmmm.
*
tables : zmmt0032.

data : begin of itab occurs 0.
        include structure zmmt0032.
data : check  type c.
data : end of itab.


data : w_goodsmvt_header like bapi2017_gm_head_01,
       w_goodsmvt_code   like BAPI2017_GM_CODE.

data : w_materialdocument like bapi2017_gm_head_ret.
data : it_goodsmvt_item
              like table of bapi2017_gm_item_create  with header line.
data : it_bapiret   like bapiret2 occurs 0 with header line.

* alv
data : w_dest(10),
       ok_code like sy-ucomm,
       w_repid like sy-repid,
       w_cnt   type i.
data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       is_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail
data : wa_save    type c   value 'A',   "for Parameter I_SAVE
       wa_variant type disvariant.      "for parameter IS_VARIANT

data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname    like line of it_fieldname.
data : wa_custom_control type        scrfname value 'ALV_CONTAINER',
       alv_grid          type ref to cl_gui_alv_grid,
       g_docking_container    type ref to cl_gui_docking_container.





selection-screen begin of block b1 with frame title text-001.
select-options: s_saedt for zmmt0032-saedt obligatory,
                s_rsnum for zmmt0032-rsnum.
selection-screen  begin of line.
selection-screen comment 1(10) text-m01.
selection-screen comment 22(10) text-u01 for field p_batch.
parameters: p_batch radiobutton group grp2 default 'X'.
selection-screen comment 45(9) text-u02 for field p_online.
parameters: p_online radiobutton group grp2.
selection-screen  end of line.
selection-screen end of block b1.





initialization.
*

at selection-screen output.
  refresh s_saedt.
  s_saedt-low = sy-datum - 1.
  s_saedt-high = sy-datum.
  append s_saedt.


start-of-selection.
  perform get_data_int.


  if itab[] is initial.
    message i009 with text-002.
    exit.
  endif.

  if p_batch = 'X'.
    perform auto_all_line.
    perform bapi_goodsmvt_int.
  else.
    perform online_display.
  endif.

end-of-selection.

*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_INT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data_int .



  select *  into corresponding fields of table itab
      from zmmt0032 as a inner join resb as b
        on a~rsnum eq b~rsnum and
           a~rspos eq b~rspos
       where a~saedt in s_saedt
         and a~rsnum in s_rsnum
         and a~type   = 'E'
         and b~xloek  = 'X'.

  sort itab by rsnum rspos saedt saeuz budat matnr.


endform.                    " GET_DATA_INT
*&---------------------------------------------------------------------*
*&      Form  BAPI_GOODSMVT_INT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_goodsmvt_int .

  data : w_bwart  type bwart,
         w_meins  type erfme.


  w_bwart = '311'.
  w_meins = 'EA'.
  w_goodsmvt_code = '04'.

  loop at itab where check = 'X'.
    clear : w_materialdocument,
            w_goodsmvt_header, it_goodsmvt_item.
    refresh: it_goodsmvt_item, it_bapiret.

    w_goodsmvt_header-pstng_date = itab-budat.
    w_goodsmvt_header-doc_date   = sy-datum.

    move : itab-matnr        to it_goodsmvt_item-material,
           itab-lgort        to it_goodsmvt_item-stge_loc,
           itab-umlgo        to it_goodsmvt_item-move_stloc,
           itab-menge        to it_goodsmvt_item-entry_qnt,
           itab-werks        to it_goodsmvt_item-plant,
           itab-werks        to it_goodsmvt_item-move_plant,
           w_bwart           to it_goodsmvt_item-move_type,
           w_meins           to it_goodsmvt_item-entry_uom.
    append it_goodsmvt_item.

*
    call function 'BAPI_GOODSMVT_CREATE'
      exporting
        goodsmvt_header  = w_goodsmvt_header
        goodsmvt_code    = w_goodsmvt_code
      importing
        materialdocument = w_materialdocument-mat_doc
      tables
        goodsmvt_item    = it_goodsmvt_item
        return           = it_bapiret.
    if not w_materialdocument-mat_doc is initial.
      COMMIT WORK AND WAIT.
      move : w_materialdocument-mat_doc     to itab-mblnr,
             'S'                            to itab-type,
             w_materialdocument-doc_year    to itab-mjahr,
             sy-uname                       to itab-atnam,
             sy-datum                       to itab-atdat,
             sy-uzeit                       to itab-attim.

      itab-message = 'Stock transfer complete against cancelled order'.
      itab-check = ' '.
      modify itab transporting mblnr mjahr message type check
              where pkkey = itab-pkkey
                and rsnum = itab-rsnum
                and rspos = itab-rspos
                and saedt = itab-saedt
                and saeuz = itab-saeuz.

    endif.
  endloop.

* last save
  modify zmmt0032 from table itab.
  if sy-subrc = 0.
    commit work and wait.
  endif.

endform.                    " BAPI_GOODSMVT_INT
*&---------------------------------------------------------------------*
*&      Form  ONLINE_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form online_display .
  call screen 0200.
endform.                    " ONLINE_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  set pf-status 'ST200'.
  set titlebar 'T200'.
endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module display_alv_200 output.

  if g_docking_container is initial.
    perform create_container_n_object.
    perform set_attributes_alv_grid.
    perform build_field_catalog.
    perform assign_itab_to_alv.

    call method alv_grid->refresh_table_display.
  endif.




endmodule.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object .
  data:   l_repid like sy-repid,
            l_dynnr like sy-dynnr.

  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  create object g_docking_container
    exporting
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.

  if sy-subrc ne 0.
    call function 'POPUP_TO_INFORM'
      exporting
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  endif.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  create object alv_grid
*         EXPORTING i_parent = grid_container
         exporting i_parent = g_docking_container
                   i_appl_events = 'X'.

endform.                    " CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_grid .

  data : lw_s_dragdrop type lvc_s_dd01. "/ Drag&Drop control settings

  clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width

*  wa_is_layout-box_fname         = 'CHECK'.

  wa_is_layout-stylefname = 'CELLTAB'.


*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.






endform.                    " SET_ATTRIBUTES_ALV_GRID

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog .

  data: lw_itab type slis_tabname,
          lw_waers like t001-waers,
          l_rqty(9),
          l_datum(8),
          l_cn(2) type n.

  data ls_fcat type slis_fieldcat_alv.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.


  lw_itab = 'ITAB'.
  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
*     i_internal_tabname = lw_itab
      i_structure_name   = 'ZMMT0032'
    changing
      ct_fieldcat        = it_fieldname.



  clear ls_fcat.
  ls_fcat-fieldname = 'CHECK'.
* Essential: declare field as checkbox and
*            mark it as editable field:
  ls_fcat-checkbox = 'X'.
  ls_fcat-edit = 'X'.

* do not forget to provide texts for this extra field
*  ls_fcat-coltext = text-f01.
*  ls_fcat-tooltip = text-f02.
*  ls_fcat-seltext = text-f03.

* optional: set column width
  ls_fcat-outputlen = 10.
*
  append ls_fcat to it_fieldname.


  perform setting_fieldcat tables it_fieldcat using :
*

                                 'S' 'RSNUM'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Reservation Requirement',
                                 'E' 'OUTPUTLEN'   '10',
*
                                 'S' 'BUDAT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Posting Date',
                                 'E' 'OUTPUTLEN'   '10',
*
                                 'S' 'MATNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material Number',
                                 'E' 'OUTPUTLEN'   '18',
*
                                 'S' 'LGORT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Storage',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'UMLGO'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Receiving Storage',
                                 'E' 'OUTPUTLEN'   '5',
*
                                 'S' 'MENGE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Quantity',
                                 'E' 'OUTPUTLEN'   '15',
*
                                 'S' 'MEINS'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Base Unit',
                                 'E' 'OUTPUTLEN'   '4',
*
                                 'S' 'WERKS'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Plant',
                                 'E' 'OUTPUTLEN'   '5'.
*


endform.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv .


  call method alv_grid->set_table_for_first_display
    exporting
      is_layout            = wa_is_layout
      i_save               = 'X'    "wa_save
      is_variant           = wa_variant
      i_default            = 'X'
*     it_toolbar_excluding = it_toolbar_excluding[]
    changing
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = itab[].
*               it_sort          = it_sort[].

endform.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0637   text
*      -->P_0638   text
*      -->P_0639   text
*----------------------------------------------------------------------*
form setting_fieldcat    tables   p_fieldcat structure it_fieldcat
                         using    p_gubun
                                  p_field
                                  p_value.
  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear: p_fieldcat.

    read table it_fieldname into w_fieldname
                            with key fieldname  = p_field.
    if sy-subrc ne 0.
      message e000(zz) with 'Check field catalog'.
    endif.

    move: w_fieldname-fieldname to p_fieldcat-fieldname.
    exit.
  endif.

* Setting The Field's Attributes
  concatenate 'P_FIELDCAT-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    add 1 to w_cnt.
    p_fieldcat-col_pos = w_cnt.
    append p_fieldcat.
  endif.
endform.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.


  case ok_code.
    when 'EXIT' or 'CANCE'.
      leave program.
    when 'BACK'.
      leave to screen 0.

    when 'M311'.
      perform check_line_select.
      perform bapi_goodsmvt_int.

    when 'SALL'.
      perform select_all_line.
*
    when 'DSALL'.
      perform deselect_all_line.
**

  endcase.
*  call method alv_grid->refresh_table_display.
endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_line_select.


  data: wt_index_rows type lvc_t_row,
        wt_row_no type lvc_t_roid,
        wa_et_row_no like line of wt_row_no,
        wn type i.

  clear:wt_index_rows,wt_row_no.
  refresh:wt_index_rows,wt_row_no.
  call method alv_grid->get_selected_rows
    importing
      et_index_rows = wt_index_rows
      et_row_no     = wt_row_no.

  describe table wt_index_rows lines wn.
  if wn = 0.
*    perform select_all_line.
  endif.

  loop at wt_row_no into wa_et_row_no.
    read table itab index wa_et_row_no-row_id.
    if sy-subrc = 0.
      move 'X'          to  itab-check.
      modify itab index wa_et_row_no-row_id transporting check.
    endif.
  endloop.
*
endform.                    " CHECK_LINE
*&---------------------------------------------------------------------*
*&      Form  LINE_SELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_all_line.


  data: wt_index_rows type lvc_t_row,
        wa_index_rows type lvc_s_row,
        wt_row_no type lvc_t_roid,
        wa_et_row_no like line of wt_row_no.
  data : w_index   type lvc_index.




  refresh : wt_index_rows,wt_row_no.
  loop at itab.
    wa_et_row_no-row_id = sy-tabix.
    append wa_et_row_no  to wt_row_no.

    w_index = sy-tabix.
    move : w_index  to wa_index_rows-index.
    append wa_index_rows to wt_index_rows.
  endloop.
*
  call method alv_grid->set_selected_rows
    exporting
      it_index_rows = wt_index_rows
      it_row_no     = wt_row_no.
*


endform.                    " LINE_SELECT
*&---------------------------------------------------------------------*
*&      Form  DESELECT_ALL_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form deselect_all_line .


  data: wt_index_rows type lvc_t_row,
        wt_row_no type lvc_t_roid.

  refresh : wt_index_rows,wt_row_no.
  call method alv_grid->get_selected_rows
    importing
      et_index_rows = wt_index_rows
      et_row_no     = wt_row_no.
*
  call method alv_grid->set_selected_rows
    exporting
      it_index_rows = wt_index_rows
      it_row_no     = wt_row_no.

  call method alv_grid->refresh_table_display.

endform.                    " DESELECT_ALL_LINE
*&---------------------------------------------------------------------*
*&      Form  AUTO_ALL_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form AUTO_ALL_LINE .


 loop at itab.
   move 'X'        TO ITAB-CHECK.
   MODIFY ITAB.
 endloop.

endform.                    " AUTO_ALL_LINE
