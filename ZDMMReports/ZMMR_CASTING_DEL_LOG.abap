************************************************************************
* Program Name      : ZMMR_CASTING_DEL_LOGN
* Creation Date     : 04/2013
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

report zmmr_casting_del_log  no standard page heading
                     line-size 132
                     line-count 64(1)
                     message-id zmmm.
tables: likp,likpuk, lips.
type-pools: vrm, slis.

*DATA: it_data LIKE TABLE OF ztmm_cast_dellog WITH HEADER LINE.
* 07/19/2013 - T00306 Start
data: begin of it_data occurs 0.
        include structure ztmm_cast_dellog.
data: matnr type matnr,
      end of it_data.
* 07/19/2013 - T00306 End

data: begin of it_output occurs 0.
        include structure ztmm_cast_dellog.
data: matnr type matnr,
      celltab type lvc_t_styl,
      end of it_output.

data: ok_code like sy-ucomm,
      w_cnt type i,
      w_repid like sy-repid.

data: it_fieldcat type lvc_t_fcat with header line,
      it_fieldname type slis_t_fieldcat_alv,
      it_sort type lvc_t_sort with header line,
      it_exclude type ui_functions.

data: wa_is_layout type lvc_s_layo, "/The Layout Structure
      w_fieldname like line of it_fieldcat,
      wa_save type c   value 'A',   "for Parameter I_SAVE
      wa_variant type disvariant,
      wa_stbl  type lvc_s_stbl.


data: wa_custom_control type scrfname value 'ALV_CONTAINER',
      alv_grid type ref to cl_gui_alv_grid,
      grid_container type ref to cl_gui_custom_container.

data: name  type vrm_id,
      list  type vrm_values,
      value like line of list.

data: w_date like sy-datum,
      w_time like sy-uzeit.

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
              importing er_data_changed.

    methods:
      handle_hotspot_click
         for event hotspot_click of cl_gui_alv_grid
              importing e_row_id   e_column_id  es_row_no.

    data: error_in_data type c.

endclass.                    "LCL_EVENT_RECEIVER DEFINITION
data :it_lvc  like lvc_s_row.
*************************************************************
* LOCAL CLASS IMPLEMENTATION
**************************************************************
class lcl_event_receiver implementation.
  method handle_data_changed.

    data: ls_good type lvc_s_modi,
          lv_value type lvc_value,
          w_qty(13),
          lvc_t_row type lvc_t_row,
          lw_output like it_output.

    error_in_data = space.
    loop at er_data_changed->mt_good_cells into ls_good.
      case ls_good-fieldname.
* check if column Name1 of this row was changed
        when 'ZUTRUCKNO'.
          call method er_data_changed->get_cell_value
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            importing
              e_value     = lv_value.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.

          read table it_output into lw_output
               index ls_good-row_id.

* 07/19/2013 - T00306 Start
          if not lw_output-zutruckno is initial
             and lv_value is initial.
            perform error_message_display using er_data_changed
                                      ls_good
                                      '009'
                                      'E'
                                      'Trailer no is required'
                                      ls_good-fieldname.
            perform set_focus_on_grid using alv_grid
                                ls_good-row_id
                                ls_good-fieldname.

          elseif not lv_value is initial.
            if lw_output-zudate is initial.
              lw_output-zudate = w_date.
            endif.
            if lw_output-zutime is initial.
              lw_output-zutime = w_time.
            endif.
* 07/19/2013 - T00306 End
            lw_output-zuuser = sy-uname.
          endif.

          modify it_output from lw_output index ls_good-row_id
              transporting zudate zutime zuuser.

        when 'ZUCOMENT'.
          call method er_data_changed->get_cell_value
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            importing
              e_value     = lv_value.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.
          read table it_output into lw_output index ls_good-row_id.
          if lv_value is initial.
            clear: lw_output-zutime, lw_output-zudate, lw_output-zuuser.
          else.
            lw_output-zutime = w_time.
            lw_output-zudate = w_date.
            lw_output-zuuser = sy-uname.
          endif.
          modify it_output from lw_output index ls_good-row_id
              transporting zudate zutime zuuser.
        when 'ZCCOMENT'.
          call method er_data_changed->get_cell_value
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
            importing
              e_value     = lv_value.
          call method er_data_changed->modify_cell
            exporting
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.
          read table it_output into lw_output index ls_good-row_id.
          if lv_value is initial.
            clear: lw_output-zctime, lw_output-zcdate, lw_output-zcuser.
          else.
            lw_output-zctime = w_time.
            lw_output-zcdate = w_date.
            lw_output-zcuser = sy-uname.
          endif.
          modify it_output from lw_output index ls_good-row_id
              transporting zcdate zctime zcuser.
      endcase.
    endloop.

*§7.Display application log if an error has occured.
    if error_in_data eq 'X'.
      call method er_data_changed->display_protocol.
    endif.

  endmethod.                    "HANDLE_DATA_CHANGED

  method handle_hotspot_click.
    data: lw_output like it_output,
          l_index like sy-tabix.

    if e_column_id-fieldname = 'LIFEX'.
      l_index = e_row_id-index.
      read table it_output into lw_output index l_index.
      if sy-subrc = 0.
        perform call_transaction using lw_output-lifex.
*        SET PARAMETER ID 'VL' FIELD it_output-vbeln.
*        CALL TRANSACTION 'VL06IF' AND SKIP FIRST SCREEN.
      endif.
    endif.
  endmethod.                    "handle_hotspot_click

endclass.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

selection-screen begin of block block1 with frame title text-001.

select-options: s_vbeln for likp-vbeln,
                s_lifex for likp-lifex,
                s_lifnr for likp-lifnr,
                s_wbstk for likpuk-wbstk,
                s_lfdat for likp-lfdat,
                s_matnr for lips-matnr,
                s_werks for lips-werks.

selection-screen end of block block1.

at selection-screen output.
  perform set_listbox_rlvl.

at selection-screen.

initialization.
  perform initial_data.

start-of-selection.
  perform check_data.
  perform get_data.
  if it_data[] is initial.
    message s999 with text-m01.
  else.
    perform display_data.
  endif.
*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form get_data.
  data: lt_celltab type lvc_t_styl,
        w_celltab type lvc_s_styl,
        l_index like sy-tabix.

  refresh it_data.

  w_date = sy-datum.
  w_time = sy-uzeit.

  select a~vbeln a~lifex zutruckno c~werks a~lifnr
         a~lfdat b~wbstk zutime
         zudate zuuser zucoment
         zctime zcdate zcuser zccoment c~matnr
    into corresponding fields of table it_data
    from likp as a
    inner join likpuk as b
    on a~vbeln = b~vbeln
    inner join lips as c
    on a~vbeln = c~vbeln
    left join ztmm_cast_dellog as d
    on a~vbeln = d~vbeln
    and a~lifex = d~lifex
    where a~vbeln in s_vbeln
     and a~lifex in s_lifex
     and a~lifnr in s_lifnr
*    and  WBSTK  in S_WBSTK
    and a~lfdat in s_lfdat
    and matnr in s_matnr
    and c~werks in s_werks
    and  a~lfart = 'EL'
    and  ( c~werks = 'E001' or c~werks = 'E002' ).

  if s_wbstk is initial.
  else.
    delete it_data where not wbstk in s_wbstk.
  endif.

  sort it_data by vbeln lifex.
  delete adjacent duplicates from it_data
      comparing vbeln lifex.

  loop at it_data.
    move-corresponding it_data to it_output.
    append it_output.
  endloop.
endform.                    "GET_DATA
*
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_rlvl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_listbox_rlvl.


  name = 'P_RLVL'.
  move: space       to value-key,
        ' For all'       to value-text.
  append value to list.
  move: 'A'      to  value-key,
        ' 1 =< Difference < 5001' to value-text.
  append value to list.

  move: 'B'      to  value-key,
          '  5001 =< Difference < 10001' to value-text.
  append value to list.

  move: 'C'      to  value-key,
          ' 10001 =< Difference < 15001' to value-text.
  append value to list.

  move: 'D'      to  value-key,
         ' 15001 =< Difference < 20001' to value-text.
  append value to list.

  move: 'E'      to  value-key,
            ' 20001 =< Difference < 30001' to value-text.
  append value to list.
  move: 'Z'      to  value-key,
            ' 30001 <= Difference' to value-text.
  append value to list.
  move: 'X'      to  value-key,
            ' Difference = 0' to value-text.
  append value to list.

  call function 'VRM_SET_VALUES'
    exporting
      id     = name
      values = list.
endform.                    " SET_LISTBOX_rlvl
*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
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
  if grid_container is initial. "/Not Created Control for ALV GRID
    perform create_container_n_object.
    perform set_attributes_alv_grid.
    perform build_sortcat_display.
    perform exclude_tb_functions.
    perform select_edit_fields.
    perform build_field_catalog using 'IT_OUTPUT'.
    perform assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  else.
    wa_stbl-row = 'X'.
    wa_stbl-col = 'X'.
    call method alv_grid->refresh_table_display
      exporting
        is_stable = wa_stbl.
  endif.

endmodule.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object.
  data:   w_repid like sy-repid.
  create object grid_container
    exporting
      container_name              = wa_custom_control
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  w_repid = sy-repid.
  if sy-subrc ne 0.
    call function 'POPUP_TO_INFORM'
      exporting
        titel = w_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  endif.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  create object alv_grid
    exporting
      i_parent      = grid_container
      i_appl_events = 'X'.

endform.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_attributes_alv_grid.

  clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure

  wa_is_layout-edit       = 'X'.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  wa_is_layout-stylefname = 'CELLTAB'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

endform.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

endform.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
form build_field_catalog using p_itab.
  data: lw_itab type slis_tabname.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  clear: w_cnt, w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_internal_tabname = 'IT_DATA'
*     i_structure_name   = 'ZTMM_CAST_DELLOG'
      i_inclname         = w_repid
    changing
      ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :

                                  'S' 'VBELN'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Delivery',
                                   'E' 'OUTPUTLEN'   '10',

                                 'S' 'LIFEX'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'DCR No',
                                  ' ' 'HOTSPOT'     'X',
                                  'E' 'OUTPUTLEN'   '35',

                                 'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Material no',
                                  ' ' 'HOTSPOT'     ' ',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'ZUTRUCKNO'       ' ',
                                  ' ' 'COLTEXT'     'Trailer no',
                                  'E' 'OUTPUTLEN'   '20',


                                  'S' 'WERKS'       ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '5',

                                 'S' 'LIFNR'       ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',
                                 'S' 'WBSTK'       ' ',
                                  ' ' 'COLTEXT'     'GR status',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'LFDAT'       ' ',
                                  ' ' 'COLTEXT'     'Del Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ZUDATE'       ' ',
                                  ' ' 'COLTEXT'     'Arrival Date',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZUTIME'      ' ',
                                  ' ' 'COLTEXT'     'Arrival Time',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZUUSER'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Arrival User',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZUCOMENT'      ' ',
                                  ' ' 'COLTEXT'     'Arrival Message',
                                  'E' 'OUTPUTLEN'   '150',

                                  'S' 'ZCDATE'       ' ',
                                  ' ' 'COLTEXT'     'Unload Date',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZCTIME'      ' ',
                                  ' ' 'COLTEXT'     'Unload Time',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZCUSER'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Unload User',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ZCCOMENT'      ' ',
                                  ' ' 'COLTEXT'     'Unload Message',
                                  'E' 'OUTPUTLEN'   '150'.

endform.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat structure it_fieldcat
                      using    p_gubun
                               p_field
                               p_value.
  data : l_col(40).
  field-symbols <fs>.

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
endform.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form assign_itab_to_alv.

  call method alv_grid->set_table_for_first_display
    exporting
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
      it_toolbar_excluding = it_exclude
    changing
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_output[].
*               it_sort          = it_sort[].

** ENTER
  call method alv_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Cursor----
  call method alv_grid->register_edit_event
    exporting
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

**ON_HOTSPOT_CLICK

*CALL METHOD alv_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>HOTSPOT_CLICK.

  create object g_event_receiver.
  set handler g_event_receiver->handle_data_changed for alv_grid.
  set handler g_event_receiver->handle_hotspot_click for alv_grid.

  call method cl_gui_control=>set_focus
    exporting
      control = alv_grid.


endform.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exclude_tb_functions.
  data ls_exclude type ui_func.

* Row manipulation
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  append ls_exclude to it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  append ls_exclude to it_exclude.

*  Sort buttons
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
*  APPEND ls_exclude TO it_exclude.
*  ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
*  APPEND ls_exclude TO it_exclude.
**  This excludes all buttons
*  LS_EXCLUDE = '&EXCLALLFC'.
*  APPEND LS_EXCLUDE TO PT_EXCLUDE.
endform.                    " EXCLUDE_TB_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data .
  call screen 0200.
endform.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.
  case ok_code.
    when 'EXIT'.
      leave program.
    when 'BACK'.
      leave to screen 0.
    when 'SAVE'.
      perform save_data.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_edit_fields.
  data: lt_celltab type lvc_t_styl,
        w_celltab type lvc_s_styl,
        l_index type i,
        l_mode type lvc_style.


  loop at it_output..
    l_index = sy-tabix.
    refresh lt_celltab.
    clear: w_celltab, l_mode.
    case it_output-wbstk.
      when 'A'.
        if it_output-zucoment is initial.
          l_mode = cl_gui_alv_grid=>mc_style_enabled.
        else.
          l_mode = cl_gui_alv_grid=>mc_style_disabled.
        endif.
        w_celltab-fieldname = 'ZUTRUCKNO'.
        w_celltab-style = l_mode.
        insert w_celltab into table lt_celltab.
        w_celltab-fieldname = 'ZUCOMENT'.
        w_celltab-style = l_mode.
        insert w_celltab into table lt_celltab.
*        w_celltab-fieldname = 'ZCCOMENT'.
*        w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*        INSERT w_celltab INTO TABLE lt_celltab.
      when 'B' or 'C'.
*        IF it_output-zccoment IS INITIAL.
*          l_mode = cl_gui_alv_grid=>mc_style_enabled.
*        ELSE.
*          l_mode = cl_gui_alv_grid=>mc_style_disabled.
*        ENDIF.

* 07/19/2013 - T00306 Start
*        w_celltab-fieldname = 'ZUTRUCKNO'.
*        w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*        insert w_celltab into table lt_celltab.
* 07/19/2013 - T00306 End
        w_celltab-fieldname = 'ZUCOMENT'.
        w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        insert w_celltab into table lt_celltab.
*        w_celltab-fieldname = 'ZCCOMENT'.
*        w_celltab-style = l_mode.
*        INSERT w_celltab INTO TABLE lt_celltab.
      when others.
* 07/19/2013 - T00306 Start
*        w_celltab-fieldname = 'ZUTRUCKNO'.
*        w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
*        insert w_celltab into table lt_celltab.
* 07/19/2013 - T00306 End
        w_celltab-fieldname = 'ZUCOMENT'.
        w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        insert w_celltab into table lt_celltab.
        w_celltab-fieldname = 'ZCCOMENT'.
        w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        insert w_celltab into table lt_celltab.
    endcase.
    if it_output-zccoment is initial.
      l_mode = cl_gui_alv_grid=>mc_style_enabled.
    else.
      l_mode = cl_gui_alv_grid=>mc_style_disabled.
    endif.
    w_celltab-fieldname = 'ZCCOMENT'.
    w_celltab-style = l_mode.
    insert w_celltab into table lt_celltab.

    w_celltab-fieldname = 'VBELN'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
* 07/19/2013 - T00306 Start
    w_celltab-fieldname = 'MATNR'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
* 07/19/2013 - T00306 End
    w_celltab-fieldname = 'LIFEX'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'LFDAT'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'LIFNR'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'WBSTK'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'ZUDATE'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'ZUTIME'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'ZUUSER'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'ZCDATE'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'ZCTIME'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'ZCUSER'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.
    w_celltab-fieldname = 'WERKS'.
    w_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    insert w_celltab into table lt_celltab.

    insert lines of lt_celltab into table it_output-celltab.
    modify it_output index l_index.

  endloop.
endform.                    " SELECT_EDIT_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_data .
  refresh: it_data.
  clear: it_data.
  data l_valid type char01.

  call method alv_grid->check_changed_data
    importing
      e_valid = l_valid.

* 07/19/2013 - T00306 Start
  data: lt_cast_dellog like table of ztmm_cast_dellog with header line.

  loop at it_output.
    move-corresponding it_output to lt_cast_dellog.
    append lt_cast_dellog.
  endloop.
  delete ztmm_cast_dellog from table lt_cast_dellog.
  insert ztmm_cast_dellog from table lt_cast_dellog
                          accepting duplicate keys.
* 07/19/2013 - T00306 Start

  if sy-subrc = 0.
    message s009 with 'Successfully Updated'.
  elseif sy-subrc = 4.
    message s009 with 'Duplicate data Updated'..
  else.
    message s009 with 'Updating Error'.
  endif.
endform.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUTPUT_VBELN  text
*----------------------------------------------------------------------*
form call_transaction  using  p_lifex.
  data: bdcdata_wa  type bdcdata,
         bdcdata_tab type table of bdcdata.

  data opt type ctu_params.

  clear bdcdata_wa.
  bdcdata_wa-program  = 'WS_MONITOR_INB_DEL_FREE'.
  bdcdata_wa-dynpro   = '1000'.
  bdcdata_wa-dynbegin = 'X'.
  append bdcdata_wa to bdcdata_tab.

  clear bdcdata_wa.
  bdcdata_wa-fnam = 'BDC_CURSOR'.
  bdcdata_wa-fval = 'IT_LIFEX-LOW'.
  append bdcdata_wa to bdcdata_tab.

  clear bdcdata_wa.
  bdcdata_wa-fnam = 'BDC_OKCODE'.
  bdcdata_wa-fval = '=ONLI'.
  append bdcdata_wa to bdcdata_tab.


  clear bdcdata_wa.
  bdcdata_wa-fnam = 'IT_LFDAT-LOW'.
  bdcdata_wa-fval = '  '.
  append bdcdata_wa to bdcdata_tab.


  clear bdcdata_wa.
  bdcdata_wa-fnam = 'IT_LFDAT-HIGH'.
  bdcdata_wa-fval = '  '.
  append bdcdata_wa to bdcdata_tab.


  clear bdcdata_wa.
  bdcdata_wa-fnam = 'IT_LIFEX-LOW'.
  bdcdata_wa-fval = p_lifex.
  append bdcdata_wa to bdcdata_tab.


* bdcdata_wa-program  = 'SAPMSSY0'.
*    bdcdata_wa-dynpro   = '0120'.
**    bdcdata_wa-dynbegin = 'X'.
*    APPEND bdcdata_wa TO bdcdata_tab.
*
*    CLEAR bdcdata_wa.
*    bdcdata_wa-fnam = 'BDC_CURSOR'.
*    bdcdata_wa-fval = '04/03'.
*    APPEND bdcdata_wa TO bdcdata_tab.
*
*     CLEAR bdcdata_wa.
*    bdcdata_wa-fnam = 'BDC_OKCODE'.
*    bdcdata_wa-fval = '=&F03'.
*    APPEND bdcdata_wa TO bdcdata_tab.


* CLEAR bdcdata_wa.
*    bdcdata_wa-program  = 'WS_MONITOR_INB_DEL_FREE'.
*    bdcdata_wa-dynpro   = '1000'.
**    bdcdata_wa-dynbegin = 'X'.
*    APPEND bdcdata_wa TO bdcdata_tab.
*
*    CLEAR bdcdata_wa.
*    bdcdata_wa-fnam = 'BDC_OKCODE'.
*    bdcdata_wa-fval = '/EE'.
*    APPEND bdcdata_wa TO bdcdata_tab.
*
*
*      CLEAR bdcdata_wa.
*    bdcdata_wa-fnam = 'BDC_CURSOR'.
*    bdcdata_wa-fval = 'IF_VSTEL-LOW'.
*    APPEND bdcdata_wa TO bdcdata_tab.

  opt-dismode = 'E'.
  opt-defsize = 'X'.

  call transaction 'VL06IF' using bdcdata_tab options from opt.

endform.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  INITIAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initial_data .
  s_lifnr-sign = 'I'.
  s_lifnr-option = 'EQ'.
  s_lifnr-low = 'AG5M'.
  append s_lifnr.
  s_lifnr-low = 'AB9N'.
  append s_lifnr.
  s_lifnr-low = 'AC34'.
  append s_lifnr.
  s_lifnr-low = 'AKJM'.
  append s_lifnr.
  s_lifnr-low = 'AKMY'.
  append s_lifnr.

endform.                    " INITIAL_DATA
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_data .
  loop at s_lfdat.
    if s_lfdat-low < '20130401'.
      message e009 with 'Delivery date must be after 04/01/2013'.
    endif.
  endloop.
endform.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  error_message_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PR_DATA_CHANGED  text
*      -->PS_MOD_CELLS     text
*      -->P_MSGNO          text
*      -->P_MSGTY          text
*      -->P_MSGV1          text
*      -->P_FIELDNAME      text
*----------------------------------------------------------------------*
form error_message_display using pr_data_changed type ref to
                                         cl_alv_changed_data_protocol
                                 ps_mod_cells type lvc_s_modi
                                 p_msgno
                                 p_msgty
                                 p_msgv1
                                 p_fieldname.

* Error Message Display
  call method pr_data_changed->add_protocol_entry
    exporting
      i_msgid     = 'ZMMM'
      i_msgno     = p_msgno
      i_msgty     = p_msgty
      i_msgv1     = p_msgv1
      i_fieldname = p_fieldname
      i_row_id    = ps_mod_cells-row_id.

endform.                    "error_message_display
*&---------------------------------------------------------------------*
*&      Form  set_focus_on_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GRID       text
*      -->P_INDEX      text
*      -->P_FIELDNAME  text
*----------------------------------------------------------------------*
form set_focus_on_grid using    p_grid  type ref to cl_gui_alv_grid
                                p_index
                                p_fieldname.

  data: ls_row  type lvc_s_row,
        ls_col  type lvc_s_col.

  if p_index is initial.
    exit.
  endif.


  ls_row-index = p_index.
  ls_col-fieldname = p_fieldname.

  call method p_grid->set_focus
    exporting
      control = p_grid.

  call method p_grid->set_current_cell_via_id
    exporting
      is_row_id    = ls_row
      is_column_id = ls_col.

endform.                    "set_focus_on_grid
