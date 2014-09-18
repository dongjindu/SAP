*&---------------------------------------------------------------------*
*& Report  ZMMT_SA_PERIOD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmmt_sa_period  no standard page heading
                     line-size 132
                     line-count 64(1)
                     message-id zmmm.
type-pools: slis, vrm.
tables : ekbe.

data : begin of itab occurs 0,
        ebeln    like ekbe-ebeln,
        ebelp    like ekbe-ebelp,
        vgabe    like ekbe-vgabe,
        gjahr    like ekbe-gjahr,
        belnr    like ekbe-belnr,
        buzei    like ekbe-buzei,
        matnr    like ekbe-matnr,
        bwart    like ekbe-bwart,
        budat    like ekbe-budat,
        menge    like ekbe-menge,
        dmbtr    like ekbe-dmbtr,
        waers    like ekbe-waers,
        shkzg    like ekbe-shkzg,
       end of itab.


data: ok_code like sy-ucomm,
      w_repid like sy-repid,
      w_cnt   type i.


data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail
data: wa_save    type c   value 'A',   "for Parameter I_SAVE
      wa_variant type disvariant.      "for parameter IS_VARIANT

data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname    like line of it_fieldname.
data: wa_custom_control type        scrfname value 'ALV_CONTAINER',
      alv_grid          type ref to cl_gui_alv_grid,
      g_docking_container    type ref to cl_gui_docking_container.









selection-screen begin of block b1 with frame title text-001.
select-options : s_ebeln  for ekbe-ebeln,
               s_matnr  for ekbe-matnr,
               s_budat  for ekbe-budat.


selection-screen end of block b1.









***********************************************************************
* START-OF-SELECTION
***********************************************************************
start-of-selection.
  perform get_data.

  if itab[] is initial.
    message i009 with 'No Data'.
    exit.
  else.
    perform display_data.
  endif.


end-of-selection.

*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data .

  data : lt_tab like zmmt_sa_period occurs 0 with header line.


  refresh lt_tab.
  select * into corresponding fields of table lt_tab
     from zmmt_sa_period
    where ebeln in s_ebeln
      and matnr in s_matnr
      and datab in s_budat.

  if not lt_tab[] is initial.
    select * into corresponding fields of table  itab
       from ekbe
     for all entries in lt_tab
     where ebeln = lt_tab-ebeln
       and matnr = lt_tab-matnr
       and budat >= lt_tab-datab
       and budat <  lt_tab-datbi
       and VGABE = '1'.
  endif.
*
*
  loop at itab.
    if itab-bwart = '102' or itab-bwart = '122'.
      itab-menge = itab-menge * -1.
      itab-dmbtr = itab-dmbtr * -1.
      modify itab.
    endif.
  endloop.

*

endform.                    " GET_DATA
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
*    perform build_sortcat_display.
    perform build_field_catalog using 'ITAB'.
    perform assign_itab_to_alv.
*    PERFORM sssign_event_9000.  ELSE.
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


*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
endform.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0304   text
*----------------------------------------------------------------------*
form build_field_catalog  using   p_itab.

  data: lw_itab type slis_tabname,
          lw_waers like t001-waers,
          l_rqty(9),
          l_datum(8),
          l_cn(2) type n.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.


  w_repid = sy-repid.


  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_structure_name   = 'EKBE'
*      i_bypassing_buffer = 'X'
*      i_inclname         = w_repid
    changing
      ct_fieldcat        = it_fieldname.


*  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
*    exporting
*      i_program_name     = w_repid
*      i_internal_tabname = 'ITAB'
*      i_bypassing_buffer = 'X'
*      i_inclname         = w_repid
*    changing
*      ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :

                                 'S' 'EBELN'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Document Number',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'EBELP'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Item Number',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'VGABE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Transaction',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'GJAHR'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Year',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'BELNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material Document',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'BUZEI'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Item',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'MATNR'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material Number',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'BWART'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Movement Type',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'BUDAT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Posting Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MENGE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Quantity',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'DMBTR'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Amount',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'WAERS'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Currency Key',
                                 'E' 'OUTPUTLEN'   '18',

                                 'S' 'SHKZG'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Debit/Credit Indicator',
                                 'E' 'OUTPUTLEN'   '5'.


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
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
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
*      -->P_0463   text
*      -->P_0464   text
*      -->P_0465   text
*----------------------------------------------------------------------*
form setting_fieldcat      tables   p_fieldcat structure it_fieldcat
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
    when 'EXIT'.
      leave program.
    when 'BACK'.
      leave to screen 0.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT
