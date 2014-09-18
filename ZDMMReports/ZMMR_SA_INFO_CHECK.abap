*&---------------------------------------------------------------------*
*& Report  ZMMR_SA_INFO_CHECK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zmmr_sa_info_check  no standard page heading
                                    line-size 132
                                    line-count 64(1)
                                    message-id zmmm.

tables: ekko,ekpo,mara,a016,a018,konp.


data : begin of it_tab occurs 0,
        ebeln      like ekko-ebeln,
        ebelp      like ekpo-ebelp,
        bsart      like ekko-bsart,
        ernam      like ekko-ernam,
        lifnr      like ekko-lifnr,
        bedat      like ekko-bedat,
        matnr      like ekpo-matnr,
        txz01      like ekpo-txz01,
        infnr      like ekpo-infnr,
        sapr       like konp-kbetr,
        inpr       like konp-kbetr,
        waers      like ekko-waers,
        prdiff     type konp-kbetr,
       end of it_tab.


data : begin of it_a016 occurs 0.
        include structure a016.
data : kbetr   like konp-kbetr.
data : end of it_a016.
*
data : begin of it_a018 occurs 0.
        include structure a018.
data : kbetr   like konp-kbetr.
data : end of it_a018.


data : ok_code like sy-ucomm,
       w_repid like sy-repid,
       w_cnt   type i.


*alv
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
*parameters : p_aedat like ekko-aedat default sy-datum.
select-options : s_aedat for ekko-aedat, " no-extension no intervals,
                 s_ebeln for ekko-ebeln,
                 s_matnr for ekpo-matnr.
selection-screen skip.
selection-screen begin of line.
selection-screen comment 1(10) text-m01.
parameters : p_nall radiobutton group gr1.
selection-screen comment 20(10) text-m02 for field p_nall.
parameters : p_all radiobutton group gr1 default 'X'.
selection-screen comment 35(10) text-m03 for field p_all.
selection-screen end of line.
selection-screen end of block b1.









start-of-selection.

  perform get_data1.

  if it_tab[] is initial.
    message i009 with text-005.
    exit.
  endif.
*
end-of-selection.
  perform data_display.


*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data1 .




  select a~ebeln b~ebelp a~bsart a~ernam a~lifnr a~bedat
         b~matnr b~txz01 a~waers b~infnr
     into corresponding fields of table it_tab
    from ekko as a inner join ekpo as b
         on a~ebeln eq b~ebeln
                   inner join mara as c
         on b~matnr eq c~matnr
      where a~bstyp = 'L'
*        and a~aedat in  s_aedat
        and a~ebeln in s_ebeln
        and b~matnr in s_matnr
        and b~loekz <> 'L'
        and b~elikz <> 'X'
        and c~mtart = 'ROH'.

  sort it_tab by ebeln ebelp.

*
* SA Iiitial period data
  if not it_tab[] is initial.
    refresh it_a016. clear it_a016.
    select * into corresponding fields of table it_a016
      from a016
     for all entries in it_tab
    where evrtn = it_tab-ebeln
      and evrtp = it_tab-ebelp
      and datbi >= s_aedat-low
      and datab <= s_aedat-low
      and kschl = 'PB00'.
    sort it_a016 by datab ascending datbi knumh datab.
  endif.
*
  if not it_a016[] is initial.
    loop at it_a016.
      clear konp.
      select single * from konp
         where knumh = it_a016-knumh.
      it_a016-kbetr = konp-kbetr / konp-kpein.
      modify it_a016.
    endloop.
  endif.
*

  loop at it_tab.
    read table it_a016 with key evrtn = it_tab-ebeln
                                evrtp = it_tab-ebelp.
    if sy-subrc = 0.
      move: it_a016-kbetr    to it_tab-sapr.
      modify it_tab transporting sapr
               where ebeln = it_tab-ebeln
                 and ebelp = it_tab-ebelp.
    endif.
  endloop.

*info record initial period data
  if not it_tab[] is initial.
    refresh it_a018. clear it_a018.
    select * into corresponding fields of table it_a018
      from a018
     for all entries in it_tab
    where matnr = it_tab-matnr
      and lifnr = it_tab-lifnr
      and datbi >= s_aedat-low
      and datab <= s_aedat-low
      and kschl = 'PB00'.
    sort it_a018 by datab ascending datbi knumh datab.
  endif.
*
  if not it_a018[] is initial.
    loop at it_a018.
      clear konp.
      select single * from konp
         where knumh = it_a018-knumh.
      it_a018-kbetr =  konp-kbetr / konp-kpein.
      modify it_a018.
    endloop.
  endif.
**
  loop at it_tab.
    read table it_a018 with key matnr = it_tab-matnr
                                lifnr = it_tab-lifnr.
    if sy-subrc = 0.
      move: it_a018-kbetr    to it_tab-inpr.

      it_tab-prdiff = it_tab-sapr - it_tab-inpr.

      modify it_tab transporting inpr prdiff
                where matnr = it_tab-matnr
                  and lifnr = it_tab-lifnr
                  and ebeln = it_tab-ebeln
                  and ebelp = it_tab-ebelp.
    endif.
  endloop.

  sort it_tab by  ebeln ebelp.

  if p_nall = 'X'.
    delete it_tab where prdiff = 0.
  endif.


endform.                    " GET_DATA1
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form data_display .
  call screen 0200.
endform.                    " DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.
  data: title  type string.

  set pf-status 'ST200'.

  title = 'T200'.
*  set titlebar 'T200'.
  set titlebar title with text-100.

endmodule.                 " STATUS_0200  OUTPUT
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

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_repid.

  lw_itab = 'IT_TAB'.
  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_structure_name   = 'ZSMM_SA_INFO_CHECK'
*     i_internal_tabname = lw_itab
      i_bypassing_buffer = 'X'
      i_inclname         = w_repid
    changing
      ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :
                                 'S' 'EBELN'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA NO',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'EBELP'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'L/N',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'BSART'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Doc.Ty',
                                 'E' 'OUTPUTLEN'   '5',

                                 'S' 'ERNAM'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Creator',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'LIFNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Vendor',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'BEDAT'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Doc.Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MATNR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material',
                                 'E' 'OUTPUTLEN'   '18',

                                 'S' 'TXZ01'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Mat.Description',
                                 'E' 'OUTPUTLEN'   '30',

                                 'S' 'INFNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Info Record',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'SAPR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'SA PRICE',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'INPR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'INFO PRICE',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'WAERS'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Currency',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'PRDIFF'      '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Price Diff',
                                 'E' 'OUTPUTLEN'   '15'.


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
      it_outtab            = it_tab[].
endform.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_ENDFORM  text
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
