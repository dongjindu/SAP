************************************************************************
* Program Name      : ZMMR_SA_INV_CHECK
* Creation Date     : 08/12/2014
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************
report  zmmr_sa_inv_check  no standard page heading
                                    line-size 132
                                    line-count 64(1)
                                    message-id zmmm.


tables : ekko,ekpo,mara,ekbe,a018,a016,konp.

data: begin of it_atab occurs 0,
       ebeln    like ekko-ebeln,
       ebelp    like ekpo-ebelp,
       bsart    like ekko-bsart,
       ernam    like ekko-ernam,
       lifnr    like ekko-lifnr,
       bedat    like ekko-bedat,
       aedat    like ekko-aedat,
       kdatb    like ekko-kdatb,
       kdate    like ekko-kdate,
       matnr    like ekpo-matnr,
       txz01    like ekpo-txz01,
       datab    like a016-datab,
       datbi    like a016-datbi,
       kbetr    like konp-kbetr,
       datab2   like a018-datab,
       datbi2   like a018-datbi,
       kbetr2   like konp-kbetr,
       ddiff    type p decimals 0,
      end of it_atab.
*
data : begin of it_a016 occurs 0.
        include structure a016.
data : kbetr   like konp-kbetr.
data : end of it_a016.
*
data : begin of it_a018 occurs 0.
        include structure a018.
data : kbetr   like konp-kbetr.
data : end of it_a018.
*
data: begin of it_btab occurs 0,
        ebeln   like ekbe-ebeln,
        ebelp   like ekbe-ebelp,
        datab2  like a018-datab,   "A018
        datab   like a016-datab,   "A016
        lifnr   like ekko-lifnr,
        gjahr   like ekbe-gjahr,
        belnr   like ekbe-belnr,
        buzei   like ekbe-buzei,
        matnr   like ekbe-matnr,
        txz01   like ekpo-txz01,
        bwart   like ekbe-bwart,
        waers   like ekbe-waers,
        shkzg   like ekbe-shkzg,
        budat   like ekbe-budat,
        menge   like ekbe-menge,
        dmbtr   like ekbe-dmbtr,
        menget  like ekbe-menge,
        dmbtrt  like ekbe-dmbtr,
        dmbtr2  like ekbe-dmbtr,
        dmbtr3  like ekbe-dmbtr,
      "
        dmbtra  like ekbe-dmbtr,   "AVG PRICE
        kbetr2  like konp-kbetr,
        kbetr   like konp-kbetr,
        kbetrba like konp-kbetr,
        kbetrcb like konp-kbetr,
        lfgja   like ekbe-lfgja,
        lfbnr   like ekbe-lfbnr,
        lfpos   like ekbe-lfpos,
      end of it_btab.
*
data: begin of it_btabe occurs 0,
        ebeln   like ekbe-ebeln,
        ebelp   like ekbe-ebelp,
*        matnr   like ekbe-matnr,
*        shkzg   like ekbe-shkzg,
        menge   like ekbe-menge,
        dmbtr   like ekbe-dmbtr,
      "
        lfgja   like ekbe-lfgja,
        lfbnr   like ekbe-lfbnr,
        lfpos   like ekbe-lfpos,
      end of it_btabe.
**
data : begin of it_ctab occurs 0,
        ebeln   like ekko-ebeln,
        ebelp   like ekpo-ebelp,
        lifnr   like ekko-lifnr,
        gjahr   like ekbe-gjahr,
        belnr   like ekbe-belnr,
        buzei   like ekbe-buzei,
        matnr   like ekbe-matnr,
        txz01   like ekpo-txz01,
        bwart   like ekbe-bwart,
        waers   like ekbe-waers,
        shkzg   like ekbe-shkzg,
        budat   like ekbe-budat,
        menge   like ekbe-menge,
        dmbtr   like ekbe-dmbtr,
        menget  like ekbe-menge,
        dmbtrt  like ekbe-dmbtr,
        dmbtr2  like ekbe-dmbtr,
        dmbtr3  like ekbe-dmbtr,
        dmbtra  like ekbe-dmbtr,
        kbetrs  like konp-kbetr,
        kbetri  like konp-kbetr,
        kbetrba like konp-kbetr,
        kbetrcb like konp-kbetr,
        lfgja   like ekbe-lfgja,
        lfbnr   like ekbe-lfbnr,
        lfpos   like ekbe-lfpos,
      end of it_ctab.

data : begin of  gt_qty occurs 0,
         ebeln   like ekbe-ebeln,
         ebelp   like ekbe-ebelp,
         lfgja   like ekbe-lfgja,
         lfbnr   like ekbe-lfbnr,
         lfpos   like ekbe-lfpos,
         menge   like ekbe-menge,
         dmbtr   like ekbe-dmbtr,
       end of gt_qty.
data : begin of  gt_dqty occurs 0,
         ebeln   like ekbe-ebeln,
         ebelp   like ekbe-ebelp,
         lfgja   like ekbe-lfgja,
         lfbnr   like ekbe-lfbnr,
         lfpos   like ekbe-lfpos,
         vgabe   like ekbe-vgabe,
         menge   like ekbe-menge,
         dmbtr   like ekbe-dmbtr,
       end of gt_dqty.

data: w_dest(10),
      ok_code like sy-ucomm,
      w_repid like sy-repid,
      w_tabix like sy-tabix,
      w_cnt   type i,
      w_check type c.

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
select-options : s_aedat    for ekko-aedat modif id ab1,
                 s_ebeln    for ekko-ebeln modif id ab1,
                 s_matnr    for ekpo-matnr modif id ab1,
                 s_lifnr    for ekko-lifnr modif id ab1,
                 s_budat    for ekbe-budat modif id ab2.  "only p_r3.
selection-screen uline.

*
parameters : p_c1 as checkbox,
             p_c2 as checkbox,
             p_c3 as checkbox,
             p_c4 as checkbox default 'X',
             p_c5 as checkbox default 'X'.
*
parameters : p_r1 radiobutton group r1 user-command fcode
                                       default 'X',
             p_r2 radiobutton group r1 ,
             p_r3 radiobutton group r1 .
selection-screen end of block b1.

*

initialization.
  concatenate sy-datum(4)'0101' into s_aedat-low.
  s_aedat-high = sy-datum.
  append s_aedat.

at selection-screen output.
  perform screen_output.

start-of-selection.
  perform check_condition using w_check.
  if w_check = 'X'.
    exit.
  endif.

  if p_r1 = 'X'.
    perform get_data1.
  elseif p_r2 = 'X'.
    perform get_data1.
    perform get_se_data2.
  elseif p_r3 = 'X'.
    perform get_data3.
  endif.
*
  if p_r1 = 'X'.
    if it_atab[] is initial.
      message i009 with text-005.
      exit.
    endif.
  elseif p_r2 = 'X'.
    if it_btab[] is initial.
      message i009 with text-005.
      exit.
    endif.
  elseif p_r3 = 'X'.
    if it_ctab[] is initial.
      message i009 with text-005.
      exit.
    endif.
  endif.

*
end-of-selection.
  perform data_display.



*
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_condition using p_check.

  if p_r1 = 'X' or p_r2 = 'X'.
    if s_aedat is initial.
      message i009 with text-010.
      p_check = 'X'.
    endif.
  endif.

  if p_r3 = 'X'.
    if s_budat is initial.
      message i009 with text-011.
      p_check = 'X'.
    endif.
  endif.

endform.                    " CHECK_CONDITION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data1 .


* BASIC CONDITION.
  if p_c2 = 'X' and p_c3 = 'X'.
    refresh it_atab.  clear it_atab.
    select a~ebeln b~ebelp a~bsart a~ernam a~lifnr a~bedat
           a~aedat a~kdatb a~kdate b~matnr b~txz01
        into corresponding fields of table it_atab
      from ekko as a inner join ekpo as b
         on a~ebeln eq b~ebeln
                   inner join mara as c
         on b~matnr eq c~matnr
      where  a~bstyp = 'L'
        and a~aedat in s_aedat
        and a~ebeln in s_ebeln
        and a~lifnr in s_lifnr
        and b~matnr in s_matnr
        and b~loekz <> 'L'
        and b~elikz <> 'X'
        and c~mtart = 'ROH'.
  elseif p_c3 = 'X'.
    refresh it_atab.  clear it_atab.
    select a~ebeln b~ebelp a~bsart a~ernam a~lifnr a~bedat
           a~aedat a~kdatb a~kdate b~matnr b~txz01
        into corresponding fields of table it_atab
      from ekko as a inner join ekpo as b
         on a~ebeln eq b~ebeln
                   inner join mara as c
         on b~matnr eq c~matnr
      where  a~bstyp = 'L'
        and a~aedat in s_aedat
        and a~ebeln in s_ebeln
        and a~lifnr in s_lifnr
        and b~matnr in s_matnr
*        and b~loekz <> 'L'
        and b~elikz <> 'X'
        and c~mtart = 'ROH'.

  elseif p_c2 = 'X'.
    refresh it_atab.  clear it_atab.
    select a~ebeln b~ebelp a~bsart a~ernam a~lifnr a~bedat
           a~aedat a~kdatb a~kdate b~matnr b~txz01
        into corresponding fields of table it_atab
      from ekko as a inner join ekpo as b
         on a~ebeln eq b~ebeln
                   inner join mara as c
         on b~matnr eq c~matnr
      where  a~bstyp = 'L'
        and a~aedat in s_aedat
        and a~ebeln in s_ebeln
        and a~lifnr in s_lifnr
        and b~matnr in s_matnr
        and b~loekz <> 'L'
*        and b~elikz <> 'X'
        and c~mtart = 'ROH'.
  elseif p_c2 = '' and p_c3 = ' '.
    refresh it_atab.  clear it_atab.
    select a~ebeln b~ebelp a~bsart a~ernam a~lifnr a~bedat
           a~aedat a~kdatb a~kdate b~matnr b~txz01
        into corresponding fields of table it_atab
      from ekko as a inner join ekpo as b
         on a~ebeln eq b~ebeln
                   inner join mara as c
         on b~matnr eq c~matnr
      where  a~bstyp = 'L'
        and a~aedat in s_aedat
        and a~ebeln in s_ebeln
        and a~lifnr in s_lifnr
        and b~matnr in s_matnr
*        and b~loekz <> 'L'
*        and b~elikz <> 'X'
        and c~mtart = 'ROH'.
  endif.
  sort it_atab by  ebeln ebelp.
*
* SA Iiitial period data
  if not it_atab[] is initial.
    refresh it_a016. clear it_a016.
    select * into corresponding fields of table it_a016
      from a016
     for all entries in it_atab
    where evrtn = it_atab-ebeln
      and evrtp = it_atab-ebelp
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
  loop at it_atab.
    read table it_a016 with key evrtn = it_atab-ebeln
                                evrtp = it_atab-ebelp.
    if sy-subrc = 0.
      move: it_a016-kbetr    to it_atab-kbetr,
            it_a016-datbi    to it_atab-datbi,
            it_a016-datab    to it_atab-datab.
      modify it_atab.
    endif.
  endloop.

  sort it_atab by  ebeln ebelp.
*
*info record initial period data
  if not it_atab[] is initial.
    refresh it_a018. clear it_a018.
    select * into corresponding fields of table it_a018
      from a018
     for all entries in it_atab
    where matnr = it_atab-matnr
      and lifnr = it_atab-lifnr
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
*
  loop at it_atab.
    read table it_a018 with key matnr = it_atab-matnr
                                lifnr = it_atab-lifnr.
    if sy-subrc = 0.
      move: it_a018-kbetr    to it_atab-kbetr2,
            it_a018-datbi    to it_atab-datbi2,
            it_a018-datab    to it_atab-datab2.

      " sa_frice_from - info_price_from
      it_atab-ddiff = it_atab-datab - it_atab-datab2.

      modify it_atab.
    endif.
  endloop.

  sort it_atab by  ebeln ebelp.
*

  if p_c1 = ''.
    delete it_atab where ddiff = 0.
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
  set pf-status 'ST200'.
  if p_r1 = 'X'.
    set titlebar 'T201'.
  elseif p_r2 = 'X'.
    set titlebar 'T202'.
  elseif p_r3 = 'X'.
    set titlebar 'T203'.
  endif.


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
    if p_r1 = 'X'.
      perform build_field_catalog.
      perform assign_itab_to_alv.
    elseif p_r2 = 'X'.
      perform build_field_catalog2.
      perform assign_itab_to_alv2.
    elseif p_r3 = 'X'.
      perform build_field_catalog3.
      perform assign_itab_to_alv3.
    endif.

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
*      -->P_0615   text
*----------------------------------------------------------------------*
form build_field_catalog.

  data: lw_itab type slis_tabname,
         lw_waers like t001-waers,
         l_rqty(9),
         l_datum(8),
         l_cn(2) type n.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.

  lw_itab = 'IT_ATAB'.
  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
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

                                 'S' 'AEDAT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Createion Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'KDATB'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA H.From',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'KDATE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA H.From',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MATNR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material',
                                 'E' 'OUTPUTLEN'   '18',

                                 'S' 'TXZ01'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Mat.Description',
                                 'E' 'OUTPUTLEN'   '30',

                                 'S' 'DATAB'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA_PRICE_FROM',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'DATBI'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA_PRICE_TO',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'KBETR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'SA PRICE',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'DATAB2'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'INFO_PRICE_FROM',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'DATBI2'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'      'INFO_PRICE_TO',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'KBETR2'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'INFO PRICE',
                                 'E' 'OUTPUTLEN'   '15',


                                 'S' 'DDIFF'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Days Diff',
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
      it_outtab            = it_atab[].
endform.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0777   text
*      -->P_0778   text
*      -->P_0779   text
*----------------------------------------------------------------------*
form setting_fieldcat     tables   p_fieldcat structure it_fieldcat
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
*&---------------------------------------------------------------------*
*&      Form  GET_SE_DATA2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_se_data2 .

  data : lt_tab  like ekbe occurs 0 with header line,
         lt_stab like ekbe occurs 0 with header line.
  data : w_date  like sy-datum.

*

  delete it_atab where ddiff = 0.  " DAYS DIFF = '0' DELETE
  sort it_atab by  ebeln ebelp.

  if not it_atab[] is initial.
    refresh lt_tab. clear lt_tab.
    select * into corresponding fields of table lt_tab
       from ekbe
     for all entries in it_atab
      where ebeln   = it_atab-ebeln
        and matnr   = it_atab-matnr
        and budat  >= it_atab-datab2
        and budat   < it_atab-datab
        and vgabe   = '1'.
*
    select * appending corresponding fields of table lt_tab
       from ekbeh
      for all entries in it_atab
    where ebeln   = it_atab-ebeln
      and matnr   = it_atab-matnr
      and budat  >= it_atab-datab2
      and budat   < it_atab-datab
      and vgabe   = '1'.


    sort lt_tab by lfgja lfbnr lfpos.
  endif.
*
  refresh it_btab. clear it_btab.
  loop at lt_tab.
    move-corresponding lt_tab   to it_btab.
    if lt_tab-shkzg = 'H'.
      it_btab-menge = lt_tab-menge * -1.
      it_btab-dmbtr = lt_tab-dmbtr * -1.
    endif.
    append it_btab.
  endloop.
*
  sort it_btab by ebeln ebelp lfgja lfbnr lfpos.
* collect. 101/102/122/123
  clear : it_btabe[], it_btabe.
  loop at it_btab.
    move-corresponding it_btab   to  it_btabe.
    collect it_btabe.
  endloop.
*
  sort it_btabe by ebeln ebelp lfgja lfbnr lfpos.

*
  sort it_btab by ebeln ebelp lfgja lfbnr lfpos.

  delete adjacent duplicates from it_btab comparing
                    ebeln ebelp gjahr belnr buzei.

  loop at it_btab.
    w_tabix = sy-tabix.
    read table it_btabe with key ebeln = it_btab-ebeln
                                 ebelp = it_btab-ebelp
                                 lfgja = it_btab-gjahr
                                 lfbnr = it_btab-belnr
                                 lfpos = it_btab-buzei
                                      binary search.
    if sy-subrc = 0.
      if it_btabe-menge = 0.
        delete it_btab where lfbnr = it_btabe-lfbnr.
      else.
        move : it_btabe-menge  to it_btab-menge,
               it_btabe-dmbtr  to it_btab-dmbtr.
        modify it_btab index w_tabix.
      endif.
    endif.
  endloop.

*
  if not lt_tab[] is initial.
    refresh lt_stab.  clear lt_stab.
    select * into corresponding fields of table lt_stab
      from ekbe
      for all entries in lt_tab
     where lfgja   = lt_tab-lfgja
       and lfbnr   = lt_tab-lfbnr
       and lfpos   = lt_tab-lfpos
       and vgabe   in ('2','3').

    sort lt_stab by ebeln ebelp matnr vgabe.
  endif.
*
  loop at lt_stab.
    if lt_stab-shkzg = 'H'.
      lt_stab-menge = lt_stab-menge * -1.
      lt_stab-dmbtr = lt_stab-dmbtr * -1.
      modify lt_stab.
    endif.
  endloop.

* QTY COUNT
  sort lt_stab by ebeln ebelp gjahr belnr buzei.
  refresh gt_qty. clear gt_qty.
  loop at lt_stab.
    move-corresponding lt_stab  to  gt_qty.
    if lt_stab-vgabe = '3'.
      clear lt_stab-menge.
      clear gt_qty-menge.
    endif.
    collect  gt_qty.
  endloop.
  sort gt_qty by ebeln ebelp.
*
* vgabe : 2.3 sum
  sort lt_stab by ebeln ebelp lfgja lfbnr lfpos.
  refresh gt_dqty. clear gt_dqty.
  loop at lt_stab.
    move-corresponding lt_stab  to  gt_dqty.
    collect gt_dqty.
  endloop.
  sort gt_dqty by ebeln ebelp lfgja lfbnr lfpos.

*
  sort gt_qty by ebeln ebelp lfgja lfbnr lfpos.
*
  loop at it_btab.
    w_tabix = sy-tabix.
    read table gt_qty with key ebeln = it_btab-ebeln
                               ebelp = it_btab-ebelp
                               lfgja = it_btab-gjahr
                               lfbnr = it_btab-belnr
                               lfpos = it_btab-buzei
                                      binary search.
    if sy-subrc = 0.
      move : gt_qty-menge   to it_btab-menget,
             gt_qty-dmbtr   to it_btab-dmbtrt.
    endif.

    if it_btab-menget > 0 and it_btab-dmbtrt > 0.
      it_btab-dmbtra =  it_btab-dmbtrt / it_btab-menget.
    endif.

    modify it_btab transporting  menget dmbtrt dmbtra
                where ebeln = it_btab-ebeln
                  and ebelp = it_btab-ebelp
                  and gjahr = it_btab-gjahr
                  and belnr = it_btab-belnr
                  and buzei = it_btab-buzei.
    clear it_btab.
  endloop.

  sort it_btab by ebeln ebelp gjahr belnr buzei.
**
  loop at gt_dqty.
    read table it_btab with key ebeln = gt_dqty-ebeln
                                ebelp = gt_dqty-ebelp
                                gjahr = gt_dqty-lfgja
                                belnr = gt_dqty-lfbnr
                                buzei = gt_dqty-lfpos.
    if sy-subrc = 0.
      if gt_dqty-vgabe = '2'.
        it_btab-dmbtr2 = gt_dqty-dmbtr.
        modify it_btab transporting dmbtr2
               where ebeln = it_btab-ebeln
                 and ebelp = it_btab-ebelp
                 and gjahr = it_btab-lfgja
                 and belnr = it_btab-lfbnr
                 and buzei = it_btab-lfpos.
        clear it_btab.
      else.
        it_btab-dmbtr3 = gt_dqty-dmbtr.
        modify it_btab transporting dmbtr3
               where ebeln = it_btab-ebeln
                 and ebelp = it_btab-ebelp
                 and gjahr = it_btab-lfgja
                 and belnr = it_btab-lfbnr
                 and buzei = it_btab-lfpos.
        clear it_btab.
      endif.
    endif.
  endloop.


*  sort it_atab by  ebeln ebelp.
  loop at it_btab.
    read table it_atab with key ebeln = it_btab-ebeln
                                ebelp = it_btab-ebelp
                                matnr = it_btab-matnr.
    if sy-subrc = 0.
      move :  it_atab-datab     to it_btab-datab,
              it_atab-datab2    to it_btab-datab2,
              it_atab-lifnr     to it_btab-lifnr,
              it_atab-txz01     to it_btab-txz01.
    endif.
    modify it_btab transporting datab datab2 lifnr txz01
               where ebeln = it_btab-ebeln
                 and ebelp = it_btab-ebelp
                 and matnr = it_btab-matnr.
  endloop.

* GR POSTING.
* INFO PRICE
  loop at it_btab.
    w_tabix = sy-tabix.
    clear a018.
    select single * from a018
     where matnr = it_btab-matnr
       and lifnr = it_btab-lifnr
       and datab <= it_btab-budat
       and datbi >= it_btab-budat
       and kschl = 'PB00'.
    if sy-subrc = 0.
      clear konp.
      select single * from konp
        where knumh = a018-knumh.
      if sy-subrc = 0.
        it_btab-kbetr2 = konp-kbetr / konp-kpein.
      endif.
    endif.
    modify it_btab index w_tabix.
  endloop.
*
* SA PRICE
  loop at it_btab.
    w_tabix = sy-tabix.
    clear a016.
    select single * from a016
    where evrtn = it_btab-ebeln
      and evrtp = it_btab-ebelp
      and datab <= it_btab-budat
      and datbi >= it_btab-budat
      and kschl = 'PB00'.
    if sy-subrc = 0.
      clear konp.
      select single * from konp
        where knumh = a016-knumh.
      if sy-subrc = 0.
        it_btab-kbetr = konp-kbetr / konp-kpein.
      endif.
    endif.
    modify it_btab index w_tabix.
  endloop.

* B - A / C - B
  loop at it_btab.
    w_tabix = sy-tabix.
    it_btab-kbetrba = it_btab-kbetr2 - it_btab-dmbtra.
    it_btab-kbetrcb = it_btab-kbetr  - it_btab-kbetr2.

    modify it_btab index w_tabix.
  endloop.
*
  if p_c4 = 'X'.
    delete it_btab where dmbtrt  = 0.
  endif.
*
  if p_c5 = 'X'.
    delete it_btab where kbetrba  = 0.
  endif.
endform.                    " GET_SE_DATA2
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog2 .

  data: lw_itab type slis_tabname,
          lw_waers like t001-waers,
          l_rqty(9),
          l_datum(8),
          l_cn(2) type n.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.

  lw_itab = 'IT_BTAB'.
  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
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

                                 'S' 'DATAB'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA Period From',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'DATAB2'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'SA Period To',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'LIFNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Vendor',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'GJAHR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Year',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'BELNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material Doc',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'BUZEI'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Item',
                                 'E' 'OUTPUTLEN'   '04',

                                 'S' 'MATNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material Number',
                                 'E' 'OUTPUTLEN'   '18',

                                 'S' 'TXZ01'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Mat.Description',
                                 'E' 'OUTPUTLEN'   '30',

                                 'S' 'BWART'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Movement Type',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'WAERS'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Currency Key',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'SHKZG'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'D/C',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'BUDAT'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'GR posting Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MENGE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'QUANTITY'    'EA',
                                 ' ' 'COLTEXT'     'GR Quantity',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'      'GR amount',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'MENGET'       '',
                                 ' ' 'KEY'         ' ',
                                ' ' 'QUANTITY'    'EA',
                                 ' ' 'COLTEXT'     'IV Quantity',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTRT'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'Tot. IV AMT',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTR2'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'Init IV AMT',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTR3'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'Reval AMT',
                                 'E' 'OUTPUTLEN'   '17',


                                 'S' 'DMBTRA'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'IV AVG PRICE(A)',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'KBETR2'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'INFO PRICE(B)',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'KBETR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'SA PRICE(C)',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'KBETRBA'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'B - A',
                                 'E' 'OUTPUTLEN'   '17'.

*                                 'S' 'KBETRCB'       '',
*                                 ' ' 'KEY'         ' ',
*                                 ' ' 'CURRENCY'    'USD',
*                                 ' ' 'COLTEXT'     'C - B',
*                                 'E' 'OUTPUTLEN'   '17'.
endform.                    " BUILD_FIELD_CATALOG2
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv2 .

  call method alv_grid->set_table_for_first_display
    exporting
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    changing
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_btab[].
endform.                    " ASSIGN_ITAB_TO_ALV2
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form screen_output .

  loop at screen.
    if p_r1 <> 'X' and
      screen-group1 = 'AB2'.
      screen-active = '1'.
    endif.
    if p_r2 <> 'X' and
      screen-group1 = 'AB2'.
      screen-active = '1'.
    endif.
    if p_r3 <> 'X' and
      screen-group1 = 'AB2'.
      screen-active = '0'.
    endif.
    if screen-group1 = 'AB2'.
      screen-intensified = '0'.
      modify screen.
      continue.
    endif.
    if screen-group1 = 'AB1'.
      screen-intensified = '0'.
      modify screen.
      continue.
    endif.
    "

    if p_r1 <> 'X' and screen-name = 'P_C1'.
      screen-invisible = '1'.
      modify screen.
    endif.

    if p_r1 = 'X' and screen-name = 'P_C4'.
      screen-invisible = '1'.
      modify screen.
    endif.
*
    if p_r1 = 'X' and screen-name = 'P_C5'.
      screen-invisible = '1'.
      modify screen.
    endif.
*    if p_r1 = 'X' and screen-name = 'P_C2'.
*      screen-invisible = '1'.
*      modify screen.
*    endif.
**
*    if p_r1 = 'X' and screen-name = 'P_C3'.
*      screen-invisible = '1'.
*      modify screen.
*    endif.




  endloop.
*
  if p_r3 = 'X'.
    clear : s_aedat, s_aedat[].
*s_budat
*    clear : s_budat, s_budat[].
*    call function 'RP_CALC_DATE_IN_INTERVAL'
*      exporting
*        date      = sy-datum
*        days      = '00'
*        months    = '03'
*        signum    = '-'
*        years     = '00'
*      importing
*        calc_date = s_budat-low.
*
*    concatenate s_budat-low(6) '01' into s_budat-low.
*
*    s_budat-high = sy-datum.
*    s_budat-sign = 'I'. s_budat-option = 'BT'.
*    append s_budat.
*
*  endif.
**
*  if p_r1 = 'X' or p_r2 = 'X'.
*    clear : s_budat, s_budat[].
*    clear : s_aedat, s_aedat[].
*    concatenate sy-datum(4)'0101' into s_aedat-low.
*    s_aedat-high = sy-datum.
*    s_aedat-sign = 'I'. s_aedat-option = 'BT'.
*    append s_aedat.
  endif.


endform.                    " SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_DATA3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data3 .

  data : lt_stab like ekbe occurs 0 with header line.
  data : lt_ekbe like ekbe occurs 0 with header line.

*
  if p_c2 = 'X' and p_c3 = 'X'.
    refresh it_ctab.  clear it_ctab.
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      into corresponding fields of table it_ctab
      from ekbe as a inner join ekko as b
        on a~ebeln eq b~ebeln
                     inner join ekpo as c
        on b~ebeln eq c~ebeln
                     inner join mara as d
        on c~matnr eq d~matnr
      where  a~budat in s_budat
        and a~matnr in s_matnr
        and a~ebeln in s_ebeln
        and a~vgabe = '1'
        and b~bstyp = 'L'
        and b~aedat in s_aedat
        and b~lifnr in s_lifnr
        and c~loekz <> 'L'   "w_loekz
        and c~elikz <> 'X'    "w_elikz
        and d~mtart = 'ROH'.
*
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      appending corresponding fields of table it_ctab
      from ekbeh as a inner join ekko as b
        on a~ebeln eq b~ebeln
                     inner join ekpo as c
        on b~ebeln eq c~ebeln
                     inner join mara as d
        on c~matnr eq d~matnr
      where  a~budat in s_budat
        and a~matnr in s_matnr
        and a~ebeln in s_ebeln
        and a~vgabe = '1'
        and b~bstyp = 'L'
        and b~aedat in s_aedat
        and b~lifnr in s_lifnr
        and c~loekz <> 'L'   "w_loekz
        and c~elikz <> 'X'    "w_elikz
        and d~mtart = 'ROH'.
  elseif p_c2 = 'X'.
    refresh it_ctab.  clear it_ctab.
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      into corresponding fields of table it_ctab
      from ekbe as a inner join ekko as b
        on a~ebeln eq b~ebeln
                     inner join ekpo as c
        on b~ebeln eq c~ebeln
                     inner join mara as d
        on c~matnr eq d~matnr
      where  a~budat in s_budat
        and a~matnr in s_matnr
        and a~ebeln in s_ebeln
        and a~vgabe = '1'
        and b~bstyp = 'L'
        and b~aedat in s_aedat
        and b~lifnr in s_lifnr
*        and c~loekz <> 'L'   "w_loekz
        and c~elikz <> 'X'    "w_elikz
        and d~mtart = 'ROH'.
*
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      appending corresponding fields of table it_ctab
     from ekbeh as a inner join ekko as b
       on a~ebeln eq b~ebeln
             inner join ekpo as c
       on b~ebeln eq c~ebeln
               inner join mara as d
       on c~matnr eq d~matnr
    where  a~budat in s_budat
      and a~matnr in s_matnr
      and a~ebeln in s_ebeln
      and a~vgabe = '1'
      and b~bstyp = 'L'
      and b~aedat in s_aedat
        and b~lifnr in s_lifnr
*        and c~loekz <> 'L'   "w_loekz
      and c~elikz <> 'X'    "w_elikz
      and d~mtart = 'ROH'.
  elseif p_c3 = 'X'.
    refresh it_ctab.  clear it_ctab.
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      into corresponding fields of table it_ctab
      from ekbe as a inner join ekko as b
        on a~ebeln eq b~ebeln
                     inner join ekpo as c
        on b~ebeln eq c~ebeln
                     inner join mara as d
        on c~matnr eq d~matnr
      where  a~budat in s_budat
        and a~matnr in s_matnr
        and a~ebeln in s_ebeln
        and a~vgabe = '1'
        and b~bstyp = 'L'
        and b~aedat in s_aedat
        and b~lifnr in s_lifnr
        and c~loekz <> 'L'   "w_loekz
*        and c~elikz <> 'X'    "w_elikz
        and d~mtart = 'ROH'.
*
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      appending corresponding fields of table it_ctab
      from ekbeh as a inner join ekko as b
        on a~ebeln eq b~ebeln
                     inner join ekpo as c
        on b~ebeln eq c~ebeln
                     inner join mara as d
        on c~matnr eq d~matnr
      where  a~budat in s_budat
        and a~matnr in s_matnr
        and a~ebeln in s_ebeln
        and a~vgabe = '1'
        and b~bstyp = 'L'
        and b~aedat in s_aedat
        and b~lifnr in s_lifnr
        and c~loekz <> 'L'   "w_loekz
*        and c~elikz <> 'X'    "w_elikz
        and d~mtart = 'ROH'.

  elseif p_c2 = ' ' and p_c3 = ' '.
    refresh it_ctab.  clear it_ctab.
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      into corresponding fields of table it_ctab
      from ekbe as a inner join ekko as b
        on a~ebeln eq b~ebeln
                     inner join ekpo as c
        on b~ebeln eq c~ebeln
                     inner join mara as d
        on c~matnr eq d~matnr
      where  a~budat in s_budat
        and a~matnr in s_matnr
        and a~ebeln in s_ebeln
        and a~vgabe = '1'
        and b~bstyp = 'L'
        and b~aedat in s_aedat
        and b~lifnr in s_lifnr
*        and c~loekz <> 'L'   "w_loekz
*        and c~elikz <> 'X'    "w_elikz
        and d~mtart = 'ROH'.
*
    select a~ebeln c~ebelp b~lifnr a~gjahr a~belnr a~buzei
           a~matnr c~txz01 a~bwart a~waers a~shkzg a~budat
           a~menge a~dmbtr a~lfgja a~lfbnr a~lfpos
      appending corresponding fields of table it_ctab
      from ekbeh as a inner join ekko as b
        on a~ebeln eq b~ebeln
                  inner join ekpo as c
        on b~ebeln eq c~ebeln
                   inner join mara as d
        on c~matnr eq d~matnr
    where a~budat in s_budat
      and a~matnr in s_matnr
      and a~ebeln in s_ebeln
      and a~vgabe = '1'
      and b~bstyp = 'L'
      and b~aedat in s_aedat
      and b~lifnr in s_lifnr
*        and c~loekz <> 'L'   "w_loekz
*        and c~elikz <> 'X'    "w_elikz
      and d~mtart = 'ROH'.
  endif.

*
  sort it_ctab by ebeln ebelp.
*
  loop at it_ctab.
    if it_ctab-shkzg = 'H'.
      it_ctab-menge = it_ctab-menge * -1.
      it_ctab-dmbtr = it_ctab-dmbtr * -1.
      modify it_ctab.
    endif.
  endloop.
  sort it_ctab by ebeln ebelp lfgja lfbnr lfpos.
*

* collect. 101/102/122/123
  clear : it_btabe[], it_btabe.
  loop at it_ctab.
    move-corresponding it_ctab   to  it_btabe.
    collect it_btabe.
  endloop.
*
  sort it_btabe by ebeln ebelp lfgja lfbnr lfpos.
  sort it_ctab by ebeln ebelp lfgja lfbnr lfpos.
*
  delete adjacent duplicates from it_ctab comparing
                      ebeln ebelp gjahr belnr buzei.

* delete duplication
  loop at it_ctab.
    w_tabix = sy-tabix.
    read table it_btabe with key ebeln = it_ctab-ebeln
                                 ebelp = it_ctab-ebelp
                                 lfgja = it_ctab-gjahr
                                 lfbnr = it_ctab-belnr
                                 lfpos = it_ctab-buzei
                                      binary search.
    if sy-subrc = 0.
      if it_btabe-menge = 0.
        delete it_ctab where lfbnr = it_btabe-lfbnr.
      else.
        move : it_btabe-menge  to it_ctab-menge,
               it_btabe-dmbtr  to it_ctab-dmbtr.
        modify it_ctab index w_tabix.
      endif.
    endif.
  endloop.
*
  if not it_ctab[] is initial.
    refresh lt_stab.  clear lt_stab.
    select * into corresponding fields of table lt_stab
      from ekbe
      for all entries in it_ctab
     where lfgja   = it_ctab-lfgja
       and lfbnr   = it_ctab-lfbnr
       and lfpos   = it_ctab-lfpos
       and vgabe   in ('2','3').

    sort lt_stab by ebeln.
  endif.
*
  loop at lt_stab.
    if lt_stab-shkzg = 'H'.
      lt_stab-menge = lt_stab-menge * -1.
      lt_stab-dmbtr = lt_stab-dmbtr * -1.
      modify lt_stab.
    endif.
  endloop.

* QTY COUNT
  sort lt_stab by ebeln ebelp gjahr belnr buzei.
  refresh gt_qty. clear gt_qty.
  loop at lt_stab.
    move-corresponding lt_stab  to  gt_qty.
    if lt_stab-vgabe = '3'.
      clear lt_stab-menge.
      clear gt_qty-menge.
    endif.
    collect gt_qty.
  endloop.
  sort gt_qty by ebeln ebelp.

* vgabe : 2.3 sum
  sort lt_stab by ebeln ebelp lfgja lfbnr lfpos.
  refresh gt_dqty. clear gt_dqty.
  loop at lt_stab.
    move-corresponding lt_stab  to  gt_dqty.
    collect gt_dqty.
  endloop.
  sort gt_dqty by ebeln ebelp lfgja lfbnr lfpos.
*
  sort it_ctab by ebeln ebelp lfgja lfbnr lfpos.
  sort gt_qty by ebeln ebelp lfgja lfbnr lfpos.

*** input main interal table
  loop at it_ctab.
    w_tabix = sy-tabix.
    read table gt_qty with key ebeln = it_ctab-ebeln
                               ebelp = it_ctab-ebelp
                               lfgja = it_ctab-gjahr
                               lfbnr = it_ctab-belnr
                               lfpos = it_ctab-buzei
                               binary search.
    if sy-subrc = 0.
      move : gt_qty-menge   to it_ctab-menget,
             gt_qty-dmbtr   to it_ctab-dmbtrt.
    endif.
    if it_ctab-menget > 0 and it_ctab-dmbtrt > 0.
      it_ctab-dmbtra =  it_ctab-dmbtrt / it_ctab-menget.
    endif.

    modify it_ctab transporting menget dmbtrt dmbtra
                where ebeln = it_ctab-ebeln
                  and ebelp = it_ctab-ebelp
                  and gjahr = it_ctab-gjahr
                  and belnr = it_ctab-belnr
                  and buzei = it_ctab-buzei.
    clear it_ctab.
  endloop.
**vgabe = '2' / '3'
  loop at gt_dqty.
    read table it_ctab with key ebeln = gt_dqty-ebeln
                                ebelp = gt_dqty-ebelp
                                gjahr = gt_dqty-lfgja
                                belnr = gt_dqty-lfbnr
                                buzei = gt_dqty-lfpos.
    if sy-subrc = 0.
      if gt_dqty-vgabe = '2'.
        it_ctab-dmbtr2 = gt_dqty-dmbtr.
        modify it_ctab transporting dmbtr2
               where ebeln = it_ctab-ebeln
                 and ebelp = it_ctab-ebelp
                 and gjahr = it_ctab-lfgja
                 and belnr = it_ctab-lfbnr
                 and buzei = it_ctab-lfpos.
        clear it_ctab.
      else.
        it_ctab-dmbtr3 = gt_dqty-dmbtr.
        modify it_ctab transporting dmbtr3
               where ebeln = it_ctab-ebeln
                 and ebelp = it_ctab-ebelp
                 and gjahr = it_ctab-lfgja
                 and belnr = it_ctab-lfbnr
                 and buzei = it_ctab-lfpos.
        clear it_ctab.
      endif.
    endif.
  endloop.

* GR POSTING.
  loop at it_ctab.
    w_tabix = sy-tabix.
    clear a018.
    select single * from a018
     where matnr = it_ctab-matnr
       and lifnr = it_ctab-lifnr
       and datab <= it_ctab-budat
       and datbi >= it_ctab-budat
       and kschl = 'PB00'.
    if sy-subrc = 0.
      clear konp.
      select single * from konp
        where knumh = a018-knumh.
      if sy-subrc = 0.
        it_ctab-kbetri = konp-kbetr / konp-kpein.
      endif.
    endif.
    modify it_ctab index w_tabix.
  endloop.
*
* SA PRICE
  loop at it_ctab.
    w_tabix = sy-tabix.
    clear a016.
    select single * from a016
    where evrtn = it_ctab-ebeln
      and evrtp = it_ctab-ebelp
      and datab <= it_ctab-budat
      and datbi >= it_ctab-budat
      and kschl = 'PB00'.
    if sy-subrc = 0.
      clear konp.
      select single * from konp
        where knumh = a016-knumh.
      if sy-subrc = 0.
        it_ctab-kbetrs = konp-kbetr / konp-kpein.
      endif.
    endif.
    modify it_ctab index w_tabix.
  endloop.

* B - A / C - B
  loop at it_ctab.
    w_tabix = sy-tabix.
    it_ctab-kbetrba = it_ctab-kbetrs - it_ctab-dmbtra.
    it_ctab-kbetrcb = it_ctab-kbetri - it_ctab-kbetrs.

    modify it_ctab index w_tabix.
  endloop.
*
  if p_c4 = 'X'.
    delete it_ctab where dmbtrt  = 0.
  endif.
*
  if p_c5 = 'X'.
    delete it_ctab where kbetrba  = 0.
  endif.

endform.                    " GET_DATA3
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_field_catalog3 .


  data: lw_itab type slis_tabname,
          lw_waers like t001-waers,
          l_rqty(9),
          l_datum(8),
          l_cn(2) type n.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.

  lw_itab = 'IT_CTAB'.
  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
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

                                 'S' 'LIFNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Vendor',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'GJAHR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Year',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'BELNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material Doc',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'BUZEI'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Item',
                                 'E' 'OUTPUTLEN'   '04',

                                 'S' 'MATNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Material No',
                                 'E' 'OUTPUTLEN'   '18',

                                 'S' 'TXZ01'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Mat.Desc',
                                 'E' 'OUTPUTLEN'   '30',

                                 'S' 'BWART'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Move Type',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'WAERS'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Currency Key',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'SHKZG'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'D/C',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'BUDAT'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'GR posting Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MENGE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'QUANTITY'    'EA',
                                 ' ' 'COLTEXT'     'GR Quantity',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'      'GR amount',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'MENGET'       '',
                                 ' ' 'KEY'         ' ',
                                ' ' 'QUANTITY'    'EA',
                                 ' ' 'COLTEXT'     'IV Quantity',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTRT'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'Tot. IV AMT',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTR2'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'Init IV AMT',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTR3'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'Reval AMT',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'DMBTRA'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'IV AVG PRICE(A)',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'KBETRS'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'SA PRICE(B)',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'KBETRI'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'INFO PRICE(C)',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'KBETRBA'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'B - A',
                                 'E' 'OUTPUTLEN'   '17',

                                 'S' 'KBETRCB'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'CURRENCY'    'USD',
                                 ' ' 'COLTEXT'     'C - B',
                                 'E' 'OUTPUTLEN'   '17'.

endform.                    " BUILD_FIELD_CATALOG3
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form assign_itab_to_alv3 .


  call method alv_grid->set_table_for_first_display
    exporting
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    changing
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_ctab[].
endform.                    " ASSIGN_ITAB_TO_ALV3
