************************************************************************
* Program Name      : ZRMM_DIFFERENT_POSTING_ERS
* Creation Date     : 08/12/2014
* Development Request No :
* Addl Documentation:
* Description       :
*The current Batch jobs of ERS are not processing the Material
*documents created with different posting date than entry date -1 .
*The jobs are collecting the material documents only based on the
*posting date and not on entry date. Material documents with posting
*date as current date and current date-1 will be processed by existing
*batch jobs. This new program picks up the Material documents with posting
*date prior to current date-1. Start date would be 01 of current month.
*If the current MM period is not open the start date would be
*first of last month.
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

report  zrmm_different_posting_ers  no standard page heading
                                    line-size 132
                                    line-count 64(1)
                                    message-id zmmm.



tables : zmmt_ers, rbkp, mkpf, marv, ekko, ekrs, rseg,cvalid.


data : w_tabix     type sy-tabix.

data : begin of gt_ekrs occurs 0.
        include structure ekrs.
data : budat1    like mkpf-budat,
       cpudt     like mkpf-cpudt,
       bukrs1    like ekko-bukrs,
       werks1    like ekpo-werks,
       mjahr     like mkpf-mjahr.
data : end of gt_ekrs.


data : begin of itab occurs 0.
        include structure zmmt_ers.
data : end of itab.





data: w_dest(10),
      ok_code like sy-ucomm,
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
select-options : s_budat    for rbkp-budat  obligatory,
                 s_bsart    for ekko-bsart default 'JIS' obligatory.
selection-screen begin of line.
selection-screen comment 1(25) for field p_ersba.
selection-screen position 33.
parameters: p_ersba like cvalid-ersba obligatory value check
                                              memory id  ersba.
selection-screen comment 37(60) ersba_tx.
selection-screen end of line.
parameters     : s_check   as checkbox  default 'X'.
selection-screen end of block b1.


initialization.
  perform set_init_data.
*

at selection-screen output.
  if sy-batch = 'X'.
    perform set_init_data.
  endif.
*
* get text for type of document selection
  perform get_ersba_text using    p_ersba
                         changing ersba_tx.


start-of-selection.
  perform get_data.

  if not gt_ekrs[] is initial.
    if s_check = 'X'.
      perform move_itab.
    else.
      perform mrer_excut.
    endif.
  else.
    message i009 with text-004.
    exit.
  endif.
*
end-of-selection.
  if sy-batch <> 'X'.
    perform data_display.
  endif.
*
*&---------------------------------------------------------------------*
*&      Form  SET_INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_init_data .
*
  data : ws_datum1  type sy-datum.



  refresh s_budat.
  s_budat-option = 'BT'.
  s_budat-sign   = 'I'.
*

  select single * from marv
    where bukrs = 'H201'
      and lfgja = sy-datum(4)
      and lfmon = sy-datum+4(2).
  if sy-subrc = 0.
*    refresh s_budat.
*    s_budat-option = 'BT'.
*    s_budat-sign   = 'I'.

*    s_budat-low = sy-datum - 1.
    s_budat-high = sy-datum.
    append s_budat.
  else.
    call function 'RP_CALC_DATE_IN_INTERVAL'
      exporting
        date      = sy-datum
        days      = '00'
        months    = '01'
        signum    = '-'
        years     = '00'
      importing
        calc_date = ws_datum1.

    concatenate ws_datum1(6) '01' into s_budat-low.
    "s_budat-low LAST DATE
    call function 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
      exporting
        day_in            = s_budat-low
      importing
        last_day_of_month = s_budat-high.


    if s_budat-low+4(2) = '01'.
      clear ws_datum1.
      call function 'RP_CALC_DATE_IN_INTERVAL'
        exporting
          date      = s_budat-low
          days      = '00'
          months    = '00'
          signum    = '-'
          years     = '01'
        importing
          calc_date = ws_datum1.
*
      concatenate ws_datum1(4) '12' '01' into s_budat-low.
      call function 'SLS_MISC_GET_LAST_DAY_OF_MONTH'
        exporting
          day_in            = s_budat-low
        importing
          last_day_of_month = s_budat-high.
    endif.
    append s_budat.
  endif.

endform.                    " SET_INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data .

  data : w_bukrs   like ekko-bukrs,
         w_werks   like ekpo-werks.

  data : wt_mkpf like mkpf occurs 0 with header line.



  refresh gt_ekrs.
  select * into corresponding fields of table gt_ekrs
    from ekrs as a inner join ekko as b
      on a~ebeln eq b~ebeln
    where a~budat in s_budat
      and b~bsart in s_bsart.
  sort gt_ekrs by budat lifnr belnr buzei gjahr.
*
  loop at gt_ekrs.
    select single bukrs into gt_ekrs-bukrs1 from ekko
        where ebeln = gt_ekrs-ebeln.

    select single werks into gt_ekrs-werks1 from ekpo
                where ebeln = gt_ekrs-ebeln.
    modify gt_ekrs.
  endloop.
  clear gt_ekrs.
*
  if not gt_ekrs[] is initial.
    refresh wt_mkpf.
    select * into corresponding fields of table wt_mkpf
      from mkpf
      for all entries in gt_ekrs
     where mblnr = gt_ekrs-belnr.
  endif.
*
  loop at gt_ekrs.
    w_tabix = sy-tabix.
    read table wt_mkpf with key mblnr = gt_ekrs-belnr.
    if sy-subrc = 0.
      move: wt_mkpf-budat   to   gt_ekrs-budat1,
            wt_mkpf-cpudt   to   gt_ekrs-cpudt,
            wt_mkpf-mjahr   to   gt_ekrs-mjahr.

      modify gt_ekrs index w_tabix.
    endif.
  endloop.
*
  sort gt_ekrs by budat.
  delete gt_ekrs where budat = sy-datum.
  delete gt_ekrs where budat = sy-datum - 1.

*  sort gt_ekrs by ebeln budat lifnr.
  sort gt_ekrs by belnr budat.
*






endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_MRER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mrer_excut.

data: lt_ekrs like gt_ekrs.

* call program : zz_delete_zzekrs
  perform zz_delete_zzekrs.

  refresh itab. clear itab.
  loop at gt_ekrs.
*
    lt_ekrs = gt_ekrs.
    at new belnr.

      clear : rseg, rbkp.
      submit rmmr1mrr
           with so_bukrs-low = lt_ekrs-bukrs1
           with so_werks-low = lt_ekrs-werks1
           with so_budat-low = lt_ekrs-budat
           with so_mjahr-low = lt_ekrs-mjahr
           with so_lifnr-low = lt_ekrs-lifnr
           with so_mblnr-low = lt_ekrs-belnr
*         with so_ebeln-low = gt_ekrs-ebeln
           with pa_ersba = p_ersba             "'2'
           with pa_budat = lt_ekrs-budat
           with pa_delnu = 'X'
           with pa_xtest = ''
          exporting list to memory
          and return.
*
      select single * from ekrs
       where belnr = lt_ekrs-belnr.
*         and ebeln = gt_ekrs-ebeln.
      if sy-subrc = 0.

        move : lt_ekrs-belnr  to itab-mblnr,
               lt_ekrs-budat1 to itab-budat1,
               lt_ekrs-cpudt  to itab-cpudt.
        move : sy-datum       to itab-erdat,
               sy-uzeit       to itab-tims,
               sy-uname       to itab-ernam.
        move : 'E'            to itab-type.
        itab-message = 'ERROR Create Invoice No'.
        append itab.
        clear itab.
      else.
        select single * from rseg
         where lfbnr = lt_ekrs-belnr.
        if sy-subrc = 0.
          select single * from rbkp
            where belnr = rseg-belnr.

          move : rbkp-belnr     to itab-belnr,
                 rbkp-budat     to itab-budat.
          move : lt_ekrs-belnr  to itab-mblnr,
                 lt_ekrs-budat1 to itab-budat1,
                 lt_ekrs-cpudt  to itab-cpudt.

          move : sy-datum       to itab-erdat,
                 sy-uzeit       to itab-tims,
                 sy-uname       to itab-ernam.

          move : 'S'            to itab-type.
          concatenate  'Created Invoice No: ' itab-belnr  into itab-message.
          append itab.
          clear itab.
        else.
          move : lt_ekrs-belnr  to itab-mblnr,
                 lt_ekrs-budat1 to itab-budat1,
                 lt_ekrs-cpudt  to itab-cpudt.

          move : sy-datum       to itab-erdat,
                 sy-uzeit       to itab-tims,
                 sy-uname       to itab-ernam.
          move : 'E'            to itab-type.
          itab-message = 'ERROR Create Invoice No'.
          append itab.
          clear itab.
        endif.
      endif.
    endat.
  endloop.
*
  modify zmmt_ers from table itab.
  if sy-subrc = 0.
    commit work and wait.
  endif.
* call program : zz_delete_zzekrs
  perform zz_delete_zzekrs.


endform.                    " CALL_MRER
*&---------------------------------------------------------------------*
*&      Form  ZZ_DELETE_ZZEKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form zz_delete_zzekrs .

  submit zz_delete_zzekrs
            with p_test  = ''
            exporting list to memory
            and return.
endform.                    " ZZ_DELETE_ZZEKRS
*&---------------------------------------------------------------------*
*&      Form  SAVE_ZTABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_ztable .

endform.                    " SAVE_ZTABLE
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
    perform build_field_catalog using 'IT_DATA'.
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
*      -->P_0736   text
*----------------------------------------------------------------------*
form build_field_catalog  using  p_itab.
  data: lw_itab type slis_tabname,
          lw_waers like t001-waers,
          l_rqty(9),
          l_datum(8),
          l_cn(2) type n.

  clear: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  clear: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = w_repid
      i_structure_name   = 'ZMMT_ERS'
*     i_internal_tabname = 'ITAB'
*     i_inclname         = w_repid
*     I_BUFFER_ACTIVE    = 'X'
    changing
      ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :

                                 'S' 'BELNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Invoice No',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'BUDAT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Posting Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MBLNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'GR No',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'BUDAT1'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Posting Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'CPUDT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'GR Entry Date',
                                 'E' 'OUTPUTLEN'   '35',

                                 'S' 'ERDAT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Created Date',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'TIMS'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Created Time',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'ERNAM'         ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Created Name',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'TYPE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Message Type',
                                 'E' 'OUTPUTLEN'   '10',

                                 'S' 'MESSAGE'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Message',
                                 'E' 'OUTPUTLEN'   '40'.

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
*      -->P_0891   text
*      -->P_0892   text
*      -->P_0893   text
*----------------------------------------------------------------------*
form setting_fieldcat   tables   p_fieldcat structure it_fieldcat
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
*&      Form  MOVE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_itab .

  loop at gt_ekrs.

    move : gt_ekrs-belnr  to itab-mblnr,
           gt_ekrs-budat1 to itab-budat1,
           gt_ekrs-cpudt  to itab-cpudt.
    move : sy-datum       to itab-erdat,
           sy-uzeit       to itab-tims,
           sy-uname       to itab-ernam.
    append itab.
  endloop.
*
  modify zmmt_ers from table itab.
  if sy-subrc = 0.
    commit work and wait.
  endif.

*
endform.                    " MOVE_ITAB
*&---------------------------------------------------------------------*
*&      Form  GET_ERSBA_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ERSBA  text
*      <--P_ERSBA_TX  text
*----------------------------------------------------------------------*
form get_ersba_text  using    if_ersba
                    changing cf_ersba_tx.

  data: lf_value like dd07v-domvalue_l,
        lf_text  like dd07v-ddtext.

  lf_value = if_ersba.

  call function 'GET_TEXT_DOMVALUE'
    exporting
      domname   = 'ERSBA'
      domvalue  = lf_value
      langu     = sy-langu
    importing
      txt       = lf_text
    exceptions
      not_found = 1
      others    = 2.
  if sy-subrc eq 0.
    cf_ersba_tx = lf_text.
  endif.
endform.                    " GET_ERSBA_TEXT
