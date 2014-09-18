************************************************************************
* Program Name      : ZASDU001_NEW
* Author            : IG.Moon
* Creation Date     : 11/11/2008
* Specifications By : Mane, Dhananjay
* Pattern           : BDC
* Description       : Mass Credit Memo Processing Ver2
* Modifications Log
* Date   Developer   Request ID    Description
************************************************************************
report zasdu001 message-id zmco.
include zasdui00.
*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
*Type-Pools
type-pools: kcde.

* Tables
tables : csks, vbrk, vbrp, *vbrp, vbuk, *a004, vbfa, likp, *vbfa, equz,
         ausp,cabn,lips,t001,*vbrk, ztsdu001,*ztsdu001, sscrfields.

types: begin of ty_condi.
types   kschl  type kscha.
types: end of ty_condi.

data:   begin of tkomk occurs 10.
        include structure komk.
data:   end   of tkomk.

data:   begin of xkomv occurs 50.
        include structure komv.
data:   end   of xkomv.

* Value List for Condition Type
data: begin of con_list occurs 0,
          kschl like t685t-kschl,
          vtext like t685t-vtext,
      end of con_list.

* For creating the help list on selection screen
data: begin of help_field occurs 0.
        include structure help_value.
data: end of help_field.

data: begin of help_vtab occurs 0.
        include structure help_vtab.
data: end of help_vtab.

data: begin of help_value occurs 0,
      value like help_vtab-value,
      end of help_value.

data: begin of it_file occurs 0,
         vin(18),
         kbetr(15),
      end of it_file.

data: begin of it_file_vin occurs 0,
         vbeln(10),
         vin(18),
         kbetr(15),
      end of it_file_vin.

types: begin of ty_row_tab.
        include structure zssdu001.
types:
       bodynum type zbodynum,
       posnr type posnr_vf,
       err_prc,
       err_bdc,
       err_body,
       iv_cancel,
       icon3 type icon-id,
       icon2 type icon-id,
       icon type icon-id,
       msg(100),
       chk(1),
       uname type uname,
       fkart type fkart.
types ref_doc	type vgbel.
types bill_doc type	bill_doc.
types: end of ty_row_tab.

data: begin of it_vbeln occurs 0,
         vbeln like vbrk-vbeln,
      end of it_vbeln.

types begin of ty_out.
include  type ty_row_tab.
types celltab  type lvc_t_styl.
types tabcolor type slis_t_specialcol_alv.
types end of ty_out.

data : begin of it_post  occurs 0.
        include structure  zssdu001.
data    bodynum type zbodynum.
data    posnr like vbrp-posnr.
data    err_prc.
data    err_body.
data : end of   it_post.
data : gt_return         like standard table of bapiret2
                         with header line.

data: g_error(1),
      g_repid  like sy-repid,
      g_ix     like sy-tabix.

data  : it_row_tab type table of ty_row_tab with header line,
        gt_out     type table of ty_out     with header line,
        gt_condi   type table of ty_condi   with header line.

data  $gt_out like gt_out occurs 0 with header line.

data : icon_red_scr type icon_d,
       icon_green_scr type icon_d,
       icon_yellow_scr type icon_d,
       icon_doc_scr  type icon_d,
       icon_cancel_scr type icon_d.

define __process.
  perform show_progress using &1 &2.
end-of-definition.
define __cls.                          " clear & refresh
  clear &1.refresh &1.
end-of-definition.
define __focus.
  call method cl_gui_control=>set_focus
      exporting
        control = &1 .
end-of-definition.

define __u_break.
  if err_brk eq true.
    break-point.
  endif.
end-of-definition.

data: r_date type datum,
      r_time type uzeit,
      r_user type uname.

*----------------------------------------------------------------------*
* Define local class
*----------------------------------------------------------------------*
class lcl_event_receiver definition.
  public section.

    types: begin of ztcou131_k,
              co_area   type kokrs,
              fisc_year type gjahr,
              version   type versn,
              kostl     type kostl,
              kstar     type kstar,
           end of ztcou131_k.

    types: ztcou131_key   type standard table of ztcou131_k,
           ztcou131_table type standard table of ztcou131.

    methods:
      handle_data_changed
         for event data_changed of cl_gui_alv_grid
             importing er_data_changed,
                       get_deleted_rows
             exporting
                       deleted_rows type ztcou131_table,

      refresh_delta_tables.


    methods:
      handle_double_click for event double_click of cl_gui_alv_grid
              importing e_row
                        e_column
                        es_row_no.

  private section.
    data deleted_rows type standard table of ztcou131.

* This flag is set if any error occured in one of the
* following methods:
    data: error_in_data type c.
    methods:
      update_delta_tables
         importing
            pr_data_changed type ref to cl_alv_changed_data_protocol.

endclass.                   " LCL_EVENT_RECEIVER Definition

*----------------------------------------------------------------------*
* Implementation local class
*----------------------------------------------------------------------*
class lcl_event_receiver implementation.

* Setting for Change data
  method handle_data_changed.

* remember deleted lines for saving
    call method update_delta_tables( er_data_changed ).

    perform data_changed using er_data_changed.
  endmethod.                    " handle_data_changed

  method get_deleted_rows.
    deleted_rows = me->deleted_rows.
  endmethod.

  method refresh_delta_tables.
    clear me->deleted_rows[].
  endmethod.

* Double Click
  method handle_double_click.
    perform double_click using e_row
                               e_column
                               es_row_no.
  endmethod.                    " handle_double_click

  method update_delta_tables.
    data: l_del_row type lvc_s_moce,
          ls_ztcou131 type ztcou131,
          ls_outtab like line of gt_out.

    loop at pr_data_changed->mt_deleted_rows into l_del_row.
      read table gt_out into ls_outtab index l_del_row-row_id.
      if sy-subrc ne 0.
        message i000(0k) with text-e01. "Internal error
      else.
        move-corresponding ls_outtab to ls_ztcou131.
        append ls_ztcou131 to deleted_rows.
      endif.
    endloop.
  endmethod.


endclass.                   " LCL_EVENT_RECEIVER Implementation

data g_event_receiver  type ref to lcl_event_receiver.

************************************************************************
data  : flag_data_changed,
        info(80).
data: begin of ftab occurs 10,
        fcode(6),
      end of ftab.
****************************** constants *******************************
constants:  false value ' ',
            true  value 'X'.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
parameters p_date like sy-datum default sy-datum.
parameters p_kschl type kschl default 'ZB01'  no-display. " OBLIGATORY

selection-screen begin of block bas with frame title text-0v1.
parameters p_rv1 radiobutton group radv  user-command ucom.
parameters p_rv2 radiobutton group radv.
selection-screen end   of block bas.

selection-screen begin of block b9s with frame title text-0s1.
parameters p_rb1 radiobutton group radx.
parameters p_rb2 radiobutton group radx.

parameters p_fkart like rv60a-fkart no-display.
selection-screen end   of block b9s.
selection-screen end of block bl1.

* block 3 {
selection-screen begin of block b3 with frame title text-005.
selection-screen begin of line.
selection-screen comment 35(12)  text-x00 for field p_exl
                                 modif id exl.
parameters p_exl   radiobutton group radi default 'X'
                                 modif id exl.
selection-screen comment 55(21) text-x01
                                 modif id exl.
parameters p_txt     radiobutton group radi
                                 modif id exl.
selection-screen end   of line.

parameters p_head as checkbox modif id exl default 'X'.
parameters: p_file  like rlgrap-filename obligatory
                    default 'c:\temp\upload.xls'
                    modif id exl.

selection-screen skip 1.
selection-screen begin of block bx with frame title text-00x.
selection-screen begin of line.
selection-screen comment 10(69) my_text modif id brt.
selection-screen end of line.
selection-screen end of block bx.

select-options s_vbeln for vbrp-vbeln no-display.

selection-screen end   of block b3.
* }

* Load saved data
selection-screen begin of block b0 with frame title text-0l1.
parameters p_load as checkbox user-command ucom modif id lod.
select-options s_date for sy-datum modif id lod default sy-datum.
selection-screen end of block b0.

selection-screen begin of block b8 with frame title text-02s.
parameters p_mode default 'N'.
parameters p_test no-display.
parameters err_brk as checkbox.
selection-screen end of block b8.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
initialization.
  p_rv1 = true.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
at selection-screen on value-request for p_file.
  perform browser changing p_file.

at selection-screen output.
  perform modify_screen.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

  if p_rb1 eq true.
    p_fkart = 'ZVL2'.
  endif.
  if p_rb2 eq true.
    p_fkart = 'ZVG2'.
  endif.

  if p_load eq true.

    __cls : it_row_tab, gt_out.
    select *
    into corresponding fields of table it_row_tab
    from ztsdu001
     where fkdat in s_date
       and fkart eq p_fkart.
  else.

    perform initialize.

* Upload File
    perform upload_file using p_file.

    perform modi_row.
  endif.

  perform move_out.
  perform set_output .

*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
end-of-selection.

* Log.
*  PERFORM DISPLAY_LOG.


*----------------------------------------------------------------------*
* Sub-Rutines
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  BROWSING_FILE_PATH
*&---------------------------------------------------------------------*
*       Browsing File Paths
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form browsing_file_path.
* Browsing
  call function 'KD_GET_FILENAME_ON_F4'
       exporting
            mask          = '*.xls'
            static        = 'X'
       changing
            file_name     = p_file
       exceptions
            mask_too_long = 1
            others        = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " BROWSING_FILE_PATH

*&---------------------------------------------------------------------*
*&      Form  CHK_FILE_EXISTENCY
*&---------------------------------------------------------------------*
*       Check File Existency
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form chk_file_existency.
* Check File Existency
  data : lv_exist.
  clear  lv_exist.
  call function 'TMP_GUI_GET_FILE_EXIST'
    exporting
      fname                = p_file
    importing
      exist                = lv_exist
*     ISDIR                =
*     FILESIZE             =
    exceptions
      fileinfo_error       = 1
      others               = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  if lv_exist ne 'X'.
    message e075 with p_file.
  endif.

endform.                    " CHK_FILE_EXISTENCY

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       Upload File data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form upload_file using filename.

  if p_file eq space.
    g_error = true.
    exit.
  endif.

  if p_file eq space.
    g_error = true.
    exit.
  endif.

  data: it_itab like standard table of alsmex_tabline with header line.
  field-symbols : <fs>.
  data : v_index type i.
  data : begin_row type i value 1.

  __process 'Upload file...' '10'.
  if p_head = true.
    add 1 to begin_row.
  endif.

  __cls : it_file_vin, it_file.

  if p_txt ne true.
    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
         exporting
              filename                = filename
              i_begin_col             = 1
              i_begin_row             = begin_row
              i_end_col               = 3
              i_end_row               = 65535
         tables
              intern                  = it_itab
         exceptions
              inconsistent_parameters = 1
              upload_ole              = 2
              others                  = 3.

    if sy-subrc ne 0.
      message s000 with 'Could not find the file.'.
      stop.
    endif.

    __process 'Upload file...' '20'.

    if it_itab[] is initial.
      message s003(zz) with 'No Data was uploaded'.
      g_error = true .
      exit.
    else.

      loop at it_itab.
        move : it_itab-col to v_index.
        assign component v_index of structure it_file_vin to <fs>.
        move : it_itab-value to <fs>.
        at end of row.
          append it_file_vin.
        endat.
      endloop.
    endif.
  else.

    data cancel.

    __process 'Check data...' '30'.

    call function 'UPLOAD'
         exporting
              filename            = filename
              filetype            = 'DAT'
         importing
              cancel              = cancel
         tables
              data_tab            = it_file_vin
         exceptions
              conversion_erro     = 1
              invalid_table_width = 2
              invalid_type        = 3.

    loop at it_file_vin.

      perform check_num changing it_file-kbetr.

      modify it_file_vin.

    endloop.

    if not cancel is initial or sy-subrc ne 0.
      message s003(zz) with 'No Data was uploaded'.
      stop.
    endif.

  endif.

  data $strlen type i.

  __cls it_row_tab.
  loop at it_file_vin.
    $strlen = strlen( it_file_vin-vbeln ).
    if $strlen < 10.
      concatenate '00' it_file_vin-vbeln into it_file_vin-vbeln.
    endif.
    move-corresponding it_file_vin to it_row_tab.
    it_row_tab-bodynum = it_file_vin-vin.
    it_row_tab-fkdat = p_date.
    append it_row_tab.
  endloop.

endform.                    " UPLOAD_FILE


*&---------------------------------------------------------------------*
*&      Form  PRE_FOR_POSTING
*&---------------------------------------------------------------------*
*       Preparation for posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_for_posting tables p_tab structure gt_out.
  data l_no(3) type n.

  clear  it_file.

  data l_filename(40).

* Collect Data
  __cls it_post.
  loop at p_tab.
    clear   it_post.
    it_post-vbeln           = p_tab-vbeln.
    it_post-bodynum         = p_tab-bodynum.
    it_post-matnr           = p_tab-matnr.
    it_post-kbetr           = p_tab-kbetr.
    it_post-fkdat           = p_tab-fkdat.
    it_post-posnr           = p_tab-posnr.
    it_post-err_prc         = p_tab-err_prc.
    append it_post.
  endloop.

* Sorting
  clear   it_post.
  sort it_post by vbeln.

  delete adjacent duplicates from it_post
    comparing vbeln bodynum.

  sort it_post by vbeln bodynum.
  delete adjacent duplicates from it_post
    comparing vbeln bodynum.

endform.                    " PRE_FOR_POSTING

*&---------------------------------------------------------------------*
*&      Form  POST_PL_CCTR_AT_CE
*&---------------------------------------------------------------------*
*       POST PLAN DATA using BAPI FM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form post_ce.

  data $it_post like it_post  occurs 0 with header line.
  data $ix type i.
  data $kschl_indx(2) type n.
  data $kschl_text(30).
  data $selkz_text(30).
  data msg(100).
  data: str_date(10).
  data: lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).
  data $cnt type i.
  data $rate(15).
  data lt_condi type table of ty_condi with header line.

  perform convert_date using p_date changing str_date.

  sort it_post by vbeln.
  __cls it_vbeln.

  loop at it_post.
    it_vbeln-vbeln = it_post-vbeln.
    collect it_vbeln.
  endloop.

  sort it_vbeln.

  loop at it_vbeln.

    select single * from vbrk where vbeln eq it_vbeln-vbeln.
    if sy-subrc ne 0.
      __u_break.
    endif.

    __cls : lt_condi,xkomv, tkomk .

    tkomk-belnr = vbrk-vbeln.
    tkomk-knumv = vbrk-knumv.
    tkomk-vkorg = vbrk-vkorg.

    call function 'RV_KONV_SELECT'
         exporting
              comm_head_i           = tkomk
              general_read          = 'X'
              read_condition_record = 'X'
         tables
              tkomv                 = xkomv.
    if sy-subrc <> 0.
    endif.

    __cls $it_post.
    loop at it_post where vbeln = it_vbeln-vbeln.
      $it_post = it_post.
      append $it_post.
    endloop.

    sort $it_post by posnr.

    loop at xkomv.
      $ix = sy-tabix.
      read table $it_post with key posnr = xkomv-kposn binary search.
      if sy-subrc ne 0.
        delete xkomv index $ix.
      endif.
    endloop.

    perform get_condition tables xkomv lt_condi.
    sort xkomv by kposn.

    __cls: gt_bdc, gt_msg.

    perform dynpro using:
     'X'  'SAPMV60A'        '0102',
     ' '  'BDC_OKCODE'      '=PICK',
     ' '  'RV60A-FKART'     p_fkart,                        "'ZVL2',
     ' '  'RV60A-FKDAT'     str_date,
     ' '  'KOMFK-VBELN(01)' it_vbeln-vbeln.

    loop at $it_post.
      $ix = sy-tabix.

      perform dynpro using:
       'X'  'SAPLV60P'        '4413',
       ' '  'BDC_CURSOR'      '%#AUTOTEXT001',
       ' '  'BDC_OKCODE'     '=POPO'.

      perform dynpro using:
       'X'  'SAPLV60P'        '0251',
       ' '  'BDC_CURSOR'      'RV45A-POSNR',
       ' '  'BDC_OKCODE'     '=POSI',
       ' '  'RV45A-POSNR'    $it_post-posnr.

      perform dynpro using:
       'X'  'SAPLV60P'        '4413',
       ' '  'BDC_CURSOR'      '%#AUTOTEXT001',
       ' '  'BDC_OKCODE'     '=MARK'.

    endloop.

    perform dynpro using:
       'X'  'SAPLV60P'        '4413',
       ' '  'BDC_CURSOR'      '%#AUTOTEXT001',
       ' '  'BDC_OKCODE'     '=RUEB'.

    perform dynpro using:
     'X'  'SAPMV60A'        '0102',
     ' '  'BDC_OKCODE'      '=FAKT',
     ' '  'RV60A-FKART'     p_fkart.

    perform dynpro using:
     'X'  'SAPMV60A'        '0103',
     ' '  'BDC_CURSOR'      '*TVFKT-VTEXT(02)',
     ' '  'BDC_OKCODE'     '=UEBP'.

*    READ TABLE lt_condi WITH KEY kschl = 'ZB01' BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      $kschl_indx = sy-tabix.
*
*      PERFORM dynpro USING:
*       'X'  'SAPMV60A'        '0104',
*       ' '  'BDC_CURSOR'      'VBRK-FKART',
*       ' '  'BDC_OKCODE'     '=KFKO'.
*
*      CONCATENATE 'KOMV-KSCHL(' $kschl_indx ')' INTO $kschl_text.
*      CONDENSE $kschl_text.
*
*      CONCATENATE 'RV61A-SELKZ(' $kschl_indx ')' INTO $selkz_text.
*      CONDENSE $selkz_text.
*
*      PERFORM dynpro USING:
*       'X'  'SAPMV60A'        '6001',
*       ' '  'BDC_OKCODE'     '=V69A_KOLO',
*       ' '  'BDC_SUBSCR'
*       'SAPMV60A                                6011SUBSCREEN_HEADER',
*       ' '  'BDC_SUBSCR'
*       'SAPLV69A                                6201SUBSCREEN_BODY',
*       ' '  'BDC_CURSOR'      $kschl_text,
*       ' '  $selkz_text       'X'.
*
*      PERFORM dynpro USING:
*       'X'  'SAPMV60A'        '6001',
*       ' '  'BDC_OKCODE'     '=BACK',
*       ' '  'BDC_SUBSCR'
*       'SAPMV60A                                6011SUBSCREEN_HEADER',
*       ' '  'BDC_SUBSCR'
*       'SAPLV69A                                6201SUBSCREEN_BODY',
*       ' '  'BDC_CURSOR'      'KOMV-KSCHL(08)'.
*
*    ENDIF.

    read table lt_condi with key kschl = 'ZB01' binary search.
    if sy-subrc eq 0.
      $kschl_indx = sy-tabix.

      perform dynpro using:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_CURSOR'      'VBRK-FKART',
       ' '  'BDC_OKCODE'     '=KFKO'.


      perform dynpro using:
       'X'  'SAPMV60A'        '6001',
       ' '  'BDC_OKCODE'     '=V69A_KOLO',
       ' '  'BDC_SUBSCR'
       'SAPMV60A                                6011SUBSCREEN_HEADER',
       ' '  'BDC_SUBSCR'
       'SAPLV69A                                6201SUBSCREEN_BODY'.

      concatenate 'KOMV-KSCHL(2)' '' into $kschl_text.
      condense $kschl_text.

      concatenate 'RV61A-SELKZ(2)' '' into $selkz_text.
      condense $selkz_text.
      perform dynpro using:
       ' '  'BDC_CURSOR'      $kschl_text,
       ' '  $selkz_text       'X'.

      concatenate 'KOMV-KSCHL(3)' '' into $kschl_text.
      condense $kschl_text.

      concatenate 'RV61A-SELKZ(3)' '' into $selkz_text.
      condense $selkz_text.
      perform dynpro using:
       ' '  'BDC_CURSOR'      $kschl_text,
       ' '  $selkz_text       'X'.

      perform dynpro using:
       'X'  'SAPMV60A'        '6001',
       ' '  'BDC_OKCODE'     '=BACK',
       ' '  'BDC_SUBSCR'
       'SAPMV60A                                6011SUBSCREEN_HEADER',
       ' '  'BDC_SUBSCR'
       'SAPLV69A                                6201SUBSCREEN_BODY',
       ' '  'BDC_CURSOR'      'KOMV-KSCHL(08)'.

    endif.

    read table $it_post index 1.
    if sy-subrc eq 0.
      perform dynpro using:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_OKCODE'     '=MKAL'.
    endif.

    loop at $it_post.
      check $it_post-err_body is initial.
      $ix = sy-tabix.

      write $it_post-kbetr to $rate.

      perform dynpro using:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_OKCODE'      '=PFKO'.

      if $it_post-err_prc eq true.
        perform dynpro using:
         'X'  'SAPMV60A'        '6002',
         ' '  'BDC_OKCODE'      '/00',
         ' '  'BDC_SUBSCR'
         'SAPMV60A                                6012SUBSCREEN_HEADER',
         ' '  'BDC_SUBSCR'
         'SAPLV69A                                6201SUBSCREEN_BODY',
         ' '  'KOMV-KSCHL(07)'  p_kschl,
         ' '  'KOMV-KBETR(07)'  $rate,
         ' '  'KOMV-KSCHL(08)'  'ZV00',
         ' '  'KOMV-KBETR(08)'  '0'.

      else.
        perform dynpro using:
         'X'  'SAPMV60A'        '6002',
         ' '  'BDC_OKCODE'      '/00',
         ' '  'BDC_SUBSCR'
         'SAPMV60A                                6012SUBSCREEN_HEADER',
         ' '  'BDC_SUBSCR'
         'SAPLV69A                                6201SUBSCREEN_BODY',
         ' '  'BDC_CURSOR'      'KOMV-KBETR(07)',
         ' '  'KOMV-KSCHL(07)'  p_kschl,
         ' '  'KOMV-KBETR(01)'  '0',
         ' '  'KOMV-KBETR(07)'  $rate.
      endif.

      perform dynpro using:
       'X'  'SAPMV60A'        '6002',
       ' '  'BDC_OKCODE'      '=BACK'.

      add 1 to $cnt.

    endloop.

    if p_test eq false.
      perform dynpro using:
       'X'  'SAPMV60A'        '0104',
       ' '  'BDC_CURSOR'      'VBRK-FKART',
       ' '  'BDC_OKCODE'      '=SICH'.
    endif.

    perform dynpro using:
     'X'  'SAPMV60A'        '0104',
     ' '  'BDC_CURSOR'      'VBRK-FKART',
     ' '  'BDC_OKCODE'      '=BACK'.

    perform dynpro using:
     'X'  'SAPMV60A'        '0103',
     ' '  'BDC_OKCODE'      '=BACK'.

    __cls gt_msg.

    call transaction 'VF01'   using         gt_bdc
                              options from  gs_opt
                              messages into gt_msg.

    clear msg.
    perform make_msg_string using msg.

    if p_test eq false.
* try two times {
      if msg cp '*S347 Field RV61A-SELKZ (2)*'.
        loop at gt_bdc.
          if gt_bdc-fnam(15) eq 'RV61A-SELKZ(02)'.
            gt_bdc-fnam = 'RV61A-SELKZ(01)'.
            modify gt_bdc index sy-tabix.
          endif.
        endloop.

        call transaction 'VF01'   using         gt_bdc
                                  options from  gs_opt
                                  messages into gt_msg.

        clear msg.
        perform make_msg_string using msg.
      endif.

      if msg cp '*S347 Field RV61A-SELKZ (3)*'.
        loop at gt_bdc.
          if gt_bdc-fnam(15) eq 'RV61A-SELKZ(3)'.
            delete gt_bdc index sy-tabix.
          endif.
        endloop.

        call transaction 'VF01'   using         gt_bdc
                                  options from  gs_opt
                                  messages into gt_msg.

        clear msg.
        perform make_msg_string using msg.
      endif.

* }

    endif.

    read table gt_out with key vbeln = it_post-vbeln.
    if sy-subrc eq 0.
      clear : gt_out-err_bdc, gt_out-msg.
      gt_out-err_bdc = 'N'.
      if sy-msgno eq '311'.
        if msg(13) eq 'S311 Document'.
          gt_out-bill_doc = msg+13(10).
          gt_out-ref_doc  = it_post-vbeln.
        endif.
      elseif sy-msgno eq '050'.
        if msg(13) eq 'S050 Document'.
          gt_out-bill_doc = msg+13(10).
          gt_out-ref_doc  = it_post-vbeln.
        endif.
      else.
        gt_out-err_bdc = true.
        __u_break.
      endif.
      gt_out-msg = msg.

      modify gt_out transporting err_bdc msg bill_doc ref_doc
                       where vbeln eq it_vbeln-vbeln
                         and chk eq true .
    endif.

    lv_cnt = lv_cnt + 1.

  endloop.

  if lv_dcnt > 0 or lv_cnt > 0.
    concatenate 'Data''s been processed;'
                 lv_cnt  'rec(s).'
            into lv_msg separated by space.
    message s000 with lv_msg.
  endif.

  clear flag_data_changed.
  perform apply_icon.


endform.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       Log.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_log.

  data : lv_execnt like sy-tfill.
  data : lv_poscnt like sy-tfill.

  describe table it_row_tab lines lv_execnt.
  describe table it_post    lines lv_poscnt.

  write : / text-011 , lv_execnt.
  skip 1.
  write : / text-012 , lv_poscnt.
  skip 1.
  write : / text-013.

endform.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_screen.

  if p_rv1 eq true.
    my_text =  text-t45.
  else.
    my_text =  text-t05.
  endif.

  loop at screen.
    if screen-group1 = 'BRT'.
      screen-intensified = 1.
      modify screen.
    endif.
  endloop.

endform.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*
form browser changing filename.
  data: it_tfile type filetable ,
        gd_subrc type i.

  call  method cl_gui_frontend_services=>file_open_dialog
        exporting
          window_title = 'Select File Name'
          default_extension = '*.*'
          default_filename = '*.*'
          file_filter = '*.*'
          initial_directory = 'c:\temp\'
*         MULTISELECTION =
*         WITH_ENCODING =
        changing
          file_table = it_tfile
          rc = gd_subrc.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  if sy-subrc <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  else.
    read table it_tfile into filename index 1.
  endif.

endform.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TEXT_S01  text
*      -->P_&1  text
*----------------------------------------------------------------------*
form show_progress using    pf_text
                            value(pf_val).
  call function 'SAPGUI_PROGRESS_INDICATOR'
       exporting
            percentage = pf_val
            text       = pf_text.

endform.                    " SHOW_PROGRESS
*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_<MONTH>  text
*----------------------------------------------------------------------*
form check_num changing n_value.
  data num(12) value ' 0123456789.'.

  replace : '"' with '' into n_value,
            '"' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value,
            ',' with '' into n_value.
  condense n_value no-gaps.

*  IF n_value CN num. n_value = 0. ENDIF.

endform.                    " CHECK_NUM
*&---------------------------------------------------------------------*
*&      Form  DATA_CHANGED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ER_DATA_CHANGED  text
*----------------------------------------------------------------------*
form data_changed using rr_data_changed
                        type ref to cl_alv_changed_data_protocol.

  flag_data_changed = true.

  data: ls_mod_cells type lvc_s_modi,
        ls_cells     type lvc_s_modi,
        lt_values type table of bapi_char_values with header line.

  loop at rr_data_changed->mt_good_cells into ls_mod_cells.
    read table gt_out index ls_mod_cells-row_id.
    if sy-subrc = 0.
      call method rr_data_changed->modify_cell
                exporting i_row_id    = ls_mod_cells-row_id
                          i_fieldname = ls_mod_cells-fieldname
                          i_value     = ls_mod_cells-value.
    endif.
  endloop.

  __set_refresh_mode true.
  call method g_grid->refresh_table_display
       exporting is_stable = stable.

endform.                    " DATA_CHANGED
*&---------------------------------------------------------------------*
*&      Form  MOVE_OUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_out.

  __process 'Preparing output...' '95'.

  data $ix type i.
  data iztsdu001 like ztsdu001 occurs 0 with header line.

  __cls gt_out.
  __cls it_vbeln.

  read table it_row_tab index 1.

  if sy-subrc eq 0.
    loop at it_row_tab.
      it_vbeln-vbeln = it_row_tab-vbeln.
      collect it_vbeln.
    endloop.

    select * from ztsdu001 into table iztsdu001
    for all entries in it_vbeln
    where vbeln eq it_vbeln-vbeln.
  endif.

  sort iztsdu001 by vbeln bodynum icon.

  loop at it_row_tab.

    clear gt_out.
    move-corresponding it_row_tab to gt_out.

    if p_load eq true.
      if not it_row_tab-bill_doc is initial.
        gt_out-icon3 = icon_document.
      else.
      endif.
    else.

      read table iztsdu001 with key vbeln = it_row_tab-vbeln
                                    bodynum = it_row_tab-bodynum
                                    icon = '@5B@'
                                    binary search.

      if sy-subrc eq 0.
        if not iztsdu001-bill_doc is initial.
*          gt_out-bill_doc = iztsdu001-bill_doc.
          gt_out-icon3 = icon_document.
        endif.
      else.

        read table iztsdu001 with key vbeln = it_row_tab-vbeln
                                      bodynum = it_row_tab-bodynum
                                      icon = '@5C@'
                                      binary search.


        if sy-subrc eq 0.
          gt_out-icon3 = icon_led_red.
        endif.

      endif.
      gt_out-fkdat = p_date.
      gt_out-uname = sy-uname.
    endif.

    append gt_out.
  endloop.

  clear : r_date,r_user.

  __process 'Finalizing...' '97'.

  EXEC SQL.
    SELECT DATUM,UZEIT,UNAME
           FROM ZTSDU001
           INTO :R_DATE,:R_TIME,:R_user
           WHERE MANDT = :SY-MANDT
             AND ROWNUM = 1
             ORDER BY DATUM,UZEIT DESC
  ENDEXEC.

  free iztsdu001.

endform.                    " MOVE_OUT
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_output.
  check : g_error is initial.
  clear flag_data_changed.
  perform apply_icon.

  call screen 100.

endform.                    " SET_OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set titlebar '100'.
*   Exclude toolbar
  perform exclude_functions.

  icon_red_scr = icon_led_red.
  icon_yellow_scr = icon_led_yellow.
  icon_green_scr = icon_led_green.
  icon_doc_scr = icon_document.
  icon_cancel_scr = icon_storno.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exclude_functions.
  perform append_exclude_functions
           tables gt_exclude[]
           using: cl_gui_alv_grid=>mc_fc_loc_undo,
                  cl_gui_alv_grid=>mc_fc_average,
                  cl_gui_alv_grid=>mc_fc_graph,
                  cl_gui_alv_grid=>mc_fc_info,
                  cl_gui_alv_grid=>mc_fc_loc_copy_row,
                  cl_gui_alv_grid=>mc_fc_loc_append_row,
                  cl_gui_alv_grid=>mc_fc_loc_cut,
                  cl_gui_alv_grid=>mc_fc_loc_insert_row,
                  cl_gui_alv_grid=>mc_fc_loc_move_row,
                  cl_gui_alv_grid=>mc_fc_loc_paste_new_row.

endform.                    " EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module display_alv_100 output.
  if g_custom_container is initial.
    perform create_and_init_alv.
*   Display alv grid
    call method g_grid->set_table_for_first_display
         exporting is_layout            = gs_layo
                   it_toolbar_excluding = gt_exclude
                   i_save               = gc_var_save
                   is_variant           = gs_variant
         changing  it_outtab            = gt_out[]
                   it_fieldcatalog      = gt_fcat[]
                   it_sort              = gt_sort[].
  else.
    call method g_grid->refresh_table_display.
  endif.
  __focus g_grid.
  perform user_status.

endmodule.                 " DISPLAY_ALV_100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  clear : g_error.

  ok_code = sy-ucomm.
  clear sy-ucomm.
  case ok_code.

    when 'BACK' or 'CANC'.
      perform free_container.
      leave to screen 0.
    when 'EXIT'.
      leave program.

    when 'SAVE'.
      check sy-dynnr eq '0100'.
      perform really?.
      check g_error ne true.
* Get BDC Options
      perform get_opt using p_mode.

      perform : apply_cre,
                refresh_alv.
      __focus g_grid.

    when 'DETA'.

    when 'SWITCH'.
      if sy-dynnr eq '0100'.
        perform switch_edit_mode.
      endif.
      __focus g_grid.

    when 'LOGV'.
      call screen '300'.
  endcase.


endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form free_container.
  if not g_event_receiver is initial.
    free g_event_receiver.
  endif.

  if not g_grid is initial.
    call method g_grid->free.
  endif.

  if not g_custom_container is initial.
    call method g_custom_container->free.
  endif.

  free : g_grid,g_custom_container.

  clear :  gs_layo,gt_exclude,gt_out[],gt_fcat[],gt_sort[].


endform.                    " FREE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  REALLY?
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form really?.
  data $exists(1).
  data l_answer(1).

  perform pop_up using
      'This will create Credit/Debit memos massively.'
      'Do you really want to proceed?' ' '
                 changing l_answer.

  if l_answer ne 'J'.
    g_error = true.
    message s000 with 'Processing was canceled by user.'.
  endif.
endform.                    " REALLY?
*&---------------------------------------------------------------------*
*&      Form  POP_UP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1886   text
*      -->P_1887   text
*      -->P_1888   text
*      <--P_L_ANSWER  text
*----------------------------------------------------------------------*
form pop_up using    p_text p_text2 p_canc
            changing p_answer.

  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
            textline1      = p_text
            textline2      = p_text2
            titel          = 'Check!'
            cancel_display = p_canc
       importing
            answer         = p_answer.


endform.                    " POP_UP
*&---------------------------------------------------------------------*
*&      Form  REFRESH_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_alv.
  __set_refresh_mode true.
  call method g_grid->refresh_table_display
       exporting is_stable = stable.

endform.                    " REFRESH_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_and_init_alv.
*   Create object
  perform create_object.

*  Create Object to verify input values.
  create object g_event_receiver.
  set handler : g_event_receiver->handle_data_changed for g_grid,
                g_event_receiver->handle_double_click for g_grid.

*   Create field category
  perform create_field_category using false.

*  CALL METHOD g_grid->register_edit_event
*       EXPORTING i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  call method g_grid->set_ready_for_input
     exporting
            i_ready_for_input = 0.

  perform sort_build using gt_sort[].

*   Setting for layout
  perform set_lvc_layout.

*   Set colors
*  PERFORM set_color.

*   Set variant
  gv_repid = gs_variant-report = sy-repid.
*  gs_variant-variant = p_vari.

*   Define cell attribute
  perform build_cell_attr.


endform.                    " CREATE_AND_INIT_ALV
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_FALSE  text
*----------------------------------------------------------------------*
form create_field_category using mode_edit.
  data: l_pos       type i.
  define __catalog.
    l_pos = l_pos + 1.
    clear gs_fcat.
    gs_fcat-col_pos       = l_pos.
    gs_fcat-key           = &1.
    gs_fcat-fieldname     = &2.
    gs_fcat-coltext       = &3.     " Column heading
    gs_fcat-outputlen     = &4.     " Column width
    gs_fcat-datatype      = &5.     " Data type
    gs_fcat-emphasize     = &6.
    append gs_fcat to gt_fcat.
  end-of-definition.

  data : $ix(2) type n,
         $mtxt(6).

  __catalog :
    'X'  'VBELN'             'I/V NO.'          10 'CHAR' '',
    'X'  'POSNR'             'Item#'             5 'NUMC' '',
    ' '  'ICON2'             'flg'               2 'ICON' '',
    'X'  'BODYNUM'           'Body#'            10 'CHAR' '',
    'X'  'VIN'               'VIN#'             18 'CHAR' '',
    ' '  'KBETR'             'Amount'           10 'CURR' '',
*    ' '  'FKDAT'             'Date'             10 'DATS' '',
    ' '  'MSG'               'Remarks'          40 'CHAR' '',
    ' '  'ICON'              'flg'               2 'ICON' '',
    ' '  'BILL_DOC'          'Postd.Doc#'       10 'NUMC' '',
    ' '  'REF_DOC'           'Ref.I/V'          10 'NUMC' '',
    ' '  'ICON3'             'post'             2 'ICON' '',
    ' '  'FKDAT'             'Pst.Date'         10 'DATS' '',
    ' '  'UNAME'             'Run By'           10 'CHAR' '',
    ' '  'FKART'             'Type'              5 'CHAR' ''.


  loop at gt_fcat into gs_fcat.
    if gs_fcat-fieldname eq 'KBETR'.
      gs_fcat-just = 'R'.
    endif.
    gs_fcat-ref_table = 'ZSSDU001'.
    gs_fcat-ref_field = gs_fieldcat-fieldname.
    modify gt_fcat from gs_fcat.
  endloop.

endform.                    " CREATE_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_SORT[]  text
*----------------------------------------------------------------------*
form sort_build using ft_sort type lvc_t_sort.
  define sort_tab.
    clear gs_sort.
    gs_sort-fieldname = &1.
    gs_sort-spos      = &2.
    gs_sort-up        = &3.
    gs_sort-group     = &4.
    gs_sort-subtot    = &5.
    gs_sort-comp      = &6.
    append gs_sort to ft_sort.
  end-of-definition.

  sort_tab :
      'VBELN'           ' ' 'X' '' 'X' '',
      'POSNR'           ' ' 'X' '' 'X' '',
      'BODYNUM'         ' ' 'X' '' 'X' '',
      'VIN'             ' ' 'X' '' 'X' ''.
endform.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_lvc_layout.

  clear gs_layo.
  gs_layo-edit       = 'X'.
  gs_layo-zebra      = 'X'.
  gs_layo-sel_mode   = 'A'.       " Column and row selection
*  gs_layo-cwidth_opt = 'X'.
  gs_layo-ctab_fname = 'TABCOLOR'.
  gs_layo-stylefname = 'CELLTAB'.

endform.                    " SET_LVC_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_color.
  data : $ix(2) type n,
         $mtxt(6).

  clear: gs_specialcol, gt_specialcol[], gt_out-tabcolor[].

  define __color.
    gs_specialcol-fieldname = &1 .
    gs_specialcol-color-col = &2 .
    gs_specialcol-color-int = &3 .
    append gs_specialcol to gt_specialcol .
  end-of-definition.

  __color :
            'VIN'              '2' 0,
            'MATNR'            '2' 0,
            'KBETR'            '3' 0,
            'FKDAT'            '1' 0,
            'ICON'             '1' 0,
            'MSG'              '1' 0.

  gt_out-tabcolor[] = gt_specialcol[].
  modify gt_out transporting tabcolor where tabcolor is initial.

endform.                    " SET_COLOR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_cell_attr.
  data: lt_celltab type lvc_t_styl,
        ls_celltab type lvc_s_styl.

  clear lt_celltab.
  refresh lt_celltab.

  clear gs_fcat.

  loop at gt_fcat into gs_fcat.
    ls_celltab-fieldname = gs_fcat-fieldname.
    if ls_celltab-fieldname eq 'KBETR' or
          ls_celltab-fieldname eq 'VIN'.

      ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    else.
      ls_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    endif.
    insert ls_celltab into table lt_celltab.
  endloop.

  clear gt_out-celltab.
  insert lines of lt_celltab into table gt_out-celltab.
  modify gt_out transporting celltab where celltab is initial.
  perform build_cell_attr1_lock.

endform.                    " BUILD_CELL_ATTR
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_cell_attr1_lock.

*  DATA: LT_CELLTAB TYPE LVC_T_STYL,
*        LS_CELLTAB TYPE LVC_S_STYL.
*
*  CLEAR LT_CELLTAB.
*  REFRESH LT_CELLTAB.
*
*  __CLS GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*
*  CLEAR GS_FCAT.
*
*  LOOP AT GT_FCAT INTO GS_FCAT.
*    LS_CELLTAB-FIELDNAME = GS_FCAT1-FIELDNAME.
*    LS_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT LS_CELLTAB INTO TABLE LT_CELLTAB.
*  ENDLOOP.
*
*  INSERT LINES OF LT_CELLTAB INTO TABLE GT_OUT-CELLTAB.
*  MODIFY GT_OUT TRANSPORTING CELLTAB WHERE FEVOR = SPACE.
*

endform.                    " BUILD_CELL_ATTR1_LOCK
*&---------------------------------------------------------------------*
*&      Form  SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form switch_edit_mode.
  data answer.
  if g_grid->is_ready_for_input( ) eq 0.
    call method g_grid->set_ready_for_input
                     exporting i_ready_for_input = 1.

    perform info_text_set using true.
  else.
**    IF FLAG_DATA_CHANGED EQ TRUE.
**      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
**           EXPORTING
**                TEXTLINE1     = 'Data has not been saved yet.'
**                TEXTLINE2     = 'Do you want to continue anyway? '
**                TITEL         = 'Confirmation'
**                DEFAULTOPTION = 'N'
**           IMPORTING
**                ANSWER        = ANSWER.
**      CHECK ANSWER EQ 'J'.
**    ENDIF.
**    CLEAR FLAG_DATA_CHANGED.
    call method g_grid->set_ready_for_input
                     exporting i_ready_for_input = 0.
    perform info_text_set using false.
  endif.

  perform build_cell_attr.
endform.                    " SWITCH_EDIT_MODE
*&---------------------------------------------------------------------*
*&      Form  INFO_TEXT_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRUE  text
*----------------------------------------------------------------------*
form info_text_set using    p_true.
  if p_true eq true.
    info = text-015.
  else.
    info = text-015.
  endif.

endform.                    " INFO_TEXT_SET
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0300 output.
  set pf-status 'ZLOG'.
  sy-title = 'Error log...'.
  suppress dialog.
  leave to list-processing and return to screen 0.
  perform error_list.
endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ERROR_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form error_list.

  loop at gt_return.
    write:/ gt_return-message_v1(20),
            gt_return-message_v2(10),
            gt_return-message(40).
  endloop.

endform.                    " ERROR_LIST
*&---------------------------------------------------------------------*
*&      Form  user_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form user_status.
  __cls ftab.

  if g_grid->is_ready_for_input( ) eq 1.
    ftab-fcode = 'SAVE'.
    append ftab.
  endif.

  ftab-fcode = 'LOGV'.
  append ftab.
  set pf-status '100' excluding ftab.
endform.                    " user_status
*&---------------------------------------------------------------------*
*&      Form  APPLY_CCA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apply_cre.

  __cls $gt_out.

  data: lt_row   type lvc_t_row,
        ls_row   type lvc_s_row,
        lt_roid  type lvc_t_roid,
        lv_cnt(5),
        lv_dcnt(5),
        lv_msg(200).                 " Message

  data  lt_messages      like messages       occurs 0 with header line.
  data  $messages      like messages       occurs 0 with header line.


* Save seleted data to table ZTCOU135
  clear: lv_cnt, lt_row[], lt_roid[].

  perform get_selected_rows tables $gt_out.

* Preparation for posting
  perform pre_for_posting tables $gt_out.
  perform post_ce.

  perform save_log.
  gt_out-chk = false .
  modify gt_out transporting chk where chk eq true.

endform.                    " APPLY_CCA
*&---------------------------------------------------------------------*
*&      Form  GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_$GT_OUT  text
*----------------------------------------------------------------------*
form get_selected_rows tables $gt_out structure gt_out.

  data: lt_rows type lvc_t_row with header line,
        lt_row_no type lvc_t_roid. "Numeric IDs of Selected Rows

  call method g_grid->get_selected_rows
           importing et_index_rows = lt_rows[]
                     et_row_no     = lt_row_no.

  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    message e000
    with 'Error founded during flushing of ALV Grid Control'.
    exit.
  endif.

  read table lt_rows index 1.
  if sy-subrc ne 0.
    gt_out-chk = true .
    modify gt_out transporting chk where chk eq false.
    $gt_out[] = gt_out[].
  else.
    loop at lt_rows where rowtype is initial.
      read table gt_out index lt_rows-index.
      gt_out-chk = true .
      modify gt_out index lt_rows-index .
    endloop.
    loop at gt_out.
      check gt_out-chk eq true.
      $gt_out = gt_out.
      append $gt_out.
    endloop.
  endif.

endform.                    " get_selected_rows
*&---------------------------------------------------------------------*
*&      Form  initialize
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initialize.

  clear : g_error.
  __cls : it_row_tab.

  select single * from t001 where bukrs = 'H201'.

endform.                    " initialize
*&---------------------------------------------------------------------*
*&      Form  convert_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_POST_FKDAT  text
*      <--P_STR_DATE  text
*----------------------------------------------------------------------*
form convert_date  using    f_date  like sy-datum
                   changing f_dtout type char10.
  call function 'CONVERT_DATE_TO_EXTERNAL'
       exporting
            date_internal            = f_date
       importing
            date_external            = f_dtout
       exceptions
            date_internal_is_invalid = 1
            others                   = 2.

endform.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      Form  make_msg_string
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSG  text
*----------------------------------------------------------------------*
form make_msg_string using    p_msg.

  call function 'RKC_MSG_STRING'
       exporting
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       importing
            msg_lin = p_msg.

endform.                    " MAKE_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  apply_icon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form apply_icon.
  data $ix like sy-tabix.

  loop at gt_out.
    $ix = sy-tabix.

    case gt_out-err_bdc.
      when 'X'.
        gt_out-icon = icon_led_red.
      when 'N'.
        gt_out-icon = icon_led_green.
      when others.
        clear gt_out-icon.
    endcase.

    case gt_out-err_prc.
      when 'X'.
        gt_out-icon2 = icon_led_yellow.
      when others.
        clear gt_out-icon2.
    endcase.

    case gt_out-err_body.
      when 'X'.
        gt_out-icon2 = icon_led_red.
      when others.
    endcase.

    if gt_out-iv_cancel eq 'X'.
      gt_out-icon2 = icon_storno.
    endif.

    modify gt_out index $ix transporting icon2 icon.
  endloop.

endform.                    " apply_icon
*&---------------------------------------------------------------------*
*&      Form  kschl_input_help
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_KSCHL  text
*----------------------------------------------------------------------*
form kschl_input_help changing p_p_kschl.
  data j like sy-index.
  __cls con_list.

  select kschl vtext
  into table con_list
  from t685t
  where spras = 'EN'
    and kvewe = 'A'
    and kappl = 'V'.

  sort con_list by kschl .

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'KSCHL'.
  help_field-selectflag = 'X'.
  append help_field.

  help_field-tabname = 'T685T'.
  help_field-fieldname = 'VTEXT'.
  help_field-selectflag = ' '.
  append help_field.

  loop at con_list.
    help_value-value = con_list-kschl.
    append help_value.
    help_value-value = con_list-vtext.
    append help_value.
  endloop.

  perform value_help changing j.

  if j > 0.
    read table con_list index j.
    p_p_kschl = con_list-kschl.
  endif.

  dynpfields-fieldname  = 'KSCHL'.
  dynpfields-fieldvalue = con_list-kschl.
  append dynpfields.

  call function 'DYNP_VALUES_UPDATE'
       exporting
            dyname     = sy-cprog
            dynumb     = sy-dynnr
       tables
            dynpfields = dynpfields.

  clear: dynpfields.
  refresh: con_list, help_field, help_vtab, help_value, dynpfields.
endform.                    " KSCHL_INPUT_HELP

*---------------------------------------------------------------------*
*       FORM value_help                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_J                                                           *
*---------------------------------------------------------------------*
form value_help changing p_j.
  call function 'HELP_VALUES_GET_WITH_TABLE_EXT'
       exporting
            display       = ' '
       importing
            index         = p_j
       tables
            fields        = help_field
            select_values = help_vtab
            valuetab      = help_value.
endform.                               " VALUE_HELP
*&---------------------------------------------------------------------*
*&      Form  get_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_XKOMV  text
*      -->P_LT_CONDI  text
*----------------------------------------------------------------------*
form get_condition tables   p_xkomv structure xkomv
                            p_lt_condi structure gt_condi.


  __cls p_lt_condi.

  loop at p_xkomv.
    p_lt_condi-kschl =  p_xkomv-kschl.
    collect p_lt_condi.
  endloop.

  sort p_lt_condi.

endform.                    " get_condition

*---------------------------------------------------------------------*
*       FORM double_click                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  E_ROW                                                         *
*  -->  E_COLUMN                                                      *
*  -->  ES_ROW_NO                                                     *
*---------------------------------------------------------------------*
form double_click using  e_row     type lvc_s_row
                         e_column  type lvc_s_col
                         es_row_no type lvc_s_roid.
  data l_index type i.

  l_index = e_row-index.

  read table gt_out index l_index.
  if sy-subrc = 0.
    if e_column = 'REF_DOC'.
      check gt_out-ref_doc ne space.
      set parameter id 'VF'  field gt_out-ref_doc .
      call transaction 'VF03' and skip first screen.

    endif.
    if e_column = 'BILL_DOC'.
      check gt_out-bill_doc ne space.
      set parameter id 'VF'  field gt_out-bill_doc.
      call transaction 'VF03' and skip first screen.
    endif.
    if e_column = 'VBELN'.
      check gt_out-vbeln ne space.
      set parameter id 'VF'  field gt_out-vbeln.
      call transaction 'VF03' and skip first screen.
    endif.

  endif.

  call method cl_gui_control=>set_focus exporting control = g_grid.

endform.                    " DOUBLE_CLICK
*&---------------------------------------------------------------------*
*&      Form  save_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_log.

  data $vbeln like gt_out-vbeln.
  data $vin like gt_out-bodynum.
  data $seqno like ztsdu001-seqno.

  loop at gt_out where chk eq true.
    clear : *ztsdu001,ztsdu001.

    EXEC SQL.
      SELECT VBELN,BODYNUM,SEQNO
             FROM ZTSDU001
             INTO :*ZTSDU001-VBELN,:*ZTSDU001-BODYNUM,:*ZTSDU001-SEQNO
             WHERE MANDT = :SY-MANDT
               AND VBELN = :GT_OUT-VBELN
               AND BODYNUM = :GT_OUT-BODYNUM
               AND ROWNUM = 1
               ORDER BY SEQNO DESC
    ENDEXEC.

    if sy-subrc eq 0.
      add 1 to *ztsdu001-seqno.
      move-corresponding gt_out to ztsdu001.
      ztsdu001-seqno = *ztsdu001-seqno.
    else.
      move-corresponding gt_out to ztsdu001.
      ztsdu001-seqno = '01'.
    endif.

    ztsdu001-uname = sy-uname.
    ztsdu001-datum = sy-datum.
    ztsdu001-uzeit = sy-uzeit.

    insert ztsdu001.
    if sy-subrc eq 0.
      commit work.
    endif.
  endloop.

endform.                    " save_log
*&---------------------------------------------------------------------*
*&      Form  modi_row
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modi_row.

  data $ix type i.
  data $atinn type atinn.

  select single atinn into $atinn
  from  cabn
  where atnam eq 'P_VIN'.
  if sy-subrc ne 0.
    $atinn = '0000003351'.
  endif.

  __process 'Check VIN# Consistency...' '20'.


  loop at it_row_tab.
    $ix = sy-tabix.
    clear it_row_tab-msg.
    if not s_vbeln[] is initial and not it_row_tab-vbeln is initial.
      if not it_row_tab-vbeln in s_vbeln.
        delete it_row_tab index $ix.
        continue.
      endif.
    endif.

    if p_rv1 eq true.
      clear it_row_tab-bodynum.
      select single objek into it_row_tab-bodynum
                        from  ausp
                        where atinn eq $atinn
                          and atwrt eq it_row_tab-vin.
      if sy-subrc ne 0.
        it_row_tab-msg = 'Invalid VIN #'.
      endif.

    else.
      clear it_row_tab-vin.
      select single atwrt into it_row_tab-vin
                        from  ausp
                        where objek eq it_row_tab-bodynum
                          and atinn eq $atinn.
      if sy-subrc ne 0.
        it_row_tab-msg = 'Invalid Body #'.
      endif.
    endif.

    select single * into *vbrp
    from vbrp where vbeln = it_row_tab-vbeln
                and vgbel = it_row_tab-bodynum.

    if sy-subrc ne 0.
      it_row_tab-err_body = true.
    else.

      it_row_tab-vbeln = *vbrp-vbeln.

      it_row_tab-posnr = *vbrp-posnr.
      clear it_row_tab-err_prc.

      select single * into *a004
          from a004
       where kappl = 'V'
         and kschl = 'ZV00'
         and matnr = *vbrp-matnr
         and datab =< *vbrp-prsdt
         and datbi >= *vbrp-prsdt.

      if sy-subrc ne 0.
        it_row_tab-err_prc = true.
        __u_break.
      endif.

    endif.
    it_row_tab-fkart = p_fkart.
    modify it_row_tab index $ix transporting
    vbeln vin bodynum posnr err_prc err_body fkart msg.
  endloop.

  if not s_vbeln[] is initial.
    delete it_row_tab where not vbeln in s_vbeln.
  endif.

  sort it_row_tab by vbeln vin.

  data $flag.
  data $vbtyp like vbuk-vbtyp.
  loop at it_row_tab.
    at new vbeln.
      $flag = true.
    endat.
    check $flag eq true.
    clear $flag.
    select single vbtyp into $vbtyp from vbuk
           where vbeln = it_row_tab-vbeln.
    if sy-subrc eq 0.
      if $vbtyp eq 'N'.
        it_row_tab-iv_cancel = true.
        modify it_row_tab transporting iv_cancel
        where vbeln = it_row_tab-vbeln.
      endif.
    endif.
  endloop.

endform.                    " modi_row
