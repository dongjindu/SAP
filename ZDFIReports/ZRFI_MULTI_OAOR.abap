*&--------------------------------------------------------------------*
*& Author                 : .....                                     *
*& Creation Date          : 08/08/2014                                *
*& Specification By       :                                           *
*& Pattern                :                                           *
*& Development Request No :                                           *
*& Description  : Upload attachment to several FI document.           *
*&                                                                    *
*& Modification Log                                                   *
*&--------------------------------------------------------------------*
*& Date        Developer      Request ID        Description           *
*&--------------------------------------------------------------------*
*                                                                     *
*The purpose of this program. To upload attachment to FB03 screen.    *
*Main logic. System is using t-code OAOR to upload pdf file.          *
*Key value in OAOR, class name=BKPF, class key=BO) # FB03 key         *
* (company code, year, document ID)                                   *
*                                                                     *
*&--------------------------------------------------------------------*

report  zrfi_multi_oaor message-id zmfi no standard page heading
                               line-size 70.

tables : bkpf.

data : begin of itab occurs 0,
         icon like icon-id,
         gjahr    type gjahr,
         belnr    like bkpf-belnr,
         result(40)   type c,
       end of itab.


*Macros.
include : <cntn01>,<icon>.
swc_container lt_message_container.
*top dec.
type-pools: slis, abap, truxs.
data:
      wt_filename type standard table of dxfilep with header line,
      wa_filename type dxfilep,
      wt_bin      type solix occurs 0,
      wa_bin      type solix,
      w_filename  type string,
      l_obj       type swc_object,
      ws_obja     type borident,
      ws_objb     type borident,
      ws_binrel   type gbinrel,
      wt_binatt   type standard table of brelattr,
      w_attsize   type wsrm_error-wsrm_direction.

*
data: ok_code like sy-ucomm,
      w_repid like sy-repid,
      w_cnt   type i.
*
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
parameters s_bukrs    like bkpf-bukrs default 'H201'.
select-options s_belnr  for bkpf-belnr no intervals.
parameters s_gjahr    like bkpf-gjahr.

parameters : s_upfile like rlgrap-filename obligatory
                      default 'c:\temp\Filename.pdf'.

selection-screen end of block b1.



at selection-screen on value-request for s_upfile.
  perform f4_s_upfile.


*


***********************************************************************
* INITIALIZATION.
***********************************************************************
initialization.
  s_gjahr = sy-datum(4).

***********************************************************************
* START-OF-SELECTION
***********************************************************************
start-of-selection.
  perform get_data.

  if itab[] is initial.
    message s000(zmfi) with 'No data '.
    exit.
  endif.

  perform oaor_bdc_upload.


end-of-selection.

  perform set_output .





*&---------------------------------------------------------------------*
*&      Form  F4_S_UPFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_s_upfile .

  call function 'WS_FILENAME_GET'
    exporting
      def_path         = s_upfile  "* File Name
      mask             = ',*.*,*.*.'
      mode             = 'O'
    importing
      filename         = s_upfile
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.
*

endform.                    " F4_S_UPFILE
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data .


  refresh itab.   clear itab.
  if not s_belnr[] is initial.
    select * into corresponding fields of table itab
        from bkpf
      where bukrs = s_bukrs
        and gjahr = s_gjahr
        and belnr in s_belnr.
  endif.
*
  sort itab by belnr.


endform.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  OAOR_BDC_UPLOAD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form oaor_bdc_upload .


  move  s_upfile to w_filename.
  call function 'GUI_UPLOAD'
    exporting
      filename                = w_filename
      filetype                = 'BIN'
    importing
      filelength              = w_attsize
    tables
      data_tab                = wt_bin
    exceptions
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      others                  = 17.
  if sy-subrc <> 0.
  else.
* filename catch
    data: str1 type string,
          str2 type string,
          str3 type string,
          str4 type string,
          w_cnt type i,
          wt_tab type table of string,
          ws_tab type string.

    split w_filename at '\' into: str1 str2 str3,
                              table wt_tab.

*    SORT WT_TAB.
    describe table wt_tab  lines w_cnt.
    read table wt_tab into ws_tab index w_cnt.
    w_filename = ws_tab.

*"'PDF'.
    split w_filename at '.' into: str1 str2.


*file Uploaded successfully.
*convert uploaded file contents into BIN format.
    data: l_seq type i.
    swc_container      l_cont.
    swc_create_object  l_obj  'MESSAGE'       ''.
    swc_set_element    l_cont 'NO_DIALOG'     'X'.
    swc_set_element    l_cont 'DOCUMENTTITLE' w_filename.
    swc_set_table      l_cont 'Content_Hex'   wt_bin.
    swc_set_element    l_cont 'DOCUMENTTYPE'  str2.    "'PDF'.
    swc_set_element    l_cont 'DOCUMENTSIZE'  w_attsize.
    swc_refresh_object l_obj.
    swc_call_method    l_obj  'CREATE'        l_cont.
    swc_get_object_key l_obj  ws_objb-objkey.

    ws_objb-objtype = 'MESSAGE'.   "type of attach document
    ws_obja-objtype = 'BKPF'.      "BO of SAP Document.
    loop at itab.
      concatenate    'H201'  "company code
                     itab-belnr  "FI Document
                     itab-gjahr  "fiscal year
                     into
                     ws_obja-objkey.

      call function 'BINARY_RELATION_CREATE_COMMIT'
        exporting
          obj_rolea      = ws_obja
          obj_roleb      = ws_objb
          relationtype   = 'ATTA'
        importing
          binrel         = ws_binrel
        tables
          binrel_attrib  = wt_binatt
        exceptions
          no_model       = 1
          internal_error = 2
          unknown        = 3
          others         = 4.
      if sy-subrc eq 0.
        message s043(sgos_msg).
      endif.
      commit work.
      itab-icon = 3.
      itab-result = 'Successfully Uploaded'.
      modify itab.
    endloop.
  endif.
endform.                    " OAOR_BDC_UPLOAD
*&---------------------------------------------------------------------*
*&      Form  SET_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_output .
  call screen 0200.
endform.                    " SET_OUTPUT
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
    perform build_field_catalog using 'ITAB'.
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

  wa_is_layout-excp_fname  = 'ICON'.

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
endform.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0597   text
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
*     i_structure_name   = 'ZSMM_CKD_ASN_GCS_DIS'
      i_internal_tabname = 'ITAB'    "lw_itab
      i_bypassing_buffer = 'X'
      i_inclname         = w_repid
    changing
      ct_fieldcat        = it_fieldname.



*         icon like icon-id,
*         gjahr    type gjahr,
*         belnr    type belnr_d,
*         result   type dokid_bkpf,

  perform setting_fieldcat tables it_fieldcat using :

                                 'S' 'BELNR'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Document ID',
                                 'E' 'OUTPUTLEN'   '15',

                                 'S' 'GJAHR'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Year',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'ICON'       '',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Status',
                                 'E' 'OUTPUTLEN'   '05',

                                 'S' 'RESULT'       ' ',
                                 ' ' 'KEY'         ' ',
                                 ' ' 'COLTEXT'     'Result',
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
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0759   text
*      -->P_0760   text
*      -->P_0761   text
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
