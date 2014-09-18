************************************************************************
* Program Name      : ZCOR_OT_UPLOAD
* Creation Date     : 07/08/2014
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

report zcor_ot_upload no standard page heading
                     line-size 132
                     line-count 64(1)
                     message-id zmco.
type-pools: slis, vrm.

data: it_ot_prod like table of ztco_ot_prod with header line,
      it_ot_7m_rule  like table of ztco_ot_7m_rule with header line,
      it_ot_punchrec  like table of ztco_ot_punchrec with header line,
      it_ot_prod_temp like table of ztco_ot_prod with header line,
      it_ot_7m_rule_temp  like table of ztco_ot_7m_rule with header line
      ,
      it_ot_punchrec_temp  like table of ztco_ot_punchrec with header
      line.

data: ok_code      like sy-ucomm,
      w_repid  like sy-repid,
      w_cnt       type   i.

data : it_fieldcat     type lvc_t_fcat with header line,
       it_fieldname    type slis_t_fieldcat_alv,
       it_sort         type lvc_t_sort with header line,
       it_fieldcat_det type lvc_t_fcat with header line. "/Detail

data : wa_is_layout type lvc_s_layo, "/The Layout Structure
       w_fieldname    like line of it_fieldcat.

data: wa_save    type c   value 'A',   "for Parameter I_SAVE
      wa_variant type disvariant.      "for parameter IS_VARIANT

data: wa_custom_control type        scrfname value 'ALV_CONTAINER',
      alv_grid          type ref to cl_gui_alv_grid,
      g_docking_container    type ref to cl_gui_docking_container.

data: w_pernr like pa0003-pernr,
      w_uname(80),
      w_file(128).

data: begin of it_error occurs 0,
      table(30),
      file(128),
      pernr(8),
      uname(80),
      end of it_error.

data: w_error(1),
      w_tot_rec1 type i,
      w_tot_rec2 type i,
      w_tot_rec3 type i.

selection-screen begin of block b1 with frame title text-001.
selection-screen begin of line.
parameters: p_up radiobutton group grp.
selection-screen comment 3(10) text-t04.
selection-screen position 25.
parameters: p_del radiobutton group grp.
selection-screen comment 28(10) text-t05.
selection-screen end of line.
selection-screen skip.

parameters: p_year type gjahr,
            p_period type nperi.
selection-screen skip.

selection-screen begin of line.
parameters:  p_f1 as checkbox default 'X'.
selection-screen comment 3(15) text-t01.

selection-screen position 25.
parameters:  p_file1 like rlgrap-filename.

selection-screen end of line.
selection-screen begin of line.
parameters: p_f2 as checkbox.
selection-screen comment 3(15) text-t02.
selection-screen position 25.
parameters: p_file2 like rlgrap-filename.
selection-screen end of line.
selection-screen begin of line.
parameters: p_f3 as checkbox.
selection-screen comment 3(15) text-t03.
selection-screen position 25.
parameters: p_file3 like rlgrap-filename.
selection-screen end of line.

parameters: p_filety like rlgrap-filetype
    default 'DAT' no-display.
selection-screen end of block b1.

at selection-screen on value-request for p_file1.
  perform at_sel_screen_on_value_request using p_file1 'O'
                    changing p_file1.

at selection-screen on value-request for p_file2.
  perform at_sel_screen_on_value_request using p_file2 'O'
                    changing p_file2.

at selection-screen on value-request for p_file3.
  perform at_sel_screen_on_value_request using p_file3 'O'
                    changing p_file3.

at selection-screen output.

  loop at screen.
    if screen-name = 'P_YEAR' or
       screen-name = 'P_PERIOD'.
      if p_up = 'X'.
        screen-input = 0.
      endif.
    endif.
    if screen-name = 'P_FILE1' or
      screen-name = 'P_FILE2' or
            screen-name = 'P_FILE3' .
      if p_del = 'X'.
        screen-input = 0.
      endif.
    endif.
    modify screen.
  endloop.

initialization.
  perform set_init_data.

start-of-selection.

  case 'X'.
    when p_up.
      if ( p_f1 is not initial and p_file1 is initial ) or
         ( p_f2 is not initial and p_file2 is initial ) or
         ( p_f3 is not initial and p_file3 is initial ).
        message s000 with 'Please input File'.
        exit.
      else.
        perform make_data.
        if it_error[] is not initial.
          perform display_data.
        else.
          perform save_data.
        endif.
      endif.
    when p_del.
      if p_year is initial or p_period is initial.
        message s000 with 'Please input Year/Period'.
        exit.
      else.
        perform delete_data.
      endif.
  endcase.

end-of-selection.

*---------------------------------------------------------------------*
*       FORM get_req_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form make_data.
  data: begin of lt_ot_prod_temp occurs 0,
        budat(10),
        gjahr(4),
        zotperio(2),
        zotweekno(2),
        zotempnum(8),
        zotempname(80),
        kostl(10),
        zottimeadm(3),
        zotworksch(10),
        zotpayrule(30),
        zothourtype(20),
        zottimeval(10),
        zotshift(3),
        zotclass1(30),
        zotclass2(30),
        zotclass3(30),
        zotmfgadm(30),
        zotlgroup(20),
        zotsgroup(20),
        zotwagetype(5),
        zotpcdesc(40),
        end of  lt_ot_prod_temp.

  data: begin of lt_ot_7m_rule_temp occurs 0,
        gjahr(4),
        zotperio(2),
        zotweekno(2),
        datum(10),
        kostl(10),
        zotprodadm(30),
        zotsgroup(20),
        zottmnum(8),
        zottmname(82),
        zot7minyesno(3),
        zotshift(3),
        zotwagetype(5),
        zotinot(12),
        zotoutot(12),
        zotot(12),
        zotcin(12),
        zotcout(12),
        zotsin(12),
        zotsout(12),
        zotcheckin(1),
        zotcheckout(1),
        zotcheck(1),
        zotinotflt(12),
        zotoutotflt(12),
        zot7mintime(12),
        zot7mindec(4),
        zotcnt(2),
*        zamt(4),
        zamt(5),  "Victor 08.20.2014
        end of  lt_ot_7m_rule_temp.

  data: begin of lt_ot_punchrec_temp occurs 0,
        kostl(10),
        zotsgroup(20),
        zottmnum(8),
        zottmname(82),
        zotdltime(10),
        zottime01(21),
        zottime02(21),
        zottime03(21),
        zottime04(21),
        zottime05(21),
        zottime06(21),
        zottime07(21),
        zottime08(21),
        zottime09(21),
        zottime10(21),
        zottime11(21),
        zottime12(21),
        end of  lt_ot_punchrec_temp.

  data: l_file  like rlgrap-filename,
        l_msg(80),
        l_date_c(8),
        l_time like sy-uzeit,
        l_text(180),
        l_total type i,
        l_recno type i,
        l_am_pm(2),
        l_seq(2) type n,
        l_fs(50),
        l_fs1(50),
        l_error(1),
        l_quote(1) value '"'.


  field-symbols: <fs>, <fs1>.

  clear: it_ot_prod.
  refresh: it_ot_prod.

** OT Prod
  if p_f1 = 'X'.
    l_file = p_file1.
    call function 'WS_UPLOAD'
      exporting
        filename         = l_file
        filetype         = 'DAT'
      tables
        data_tab         = lt_ot_prod_temp
      exceptions
        conversion_error = 1
        file_open_error  = 2
        file_read_error  = 3
        invalid_type     = 4
        no_batch         = 5
*       BAD_DATA_FORMAT  = 6
        others           = 9.

    if sy-subrc ne 0.
      if sy-subrc = 1.
        l_msg = 'FILE UPLOAD CONVERSION ERROR'.
      elseif sy-subrc = 2.
        l_msg = 'UPLOAD FILE OPEN ERROR'.
      elseif sy-subrc = 3.
        l_msg = 'UPLOAD FILE READ ERROR'.
      elseif sy-subrc = 4.
        l_msg = 'INVALID DATA TYPE!'.
      endif.
      concatenate l_msg '(' l_file ')' into l_msg separated by space.
      message e000 with l_msg.
    endif.

    concatenate l_file ' processing, Please wait...'
           into l_text separated by space.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
*       PERCENTAGE = 0
        text       = l_text.

    delete lt_ot_prod_temp index 1.

    clear: it_ot_prod, it_ot_prod[], l_total.

    loop at lt_ot_prod_temp.
      clear: l_error, it_ot_prod.
      move-corresponding lt_ot_prod_temp to it_ot_prod.

      replace all occurrences of substring l_quote
          in it_ot_prod-zotempname with ' '.
      condense it_ot_prod-zotempname no-gaps.

      replace all occurrences of substring l_quote
           in it_ot_prod-zotpayrule with ' '.
      condense it_ot_prod-zotpayrule no-gaps.

      concatenate lt_ot_prod_temp-budat+6(4)
                        lt_ot_prod_temp-budat+0(2)
                        lt_ot_prod_temp-budat+3(2)
                        into l_date_c.

      it_ot_prod-budat = l_date_c.
      it_ot_prod-uname = sy-uname.
      it_ot_prod-zdatum = sy-datum.
      it_ot_prod-uzeit = sy-uzeit.
      w_pernr = it_ot_prod-zotempnum.
      w_uname = it_ot_prod-zotempname.
      w_file = p_file1.
      l_total = l_total + 1.
      it_ot_prod-seq = l_total.
      perform check_pernr using '1' l_error.
      if l_error is not initial.
        w_error = 'X'.
      endif.
      append it_ot_prod.
    endloop.

    refresh: lt_ot_prod_temp.
    clear: it_ot_prod_temp.

  endif.

** 7M Rule
  if p_f2 = 'X'.
    l_file = p_file2.
    call function 'WS_UPLOAD'
      exporting
        filename         = l_file
        filetype         = 'DAT'
      tables
        data_tab         = lt_ot_7m_rule_temp
      exceptions
        conversion_error = 1
        file_open_error  = 2
        file_read_error  = 3
        invalid_type     = 4
        no_batch         = 5
*       BAD_DATA_FORMAT  = 6
        others           = 9.

    if sy-subrc ne 0.
      if sy-subrc = 1.
        l_msg = 'FILE UPLOAD CONVERSION ERROR'.
      elseif sy-subrc = 2.
        l_msg = 'UPLOAD FILE OPEN ERROR'.
      elseif sy-subrc = 3.
        l_msg = 'UPLOAD FILE READ ERROR'.
      elseif sy-subrc = 4.
        l_msg = 'INVALID DATA TYPE!'.
      endif.
      concatenate l_msg '(' l_file ')' into l_msg separated by space.
      message e000 with l_msg.
    endif.

    concatenate l_file ' processing, Please wait...'
           into l_text separated by space.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
*       PERCENTAGE = 0
        text       = l_text.

    delete lt_ot_7m_rule_temp index 1.

    clear: it_ot_7m_rule, it_ot_7m_rule[], l_total.

    loop at lt_ot_7m_rule_temp.
      clear: it_ot_7m_rule, l_error.
      move-corresponding lt_ot_7m_rule_temp to it_ot_7m_rule.

*      do 100 TIMES.
      replace all occurrences of substring l_quote
         in it_ot_7m_rule-zottmname with ' '.
      condense it_ot_7m_rule-zottmname no-gaps.
*      endoo.
      concatenate lt_ot_7m_rule_temp-datum+6(4)
                  lt_ot_7m_rule_temp-datum+0(2)
                  lt_ot_7m_rule_temp-datum+3(2)
                  into l_date_c.
      it_ot_7m_rule-datum = l_date_c.

      concatenate lt_ot_7m_rule_temp-zotot+0(2)
                  lt_ot_7m_rule_temp-zotot+3(2)
                  lt_ot_7m_rule_temp-zotot+6(2)
                  into l_time.

      l_am_pm = lt_ot_7m_rule_temp-zotot+9(2) .

*      PERFORM convert_time USING l_time l_am_pm l_time.

*      CALL FUNCTION 'HRVE_CONVERT_TIME'
*     EXPORTING
*          type_time             = 'B'
*          input_time            = l_time
*          input_am_pm           = l_am_pm
*       IMPORTING
*         output_time           = l_time
**           OUTPUT_AM_PM          =
**         EXCEPTIONS
**           PARAMETER_ERROR       = 1
**           OTHERS                = 2
*.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.

      it_ot_7m_rule-zotot = l_time.

      if lt_ot_7m_rule_temp-zotot+9(2) = 'PM'.
        it_ot_7m_rule-zotot = it_ot_7m_rule-zotot + 1800.
      endif.

      concatenate lt_ot_7m_rule_temp-zotcin+0(2)
                  lt_ot_7m_rule_temp-zotcin+3(2)
                  lt_ot_7m_rule_temp-zotcin+6(2)
                  into it_ot_7m_rule-zotcin.


      l_am_pm =  lt_ot_7m_rule_temp-zotcin+9(2) .

*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zotcin = l_time..

      concatenate lt_ot_7m_rule_temp-zotcout+0(2)
                  lt_ot_7m_rule_temp-zotcout+3(2)
                  lt_ot_7m_rule_temp-zotcout+6(2)
                  into it_ot_7m_rule-zotcout.

      l_am_pm =  lt_ot_7m_rule_temp-zotcout+9(2) .
*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zotcout = l_time.

      concatenate lt_ot_7m_rule_temp-zotsin+0(2)
       lt_ot_7m_rule_temp-zotsin+3(2)
       lt_ot_7m_rule_temp-zotsin+6(2)
       into it_ot_7m_rule-zotsin.
      l_am_pm =  lt_ot_7m_rule_temp-zotsin+9(2) .

*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zotsin = l_time.

      concatenate lt_ot_7m_rule_temp-zotsout+0(2)
                  lt_ot_7m_rule_temp-zotsout+3(2)
                  lt_ot_7m_rule_temp-zotsout+6(2)
                  into it_ot_7m_rule-zotsout.
      l_am_pm =  lt_ot_7m_rule_temp-zotsout+9(2) .
*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zotsout = l_time.

      concatenate lt_ot_7m_rule_temp-zotinotflt+0(2)
                  lt_ot_7m_rule_temp-zotinotflt+3(2)
                  lt_ot_7m_rule_temp-zotinotflt+6(2)
                  into it_ot_7m_rule-zotinotflt.
      l_am_pm =  lt_ot_7m_rule_temp-zotinotflt+9(2) .
*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zotinotflt = l_time.

      concatenate lt_ot_7m_rule_temp-zotoutotflt+0(2)
                  lt_ot_7m_rule_temp-zotoutotflt+3(2)
                  lt_ot_7m_rule_temp-zotoutotflt+6(2)
                  into it_ot_7m_rule-zotoutotflt.
      l_am_pm = lt_ot_7m_rule_temp-zotoutotflt+9(2) .
*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zotoutotflt = l_time.

      concatenate lt_ot_7m_rule_temp-zot7mintime+0(2)
                  lt_ot_7m_rule_temp-zot7mintime+3(2)
                  lt_ot_7m_rule_temp-zot7mintime+6(2)
                  into it_ot_7m_rule-zot7mintime.
      l_am_pm = lt_ot_7m_rule_temp-zot7mintime+9(2) .
*      PERFORM convert_time USING l_time l_am_pm l_time.
*      it_ot_7m_rule-zot7mintime = l_time.

      it_ot_7m_rule-uname = sy-uname.
      it_ot_7m_rule-zdatum = sy-datum.
      it_ot_7m_rule-uzeit = sy-uzeit.
      w_pernr = it_ot_7m_rule-zottmnum.
      w_uname = it_ot_7m_rule-zottmname.
      w_file = p_file2.
      l_total = l_total + 1.
      it_ot_7m_rule-seq = l_total.
      perform check_pernr using '2' l_error.
      if l_error is not initial.
        w_error = 'X'.
      endif.
      append it_ot_7m_rule.
    endloop.
    refresh: lt_ot_7m_rule_temp.

  endif.

** Punch Record
  if p_f3 = 'X'.
    l_file = p_file3.
    call function 'WS_UPLOAD'
      exporting
        filename         = l_file
        filetype         = 'DAT'
      tables
        data_tab         = lt_ot_punchrec_temp
      exceptions
        conversion_error = 1
        file_open_error  = 2
        file_read_error  = 3
        invalid_type     = 4
        no_batch         = 5
*       BAD_DATA_FORMAT  = 6
        others           = 9.

    if sy-subrc ne 0.

      if sy-subrc = 1.
        l_msg = 'FILE UPLOAD CONVERSION ERROR'.
      elseif sy-subrc = 2.
        l_msg = 'UPLOAD FILE OPEN ERROR'.
      elseif sy-subrc = 3.
        l_msg = 'UPLOAD FILE READ ERROR'.
      elseif sy-subrc = 4.
        l_msg = 'INVALID DATA TYPE!'.
      endif.
      concatenate l_msg '(' l_file ')' into l_msg separated by space.
      message e000 with l_msg.
    endif.

    concatenate l_file ' processing, Please wait...'
             into l_text separated by space.
    call function 'SAPGUI_PROGRESS_INDICATOR'
      exporting
*       PERCENTAGE = 0
        text       = l_text.

    delete lt_ot_punchrec_temp index 1.

    clear: it_ot_punchrec, it_ot_punchrec[], l_total.

    loop at lt_ot_punchrec_temp.
      clear: it_ot_punchrec, l_error.
      move-corresponding lt_ot_punchrec_temp to it_ot_punchrec.
      concatenate lt_ot_punchrec_temp-zotdltime+6(4)
                          lt_ot_punchrec_temp-zotdltime+0(2)
                  lt_ot_punchrec_temp-zotdltime+3(2)
                  into l_date_c.
      it_ot_punchrec-zotdltime = l_date_c.

** Convertion  - commented by Victor 08.28.2014
*      l_seq = '01'.
*      do 12 times.
*        concatenate 'lt_ot_punchrec_temp-zottime' l_seq into l_fs.
*        assign (l_fs) to <fs>.
*        concatenate 'it_ot_punchrec-zottime' l_seq into l_fs1.
*        assign (l_fs1) to <fs1>.
*
*        concatenate <fs>+6(4) <fs>+0(2) <fs>+3(2) <fs>+11(2)
*                    <fs>+14(2) '00'
*                     into <fs1>.
*        l_seq = l_seq + 1.
*        unassign: <fs>, <fs1>.
*      enddo.

      it_ot_punchrec-uname = sy-uname.
      it_ot_punchrec-zdatum = sy-datum.
      it_ot_punchrec-uzeit = sy-uzeit.
      w_pernr = it_ot_punchrec-zottmnum.
      w_uname = it_ot_punchrec-zottmname.
      w_file = p_file3.
      l_total = l_total + 1.
      it_ot_punchrec-seq = l_total.
*      PERFORM check_pernr USING '3' l_error.
*      IF l_error IS NOT INITIAL.
*        w_error = 'X'.
*      ENDIF.
      append it_ot_punchrec.
    endloop.
    refresh: lt_ot_punchrec_temp.
  endif.

  sort it_error.
  delete adjacent duplicates from it_error
     comparing all fields.
endform.                    "READ_DATA
*&---------------------------------------------------------------------*
*&      Form  set_init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_init_data.
  p_year = sy-datum+0(4).
  p_period = sy-datum+4(2).
  loop at screen.
    if screen-name = 'P_YEAR' or
       screen-name = 'P_PERIOD'.
      screen-input = 0.
      modify screen.
    endif.
  endloop.
endform.                    " set_init_data

*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_container_n_object.
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
  create object alv_grid
*         EXPORTING i_parent = grid_container
         exporting i_parent = g_docking_container
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
  data : lw_s_dragdrop type lvc_s_dd01. "/ Drag&Drop control settings

  clear : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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
*  it_sort-fieldname      = ''.
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
*     i_structure_name   = 'ZSMM_CKD_ENG_PO'
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    changing
      ct_fieldcat        = it_fieldname.

  perform setting_fieldcat tables it_fieldcat using :

                                'S' 'TABLE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Table Name',
                                  'E' 'OUTPUTLEN'   '30',

                                   'S' 'FILE'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'File Name',
                                  'E' 'OUTPUTLEN'   '128',

                                  'S' 'PERNR'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Emp ID',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'UNAME'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Name',
                                  'E' 'OUTPUTLEN'   '80'.

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
*     it_toolbar_excluding = it_toolbar_excluding[]
    changing
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_error[]
      it_sort              = it_sort[].

endform.                    " assign_itab_to_alv
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
    when 'SAVEFILES'.
      perform save_data.
  endcase.
endmodule.                 " USER_COMMAND_0200  INPUT

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
    perform build_sortcat_display.
    perform build_field_catalog using 'IT_ERROR'.
    perform assign_itab_to_alv.
    call method alv_grid->refresh_table_display.
  endif.

endmodule.                 " DISPLAY_ALV_200  OUTPUT
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
*&      Form  CHECK_PERNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OT_PROD_ZOTEMPNUM  text
*----------------------------------------------------------------------*
form check_pernr using p_type p_error.
  clear: it_error, p_error.
  select single pernr into it_error-pernr
     from pa0003
    where pernr = w_pernr.
  if sy-subrc <> 0.
    case p_type.
      when '1'.
        it_error-table = 'ZTCO_OT_PROD'.
      when '2'.
        it_error-table = 'ZTCO_OT_7M_RULE'.
      when '3'.
        it_error-table = 'ZTCO_OT_PUNCHREC'.
    endcase.
    it_error-file = w_file.
    it_error-pernr = w_pernr.
    it_error-uname = w_uname.
    append it_error.
    p_error = 'X'.
  endif.
endform.                    " CHECK_PERNR


*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0040   text
*----------------------------------------------------------------------*
form at_sel_screen_on_value_request using def_path like rlgrap-filename
                                          mode     type c
                                    changing p_file.

  data: tmp_filename like rlgrap-filename.
  data: tmp_mask(80).
  data: fieldln type i.
  field-symbols: <tmp_sym>.

  fieldln = strlen( def_path ) - 1.
  assign def_path+fieldln(1) to <tmp_sym>.
  if <tmp_sym> = '/' or <tmp_sym> = '\'.
    clear <tmp_sym>.
  endif.

  call function 'F4_FILENAME'
    exporting
      program_name  = sy-cprog
      dynpro_number = sy-dynnr
      field_name    = ' '
    importing
      file_name     = tmp_filename.

  if sy-subrc = 0.
    p_file  = tmp_filename.
  else.
    message e000 with 'FILE SELECT WINDOW OPEN ERROR!'.
  endif.

endform.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_data .
  data:  l_time_fr(14),
         l_time_to(14),
         l_date_c(8),
         l_date like sy-datum.

  write /'Table Name'.
  write 20(20) 'Message'.
  write /'===================='.
  write 20(20) '==================='.
  if p_f1 = 'X'.
    delete from ztco_ot_prod where gjahr = p_year
                        and zotperio = p_period.
    if sy-subrc = 0.
      commit work.
      write /'ZTCO_OT_PROD'.
      write 20(20) 'Successfully'.
    else.
      rollback work.
      write /'ZTCO_OT_PROD'.
      write 20(20) 'No record deleted'.
    endif.
  endif.
  if p_f2 = 'X'.
    delete from ztco_ot_7m_rule where gjahr = p_year
                        and zotperio = p_period.
    if sy-subrc = 0.
      commit work.
      write /'ZTCO_OT_7M_RULE'.
      write 20(20) 'Successfully'.
    else.
      rollback work.
      write /'ZTCO_OT_7M_RULE'.
      write 20(20) 'No record deleted'.
    endif.
  endif.

  if p_f3 = 'X'.
    concatenate p_year p_period '01' into l_date_c.
    l_date = l_date_c.

    call function 'DATE_GET_MONTH_LASTDAY'
      exporting
        i_date = l_date
      importing
        e_date = l_date.

*-<  Victor: Requested by Fisher 08.25.2014
*    l_date_c = l_date.
*    CONCATENATE p_year p_period '01000000' INTO l_time_fr.
*    CONCATENATE l_date_c '595959' INTO l_time_to.
*
*    DELETE FROM ztco_ot_punchrec WHERE zottime01 BETWEEN l_time_fr
*                       AND l_time_to.
    delete from ztco_ot_punchrec where zotdltime
                                 between l_date_c and l_date.
*->
    if sy-subrc = 0.
      commit work.
      write /'ZTCO_OT_PUNCHREC'.
      write 20(20) 'Successfully'.
    else.
      rollback work.
      write /'ZTCO_OT_PUNCHREC'.
      write 20(20) 'No record deleted'.
    endif.
  endif.

endform.                    " DELETE_DATA
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_data .
  if p_f1 = 'X'.
    if it_ot_prod[] is not initial.
      insert ztco_ot_prod from table it_ot_prod
      accepting duplicate keys.
      if sy-subrc = 0.
        describe table it_ot_prod lines  w_tot_rec1.
        commit work.
        message s000 with w_tot_rec1 ' records uploaded to '
         'ZTCO_OT_PROD'.
      else.
        rollback work.
        message s000  with 'ZTCO_OT_PROD update ERROR'.
      endif.
      refresh: it_ot_prod.
    endif.
  endif.

  if p_f2 = 'X'.
    if it_ot_7m_rule[] is not initial.
      insert ztco_ot_7m_rule from table it_ot_7m_rule
        accepting duplicate keys.
      if sy-subrc = 0.
        describe table  it_ot_7m_rule lines w_tot_rec2.
        commit work.
        message s000 with w_tot_rec2 ' records uploaded to '
          'ZTCO_OT_7M_RULE'.
      else.
        rollback work.
        message s000 with 'ZTCO_OT_7M_RULE update ERROR'.
      endif.
    endif.
    refresh: it_ot_7m_rule.
  endif.

  if p_f3 = 'X'.
    if it_ot_punchrec[] is not initial.
      insert ztco_ot_punchrec from table it_ot_punchrec
        accepting duplicate keys.
      if sy-subrc = 0.
        describe table  it_ot_punchrec lines w_tot_rec3.
        commit work.
        message s000 with w_tot_rec1 ' records uploaded to '
          'ZTCO_OT_PUNCHREC'.
      else.
        rollback work.
        message s000 with 'ZTCO_OT_PUNCHREC update ERROR'.
      endif.
    endif.
    refresh it_ot_punchrec.
  endif.

endform.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  CONVERT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TIM  text
*      -->P_L_AM_PM  text
*----------------------------------------------------------------------*
form convert_time  using p_input_time p_input_am_pm
                   changing p_output_time.
  data : hora type tims.
* Validation of input parameters
  clear: hora.
  if p_input_am_pm = 'AM' or p_input_am_pm = 'PM' .
*      IF P_INPUT_TIME < '010000' OR P_INPUT_TIME > '125959'.
*      RAISE PARAMETER_ERROR.
*    ENDIF.
  else.
    raise parameter_error.
  endif.
* Convertion
  hora(2) = '12'.
  hora+2(4) = '0000'.
  if p_input_am_pm = 'AM'.
*    IF P_INPUT_TIME > '115900' AND P_INPUT_TIME <= '125959'.
*      P_OUTPUT_TIME = P_INPUT_TIME - HORA.
*    ELSE.
    p_output_time = p_input_time.
*    ENDIF.
  else.
*    IF P_INPUT_TIME > '115900' AND P_INPUT_TIME <= '125959'.
*      P_OUTPUT_TIME = P_INPUT_TIME.
*    ELSE.
    p_output_time = p_input_time + hora.
*    ENDIF.
  endif.
endform.                    " CONVERT_TIME
