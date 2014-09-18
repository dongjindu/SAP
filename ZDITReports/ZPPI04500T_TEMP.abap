*&----------------------------------------------------------------------
*& Development ID :
*& Program ID     : ZPPI04500T
*& Program Name   : AFFW Backlog Interface
*& Created by     : Victor Park
*& Created on     : 06.20.2011
*& Issue Doc No.  :
*&
*& Modification Log
*& Date        Developer Issue No Description
*& 01.05.2012  Victor    use Posting Date
*&======================================================================
*& RFC func. :
*& Stru.     : ZSPPAFFW_LOG, ZSPPAFFW_CNT,  ZTPPAFFW_LOG
*&----------------------------------------------------------------------

report zppi04500t_temp message-id zmpp.

tables : affw, makt, mara, marc, ztppaffw_log.

*- ALV
type-pools: slis.
data: gt_fieldcat         type slis_t_fieldcat_alv,
      gs_layout           type slis_layout_alv,
      gs_sort             type slis_sortinfo_alv,
      gt_sort             type slis_t_sortinfo_alv,
      gs_light            type lvc_s_layo,
      gs_print            type slis_print_alv,
      gt_sp_group         type slis_t_sp_group_alv,
      gt_events           type slis_t_event,
      gs_events           like  line of gt_events,
      g_save              value 'A',
      gx_variant          like disvariant,
      g_variant           like disvariant.

data : ls_title         type slis_listheader, "alv header
       alv_t_listheader type slis_t_listheader.

data : g_extab          type slis_t_extab,
       g_extab_ln       like   line  of  g_extab.

data : g_user_command  type slis_formname value 'USER_COMMAND'.
data : t_colinfo_table type slis_t_specialcol_alv with header line.
data : g_repid         like sy-repid.

data : begin of it_data occurs 0.
        include structure zsppaffw_log.
data :  msgv1   like affw-msgv1,
        msgv2   like affw-msgv2,
        msgv3   like affw-msgv3,
        msgv4   like affw-msgv4,
        err_cnt type z_count.
data : end of it_data.

data : begin of it_data_tmp occurs 0,
         matnr  type matnr,
         msgid  type arbgb,
         msgno  type msgno,
         erfme  type erfme,
       end of it_data_tmp.


data : it_log  like ztppaffw_log  occurs 0 with header line.
data : wa_save type ztppaffw_log.
data : it_save type standard table of ztppaffw_log with header line.

data : it_send  like zsppaffw_log occurs 0 with header line.
data : it_interface  like zsppaffw_log occurs 0 with header line.
data : it_cnt   like zsppaffw_cnt occurs 0 with header line.

*- RETURN MESSAGE
data : e_return type zmms0053.
data : l_msgtxt(200).

selection-screen begin of block b1 with frame title text-b01.
parameters: p_date like ztppaffw_log-zdate.
selection-screen end of block b1.


selection-screen begin of block b2 with frame title text-b02.
selection-screen begin of line.
parameters : p_send    radiobutton group r2 user-command ra.
selection-screen comment (25) text-t21 for field p_send.
parameters : p_alv     radiobutton group r2 default 'X'.
selection-screen comment 30(20) text-t22 for field p_alv.
parameters : p_read  radiobutton group r2.
selection-screen comment 70(10) text-t23 for field p_read.
selection-screen end of line.
selection-screen end of block b2.
*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*
initialization.
  perform initial.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
at selection-screen.
  case sy-ucomm.
    when 'COM1'.
      perform set_date.
  endcase.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
at selection-screen output.
  perform modify_screen .

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
start-of-selection.

  perform select_data.
  perform modify_data.

  if p_send = 'X'.
    perform  pro_batch.
  else.
    perform pro_alv.
*    IF it_interface[] IS INITIAL.
*      MESSAGE s001 WITH 'There is No data'.
*      STOP.
*    ELSE.
*      PERFORM save_log  USING ' '   ' '    ''.
*      WRITE : 'No Interface, but Data has been saved in Table'.
*    ENDIF.
  endif.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
end-of-selection.


*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
form select_data .
  data : l_date          type sy-datum,
         l_time          type sy-uzeit.

  data : lv_from_time    type sy-uzeit,
         lv_to_time      type sy-uzeit,
         from_date_time  type ztimestamp,
         to_date_time    type ztimestamp.


  select distinct matnr msgid msgno erfme
  from ztppaffw
  into table it_data_tmp
  where zdate = p_date
    and ersda = p_date
    and erzet > '060000'
    and bwart   = '261'
    and msgty   = 'E'
    and msgid   <> 'M3'   "exclude System Lock
    and ( msgid   = 'M7' and msgno  <> '112' ).


  data: l_date2 like sy-datum.
  l_date2 = p_date + 1.
  select distinct matnr msgid msgno erfme
  from ztppaffw
  appending table it_data_tmp
  where zdate = l_date2
    and ersda = l_date2
    and erzet < '060000'
    and bwart   = '261'
    and msgty   = 'E'
    and msgid   <> 'M3'   "exclude System Lock
    and ( msgid   = 'M7'   and msgno  <> '112' ).


endform.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
form modify_data .
  data : l_labst     type mard-labst,
         l_klabs     type mard-klabs,
         l_lsqty     type mard-labst.  "Available stock
  data : lv_zzcdate type ztimestamp.

  clear : it_cnt[], it_cnt, it_send[], it_send,
          it_interface[], it_interface.

  concatenate sy-datum sy-uzeit into lv_zzcdate.

  loop at it_data_tmp.

*    it_interface-ersda = it_data-ersda.
    it_interface-matnr = it_data_tmp-matnr.
    it_interface-erfmg = 1.  "qty = 1 default
    it_interface-msgid = it_data_tmp-msgid.
    it_interface-msgno = it_data_tmp-msgno.


    collect it_interface.

  endloop.

* make index key
  loop at it_interface.
    it_interface-zmandt  =  sy-mandt.
    it_interface-weblnr  = p_date.
    it_interface-weblpos = sy-tabix.
    it_interface-werks   = 'HVA1'.
    it_interface-bwart   = '261'.
    it_interface-msgty   = 'E'.
    it_interface-ersda   = p_date.


    modify it_interface index sy-tabix.
  endloop.
endform.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
form pro_batch .
  data : v_dest(30) value 'WMPP01'.   "Interface Destination.

  clear : e_return.

*  CHECK NOT it_interface[] IS INITIAL.

*if case of no data, send only summary line.
  if  it_interface[] is initial.
    it_cnt-werks    = 'HVA1'.
    it_cnt-fwdat    = p_date.
    it_cnt-err_cnt  = 0.
    concatenate sy-datum sy-uzeit into it_cnt-zzcdate.
    append it_cnt.
  endif.

  call function 'Z_PP_IF_OB_BACKLOG_DAY' destination v_dest
      importing
        e_return              = e_return
      tables
        t_data                = it_interface
        t_cnt                 = it_cnt
      exceptions
        communication_failure = 1  message l_msgtxt
        system_failure        = 2  message l_msgtxt.

  if e_return-type = 'S' and  sy-subrc = 0.   "Success
*   perform save_log  using 'S' 'Success'    ''.
    write : 'Interface : Success'.
    message s003 with 'Interface : Success'.
  else.
*   perform save_log  using 'E' e_return-message l_msgtxt.
    write :/ e_return-message, l_msgtxt.      "For Spool
    message e003 with  e_return-message l_msgtxt.
  endif.

endform.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
form save_log  using    p_type p_msg1 p_msg2.
  data : l_zseq(10) type n.

  clear : it_save[], it_save.

  check not it_interface[] is initial.                      "07.06.2011

  select zseq into l_zseq
    from ztppaffw_log
    up to 1 rows
    where zdate = sy-datum
    order by zseq descending.
  endselect.

  loop at it_interface.
    move-corresponding it_interface to it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    if p_type = 'E'.
      if not p_msg1 is  initial.
        it_save-zmsg  = p_msg1.
      else.
        it_save-zmsg  = p_msg2.
      endif.
    endif.

    append it_save.
    clear : it_save.

  endloop.

  insert ztppaffw_log from table it_save
                             accepting duplicate keys .
  commit work and wait.


endform.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  INITIAL
*&---------------------------------------------------------------------*
form initial .

  g_repid  = sy-repid.


endform.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
form set_date .

endform.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
form pro_alv .
  field-symbols : <itab>  type standard table.
  data : lv_name(30),
         lv_stru(30).


  if p_read = 'X'.
    lv_name  = 'IT_LOG[]'.
    lv_stru  = 'IT_LOG'.
  else.
    lv_name  = 'IT_INTERFACE[]'.
    lv_stru  = 'IT_INTERFACE'.
  endif.

  assign : (lv_name) to <itab>.

  perform layout_build       using   gs_layout.
  perform sorttab_build      using   gt_sort.
  perform fieldcat           tables  gt_fieldcat
                             using   lv_stru.

  perform append_alv_event  changing   gt_events.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = g_repid
*     i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = g_user_command
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_sort                  = gt_sort
      i_save                   = g_save
*     is_variant               = g_variant
      it_events                = gt_events[]
    tables
      t_outtab                 = <itab>
    exceptions
      program_error            = 1
      others                   = 2.

endform.                    " PRO_ALV
*&---------------------------------------------------------------------*
*&      Form  LAYOUT_BUILD
*&---------------------------------------------------------------------*
form layout_build  using  p_layout type slis_layout_alv.

  p_layout-zebra             = 'X'.
  p_layout-colwidth_optimize = 'X'.
*  p_layout-key_hotspot = 'X'.
*  p_layout-box_fieldname  =    'CHK'.  "SELECTION FIELD
*  p_layout-coltab_fieldname = 'COL_COLOR'. "color field of itabe
*  p_layout-cell_merge        = 'X'.
*  p_layout-detail_popup      = 'X'.
*  p_layout-detail_titlebar   = sy-title.
*  p_layout-no_subtotals      = ''.

endform.                    " LAYOUT_BUILD
*&---------------------------------------------------------------------*
*&      Form  SORTTAB_BUILD
*&---------------------------------------------------------------------*
form sorttab_build  using   p_sort type slis_t_sortinfo_alv.

  clear: gs_sort, p_sort[].

  if p_read = 'X'.
    gs_sort-spos      = '1'.
    gs_sort-tabname   = 'IT_LOG'.
    gs_sort-fieldname = 'ZDATE'.
    gs_sort-up        = 'X'.
    gs_sort-group     = 'BL'.
    gs_sort-subtot    = ''.
    append gs_sort to p_sort.

    gs_sort-spos      = '2'.
    gs_sort-tabname   = 'IT_LOG'.
    gs_sort-fieldname = 'ZSEQ'.
    gs_sort-up        = 'X'.
    gs_sort-group     = 'BL'.
    gs_sort-subtot    = ''.
    append gs_sort to p_sort.
  else.
    gs_sort-spos      = '1'.
    gs_sort-tabname   = 'IT_INTERFACE'.
    gs_sort-fieldname = 'MATNR'.
    gs_sort-up        = 'X'.
    gs_sort-group     = 'BL'.
    gs_sort-subtot    = ''.
    append gs_sort to p_sort.

    gs_sort-spos      = '2'.
    gs_sort-tabname   = 'IT_INFERFACE'.
    gs_sort-fieldname = 'ERSDA'.
    gs_sort-up        = 'X'.
    gs_sort-group     = 'BL'.
    gs_sort-subtot    = ''.
    append gs_sort to p_sort.
  endif.
endform.                    " SORTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
form fieldcat  tables   pt_fieldcat type  slis_t_fieldcat_alv
               using    p_name      type  slis_tabname.

  data: l_datum(08).

  sy-datum = sy-datum + 1.
  move sy-datum to l_datum.
  set parameter id 'ALVBUFFER' field l_datum.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name     = g_repid
*     i_structure_name   = p_name
      i_internal_tabname = p_name
      i_inclname         = g_repid
    changing
      ct_fieldcat        = pt_fieldcat[].

  loop at pt_fieldcat.
    case pt_fieldcat-fieldname.
      when 'MSGTEXT'.
        pt_fieldcat-seltext_m  = 'Error Text'.
    endcase.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    modify pt_fieldcat.

  endloop.

endform.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  APPEND_ALV_EVENT
*&---------------------------------------------------------------------*
form append_alv_event  changing p_alv_event type slis_t_event.
* TOP-OF-PAGE Event
  data ls_events type slis_alv_event.
  ls_events-name  =  'TOP_OF_PAGE'.
  ls_events-form  =  'TOP_OF_PAGE'.
  append ls_events to p_alv_event.

endform.                    " APPEND_ALV_EVENT

*&---------------------------------------------------------------------
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------
form top_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = alv_t_listheader.

endform. " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
form modify_screen .

  loop at screen.
    if p_read = 'X'.
      if screen-group1 = 'GR1'.
        screen-invisible = 1.
        screen-active    = 0.
        modify screen.
      endif.
    else.
      if screen-group1 = 'GR2'.
        screen-invisible = 1.
        screen-active    = 0.
        modify screen.
      endif.
    endif.
  endloop.

endform.                    " MODIFY_SCREEN
