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

report zppi04500t message-id zmpp.

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
        zdate   like ztppaffw-zdate,
        ztime   like ztppaffw-ztime,
        err_cnt type z_count.
data : end of it_data.

data : it_data_tmp like it_data occurs 0 with header line.
data : it_data_add like it_data occurs 0 with header line. "Remain qty

data : it_log  like ztppaffw_log  occurs 0 with header line.
data : wa_save type ztppaffw_log.
data : it_save type standard table of ztppaffw_log with header line.
data : w_save_time like sy-uzeit.

data : it_send  like zsppaffw_log occurs 0 with header line.
data : it_interface  like zsppaffw_log occurs 0 with header line.
data : it_cnt   like zsppaffw_cnt occurs 0 with header line.


*-Current Remain backlog
data : it_interface_add  like zsppaffw_log occurs 0 with header line.
data : it_cnt_add        like zsppaffw_cnt occurs 0 with header line.
data : lv_d0_lines(7).   "D0 total line
*-alv
data : begin of it_alv occurs 0.
data : type   like ztppaffw_log-type.
        include structure zsppaffw_log.
data : end of it_alv.

*- RETURN MESSAGE
data : e_return type zmms0053.
data : l_msgtxt(200).

DATA:  dyfields LIKE dynpread OCCURS 0 WITH HEADER LINE.

selection-screen begin of block b1 with frame title text-b01.
select-options :  so_matnr for mara-matnr,
                so_werks for affw-werks, " OBLIGATORY,
                so_lgort for affw-lgort,
                so_mtart for mara-mtart,
*                so_matkl FOR mara-matkl ,
                so_dispo for marc-dispo,
*                so_msgid FOR affw-msgid,
                so_profl for mara-profl,
                so_msgid for affw-msgid,
                s_budat  for affw-budat obligatory
                             modif id gr1 no intervals.  "Creation Date
*                s_log    FOR ztppaffw_log-zdate MODIF ID gr2."Log Date

selection-screen end of block b1.

selection-screen begin of block b3 with frame title text-b03.
parameters : p_date like ztppaffw_log-zdate.
selection-screen skip.

selection-screen begin of line.
parameters : p_today  radiobutton group r2 default 'X'.
selection-screen comment (50) text-t11 for field p_today.
parameters : p_old    radiobutton group r2.
selection-screen comment 60(22) text-t12 for field p_old.
selection-screen end of line.
parameters : p_time like ztppaffw_log-ztime.
selection-screen end of block b3.


selection-screen begin of block b2 with frame title text-b02.
selection-screen begin of line.
parameters : p_send    as checkbox.
selection-screen comment (25) text-t21 for field p_send.
*PARAMETERS : p_alv     RADIOBUTTON GROUP r2 DEFAULT 'X'.
*SELECTION-SCREEN COMMENT 30(20) text-t22 FOR FIELD p_alv.
*PARAMETERS : p_read  RADIOBUTTON GROUP r2.
*SELECTION-SCREEN COMMENT 70(10) text-t23 FOR FIELD p_read.
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
*  CASE sy-ucomm.
*    WHEN 'COM1'.
*      PERFORM set_date.
*  ENDCASE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------*
at selection-screen output.
  perform modify_screen .

*at selection-screen on p_date.
*  if s_budat-low > p_date.
*
*    dyfields-fieldname = 'S_BUDAT'.
*    dyfields-fieldvalue = p_date.
*    append dyfields.
*    call function 'DYNP_VALUES_UPDATE'
*      exporting
*        dyname     = sy-cprog
*        dynumb     = sy-dynnr
*      tables
*        dynpfields = dyfields.
*
*  endif.
*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
start-of-selection.

  perform check_condition.
*  if p_time is initial.
  perform select_data.
*  else.
*    PERFORM select_data_display.
*  endif.
  perform modify_data.      "Set time zone for remain backlog
  perform modify_data_add.  "Current Remain Backlog

  if p_send = 'X'.
    perform  pro_batch_add.
    perform  pro_batch.

  else.
    perform pro_integrate.
    perform pro_alv.
  endif.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
end-of-selection.


*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
form select_data .
*-Should check functional spec for understanding why process is likethis

  data : l_date          type sy-datum,
         max_date        type sy-datum,
         l_timef         type sy-uzeit,
         l_timet         type sy-uzeit,
         l_time          type sy-uzeit.

  data : lv_from_time    type sy-uzeit,
         lv_to_time      type sy-uzeit,
         from_date_time  type ztimestamp,
         to_date_time    type ztimestamp.

*      l_date  = s_budat-low + 1.  " input day + 1 day

*-data selection : D-1 6:00 ~ D 5:00
*  lv_from_time  = '060000'.
*  lv_to_time    = lv_from_time - 1.
*  MOVE '0000' TO lv_to_time+2(4).

  w_save_time = SY-UZEIT.
  l_date  = p_date + 1.                  "input day + 1 day

*-  Current Backlog
  select a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
         a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
         a~msgno  a~msgty   a~ersda a~erzet  a~budat as fwdat
         a~fevor  a~dispo   a~zdate a~ztime
    into corresponding fields of table it_data
  from ztppaffw as a inner join makt as b
                on a~matnr = b~matnr
                 inner join mara as c
                on a~matnr  = c~matnr
*    WHERE a~zdate = l_date
  where a~zdate >= p_date
    and  a~zdate <= l_date
*      AND a~ztime = l_time
*      AND a~budat   >=  s_budat-low
*      AND a~budat   <=  l_date
    and a~budat   =  p_date
    and a~bwart   = '261'
*    AND a~lgort   <> 'L001'
    and a~matnr   in so_matnr
    and a~werks   in so_werks
    and a~lgort   in so_lgort
    and a~dispo   in so_dispo
      and a~msgid   in so_msgid
    and c~mtart   in so_mtart
*        AND c~matkl   IN so_matkl
    and c~profl in so_profl
    and a~msgty   = 'E'
*     exclude System Lock: 10.31.2011 by Victor
    and a~msgid   <> 'M3'
    and ( a~msgid   = 'M7'   and a~msgno  <> '112' )
    and b~spras   =  sy-langu .



  if p_today = 'X'.  "Set time zone fo rremain backlog
    l_date  = p_date + 1.                  "input day + 1 day

    if p_time is initial.
      select ztime  into  l_time
      from ztppaffw
        up to 1 rows
      where zdate = l_date
       and ztime <= p_time
      order by   ztime descending.
      endselect.

    else.
      l_time = p_time+0(2).
    endif.

    l_timef = p_time+0(2).
    l_timet = l_timef + 3600 - 1.


* remaining backlog
    select a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
           a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
           a~msgno  a~msgty   a~ersda a~erzet  a~budat as fwdat
           a~fevor  a~dispo
      into corresponding fields of table it_data_add
    from ztppaffw as a inner join makt as b
                  on a~matnr = b~matnr
                   inner join mara as c
                  on a~matnr  = c~matnr
    where a~zdate = l_date
*    WHERE a~zdate >= p_date
*      and  a~zdate <= l_date
      and a~ztime between l_timef and l_timet
      and ( a~budat   between s_budat-low and p_date )
      and a~bwart   = '261'
*    AND a~lgort   <> 'L001'
      and a~matnr   in so_matnr
      and a~werks   in so_werks
      and a~lgort   in so_lgort
      and a~dispo   in so_dispo
        and a~msgid   in so_msgid
      and c~mtart   in so_mtart
*        AND c~matkl   IN so_matkl
      and c~profl in so_profl
      and a~msgty   = 'E'
*     exclude System Lock: 10.31.2011 by Victor
      and a~msgid   <> 'M3'
      and ( a~msgid   = 'M7'   and a~msgno  <> '112' )
      and b~spras   =  sy-langu .

*    CONCATENATE s_budat-low lv_from_time INTO from_date_time.
*    CONCATENATE l_date      lv_to_time   INTO to_date_time.
*
*    LOOP AT it_data.
*      CONCATENATE it_data-budat it_data-erzet INTO it_data-zzcdate.
*      IF  it_data-zzcdate < from_date_time OR
*          it_data-zzcdate > to_date_time.
*        DELETE it_data.
*      ENDIF.
*    ENDLOOP.
  endif.

  if p_old = 'X'.   "Current Remain Backlog
    l_date  = p_date + 1.                  "input day + 1 day
    clear : max_date, l_time.
    select zdate  ztime  into (max_date, l_time)
    from ztppaffw
      up to 1 rows
*    where zdate = l_date
*      and ztime <= p_time
    order by zdate descending  ztime descending.
    endselect.

    select a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
           a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
           a~msgno  a~msgty   a~ersda a~erzet  a~budat as fwdat
           a~fevor  a~dispo
      into corresponding fields of table it_data_add
    from ztppaffw as a inner join makt as b
                  on a~matnr = b~matnr
                   inner join mara as c
                  on a~matnr  = c~matnr
    where a~zdate = max_date
      and a~ztime = l_time
*      AND a~budat   >=  s_budat-low
*      AND a~budat   <=  l_date
      and a~budat   <=  p_date
      and a~bwart   = '261'
*    AND a~lgort   <> 'L001'
      and a~matnr   in so_matnr
      and a~werks   in so_werks
      and a~lgort   in so_lgort
      and a~dispo   in so_dispo
      and a~msgid   in so_msgid
      and c~mtart   in so_mtart
*        AND c~matkl   IN so_matkl
      and c~profl in so_profl
      and a~msgty   = 'E'
*     exclude System Lock: 10.31.2011 by Victor
      and a~msgid   <> 'M3'
      and ( a~msgid   = 'M7'   and a~msgno  <> '112' )
      and b~spras   =  sy-langu .

*    CONCATENATE s_budat-low lv_from_time INTO from_date_time.
*    CONCATENATE l_date    lv_to_time   INTO to_date_time.

*    LOOP AT it_data_add.
*      CONCATENATE it_data_add-budat it_data_add-erzet
*      INTO it_data_add-zzcdate.
*      IF  it_data_add-zzcdate < from_date_time OR
*          it_data_add-zzcdate > to_date_time.
*        DELETE it_data_add.
*      ELSE.
*      ENDIF.
*    ENDLOOP.
  endif.

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


  sort it_data by  matnr msgid msgno  zdate descending
                                         ztime descending.
  delete adjacent duplicates from it_data
                    comparing  matnr msgid msgno.

** on 03/14/12
*  sort it_data by  matnr msgid msgno fwdat zdate descending
*                                         ztime descending.
*  delete adjacent duplicates from it_data
*                    comparing  matnr msgid msgno fwdat.
** end on 03/14/12

  it_data_tmp[]  = it_data[].

  sort it_data_tmp by  matnr msgid msgno fwdat descending.
  delete adjacent duplicates from it_data_tmp
                    comparing  matnr msgid msgno.
  loop at it_data.

*    it_send-budat = it_data-budat.
    it_send-matnr = it_data-matnr.
    it_send-erfmg = it_data-erfmg.
    it_send-msgid = it_data-msgid.
    it_send-msgno = it_data-msgno.

    collect it_send.

  endloop.

*--final table : it_interface[]
*  SORT it_send BY budat matnr msgid msgno.
  sort it_send by  matnr msgid msgno.
  loop at it_data_tmp.
    clear : it_send.

    read table it_send with key " budat = it_data_tmp-budat
                                matnr = it_data_tmp-matnr
                                msgid = it_data_tmp-msgid
                                msgno = it_data_tmp-msgno
                                binary search.

**-< Check Warehouse Stock: exist? -> delete from I/F,Victor 02.23.2012
*    IF  it_data_tmp-msgid = 'M7' AND it_data_tmp-msgno = '021'.
*
**---Get Warehouse Stock
*      CLEAR : l_labst, l_klabs.
*      SELECT SUM( labst ) SUM( klabs )
*           INTO (l_labst, l_klabs)
*      FROM mard
*      WHERE matnr EQ it_data_tmp-matnr
*        AND werks EQ it_data_tmp-werks
*        AND lgort EQ it_data_tmp-lgort.
*
*      l_lsqty = l_labst + l_klabs.
*
*      IF it_data_tmp-erfmg <= l_lsqty.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
**->

    it_data_tmp-err_cnt = 1.
    it_data_tmp-zzcdate =  lv_zzcdate.

* by Daniel on 07/19/11 {
*    IF it_data_tmp-werks = 'P001'.
*      it_data_tmp-werks   = 'HVA1'.
*    ELSE.
*      it_data_tmp-werks   = 'HEA1'. "Engine E001 -> HEA1
*    ENDIF.
    it_data_tmp-werks   = 'HVA1'.
    it_data_tmp-zmandt  =  sy-mandt.
* }

    move-corresponding it_data_tmp to it_interface.
    move-corresponding it_data_tmp to it_cnt.
*    it_cnt-fwdat  =  it_data_tmp-fwdat.               "Create Date

*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*      EXPORTING
*        msgid               = it_data_tmp-msgid
*        msgnr               = it_data_tmp-msgno
*        msgv1               = it_data_tmp-msgv1
*        msgv2               = it_data_tmp-msgv2
*        msgv3               = it_data_tmp-msgv3
*        msgv4               = it_data_tmp-msgv4
*      IMPORTING
*        message_text_output = it_interface-msgtext.

    if it_send-erfmg <> 0.
      it_interface-erfmg  = it_send-erfmg.
      append it_interface.
      collect it_cnt.
    endif.
  endloop.

endform.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH
*&---------------------------------------------------------------------*
form pro_batch .
  data : v_dest(30) value 'WMPP01'.   "Interface Destination.

  clear : e_return.

*if case of no data, send only summary line.
  if  it_interface[] is initial.
    it_cnt-werks  =	'HVA1'.
    if s_budat-high = '00000000'.
      it_cnt-fwdat  = s_budat-low.
    else.
      it_cnt-fwdat  = s_budat-high.
    endif.
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
    perform save_log  using 'S' 'Success'    '' 'D0'.
    write : 'Interface : Success'.
    message s003 with 'Interface : Success'.
  else.
    perform save_log  using 'E' e_return-message l_msgtxt 'D0'.
    write :/ e_return-message, l_msgtxt.      "For Spool
    message e003 with  e_return-message l_msgtxt.
  endif.

endform.                    " PRO_BATCH

*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
form save_log  using    p_type p_msg1 p_msg2 p_gubun.
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

** Changed on 03/14/12 - set remain to 'X' if material includes in
** remain log
    read table it_interface_add WITH key matnr = it_interface-matnr
                               msgid = it_interface-msgid
                               msgno = it_interface-msgno
                               fwdat = it_interface-fwdat.
    if sy-subrc = 0.
       CONTINUE.
    ENDIF.
    l_zseq = l_zseq + 1.
** end on 03/14/12

    move-corresponding it_interface to it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq.
      it_save-ztime = w_save_time.
*    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    it_save-type  = p_gubun.
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

  s_budat-sign   = 'I'.
  s_budat-option = 'EQ'.
  s_budat-low = sy-datum - 1.
*  s_budat-low = sy-datum.
  append s_budat.

  p_date  = sy-datum - 1.
  p_time  = sy-uzeit.
  move '0000' to p_time+2(4).

*  so_werks-sign   = 'I'.
*  so_werks-option = 'EQ'.
*  so_werks-low = 'P001'.
*  APPEND so_werks.
*
*  so_werks-sign   = 'I'.
*  so_werks-option = 'EQ'.
*  so_werks-low = 'E001'.
*  APPEND so_werks.

endform.                    " INITIAL
*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
form set_date .
*  CLEAR : s_budat[].
*
*  IF p_today = 'X'.
*    s_budat-low = sy-datum - 1.
*    s_budat-sign   = 'I'.
*    s_budat-option = 'EQ'.
*    APPEND s_budat.
**  ELSE.
**    CLEAR : s_budat[].
*  ENDIF.


endform.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  PRO_ALV
*&---------------------------------------------------------------------*
form pro_alv .
  field-symbols : <itab>  type standard table.
  data : lv_name(30),
         lv_stru(30).


*  IF p_read = 'X'.
*    lv_name  = 'IT_LOG[]'.
*    lv_stru  = 'IT_LOG'.
*  ELSE.
  lv_name  = 'IT_ALV[]'.
  lv_stru  = 'IT_ALV'.
*  ENDIF.

  assign : (lv_name) to <itab>.

  perform layout_build       using   gs_layout.
  perform sorttab_build      using   gt_sort.
  perform fieldcat           tables  gt_fieldcat
                             using   lv_stru.

  perform list_header_write using alv_t_listheader[].
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

*  IF p_read = 'X'.
*    gs_sort-spos      = '1'.
*    gs_sort-tabname   = 'IT_LOG'.
*    gs_sort-fieldname = 'ZDATE'.
*    gs_sort-up        = 'X'.
*    gs_sort-group     = 'BL'.
*    gs_sort-subtot    = ''.
*    APPEND gs_sort TO p_sort.
*
*    gs_sort-spos      = '2'.
*    gs_sort-tabname   = 'IT_LOG'.
*    gs_sort-fieldname = 'ZSEQ'.
*    gs_sort-up        = 'X'.
*    gs_sort-group     = 'BL'.
*    gs_sort-subtot    = ''.
*    APPEND gs_sort TO p_sort.
*  ELSE.

  gs_sort-spos      = '1'.
  gs_sort-tabname   = 'IT_ALV'.
  gs_sort-fieldname = 'TYPE'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  append gs_sort to p_sort.

  gs_sort-spos      = '2'.
  gs_sort-tabname   = 'IT_ALV'.
  gs_sort-fieldname = 'MATNR'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  append gs_sort to p_sort.

  gs_sort-spos      = '3'.
  gs_sort-tabname   = 'IT_ALV'.
  gs_sort-fieldname = 'FWDAT'.
  gs_sort-up        = 'X'.
  gs_sort-group     = 'BL'.
  gs_sort-subtot    = ''.
  append gs_sort to p_sort.
*  ENDIF.
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
      when 'TYPE'.
        pt_fieldcat-seltext_m  = 'D0/D1'.
      when 'FWDAT'.
        pt_fieldcat-seltext_m  = 'Posting Date'.
    endcase.
    pt_fieldcat-reptext_ddic =
    pt_fieldcat-seltext_s    =
    pt_fieldcat-seltext_l    =
    pt_fieldcat-seltext_m.

    modify pt_fieldcat.

  endloop.

endform.                    " FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  LIST_HEADER_WRITE
*&---------------------------------------------------------------------*
form list_header_write   using alv_t_listheader type slis_t_listheader.
  data : lv_cnt(7).

  clear   : ls_title, alv_t_listheader.
  refresh : alv_t_listheader.

  data : h_title(30), s_title(60),  a_title(60).

*  IF p_alv = 'X'.
  describe table it_alv lines lv_cnt.

  ls_title-typ = 'H'. "(H:Header, S:Selection, A:Action)
  concatenate 'Remaining / Occured  : '  lv_cnt ' /' lv_d0_lines
                           into ls_title-info.
  append ls_title to alv_t_listheader.

  if p_today = 'X'.
    ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
    ls_title-info =    '*Set Time Zone for Remain Backlog'.
  else.
    ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
    ls_title-info =    '*Current Remain Backlog'.
  endif.
  append ls_title to alv_t_listheader.

  ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
  concatenate '*Creation Date : '  s_budat-low
                                   into ls_title-info.
  append ls_title to alv_t_listheader.
*  ELSE.
*    DESCRIBE TABLE it_log LINES lv_cnt.
*    ls_title-typ = 'H'. "(H:Header, S:Selection, A:Action)
*    CONCATENATE '*Interface log counts : ' lv_cnt INTO ls_title-info .
*    APPEND ls_title TO alv_t_listheader.
*
*    ls_title-typ = 'S'. "(H:Header, S:Selection, A:Action)
*    CONCATENATE '*Interface Date : '  s_log-low '~' s_log-high
*                                     INTO ls_title-info.
*    APPEND ls_title TO alv_t_listheader.
*  ENDIF.

endform.                    " LIST_HEADER_WRITE
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

*&--------------------------------------------------------------------*
*&      Form  ALV_USER_COMMAND
*&--------------------------------------------------------------------*
form  user_command using ucomm    like sy-ucomm
                    p_selfield    type slis_selfield.
* double click : UCOMM = &IC1
  case ucomm.

  endcase.

  p_selfield-refresh = 'X'.
endform.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  CHECK_CONDITION
*&---------------------------------------------------------------------*
form check_condition .
*  IF p_today = 'X' AND s_budat-low IS INITIAL.
*    MESSAGE s000 WITH 'Please input Creation Date'.
*    STOP.
*  ENDIF.
**  IF p_today = 'X' AND p_time IS INITIAL.
**    MESSAGE s000 WITH 'Please input Time'.
**    STOP.
**  ENDIF.
*
*  IF p_old = 'X' AND p_time IS NOT INITIAL.
*    MESSAGE s000 WITH 'Please do not input time'.
*    STOP.
*  ENDIF.

endform.                    " CHECK_CONDITION
*&---------------------------------------------------------------------*
*&      Form  MODIFY_SCREEN
*&---------------------------------------------------------------------*
form modify_screen .

*  LOOP AT SCREEN.
*    IF p_read = 'X'.
*      IF screen-group1 = 'GR1'.
*        screen-invisible = 1.
*        screen-active    = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ELSE.
*      IF screen-group1 = 'GR2'.
*        screen-invisible = 1.
*        screen-active    = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

endform.                    " MODIFY_SCREEN
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data_display .
**  DATA : l_date          TYPE sy-datum,
**         l_time          TYPE sy-uzeit.
*
*  DATA : lv_from_time    TYPE sy-uzeit,
*         lv_to_time      TYPE sy-uzeit,
*         from_date_time  TYPE ztimestamp,
*         to_date_time    TYPE ztimestamp.
*
*  RANGES: lr_time FOR ztppaffw_log-ztime.
*
*  lr_time-low = p_time.
*  CONCATENATE p_time+0(2) '5959' INTO lr_time-high.
*  lr_time-sign = 'I'.
*  lr_time-option = 'BT'.
*  APPEND lr_time.
*
*  IF p_read = 'X'.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_log
*    FROM ztppaffw_log
*    WHERE zdate IN s_log.
*  ELSE.
*
*** Furong on 03/05/12
*    IF p_today = 'X'.
*
**      SELECT zdate  ztime  INTO (l_date, l_time)
**      FROM ztppaffw
**        UP TO 1 ROWS
**      ORDER BY zdate DESCENDING  ztime DESCENDING.
**      ENDSELECT.
*
*      SELECT a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
*             a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
*             a~msgno  a~msgty   a~budat a~erzet
*             a~fevor  a~dispo
*        INTO CORRESPONDING FIELDS OF TABLE it_data
*      FROM ztppaffw AS a INNER JOIN makt AS b
*                    ON a~matnr = b~matnr
*                     INNER JOIN mara AS c
*                    ON a~matnr  = c~matnr
*      WHERE a~zdate IN s_budat
*        AND a~ztime IN lr_time
*        AND a~bwart   = '261'
**    AND a~lgort   <> 'L001'
*        AND a~matnr   IN so_matnr
*        AND a~werks   IN so_werks
*        AND a~lgort   IN so_lgort
*        AND a~dispo   IN so_dispo
**        AND a~msgid   IN so_msgid
*        AND c~mtart   IN so_mtart
**        AND c~matkl   IN so_matkl
*        AND c~profl IN so_profl
*        AND a~msgty   = 'E'
*     "exclude System Lock: 10.31.2011 by Victor
*        AND a~msgid   <> 'M3'
*        AND ( a~msgid   = 'M7'   AND a~msgno  <> '112' )
*        AND b~spras   =  sy-langu .
*
*    ELSE.
**      SELECT zdate  ztime  INTO (l_date, l_time)
**      FROM ztppaffw
**        UP TO 1 ROWS
**      ORDER BY zdate DESCENDING  ztime DESCENDING.
**      ENDSELECT.
*
*      SELECT a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
*             a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
*             a~msgno  a~msgty   a~budat a~erzet
*             a~fevor  a~dispo
*        INTO CORRESPONDING FIELDS OF TABLE it_data
*      FROM ztppaffw AS a INNER JOIN makt AS b
*                    ON a~matnr = b~matnr
*                     INNER JOIN mara AS c
*                    ON a~matnr  = c~matnr
*     WHERE NOT a~zdate IN s_budat
*        AND a~ztime IN lr_time
*        AND a~bwart   = '261'
**    AND a~lgort   <> 'L001'
*        AND a~matnr   IN so_matnr
*        AND a~werks   IN so_werks
*        AND a~lgort   IN so_lgort
*        AND a~dispo   IN so_dispo
**        AND a~msgid   IN so_msgid
*        AND c~mtart   IN so_mtart
**        AND c~matkl   IN so_matkl
*        AND c~profl IN so_profl
*        AND a~msgty   = 'E'
*     "exclude System Lock: 10.31.2011 by Victor
*        AND a~msgid   <> 'M3'
*        AND ( a~msgid   = 'M7'   AND a~msgno  <> '112' )
*        AND b~spras   =  sy-langu .
*    ENDIF.
*
**    IF p_today = 'X'.
**
**      SELECT zdate  ztime  INTO (l_date, l_time)
**      FROM ztppaffw
**        UP TO 1 ROWS
**      ORDER BY zdate DESCENDING  ztime DESCENDING.
**      ENDSELECT.
**
**      SELECT a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
**             a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
**             a~msgno  a~msgty   a~budat a~erzet
**             a~fevor  a~dispo
**        INTO CORRESPONDING FIELDS OF TABLE it_data
**      FROM ztppaffw AS a INNER JOIN makt AS b
**                    ON a~matnr = b~matnr
**                     INNER JOIN mara AS c
**                    ON a~matnr  = c~matnr
**      WHERE a~zdate = l_date
**        AND a~ztime = l_time
**        AND a~budat   >=  s_budat-low
**        AND  a~budat   <=  sy-datum
**        AND a~bwart   = '261'
***    AND a~lgort   <> 'L001'
**        AND a~matnr   IN so_matnr
**        AND a~werks   IN so_werks
**        AND a~lgort   IN so_lgort
**        AND a~dispo   IN so_dispo
***        AND a~msgid   IN so_msgid
**        AND c~mtart   IN so_mtart
***        AND c~matkl   IN so_matkl
**        AND c~profl IN so_profl
**        AND a~msgty   = 'E'
**     "exclude System Lock: 10.31.2011 by Victor
**        AND a~msgid   <> 'M3'
**        AND ( a~msgid   = 'M7'   AND a~msgno  <> '112' )
**        AND b~spras   =  sy-langu .
**
**    ELSE.
**      SELECT zdate  ztime  INTO (l_date, l_time)
**      FROM ztppaffw
**        UP TO 1 ROWS
**      ORDER BY zdate DESCENDING  ztime DESCENDING.
**      ENDSELECT.
**
**      SELECT a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort
**             a~bwart  a~sobkz   a~erfmg a~erfme  a~autyp a~msgid
**             a~msgno  a~msgty   a~budat a~erzet
**             a~fevor  a~dispo
**        INTO CORRESPONDING FIELDS OF TABLE it_data
**      FROM ztppaffw AS a INNER JOIN makt AS b
**                    ON a~matnr = b~matnr
**                     INNER JOIN mara AS c
**                    ON a~matnr  = c~matnr
**      WHERE a~zdate = l_date
**        AND a~ztime = l_time
**        AND a~budat <> sy-datum
**        AND a~bwart   = '261'
***    AND a~lgort   <> 'L001'
**        AND a~matnr   IN so_matnr
**        AND a~werks   IN so_werks
**        AND a~lgort   IN so_lgort
**        AND a~dispo   IN so_dispo
***        AND a~msgid   IN so_msgid
**        AND c~mtart   IN so_mtart
***        AND c~matkl   IN so_matkl
**        AND c~profl IN so_profl
**        AND a~msgty   = 'E'
**     "exclude System Lock: 10.31.2011 by Victor
**        AND a~msgid   <> 'M3'
**        AND ( a~msgid   = 'M7'   AND a~msgno  <> '112' )
**        AND b~spras   =  sy-langu .
**    ENDIF.
*
*** End on 03/05/12
*
*  ENDIF.
*
**  IF p_read = 'X'.
**    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_log
**    FROM ztppaffw_log
**    WHERE zdate IN s_log.
**  ELSE.
**    IF p_today = 'X'.
**  SELECT a~weblnr a~weblpos a~matnr b~maktx a~werks a~lgort a~charg
**        a~bwart  a~sobkz   a~erfmg a~erfme a~aufnr a~autyp a~msgid
**             a~msgno  a~msgty   a~budat a~erzet a~fwdat a~fwzet
**             a~fevor  a~dispo  a~msgv1   a~msgv2 a~msgv3 a~msgv4
**        INTO CORRESPONDING FIELDS OF TABLE it_data
**      FROM affw AS a INNER JOIN makt AS b
**                    ON a~matnr = b~matnr
**                     INNER JOIN mara AS c
**                    ON a~matnr  = c~matnr
**      WHERE  a~budat   >=  s_budat-low
**        AND  a~budat   <=  sy-datum
**        AND a~bwart   = '261'
***    AND a~lgort   <> 'L001'
**        AND a~matnr   IN so_matnr
**        AND a~werks   IN so_werks
**        AND a~lgort   IN so_lgort
**        AND a~dispo   IN so_dispo
**        AND a~msgid   IN so_msgid
**        AND c~mtart   IN so_mtart
**        AND c~matkl   IN so_matkl
**        AND a~msgty   = 'E'
**     "exclude System Lock: 10.31.2011 by Victor
**        AND a~msgid   <> 'M3'
**        AND ( a~msgid   = 'M7'   AND a~msgno  <> '112' )
**        AND b~spras   =  sy-langu .
**
**    ELSE.
**      SELECT zdate  ztime  INTO (l_date, l_time)
**      FROM ztppaffw
**        UP TO 1 ROWS
**      ORDER BY zdate DESCENDING  ztime DESCENDING.
**      ENDSELECT.
**
**      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
**      FROM ztppaffw
**      WHERE zdate = l_date
**        AND ztime = l_time
**        AND budat <> sy-datum.
**    ENDIF.
**  ENDIF.
*
*
**-data selection : D-1 6:00 ~ D 5:59
*
*** FUrong on 03/05/12
**  lv_from_time  = '060000'.
**  lv_to_time    = lv_from_time - 1.
**  CONCATENATE s_budat-low lv_from_time INTO from_date_time.
**  CONCATENATE sy-datum    lv_to_time   INTO to_date_time.
**
**  IF p_today = 'X'.
**    LOOP AT it_data.
**      CONCATENATE it_data-budat it_data-erzet INTO it_data-zzcdate.
**      IF  it_data-zzcdate <= from_date_time OR
**          it_data-zzcdate >= to_date_time.
**        DELETE it_data.
**      ENDIF.
**    ENDLOOP.
**  ENDIF.
*** End on 03/05/12

endform.                    " SELECT_DATA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA_ADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form modify_data_add .
  data : l_labst     type mard-labst,
          l_klabs     type mard-klabs,
          l_lsqty     type mard-labst.  "Available stock
  data : lv_zzcdate type ztimestamp.

  clear : it_send[], it_send,  it_cnt_add[], it_cnt_add,
         it_interface_add[], it_interface_add.

  check p_today  = 'X'  or p_old = 'X'.

  concatenate sy-datum sy-uzeit into lv_zzcdate.


*-budat + MATNR
  it_data_tmp[]  = it_data_add[].
  sort it_data_tmp by  matnr msgid msgno fwdat descending.
  delete adjacent duplicates from it_data_tmp
                    comparing  matnr msgid msgno.
  loop at it_data_add.

*    it_send-budat = it_data-budat.
    it_send-matnr = it_data_add-matnr.
    it_send-erfmg = it_data_add-erfmg.
    it_send-msgid = it_data_add-msgid.
    it_send-msgno = it_data_add-msgno.

    collect it_send.

  endloop.

*--final table : it_interface[]
*  SORT it_send BY budat matnr msgid msgno.
  sort it_send by  matnr msgid msgno.
  loop at it_data_tmp.
    clear : it_send.

    read table it_send with key " budat = it_data_tmp-budat
                                matnr = it_data_tmp-matnr
                                msgid = it_data_tmp-msgid
                                msgno = it_data_tmp-msgno
                                binary search.
    it_data_tmp-err_cnt = 1.
    it_data_tmp-zzcdate =  lv_zzcdate.
    it_data_tmp-werks   = 'HVA1'.
    it_data_tmp-zmandt  =  sy-mandt.


    move-corresponding it_data_tmp to it_interface_add.
    move-corresponding it_data_tmp to it_cnt_add.
*    it_cnt_add-fwdat  =  it_data_tmp-fwdat.               "Create Date

    if it_send-erfmg <> 0.
      it_interface_add-erfmg  = it_send-erfmg.
      append it_interface_add.
      collect it_cnt_add.
    endif.
  endloop.

endform.                    " MODIFY_DATA_ADD
*&---------------------------------------------------------------------*
*&      Form  PRO_BATCH_ADD
*&---------------------------------------------------------------------*
form pro_batch_add .
  data : v_dest(30) value 'WMPP01'.   "Interface Destination.

  clear : e_return.

*  CHECK p_old = 'X'.

*if case of no data, send only summary line.
  if  it_interface_add[] is initial.
    it_cnt_add-werks  =	'HVA1'.
    if s_budat-high = '00000000'.
      it_cnt_add-fwdat  = s_budat-low.
    else.
      it_cnt_add-fwdat  = s_budat-high.
    endif.
    it_cnt_add-err_cnt  = 0.
    concatenate sy-datum sy-uzeit into it_cnt_add-zzcdate.
    append it_cnt_add.
  endif.

  call function 'Z_PP_IF_OB_BACKLOG' destination v_dest
    importing
      e_return              = e_return
    tables
      t_data                = it_interface_add
      t_cnt                 = it_cnt_add
    exceptions
      communication_failure = 1  message l_msgtxt
      system_failure        = 2  message l_msgtxt.

  if e_return-type = 'S' and  sy-subrc = 0.   "Success
    perform save_log_add  using 'S' 'Success'    '' 'D1'.
    write : 'Interface : Success'.
    message s003 with 'Interface : Success'.
  else.
    perform save_log_add  using 'E' e_return-message l_msgtxt 'D1'.
    write :/ e_return-message, l_msgtxt.      "For Spool
    message e003 with  e_return-message l_msgtxt.
  endif.

endform.                    " PRO_BATCH_ADD
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG_ADD
*&---------------------------------------------------------------------*
form save_log_add  using    p_type p_msg1 p_msg2 p_gubun.

  data : l_zseq(10) type n.

  clear : it_save[], it_save.

  check not it_interface_add[] is initial.                  "07.06.2011

  select zseq into l_zseq
    from ztppaffw_log
    up to 1 rows
    where zdate = sy-datum
    order by zseq descending.
  endselect.

  loop at it_interface_add.
    move-corresponding it_interface_add to it_save.

    it_save-zdate = sy-datum.
    it_save-zseq  = l_zseq + sy-tabix.
    it_save-ztime = w_save_time.
*    it_save-ztime = sy-uzeit.
    it_save-ernam = sy-uname.
    it_save-zrslt = p_type.
    it_save-type  = p_gubun.

    if p_type = 'E'.
      if not p_msg1 is  initial.
        it_save-zmsg  = p_msg1.
      else.
        it_save-zmsg  = p_msg2.
      endif.
    endif.

** on 03/14/12 set reamin indicator
    it_save-remain = 'X'.
** end on 03/14/12

    append it_save.
    clear : it_save.

  endloop.

  insert ztppaffw_log from table it_save
                             accepting duplicate keys .
  commit work and wait.
endform.                    " SAVE_LOG_ADD
*&---------------------------------------------------------------------*
*&      Form  PRO_INTEGRATE
*&---------------------------------------------------------------------*
form pro_integrate .
  clear : it_alv.
  if it_interface[] is not initial.
    describe table it_interface lines lv_d0_lines.
*    LOOP AT it_interface.
*      MOVE-CORRESPONDING it_interface TO it_alv.
*      it_alv-type = 'D0'.
*      APPEND it_alv.
*    ENDLOOP.
  endif.

  if it_interface_add[] is not initial.
    loop at it_interface_add.
      move-corresponding it_interface_add to it_alv.
      it_alv-type = 'D1'.
      append it_alv.
    endloop.
  endif.

endform.                    " PRO_INTEGRATE
