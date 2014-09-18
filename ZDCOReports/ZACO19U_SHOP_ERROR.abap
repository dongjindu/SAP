*&--------------------------------------------------------------------
*& REPORT                 : ZACO19U_SHOP_ERROR
*& Author                 : HS Jung
*& Creation Date          : 10/25/2006
*& Specification By       : Andy Choi
*& Development Request No :
*& Addl documentation     :
*& Description            : Shop Cost collection Error display
*& Modification Log
*& Date     Developer      Request ID      Description
*&
*&--------------------------------------------------------------------

report zaco19u_shop_error message-id zmco
        no standard page heading line-size 118
                                 line-count 87.


* For TOP include
include zaco19u_shop_error_top.

include zco_alv_top.
include zco_alv_form .

* for combobox
type-pools: vrm.
data: it_rt     type vrm_values,
      w_rt_line like line of it_rt.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
selection-screen begin of block bl1 with frame title text-001.
* Planing Year
parameters : p_kokrs like ztco_batch_log-kokrs memory id cac obligatory,
             p_bdatj like ztco_batch_log-bdatj
             memory id bdtj obligatory,
             p_poper like ztco_batch_log-poper memory id vpe
             modif id per obligatory.
select-options: s_times for ztco_batch_log-times.
parameters : p_flag  like ztco_batch_log-flag default 'E'.
selection-screen end of block bl1.

parameters: p_rt(10) type c as listbox visible length 25." obligatory.

*---------------------------------------------------------------------*
initialization.
*---------------------------------------------------------------------*
  p_rt = 'SHOP_ACT'.

at selection-screen output.
* for combo box
*---report type
  data: begin of lt_log occurs 0,
          repid like ztco_batch_log-repid,
        end of lt_log.

  refresh it_rt.

  if p_kokrs = space.
    select distinct repid into table lt_log
       from ztco_batch_log.
  else.
    select distinct repid into table lt_log
       from ztco_batch_log
       where kokrs = p_kokrs.
*       and bdatj = p_bdatj
*       and poper = p_poper.
  endif.
  loop at lt_log.
    w_rt_line-key  = lt_log-repid.
*    w_rt_line-text = lt_log-repid.
    append w_rt_line to it_rt.
  endloop.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_RT'
      values = it_rt.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

* Log selection
  perform select_ztco_batch_log.

  perform display_data.


*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.
*-- ALV layout
  perform alv_set_layout   using  space  space
                                  space  space.
  gs_layout-box_fieldname          = 'CHK'.

  gs_layout-lights_tabname = 'IT_DISPLAY'.
  gs_layout-lights_fieldname = 'ICON_FIELD'.

*-- Event
  perform alv_get_event    using  gt_events.

*-- Fieldcategory
* PERFORM alv_get_fieldcat TABLES gt_fieldcat USING 'DISPLAY'.
* PERFORM alv_chg_fieldcat TABLES gt_fieldcat.
  perform call_alv_list.


*-- Sort
* perform set_sort         tables gt_alv_sort.

*-- Top of page
*  PERFORM SET_TOP_PAGE.

*-- Display
  perform alv_grid_display tables it_display.


endform.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  alv_chg_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*----------------------------------------------------------------------*
form alv_chg_fieldcat tables pt_fieldcat type slis_t_fieldcat_alv.
  read table it_display index 1.

  loop at pt_fieldcat into gs_fieldcat.
    clear :  gs_fieldcat-key, gs_fieldcat-no_out.

    if gs_fieldcat-col_pos < 5.
      gs_fieldcat-key     = 'X'.
    endif.

    case gs_fieldcat-fieldname.
      when 'MATNR'.
        set_fieldcat gs_fieldcat 'Product'.
      when 'MSG'.
        set_fieldcat gs_fieldcat 'Message'.
      when 'FLAG'.
        set_fieldcat gs_fieldcat 'STATUS'.
      when 'CHK'.
        set_fieldcat gs_fieldcat 'STATUS'.

    endcase.
    clear: gs_fieldcat-cfieldname,
           gs_fieldcat-ctabname.
    modify pt_fieldcat from gs_fieldcat.
  endloop.
endform.                    " alv_chg_fieldcat
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
form pf_status_set using rt_extab type slis_t_extab.
  set pf-status 'STANDARD'.
endform.                    " PF_STATUS_SET

*---------------------------------------------------------------------*
*      Form USER_COMMAND
*---------------------------------------------------------------------*
form user_command using r_ucomm like sy-ucomm
                                  rs_selfield type slis_selfield.

  case r_ucomm.
    when 'EXCU'.
      rs_selfield-refresh = 'X'.
      perform get_execute_item.

      case p_rt.
        when 'SHOP_ACT'.
          perform submit_ztco_shop_sum.

      endcase.
  endcase.

endform.                    "ALV_EVENT_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  select_ztco_BATCH_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_ztco_batch_log.

  data : l_repid type repid.

  l_repid = p_rt.

  if p_flag = 'A' or p_flag = ''.
    select  * into corresponding fields of table  it_log
         from ztco_batch_log
        where kokrs = p_kokrs
          and bdatj = p_bdatj
          and poper = p_poper
          and repid = l_repid
          and times in s_times.
  else.
    select  * into corresponding fields of table  it_log
         from ztco_batch_log
        where kokrs = p_kokrs
          and bdatj = p_bdatj
          and poper = p_poper
          and flag  = p_flag
          and repid = l_repid
          and times in s_times.
  endif.


  loop at it_log.
    move-corresponding it_log to it_display.
    case it_log-flag.
      when 'E' or 'X'.
        it_display-icon_field = 1.
      when 'F'.
        it_display-icon_field = 3.
      when others.
        it_display-icon_field = 2.
    endcase.
    append it_display. clear it_display.
  endloop.

endform.                    " select_ztco_BATCH_LOG
*&---------------------------------------------------------------------*
*&      Form  get_execute_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_execute_item.
  loop at it_display where chk = 'X'.
    move-corresponding it_display to it_excu.
    append it_excu. clear it_excu.
  endloop.


  if it_excu[] is initial.
    message e000 with 'Please select at least 1 product!!!'.
  endif.

endform.                    " get_execute_item
*&---------------------------------------------------------------------*
*&      Form  submit_ztco_shop_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form submit_ztco_shop_sum.
  data : g_batch(1).
  loop at it_excu.
    perform create_seltab.

    submit zaco19u_shop_new "IA SELECTION-SCREEN
    with selection-table seltab
    and return.

  endloop.
endform.                    " submit_ztco_shop_sum
*&---------------------------------------------------------------------*
*&      Form  create_seltab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_seltab.
  seltab-selname = 'S_MATNR'.
  seltab-kind    = 'S'.
  seltab-sign    = 'I' .
  seltab-option  = 'EQ'.
  seltab-low     = it_excu-matnr.
  seltab-high    = it_excu-matnr.
  append seltab. clear seltab.

  seltab-selname = 'P_KOKRS'.
  seltab-kind    = 'P'.
  seltab-low     =  p_kokrs.
  append seltab. clear seltab.

  seltab-selname = 'P_BDATJ'.
  seltab-kind    = 'P'.
  seltab-low     =  p_bdatj.
  append seltab. clear seltab.

  seltab-selname = 'P_PERAB'.
  seltab-kind    = 'P'.
  seltab-low     =  p_poper.
  append seltab. clear seltab.


endform.                    " create_seltab
*&---------------------------------------------------------------------*
*&      Form  call_alv_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_alv_list.

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.

  ls_fieldcat-fieldname  = 'ICON_FIELD'.
  ls_fieldcat-outputlen  = '04'.
  ls_fieldcat-just       = 'L'.
  ls_fieldcat-icon       = 'X'.
  append ls_fieldcat to gt_fieldcat.

  perform field_setting(zcogsrev) tables gt_fieldcat using :
 'MATNR'            'PRODUCT'    '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'AUFNR'            'Order Number' '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'TIMES'            'Counter'    '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'FLAG'             'FLAG'       '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MSG'              'Message'    '15' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ERDAT'            'Date'       '10' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ERZET'            'Time'       '08' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'ERNAM'            'User'       '12' ' ' 'L'  ' '  ' '  '  ' ' '  ' '.

endform.                    " call_alv_list
