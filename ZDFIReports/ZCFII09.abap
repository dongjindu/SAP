report zcfii09 message-id  zmfi.
*& to be deleted... replaced with other program for easy planning - ANDY
*&-------------------------------------------------------------------
*& Author                 : HS.JEONG
*& Creation Date          : 09/25/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No : UD1K902296
*& Addl documentation     :
*& Description  : AR-FM Create Monthly Budget
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------
type-pools: slis.
include <icon>.
include <symbol>.
class cl_gui_resources definition load.

constants:
  c_f2code               like sy-ucomm                    value '&ETA'.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv.


data: wa_repid like sy-repid,
      wa_cnt type i,
      wa_length type i,
      wa_var_save(1) type c             value  'A',
      wa_default(1)  type c,
      wa_exit(1) type c,
      wa_overall_chk,
      wa_overall_p_chk,
      wa_variant like disvariant,
      wa_var like disvariant,
      wa_alv_function_name(30) type c value 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) type c,
      w_int type i.
data  :f_flg.
*DATA: gt_list_top_of_page TYPE slis_t_listheader.

*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
*
tables: imak, imakpa, ztfi_imfm,taif2,impr.

data: it_out type table of imak with header line.
data: it_ztfi_pi type table of ztfi_pi with header line.

data: begin of gt_out occurs 0,
        posnr       like imak-posnr,
        posid       like imak-posid,
        txt50       like imakt-txt50,
        varnt       like imav-varnt,
        cost_center like bapiappreqmaster-rsp_cost_center,
*        status,
        s_status     like bapiappreqstatus-status,
        status     like bapiappreqstatus-status,
        status_txt(10),
        ivart       like imak-ivart,
        gjahr       like imak-gjahr,
        overall(15) type p, " DECIMALS 2,
        planall(15) type p, " DECIMALS 2,
        before(15)  type p, " DECIMALS 2,
        last(14)    type p, " DECIMALS 2,
        year(14)    type p, " DECIMALS 2,
        year1(14)   type p, " DECIMALS 2,
        year2(14)   type p, " DECIMALS 2,
        year3(14)   type p, " DECIMALS 2,
        after(15)   type p, " DECIMALS 2,
        pi(15)      type p,
        min         like imak-gjahr,
        max         like imak-gjahr.
data:   chkbox type c,
        light   type c,
        tabcolor     type slis_t_specialcol_alv,
      end of gt_out.

data: g_minyear         like imak-gjahr.

data: begin of gt_ar occurs 0,
        posnr       like imak-posnr,
        txt50       like imakt-txt50,
        ivart       like imak-ivart,
        gjahr       like imak-gjahr,
        overall(15) type p, " DECIMALS 2,
        before(15)  type p, " DECIMALS 2,
        last(15)    type p, " DECIMALS 2,
        year(15)    type p, " DECIMALS 2,
        year1(15)   type p, " DECIMALS 2,
        year2(15)   type p, " DECIMALS 2,
        year3(15)   type p, " DECIMALS 2,
        after(15)   type p. " DECIMALS 2.
data:   chkbox type c,
        light   type c,
        tabcolor     type slis_t_specialcol_alv,
      end of gt_ar.
*********************Double click
data: begin of gt_out1 occurs 0,
        posnr       like imak-posnr,
        txt50       like imakt-txt50,
        ivart       like imak-ivart.
data:   chkbox type c,
        light   type c,
        tabcolor     type slis_t_specialcol_alv,
      end of gt_out1.
*----Internal decl
data : begin of it_imakt occurs 0.
        include structure imakt.
data : end of it_imakt.

data : begin of it_imakpa occurs 0.
        include structure imakpa.
data : end of it_imakpa.
*----Internal decl
data: it_ztfi_imfm type table of ztfi_imfm with header line.

*DATA : BEGIN OF it_ZTFI_IMFM OCCURS 0.
*        INCLUDE STRUCTURE ZTFI_IMFM.
*DATA : END OF it_ZTFI_IMFM.
*-----AR Detail  From Bapi
data : wa_master_data like  bapiappreqmaster.
data : wa_user_field       like   bapiapprequser.
data : wa_parent  like bapiprogaux-parent.

data : wa_co_area like bapi_appreq_id-cntrl_area.

data : it_variant like bapiappreqvarntmulti occurs 0 with header line.
data : it_variant_to_version like bapiappreqvarntassignmulti occurs 0
                                                 with header line.
data : it_invest_reson like bapiappreqinvreason occurs 0
                                             with header line.
data : it_env_invest   like bapiappreqenvinvest  occurs 0
                                             with header line.
data : it_org_units like bapiappreqorgunit occurs 0
                                             with header line.
data: it_status like bapiappreqstatus occurs 0 with header line.
data: it_user_status like bapiappreqstatus occurs 0 with header line.
data : begin of it_plan_tot occurs 0.
        include structure bapiappreqplantotalmulti.
data : end of it_plan_tot.

data : begin of it_plan_year occurs 0.
        include structure bapiappreqplanyearmulti.
data : end of it_plan_year.

data : it_return like bapiret2 occurs 0 with header line.

*====FOR BDC
data : it_bdc      like bdcdata occurs 0 with header line.
data:  it_messtab  like bdcmsgcoll occurs 0 with header line.
data : tcode like tstc-tcode.
*====Work area
data : wk_d_cnt   type i,
       wk_t_cnt type i,
       wa_t_cnt type i,
       wa_con,
       wa_varnt  like imav-varnt,
       wa_othbefore like bapiappreqplanyearmulti-fiscal_year,
       wa_before like bapiappreqplanyearmulti-fiscal_year,
       wa_before_txt(5),
       wa_last   like bapiappreqplanyearmulti-fiscal_year,
       wa_year   like bapiappreqplanyearmulti-fiscal_year,
       wa_year1  like bapiappreqplanyearmulti-fiscal_year,
       wa_year2  like bapiappreqplanyearmulti-fiscal_year,
       wa_year3  like bapiappreqplanyearmulti-fiscal_year,
       wa_year4  like bapiappreqplanyearmulti-fiscal_year,
       wa_year5  like bapiappreqplanyearmulti-fiscal_year,
       wa_after  like bapiappreqplanyearmulti-fiscal_year,
       wa_after_txt(5),
       ok_code  like sy-ucomm,
       wa_okcode1  like sy-ucomm,
       wa_okcode2  like sy-ucomm,
       ok_code1  like sy-ucomm,
       ok_code2  like sy-ucomm,
       wa_okcode  like sy-ucomm,
       wa_confirm,
       wa_save_check,
       wa_sum_check,
       wa_f_gjahr like imak-gjahr,
       wa_t_gjahr like imak-gjahr,
       wa_err_chk,
       wa_save_chk,
      wa_stat  like bapiappreqstatus-status,
*      wa_stat  LIKE ztfi_imfm-status,
       wa_chk,
       wa_tabix like sy-tabix,
       wa_return like bapiret2,
       wa_actual_tot like ztfi_imfm-tot,
       wa_cbo_tot    like ztfi_imfm-tot,
       wa_tot        like ztfi_imfm-tot,
       wa_pi         like ztfi_imfm-tot,
       wa_cha        like ztfi_imfm-tot,
       wa_status     like ztfi_imfm-status,
       wa_ar_sum   like ztfi_imfm-tot,
       wa_ov_sum   like ztfi_imfm-tot,
       wa_ar_sum1   like ztfi_imfm-tot,
       wa_ov_sum1   like ztfi_imfm-tot,
       wa_gjahr    like  imak-gjahr,
       wa_posid    like  impr-posid,
       wa_overall_act(15) type p,
       wa_monthly_tot(15)    type p.
.

*----NEXT SCREEN
data:   ss_posnr       like imak-posnr,
        ss_txt50       like imakt-txt50,
        ss_varnt       like imav-varnt,
        ss_cost_center like bapiappreqmaster-rsp_cost_center.
data : ss_gjahr like imak-gjahr.
data : wa_year_cnt type i.
*=====*
data : it_pi_budget_p like zfi_pi_budget
                      occurs 0 with header line.
data : it_pi_budget like zfi_pi_budget
                      occurs 0 with header line.
data : it_pi_actual like zfi_pi_actual
                      occurs 0 with header line.
data : begin of it_dis occurs 0,
        ss_year like bapiappreqplanyearmulti-fiscal_year,
        ss_year_txt(7),
        ss_ar(15)   type p, " DECIMALS 2,
        ss_ov(15)   type p, " DECIMALS 2,
        ss_re(15)   type p, " DECIMALS 2,
        ss_pi_p(15)   type p, " DECIMALS 2,
        ss_pi(15)   type p, " DECIMALS 2,
        ss_act(15)  type p, " DECIMALS 2,
       end of it_dis.
*============
data:  begin of it_month occurs 0,
        ss_year like bapiappreqplanyearmulti-fiscal_year,
        ss_mm1(12)    type p, " DECIMALS 2,
        ss_mm2(12)    type p, " DECIMALS 2,
        ss_mm3(12)    type p, " DECIMALS 2,
        ss_mm4(12)    type p, " DECIMALS 2,
        ss_mm5(12)    type p, " DECIMALS 2,
        ss_mm6(12)    type p, " DECIMALS 2,
        ss_mm7(12)    type p,  " DECIMALS 2,
        ss_mm8(12)    type p,  " DECIMALS 2,
        ss_mm9(12)    type p,  " DECIMALS 2,
        ss_mm10(12)    type p, " DECIMALS 2,
        ss_mm11(12)    type p, " DECIMALS 2,
        ss_mm12(12)    type p, " DECIMALS 2,
        ss_tot(14)    type p,
        chk(10),
       end of it_month.
*-----*
data : begin of it_err1 occurs 0,
        posnr       like imak-posnr,
        type,
        txt(50),
       end of it_err1.

data : begin of it_err2 occurs 0,
        year like bapiappreqplanyearmulti-fiscal_year,
        ar(15)    type p, " DECIMALS 2,
        act(15)   type p, " DECIMALS 2,
        diff(15) type p, " DECIMALS 2,
       end of it_err2.

data : begin of it_err occurs 0,
        year like bapiappreqplanyearmulti-fiscal_year,
        mm(15)   type p, " DECIMALS 2,
        ar(15)   type p, " DECIMALS 2,
        diff(15) type p, " DECIMALS 2,
       end of it_err.

controls: tc_0 type tableview using screen 0900.
controls: tc_1 type tableview using screen 0900.
controls: tc_2 type tableview using screen 0999.
ranges : r_status for  ztfi_imfm-status.
*---Start
data: it_aufk  type table of aufk with header line.
data: it_imzo type table of imzo with header line.
*data: it_impr type table of impr with header line.

data : it_budget   like zfi_io_budget occurs 0 with header line.
data : it_actual_f like zfi_io_actual occurs 0 with header line.
data : it_actual   like zfi_io_actual occurs 0 with header line.

data: begin of gt_out2 occurs 0,
        aufnr         like  aufk-aufnr,
        ktext         like  aufk-ktext,
        akstl         like  aufk-akstl,
        user0         like  aufk-user0,
        key(2),
        act(20),
        gjahr        like  imzo-gjahr,
        tot          like  cosp-wtg001,
        othbefore    like  cosp-wtg001,
        before       like  cosp-wtg001,
        last         like  cosp-wtg001,
        year         like  cosp-wtg001,
        year1        like  cosp-wtg001,
        year2        like  cosp-wtg001,
        year3        like  cosp-wtg001,
        year4        like  cosp-wtg001,
        year5        like  cosp-wtg001,
        after        like  cosp-wtg001,
        posnr        like  impr-posnr,
        posid        like  impr-posid,
        chkbox       type c,
        light        type c,
        tabcolor     type slis_t_specialcol_alv,
      end of gt_out2.
*---Temp
data: begin of gt_temp occurs 0,
        aufnr         like  aufk-aufnr,
        ktext         like  aufk-ktext,
        akstl         like  aufk-akstl,
        user0         like  aufk-user0,
        posnr        like  impr-posnr,
        posid        like  impr-posid,
      end of gt_temp.
*---Downpayment
data :  wa_d_tot          like  cosp-wtg001,
        wa_d_obefore       like  cosp-wtg001,
        wa_d_before       like  cosp-wtg001,
        wa_d_last         like  cosp-wtg001,
        wa_d_year         like  cosp-wtg001,
        wa_d_year1        like  cosp-wtg001,
        wa_d_year2        like  cosp-wtg001,
        wa_d_year3        like  cosp-wtg001,
        wa_d_year4        like  cosp-wtg001,
        wa_d_year5        like  cosp-wtg001,
        wa_d_after        like  cosp-wtg001.

data : wa_before_amt   like  cosp-wtg001,
       wa_after_amt    like  cosp-wtg001.
*---End
*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
selection-screen begin of block b22 with frame title text-010.
parameters :
  p_comp  like bapiappreqmaster-rsp_comp_code default 'H201',
  p_gjahr like bapiappreqplanyearmulti-fiscal_year
                                        obligatory
                                        default sy-datum+0(4),
  p_versn like imavz-versi default '0'.
selection-screen end of block b22.
*PARAMETERS :
*  p_appr LIKE bapiappreqplanyearmulti-fiscal_year
*                                        OBLIGATORY
*                                        DEFAULT sy-datum+0(4).
select-options:
  s_appr   for    imak-gjahr,
  s_ivart   for   imak-ivart,
  s_posnr   for   imak-posnr,
  s_vkostl  for   imak-vkostl,      "res c c
  s_akostl  for   imakpa-akostl,    "req c c
  s_abp     for   imak-STRATFLG.
*  s_prnam   FOR   impr-prnam."OBLIGATORY.

selection-screen skip.

selection-screen begin of block b33 with frame title text-033.
PARAMETERS:
P_INIT RADIOBUTTON GROUP RAD1 ,
P_PROG RADIOBUTTON GROUP RAD1 DEFAULT 'X'.

parameters :   p_chk1 as checkbox default 'X',
               p_chk2 as checkbox,
               p_chk3 as checkbox.
selection-screen end of block b33.

selection-screen begin of block b2 with frame.
parameters :
  p_layout like disvariant-variant.   "LAYOUT
selection-screen end of block b2.
*----------------------------------------------------------------------
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------
at selection-screen on value-request for p_layout.
  perform f4_variant changing p_layout.
*----------------------------------------------------------------------
* INITIALIZATION
*----------------------------------------------------------------------
initialization.
  wa_repid = sy-repid.
* ==> Change Variant saving type
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
*  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  refresh : gt_fieldcat.
  clear   : gs_layout.

  s_appr-low = sy-datum+0(4).
  s_appr-option = 'EQ'.
  s_appr-sign   = 'I'.
  append s_appr.
*  IMPORT wa_gjahr wa_posnr
*          FROM MEMORY.
*---------------------------------------------------------------------
*    M   A   I   N
*---------------------------------------------------------------------
end-of-selection.
  tables: t001.
  select single * from t001 where bukrs = p_comp.

  if p_chk1 = ' ' and p_chk2 = ' ' and p_chk3 = ' '.
    exit.
  endif.

*--- fill data investment value
  perform set_year.
* ==> 5. build field category
  perform build_field_category
  using :
   'POSNR'     'AR Number'    '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'TXT50'     'Name'         '12' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'VARNT'     'Vari'         '04' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'STATUS_TXT'  'Status'     '11' ' ' 'L'  ' '  ' '  '  ' '  ' ,
   'OVERALL'   'Overall'      '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'BEFORE'     wa_before_txt '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'LAST'       wa_last       '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR'       wa_year       '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR1'      wa_year1      '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR2'      wa_year2      '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'YEAR3'      wa_year3      '13' ' ' 'R'  ' '  ' '  '  ' '  ' ,
   'AFTER'      wa_after_txt  '13' ' ' 'R'  ' '  ' '  '  ' '  ' .
* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'VTWEG'   'X'   'X'   '*'.
* ==> 1. select data from db
  perform initial_data.
  perform select_data.
  perform fill_gt_out.
  perform check_status.
  if gt_out[] is initial.
    message s000(zmfi) with 'No Exist Data'.
    exit.
  endif.
* ==> 2. set variant default
  perform set_variant changing wa_var.
* ==> 3. set layout for alv style
  perform set_layout changing gs_layout.
* ==> 4. set events for alv
  perform set_events changing gt_events.
* ==> 7. call function display alv.

  call function wa_alv_function_name
    exporting
      i_callback_program       = wa_repid
      i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      it_special_groups        = gt_sp_group[]
      it_sort                  = gt_sorts[]
*     IT_FILTER                =
      i_default                = wa_default
      i_save                   = wa_var_save
      is_variant               = wa_var
      it_events                = gt_events[]
      is_print                 = gs_prnt
*     IT_EVENT_EXIT            =
*     I_SCREEN_START_COLUMN    = 10
*     I_SCREEN_START_LINE      = 2
*     I_SCREEN_END_COLUMN      = 80
*     I_SCREEN_END_LINE        = 23
    tables
      t_outtab                 = gt_out.
***********************************************************************

*&---------------------------------------------------------------------
*&      Form  select_data
*&---------------------------------------------------------------------
form select_data.
  data: lv_cnt type i.
  data: begin of lt_posid occurs 0,
          posid type ima_posnr,
        end of lt_posid.

  perform select_ar_masters.

  describe table it_out lines wk_d_cnt.
*----GET AR Text
  if wk_d_cnt > 0.
*Issue : FI-20040802-001 : 2004.08.10 Request by YCYOON
*----Start
*CHECK : Approval year
    select single * from taif2
           where gjahr eq p_gjahr
*          AND PRART EQ it_out-posnr
             and versi eq p_versn.

    if sy-subrc <> 0.
      refresh it_out.
      exit.
    endif.
*----end
    select * into corresponding fields of table it_imakt
    from imakt
    for all entries in it_out
    where spras = sy-langu
    and   posnr eq it_out-posnr.

    select * into corresponding fields of table it_imakpa
    from imakpa
    for all entries in it_out
    where posnr = it_out-posnr
    and   lfdnr = '001'.

    refresh : it_ztfi_imfm, it_month.
    clear   : it_ztfi_imfm, it_month.
    clear : wa_cnt.

    describe table it_out lines wa_cnt.
*Problem
*    SELECT * INTO TABLE it_ztfi_imfm
*             FROM ztfi_imfm
*                FOR ALL ENTRIES IN it_out
*                  WHERE  posid = it_out-posid
**    AND   ayear = it_out-gjahr
*                   AND   ayear = p_gjahr
*                   AND   gubun = '1'      "Original
*                   AND   seq   = '0000'
*                   AND   status IN r_status.
    if p_init = ' '.
      loop at  it_out.
        select  count( * ) into lv_cnt
                 from ztfi_imfm
                      where  posid = it_out-posid
                       and   ayear = p_gjahr
                       and   gubun = 'P'.      "Original
*                       and   seq   = '0000'
        if sy-subrc = 0.
          delete table it_out from it_out.
        endif.
      endloop.
    else.
      loop at it_out.
        lt_posid-posid = gt_out-posid.
        append lt_posid.
      endloop.

* please FIXME -> SUM ???
      select * into table it_ztfi_imfm
          from ztfi_imfm
                 for all entries in lt_posid
                   where  posid = lt_posid-posid
                    and   ayear = p_gjahr
                    and   gubun = 'P'      "Original
*                    and   seq   = '0000'
                    and   status in r_status.
    endif.

*---pi check
    refresh : it_ztfi_pi.
    clear   : it_ztfi_pi.
    select * into table it_ztfi_pi
    from ztfi_pi.
  endif.

endform.                    " select_data

*---------------------------------------------------------------------*
*  FORM alv_event_pf_status_set
*---------------------------------------------------------------------*
form alv_event_pf_status_set using rt_extab type slis_t_extab.
                                                            "#EC *
  if wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    set pf-status 'STANDARD_GRID' excluding rt_extab.
  else.
    set pf-status 'STANDARD' excluding rt_extab.
  endif.
  set titlebar  'STANDARD'.


endform.                    "alv_event_pf_status_set

*---------------------------------------------------------------------*
*  FORM alv_event_user_command
*---------------------------------------------------------------------*
form alv_event_user_command using r_ucomm     like sy-ucomm
                                      rs_selfield type slis_selfield.
                                                            "#EC *
  clear f_flg.
  case r_ucomm.
*   ---------------------------------- processing on double click.
    when '&IC1'.
      read table gt_out index rs_selfield-tabindex.
      clear wa_save_check.
      case rs_selfield-fieldname.
        when 'POSNR' or 'TXT50'.
          set parameter id 'IAF' field gt_out-posid.
          call transaction 'IMA3N' and skip first screen.
        when 'STATUS_TXT'.
          if gt_out-status = 'I0001' or
             gt_out-status = 'I0354'.
            if gt_out-varnt = ' '.
              perform popup_screen using wa_confirm.
              if wa_confirm = 'J'.
*                CHECK gt_out-status = 'I0354'.
                tcode = 'IMA2'.
                perform create_bdc using gt_out-posid.
                modify  gt_out index rs_selfield-tabindex.
                rs_selfield-refresh = 'X'.
              endif.
            endif.
          endif.
        when others.
          ss_posnr = gt_out-posnr.
          ss_txt50 = gt_out-txt50.
          ss_varnt = gt_out-varnt.
          ss_cost_center = gt_out-cost_center.
          wa_tabix = rs_selfield-tabindex.
*-----GET AR Detail.
          perform call_bapi_ar_detail using gt_out-posid.

*----find varaint
          clear : wa_varnt.
          perform get_variant changing wa_varnt p_gjahr.

          perform get_dis_data  using gt_out-posid
                                          wa_varnt
                                          p_gjahr.
*                                          gt_out-gjahr.

          perform get_monthly_data using gt_out-posid
*                                         gt_out-gjahr.
                                          p_gjahr.
          perform clear_process.

*                                          gt_out-gjahr.
          clear : wa_save_chk.
*          IF gt_out-varnt = ' '.    "VARAINT ASSIGN
*            PERFORM bapi_assign_variant USING gt_out-posid.
*          ENDIF.
          call screen 900   starting at 08 08  ending at 132 25 ."116
          perform refresh_dis_list.
          leave to screen 0.
          clear : wa_save_chk.
      endcase.
*   ---------------------------------- switching view type grid or list
    when 'LIST' or 'GRID'.
      perform switch_list_or_grid using r_ucomm.
*----FOR APPROVAL
    when '&FAPP'.
      perform auth_check using f_flg r_ucomm gt_out-status .
      if f_flg eq 'X'.
        exit.
      else.
        perform popup_screen using wa_confirm.
        if wa_confirm = 'J'.
          loop at gt_out where chkbox = 'X'.
*----PI check
*          PERFORM pi_check USING gt_out-posid.
*          IF wa_pi_chk = 'X'.
*            MESSAGE w009(zmfi) WITH gt_out-posid.
*            CONTINUE.
*          ENDIF.
            if gt_out-status <> 'R'.
              message w000(zmfi) with 'Not process'.
              continue.
            endif.
*          IF gt_out-s_status <> 'I0001'.
*            MESSAGE w000(zmfi) WITH 'Found Not Request SAP'.
*            CONTINUE.
*          ENDIF.
            if gt_out-status = 'R'.
              tcode = 'IMA2'.
              perform create_bdc using gt_out-posid.
              modify  gt_out. " INDEX rs_selfield-tabindex.
              rs_selfield-refresh = 'X'.
            endif.
          endloop.
        endif.
      endif.
*-----Approved
    when '&APPR'.
      perform auth_check using f_flg r_ucomm gt_out-status .
      if  f_flg eq 'X'.
        exit.
      else.
        perform popup_screen using wa_confirm.
        if wa_confirm = 'J'.
          loop at gt_out where chkbox = 'X'.
            if gt_out-light  = '1'.
              message w000(zmfi) with 'Not process'.
              continue.
            endif.
            if gt_out-status <> 'F'.
              message w000(zmfi) with 'Not process'.
              continue.
            endif.

*          IF gt_out-s_status <> 'I0354'.
*            MESSAGE w000(zmfi) WITH 'Not Ready for app SAP'.
*            CONTINUE.
*          ENDIF.
*
            if gt_out-status = 'F'.
              tcode = 'IMA2'.
              perform create_bdc using gt_out-posid.
              modify  gt_out. "INDEX rs_selfield-tabindex.
              rs_selfield-refresh = 'X'.
            endif.
          endloop.
        endif.
      endif.
*      rs_selfield-refresh = 'X'.
*-----Cancel
    when '&CANC'.
      perform auth_check using f_flg r_ucomm gt_out-status .
      if  f_flg eq 'X'.
        exit.
      else.
        perform popup_screen using wa_confirm.
        if wa_confirm = 'J'.
          loop at gt_out where chkbox = 'X'.
*          IF gt_out-s_status <> 'I0354'.
*            MESSAGE w000(zmfi) WITH 'Not Ready for app SAP'.
*            CONTINUE.
*          ENDIF.
*          IF gt_out-status <> 'F'.
*            MESSAGE w000(zmfi) WITH 'Not process'.
*            CONTINUE.
*          ENDIF.
            if gt_out-status = 'P' or gt_out-status = 'R'.
*            tcode = 'IMA2'.
*            PERFORM create_bdc_cancel USING gt_out-posid.
              if gt_out-status = 'P'.
                move 'Request'  to gt_out-status_txt.
                move 'R'                 to gt_out-status.
                perform update_status using gt_out-posid 'P' 'R'.
              elseif gt_out-status = 'R'.
                move 'Read For Approval'          to gt_out-status_txt.
                move 'A'                 to gt_out-status.
                perform update_status using gt_out-posid 'R' 'A'.
              endif.

              modify  gt_out. " INDEX rs_selfield-tabindex.
              rs_selfield-refresh = 'X'.

            endif.
          endloop.
        endif.
      endif.
  endcase.
  check r_ucomm eq 'LIST' or
        r_ucomm eq 'GRID'.

  rs_selfield-exit = 'X'.

endform.                    "alv_event_user_command
*&---------------------------------------------------------------------
*&      Form  set_variant
*&---------------------------------------------------------------------
form set_variant changing cs_vari type disvariant.

  check p_layout ne space.

  cs_vari-report      = sy-repid.
  cs_vari-handle      = space.
  cs_vari-log_group   = space.
  cs_vari-username    = space.
  cs_vari-variant     = p_layout.
  cs_vari-text        = space.
  cs_vari-dependvars  = space.

endform.                    " set_variant

*&---------------------------------------------------------------------
*&      Form  set_events
*&---------------------------------------------------------------------
form set_events changing ct_events type slis_t_event.

  field-symbols: <ls_event> type slis_alv_event.

  data: l_event type lvc_fname.

  call function 'REUSE_ALV_EVENTS_GET'
    exporting
      i_list_type     = 0
    importing
      et_events       = ct_events
    exceptions
      list_type_wrong = 1
      others          = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    delete ct_events where name ne 'END_OF_PAGE'
                       and name ne 'TOP_OF_PAGE'
                       and name ne 'TOP_OF_LIST'
                       and name ne 'END_OF_LIST'.
    loop at ct_events assigning <ls_event>.
      concatenate 'ALV_EVENT_'
                  <ls_event>-name
                  into <ls_event>-form.
    endloop.
  endif.

endform.                    " f01_set_evts


*&---------------------------------------------------------------------
*&      Form  set_layout
*&---------------------------------------------------------------------
form set_layout changing cs_layo type slis_layout_alv.

*... Display options
  cs_layo-colwidth_optimize      = space. "'X'.
  "?????
  cs_layo-no_colhead             = space.
  cs_layo-no_hotspot             = space.
  cs_layo-zebra                  = ' '.
  cs_layo-no_vline               = space.
  cs_layo-cell_merge             = space.
  cs_layo-no_min_linesize        = space.
  cs_layo-min_linesize           = space.
  cs_layo-max_linesize           = space.
  cs_layo-window_titlebar        = space.
  cs_layo-no_uline_hs            = space.
*... Edit
  cs_layo-edit                   = ' '."space.
  cs_layo-edit_mode              = ' '."space.
*... Exceptions
  cs_layo-lights_fieldname       = 'LIGHT'.
  "=> ??? ??? ???
  cs_layo-lights_tabname         = space.
  cs_layo-lights_rollname        = space.
  cs_layo-lights_condense        = space.
*... Sums
  cs_layo-no_sumchoice           = space.
  cs_layo-no_totalline           = space.
  cs_layo-totals_before_items    = space.
  cs_layo-totals_only            = space.
  cs_layo-totals_text            = space.
  cs_layo-no_subchoice           = space.
  cs_layo-no_subtotals           = space.
  cs_layo-subtotals_text         = space.
  cs_layo-numc_sum               = 'X'.
  cs_layo-no_unit_splitting      = space.
*... Interaction
  cs_layo-box_fieldname          = 'CHKBOX'.
  cs_layo-box_tabname            = space.
  cs_layo-box_rollname           = space.
  cs_layo-expand_fieldname       = space.
  cs_layo-hotspot_fieldname      = space.
  cs_layo-no_input               = ' '.
  cs_layo-f2code                 = space.
  cs_layo-confirmation_prompt    = space.
  cs_layo-key_hotspot            = space.
  cs_layo-flexible_key           = space.
  cs_layo-reprep                 = space.
  cs_layo-group_buttons          = 'X'.
  cs_layo-no_keyfix              = space.
  cs_layo-get_selinfos           = space.
  cs_layo-group_change_edit      = 'X'.
  cs_layo-no_scrolling           = space.
  cs_layo-expand_all             = space.
  cs_layo-no_author              = space.
*... Detailed screen
  cs_layo-detail_popup           = 'X'.
  cs_layo-detail_initial_lines   = space.
  cs_layo-detail_titlebar        = space.
*... PF-status
  cs_layo-def_status             = space.
*... Display variants
  cs_layo-header_text            = space.
  cs_layo-item_text              = space.
  cs_layo-default_item           = space.
*... colour
  cs_layo-info_fieldname         = space.
  cs_layo-coltab_fieldname       = 'TABCOLOR'.
*... others
  cs_layo-list_append            = space.

endform.                    " set_layout
*---------------------------------------------------------------------*
*  FORM f01_alv_event_top_of_page
*---------------------------------------------------------------------*
form alv_event_top_of_page.                                 "#EC CALLED
endform.                    "alv_event_top_of_page
*---------------------------------------------------------------------*
*       FORM alv_event_top_of_LIST                                    *
*---------------------------------------------------------------------*
form alv_event_top_of_list.                                 "#EC CALLED


endform.                    "alv_event_top_of_page
*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_page
*---------------------------------------------------------------------*
form alv_event_end_of_page.

endform.                    "alv_event_end_of_page
*---------------------------------------------------------------------*
*  FORM f01_alv_event_end_of_list
*---------------------------------------------------------------------*
form alv_event_end_of_list.


endform.                    "alv_event_end_of_list

*&---------------------------------------------------------------------
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------
form switch_list_or_grid using r_ucomm.

  data: ls_vari      type disvariant,
       ls_slis_layo type slis_layout_alv,
       lt_slis_fcat type slis_t_fieldcat_alv,
       lt_slis_sort type slis_t_sortinfo_alv,
       lt_slis_filt type slis_t_filter_alv,
       ls_slis_prnt type slis_print_alv.


  if r_ucomm = 'LIST' and
     wa_alv_function_name = 'REUSE_ALV_LIST_DISPLY'.
    exit.
  endif.
  if r_ucomm = 'GRID' and
     wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    exit.
  endif.
  case wa_alv_function_name.
    when 'REUSE_ALV_LIST_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_LIST_LAYOUT_INFO_GET'.
    when 'REUSE_ALV_GRID_DISPLAY'.
      wa_alv_get_info_name = 'REUSE_ALV_GRID_LAYOUT_INFO_GET'.

  endcase.

  call function wa_alv_get_info_name
    importing
      es_layout     = ls_slis_layo
      et_fieldcat   = lt_slis_fcat
      et_sort       = lt_slis_sort
      et_filter     = lt_slis_filt
      es_variant    = ls_vari
    exceptions
      no_infos      = 1
      program_error = 2
      others        = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  if r_ucomm = 'LIST'.
    wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
    call function wa_alv_function_name
      exporting
        i_callback_program       = wa_repid
        i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
        i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
        is_layout                = ls_slis_layo
        it_fieldcat              = lt_slis_fcat
        it_sort                  = lt_slis_sort
        it_filter                = lt_slis_filt
        i_default                = ' '  "gs_test-vari_default
        i_save                   = wa_var_save
        is_variant               = ls_vari
        is_print                 = ls_slis_prnt
        it_events                = gt_events[]
      tables
        t_outtab                 = gt_out
      exceptions
        program_error            = 1
        others                   = 2.
  endif.
  if r_ucomm = 'GRID'.
    wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
    call function wa_alv_function_name
      exporting
        i_callback_program       = wa_repid
        i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
        i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
        is_layout                = ls_slis_layo
        it_fieldcat              = lt_slis_fcat
        it_sort                  = lt_slis_sort
        it_filter                = lt_slis_filt
        i_default                = ' '  "gs_test-vari_default
        i_save                   = wa_var_save
        is_variant               = ls_vari
        is_print                 = ls_slis_prnt
*       it_events                = gt_events[]
      tables
        t_outtab                 = gt_out
      exceptions
        program_error            = 1
        others                   = 2.

  endif.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " switch_list_or_grid
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form build_field_category using
                                  p_fieldname       " field name
                                  p_title           " field title
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_edit            "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  .

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
  ls_fieldcat-fieldname = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l = p_title.
  ls_fieldcat-outputlen = p_outputlen.
  ls_fieldcat-key       = p_key.
  ls_fieldcat-just      = p_just.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  if p_fieldname = 'OVERALL'.
    ls_fieldcat-emphasize = 'C100'.
  endif.
  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
form f4_variant changing c_variant type disvariant-variant.

  data: ls_variant type disvariant,
        l_exit     type char1.

  ls_variant-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant          = ls_variant
      i_save              = 'A'
*     it_default_fieldcat =
    importing
      e_exit              = l_exit
      es_variant          = ls_variant
    exceptions
      not_found           = 2.
  if sy-subrc = 2.
    message id sy-msgid type 'S' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    if l_exit eq space.
      c_variant = ls_variant-variant.
    endif.
  endif.
endform.                    " f4_variant
*&---------------------------------------------------------------------*
*&      Form  build_sort_table
*&---------------------------------------------------------------------*
form build_sort_table using  p_spos
                             p_fieldname
                             p_up
                             p_subtot
                             p_group.
  data: ls_sort type slis_sortinfo_alv.

  ls_sort-spos      = p_spos.
  ls_sort-fieldname = p_fieldname.
  ls_sort-up        = p_up.
  ls_sort-subtot    = p_subtot.
  ls_sort-group     = p_group.
  append ls_sort to gt_sorts.
endform.                    " build_sort_table
*&---------------------------------------------------------------------*
*&      Form  set_line_color
*&---------------------------------------------------------------------*
form set_line_color using    p_color.
  data: ls_fieldcat   type slis_fieldcat_alv,
        lt_color      type slis_t_specialcol_alv,
        ls_color      type slis_specialcol_alv.

  refresh lt_color.
  clear   lt_color.
  loop at gt_fieldcat into ls_fieldcat.
    ls_color-fieldname = ls_fieldcat-fieldname.
    ls_color-color-col = p_color.
*    "cl_gui_resources=>list_col_positive.
    ls_color-color-int = cl_gui_resources=>list_intensified.
    ls_color-color-inv = 0.
    ls_color-nokeycol  = 'X'.
    append ls_color to lt_color.
    gt_out-tabcolor = lt_color.
  endloop.
endform.                    " set_line_color
*&---------------------------------------------------------------------*
*&      Form  call_bapi_ar_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSNR  text
*----------------------------------------------------------------------*
form call_bapi_ar_detail using    u_posid.
  refresh : it_variant,
            it_plan_tot, it_plan_year,
            it_invest_reson,
            it_org_units,
            it_invest_reson,
            it_variant_to_version,
            it_env_invest.

  clear : wa_master_data, wa_user_field, wa_co_area.

  call function 'BAPI_APPREQUEST_GETDETAIL'
    exporting
      externalnumber     = u_posid
      language           = sy-langu
*     LANGUAGE_ISO       =
    importing
      master_data        = wa_master_data
      user_fields        = wa_user_field
      controlling_area   = wa_co_area
    tables
      org_units          = it_org_units
      invest_reason      = it_invest_reson
      environmnt_invest  = it_env_invest
      variant            = it_variant
      variant_to_version = it_variant_to_version
      plan_total         = it_plan_tot
      plan_year          = it_plan_year
*     RETURN             =
    .
endform.                    " call_bapi_ar_detail
*&---------------------------------------------------------------------*
*&      Form  get_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VARNT  text
*----------------------------------------------------------------------*
form get_variant changing c_varnt u_gjahr.
  clear : c_varnt.
  read table it_variant_to_version
            with key appr_year    = u_gjahr                 "2004/02/05
                     plan_version = p_versn.                "'IM 0 '.
  if sy-subrc = 0.
    move it_variant_to_version-appreqvrnt  to c_varnt.
    move ' ' to  wa_con.
  else.
    move 'X' to  wa_con.
  endif.
endform.                    " get_variant
*&---------------------------------------------------------------------*
*&      Form  make_plan_year
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_VARNT  text
*----------------------------------------------------------------------*
form make_plan_year using    u_varnt.
  describe table it_plan_tot lines wk_d_cnt.
  if wk_d_cnt > 0.
*---Overall
    read table it_plan_tot with key appreqvrnt = u_varnt.
    if sy-subrc = 0.
      move it_plan_tot-investment_costs to gt_out-overall.
    endif.
*---find max min.
    sort it_plan_year  ascending by fiscal_year.
    read table it_plan_year index 1.
    if sy-subrc = 0.
      move it_plan_year-fiscal_year to g_minyear.
      move it_plan_year-fiscal_year to gt_out-min.
    endif.
*---find max min.
    sort it_plan_year  descending by fiscal_year.
    read table it_plan_year index 1.
    if sy-subrc = 0.
      move it_plan_year-fiscal_year to gt_out-max.
*----modify 2003.11.04
      gt_out-max = gt_out-max + 5.
    endif.
*---Year amount
    loop at it_plan_year where appreqvrnt = u_varnt.
      case  it_plan_year-fiscal_year.
        when  wa_last.   "Last year
          add it_plan_year-investment_costs to gt_out-last.
        when  wa_year.   "Current year
          move it_plan_year-investment_costs to gt_out-year.
        when  wa_year1.                                     " After 1
          move it_plan_year-investment_costs to gt_out-year1.
        when  wa_year2.                                     " After 2
          move it_plan_year-investment_costs to gt_out-year2.
        when  wa_year3.                                     " After 3
          move it_plan_year-investment_costs to gt_out-year3.
        when others.
          if it_plan_year-fiscal_year < wa_last.
            add it_plan_year-investment_costs to gt_out-before.
          elseif it_plan_year-fiscal_year > wa_year3.
            add  it_plan_year-investment_costs to gt_out-after.
          endif.
      endcase.
* total
      add  it_plan_year-investment_costs to gt_out-planall.
    endloop.
  endif.
endform.                    " make_plan_year
*&---------------------------------------------------------------------*
*&      Form  SET_YEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_year.
  clear : wa_before, wa_last, wa_year, wa_year1, wa_year2,
          wa_year3.
  wa_last  = p_gjahr - 1.
  wa_before =  wa_last - 1.
  wa_othbefore = wa_before - 1.
  wa_year  = p_gjahr.
  wa_year1 = p_gjahr + 1.
  wa_year2 = p_gjahr + 2.
  wa_year3 = p_gjahr + 3.
  wa_after = p_gjahr + 4.
*  CONCATENATE '~' wa_last  INTO wa_before_txt.
  concatenate '~' wa_before  into wa_before_txt.
  concatenate wa_after '~' into wa_after_txt.
endform.                    " SET_YEAR
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0900 output.
  set pf-status '900'.
  set titlebar '900' with ss_posnr ss_txt50 ss_varnt ss_cost_center.
endmodule.                 " STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit_screen input.
  leave to screen 0.
endmodule.                 " EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0900 input.
  clear wa_okcode.
*  clear wa_save_check.
  wa_okcode = ok_code.
  case wa_okcode.
    when 'SUM'.
      clear wa_sum_check.
      clear wa_save_check.
      perform sum_check.
      wa_sum_check = 'Q'.
    when 'CHK1'.
*-----2004/04/07 new insert
      perform chk_inv_pre.

*      PERFORM data_check_process1 USING gt_out-gjahr.
*      CLEAR wk_d_cnt.
*      DESCRIBE TABLE it_err2 LINES wk_d_cnt.
*      IF wk_d_cnt > 0.
*        CALL SCREEN 888 STARTING AT 40 06  ENDING AT 120 24 .
*        LEAVE TO SCREEN 900.
*      ENDIF.
**      PERFORM err_dis_process.
      clear ok_code2.
    when 'VAR'.
      if gt_out-varnt = ' '.    "VARAINT ASSIGN
        perform bapi_assign_variant using gt_out-posid gt_out-gjahr.
      else.
        message s000(zmfi) with 'Already Assigned Varaint'.
      endif.
    when 'SAV1'.
      clear ok_code.
      perform save_investmentcost.
**      if  wa_sum_check <> 'Q'.
**         message w000(zmfi) with 'Check Sum amount'.
**      endif.
*      CLEAR : wa_overall_chk.
*      PERFORM over_check.
*      IF wa_overall_chk = 'X'.
*        MESSAGE w000(zmfi) WITH 'Overall amount Check'.
*        EXIT.
*      ENDIF.
*      PERFORM chk_inv_pre.
*      IF gt_out-varnt = ' '.    "VARAINT ASSIGN
*        MESSAGE w000(zmfi) WITH 'Not Assigned Varaint'.
**        PERFORM bapi_assign_variant USING gt_out-posid.
*      ENDIF.
*
*      PERFORM ar_plan_update USING gt_out-posid gt_out-varnt.
*
    when 'EXIT'.
      refresh : it_month, it_dis.
      clear   : it_month, it_dis.
      clear ok_code.
*     2004/02/05 jhs modify
*      PERFORM initial_data.
*      PERFORM select_data.
*      PERFORM fill_gt_out.
*      PERFORM check_status.
      leave to screen 0.
    when 'SAVE'.
      perform save_investmentcost.

*      IF wa_save_check <> 'Q'.
*        MESSAGE w000(zmfi) WITH 'You should Save Plan First'.
*        MOVE ' ' TO  wa_save_check.
*        EXIT.
*      ENDIF.
*----end
      if wa_overall_p_chk = 'X'.
        message w000(zmfi) with 'Previous Overall <>  CBO total'.
        exit.
      endif.
      perform data_refresh_process.

      perform data_check_rpocess.

      perform save_process using gt_out-posid.
*      perform select_data.

      clear ok_code.
*      LEAVE TO SCREEN 0.
    when 'CHK'.
*      IF wa_save_check <> 'Q'.
*        MESSAGE w000(zmfi) WITH 'You should Save Plan First'.
*        MOVE ' ' TO  wa_save_check.
*        EXIT.
*      ENDIF.
      perform data_refresh_process.

      perform data_check_rpocess.

      move ' ' to wa_overall_p_chk.
*---2004/04/07
      if p_gjahr <> gt_out-gjahr.
        perform previous_chk.
      endif.
      if wa_overall_p_chk = 'X'.
        message w000(zmfi) with 'Previous Overall <>  CBO total'.
        exit.
      endif.

      if wa_err_chk = 'X'.
        message w000(zmfi) with 'Found error data'.
      endif.

      clear ok_code.
    when 'RST'.
*      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 900.
      perform data_refresh_process.
      perform data_check_rpocess.
      clear wk_d_cnt.
      describe table it_err lines wk_d_cnt.
      if wk_d_cnt > 0.
        call screen 999 starting at 40 06  ending at 116 20 .
      else.
        message w000(zmfi) with 'It is no difference'.
      endif.
*      PERFORM err_dis_process.
      clear ok_code.
*----2004/03/18
    when 'RET'.
      loop at it_month.
        move   ' '  : to  it_month-ss_tot,
                      to  it_month-ss_mm1,
                      to  it_month-ss_mm2,
                      to  it_month-ss_mm3,
                      to  it_month-ss_mm4,
                      to  it_month-ss_mm5,
                      to  it_month-ss_mm6,
                      to  it_month-ss_mm7,
                      to  it_month-ss_mm8,
                      to  it_month-ss_mm9,
                      to  it_month-ss_mm10,
                      to  it_month-ss_mm11,
                      to  it_month-ss_mm12.
        modify it_month.
      endloop.

    when others.

  endcase.
  clear ok_code.
endmodule.                 " USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*&      Module  INIT_PROC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module init_proc output.

endmodule.                 " INIT_PROC  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_process using u_posid.
  data: l_rc like sy-subrc.

  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
*     DEFAULTOPTION  = 'Y'
      textline1      = 'Are you really data save?'
*     TEXTLINE2      = ' '
      titel          = 'Data Save(Y/N)'
*     START_COLUMN   = 25
*     START_ROW      = 6
*     CANCEL_DISPLAY = 'X'
    importing
      answer         = wa_confirm.
  if wa_confirm = 'J'.
    perform pi_check using gt_out-posid l_rc.

    check l_rc = 0.
    clear wk_d_cnt.
    describe table it_err lines wk_d_cnt.
    if wk_d_cnt > 0.
      message s000(zmfi) with 'You should Check error list'.
      exit.
    else.
      move ' ' to wa_save_chk.
      loop at it_month. " WHERE ss_year >=  wa_gjahr. "p_gjahr.
        read table it_ztfi_imfm with key posid = gt_out-posid
                                         ayear = p_gjahr
                                         gjahr = it_month-ss_year.
        if sy-subrc <> 0.
*       CHECK it_month-ss_tot <> 0.
          if it_month-ss_mm1  = 0 and
             it_month-ss_mm2  = 0 and
             it_month-ss_mm3  = 0 and
             it_month-ss_mm4  = 0 and
             it_month-ss_mm5  = 0 and
             it_month-ss_mm6  = 0 and
             it_month-ss_mm7  = 0 and
             it_month-ss_mm8  = 0 and
             it_month-ss_mm9  = 0 and
             it_month-ss_mm10 = 0 and
             it_month-ss_mm11 = 0 and
             it_month-ss_mm12 = 0.

            select * from ztfi_imfm
               where posid = gt_out-posid
                 and ayear = p_gjahr
                 and gjahr = it_month-ss_year.
              delete ztfi_imfm.
            endselect.

            continue.
          endif.
        endif.

        move :  sy-mandt           to ztfi_imfm-mandt,
                ss_posnr           to ztfi_imfm-posnr,
                p_gjahr            to ztfi_imfm-ayear,
*                gt_out-gjahr TO ztfi_imfm-ayear,
                u_posid            to ztfi_imfm-posid,
                '0000'             to ztfi_imfm-seq,
                gt_out-posnr       to ztfi_imfm-posnr,
*                gt_out-cost_center to ztfi_imfm-kostl,
                sy-uname           to ztfi_imfm-uname,
                sy-datum           to ztfi_imfm-zdate.
*---Start
        if gt_out-posid(1) eq 'P'.
          move 'HMMA0001' to ztfi_imfm-prnam.
        elseif gt_out-posid(1) eq 'H'.
          move 'HMMA0002' to ztfi_imfm-prnam.
        endif.
        select single ivart into ztfi_imfm-type
         from imak
          where posnr eq gt_out-posid.
*---End
        move  : it_month-ss_year   to ztfi_imfm-gjahr,
               '1'                 to ztfi_imfm-gubun,
                t001-waers         to ztfi_imfm-twaer,
                it_month-ss_mm1    to ztfi_imfm-wtp01,
                it_month-ss_mm2    to ztfi_imfm-wtp02,
                it_month-ss_mm3    to ztfi_imfm-wtp03,
                it_month-ss_mm4    to ztfi_imfm-wtp04,
                it_month-ss_mm5    to ztfi_imfm-wtp05,
                it_month-ss_mm6    to ztfi_imfm-wtp06,
                it_month-ss_mm7    to ztfi_imfm-wtp07,
                it_month-ss_mm8    to ztfi_imfm-wtp08,
                it_month-ss_mm9    to ztfi_imfm-wtp09,
                it_month-ss_mm10   to ztfi_imfm-wtp10,
                it_month-ss_mm11   to ztfi_imfm-wtp11,
                it_month-ss_mm12   to ztfi_imfm-wtp12.
        clear ztfi_imfm-tot.
        ztfi_imfm-tot = it_month-ss_mm1  + it_month-ss_mm2
                      + it_month-ss_mm3  + it_month-ss_mm4
                      + it_month-ss_mm5  + it_month-ss_mm6
                      + it_month-ss_mm7  + it_month-ss_mm8
                      + it_month-ss_mm9  + it_month-ss_mm10
                      + it_month-ss_mm11 + it_month-ss_mm12.
*       MOVE  1     TO ztfi_imfm-seq.
        move 'R'    to ztfi_imfm-status.
        move 'Q' to wa_save_chk.
        modify ztfi_imfm.
        clear  ztfi_imfm.
      endloop.
      if wa_save_chk = 'Q'.
        message s000(zmfi) with 'Success Save  : Monthly '.
        move 'Request'        to  gt_out-status_txt.
        modify gt_out transporting status_txt
                  where posid = u_posid.

      else.
        message s000(zmfi) with 'Error : Save '.
      endif.
    endif.
  endif.
endform.                    " SAVE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  GET_MONTHLY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSNR  text
*      -->P_p_gjahr  text
*----------------------------------------------------------------------*
form get_monthly_data using    u_posid
                               u_gjahr.
  refresh : it_ztfi_imfm, it_month.
  clear   : it_ztfi_imfm, it_month.
**fixit

  perform move_monthly_data using gt_out-posid
                                   p_gjahr.
*---Start
*addition : actual + " down payment"

* what is this for???
  if gt_out-status = space.
    perform add_monthly_data.
  endif.
*---End
endform.                    " GET_MONTHLY_DATA
*&---------------------------------------------------------------------*
*&      Form  MOVE_MONTHLY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_monthly_data using u_posid u_gjahr.
  clear wa_year_cnt.
  clear : wa_year_cnt, wa_f_gjahr.

*======*
  wa_year_cnt = gt_out-max - gt_out-min + 1.
  wa_f_gjahr = gt_out-min.
  refresh : it_month, it_ztfi_imfm.
  clear   it_month.

  perform get_planned_amt using u_posid.

  do wa_year_cnt times.
    it_month-ss_year = wa_f_gjahr.
    it_month-chk = ' '.

    if wa_f_gjahr < p_gjahr.
      perform move_actual_data using u_posid wa_f_gjahr.
    else.
      read table it_ztfi_imfm with key gjahr = wa_f_gjahr.
      if sy-subrc = 0.
        perform move_cbotable_data using  ' '.
      else.
        perform move_actual_data using u_posid wa_f_gjahr.
      endif.
    endif.

    append it_month.
    clear : it_month.
    add 1 to wa_f_gjahr.
  enddo.

**if wa_year_cnt > 1.
*  DO wa_year_cnt TIMES.
*    MOVE wa_f_gjahr TO it_month-ss_year.
**---get actual by monthly
*    IF wa_f_gjahr <  p_gjahr."wa_gjahr. "p_gjahr. 2004/01/06
*
*      PERFORM move_actual_data USING u_posid
*                                     wa_f_gjahr.
*    ELSEIF wa_f_gjahr EQ wa_last.
*
*    ELSE.
*
*
*      READ TABLE it_ztfi_imfm WITH KEY posid = gt_out-posid
**                                       ayear = p_gjahr
*                                       ayear = u_gjahr
*                                       gjahr = wa_f_gjahr
*                                       gubun = '1'.       "original
*      MOVE ' '                TO it_month-chk.
*      IF sy-subrc = 0.
*        MOVE it_ztfi_imfm-wtp01 TO it_month-ss_mm1.
*        MOVE it_ztfi_imfm-wtp02 TO it_month-ss_mm2.
*        MOVE it_ztfi_imfm-wtp03 TO it_month-ss_mm3.
*        MOVE it_ztfi_imfm-wtp04 TO it_month-ss_mm4.
*        MOVE it_ztfi_imfm-wtp05 TO it_month-ss_mm5.
*        MOVE it_ztfi_imfm-wtp06 TO it_month-ss_mm6.
*        MOVE it_ztfi_imfm-wtp07 TO it_month-ss_mm7.
*        MOVE it_ztfi_imfm-wtp08 TO it_month-ss_mm8.
*        MOVE it_ztfi_imfm-wtp09 TO it_month-ss_mm9.
*        MOVE it_ztfi_imfm-wtp10 TO it_month-ss_mm10.
*        MOVE it_ztfi_imfm-wtp11 TO it_month-ss_mm11.
*        MOVE it_ztfi_imfm-wtp12 TO it_month-ss_mm12.
*        MOVE ' '                TO it_month-chk.
*        CLEAR it_month-ss_tot.
*        it_month-ss_tot = it_ztfi_imfm-wtp01 + it_ztfi_imfm-wtp02 +
*                          it_ztfi_imfm-wtp03 + it_ztfi_imfm-wtp04 +
*                          it_ztfi_imfm-wtp05 + it_ztfi_imfm-wtp06 +
*                          it_ztfi_imfm-wtp07 + it_ztfi_imfm-wtp08 +
*                          it_ztfi_imfm-wtp09 + it_ztfi_imfm-wtp10 +
*                          it_ztfi_imfm-wtp11 + it_ztfi_imfm-wtp12.
*      ENDIF.
*
*    ENDIF.
*
*    APPEND it_month.
*    CLEAR : it_month, it_ztfi_imfm.
*    ADD 1 TO wa_f_gjahr.
*  ENDDO.
** endif.
endform.                    " MOVE_MONTHLY_DATA
*&---------------------------------------------------------------------*
*&      Form  CLEAR_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form clear_process.
*  CLEAR : SS_LAST_MM1, SS_LAST_MM2, SS_LAST_MM3,
*          SS_LAST_MM4, SS_LAST_MM5, SS_LAST_MM6,
*          SS_LAST_MM7, SS_LAST_MM8, SS_LAST_MM9,
*          SS_LAST_MM10, SS_LAST_MM11, SS_LAST_MM12.
endform.                    " CLEAR_PROCESS

* DECLARATION OF TABLECONTROL 'TC_1' ITSELF
*CONTROLS: tc_1 TYPE TABLEVIEW USING SCREEN 0900.

* INPUT MODULE FOR TABLECONTROL 'TC_1': MODIFY TABLE
module tc_1_modify input.
  it_month-ss_tot = it_month-ss_mm1 + it_month-ss_mm2 + it_month-ss_mm3
                  + it_month-ss_mm4 + it_month-ss_mm5 + it_month-ss_mm6
                  + it_month-ss_mm7 + it_month-ss_mm8 + it_month-ss_mm9
                  + it_month-ss_mm10 + it_month-ss_mm11 +
                  it_month-ss_mm12.

  modify it_month
    index tc_1-current_line.

* RECALC. AR INVESTMENT COST
*  move   it_month-ss_tot to it_dis-ss_ar.
*  modify it_dis transporting   ss_ar
*             where ss_year eq it_month-ss_year.
  loop at it_dis.
    read table it_month with key ss_year = it_dis-ss_year.
    if sy-subrc = 0.
      it_dis-ss_ar =  it_month-ss_tot.
    else.
      it_dis-ss_ar =  0.
    endif.
    modify it_dis.
  endloop.

  clear wa_sum_check.
  clear wa_save_check.
  perform sum_check.

endmodule.                    "tc_1_modify INPUT
*&---------------------------------------------------------------------*
*&      Module  CLEAR_PROC  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module clear_proc output.

endmodule.                 " CLEAR_PROC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_1_change_field_attr output.
endmodule.                 " TC_1_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SEL_DATA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module sel_data output.
  perform get_monthly_data using gt_out-posnr
                                 gt_out-gjahr.
*                                   p_gjahr.

  perform move_monthly_data using gt_out-posid gt_out-gjahr.
endmodule.                 " SEL_DATA  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  GET_AR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSNR  text
*----------------------------------------------------------------------*
form get_dis_data using    u_posid u_varnt u_gjahr.
*====== MAKE DISPLAY DATA & AR PLAN
  refresh it_dis.
  clear   it_dis.
  clear wa_year_cnt.
  clear : wa_f_gjahr.
*---2004/01/06 year setting.------------------------*
  call function 'CONVERSION_EXIT_POSID_INPUT'
    exporting
      input  = gt_out-posid+0(3)
    importing
      output = wa_posid.

  select  single abjhr bijhr
      into  (gt_out-min, gt_out-max)
      from  impr
      where gjahr = p_gjahr
        and posid = wa_posid.

* New AR...
  if sy-subrc <> 0 or gt_out-varnt = ' '.
    gt_out-min = gt_out-gjahr - 1.
    gt_out-max = gt_out-gjahr + 10.
  endif.

  wa_year_cnt = gt_out-max - gt_out-min + 1.
  wa_f_gjahr  = gt_out-min.



*---Overall
  read table it_plan_tot with key appreqvrnt = u_varnt.
  move '1111'  to  it_dis-ss_year.
  if sy-subrc = 0.
    move it_plan_tot-investment_costs to it_dis-ss_ar.
    move it_plan_tot-overhead_costs   to it_dis-ss_ov.
  endif.
  append it_dis.
  clear it_dis.

  do wa_year_cnt times.
    move wa_f_gjahr to it_dis-ss_year.
*--INITIAL VALUE FROM AR VARIANT
    read table it_plan_year with key
                                 appreqvrnt  = u_varnt
                                 fiscal_year = it_dis-ss_year.
    if sy-subrc = 0.
      move it_plan_year-investment_costs to it_dis-ss_ar.
      move it_plan_year-overhead_costs   to it_dis-ss_ov.
      move it_plan_year-revenue          to it_dis-ss_re.
    endif.
    append it_dis.
    clear : it_dis.
    add 1 to wa_f_gjahr.
  enddo.
*=========================================*
*====== MAKE PI BUDGET prevoius
  perform get_pi_budget_p using u_posid wa_last.
*====== MAKE PI BUDGET current
  data : c_year like bapiappreqplanyearmulti-fiscal_year.
  clear c_year.
  move sy-datum(4) to c_year.
  perform get_pi_budget using u_posid c_year."p_gjahr.

  clear wk_d_cnt.
  describe table it_pi_budget lines wk_d_cnt.
  if wk_d_cnt > 0.
    loop at it_dis.
*--- over all
      if it_dis-ss_year  = '1111'.
*--previous
        read table it_pi_budget_p with key posid = u_posid
                                         gjahr  = '1111'.
        if sy-subrc = 0.
          move it_pi_budget_p-wtjhr   to it_dis-ss_pi_p.
          modify it_dis.
        endif.
*---current
        read table it_pi_budget with key posid = u_posid
                                         gjahr  = '1111'.
        if sy-subrc = 0.
          move it_pi_budget-wtjhr   to it_dis-ss_pi.
          modify it_dis.
        endif.
      else.
*---previous annual
        read table it_pi_budget_p with key posid = u_posid
                                      gjahr  = it_dis-ss_year.
        if sy-subrc = 0.
          move it_pi_budget_p-wtjhr   to it_dis-ss_pi_p.
          modify it_dis.
        endif.
*---current annual
        read table it_pi_budget with key posid = u_posid
                                      gjahr  = it_dis-ss_year.
        if sy-subrc = 0.
          move it_pi_budget-wtjhr   to it_dis-ss_pi.
          modify it_dis.
        endif.
      endif.
    endloop.
  endif.
*===== make PI ACTUAL
  perform get_pi_actual using u_posid.

  clear : wk_d_cnt, wa_overall_act.
  describe table it_pi_actual lines wk_d_cnt.

  if wk_d_cnt > 0.
    loop at it_dis.
      read table it_pi_actual with key posid = u_posid
                                     gjahr  = it_dis-ss_year
                                     ippos = ' '
                                     wrttp = ' '.
      if sy-subrc = 0.
        move it_pi_actual-tot   to it_dis-ss_act.
        wa_overall_act = wa_overall_act + it_pi_actual-tot.
        modify it_dis.
      endif.
    endloop.
  endif.
*---
  loop at it_dis.
    if it_dis-ss_year = '1111'.
      move 'Overall' to it_dis-ss_year_txt.
      move wa_overall_act to it_dis-ss_act.   "actual overall
    else.
      move it_dis-ss_year to it_dis-ss_year_txt.
    endif.
    modify it_dis.
  endloop.
*Issue : FI-20040802-001
*Requested by Y.C.Yoon Changed by wskim 2004/08/14
*----Start
*ADD : Actual data + downpayment data
*  perform actual_downpayment using    u_posid u_varnt u_gjahr.
*Investment cost value correction with ACTUAL difference


*FIXIT
  perform correction_difference using    u_posid u_varnt u_gjahr.
*---End
endform.                    " GET_AR
*&---------------------------------------------------------------------*
*&      Form  GET_PI_BUDGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*----------------------------------------------------------------------*
form get_pi_budget using    u_posid u_gjahr.
  refresh : it_pi_budget.
  clear   : it_pi_budget.
  call function 'Z_FFI_GET_PI_BUDGET'
    exporting
      posid = u_posid
      prnam = ' '
      gjahr = u_gjahr
    tables
      out   = it_pi_budget.

endform.                    " GET_PI_BUDGET
*&---------------------------------------------------------------------*
*&      Form  GET_PI_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*----------------------------------------------------------------------*
form get_pi_actual using    u_posid.

  refresh : it_pi_actual.
  clear   : it_pi_actual.
  call function 'Z_FFI_GET_PI_ACTUAL'
    exporting
      posid         =  u_posid
* IMPORTING
*   AMT           =
    tables
      out           = it_pi_actual
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " GET_PI_ACTUAL

* DECLARATION OF TABLECONTROL 'TC_0' ITSELF
*&---------------------------------------------------------------------*
*&      Module  DATA_CNT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module data_cnt output.
  describe table it_month lines tc_1-lines.
  describe table it_dis   lines tc_0-lines.
endmodule.                 " DATA_CNT  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_RPOCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form data_check_rpocess.
  data : g_diff like it_dis-ss_ar.

  refresh : it_err.
  clear   : it_err, wa_err_chk,g_diff.
  loop at it_month where ss_year >= wa_gjahr. "p_gjahr.
    read table it_dis with key ss_year = it_month-ss_year.
    if sy-subrc = 0.
      if it_month-ss_tot <> it_dis-ss_ar.
        g_diff = it_dis-ss_ar - it_month-ss_tot.
        if g_diff < -1 or   g_diff  > 1.
          move it_month-ss_year  to it_err-year.
          move it_month-ss_tot   to it_err-mm.
          move it_dis-ss_ar      to it_err-ar.
          it_err-diff = it_dis-ss_ar - it_month-ss_tot.
          wa_err_chk = 'X'.
          append it_err.
          clear  it_err.
        endif.
      endif.
    endif.
  endloop.
endform.                    " DATA_CHECK_RPOCESS
*&---------------------------------------------------------------------*
*&      Form  ERR_DIS_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form err_dis_process.
  clear wk_d_cnt.
  describe table it_err lines wk_d_cnt.
endform.                    " ERR_DIS_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_REFRESH_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form data_refresh_process.
  data : wa_gjahr like impr-gjahr.
*  wa_gjahr = p_gjahr - 1.
  wa_gjahr = gt_out-gjahr - 1.
  clear wa_monthly_tot.
  loop at it_month.
    if it_month-ss_year < wa_gjahr. "p_gjahr .
      perform move_actual_data using gt_out-posid
                                     it_month-ss_year.
    endif.
    it_month-ss_tot = it_month-ss_mm1  + it_month-ss_mm2
                    + it_month-ss_mm3  + it_month-ss_mm4
                    + it_month-ss_mm5  + it_month-ss_mm6
                    + it_month-ss_mm7  + it_month-ss_mm8
                    + it_month-ss_mm9  + it_month-ss_mm10
                    + it_month-ss_mm11 + it_month-ss_mm12.
    wa_monthly_tot = wa_monthly_tot + it_month-ss_tot.
    modify it_month.
  endloop.

endform.                    " DATA_REFRESH_PROCESS

* DECLARATION OF TABLECONTROL 'TC_2' ITSELF
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0999  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0999 input.
  wa_okcode1 = ok_code1.
  clear ok_code1.
  case wa_okcode1.
    when 'EXIT'.
      leave to screen 0.
      clear ok_code1.
      clear wa_okcode1.
  endcase.
endmodule.                 " USER_COMMAND_0999  INPUT
*&---------------------------------------------------------------------*
*&      Form  MOVE_ACTUAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_WA_F_GJAHR  text
*----------------------------------------------------------------------*
form move_actual_data using    u_posid
                               u_wa_f_gjahr.

  move 'Actual'              to it_month-chk.
  read table it_pi_actual with key posid = u_posid
                                   gjahr = u_wa_f_gjahr
                                   ippos = ' '.
  if sy-subrc = 0.
    move it_pi_actual-wtg001   to it_month-ss_mm1.
    move it_pi_actual-wtg002   to it_month-ss_mm2.
    move it_pi_actual-wtg003   to it_month-ss_mm3.
    move it_pi_actual-wtg004   to it_month-ss_mm4.
    move it_pi_actual-wtg005   to it_month-ss_mm5.
    move it_pi_actual-wtg006   to it_month-ss_mm6.
    move it_pi_actual-wtg007   to it_month-ss_mm7.
    move it_pi_actual-wtg008   to it_month-ss_mm8.
    move it_pi_actual-wtg009   to it_month-ss_mm9.
    move it_pi_actual-wtg010   to it_month-ss_mm10.
    move it_pi_actual-wtg011   to it_month-ss_mm11.
    move it_pi_actual-wtg012   to it_month-ss_mm12.
    it_month-ss_tot = it_pi_actual-wtg001 + it_pi_actual-wtg002
                    + it_pi_actual-wtg003 + it_pi_actual-wtg004
                    + it_pi_actual-wtg005 + it_pi_actual-wtg006
                    + it_pi_actual-wtg007 + it_pi_actual-wtg008
                    + it_pi_actual-wtg009 + it_pi_actual-wtg010
                    + it_pi_actual-wtg011 + it_pi_actual-wtg012.
    move 'Actual'              to it_month-chk.
  else.
    move 0 to it_month-ss_mm1.
    move 0 to it_month-ss_mm2.
    move 0 to it_month-ss_mm3.
    move 0 to it_month-ss_mm4.
    move 0 to it_month-ss_mm5.
    move 0 to it_month-ss_mm6.
    move 0 to it_month-ss_mm7.
    move 0 to it_month-ss_mm8.
    move 0 to it_month-ss_mm9.
    move 0 to it_month-ss_mm10.
    move 0 to it_month-ss_mm11.
    move 0 to it_month-ss_mm12.
    move 0 to it_month-ss_tot.
  endif.
endform.                    " MOVE_ACTUAL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
form get_status using    u_posid
                changing c_stat like bapiappreqstatus-status
                         c_chk.
  refresh : it_status, it_user_status.
  clear   : it_status, it_user_status.
  call function 'BAPI_APPREQUEST_GETSTATUS'
    exporting
      externalnumber              = u_posid
      language                    = sy-langu
*     LANGUAGE_ISO                =
    tables
      apprequest_status           = it_status
      apprequest_user_status      = it_user_status
*     APPREQUESTVARNT_STATUS      =
*     APPREQUESTVARNT_USER_STATUS =
*     RETURN                      =
    .
*  WAIT  UP TO '2.0' SECONDS.
  call function 'BAPI_TRANSACTION_COMMIT'.

  read table it_status index 1.
  if sy-subrc = 0.
    c_stat = it_status-status.
  endif.
  clear : wk_t_cnt.
  describe table it_user_status lines wk_t_cnt.
  if wk_t_cnt < 1.
    move 'X' to c_chk.
  else.
    move ' '  to c_chk.
  endif.
endform.                    " GET_STATUS
*&---------------------------------------------------------------------*
*&      Form  get_status_appro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      <--P_WA_STAT  text
*----------------------------------------------------------------------*
form get_status_appro using    u_posid
                changing c_stat like bapiappreqstatus-status.
  refresh : it_status.
  clear   : it_status.
  call function 'BAPI_APPREQUEST_GETSTATUS'
    exporting
      externalnumber              = u_posid
      language                    = sy-langu
*     LANGUAGE_ISO                =
    tables
      apprequest_status           = it_status
*     apprequest_user_status      =
*     APPREQUESTVARNT_STATUS      =
*     APPREQUESTVARNT_USER_STATUS =
*     RETURN                      =
    .
*  WAIT  UP TO '2.0' SECONDS.
  call function 'BAPI_TRANSACTION_COMMIT'.

  read table it_status index 1.
  if sy-subrc = 0.
    c_stat = it_status-status.
    gt_out-status_txt = it_status-description.
  endif.
endform.                    " get_status_appro
*&---------------------------------------------------------------------*
*&      Form  create_bdc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
form create_bdc using    u_posid.
  refresh : it_bdc.
  clear   : it_bdc.

  refresh : it_bdc.
  clear   : it_bdc.
*----
  if gt_out-status = 'P'.
    if gt_out-s_status = 'I0001'.
      perform bdc_change_status using u_posid.
    else.
      move 'Requested'  to gt_out-status_txt.
      move 'R'                 to gt_out-status.
      perform update_status using gt_out-posid 'P' 'R'.
    endif.
  elseif gt_out-status = 'R'.
    if gt_out-s_status = 'I0354'.
      perform bdc_change_status using u_posid.
    else.
      move 'Approved'          to gt_out-status_txt.
      move 'A'                 to gt_out-status.
      move 'I0364'             to gt_out-s_status.
      perform update_status using gt_out-posid 'R' 'A'.
    endif.
  endif.
endform.                    " create_bdc
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3467   text
*      -->P_3468   text
*      -->P_3469   text
*----------------------------------------------------------------------*
form make_bdc_rtn using   dynbegin program dynpro.
  clear it_bdc.
  if dynbegin = 'X'.
    it_bdc-program  = program.
    it_bdc-dynpro   = dynpro.
    it_bdc-dynbegin = 'X'.
  else.
    it_bdc-fnam     = program.
    it_bdc-fval     = dynpro.
  endif.
  append it_bdc.
endform.                    " make_bdc_rtn
*&---------------------------------------------------------------------*
*&      Form  AR_PLAN_UPDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
form ar_plan_update using    u_posid u_varnt.
  data : it_ar_plan like bapiappreqplanyear occurs 0
                          with header line.
  data : wa_ar_total like bapiappreqplantotal.

  clear : it_ar_plan, it_ar_plan[].

  read table it_dis with key ss_year = '1111'.
  if sy-subrc = 0.
    move it_dis-ss_ar    to wa_ar_total-investment_costs.
    move it_dis-ss_ov    to wa_ar_total-overhead_costs.
  endif.

  clear : wa_ar_sum, wa_ov_sum.
  refresh : it_err2.
  clear   : it_err2.
  loop at it_dis where ss_year <> '1111'.
*    CHECK it_dis-ss_year >= wa_last.
*---AR Vaule VS Actual Value
    if it_dis-ss_year < wa_last.
      if it_dis-ss_ar <> it_dis-ss_act.
        move it_dis-ss_year   to it_err2-year.
        move it_dis-ss_ar     to it_err2-ar.
        move it_dis-ss_act    to it_err2-act.
        it_err2-diff = it_dis-ss_ar - it_dis-ss_act.
        append it_err2.
        clear  it_err2.
      endif.
    elseif it_dis-ss_year = wa_last.
      if it_dis-ss_ar < it_dis-ss_act.
        move it_dis-ss_year   to it_err2-year.
        move it_dis-ss_ar     to it_err2-ar.
        move it_dis-ss_act    to it_err2-act.
        it_err2-diff = it_dis-ss_ar - it_dis-ss_act.
        append it_err2.
        clear  it_err2.
      endif.
**---2004/01/05
*      IF it_dis-ss_ar < it_ztfi_imfm-tot.
*        MOVE it_dis-ss_year   TO it_err2-year.
*        MOVE it_dis-ss_ar     TO it_err2-ar.
*        MOVE it_ztfi_imfm-tot TO it_err2-act.
*        it_err2-diff = it_dis-ss_ar - it_ztfi_imfm-tot.
*        APPEND it_err2.
*        CLEAR  it_err2.
*      ENDIF.
    endif.

    move it_dis-ss_year   to   it_ar_plan-fiscal_year.
    move it_dis-ss_ar     to   it_ar_plan-investment_costs.
    move it_dis-ss_ov     to   it_ar_plan-overhead_costs.
    move it_dis-ss_re     to   it_ar_plan-revenue.
    wa_ar_sum = wa_ar_sum + it_dis-ss_ar.
    wa_ov_sum = wa_ov_sum + it_dis-ss_ov.
    append it_ar_plan.
    clear  it_ar_plan.
  endloop.
  clear : wk_d_cnt.
  describe table it_err2 lines wk_d_cnt.
  if wk_d_cnt > 0.
    message i000(zmfi) with 'Founded Error Data'.
    leave to screen 900.
  endif.
*---Check Sum
  if  wa_ar_total-investment_costs <> wa_ar_sum    or
      wa_ar_total-overhead_costs   <> wa_ov_sum.
    message s000(zmfi) with 'Check Sum'.
  else.
    refresh : it_return.
    clear   : it_return.
*    CONCATENATE '00' gt_out-varnt INTO u_varnt.
    call function 'BAPI_APPREQUEST_SETPLANVALUES'
      exporting
        externalnumber                    = u_posid
        appropriationrequestvariant       = u_varnt "wa_varnt
*'0010' "u_varnt
        plan_total                        = wa_ar_total
*   TEST_RUN                          = ' '
     tables
       plan_year                         = it_ar_plan
       return                            = it_return.

    call function 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*  WAIT          =
     importing
       return        =  it_return.
  endif.

  .
  read table it_return index 1.
  if sy-subrc = 0.
    message s000(zmfi) with it_return-message.
  else.
*    MESSAGE s000(zmfi) WITH 'Success Save:InvestmentCost'.
    move 'Q' to wa_save_check.
  endif.
*====2004/02/05 jhs modify
  data : b_num type i,a_num type i.
  if  wa_save_check = 'Q'.
    b_num  = 1.a_num = 1.
    loop at it_ar_plan.
      case it_ar_plan-fiscal_year.
        when wa_last.
          move it_ar_plan-investment_costs to gt_out-last.
        when wa_year.
          move it_ar_plan-investment_costs to gt_out-year.
        when wa_year1.
          move it_ar_plan-investment_costs to gt_out-year1.
        when wa_year2.
          move it_ar_plan-investment_costs to gt_out-year2.
        when wa_year3.
          move it_ar_plan-investment_costs to gt_out-year3.
        when others.
          if it_ar_plan-fiscal_year < wa_last.
            read table gt_out with key posid = u_posid.
            if b_num = 1.
              clear  gt_out-before . b_num = b_num + 1.
            endif.
            gt_out-before = gt_out-before
                          + it_ar_plan-investment_costs.
          elseif it_ar_plan-fiscal_year > wa_year3.
            read table gt_out with key posid = u_posid.
            if a_num = 1.
              clear  gt_out-after . a_num = a_num + 1.
            endif.
            gt_out-after  = gt_out-after
                          + it_ar_plan-investment_costs.
          endif.
      endcase.
      modify gt_out transporting before last year year1
                                 year2 year3 after
                where posid = u_posid.
    endloop.
    gt_out-overall = wa_ar_total-investment_costs.
    modify gt_out transporting overall
              where posid = u_posid.
  endif.
endform.                    " AR_PLAN_UPDATE
*&---------------------------------------------------------------------*
*&      Module  TC_0_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_0_change_field_attr output.

endmodule.                 " TC_0_CHANGE_FIELD_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_0_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_0_modify input.
  modify it_dis
    index tc_0-current_line.
endmodule.                 " TC_0_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DIS_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form refresh_dis_list.
  call function wa_alv_function_name
    exporting
      i_callback_program       = wa_repid
      i_callback_pf_status_set = 'ALV_EVENT_PF_STATUS_SET'
      i_callback_user_command  = 'ALV_EVENT_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat[]
      it_special_groups        = gt_sp_group[]
      it_sort                  = gt_sorts[]
*     IT_FILTER                =
      i_default                = wa_default
      i_save                   = wa_var_save
      is_variant               = wa_var
      it_events                = gt_events[]
      is_print                 = gs_prnt
*     IT_EVENT_EXIT            =
*     I_SCREEN_START_COLUMN    = 10
*     I_SCREEN_START_LINE      = 2
*     I_SCREEN_END_COLUMN      = 80
*     I_SCREEN_END_LINE        = 23
    tables
      t_outtab                 = gt_out.
*          IF wa_save_chk = 'Q'.
*            MOVE 'Planned'   TO gt_out-status.
*            MODIFY gt_out INDEX rs_selfield-tabindex.
*            rs_selfield-refresh = 'X'.
*          ENDIF.

endform.                    " REFRESH_DIS_LIST
*&---------------------------------------------------------------------*
*&      Form  CAL_PI_ACTUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
form cal_pi_actual using    p_it_out_posid.
  clear : wa_actual_tot.
  loop at it_pi_actual where gjahr < gt_out-gjahr. " p_gjahr.
    wa_actual_tot = wa_actual_tot + it_pi_actual-tot.
  endloop.
endform.                    " CAL_PI_ACTUAL
*&---------------------------------------------------------------------*
*&      Form  CAL_CBO_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSNR  text
*      -->P_p_gjahr  text
*----------------------------------------------------------------------*
form cal_cbo_tot using    u_posnr
                          u_gjahr.
*fixit
  select * into table it_ztfi_imfm
      from ztfi_imfm
      where posid =  u_posnr
        and ayear = u_gjahr
        and gubun = '1'        "original
        and seq   = '0000'.    "plan

  clear : wa_cbo_tot.
  loop at it_ztfi_imfm.
    wa_cbo_tot = wa_cbo_tot + it_ztfi_imfm-tot.
  endloop.
endform.                    " CAL_CBO_TOT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSNR  text
*----------------------------------------------------------------------*
form update_status using    u_posid u_status u_status1.

  update  ztfi_imfm set status = u_status1
  where posid = u_posid
  and   ayear = p_gjahr
* AND   gjahr >= p_gjahr
  and   gubun  = 'P'
*  and   seq    = '0000'    "Plan
  and   status = u_status.

  if sy-subrc = 0.
    message s000(zmfi) with 'Update Success process'.
    if u_status1 = 'P'.
      move 'Parked'    to gt_out-status_txt.
    elseif u_status1 = 'R'.
      move 'Requested' to gt_out-status_txt.
    elseif u_status1 = 'A'.
      move 'Approved'  to gt_out-status_txt.
    else.
      move 'Unknown'   to gt_out-status_txt.
    endif.
  else.
    message s000(zmfi) with 'Update Error'.
    move 'Update Error' to gt_out-status_txt.
  endif.

endform.                    " UPDATE_STATUS
*&---------------------------------------------------------------------*
*&      Form  SUM_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sum_check.
  clear : wa_ar_sum, wa_ov_sum.
  loop at it_dis where ss_year <> '1111'.
*    CHECK it_dis-ss_year >= wa_last.
    wa_ar_sum = wa_ar_sum + it_dis-ss_ar.
    wa_ov_sum = wa_ov_sum + it_dis-ss_ov.
  endloop.

  loop at it_dis where ss_year = '1111'.
    it_dis-ss_ar = wa_ar_sum.
    it_dis-ss_ov = wa_ov_sum.
    modify it_dis index sy-tabix.
  endloop.
endform.                    " SUM_CHECK

* DECLARATION OF TABLECONTROL 'TC_3' ITSELF
controls: tc_3 type tableview using screen 0888.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0888  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0888 input.
  wa_okcode2 = ok_code2.
  clear ok_code2.
  case wa_okcode2.
    when 'EXIT'.
      leave to screen 0.
      clear ok_code2.
      clear wa_okcode2.
  endcase.
endmodule.                 " USER_COMMAND_0888  INPUT
*&---------------------------------------------------------------------*
*&      Form  DATA_CHECK_PROCESS1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form data_check_process1 using u_gjahr.
  refresh : it_err2.
  clear   : it_err2.
  read table it_ztfi_imfm with key posid = gt_out-posid
                                   ayear = u_gjahr "p_gjahr
                                   gjahr = wa_last
                                   gubun = '1'.       "original

  loop at it_dis where ss_year <> '1111'.
    check it_dis-ss_year <= wa_last.
*---AR Vaule VS Actual Value
    if it_dis-ss_year < wa_last.
      if it_dis-ss_ar <> it_dis-ss_act.
        move it_dis-ss_year   to it_err2-year.
        move it_dis-ss_ar     to it_err2-ar.
        move it_dis-ss_act    to it_err2-act.
        it_err2-diff = it_dis-ss_ar - it_dis-ss_act.
        append it_err2.
        clear  it_err2.
      endif.
    elseif it_dis-ss_year = wa_last.
      if it_dis-ss_ar < it_dis-ss_act.
        move it_dis-ss_year   to it_err2-year.
        move it_dis-ss_ar     to it_err2-ar.
        move it_dis-ss_act    to it_err2-act.
        it_err2-diff = it_dis-ss_ar - it_dis-ss_act.
        append it_err2.
        clear  it_err2.
      endif.
*---2004/01/05
      if it_dis-ss_ar < it_ztfi_imfm-tot.
        move it_dis-ss_year   to it_err2-year.
        move it_dis-ss_ar     to it_err2-ar.
        move it_ztfi_imfm-tot to it_err2-act.
        it_err2-diff = it_dis-ss_ar - it_ztfi_imfm-tot.
        append it_err2.
        clear  it_err2.
      endif.
    endif.
  endloop.
endform.                    " DATA_CHECK_PROCESS1
*&---------------------------------------------------------------------*
*&      Form  POPUP_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CONFIRM  text
*----------------------------------------------------------------------*
form popup_screen using    u_confirm.
  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
*     DEFAULTOPTION  = 'Y'
      textline1      = 'Are you really Process?'
*     TEXTLINE2      = ' '
      titel          = 'Process ok (Y/N)'
*     START_COLUMN   = 25
*     START_ROW      = 6
*     CANCEL_DISPLAY = 'X'
    importing
      answer         = u_confirm.

endform.                    " POPUP_SCREEN
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form bapi_assign_variant using u_posid u_gjahr.
  data : it_to_version like bapiappreqvarntassign
                          occurs 0 with header line,
         it_return     like   bapiret2
                          occurs 0 with header line.

  data : wa_varnt like bapi_appreq_id-appreqvrnt.

  refresh : it_to_version, it_return.
  clear   : it_to_version, it_return.
*---2005/02/05 jhs
*  MOVE p_appr TO it_to_version-appr_year.
  move u_gjahr to it_to_version-appr_year.
  move p_versn to it_to_version-plan_version.
*  MOVE 'IM'    TO it_to_version-plan_version.
  append it_to_version.
*--get varaint
  clear wa_varnt.
  select single varnt into wa_varnt
  from imav
  where posnr = u_posid.

  call function 'BAPI_APPREQUEST_ASSGNVRNTVERSN'
    exporting
      externalnumber              = u_posid
      appropriationrequestvariant = wa_varnt
*     TEST_RUN                    = ' '
    tables
      variant_to_version          = it_to_version
*     RETURN                      =
    .
  wait up to '1.8' seconds.

  call function 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*   WAIT          =
   importing
     return        = it_return
            .
  clear : wa_t_cnt.
  describe table it_return lines wa_t_cnt.
  if wa_t_cnt < 1.
    move wa_varnt to gt_out-varnt.
    message s000(zmfi) with 'Varaint assign'.
*---2004/03/18
    modify gt_out transporting varnt
                where posid = u_posid.
  else.
    message s000(zmfi) with 'Varaint Not assign'.
  endif.
endform.                    " ASSIGN_VARIANT
*&---------------------------------------------------------------------*
*&      Form  GET_CBO_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
form get_cbo_status using    u_posid u_gjahr .
  read table it_ztfi_imfm with key posid = u_posid
                                   ayear = u_gjahr "p_gjahr
                                   gubun = '1'
                                   seq   = '0000'.
  if sy-subrc = 0 .
    move  it_ztfi_imfm-status to gt_out-status.
  endif.
endform.                    " GET_CBO_STATUS
*&---------------------------------------------------------------------*
*&      Form  get_CBO_status_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_STATUS  text
*----------------------------------------------------------------------*
form get_cbo_status_name using    u_status.
  if u_status = 'R'.
    move 'Request'        to  gt_out-status_txt.
  elseif u_status = 'F'.
    move 'Ready for approval' to gt_out-status_txt.
  elseif u_status = 'A'.
    move 'Approved' to gt_out-status_txt.
  else.
    move 'Initial' to gt_out-status_txt.
  endif.
endform.                    " get_CBO_status_name
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module screen_control output.
  if gt_out-status = 'A'.
    loop at screen.
      if screen-group1 = 'G1'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  endif.
  if it_dis-ss_year eq  '1111' or it_dis-ss_year < wa_last.
    loop at screen.
      if screen-group1 = 'G1'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  endif.
endmodule.                 " SCREEN_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_bdc_cancel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
form create_bdc_cancel using    u_posid.
  refresh : it_bdc.
  clear   : it_bdc.

  refresh : it_bdc.
  clear   : it_bdc.
  perform make_bdc_rtn using :
                     'X'  'SAPLAIA1'        '0100',
                     ' '  'IMAK-POSNR'      u_posid,
                     ' '  'BDC_OKCODE'      '/00'.
  perform make_bdc_rtn using :
                      'X'  'SAPLAIA1'       '0200',
                     ' '  'BDC_OKCODE'      '=STAP'.
  perform make_bdc_rtn using :
                     'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=BUCH'.
  call transaction tcode   using it_bdc
                           mode   'A'
                           update 'S'
                    messages into it_messtab.

  read table it_messtab with key msgtyp = 'S'
                                 msgid = 'AO'
                                 msgnr = '010'.
  if sy-subrc = 0.
    if gt_out-status = 'F'.
      move 'Request'  to gt_out-status_txt.
      move 'R'                 to gt_out-status.
      move 'I0001'             to gt_out-s_status.
      perform update_status using gt_out-posid 'F' 'R'.
    elseif gt_out-status = 'A'.
      move 'Read For Approval'          to gt_out-status_txt.
      move 'F'                 to gt_out-status.
      move 'I0354'             to gt_out-s_status.
      perform update_status using gt_out-posid 'A' 'F'.
    endif.
  else.
    move 'failure'    to gt_out-status_txt.
  endif.
endform.                    " create_bdc_cancel
*&---------------------------------------------------------------------*
*&      Form  check_status
*&---------------------------------------------------------------------*
form check_status.
*------
  loop at gt_out.
**=== AR Plan VS AR monthly + actual
*    PERFORM get_pi_actual USING gt_out-posid.
*
*    PERFORM cal_pi_actual USING gt_out-posid.

    perform get_monthly_data using gt_out-posnr
*                                   gt_out-gjahr.
                                       p_gjahr.
    perform cal_cbo_tot using gt_out-posnr
*                              gt_out-gjahr.
                              p_gjahr.
    clear : wa_tot, wa_cha.
*   PERFORM get_pi_budget USING gt_out-posid p_gjahr.
    clear : wk_d_cnt, wa_pi.

*====== MAKE PI BUDGET prevoius
    perform get_pi_budget_p using gt_out-posid wa_last.


*    DESCRIBE TABLE it_pi_budget_p LINES wk_d_cnt.
*    IF wk_d_cnt > 0.
** check overall budget = total monthly budget
    read table it_pi_budget_p with key posid = gt_out-posid
                                     gjahr  = '1111'.

*      IF sy-subrc = 0.
*        MOVE it_pi_budget-wtjhr   TO wa_pi.
*      ENDIF.
*    ENDIF.
*
*    wa_cha = gt_out-planall - wa_pi.
*    wa_tot = wa_actual_tot + wa_cbo_tot.
*
    gt_out-light =  '3'.

    if it_pi_budget_p-wtjhr <> gt_out-overall.
      gt_out-light = '2'.
    endif.

    if gt_out-planall <> wa_cbo_tot.
      gt_out-light =  '1'.
    endif.

** check yearly budget = total monthly budget
*    DATA: l_yr1   LIKE imak-gjahr,
*          l_yr2   LIKE imak-gjahr,
*          l_yr3   LIKE imak-gjahr,
*          l_yr4   LIKE imak-gjahr,
*          l_yr5   LIKE imak-gjahr,
*          l_yr6   LIKE imak-gjahr,
*          l_yr7   LIKE imak-gjahr.
*    l_yr1 = g_minyear.
*    l_yr2 = g_minyear + 1.
*    l_yr3 = g_minyear + 2.
*    l_yr4 = g_minyear + 3.
*    l_yr5 = g_minyear + 4.
*    l_yr6 = g_minyear + 5.
*    l_yr7 = g_minyear + 6.
*
*    IF gt_out-light = '3'.
*      LOOP AT it_ztfi_imfm.
*        AT END OF gjahr.
*          SUM.
*          IF     it_ztfi_imfm-gjahr = l_yr1.
*            IF gt_out-before <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSEIF it_ztfi_imfm-gjahr = l_yr2.
*            IF gt_out-last <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSEIF it_ztfi_imfm-gjahr = l_yr3.
*            IF gt_out-year <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSEIF it_ztfi_imfm-gjahr = l_yr4.
*            IF gt_out-year1 <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSEIF it_ztfi_imfm-gjahr = l_yr5.
*            IF gt_out-year2 <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSEIF it_ztfi_imfm-gjahr = l_yr6.
*            IF gt_out-year3 <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSEIF it_ztfi_imfm-gjahr = l_yr7.
*            IF gt_out-after <> it_ztfi_imfm-tot.
*              gt_out-light =  '1'.
*            ENDIF.
*          ELSE.
*
*          ENDIF.
*        ENDAT.
*      ENDLOOP.
*    ENDIF.
    modify gt_out.
  endloop.
endform.                    " check_status
*&---------------------------------------------------------------------*
*&      Form  fill_gt_out
*&---------------------------------------------------------------------*
form fill_gt_out.

  clear : gt_out,f_flg.

  loop at it_out.
*----AR Name
    read table it_imakt with key posnr = it_out-posid.
    if sy-subrc = 0.
      move it_imakt-txt50 to gt_out-txt50.
    endif.
*---2003.11.04 modify jhs  status
*---GET Status
    clear : wa_stat, wa_chk.


    perform get_status using it_out-posid
                       changing wa_stat
                                wa_chk.
    move wa_stat     to gt_out-s_status.
    if wa_chk = 'X'.
      continue.
    endif.
    perform get_cbo_status using it_out-posid p_gjahr.
*---2004/04/03
    check gt_out-status in r_status.

    perform get_cbo_status_name using gt_out-status.
**----GET STATUS CBO
*    PERFORM get_status USING it_out-posnr.
*-----GET AR Detail.

    perform call_bapi_ar_detail using it_out-posid.
*--
    read table it_org_units index 1.
    check it_org_units-req_cost_center in s_akostl.
    gt_out-cost_center = wa_master_data-rsp_cost_center.
*----find varaint
    clear : wa_varnt.
    perform get_variant changing wa_varnt p_gjahr.
    gt_out-varnt = wa_varnt.

* fill display layout from AR amount
    perform make_plan_year using wa_varnt.
*
    move-corresponding it_out to gt_out.
    move 1 to gt_out-light.
    append gt_out.
    clear gt_out.
  endloop.
**===============MAKE GT_OUT
*  REFRESH : gt_out.
*  CLEAR   : gt_out.
**--ALL
*  IF p_chk1 = ' ' AND p_chk2 = ' '
*     AND p_chk3 = ' ' AND p_chk4 = ' '.
*    LOOP AT gt_temp.
*      MOVE-CORRESPONDING gt_temp TO gt_out.
*      APPEND gt_out.
*      CLEAR  gt_out.
*    ENDLOOP.
*  ENDIF.
**---Not planned
*  IF p_chk1 = 'X'.
*    LOOP AT gt_temp.
*      CHECK gt_temp-status EQ ' '.
*      MOVE-CORRESPONDING gt_temp TO gt_out.
*      MOVE 1 TO gt_out-light.
*      APPEND gt_out.
*      CLEAR gt_out.
*    ENDLOOP.
*  ENDIF.
**---REQUEST
*  IF p_chk2 = 'X'.
*    LOOP AT gt_temp.
**      CHECK gt_temp-status EQ 'P'.
*      CHECK gt_temp-status EQ 'R'.
*      MOVE-CORRESPONDING gt_temp TO gt_out.
*      MOVE 1 TO gt_out-light.
*      APPEND gt_out.
*      CLEAR gt_out.
*    ENDLOOP.
*  ENDIF.
**---Read for approved
*  IF p_chk3 = 'X'.
*    LOOP AT gt_temp.
**      CHECK gt_temp-status EQ 'F'.
*      CHECK gt_temp-status EQ 'F'.
*      MOVE-CORRESPONDING gt_temp TO gt_out.
*      MOVE 1 TO gt_out-light.
*      APPEND gt_out.
*      CLEAR gt_out.
*    ENDLOOP.
*  ENDIF.
**---Approved
*  IF p_chk4 = 'X'.
*    LOOP AT gt_temp.
**      CHECK gt_temp-status EQ 'A'.
*      CHECK gt_temp-status EQ 'A'.
*      MOVE-CORRESPONDING gt_temp TO gt_out.
*      MOVE 1 TO gt_out-light.
*      APPEND gt_out.
*      CLEAR gt_out.
*    ENDLOOP.
*  ENDIF.
*

endform.                    " fill_gt_out
*&---------------------------------------------------------------------*
*&      Form  initial_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initial_data.
  refresh : it_out, gt_out, it_dis, it_month, it_imakt,
            gt_out1, it_imakpa.

  clear   : it_out, gt_out, it_dis, it_month, it_imakt,
            gt_out1, it_imakpa.

  move 'I' to r_status-sign.
  move 'EQ' to r_status-option.

  if p_chk1 = 'X'.
    move 'P' to r_status-low.
    append r_status.
    move ' ' to r_status-low.
    append r_status.
  endif.
  if p_chk2 = 'X'.
    move 'R' to r_status-low.
    append r_status.
  endif.
  if p_chk3 = 'X'.
    move 'A' to r_status-low.
    append r_status.
  endif.

endform.                    " initial_data
*&---------------------------------------------------------------------*
*&      Form  PI_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
form pi_check using    u_posid
                       f_rc.
  data: l_msg(250) type c.
  clear : f_rc.


* select count(*) into wa_cnt from ztfi_pi
* where type = '1'
* and   code = u_posid+0(3).
* if wa_cnt < 1.
*    wa_pi_chk1 = 'X'.
* endif.
  wa_length = strlen( gt_out-posid ).
  if wa_length <> 11.
    f_rc = 1.
    concatenate l_msg 'Error: Length; ' into l_msg.
  endif.

  read table it_ztfi_pi with key type = '1'
                                 code = u_posid+0(3).
  if sy-subrc <> 0.
    f_rc = 2.
    concatenate l_msg 'Prj Code; ' into l_msg.
  endif.

  read table it_ztfi_pi with key type = '2'
                                 code = u_posid+3(1).
  if sy-subrc <> 0.
    f_rc = 3.
    concatenate l_msg 'Prj Type; ' into l_msg.
  endif.
  read table it_ztfi_pi with key type = '3'
                                 code = u_posid+4(1).
  if sy-subrc <> 0.
    f_rc = 4.
    concatenate l_msg 'Division; ' into l_msg.
  endif.
  read table it_ztfi_pi with key type = '4'
                                 code = u_posid+5(1).
  if sy-subrc <> 0.
    f_rc = 5.
    concatenate l_msg 'Department; ' into l_msg.
  endif.
  read table it_ztfi_pi with key type = '5'
                                 code = u_posid+6(1).
  if sy-subrc <> 0.
    f_rc = 6.
    concatenate l_msg 'Account; ' into l_msg.
  endif.

  if f_rc <> 0.
    message w009(zmfi) with l_msg.
  endif.
endform.                    " PI_CHECK
*&---------------------------------------------------------------------*
*&      Form  get_pi_budget_p
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_WA_LAST  text
*----------------------------------------------------------------------*
form get_pi_budget_p using    u_posid
                              u_last.

  refresh : it_pi_budget_p.
  clear   : it_pi_budget_p.
  call function 'Z_FFI_GET_PI_BUDGET'
    exporting
      posid = u_posid
      prnam = ' '
      gjahr = u_last
    tables
      out   = it_pi_budget_p.
endform.                    " get_pi_budget_p
*&---------------------------------------------------------------------*
*&      Form  bdc_change_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*----------------------------------------------------------------------*
form bdc_change_status using    u_posid.
  perform make_bdc_rtn using :
                     'X'  'SAPLAIA1'        '0100',
                     ' '  'IMAK-POSNR'      u_posid,
                     ' '  'BDC_OKCODE'      '/00'.
  perform make_bdc_rtn using :
                      'X'  'SAPLAIA1'       '0200',
                     ' '  'BDC_OKCODE'      '=STAV'.
  perform make_bdc_rtn using :
                     'X'  'SAPLAIA1'        '0200',
                     ' '  'BDC_OKCODE'      '=BUCH'.
  call transaction tcode   using it_bdc
                           mode   'E'
                           update 'S'
*                    OPTIONS  FROM CTU_PARAMS
                    messages into it_messtab.
  read table it_messtab with key msgtyp = 'S'
                                 msgid = 'AO'
                                 msgnr = '010'.
  if sy-subrc = 0.
    if gt_out-status = 'R'.
      move 'Read for Approve'  to gt_out-status_txt.
      move 'F'                 to gt_out-status.
      move 'I0001'             to gt_out-s_status.
      perform update_status using gt_out-posid 'R' 'F'.
    elseif gt_out-status = 'F'.
      move 'Approved'          to gt_out-status_txt.
      move 'A'                 to gt_out-status.
      move 'I0354'             to gt_out-s_status.
      perform update_status using gt_out-posid 'F' 'A'.
    endif.
  else.
    move 'failure'    to gt_out-status_txt.
  endif.

endform.                    " bdc_change_status
*&---------------------------------------------------------------------*
*&      Form  over_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form over_check.
  clear : wa_ar_sum1, wa_ov_sum1.
  loop at it_dis where ss_year <> '1111'.
*    CHECK it_dis-ss_year >= wa_last.
    wa_ar_sum1 = wa_ar_sum1 + it_dis-ss_ar.
    wa_ov_sum1 = wa_ov_sum1 + it_dis-ss_ov.
  endloop.

  read table it_dis with key ss_year = '1111'.
  if it_dis-ss_ar <> wa_ar_sum1.
    move 'X' to wa_overall_chk.
    if wa_overall_chk = 'X'.
      message w000(zmfi) with 'Overall amount Check'.
    endif.
  endif.
endform.                    " over_check
*&---------------------------------------------------------------------*
*&      Form  previous_chk
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form previous_chk.
  read table it_dis with key ss_year = '1111'.
  if it_dis-ss_pi_p <> wa_monthly_tot.
    move 'X' to wa_overall_p_chk.
  endif.

endform.                    " previous_chk
*&---------------------------------------------------------------------*
*&      Form  chk_inv_pre
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form chk_inv_pre.
  data : f_diff like it_dis-ss_ar.
  clear f_diff.
  if p_gjahr <> gt_out-gjahr.
    read table it_dis with key ss_year = '1111'.
    if it_dis-ss_ar <> it_dis-ss_pi.
      f_diff = it_dis-ss_pi - it_dis-ss_ar.
      if f_diff < -1 or   f_diff  > 1.
        message i000(zmfi) with text-041 '$' f_diff '.'.
        leave to screen 900.
      endif.
    endif.
  endif.
endform.                    " chk_inv_pre
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL_MONTH  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module screen_control_month output.
  if gt_out-status = 'A'.
    loop at screen.
      if screen-group1 = 'G1'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  endif.
  if it_month-ss_year eq  '1111' or it_month-ss_year < wa_last.
    if sy-uname eq '9174547' or sy-uname eq 'WSKIM'.
      loop at screen.
        if screen-group1 = 'G1'.
          screen-input = '1'.
          modify screen.
        endif.
      endloop.
    else.
      loop at screen.
        if screen-group1 = 'G1'.
          screen-input = '0'.
          modify screen.
        endif.
      endloop.
    endif.
  endif.

endmodule.                 " SCREEN_CONTROL_MONTH  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  save_investmentcost
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_investmentcost.
  clear : wa_overall_chk.
  perform over_check.
  perform chk_inv_pre.
  if gt_out-varnt = ' '.    "VARAINT ASSIGN
    message w000(zmfi) with 'Not Assigned Varaint'.
*        PERFORM bapi_assign_variant USING gt_out-posid.
  endif.

  perform ar_plan_update using gt_out-posid gt_out-varnt.

endform.                    " save_investmentcost
*&---------------------------------------------------------------------*
*&      Form  get_planned_amt
*&---------------------------------------------------------------------*
form get_planned_amt using  u_posid.
  data: l_gjahr like ztfi_imfm-ayear.

*FIXME SUM
  select * into table it_ztfi_imfm
      from ztfi_imfm
      where posid = u_posid
        and ayear = p_gjahr
        and gubun = 'P'.        "original
*        and seq   = '0000'.    "plan

*GET PREVIOUS BUDGET
  if sy-subrc <> 0.
    l_gjahr = p_gjahr - 1.

*FIXME SUM
    select * into table it_ztfi_imfm
        from ztfi_imfm
        where posid = u_posid
          and ayear = l_gjahr
          and gubun = 'P'.        "original
*          and seq   = '0000'.    "plan
  endif.
endform.                    " get_planned_amt
*&---------------------------------------------------------------------*
*&      Form  move_cbotable_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3662   text
*----------------------------------------------------------------------*
form move_cbotable_data using    value(p_3662).
  move it_ztfi_imfm-wtp01 to it_month-ss_mm1.
  move it_ztfi_imfm-wtp02 to it_month-ss_mm2.
  move it_ztfi_imfm-wtp03 to it_month-ss_mm3.
  move it_ztfi_imfm-wtp04 to it_month-ss_mm4.
  move it_ztfi_imfm-wtp05 to it_month-ss_mm5.
  move it_ztfi_imfm-wtp06 to it_month-ss_mm6.
  move it_ztfi_imfm-wtp07 to it_month-ss_mm7.
  move it_ztfi_imfm-wtp08 to it_month-ss_mm8.
  move it_ztfi_imfm-wtp09 to it_month-ss_mm9.
  move it_ztfi_imfm-wtp10 to it_month-ss_mm10.
  move it_ztfi_imfm-wtp11 to it_month-ss_mm11.
  move it_ztfi_imfm-wtp12 to it_month-ss_mm12.
  move ' '                to it_month-chk.
  clear it_month-ss_tot.
  it_month-ss_tot = it_ztfi_imfm-wtp01 + it_ztfi_imfm-wtp02 +
                    it_ztfi_imfm-wtp03 + it_ztfi_imfm-wtp04 +
                    it_ztfi_imfm-wtp05 + it_ztfi_imfm-wtp06 +
                    it_ztfi_imfm-wtp07 + it_ztfi_imfm-wtp08 +
                    it_ztfi_imfm-wtp09 + it_ztfi_imfm-wtp10 +
                    it_ztfi_imfm-wtp11 + it_ztfi_imfm-wtp12.

endform.                    " move_cbotable_data
*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form auth_check using f_flg r_ucomm g_status.
  data:ls_taif2 like taif2.
  clear ls_taif2-appst.
*  SELECT SINGLE  appst INTO ls_taif2-appst
*   FROM taif2
*    WHERE gjahr EQ  p_gjahr
**      and PRART eq p_versn
*      AND versi EQ  p_versn.

  case r_ucomm.
    when '&FAPP'.
      authority-check object 'A_IMPR_APS'
               id 'IM_APPST' field  '1'
               id 'IM_ACTVT' field  '34'.
      check sy-subrc <> 0.
      message s000 with 'You have no authorization!!!'.
      f_flg = 'X'.
    when '&APPR'.
      authority-check object 'A_IMPR_APS'
              id 'IM_APPST' field '*'
              id 'IM_ACTVT' field '50'.
      check sy-subrc <> 0.
      message s000 with 'You have no authorization!!!'.
      f_flg = 'X'.
    when '&CANC'.
      if g_status eq 'A'.
        authority-check object 'A_IMPR_APS'
                id 'IM_APPST' field '*'
                id 'IM_ACTVT' field '50'.
        check sy-subrc <> 0.
        message s000 with 'You have no authorization!!!'.
        f_flg = 'X'.
      elseif g_status eq 'F'.
        authority-check object 'A_IMPR_APS'
                id 'IM_APPST' field '*'
                id 'IM_ACTVT' field '50'.
        if sy-subrc <> 0.
          authority-check object 'A_IMPR_APS'
                id 'IM_APPST' field '*'
                id 'IM_ACTVT' field '34'.
          if sy-subrc <> 0.
            message s000 with 'You have no authorization!!!'.
            f_flg = 'X'.
          endif.
        endif.
      else.
        f_flg = 'X'.
      endif.
  endcase.
endform.                    " auth_check
*&---------------------------------------------------------------------*
*&      Form  correction_difference
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form correction_difference using    u_posid u_varnt u_gjahr.
*FIXME what is this???
  exit.

  data : f_gap like it_dis-ss_ar,
         g_gap like it_dis-ss_ar,
         t_sum like it_dis-ss_ar.

  select * into table it_ztfi_imfm
      from ztfi_imfm
      where posid = u_posid
        and ayear = u_gjahr
        and gubun = 'P'.        "original
*        and seq   = '0000'.    "plan
  check sy-subrc <> 0.
  clear : f_gap,g_gap.
  loop at it_dis.
    if it_dis-ss_year <> '1111' and
       it_dis-ss_year < wa_last and it_dis-ss_pi <> it_dis-ss_act.
      f_gap =  it_dis-ss_pi - it_dis-ss_act.
      if f_gap <> 0.
        it_dis-ss_ar =  it_dis-ss_act.
      endif.
      g_gap = g_gap + f_gap.
      modify it_dis.
    endif.
  endloop.
  if g_gap <> space.
    read table it_dis with key ss_year = wa_last.
    it_dis-ss_ar = it_dis-ss_pi  + g_gap.
    modify it_dis transporting ss_ar
          where ss_year eq wa_last.
    clear g_gap.
  endif.
  loop at it_dis where ss_year <> '1111'.
    t_sum = t_sum +  it_dis-ss_ar.
  endloop.
  if t_sum <> space.
    read table it_dis with key ss_year = wa_last.
    it_dis-ss_ar = t_sum.
    modify it_dis transporting ss_ar
          where ss_year eq '1111'.
    clear g_gap.
  endif.

endform.                    " correction_difference
*&---------------------------------------------------------------------*
*&      Form  actual_downpayment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_U_VARNT  text
*      -->P_U_GJAHR  text
*----------------------------------------------------------------------*
form actual_downpayment using    u_posid u_varnt u_gjahr.
  data : begin of gt_out3 occurs 0,
*         aufnr         LIKE  aufk-aufnr,
         othbefore    like  cosp-wtg001,
         before       like  cosp-wtg001,
         last         like  cosp-wtg001,
         year         like  cosp-wtg001,
         year1        like  cosp-wtg001,
         year2        like  cosp-wtg001,
         year3        like  cosp-wtg001,
         year4        like  cosp-wtg001,
         year5        like  cosp-wtg001,
         after        like  cosp-wtg001,
        end of gt_out3.

  data: w_impr  like impr.

  data : gt_out4 like gt_out3 occurs 0 with header line.
  refresh : it_aufk,it_imzo,gt_temp,gt_out2,it_actual.
  clear : it_aufk,it_imzo,gt_temp,gt_out2,it_actual.


  select single * into w_impr from impr
     where  posid = u_posid
       and  gjahr = p_gjahr.

* if NO PI, then just AR status
  check sy-subrc = 0.

  select * into table it_imzo from imzo
     where posnr = w_impr-posnr.

  check sy-subrc = 0.

  select * into table it_aufk from aufk
     for all entries in it_imzo
     where objnr = it_imzo-objnr.

*    WHERE auart EQ 'P'
*      AND autyp EQ '1'.
*    WHERE aufnr IN s_aufnr
*    AND   akstl IN s_akstl
*    AND   autyp IN so_ippos
*    AND   user0  IN s_usr02.
*--GET IMZO
*  clear wa_t_cnt.
*  describe table it_aufk lines wa_t_cnt.
*  if wa_t_cnt > 0.
*    select * into table it_imzo from imzo
*    for all entries in it_aufk
*    where objnr = it_aufk-objnr.
*  endif.
*---GET IMPR
*  clear wa_t_cnt.
*  describe table it_imzo lines wa_t_cnt.
*  if wa_t_cnt > 0.
*    select * into table it_impr from impr
*    for all entries in it_imzo
*    where  posnr = it_imzo-posnr
*      and  posid eq  u_posid
*      and  gjahr = it_imzo-gjahr
*      and  prnam in s_prnam.
*  endif.
*====*



  clear gt_out2.
  loop at it_aufk.
    move-corresponding it_aufk to gt_temp.
    read table it_imzo with key objnr = it_aufk-objnr.
*                                gjahr = wa_last ."u_gjahr.
    if sy-subrc = 0.
      move it_imzo-posnr to gt_temp-posnr.
      move w_impr-posid to gt_temp-posid.
      append gt_temp.
    endif.
    clear  gt_temp.
  endloop.
*---make gt_out by activity
  delete adjacent duplicates from gt_temp.

  loop at gt_temp.
    move gt_temp-ktext to gt_out2-ktext.
    perform get_activity    using
                           gt_temp-aufnr
                           gt_temp-akstl
                           gt_temp-posid
                           gt_temp-user0.
  endloop.

  sort gt_out2 by aufnr key ascending.
  clear wa_chk.

  loop at gt_out2.
*----get i/o budget
    at new aufnr.
*      PERFORM get_io_buget USING gt_out1-aufnr.
      perform get_io_actual using gt_out2-aufnr.
    endat.
*---actual i/o
    if gt_out2-key > 06.
      perform make_io_actual using gt_out2-aufnr gt_out2-key.
      clear wa_chk.
      loop at it_actual_f.
        move-corresponding it_actual_f to it_actual.
        append it_actual.
      endloop.

    endif.
  endloop.

  refresh gt_out3.clear gt_out3.
  loop at gt_out2.
    move-corresponding gt_out2 to gt_out3.
    append gt_out3.
  endloop.

  clear gt_out3.
  loop at gt_out3.
    collect gt_out3 into gt_out4.
  endloop.

  read table gt_out4 index 1.
  if not gt_out4-last is initial.
    loop at it_dis where  ss_year eq wa_last.
      it_dis-ss_act = it_dis-ss_act + gt_out4-last.
      modify it_dis transporting ss_act
       where  ss_year eq wa_last.
    endloop.
  endif.
  if not gt_out4-before is initial.
    loop at it_dis where ss_year eq wa_before.
      it_dis-ss_act = it_dis-ss_act + gt_out4-before.
      modify it_dis transporting ss_act
       where  ss_year eq wa_before.
    endloop.
  endif.
  if not gt_out4-othbefore is initial.
    loop at it_dis where ss_year eq wa_othbefore.
      it_dis-ss_act = it_dis-ss_act + gt_out4-othbefore.
      modify it_dis transporting ss_act
       where  ss_year eq wa_othbefore.
    endloop.
  endif.

*SUM
  read table it_dis with key ss_year = '1111'.
  if it_dis-ss_act = space.
    loop at it_dis where ss_year <> '1111'.
      at last.
        sum.
        modify it_dis transporting ss_act
          where  ss_year eq '1111'.
      endat.
    endloop.
  else.
    loop at it_dis where ss_year eq '1111'.
      clear : it_dis-ss_act.
      modify it_dis transporting ss_act
        where  ss_year eq '1111'.
    endloop.
    loop at it_dis where ss_year <> '1111'.
      at last.
        sum.
        modify it_dis transporting ss_act
          where  ss_year eq '1111'.
      endat.
    endloop.
  endif.
endform.                    " actual_downpayment
*&---------------------------------------------------------------------*
*&      Form  get_activity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_TEMP_AUFNR  text
*      -->P_GT_TEMP_AKSTL  text
*      -->P_GT_TEMP_POSID  text
*      -->P_GT_TEMP_USER0  text
*----------------------------------------------------------------------*
form get_activity using    u_aufnr
                           u_akstl
                           u_posid
                           u_user0.
*  REFRESH : GT_OUT.
*  CLEAR   : GT_OUT.
  move u_user0       to gt_out2-user0.
  move u_aufnr       to gt_out2-aufnr.
  move u_akstl       to gt_out2-akstl.
  move '09'          to gt_out2-key.
  move 'Downpayment' to gt_out2-act.
  move u_posid       to gt_out2-posid.
  move u_user0       to gt_out2-user0.
  append gt_out2.
  clear  gt_out2.
endform.                    " get_activity
*&---------------------------------------------------------------------*
*&      Form  get_io_buget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
form get_io_buget  using    u_aufnr.
  refresh : it_budget.
  clear   : it_budget.
  call function 'Z_FFI_GET_IO_BUDGET'
    exporting
      aufnr = u_aufnr
    tables
      out   = it_budget.
endform.                    " get_io_buget
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
form get_io_actual using    u_aufnr.
  call function 'Z_FFI_GET_IO_ACTUAL'
    exporting
      aufnr         = u_aufnr
* IMPORTING
*   AMT           =
    tables
      out           = it_actual_f
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .

endform.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  make_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT1_AUFNR  text
*      -->P_GT_OUT1_KEY  text
*----------------------------------------------------------------------*
form make_io_actual using    u_aufnr
                            u_key.

  loop at it_actual_f.
    case u_key.
      when '09'.       "Downpayment
        case it_actual_f-wrttp.
          when '12'.
            add it_actual_f-tot     to   gt_out2-tot.
            add it_actual_f-tot     to   wa_d_tot.
            case it_actual_f-gjahr.
              when wa_othbefore.
                move it_actual_f-tot     to   gt_out2-othbefore.
                move it_actual_f-tot     to   wa_d_obefore.
              when wa_before.
                move it_actual_f-tot     to   gt_out2-before.
                move it_actual_f-tot     to   wa_d_before.
              when wa_last.
                move it_actual_f-tot     to   gt_out2-last.
                move it_actual_f-tot     to   wa_d_last.
              when wa_year.
                move it_actual_f-tot     to   gt_out2-year.
                move it_actual_f-tot     to   wa_d_year.
              when wa_year1.
                move it_actual_f-tot     to   gt_out2-year1.
                move it_actual_f-tot     to   wa_d_year1.
              when wa_year2.
                move it_actual_f-tot     to   gt_out2-year2.
                move it_actual_f-tot     to   wa_d_year2.
              when wa_year3.
                move it_actual_f-tot     to   gt_out2-year3.
                move it_actual_f-tot     to   wa_d_year3.
              when wa_year4.
                move it_actual_f-tot     to   gt_out2-year4.
                move it_actual_f-tot     to   wa_d_year4.
              when wa_year5.
                move it_actual_f-tot     to   gt_out2-year5.
                move it_actual_f-tot     to   wa_d_year5.
              when others.
                if it_actual_f-gjahr < wa_last.
                  add it_actual_f-tot    to   gt_out2-othbefore.
                  add it_actual_f-tot    to   wa_d_obefore.
                elseif it_actual_f-gjahr > wa_year5.
                  add it_actual_f-tot    to   gt_out2-after.
                  add it_actual_f-tot    to   wa_d_after.
                endif.
            endcase.
        endcase.
    endcase.
    modify gt_out2.
  endloop.
endform.                    " make_io_actual
*&---------------------------------------------------------------------*
*&      Form  add_monthly_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_monthly_data.
  data:begin of it_act1 occurs 0,
       gjahr like zfi_io_actual-gjahr,
       wtg001 like zfi_io_actual-wtg001,
       wtg002 like zfi_io_actual-wtg001,
       wtg003 like zfi_io_actual-wtg001,
       wtg004 like zfi_io_actual-wtg001,
       wtg005 like zfi_io_actual-wtg001,
       wtg006 like zfi_io_actual-wtg001,
       wtg007 like zfi_io_actual-wtg001,
       wtg008 like zfi_io_actual-wtg001,
       wtg009 like zfi_io_actual-wtg001,
       wtg010 like zfi_io_actual-wtg001,
       wtg011 like zfi_io_actual-wtg001,
       wtg012 like zfi_io_actual-wtg001,
       end of it_act1.
  data : it_act2 like it_act1 occurs 0 with header line.
  refresh : it_act1,it_act2. clear : it_act1,it_act2.
  loop at it_actual where wrttp eq '12'.
    move-corresponding it_actual to it_act1.
    append it_act1.
  endloop.
  loop at it_act1.
    collect it_act1 into it_act2.
  endloop.

  clear it_month.
  loop at it_month.
    loop at it_act2 "WHERE wrttp EQ '12'
                      where gjahr eq  it_month-ss_year.
      it_month-ss_mm1  =  it_month-ss_mm1  + it_act2-wtg001.
      it_month-ss_mm2  =  it_month-ss_mm2  + it_act2-wtg002.
      it_month-ss_mm3  =  it_month-ss_mm3  + it_act2-wtg003.
      it_month-ss_mm4  =  it_month-ss_mm4  + it_act2-wtg004.
      it_month-ss_mm5  =  it_month-ss_mm5  + it_act2-wtg005.
      it_month-ss_mm6  =  it_month-ss_mm6  + it_act2-wtg006.
      it_month-ss_mm7  =  it_month-ss_mm7  + it_act2-wtg007.
      it_month-ss_mm8  =  it_month-ss_mm8  + it_act2-wtg008.
      it_month-ss_mm9  =  it_month-ss_mm9  + it_act2-wtg009.
      it_month-ss_mm10 =  it_month-ss_mm10 + it_act2-wtg010.
      it_month-ss_mm11 =  it_month-ss_mm11 + it_act2-wtg011.
      it_month-ss_mm12 =  it_month-ss_mm12 + it_act2-wtg012.
      it_month-ss_tot = it_month-ss_mm1 + it_month-ss_mm2 +
             it_month-ss_mm3 + it_month-ss_mm4 + it_month-ss_mm5 +
             it_month-ss_mm6 + it_month-ss_mm7 + it_month-ss_mm8 +
             it_month-ss_mm9 + it_month-ss_mm10 + it_month-ss_mm11 +
             it_month-ss_mm12 .
      modify it_month transporting  ss_mm1 ss_mm2 ss_mm3 ss_mm4 ss_mm5
           ss_mm6 ss_mm7 ss_mm8 ss_mm9 ss_mm10 ss_mm11 ss_mm12 ss_tot
              where ss_year = it_month-ss_year.
    endloop.
  endloop.
endform.                    " add_monthly_data
*&---------------------------------------------------------------------*
*&      Form  select_ar_masters
*&---------------------------------------------------------------------*
form select_ar_masters.
  data: begin of i_posnr occurs 0,
          posnr like imak-posnr,
        end   of i_posnr.

*  if p_chk1 = space.
*    select  posid into table i_posnr
*       from ztfi_imfm
*       where posid in s_posnr
*         and ayear = p_gjahr
*         and gubun = 'P'      "Original
**         and seq   = '0000'
*         and status in r_status.
*    if sy-subrc = 0.
*      select * into table it_out from imak
*          for all entries in i_posnr
*            where posnr  =  i_posnr-posnr
*              and ivart  in s_ivart
*              and vkostl in s_vkostl
*              and gjahr  in s_appr.
*    endif.
*  else.
    select * into table it_out from imak
        where posnr  in s_posnr  and
              ivart  in s_ivart  and
              vkostl in s_vkostl and
              gjahr  in s_appr   and
              STRATFLG in s_abp.
*           gjahr  EQ p_appr.
*  endif.

  tables: jest.
  loop at it_out.
    select single * from jest
       where objnr = it_out-objnr
         and stat  = 'I0065'       "LOCK
         and inact = space.
    if sy-subrc = 0.
      delete it_out index sy-tabix.
    endif.
  endloop.

endform.                    " select_ar_masters
