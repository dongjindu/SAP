
************************************************************************
**                                                                    **
**            C O M P A N Y   C O N F I D E N T I A L                 **
**                                                                    **
**      This program is the property of  HMMA LLC                     **
**      Care should be taken to prevent its unauthorized use.         **
**                                                                    **
************************************************************************
*&---------------------------------------------------------------------*
*& Program: ZAFI_WARRANTY_SALES_UPLOAD                                 *
*& Type   : Interface                                                  *
*& Author : Manju                                                      *
*& Title  : Warranty BW Sales Upload  Report                           *
*&---------------------------------------------------------------------*
* Help Desk Request No  :   67BG5A6449                                 *
* System Id:                                                           *
*                                                                      *
*   Requested by:        Andy Choi
*   Assigned to:         Sudhakar                                      *
*   Original Request #:                                                *
*   ABAP Analyst:    Manjunath Venkatesh                               *
*                                                                      *
* Business Users:                                                      *
*                                                                      *
* Business Requirement Description:                                    *
*                                                                      *
*                                                                      *
* Processing Logic:                                                    *
*     < Outline the flow of the main processing logic >                *
*                                                                      *
* Configuration Requirements:                                          *
*     < Document any special config requirements that must exist for   *
*       this program to work correctly >                               *
*                                                                      *
* Program Inputs:                                                      *
*     < Input File Path & Name >                                       *
*     < Any variants program would be typically run with >             *
*                                                                      *
* Program Outputs:                                                     *
*       Online Report                                                  *
*                                                                      *
* Authorization Checks:                                                *
*     < Document if any authorization objects are checked >            *
*                                                                      *
* Direct Update Database Tables:                                       *
*   < No direct updates to SAP tables are allowed.List custom tables > *
*                                                                      *
* Outstanding Issues:                                                  *
*     < If the program is being delivered with any known open issues   *
*       document them here; they could be planned for next release >   *
*                                                                      *
* Instructions on how to test this program:                            *
*     < If this program needs any special inputs like an inbound file  *
*       from an EDI subsystem for testing, document it here >          *
*                                                                      *
* Instructions on how to re-start this program:                        *
*                                                                      *
* Volume Estimates:                                                    *
*                                                                      *
* Frequency of Execution:                                              *
*   o On demand                                                        *
*                                                                      *
* Execution Mode:                                                      *
*   o Online      - Transaction Code -                                 *
*                                                                      *
* Other Comments:                                                      *
*                                                                      *
*&----------------------------------------------------------------------
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 07/13/06    Manjunath    UD1K921321   Initial Coding
************************************************************************
report zafi_warranty_sales_upload  line-count 65 line-size 132
                             no standard page heading message-id db .


*-------------------------------------------------------------*
* Tables
*-------------------------------------------------------------*
tables : ztfi_war_sales.


*-------------------------------------------------------------*
* Data Declarations
*-------------------------------------------------------------*
include: <icon>.
include : <list>.
type-pools: slis.

*********ALV DEFINITION****************
data: gt_fieldcat type slis_t_fieldcat_alv with header line,
      gt_fc       type slis_t_fieldcat_alv,
      g_fieldcat_s like line of gt_fieldcat,
      gs_layout   type slis_layout_alv,
      gs_print    type slis_print_alv,
      gt_sort     type slis_t_sortinfo_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_header   type slis_t_listheader,
      g_repid     like sy-repid,
      gt_colinfo_table type slis_t_specialcol_alv. "line color.


*-------------------------------------------------------------*
* Internal table
*-------------------------------------------------------------*
data: begin of it_tab occurs 0,
        kokrs(4)  ,   " Company Code
        s_year(4),    " Sales year
        smonth(2),    " Sales Month
        country(2)  , " Country
        model(2) ,    " Model
        m_year(4)  ,  " Model Year
        matnr(18),    " Material
        sales_qty type p decimals 3, " Sales Qty
      end of it_tab.
data : it_tab1 like table of it_tab with header line.

data : begin of it_likp occurs 0,
        vkorg like likp-vkorg,
        kunag like likp-kunag,
        vbeln like likp-vbeln,
        matnr like lips-matnr,
        lfimg like lips-lfimg,
        wadat_ist like likp-wadat_ist,
       end of it_likp.


data : begin of it_kna1 occurs 0,
        kunnr like kna1-kunnr,
        land1 like kna1-land1,
       end of it_kna1.


data : begin of it_tvko occurs 0,
        vkorg like tvko-vkorg,
        bukrs like tvko-bukrs,
       end of it_tvko.


data : g_exit_caused_by_caller  type c,
       gs_exit_caused_by_user   type slis_exit_by_user,
       flag type c,
       r_cnt(5) type n.
*-------------------------------------------------------------*
* Selection Screen
*--------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-001.
parameters: p_bukrs like ztfi_war_sales-bukrs default 'H201',
            p_gjahr like ztfi_war_sales-s_gjahr
                          memory id bdtj obligatory,
            p_monat like ztfi_war_sales-monat
                          memory id vpe modif id per obligatory.

*  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY,
*  p_filety LIKE rlgrap-filetype DEFAULT 'DAT' NO-DISPLAY.
selection-screen end   of block b1.

*-------------------------------------------------------------*
* Start-of-selection
*--------------------------------------------------------------*
start-of-selection.
* Read data from file to Internal table for further processing
* Perform upload_data.
  perform select_data.
  perform creat_itab.

* Display BW Sales Data read from input file
  perform display_data.
*  perform confirm_changes.
end-of-selection.


*at selection-screen on value-request for p_file.
*  perform at_sel_screen_on_value_request using p_file 'O'.
*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form upload_data.
*  call function 'WS_UPLOAD'
*       exporting
*         filename  = p_file
*         filetype  = 'DAT'
*       tables
*         data_tab  = it_tab
*       exceptions
*        conversion_error = 1
*        file_open_error  = 2
*        file_read_error  = 3
*        invalid_type     = 4
*        no_batch         = 5
**      BAD_DATA_FORMAT  = 6
*        others           = 9.
*  if sy-subrc ne 0.
*    if sy-subrc = 1.
*      write :/  'FILE UPLOAD CONVERSION ERROR'.
*    elseif sy-subrc = 2.
*      write :/  'UPLOAD FILE OPEN ERROR'.
*    elseif sy-subrc = 3.
*      write :/  'UPLOAD FILE READ ERROR'.
*    elseif sy-subrc = 4.
*      write :/ 'INVALID DATA TYPE!'.
*    endif.
*  endif.
*
** Sum the Qty if Duplicate records exists for the same Qty
*  loop at it_tab.
*    collect it_tab into it_tab1.
*  endloop.

endform.                    " upload_data
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.
  perform get_events.
  perform build_fieldcatalog.
  perform build_layout  using 'X' 'X' space.
  perform build_comment     using  gt_header[].
  perform display_grid.
endform.                    " display_data
*&---------------------------------------------------------------------*
*&      Form  get_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_events.

  constants : c_pss type slis_formname value 'PF_STATUS_SET',
                c_uc  type slis_formname value 'USER_COMMAND',
                c_top type slis_formname value 'TOP_OF_PAGE'.
  refresh gt_events.

  call function 'REUSE_ALV_EVENTS_GET'
       exporting
            i_list_type = 0
       importing
            et_events   = gt_events.

  perform modify_gt_events
          tables  gt_events
          using :
            slis_ev_pf_status_set c_pss,
            slis_ev_user_command  c_uc.

endform.                    " get_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_PF_STATUS_SET  text
*      -->P_C_PSS  text
*----------------------------------------------------------------------*
form modify_gt_events tables p_events_t like gt_events
                      using   p_form p_value.

  data: ls_event type slis_alv_event.

  read table  p_events_t  with key  name = p_form
                          into ls_event.
  if sy-subrc eq 0.
    move     p_value     to   ls_event-form.
    modify   p_events_t  from ls_event index sy-tabix.
  endif.


endform.                    " modify_gt_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_fieldcatalog.
  g_repid = sy-repid.
  perform build_fieldcat  using  'IT_TAB'.

endform.                    " build_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0297   text
*----------------------------------------------------------------------*
form build_fieldcat   using p_intab type slis_tabname.

  data: l_pre_day         type d ,
          l_pre(10) ,
          l_date(10).
  data: i type i.
  data: gs_fieldcat like line of gt_fieldcat.
  clear   : gt_fieldcat, gt_fc.
  refresh : gt_fieldcat, gt_fc.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name         = g_repid
            i_internal_tabname     = p_intab
            i_inclname             = g_repid
       changing
            ct_fieldcat            = gt_fc
       exceptions
            inconsistent_interface = 1
            program_error          = 2
            others                 = 3.

  gt_fieldcat[] = gt_fc[].

  loop at gt_fieldcat.

    case gt_fieldcat-fieldname.
      when 'KOKRS'.
        gt_fieldcat-seltext_l = 'Company Code'.
      when 'S_YEAR'.
        gt_fieldcat-seltext_l = 'Sales Year'.
      when 'SMONTH'.
        gt_fieldcat-seltext_l = 'Sales Month'.
      when 'COUNTRY'.
        gt_fieldcat-seltext_l = 'Country'.
      when 'MODEL'.
        gt_fieldcat-seltext_l = 'Model'.
      when 'M_YEAR'.
        gt_fieldcat-seltext_l = 'Model Year'.
      when 'MATNR'.
        gt_fieldcat-seltext_l = 'FSC'.
      when 'SALES_QTY'.
        gt_fieldcat-seltext_l = 'Sales Qty'.
    endcase.
    modify gt_fieldcat transporting seltext_l.

  endloop.


endform.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0204   text
*      -->P_0205   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form build_layout using p_cb p_color p_sum.
  clear gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.


endform.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
form build_comment using    p_gt_header type slis_t_listheader.
  data: ls_line  type slis_listheader,
        ls_color type slis_specialcol_alv,
        l_date(50).
  data: l_text(70) type c.
  data: i_lines(5).
  data: i_count(5).

  clear ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = 'BW Warranty Sales Data'.
  append ls_line to p_gt_header.

endform.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  display_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_grid.
  perform  grid_viewer1 tables  it_tab1.
endform.                    " display_grid
*&---------------------------------------------------------------------*
*&      Form  grid_viewer1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB  text
*----------------------------------------------------------------------*
form grid_viewer1 tables   p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_bypassing_buffer       = 'X'
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_user_command  = 'USER_COMMAND'
            it_fieldcat              = gt_fieldcat[]
            i_save                   = 'A'
            it_events                = gt_events[]
            is_print                 = gs_print
       importing
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       tables
            t_outtab                 = p_intab.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


endform.                    " grid_viewer1


*-----------------PF-STATUS-----------------
form pf_status_set using rt_extab type slis_t_extab.

  set pf-status 'MYSTATUS' excluding rt_extab.

endform.

*-----------------------------------------------------------*
*   USER_COMMAND
*-----------------------------------------------------------*
form user_command using r_ucomm like sy-ucomm
                        rs_selfield type slis_selfield.

  if r_ucomm = '&SAVE'.
* Upload Legacy records to Database
    perform update_database.
* Confirm changes
    perform confirm_changes.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  update_database
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_database.
  data:l_cnt type i.

  delete from ztfi_war_sales where s_gjahr = p_gjahr
                               and monat   = p_monat  .
  commit work.
  clear :flag,l_cnt,r_cnt.
  loop at it_tab1.
    add 1 to l_cnt.
    add 1 to r_cnt.
    ztfi_war_sales-bukrs     = it_tab1-kokrs.
    ztfi_war_sales-s_gjahr   = it_tab1-s_year.
    ztfi_war_sales-monat     = it_tab1-smonth.
    ztfi_war_sales-land1     = it_tab1-country.
    ztfi_war_sales-model     = it_tab1-model.
    ztfi_war_sales-m_gjahr   = it_tab1-m_year.
    ztfi_war_sales-matnr     = it_tab1-matnr.
    ztfi_war_sales-sal_qty   = it_tab1-sales_qty.
    ztfi_war_sales-uname     = sy-uname.
    ztfi_war_sales-cdate     = sy-datum.
    insert ztfi_war_sales .
    if l_cnt = 1000.
      commit work. clear l_cnt.
    endif.
  endloop.
  commit work.
  if sy-subrc eq 0.
    flag = 'X'.
  endif.
endform.                    " update_database
*&---------------------------------------------------------------------*
*&      Form  at_sel_screen_on_value_request
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0143   text
*----------------------------------------------------------------------*
form at_sel_screen_on_value_request using def_path like rlgrap-filename
                                              mode     type c.
*  data: tmp_filename like rlgrap-filename.
*  data: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
*  data: fieldln type i.
*  field-symbols: <tmp_sym>.
*
*
*  fieldln = strlen( def_path ) - 1.
*  assign def_path+fieldln(1) to <tmp_sym>.
*  if <tmp_sym> = '/' or <tmp_sym> = '\'.
*    clear <tmp_sym>.
*  endif.
*
*  call function 'F4_FILENAME'
*       exporting
*            program_name  = sy-cprog
*            dynpro_number = sy-dynnr
*            field_name    = ' '
*       importing
*            file_name     = tmp_filename.
*
*
*  if sy-subrc = 0.
*    p_file = tmp_filename.
*  else.
*    message e000 with 'FILE SELECT WINDOW OPEN ERROR!'.
*  endif.
*


endform.                    " at_sel_screen_on_value_request
*&---------------------------------------------------------------------*
*&      Form  confirm_changes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form confirm_changes.
  data : txt1(80) type c.
  if flag = 'X'.
    concatenate r_cnt '' 'Records uploaded into Database Sucessfully'
into txt1.
    call function 'POPUP_TO_INFORM'
      exporting
        titel         =  ' Confirmation'
        txt1          =  txt1
        txt2          =  'You may exit the program'
*     TXT3          = ' '
*     TXT4          = ' '
              .

    .

  endif.
endform.                    " confirm_changes
*&---------------------------------------------------------------------*
*&      Form  select_Data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data.

  data : l_fdate type datum,
         l_tdate type datum.

  perform get_last_date changing l_fdate l_tdate.

* LIKP & LIPS
  select a~vbeln a~kunag a~vkorg b~matnr b~lfimg a~wadat_ist
   into corresponding fields of table it_likp
   from likp as a inner join lips as b
    on a~vbeln = b~vbeln
    where a~wadat_ist between l_fdate and l_tdate
      and a~lfart = 'ZVLF'   "Vehicle Delivery
      and a~fkarv <> space.  "Billing

  if not it_likp[] is initial.

* For seleting Country
    select kunnr land1
     into corresponding fields of table it_kna1
     from kna1
      for all entries in it_likp
      where kunnr = it_likp-kunag .


* For seleting Company Code
    select vkorg bukrs
     into corresponding fields of table it_tvko
     from tvko
     where vkorg <> ''.
  endif.

endform.                    " select_Data
*&---------------------------------------------------------------------*
*&      Form  get_last_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_last_date changing p_fdate p_tdate .

  data : l_temp_date(8) type n,
         l_yearmn(6) type n,
         l_lastday(2) type n.

  concatenate p_gjahr p_monat into l_yearmn.
  concatenate l_yearmn '01' into l_temp_date.
  p_fdate = l_temp_date.

  clear : l_temp_date.
  call function 'RE_LAST_DAY_OF_MONTH'
       exporting
            i_datum = p_fdate
       importing
            e_tt    = l_lastday.

  concatenate l_yearmn l_lastday into l_temp_date.
  p_tdate = l_temp_date.

endform.                    " get_last_date
*&---------------------------------------------------------------------*
*&      Form  creat_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form creat_itab.
  data : l_mapar like itob-mapar,
         l_objek like ausp-objek,
         l_atwrt like ausp-atwrt.
  loop at it_likp.
    clear it_tvko.
    read table it_tvko with key vkorg = it_likp-vkorg.
    clear it_kna1.
    read table it_kna1 with key kunnr = it_likp-kunag.

*- by ig.moon 4/10/2008 {
*   Material Year
*    select single mapar into l_mapar from itob
*      where equnr = it_likp-vbeln
*        and lvorm = '' .
* }

    check sy-subrc = 0 .
    l_objek = it_likp-vbeln .

*   Exclude Usage Car
    clear l_atwrt.
    select single au~atwrt into l_atwrt
        from ausp as au
             inner join cabn as ca on au~atinn = ca~atinn
        where au~mafid eq 'O'
          and au~klart = '002'    "class type
          and au~objek = l_objek
          and ca~atnam = 'P_USAGE_CAR'. "'P_DEALER'. "'USAGE_CAR'.

    check l_atwrt <> 'T' and l_atwrt <> 'S' and l_atwrt <> 'D' .

    clear l_atwrt.
    select single au~atwrt into l_atwrt
        from ausp as au
             inner join cabn as ca on au~atinn = ca~atinn
        where au~mafid eq 'O'
          and au~klart = '002'    "class type
          and au~objek = l_objek
          and ca~atnam = 'P_DESTINATION_CODE'.

    check l_atwrt+3(2) <> 'XX' and l_atwrt+3(2) <> 'XY'.

    it_tab1-kokrs      = it_tvko-bukrs.
    it_tab1-s_year     = it_likp-wadat_ist(4).
    it_tab1-smonth     = it_likp-wadat_ist+4(2).

    it_tab1-country    = it_kna1-land1.
    it_tab1-model      = it_likp-vbeln(2).

*+ by ig.moon 4/10/2008 {
*   Material Year
    select single au~atwrt into l_mapar
        from ausp as au
             inner join cabn as ca on au~atinn = ca~atinn
        where au~mafid eq 'O'
          and au~klart = '002'    "class type
          and au~objek = l_objek
          and ca~atnam = 'P_VIN'.
* }

    it_tab1-m_year     = l_mapar+9(1).

    it_tab1-matnr      = it_likp-matnr.
    it_tab1-sales_qty  = it_likp-lfimg.

    collect it_tab1. clear it_tab1.

  endloop.
endform.                    " creat_itab
