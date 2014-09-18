************************************************************************
* Program Name      : ZRPP203R_DAILY_ACTUAL_RESULTS
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.12.31.
* Specifications By : B. Choi
* Pattern           : 1.1
* Development Request No : UD1K902277
* Addl Documentation:
* Description       : Daily Actual Results Per Code(ALC).
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************

report  zrpp203r_daily_actual_results no standard page heading .

tables: ausp,
        cabn,
        ztpp_wosum,
        ztpp_wosum2.

include <icon>.
include <list>.

*****// ALV //**********************************************************
type-pools: slis.
data: gt_fieldcat type slis_t_fieldcat_alv,
      gt_fc       type slis_t_fieldcat_alv,
      g_fieldcat_s like line of gt_fieldcat,
      gs_layout   type slis_layout_alv,
      gs_print    type slis_print_alv,
      gt_sort     type slis_t_sortinfo_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_header   type slis_t_listheader,
*      gt_header1  TYPE slis_t_listheader,
      gt_colinfo_table type slis_t_specialcol_alv. "line color.

* hierarchy(simple)
data : g_tabname_header       type slis_tabname,       "header part
       g_tabname_item         type slis_tabname,       "detail list
       gs_keyinfo             type slis_keyinfo_alv.   "relation key

* return
data : g_exit_caused_by_caller  type c,
       gs_exit_caused_by_user   type slis_exit_by_user.

data: col_pos type i,
      cnt     type i,
      g_save  type c,
      g_repid like sy-repid,
      g_variant like disvariant.
data: gt_extab type slis_t_extab with header line.

data: begin of it_data occurs 0,
        date        type d,
        year(02)    type c,
        code(05)    type c,
        option(10)  type c,
        part(20)    type c,
        model(03)   type c,
        trim_in(05) type p,
        pbs_qty(05) type p,
        rej_qty(05) type p,
        wbs_qty(05) type p,
        t01_qty(05) type p,
        t02_qty(05) type p,
        t03_qty(05) type p,
        t04_qty(05) type p,
        t05_qty(05) type p,
        t06_qty(05) type p,
        t07_qty(05) type p,
        t08_qty(05) type p,
        t09_qty(05) type p,
        t10_qty(05) type p,
        tot_qty(05) type p,
      end of it_data .

selection-screen  begin of block blk1 with frame title text-001.
parameters: p_date like sy-datum obligatory ,
            p_model(03) type c,
            p_year(02) type c,
            p_code(04) type c.
selection-screen  end of block blk1.

selection-screen begin of block blk2 with frame title text004.
selection-screen begin of line.
selection-screen comment 2(10) text-002 for field p_uni.
parameters: p_uni radiobutton group rad1 .
selection-screen comment 18(10) text-003 for field p_col.
parameters: p_col radiobutton group rad1 .
selection-screen end   of line.
parameters: p_pnum(03) type n .
selection-screen end of block blk2.

data: wa_rp(02) type n.
*       Parameter For Working Time Zone.
ranges: r_time for sy-uzeit,
*       Constants For Reporting Points of V/M
        r_trim_rp for wa_rp,
        r_pbs_rp for wa_rp,
        r_rej_rp for wa_rp,
        r_wbs_rp for wa_rp.

*************************************************
initialization.
*************************************************
  g_repid     = sy-repid.
  p_date = sy-datum.
  p_pnum = 1.
  clear: r_trim_rp, r_trim_rp[].
**Setting V/M's RPs For Trim Input
  r_trim_rp-low = '07'. append r_trim_rp.
  r_trim_rp-low = '09'. append r_trim_rp.

  clear: r_pbs_rp, r_pbs_rp[].
**Setting V/M's RPs For PBS
  r_pbs_rp-low = '05'. append r_pbs_rp.
  r_pbs_rp-low = '06'. append r_pbs_rp.

*******************************************
* It needs to be discussed more ...
*******************************************
  clear: r_rej_rp, r_rej_rp[].
**Setting V/M's RPs For REJ

  clear: r_wbs_rp, r_wbs_rp[].
**Setting V/M's RPs For WBS

  clear: r_time, r_time[].
**Setting Working Time**
  r_time-low = '040001'. r_time-high = '060000'.  "The First
  append r_time.
  r_time-low = '060001'. r_time-high = '080000'.  "The Second
  append r_time.
  r_time-low = '080001'. r_time-high = '100000'.  "The Third
  append r_time.
  r_time-low = '100001'. r_time-high = '120000'.
  append r_time.
  r_time-low = '120001'. r_time-high = '140000'.
  append r_time.
  r_time-low = '140001'. r_time-high = '160000'.
  append r_time.
  r_time-low = '160001'. r_time-high = '180000'.
  append r_time.
  r_time-low = '180001'. r_time-high = '200000'.
  append r_time.
  r_time-low = '200001'. r_time-high = '220000'.
  append r_time.
  r_time-low = '220001'. r_time-high = '240000'.
  append r_time.


*************************************************
start-of-selection.
*************************************************
  perform initial_data.
  perform delete_data_per_condition.

*************************************************
end-of-selection.
*************************************************
  perform  build_events.
  perform  build_fieldcat    using  'IT_DATA'.
  perform  build_layout      using  'X'   space   space.
  perform  build_comment     using  gt_header[].
* ALV FUNCTION CALL
  perform  start_grid_viewer tables  it_data.

*************************************************
top-of-page.
*************************************************
  perform top_of_page.


*&---------------------------------------------------------------------*
*&      Form  INITIAL_DATA
*&---------------------------------------------------------------------*
*       Searching Initial Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form initial_data.
  ranges: lr_atnam for cabn-atnam .
  data: l_num(02) type n,
        l_atwrt type ausp-atwrt,
        begin of l_it_char occurs 0,
          objek type ausp-objek,
          atnam type cabn-atnam,
          atwrt type ausp-atwrt,
        end of l_it_char,
        l_it_data like table of it_data with header line,
        l_wo_no type ausp-objek ,
        l_int03 type i,
        l_cha03(03) type c.

  clear: l_num, l_atwrt.
  lr_atnam-option = 'EQ'.
  lr_atnam-sign   = 'I'.
  do 28 times.
    l_num = l_num + 1.
    concatenate 'P_RP'
                l_num
                '_ACTUAL_DATE'
    into lr_atnam-low.
    append lr_atnam.
  enddo.

  concatenate p_date
              '%'
    into l_atwrt .

  select au~objek ca~atnam au~atwrt
    into corresponding fields of table l_it_char
    from ( ausp as au
          inner join cabn as ca on au~atinn = ca~atinn )
    where ca~atnam in lr_atnam and
          au~klart = '002'     and
          au~atwrt like l_atwrt .
*
  clear: l_it_data, l_it_data[],
         it_data,   it_data[].
* Vehicle No., RPs, RPs' Actual Date&Time
  sort l_it_char by objek atnam atwrt.
*
  loop at l_it_char.
    at new objek .
      clear l_it_data.
      if p_uni = 'X'.  "Unique Part
        clear l_it_data .
        clear l_wo_no .
        perform search_wo using 'U'
                                 l_it_char-objek
                          changing l_wo_no .

      else.            "Color Part
        perform search_wo using 'C'
                                 l_it_char-objek
                          changing l_wo_no .
      endif.
*     date        TYPE d,
      move l_it_char-atwrt+00(08) to l_it_data-date.
*     model(03)   TYPE c,
      perform reading_wo_inf using l_wo_no
                                   'P_MODEL'
                             changing l_it_data-model.
*     year(02)    TYPE c,
      perform reading_wo_inf using l_wo_no
                                   'P_MODEL_YEAR'
                             changing l_it_data-year.
    endat.
*   Clear All Quantity Parameters.
    clear: l_it_data-trim_in, l_it_data-pbs_qty,
           l_it_data-wbs_qty, l_it_data-rej_qty,
           l_it_data-t01_qty, l_it_data-t02_qty,
           l_it_data-t03_qty, l_it_data-t04_qty,
           l_it_data-t05_qty, l_it_data-t06_qty,
           l_it_data-t07_qty, l_it_data-t08_qty,
           l_it_data-t09_qty, l_it_data-t10_qty,
           l_it_data-tot_qty.

*   Inputting Common Data.
*   trim_in(05) TYPE p,
    read table r_trim_rp with key low = l_it_char-atnam+04(02).
    if sy-subrc = 0.
      l_it_data-trim_in = 1.
    endif.
*   pbs_qty(05) TYPE p,
    read table r_pbs_rp with key low = l_it_char-atnam+04(02).
    if sy-subrc = 0.
      l_it_data-pbs_qty = 1.
    endif.
*   wbs_qty(05) TYPE p,
    read table r_wbs_rp with key low = l_it_char-atnam+04(02).
    if sy-subrc = 0.
      l_it_data-wbs_qty = 1.
    endif.
*   rej_qty(05) TYPE p,
    read table r_rej_rp with key low = l_it_char-atnam+04(02).
    if sy-subrc = 0.
      l_it_data-rej_qty = 1.
    endif.

    loop at r_time.
      if r_time-low  <= l_it_char-atwrt+08(06) and
         r_time-high >= l_it_char-atwrt+08(06) .
        case sy-tabix.
          when 1.
            l_it_data-t01_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 2.
            l_it_data-t02_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 3.
            l_it_data-t03_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 4.
            l_it_data-t04_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 5.
            l_it_data-t05_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 6.
            l_it_data-t06_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 7.
            l_it_data-t07_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 8.
            l_it_data-t08_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 9.
            l_it_data-t09_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
          when 10.
            l_it_data-t10_qty = 1.
            l_it_data-tot_qty = 1.
            exit.
        endcase.
      endif.
    endloop.

    if p_pnum = space.  "If there isn't a value in the parameter.
      clear l_int03.
      if p_uni = 'X'.  "UNIQUE PART
        do 200 times.  "For Part Name and Code Name
          l_int03 = l_int03 + 1.
          move l_int03 to l_cha03.
          condense l_cha03.
*         part(20)    TYPE c,
          concatenate 'P_ALC_U_'
                      l_cha03
            into l_it_data-part .
*         code(05)    TYPE c,
          perform search_code using    l_wo_no
                                       l_it_data-part
                              changing l_it_data-code .
          if l_it_data-code <> space.
****************************************************
*           option(10)  TYPE c,
****************************************************
            append l_it_data.
          endif.
        enddo.  "For Part Name and Code Name
      else.            "COLOR PART
        do 50 times.  "For Part Name and Code Name
          l_int03 = l_int03 + 1.
          move l_int03 to l_cha03.
          condense l_cha03.
*         part(20)    TYPE c,
          concatenate 'P_ALC_C_'
                      l_cha03
            into l_it_data-part .
*         code(05)    TYPE c,
          perform search_code using    l_wo_no
                                       l_it_data-part
                              changing l_it_data-code .
          if l_it_data-code <> space.
****************************************************
*           option(10)  TYPE c,
****************************************************
            append l_it_data.
          endif.
        enddo.  "For Part Name and Code Name
      endif.
    else.  "If there is a value in the parameter.
      move p_pnum to l_int03.
      move l_int03 to l_cha03.
      condense l_cha03.
      if p_uni = 'X'.  "UNIQUE PART
*       part(20)    TYPE c,
        concatenate 'P_ALC_U_'
                    l_cha03
          into l_it_data-part .
      else.            "COLOR PART
*       part(20)    TYPE c,
        concatenate 'P_ALC_C_'
                    l_cha03
          into l_it_data-part .
      endif.
*     code(05)    TYPE c,
      perform search_code using    l_wo_no
                                   l_it_data-part
                          changing l_it_data-code .
****************************************************
*     option(10)  TYPE c,
****************************************************
      append l_it_data.
    endif.
  endloop.

  sort l_it_data by date year code option part model .
  loop at l_it_data.
    clear it_data.
    move-corresponding l_it_data to it_data.
    collect it_data.
  endloop.

endform.                    " INITIAL_DATA
*&---------------------------------------------------------------------*
*&      Form  setting_per_code
*&---------------------------------------------------------------------*
*       Searching Work Order Number
*----------------------------------------------------------------------*
*      -->P_0430   text
*      -->P_L_IT_CHAR_OBJEK  text
*      -->P_L_IT_DATA  text
*----------------------------------------------------------------------*
form search_wo using    p_part
                        p_vm_no
               changing p_wo_no.
  data: l_wo type ausp-objek,
        lc_wo_header type cabn-atnam value 'P_WORK_ORDER',
        lc_wo_extc   type cabn-atnam value 'P_EXT_COLOR',
        lc_wo_intc   type cabn-atnam value 'P_INT_COLOR'.
* Reading Work Order Header
  select single atwrt
    into p_wo_no
    from ausp as au
         inner join cabn as ca on au~atinn = ca~atinn
    where au~objek = p_vm_no and
          au~klart = '002' and
          ca~atnam = lc_wo_header .

  if p_part = 'C'.
*   Reading Work Order External Color
    select single atwrt
      into l_wo
      from ausp as au
           inner join cabn as ca on au~atinn = ca~atinn
      where au~objek = p_vm_no and
            au~klart = '002' and
            ca~atnam = lc_wo_extc .
    concatenate p_wo_no
                l_wo
      into p_wo_no .
*   Reading Work Order Internal Color
    select single atwrt
      into l_wo
      from ausp as au
           inner join cabn as ca on au~atinn = ca~atinn
      where au~objek = p_vm_no and
            au~klart = '002' and
            ca~atnam = lc_wo_intc .
    concatenate p_wo_no
                l_wo
      into p_wo_no .
  endif.
endform.                    " setting_per_code
*&---------------------------------------------------------------------*
*&      Form  reading_wo_inf
*&---------------------------------------------------------------------*
*       Getting Worker Order Information
*----------------------------------------------------------------------*
*      -->P_L_WO_NO  text
*      -->P_0463   text
*      <--P_L_IT_DATA_MODEL  text
*----------------------------------------------------------------------*
form reading_wo_inf using    p_wo_no
                             p_atnam
                    changing p_atwrt .
  data: l_atwrt type ausp-atwrt.
  select single au~atwrt
    into l_atwrt
    from ausp as au
         inner join cabn as ca on au~atinn = ca~atinn
    where au~klart = '001'   and
          au~objek = p_wo_no and
          ca~atnam = p_atnam .
  move l_atwrt to p_atwrt .
endform.                    " reading_wo_inf
*&---------------------------------------------------------------------*
*&      Form  search_code
*&---------------------------------------------------------------------*
*       Searching Code
*----------------------------------------------------------------------*
*      -->P_L_WO_NO  text
*      -->P_L_IT_DATA_PART  text
*      <--P_L_IT_DATA_CODE  text
*----------------------------------------------------------------------*
form search_code using    p_no
                          p_part
                 changing p_code.
  data: l_atwrt type ausp-atwrt.
  select single atwrt
    into l_atwrt
    from ausp as au inner join
         cabn as ca on au~atinn = ca~atinn
    where ca~atnam = p_part and
          au~objek = p_no   and
          au~klart = '001'  .
  move l_atwrt to p_code .
endform.                    " search_code
*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       Building Events For ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form build_events.
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
*            slis_ev_pf_status_set c_pss,
*            slis_ev_user_command  c_uc,
            slis_ev_top_of_page   c_top.

endform.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       Modification of Events For ALV
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_C_TOP  text
*----------------------------------------------------------------------*
form modify_gt_events tables p_events_t like gt_events
                      using  p_form p_value.

  data: ls_event type slis_alv_event.

  read table  p_events_t  with key  name = p_form
                          into ls_event.
  if sy-subrc eq 0.
    move     p_value     to   ls_event-form.
    modify   p_events_t  from ls_event index sy-tabix.
  endif.

endform.                    " modify_gt_events
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       Building Field Categories For ALV
*----------------------------------------------------------------------*
*      -->P_0441   text
*----------------------------------------------------------------------*
form build_fieldcat using p_intab.

  clear   : gt_fieldcat, gt_fc.
  refresh : gt_fieldcat, gt_fc.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
       exporting
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       changing
            ct_fieldcat        = gt_fc.

  perform setting_fieldcat tables gt_fieldcat using :

                                  'S' 'YEAR'  ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '02',
                                  'E' 'SELTEXT_L'   'Year Type',

                                  'S' 'CODE'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Code',

                                  'S' 'PART'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'Part',

                                  'S' 'MODEL'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '03',
                                  'E' 'SELTEXT_L'   'Model',

                                  'S' 'TRIM_IN'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Trim In',

                                  'S' 'PBS_QTY'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'PBS QTY',

                                  'S' 'REJ_QTY'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'Rej.QTY',

                                  'S' 'WBS_QTY'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '05',
                                  'E' 'SELTEXT_L'   'WBS QTY',

                                  'S' 'T01_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T02_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T03_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T04_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T05_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T06_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T07_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T08_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T09_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'T10_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   '2T',

                                  'S' 'TOT_QTY'     ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '06',
                                  'E' 'SELTEXT_L'   'TOT QTY'.



endform.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       Setting Field Category For ALV
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_1259   text
*      -->P_1260   text
*      -->P_1261   text
*----------------------------------------------------------------------*
form setting_fieldcat tables   p_fieldcat like gt_fieldcat
                      using    p_gubun
                               p_field
                               p_value.

  data : l_col(40).

  field-symbols <fs>.

* START - FIELD ATTRIBUTE SETTING
  if p_gubun = 'S'.
    clear : g_fieldcat_s.
    read table gt_fc into g_fieldcat_s
                     with key fieldname  = p_field.
    exit.
  endif.

  concatenate 'G_FIELDCAT_S-' p_field  into l_col.
  assign (l_col) to <fs>.
  move   p_value to <fs>.

* END - FIELD ATTRIBUTE SETTING
  if p_gubun = 'E'.
    add 1 to cnt.
    g_fieldcat_s-col_pos = cnt.
    append g_fieldcat_s to p_fieldcat.
  endif.

endform.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       Building Layout For ALV
*----------------------------------------------------------------------*
*      -->P_0445   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form build_layout using p_cb p_color p_sum.

  clear gs_layout.


  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.
* check box
  if p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  endif.
* line color
  if p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  endif.
* sum
  if p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  endif.
*
endform.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       Building Comments For ALV
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
form build_comment using    p_gt_header type slis_t_listheader.
  data: ls_line type slis_listheader,
*        L_MANAGER(50),
        l_date(50),
        l_list(50),
        l_dsnam like t024d-dsnam,
        l_h_dsnam like t024d-dsnam,
*        l_total(5) TYPE i,
*        l_succ(5) TYPE i,
        l_ldate(10),
        l_hdate(10).

* Title
  clear ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-001.
  append ls_line to p_gt_header.
* Date
  ls_line-typ = 'S'.
  concatenate 'AS OF :'
              p_date
    into ls_line-info separated by ' '.
  append ls_line to p_gt_header.
* User
  ls_line-typ  = 'A'.
  ls_line-key  = 'User: '.
  ls_line-info = sy-uname.
  append ls_line to p_gt_header.

* today
  ls_line-typ  = 'A'.
  ls_line-key  = 'Today : '.
  ls_line-info = sy-datum .
  append ls_line to p_gt_header.

endform.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       Running GRID Viewer
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
form start_grid_viewer tables p_intab.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_bypassing_buffer       = 'X'
*            i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'SET_STATUS'
            i_callback_top_of_page   = 'TOP_OF_PAGE'
*            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
*            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
*            IT_SORT                  = GT_SORT[]
            i_save                   = 'A'
            is_variant               = g_variant
            it_events                = gt_events[]
            is_print                 = gs_print
            it_list_commentary       = gt_header
       importing
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       tables
            t_outtab                 = p_intab.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       Top of Page
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form top_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
       exporting
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            it_list_commentary = gt_header.

endform.                    " TOP_OF_PAGE
*---------------------------------------------------------------------*
*       FORM SET_STATUS                                               *
*---------------------------------------------------------------------*
form set_status using rt_extab type slis_t_extab.
  set pf-status 'STATUS' excluding rt_extab.
endform.                    "
*&---------------------------------------------------------------------*
*&      Form  delete_data_per_condition
*&---------------------------------------------------------------------*
*       Modification of Data By Parameters
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_data_per_condition.
  if p_model <> space.
    delete it_data where model <> p_model.
  endif.
  if p_year <> space.
    delete it_data where year <> p_year.
  endif.
  if p_code <> space.
    delete it_data where code <> p_code.
  endif.
endform.                    " delete_data_per_condition
