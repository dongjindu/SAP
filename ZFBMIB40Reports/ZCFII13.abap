report zcfii13  message-id  zmfi.
*&--------------------------------------------------------------------
*& Author                 : Hs.Jeong
*& Creation Date          : 07/11/2003
*& Specification By       : Andy Choi
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : Pi Adjustment Monthly Budget
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*& #1 02/04/2005 wskim
*&
*&--------------------------------------------------------------------
type-pools: slis.
include <icon>.
include <symbol>.
class cl_gui_resources definition load.

*----- LIST BOX DATA
type-pools vrm.
data: list  type vrm_values,
      value like line of list.

constants:
  c_f2code               like sy-ucomm                    value '&ETA'.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv.

data: wa_repid like sy-repid,
      wa_var_save(1) type c             value  'A',
      wa_default(1)  type c,
      wa_exit(1) type c,
      wa_variant like disvariant,
      wa_var like disvariant,
      wa_alv_function_name(30) type c value 'REUSE_ALV_GRID_LIST',
      wa_alv_get_info_name(40) type c,
      wa_cbo_curr_tot(15) type p,
      wa_cbo_org_tot(15) type p,
      wa_cbo_act_tot(15) type p,
      wa_cbo_after_tot(15) type p,
      wa_type like ztfi_reason-type,
      wa_mode.

data: begin of it_impr occurs 0,
        posnr   like impr-posnr,
        gjahr   like impr-gjahr,
        posid   like impr-posid,
        prnam   like impr-prnam,
        kostl   like impr-kostl,
        tot     like ztfi_imfm-tot,
      end of it_impr.

data: begin of it_impu occurs 0,
        posnr   like impu-posnr,
        gjahr   like impu-gjahr,
        post1   like impu-post1,
      end of it_impu.

data: begin of it_imfm occurs 0,
        posid   like ztfi_imfm-posid,
        post1   like impu-post1,
        prnam   like impr-prnam,
        kostl   like impr-kostl,
        ayear   like ztfi_imfm-ayear,
        gjahr   like ztfi_imfm-gjahr,
        seq     like ztfi_imfm-seq,
        posnr   like ztfi_imfm-posnr,
        gubun   like ztfi_imfm-gubun,
        status  like ztfi_imfm-status,
        trans   like ztfi_imfm-trans,
        zdate   like ztfi_imfm-zdate,
        type    like ztfi_imfm-type,
        reson   like ztfi_imfm-reson,
        tot     like ztfi_imfm-tot,
        wtp01     like ztfi_imfm-wtp01,
        wtp02     like ztfi_imfm-wtp02,
        wtp03     like ztfi_imfm-wtp03,
        wtp04     like ztfi_imfm-wtp04,
        wtp05     like ztfi_imfm-wtp05,
        wtp06     like ztfi_imfm-wtp06,
        wtp07     like ztfi_imfm-wtp07,
        wtp08     like ztfi_imfm-wtp08,
        wtp09     like ztfi_imfm-wtp09,
        wtp10    like ztfi_imfm-wtp10,
        wtp11    like ztfi_imfm-wtp11,
        wtp12    like ztfi_imfm-wtp12,
        text     like ztfi_imfm-text,
      end of it_imfm.
*---approval
data: begin of it_imfm_a occurs 0,
        posid   like ztfi_imfm-posid,
        post1   like impu-post1,
        prnam   like impr-prnam,
        kostl   like impr-kostl,
        ayear   like ztfi_imfm-ayear,
        gjahr   like ztfi_imfm-gjahr,
        seq     like ztfi_imfm-seq,
        posnr   like ztfi_imfm-posnr,
        gubun   like ztfi_imfm-gubun,
        status  like ztfi_imfm-status,
        trans   like ztfi_imfm-trans,
        zdate   like ztfi_imfm-zdate,
        type    like ztfi_imfm-type,
        reson   like ztfi_imfm-reson,
        tot     like ztfi_imfm-tot,
        wtp01     like ztfi_imfm-wtp01,
        wtp02     like ztfi_imfm-wtp02,
        wtp03     like ztfi_imfm-wtp03,
        wtp04     like ztfi_imfm-wtp04,
        wtp05     like ztfi_imfm-wtp05,
        wtp06     like ztfi_imfm-wtp06,
        wtp07     like ztfi_imfm-wtp07,
        wtp08     like ztfi_imfm-wtp08,
        wtp09     like ztfi_imfm-wtp09,
        wtp10    like ztfi_imfm-wtp10,
        wtp11    like ztfi_imfm-wtp11,
        wtp12    like ztfi_imfm-wtp12,
        text     like ztfi_imfm-text,
      end of it_imfm_a.

data: begin of it_imfm_sum occurs 0,
        posid   like ztfi_imfm-posid,
        post1   like impu-post1,
        ayear   like ztfi_imfm-ayear,
        prnam   like impr-prnam,
        posnr   like ztfi_imfm-posnr,
        gubun   like ztfi_imfm-gubun,
        seq     like ztfi_imfm-seq,
        status  like ztfi_imfm-status,
        act_txt(20),
        trans   like ztfi_imfm-trans,
        zdate   like ztfi_imfm-zdate,
        type    like ztfi_imfm-type,
        reson   like ztfi_imfm-reson,
        overall(15) type p, " DECIMALS 2,
        before(15)  type p, " DECIMALS 2,
        year(15)    type p, " DECIMALS 2,
        year1(15)   type p, " DECIMALS 2,
        after(15)   type p, " DECIMALS 2.
        text     like ztfi_imfm-text,
      end of it_imfm_sum.
*
data: begin of gt_out occurs 0,
        posnr   like impr-posnr,
        posid   like impr-posid,
        ayear   like impr-gjahr,
        post1   like impu-post1,
        prnam   like impr-prnam,
        kostl   like impr-kostl,
        gubun   like ztfi_imfm-gubun,
        seq     like ztfi_imfm-seq,
        status  like ztfi_imfm-status,
        trans   like ztfi_imfm-trans,
        status_txt(20),
        act_txt(20),
        zdate   like ztfi_imfm-zdate,
        type    like ztfi_imfm-type,
        reson   like ztfi_imfm-reson,
        text     like ztfi_imfm-text,
        amt(15)     type p,     " PI Budget
        overall(15) type p, " DECIMALS 2,
        before(15)  type p, " DECIMALS 2,
        year(15)    type p, " DECIMALS 2,
        year1(15)   type p, " DECIMALS 2,
*---2004/03/17
        year2(15)   type p, " DECIMALS 2,
        year3(15)   type p, " DECIMALS 2,
        year4(15)   type p, " DECIMALS 2,
        year5(15)   type p, " DECIMALS 2,
        year6(15)   type p, " DECIMALS 2,
        year7(15)   type p, " DECIMALS 2,
        year8(15)   type p, " DECIMALS 2,
*--------------*
        after(15)   type p, " DECIMALS 2.
        chkbox  type     c,
        light   type     c,
        tabcolor    type slis_t_specialcol_alv,
      end of gt_out.
*------ display current budget/ supplement/ after budget
data:  begin of it_dis occurs 0,
         year like  impr-gjahr,
         year_txt(7),
         curr(12)     type p, " DECIMALS 2,
         org(12)      type p, " DECIMALS 2,
         act(12)      type p, " DECIMALS 2,
         after(12)    type p, " DECIMALS 2,
       end of it_dis.

data:  begin of it_sap occurs 0,
         year like  impr-gjahr,
         year_txt(7),
         curr(12)     type p, " DECIMALS 2,
         io(12)       type p, " DECIMALS 2,
         act(12)      type p, " DECIMALS 2,
         after(12)    type p, " DECIMALS 2,
         sender(12)   type p,
         act_s(12)   type p,
         afer_s(12)   type p,
       end of it_sap.

data:  begin of it_sender occurs 0,
         year like  impr-gjahr,
         curr(12)     type p, " DECIMALS 2,
         act(12)      type p, " DECIMALS 2,
         after(12)    type p, " DECIMALS 2,
       end of it_sender.


data:  begin of it_month occurs 0,
         year like  impr-gjahr,
         mm1(12)    type p, " DECIMALS 2,
         mm2(12)    type p, " DECIMALS 2,
         mm3(12)    type p, " DECIMALS 2,
         mm4(12)    type p, " DECIMALS 2,
         mm5(12)    type p, " DECIMALS 2,
         mm6(12)    type p, " DECIMALS 2,
         mm7(12)    type p,  " DECIMALS 2,
         mm8(12)    type p,  " DECIMALS 2,
         mm9(12)    type p,  " DECIMALS 2,
         mm10(12)   type p, " DECIMALS 2,
         mm11(12)   type p, " DECIMALS 2,
         mm12(12)   type p, " DECIMALS 2,
         tot(14)    type p,
        chk(10),
       end of it_month.

data : it_ztfi_imfm like ztfi_imfm occurs 0 with header line.

data : it_pi_budget like zfi_pi_budget occurs 0 with header line.

data : it_io_budget like zfi_pi_budget occurs 0 with header line.

data : it_sender_budget like zfi_pi_budget occurs 0 with header line.

data : gt_pi_budget like zfi_pi_budget occurs 0 with header line.

data : begin of it_imakt occurs 0.
        include structure imakt.
data : end of it_imakt.
data : it_return like bapiret2 occurs 0 with header line.
*=====WorK area
data : wk_cnt type i,
       wa_cnt type i,
       wa_d_cnt type i,
       wa_posid  like impr-posid,
       wa_amt(15),
       wa_cbo_sum(15) type p,
       wa_sap_sum(15) type p,
       wa_cbo_overall(15),
       wa_amt1(15),
       wa_length(2) type n,
       wa_amt2   like ztfi_imfm-tot,
       wa_amt3   like ztfi_imfm-tot,
       wa_tot    like ztfi_imfm-tot,
       wa_hap    like ztfi_imfm-tot,
       wa_before like impr-gjahr,
       wa_before_txt(5),
       wa_year_txt(12),
       wa_last   like impr-gjahr,
       wa_year   like impr-gjahr,
       wa_year1  like impr-gjahr,
       wa_year2  like impr-gjahr,
       wa_year3  like impr-gjahr,
       wa_year4  like impr-gjahr,
       wa_year5  like impr-gjahr,
       wa_year6  like impr-gjahr,
       wa_year7  like impr-gjahr,
       wa_year8  like impr-gjahr,
       wa_after  like impr-gjahr,
       wa_f_year  like impr-gjahr,
       wa_t_year  like impr-gjahr,
       wa_after_txt(5),
       wa_text(100),
       wa_text1 like bpdy-sgtxt,
       ok_code   like sy-ucomm,
       wa_okcode like sy-ucomm,
       wa_confirm,
       wa_save_chk,
       wa_seq like ztfi_im_num-seq,
       wa_sender  like impr-posid,
       wa_chk_click,
       wa_textline(50),
       wa_texttitle(50),
       wa_trans,
       wa_save,
       wa_return like ztfi_imfm-tot,
       wa_belnr  like ztfi_imfm-belnr.
data : wa_act_cbo(15) type p,
       wa_org_tot(15) type p.

*====FOR BDC
data : it_bdc      like bdcdata occurs 0 with header line.

data:  it_messtab  like bdcmsgcoll occurs 0 with header line.

data : begin of it_err occurs 0,
          posid  like  impr-posid,
          msgid  like  bdcmsgcoll-msgid,
          msgv1  like  bdcmsgcoll-msgv1,
       end of it_err.

data : tcode like tstc-tcode.
ranges : so_versn for bpge-versn.

*- POSS.ENTRY
data : begin of it_value occurs 0,
        reson like ztfi_reason-reson,
        descr like ztfi_reason-descr,
       end of it_value.
data: g_fik   like fmfctr-fikrs. " memory id fik obligatory,

ranges : r_status for ztfi_imfm-status.
*----------------------------------------------------------------------
*
* define tables and internal structure
*
*----------------------------------------------------------------------
tables : impr, imak, imtp,
         ztfi_imfm, ztfi_reason, ztfi_im_num,bpja,bpge.

*----------------------------------------------------------------------
*
* SELECTION-SCREEN
*
*----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
*PARAMETERS : p_r1 RADIOBUTTON GROUP rad,
*             p_r2 RADIOBUTTON GROUP rad DEFAULT 'X',
*             p_r3 RADIOBUTTON GROUP rad.
*SELECTION-SCREEN END OF BLOCK b01.
*
parameters:
p_new   radiobutton group rad1 default 'X',
p_exist radiobutton group rad1 .

*PARAMETERS :   p_chk1 AS CHECKBOX ,
*               p_chk2 AS CHECKBOX DEFAULT 'X'.

selection-screen skip.

selection-screen begin of block b0 with frame title text-010.
select-options : s_prnam  for  impr-prnam obligatory.
parameters :  p_gjahr  like    imtp-gjahr obligatory,
              p_ippos  like    ripasw-ippos default '1'.

selection-screen skip.
select-options:
  s_posid   for   imak-posid,  "impr-posid,
  s_kostl   for   impr-kostl,
  s_gubun   for   ztfi_imfm-gubun.
selection-screen end of block b0.

*PARAMETERS : p_posid(20). " LIKE impr-posid.

selection-screen begin of block b2 with frame.
parameters :
  p_layout like disvariant-variant no-display.   "LAYOUT
selection-screen end of block b2.

selection-screen begin of block b33 with frame title text-033.
selection-screen end of block b33.
parameters: p_rollup as checkbox default 'X'.
parameters: p_auth(1) type c default 'X' no-display.
parameters: p_credit(1) type c default ' ' no-display.
parameters: p_mode(1) type c default 'E' no-display.

*----------------------------------------------------------------------
*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*
*----------------------------------------------------------------------
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_layout.
*  PERFORM f4_variant CHANGING p_layout.
*----------------------------------------------------------------------
*
* INITIALIZATION
*
*----------------------------------------------------------------------
*
initialization.

  wa_repid = sy-repid.
* ==> Change Variant saving type
*                         U-???, X-??(??), A-??, ' '-????
  wa_var_save = 'A'.
* ==> Change first mode   GRID or LIST
*  wa_alv_function_name = 'REUSE_ALV_GRID_DISPLAY'.
  wa_alv_function_name = 'REUSE_ALV_LIST_DISPLAY'.
  refresh : gt_fieldcat.
  clear   : gs_layout.

  p_gjahr = sy-datum+0(4).
  s_prnam-option = 'EQ'.
  s_prnam-sign   = 'I'.
  select * from imtp.
    s_prnam-low = imtp-prnam.
    append s_prnam.
  endselect.


*---------------------------------------------------------------------
*    M   A   I   N
*
*---------------------------------------------------------------------
end-of-selection.
  select single * from imtp
    where prnam in s_prnam.

  perform set_year.

  perform build_field_category
   using :
    'POSID'      'Position'      '20' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'SEQ'        'Num'           '04' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'POST1'      'Description'   '24' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'ACT_TXT'    'Activity'      '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'KOSTL'      'CCtr'          '05' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'ZDATE'      'Date'          '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'STATUS_TXT' 'Status'        '10' ' ' 'L'  ' '  ' '  '  ' '  ' ,
    'OVERALL'    'Overall'       '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'BEFORE'      wa_before_txt  '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'YEAR'        wa_year_txt    '12' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'YEAR1'       wa_year1       '10' ' ' 'R'  ' '  ' '  '  ' '  ' ,
    'AFTER'       wa_after_txt   '10' ' ' 'R'  ' '  ' '  '  ' '  ' .

* ==> 6. build sorts info
*  REFRESH gt_sorts.
*  PERFORM build_sort_table
*    USING :
*       '1'    'VTWEG'   'X'   'X'   '*'.
* ==> 1. select data from db
*AUTHORITY-CHECK OBJECT 'Z_FICTR'
*         ID 'FM_FIKRS'   FIELD 'H201'  "g_fik
*         ID 'FM_FICTR'   FIELD '*'.
*if sy-subrc <> 0.
*  clear: p_auth.
*endif.
*
  perform select_data.
  if gt_out[] is initial.
    message s000(zmfi) with 'No Data'.
    exit.
  endif.
* ==> 2. set variant default
  perform set_variant changing wa_var.
* ==> 3. set layout for alv style
  perform set_layout changing gs_layout.
* ==> 4. set events for alv
  perform set_events changing gt_events.
* ==> 7. call function display alv.
  perform display_alv.

***********************************************************************

*&---------------------------------------------------------------------*
*&      Form  f4_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_LAYOUT  text
*----------------------------------------------------------------------*
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
*&      Form  build_field_category
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0218   text
*      -->P_0219   text
*      -->P_0220   text
*      -->P_0221   text
*      -->P_0222   text
*      -->P_0223   text
*      -->P_0224   text
*      -->P_0225   text
*      -->P_0226   text
*----------------------------------------------------------------------*
form build_field_category using
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
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
*  if p_fieldname = 'KUNNR'.
*    ls_fieldcat-emphasize = 'C100'.
*  endif.
  append ls_fieldcat to gt_fieldcat.

endform.                    " build_field_category
*&---------------------------------------------------------------------*
*&      Form  select_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_data.
  data : "wa_posid(20),
         wa_pos type i.
*
*  CONCATENATE p_posid '%'  INTO wa_posid.

  refresh :  gt_out, it_impr, it_impu.
  clear   :  gt_out, it_impr, it_impu.
  clear   : gt_pi_budget, gt_pi_budget[].
  clear   : it_pi_budget, it_pi_budget[].
  clear : it_month, it_month[].
  clear : it_dis,   it_dis[].
  clear : it_sap,   it_sap[].
  refresh : it_sap, it_dis.
*========================================2004/04/07

*---2004/04/07
  if p_new = 'X'.  "New request
    select * into corresponding fields of table it_impr
       from impr
       where gjahr = p_gjahr
       and   posid in s_posid
       and   prnam in s_prnam
       and   kostl in s_kostl
       and   pprio <>  ' '.    "lower level

  else.            "Existing Request
    move 'I'  to r_status-sign.
    move 'EQ' to r_status-option.
    move ' '  to r_status-low. append r_status.  "Parked
    move 'P'  to r_status-low. append r_status.  "Parked

    select * into corresponding fields of table it_imfm
    from ztfi_imfm
    where ayear = p_gjahr
    and   posid in s_posid
*  AND   seq   <> '0000'
    and   gubun in s_gubun
    and   prnam in s_prnam
*    AND   kostl IN s_kostl
    and   status in r_status.

    loop at it_imfm.
      select * appending corresponding fields of table it_impr
      from impr
      where gjahr = p_gjahr
      and   posid = it_imfm-posid.
    endloop.
  endif.


**fix sign
*  perform fix_sign tables it_imfm.

*
  clear wa_d_cnt.
  if p_new = 'X'.       " Initial
    perform get_pi_description.
  else.                 " Ready for approval / approval
    perform get_pi_description2.
  endif.

  if p_new = 'X'.
    clear : it_pi_budget, it_pi_budget[].
    loop at it_impr.
      move-corresponding it_impr to gt_out.
      move it_impr-gjahr to gt_out-ayear.
*---Get PI name
      read table it_impu with key posnr = it_impr-posnr.
      if sy-subrc = 0.
        move it_impu-post1 to gt_out-post1.
      endif.
*---Get pi budget   supplement + return
      perform get_pi_budget using it_impr-posid p_gjahr it_impr-prnam.
*--Get cbp budget
      append gt_out.
      clear  gt_out.
    endloop.

  else.
    clear : it_imfm_sum, it_imfm_sum[].

    perform transform_imfm_to_sum.

    loop at it_imfm_sum.

      move-corresponding it_imfm_sum to gt_out.
*---Get pi budget   supplement + return
*     PERFORM get_pi_budget USING it_imfm_sum-posid p_gjahr s_prnam-low.

*      IF it_imfm_sum-status = 'R'.
*        MOVE 'Requested'   TO gt_out-status_txt.
*      ELSE
      if it_imfm_sum-status = 'P'.
        move 'Pakred'      to gt_out-status_txt.
      elseif it_imfm_sum-status = 'A'.
        move 'Approved'    to gt_out-status_txt.
      endif.

      read table it_impu with key posnr = it_imfm_sum-posnr binary search.
      if sy-subrc = 0.
        move it_impu-post1 to gt_out-post1.
      endif.
      case it_imfm_sum-gubun.
        when '1'.
          move 'Original'  to gt_out-act_txt.
        when '2'.
          move 'Supplement' to gt_out-act_txt.
        when '3'.
          move 'Return'     to gt_out-act_txt.
        when '4'.
          move 'Transfer'   to gt_out-act_txt.
      endcase.

      append gt_out.
      clear  gt_out.
    endloop.
  endif.

  sort gt_out by posid seq zdate ascending.
endform.                    " select_data
*&---------------------------------------------------------------------*
*&      Form  set_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WA_VAR  text
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
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
  cs_layo-lights_fieldname       = ' '. "LIGHT'.
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
*&---------------------------------------------------------------------*
*&      Form  set_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GT_EVENTS  text
*----------------------------------------------------------------------*
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

endform.                    " set_events

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


  case r_ucomm.
*   ---------------------------------- processing on double click.
    when '&IC1'.
      read table gt_out index rs_selfield-tabindex.
      case rs_selfield-fieldname.
        when 'POSID'.
          set parameter id 'IMT' field gt_out-prnam.
          set parameter id 'IMP' field gt_out-posid.
          set parameter id 'GJR' field p_gjahr.
          call transaction 'IM23' and skip first screen.
        when  others.
          perform get_pi_budget using gt_out-posid p_gjahr s_prnam-low.
*---order budget assigned by position
          perform get_io_budget using gt_out-posid p_gjahr s_prnam-low.
          perform make_monthly_data.
*          if gt_out-trans <> ' '.
*             perform get_sender_data.
*          endif.
          call screen 500   starting at 04 01  ending at 160 28 .
          rs_selfield-refresh = 'X'.
          perform select_data.
      endcase.

*-- show parked entries
    when '&INIT'.
      p_new   = 'X'.
      p_exist = ' '.
      rs_selfield-refresh = 'X'.
      perform select_data.
*-- show requested entries
    when '&REQ'.
      p_new   = ' '.
      p_exist = 'X'.
      rs_selfield-refresh = 'X'.
      clear : it_dis,   it_dis[].
      clear : it_sap,   it_sap[].
      refresh : it_sap, it_dis,  gt_out.
      perform select_data.
*-- process posting
    when '&APPRQ'.
      check p_new = space.  "if not new request mode
      data : wa_app_cnt type i.
      clear : wa_app_cnt.

      loop at  gt_out where chkbox = 'X'.
*       CHECK  gt_out-status = 'R'.  " need to check? let's skip to make it simple
        perform make_cbo_data using gt_out-posid
                                    gt_out-ayear
                                    gt_out-gubun
                                    gt_out-seq
                                    gt_out-status.
        perform update_transaction.

        add 1 to wa_app_cnt.
        clear wa_d_cnt.

      endloop.

      describe table it_err lines wa_d_cnt.
      if wa_d_cnt > 1.
        loop at it_err.
          write : /10(24)  it_err-posid,
                   40(20)  it_err-msgid,
                   64(50)  it_err-msgv1.
        endloop.
      endif.

*--   process roll-up values
      if wa_app_cnt > 0.
        if p_rollup = 'X'.
          submit zaimbpup and return
                 with program  =  s_prnam-low
*               WITH POSITION =  POSITION
                 with app_year =  p_gjahr
                 with budget   =  'X'
                 with fromleaf =  'X'.
*            WITH so_versn IN so_versn.
        endif.
      else.
        message s000(zmfi) with 'No selected'.
      endif.
      rs_selfield-refresh = 'X'.
      perform select_data.
*    WHEN '&PRO'.
**--Next year
*      LOOP AT gt_out WHERE chkbox = 'X'.
*        PERFORM bdc_process USING gt_out-posid '1'.
**        PERFORM bdc_process USING gt_out-posid '2'.
**        PERFORM bdc_process USING gt_out-posid '3'.
**        PERFORM bdc_process USING gt_out-posid '4'.
**        PERFORM bdc_process USING gt_out-posid '5'.
**        PERFORM bdc_process USING gt_out-posid '6'.
*        MODIFY  gt_out INDEX rs_selfield-tabindex.
*      ENDLOOP.
*      rs_selfield-refresh = 'X'.
*      PERFORM select_data.

*    WHEN '&RET'.
*      LOOP AT it_err.
*        WRITE : /10(24)  it_err-posid,
*                 40(20)  it_err-msgid,
*                 64(50)  it_err-msgv1.
*      ENDLOOP.

* delete requested entries
    when '&DEL'.
*      IF chk1 = ' '.
      wa_textline = 'Are you really data Delete?'.
      wa_texttitle = 'Data Delete(Y/N)'.
      perform popup_step_rtn using
                         wa_confirm wa_textline wa_texttitle.
      if wa_confirm = 'J'.
        loop at gt_out where chkbox = 'X'.
          if gt_out-status = 'A'.
            message s000(zmfi) with 'already approved'.
            continue.
          endif.
          perform delete_cbo_rtn.
        endloop.
        rs_selfield-refresh = 'X'.
        perform select_data.
      endif.
*      ENDIF.
*   ---------------------------------- switching view type grid or list
    when 'LIST' or 'GRID'.
      perform switch_list_or_grid using r_ucomm.
  endcase.

  check r_ucomm eq 'LIST' or
        r_ucomm eq 'GRID'.

  rs_selfield-exit = 'X'.

endform.                    "alv_event_user_command
*&---------------------------------------------------------------------*
*&      Form  switch_list_or_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_R_UCOMM  text
*----------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_alv.
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

endform.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_PI_BUDGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_IMPR_POSID  text
*      -->P_P_GJAHR  text
*      -->P_IT_IMPR_PRNAM  text
*----------------------------------------------------------------------*
form get_pi_budget using    u_posid
                            u_gjahr
                            u_prnam.
  refresh : it_pi_budget.
  clear   : it_pi_budget.

  call function 'Z_FFI_GET_PI_BUDGET'
    exporting
      posid = u_posid
      prnam = u_prnam
      gjahr = u_gjahr
    tables
      out   = it_pi_budget.

  clear : wa_amt2, wa_amt3.

  loop at it_pi_budget where posid = u_posid.
    case it_pi_budget-gjahr.
      when '1111'.
        move it_pi_budget-wtjhr to gt_out-overall.
      when wa_year.
        move it_pi_budget-wtjhr to gt_out-year.
      when wa_year1.
        move it_pi_budget-wtjhr to gt_out-year1.
      when others.
        if  it_pi_budget-gjahr < wa_year.
          wa_amt2 = wa_amt2 + it_pi_budget-wtjhr.
          move wa_amt2   to  gt_out-before.
        elseif it_pi_budget-gjahr > wa_year1.
          wa_amt3 = wa_amt3 + it_pi_budget-wtjhr.
          move wa_amt3   to  gt_out-after.
        endif.
    endcase.

  endloop.

endform.                    " GET_PI_BUDGET
*&---------------------------------------------------------------------*
*&      Form  make_bdc_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1098   text
*      -->P_1099   text
*      -->P_1100   text
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
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
form bdc_process using    u_posid u_level.
*
  if gt_out-gubun = '3'.
    tcode = 'IM38'.
  elseif gt_out-gubun = '1'.
    tcode = 'IM32'.
  else.
    tcode = 'IM30'.
  endif.

  refresh : it_bdc, it_messtab.
*
  perform make_bdc_rtn using :
                 'X'  'SAPMKBUD'        '0800',
                 ' '  'IMTP-PRNAM'      gt_out-prnam,
                 ' '  'IMPR-POSID'     u_posid, "wa_posid,
                 ' '  'IMPR-GJAHR'      p_gjahr,
                 ' '  'BDC_OKCODE'      '/00'.

*** 11/04/2013 T00206 Start
  data: ls_xaktb type im_xaktb.
  select single xaktb
    into ls_xaktb
    from impr
   where gjahr = p_gjahr
     and posid = u_posid.

  if sy-subrc eq 0.
    if ls_xaktb eq 'X'.
      clear p_ippos.
    endif.
  endif.
*** 11/04/2013 T00206 End

  if p_ippos = '1'.
    perform make_bdc_rtn using :
                   'X'  'SAPLAIPA'        '0400',
                   ' '  'MARKER(01) '     'X',
                   ' '  'BDC_OKCODE'      '=TAKE'.
  elseif p_ippos = '2'.
    perform make_bdc_rtn using :
                   'X'  'SAPLAIPA'        '0400',
                   ' '  'MARKER(02) '     'X',
                   ' '  'BDC_OKCODE'      '=TAKE'.
  else.
*    EXIT.
  endif.
*----GET monthly      ANNUAL.
  clear  : wa_amt, wa_amt1, wa_cbo_sum, wa_cbo_overall.
*---process amount update
  if gt_out-before <> 0.
    perform bdc_amount_process using wa_before gt_out-before.
  endif.
  if gt_out-year <> 0.
    perform bdc_amount_process using wa_year gt_out-year.
  endif.
  if gt_out-year1 <> 0.
    perform bdc_amount_process using wa_year1 gt_out-year1.
  endif.
  if gt_out-year2 <> 0.
    perform bdc_amount_process using wa_year2 gt_out-year2.
  endif.
  if gt_out-year3 <> 0.
    perform bdc_amount_process using wa_year3 gt_out-year3.
  endif.
  if gt_out-year4 <> 0.
    perform bdc_amount_process using wa_year4 gt_out-year4.
  endif.
  if gt_out-year5 <> 0.
    perform bdc_amount_process using wa_year5 gt_out-year5.
  endif.
  if gt_out-year6 <> 0.
    perform bdc_amount_process using wa_year6 gt_out-year6.
  endif.
  if gt_out-year7 <> 0.
    perform bdc_amount_process using wa_year7 gt_out-year7.
  endif.
  if gt_out-year8 <> 0.
    perform bdc_amount_process using wa_year8 gt_out-year8.
  endif.
*
*----2004/04/15  original amount check
  if gt_out-gubun = '1'.
*    PERFORM get_cbo_approval_original USING   gt_out-gubun.
*    wa_amt = wa_cbo_sum.
    clear : wa_sap_sum.
*issue #200501020-002 requested by ycyoon changed by wskim,02/04/05
*-----Start : change logic
*    READ TABLE it_pi_budget WITH KEY posid = u_posid
*                                     gjahr = '1111'.
*    IF sy-subrc = 0.
*      wa_sap_sum = it_pi_budget-org.
*    ELSE.
*      wa_sap_sum = 0.
*    ENDIF.
    perform get_sap_budget_im33_ov using u_posid
                                         p_gjahr
                                         wa_sap_sum.

    wa_amt1 = gt_out-overall + wa_sap_sum.
  else.
*change sign
    if gt_out-gubun = '3'.
      wa_amt1 = gt_out-overall * -1.
    else.
      wa_amt1 = gt_out-overall.
    endif.
  endif.
*----

  translate  wa_amt1  using ', '.
  condense   wa_amt1 no-gaps.
  perform make_bdc_rtn using :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     '0',
                 ' '  'BDC_OKCODE'      '=DROT'.
  perform make_bdc_rtn using :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     '0',
                 ' '  'BPDY-WERT1(01)'  wa_amt1,
*                ' '  'BDC_OKCODE'      '/00'.
                 ' '  'BDC_OKCODE'      '=BELE'.
*---text
  perform make_bdc_rtn using :
                 'X'  'SAPLKBPP'        '0702',
                 ' '  'BPDY-SGTXT'       wa_text1,
                 ' '  'BDC_OKCODE'      '=ENTE'.
  perform make_bdc_rtn using :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'BDC_OKCODE'      '=SAVE'.


  call transaction tcode   using it_bdc
*                          MODE   'E'
                           mode   p_mode
                           update 'S'
                    messages into it_messtab.
  loop at it_messtab.
    if it_messtab-msgtyp = 'S'.
      if it_messtab-msgid = 'BP'.
        if it_messtab-msgnr = '043'.
          clear wa_belnr.
          wa_belnr = it_messtab-msgv1.
*          IF u_level = '6'.
*            PERFORM create_cf_cbo USING
*              p_gjahr  wa_posid.
*          ENDIF.
          continue.
        endif.
      endif.
    endif.
    move-corresponding it_messtab to it_err.
    move wa_posid     to  it_err-posid.
    append it_err.
    clear  it_err.
  endloop.

  refresh : it_bdc, it_messtab.
  clear   : it_bdc, it_messtab.

endform.                    " BDC_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CREATE_CF_CBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM create_cf_cbo USING u_gjahr u_posid.
*  LOOP AT gt_pi_budget  WHERE posid = u_posid.
*    CLEAR wa_amt1.
*    wa_amt1 = gt_pi_budget-supp + gt_pi_budget-ret.
*    ztfi_imfm-mandt    = sy-mandt.
*    ztfi_imfm-posnr    = gt_out-posnr.
*    ztfi_imfm-ayear    = p_gjahr.
*    ztfi_imfm-gjahr    = gt_pi_budget-gjahr.
*    ztfi_imfm-gubun    = '9'.
*    ztfi_imfm-seq      = '01'.
*    ztfi_imfm-posid    = u_posid.
*    ztfi_imfm-tot      = wa_amt1.
*    ztfi_imfm-uname    = sy-uname.
*    ztfi_imfm-zdate    = sy-datum.
*    ztfi_imfm-TWAER    = imtp-waers.
*
*    MODIFY ztfi_imfm.
*  ENDLOOP.
*ENDFORM.                    " CREATE_CF_CBO
*&---------------------------------------------------------------------*
*&      Form  set_year
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_year.
  clear : wa_before, wa_last, wa_year, wa_year1, wa_year2,
          wa_year3.
  wa_before  = p_gjahr - 1.
  wa_year    = p_gjahr.
  wa_year1   = p_gjahr + 1.
  wa_year2   = p_gjahr + 2.
  wa_year3   = p_gjahr + 3.
  wa_year4   = p_gjahr + 4.
  wa_year5   = p_gjahr + 5.
  wa_year6   = p_gjahr + 6.
  wa_year7   = p_gjahr + 7.
  wa_year8   = p_gjahr + 8.
  concatenate '~' wa_before  into wa_before_txt.
  concatenate 'AY ' wa_year  into wa_year_txt.
  concatenate wa_year2 '~' into wa_after_txt.

endform.                    " set_year
*&---------------------------------------------------------------------*
*&      Form  MAKE_MONTHLY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_monthly_data.
  clear wa_f_year.
*issue :20050120-002
*-----Start #1
  data : w_int type i.
  clear w_int.
  sort it_pi_budget by gjahr .
  describe table it_pi_budget lines w_int.
  if w_int <> 0.
    read table it_pi_budget index 2.
    wa_f_year = it_pi_budget-gjahr.
  else.
    wa_f_year = p_gjahr.
  endif.
*-----End
  clear : it_month, it_month[].
  clear : it_dis,   it_dis[].
  clear : it_sap,   it_sap[].
  refresh : it_sap, it_dis.
  if p_new = 'X'.     "Initial
*--number ranges
    perform get_number.
    move wa_seq    to  gt_out-seq.
    move wa_seq    to  ztfi_imfm-seq.
  endif.

  do 10 times.
    move wa_f_year to it_dis-year.
    move wa_f_year to it_dis-year_txt.
    move wa_f_year to it_month-year.
    move wa_f_year to it_sap-year.
    add 1 to wa_f_year.
    append it_month.
    clear  it_month.
    append it_dis.
    clear  it_dis.
    append it_sap.
    clear  it_sap.
  enddo.
  wa_sender = gt_out-trans.
*---REASON CODE
  ztfi_reason-type  = gt_out-gubun.
  ztfi_reason-reson = gt_out-reson.
  wa_text           = gt_out-text.
*--SAP current budget
  perform move_pi_budget.
*----------------------*
* IF p_chk2 = 'X' OR p_chk3 = 'X' OR p_chk4 = 'X'.
  clear : wa_cbo_curr_tot, wa_cbo_act_tot, wa_cbo_after_tot,
          wa_cbo_org_tot.

*---2004/04/07
  perform get_cbo_approval using gt_out-posid.

  loop at it_dis.
*
    loop at it_imfm_a where posid = gt_out-posid
                        and ayear = p_gjahr
                        and status ne 'P'.
      case it_imfm_a-gjahr.
        when it_dis-year.
* change sign logic
*          IF it_imfm_a-gubun = '3'.
*            it_dis-curr = it_dis-curr - it_imfm_a-tot.
*            wa_cbo_curr_tot = wa_cbo_curr_tot - it_imfm_a-tot.
*          ELSE.
*            it_dis-curr = it_dis-curr + it_imfm_a-tot.
*            wa_cbo_curr_tot = wa_cbo_curr_tot + it_imfm_a-tot.
*            IF it_imfm_a-gubun = '1'.
*              it_dis-org = it_dis-org + it_imfm_a-tot.
*              wa_cbo_org_tot = wa_cbo_org_tot + it_imfm_a-tot.
*            ENDIF.
*          ENDIF.
          it_dis-curr = it_dis-curr + it_imfm_a-tot.
          wa_cbo_curr_tot = wa_cbo_curr_tot + it_imfm_a-tot.
          if it_imfm_a-gubun = '1'.
            it_dis-org = it_dis-org + it_imfm_a-tot.
            wa_cbo_org_tot = wa_cbo_org_tot + it_imfm_a-tot.
          endif.

          modify it_dis.
      endcase.
    endloop.

  endloop.

*---CBO total.
  move '1111' to it_dis-year.
  move 'Overall' to it_dis-year_txt.
  move wa_cbo_curr_tot to it_dis-curr.
  move wa_cbo_org_tot to it_dis-org.
  append it_dis.
  sort it_dis  ascending by year.
*----
  loop at it_month.
    read table it_imfm with key posid = gt_out-posid
                                ayear = p_gjahr
                                gjahr = it_month-year
                                kostl = gt_out-kostl
                                gubun = gt_out-gubun
                                seq   = gt_out-seq.
    if sy-subrc = 0.
      move it_imfm-wtp01   to it_month-mm1.
      move it_imfm-wtp02   to it_month-mm2.
      move it_imfm-wtp03   to it_month-mm3.
      move it_imfm-wtp04   to it_month-mm4.
      move it_imfm-wtp05   to it_month-mm5.
      move it_imfm-wtp06   to it_month-mm6.
      move it_imfm-wtp07   to it_month-mm7.
      move it_imfm-wtp08   to it_month-mm8.
      move it_imfm-wtp09   to it_month-mm9.
      move it_imfm-wtp10   to it_month-mm10.
      move it_imfm-wtp11   to it_month-mm11.
      move it_imfm-wtp12   to it_month-mm12.
      clear it_month-tot.
      it_month-tot =    it_imfm-wtp01   + it_imfm-wtp02   +
                        it_imfm-wtp03   + it_imfm-wtp04   +
                        it_imfm-wtp05   + it_imfm-wtp06   +
                        it_imfm-wtp07   + it_imfm-wtp08   +
                        it_imfm-wtp09   + it_imfm-wtp10   +
                        it_imfm-wtp11   + it_imfm-wtp12.
      modify it_month.
    endif.
  endloop.
* ENDIF.
*
endform.                    " MAKE_MONTHLY_DATA

*&spwizard: declaration of tablecontrol 'TC_1' itself
controls: tc_1 type tableview using screen 0500.

*&spwizard: output module for tc 'TC_1'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module tc_1_change_tc_attr output.
  describe table it_dis lines tc_1-lines.
endmodule.                    "tc_1_change_tc_attr OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit_screen input.
  case sy-ucomm.
    when 'EXIT'.
      leave to screen 0.
  endcase.
endmodule.                 " EXIT_SCREEN  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0500 output.
  set pf-status '500'.
  set titlebar '500'.

endmodule.                 " STATUS_0500  OUTPUT

*&spwizard: declaration of tablecontrol 'TC_2' itself
controls: tc_2 type tableview using screen 0500.

*&spwizard: output module for tc 'TC_2'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module tc_2_change_tc_attr output.
  describe table it_month lines tc_2-lines.
endmodule.                    "tc_2_change_tc_attr OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0500 input.
  clear wa_okcode.
  wa_okcode = ok_code.
  clear ok_code.
  case wa_okcode.
*
    when 'EXIT'.
      clear : wa_sender, ztfi_reason-type, ztfi_reason-reson.
      leave to screen 0.
*
    when 'CHK'.
*-----Start wskim : add logic-if status of some request is not yet
*approval
      move ' ' to wa_save.
      if p_new = 'X'.  "if new mode, check pending approval.
        select single * from ztfi_imfm
         where posid eq gt_out-posid
           and ayear eq p_gjahr
           and status eq 'P'.

        if sy-subrc = 0.
          message w000(zmfi) with text-034 text-035.
          move 'X' to wa_save.
          exit.
        endif.
      endif.
*-----End

      move ' ' to wa_save.
      move 'Q' to wa_chk_click.
      if ztfi_reason-type = ' ' or ztfi_reason-reson = ' '.
        message i000(zmfi) with 'Check Reason code'.
      endif.
      if wa_text = ' '.
        message i000(zmfi) with 'Check Reason Text'.
      endif.
      perform check_reason.
      if wa_d_cnt < 1.
        message i000(zmfi) with 'Check Reason code'.
      endif.
*--
      data : wa_act_tot(15) type p,
             wa_curr_tot(15) type p,
             wa_after_tot(15) type p.
      data : wa_act_tot_s(15) type p,
             wa_after_tot_s(15) type p.
      clear : wa_act_tot, wa_after_tot.

      data : wa_act_sap_tot(15) type p,
             wa_after_sap(15) type p,
             wa_act_sap_s(15) type p,
             wa_after_sap_s(15) type p.

      clear : wa_act_sap_tot, wa_after_sap, wa_hap.
      clear : wa_act_sap_s, wa_after_sap_s.
      clear : wa_act_tot_s, wa_after_tot_s, wa_curr_tot.
      clear : wa_org_tot.
      loop at it_month.

        it_month-tot = it_month-mm1 + it_month-mm2 + it_month-mm3 +
                       it_month-mm4 + it_month-mm5 + it_month-mm6 +
                       it_month-mm7 + it_month-mm8 + it_month-mm9 +
                       it_month-mm10 + it_month-mm11 + it_month-mm12.

        modify it_month. "IDEX sy-tabix.
        wa_hap = wa_hap + it_month-tot.

        perform check_curr_rtn using  it_month-year it_month-tot.
      endloop.
*---over all cbo
      loop at it_dis where  year = '1111'.
        if sy-subrc = 0.
*          it_dis-curr = wa_curr_tot.
          it_dis-act = wa_curr_tot.
          if ztfi_reason-type = '1'.
*           it_dis-after = it_dis-org + it_dis-act.
            it_dis-after = wa_org_tot.
          else.
            it_dis-after = it_dis-curr + it_dis-act.
          endif.
          modify it_dis.
        endif.
      endloop.
*---over all sap
      loop at it_sap where  year = '1111'.
        if sy-subrc = 0.
*---2004/03/31
          it_sap-act   = wa_act_sap_tot.
          it_sap-after = it_sap-curr - it_sap-io + wa_act_sap_tot.
          it_sap-afer_s = it_sap-sender - wa_act_sap_tot.
          modify it_sap.
        endif.
      endloop.
*===2004/03/17 03/31

      read table it_sap with key year = '1111'.
      if sy-subrc = 0.
*---2004/03/31
** Ebable on 01/07/14 by Requested by Mr. Hong
        IF  it_sap-after < 0.
          MESSAGE w000(zmfi) WITH
                          'Check Amount'.
          MOVE 'X' TO wa_save.
          EXIT.
        ENDIF.
      endif.
** End on 01/07/14
*      ENDIF.

    when 'SAVE'.
      if ztfi_reason-type = ' ' or ztfi_reason-reson = ' '.
        message i000(zmfi) with 'Check Reason code'.
        exit.
      endif.
      if wa_text = ' '.
        message i000(zmfi) with 'Check Reason Text'.
        exit.
      endif.
      if wa_chk_click <> 'Q'.
        message i000(zmfi) with 'Click check Button'.
        exit.
      endif.

      if wa_save eq 'X'.
        message w000(zmfi) with 'Found error'.
        exit.
      endif.
**---2004/03/16
*      IF wa_hap = 0.
*        MESSAGE w000(zmfi) WITH 'Total amount zeors'.
*        EXIT.
*      ENDIF.
      if ztfi_reason-type = '4'.
        if wa_sender is initial.
          message i000(zmfi) with 'Miss Sender PI Code'.
          exit.
        endif.
      endif.

      perform save_cbo_rtn.
      leave to screen 0.

      message s000(zmfi) with 'Parked successfully'.
      move 'X' to wa_chk_click.

  endcase.
endmodule.                 " USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_2_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_2_modify input.
  it_month-tot = it_month-mm1 + it_month-mm2 + it_month-mm3 + it_month-mm4
                          + it_month-mm5 + it_month-mm6 + it_month-mm7  +
                            it_month-mm8 + it_month-mm9 + it_month-mm10 +
                                           it_month-mm11  + it_month-mm12.
  modify it_month
    index tc_2-current_line.
endmodule.                 " TC_2_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_CBO_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form save_cbo_rtn.
  move gt_out-posid     to    ztfi_imfm-posid.
  move p_gjahr          to    ztfi_imfm-ayear.
  move gt_out-posid     to    ztfi_imfm-posnr.  "with masking

  move ztfi_reason-type to    ztfi_imfm-gubun.
  move ztfi_reason-reson to    ztfi_imfm-reson.
  move gt_out-prnam     to    ztfi_imfm-prnam.
*  MOVE gt_out-kostl     TO    ztfi_imfm-kostl.
  move sy-datum         to    ztfi_imfm-zdate.
  move sy-uname         to    ztfi_imfm-uname.
  move p_ippos          to    ztfi_imfm-type.
*  MOVE ztfi_reason-type TO    ztfi_imfm-type.
  move ztfi_reason-reson to   ztfi_imfm-reson.
  move wa_sender         to   ztfi_imfm-trans.
  move wa_text+0(50)     to   ztfi_imfm-text.
  move ' ' to wa_save_chk.

* yearly records in z-table
  loop at it_month.
*    CHECK it_month-tot <> 0.
    if it_month-mm1  = 0 and
       it_month-mm2  = 0 and
       it_month-mm3  = 0 and
       it_month-mm4  = 0 and
       it_month-mm5  = 0 and
       it_month-mm6  = 0 and
       it_month-mm7  = 0 and
       it_month-mm8  = 0 and
       it_month-mm9  = 0 and
       it_month-mm10 = 0 and
       it_month-mm11 = 0 and
       it_month-mm12 = 0.
      continue.
    endif.
    move  it_month-year to ztfi_imfm-gjahr.
    move  gt_out-seq      to  ztfi_imfm-seq.
    move : it_month-mm1   to  ztfi_imfm-wtp01,
           it_month-mm2   to  ztfi_imfm-wtp02,
           it_month-mm3   to  ztfi_imfm-wtp03,
           it_month-mm4   to  ztfi_imfm-wtp04,
           it_month-mm5   to  ztfi_imfm-wtp05,
           it_month-mm6   to  ztfi_imfm-wtp06,
           it_month-mm7   to  ztfi_imfm-wtp07,
           it_month-mm8   to  ztfi_imfm-wtp08,
           it_month-mm9   to  ztfi_imfm-wtp09,
           it_month-mm10  to  ztfi_imfm-wtp10,
           it_month-mm11  to  ztfi_imfm-wtp11,
           it_month-mm12  to  ztfi_imfm-wtp12,
           it_month-tot   to  ztfi_imfm-tot.

*FIXME
    move gt_out-status    to  ztfi_imfm-status.
    move 'P'              to  ztfi_imfm-status.  "parked
*    ELSEIF gt_out-status = 'R'.
*      MOVE 'F'   TO ztfi_imfm-status.
*    ELSEIF gt_out-status = 'F'.
*      MOVE 'A'   TO ztfi_imfm-status.
*    ENDIF.

    ztfi_imfm-twaer  = imtp-waers.
** On 03/25/14 (
    ZTFI_IMFM-VERSI = '000'.
** ) End
    modify ztfi_imfm.  "insert or change

*in case of transfer, create another records for return/supplement

* change sign
    if ztfi_imfm-gubun = '4'.
*---  not yet supported.
      message e000 with 'Transfer is not yet supported'.
      ztfi_imfm-posid = gt_out-trans.
      ztfi_imfm-posnr = gt_out-trans.
      ztfi_imfm-trans = gt_out-posid.

      ztfi_imfm-wtp01 = ztfi_imfm-wtp01 * -1.
      ztfi_imfm-wtp02 = ztfi_imfm-wtp02 * -1.
      ztfi_imfm-wtp03 = ztfi_imfm-wtp03 * -1.
      ztfi_imfm-wtp04 = ztfi_imfm-wtp04 * -1.
      ztfi_imfm-wtp05 = ztfi_imfm-wtp05 * -1.
      ztfi_imfm-wtp06 = ztfi_imfm-wtp06 * -1.
      ztfi_imfm-wtp07 = ztfi_imfm-wtp07 * -1.
      ztfi_imfm-wtp08 = ztfi_imfm-wtp08 * -1.
      ztfi_imfm-wtp09 = ztfi_imfm-wtp09 * -1.
      ztfi_imfm-wtp10 = ztfi_imfm-wtp10 * -1.
      ztfi_imfm-wtp11 = ztfi_imfm-wtp11 * -1.
      ztfi_imfm-wtp12 = ztfi_imfm-wtp12 * -1.
      ztfi_imfm-tot   = ztfi_imfm-tot   * -1.
** On 03/25/14
      ZTFI_IMFM-VERSI = '000'.
** End
      modify ztfi_imfm.  "insert or change

*FIXME what if sender changed???
    endif.

    move 'Q' to wa_save_chk.

  endloop.

* user exit - ZXKBPU04
  commit work.

endform.                    " SAVE_CBO_RTN
*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_number.
  clear ztfi_imfm-seq.
  clear wa_seq.

  select max( seq )  into wa_seq
*    FROM ztfi_im_num
    from ztfi_imfm
    where posid = gt_out-posid
      and gjahr = p_gjahr.

  wa_seq = wa_seq + 1.

  move gt_out-posid to ztfi_im_num-posid.
  move p_gjahr      to ztfi_im_num-gjahr.
  move wa_seq       to ztfi_im_num-seq.
  modify ztfi_im_num.
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*       EXPORTING
*            nr_range_nr             = '01'
*            object                  = 'ZNFI_IM'
*            quantity                = '1'
*            toyear                  = p_gjahr
*       IMPORTING
*            number                  = ztfi_imfm-seq
*       EXCEPTIONS
*            interval_not_found      = 1
*            number_range_not_intern = 2
*            object_not_found        = 3
*            quantity_is_0           = 4
*            quantity_is_not_1       = 5
*            interval_overflow       = 6
*            OTHERS                  = 7.
*

endform.                    " GET_NUMBER
*&---------------------------------------------------------------------*
*&      Form  POPUP_STEP_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CONFIRM  text
*----------------------------------------------------------------------*
form popup_step_rtn using    u_confirm u_textline u_texttitle.
  clear u_confirm.
  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
*     DEFAULTOPTION  = 'Y'
      textline1      = u_textline "'Are you really data save?'
*     TEXTLINE2      = ' '
      titel          = u_texttitle "'Data Save(Y/N)'
*     START_COLUMN   = 25
*     START_ROW      = 6
*     CANCEL_DISPLAY = 'X'
    importing
      answer         = u_confirm.

endform.                    " POPUP_STEP_RTN
*&---------------------------------------------------------------------*
*&      Form  get_pi_description
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_pi_description.
  describe table it_impr lines wa_d_cnt.
  if wa_d_cnt > 0.
    select * into corresponding fields of table it_impu
    from impu
    for all entries in it_impr
    where spras = sy-langu
    and   posnr = it_impr-posnr.

    sort it_impu by posnr.
  endif.

endform.                    " get_pi_description
*&---------------------------------------------------------------------*
*&      Form  get_pi_description2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_pi_description2.
  data: lv_posnr like impu-posnr.

  describe table it_imfm lines wa_d_cnt.
  if wa_d_cnt > 0.
    loop at it_imfm.
      lv_posnr = it_imfm-posnr.
      select * appending corresponding fields of table it_impu
        from impu
        where spras = sy-langu
          and posnr = lv_posnr.

    endloop.
    sort it_impu by posnr.
  endif.

endform.                    " get_pi_description2
*&---------------------------------------------------------------------*
*&      Form  CHECK_CURR_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MONTH_TOT  text
*----------------------------------------------------------------------*
form check_curr_rtn using   u_year u_tot.
*-----Start wskim
* change sign
*  IF ztfi_reason-type = '3'.
*    u_tot = u_tot * -1.
*  ENDIF.
*-----End
  loop at it_dis where year = u_year
                  and  year <> '1111'.

*-----Start wskim
*    IF ztfi_reason-type = '3'.
*      u_tot = u_tot * -1.
*    ENDIF.
*-----End
    it_dis-act = u_tot.
    if ztfi_reason-type = '1'.
      it_dis-after = it_dis-org + it_dis-act.
      wa_org_tot = wa_org_tot + it_dis-after.
    else.
      it_dis-after = it_dis-curr + it_dis-act.
    endif.
    wa_curr_tot = wa_curr_tot + u_tot.
    modify it_dis.
  endloop.

  loop at it_sap where year = u_year
                  and  year <> '1111'.
    if sy-subrc = 0.
*---2004/03/31
*-----Start wskim
*      IF ztfi_reason-type = '3'.
*        u_tot = u_tot * -1.
*      ENDIF.
*-----End
      it_sap-act =  u_tot.
      it_sap-after = it_sap-curr - it_sap-io + it_sap-act.
*----------------------------*
      modify it_sap. " INDEX  sy-tabix.
      wa_act_sap_tot = wa_act_sap_tot + it_sap-act.
    endif.
  endloop.
endform.                    " CHECK_CURR_RTN
*&---------------------------------------------------------------------*
*&      Form  MOVE_PI_BUDGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_pi_budget.

  loop at it_sap.
*---pi budget
    read table it_pi_budget with key posid = gt_out-posid
                                     gjahr = it_sap-year.
    if sy-subrc = 0.
      it_sap-curr = it_pi_budget-wtjhr.
      move it_sap-year to it_sap-year_txt.
      modify it_sap.
    else.
      move it_sap-year to it_sap-year_txt.
      modify it_sap.
    endif.
*---2004/03/31 io budget
    read table it_io_budget with key posid = gt_out-posid
                                     gjahr = it_sap-year.
    if sy-subrc = 0.
      it_sap-io = it_io_budget-wtjhr.
      modify it_sap.
    endif.

  endloop.
*---OVER ALL
  read table it_pi_budget with key posid = gt_out-posid
                                   gjahr = '1111'.
  if sy-subrc = 0.
    move '1111' to it_sap-year.
    move 'Overall' to it_sap-year_txt.
    it_sap-curr = it_pi_budget-wtjhr.
  else.
    move '1111' to it_sap-year.
    move 'Overall' to it_sap-year_txt.
  endif.
*--io budget 2004/03/31
  read table it_io_budget with key posid = gt_out-posid
                                   gjahr = '1111'.
  if sy-subrc = 0.
    move '1111' to it_sap-year.
    move 'Overall' to it_sap-year_txt.
    it_sap-io = it_io_budget-wtjhr.
  else.
    move '1111' to it_sap-year.
    move 'Overall' to it_sap-year_txt.
  endif.

  append it_sap.
  clear  it_sap.

  sort it_sap  ascending by year.

endform.                    " MOVE_PI_BUDGET
*&---------------------------------------------------------------------*
*&      Module  TC_1_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_1_modify input.
  modify it_dis
    index tc_1-current_line.
endmodule.                 " TC_1_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELETE_CBO_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_cbo_rtn.
  delete from ztfi_imfm where posid = gt_out-posid
                        and   ayear = gt_out-ayear
                        and   gubun = gt_out-gubun
                        and   seq   = gt_out-seq.
endform.                    " DELETE_CBO_RTN
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_transaction.
  clear : wa_belnr.
*--REASON CODE   2003.11.20
** By Furong on 02/04/14 (
   wa_text1 = gt_out-text.
*  if gt_out-trans <> ' '.
*    if wa_text is initial.
*      concatenate gt_out-seq gt_out-gubun gt_out-reson gt_out-trans
*                  gt_out-text+0(46)
*                into wa_text1.
*    else.
*      concatenate gt_out-seq gt_out-gubun gt_out-reson gt_out-trans
*                  wa_text+0(46)
*                into wa_text1.
*    endif.
*  else.
*    if wa_text is initial.
*      concatenate gt_out-seq gt_out-gubun gt_out-reson gt_out-text+0(46)
*                into wa_text1.
*    else.
*      concatenate gt_out-seq gt_out-gubun gt_out-reson wa_text+0(46)
*                into wa_text1.
*    endif.
*  endif.
* )
*--Next year
  clear wa_trans.
  if gt_out-gubun = '4'.
    if gt_out-trans <> ' '.
      perform bdc_process using gt_out-posid '1'.
      move 'Q'  to wa_trans.
      perform bdc_process using gt_out-trans '1'.
    endif.
  else.
    perform bdc_process using gt_out-posid '1'.
  endif.
*

* update CBO status to 'A'.
  update ztfi_imfm set status = 'A'
                      where posid = gt_out-posid
                        and ayear = gt_out-ayear
                        and gubun = gt_out-gubun
                        and seq   = gt_out-seq.

endform.                    " UPDATE_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_CBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1280   text
*----------------------------------------------------------------------*
*FORM get_data_cbo USING    u_status.
*
*  LOOP AT it_imfm.
*
*    CHECK it_imfm-status = u_status.
*    MOVE-CORRESPONDING it_imfm TO it_imfm_sum.
*
*    CASE it_imfm-gjahr.
*      WHEN wa_year.
*        MOVE it_imfm-tot   TO it_imfm_sum-year.
*        MOVE it_imfm-tot   TO it_imfm_sum-overall.
*        COLLECT it_imfm_sum.
*        CLEAR   it_imfm_sum.
*      WHEN wa_year1.
*        MOVE it_imfm-tot   TO it_imfm_sum-year1.
*        MOVE it_imfm-tot   TO it_imfm_sum-overall.
*        COLLECT it_imfm_sum.
*        CLEAR   it_imfm_sum.
*    ENDCASE.
*    MOVE-CORRESPONDING it_imfm TO it_imfm_sum.
*
*    IF  it_imfm-gjahr < wa_year.
*      MOVE it_imfm-tot   TO it_imfm_sum-before.
*      MOVE it_imfm-tot   TO it_imfm_sum-overall.
*      COLLECT it_imfm_sum.
*      CLEAR   it_imfm_sum.
*    ELSEIF it_imfm-gjahr > wa_year1.
*      MOVE it_imfm-tot   TO it_imfm_sum-after.
*      MOVE it_imfm-tot   TO it_imfm_sum-overall.
*      COLLECT it_imfm_sum.
*      CLEAR   it_imfm_sum.
*    ENDIF.
*
*  ENDLOOP.
*
*ENDFORM.                    " GET_DATA_CBO
*&---------------------------------------------------------------------*
*&      Form  MAKE_CBO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form make_cbo_data  using u_posid
                          u_ayear
                          u_gubun
                          u_seq
                          u_status.
  clear : it_month, it_month[].

  loop at it_imfm where posid = u_posid
                  and   ayear = u_ayear
                  and   gubun = u_gubun
                  and   seq   = u_seq.
    move it_imfm-gjahr  to it_month-year.
    move it_imfm-tot    to it_month-tot.
    append it_month.
    clear  it_month.
  endloop.

endform.                    " MAKE_CBO_DATA
*&---------------------------------------------------------------------*
*&      Module  SCREEN_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module screen_control output.
  loop at screen.
    if gt_out-status = 'F' or gt_out-status = 'A'.
      if screen-name = 'WA_SENDER' or
         screen-name = 'ZTFI_REASON-TYPE' or
         screen-name = 'ZTFI_REASON-RESON' or
         screen-name = 'WA_TEXT'.
        screen-input = 0.
      endif.
    endif.
*    IF wa_sender <> ' '.
*      ztfi_reason-type = '4'.
*    ENDIF.
    modify screen.
  endloop.
endmodule.                 " SCREEN_CONTROL  OUTPUT

*&spwizard: declaration of tablecontrol 'TC_3' itself
controls: tc_3 type tableview using screen 0500.

*&spwizard: output module for tc 'TC_3'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
module tc_3_change_tc_attr output.
  describe table it_sap lines tc_3-lines.
endmodule.                    "tc_3_change_tc_attr OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_3_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module tc_3_modify input.
  modify it_sap
    index tc_3-current_line.
endmodule.                 " TC_3_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_SENDER_BUDGET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_AYEAR  text
*      -->P_GT_OUT_PRNAM  text
*----------------------------------------------------------------------*
form get_sender_budget using    u_posid
                                u_ayear
                                u_prnam.

  clear : it_sender_budget, it_sender_budget[].

  call function 'Z_FFI_GET_PI_BUDGET'
    exporting
      posid = u_posid
      prnam = u_prnam
      gjahr = u_ayear
    tables
      out   = it_sender_budget.

endform.                    " GET_SENDER_BUDGET
*&---------------------------------------------------------------------*
*&      Module  F4_RESON  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module f4_reson input.
  refresh : it_value.
  clear   : it_value.


  select * into corresponding fields of table it_value
  from ztfi_reason
  where type = ztfi_reason-type.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'RESON'
      dynpprog        = 'ZCFII13'
      dynpnr          = '500'
      dynprofield     = 'ZTFI_REASON-RESON'
      window_title    = 'Reason Code'
      value_org       = 'S'
    tables
      value_tab       = it_value
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

endmodule.                 " F4_RESON  INPUT
*&---------------------------------------------------------------------*
*&      Module  RESON_CHANGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module reson_change input.
  refresh : it_value.
  clear   : it_value.

  move '1' to it_value-reson.
  move 'Original' to it_value-descr.
  append it_value.
  move '2' to it_value-reson.
  move 'Supplement' to it_value-descr.
  append it_value.
  move '3' to it_value-reson.
  move 'Return' to it_value-descr.
  append it_value.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'RESON'
      dynpprog        = 'ZCFII13'
      dynpnr          = '500'
      dynprofield     = 'ZTFI_REASON-TYPE'
      window_title    = 'Reason Type'
      value_org       = 'S'
    tables
      value_tab       = it_value
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.

endmodule.                 " RESON_CHANGE  INPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_reason.
  clear wa_d_cnt.
  select count(*) into wa_d_cnt
  from ztfi_reason
  where type = ztfi_reason-type
  and   reson = ztfi_reason-reson.

endform.                    " CHECK_REASON
*&---------------------------------------------------------------------*
*&      Form  get_sender_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_sender_data.
*--SENDER
  if not wa_sender is initial.
    perform get_sender_budget using
                              wa_sender
                              gt_out-ayear
                              gt_out-prnam.
  endif.

  loop at it_sap.
    read table it_sender_budget with key posid = wa_sender
                                         gjahr = it_sap-year.
    if sy-subrc = 0.
      move it_sender_budget-wtjhr to it_sap-sender.
      modify it_sap.
    endif.
  endloop.

endform.                    " get_sender_data
*&---------------------------------------------------------------------*
*&      Form  bdc_amount_process
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_BEFORE  text
*----------------------------------------------------------------------*
form bdc_amount_process using    u_year u_amount.
*----2004/04/15  original amount check
  if gt_out-gubun = '1'.
*    PERFORM get_cbo_approval_original_item USING   gt_out-gubun
*                                                   u_year.
*issue #200501020-002 requested by ycyoon changed by wskim,02/04/05
*-----Start : change logic
**---Get pi budget   supplement + return
*    PERFORM get_sap_budget USING gt_out-posid
*                                 gt_out-ayear
*                                 gt_out-prnam
*                                  u_year.
*get current budget : t-code im33
    perform get_sap_budget_im33 using  gt_out-posid
                                       gt_out-ayear
                                       gt_out-prnam
                                       u_year
                                       wa_sap_sum.
*-----End
    wa_amt = u_amount + wa_sap_sum.
  else.
*change sign
    if gt_out-gubun = '3'.
      wa_amt = u_amount * -1.
    else.
      wa_amt = u_amount.
    endif.

  endif.

  translate  wa_amt using ', '.
  condense   wa_amt no-gaps.

  perform make_bdc_rtn using :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'DROPT-PTIME'     u_year,
                 ' '  'BDC_OKCODE'      '=DROT'.

  perform make_bdc_rtn using :
                 'X'  'SAPLKBPP'        '0320',
                 ' '  'BPDY-WERT1(01)'  wa_amt,
                 ' '  'BDC_OKCODE'      '/00'.

*  wa_amt1 = wa_amt1 + it_month-tot.
endform.                    " bdc_amount_process
*&---------------------------------------------------------------------*
*&      Form  get_io_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_P_GJAHR  text
*      -->P_S_PRNAM_LOW  text
*----------------------------------------------------------------------*
form get_io_budget using    u_posid
                            u_gjahr
                            u_prnam.
  refresh : it_io_budget.
  clear   : it_io_budget.

  call function 'Z_FFI_GET_PI_BUDGET_IO'
    exporting
      posid = u_posid
      prnam = u_prnam
      gjahr = u_gjahr
    tables
      out   = it_io_budget.

* only run if checked...
  check p_credit = 'X'.
  data: l_pi_act like zfi_pi_actual_act occurs 0 with header line,
        l_credit like bsis-dmbtr.

  refresh: l_pi_act.  clear: l_pi_act, l_credit.
  call function 'Z_FFI_GET_PI_ACTUAL_ACT'
    exporting
      posid = u_posid
      prnam = u_prnam
      gjahr = u_gjahr
    tables
      out   = l_pi_act.
* check statistical credit
  loop at l_pi_act where ippos <> space
                     and wrttp = '11'
                     and tot < 0.
    l_credit = l_credit + l_pi_act-tot.

    read table it_io_budget with key posid = u_posid
                                     gjahr = l_pi_act-gjahr.
    if sy-subrc = 0.
      it_io_budget-wtjhr = it_io_budget-wtjhr + l_pi_act-tot.
      modify it_io_budget index sy-tabix.
    endif.
  endloop.

  read table it_io_budget with key posid = u_posid
                                   gjahr = '1111'.
  if sy-subrc = 0.
    it_io_budget-wtjhr = it_io_budget-wtjhr + l_credit.
    modify it_io_budget index sy-tabix.
  endif.

endform.                    " get_io_budget
*&---------------------------------------------------------------------*
*&      Form  get_cbo_approval
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
form get_cbo_approval using    u_posid.

*FIXME

  select * into corresponding fields of table it_imfm_a
  from ztfi_imfm
  where ayear = p_gjahr
    and posid = gt_out-posid
    and gubun  ne 'P'  "exclude plan
    and status ne 'P'. "exclude parked

*  PERFORM fix_sign  tables   it_imfm_a.

endform.                    " get_cbo_approval
*&---------------------------------------------------------------------*
*&      Form  get_cbo_approval_original
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*----------------------------------------------------------------------*
*FORM get_cbo_approval_original USING    u_gubun.
**  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_imfm_a
*  CLEAR  : wa_cbo_sum.
*  SELECT SUM( tot ) INTO wa_cbo_sum
*  FROM ztfi_imfm
*  WHERE posid = gt_out-posid
*  AND   ayear = p_gjahr
**AND   gjahr = u_year
*  AND   seq   <> '0000'
*  AND   gubun = u_gubun
*  AND   status = 'A'.
*ENDFORM.                    " get_cbo_approval_original
*&---------------------------------------------------------------------*
*&      Form  get_cbo_approval_original_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_GUBUN  text
*      -->P_U_YEAR  text
*----------------------------------------------------------------------*
*FORM get_cbo_approval_original_item USING    u_gubun
*                                             u_year.
*  CLEAR  : wa_cbo_sum.
*  SELECT SUM( tot ) INTO wa_cbo_sum
*  FROM ztfi_imfm
*  WHERE posid = gt_out-posid
*  AND   ayear = p_gjahr
*  AND   gjahr = u_year
*  AND   seq   <> '0000'
*  AND   gubun = u_gubun
*  AND   status = 'A'.
*
*ENDFORM.                    " get_cbo_approval_original_item
*&---------------------------------------------------------------------*
*&      Form  get_sap_budget
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_P_GJAHR  text
*      -->P_GT_OUTR_PRNAM  text
*----------------------------------------------------------------------*
form get_sap_budget using    u_posid
                             u_ayear
                             u_prnam
                             u_year.
  refresh : it_pi_budget.
  clear   : it_pi_budget.

  call function 'Z_FFI_GET_PI_BUDGET'
    exporting
      posid = u_posid
      prnam = u_prnam
      gjahr = u_ayear
    tables
      out   = it_pi_budget.

  clear : wa_sap_sum.
  read table it_pi_budget with key posid = u_posid
                                   gjahr = u_year.
  if sy-subrc = 0.
    wa_sap_sum = it_pi_budget-org.
  else.
    wa_sap_sum = 0.
  endif.
endform.                    " get_sap_budget
*&---------------------------------------------------------------------*
*&      Module  SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_listbox output.
  perform set_listbox_type.
  perform set_listbox_reason.
endmodule.                 " SET_LISTBOX  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_listbox_type.
  tables: dd07t.

  clear: list.

  select valpos domvalue_l ddtext
    into (dd07t-valpos,dd07t-domvalue_l,dd07t-ddtext)
    from dd07t
   where domname    = 'Z_D_BUDGET'
     and ddlanguage = sy-langu
     and as4local   = 'A'
     and as4vers    = '0000'
   order by valpos.

    move: dd07t-domvalue_l to value-key.
    concatenate dd07t-domvalue_l dd07t-ddtext into value-text
      separated by space.

    append value to list.
  endselect.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'ZTFI_REASON-TYPE'
      values = list.
endform.                    " SET_LISTBOX_TYPE
*&---------------------------------------------------------------------*
*&      Form  SET_LISTBOX_REASON
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_listbox_reason.
  data: lw_reson like ztfi_reason-reson,
        lw_descr like ztfi_reason-descr.

*  CHECK ZTFI_REASON-RESON IS INITIAL.

  clear: list.

  select reson descr
    into (lw_reson,lw_descr)
    from ztfi_reason
   where type = ztfi_reason-type
   order by reson.

    move: lw_reson to value-key.
    concatenate lw_reson lw_descr into value-text
      separated by space.

    append value to list.
  endselect.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'ZTFI_REASON-RESON'
      values = list.
endform.                    " SET_LISTBOX_REASON
*&---------------------------------------------------------------------*
*&      Form  get_sap_budget_im33
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_POSID  text
*      -->P_GT_OUT_AYEAR  text
*      -->P_GT_OUT_PRNAM  text
*      -->P_U_YEAR  text
*----------------------------------------------------------------------*
form get_sap_budget_im33 using u_posid
                               u_ayear
                               u_prnam
                               u_year
                               la_sap_sum.
  data : bp_objnr like bpja-objnr.

  clear bp_objnr.

  select single * from impr where posid eq u_posid
                       and gjahr eq  p_gjahr.
  if sy-subrc = 0.

    concatenate 'IP' impr-posnr into bp_objnr.

    select  single wtjhr into la_sap_sum from bpja
     where objnr eq bp_objnr
       and trgkz eq 'N'
       and wrttp eq '47'
       and gjahr eq  u_year.

  endif.

endform.                    " get_sap_budget_im33
*&---------------------------------------------------------------------*
*&      Form  get_sap_budget_im33_ov
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_WA_SAP_SUM  text
*----------------------------------------------------------------------*
form get_sap_budget_im33_ov using   u_posid p_gjahr
                                    la_sap_sum.
  data : bp_objnr like bpja-objnr.

  clear bp_objnr.

  select single * from impr where posid eq u_posid
                       and gjahr eq p_gjahr.
  if sy-subrc = 0.

    concatenate 'IP' impr-posnr into bp_objnr.

    select  single wtges into la_sap_sum from bpge
     where objnr eq bp_objnr
       and trgkz eq 'N'
       and wrttp eq '47'.

  endif.

endform.                    " get_sap_budget_im33_ov
*&---------------------------------------------------------------------*
*&      Form  fix_sign
*&---------------------------------------------------------------------*
form fix_sign tables   p_tab structure ztfi_imfm.
*sign for return
*  loop at p_tab where gubun = '3'.
*    p_tab-wtp01 = p_tab-wtp01 * -1.
*    p_tab-wtp02 = p_tab-wtp02 * -1.
*    p_tab-wtp03 = p_tab-wtp03 * -1.
*    p_tab-wtp04 = p_tab-wtp04 * -1.
*    p_tab-wtp05 = p_tab-wtp05 * -1.
*    p_tab-wtp06 = p_tab-wtp06 * -1.
*    p_tab-wtp07 = p_tab-wtp07 * -1.
*    p_tab-wtp08 = p_tab-wtp08 * -1.
*    p_tab-wtp09 = p_tab-wtp09 * -1.
*    p_tab-wtp10 = p_tab-wtp10 * -1.
*    p_tab-wtp11 = p_tab-wtp11 * -1.
*    p_tab-wtp12 = p_tab-wtp12 * -1.
*    p_tab-tot   = p_tab-tot   * -1.
*    modify p_tab index sy-tabix.
*  endloop.

endform.                    " fix_sign
*&---------------------------------------------------------------------*
*&      Form  TRANSFORM_IMFM_TO_SUM
*&---------------------------------------------------------------------*
form transform_imfm_to_sum .

  loop at it_imfm.
    clear it_imfm_sum.
    move-corresponding it_imfm to it_imfm_sum.

    it_imfm_sum-overall = it_imfm-tot.
    case it_imfm-gjahr.
      when wa_before.  it_imfm_sum-before = it_imfm-tot.
      when wa_year.    it_imfm_sum-year   = it_imfm-tot.
      when wa_year1.   it_imfm_sum-year1  = it_imfm-tot.
      when others.     it_imfm_sum-after  = it_imfm-tot.  "year+2 ~
    endcase.

    collect it_imfm_sum.
  endloop.

endform.                    " TRANSFORM_IMFM_TO_SUM
