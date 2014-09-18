*&---------------------------------------------------------------------*
*& Report  ZCFII92                                                     *
*& Author                 :  WSKIM
*& Creation Date          : 10/08/2004
*& Specification By       : YC, YOON
*& Pattern                : Report 1-1
*& Development Request No :
*& Addl documentation     :
*& Description  : AR  Upload
*&
*& Modification Log
*& Date     Developer      Request ID      Description
*&--------------------------------------------------------------------

report zcfii92_old   no standard page heading
                     line-size 250
                     line-count 65
                     message-id zmfi.
*Tables
tables : imak,taif2.
*Internal tables
data: it_out type table of imak with header line.
data : it_variant like bapiappreqvarntmulti occurs 0 with header line.
data : it_variant_to_version like bapiappreqvarntassignmulti occurs 0
                                                 with header line.
data : it_invest_reson like bapiappreqinvreason occurs 0
                                             with header line.
data : it_env_invest   like bapiappreqenvinvest  occurs 0
                                             with header line.
data : it_org_units like bapiappreqorgunit occurs 0
                                             with header line.
data : begin of it_plan_tot occurs 0.
        include structure bapiappreqplantotalmulti.
data : end of it_plan_tot.

data : begin of it_plan_year occurs 0.
        include structure bapiappreqplanyearmulti.
data : end of it_plan_year.

data : it_return like bapiret2 occurs 0 with header line.
data : it_pi_actual like zfi_pi_actual
                      occurs 0 with header line.
data: it_aufk  type table of aufk with header line.
data: it_imzo type table of imzo with header line.
data: it_impr type table of impr with header line.
data : wa_t_cnt type i,
       wa_chk.

data: begin of gt_temp occurs 0,
        aufnr         like  aufk-aufnr,
        ktext         like  aufk-ktext,
        akstl         like  aufk-akstl,
        user0         like  aufk-user0,
        posnr        like  impr-posnr,
        posid        like  impr-posid,
      end of gt_temp.
data: begin of gt_out occurs 0,
        aufnr         like  aufk-aufnr,
        key(2),
        act(20),
        posid        like  impr-posid,
        gjahr(4),
        tot          like  cosp-wtg001,
      end of gt_out.
data : begin of it_down occurs 0,
        gjahr(4),
        tot          like  cosp-wtg001,
       end of it_down.
data : it_actual like zfi_io_actual occurs 0 with header line.
*---Downpayment
data :  wa_d_tot          like  cosp-wtg001,
        wa_d_before       like  cosp-wtg001,
        wa_d_last         like  cosp-wtg001,
        wa_d_year         like  cosp-wtg001,
        wa_d_year1        like  cosp-wtg001,
        wa_d_year2        like  cosp-wtg001,
        wa_d_year3        like  cosp-wtg001,
        wa_d_year4        like  cosp-wtg001,
        wa_d_year5        like  cosp-wtg001,
        wa_d_after        like  cosp-wtg001.
data :  wa_before  like  imzo-gjahr,
        wa_last    like  imzo-gjahr,
        wa_year    like  imzo-gjahr,
        wa_year1   like  imzo-gjahr,
        wa_year2   like  imzo-gjahr,
        wa_year3   like  imzo-gjahr,
        wa_year4   like  imzo-gjahr,
        wa_year5   like  imzo-gjahr,
        wa_year6   like  imzo-gjahr,
        wa_after   like  imzo-gjahr.
*Data
data : wa_master_data	like	bapiappreqmaster,
       wa_user_field       like   bapiapprequser,
       wa_parent  like bapiprogaux-parent,
       wa_co_area like bapi_appreq_id-cntrl_area,
       z_actual(14)    type p,
       z_down(14) type p,
       wk_d_cnt type i,
       b_gjahr like bapiappreqplanyearmulti-fiscal_year,
       p_flag.


parameters : r_1 type c radiobutton group rg1 default 'X',
             r_2 type c radiobutton group rg1 .

selection-screen begin of block b1 with frame.
parameters :
  p_comp  like bapiappreqmaster-rsp_comp_code default 'H201',
  p_gjahr like bapiappreqplanyearmulti-fiscal_year
                                        obligatory
                                        default '2006',
  p_mon(2) type n           obligatory,
  p_verfr  like imavz-versi default 'IM',
  p_verto  like imavz-versi default 'ID'.

selection-screen end of block b1.
select-options : s_posnr   for   imak-posnr,
                 s_ivart   for   imak-ivart.


at selection-screen.
  perform screen_month.

initialization.

load-of-program.

start-of-selection.
  perform get_data.

end-of-selection.
  perform write.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data.
  if p_flag <> 'X'.
    refresh it_out.
    select * into table it_out from imak
      where posnr  in s_posnr  and
            ivart  in s_ivart.

    describe table it_out lines wk_d_cnt.
    if wk_d_cnt > 0.
*check : approval year
      select single * from taif2
             where gjahr eq p_gjahr
*          AND PRART EQ it_out-posnr
               and versi eq p_verfr.

      if sy-subrc <> 0.
        refresh it_out.
        exit.
      endif.
      loop at it_out.
        perform call_bapi_ar_detail using it_out-posid.

      endloop.
    endif.
  endif.
endform.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  call_bapi_ar_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_POSID  text
*----------------------------------------------------------------------*
form call_bapi_ar_detail using    u_posid.
  refresh : it_variant,
            it_plan_tot,
            it_plan_year,
            it_invest_reson,
            it_org_units,
            it_invest_reson,
            it_variant_to_version,
            it_env_invest.

  clear : wa_master_data, wa_user_field, wa_co_area.

  call function 'BAPI_APPREQUEST_GETDETAIL'
    exporting
      externalnumber              = u_posid
      language                    = sy-langu
*   LANGUAGE_ISO                   =
    importing
      master_data                 = wa_master_data
      user_fields                 = wa_user_field
      controlling_area            = wa_co_area
    tables
      org_units                   = it_org_units
      invest_reason               = it_invest_reson
      environmnt_invest           = it_env_invest
      variant                     = it_variant
      variant_to_version          = it_variant_to_version
      plan_total                  = it_plan_tot
      plan_year                   = it_plan_year.
*    return                        = it_return
*      .

* create ID version with same amount with source version
  read table it_variant_to_version with key appr_year    = p_gjahr
                                            plan_version = p_verfr.
  read table it_plan_tot with key
                      appreqvrnt = it_variant_to_version-appreqvrnt.

  perform consistency using u_posid
                      it_variant_to_version-appreqvrnt
                      it_plan_tot-investment_costs.


* adjust ID version : AR - IO actual - downpayment
  if r_1 eq 'X'.
    read table it_variant_to_version with key appr_year    = p_gjahr
                                              plan_version = p_verfr.

    perform downpayment using u_posid.

    perform update_bapi using u_posid
                              it_variant_to_version-appreqvrnt
                              it_plan_tot-investment_costs.
  endif.

*                                it_plan_year-investment_costs
*                                z_actual.
*  elseif r_2 eq 'X'.
*    read table it_variant_to_version with key appr_year = p_gjahr
*                                          plan_version  = p_verfr.
*    read table it_plan_tot with key
*                          appreqvrnt = it_variant_to_version-appreqvrnt
*.
*
*    perform consistency using u_posid
*                              it_variant_to_version-appreqvrnt
*                              it_plan_tot-investment_costs.
*
*  endif.

endform.                    " call_bapi_ar_detail
*&---------------------------------------------------------------------*
*&      Form  update_bapi
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_PLAN_TOT_INVESTMENT_COSTS  text
*      -->P_IT_PLAN_YEAR_INVESTMENT_COSTS  text
*----------------------------------------------------------------------*
form update_bapi using    p_posid
                          p_variant
                          p_tot_investment_costs.
*                          p_year_investment_costs
*                          p_actual.

  data : it_plan like bapiappreqplanyear occurs 0 with header line.
  data : plan_total like bapiappreqplantotal,
         p_actual(14) type p,
         z_tot like it_down-tot.
  refresh : it_plan.
  clear :p_actual,z_tot.

  perform actual_data using p_posid.
*                       CHANGING z_actual.
*Overall
  loop at it_pi_actual where  posid = p_posid
                         and ippos = ' '
                         and wrttp = 'I'.   "Invoice
    p_actual    = p_actual + it_pi_actual-wtg001
               + it_pi_actual-wtg002
               + it_pi_actual-wtg003 + it_pi_actual-wtg004
               + it_pi_actual-wtg005 + it_pi_actual-wtg006
               + it_pi_actual-wtg007 + it_pi_actual-wtg008
               + it_pi_actual-wtg009 + it_pi_actual-wtg010
               + it_pi_actual-wtg011 + it_pi_actual-wtg012.

  endloop.
  loop at it_down ."WHERE  gjahr = it_plan_year-fiscal_year.
    add it_down-tot to z_tot.
  endloop.
  plan_total-investment_costs = p_tot_investment_costs
                    -  p_actual - z_tot.

*year
  data : f_gjahr like it_plan_year-fiscal_year .
  f_gjahr = p_gjahr.
  do 5 times.

    read table it_plan_year with key appreqvrnt =
                                       it_variant_to_version-appreqvrnt
                                       fiscal_year = f_gjahr.
    if  sy-subrc <> 0.
      it_plan_year-appreqvrnt = it_variant_to_version-appreqvrnt.
      it_plan_year-fiscal_year = f_gjahr.
      it_plan_year-investment_costs = 0.
      append it_plan_year.
    endif.
    f_gjahr = f_gjahr - 1.
  enddo.

  loop at it_plan_year  where appreqvrnt =
                                       it_variant_to_version-appreqvrnt.
    loop at it_pi_actual where  posid = p_posid
                           and  gjahr = it_plan_year-fiscal_year
                           and ippos = ' '
                           and wrttp = 'I'.
      z_actual    = z_actual + it_pi_actual-wtg001
                 + it_pi_actual-wtg002
                 + it_pi_actual-wtg003 + it_pi_actual-wtg004
                 + it_pi_actual-wtg005 + it_pi_actual-wtg006
                 + it_pi_actual-wtg007 + it_pi_actual-wtg008
                 + it_pi_actual-wtg009 + it_pi_actual-wtg010
                 + it_pi_actual-wtg011 + it_pi_actual-wtg012.
    endloop.

    it_plan-fiscal_year = it_plan_year-fiscal_year.
    clear z_tot.
    sort it_down by gjahr.
    loop at it_down where  gjahr = it_plan_year-fiscal_year.
      add it_down-tot to z_tot.
    endloop.
    it_plan-investment_costs = it_plan_year-investment_costs
                      -  z_actual - z_tot  .

    append it_plan. clear : z_actual,it_plan_year,it_pi_actual.
  endloop.

  read table it_variant_to_version with key appr_year = p_gjahr
                                            plan_version = p_verto.

  call function 'BAPI_APPREQUEST_SETPLANVALUES'
    exporting
      externalnumber                    =  p_posid
      appropriationrequestvariant       =
                             it_variant_to_version-appreqvrnt
      plan_total                        =  plan_total
*   TEST_RUN                          = ' '
   tables
     plan_year                         = it_plan
     return                            = it_return
            .
  read table it_return with key type = 'E'.
  if sy-subrc <> 0.
    it_return-type = 'S'.
    it_return-message = 'Success'.
    it_return-message_v1 = p_posid.
    append it_return.
    call function 'BAPI_TRANSACTION_COMMIT'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    exit.
  endif.
endform.                    " update_bapi
*&---------------------------------------------------------------------*
*&      Form  write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write.
  write : / 'Results of AR Update'.
  if p_flag <> 'X'.
    loop at it_return.
      write : / it_return-type,it_return-number,(50) it_return-message,
                it_return-message_v1.
    endloop.
  else.
    write : / 'Check input data month'.
  endif.
endform.                    " write
*&---------------------------------------------------------------------*
*&      Form  actual_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*----------------------------------------------------------------------*
form actual_data using  u_posid.
*                 CHANGING z_actual.

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
  clear z_actual.
endform.                    " actual_data
*&---------------------------------------------------------------------*
*&      Form  downpayment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      <--P_Z_DOWN  text
*----------------------------------------------------------------------*
form downpayment using  u_posid.
  refresh : it_aufk,it_imzo,it_impr,gt_temp.
  select * into table it_aufk from aufk
    where  autyp eq '01'.
*     AND   user0  IN s_usr02.
*--GET IMZO
  clear wa_t_cnt.
  describe table it_aufk lines wa_t_cnt.
  if wa_t_cnt > 0.
    select * into table it_imzo from imzo
    for all entries in it_aufk
    where objnr = it_aufk-objnr.
  endif.
*---GET IMPR
  clear wa_t_cnt.
  describe table it_imzo lines wa_t_cnt.
  if wa_t_cnt > 0.
    select * into table it_impr from impr
    for all entries in it_imzo
    where posnr eq it_imzo-posnr
    and   posid eq u_posid
    and   gjahr = it_imzo-gjahr.
  endif.
*====*
*    CLEAR gt_out.
  b_gjahr =  p_gjahr - 1.
  loop at it_aufk.
    move-corresponding it_aufk to gt_temp.
    read table it_imzo with key objnr = it_aufk-objnr
                                gjahr =  b_gjahr .
    if sy-subrc = 0.
      move it_imzo-posnr to gt_temp-posnr.
      read table it_impr with key posnr = it_imzo-posnr
                                  gjahr = it_imzo-gjahr.
      if sy-subrc = 0.
        move it_impr-posid to gt_temp-posid.
        append gt_temp.
      endif.
    endif.
    clear  gt_temp.
  endloop.
*---make gt_out by activity
  refresh gt_out.
  loop at gt_temp.
    perform get_activity    using
                           gt_temp-aufnr
                           gt_temp-akstl
                           gt_temp-posid
                           gt_temp-user0.
  endloop.

  sort gt_out by aufnr key ascending.
  clear wa_chk.
  refresh :it_down,it_actual.

  loop at gt_out.
*----get i/o budget
    at new aufnr.
      perform get_io_actual using gt_out-aufnr.
    endat.
*---actual i/o
    if gt_out-key > 06.
      perform make_io_actual using gt_out-aufnr gt_out-key.
      clear wa_chk.
    endif.
  endloop.

endform.                    " downpayment
*&---------------------------------------------------------------------*
*&      Form  consistency
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_U_POSID  text
*      -->P_IT_VARIANT_TO_VERSION_APPREQVR  text
*----------------------------------------------------------------------*
form consistency using    u_posid
                          p_variant
                          p_tot_investment_costs.
  data : it_plan like bapiappreqplanyear occurs 0 with header line.
  data : plan_total like bapiappreqplantotal,
         p_actual(14) type p.
  refresh : it_plan.
  clear p_actual.

  plan_total-investment_costs = p_tot_investment_costs.
*year
  data : f_gjahr like it_plan_year-fiscal_year .
  f_gjahr = p_gjahr.
  do 5 times.
    read table it_plan_year with key appreqvrnt =
                                       it_variant_to_version-appreqvrnt
                                       fiscal_year = f_gjahr.
    if  sy-subrc <> 0.
      it_plan_year-appreqvrnt = it_variant_to_version-appreqvrnt.
      it_plan_year-fiscal_year = f_gjahr.
      it_plan_year-investment_costs = 0.
      append it_plan_year.
    endif.
    f_gjahr = f_gjahr - 1.
  enddo.

  loop at it_plan_year
      where appreqvrnt = it_variant_to_version-appreqvrnt.
    it_plan-fiscal_year = it_plan_year-fiscal_year.
    it_plan-investment_costs = it_plan_year-investment_costs.

    append it_plan. clear : it_plan_year,it_pi_actual.
  endloop.

  read table it_variant_to_version with key appr_year    = p_gjahr
                                            plan_version = p_verto.

  call function 'BAPI_APPREQUEST_SETPLANVALUES'
    exporting
      externalnumber                    =  u_posid
      appropriationrequestvariant       =
                                  it_variant_to_version-appreqvrnt
      plan_total                        =  plan_total
*   TEST_RUN                          = ' '
   tables
     plan_year                         = it_plan
     return                            = it_return
            .
  read table it_return with key type = 'E'.
  if sy-subrc <> 0.
    it_return-type = 'S'.
    it_return-message = 'Success'.
    it_return-message_v1 = u_posid.
    append it_return.
    call function 'BAPI_TRANSACTION_COMMIT'
         exporting
              wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
    exit.
  endif.


endform.                    " consistency
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
  move u_aufnr    to   gt_out-aufnr.
  move '09'           to gt_out-key.
  move 'Downpayment'  to gt_out-act.
  move u_posid        to gt_out-posid.
  append gt_out.
  clear  gt_out.

endform.                    " get_activity
*&---------------------------------------------------------------------*
*&      Form  get_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*----------------------------------------------------------------------*
form get_io_actual  using    u_aufnr.
  call function 'Z_FFI_GET_IO_ACTUAL'
    exporting
      aufnr         = u_aufnr
* IMPORTING
*   AMT           =
    tables
      out           = it_actual
* EXCEPTIONS
*   NO_DATA       = 1
*   OTHERS        = 2
            .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " get_io_actual
*&---------------------------------------------------------------------*
*&      Form  make_io_actual
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_OUT_AUFNR  text
*      -->P_GT_OUT_KEY  text
*----------------------------------------------------------------------*
form make_io_actual using   u_aufnr
                            u_key.
  data : m_num(3) type n,
         c_num(3) type n,
         field(16),
         d_sum like it_actual-tot .
  clear :m_num,c_num,d_sum.
  field-symbols : <fs> type any.
  loop at it_actual.
    case u_key.
      when '09'.       "Downpayment
        case it_actual-wrttp.
          when '12'.
            if it_actual-gjahr <> sy-datum(4).
              move it_actual-gjahr to it_down-gjahr.
              move it_actual-tot   to it_down-tot.
              append it_down.
            else.
              m_num = 1.
              do p_mon times.
                concatenate 'IT_ACTUAL-' 'WTG' m_num into field.
                assign (field) to <fs>.
                d_sum = d_sum + <fs>.
                m_num = m_num + 1.
              enddo.
              move it_actual-gjahr to it_down-gjahr.
              move d_sum   to it_down-tot.
              append it_down. clear: d_sum,m_num.
            endif.
        endcase.
    endcase.
  endloop.

endform.                    " make_io_actual
*&---------------------------------------------------------------------*
*&      Form  screen_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form screen_month.
  clear p_flag.
  check p_mon <> 0.
  if p_mon < 1 or p_mon > 12.
    message i011 with 'Check input data Month'.
    p_flag = 'X'.
  endif.

endform.                    " screen_month
