function z_hr_ess_get_pay_slip_data.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"     VALUE(PDATE) LIKE  T549S-PDATE
*"  TABLES
*"      RHEADER STRUCTURE  ZESS_SLIP_HEADER1
*"      RTAXSTATUS STRUCTURE  ZESS_SLIP_TAXSTATUS
*"      RADRS STRUCTURE  ADRS
*"      PAY_SLIP_SUMMARY STRUCTURE  ZESS_PAY_SLIP_SUMMARY
*"      PAY_SLIP_EARNING STRUCTURE  ZESS_PAY_SLIP_EARNING
*"      PAY_SLIP_EARNING_SUM STRUCTURE  ZESS_PAY_SLIP_EARNING_SUM
*"      PAY_SLIP_EARNING_TOTAL STRUCTURE  ZESS_PAY_SLIP_EARNING_TOTAL
*"      PAY_SLIP_TAXES STRUCTURE  ZESS_PAY_SLIP_TAXES
*"      PAY_SLIP_PRE_TAX STRUCTURE  ZESS_PAY_SLIP_PRE_TAX
*"      PAY_SLIP_POST_TAX STRUCTURE  ZESS_PAY_SLIP_POST_TAX
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
* Modification Logs
* Date       Developer  Request ID  Description
*-----------------------------------------------------------------------
* 03/16/2012 Valerian   UD1K954297  Get Variable Assignment Number
*                                   Summarize Pay Slip Earning Statemnt
* 04/03/2012 Valerian   UD1K954409  Adjust Position of YTD Amount
* 08/15/2012 Valerian   UD1K955403  Add summarized version of Earnings
* 02/18/2013 Valerian   UD1K956779  Include Off-Cycle Date in Date
*                                   selection
*-----------------------------------------------------------------------
  data : begin of ipay_slip_earning occurs 0,
          lgart type lgart.
          include structure  zess_pay_slip_earning.
  data : end of ipay_slip_earning.

* BEGIN OF UD1K954297
  data : begin of ipay_slip_earning_sum occurs 0,
          sumcd(6) type c.
          include structure  zess_pay_slip_earning_sum.
  data : end of ipay_slip_earning_sum.

  data : v_tot type abrst.
* END OF UD1K954297

  data : begin of ipay_slip_taxes occurs 0,
          lgart type lgart.
          include structure  zess_pay_slip_taxes.
  data : end of ipay_slip_taxes.

  data : begin of ipay_slip_pre_tax occurs 0,
          lgart type lgart.
          include structure  zess_pay_slip_pre_tax.
  data : end of ipay_slip_pre_tax.

  data : begin of ipay_slip_post_tax occurs 0,
          lgart type lgart.
          include structure  zess_pay_slip_post_tax.
  data : end of ipay_slip_post_tax.


  data abkrs like pa0001-abkrs.
  data payslip_variant like  bapi7004-payslip_variant.

  data curr_tax like ipay_slip_taxes-curr_amt.
  data ytd_tax like ipay_slip_taxes-curr_amt.


  payslip_variant = 'ZHMA'.

  select single * from pa0001 where pernr eq employee_number
                                and endda eq '99991231'.
  if sy-subrc ne 0.
    return-type = 'E'.
    return-message = 'Invalid Employee Number'.
    append return.
    exit.
  endif.
  abkrs = pa0001-abkrs.
  select single * from t549a where abkrs eq pa0001-abkrs.

* BEGIN OF UD1K956779
  data: l_frstday type sy-datum,
        lt_result like bapi7004_rl occurs 0 with header line.

  select single * from t549s where molga eq '10'
                               and datmo eq '01'
                               and permo eq t549a-permo
                               and datid eq '01'
                               and pdate eq pdate.

  if sy-subrc <> 0.
    concatenate pdate(6) '01' into l_frstday.

    call function 'BAPI_GET_PAYROLL_RESULT_LIST'
      exporting
        employeenumber = employee_number
        fromdate       = l_frstday
        todate         = pdate
      tables
        results        = lt_result.

    t549s-pabrj = pdate(4).
    read table lt_result with key paydate = pdate.
  endif.
* END OF UD1K956779

  if sy-subrc ne 0.
    return-type = 'E'.
    return-message = 'Invalid Pay Date'.
    append return.
    exit.
  endif.

  w_abkrs = abkrs.
  w_pernr = employee_number.
  w_pabrj = t549s-pabrj.
  w_pabrp = t549s-pabrp.

  perform get_payroll_period.
  perform check_payroll_area.
  perform get_sequential_number using pdate.
  check w_subrc = 0.

  perform get_result_table using pdate.

  perform fill_adrs using employee_number adrs payslip_variant.
  radrs = adrs.
  append radrs.

  __cls rheader.

  select single * from t500p where persa eq '1010'.

  header_1-comp_addr1 = 'Hyundai Motor Manufacturing Alabama'.
  header_1-comp_addr2 = t500p-stras.
  concatenate t500p-ort01 t500p-pstlz into header_1-comp_addr3 separated
                                                                by space.


  perform get_check_no using w_pernr
                       changing  xbt-chkno.

  header_1-check_no = xbt-chkno(14).

  write : pdate to header_1-check_date,
          w_begda to header_1-pay_per_from,
          w_endda to  header_1-pay_per_to.
  header_1-pernr = employee_number.
  append header_1 to rheader.

  __cls rtaxstatus.

  call function 'HR_READ_INFOTYPE'
    exporting
      pernr     = employee_number
      infty     = '0207'
    tables
      infty_tab = p0207.

  call function 'HR_READ_INFOTYPE'
    exporting
      pernr     = employee_number
      infty     = '0208'
    tables
      infty_tab = p0208.

  loop at tax.

    select single * from t5utk
      where taxau = tax-taxau
        and txsta = tax-txsta
        and endda >= w_endda
        and begda <= w_endda.

*    CONCATENATE TAX-TAXAU T5UTK-STEXT
*        INTO TAXSTATUS-TAX_STATUS SEPARATED BY SPACE.
*    CONCATENATE TAX-TAXAU ''
*        INTO TAXSTATUS-TAX_STATUS SEPARATED BY SPACE.

    taxstatus-tax_status = tax-taxau.
    taxstatus-tax_marital_text = t5utk-stext.


    taxstatus-tax_exempt = tax-nbrex.

    concatenate 'Res:' p0207-taxar into taxstatus-tax_res
    separated by space.
    concatenate 'Wrk:' p0208-wtart into taxstatus-tax_work_area
    separated by space.
    append taxstatus to rtaxstatus.
  endloop.

  data : begin of i_wage_grp occurs 0,
          grp(2),
          lgart type lgart,
         end of i_wage_grp.

  select * from t512w where molga eq '10'
                       and endda eq '99991231'.
    check t512w-aklas+2(2) ne space.

    i_wage_grp-grp = t512w-aklas+2(2).
    i_wage_grp-lgart = t512w-lgart.
    append i_wage_grp.
  endselect.
  sort i_wage_grp by lgart.

  __cls pay_slip_summary.

  sort : rt by lgart.
  sort : crt by lgart.

  loop at rt.

    if rt-lgart eq '0318' or rt-lgart eq '/551' or rt-lgart eq '/552'.
      pay_slip_summary-gross_amt =
           pay_slip_summary-gross_amt + rt-betrg.
    endif.

    if rt-lgart eq '1101' or rt-lgart eq '7003'.
      pay_slip_summary-non_tax =
           pay_slip_summary-non_tax + rt-betrg.
    endif.

    if rt-lgart eq '/559'.
      pay_slip_summary-net_pay =
           pay_slip_summary-net_pay + rt-betrg.
    endif.

    read table i_wage_grp with key lgart = rt-lgart binary search.
    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '01'.
        if rt-lgart eq '0318' or rt-lgart eq '/551'
        or rt-lgart eq '/552'.
        else.
          pay_slip_summary-gross_amt =
               pay_slip_summary-gross_amt + rt-betrg.
        endif.

      endif.

      if  i_wage_grp-grp eq '06'.
        pay_slip_summary-pre_tax =
             pay_slip_summary-pre_tax + rt-betrg.
      endif.

      if  i_wage_grp-grp eq '08' or i_wage_grp-grp eq '11'.
        pay_slip_summary-post_tax =
             pay_slip_summary-post_tax + rt-betrg.
      endif.

      if  i_wage_grp-grp eq '02'.
        pay_slip_summary-taxes =
             pay_slip_summary-taxes + rt-betrg.
      endif.
    endif.
  endloop.

  pay_slip_summary-taxable =
*      pay_slip_summary-pre_tax + pay_slip_summary-non_tax +
      pay_slip_summary-pre_tax - pay_slip_summary-non_tax +
      pay_slip_summary-gross_amt.

  pay_slip_summary-post_tax = pay_slip_summary-post_tax * -1.
  pay_slip_summary-summary_type = 'CURR.'.
  append pay_slip_summary. clear pay_slip_summary.

  loop at crt.
    check crt-cumyr eq w_pabrj.

    if crt-lgart eq '0318'.
      pay_slip_summary-gross_amt =
           pay_slip_summary-gross_amt + crt-betrg.
    endif.

    if crt-lgart eq '1101' or crt-lgart eq '7003'.
      pay_slip_summary-non_tax =
           pay_slip_summary-non_tax + crt-betrg.
    endif.

    read table i_wage_grp with key lgart = crt-lgart binary search.
    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '01'.
        if crt-lgart eq '0318'.
        else.
          pay_slip_summary-gross_amt =
               pay_slip_summary-gross_amt + crt-betrg.
        endif.
      endif.

      if  i_wage_grp-grp eq '06'.
        pay_slip_summary-pre_tax =
             pay_slip_summary-pre_tax + crt-betrg.
      endif.

      if  i_wage_grp-grp eq '08' or i_wage_grp-grp eq '11'.
        pay_slip_summary-post_tax =
             pay_slip_summary-post_tax + crt-betrg.
      endif.

      if  i_wage_grp-grp eq '02'.
        pay_slip_summary-taxes =
             pay_slip_summary-taxes + crt-betrg.
      endif.
    endif.

  endloop.

  pay_slip_summary-taxable =
*      pay_slip_summary-pre_tax + pay_slip_summary-non_tax +
      pay_slip_summary-pre_tax - pay_slip_summary-non_tax +
      pay_slip_summary-gross_amt.

  pay_slip_summary-post_tax = pay_slip_summary-post_tax * -1.
  pay_slip_summary-summary_type = 'YTD'.
  append pay_slip_summary.

  __cls ipay_slip_earning.

  sort it_crtxt by lgart.

  loop at rt.
    clear ipay_slip_earning.
    read table i_wage_grp with key lgart = rt-lgart binary search.
    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '01'.
        ipay_slip_earning-curr_amt = rt-betrg.
        read table it_crtxt with key lgart = rt-lgart binary search.
        if sy-subrc eq 0.
          ipay_slip_earning-earning_title = it_crtxt-kztxt.
        endif.
        ipay_slip_earning-lgart = rt-lgart.
        ipay_slip_earning-rate = rt-betpe.
        ipay_slip_earning-hours = rt-anzhl.
        ipay_slip_earning-var_num = rt-v0znr.               "UD1K954297
        append ipay_slip_earning.
        clear ipay_slip_earning.
      endif.
    endif.

* BEGIN OF UD1K954297
    clear ipay_slip_earning_sum.

    case rt-lgart.
      when '0200'.
        ipay_slip_earning_sum-sumcd = '1'.         "Reg Hrs
        ipay_slip_earning_sum-earning_title = 'REG'.
        ipay_slip_earning_sum-hours = rt-anzhl.
        collect ipay_slip_earning_sum.

      when '0303' or '0310'.
        ipay_slip_earning_sum-sumcd = '2'.         "Vac/Pers
        ipay_slip_earning_sum-earning_title = 'VAC'.
        ipay_slip_earning_sum-hours = rt-anzhl.
        collect ipay_slip_earning_sum.

      when '0312'.
        ipay_slip_earning_sum-sumcd = '3'.         "Holiday
        ipay_slip_earning_sum-earning_title = 'HOL'.
        ipay_slip_earning_sum-hours = rt-anzhl.
        collect ipay_slip_earning_sum.

      when '0300' or '0304' or '0311' or '0319' or
           '0603' or '0604' or '0605' or '1072'.
        ipay_slip_earning_sum-sumcd = '4'.         "Other
        ipay_slip_earning_sum-earning_title = 'OTH'.
        ipay_slip_earning_sum-hours = rt-anzhl.
        collect ipay_slip_earning_sum.

      when '0901' or '0902'.
        ipay_slip_earning_sum-sumcd = '6'.         "Total OT
        ipay_slip_earning_sum-earning_title = 'TOT_OT'.
        ipay_slip_earning_sum-hours = rt-anzhl.
        collect ipay_slip_earning_sum.

      when '0100' or '0101' or '0102'.
        ipay_slip_earning_sum-sumcd = '7'.         "Total SP
        ipay_slip_earning_sum-earning_title = 'TOT_SP'.
        ipay_slip_earning_sum-hours = rt-anzhl.
        collect ipay_slip_earning_sum.
    endcase.
* END OF UD1K954297

    if  rt-lgart eq '/551'.
      ipay_slip_earning-curr_amt = rt-betrg.
      read table it_crtxt with key lgart = rt-lgart binary search.
      if sy-subrc eq 0.
        ipay_slip_earning-earning_title = it_crtxt-kztxt.
      endif.
      ipay_slip_earning-lgart = rt-lgart.
      ipay_slip_earning-rate = rt-betpe.
      ipay_slip_earning-hours = rt-anzhl.
      append ipay_slip_earning.
      clear ipay_slip_earning.
    endif.

    if  rt-lgart eq '/552'.
      ipay_slip_earning-curr_amt = rt-betrg.
      read table it_crtxt with key lgart = rt-lgart binary search.
      if sy-subrc eq 0.
        ipay_slip_earning-earning_title = it_crtxt-kztxt.
      endif.
      ipay_slip_earning-lgart = rt-lgart.
      ipay_slip_earning-rate = rt-betpe.
      ipay_slip_earning-hours = rt-anzhl.
      append ipay_slip_earning.
      clear ipay_slip_earning.
    endif.

    if  rt-lgart eq '0603'.
      ipay_slip_earning-curr_amt = rt-betrg.
      read table it_crtxt with key lgart = rt-lgart binary search.
      if sy-subrc eq 0.
        ipay_slip_earning-earning_title = it_crtxt-kztxt.
      endif.
      ipay_slip_earning-lgart = rt-lgart.
      ipay_slip_earning-rate = rt-betpe.
      ipay_slip_earning-hours = rt-anzhl.
      append ipay_slip_earning.
      clear ipay_slip_earning.
    endif.

    if  rt-lgart eq '0605'.
      ipay_slip_earning-curr_amt = rt-betrg.
      read table it_crtxt with key lgart = rt-lgart binary search.
      if sy-subrc eq 0.
        ipay_slip_earning-earning_title = it_crtxt-kztxt.
      endif.
      ipay_slip_earning-lgart = rt-lgart.
      ipay_slip_earning-rate = rt-betpe.
      ipay_slip_earning-hours = rt-anzhl.
      append ipay_slip_earning.
      clear ipay_slip_earning.
    endif.

  endloop.

  sort ipay_slip_earning by lgart var_num.                  "UD1K954409
  loop at crt.
    check crt-cumyr eq w_pabrj.

    read table i_wage_grp with key lgart = crt-lgart binary search.

    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '01'.

        read table it_crtxt with key lgart = crt-lgart binary search.
        read table ipay_slip_earning
                with key earning_title = it_crtxt-kztxt.
        if sy-subrc eq 0.
          $ix = sy-tabix.
          ipay_slip_earning-ytd_amt = crt-betrg.
          modify ipay_slip_earning index $ix transporting ytd_amt.
          clear ipay_slip_earning.
        else.
          read table it_crtxt with key lgart = crt-lgart binary search.
          if sy-subrc eq 0.
            ipay_slip_earning-earning_title = it_crtxt-kztxt.
          endif.
          ipay_slip_earning-lgart = crt-lgart.
          ipay_slip_earning-ytd_amt = crt-betrg.
          append ipay_slip_earning.
          clear ipay_slip_earning.
        endif.
      endif.
    endif.

  endloop.
  data $hours like ipay_slip_earning-hours.
  loop at ipay_slip_earning.
    $hours = ipay_slip_earning-hours + $hours.
  endloop.

  ipay_slip_earning-lgart = 'Z'.
  ipay_slip_earning-earning_title = 'Total'.
  clear ipay_slip_earning-rate.
  ipay_slip_earning-hours = $hours.
  read table pay_slip_summary with key summary_type = 'CURR.'.
  ipay_slip_earning-curr_amt = pay_slip_summary-gross_amt.
  read table pay_slip_summary with key summary_type = 'YTD'.
  ipay_slip_earning-ytd_amt = pay_slip_summary-gross_amt.
  append ipay_slip_earning.

  __cls ipay_slip_taxes.

  loop at rt.

    if rt-lgart eq '/401' or rt-lgart eq '/403'
       or rt-lgart eq '/405'.
      if rt-cntr1 eq '01'.
        read table it_crtxt with key lgart = rt-lgart binary search.
        if sy-subrc eq 0.
          ipay_slip_taxes-tax_title = it_crtxt-kztxt.
        endif.

        ipay_slip_taxes-lgart = rt-lgart.
        ipay_slip_taxes-tax_type = 'FED'.
        ipay_slip_taxes-curr_amt = rt-betrg.
        append ipay_slip_taxes.
      endif.

      if rt-cntr1 eq '02'.
        read table it_crtxt with key lgart = rt-lgart binary search.
        if sy-subrc eq 0.
          ipay_slip_taxes-tax_title = it_crtxt-kztxt.
        endif.

        ipay_slip_taxes-lgart = rt-lgart.
        ipay_slip_taxes-tax_type = 'AL'.
        ipay_slip_taxes-curr_amt = rt-betrg.
        append ipay_slip_taxes.
      endif.

    endif.

  endloop.

  loop at tcrt.
    check tcrt-cumyr eq w_pabrj.

    clear ipay_slip_taxes.

    check tcrt-ctype eq 'Y'.
    if tcrt-lgart eq '/401' or tcrt-lgart eq '/403'
       or tcrt-lgart eq '/405'.
      read table it_crtxt with key lgart = tcrt-lgart binary search.

      if tcrt-taxau  eq 'FED'.
        read table ipay_slip_taxes with key tax_title = it_crtxt-kztxt
                                            tax_type  = tcrt-taxau.
        if sy-subrc eq 0.
          $ix = sy-tabix.
          ipay_slip_taxes-ytd_amt = tcrt-betrg.
          modify ipay_slip_taxes index $ix transporting ytd_amt.
        else.
          ipay_slip_taxes-lgart = tcrt-lgart.
          ipay_slip_taxes-tax_title = it_crtxt-kztxt.
          ipay_slip_taxes-tax_type = 'FED'.
          ipay_slip_taxes-ytd_amt = tcrt-betrg.
          append ipay_slip_taxes.
        endif.
      endif.

      if tcrt-taxau  eq 'AL'.
        read table ipay_slip_taxes with key tax_title = it_crtxt-kztxt
                                            tax_type  = tcrt-taxau.
        if sy-subrc eq 0.
          $ix = sy-tabix.
          ipay_slip_taxes-ytd_amt = tcrt-betrg.
          modify ipay_slip_taxes index $ix transporting ytd_amt.
        else.
          ipay_slip_taxes-lgart = tcrt-lgart.
          ipay_slip_taxes-tax_title = it_crtxt-kztxt.
          ipay_slip_taxes-tax_type = 'AL'.
          ipay_slip_taxes-ytd_amt = tcrt-betrg.
          append ipay_slip_taxes.
        endif.
      endif.
    endif.
  endloop.

  __cls ipay_slip_pre_tax.

  loop at rt.
    clear ipay_slip_pre_tax.
    read table i_wage_grp with key lgart = rt-lgart binary search.
    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '06'.
        ipay_slip_pre_tax-curr_amt = rt-betrg.
        read table it_crtxt with key lgart = rt-lgart binary search.
        if sy-subrc eq 0.
          ipay_slip_pre_tax-pre_tax_title = it_crtxt-kztxt.
        endif.
        ipay_slip_pre_tax-lgart = rt-lgart.
        append ipay_slip_pre_tax.
      endif.
    endif.

  endloop.

  loop at crt.
    check crt-cumyr eq w_pabrj.

    read table i_wage_grp with key lgart = crt-lgart binary search.

    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '06'.

        read table it_crtxt with key lgart = crt-lgart binary search.

        read table ipay_slip_pre_tax
                with key pre_tax_title = it_crtxt-kztxt.
        if sy-subrc eq 0.
          $ix = sy-tabix.
          ipay_slip_pre_tax-ytd_amt = crt-betrg.
          modify ipay_slip_pre_tax index $ix transporting ytd_amt.
          clear ipay_slip_pre_tax.
        else.
          read table it_crtxt with key lgart = crt-lgart binary search.
          if sy-subrc eq 0.
            ipay_slip_pre_tax-pre_tax_title = it_crtxt-kztxt.
          endif.
          ipay_slip_pre_tax-lgart = crt-lgart.
          ipay_slip_pre_tax-ytd_amt = crt-betrg.
          append ipay_slip_pre_tax.
          clear ipay_slip_pre_tax.
        endif.
      endif.
    endif.

  endloop.

  __cls ipay_slip_post_tax.

  loop at rt.
    clear ipay_slip_post_tax.
    read table i_wage_grp with key lgart = rt-lgart binary search.
    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '08' or i_wage_grp-grp eq '11'.
        ipay_slip_post_tax-curr_amt = rt-betrg.
        read table it_crtxt with key lgart = rt-lgart binary search.
        if sy-subrc eq 0.
          ipay_slip_post_tax-post_tax_title = it_crtxt-kztxt.
        endif.
        ipay_slip_post_tax-lgart = rt-lgart.
        append ipay_slip_post_tax.
      endif.
    endif.

  endloop.

  loop at crt.
    check crt-cumyr eq w_pabrj.

    read table i_wage_grp with key lgart = crt-lgart binary search.

    if sy-subrc eq 0.
      if  i_wage_grp-grp eq '08' or i_wage_grp-grp eq '11'.

        read table it_crtxt with key lgart = crt-lgart binary search.

        read table ipay_slip_post_tax
                with key post_tax_title = it_crtxt-kztxt.
        if sy-subrc eq 0.
          $ix = sy-tabix.
          ipay_slip_post_tax-ytd_amt = crt-betrg.
          modify ipay_slip_post_tax index $ix transporting ytd_amt.
          clear ipay_slip_post_tax.
        else.
          read table it_crtxt with key lgart = crt-lgart binary search.
          if sy-subrc eq 0.
            ipay_slip_post_tax-post_tax_title = it_crtxt-kztxt.
          endif.
          ipay_slip_post_tax-ytd_amt = crt-betrg.
          ipay_slip_post_tax-lgart = crt-lgart.
          append ipay_slip_post_tax.
          clear ipay_slip_post_tax.
        endif.
      endif.
    endif.

  endloop.

  return-type = 'S'.
  return-message = 'Success!'.
  append return.

  sort : ipay_slip_earning by lgart var_num,                "UD1K954297
*        ipay_slip_earning BY lgart,                        "UD1K954297
         ipay_slip_taxes by lgart,
         ipay_slip_pre_tax by lgart,
         ipay_slip_post_tax by lgart.

  loop at ipay_slip_earning.
    move-corresponding ipay_slip_earning to pay_slip_earning.
    pay_slip_earning-wage_type = ipay_slip_earning-lgart.   "UD1K954297
    append pay_slip_earning.
  endloop.

* BEGIN OF UD1K954297
  loop at ipay_slip_earning_sum where sumcd = '1' or
                                      sumcd = '2' or
                                      sumcd = '3' or
                                      sumcd = '4'.

    v_tot = v_tot + ipay_slip_earning_sum-hours.
  endloop.

  if sy-subrc = 0.
    clear ipay_slip_earning_sum.
    ipay_slip_earning_sum-sumcd = '5'.           "Total
    ipay_slip_earning_sum-earning_title = 'TOTAL'.
    ipay_slip_earning_sum-hours = v_tot.
    append ipay_slip_earning_sum.
  endif.

  sort ipay_slip_earning_sum by sumcd.
  loop at ipay_slip_earning_sum.
    move-corresponding ipay_slip_earning_sum to pay_slip_earning_sum.
    append pay_slip_earning_sum.
  endloop.
* END OF UD1K954297

* BEGIN OF UD1K955403
  loop at ipay_slip_earning.
    move-corresponding ipay_slip_earning to pay_slip_earning_total.
    at end of lgart.
      sum.
      pay_slip_earning_total-hours    = ipay_slip_earning-hours.
      pay_slip_earning_total-curr_amt = ipay_slip_earning-curr_amt.
      pay_slip_earning_total-ytd_amt  = ipay_slip_earning-ytd_amt.
      append pay_slip_earning_total.
    endat.
  endloop.
* END OF UD1K955403

  loop at ipay_slip_taxes.
    move-corresponding ipay_slip_taxes to pay_slip_taxes.
    append pay_slip_taxes.
    add ipay_slip_taxes-curr_amt to curr_tax.
    add ipay_slip_taxes-ytd_amt to ytd_tax.
  endloop.

  loop at ipay_slip_pre_tax.
    move-corresponding ipay_slip_pre_tax to pay_slip_pre_tax.
    collect pay_slip_pre_tax.
  endloop.

  loop at ipay_slip_post_tax.
    move-corresponding ipay_slip_post_tax to pay_slip_post_tax.
    collect pay_slip_post_tax.
  endloop.

  loop at pay_slip_summary.
    if pay_slip_summary-summary_type eq 'CURR.'.
      pay_slip_summary-taxes = curr_tax.
    endif.
    if pay_slip_summary-summary_type eq 'YTD'.
      pay_slip_summary-taxes = ytd_tax.
    endif.
    modify pay_slip_summary index sy-tabix transporting taxes.
  endloop.

*  PERFORM GET_RESULT_DATA_BY_FUNCTION USING PAYSLIP_VARIANT.

endfunction.
