************************************************************************
* Program Name      : ZAPP903R_SEQ_SUM01
* Author            : Bobby
* Creation Date     : 2003.12.23.
* Specifications By : Bobby
* Pattern           : 2.1
* Development Request No : UD1K902288
* Addl Documentation:
* Description       :
*          Sequence Parts Summary (Bucket: 2 Hourly, Horz.: 3 Days)
* Modification Logs
* Date       Developer    RequestNo    Description
* Change : Change the Time Tule(Tack-Time --> Lead Time)
* 03/03/2005 chris       UD1K914781   Start date => system date
************************************************************************
report  zapp903r_seq_sum01    message-id zmpp.

*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
tables: ztpp_common_vals,
        ztpp_input_plan,
        ausp .

*----------------------------------------------------------------------
* Gloval Variables Definition
*----------------------------------------------------------------------
data: wa_uph_b                type zvpp_ld-lrate,
      wa_uph_p                type zvpp_ld-lrate,
      wa_uph_t                type zvpp_ld-lrate.
data: c_factor(13)   type p decimals 3 value 1.
*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION
*----------------------------------------------------------------------
data: begin of it_data        occurs 0,
        objek                 like ausp-objek.      " Vehicle Code
        include structure     ztpp_input_plan.
data: end of it_data.

data: begin of is_prod        occurs 0,
        objek                 like ausp-objek.      " Vehicle Code
        include structure     ztpp_day_sum   .
data:   cnt                   type i         ,
        model(4)              type c         ,
      end of is_prod.

data: begin of it_alc         occurs 0.
        include structure     cukb    .
data:   model(3)              type c  ,
        knktx                 like cukbt-knktx,
        code(3)               type c  ,
        rp(2)                 type n  ,
        type_alc              type c  ,
        char_alc              like cabn-atnam,
        full_alc(4)           type c  ,
      end of it_alc .

data: begin of it_model       occurs 0,
        modl                  type zpp_model,
      end of it_model.

data: begin of it_sum         occurs 0,
        rp(2)                 type n  ,
        alc(9)                type c  ,              " For Summary Field
        worder                like mara-matnr,
        knnam                 like cukb-knnam,
        status                like ztpp_input_plan-status,
        code(4)               type c  ,              " ALC CODE
        vals(5)               type c  ,              " ALC CODE VALUE
        hours                 type i  ,
        vm_model              like ztpp_input_plan-modl ,
        vm_bodyser            like ztpp_input_plan-body_ser,
        extc                  like ztpp_input_plan-extc,
        intc                  like ztpp_input_plan-intc,
        mitu                  type zmitu,
        mitucnt               type i  ,
        cnt                   type i  ,
        serial                like ztpp_input_plan-serial,
      end of it_sum .

*DATA: BEGIN OF it_seq         OCCURS 0.
*       INCLUDE STRUCTURE     zspp_vm_gen.
*DATA:   alc(11)               TYPE c  ,             " For Summary Field
*        knnam                 LIKE cukb-knnam,
*        cnt                   TYPE i  ,
*        mitu                  TYPE i  ,
*        code(4)               TYPE c  ,              " ALC CODE
*        vals(5)               TYPE c  ,              " ALC CODE VALUE
*      END OF it_seq.

data: begin of it_master  occurs 0,
        seq               type i  ,             " Sequence
        date              type d  ,             " Date
        day               like kapa-tagnr,      " Day
        shift             like kapa-schnr,      " Shift
        time              type kapendzt  ,      " Times for working
        uph               type zvpp_ld-lrate,   " UPH
        tqty              type i  ,             " Day's Total Veh.
      end of it_master.

data: begin of it_shift   occurs 0,
        ser               type i  ,             " Serial
        seq               type i  ,             " Sequence
        date              type d  ,             " Date
        day               like kapa-tagnr,      " Day
        shift             like kapa-schnr,      " Shift
        time              type kapendzt  ,      " Times for working(NET)
        total             type kapendzt  ,      " Times for working(GRS)
        ftime             type kapendzt  ,      " Start Time.
        uph               type zvpp_ld-lrate,   " UPH
        tqty              type i,               " Shift's Total Veh.
        hqty              type i  ,             " Hour's Total Veh.
        hloop             type i  ,             " Hour's LOOP.
      end of it_shift .

data: it_prod             like table of is_prod        with header line,
      it_seq              like table of it_sum         with header line,
      it_d1               like table of it_sum         with header line,
      it_bi               like table of it_sum         with header line,
      it_wbs              like table of it_sum         with header line,
      it_pi               like table of it_sum         with header line,
      it_prj              like table of it_sum         with header line,
      it_pbs              like table of it_sum         with header line,
*     it_d21              LIKE TABLE OF it_sum         WITH HEADER LINE,
      it_disp_d1          like table of ztpp_seq_sum01 with header line,
      it_disp_seq         like table of ztpp_seq_sum01 with header line,
      it_disp_bi          like table of ztpp_seq_sum01 with header line,
      it_disp_pi          like table of ztpp_seq_sum01 with header line,
      it_disp_prj         like table of ztpp_seq_sum01 with header line,
      it_disp_wbs         like table of ztpp_seq_sum01 with header line,
      it_disp_pbs         like table of ztpp_seq_sum01 with header line,
*     it_disp_d21         LIKE TABLE OF ztpp_seq_sum01 WITH HEADER LINE,
      it_disp             like table of ztpp_seq_sum01 with header line.

*----------------------------------------------------------------------
* WORKING-AREA DECLARATION
*----------------------------------------------------------------------
data: wa_disp             like it_disp                             ,
      wa_data             like it_data                             ,
      wa_wdate            like ztpp_day_sum-wdate                  ,
      wa_kalid            like kako-kalid                          ,
      wa_repid            like sy-repid                            ,
      wa_uzeit            like sy-uzeit                            ,
      wa_index            like sy-tabix                            ,
      wa_model(3)         type c                                   ,
      wa_error            type c                                   ,
      wa_flag             type c                                   ,
      wa_hour             type i                                   .


*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION
*----------------------------------------------------------------------
field-symbols: <wa_dfield>    type any.


*----------------------------------------------------------------------
* CONSTANTS DECLARATION
*----------------------------------------------------------------------
data: c_jobs(40)              value 'ZAPP903R_INPUT_PLAN',
      c_key1(18)              value 'SEQ_SUM01' .


*----------------------------------------------------------------------
* SCREEN-VARIABLES DECLARATION
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
selection-screen begin of block b1 with frame .
selection-screen begin of block b3 with frame title text-002.
parameters: p_dates       type d        obligatory,
            p_test        type c                  ,
            p_mitu        type c                  ,
            p_wbs(2)      type n        obligatory,
            p_prj(2)      type n        obligatory.
selection-screen end of block b3.
selection-screen end of block b1.

*----------------------------------------------------------------------
* INCLUDE PROGRAM.....
*----------------------------------------------------------------------

*----------------------------------------------------------------------
initialization.
*----------------------------------------------------------------------
  data: l_vals                like ztpp_common_vals-item4.

  " Get the Date for the Production Reporting Date(Last Date)
* requested by MY Mur changed by chris
*  PERFORM get_start_day     USING wa_wdate     .
*  p_dates = wa_wdate     .
  p_dates  = wa_wdate = sy-datum.
* end of change on 03/03/2005
  wa_wdate = wa_wdate - 1.  wa_repid = sy-repid.
  perform read_shop_calid   using wa_kalid.
  perform read_working_date using '-'  wa_kalid  wa_wdate.

  if p_wbs is initial.
    select single item4  into l_vals
      from ztpp_common_vals
     where jobs = wa_repid
       and key2 = 'WBS'   .
    p_wbs = l_vals        .   clear: l_vals.
  endif.

  if p_prj is initial.
    select single item4  into l_vals
      from ztpp_common_vals
     where jobs = wa_repid
       and key2 = 'PRJ'   .
    p_prj = l_vals        .   clear: l_vals.
  endif.

*----------------------------------------------------------------------
start-of-selection.
*----------------------------------------------------------------------
  perform clear_variable               .
  perform set_information              .
  perform read_inputplan               .
  perform read_alc_model               .
  perform create_summary               .
  perform insert_field_vals            .
  perform summary_disp_final           .
  perform display_data                 .

*----------------------------------------------------------------------
end-of-selection.
*----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form clear_variable.
  clear: it_data,   it_alc,   it_sum,   it_disp,
         it_data[], it_alc[], it_sum[], it_disp[],
         wa_data,  wa_uzeit, wa_index, wa_hour.
endform.                    " CLEAR_VARIABLE

*&---------------------------------------------------------------------*
*&      Form  READ_INPUTPLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_inputplan.
  if p_mitu = 'X'.                                          "UD1K912950
    select * into corresponding fields of table it_data     "UD1K912950
     from ztpp_input_plan                                   "UD1K912950
     where mitu ne 'Y'   .                                  "UD1K912950
  else.                                                     "UD1K912950
    select * into corresponding fields of table it_data
     from ztpp_input_plan .
  endif.

  " Elemenate the TEST-CAR that Dealer code is 'XX', 'XY' .
  if p_test = 'X'.
    loop at it_data.
      if it_data-work_order+12(2) = 'XX' or
         it_data-work_order+12(2) = 'XY' .
        delete it_data.
      endif.
    endloop.
  endif.

  describe table it_data lines  wa_hour .
  if wa_hour = 0.
    delete from ztpp_seq_sum01  client specified where mandt = sy-mandt.
    leave program .
  endif.

  select * into corresponding fields of table it_prod
    from ztpp_day_sum
   where wdate = wa_wdate and
         dealer ne 'XX'   and
         dealer ne 'XY' .

  sort it_data by rsnum serial .
*---start#1 wskim : check holding car
  perform check_holding_car.
*---end

endform.                    " READ_INPUTPLAN

*&---------------------------------------------------------------------*
*&      Form  READ_ALC_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODEL  text
*----------------------------------------------------------------------*
form read_alc_model  .
  data: lc_model(15)         type c         ,
        l_knobj              like cuco-knobj,
        l_knnum              like cuob-knobj,
        l_knnam              like cukb-knnam.

  clear: it_model, it_model[].
  perform get_models .

  loop at it_model   .
    wa_model = it_model-modl .
    concatenate 'D_' wa_model '_ALC_'  into lc_model    .
    concatenate wa_model '_WOHD'           into  l_knnam.
    perform get_knobj                      using l_knnam  l_knobj.
    perform get_knnum                      using l_knobj.
    concatenate wa_model '_WOCL'           into  l_knnam.
    perform get_knobj                      using l_knnam  l_knobj.
    perform get_knnum                      using l_knobj.
    " Set the Model Code...
    loop at it_alc  where model = space.
      it_alc-model = wa_model .
      modify it_alc.
    endloop.
  endloop.

  loop at it_alc.
    concatenate 'D_' it_alc-model '_ALC_'  into lc_model    .
    select single b~knnam t~knktx
      into corresponding fields of it_alc
      from cukb as b inner join cukbt as t
        on b~knnum = t~knnum
     where b~knnum = it_alc-knnum
       and t~spras = sy-langu   .

    if it_alc-knnam(10) ne lc_model .
      delete it_alc .
      continue .
    endif.
    it_alc-code     = it_alc-knnam+12(3) .
    it_alc-type_alc = it_alc-knnam+10(1) .
    it_alc-rp       = it_alc-knktx(2)    .
    concatenate it_alc-type_alc it_alc-code into it_alc-full_alc .
    concatenate 'P' it_alc-knnam+5(10)      into it_alc-char_alc .
    modify it_alc .
  endloop.
  sort it_alc by knnum rp code .
endform.                    " READ_ALC_MODEL

*&---------------------------------------------------------------------*
*&      Form  CREATE_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_summary.
  data: lt_alc               like table of it_alc      with header line,
        lt_wbs               like table of it_wbs      with header line,
        lt_prj               like table of it_prj      with header line,
        lt_seq               like table of it_seq      with header line,
        lt_bi                like table of it_bi       with header line,
        lt_pi                like table of it_pi       with header line,
        lt_d1                like table of it_d1       with header line,
        lt_data              like table of it_data     with header line.

  perform inline_status .
  lt_data[] = it_data[] .  lt_wbs[] = it_wbs[].  lt_bi[] = it_bi[].
  lt_d1[]   = it_d1[]   .  lt_prj[] = it_prj[].  lt_pi[] = it_pi[].
  lt_seq[]  = it_seq[]  .  lt_alc[] = it_alc[].

  clear: wa_index, wa_hour, it_sum, it_sum[].
  perform read_internal_table .
  perform read_alc            .
  perform calc_alc            .

  clear: wa_index, wa_hour, it_sum, it_sum[].
  perform calc_alc_prod       .
  perform calc_alc_seq        .
  perform calc_alc_bi         .
  perform calc_alc_wbs        .
  perform calc_alc_pi         .
  perform calc_alc_prj        .
  perform calc_alc_pbs        .

  perform summary_disp          .
endform.                    " CREATE_SUMMARY

*&---------------------------------------------------------------------*
*&      Form  READ_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_RP  text
*----------------------------------------------------------------------*
form read_internal_table .
  data: l_name(40)           type c ,
        l_flag               type c ,
        l_max                type i ,
        l_cnt                type i ,
        l_shift              like kapa-schnr,
        l_hours              type i ,
        l_pos                type i ,
        l_no2(2)             type n ,
        l_chk                type p decimals 3,
        l_tabix              like sy-tabix,
        l_start              like sy-tabix,
        l_end                like sy-tabix,
        l_index              like sy-tabix.
  data: l_rq                 type i,
        l_lpcnt              type i.

  read table it_data with key rp06 = space .
  l_index = sy-tabix .
  if l_index > 1.
    l_index = l_index - 1 .
    delete it_data from 1 to l_index.
  endif.
  l_index = 1.

  " 3 Days Data...
  clear: l_pos.   sort it_shift by ser seq shift.
  loop at it_master.

** Furong on 06/13/12
    l_hours = 24 * it_master-seq - 24 .
    l_shift = 1.
    loop at it_shift where seq = it_master-seq .
      if it_shift-shift ne 1.
        if it_shift-shift = 2.
          l_hours = 24 * it_master-seq - 16.
        else.
          l_hours = 24 * it_master-seq - 8.
        endif.
        l_shift = it_shift-shift .
      endif.

*    l_hours = 20 * it_master-seq - 20 .
*    l_shift = 1.
*    LOOP AT it_shift WHERE seq = it_master-seq .
*      IF it_shift-shift NE l_shift.
*        l_hours = 20 * it_master-seq - 10 .
*        l_shift = it_shift-shift .
*      ENDIF.
** End 06/13/12
      clear: l_cnt,
             l_lpcnt.                                       "UD1K914561
      l_rq      = it_shift-tqty.                            "UD1K914561
      l_lpcnt  = it_shift-hloop.                            "UD1K914561

      do it_shift-hloop times.

*8/2/12 { IG.MOON

          IF sy-index EQ it_shift-hloop.
            it_shift-hqty = l_rq.
          ENDIF.
* }

        l_rq    = l_rq - it_shift-hqty.                     "UD1K914561
        l_lpcnt = l_lpcnt - 1.                              "UD1K914561

*        l_cnt = l_cnt + it_shift-hqty            .
*        l_chk = it_master-tqty - l_cnt           .
*        IF l_chk < 0                             .
*          it_shift-hqty = it_shift-hqty + l_chk  .
*        ENDIF.

        l_pos = it_shift-hqty + l_index - 1 .
        l_hours = l_hours + 2 .
        loop at it_data from l_index to l_pos.
          clear: it_sum.
          it_sum-hours      = l_hours .
          it_sum-vm_model   = it_data-modl .
          it_sum-vm_bodyser = it_data-body_ser.
          it_sum-worder     = it_data-work_order .
          it_sum-mitu       = it_data-mitu       .
          it_sum-extc       = it_data-extc       .
          it_sum-intc       = it_data-intc       .
          it_sum-status     = it_data-status     .
          it_sum-rp         = it_alc-rp          .
          it_sum-knnam      = it_alc-knnam       .
          it_sum-cnt        = 1                  .
          concatenate it_alc-type_alc it_alc-code into it_sum-code .
          append it_sum.
        endloop.
        l_index = l_pos + 1 .
      enddo.
    endloop.
  endloop.
endform.                    " READ_INTERNAL_TABLE

*&---------------------------------------------------------------------*
*&      Form  calc_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc.
  data: l_name(40)              type c  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)               type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp.
  describe table it_sum lines l_line.
  check l_line > 0 .
  sort it_sum by vm_model hours  code vals.
  read table it_sum index 1.
  l_hours   = it_sum-hours    .
  l_code   = it_sum-code    .
  l_vals   = it_sum-vals    .
  l_model  = it_sum-vm_model.

  concatenate 'IT_DISP-H'  l_hours  into l_name.
  assign (l_name)                   to   <wa_dfield>.
  clear: <wa_dfield>.

  loop at it_sum.
    if l_hours = it_sum-hours and l_code  = it_sum-code and
       l_vals  = it_sum-vals  .
      <wa_dfield> = <wa_dfield> + it_sum-cnt .
      continue.
    else.
      it_disp-model    = l_model  .
      it_disp-alc_code = l_code   .
      it_disp-alc_vals = l_vals   .
      it_disp-rp       = it_alc-rp.
      append it_disp.    clear: it_disp.
      l_hours    = it_sum-hours    .
      l_code     = it_sum-code     .
      l_vals     = it_sum-vals     .
      l_model    = it_sum-vm_model .
      concatenate 'IT_DISP-H'  l_hours  into l_name.
      assign (l_name)                   to   <wa_dfield>.
      <wa_dfield>  = it_sum-cnt .
    endif.
  endloop.

  describe table it_sum  lines  l_line.
  if l_line > 0.
    it_disp-model    = l_model  .
    it_disp-alc_code = l_code   .
    it_disp-alc_vals = l_vals   .
    it_disp-rp       = it_alc-rp.
    append it_disp.
  endif.
endform.                    " calc_alc

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_prod.
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_d1  .
  sort it_d1 by vm_model hours  code vals.
  read table it_d1 index 1.  clear: l_count.
* l_hours  = it_sum_PROD-hours   .
  l_code   = it_d1-code    .
  l_vals   = it_d1-vals    .
  l_model  = it_d1-vm_model.

  loop at it_d1.
    if l_vals  = it_d1-vals  and l_code  = it_d1-code and
       l_model  = it_d1-vm_model.
*      l_hours = IT_D1-hours.
      l_count  = l_count + it_d1-cnt .
      continue.
    else.
      it_disp_d1-model    = l_model  .
      it_disp_d1-alc_code = l_code   .
      it_disp_d1-alc_vals = l_vals   .
      it_disp_d1-rp       = it_alc-rp.
      it_disp_d1-d_1      = l_count  .
      append it_disp_d1.    clear: it_disp_d1.
*     l_hours = IT_D1-hours      .
      l_code  = it_d1-code       .
      l_vals  = it_d1-vals       .
      l_model = it_d1-vm_model   .
      l_count = it_d1-cnt        .
    endif.
  endloop.

  describe table it_d1  lines  l_line.
  if l_line > 0.
    it_disp_d1-model    = l_model  .
    it_disp_d1-alc_code = l_code   .
    it_disp_d1-alc_vals = l_vals   .
    it_disp_d1-rp       = it_alc-rp.
    it_disp_d1-d_1      = l_count  .
    append it_disp_d1.
  endif.
endform.                    " calc_alc_PROD

*&---------------------------------------------------------------------*
*&      Form  calc_alc_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_seq .
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_mitucnt               type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_seq .
  sort it_seq  by vm_model code vals.
  read table it_seq  index 1.  clear: l_count, l_mitucnt.
* l_hours  = it_SEQ-hours   .
  l_code   = it_seq-code    .
  l_vals   = it_seq-vals    .
  l_model  = it_seq-vm_model.

  loop at it_seq .
    if l_vals  = it_seq-vals  and l_code  = it_seq-code and
       l_model  = it_seq-vm_model.
      l_count  = l_count + it_seq-cnt .
      l_mitucnt = l_mitucnt + it_seq-mitucnt .
      continue.
    else.
      it_disp_seq-model    = l_model  .
      it_disp_seq-alc_code = l_code   .
      it_disp_seq-alc_vals = l_vals   .
      it_disp_seq-rp       = it_alc-rp.
      it_disp_seq-d_1      = l_count  .
      it_disp_seq-mitu     = l_mitucnt.
      append it_disp_seq .    clear: it_disp_seq, l_mitucnt.
*     l_hours = it_SEQ-hours      .
      l_code  = it_seq-code       .
      l_vals  = it_seq-vals       .
      l_model   = it_seq-vm_model   .
      l_count   = it_seq-cnt        .
      l_mitucnt = it_seq-mitucnt .
    endif.
  endloop.

  describe table it_seq   lines  l_line.
  if l_line > 0.
    it_disp_seq-model    = l_model  .
    it_disp_seq-alc_code = l_code   .
    it_disp_seq-alc_vals = l_vals   .
    it_disp_seq-rp       = it_alc-rp.
    it_disp_seq-d_1      = l_count  .
    it_disp_seq-mitu     = l_mitucnt.
    append it_disp_seq .
  endif.
endform.                    " calc_alc_SEQ

*&---------------------------------------------------------------------*
*&      Form  calc_alc_BI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_bi  .
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_bi .
  sort it_bi   by vm_model code vals.
  read table it_bi   index 1.  clear: l_count.
  l_code   = it_bi-code    .
  l_vals   = it_bi-vals    .
  l_model  = it_bi-vm_model.

  loop at it_bi  .
    if l_vals  = it_bi-vals  and l_code  = it_bi-code and
       l_model  = it_bi-vm_model.
      l_count  = l_count + it_bi-cnt .
      continue.
    else.
      it_disp_bi-model    = l_model  .
      it_disp_bi-alc_code = l_code   .
      it_disp_bi-alc_vals = l_vals   .
      it_disp_bi-rp       = it_alc-rp.
      it_disp_bi-d_1      = l_count  .
      append it_disp_bi .    clear: it_disp_bi .
      l_code  = it_bi-code       .
      l_vals  = it_bi-vals       .
      l_model = it_bi-vm_model   .
      l_count = it_bi-cnt        .
    endif.
  endloop.

  describe table it_bi    lines  l_line.
  if l_line > 0.
    it_disp_bi-model    = l_model  .
    it_disp_bi-alc_code = l_code   .
    it_disp_bi-alc_vals = l_vals   .
    it_disp_bi-rp       = it_alc-rp.
    it_disp_bi-d_1      = l_count  .
    append it_disp_bi  .
  endif.
endform.                    " calc_alc_BI

*&---------------------------------------------------------------------*
*&      Form  calc_alc_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_wbs .
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_wbs .
  sort it_wbs  by vm_model code vals.
  read table it_wbs  index 1.  clear: l_count.
* l_hours  = it_WBS-hours   .
  l_code   = it_wbs-code    .
  l_vals   = it_wbs-vals    .
  l_model  = it_wbs-vm_model.

  loop at it_wbs .
    if l_vals  = it_wbs-vals  and l_code  = it_wbs-code and
       l_model  = it_wbs-vm_model.
      l_count  = l_count + it_wbs-cnt .
      continue.
    else.
      it_disp_wbs-model    = l_model  .
      it_disp_wbs-alc_code = l_code   .
      it_disp_wbs-alc_vals = l_vals   .
      it_disp_wbs-rp       = it_alc-rp.
      it_disp_wbs-d_1      = l_count  .
      append it_disp_wbs .    clear: it_disp_wbs .
      l_code  = it_wbs-code       .
      l_vals  = it_wbs-vals       .
      l_model = it_wbs-vm_model   .
      l_count = it_wbs-cnt        .
    endif.
  endloop.

  describe table it_wbs   lines  l_line.
  if l_line > 0.
    it_disp_wbs-model    = l_model  .
    it_disp_wbs-alc_code = l_code   .
    it_disp_wbs-alc_vals = l_vals   .
    it_disp_wbs-rp       = it_alc-rp.
    it_disp_wbs-d_1      = l_count  .
    append it_disp_wbs .
  endif.
endform.                    " calc_alc_WBS

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_pi  .
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_pi .
  sort it_pi   by vm_model code vals.
  read table it_pi   index 1.  clear: l_count.
  l_code   = it_pi-code    .
  l_vals   = it_pi-vals    .
  l_model  = it_pi-vm_model.

  loop at it_pi  .
    if l_vals  = it_pi-vals  and l_code  = it_pi-code and
       l_model  = it_pi-vm_model.
      l_count  = l_count + it_pi-cnt .
      continue.
    else.
      it_disp_pi-model    = l_model  .
      it_disp_pi-alc_code = l_code   .
      it_disp_pi-alc_vals = l_vals   .
      it_disp_pi-rp       = it_alc-rp.
      it_disp_pi-d_1      = l_count  .
      append it_disp_pi .    clear: it_disp_pi .
*     l_hours = it_PI-Hours      .
      l_code  = it_pi-code       .
      l_vals  = it_pi-vals       .
      l_model = it_pi-vm_model   .
      l_count = it_pi-cnt        .
    endif.
  endloop.

  describe table it_pi    lines  l_line.
  if l_line > 0.
    it_disp_pi-model    = l_model  .
    it_disp_pi-alc_code = l_code   .
    it_disp_pi-alc_vals = l_vals   .
    it_disp_pi-rp       = it_alc-rp.
    it_disp_pi-d_1      = l_count  .
    append it_disp_pi  .
  endif.
endform.                    " calc_alc_PI

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PRJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_prj .
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_prj .
  sort it_prj by vm_model code vals.
  read table it_prj index 1.  clear: l_count.
  l_code   = it_prj-code    .
  l_vals   = it_prj-vals    .
  l_model  = it_prj-vm_model.

  loop at it_prj .
    if l_vals  = it_prj-vals  and l_code  = it_prj-code and
       l_model  = it_prj-vm_model.
*      l_hours = it_PRJ-hours.
      l_count  = l_count + it_prj-cnt .
      continue.
    else.
      it_disp_prj-model    = l_model  .
      it_disp_prj-alc_code = l_code   .
      it_disp_prj-alc_vals = l_vals   .
      it_disp_prj-rp       = it_alc-rp.
      it_disp_prj-d_1      = l_count  .
      append it_disp_prj.    clear: it_disp_prj .
*     l_hours = it_PRJ-hours      .
      l_code  = it_prj-code       .
      l_vals  = it_prj-vals       .
      l_model = it_prj-vm_model   .
      l_count = it_prj-cnt        .
    endif.
  endloop.

  describe table it_prj   lines  l_line.
  if l_line > 0.
    it_disp_prj-model    = l_model  .
    it_disp_prj-alc_code = l_code   .
    it_disp_prj-alc_vals = l_vals   .
    it_disp_prj-rp       = it_alc-rp.
    it_disp_prj-d_1      = l_count  .
    append it_disp_prj.
  endif.
endform.                    " calc_alc_PRJ

*&---------------------------------------------------------------------*
*&      Form  calc_alc_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_alc_pbs .
  data: l_count                 type i  ,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Summary of the data by Time-stamp...
  clear: it_disp_pbs .
  sort it_pbs  by vm_model code vals.
  read table it_pbs  index 1.  clear: l_count.
* l_hours  = it_PBS-hours   .
  l_code   = it_pbs-code    .
  l_vals   = it_pbs-vals    .
  l_model  = it_pbs-vm_model.

  loop at it_pbs .
    if l_vals  = it_pbs-vals  and l_code  = it_pbs-code and
       l_model  = it_pbs-vm_model.
      l_count  = l_count + it_pbs-cnt .
      continue.
    else.
      it_disp_pbs-model    = l_model  .
      it_disp_pbs-alc_code = l_code   .
      it_disp_pbs-alc_vals = l_vals   .
      it_disp_pbs-rp       = it_alc-rp.
      it_disp_pbs-d_1      = l_count  .
      append it_disp_pbs .    clear: it_disp_pbs .
      l_code  = it_pbs-code       .
      l_vals  = it_pbs-vals       .
      l_model = it_pbs-vm_model   .
      l_count = it_pbs-cnt        .
    endif.
  endloop.

  describe table it_pbs   lines  l_line.
  if l_line > 0.
    it_disp_pbs-model    = l_model  .
    it_disp_pbs-alc_code = l_code   .
    it_disp_pbs-alc_vals = l_vals   .
    it_disp_pbs-rp       = it_alc-rp.
    it_disp_pbs-d_1      = l_count  .
    append it_disp_pbs .
  endif.
endform.                    " calc_alc_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_ALC_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_WORDER  text
*      -->P_IT_SUM_CODE  text
*      <--P_IT_SUM_VALS  text
*----------------------------------------------------------------------*
form get_alc_value using    pa_worder  pa_code
                   changing pa_vals.
  data: l_vals            like table of zspp_vin_value with header line,
        l_matnr           like mara-matnr.

  clear: l_vals, l_vals[].
  case pa_code(1).
    when 'U'.
      l_matnr = pa_worder(14).
      concatenate 'P_ALC_U_' pa_code+1(3) into l_vals-atnam.
    when 'C'.
      l_matnr = pa_worder    .
      concatenate 'P_ALC_C_' pa_code+1(3) into l_vals-atnam.
  endcase.
  append l_vals.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = l_matnr
      ctype        = '001'
    TABLES
      val_table    = l_vals
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      others       = 5.

  read table l_vals index 1  .
  pa_vals = l_vals-atwrt     .
endform.                    " GET_ALC_VALUE

*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_data.
  data: l_cnt                like it_disp-serial.
  delete from ztpp_seq_sum01 client specified where mandt = sy-mandt.
  sort it_disp by serial alc_code model descending alc_vals .
*  LOOP AT it_disp.
*    LOOP AT it_seq WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-seq = it_disp-seq + it_seq-cnt     .
*    ENDLOOP.
*    LOOP AT it_bi  WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-bodyin = it_disp-bodyin + it_bi-cnt     .
*    ENDLOOP.
*    LOOP AT it_pi WHERE  model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-paint = it_disp-paint + it_pi-cnt     .
*    ENDLOOP.
*    LOOP AT it_pbs WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-pbs = it_disp-pbs + it_pbs-cnt     .
*    ENDLOOP.
*    LOOP AT it_prj WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-prj = it_disp-prj + it_prj-cnt     .
*    ENDLOOP.
*    LOOP AT it_wbs WHERE model = it_disp-model    AND
*                         code  = it_disp-alc_code AND
*                         vals  = it_disp-alc_vals .
*      it_disp-wbs = it_disp-wbs + it_wbs-cnt     .
*    ENDLOOP.
*    MODIFY it_disp.
*  ENDLOOP.
  modify ztpp_seq_sum01     from table it_disp .
  if sy-subrc = 0.
    commit work and wait.
    message s000 with 'Update successful'.
  else.
    rollback work.
    message s000 with 'Update failed'.
  endif.
endform.                    " display_data

*&---------------------------------------------------------------------*
*&      Form  INLINE_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form inline_status.
  " Summary of the ALC for the Current MIP Vehicle..
  perform create_data   tables  it_seq   using '00'.
  perform calc_seq                                 .
  perform create_data   tables  it_bi    using '01'.
  perform calc_bodyinput                           .
  perform create_data   tables  it_wbs   using '99'.
  perform calc_wbs                                 .
  perform create_data   tables  it_pi    using '02'.
  perform calc_paintinput                          .
  perform create_data   tables  it_prj   using '88'.
  perform calc_paintreject                         .
  perform create_data   tables  it_pbs   using '06'.
  perform calc_pbs                                 .
* PERFORM calc_d21                                 .
  perform calc_prod                                .
endform.                    " INLINE_STATUS

*&---------------------------------------------------------------------*
*&      Form  add_routine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
form add_routine using    pa_disp  like  it_disp.
  it_disp-h02  = it_disp-h02  + pa_disp-h02 .
  it_disp-h04  = it_disp-h04  + pa_disp-h04 .
  it_disp-h06  = it_disp-h06  + pa_disp-h06 .
  it_disp-h08  = it_disp-h08  + pa_disp-h08 .
  it_disp-h10  = it_disp-h10  + pa_disp-h10 .
  it_disp-h12  = it_disp-h12  + pa_disp-h12 .
  it_disp-h14  = it_disp-h14  + pa_disp-h14 .
  it_disp-h16  = it_disp-h16  + pa_disp-h16 .
  it_disp-h18  = it_disp-h18  + pa_disp-h18 .
  it_disp-h20  = it_disp-h20  + pa_disp-h20 .
  it_disp-h22  = it_disp-h22  + pa_disp-h22 .
  it_disp-h24  = it_disp-h24  + pa_disp-h24 .
  it_disp-h26  = it_disp-h26  + pa_disp-h26 .
  it_disp-h28  = it_disp-h28  + pa_disp-h28 .
  it_disp-h30  = it_disp-h30  + pa_disp-h30 .
  it_disp-h32  = it_disp-h32  + pa_disp-h32 .
  it_disp-h34  = it_disp-h34  + pa_disp-h34 .
  it_disp-h36  = it_disp-h36  + pa_disp-h36 .
  it_disp-h38  = it_disp-h38  + pa_disp-h38 .
  it_disp-h40  = it_disp-h40  + pa_disp-h40 .
  it_disp-h42  = it_disp-h42  + pa_disp-h42 .
  it_disp-h44  = it_disp-h44  + pa_disp-h44 .
  it_disp-h46  = it_disp-h46  + pa_disp-h46 .
  it_disp-h48  = it_disp-h48  + pa_disp-h48 .
  it_disp-h50  = it_disp-h50  + pa_disp-h50 .
  it_disp-h52  = it_disp-h52  + pa_disp-h52 .
  it_disp-h54  = it_disp-h54  + pa_disp-h54 .
  it_disp-h56  = it_disp-h56  + pa_disp-h56 .
  it_disp-h58  = it_disp-h58  + pa_disp-h58 .
  it_disp-h60  = it_disp-h60  + pa_disp-h60 .
** Furong on 06/14/12 for 3 shift
  it_disp-h62  = it_disp-h62  + pa_disp-h62 .
  it_disp-h64  = it_disp-h64  + pa_disp-h64 .
  it_disp-h66  = it_disp-h66  + pa_disp-h66 .
  it_disp-h68  = it_disp-h68  + pa_disp-h68 .
  it_disp-h70  = it_disp-h70  + pa_disp-h70 .
  it_disp-h72  = it_disp-h72  + pa_disp-h72 .
** End on 06/14/12
  it_disp-mitu = it_disp-mitu + pa_disp-mitu.

endform.                    " add_routine
*&---------------------------------------------------------------------*
*&      Form  SUB_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PA_DISP  text
*----------------------------------------------------------------------*
form sub_total using pa_disp like it_disp.
** Furong on 06/15/12 for 3 shift
*  IT_DISP-STOT = IT_DISP-STOT + PA_DISP-H02 + PA_DISP-H04 + PA_DISP-H06
*               + PA_DISP-H08  + PA_DISP-H10 + PA_DISP-H12 + PA_DISP-H14
*               + PA_DISP-H16  + PA_DISP-H18 + PA_DISP-H20 + PA_DISP-H22
*               + PA_DISP-H24  + PA_DISP-H26 + PA_DISP-H28 + PA_DISP-H30
*               + PA_DISP-H32  + PA_DISP-H34 + PA_DISP-H36 + PA_DISP-H38
*               + PA_DISP-H40  + PA_DISP-H42 + PA_DISP-H44 + PA_DISP-H46
*               + PA_DISP-H48  + PA_DISP-H50 + PA_DISP-H52 + PA_DISP-H54
*               + PA_DISP-H56  + PA_DISP-H58 + PA_DISP-H60.

  it_disp-stot = it_disp-stot + pa_disp-h02 + pa_disp-h04 + pa_disp-h06
               + pa_disp-h08  + pa_disp-h10 + pa_disp-h12 + pa_disp-h14
               + pa_disp-h16  + pa_disp-h18 + pa_disp-h20 + pa_disp-h22
               + pa_disp-h24  + pa_disp-h26 + pa_disp-h28 + pa_disp-h30
               + pa_disp-h32  + pa_disp-h34 + pa_disp-h36 + pa_disp-h38
               + pa_disp-h40  + pa_disp-h42 + pa_disp-h44 + pa_disp-h46
               + pa_disp-h48  + pa_disp-h50 + pa_disp-h52 + pa_disp-h54
               + pa_disp-h56  + pa_disp-h58 + pa_disp-h60 + pa_disp-h62
               + pa_disp-h64  + pa_disp-h66 + pa_disp-h68 + pa_disp-h70
               + pa_disp-h72.
** End of 06/15/12
endform.                    "SUB_TOTAL

*&---------------------------------------------------------------------*
*&      Form  add_routine_PROD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_DISP  text
*----------------------------------------------------------------------*
form add_routine_prod using    pa_disp  like  it_disp.
  it_disp-d_1    = it_disp-d_1  + pa_disp-d_1 .
  it_disp-seq    = it_disp-seq  + pa_disp-seq .
  it_disp-bodyin = it_disp-bodyin  + pa_disp-bodyin .
  it_disp-wbs    = it_disp-wbs  + pa_disp-wbs .
  it_disp-paint  = it_disp-paint  + pa_disp-paint .
  it_disp-prj    = it_disp-prj  + pa_disp-prj .
  it_disp-pbs    = it_disp-pbs  + pa_disp-pbs .
  it_disp-mitu   = it_disp-mitu + pa_disp-mitu.
  it_disp-h02  = it_disp-h02  + pa_disp-h02 .
  it_disp-h04  = it_disp-h04  + pa_disp-h04 .
  it_disp-h06  = it_disp-h06  + pa_disp-h06 .
  it_disp-h08  = it_disp-h08  + pa_disp-h08 .
  it_disp-h10  = it_disp-h10  + pa_disp-h10 .
  it_disp-h12  = it_disp-h12  + pa_disp-h12 .
  it_disp-h14  = it_disp-h14  + pa_disp-h14 .
  it_disp-h16  = it_disp-h16  + pa_disp-h16 .
  it_disp-h18  = it_disp-h18  + pa_disp-h18 .
  it_disp-h20  = it_disp-h20  + pa_disp-h20 .
  it_disp-h22  = it_disp-h22  + pa_disp-h22 .
  it_disp-h24  = it_disp-h24  + pa_disp-h24 .
  it_disp-h26  = it_disp-h26  + pa_disp-h26 .
  it_disp-h28  = it_disp-h28  + pa_disp-h28 .
  it_disp-h30  = it_disp-h30  + pa_disp-h30 .
  it_disp-h32  = it_disp-h32  + pa_disp-h32 .
  it_disp-h34  = it_disp-h34  + pa_disp-h34 .
  it_disp-h36  = it_disp-h36  + pa_disp-h36 .
  it_disp-h38  = it_disp-h38  + pa_disp-h38 .
  it_disp-h40  = it_disp-h40  + pa_disp-h40 .
  it_disp-h42  = it_disp-h42  + pa_disp-h42 .
  it_disp-h44  = it_disp-h44  + pa_disp-h44 .
  it_disp-h46  = it_disp-h46  + pa_disp-h46 .
  it_disp-h48  = it_disp-h48  + pa_disp-h48 .
  it_disp-h50  = it_disp-h50  + pa_disp-h50 .
  it_disp-h52  = it_disp-h52  + pa_disp-h52 .
  it_disp-h54  = it_disp-h54  + pa_disp-h54 .
  it_disp-h56  = it_disp-h56  + pa_disp-h56 .
  it_disp-h58  = it_disp-h58  + pa_disp-h58 .
  it_disp-h60  = it_disp-h60  + pa_disp-h60 .
** Furong on 06/15/12 for 3 shift
  it_disp-h62  = it_disp-h62  + pa_disp-h62 .
  it_disp-h64  = it_disp-h64  + pa_disp-h64 .
  it_disp-h66  = it_disp-h66  + pa_disp-h66 .
  it_disp-h68  = it_disp-h68  + pa_disp-h68 .
  it_disp-h70  = it_disp-h70  + pa_disp-h70 .
  it_disp-h72  = it_disp-h72  + pa_disp-h72 .
** END ON 06/15/12
endform.                    " add_routine_PROD

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form summary_disp.
  data: lt_disp                 like table of it_disp  with header line,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_status(3)             type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  lt_disp[] = it_disp[].

  " Accumulate the Data..
  clear: it_disp, it_disp[].
  sort lt_disp by alc_code model alc_vals.
  read table lt_disp index 1.
  l_model = lt_disp-model   .
  l_code  = lt_disp-alc_code.
  l_vals  = lt_disp-alc_vals.
  it_disp = lt_disp.
  perform clear_qty_disp  using it_disp     .

  loop at lt_disp.
    if l_model = lt_disp-model  and  l_code  = lt_disp-alc_code  and
       l_vals  = lt_disp-alc_vals.
      perform add_routine  using  lt_disp .
      continue.
    else.
      condense lt_disp-alc_code .
      it_disp-serial = strlen( l_vals )    .
      append it_disp.     clear: it_disp   .
      it_disp = lt_disp.
      l_model = lt_disp-model   .
      l_code  = lt_disp-alc_code.
      l_vals  = lt_disp-alc_vals.
    endif.
  endloop.

  describe table it_disp lines  l_line.
  if l_line > 0.
    condense lt_disp-alc_code .
    it_disp-serial = strlen( l_vals ) .
    append it_disp.
  endif.
endform.                    " SUMMARY_DISP

*&---------------------------------------------------------------------*
*&      Form  SUMMARY_DISP_final
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form summary_disp_final.
  data: lt_disp                 like table of it_disp  with header line,
        l_line                  type i  ,
        l_model(3)              type c  ,
        l_status(3)             type c  ,
        l_hours(2)              type n  ,
        l_code(4)               type c  ,              " ALC CODE
        l_vals(5)               type c  .              " ALC CODE VALUE

  " Final Summarize...
  lt_disp[] = it_disp[].    clear: it_disp, it_disp[].
  sort lt_disp by model alc_code alc_vals .
  read table lt_disp index 1.
  l_model = lt_disp-model   .
  l_code  = lt_disp-alc_code.
  l_vals  = lt_disp-alc_vals.
  it_disp     = lt_disp         .
  perform clear_qty_disp  using it_disp     .

  loop at lt_disp.
    if l_model = lt_disp-model  and  l_code  = lt_disp-alc_code  and
       l_vals  = lt_disp-alc_vals.
      perform add_routine_prod  using  lt_disp .
      perform sub_total using lt_disp.                   "UD1K_912950
      continue.
    else.
      condense lt_disp-alc_code .
      it_disp-serial = strlen( l_vals )    .
      append it_disp.     clear: it_disp   .
      it_disp     = lt_disp         .
      perform sub_total using lt_disp.                   "UD1K_912950

      l_model = lt_disp-model   .
      l_code  = lt_disp-alc_code.
      l_vals  = lt_disp-alc_vals.
    endif.
  endloop.

  describe table lt_disp lines  l_line.
  if l_line > 0.
    condense lt_disp-alc_code .
    it_disp-serial = strlen( l_vals ) .
    append it_disp.
  endif.
endform.                    " SUMMARY_DISP_FINAL


*&---------------------------------------------------------------------*
*&      Form  GET_KNOBJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
form get_knobj using    pa_knnam  pa_knobj.
  clear: pa_knobj.
  select single knobj into pa_knobj
    from cuco
   where obtab = 'MARA'
     and objek = pa_knnam .
endform.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
form get_knnum using    pa_knobj.
  select knnum appending corresponding fields of table it_alc
    from cuob
   where knobj = pa_knobj.
endform.                    " get_KNNUM

*&---------------------------------------------------------------------*
*&      Form  read_alc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_alc.
  data: lt_sum              like table of it_sum       with header line,
        l_model             like it_sum-vm_model,
        l_worder            like mara-matnr  ,
        l_hours             like it_sum-hours,
        l_mitucnt           type i           ,
        l_cnt               like it_sum-cnt  ,
        l_ext               like it_sum-extc ,
        l_int               like it_sum-intc ,
        l_size              like it_sum-cnt  .

  sort it_sum by hours worder extc intc .
  read table it_sum index 1.
  l_hours  = it_sum-hours   .
  l_worder = it_sum-worder  .
  l_ext    = it_sum-extc    .
  l_int    = it_sum-intc    .
  l_model  = it_sum-vm_model  .

  " Work Order Summarize in the same time terms.
  loop at it_sum.
    if l_hours = it_sum-hours and l_worder = it_sum-worder and
       l_ext   = it_sum-extc  and l_int = it_sum-intc   and
       l_model = it_sum-vm_model .
      l_cnt   = l_cnt + 1       .
      continue.
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_ext    l_int   into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_sum-vals                .
        lt_sum-vm_model   = l_model      .
        lt_sum-hours       = l_hours            .
        lt_sum-worder     = l_worder          .
        lt_sum-extc       = l_ext             .
        lt_sum-intc       = l_int             .
        lt_sum-cnt        = l_cnt             .
        lt_sum-code       = it_alc-full_alc   .
        append lt_sum      .
      endloop.
      lt_sum   = it_sum         .
      l_hours   = it_sum-hours    .
      l_worder = it_sum-worder  .
      l_ext    = it_sum-extc    .
      l_int    = it_sum-intc    .
      l_model  = it_sum-vm_model.
      l_cnt    = 1              .
    endif.
  endloop.

  describe table it_sum lines l_size .
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_ext    l_int   into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                         changing lt_sum-vals                .
      lt_sum-vm_model   = l_model      .
      lt_sum-worder     = l_worder          .
      lt_sum-extc       = l_ext             .
      lt_sum-intc       = l_int             .
      lt_sum-cnt        = l_cnt             .
      lt_sum-code       = it_alc-full_alc   .
      append       lt_sum      .
    endloop.
  endif.

  it_sum[] = lt_sum[].
endform.                    " read_alc

*&---------------------------------------------------------------------*
*&      Form  create_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SEQ  text
*      -->P_1320   text
*----------------------------------------------------------------------*
form create_data  tables pa_tab structure it_sum  using  pa_rp .
  data: l_data    like table of ztpp_input_plan      with header line.

  case pa_rp.
    when '00' or '01'.
      select * into      corresponding fields of table l_data
        from ztpp_input_plan
       where status = pa_rp .
    when '02' or '88'.
      select * into      corresponding fields of table l_data
        from ztpp_input_plan
       where status = '02'  .

      select * appending corresponding fields of table l_data
        from ztpp_input_plan
       where status = '03'  .
    when '06'.
      select * into      corresponding fields of table l_data
        from ztpp_input_plan
       where status = '04'  .

      select * appending corresponding fields of table l_data
        from ztpp_input_plan
       where status = '05'  .
    when '99'.
      select * into      corresponding fields of table l_data
        from ztpp_input_plan
       where status = '01'  .
  endcase.

  if p_test = 'X'.
    loop at l_data.
      if l_data-work_order+12(2) = 'XX' or
         l_data-work_order+12(2) = 'XY' .
        delete l_data.
      endif.
    endloop.
  endif.

  sort l_data by serial descending.
  loop at l_data.
    clear: pa_tab.
    pa_tab-vm_model   = l_data-modl       .
    pa_tab-vm_bodyser = l_data-body_ser   .
    pa_tab-worder     = l_data-work_order .
    pa_tab-mitu       = l_data-mitu       .
    pa_tab-extc       = l_data-extc       .
    pa_tab-intc       = l_data-intc       .
    pa_tab-status     = l_data-status     .
    pa_tab-rp         = '06'              .
    pa_tab-cnt        = 1                 .
    pa_tab-serial     = l_data-serial     .
    append pa_tab .
  endloop.
endform.                    " create_DATA

*&---------------------------------------------------------------------*
*&      Form  read_alc_D21
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_alc_d21 .
* DATA: lt_sum_d21          LIKE TABLE OF it_sum       WITH HEADER LINE,
*        l_model             LIKE it_sum-vm_model,
*        l_worder            LIKE mara-matnr  ,
*        l_hours             LIKE it_sum-hours,
*        l_wo                LIKE mara-matnr  ,
*        l_cnt               LIKE it_sum-cnt  ,
*        l_ext               LIKE it_sum-extc ,
*        l_int               LIKE it_sum-intc ,
*        l_size              LIKE it_sum-cnt  .
*
*  SORT it_sum_d21  BY hours worder extc intc .
*  READ TABLE it_sum_d21  INDEX 1.
*  l_hours = it_sum_d21-hours   .
*  l_wo    = it_sum_d21-worder  .
*  l_ext   = it_sum_d21-extc    .
*  l_int   = it_sum_d21-intc    .
*  l_model = it_sum_d21-vm_model  .
*
*  " Work Order Summarize in the same time terms.
*  LOOP AT it_sum_d21 .
*    IF l_hours = it_sum_d21-hours AND l_wo  = it_sum_d21-worder AND
*       l_ext   = it_sum_d21-extc  AND l_int = it_sum_d21-intc   AND
*       l_model = it_sum_d21-vm_model .
*      lt_sum_d21   = it_sum_d21      .
*      l_cnt   = l_cnt + 1            .
*      CONTINUE.
*    ELSE.
*      PERFORM get_wororder   USING it_alc-type_alc  l_worder
*               it_sum_d21-worder it_sum_d21-extc  it_sum_d21-intc .
*      PERFORM get_alc_value  USING l_worder         lt_sum_d21-code
*                          CHANGING lt_sum_d21-vals                  .
*      lt_sum_d21-cnt = l_cnt       .
*      APPEND       lt_sum_d21      .
*      lt_sum_d21  = it_sum_d21     .
*      l_cnt   = 1              .
*      l_hours = it_sum_prj-hours   .
*      l_wo    = it_sum_prj-worder  .
*      l_ext   = it_sum_prj-extc    .
*      l_int   = it_sum_prj-intc    .
*      l_model = it_sum_prj-vm_model.
*    ENDIF.
*  ENDLOOP.
*
*  DESCRIBE TABLE it_sum_d21  LINES l_size .
*  IF l_size > 0 .
*    PERFORM get_alc_value  USING lt_sum_d21-worder  lt_sum_d21-code
*                        CHANGING lt_sum_d21-vals                .
*    lt_sum_d21-cnt = l_cnt       .
*    APPEND       lt_sum_d21      .
*  ENDIF.
*
*  it_sum_d21[] = lt_sum_d21[].
endform.                    " read_alc_D21

*&---------------------------------------------------------------------*
*&      Form  CALC_SEQ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_seq.
  data: l_mitucnt            type i ,
        l_cnt                type i ,
        l_size               type i ,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_code               like it_seq-code ,
        l_hours(2)           type n           ,
        l_vals               like it_seq-vals ,
        lt_seq               like table of it_sum      with header line.

  " Create the Previous Production Data..
  describe table it_seq lines l_cnt  .
  check l_cnt > 0 .

  sort it_seq by hours  worder extc intc vm_model.
  read table it_seq  index 1.    clear: l_cnt, l_mitucnt.
  l_hours  = it_seq-hours   .
  l_worder = it_seq-worder  .
  l_extc   = it_seq-extc    .
  l_intc   = it_seq-intc    .
  l_model  = it_seq-vm_model.
  lt_seq   = it_seq         .

  loop at it_seq            .
    if l_hours  = it_seq-hours  and l_worder = it_seq-worder and
       l_extc  = it_seq-extc  and l_intc   = it_seq-intc   and
       l_model = it_seq-vm_model .
      l_cnt    = l_cnt  + 1          .
      if it_seq-mitu = 'Y'      .
        l_mitucnt = l_mitucnt + 1    .
      endif.
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_seq-vals                    .
        lt_seq-vm_model   = l_model      .
        lt_seq-worder     = l_worder          .
        lt_seq-extc       = l_extc            .
        lt_seq-intc       = l_intc            .
        lt_seq-cnt        = l_cnt             .
        lt_seq-mitucnt    = l_mitucnt         .
        lt_seq-code       = it_alc-full_alc   .
        append lt_seq               .
      endloop.
      lt_seq   = it_seq             .
      l_worder = it_seq-worder      .
      l_extc   = it_seq-extc        .
      l_intc   = it_seq-intc        .
      l_model  = it_seq-vm_model    .
      l_cnt    = 1                  .
      if it_seq-mitu = 'Y'      .
        l_mitucnt = 1    .
      else.
        clear: l_mitucnt.
      endif.
    endif.
  endloop.

  describe table it_seq  lines l_size.
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                          changing lt_seq-vals                    .
      lt_seq-vm_model   = l_model      .
      lt_seq-worder     = l_worder          .
      lt_seq-extc       = l_extc            .
      lt_seq-intc       = l_intc            .
      lt_seq-cnt        = l_cnt             .
      lt_seq-mitucnt    = l_mitucnt         .
      lt_seq-code       = it_alc-full_alc   .
      append lt_seq               .
    endloop.
  endif.

  " Summary ...
  sort lt_seq by vm_model code vals.
  clear: it_seq, it_seq[], l_cnt, l_mitucnt.
  read table lt_seq index 1.
  l_model =  lt_seq-vm_model  .
  l_code  =  lt_seq-code   .
  l_vals  =  lt_seq-vals   .
  it_seq  =  lt_seq        .

  loop at lt_seq.
    if l_model = lt_seq-vm_model and l_code = lt_seq-code and
       l_vals  = lt_seq-vals   .
      l_cnt   = l_cnt   + lt_seq-cnt  .
      l_mitucnt  = l_mitucnt  + lt_seq-mitucnt .
      continue.
    else.
      it_seq-vm_model = l_model .
      it_seq-cnt      = l_cnt   .
      it_seq-mitucnt  = l_mitucnt  .
      append it_seq          .
      it_seq  = lt_seq    .
      l_model = lt_seq-vm_model .
      l_code  = lt_seq-code  .
      l_vals  = lt_seq-vals  .
      l_cnt   = lt_seq-cnt  .
      l_mitucnt  = lt_seq-mitucnt .
    endif.
  endloop.

  describe table lt_seq lines l_size.
  if l_size > 0.
    it_seq-vm_model = l_model .
    it_seq-cnt  = l_cnt    .
    it_seq-mitucnt = l_mitucnt   .
    append it_seq         .
  endif.
endform.                    " CALC_SEQ

*&---------------------------------------------------------------------*
*&      Form  CALC_BODYINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_bodyinput.
  data: l_cnt                type i ,
        l_size               type i ,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_code               like it_seq-code ,
        l_hours(2)            type n           ,
        l_vals               like it_seq-vals ,
        lt_bi                like table of it_sum      with header line.

  describe table it_bi lines l_cnt  .
  check l_cnt > 0 .

  sort it_bi by hours  worder extc intc vm_model.
  read table it_bi   index 1.    clear: l_cnt.
  l_hours   = it_bi-hours    .
  l_worder = it_bi-worder  .
  l_extc   = it_bi-extc    .
  l_intc   = it_bi-intc    .
  l_model  = it_bi-vm_model.
  lt_bi    = it_bi         .

  loop at it_bi             .
    if l_hours  = it_bi-hours  and l_worder = it_bi-worder and
       l_extc   = it_bi-extc  and l_intc = it_bi-intc   and
       l_model = it_bi-vm_model .
      l_cnt    = l_cnt  + 1          .
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_bi-vals                    .
        lt_bi-vm_model   = l_model      .
        lt_bi-worder     = l_worder          .
        lt_bi-extc       = l_extc            .
        lt_bi-intc       = l_intc            .
        lt_bi-cnt        = l_cnt             .
        lt_bi-code       = it_alc-full_alc   .
        append lt_bi               .
      endloop.
      lt_bi    = it_bi             .
      l_worder = it_bi-worder      .
      l_extc   = it_bi-extc        .
      l_intc   = it_bi-intc        .
      l_model  = it_bi-vm_model    .
      l_cnt    = 1                  .
    endif.
  endloop.

  describe table it_bi   lines l_size .
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                          changing lt_bi-vals                    .
      lt_bi-vm_model   = l_model      .
      lt_bi-worder     = l_worder          .
      lt_bi-extc       = l_extc            .
      lt_bi-intc       = l_intc            .
      lt_bi-cnt        = l_cnt             .
      lt_bi-code       = it_alc-full_alc   .
      append lt_bi                .
    endloop.
  endif.

  " Summary ...
  sort lt_bi  by vm_model code vals.
  clear: it_bi, it_bi[], l_cnt.
  read table lt_bi  index 1.
  l_model =  lt_bi-vm_model  .
  l_code  =  lt_bi-code   .
  l_vals  =  lt_bi-vals   .
  it_bi   =  lt_bi        .

  loop at lt_bi .
    if l_model = lt_bi-vm_model and l_code = lt_bi-code and
       l_vals  = lt_bi-vals   .
      l_cnt   = l_cnt   + lt_bi-cnt  .
      continue.
    else.
      it_bi-vm_model = l_model .
      it_bi-cnt   = l_cnt   .
      append it_bi           .
      it_bi   = lt_bi    .
      l_model = lt_bi-vm_model .
      l_code  = lt_bi-code  .
      l_vals  = lt_bi-vals  .
      l_cnt   = lt_bi-cnt  .
    endif.
  endloop.

  describe table lt_bi  lines l_size.
  if l_size > 0.
    it_bi-vm_model = l_model .
    it_bi-cnt  = l_cnt    .
    append it_bi          .
  endif.
endform.                    " CALC_BODYINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_WBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_wbs.
  data: l_mitucnt            type i ,
        l_cnt                type i ,
        l_size               type i ,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_code               like it_seq-code ,
        l_hours(2)            type n           ,
        l_vals               like it_seq-vals ,
        lt_wbs               like table of it_sum      with header line.

  describe table it_wbs lines l_cnt  .
  check l_cnt > 0 .

  l_mitucnt = p_wbs * wa_uph_b.
  if l_mitucnt > 0 .
    if l_mitucnt < l_cnt .
      delete it_wbs to l_mitucnt.
    else.
      delete it_wbs to l_cnt    .
    endif.
  else.
    delete it_wbs to l_cnt .
  endif.
  clear: l_mitucnt, l_cnt.

  sort it_wbs by hours  worder extc intc vm_model.
  read table it_wbs  index 1.    clear: l_cnt.
  l_hours   = it_wbs-hours    .
  l_worder = it_wbs-worder  .
  l_extc   = it_wbs-extc    .
  l_intc   = it_wbs-intc    .
  l_model  = it_wbs-vm_model.
  lt_wbs   = it_wbs         .

  loop at it_wbs            .
    if l_hours  = it_wbs-hours  and l_worder  = it_wbs-worder and
       l_extc   = it_wbs-extc  and l_intc = it_wbs-intc   and
       l_model = it_wbs-vm_model .
      l_cnt    = l_cnt  + 1          .
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_wbs-vals                    .
        lt_wbs-vm_model   = l_model      .
        lt_wbs-worder     = l_worder          .
        lt_wbs-extc       = l_extc            .
        lt_wbs-intc       = l_intc            .
        lt_wbs-cnt        = l_cnt             .
        lt_wbs-code       = it_alc-full_alc   .
        append lt_wbs               .
      endloop.
      lt_wbs   = it_wbs             .
      l_worder = it_wbs-worder      .
      l_extc   = it_wbs-extc        .
      l_intc   = it_wbs-intc        .
      l_model  = it_wbs-vm_model    .
      l_cnt    = 1                  .
    endif.
  endloop.

  describe table it_wbs  lines l_size.
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                          changing lt_wbs-vals                    .
      lt_wbs-vm_model   = l_model      .
      lt_wbs-worder     = l_worder          .
      lt_wbs-extc       = l_extc            .
      lt_wbs-intc       = l_intc            .
      lt_wbs-cnt        = l_cnt             .
      lt_wbs-code       = it_alc-full_alc   .
      append lt_wbs               .
    endloop.
  endif.

  " Summary ...
  sort lt_wbs by vm_model code vals.
  clear: it_wbs, it_wbs[], l_cnt.
  read table lt_wbs index 1.
  l_model =  lt_wbs-vm_model  .
  l_code  =  lt_wbs-code   .
  l_vals  =  lt_wbs-vals   .
  it_wbs  =  lt_wbs        .

  loop at lt_wbs.
    if l_model = lt_wbs-vm_model and l_code = lt_wbs-code and
       l_vals  = lt_wbs-vals   .
      l_cnt   = l_cnt   + lt_wbs-cnt  .
      continue.
    else.
      it_wbs-vm_model = l_model .
      it_wbs-cnt   = l_cnt   .
      append it_wbs          .
      it_wbs  = lt_wbs    .
      l_model = lt_wbs-vm_model .
      l_code  = lt_wbs-code  .
      l_vals  = lt_wbs-vals  .
      l_cnt   = lt_wbs-cnt  .
    endif.
  endloop.

  describe table lt_wbs lines l_size.
  if l_size > 0.
    it_wbs-vm_model = l_model .
    it_wbs-cnt  = l_cnt    .
    append it_wbs         .
  endif.
endform.                    " CALC_WBS

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTINPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_paintinput.
  data: l_cnt                type i ,
        l_size               type i ,
        l_atinn              like cabn-atinn,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_hours(2)            type n           ,
        l_code               like it_seq-code ,
        l_vals               like it_seq-vals ,
        lt_pi                like table of it_sum      with header line.

  describe table it_pi lines l_cnt  .
  check l_cnt > 0 .

  sort it_pi by hours  worder extc intc vm_model.
  read table it_pi   index 1.    clear: l_cnt.
  l_hours   = it_pi-hours    .
  l_worder = it_pi-worder  .
  l_extc   = it_pi-extc    .
  l_intc   = it_pi-intc    .
  l_model  = it_pi-vm_model.
  lt_pi    = it_pi         .

  loop at it_pi             .
    if l_hours  = it_pi-hours  and l_worder  = it_pi-worder and
       l_extc   = it_pi-extc  and l_intc = it_pi-intc   and
       l_model = it_pi-vm_model .
      l_cnt    = l_cnt  + 1          .
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_pi-vals                    .
        lt_pi-vm_model   = l_model      .
        lt_pi-worder     = l_worder          .
        lt_pi-extc       = l_extc            .
        lt_pi-intc       = l_intc            .
        lt_pi-cnt        = l_cnt             .
        lt_pi-code       = it_alc-full_alc   .
        append lt_pi               .
      endloop.
      lt_pi    = it_pi             .
      l_worder = it_pi-worder      .
      l_extc   = it_pi-extc        .
      l_intc   = it_pi-intc        .
      l_model  = it_pi-vm_model    .
      l_cnt    = 1                  .
    endif.
  endloop.

  describe table it_pi   lines l_size.
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                          changing lt_pi-vals                    .
      lt_pi-vm_model   = l_model      .
      lt_pi-worder     = l_worder          .
      lt_pi-extc       = l_extc            .
      lt_pi-intc       = l_intc            .
      lt_pi-cnt        = l_cnt             .
      lt_pi-code       = it_alc-full_alc   .
      append lt_pi                .
    endloop.
  endif.

  " Summary ...
  sort lt_pi  by vm_model code vals.
  clear: it_pi, it_pi[], l_cnt.
  read table lt_pi  index 1.
  l_model =  lt_pi-vm_model  .
  l_code  =  lt_pi-code   .
  l_vals  =  lt_pi-vals   .
  it_pi   =  lt_pi        .

  loop at lt_pi .
    if l_model = lt_pi-vm_model and l_code = lt_pi-code and
       l_vals  = lt_pi-vals   .
      l_cnt   = l_cnt   + lt_pi-cnt  .
      continue.
    else.
      it_pi-vm_model = l_model .
      it_pi-cnt   = l_cnt   .
      append it_pi           .
      it_pi   = lt_pi    .
      l_model = lt_pi-vm_model .
      l_code  = lt_pi-code  .
      l_vals  = lt_pi-vals  .
      l_cnt   = lt_pi-cnt  .
    endif.
  endloop.

  describe table lt_pi  lines l_size.
  if l_size > 0.
    it_pi-vm_model = l_model .
    it_pi-cnt  = l_cnt    .
    append it_pi          .
  endif.
endform.                    " CALC_PAINTINPUT

*&---------------------------------------------------------------------*
*&      Form  CALC_PAINTREJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_paintreject.
  data: l_mitucnt            type i ,
        l_cnt                type i ,
        l_size               type i ,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_hours(2)            type n           ,
        l_code               like it_seq-code ,
        l_vals               like it_seq-vals ,
        lt_prj               like table of it_sum      with header line.

  describe table it_prj lines l_cnt  .
  check l_cnt > 0 .

  l_mitucnt = p_prj * wa_uph_p.
  if l_mitucnt > 0 .
    if l_mitucnt < l_cnt .
      delete it_prj to l_mitucnt.
    else.
      delete it_prj to l_cnt    .
    endif.
  else.
    delete it_prj to l_cnt.
  endif.
  clear: l_mitucnt, l_cnt.

  sort it_prj by hours  worder extc intc vm_model.
  read table it_prj  index 1.    clear: l_cnt.
  l_hours  = it_prj-hours    .
  l_worder = it_prj-worder  .
  l_extc   = it_prj-extc    .
  l_intc   = it_prj-intc    .
  l_model  = it_prj-vm_model.
  lt_prj   = it_prj         .

  loop at it_prj            .
    if l_hours  = it_prj-hours  and l_worder = it_prj-worder and
       l_extc   = it_prj-extc  and l_intc = it_prj-intc   and
       l_model = it_prj-vm_model .
      l_cnt    = l_cnt  + 1          .
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_prj-vals                    .
        lt_prj-vm_model   = l_model      .
        lt_prj-worder     = l_worder          .
        lt_prj-extc       = l_extc            .
        lt_prj-intc       = l_intc            .
        lt_prj-cnt        = l_cnt             .
        lt_prj-code       = it_alc-full_alc   .
        append lt_prj               .
      endloop.
      lt_prj   = it_prj             .
      l_worder = it_prj-worder      .
      l_extc   = it_prj-extc        .
      l_intc   = it_prj-intc        .
      l_model  = it_prj-vm_model    .
      l_cnt    = 1                  .
    endif.
  endloop.

  describe table it_prj  lines l_size .
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                          changing lt_prj-vals                    .
      lt_prj-vm_model   = l_model      .
      lt_prj-worder     = l_worder          .
      lt_prj-extc       = l_extc            .
      lt_prj-intc       = l_intc            .
      lt_prj-cnt        = l_cnt             .
      lt_prj-code       = it_alc-full_alc   .
      append lt_prj               .
    endloop.
  endif.

  " Summary ...
  sort lt_prj by vm_model code vals.
  clear: it_prj, it_prj[], l_cnt.
  read table lt_prj index 1.
  l_model =  lt_prj-vm_model  .
  l_code  =  lt_prj-code   .
  l_vals  =  lt_prj-vals   .
  it_prj  =  lt_prj        .

  loop at lt_prj.
    if l_model = lt_prj-vm_model and l_code = lt_prj-code and
       l_vals  = lt_prj-vals   .
      l_cnt   = l_cnt   + lt_prj-cnt  .
      continue.
    else.
      it_prj-vm_model = l_model .
      it_prj-cnt   = l_cnt   .
      append it_prj          .
      it_prj  = lt_prj    .
      l_model = lt_prj-vm_model .
      l_code  = lt_prj-code  .
      l_vals  = lt_prj-vals  .
      l_cnt   = lt_prj-cnt  .
    endif.
  endloop.

  describe table lt_prj lines l_size.
  if l_size > 0.
    it_prj-vm_model = l_model .
    it_prj-cnt  = l_cnt    .
    append it_prj         .
  endif.
endform.                    " CALC_PAINTREJECT

*&---------------------------------------------------------------------*
*&      Form  CALC_PBS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_pbs.
  data: l_cnt                type i ,
        l_size               type i ,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_hours(2)            type n           ,
        l_code               like it_seq-code ,
        l_vals               like it_seq-vals ,
        lt_pbs               like table of it_sum      with header line.

  describe table it_pbs lines l_cnt  .
  check l_cnt > 0 .

  sort it_pbs by hours  worder extc intc vm_model.
  read table it_pbs  index 1.    clear: l_cnt.
  l_hours   = it_pbs-hours    .
  l_worder = it_pbs-worder  .
  l_extc   = it_pbs-extc    .
  l_intc   = it_pbs-intc    .
  l_model  = it_pbs-vm_model.
  lt_pbs   = it_pbs         .

  loop at it_pbs            .
    if l_hours  = it_pbs-hours  and l_worder  = it_pbs-worder and
       l_extc   = it_pbs-extc  and l_intc = it_pbs-intc   and
       l_model = it_pbs-vm_model .
      l_cnt    = l_cnt  + 1          .
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder        it_alc-full_alc
                            changing lt_pbs-vals                    .
        lt_pbs-vm_model   = l_model      .
        lt_pbs-worder     = l_worder          .
        lt_pbs-extc       = l_extc            .
        lt_pbs-intc       = l_intc            .
        lt_pbs-cnt        = l_cnt             .
        lt_pbs-code       = it_alc-full_alc   .
        append lt_pbs               .
      endloop.
      lt_pbs   = it_pbs             .
      l_worder = it_pbs-worder      .
      l_extc   = it_pbs-extc        .
      l_intc   = it_pbs-intc        .
      l_model  = it_pbs-vm_model    .
      l_cnt    = 1                  .
    endif.
  endloop.

  describe table it_pbs  lines l_size.
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder        it_alc-full_alc
                          changing lt_pbs-vals                    .
      lt_pbs-vm_model   = l_model      .
      lt_pbs-worder     = l_worder          .
      lt_pbs-extc       = l_extc            .
      lt_pbs-intc       = l_intc            .
      lt_pbs-cnt        = l_cnt             .
      lt_pbs-code       = it_alc-full_alc   .
      append lt_pbs               .
    endloop.
  endif.

  " Summary ...
  sort lt_pbs by vm_model code vals.
  clear: it_pbs, it_pbs[], l_cnt.
  read table lt_pbs index 1.
  l_model =  lt_pbs-vm_model  .
  l_code  =  lt_pbs-code   .
  l_vals  =  lt_pbs-vals   .
  it_pbs  =  lt_pbs        .

  loop at lt_pbs.
    if l_model = lt_pbs-vm_model and l_code = lt_pbs-code and
       l_vals  = lt_pbs-vals   .
      l_cnt   = l_cnt   + lt_pbs-cnt  .
      continue.
    else.
      it_pbs-vm_model = l_model .
      it_pbs-cnt   = l_cnt   .
      append it_pbs          .
      it_pbs  = lt_pbs    .
      l_model = lt_pbs-vm_model .
      l_code  = lt_pbs-code  .
      l_vals  = lt_pbs-vals  .
      l_cnt   = lt_pbs-cnt  .
    endif.
  endloop.

  describe table lt_pbs lines l_size.
  if l_size > 0.
    it_pbs-vm_model = l_model .
    it_pbs-cnt  = l_cnt    .
    append it_pbs         .
  endif.
endform.                    " CALC_PBS

*&---------------------------------------------------------------------*
*&      Form  GET_WORORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ALC_TYPE_ALC  text
*      -->P_L_WORDER  text
*----------------------------------------------------------------------*
form get_wororder using  pa_type  pa_worder  pa_wo  pa_ext  pa_int .
  if pa_type = 'C'.
    concatenate pa_wo  pa_ext  pa_int  into  pa_worder .
  else.
    pa_worder = pa_wo .
  endif.
endform.                    " GET_WORORDER

*&---------------------------------------------------------------------*
*&      Form  INSERT_FIELD_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_field_vals.
  " Insert D-1 Field Value into Internal Table IT_DISP...
  loop at it_disp.
    read table it_disp_d1 with key model    = it_disp-model
                                   alc_code = it_disp-alc_code
                                   alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-d_1 = it_disp_d1-d_1 .
      delete it_disp_d1 where alc_code = it_disp-alc_code
                          and  model   = it_disp-model
                          and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-d_1 .
    endif.

    read table it_disp_seq  with key  model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-seq = it_disp_seq-d_1 .
      it_disp-mitu = it_disp_seq-mitu.
      delete it_disp_seq  where alc_code = it_disp-alc_code
                            and model    = it_disp-model
                            and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-seq, it_disp-mitu.
    endif.

    read table it_disp_bi   with key model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-bodyin = it_disp_bi-d_1 .
      delete it_disp_bi   where alc_code = it_disp-alc_code
                            and model    = it_disp-model
                            and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-bodyin  .
    endif.

    read table it_disp_wbs  with key model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-wbs = it_disp_wbs-d_1 .
      delete it_disp_wbs  where alc_code = it_disp-alc_code
                            and model    = it_disp-model
                            and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-wbs .
    endif.

    read table it_disp_pi   with key model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-paint  = it_disp_pi-d_1 .
      delete it_disp_pi   where alc_code = it_disp-alc_code
                            and model    = it_disp-model
                            and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-paint  .
    endif.

    read table it_disp_prj  with key model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-prj = it_disp_prj-d_1 .
      delete it_disp_prj  where alc_code = it_disp-alc_code
                            and model    = it_disp-model
                            and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-prj .
    endif.

    read table it_disp_pbs  with key model    = it_disp-model
                                     alc_code = it_disp-alc_code
                                     alc_vals = it_disp-alc_vals .
    if sy-subrc = 0.
      it_disp-pbs = it_disp_pbs-d_1 .
      delete it_disp_pbs  where alc_code = it_disp-alc_code
                            and model    = it_disp-model
                            and alc_vals = it_disp-alc_vals .
    else.
      clear: it_disp-pbs .
    endif.

    modify it_disp.
  endloop.

  " Remain Fiedls Insert.....
  loop at it_disp_d1 .
    clear: it_disp.
    move-corresponding it_disp_d1 to it_disp.
    clear: it_disp-d_1, it_disp-d_1.
    it_disp-d_1 = it_disp_d1-d_1.
    append it_disp.
  endloop.
  clear: it_disp_d1, it_disp_d1[].

  loop at it_disp_seq  .
    clear: it_disp.
    move-corresponding it_disp_seq  to it_disp.
    clear: it_disp-d_1, it_disp-seq, it_disp-mitu.
    it_disp-seq = it_disp_seq-d_1.
    it_disp-mitu = it_disp_seq-mitu.
    append it_disp.
  endloop.
  clear: it_disp_seq , it_disp_seq[].

  loop at it_disp_bi   .
    clear: it_disp.
    move-corresponding it_disp_bi   to it_disp.
    clear: it_disp-d_1, it_disp-bodyin.
    it_disp-bodyin = it_disp_bi-d_1.
    append it_disp.
  endloop.
  clear: it_disp_bi  , it_disp_bi[].

  loop at it_disp_wbs  .
    clear: it_disp.
    move-corresponding it_disp_wbs  to it_disp.
    clear: it_disp-d_1, it_disp-wbs.
    it_disp-wbs = it_disp_wbs-d_1.
    append it_disp.
  endloop.
  clear: it_disp_wbs , it_disp_wbs[].

  loop at it_disp_pi   .
    clear: it_disp.
    move-corresponding it_disp_pi   to it_disp.
    clear: it_disp-d_1, it_disp-paint.
    it_disp-paint  = it_disp_pi-d_1.
    append it_disp.
  endloop.
  clear: it_disp_pi  , it_disp_pi[].

  loop at it_disp_prj  .
    clear: it_disp.
    move-corresponding it_disp_prj  to it_disp.
    clear: it_disp-d_1, it_disp-prj.
    it_disp-prj = it_disp_prj-d_1.
    append it_disp.
  endloop.
  clear: it_disp_prj , it_disp_prj[].

  loop at it_disp_pbs  .
    clear: it_disp.
    move-corresponding it_disp_pbs  to it_disp.
    clear: it_disp-d_1, it_disp-pbs.
    it_disp-pbs = it_disp_pbs-d_1.
    append it_disp.
  endloop.
  clear: it_disp_pbs , it_disp_pbs[].
endform.                    " INSERT_FIELD_VALS

*&---------------------------------------------------------------------*
*&      Form  calc_prod
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form calc_prod.
  data: l_cnt                type i ,
        l_size               type i ,
        l_model              like it_seq-vm_model,
        l_worder             like mara-matnr  ,
        l_extc               like ztpp_input_plan-extc,
        l_intc               like ztpp_input_plan-intc,
        l_code               like it_seq-code ,
        l_vals               like it_seq-vals ,
        lt_d1                like table of it_sum      with header line.

  "1. Take the data of The Table(IT_PROD) of which RP06's Qty is GT 0.
  delete it_prod where rp06q = 0 .

  "2. Conversion the Data for the Internal Table Format..
  loop at it_prod.
    clear: it_d1 .
    concatenate it_prod-wo_ser it_prod-nation it_prod-dealer
                it_prod-extc   it_prod-intc   into l_worder .
    perform get_model using l_worder  it_d1-vm_model .
    do it_prod-rp06q times.
      it_d1-worder     = l_worder          .
      it_d1-extc       = it_prod-extc      .
      it_d1-intc       = it_prod-intc      .
      it_d1-rp         = '06'              .
      it_d1-cnt        = 1                 .
      append it_d1  .
    enddo.
  endloop.

  " Create the Previous Production Data..
  sort it_d1 by hours  worder extc intc vm_model.
  read table  it_d1 index 1.   clear: l_cnt.
  l_worder = it_d1-worder  .
  l_extc   = it_d1-extc    .
  l_intc   = it_d1-intc    .
  l_model  = it_d1-vm_model.
  lt_d1    = it_d1         .

  loop at it_d1.
    if l_worder = it_d1-worder and  l_extc   = it_d1-extc   and
       l_intc   = it_d1-intc   and  l_model  = it_d1-vm_model .
      l_cnt    = l_cnt  + 1.
      continue .
    else.
      loop at it_alc where model = l_model .
        concatenate  l_worder  l_extc   l_intc  into l_worder       .
        perform get_alc_value  using l_worder      it_alc-full_alc
                            changing lt_d1-vals                    .
        lt_d1-vm_model   = l_model           .
        lt_d1-worder     = l_worder          .
        lt_d1-extc       = l_extc            .
        lt_d1-intc       = l_intc            .
        lt_d1-cnt        = l_cnt             .
        lt_d1-code       = it_alc-full_alc   .
        append lt_d1                .
      endloop.
      lt_d1    = it_d1              .
      l_worder = it_d1-worder      .
      l_extc   = it_d1-extc        .
      l_intc   = it_d1-intc        .
      l_model  = it_d1-vm_model    .
      l_cnt    = 1                  .
    endif.
  endloop.

  describe table it_d1 lines l_size .
  if l_size > 0 .
    loop at it_alc where model = l_model .
      concatenate  l_worder  l_extc   l_intc  into l_worder       .
      perform get_alc_value  using l_worder      it_alc-full_alc
                          changing lt_d1-vals                    .
      lt_d1-vm_model   = l_model           .
      lt_d1-worder     = l_worder          .
      lt_d1-extc       = l_extc            .
      lt_d1-intc       = l_intc            .
      lt_d1-cnt        = l_cnt             .
      lt_d1-code       = it_alc-full_alc   .
      append lt_d1                .
    endloop.
  endif.

  " Summary ...
  sort lt_d1 by vm_model code vals.
  clear: it_d1, it_d1[], l_cnt.
  read table lt_d1 index 1.
  l_model =  lt_d1-vm_model  .
  l_code  =  lt_d1-code   .
  l_vals  =  lt_d1-vals   .
  it_d1   =  lt_d1        .

  loop at lt_d1.
    if l_model = lt_d1-vm_model and l_code = lt_d1-code and
       l_vals  = lt_d1-vals   .
      l_cnt   = l_cnt   + lt_d1-cnt  .
      continue.
    else.
      it_d1-vm_model = l_model .
      it_d1-cnt   = l_cnt   .
      append it_d1          .
      it_d1   = lt_d1    .
      l_model = lt_d1-vm_model .
      l_code  = lt_d1-code  .
      l_vals  = lt_d1-vals  .
      l_cnt   = lt_d1-cnt  .
    endif.
  endloop.

  describe table lt_d1 lines l_size.
  if l_size > 0.
    it_d1-vm_model = l_model .
    it_d1-cnt  = l_cnt    .
    append it_d1         .
  endif.
endform.                    " calc_prod

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
form read_working_date using  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option               = pa_type
      date                         = pa_wdate
      factory_calendar_id          = pa_kalid
    IMPORTING
      date                         = pa_wdate
    EXCEPTIONS
      calendar_buffer_not_loadable = 1
      correct_option_invalid       = 2
      date_after_range             = 3
      date_before_range            = 4
      date_invalid                 = 5
      factory_calendar_not_found   = 6
      others                       = 7.
endform.                    " READ_WORKING_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
form read_shop_calid using    pa_kalid.
  select single kalid into pa_kalid
    from zvpp_capacity
   where arbpl = 'T'   .
endform.                    " READ_SHOP_CALID

*&---------------------------------------------------------------------*
*&      Form  GET_MODELS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_models.
  data: l_atwrt          like ausp-atwrt,
        l_atinn          like cabn-atinn.

  select single atinn into l_atinn
    from cabn
   where atnam = 'P_MODEL'.

  select n~atwrt into l_atwrt
    from cawn as n inner join cawnt as t
      on n~atinn = t~atinn
     and n~atzhl = t~atzhl
   where n~atinn = l_atinn
     and t~spras = sy-langu .
    it_model-modl = l_atwrt(3).
    append it_model .
  endselect.
endform.                    " GET_MODELS

*&---------------------------------------------------------------------*
*&      Form  SET_INFORMATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_information.
  data: l_check              type c           ,
        l_chk                type p decimals 3,
        l_tot                type p decimals 3,
        l_htot               type p decimals 3,
        l_date               type d ,
        l_count              type i .

  " Set the BASIC Information for the UPH & Work Time...
  " Gather the 3 day's UPH Except shift...
  clear: l_date, l_count.        l_date = p_dates - 1 .

  do 3 times.
    l_count  = l_count + 1.      clear: l_chk.
    l_date   = l_date  + 1.
    perform read_working_date using '+'  wa_kalid  l_date.

* -- start
*    perform get_day          using l_date it_master-day  .
*    perform get_working_time using l_date it_master-time it_master-day.
*    perform get_uph          using l_date it_master-uph it_master-shift.
*    if l_check = space.
*      perform get_uph_shop   using l_date  wa_uph_t   'T'              .
*      perform get_uph_shop   using l_date  wa_uph_b   'B'              .
*      perform get_uph_shop   using l_date  wa_uph_p   'P'              .
*      l_check = 'X'.
*    endif.
*    it_master-tqty = it_master-uph * ( it_master-time / 3600 ).
*-- end replaced with function

    call function 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        I_DATE        = l_date
        I_SHOP        = 'T'
      IMPORTING
        E_QTY         = it_master-tqty.


    it_master-seq    = l_count.
    it_master-date   = l_date .
    append it_master.  clear: it_master.
  enddo.

  clear: l_date, l_count, wa_hour.  l_date = p_dates - 1  .
  do 3 times   .
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    perform read_working_date using '+'  wa_kalid  l_date .
    perform get_day       using l_date it_shift-day  .
    perform get_uph       using l_date it_shift-uph it_shift-shift.
    it_shift-seq    = l_count .   it_shift-date   = l_date .

    perform get_worktime1 using l_date        l_count
                                it_shift-time it_shift-day .

    clear: it_shift.

  enddo.

  " Set the Day's Total Quantity(IT_MASTER, IT_SHIFT)
  data: wa_shift like it_shift, l_idx like sy-tabix.
  sort it_shift by ser seq shift .
  loop at it_master.
    clear: l_chk, l_tot, l_htot  .

    loop at it_shift where seq = it_master-seq .
      l_tot = l_tot + it_shift-tqty            .
      wa_shift = it_shift.
      l_idx = sy-tabix.
    endloop.
    l_chk = it_master-tqty - l_tot           .
    if l_chk <> 0.                                          "7/31/2012
*FIXIT LATER
*      wa_shift-tqty = wa_shift-tqty + l_chk  .
*      wa_shift-hqty = wa_shift-tqty / wa_shift-hloop.
*      modify it_shift index l_idx from wa_shift transporting tqty hqty.
    endif.

  endloop.

endform.                    " SET_INFORMATION

*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
form get_working_time using    pa_wdate  pa_wktime  pa_day.
  data: l_wtime       like zvpp_capacity-endzt ,
        l_date        type d ,
        l_einzt       like tc37a-einzt ,
        lt_capa       like table of zvpp_capacity      with header line.

  clear: lt_capa, lt_capa[], l_wtime.
  select * into table lt_capa
    from zvpp_capacity
   where arbpl = 'T'
     and datub >= pa_wdate .

  sort lt_capa by datub .
  read table lt_capa index 1.
  l_date = lt_capa-datub    .

  loop at lt_capa where datub = l_date and tagnr = pa_day .
    clear: l_einzt.
    select single einzt into l_einzt
      from tc37a
     where schgrup  = lt_capa-mosid
       and kaptprog = lt_capa-tprog
       and endda   >= pa_wdate
       and begda   <= pa_wdate     .
    l_wtime = l_wtime + l_einzt    .
  endloop.
  pa_wktime = l_wtime .
endform.                    " GET_WORKING_TIME

*&---------------------------------------------------------------------*
*&      Form  GET_WORKTIME1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_WKTIME  text
*----------------------------------------------------------------------*
form get_worktime1 using    pa_wdate  pa_cnt  pa_wktime  pa_day.
  data: l_wtime       like zvpp_capacity-endzt ,
        l_chk         type p decimals 3        ,
        l_tc37a       like tc37a               ,
        l_date        type d ,
        l_flag        type c ,
        l_einzt       like tc37a-einzt ,
        lt_capa       like table of zvpp_capacity      with header line.

  clear: lt_capa, lt_capa[], l_wtime.
  select * into table lt_capa
    from zvpp_capacity
   where arbpl = 'T'
     and datub >= pa_wdate .

  sort lt_capa by datub tagnr schnr .
  read table lt_capa index 1.
  l_date = lt_capa-datub    .

  loop at lt_capa where datub = l_date and tagnr = pa_day .
    wa_hour = wa_hour + 1 .
    clear: l_einzt.
    select single * into l_tc37a       " l_einzt
      from tc37a
     where schgrup  = lt_capa-mosid
       and kaptprog = lt_capa-tprog
       and endda   >= pa_wdate
       and begda   <= pa_wdate     .
    it_shift-ser   = wa_hour       .
    it_shift-time  = l_tc37a-einzt .
    if l_tc37a-begzt >= l_tc37a-endzt.
      it_shift-total = l_tc37a-endzt +   l_tc37a-begzt            .
    else.
      it_shift-total = l_tc37a-endzt -   l_tc37a-begzt            .
    endif.
    it_shift-ftime = l_tc37a-begzt .
    it_shift-shift = lt_capa-schnr .

*7/31/12 requested by MY Hur (ceil -> floor)
    call function 'Z_FPP_SHIFT_PLAN_QTY'
      EXPORTING
        I_DATE        = pa_wdate
        I_SHOP        = 'T'
        I_SHIFT       = it_shift-shift
      IMPORTING
        E_QTY         = it_shift-tqty.
*    it_shift-tqty  =  floor( it_shift-uph * ( it_shift-time / 3600 ) ).
    it_shift-hqty  = floor( it_shift-tqty / ( it_shift-time / 7200 ) ).

    it_shift-hloop = it_shift-time / 7200 .
    append it_shift.
  endloop.
endform.                    " GET_WORKTIME1

*&---------------------------------------------------------------------*
*&      Form  GET_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_DAY  text
*----------------------------------------------------------------------*
form get_day using    pa_wdate  pa_day.
  data: l_day         like scal-indicator .

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      date = pa_wdate
    IMPORTING
      day  = l_day.

  pa_day = l_day.
endform.                    " GET_DAY

*&---------------------------------------------------------------------*
*&      Form  GET_UPH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*      -->P_IT_MASTER_UPH  text
*----------------------------------------------------------------------*
form get_uph using    pa_wdate  pa_uph  pa_shift .
  data: w_uph  like ztpp_status-uph.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
      shift = pa_shift
      shop  = 'T'
    IMPORTING
      uph   = w_uph.
  pa_uph  = w_uph.

*  DATA lw_ld          LIKE zvpp_ld .
*data: lt_ld   like zvpp_ld occurs 0 with header line.
*
*  IF pa_shift IS INITIAL .
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND arbpl     = 'T'      .
*
** end of change on 06/13/2005
*  ELSE.
** requested by MY HUR changed by chris
** because two shift could exist, read one record
** only one shift is calculated
** and one shift could have more than one record
** to difine diferent rate for different period
** of time
**    SELECT SINGLE * INTO lw_ld
**      FROM zvpp_ld
**     WHERE ld_perst <= pa_wdate
**       AND ld_pered >= pa_wdate
**       AND ld_shift  = pa_shift
**       AND arbpl     = 'T'      .
*    SELECT * INTO table lt_ld
*      FROM zvpp_ld
*     WHERE ld_perst <= pa_wdate
*       AND ld_pered >= pa_wdate
*       AND ld_shift  = pa_shift
*       AND arbpl     = 'T'      .
*
*
*  ENDIF.
*
** add by chris on 06/13/2005
*    loop at lt_ld.
*      lw_ld-lrate = lw_ld-lrate + lt_ld-lrate.
*      lw_ld-lantu = lw_ld-lantu + lt_ld-lantu.
*    endloop.
** end of add.
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
endform.                    " GET_UPH

*&---------------------------------------------------------------------*
*&      Form  GET_START_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
form get_start_day using    pa_datum.
  clear: ztpp_common_vals.
  select single *
    from ztpp_common_vals
   where jobs = c_jobs
     and key2 = c_key1 .

  pa_datum = ztpp_common_vals-dates.
endform.                    " GET_START_DAY

*&---------------------------------------------------------------------*
*&      Form  CLEAR_QTY_DISP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DISP_PRJ  text
*----------------------------------------------------------------------*
form clear_qty_disp using    pa_disp  structure it_disp .
  clear: pa_disp-h02, pa_disp-h04, pa_disp-h06, pa_disp-h08,
         pa_disp-h10, pa_disp-h12, pa_disp-h14, pa_disp-h16,
         pa_disp-h18, pa_disp-h20, pa_disp-h22, pa_disp-h24,
         pa_disp-h26, pa_disp-h28, pa_disp-h30, pa_disp-h32,
         pa_disp-h34, pa_disp-h36, pa_disp-h38, pa_disp-h40,
         pa_disp-h42, pa_disp-h44, pa_disp-h46, pa_disp-h48,
         pa_disp-h50, pa_disp-h52, pa_disp-h54, pa_disp-h56,
         pa_disp-h58, pa_disp-h60, pa_disp-h62, pa_disp-h64,
         pa_disp-h66, pa_disp-h68, pa_disp-h70, pa_disp-h72,
         pa_disp-mitu,              "Missed      UD1K912950
         pa_disp-stot,              "Missed      UD1K912950
                      pa_disp-d_1, pa_disp-seq, pa_disp-bodyin,
         pa_disp-wbs, pa_disp-prj, pa_disp-pbs, pa_disp-paint.
endform.                    " CLEAR_QTY_DISP

*&---------------------------------------------------------------------*
*&      Form  GET_MODEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_WORDER  text
*      -->P_IT_D1_VM_MODEL  text
*----------------------------------------------------------------------*
form get_model using    pa_worder   pa_model.
  data: lt_val          like table of zspp_vin_value   with header line.

  lt_val-atnam = 'P_MODEL'.  append lt_val.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      object       = pa_worder
      ctype        = '001'
*     DISPLAY      = 'D'
    TABLES
      val_table    = lt_val
    EXCEPTIONS
      no_data      = 1
      error_mode   = 2
      error_object = 3
      error_value  = 4
      others       = 5.

  read table lt_val  index 1.
  if sy-subrc = 0 and lt_val-zflag is initial.
    pa_model = lt_val-atwrt.
  else.
    clear: pa_model.
  endif.
endform.                    " GET_MODEL

*&---------------------------------------------------------------------*
*&      Form  get_uph_shop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      -->P_WA_UPH_T  text
*      -->P_6752   text
*----------------------------------------------------------------------*
form get_uph_shop using    pa_wdate  pa_uph  pa_arbpl .
  data: w_uph  like ztpp_status-uph.
  CALL FUNCTION 'Z_FPP_GET_UPH'
    EXPORTING
      date  = pa_wdate
*     SHIFT = PA_SHIFT
      shop  = pa_arbpl
    IMPORTING
      uph   = w_uph.
  pa_uph  = w_uph.

*  DATA lw_ld          LIKE zvpp_ld .
*
*  SELECT SINGLE * INTO lw_ld
*    FROM zvpp_ld
*   WHERE ld_perst <= pa_wdate
*     AND ld_pered >= pa_wdate
*     AND arbpl     = pa_arbpl .
*
*  IF lw_ld-lantu = 0.
*    pa_uph = 0 .
*  ELSE.
*    pa_uph = lw_ld-lrate / lw_ld-lantu .
*  ENDIF.
endform.                    " get_uph_shop
*&---------------------------------------------------------------------*
*&      Form  check_holding_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_holding_car.
  data : it_hold like ztpp_hold_car occurs 0 with header line.

  refresh it_hold.

  select * into table it_hold from ztpp_hold_car
         where  ( status eq 'W' ) or ( status eq space ).

  loop at it_hold where res_date > p_dates.
    read table it_data with key modl = it_hold-modl
                            body_ser = it_hold-body_ser.
    if sy-subrc = 0.
*          DELETE TABLE it_data FROM it_data.
      it_hold-status = 'W'.
      modify it_hold from it_hold.
    else.
      it_hold-status = 'P'.
      modify it_hold from it_hold.
    endif.
  endloop.

  loop at it_hold where res_date <= p_dates.
    if it_hold-res_date =  p_dates.
      it_hold-status = 'D'.
    else.
      read table it_data with key modl = it_hold-modl
                           body_ser = it_hold-body_ser.
      if sy-subrc = 0.
        delete table it_data from it_data.
      endif.
      it_hold-status = 'P'.
    endif.
    modify it_hold from it_hold.
  endloop.

  modify ztpp_hold_car from table it_hold.
  if sy-subrc = 0.
    message s000 with text-003.
    commit work.
  else.
    message s000 with text-004.
    rollback work.
  endif.
endform.                    " check_holding_car
