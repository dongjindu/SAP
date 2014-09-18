************************************************************************
* Program Name      : ZRPP802R_VM_WO_ALC_TIME_MPNG
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2003.02.02.
* Specifications By : JH Shin
* Development Request No : UD1K906773
* Addl Documentation:
* Description       : V/M & W/O Summary & ALC Log's Time Zone Mapping
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

report  zrpp802r_vm_wo_alc_time_mpng message-id zmpp
                                     no standard page heading
                                     line-size 330  .

tables : ztpp_wosum,    "ERP_WO QTY SUMMARY
         ztpp_alclog1,  "ALC LOG DATA-1 from PHT02GB
         ztpp_alclog2,  "ALC LOG DATA-2 from PHT02GB
         ausp,          "Characteristic Values
         cabn.          "Characteristic

include <icon>.
include <list>.

data: p_working_day type d .  "Working Date

ranges: r_atnam for cabn-atnam .

* Working Date's Data(Present)
data: begin of it_actl occurs 0,
        wo_no(18) type c,           "Work Order Number
        wo_ser(14),
        ecolor(03),
        icolor(03),
        vm_no(18) type c,           "Vehicle Master Number
        working_day  type d,           "Working Day

        rp07_date type ausp-atwrt,  " RP07's Actual Date
      end of it_actl .

* Working Date-1's Data(Previous)
data: begin of it_actl_pre occurs 0,
      wo_no(18) type c,           "Work Order Number
      wo_ser(14),
      ecolor(03),
      icolor(03),
      vm_no(18) type c,           "Vehicle Master Number
      working_day  type d,        "Working Day

      rp07_date type ausp-atwrt,
    end of it_actl_pre .

data: begin of it_alc occurs 0,
        wo_ser(14),                 "P_WORK_ORDER
        vm_no(18) type c,           "Vehicle Master Number
        mi(08)    type c,           "MI
        working_day  type d,        "Working Day

        rp01_date type ausp-atwrt,  " RP01's Actual Date
        rp02_date type ausp-atwrt,  " RP02's Actual Date
        rp03_date type ausp-atwrt,  " RP03's Actual Date
        rp04_date type ausp-atwrt,
        rp05_date type ausp-atwrt,
        rp06_date type ausp-atwrt,
        rp07_date type ausp-atwrt,  " RP07's Actual Date
        rp08_date type ausp-atwrt,
        rp09_date type ausp-atwrt,
        rp10_date type ausp-atwrt,
        rp11_date type ausp-atwrt,
        rp12_date type ausp-atwrt,
        rp13_date type ausp-atwrt,
        rp14_date type ausp-atwrt,
        rp15_date type ausp-atwrt,
        rp16_date type ausp-atwrt,
        rp17_date type ausp-atwrt,
        rp18_date type ausp-atwrt,
        rp19_date type ausp-atwrt,
        rp20_date type ausp-atwrt,
        rp21_date type ausp-atwrt,
        rp22_date type ausp-atwrt,
        rp23_date type ausp-atwrt,
        rp24_date type ausp-atwrt,
        rp25_date type ausp-atwrt,
        rp26_date type ausp-atwrt,
        rp27_date type ausp-atwrt,
        rp28_date type ausp-atwrt,
      end of it_alc.

********************************************************************
* Selection-Screen.
********************************************************************
selection-screen  begin of block blk1 with frame title text-001.
parameters: p_date  type d obligatory , "Basic Date
            p_time  type t obligatory . "Basic Time
selection-screen  end of block blk1.


***********************************************************************
initialization.
***********************************************************************
  data: l_num_02(02) type n .

  p_date = sy-datum.  "Basic Date
  p_time = sy-uzeit.  "Basic Time

  r_atnam-sign = 'I'.
  r_atnam-option = 'EQ'.

  r_atnam-low = 'P_WORK_ORDER'. append r_atnam.
  r_atnam-low = 'P_EXT_COLOR'.  append r_atnam.
  r_atnam-low = 'P_INT_COLOR'.  append r_atnam.
*  r_atnam-low = 'P_MI'.         APPEND r_atnam.
  r_atnam-low = 'P_RP07_ACTUAL_DATE'. append r_atnam.


***********************************************************************
at selection-screen.
***********************************************************************
  data: l_flag ,
        l_date type d,
        l_datetime(20).
  if p_time < '063000'.
    l_date = p_date - 1.
  else.
    l_date = p_date .
  endif.
  perform check_holiday using    l_date
                        changing l_flag   .
  concatenate l_date p_time
    into l_datetime .
  if l_flag <> space.
    message e001 with l_datetime 'The date isn''t a working day!!!'.
    exit.
  endif.

***********************************************************************
start-of-selection.
***********************************************************************
  data: l_it_ztpp_wosum_t like table of ztpp_wosum with header line,
        l_it_ztpp_wosum like table of ztpp_wosum with header line,
        l_it_ztpp_alclog2_t like table of ztpp_alclog2 with header line,
        l_it_ztpp_alclog2 like table of ztpp_alclog2 with header line,
        l_tabix type sy-tabix,
        l_c_error_flag.

**************************************************
* The First Mission : ZTPP_WOSUM Updating.
**************************************************
* Setting Working Date.
  p_working_day = p_date .
  if p_time < '063000' .
    p_working_day = p_working_day - 1.
  endif.

* Getting Data of The Working Day's(Present)
  perform get_present_data .

  p_working_day = p_working_day - 1.
* Calculating The Previous Working Day with The Work Center(Trim)
  perform calculate_prev_date using 'T'
                                     p_working_day .
* Getting Data of The day before Working Day(Previous)
  perform get_previous_data .

  sort it_actl by wo_no .
  sort it_actl_pre by wo_no .
* Appending Actual QTYs into The Temp Internal Table.
  perform append_qty tables l_it_ztpp_wosum_t .

  sort l_it_ztpp_wosum_t by wo_ser nation dealer extc intc.
* Collecting Actual QTYs
  loop at l_it_ztpp_wosum_t.
    clear l_it_ztpp_wosum.
    move-corresponding l_it_ztpp_wosum_t to l_it_ztpp_wosum.
    collect l_it_ztpp_wosum.
  endloop.

  commit work.
* Updating Data In ZTPP_WOSUM .
  perform update_ztpp_wosum tables l_it_ztpp_wosum
                            using  l_c_error_flag  .
  if l_c_error_flag = space.
    commit work.
  else.
    rollback work.
  endif.

**************************************************
* The Second Mission : ZTPP_ALCLOG2 Updating.
**************************************************
* Setting Working Date.
  p_working_day = p_date .
  if p_time < '063000' .
    p_working_day = p_working_day - 1.
  endif.

* Getting Data of The Working Day's(Present)
  perform get_present_data_for_alc_log .

  sort it_alc by wo_ser mi .
* Appending Actual QTYs into The Temp Internal Table.
  perform append_qty_for_alc_log tables l_it_ztpp_alclog2_t .

  sort l_it_ztpp_alclog2_t by cdate plant line zusage mi .
* Collecting Actual QTYs
  loop at l_it_ztpp_alclog2_t.
    clear l_it_ztpp_alclog2.
    move-corresponding l_it_ztpp_alclog2_t to l_it_ztpp_alclog2.
    collect l_it_ztpp_alclog2.
  endloop.

  commit work.
  clear l_c_error_flag.
* Updating Data In ZTPP_ALCLOG2 .
  perform update_ztpp_alclog2 tables l_it_ztpp_alclog2
                              using  l_c_error_flag  .
  if l_c_error_flag = space.
    commit work.
  else.
    rollback work.
  endif.

***********************************************************************
end-of-selection.
***********************************************************************
  skip. skip. skip.
* Writing The Day's QTYs
  perform write_qty_of_the_day .
  skip. skip. skip.
* Writing QTYs of The Day Before Working Day
  perform write_qty_of_previous_day .
  skip. skip. skip.
* Writing QTYs of ZTPP_ALCLOG2
  perform write_qty_of_alclog2 .

***********************************************************************
top-of-page.
***********************************************************************
  write:/ 'Basic Date : ', p_date,
        / 'Basic Time : ', p_time .

*
*&---------------------------------------------------------------------*
*&      Form  make_atnam_for_rp_shop_date
*&---------------------------------------------------------------------*
*       Setting Characteristic's Name For Shop Date
*----------------------------------------------------------------------*
*      -->P_R_ATNAM_LOW  text
*----------------------------------------------------------------------*
form make_atnam_for_rp_shop_date using    p_atnam
                                         p_num .
  concatenate 'P_RP'
              p_num
              '_SHOP_DATE'
    into p_atnam .
endform.                    " make_atnam_for_rp_act_date
*&---------------------------------------------------------------------*
*&      Form  set_act_date_per_rp
*&---------------------------------------------------------------------*
*       Setting Actual Date Per Reporting Point
*----------------------------------------------------------------------*
*      -->P_L_IT_AUSP_ATNAM  text
*----------------------------------------------------------------------*
form set_act_date_per_rp using    p_atnam
                                  p_atwrt .
  case p_atnam+04(02).
    when '01'.
      move p_atwrt to it_alc-rp01_date.
    when '02'.
      move p_atwrt to it_alc-rp02_date.
    when '03'.
      move p_atwrt to it_alc-rp03_date.
    when '04'.
      move p_atwrt to it_alc-rp04_date.
    when '05'.
      move p_atwrt to it_alc-rp05_date.
    when '06'.
      move p_atwrt to it_alc-rp06_date.
    when '07'.
      move p_atwrt to it_alc-rp07_date.
    when '08'.
      move p_atwrt to it_alc-rp08_date.
    when '09'.
      move p_atwrt to it_alc-rp09_date.
    when '10'.
      move p_atwrt to it_alc-rp10_date.
    when '11'.
      move p_atwrt to it_alc-rp11_date.
    when '12'.
      move p_atwrt to it_alc-rp12_date.
    when '13'.
      move p_atwrt to it_alc-rp13_date.
    when '14'.
      move p_atwrt to it_alc-rp14_date.
    when '15'.
      move p_atwrt to it_alc-rp15_date.
    when '16'.
      move p_atwrt to it_alc-rp16_date.
    when '17'.
      move p_atwrt to it_alc-rp17_date.
    when '18'.
      move p_atwrt to it_alc-rp18_date.
    when '19'.
      move p_atwrt to it_alc-rp19_date.
    when '20'.
      move p_atwrt to it_alc-rp20_date.
    when '21'.
      move p_atwrt to it_alc-rp21_date.
    when '22'.
      move p_atwrt to it_alc-rp22_date.
    when '23'.
      move p_atwrt to it_alc-rp23_date.
    when '24'.
      move p_atwrt to it_alc-rp24_date.
    when '25'.
      move p_atwrt to it_alc-rp25_date.
    when '26'.
      move p_atwrt to it_alc-rp26_date.
    when '27'.
      move p_atwrt to it_alc-rp27_date.
    when '28'.
      move p_atwrt to it_alc-rp28_date.

  endcase.

endform.                    " set_act_date_per_rp
*&---------------------------------------------------------------------*
*&      Form  get_present_data
*&---------------------------------------------------------------------*
*       Getting Working Day's Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_present_data.
  data: begin of l_it_ausp occurs 0,
          objek type ausp-objek,  "Vehicle Master Number
          atnam type cabn-atnam,  "Characteristic's Name
          atwrt type ausp-atwrt,  "Characteristic's Value
        end of l_it_ausp,

        l_num_08(08) type n,
        l_atflv type ausp-atflv,
        l_count type i.

  ranges : l_r_vmno for ausp-objek.  "Vehicle Master Number

  move: p_working_day to l_num_08,
        l_num_08      to l_atflv .

* Getting Vehicle Master Numbers By Working Date(Shop Date).
  select distinct objek
    into l_r_vmno-low
    from ausp as au
         inner join cabn as ca
            on au~atinn = ca~atinn
    where ca~atnam = 'P_RP07_SHOP_DATE' and
          au~klart = '002'              and
          au~atflv = l_atflv .
    l_r_vmno-sign = 'I'.
    l_r_vmno-option = 'EQ'.
    append l_r_vmno.
  endselect.

  describe table l_r_vmno lines l_count .
  check l_count >= 1.
* Getting Initial Data From Vehicle Master By V/M No.
  loop at l_r_vmno.
    select au~objek ca~atnam au~atwrt
      into corresponding fields of l_it_ausp
      from ausp as au
           inner join cabn as ca
              on au~atinn = ca~atinn
      where au~objek = l_r_vmno-low  and
            ca~atnam in r_atnam      and
            au~klart = '002'     .
      append l_it_ausp .
    endselect.
  endloop.

  sort l_it_ausp by objek atnam .
* Making Data From Low Data.
  loop at l_it_ausp.
    at new objek.
      clear it_actl.
      move p_working_day   to it_actl-working_day .
      move l_it_ausp-objek to it_actl-vm_no. "Setting V/M No.
    endat.

    case l_it_ausp-atnam.
      when 'P_WORK_ORDER'.                      "W/O Serial
        move l_it_ausp-atwrt to it_actl-wo_ser.

      when 'P_EXT_COLOR'.                       "External Color
        move l_it_ausp-atwrt to it_actl-ecolor.

      when 'P_INT_COLOR'.                       "Internal Color
        move l_it_ausp-atwrt to it_actl-icolor.

      when 'P_RP07_ACTUAL_DATE'.                "RP07'S ACTUAL DATE
        move l_it_ausp-atwrt to it_actl-rp07_date.

    endcase.

    at end of objek.
      concatenate it_actl-wo_ser
                  it_actl-ecolor
                  it_actl-icolor
        into it_actl-wo_no.  "Work Order Number
      append it_actl.
    endat.

  endloop.

endform.                    " get_present_data
*&---------------------------------------------------------------------*
*&      Form  get_previous_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_previous_data.
  data: begin of l_it_ausp occurs 0,
          objek type ausp-objek,  "Vehicle Master Number
          atnam type cabn-atnam,  "Characteristic's Name
          atwrt type ausp-atwrt,  "Characteristic's Value
        end of l_it_ausp,

        l_num_08(08) type n,
        l_atflv type ausp-atflv,
        l_count type i.

  ranges : l_r_vmno for ausp-objek.  "Vehicle Master Number

  move: p_working_day to l_num_08,
        l_num_08      to l_atflv .

* Getting Vehicle Master Numbers By Working Date(Shop Date).
  select distinct objek
    into l_r_vmno-low
    from ausp as au
         inner join cabn as ca
            on au~atinn = ca~atinn
    where ca~atnam = 'P_RP07_SHOP_DATE' and
          au~klart = '002'              and
          au~atflv = l_atflv .
    l_r_vmno-sign = 'I'.
    l_r_vmno-option = 'EQ'.
    append l_r_vmno.
  endselect.

  describe table l_r_vmno lines l_count.
  check l_count >= 1.
* Getting Initial Data From Vehicle Master By V/M No.
  loop at l_r_vmno.
    select au~objek ca~atnam au~atwrt
      into corresponding fields of l_it_ausp
      from ausp as au
           inner join cabn as ca
              on au~atinn = ca~atinn
      where au~objek = l_r_vmno-low and
            ca~atnam in r_atnam     and
            au~klart = '002'     .
      append l_it_ausp.
    endselect.
  endloop.

  sort l_it_ausp by objek atnam .
* Making Data From Low Data.
  loop at l_it_ausp.
    at new objek.
      clear it_actl_pre.
      move p_working_day   to it_actl_pre-working_day .
      move l_it_ausp-objek to it_actl_pre-vm_no. "Setting V/M No.
    endat.

    case l_it_ausp-atnam.
      when 'P_WORK_ORDER'.                      "W/O Serial
        move l_it_ausp-atwrt to it_actl_pre-wo_ser.

      when 'P_EXT_COLOR'.                       "External Color
        move l_it_ausp-atwrt to it_actl_pre-ecolor.

      when 'P_INT_COLOR'.                       "Internal Color
        move l_it_ausp-atwrt to it_actl_pre-icolor.

      when 'P_RP07_ACTUAL_DATE'.
        move l_it_ausp-atwrt to it_actl_pre-rp07_date.

    endcase.

    at end of objek.
      concatenate it_actl_pre-wo_ser
                  it_actl_pre-ecolor
                  it_actl_pre-icolor
        into it_actl_pre-wo_no.  "Work Order Number
      append it_actl_pre.
    endat.

  endloop.

endform.                    " get_previous_data
*&---------------------------------------------------------------------*
*&      Form  set_today_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_IT_ZTPP_WOSUM  text
*      -->P_IT_ACTL_RP07_DATE+08(06)  text
*----------------------------------------------------------------------*
form set_today_qty tables   p_it_ztpp_wosum structure ztpp_wosum
                   using    p_rp07_time.
  if     ( p_rp07_time >  '200000' and
           p_rp07_time <= '240000' )  or
         ( p_rp07_time >= '000000' and
           p_rp07_time <= '010000' ) .
    p_it_ztpp_wosum-t01dq = 1.
  elseif ( p_rp07_time >  '010000' and
           p_rp07_time <= '060000' ) .
    p_it_ztpp_wosum-t06dq = 1.
  elseif ( p_rp07_time >  '060000' and
           p_rp07_time <= '080000' ) .
    p_it_ztpp_wosum-t08dq = 1.
  elseif ( p_rp07_time >  '080000' and
           p_rp07_time <= '120000' ) .
    p_it_ztpp_wosum-t12dq = 1.
  elseif ( p_rp07_time >  '120000' and
           p_rp07_time <= '170000' ) .
    p_it_ztpp_wosum-t17dq = 1.
  elseif ( p_rp07_time >  '170000' and
           p_rp07_time <= '200000' ) .
    p_it_ztpp_wosum-t20dq = 1.
  endif.

endform.                    " set_today_qty
*&---------------------------------------------------------------------*
*&      Form  set_yesterday_qty
*&---------------------------------------------------------------------*
*       Setting Previous day's QTY
*----------------------------------------------------------------------*
*      -->P_L_IT_ZTPP_WOSUM  text
*      -->P_IT_ACTL_PRE_RP07_DATE+08(06)  text
*----------------------------------------------------------------------*
form set_yesterday_qty tables   p_it_ztpp_wosum structure ztpp_wosum
                       using    p_rp07_time .
  if     ( p_rp07_time >  '200000' and
           p_rp07_time <= '240000' )  or
         ( p_rp07_time >  '000000' and
           p_rp07_time <= '010000' ) .
    p_it_ztpp_wosum-t01pq =  1.
  elseif ( p_rp07_time >  '010000' and
           p_rp07_time <= '060000' ) .
    p_it_ztpp_wosum-t06pq =  1.
  elseif ( p_rp07_time >  '060000' and
           p_rp07_time <= '080000' ) .
    p_it_ztpp_wosum-t08pq =  1.
  elseif ( p_rp07_time >  '080000' and
           p_rp07_time <= '120000' ) .
    p_it_ztpp_wosum-t12pq =  1.
  elseif ( p_rp07_time >  '120000' and
           p_rp07_time <= '170000' ) .
    p_it_ztpp_wosum-t17pq =  1.
  elseif ( p_rp07_time >  '170000' and
           p_rp07_time <= '200000' ) .
    p_it_ztpp_wosum-t20pq =  1.
  endif.

endform.                    " set_yesterday_qty
*&---------------------------------------------------------------------*
*&      Form  append_qty
*&---------------------------------------------------------------------*
*       Appending Actual QTYs
*----------------------------------------------------------------------*
*      -->P_L_IT_ZTPP_WOSUM_T  text
*----------------------------------------------------------------------*
form append_qty tables   p_it_wosum_t structure ztpp_wosum.
* Appending Today's Actual QTY
  loop at it_actl.
    clear: p_it_wosum_t.

    move: it_actl-wo_no+00(09) to p_it_wosum_t-wo_ser,
          it_actl-wo_no+09(03) to p_it_wosum_t-nation,
          it_actl-wo_no+12(02) to p_it_wosum_t-dealer,
          it_actl-wo_no+14(02) to p_it_wosum_t-extc,
          it_actl-wo_no+16(02) to p_it_wosum_t-intc.
    if it_actl-rp07_date+00(08) = it_actl-working_day .
*     Setting Today's Actual QTY Per Time-Zone.
      perform set_today_qty tables p_it_wosum_t
                            using  it_actl-rp07_date+08(06) .
      append p_it_wosum_t.
    endif.
  endloop.
* Appending The Previous Day's Actual QTY
  loop at it_actl_pre .
    clear: p_it_wosum_t.

    move: it_actl_pre-wo_no+00(09) to p_it_wosum_t-wo_ser,
          it_actl_pre-wo_no+09(03) to p_it_wosum_t-nation,
          it_actl_pre-wo_no+12(02) to p_it_wosum_t-dealer,
          it_actl_pre-wo_no+14(02) to p_it_wosum_t-extc,
          it_actl_pre-wo_no+16(02) to p_it_wosum_t-intc.
    if it_actl_pre-rp07_date+00(08) = it_actl_pre-working_day.
*     Setting The Previous Day's Actual QTY Per Time-Zone.
      perform set_yesterday_qty tables p_it_wosum_t
                                using  it_actl_pre-rp07_date+08(06).
      append p_it_wosum_t.
    endif.
  endloop.

endform.                    " append_qty
*&---------------------------------------------------------------------*
*&      Form  update_ztpp_wosum
*&---------------------------------------------------------------------*
*       Updating ZTPP_WOSUM
*----------------------------------------------------------------------*
*      -->P_L_IT_ZTPP_WOSUM  text
*      -->P_L_C_ERROR_FLAG  text
*----------------------------------------------------------------------*
form update_ztpp_wosum tables   p_it_wosum structure ztpp_wosum
                       using    p_flag.
  data: l_wo_no(18).
  loop at p_it_wosum.
    update ztpp_wosum set: t01dq = p_it_wosum-t01dq
                           t06dq = p_it_wosum-t06dq
                           t08dq = p_it_wosum-t08dq
                           t12dq = p_it_wosum-t12dq
                           t17dq = p_it_wosum-t17dq
                           t20dq = p_it_wosum-t20dq

                           t01pq = p_it_wosum-t01pq
                           t06pq = p_it_wosum-t06pq
                           t08pq = p_it_wosum-t08pq
                           t12pq = p_it_wosum-t12pq
                           t17pq = p_it_wosum-t17pq
                           t20pq = p_it_wosum-t20pq

                           aedat = sy-datum
                           aezet = sy-uzeit
                           aenam = sy-uname

      where wo_ser = p_it_wosum-wo_ser and
            nation = p_it_wosum-nation and
            dealer = p_it_wosum-dealer and
            extc   = p_it_wosum-extc   and
            intc   = p_it_wosum-intc  .

    concatenate p_it_wosum-wo_ser
                p_it_wosum-nation
                p_it_wosum-dealer
                p_it_wosum-extc
                p_it_wosum-intc
      into l_wo_no.
    if sy-subrc <> 0.
      rollback work.
      p_flag = 'X'.
      write:/ l_wo_no, ': Error!!! - During ZTPP_WOSUM Updating.'.
      exit.
    else.
      write:/ l_wo_no, ': Updating in ZTPP_WOSUM has been done.'.
    endif.
  endloop.

endform.                    " update_ztpp_wosum
*&---------------------------------------------------------------------*
*&      Form  get_present_data_for_alc_log
*&---------------------------------------------------------------------*
*       Getting Working Day's Data For ALC Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_present_data_for_alc_log.
  data: begin of l_it_ausp occurs 0,
          objek type ausp-objek,  "Vehicle Master Number
          atnam type cabn-atnam,  "Characteristic's Name
          atwrt type ausp-atwrt,  "Characteristic's Value
        end of l_it_ausp,

        l_num_08(08) type n,
        l_num_02(02) type n,
        l_atflv type ausp-atflv,
        l_count type i.

  ranges : l_r_vmno for ausp-objek,    "Vehicle Master Number
           l_r_atnam for cabn-atnam,   "P_RPxx_SHOP_DATE
           l_r_atnam02 for cabn-atnam. "P_RPxx_ACTUAL_DATE

  move: p_working_day to l_num_08,
        l_num_08      to l_atflv .

  clear l_num_02.
* Setting ATNAM For 'P_RPxx_SHOP_DATE'
  do 28 times.
    l_num_02 = l_num_02 + 1 .
    perform make_atnam_for_rp_shop_date using l_r_atnam-low
                                             l_num_02 .
    l_r_atnam-sign = 'I'.
    l_r_atnam-option = 'EQ'.
    append l_r_atnam.
  enddo.
* Getting Vehicle Master Numbers By Working Date(Shop Date).
  select distinct objek
    into l_r_vmno-low
    from ausp as au
         inner join cabn as ca
            on au~atinn = ca~atinn
    where ca~atnam in l_r_atnam         and
          au~klart = '002'              and
          au~atflv = l_atflv .
    l_r_vmno-sign = 'I'.
    l_r_vmno-option = 'EQ'.
    append l_r_vmno.
  endselect.

  describe table l_r_vmno lines l_count.
  check l_count >= 1.
  clear l_num_02.
* Setting ATNAM For 'P_RPxx_ACTUAL_DATE'
  do 28 times.
    l_num_02 = l_num_02 + 1 .
    perform make_atnam_for_rp_act_date using l_r_atnam02-low
                                             l_num_02 .
    l_r_atnam02-sign = 'I'.
    l_r_atnam02-option = 'EQ'.
    append l_r_atnam02.
  enddo.
* Setting ATNAM For 'P_WORK_ORDER' and 'P_MI'
  l_r_atnam02-low = 'P_WORK_ORDER'. append l_r_atnam02.
  l_r_atnam02-low = 'P_MI'. append l_r_atnam02.
* Getting Initial Data From Vehicle Master By V/M No.
  loop at l_r_vmno.
    select au~objek ca~atnam au~atwrt
      into corresponding fields of l_it_ausp
      from ausp as au
           inner join cabn as ca
              on au~atinn = ca~atinn
      where au~objek = l_r_vmno-low and
            ca~atnam in l_r_atnam02 and
            au~klart = '002'     .
      append l_it_ausp.
    endselect.
  endloop.

  sort l_it_ausp by objek atnam .
* Making Data From Low Data.
  loop at l_it_ausp.
    at new objek.
      clear it_alc.
      move p_working_day   to it_alc-working_day .
      move l_it_ausp-objek to it_alc-vm_no.     "Setting V/M No.
    endat.

    case l_it_ausp-atnam.
      when 'P_WORK_ORDER'.                      "W/O Serial
        move l_it_ausp-atwrt to it_alc-wo_ser.

      when 'P_MI'.                              "P_MI
        move l_it_ausp-atwrt to it_alc-mi.

      when others.                              "P_RPxx_ACTUAL_DATE
        perform set_act_date_per_rp using l_it_ausp-atnam
                                          l_it_ausp-atwrt .
    endcase.

    at end of objek.
      append it_alc.
    endat.

  endloop.

endform.                    " get_present_data_for_alc_log
*&---------------------------------------------------------------------*
*&      Form  make_atnam_for_rp_act_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_R_ATNAM02_LOW  text
*      -->P_L_NUM_02  text
*----------------------------------------------------------------------*
form make_atnam_for_rp_act_date using    p_atnam
                                         p_num.
  concatenate 'P_RP'
              p_num
              '_ACTUAL_DATE'
    into p_atnam .

endform.                    " make_atnam_for_rp_act_date
*&---------------------------------------------------------------------*
*&      Form  append_qty_for_alc_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_IT_ZTPP_ALCLOG2_T  text
*----------------------------------------------------------------------*
form append_qty_for_alc_log tables
                              p_it_alclog2_t structure ztpp_alclog2.
* Appending Today's Actual QTY
  loop at it_alc.
    clear: p_it_alclog2_t.

    move: it_alc-working_day   to p_it_alclog2_t-cdate,
*          p_it_alclog2_t-plant,
*          p_it_alclog2_t-line,
          it_alc-wo_ser+00(01) to p_it_alclog2_t-zusage,
          it_alc-mi            to p_it_alclog2_t-mi.
    if it_alc-rp01_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP01.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '01'
                                    it_alc-rp01_date+08(06) .
    endif.
    if it_alc-rp02_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP02.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '02'
                                    it_alc-rp02_date+08(06) .
    endif.
    if it_alc-rp03_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP03.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '03'
                                    it_alc-rp03_date+08(06) .
    endif.
    if it_alc-rp04_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP04.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '04'
                                    it_alc-rp04_date+08(06) .
    endif.
    if it_alc-rp05_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP05.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '05'
                                    it_alc-rp05_date+08(06) .
    endif.
    if it_alc-rp06_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP06.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '06'
                                    it_alc-rp06_date+08(06) .
    endif.
    if it_alc-rp07_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP07.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '07'
                                    it_alc-rp07_date+08(06) .
    endif.
    if it_alc-rp08_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP08.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '08'
                                    it_alc-rp08_date+08(06) .
    endif.
    if it_alc-rp09_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP09.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '09'
                                    it_alc-rp09_date+08(06) .
    endif.
    if it_alc-rp10_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP10.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '10'
                                    it_alc-rp10_date+08(06) .
    endif.
    if it_alc-rp11_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP11.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '11'
                                    it_alc-rp11_date+08(06) .
    endif.
    if it_alc-rp12_date+00(08) = it_alc-working_day .
*     Setting Today's Actual QTY Per Time-Zone with RP12.
      perform set_actual_qty tables p_it_alclog2_t
                             using  '12'
                                    it_alc-rp12_date+08(06) .
    endif.
*
    append p_it_alclog2_t.
*
  endloop.

endform.                    " append_qty_for_alc_log
*&---------------------------------------------------------------------*
*&      Form  set_actual_qty
*&---------------------------------------------------------------------*
*       Setting Actual QTY Per RP
*----------------------------------------------------------------------*
*      -->P_P_IT_ALCLOG2_T  text
*      -->P_1657   text
*      -->P_IT_ALC_RP01_DATE+08(06)  text
*----------------------------------------------------------------------*
form set_actual_qty tables   p_it_alc_t structure ztpp_alclog2
                    using    p_point
                             p_time .
  case p_point.
    when '01'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty01_d01
                                         p_it_alc_t-qty01_d02
                                         p_it_alc_t-qty01_d03
                                         p_it_alc_t-qty01_d04
                                         p_it_alc_t-qty01_d05

                                         p_it_alc_t-qty01_n01
                                         p_it_alc_t-qty01_n02
                                         p_it_alc_t-qty01_n03
                                         p_it_alc_t-qty01_n04
                                         p_it_alc_t-qty01_n05

                                         p_time .

    when '02'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty02_d01
                                         p_it_alc_t-qty02_d02
                                         p_it_alc_t-qty02_d03
                                         p_it_alc_t-qty02_d04
                                         p_it_alc_t-qty02_d05

                                         p_it_alc_t-qty02_n01
                                         p_it_alc_t-qty02_n02
                                         p_it_alc_t-qty02_n03
                                         p_it_alc_t-qty02_n04
                                         p_it_alc_t-qty02_n05

                                         p_time .
    when '03'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty03_d01
                                         p_it_alc_t-qty03_d02
                                         p_it_alc_t-qty03_d03
                                         p_it_alc_t-qty03_d04
                                         p_it_alc_t-qty03_d05

                                         p_it_alc_t-qty03_n01
                                         p_it_alc_t-qty03_n02
                                         p_it_alc_t-qty03_n03
                                         p_it_alc_t-qty03_n04
                                         p_it_alc_t-qty03_n05

                                         p_time .
    when '04'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty04_d01
                                         p_it_alc_t-qty04_d02
                                         p_it_alc_t-qty04_d03
                                         p_it_alc_t-qty04_d04
                                         p_it_alc_t-qty04_d05

                                         p_it_alc_t-qty04_n01
                                         p_it_alc_t-qty04_n02
                                         p_it_alc_t-qty04_n03
                                         p_it_alc_t-qty04_n04
                                         p_it_alc_t-qty04_n05

                                         p_time .
    when '05'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty05_d01
                                         p_it_alc_t-qty05_d02
                                         p_it_alc_t-qty05_d03
                                         p_it_alc_t-qty05_d04
                                         p_it_alc_t-qty05_d05

                                         p_it_alc_t-qty05_n01
                                         p_it_alc_t-qty05_n02
                                         p_it_alc_t-qty05_n03
                                         p_it_alc_t-qty05_n04
                                         p_it_alc_t-qty05_n05

                                         p_time .
    when '06'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty06_d01
                                         p_it_alc_t-qty06_d02
                                         p_it_alc_t-qty06_d03
                                         p_it_alc_t-qty06_d04
                                         p_it_alc_t-qty06_d05

                                         p_it_alc_t-qty06_n01
                                         p_it_alc_t-qty06_n02
                                         p_it_alc_t-qty06_n03
                                         p_it_alc_t-qty06_n04
                                         p_it_alc_t-qty06_n05

                                         p_time .
    when '07'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty07_d01
                                         p_it_alc_t-qty07_d02
                                         p_it_alc_t-qty07_d03
                                         p_it_alc_t-qty07_d04
                                         p_it_alc_t-qty07_d05

                                         p_it_alc_t-qty07_n01
                                         p_it_alc_t-qty07_n02
                                         p_it_alc_t-qty07_n03
                                         p_it_alc_t-qty07_n04
                                         p_it_alc_t-qty07_n05

                                         p_time .
    when '08'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty08_d01
                                         p_it_alc_t-qty08_d02
                                         p_it_alc_t-qty08_d03
                                         p_it_alc_t-qty08_d04
                                         p_it_alc_t-qty08_d05

                                         p_it_alc_t-qty08_n01
                                         p_it_alc_t-qty08_n02
                                         p_it_alc_t-qty08_n03
                                         p_it_alc_t-qty08_n04
                                         p_it_alc_t-qty08_n05

                                         p_time .
    when '09'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty09_d01
                                         p_it_alc_t-qty09_d02
                                         p_it_alc_t-qty09_d03
                                         p_it_alc_t-qty09_d04
                                         p_it_alc_t-qty09_d05

                                         p_it_alc_t-qty09_n01
                                         p_it_alc_t-qty09_n02
                                         p_it_alc_t-qty09_n03
                                         p_it_alc_t-qty09_n04
                                         p_it_alc_t-qty09_n05

                                         p_time .
    when '10'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty10_d01
                                         p_it_alc_t-qty10_d02
                                         p_it_alc_t-qty10_d03
                                         p_it_alc_t-qty10_d04
                                         p_it_alc_t-qty10_d05

                                         p_it_alc_t-qty10_n01
                                         p_it_alc_t-qty10_n02
                                         p_it_alc_t-qty10_n03
                                         p_it_alc_t-qty10_n04
                                         p_it_alc_t-qty10_n05

                                         p_time .
    when '11'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty11_d01
                                         p_it_alc_t-qty11_d02
                                         p_it_alc_t-qty11_d03
                                         p_it_alc_t-qty11_d04
                                         p_it_alc_t-qty11_d05

                                         p_it_alc_t-qty11_n01
                                         p_it_alc_t-qty11_n02
                                         p_it_alc_t-qty11_n03
                                         p_it_alc_t-qty11_n04
                                         p_it_alc_t-qty11_n05

                                         p_time .
    when '12'.
      perform set_actual_qty_by_rp using p_it_alc_t-qty12_d01
                                         p_it_alc_t-qty12_d02
                                         p_it_alc_t-qty12_d03
                                         p_it_alc_t-qty12_d04
                                         p_it_alc_t-qty12_d05

                                         p_it_alc_t-qty12_n01
                                         p_it_alc_t-qty12_n02
                                         p_it_alc_t-qty12_n03
                                         p_it_alc_t-qty12_n04
                                         p_it_alc_t-qty12_n05

                                         p_time .
  endcase.

endform.                    " set_actual_qty
*&---------------------------------------------------------------------*
*&      Form  set_actual_qty_by_rp
*&---------------------------------------------------------------------*
*       Setting Actual QTY Per Time-Zone
*----------------------------------------------------------------------*
*      -->P_P_IT_ALC_T_QTY01_D01  text
*      -->P_P_IT_ALC_T_QTY01_D02  text
*      -->P_P_IT_ALC_T_QTY01_D03  text
*      -->P_P_IT_ALC_T_QTY01_D04  text
*      -->P_P_IT_ALC_T_QTY01_D05  text
*      -->P_P_IT_ALC_T_QTY01_N01  text
*      -->P_P_IT_ALC_T_QTY01_N02  text
*      -->P_P_IT_ALC_T_QTY01_N03  text
*      -->P_P_IT_ALC_T_QTY01_N04  text
*      -->P_P_IT_ALC_T_QTY01_N05  text
*      -->P_P_TIME  text
*----------------------------------------------------------------------*
form set_actual_qty_by_rp using    p_d01
                                   p_d02
                                   p_d03
                                   p_d04
                                   p_d05

                                   p_n01
                                   p_n02
                                   p_n03
                                   p_n04
                                   p_n05

                                   p_time.
*
  if     p_time > '063000' and p_time <= '083000'.
    p_d01 = 1 .
  elseif p_time > '083000' and p_time <= '103000'.
    p_d02 = 1 .
  elseif p_time > '103000' and p_time <= '131500'.
    p_d03 = 1 .
  elseif p_time > '131500' and p_time <= '151500'.
    p_d04 = 1 .
  elseif p_time > '151500' and p_time <= '171500'.
    p_d05 = 1 .
*
  elseif p_time > '171500' and p_time <= '191500'.
    p_n01 = 1 .
  elseif p_time > '191500' and p_time <= '211500'.
    p_n02 = 1 .
  elseif p_time > '211500' and p_time <= '240000'.
    p_n03 = 1 .
  elseif p_time > '000000' and p_time <= '200000'.
    p_n04 = 1 .
  elseif p_time > '200000' and p_time <= '063000'.
    p_n05 = 1 .
  endif.
endform.                    " set_actual_qty_by_rp
*&---------------------------------------------------------------------*
*&      Form  update_ztpp_alclog2
*&---------------------------------------------------------------------*
*       ZTPP_ALCLOG2 Updating
*----------------------------------------------------------------------*
*      -->P_L_IT_ZTPP_ALCLOG2  text
*      -->P_L_C_ERROR_FLAG  text
*----------------------------------------------------------------------*
form update_ztpp_alclog2 tables
                           p_it_alclog2 structure ztpp_alclog2
                         using    p_flag.
  loop at p_it_alclog2.
    update ztpp_alclog2 set: qty01_d01 = p_it_alclog2-qty01_d01
                             qty01_d02 = p_it_alclog2-qty01_d02
                             qty01_d03 = p_it_alclog2-qty01_d03
                             qty01_d04 = p_it_alclog2-qty01_d04
                             qty01_d05 = p_it_alclog2-qty01_d05
                             qty01_n01 = p_it_alclog2-qty01_n01
                             qty01_n02 = p_it_alclog2-qty01_n02
                             qty01_n03 = p_it_alclog2-qty01_n03
                             qty01_n04 = p_it_alclog2-qty01_n04
                             qty01_n05 = p_it_alclog2-qty01_n05

                             qty02_d01 = p_it_alclog2-qty02_d01
                             qty02_d02 = p_it_alclog2-qty02_d02
                             qty02_d03 = p_it_alclog2-qty02_d03
                             qty02_d04 = p_it_alclog2-qty02_d04
                             qty02_d05 = p_it_alclog2-qty02_d05
                             qty02_n01 = p_it_alclog2-qty02_n01
                             qty02_n02 = p_it_alclog2-qty02_n02
                             qty02_n03 = p_it_alclog2-qty02_n03
                             qty02_n04 = p_it_alclog2-qty02_n04
                             qty02_n05 = p_it_alclog2-qty02_n05

                             qty03_d01 = p_it_alclog2-qty03_d01
                             qty03_d02 = p_it_alclog2-qty03_d02
                             qty03_d03 = p_it_alclog2-qty03_d03
                             qty03_d04 = p_it_alclog2-qty03_d04
                             qty03_d05 = p_it_alclog2-qty03_d05
                             qty03_n01 = p_it_alclog2-qty03_n01
                             qty03_n02 = p_it_alclog2-qty03_n02
                             qty03_n03 = p_it_alclog2-qty03_n03
                             qty03_n04 = p_it_alclog2-qty03_n04
                             qty03_n05 = p_it_alclog2-qty03_n05

                             qty04_d01 = p_it_alclog2-qty04_d01
                             qty04_d02 = p_it_alclog2-qty04_d02
                             qty04_d03 = p_it_alclog2-qty04_d03
                             qty04_d04 = p_it_alclog2-qty04_d04
                             qty04_d05 = p_it_alclog2-qty04_d05
                             qty04_n01 = p_it_alclog2-qty04_n01
                             qty04_n02 = p_it_alclog2-qty04_n02
                             qty04_n03 = p_it_alclog2-qty04_n03
                             qty04_n04 = p_it_alclog2-qty04_n04
                             qty04_n05 = p_it_alclog2-qty04_n05

                             qty05_d01 = p_it_alclog2-qty05_d01
                             qty05_d02 = p_it_alclog2-qty05_d02
                             qty05_d03 = p_it_alclog2-qty05_d03
                             qty05_d04 = p_it_alclog2-qty05_d04
                             qty05_d05 = p_it_alclog2-qty05_d05
                             qty05_n01 = p_it_alclog2-qty05_n01
                             qty05_n02 = p_it_alclog2-qty05_n02
                             qty05_n03 = p_it_alclog2-qty05_n03
                             qty05_n04 = p_it_alclog2-qty05_n04
                             qty05_n05 = p_it_alclog2-qty05_n05

                             qty06_d01 = p_it_alclog2-qty06_d01
                             qty06_d02 = p_it_alclog2-qty06_d02
                             qty06_d03 = p_it_alclog2-qty06_d03
                             qty06_d04 = p_it_alclog2-qty06_d04
                             qty06_d05 = p_it_alclog2-qty06_d05
                             qty06_n01 = p_it_alclog2-qty06_n01
                             qty06_n02 = p_it_alclog2-qty06_n02
                             qty06_n03 = p_it_alclog2-qty06_n03
                             qty06_n04 = p_it_alclog2-qty06_n04
                             qty06_n05 = p_it_alclog2-qty06_n05

                             qty07_d01 = p_it_alclog2-qty07_d01
                             qty07_d02 = p_it_alclog2-qty07_d02
                             qty07_d03 = p_it_alclog2-qty07_d03
                             qty07_d04 = p_it_alclog2-qty07_d04
                             qty07_d05 = p_it_alclog2-qty07_d05
                             qty07_n01 = p_it_alclog2-qty07_n01
                             qty07_n02 = p_it_alclog2-qty07_n02
                             qty07_n03 = p_it_alclog2-qty07_n03
                             qty07_n04 = p_it_alclog2-qty07_n04
                             qty07_n05 = p_it_alclog2-qty07_n05

                             qty08_d01 = p_it_alclog2-qty08_d01
                             qty08_d02 = p_it_alclog2-qty08_d02
                             qty08_d03 = p_it_alclog2-qty08_d03
                             qty08_d04 = p_it_alclog2-qty08_d04
                             qty08_d05 = p_it_alclog2-qty08_d05
                             qty08_n01 = p_it_alclog2-qty08_n01
                             qty08_n02 = p_it_alclog2-qty08_n02
                             qty08_n03 = p_it_alclog2-qty08_n03
                             qty08_n04 = p_it_alclog2-qty08_n04
                             qty08_n05 = p_it_alclog2-qty08_n05

                             qty09_d01 = p_it_alclog2-qty09_d01
                             qty09_d02 = p_it_alclog2-qty09_d02
                             qty09_d03 = p_it_alclog2-qty09_d03
                             qty09_d04 = p_it_alclog2-qty09_d04
                             qty09_d05 = p_it_alclog2-qty09_d05
                             qty09_n01 = p_it_alclog2-qty09_n01
                             qty09_n02 = p_it_alclog2-qty09_n02
                             qty09_n03 = p_it_alclog2-qty09_n03
                             qty09_n04 = p_it_alclog2-qty09_n04
                             qty09_n05 = p_it_alclog2-qty09_n05

                             qty10_d01 = p_it_alclog2-qty10_d01
                             qty10_d02 = p_it_alclog2-qty10_d02
                             qty10_d03 = p_it_alclog2-qty10_d03
                             qty10_d04 = p_it_alclog2-qty10_d04
                             qty10_d05 = p_it_alclog2-qty10_d05
                             qty10_n01 = p_it_alclog2-qty10_n01
                             qty10_n02 = p_it_alclog2-qty10_n02
                             qty10_n03 = p_it_alclog2-qty10_n03
                             qty10_n04 = p_it_alclog2-qty10_n04
                             qty10_n05 = p_it_alclog2-qty10_n05

                             qty11_d01 = p_it_alclog2-qty11_d01
                             qty11_d02 = p_it_alclog2-qty11_d02
                             qty11_d03 = p_it_alclog2-qty11_d03
                             qty11_d04 = p_it_alclog2-qty11_d04
                             qty11_d05 = p_it_alclog2-qty11_d05
                             qty11_n01 = p_it_alclog2-qty11_n01
                             qty11_n02 = p_it_alclog2-qty11_n02
                             qty11_n03 = p_it_alclog2-qty11_n03
                             qty11_n04 = p_it_alclog2-qty11_n04
                             qty11_n05 = p_it_alclog2-qty11_n05

                             qty12_d01 = p_it_alclog2-qty12_d01
                             qty12_d02 = p_it_alclog2-qty12_d02
                             qty12_d03 = p_it_alclog2-qty12_d03
                             qty12_d04 = p_it_alclog2-qty12_d04
                             qty12_d05 = p_it_alclog2-qty12_d05
                             qty12_n01 = p_it_alclog2-qty12_n01
                             qty12_n02 = p_it_alclog2-qty12_n02
                             qty12_n03 = p_it_alclog2-qty12_n03
                             qty12_n04 = p_it_alclog2-qty12_n04
                             qty12_n05 = p_it_alclog2-qty12_n05

                             aedat = sy-datum
                             aezet = sy-uzeit
                             aenam = sy-uname

      where cdate = p_it_alclog2-cdate   and
*            plant = ' '                  and
*            line  = ' '                  and
            zusage = p_it_alclog2-zusage and
            mi     = p_it_alclog2-mi .
    if sy-subrc <> 0.
      rollback work.
      p_flag = 'X'.
      write:/ p_it_alclog2-cdate, ';',
              p_it_alclog2-plant, ';',
              p_it_alclog2-line, ';',
              p_it_alclog2-zusage, ';',
              p_it_alclog2-mi,
              ': Error!!! - During ZTPP_ALCLOG2 Updating.'.
      exit.
    else.
      write:/ p_it_alclog2-cdate, ';',
              p_it_alclog2-plant, ';',
              p_it_alclog2-line, ';',
              p_it_alclog2-zusage, ';',
              p_it_alclog2-mi,
              ': Updating in ZTPP_ALCLOG2 has been done.'.
    endif.

  endloop.

endform.                    " update_ztpp_alclog2
*&---------------------------------------------------------------------*
*&      Form  write_qty_of_the_day
*&---------------------------------------------------------------------*
*       Writing The Day's QTYs
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_qty_of_the_day.
  write:/ '*** The Day''s Actual Quantities For ZTPP_WOSUM ***'.
  write:/(15) 'Working Day', ';',
         (20) 'Work Order No.', ';',
         (20) 'Vehicle Master No.', ';',
         (20) 'RP07''s Actual Date'.
  sort it_actl by working_day wo_no vm_no.
  loop at it_actl.
    write:/(15) it_actl-working_day, ';',
           (20) it_actl-wo_no, ';',
           (20) it_actl-vm_no, ';',
           (20) it_actl-rp07_date .

  endloop.

endform.                    " write_qty_of_the_day
*&---------------------------------------------------------------------*
*&      Form  write_qty_of_alclog2
*&---------------------------------------------------------------------*
*       Writing ALC log's QTY
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_qty_of_alclog2.
  write:/ '*** The Day''s Quantities For ZTPP_ALCLOG2 ***'.
  write:/(15) 'Working Day', ';',
         (15) 'Work Order Ser', ';',
         (20) 'Vehicle Master No.', ';',
         (10) 'Model Index', ';',
         (18) 'RP01''s Act.Date', ';',
         (18) 'RP02''s Act.Date', ';',
         (18) 'RP03''s Act.Date', ';',
         (18) 'RP04''s Act.Date', ';',
         (18) 'RP05''s Act.Date', ';',
         (18) 'RP06''s Act.Date', ';',
         (18) 'RP07''s Act.Date', ';',
         (18) 'RP08''s Act.Date', ';',
         (18) 'RP09''s Act.Date', ';',
         (18) 'RP10''s Act.Date', ';',
         (18) 'RP11''s Act.Date', ';',
         (18) 'RP12''s Act.Date'  .
  sort it_alc by working_day wo_ser vm_no mi.
  loop at it_alc.
    write:/(15) it_alc-working_day, ';',
           (15) it_alc-wo_ser, ';',
           (20) it_alc-vm_no, ';',
           (10) it_alc-mi, ';',
           (18) it_alc-rp01_date, ';',
           (18) it_alc-rp02_date, ';',
           (18) it_alc-rp03_date, ';',
           (18) it_alc-rp04_date, ';',
           (18) it_alc-rp05_date, ';',
           (18) it_alc-rp06_date, ';',
           (18) it_alc-rp07_date, ';',
           (18) it_alc-rp08_date, ';',
           (18) it_alc-rp09_date, ';',
           (18) it_alc-rp10_date, ';',
           (18) it_alc-rp11_date, ';',
           (18) it_alc-rp12_date  .

  endloop.

endform.                    " write_qty_of_alclog2
*&---------------------------------------------------------------------*
*&      Form  write_qty_of_previous_day
*&---------------------------------------------------------------------*
*       Writing QTY of The Day Before Working Day
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_qty_of_previous_day.
  write:/
    '*** Actual Quantities of The Previous Day For ZTPP_WOSUM ***'.
  write:/(15) 'The Previous Day', ';',
         (20) 'Work Order No.', ';',
         (20) 'Vehicle Master No.', ';',
         (20) 'RP07''s Actual Date'.
  sort it_actl_pre by working_day wo_no vm_no.
  loop at it_actl_pre.
    write:/(15) it_actl_pre-working_day, ';',
           (20) it_actl_pre-wo_no, ';',
           (20) it_actl_pre-vm_no, ';',
           (20) it_actl_pre-rp07_date .

  endloop.

endform.                    " write_qty_of_previous_day
*&---------------------------------------------------------------------*
*&      Form  check_holiday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DATE  text
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
form check_holiday using    p_date
                   changing p_flag.
  data: lt_holiday type iscal_day occurs 0 .
  data: l_line type i.
  clear: lt_holiday, lt_holiday[].
  call function 'HOLIDAY_GET'
   exporting
*     HOLIDAY_CALENDAR                 = ' '
     factory_calendar                 = 'HM'
     date_from                        = p_date
     date_to                          = p_date
*   IMPORTING
*     YEAR_OF_VALID_FROM               =
*     YEAR_OF_VALID_TO                 =
*     RETURNCODE                       =
    tables
      holidays                         = lt_holiday
   exceptions
     factory_calendar_not_found       = 1
     holiday_calendar_not_found       = 2
     date_has_invalid_format          = 3
     date_inconsistency               = 4
     others                           = 5
            .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
  describe table lt_holiday lines l_line.
  if l_line > 0 .
    p_flag = 'X'.
  endif.
endform.                    " check_holiday
*&---------------------------------------------------------------------*
*&      Form  calculate_prev_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0401   text
*      -->P_P_WORKING_DAY  text
*----------------------------------------------------------------------*
form calculate_prev_date using    p_arbpl
                                  p_date_t.
  data: l_ident type zvpp_capacity-kalid .
  select single kalid  into l_ident
    from zvpp_capacity
   where arbpl = p_arbpl .

  call function 'DATE_CONVERT_TO_FACTORYDATE'
       exporting
            correct_option      = '-'
            date                = p_date_t
            factory_calendar_id = l_ident
       importing
            date                = p_date_t
       exceptions
            ca.
endform.                    " calculate_prev_date
